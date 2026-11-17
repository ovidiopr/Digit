unit uMapRaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, BGRABitmap, BGRABitmapTypes, uSciDM3;

type
  // Raw intensity values (counts, float) of a 2D image
  TMapRaster = class(TObject)
  private
    FWidth: Integer;
    FHeight: Integer;
    FData: array of Single;
    FUnits: String;
    FMinValue: Double;
    FMaxValue: Double;
    function GetValue(X, Y: Integer): Double;
    procedure SetValue(X, Y: Integer; AValue: Double);
  public
    procedure SetSize(AWidth, AHeight: Integer);
    procedure UpdateRange;
    procedure LoadFromDM3(DM3: TSciDM3; Slice: Integer = -1);
    procedure LoadLuminance(Bmp: TBGRABitmap);

    function Sample(X, Y: Double): Double;
    function InRange(X, Y: Integer): Boolean;

    // 8-bit grey image between ALow and AHigh
    function ToBitmap(ALow, AHigh: Double): TBGRABitmap;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Units: String read FUnits write FUnits;
    property MinValue: Double read FMinValue;
    property MaxValue: Double read FMaxValue;
    property Value[X, Y: Integer]: Double read GetValue write SetValue; default;
  end;

// Rec. 709 luminance of a pixel
function Luminance(const c: TBGRAPixel): Double; inline;

implementation

function Luminance(const c: TBGRAPixel): Double;
begin
  Result := 0.2126*c.red + 0.7152*c.green + 0.0722*c.blue;
end;

procedure TMapRaster.SetSize(AWidth, AHeight: Integer);
begin
  FWidth := Max(AWidth, 0);
  FHeight := Max(AHeight, 0);
  SetLength(FData, FWidth*FHeight);
end;

function TMapRaster.GetValue(X, Y: Integer): Double;
begin
  Result := FData[Y*FWidth + X];
end;

procedure TMapRaster.SetValue(X, Y: Integer; AValue: Double);
begin
  FData[Y*FWidth + X] := AValue;
end;

function TMapRaster.InRange(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < FWidth) and (Y < FHeight);
end;

procedure TMapRaster.UpdateRange;
var
  i: Integer;
begin
  FMinValue := 0;
  FMaxValue := 0;
  if (System.Length(FData) = 0) then Exit;
  FMinValue := FData[0];
  FMaxValue := FData[0];
  for i := 1 to High(FData) do
  begin
    if (FData[i] < FMinValue) then FMinValue := FData[i];
    if (FData[i] > FMaxValue) then FMaxValue := FData[i];
  end;
end;

procedure TMapRaster.LoadFromDM3(DM3: TSciDM3; Slice: Integer = -1);
var
  x, y, k, k0, k1: Integer;
  s: Double;
begin
  SetSize(DM3.ImageWidth, DM3.ImageHeight);
  // A negative slice sums all of them (spectrum images)
  if (Slice < 0) then
  begin
    k0 := 0;
    k1 := Max(DM3.ImageDepth, 1) - 1;
  end
  else
  begin
    k0 := Slice;
    k1 := Slice;
  end;
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      s := 0;
      for k := k0 to k1 do
        s := s + DM3.PixelValue(x, y, k);
      FData[y*FWidth + x] := s;
    end;
  FUnits := DM3.SptUnits;
  UpdateRange;
end;

procedure TMapRaster.LoadLuminance(Bmp: TBGRABitmap);
var
  x, y: Integer;
  p: PBGRAPixel;
begin
  SetSize(Bmp.Width, Bmp.Height);
  for y := 0 to FHeight - 1 do
  begin
    p := Bmp.ScanLine[y];
    for x := 0 to FWidth - 1 do
    begin
      FData[y*FWidth + x] := Luminance(p^);
      Inc(p);
    end;
  end;
  FUnits := '';
  UpdateRange;
end;

function TMapRaster.Sample(X, Y: Double): Double;
var
  x0, y0, x1, y1: Integer;
  fx, fy: Double;
begin
  // Bilinear interpolation, clamped at the borders
  Result := NaN;
  if (FWidth = 0) or (FHeight = 0) then Exit;
  X := EnsureRange(X, 0, FWidth - 1);
  Y := EnsureRange(Y, 0, FHeight - 1);
  x0 := Floor(X);
  y0 := Floor(Y);
  x1 := Min(x0 + 1, FWidth - 1);
  y1 := Min(y0 + 1, FHeight - 1);
  fx := X - x0;
  fy := Y - y0;
  Result := (1 - fx)*(1 - fy)*GetValue(x0, y0) + fx*(1 - fy)*GetValue(x1, y0) +
            (1 - fx)*fy*GetValue(x0, y1) + fx*fy*GetValue(x1, y1);
end;

function TMapRaster.ToBitmap(ALow, AHigh: Double): TBGRABitmap;
var
  x, y: Integer;
  c: Byte;
  p: PBGRAPixel;
begin
  Result := TBGRABitmap.Create(FWidth, FHeight);
  if (AHigh <= ALow) then AHigh := ALow + 1;
  for y := 0 to FHeight - 1 do
  begin
    p := Result.ScanLine[y];
    for x := 0 to FWidth - 1 do
    begin
      c := Round(255*EnsureRange((FData[y*FWidth + x] - ALow)/(AHigh - ALow), 0, 1));
      p^ := BGRA(c, c, c);
      Inc(p);
    end;
  end;
  Result.InvalidateBitmap;
end;

end.
