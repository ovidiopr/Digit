unit uColorBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DOM, BGRABitmap, BGRABitmapTypes, uCoordinates,
  uMapScale;

type
  TLab = record
    L, A, B: Double;
  end;

  // Colour -> value lookup sampled along a colour bar in the image
  TColorBar = class(TObject)
  private
    FStart: TCurvePoint;
    FEnd: TCurvePoint;
    FValueStart: Double;
    FValueEnd: Double;
    FLogScale: Boolean;
    FHalfWidth: Integer;
    FTolerance: Double;
    FUnits: String;
    FLab: array of TLab;
    FT: array of Double;
    FDefined: Boolean;
    FNeedsRebuild: Boolean;
    FLastColor: TBGRAPixel;
    FLastValue: Double;
    FHasLast: Boolean;
    function GetCount: Integer;
  public
    constructor Create;

    procedure Clear;
    procedure Define(Bmp: TBGRABitmap; const P1, P2: TCurvePoint; V1, V2: Double;
                     ALogScale: Boolean; const AUnits: String);
    procedure Rebuild(Bmp: TBGRABitmap);
    procedure ScaleImage(Factor: Double);

    function ValueOfT(t: Double): Double;
    function ValueOfColor(const c: TBGRAPixel): Double;

    function ExportToXML(Doc: TXMLDocument): TDOMNode;
    procedure ImportFromXML(Node: TDOMNode);

    property Defined: Boolean read FDefined;
    property NeedsRebuild: Boolean read FNeedsRebuild;
    property StartPoint: TCurvePoint read FStart;
    property EndPoint: TCurvePoint read FEnd;
    property ValueStart: Double read FValueStart;
    property ValueEnd: Double read FValueEnd;
    property LogScale: Boolean read FLogScale;
    property Units: String read FUnits;
    property HalfWidth: Integer read FHalfWidth write FHalfWidth;
    property Tolerance: Double read FTolerance write FTolerance;
    property Count: Integer read GetCount;
  end;

function RGBToLab(R, G, B: Double): TLab;
function LabDistance(const a, b: TLab): Double; inline;

implementation

function RGBToLab(R, G, B: Double): TLab;

  function Lin(c: Double): Double;
  begin
    c := c/255;
    if (c <= 0.04045) then
      Result := c/12.92
    else
      Result := Power((c + 0.055)/1.055, 2.4);
  end;

  function F(t: Double): Double;
  begin
    if (t > 216/24389) then
      Result := Power(t, 1/3)
    else
      Result := t/(3*Sqr(6/29)) + 4/29;
  end;

var
  rl, gl, bl, X, Y, Z: Double;
begin
  // sRGB (D65) -> CIE L*a*b*
  rl := Lin(R);
  gl := Lin(G);
  bl := Lin(B);
  X := (0.4124564*rl + 0.3575761*gl + 0.1804375*bl)/0.95047;
  Y := (0.2126729*rl + 0.7151522*gl + 0.0721750*bl);
  Z := (0.0193339*rl + 0.1191920*gl + 0.9503041*bl)/1.08883;
  Result.L := 116*F(Y) - 16;
  Result.A := 500*(F(X) - F(Y));
  Result.B := 200*(F(Y) - F(Z));
end;

function LabDistance(const a, b: TLab): Double;
begin
  Result := Sqrt(Sqr(a.L - b.L) + Sqr(a.A - b.A) + Sqr(a.B - b.B));
end;

//=============================|TColorBar|====================================//
constructor TColorBar.Create;
begin
  inherited Create;
  FHalfWidth := 3;
  FTolerance := 10;
  Clear;
end;

procedure TColorBar.Clear;
begin
  FDefined := False;
  FNeedsRebuild := False;
  FHasLast := False;
  SetLength(FLab, 0);
  SetLength(FT, 0);
end;

function TColorBar.GetCount: Integer;
begin
  Result := Length(FLab);
end;

procedure TColorBar.Define(Bmp: TBGRABitmap; const P1, P2: TCurvePoint; V1, V2: Double;
  ALogScale: Boolean; const AUnits: String);
begin
  FStart := P1;
  FEnd := P2;
  FValueStart := V1;
  FValueEnd := V2;
  FLogScale := ALogScale and (V1 > 0) and (V2 > 0);
  FUnits := AUnits;
  Rebuild(Bmp);
end;

procedure TColorBar.Rebuild(Bmp: TBGRABitmap);
var
  i, k, n, Cnt, px, py: Integer;
  t, Len, sr, sg, sb: Double;
  D, Nrm, P: TCurvePoint;
  c: TBGRAPixel;
begin
  Clear;
  Len := FStart.DistanceTo(FEnd);
  if (Len < 2) or (Bmp = nil) or (Bmp.Width = 0) then Exit;

  n := Ceil(Len) + 1;
  SetLength(FLab, n);
  SetLength(FT, n);
  D := FEnd - FStart;
  Nrm := TCurvePoint.Create(-D.Y/Len, D.X/Len);

  // Mean colour across the bar width at each step
  for i := 0 to n - 1 do
  begin
    t := i/(n - 1);
    sr := 0; sg := 0; sb := 0; Cnt := 0;
    for k := -FHalfWidth to FHalfWidth do
    begin
      P := FStart + t*D + k*Nrm;
      px := Round(P.X);
      py := Round(P.Y);
      if (px >= 0) and (py >= 0) and (px < Bmp.Width) and (py < Bmp.Height) then
      begin
        c := Bmp.GetPixel(px, py);
        sr := sr + c.red;
        sg := sg + c.green;
        sb := sb + c.blue;
        Inc(Cnt);
      end;
    end;
    if (Cnt = 0) then Cnt := 1;
    FLab[i] := RGBToLab(sr/Cnt, sg/Cnt, sb/Cnt);
    FT[i] := t;
  end;
  FDefined := True;
end;

procedure TColorBar.ScaleImage(Factor: Double);
begin
  FStart := Factor*FStart;
  FEnd := Factor*FEnd;
  // LUT must be sampled again from the resampled image
  FNeedsRebuild := FDefined or FNeedsRebuild;
  FDefined := False;
end;

function TColorBar.ValueOfT(t: Double): Double;
begin
  if FLogScale then
    Result := Exp(Ln(FValueStart) + t*(Ln(FValueEnd) - Ln(FValueStart)))
  else
    Result := FValueStart + t*(FValueEnd - FValueStart);
end;

function TColorBar.ValueOfColor(const c: TBGRAPixel): Double;
var
  i, j, k: Integer;
  Lab, AB: TLab;
  d, dBest, u, uDen, tBest: Double;
begin
  Result := NaN;
  if (not FDefined) or (Length(FLab) = 0) then Exit;

  // Neighbouring pixels often share a colour
  if FHasLast and (c.red = FLastColor.red) and (c.green = FLastColor.green) and
     (c.blue = FLastColor.blue) then
    Exit(FLastValue);

  Lab := RGBToLab(c.red, c.green, c.blue);
  k := 0;
  dBest := LabDistance(Lab, FLab[0]);
  for i := 1 to High(FLab) do
  begin
    d := LabDistance(Lab, FLab[i]);
    if (d < dBest) then
    begin
      dBest := d;
      k := i;
    end;
  end;

  if (dBest <= FTolerance) then
  begin
    // Refine between the nearest entry and its neighbours
    tBest := FT[k];
    for j := k - 1 to k + 1 do
      if (j <> k) and (j >= 0) and (j <= High(FLab)) then
      begin
        AB.L := FLab[j].L - FLab[k].L;
        AB.A := FLab[j].A - FLab[k].A;
        AB.B := FLab[j].B - FLab[k].B;
        uDen := Sqr(AB.L) + Sqr(AB.A) + Sqr(AB.B);
        if (uDen <= 1e-12) then Continue;
        u := EnsureRange(((Lab.L - FLab[k].L)*AB.L + (Lab.A - FLab[k].A)*AB.A +
                          (Lab.B - FLab[k].B)*AB.B)/uDen, 0, 1);
        d := Sqrt(Sqr(FLab[k].L + u*AB.L - Lab.L) + Sqr(FLab[k].A + u*AB.A - Lab.A) +
                  Sqr(FLab[k].B + u*AB.B - Lab.B));
        if (d < dBest) then
        begin
          dBest := d;
          tBest := FT[k] + u*(FT[j] - FT[k]);
        end;
      end;
    Result := ValueOfT(tBest);
  end;

  FLastColor := c;
  FLastValue := Result;
  FHasLast := True;
end;

function TColorBar.ExportToXML(Doc: TXMLDocument): TDOMNode;
begin
  Result := Doc.CreateElement('colorbar');
  with TDOMElement(Result) do
  begin
    SetAttribute('Defined', UTF8Decode(BoolToStr(FDefined or FNeedsRebuild, True)));
    SetAttribute('X1', UTF8Decode(XMLFloat(FStart.X)));
    SetAttribute('Y1', UTF8Decode(XMLFloat(FStart.Y)));
    SetAttribute('X2', UTF8Decode(XMLFloat(FEnd.X)));
    SetAttribute('Y2', UTF8Decode(XMLFloat(FEnd.Y)));
    SetAttribute('V1', UTF8Decode(XMLFloat(FValueStart)));
    SetAttribute('V2', UTF8Decode(XMLFloat(FValueEnd)));
    SetAttribute('Log', UTF8Decode(BoolToStr(FLogScale, True)));
    SetAttribute('HalfWidth', UTF8Decode(IntToStr(FHalfWidth)));
    SetAttribute('Tolerance', UTF8Decode(XMLFloat(FTolerance)));
    SetAttribute('Units', UTF8Decode(FUnits));
  end;
end;

procedure TColorBar.ImportFromXML(Node: TDOMNode);
begin
  Clear;
  FStart := TCurvePoint.Create(GetXMLFloat(Node, 'X1'), GetXMLFloat(Node, 'Y1'));
  FEnd := TCurvePoint.Create(GetXMLFloat(Node, 'X2'), GetXMLFloat(Node, 'Y2'));
  FValueStart := GetXMLFloat(Node, 'V1');
  FValueEnd := GetXMLFloat(Node, 'V2');
  FLogScale := SameText(GetXMLAttr(Node, 'Log', 'False'), 'True');
  FHalfWidth := StrToIntDef(GetXMLAttr(Node, 'HalfWidth', '3'), 3);
  FTolerance := GetXMLFloat(Node, 'Tolerance', 10);
  FUnits := GetXMLAttr(Node, 'Units');
  // The owner rebuilds the LUT from the image
  FNeedsRebuild := SameText(GetXMLAttr(Node, 'Defined', 'False'), 'True');
end;

end.
