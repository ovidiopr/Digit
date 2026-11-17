unit uMapScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, DOM, uCoordinates;

type
  TMapCalibration = (mcNone, mcScaleBar, mcReference, mcAxes);

  // Image pixels <-> map coordinates through an affine transform
  TMapScale = class(TObject)
  private
    FMode: TMapCalibration;
    FUnits: String;
    FImageHeight: Integer;
    FYUp: Boolean;

    FBarStart: TCurvePoint;
    FBarEnd: TCurvePoint;
    FBarLength: Double;

    FImgPoints: TScalePoints;
    FMapPoints: TScalePoints;

    FOrigin: TCurvePoint;
    FPixelSize: TCurvePoint;

    // map = A*img + B
    FA11, FA12, FA21, FA22: Double;
    FB: TCurvePoint;
    FValid: Boolean;

    FOnChange: TNotifyEvent;

    procedure Update;
    procedure Changed;
    function GetUnits: String;
    function GetAreaUnits: String;
    function GetImgPoint(Index: Integer): TCurvePoint;
    function GetMapPoint(Index: Integer): TCurvePoint;
    function GetIsCalibrated: Boolean;
    procedure SetImageHeight(Value: Integer);
    procedure SetYUp(Value: Boolean);
  public
    constructor Create;

    procedure Assign(Source: TMapScale);
    procedure Clear;

    procedure SetScaleBar(const P1, P2: TCurvePoint; ALength: Double; const AUnits: String);
    procedure SetReference(const ImgPts, MapPts: TScalePoints; const AUnits: String);
    procedure SetAxes(const AOrigin, APixelSize: TCurvePoint; const AUnits: String);
    procedure ScaleImage(Factor: Double);

    function ImgToMap(const P: TCurvePoint): TCurvePoint;
    function MapToImg(const P: TCurvePoint): TCurvePoint;
    function Distance(const P1, P2: TCurvePoint): Double;
    function PixelArea: Double;

    function ExportToXML(Doc: TXMLDocument): TDOMNode;
    procedure ImportFromXML(Node: TDOMNode);

    property Mode: TMapCalibration read FMode;
    property Units: String read GetUnits;
    property AreaUnits: String read GetAreaUnits;
    property IsCalibrated: Boolean read GetIsCalibrated;
    property IsValid: Boolean read FValid;
    property ImageHeight: Integer read FImageHeight write SetImageHeight;
    property YUp: Boolean read FYUp write SetYUp;
    property BarStart: TCurvePoint read FBarStart;
    property BarEnd: TCurvePoint read FBarEnd;
    property BarLength: Double read FBarLength;
    property ImgPoint[Index: Integer]: TCurvePoint read GetImgPoint;
    property MapPoint[Index: Integer]: TCurvePoint read GetMapPoint;
    property Origin: TCurvePoint read FOrigin;
    property PixelSize: TCurvePoint read FPixelSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

// Locale-independent float text for project files
function XMLFloat(Value: Double): String;
function ParseFloat(const S: String; Default: Double = 0): Double;
function GetXMLAttr(Node: TDOMNode; const Name: String; const Default: String = ''): String;
function GetXMLFloat(Node: TDOMNode; const Name: String; Default: Double = 0): Double;

implementation

var
  InvariantFS: TFormatSettings;

function XMLFloat(Value: Double): String;
begin
  Result := FloatToStr(Value, InvariantFS);
end;

function ParseFloat(const S: String; Default: Double = 0): Double;
begin
  // Accept both '.' and ',' as decimal separator
  if not TryStrToFloat(StringReplace(Trim(S), ',', '.', [rfReplaceAll]), Result, InvariantFS) then
    Result := Default;
end;

function GetXMLAttr(Node: TDOMNode; const Name: String; const Default: String = ''): String;
var
  Attr: TDOMNode;
begin
  Result := Default;
  if Assigned(Node) and Assigned(Node.Attributes) then
  begin
    Attr := Node.Attributes.GetNamedItem(UTF8Decode(Name));
    if Assigned(Attr) then
      Result := UTF8Encode(Attr.NodeValue);
  end;
end;

function GetXMLFloat(Node: TDOMNode; const Name: String; Default: Double = 0): Double;
begin
  Result := ParseFloat(GetXMLAttr(Node, Name), Default);
end;

function ModeToStr(Value: TMapCalibration): String;
begin
  case Value of
    mcScaleBar: Result := 'ScaleBar';
    mcReference: Result := 'Reference';
    mcAxes: Result := 'Axes';
    else Result := 'None';
  end;
end;

function StrToMode(const Value: String): TMapCalibration;
begin
  if SameText(Value, 'ScaleBar') then Result := mcScaleBar
  else if SameText(Value, 'Reference') then Result := mcReference
  else if SameText(Value, 'Axes') then Result := mcAxes
  else Result := mcNone;
end;

//=============================|TMapScale|====================================//
constructor TMapScale.Create;
begin
  inherited Create;
  Clear;
end;

procedure TMapScale.Assign(Source: TMapScale);
begin
  FMode := Source.FMode;
  FUnits := Source.FUnits;
  FImageHeight := Source.FImageHeight;
  FYUp := Source.FYUp;
  FBarStart := Source.FBarStart;
  FBarEnd := Source.FBarEnd;
  FBarLength := Source.FBarLength;
  FImgPoints := Source.FImgPoints;
  FMapPoints := Source.FMapPoints;
  FOrigin := Source.FOrigin;
  FPixelSize := Source.FPixelSize;
  Update;
  Changed;
end;

procedure TMapScale.Clear;
var
  i: Integer;
begin
  FMode := mcNone;
  FUnits := '';
  FYUp := False;
  FBarStart := TCurvePoint.Create(0, 0);
  FBarEnd := TCurvePoint.Create(0, 0);
  FBarLength := 0;
  for i := 1 to 3 do
  begin
    FImgPoints[i] := TCurvePoint.Create(0, 0);
    FMapPoints[i] := TCurvePoint.Create(0, 0);
  end;
  FOrigin := TCurvePoint.Create(0, 0);
  FPixelSize := TCurvePoint.Create(1, 1);
  Update;
  Changed;
end;

procedure TMapScale.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMapScale.Update;
var
  s, d: Double;
  x1, y1, x2, y2, x3, y3: Double;

  // Cramer's rule for [x y 1] * [a b c]' = [r1 r2 r3]'
  procedure Solve(r1, r2, r3: Double; out a, b, c: Double);
  begin
    a := (r1*(y2 - y3) - y1*(r2 - r3) + (r2*y3 - r3*y2))/d;
    b := (x1*(r2 - r3) - r1*(x2 - x3) + (x2*r3 - x3*r2))/d;
    c := (x1*(y2*r3 - y3*r2) - y1*(x2*r3 - x3*r2) + r1*(x2*y3 - x3*y2))/d;
  end;

begin
  // Identity: map units are pixels
  FA11 := 1; FA12 := 0; FA21 := 0; FA22 := 1;
  FB := TCurvePoint.Create(0, 0);
  FValid := True;

  case FMode of
    mcScaleBar: begin
      d := FBarStart.DistanceTo(FBarEnd);
      FValid := (d > 0) and (FBarLength > 0);
      if FValid then
      begin
        s := FBarLength/d;
        FA11 := s;
        if FYUp then
        begin
          FA22 := -s;
          FB := TCurvePoint.Create(0, s*FImageHeight);
        end
        else
          FA22 := s;
      end;
    end;
    mcReference: begin
      x1 := FImgPoints[1].X; y1 := FImgPoints[1].Y;
      x2 := FImgPoints[2].X; y2 := FImgPoints[2].Y;
      x3 := FImgPoints[3].X; y3 := FImgPoints[3].Y;
      d := x1*(y2 - y3) - y1*(x2 - x3) + (x2*y3 - x3*y2);
      FValid := Abs(d) > 1e-9;
      if FValid then
      begin
        Solve(FMapPoints[1].X, FMapPoints[2].X, FMapPoints[3].X, FA11, FA12, FB.X);
        Solve(FMapPoints[1].Y, FMapPoints[2].Y, FMapPoints[3].Y, FA21, FA22, FB.Y);
      end;
    end;
    mcAxes: begin
      FValid := (FPixelSize.X <> 0) and (FPixelSize.Y <> 0);
      if FValid then
      begin
        // DM convention: value = (pixel - origin)*scale
        FA11 := FPixelSize.X;
        FA22 := FPixelSize.Y;
        FB := TCurvePoint.Create(-FOrigin.X*FPixelSize.X, -FOrigin.Y*FPixelSize.Y);
      end;
    end;
  end;

  if not FValid then
  begin
    FA11 := 1; FA12 := 0; FA21 := 0; FA22 := 1;
    FB := TCurvePoint.Create(0, 0);
  end;
end;

function TMapScale.GetUnits: String;
begin
  if (FMode = mcNone) or (not FValid) then
    Result := 'px'
  else
    Result := FUnits;
end;

function TMapScale.GetAreaUnits: String;
begin
  Result := Units + '²';
end;

function TMapScale.GetImgPoint(Index: Integer): TCurvePoint;
begin
  Result := FImgPoints[Index];
end;

function TMapScale.GetMapPoint(Index: Integer): TCurvePoint;
begin
  Result := FMapPoints[Index];
end;

function TMapScale.GetIsCalibrated: Boolean;
begin
  Result := (FMode <> mcNone) and FValid;
end;

procedure TMapScale.SetImageHeight(Value: Integer);
begin
  if (Value <> FImageHeight) then
  begin
    FImageHeight := Value;
    Update;
  end;
end;

procedure TMapScale.SetYUp(Value: Boolean);
begin
  if (Value <> FYUp) then
  begin
    FYUp := Value;
    Update;
    Changed;
  end;
end;

procedure TMapScale.SetScaleBar(const P1, P2: TCurvePoint; ALength: Double; const AUnits: String);
begin
  FMode := mcScaleBar;
  FBarStart := P1;
  FBarEnd := P2;
  FBarLength := ALength;
  FUnits := AUnits;
  Update;
  Changed;
end;

procedure TMapScale.SetReference(const ImgPts, MapPts: TScalePoints; const AUnits: String);
begin
  FMode := mcReference;
  FImgPoints := ImgPts;
  FMapPoints := MapPts;
  FUnits := AUnits;
  Update;
  Changed;
end;

procedure TMapScale.SetAxes(const AOrigin, APixelSize: TCurvePoint; const AUnits: String);
begin
  FMode := mcAxes;
  FOrigin := AOrigin;
  FPixelSize := APixelSize;
  FUnits := AUnits;
  Update;
  Changed;
end;

procedure TMapScale.ScaleImage(Factor: Double);
var
  i: Integer;
begin
  // Image resampled by Factor: keep the same physical calibration
  if (Factor <= 0) then Exit;
  FImageHeight := Round(Factor*FImageHeight);
  FBarStart := Factor*FBarStart;
  FBarEnd := Factor*FBarEnd;
  for i := 1 to 3 do
    FImgPoints[i] := Factor*FImgPoints[i];
  FOrigin := Factor*FOrigin;
  FPixelSize := FPixelSize/Factor;
  Update;
  Changed;
end;

function TMapScale.ImgToMap(const P: TCurvePoint): TCurvePoint;
begin
  Result := TCurvePoint.Create(FA11*P.X + FA12*P.Y + FB.X,
                               FA21*P.X + FA22*P.Y + FB.Y);
end;

function TMapScale.MapToImg(const P: TCurvePoint): TCurvePoint;
var
  d, u, v: Double;
begin
  d := FA11*FA22 - FA12*FA21;
  u := P.X - FB.X;
  v := P.Y - FB.Y;
  Result := TCurvePoint.Create((FA22*u - FA12*v)/d, (FA11*v - FA21*u)/d);
end;

function TMapScale.Distance(const P1, P2: TCurvePoint): Double;
begin
  Result := ImgToMap(P1).DistanceTo(ImgToMap(P2));
end;

function TMapScale.PixelArea: Double;
begin
  Result := Abs(FA11*FA22 - FA12*FA21);
end;

function TMapScale.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  Node: TDOMElement;
begin
  Result := Doc.CreateElement('scale');
  with TDOMElement(Result) do
  begin
    SetAttribute('Mode', UTF8Decode(ModeToStr(FMode)));
    SetAttribute('Units', UTF8Decode(FUnits));
    SetAttribute('YUp', UTF8Decode(BoolToStr(FYUp, True)));
    SetAttribute('ImageHeight', UTF8Decode(IntToStr(FImageHeight)));
  end;

  Node := Doc.CreateElement('bar');
  Node.SetAttribute('X1', UTF8Decode(XMLFloat(FBarStart.X)));
  Node.SetAttribute('Y1', UTF8Decode(XMLFloat(FBarStart.Y)));
  Node.SetAttribute('X2', UTF8Decode(XMLFloat(FBarEnd.X)));
  Node.SetAttribute('Y2', UTF8Decode(XMLFloat(FBarEnd.Y)));
  Node.SetAttribute('Length', UTF8Decode(XMLFloat(FBarLength)));
  Result.AppendChild(Node);

  for i := 1 to 3 do
  begin
    Node := Doc.CreateElement('reference');
    Node.SetAttribute('ImgX', UTF8Decode(XMLFloat(FImgPoints[i].X)));
    Node.SetAttribute('ImgY', UTF8Decode(XMLFloat(FImgPoints[i].Y)));
    Node.SetAttribute('MapX', UTF8Decode(XMLFloat(FMapPoints[i].X)));
    Node.SetAttribute('MapY', UTF8Decode(XMLFloat(FMapPoints[i].Y)));
    Result.AppendChild(Node);
  end;

  Node := Doc.CreateElement('axes');
  Node.SetAttribute('OriginX', UTF8Decode(XMLFloat(FOrigin.X)));
  Node.SetAttribute('OriginY', UTF8Decode(XMLFloat(FOrigin.Y)));
  Node.SetAttribute('PixelSizeX', UTF8Decode(XMLFloat(FPixelSize.X)));
  Node.SetAttribute('PixelSizeY', UTF8Decode(XMLFloat(FPixelSize.Y)));
  Result.AppendChild(Node);
end;

procedure TMapScale.ImportFromXML(Node: TDOMNode);
var
  i: Integer;
  Child: TDOMNode;
begin
  Clear;
  FMode := StrToMode(GetXMLAttr(Node, 'Mode', 'None'));
  FUnits := GetXMLAttr(Node, 'Units');
  FYUp := SameText(GetXMLAttr(Node, 'YUp', 'False'), 'True');
  FImageHeight := StrToIntDef(GetXMLAttr(Node, 'ImageHeight', '0'), 0);

  i := 0;
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    if (Child.CompareName('bar') = 0) then
    begin
      FBarStart := TCurvePoint.Create(GetXMLFloat(Child, 'X1'), GetXMLFloat(Child, 'Y1'));
      FBarEnd := TCurvePoint.Create(GetXMLFloat(Child, 'X2'), GetXMLFloat(Child, 'Y2'));
      FBarLength := GetXMLFloat(Child, 'Length');
    end
    else if (Child.CompareName('reference') = 0) and (i < 3) then
    begin
      Inc(i);
      FImgPoints[i] := TCurvePoint.Create(GetXMLFloat(Child, 'ImgX'), GetXMLFloat(Child, 'ImgY'));
      FMapPoints[i] := TCurvePoint.Create(GetXMLFloat(Child, 'MapX'), GetXMLFloat(Child, 'MapY'));
    end
    else if (Child.CompareName('axes') = 0) then
    begin
      FOrigin := TCurvePoint.Create(GetXMLFloat(Child, 'OriginX'), GetXMLFloat(Child, 'OriginY'));
      FPixelSize := TCurvePoint.Create(GetXMLFloat(Child, 'PixelSizeX', 1), GetXMLFloat(Child, 'PixelSizeY', 1));
    end;
    Child := Child.NextSibling;
  end;

  Update;
  Changed;
end;

initialization
  InvariantFS := DefaultFormatSettings;
  InvariantFS.DecimalSeparator := '.';
  InvariantFS.ThousandSeparator := #0;
end.
