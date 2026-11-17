unit uMapFeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Fgl, DOM, Graphics, BGRABitmap, BGRABitmapTypes,
  uCoordinates, uMapScale;

type
  TFeatureKind = (fkPoint, fkPolyline, fkPolygon, fkDistance, fkAngle, fkProfile);

  // Named point set in image pixels; measured through a TMapScale
  TMapFeature = class(TObject)
  private
    FKind: TFeatureKind;
    FName: String;
    FColor: TColor;
    FPoints: array of TCurvePoint;
    function GetCount: Integer;
    function GetPoint(Index: Integer): TCurvePoint;
    procedure SetPoint(Index: Integer; const Value: TCurvePoint);
  public
    constructor Create(AKind: TFeatureKind; const AName: String; AColor: TColor);

    procedure Assign(Source: TMapFeature);
    procedure AddPoint(const P: TCurvePoint);
    procedure DeletePoint(Index: Integer);
    procedure Clear;
    procedure ScalePoints(Factor: Double);

    function MinPoints: Integer;
    function MaxPoints: Integer;
    function IsComplete: Boolean;

    function Length(Scale: TMapScale): Double;
    function Area(Scale: TMapScale): Double;
    function Angle(Scale: TMapScale): Double;
    function Centroid(Scale: TMapScale): TCurvePoint;
    function Summary(Scale: TMapScale): String;

    function HitTest(const P: TCurvePoint; Tolerance: Double): Boolean;
    function NearestVertex(const P: TCurvePoint; Tolerance: Double): Integer;
    procedure Draw(Bmp: TBGRABitmap; Zoom: Double; Scale: TMapScale; Selected: Boolean);

    function ExportToXML(Doc: TXMLDocument): TDOMNode;
    procedure ImportFromXML(Node: TDOMNode);

    property Kind: TFeatureKind read FKind write FKind;
    property Name: String read FName write FName;
    property Color: TColor read FColor write FColor;
    property Count: Integer read GetCount;
    property Point[Index: Integer]: TCurvePoint read GetPoint write SetPoint; default;
  end;

  TMapFeatureListBase = specialize TFPGObjectList<TMapFeature>;

  TMapFeatureList = class(TMapFeatureListBase)
  public
    procedure Assign(Source: TMapFeatureList);
    function NextName(AKind: TFeatureKind): String;
    function FeatureAt(const P: TCurvePoint; Tolerance: Double): Integer;

    function ExportToXML(Doc: TXMLDocument): TDOMNode;
    procedure ImportFromXML(Node: TDOMNode);

    procedure SaveVerticesCSV(const FileName: String; Scale: TMapScale);
    procedure SaveSummaryCSV(const FileName: String; Scale: TMapScale);
  end;

function KindToStr(Value: TFeatureKind): String;
function StrToKind(const Value: String): TFeatureKind;
function SegmentDistance(const P, A, B: TCurvePoint): Double;

implementation

const
  KindNames: array [TFeatureKind] of String =
    ('Point', 'Polyline', 'Polygon', 'Distance', 'Angle', 'Profile');

function KindToStr(Value: TFeatureKind): String;
begin
  Result := KindNames[Value];
end;

function StrToKind(const Value: String): TFeatureKind;
var
  k: TFeatureKind;
begin
  Result := fkPoint;
  for k := Low(TFeatureKind) to High(TFeatureKind) do
    if SameText(Value, KindNames[k]) then
      Exit(k);
end;

function SegmentDistance(const P, A, B: TCurvePoint): Double;
var
  t, L2: Double;
  D: TCurvePoint;
begin
  D := B - A;
  L2 := Sqr(D.X) + Sqr(D.Y);
  if (L2 = 0) then
    Exit(P.DistanceTo(A));
  t := EnsureRange(((P.X - A.X)*D.X + (P.Y - A.Y)*D.Y)/L2, 0, 1);
  Result := P.DistanceTo(A + t*D);
end;

//============================|TMapFeature|===================================//
constructor TMapFeature.Create(AKind: TFeatureKind; const AName: String; AColor: TColor);
begin
  inherited Create;
  FKind := AKind;
  FName := AName;
  FColor := AColor;
  SetLength(FPoints, 0);
end;

procedure TMapFeature.Assign(Source: TMapFeature);
begin
  FKind := Source.FKind;
  FName := Source.FName;
  FColor := Source.FColor;
  FPoints := Copy(Source.FPoints);
end;

function TMapFeature.GetCount: Integer;
begin
  Result := System.Length(FPoints);
end;

function TMapFeature.GetPoint(Index: Integer): TCurvePoint;
begin
  Result := FPoints[Index];
end;

procedure TMapFeature.SetPoint(Index: Integer; const Value: TCurvePoint);
begin
  FPoints[Index] := Value;
end;

procedure TMapFeature.AddPoint(const P: TCurvePoint);
begin
  SetLength(FPoints, Count + 1);
  FPoints[Count - 1] := P;
end;

procedure TMapFeature.DeletePoint(Index: Integer);
begin
  if (Index >= 0) and (Index < Count) then
    Delete(FPoints, Index, 1);
end;

procedure TMapFeature.Clear;
begin
  SetLength(FPoints, 0);
end;

procedure TMapFeature.ScalePoints(Factor: Double);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    FPoints[i] := Factor*FPoints[i];
end;

function TMapFeature.MinPoints: Integer;
begin
  case FKind of
    fkPoint: Result := 1;
    fkAngle, fkPolygon: Result := 3;
    else Result := 2;
  end;
end;

function TMapFeature.MaxPoints: Integer;
begin
  // Zero means open-ended
  case FKind of
    fkPoint: Result := 1;
    fkDistance: Result := 2;
    fkAngle: Result := 3;
    else Result := 0;
  end;
end;

function TMapFeature.IsComplete: Boolean;
begin
  Result := (Count >= MinPoints) and ((MaxPoints = 0) or (Count <= MaxPoints));
end;

function TMapFeature.Length(Scale: TMapScale): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count - 1 do
    Result := Result + Scale.Distance(FPoints[i - 1], FPoints[i]);
  // Perimeter closes the polygon
  if (FKind = fkPolygon) and (Count > 2) then
    Result := Result + Scale.Distance(FPoints[Count - 1], FPoints[0]);
end;

function TMapFeature.Area(Scale: TMapScale): Double;
var
  i, j: Integer;
  Pi_, Pj: TCurvePoint;
begin
  Result := 0;
  if (FKind <> fkPolygon) or (Count < 3) then Exit;
  // Shoelace formula in map coordinates
  for i := 0 to Count - 1 do
  begin
    j := (i + 1) mod Count;
    Pi_ := Scale.ImgToMap(FPoints[i]);
    Pj := Scale.ImgToMap(FPoints[j]);
    Result := Result + Pi_.X*Pj.Y - Pj.X*Pi_.Y;
  end;
  Result := Abs(Result)/2;
end;

function TMapFeature.Angle(Scale: TMapScale): Double;
var
  A, B, C, U, V: TCurvePoint;
begin
  Result := 0;
  if (Count < 3) then Exit;
  // Angle at the second point, measured in map coordinates
  A := Scale.ImgToMap(FPoints[0]);
  B := Scale.ImgToMap(FPoints[1]);
  C := Scale.ImgToMap(FPoints[2]);
  U := A - B;
  V := C - B;
  Result := RadToDeg(Abs(ArcTan2(U.X*V.Y - U.Y*V.X, U.X*V.X + U.Y*V.Y)));
end;

function TMapFeature.Centroid(Scale: TMapScale): TCurvePoint;
var
  i: Integer;
begin
  Result := TCurvePoint.Create(0, 0);
  if (Count = 0) then Exit;
  for i := 0 to Count - 1 do
    Result := Result + Scale.ImgToMap(FPoints[i]);
  Result := Result/Count;
end;

function TMapFeature.Summary(Scale: TMapScale): String;
var
  P: TCurvePoint;
begin
  case FKind of
    fkPoint: begin
      P := Scale.ImgToMap(FPoints[0]);
      Result := Format('(%.4g, %.4g) %s', [P.X, P.Y, Scale.Units]);
    end;
    fkAngle: Result := Format('%.2f°', [Angle(Scale)]);
    fkPolygon: Result := Format('A = %.4g %s, P = %.4g %s',
                                [Area(Scale), Scale.AreaUnits, Length(Scale), Scale.Units]);
    else Result := Format('%.4g %s', [Length(Scale), Scale.Units]);
  end;
end;

function TMapFeature.HitTest(const P: TCurvePoint; Tolerance: Double): Boolean;
var
  i: Integer;
begin
  Result := NearestVertex(P, Tolerance) >= 0;
  if Result then Exit;
  for i := 1 to Count - 1 do
    if SegmentDistance(P, FPoints[i - 1], FPoints[i]) <= Tolerance then
      Exit(True);
  if (FKind = fkPolygon) and (Count > 2) then
    Result := SegmentDistance(P, FPoints[Count - 1], FPoints[0]) <= Tolerance;
end;

function TMapFeature.NearestVertex(const P: TCurvePoint; Tolerance: Double): Integer;
var
  i: Integer;
  d, Best: Double;
begin
  Result := -1;
  Best := Tolerance;
  for i := 0 to Count - 1 do
  begin
    d := P.DistanceTo(FPoints[i]);
    if (d <= Best) then
    begin
      Best := d;
      Result := i;
    end;
  end;
end;

procedure TMapFeature.Draw(Bmp: TBGRABitmap; Zoom: Double; Scale: TMapScale; Selected: Boolean);
var
  i: Integer;
  Pts: array of TPointF;
  c, Fill: TBGRAPixel;
  w: Single;
  Lbl: TCurvePoint;
begin
  if (Count = 0) then Exit;

  SetLength(Pts, Count);
  for i := 0 to Count - 1 do
    Pts[i] := PointF(Zoom*FPoints[i].X, Zoom*FPoints[i].Y);

  c := ColorToBGRA(FColor);
  if Selected then w := 3 else w := 2;

  case FKind of
    fkPoint: begin
      Bmp.DrawLineAntialias(Pts[0].X - 6, Pts[0].Y, Pts[0].X + 6, Pts[0].Y, c, w);
      Bmp.DrawLineAntialias(Pts[0].X, Pts[0].Y - 6, Pts[0].X, Pts[0].Y + 6, c, w);
    end;
    fkPolygon: begin
      Fill := c;
      Fill.alpha := 60;
      if (Count > 2) then
        Bmp.DrawPolygonAntialias(Pts, c, w, Fill)
      else
        Bmp.DrawPolyLineAntialias(Pts, c, w);
    end;
    fkProfile: begin
      Bmp.PenStyle := psDash;
      Bmp.DrawPolyLineAntialias(Pts, c, w);
      Bmp.PenStyle := psSolid;
    end;
    else
      Bmp.DrawPolyLineAntialias(Pts, c, w);
  end;

  // Vertex dots
  for i := 0 to Count - 1 do
    Bmp.FillEllipseAntialias(Pts[i].X, Pts[i].Y, 2.5, 2.5, c);

  // Label with the measured value
  if IsComplete and (FKind <> fkPolyline) then
  begin
    if (FKind = fkAngle) then
      Lbl := Zoom*FPoints[1]
    else
      Lbl := Zoom*FPoints[Count - 1];
    Bmp.FontHeight := 13;
    Bmp.TextOut(Lbl.X + 6, Lbl.Y + 4, FName + ': ' + Summary(Scale), c);
  end;
end;

function TMapFeature.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  Node: TDOMElement;
begin
  Result := Doc.CreateElement('feature');
  with TDOMElement(Result) do
  begin
    SetAttribute('Kind', UTF8Decode(KindToStr(FKind)));
    SetAttribute('Name', UTF8Decode(FName));
    SetAttribute('Color', UTF8Decode(IntToStr(FColor)));
  end;
  for i := 0 to Count - 1 do
  begin
    Node := Doc.CreateElement('point');
    Node.SetAttribute('X', UTF8Decode(XMLFloat(FPoints[i].X)));
    Node.SetAttribute('Y', UTF8Decode(XMLFloat(FPoints[i].Y)));
    Result.AppendChild(Node);
  end;
end;

procedure TMapFeature.ImportFromXML(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  FKind := StrToKind(GetXMLAttr(Node, 'Kind', 'Point'));
  FName := GetXMLAttr(Node, 'Name');
  FColor := StrToIntDef(GetXMLAttr(Node, 'Color', '255'), clRed);
  Clear;
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    if (Child.CompareName('point') = 0) then
      AddPoint(TCurvePoint.Create(GetXMLFloat(Child, 'X'), GetXMLFloat(Child, 'Y')));
    Child := Child.NextSibling;
  end;
end;

//==========================|TMapFeatureList|=================================//
procedure TMapFeatureList.Assign(Source: TMapFeatureList);
var
  i: Integer;
  F: TMapFeature;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    F := TMapFeature.Create(fkPoint, '', clRed);
    F.Assign(Source[i]);
    Add(F);
  end;
end;

function TMapFeatureList.NextName(AKind: TFeatureKind): String;
var
  i, n: Integer;
begin
  n := 1;
  for i := 0 to Count - 1 do
    if (Items[i].Kind = AKind) then
      Inc(n);
  Result := KindToStr(AKind) + ' ' + IntToStr(n);
end;

function TMapFeatureList.FeatureAt(const P: TCurvePoint; Tolerance: Double): Integer;
var
  i: Integer;
begin
  // Topmost (last drawn) first
  for i := Count - 1 downto 0 do
    if Items[i].HitTest(P, Tolerance) then
      Exit(i);
  Result := -1;
end;

function TMapFeatureList.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
begin
  Result := Doc.CreateElement('features');
  TDOMElement(Result).SetAttribute('Count', UTF8Decode(IntToStr(Count)));
  for i := 0 to Count - 1 do
    Result.AppendChild(Items[i].ExportToXML(Doc));
end;

procedure TMapFeatureList.ImportFromXML(Node: TDOMNode);
var
  Child: TDOMNode;
  F: TMapFeature;
begin
  Clear;
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    if (Child.CompareName('feature') = 0) then
    begin
      F := TMapFeature.Create(fkPoint, '', clRed);
      F.ImportFromXML(Child);
      Add(F);
    end;
    Child := Child.NextSibling;
  end;
end;

procedure TMapFeatureList.SaveVerticesCSV(const FileName: String; Scale: TMapScale);
var
  i, j: Integer;
  P: TCurvePoint;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(Format('name,kind,vertex,x_px,y_px,x_%s,y_%s', [Scale.Units, Scale.Units]));
    for i := 0 to Count - 1 do
      for j := 0 to Items[i].Count - 1 do
      begin
        P := Scale.ImgToMap(Items[i][j]);
        Lines.Add(Format('"%s",%s,%d,%s,%s,%s,%s',
          [Items[i].Name, KindToStr(Items[i].Kind), j,
           XMLFloat(Items[i][j].X), XMLFloat(Items[i][j].Y), XMLFloat(P.X), XMLFloat(P.Y)]));
      end;
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TMapFeatureList.SaveSummaryCSV(const FileName: String; Scale: TMapScale);
var
  i: Integer;
  F: TMapFeature;
  C: TCurvePoint;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(Format('name,kind,points,length_%s,area_%s,angle_deg,cx_%s,cy_%s',
      [Scale.Units, Scale.AreaUnits, Scale.Units, Scale.Units]));
    for i := 0 to Count - 1 do
    begin
      F := Items[i];
      C := F.Centroid(Scale);
      Lines.Add(Format('"%s",%s,%d,%s,%s,%s,%s,%s',
        [F.Name, KindToStr(F.Kind), F.Count, XMLFloat(F.Length(Scale)),
         XMLFloat(F.Area(Scale)), XMLFloat(F.Angle(Scale)), XMLFloat(C.X), XMLFloat(C.Y)]));
    end;
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

end.
