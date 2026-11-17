unit uParticles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Types, DOM, uCoordinates, uMapScale;

type
  TParticleParams = record
    AutoThreshold: Boolean;
    Threshold: Double;
    DarkObjects: Boolean;
    OpenRadius: Integer;
    CloseRadius: Integer;
    FillHoles: Boolean;
    MinArea: Integer;
    ExcludeEdges: Boolean;
  end;

  TParticle = record
    Id: Integer;
    AreaPx: Integer;
    PerimeterPx: Double;
    Centroid: TCurvePoint;
    CentroidMap: TCurvePoint;
    Area: Double;
    Perimeter: Double;
    EqDiameter: Double;
    FeretMax: Double;
    FeretMin: Double;
    Aspect: Double;
    Circularity: Double;
    Contour: array of TPoint;
  end;
  TParticleArray = array of TParticle;

  TParticleStats = record
    Count: Integer;
    Mean, SD, Median, MinValue, MaxValue: Double;
  end;

  TCancelQuery = function: Boolean of object;
  TProgressNotify = procedure(Percent: Integer) of object;

  // Threshold, label and measure particles in a grey-level region
  TParticleAnalysis = class(TObject)
  private
    FParams: TParticleParams;
    FParticles: TParticleArray;
    FUsedThreshold: Double;
    FRegion: TRect;
    FOnCancel: TCancelQuery;
    FOnProgress: TProgressNotify;
    function GetCount: Integer;
    function Cancelled: Boolean;
    procedure Progress(Percent: Integer);
  public
    constructor Create;

    procedure Clear;
    function Run(const Grey: array of Double; ARegion: TRect; Scale: TMapScale): Boolean;
    function DiameterStats: TParticleStats;
    procedure DiameterHistogram(Bins: Integer; out Edges, Counts: TDoubleDynArray);
    procedure SaveCSV(const FileName: String; Scale: TMapScale);

    function ExportToXML(Doc: TXMLDocument): TDOMNode;
    procedure ImportFromXML(Node: TDOMNode);

    property Params: TParticleParams read FParams write FParams;
    property Particles: TParticleArray read FParticles;
    property Count: Integer read GetCount;
    property UsedThreshold: Double read FUsedThreshold;
    property Region: TRect read FRegion write FRegion;
    property OnCancel: TCancelQuery read FOnCancel write FOnCancel;
    property OnProgress: TProgressNotify read FOnProgress write FOnProgress;
  end;

function OtsuThreshold(const Values: array of Double; Bins: Integer = 256): Double;

implementation

const
  // Moore neighbourhood, clockwise with y down: E, SE, S, SW, W, NW, N, NE
  DX: array [0..7] of Integer = (1, 1, 0, -1, -1, -1, 0, 1);
  DY: array [0..7] of Integer = (0, 1, 1, 1, 0, -1, -1, -1);

type
  TBoolGrid = array of Boolean;
  TCurvePointArray = array of TCurvePoint;

function OtsuThreshold(const Values: array of Double; Bins: Integer = 256): Double;
var
  i, k, Best: Integer;
  Lo, Hi, w0, mu, mt, sb, sbBest, Total: Double;
  Hist: array of Double;
begin
  Result := 0;
  if (Length(Values) = 0) then Exit;
  Lo := Values[0];
  Hi := Values[0];
  for i := 1 to High(Values) do
  begin
    if (Values[i] < Lo) then Lo := Values[i];
    if (Values[i] > Hi) then Hi := Values[i];
  end;
  if (Hi <= Lo) then Exit(Lo);

  SetLength(Hist, Bins);
  for i := 0 to High(Values) do
  begin
    k := Min(Bins - 1, Trunc(Bins*(Values[i] - Lo)/(Hi - Lo)));
    Hist[k] := Hist[k] + 1;
  end;

  // Maximise the between-class variance
  Total := Length(Values);
  mt := 0;
  for k := 0 to Bins - 1 do
    mt := mt + k*Hist[k]/Total;
  w0 := 0;
  mu := 0;
  sbBest := -1;
  Best := 0;
  for k := 0 to Bins - 2 do
  begin
    w0 := w0 + Hist[k]/Total;
    mu := mu + k*Hist[k]/Total;
    if (w0 <= 0) or (w0 >= 1) then Continue;
    sb := Sqr(mt*w0 - mu)/(w0*(1 - w0));
    if (sb > sbBest) then
    begin
      sbBest := sb;
      Best := k;
    end;
  end;
  Result := Lo + (Best + 1)*(Hi - Lo)/Bins;
end;

// Binary erosion or dilation with a disk of radius R
procedure Morph(var M: TBoolGrid; W, H, R: Integer; Dilate: Boolean);
var
  x, y, i, j, n, xx, yy: Integer;
  OffX, OffY: array of Integer;
  Src: TBoolGrid;
  v: Boolean;
begin
  if (R <= 0) then Exit;
  n := 0;
  SetLength(OffX, Sqr(2*R + 1));
  SetLength(OffY, Sqr(2*R + 1));
  for j := -R to R do
    for i := -R to R do
      if (i*i + j*j <= R*R) then
      begin
        OffX[n] := i;
        OffY[n] := j;
        Inc(n);
      end;

  Src := Copy(M);
  for y := 0 to H - 1 do
    for x := 0 to W - 1 do
    begin
      v := not Dilate;
      for i := 0 to n - 1 do
      begin
        xx := x + OffX[i];
        yy := y + OffY[i];
        // Pixels outside the region are ignored
        if (xx < 0) or (yy < 0) or (xx >= W) or (yy >= H) then Continue;
        if Dilate and Src[yy*W + xx] then
        begin
          v := True;
          Break;
        end;
        if (not Dilate) and (not Src[yy*W + xx]) then
        begin
          v := False;
          Break;
        end;
      end;
      M[y*W + x] := v;
    end;
end;

// Set background pixels not connected to the border as object
procedure FillHoles(var M: TBoolGrid; W, H: Integer);
var
  Outside: TBoolGrid;
  Stack: array of Integer;
  Top, p, x, y, k, q: Integer;

  procedure Push(xx, yy: Integer);
  begin
    q := yy*W + xx;
    if (not M[q]) and (not Outside[q]) then
    begin
      Outside[q] := True;
      if (Top >= Length(Stack)) then
        SetLength(Stack, 2*Length(Stack) + 1024);
      Stack[Top] := q;
      Inc(Top);
    end;
  end;

begin
  SetLength(Outside, W*H);
  SetLength(Stack, 1024);
  Top := 0;
  for x := 0 to W - 1 do
  begin
    Push(x, 0);
    Push(x, H - 1);
  end;
  for y := 0 to H - 1 do
  begin
    Push(0, y);
    Push(W - 1, y);
  end;
  // 4-connected background flood fill
  while (Top > 0) do
  begin
    Dec(Top);
    p := Stack[Top];
    x := p mod W;
    y := p div W;
    if (x > 0) then Push(x - 1, y);
    if (x < W - 1) then Push(x + 1, y);
    if (y > 0) then Push(x, y - 1);
    if (y < H - 1) then Push(x, y + 1);
  end;
  for k := 0 to W*H - 1 do
    if not Outside[k] then
      M[k] := True;
end;

function Cross(const O, A, B: TCurvePoint): Double; inline;
begin
  Result := (A.X - O.X)*(B.Y - O.Y) - (A.Y - O.Y)*(B.X - O.X);
end;

function ComparePts(const A, B: TCurvePoint): Integer;
begin
  if (A.X < B.X) then Result := -1
  else if (A.X > B.X) then Result := 1
  else if (A.Y < B.Y) then Result := -1
  else if (A.Y > B.Y) then Result := 1
  else Result := 0;
end;

procedure SortPts(var P: array of TCurvePoint; L, R: Integer);
var
  i, j: Integer;
  Pivot, T: TCurvePoint;
begin
  // Quicksort by x, then y
  while (L < R) do
  begin
    i := L;
    j := R;
    Pivot := P[(L + R) div 2];
    repeat
      while ComparePts(P[i], Pivot) < 0 do Inc(i);
      while ComparePts(P[j], Pivot) > 0 do Dec(j);
      if (i <= j) then
      begin
        T := P[i]; P[i] := P[j]; P[j] := T;
        Inc(i);
        Dec(j);
      end;
    until (i > j);
    if (L < j) then SortPts(P, L, j);
    L := i;
  end;
end;

// Monotone chain convex hull
function ConvexHull(Pts: array of TCurvePoint): TCurvePointArray;
var
  i, n, k, t: Integer;
  H: array of TCurvePoint;
begin
  n := Length(Pts);
  SetLength(Result, 0);
  if (n = 0) then Exit;
  SortPts(Pts, 0, n - 1);
  SetLength(H, 2*n);
  k := 0;
  for i := 0 to n - 1 do
  begin
    while (k >= 2) and (Cross(H[k - 2], H[k - 1], Pts[i]) <= 0) do Dec(k);
    H[k] := Pts[i];
    Inc(k);
  end;
  t := k + 1;
  for i := n - 2 downto 0 do
  begin
    while (k >= t) and (Cross(H[k - 2], H[k - 1], Pts[i]) <= 0) do Dec(k);
    H[k] := Pts[i];
    Inc(k);
  end;
  SetLength(Result, Max(k - 1, 1));
  for i := 0 to High(Result) do
    Result[i] := H[i];
end;

procedure Ferets(const Hull: TCurvePointArray; out FMax, FMin: Double);
var
  i, j, n: Integer;
  E, Nrm: TCurvePoint;
  L, d, w: Double;
begin
  FMax := 0;
  FMin := 0;
  n := Length(Hull);
  if (n < 2) then Exit;
  for i := 0 to n - 1 do
    for j := i + 1 to n - 1 do
      FMax := Max(FMax, Hull[i].DistanceTo(Hull[j]));
  if (n < 3) then Exit;
  // Minimum caliper width over the hull edges
  FMin := Infinity;
  for i := 0 to n - 1 do
  begin
    E := Hull[(i + 1) mod n] - Hull[i];
    L := Sqrt(Sqr(E.X) + Sqr(E.Y));
    if (L = 0) then Continue;
    Nrm := TCurvePoint.Create(-E.Y/L, E.X/L);
    w := 0;
    for j := 0 to n - 1 do
    begin
      d := Abs((Hull[j].X - Hull[i].X)*Nrm.X + (Hull[j].Y - Hull[i].Y)*Nrm.Y);
      if (d > w) then w := d;
    end;
    if (w < FMin) then FMin := w;
  end;
  if IsInfinite(FMin) then FMin := 0;
end;

//==========================|TParticleAnalysis|===============================//
constructor TParticleAnalysis.Create;
begin
  inherited Create;
  with FParams do
  begin
    AutoThreshold := True;
    Threshold := 128;
    DarkObjects := True;
    OpenRadius := 0;
    CloseRadius := 0;
    FillHoles := True;
    MinArea := 10;
    ExcludeEdges := True;
  end;
  FRegion := TRect.Create(0, 0, 0, 0);
end;

procedure TParticleAnalysis.Clear;
begin
  SetLength(FParticles, 0);
end;

function TParticleAnalysis.GetCount: Integer;
begin
  Result := Length(FParticles);
end;

function TParticleAnalysis.Cancelled: Boolean;
begin
  Result := Assigned(FOnCancel) and FOnCancel();
end;

procedure TParticleAnalysis.Progress(Percent: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Percent);
end;

function TParticleAnalysis.Run(const Grey: array of Double; ARegion: TRect; Scale: TMapScale): Boolean;
var
  W, H, x, y, i, k, n, p, q, Top, Lbl, Seed, nx, ny, d, Dir, StartDir: Integer;
  ne, no, nc: Integer;
  Mask: TBoolGrid;
  Labels: array of Integer;
  Stack: array of Integer;
  Part: TParticle;
  Edge, Done, Found: Boolean;
  SumX, SumY, LinScale: Double;
  Codes: array of Integer;
  Corners: array of TCurvePoint;
  Hull: TCurvePointArray;
  Cur: TPoint;
begin
  Result := False;
  Clear;
  FRegion := ARegion;
  W := ARegion.Width;
  H := ARegion.Height;
  if (W <= 0) or (H <= 0) or (Length(Grey) < W*H) then Exit;

  // Threshold
  if FParams.AutoThreshold then
    FUsedThreshold := OtsuThreshold(Grey)
  else
    FUsedThreshold := FParams.Threshold;

  SetLength(Mask, W*H);
  for k := 0 to W*H - 1 do
    if FParams.DarkObjects then
      Mask[k] := Grey[k] <= FUsedThreshold
    else
      Mask[k] := Grey[k] > FUsedThreshold;
  Progress(10);

  // Opening, closing and hole filling
  if (FParams.OpenRadius > 0) then
  begin
    Morph(Mask, W, H, FParams.OpenRadius, False);
    Morph(Mask, W, H, FParams.OpenRadius, True);
  end;
  if (FParams.CloseRadius > 0) then
  begin
    Morph(Mask, W, H, FParams.CloseRadius, True);
    Morph(Mask, W, H, FParams.CloseRadius, False);
  end;
  if FParams.FillHoles then
    FillHoles(Mask, W, H);
  Progress(30);
  if Cancelled then Exit;

  SetLength(Labels, W*H);
  SetLength(Stack, 1024);
  LinScale := Sqrt(Scale.PixelArea);
  Lbl := 0;
  n := 0;

  for Seed := 0 to W*H - 1 do
  begin
    if (Seed mod (W*16) = 0) then
    begin
      Progress(30 + Round(70*Seed/(W*H)));
      if Cancelled then
      begin
        Clear;
        Exit;
      end;
    end;

    if (not Mask[Seed]) or (Labels[Seed] <> 0) then Continue;

    // 8-connected flood fill from the topmost-leftmost pixel
    Inc(Lbl);
    Labels[Seed] := Lbl;
    Top := 1;
    Stack[0] := Seed;
    Part := Default(TParticle);
    SumX := 0;
    SumY := 0;
    Edge := False;
    while (Top > 0) do
    begin
      Dec(Top);
      p := Stack[Top];
      x := p mod W;
      y := p div W;
      Inc(Part.AreaPx);
      SumX := SumX + x;
      SumY := SumY + y;
      if (x = 0) or (y = 0) or (x = W - 1) or (y = H - 1) then
        Edge := True;
      for d := 0 to 7 do
      begin
        nx := x + DX[d];
        ny := y + DY[d];
        if (nx < 0) or (ny < 0) or (nx >= W) or (ny >= H) then Continue;
        q := ny*W + nx;
        if Mask[q] and (Labels[q] = 0) then
        begin
          Labels[q] := Lbl;
          if (Top >= Length(Stack)) then
            SetLength(Stack, 2*Length(Stack));
          Stack[Top] := q;
          Inc(Top);
        end;
      end;
    end;

    if (Part.AreaPx < FParams.MinArea) or (FParams.ExcludeEdges and Edge) then
      Continue;

    // Moore-neighbour tracing of the outer contour (Jacob's stopping rule)
    SetLength(Part.Contour, 1);
    SetLength(Codes, 0);
    Cur := Point(Seed mod W, Seed div W);
    Part.Contour[0] := Cur;
    Dir := 4;
    StartDir := -1;
    Done := False;
    while not Done do
    begin
      Found := False;
      for i := 0 to 7 do
      begin
        d := (Dir + 1 + i) mod 8;
        nx := Cur.X + DX[d];
        ny := Cur.Y + DY[d];
        if (nx >= 0) and (ny >= 0) and (nx < W) and (ny < H) and
           (Labels[ny*W + nx] = Lbl) then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found then Break;
      if (StartDir < 0) then
        StartDir := d
      else if (Cur.X = Part.Contour[0].X) and (Cur.Y = Part.Contour[0].Y) and (d = StartDir) then
      begin
        SetLength(Part.Contour, Length(Part.Contour) - 1);
        Break;
      end;
      SetLength(Codes, Length(Codes) + 1);
      Codes[High(Codes)] := d;
      Cur := Point(nx, ny);
      SetLength(Part.Contour, Length(Part.Contour) + 1);
      Part.Contour[High(Part.Contour)] := Cur;
      Dir := (d + 4) mod 8;
    end;

    // Vossepoel-Smeulders perimeter from the chain code
    ne := 0; no := 0; nc := 0;
    for i := 0 to High(Codes) do
    begin
      if (Codes[i] mod 2 = 0) then Inc(ne) else Inc(no);
      if (Codes[i] <> Codes[(i + 1) mod Length(Codes)]) then Inc(nc);
    end;
    Part.PerimeterPx := 0.980*ne + 1.406*no - 0.091*nc;

    // Contour back to image coordinates
    for i := 0 to High(Part.Contour) do
      Part.Contour[i] := Point(Part.Contour[i].X + ARegion.Left, Part.Contour[i].Y + ARegion.Top);

    // Feret diameters from the pixel corners, in map coordinates
    SetLength(Corners, 4*Length(Part.Contour));
    for i := 0 to High(Part.Contour) do
    begin
      Corners[4*i] := Scale.ImgToMap(TCurvePoint.Create(Part.Contour[i].X - 0.5, Part.Contour[i].Y - 0.5));
      Corners[4*i + 1] := Scale.ImgToMap(TCurvePoint.Create(Part.Contour[i].X + 0.5, Part.Contour[i].Y - 0.5));
      Corners[4*i + 2] := Scale.ImgToMap(TCurvePoint.Create(Part.Contour[i].X - 0.5, Part.Contour[i].Y + 0.5));
      Corners[4*i + 3] := Scale.ImgToMap(TCurvePoint.Create(Part.Contour[i].X + 0.5, Part.Contour[i].Y + 0.5));
    end;
    Hull := ConvexHull(Corners);
    Ferets(Hull, Part.FeretMax, Part.FeretMin);

    Inc(n);
    Part.Id := n;
    Part.Centroid := TCurvePoint.Create(SumX/Part.AreaPx + ARegion.Left, SumY/Part.AreaPx + ARegion.Top);
    Part.CentroidMap := Scale.ImgToMap(Part.Centroid);
    Part.Area := Part.AreaPx*Scale.PixelArea;
    Part.Perimeter := Part.PerimeterPx*LinScale;
    Part.EqDiameter := 2*Sqrt(Part.Area/Pi);
    if (Part.FeretMin > 0) then
      Part.Aspect := Part.FeretMax/Part.FeretMin;
    if (Part.PerimeterPx > 0) then
      Part.Circularity := 4*Pi*Part.AreaPx/Sqr(Part.PerimeterPx);

    SetLength(FParticles, n);
    FParticles[n - 1] := Part;
  end;

  Progress(100);
  Result := True;
end;

function TParticleAnalysis.DiameterStats: TParticleStats;
var
  i, j: Integer;
  v: TDoubleDynArray;
  s, s2: Double;
begin
  Result := Default(TParticleStats);
  Result.Count := Count;
  if (Count = 0) then Exit;
  SetLength(v, Count);
  s := 0;
  s2 := 0;
  for i := 0 to Count - 1 do
  begin
    v[i] := FParticles[i].EqDiameter;
    s := s + v[i];
    s2 := s2 + Sqr(v[i]);
  end;
  Result.Mean := s/Count;
  if (Count > 1) then
    Result.SD := Sqrt(Max(0, (s2 - Count*Sqr(Result.Mean))/(Count - 1)));
  // Insertion sort is enough for the median of a few thousand values
  for i := 1 to Count - 1 do
  begin
    s := v[i];
    j := i - 1;
    while (j >= 0) and (v[j] > s) do
    begin
      v[j + 1] := v[j];
      Dec(j);
    end;
    v[j + 1] := s;
  end;
  Result.MinValue := v[0];
  Result.MaxValue := v[Count - 1];
  if (Count mod 2 = 1) then
    Result.Median := v[Count div 2]
  else
    Result.Median := (v[Count div 2 - 1] + v[Count div 2])/2;
end;

procedure TParticleAnalysis.DiameterHistogram(Bins: Integer; out Edges, Counts: TDoubleDynArray);
var
  i, k: Integer;
  St: TParticleStats;
  Width: Double;
begin
  Bins := Max(Bins, 1);
  SetLength(Edges, Bins + 1);
  SetLength(Counts, Bins);
  St := DiameterStats;
  if (St.Count = 0) then Exit;
  Width := (St.MaxValue - St.MinValue)/Bins;
  if (Width <= 0) then Width := 1;
  for i := 0 to Bins do
    Edges[i] := St.MinValue + i*Width;
  for i := 0 to Count - 1 do
  begin
    k := Min(Bins - 1, Trunc((FParticles[i].EqDiameter - St.MinValue)/Width));
    Counts[k] := Counts[k] + 1;
  end;
end;

procedure TParticleAnalysis.SaveCSV(const FileName: String; Scale: TMapScale);
var
  i: Integer;
  U: String;
  Lines: TStringList;
begin
  U := Scale.Units;
  Lines := TStringList.Create;
  try
    Lines.Add(Format('id,x_%s,y_%s,area_px,area_%s,eq_diameter_%s,perimeter_%s,' +
      'feret_max_%s,feret_min_%s,aspect,circularity', [U, U, Scale.AreaUnits, U, U, U, U]));
    for i := 0 to Count - 1 do
      with FParticles[i] do
        Lines.Add(Format('%d,%s,%s,%d,%s,%s,%s,%s,%s,%s,%s',
          [Id, XMLFloat(CentroidMap.X), XMLFloat(CentroidMap.Y), AreaPx, XMLFloat(Area),
           XMLFloat(EqDiameter), XMLFloat(Perimeter), XMLFloat(FeretMax),
           XMLFloat(FeretMin), XMLFloat(Aspect), XMLFloat(Circularity)]));
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

function TParticleAnalysis.ExportToXML(Doc: TXMLDocument): TDOMNode;
begin
  Result := Doc.CreateElement('particles');
  with TDOMElement(Result) do
  begin
    SetAttribute('AutoThreshold', UTF8Decode(BoolToStr(FParams.AutoThreshold, True)));
    SetAttribute('Threshold', UTF8Decode(XMLFloat(FParams.Threshold)));
    SetAttribute('DarkObjects', UTF8Decode(BoolToStr(FParams.DarkObjects, True)));
    SetAttribute('OpenRadius', UTF8Decode(IntToStr(FParams.OpenRadius)));
    SetAttribute('CloseRadius', UTF8Decode(IntToStr(FParams.CloseRadius)));
    SetAttribute('FillHoles', UTF8Decode(BoolToStr(FParams.FillHoles, True)));
    SetAttribute('MinArea', UTF8Decode(IntToStr(FParams.MinArea)));
    SetAttribute('ExcludeEdges', UTF8Decode(BoolToStr(FParams.ExcludeEdges, True)));
    SetAttribute('Left', UTF8Decode(IntToStr(FRegion.Left)));
    SetAttribute('Top', UTF8Decode(IntToStr(FRegion.Top)));
    SetAttribute('Right', UTF8Decode(IntToStr(FRegion.Right)));
    SetAttribute('Bottom', UTF8Decode(IntToStr(FRegion.Bottom)));
  end;
end;

procedure TParticleAnalysis.ImportFromXML(Node: TDOMNode);

  function ReadBool(const AName: String; Def: Boolean): Boolean;
  begin
    Result := SameText(GetXMLAttr(Node, AName, BoolToStr(Def, True)), 'True');
  end;

begin
  Clear;
  FParams.AutoThreshold := ReadBool('AutoThreshold', True);
  FParams.Threshold := GetXMLFloat(Node, 'Threshold', 128);
  FParams.DarkObjects := ReadBool('DarkObjects', True);
  FParams.OpenRadius := StrToIntDef(GetXMLAttr(Node, 'OpenRadius'), 0);
  FParams.CloseRadius := StrToIntDef(GetXMLAttr(Node, 'CloseRadius'), 0);
  FParams.FillHoles := ReadBool('FillHoles', True);
  FParams.MinArea := StrToIntDef(GetXMLAttr(Node, 'MinArea'), 10);
  FParams.ExcludeEdges := ReadBool('ExcludeEdges', True);
  FRegion := TRect.Create(StrToIntDef(GetXMLAttr(Node, 'Left'), 0),
                          StrToIntDef(GetXMLAttr(Node, 'Top'), 0),
                          StrToIntDef(GetXMLAttr(Node, 'Right'), 0),
                          StrToIntDef(GetXMLAttr(Node, 'Bottom'), 0));
end;

end.
