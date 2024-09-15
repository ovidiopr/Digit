unit ucoordinates;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, DOM, Types;

type
  TCoordSystem = (csCartesian, csPolar);
  TScaleType = (stLinear, stLog, stLn, stInverse);

  TCurvePoint = record
    X: Double;
    Y: Double;

    constructor Create(Xi, Yi: Double);

    class operator = (a, b: TCurvePoint): Boolean;
    class operator + (a, b: TCurvePoint): TCurvePoint;
    class operator - (a: TCurvePoint): TCurvePoint;
    class operator - (a, b: TCurvePoint): TCurvePoint;
    class operator * (a: Double; b: TCurvePoint): TCurvePoint;
    class operator / (a: TCurvePoint; b: Double): TCurvePoint;
    class operator := (a: TPoint) b: TCurvePoint;
    class operator := (a: TCurvePoint) b: TPoint;

    function DistanceTo(a: TCurvePoint): Double;
    function ToStr: String; overload;
    function ToStr(Fmt: String = '%.5g'): String; overload;
  end;
  TBasis = Array [1..2] of TCurvePoint;
  TBasisChangeMtrx = Array [1..2, 1..2] of Double;
  TScalePoints = Array [1..3] of TCurvePoint;

  ArrayOfTPointF = Array of TPointF;

  TPolygon = class(TObject)
  private
    { Private declarations }
    FVertices: Array of TCurvePoint;
    FSlopes: Array of Double;
    FHoriLines: Array of Boolean;
    FRotated: Boolean;
    FSine: Double;
    FCosine: Double;

    FOnChange: TNotifyEvent;

    function GetNumvertices: Integer;
    function GetVertex(Index: Integer): TCurvePoint;
    function GetEdge(Index: Integer): TCurvePoint;
    function GetCenter: TCurvePoint;
    function GetPolygonPoints(Zoom: Double): ArrayOfTPointF;
    function GetDrawPoints(Zoom: Double): ArrayOfTPointF; virtual;
    function GetRect(Zoom: Double): TRect;

    procedure SetNumVertices(const Value: Integer);
    procedure SetVertex(Index: Integer; const Value: TCurvePoint); virtual;

    procedure RecalcSlopes;
    function CalcVertexPos(Pn, Pa: TCurvePoint; Index, Idx: Integer): TCurvePoint;
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create(NumVert: Integer = 4);
    {@exclude}
    destructor Destroy; override;

    procedure Reset;
    procedure MoveVertex(Index: Integer; Pn: TCurvePoint); virtual;
    procedure MoveEdge(Index: Integer; Pn: TCurvePoint); virtual;
    procedure Rotate(Index: Integer; Pn: TCurvePoint); virtual;
    procedure ApplyRotation;
    procedure CancelRotation;

    function NextVertIdx(Index: Integer): Integer;
    function PrevVertIdx(Index: Integer): Integer;

    function Contains(p: TCurvePoint): Boolean; virtual;
    function IsConvex: Boolean; virtual;
    function IsCW: Boolean; virtual;
    function IsCCW: Boolean; virtual;

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    property NumVertices: Integer read GetNumvertices write SetNumVertices;
    property NumEdges: Integer read GetNumvertices; // It's the same number
    property Vertex[Index: Integer]: TCurvePoint read GetVertex write SetVertex; default;
    property Edge[Index: Integer]: TCurvePoint read GetEdge;
    property Center: TCurvePoint read GetCenter;
    property PolygonPoints[Zoom: Double]: ArrayOfTPointF read GetPolygonPoints;
    property DrawPoints[Zoom: Double]: ArrayOfTPointF read GetDrawPoints;
    property Rect[Zoom: Double]: TRect read GetRect;
    property Rotated: Boolean read FRotated;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPlotQuad = class(TPolygon)
  private
    { Private declarations }
    FPolarCoordinates: Boolean;
    FRecalculateDirect: Boolean;
    FRecalculateInverse: Boolean;

    FDMat: Array of Array of Double;
    FIMat: Array of Array of Double;

    function GetDrawPoints(Zoom: Double): ArrayOfTPointF; override;

    procedure SetVertex(Index: Integer; const Value: TCurvePoint); override;
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create;
    {@exclude}
    destructor Destroy; override;

    procedure MoveVertex(Index: Integer; Pn: TCurvePoint); override;
    procedure MoveEdge(Index: Integer; Pn: TCurvePoint); override;
    procedure Rotate(Index: Integer; Pn: TCurvePoint); override;

    function Contains(p: TCurvePoint): Boolean; override;

    property PolarCoordinates: Boolean read FPolarCoordinates write FPolarCoordinates;
  end;

const
  cp: TScalePoints = ((X: 0; Y: 1),
                      (X: 0; Y: 0),
                      (X: 1; Y: 0));
  cb: TBasis = ((X: 1; Y: 0),
                (X: 0; Y: 1));

  function TCurvePointComparator(const a, b: TCurvePoint): Integer;
  function GetCurvePoint(X, Y: Double): TCurvePoint;
  function Modulus(v: TCurvePoint): Double;
  function Normalize(v: TCurvePoint): TCurvePoint;
  function VectorsAreParallel(p1, p2: TCurvePoint): Boolean;
  function PlotBox(p: TScalePoints; CoordSystem: TCoordSystem): TRect;
  function BoxHeight(p: TScalePoints): Double;
  function BoxWidth(p: TScalePoints): Double;
  function PointsToBase(p: TScalePoints): TBasis;
  function SolveLinearSystem(b: TBasis; c: TCurvePoint): TCurvePoint;
  function TransfMatrix(b1, b2: TBasis): TBasisChangeMtrx;
  function ChangeBasis(b1, b2: TBasis; p: TCurvePoint): TCurvePoint;
  function ChangeCoords(OldPoints, NewPoints: TScalePoints; p: TCurvePoint): TCurvePoint;
  function CartesianToPolar(p: TCurvePoint): TCurvePoint; overload;
  function CartesianToPolar(X, Y: Double): TCurvePoint; overload;
  function PolarToCartesian(p: TCurvePoint): TCurvePoint; overload;
  function PolarToCartesian(X, Y: Double): TCurvePoint; overload;

implementation

constructor TCurvePoint.Create(Xi, Yi: Double);
begin
  X := Xi;
  Y := Yi;
end;

class operator TCurvePoint.= (a, b: TCurvePoint): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

class operator TCurvePoint.+ (a, b: TCurvePoint): TCurvePoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

class operator TCurvePoint.- (a: TCurvePoint): TCurvePoint;
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
end;

class operator TCurvePoint.- (a, b: TCurvePoint): TCurvePoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

class operator TCurvePoint.* (a: Double; b: TCurvePoint): TCurvePoint;
begin
  Result.X := a*b.X;
  Result.Y := a*b.Y;
end;

class operator TCurvePoint./ (a: TCurvePoint; b: Double): TCurvePoint;
begin
  Result.X := a.X/b;
  Result.Y := a.Y/b;
end;

class operator TCurvePoint.:= (a: TPoint) b: TCurvePoint;
begin
  b.X := a.X;
  b.Y := a.Y;
end;

class operator TCurvePoint.:= (a: TCurvePoint) b: TPoint;
begin
  b := TPoint.Create(Round(a.X), Round(a.Y));
end;

function TCurvePoint.DistanceTo(a: TCurvePoint): Double;
begin
  Result := Sqrt(Power(X - a.X, 2) + Power(Y - a.Y, 2))
end;

function TCurvePoint.ToStr: String;
begin
  Result := Format('X = %.5g, Y = %.5g', [X, Y]);
end;

function TCurvePoint.ToStr(Fmt: String = '%.5g'): String;
begin
  Result := Format(Fmt + ', ' + Fmt, [X, Y]);
end;

function TCurvePointComparator(const a, b: TCurvePoint): Integer;
begin
  Result := Sign(a.X - b.X);
end;


//==============================| TPolygon |===============================//

constructor TPolygon.Create(NumVert: Integer = 4);
begin
  inherited Create;

  NumVertices := NumVert;

  Reset;
end;

destructor TPolygon.Destroy;
begin
  SetLength(FVertices, 0);
  SetLength(FSlopes, 0);
  SetLength(FHoriLines, 0);

  inherited Destroy;
end;

procedure TPolygon.Reset;
var
  i: Integer;
begin
  FRotated := False;
  FSine := 0.0;
  FCosine := 1.0;

  for i := 0 to NumVertices - 1 do
    FVertices[i] := TCurvePoint.Create(-1, -1);
end;

procedure TPolygon.MoveVertex(Index: Integer; Pn: TCurvePoint);
var
  Idx1, Idx2: Integer;
begin
  if (Index >= 0) and (Index < NumVertices) then
  begin
    Idx1 := NextVertIdx(Index);
    Idx2 := NextVertIdx(Idx1);
    FVertices[Idx1] := CalcVertexPos(Pn, Vertex[Idx2], Index, Idx1);

    Idx1 := PrevVertIdx(Index);
    Idx2 := PrevVertIdx(Idx1);
    FVertices[Idx1] := CalcVertexPos(Pn, Vertex[Idx2], Idx1, Idx2);

    // Notify the parent that the Polygon has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPolygon.MoveEdge(Index: Integer; Pn: TCurvePoint);
var
  Idx1, Idx2: Integer;
begin
  if (Index >= 0) and (Index < NumVertices) then
  begin
    Idx1 := NextVertIdx(Index);
    Idx2 := NextVertIdx(Idx1);
    FVertices[Idx1] := CalcVertexPos(Pn, Vertex[Idx2], Index, Idx1);

    Idx1 := Index;
    Idx2 := PrevVertIdx(Index);
    FVertices[Idx1] := CalcVertexPos(Pn, Vertex[Idx2], Index, Idx2);

    // Notify the parent that the Polygon has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPolygon.Rotate(Index: Integer; Pn: TCurvePoint);
var
  a, b, c: TCurvePoint;
begin
  c := Center;
  a := FVertices[Index] - c;
  b := Pn - c;

  FRotated := (Modulus(a) > 0) and (Modulus(b) > 0);
  if FRotated then
  begin
    FCosine := (a.X*b.X + a.Y*b.Y)/Modulus(a)/Modulus(b);
    FSine := sign(a.x*(Pn.Y - c.Y) - a.y*(Pn.X - c.X))*sqrt(1 - FCosine*FCosine);
  end
  else
  begin
    FCosine := 1.0;
    FSine := 0.0;
   end;
end;

procedure TPolygon.ApplyRotation;
var
  i: Integer;
  a, c: TCurvePoint;
  X1, Y1: Double;
begin
  if Rotated then
  begin
    c := Center;

    for i := 0 to NumVertices - 1 do
    begin
      a := FVertices[i] - c;
      X1 := a.X*FCosine - a.Y*FSine;
      Y1 := a.X*FSine + a.Y*FCosine;
      FVertices[i] := c + TCurvePoint.Create(X1, Y1);
    end;

    CancelRotation;

    RecalcSlopes;

    // Notify the parent that the Polygon has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPolygon.CancelRotation;
begin
  FRotated := False;
  FSine := 0.0;
  FCosine := 1.0;
end;

function TPolygon.GetNumVertices: Integer;
begin
  Result :=  Length(FVertices);
end;

function TPolygon.GetVertex(Index: Integer): TCurvePoint;
var
  a, c: TCurvePoint;
  X1, Y1: Double;
begin
  if (Index >= 0) and (Index < NumVertices) then
  begin
    if Rotated then
    begin
      c := Center;

      a := FVertices[Index] - c;
      X1 := a.X*FCosine - a.Y*FSine;
      Y1 := a.X*FSine + a.Y*FCosine;
      Result := c + TCurvePoint.Create(X1, Y1);
    end
    else
      Result := FVertices[Index];
  end
  else
    Result :=  TCurvePoint.Create(-1, -1);
end;

function TPolygon.GetEdge(Index: Integer): TCurvePoint;
begin
  if (Index >= 0) and (Index < NumEdges) then
  begin
    if (Index = (NumEdges - 1)) then
      Result := (Vertex[Index] + Vertex[0])/2
    else
      Result := (Vertex[Index] + Vertex[Index + 1])/2;
  end
  else
    Result :=  TCurvePoint.Create(-1, -1);
end;

function TPolygon.GetCenter: TCurvePoint;
var
  i, j: Integer;
  A, Cx, Cy, C: Double;
begin
  A := 0;
  Cx := 0;
  Cy := 0;

  j := NumVertices - 1;
  for i := 0 to NumVertices - 1 do
  begin
    C := (FVertices[j].X*FVertices[i].Y - FVertices[i].X*FVertices[j].Y);

    A := A + C;

    Cx := Cx + (FVertices[j].X + FVertices[i].X)*C;
    Cy := Cy + (FVertices[j].Y + FVertices[i].Y)*C;

    j := i;
  end;

  Result := TCurvePoint.Create(Cx/A/3, Cy/A/3);
end;

function TPolygon.GetPolygonPoints(Zoom: Double): ArrayOfTPointF;
var i: Integer;
begin
  Setlength(Result, NumVertices);
  Result[0].X := Zoom*Vertex[0].X;
  Result[0].Y := Zoom*Vertex[0].Y;
  if IsCW then
  begin
    for i := Low(Result) + 1 to High(Result) do
    begin
      Result[i].X := Zoom*Vertex[i].X;
      Result[i].Y := Zoom*Vertex[i].Y;
    end;
  end
  else
  begin
    for i := High(Result) downto Low(Result) + 1 do
    begin
      Result[High(Result) - i + 1].X := Zoom*Vertex[i].X;
      Result[High(Result) - i + 1].Y := Zoom*Vertex[i].Y;
    end;
  end;
end;

function TPolygon.GetDrawPoints(Zoom: Double): ArrayOfTPointF;
var i: Integer;
begin
  Setlength(Result, NumVertices);
  for i := Low(Result) to High(Result) do
  begin
    Result[i].X := Zoom*Vertex[i].X;
    Result[i].Y := Zoom*Vertex[i].Y;
  end;
end;

function TPolygon.GetRect(Zoom: Double): TRect;
var
  i, Xo, Yo, Xf, Yf: Integer;
begin
  Result := TRect.Create(0, 0, 0, 0);

  if (NumVertices > 0) then
  begin
    Xo := Round(Vertex[0].X);
    Yo := Round(Vertex[0].Y);
    Xf := Round(Vertex[0].X);
    Yf := Round(Vertex[0].Y);
    for i := 1 to NumVertices -1 do
    begin
      if (Vertex[i].X < Xo) then Xo := Round(Vertex[i].X);
      if (Vertex[i].Y < Yo) then Yo := Round(Vertex[i].Y);
      if (Vertex[i].X > Xf) then Xf := Round(Vertex[i].X);
      if (Vertex[i].Y > Yf) then Yf := Round(Vertex[i].Y);
    end;

    Result := TRect.Create(Round(Xo*Zoom), Round(Yo*Zoom),
                           Round(Xf*Zoom), Round(Yf*Zoom));
  end;
end;

function TPolygon.NextVertIdx(Index: Integer): Integer;
begin
  if (Index = NumVertices - 1) then
    Result := 0
  else
    Result := Index + 1;
end;

function TPolygon.PrevVertIdx(Index: Integer): Integer;
begin
  if (Index = 0) then
    Result := NumVertices - 1
  else
    Result := Index - 1;
end;

procedure TPolygon.SetNumVertices(const Value: Integer);
begin
  if (Value >= 3) then
  begin
    SetLength(FVertices, Value);
    SetLength(FSlopes, Value);
    SetLength(FHoriLines, Value);

    // Notify the parent that the Polygon has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPolygon.SetVertex(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 0) and (Index < NumVertices) and (not Rotated) then
  begin
    FVertices[Index] := Value;

    RecalcSlopes;

    // Notify the parent that the Polygon has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPolygon.RecalcSlopes;
var
  i, j: Integer;
begin
  j := 0;
  for i := NumVertices - 1 downto 0 do
  begin
    if (Vertex[i] = Vertex[j]) then
    begin
      FHoriLines[i] := True;
      FSlopes[i] := 0;
    end
    else
    begin
      FHoriLines[i] := abs(Vertex[j].X - Vertex[i].X) >
                       abs(Vertex[j].Y - Vertex[i].Y);

      if FHoriLines[i] then
        FSlopes[i] := (Vertex[j].Y - Vertex[i].Y)/(Vertex[j].X - Vertex[i].X)
      else
        FSlopes[i] := (Vertex[j].X - Vertex[i].X)/(Vertex[j].Y - Vertex[i].Y);
    end;

    j := i;
  end;
end;

function TPolygon.CalcVertexPos(Pn, Pa: TCurvePoint; Index, Idx: Integer): TCurvePoint;
var
  a, b, c: Double;
begin
  if FHoriLines[Index] then
  begin
    if FHoriLines[Idx] then
    begin
      a := Pn.Y/FSlopes[Index]/Pn.X;
      b := Pa.Y/FSlopes[Idx]/Pa.X;
      c := FSlopes[Index] - FSlopes[Idx];

      Result.X := (b - a)/c;
      Result.Y := (b*FSlopes[Index] - a*FSlopes[Idx])/c;
    end
    else
    begin
      a := Pa.X - FSlopes[Idx]*Pa.Y;
      b := Pn.Y - FSlopes[Index]*Pn.X;
      c := FSlopes[Index]*FSlopes[Idx] - 1;

      Result.X := (-a - b*FSlopes[Idx])/c;
      Result.Y := (-a*FSlopes[Index] - b)/c;
    end;
  end
  else
  begin
    if FHoriLines[Idx] then
    begin
      a := Pn.X - FSlopes[Index]*Pn.Y;
      b := Pa.Y - FSlopes[Idx]*Pa.X;
      c := FSlopes[Index]*FSlopes[Idx] - 1;

      Result.X := (-a - b*FSlopes[Index])/c;
      Result.Y := (-a*FSlopes[Idx] - b)/c;
    end
    else
    begin
      a := Pn.X/FSlopes[Index]/Pn.Y;
      b := Pa.X/FSlopes[Idx]/Pa.Y;
      c := FSlopes[Index] - FSlopes[Idx];

      Result.X := (b*FSlopes[Index] - a*FSlopes[Idx])/c;
      Result.Y := (b - a)/c;
    end;
  end;
end;

function TPolygon.Contains(p: TCurvePoint): Boolean;
var
  i, j: Integer;
begin
  Result := False;

  j := NumVertices - 1;
  for i := 0 to NumVertices - 1 do
  begin
    // The point lies in an horizontal edge
    if  (Vertex[i].Y = p.Y) and (Vertex[j].Y = p.Y) and
       ((Vertex[i].X >= p.X) xor (Vertex[j].X >= p.X)) then
    begin
      Result := True;
      Exit;
    end;

    // The point lies in a non-horizontal edge
    if ((Vertex[i].Y >= p.Y) xor (Vertex[j].Y >= p.Y)) and
       ((Vertex[j].X - p.X)*(Vertex[j].Y - Vertex[i].Y) =
        (Vertex[j].X - Vertex[i].X)*(Vertex[j].Y - p.Y)) then
    begin
      Result := True;
      Exit;
    end;

    // General case, the line crosses an edge
    if ((Vertex[i].Y > p.Y) xor (Vertex[j].Y > p.Y)) and
       ((Vertex[j].X - p.X)*abs(Vertex[j].Y - Vertex[i].Y) <
        (Vertex[j].X - Vertex[i].X)*abs(Vertex[j].Y - p.Y)) then
      Result := not Result;

    j := i;
  end;
end;

// This function is adapted from:
// https://math.stackexchange.com/questions/1743995/determine-whether-a-polygon-is-convex-based-on-its-vertices/1745427#1745427
function TPolygon.IsConvex: Boolean;
var
  i, j, k: Integer;
  w, wSign,
  xSign, xFirstSign, xFlips,
  ySign, yFirstSign, yFlips: Integer;
  prev, curr, next: TCurvePoint;
  ax, ay, bx, by: Double;
begin
  if (NumVertices < 3) then
  begin
    Result := False;
    Exit;
  end;

  wSign := 0;        // First nonzero orientation (positive or negative)

  xSign := 0;
  xFirstSign := 0;   // Sign of first nonzero edge vector x
  xFlips := 0;       // Number of sign changes in x

  ySign := 0;
  yFirstSign := 0;   // Sign of first nonzero edge vector y
  yFlips := 0;       // Number of sign changes in y

  k := NumVertices - 2; // Second-to-last vertex
  j := NumVertices - 1; // Last vertex

  for i := 0 to NumVertices - 1 do   // Each vertex, in order
  begin
    prev := Vertex[k];          // Previous vertex
    curr := Vertex[j];          // Current vertex
    next := Vertex[i];          // Next vertex

    // Previous edge vector ("before"):
    bx := curr.x - prev.x;
    by := curr.y - prev.y;

    // Next edge vector ("after"):
    ax := next.x - curr.x;
    ay := next.y - curr.y;

    // Calculate sign flips using the next edge vector ("after"),
    // recording the first sign.
    if (ax > 0) then
    begin
      if (xSign = 0) then
        xFirstSign := 1
      else if (xSign < 0) then
        inc(xFlips);
      xSign := 1;
    end
    else if (ax < 0) then
    begin
      if (xSign = 0) then
        xFirstSign := -1
      else if (xSign > 0) then
        inc(xFlips);
      xSign := -1;
    end;

    if (xFlips > 2) then
    begin
      Result := False;
      Exit;
    end;

    if (ay > 0) then
    begin
      if (ySign = 0) then
        yFirstSign := 1
      else if (ySign < 0) then
        inc(yFlips);

      ySign := 1;
    end
    else if (ay < 0) then
    begin
      if (ySign = 0) then
        yFirstSign := -1
      else if (ySign > 0) then
        inc(yFlips);

      ySign := -1;
    end;

    if (yFlips > 2) then
    begin
      Result := False;
      Exit;
    end;

    // Find out the orientation of this pair of edges,
    // and ensure it does not differ from previous ones.
    w := Round(bx*ay - ax*by);
    if (wSign = 0) and (w <> 0) then
      wSign := w
    else if (wSign > 0) and (w < 0) then
    begin
      Result := False;
      Exit;
    end
    else if (wSign < 0) and (w > 0) then
    begin
      Result := False;
      Exit;
    end;

    k := j;
    j := i;
  end;

  // Final/wraparound sign flips:
  if (xSign <> 0) and (xFirstSign <> 0) and (xSign <> xFirstSign) then
    inc(xFlips);

  if (ySign <> 0) and (yFirstSign <> 0) and (ySign <> yFirstSign) then
    inc(yFlips);

  // Concave polygons have two sign flips along each axis.
  Result := (xFlips = 2) and (yFlips = 2);
end;

function TPolygon.IsCW: Boolean;
var
  i, j: Integer;
  sum: Double;
begin
  sum := 0.0;

  j := NumVertices - 1;
  for i := 0 to NumVertices - 1 do
  begin
    sum := sum  + (Vertex[i].X - Vertex[j].X)*(Vertex[i].Y + Vertex[j].Y);

    j := i;
  end;

  Result := (sum <= 0.0);
end;

function TPolygon.IsCCW: Boolean;
begin
  Result := not IsCW;
end;

function TPolygon.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, j: Integer;
  X, Y: Double;
  Child: TDOMNode;
begin
  Result := False;
  try
    with Item.Attributes do
    begin
      for i := 0 to Length - 1 do
      begin
        if (Item[i].CompareName('NumVertices') = 0) then
          NumVertices := StrToInt(UTF8Encode(Item[i].NodeValue));
      end;
    end;
    Child := Item.FirstChild;
    while assigned(Child) do
    begin
      for i := 1 to NumVertices do
      begin
        // Vertex points
        if (Child.CompareName(UTF8Decode('VertexPoint' + IntToStr(i))) = 0) then
        begin
          X := 0.0;
          Y := 0.0;
          for j := 0 to Child.Attributes.Length - 1 do
          begin
            if (Child.Attributes.Item[j].CompareName('X') = 0) then
              X := StrToFloat(UTF8Encode(Child.Attributes.Item[j].NodeValue));
            if (Child.Attributes.Item[j].CompareName('Y') = 0) then
              Y := StrToFloat(UTF8Encode(Child.Attributes.Item[j].NodeValue));
          end;

          Vertex[i - 1] := TCurvePoint.Create(X, Y);
        end;
      end;

      // Go for the next vertex point
      Child := Child.NextSibling;
    end;

    Result := True;
  except
    // Do nothing, just catch the error
  end;
end;

function TPolygon.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  VertexNode: TDOMNode;
begin
  try
    // Create box node
    Result := Doc.CreateElement('box');
    with TDOMElement(Result) do
    begin
      SetAttribute('NumVertices', UTF8Decode(IntToStr(NumVertices)));
    end;
    // Vertex points
    for i := 1 to NumVertices do
    begin
      VertexNode := Doc.CreateElement(UTF8Decode('VertexPoint' + IntToStr(i)));
      TDOMElement(VertexNode).SetAttribute('X', UTF8Decode(FloatToStr(Vertex[i - 1].X)));
      TDOMElement(VertexNode).SetAttribute('Y', UTF8Decode(FloatToStr(Vertex[i - 1].Y)));
      Result.Appendchild(VertexNode);
    end;
  except
    Result := nil;
  end;
end;

//==============================| TPolygon |===============================//


//==============================| TPlotQuad |==============================//
//
//           E0
// V0 |--------------| V1
//    |              |
//    |              |
// E3 |              | E1
//    |              |
//    |              |
// V3 |--------------| V2
//           E2
//

constructor TPlotQuad.Create;
begin
  inherited Create(4);

  FPolarCoordinates := False;
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  SetLength(FDMat, 3, 3);
  SetLength(FIMat, 3, 3);
end;

destructor TPlotQuad.Destroy;
begin
  SetLength(FDMat, 0);
  SetLength(FIMat, 0);

  inherited Destroy;
end;

function TPlotQuad.GetDrawPoints(Zoom: Double): ArrayOfTPointF;
const
  NumEllipsePoints = 360;
var
  l, m, u: Double;
  Dn, xp, yp, zp: Double;
  a, b, c, d: TCurvePoint;
  t: Integer;
begin
  if PolarCoordinates then
  begin
    if FRecalculateDirect then
    begin
      // Project a circle inscribed in the square x = [-1..1], y = [-1..1]
      // to the polygon. Only works for a convex polygon.
      a := Vertex[0];
      b := Vertex[1];
      c := Vertex[2];
      d := Vertex[3];

      Dn := a.X*(b.Y - c.Y) + b.X*(c.Y - a.Y) + c.X*(a.Y - b.Y);
      l := (b.X*(c.Y - d.Y) + c.X*(d.Y - b.Y) + d.X*(b.Y - c.Y))/Dn;
      m := (a.X*(d.Y - c.Y) + c.X*(a.Y - d.Y) + d.X*(c.Y - a.Y))/Dn;
      u := (a.X*(b.Y - d.Y) + b.X*(d.Y - a.Y) + d.X*(a.Y - b.Y))/Dn;

      FDMat[0, 0] := -(a.X*l + b.X*m);
      FDMat[0, 1] := b.X*m + c.X*u;
      FDMat[0, 2] := a.X*l + c.X*u;

      FDMat[1, 0] := -(a.Y*l + b.Y*m);
      FDMat[1, 1] := b.Y*m + c.Y*u;
      FDMat[1, 2] := a.Y*l + c.Y*u;

      FDMat[2, 0] := -(l + m);
      FDMat[2, 1] := m + u;
      FDMat[2, 2] := l + u;

      FRecalculateDirect := False;
    end;


    Setlength(Result, NumEllipsePoints);
    for t := Low(Result) to High(Result) do
    begin
      xp := cos(t*arctan(1)/45);
      yp := sin(t*arctan(1)/45);
      zp := xp*FDMat[2, 0] + yp*FDMat[2, 1] + FDMat[2, 2];

      Result[t].X := Zoom*(xp*FDMat[0, 0] + yp*FDMat[0, 1] + FDMat[0, 2])/zp;
      Result[t].Y := Zoom*(xp*FDMat[1, 0] + yp*FDMat[1, 1] + FDMat[1, 2])/zp;
    end;
  end
  else
    Result := inherited GetDrawPoints(Zoom);
end;

procedure TPlotQuad.SetVertex(Index: Integer; const Value: TCurvePoint);
begin
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  inherited SetVertex(Index, Value);
end;

procedure TPlotQuad.MoveVertex(Index: Integer; Pn: TCurvePoint);
begin
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  inherited MoveVertex(Index, Pn);
end;

procedure TPlotQuad.MoveEdge(Index: Integer; Pn: TCurvePoint);
begin
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  inherited MoveEdge(Index, Pn);
end;

procedure TPlotQuad.Rotate(Index: Integer; Pn: TCurvePoint);
begin
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  inherited Rotate(Index, Pn);
end;

function TPlotQuad.Contains(p: TCurvePoint): Boolean;
var
  l, m, u: Double;
  Dn, xp, yp, zp: Double;
  a, b, c, d: TCurvePoint;
begin
  if PolarCoordinates then
  begin
    if FRecalculateInverse then
    begin
      // Project the point p to the square x = [-1..1], y = [-1..1] and check
      // that it falls within the inscribed circle
      a := Vertex[0];
      b := Vertex[1];
      c := Vertex[2];
      d := Vertex[3];

      Dn := a.X*(b.Y - c.Y) + b.X*(c.Y - a.Y) + c.X*(a.Y - b.Y);
      l := (b.X*(c.Y - d.Y) + c.X*(d.Y - b.Y) + d.X*(b.Y - c.Y))/Dn;
      m := (a.X*(d.Y - c.Y) + c.X*(a.Y - d.Y) + d.X*(c.Y - a.Y))/Dn;
      u := (a.X*(b.Y - d.Y) + b.X*(d.Y - a.Y) + d.X*(a.Y - b.Y))/Dn;

      FIMat[0, 0] := a.Y*l*(m + u) - b.Y*m*(l + u) + c.Y*u*(m - l);
      FIMat[0, 1] := -a.X*l*(m + u) + b.X*m*(l + u) + c.X*u*(l - m);
      FIMat[0, 2] := a.X*l*(b.Y*m + c.Y*u) - b.X*m*(a.Y*l + c.Y*u) + c.X*u*(b.Y*m - a.Y*l);

      FIMat[1, 0] := a.Y*l*(m - u) - b.Y*m*(l + u) + c.Y*u*(l + m);
      FIMat[1, 1] := a.X*l*(u - m) + b.X*m*(l + u) - c.X*u*(l + m);
      FIMat[1, 2] := a.X*l*(b.Y*m - c.Y*u) - b.X*m*(a.Y*l + c.Y*u) + c.X*u*(a.Y*l + b.Y*m);

      FIMat[2, 0] := a.Y*l*(m + u) + b.Y*m*(u - l) - c.Y*u*(l + m);
      FIMat[2, 1] := -a.X*l*(m + u) + b.X*m*(l - u) + c.X*u*(l + m);
      FIMat[2, 2] := a.X*l*(b.Y*m + c.Y*u) + b.X*m*(c.Y*u - a.Y*l) - c.X*u*(a.Y*l + b.Y*m);

      FRecalculateInverse := False;
    end;

    zp := p.X*FIMat[2, 0] + p.Y*FIMat[2, 1] + FIMat[2, 2];
    xp := (p.X*FIMat[0, 0] + p.Y*FIMat[0, 1] + FIMat[0, 2])/zp;
    yp := (p.X*FIMat[1, 0] + p.Y*FIMat[1, 1] + FIMat[1, 2])/zp;

    // Check that the point is inside the circle
    Result := (xp*xp + yp*yp) <= 1;
  end
  else
    Result := inherited Contains(p);
end;

//==============================| TPlotQuad |==============================//


function GetCurvePoint(X, Y: Double): TCurvePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Modulus(v: TCurvePoint): Double;
begin
  Result := sqrt(v.X*v.X + v.Y*v.Y);
end;

function Normalize(v: TCurvePoint): TCurvePoint;
begin
  Result := v/Modulus(v);
end;

function VectorsAreParallel(p1, p2: TCurvePoint): Boolean;
var
  n1, n2: TCurvePoint;
begin
  n1 := Normalize(p1);
  n2 := Normalize(p2);

  Result := (n1 = n2) or (n1 = -n2);
end;

function PlotBox(p: TScalePoints; CoordSystem: TCoordSystem): TRect;
begin
  case CoordSystem of
    csCartesian: begin
      Result := TRect.Create(0, 0, Round(BoxWidth(p)), Round(BoxHeight(p)));
    end;
    csPolar: begin
      Result := TRect.Create(0, 0, 360, Round(Max(BoxWidth(p), BoxHeight(p))));
    end;
    else Result := TRect.Create(0, 0, 0, 0);
  end;
end;

function BoxHeight(p: TScalePoints): Double;
begin
  //Result := Max(p[1].Y, Max(p[2].Y, p[3].Y)) - Min(p[1].Y, Min(p[2].Y, p[3].Y));
  Result := Modulus(p[1] - p[2]);
end;

function BoxWidth(p: TScalePoints): Double;
begin
  //Result := Max(p[1].X, Max(p[2].X, p[3].X)) - Min(p[1].X, Min(p[2].X, p[3].X));
  Result := Modulus(p[3] - p[2]);
end;

function VirtualImagePoints(ImgPoints: TScalePoints; CoordSystem: TCoordSystem): TScalePoints;
var
  PB: TRect;
begin
  PB := PlotBox(ImgPoints, CoordSystem);
  Result[1].X := 0;
  Result[1].Y := PB.Height;
  Result[2].X := 0;
  Result[2].Y := 0;
  Result[3].X := PB.Width;
  Result[3].Y := 0;
  end;

function PointsToBase(p: TScalePoints): TBasis;
begin
  Result[1] := p[3] - p[2];
  Result[2] := p[1] - p[2];
end;

function SolveLinearSystem(b: TBasis; c: TCurvePoint): TCurvePoint;
begin
  Result.X := (c.X*b[2].Y - c.Y*b[2].X)/(b[1].X*b[2].Y - b[1].Y*b[2].X);
  Result.Y := (c.X*b[1].Y - c.Y*b[1].X)/(b[2].X*b[1].Y - b[2].Y*b[1].X);
end;

function TransfMatrix(b1, b2: TBasis): TBasisChangeMtrx;
var ab, cd: TCurvePoint;
begin
  ab := SolveLinearSystem(b2, b1[1]);
  cd := SolveLinearSystem(b2, b1[2]);
  Result[1, 1] := ab.X;
  Result[1, 2] := cd.X;
  Result[2, 1] := ab.Y;
  Result[2, 2] := cd.Y;
end;

function ChangeBasis(b1, b2: TBasis; p: TCurvePoint): TCurvePoint;
var
  BCM: TBasisChangeMtrx;
begin
  BCM := TransfMatrix(b1, b2);
  Result.X := BCM[1, 1]*p.X + BCM[1, 2]*p.Y;
  Result.Y := BCM[2, 1]*p.X + BCM[2, 2]*p.Y;
end;

function ChangeCoords(OldPoints, NewPoints: TScalePoints; p: TCurvePoint): TCurvePoint;
var
  OldBase, NewBase: TBasis;
  PntOldBase: TCurvePoint;
begin
  OldBase := PointsToBase(OldPoints);
  NewBase := PointsToBase(NewPoints);

  PntOldBase := SolveLinearSystem(OldBase, p - OldPoints[2]);

  Result := NewPoints[2] + PntOldBase.X*NewBase[1] + PntOldBase.Y*NewBase[2];
end;

function CartesianToPolar(p: TCurvePoint): TCurvePoint;
begin
  Result.X := arctan2(p.Y, p.X)*45/arctan(1);
  if (Result.X < 0) then
    Result.X := 360 + Result.X;
  Result.Y := sqrt(p.X*p.X + p.Y*p.Y);
end;

function CartesianToPolar(X, Y: Double): TCurvePoint;
begin
  Result := CartesianToPolar(TCurvePoint.Create(X, Y));
end;

function PolarToCartesian(p: TCurvePoint): TCurvePoint;
begin
  Result.X := p.Y*cos(arctan(1)*(p.X/45));
  Result.Y := p.Y*sin(arctan(1)*(p.X/45));
end;

function PolarToCartesian(X, Y: Double): TCurvePoint;
begin
  Result := PolarToCartesian(TCurvePoint.Create(X, Y));
end;

end.

