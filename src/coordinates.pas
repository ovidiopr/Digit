unit coordinates;

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
    class operator = (a, b: TCurvePoint): Boolean;
    class operator + (a, b: TCurvePoint): TCurvePoint;
    class operator - (a: TCurvePoint): TCurvePoint;
    class operator - (a, b: TCurvePoint): TCurvePoint;
    class operator * (a: Double; b: TCurvePoint): TCurvePoint;
    class operator / (a: TCurvePoint; b: Double): TCurvePoint;
    class operator := (a: TPoint) b: TCurvePoint;
    class operator := (a: TCurvePoint) b: TPoint;
  end;
  TBasis = Array [1..2] of TCurvePoint;
  TBasisChangeMtrx = Array [1..2, 1..2] of Double;
  TScalePoints = Array [1..3] of TCurvePoint;

  ArrayOfTPointF = Array of TPointF;

  TScale = class(TObject)
  private
    { Private declarations }
    FCoords: TCoordSystem;
    FXScale: TScaleType;
    FYScale: TScaleType;

    FXLabel: String;
    FYLabel: String;

    FPntImg: TScalePoints;
    FPntPlt: TScalePoints;

    FImgPointIsSet: Array [1..3] of Boolean;
    FPltPointIsSet: Array [1..3] of Boolean;

    function GetImagePoint(Index: Integer): TCurvePoint;
    function GetPlotPoint(Index: Integer): TCurvePoint;
    function GetPointIsSet(Index: Integer): Boolean;
    function GetPointsAreSet: Boolean;
    function GetIsValidScale: Boolean;

    procedure SetCoordSystem(const Value: TCoordSystem);
    procedure SetImagePoint(Index: Integer; const Value: TCurvePoint);
    procedure SetPlotPoint(Index: Integer; const Value: TCurvePoint);
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create;
    {@exclude}
    destructor Destroy; override;

    procedure Reset;

    function FromImgToPlot(p: TCurvePoint): TCurvePoint; overload;
    function FromImgToPlot(X, Y: Double): TCurvePoint; overload;
    function FromPlotToImg(p: TCurvePoint): TCurvePoint; overload;
    function FromPlotToImg(X, Y: Double): TCurvePoint; overload;

    function Distance(P1, P2: TCurvePoint): Double;

    function Nx(Pi: TCurvePoint): TCurvePoint;
    function Ny(Pi: TCurvePoint): TCurvePoint;
    function dx(Pi: TCurvePoint): Double;
    function dy(Pi: TCurvePoint): Double;

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    property CoordSystem: TCoordSystem read FCoords write SetCoordSystem;
    property XScale: TScaleType read FXScale write FXScale;
    property YScale: TScaleType read FYScale write FYScale;
    property XLabel: String read FXLabel write FXLabel;
    property YLabel: String read FYLabel write FYLabel;
    property ImagePoints: TScalePoints read FPntImg;
    property PlotPoints: TScalePoints read FPntPlt;

    property ImagePoint[Index: Integer]: TCurvePoint read GetImagePoint write SetImagePoint;
    property PlotPoint[Index: Integer]: TCurvePoint read GetPlotPoint write SetPlotPoint;
    property PointIsSet[Index: Integer]: Boolean read GetPointIsSet;
    property AllPointsAreSet: Boolean read GetPointsAreSet;
    property IsValid: Boolean read GetIsValidScale;
  end;

  TPlotPoly = class(TObject)
  private
    { Private declarations }
    FVertices: Array of TCurvePoint;

    function GetNumvertices: Integer;
    function GetVertex(Index: Integer): TCurvePoint;
    function GetPolygonPoints: ArrayOfTPointF; virtual;
    function GetRect: TRect;

    procedure SetNumVertices(const Value: Integer);
    procedure SetVertex(Index: Integer; const Value: TCurvePoint); virtual;
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create(NumVert: Integer = 4);
    {@exclude}
    destructor Destroy; override;

    procedure Reset;

    function Contains(p: TCurvePoint): Boolean; virtual;

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    property NumVertices: Integer read GetNumvertices write SetNumVertices;
    property Vertex[Index: Integer]: TCurvePoint read GetVertex write SetVertex;
    property PolygonPoints: ArrayOfTPointF read GetPolygonPoints;
    property Rect: TRect read GetRect;
  end;

  TPlotQuad = class(TPlotPoly)
  private
    { Private declarations }
    FPolarCoordinates: Boolean;
    FRecalculateDirect: Boolean;
    FRecalculateInverse: Boolean;

    FDMat: Array of Array of Double;
    FIMat: Array of Array of Double;

    function GetPolygonPoints: ArrayOfTPointF; override;

    procedure SetVertex(Index: Integer; const Value: TCurvePoint); override;
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create;
    {@exclude}
    destructor Destroy; override;

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

function TCurvePointComparator(const a, b: TCurvePoint): Integer;
begin
  Result := Sign(a.X - b.X);
end;


//===============================| TScale |===============================//
constructor TScale.Create;
begin
  inherited Create;
  Reset;
end;

destructor TScale.Destroy;
begin
  inherited Destroy;
end;

procedure TScale.Reset;
var
  i: Integer;
begin
  CoordSystem := csCartesian;
  XScale := stLinear;
  YScale := stLinear;

  XLabel := 'X';
  YLabel := 'Y';

  for i := 1 to 3 do
  begin
    ImagePoint[i] := GetCurvePoint(0, 0);
    PlotPoint[i] := GetCurvePoint(0, 0);

    FImgPointIsSet[i] := False;
    FPltPointIsSet[i] := False;
  end;
end;

function TScale.FromImgToPlot(p: TCurvePoint): TCurvePoint;
var
  pnt: TCurvePoint;
  TmpPlotPoint: TScalePoints;
  Y0: Double;
begin
  if AllPointsAreSet then
  begin
    TmpPlotPoint := PlotPoints;
    Y0 := PlotPoints[2].Y;

    // Make sure that the basis is linear
    case FXScale of
      stLog: begin
        TmpPlotPoint[1].X := log10(TmpPlotPoint[1].X);
        TmpPlotPoint[2].X := log10(TmpPlotPoint[2].X);
        TmpPlotPoint[3].X := log10(TmpPlotPoint[3].X);
      end;
      stLn: begin
        TmpPlotPoint[1].X := ln(TmpPlotPoint[1].X);
        TmpPlotPoint[2].X := ln(TmpPlotPoint[2].X);
        TmpPlotPoint[3].X := ln(TmpPlotPoint[3].X);
      end;
      stInverse: begin
        TmpPlotPoint[1].X := 1/TmpPlotPoint[1].X;
        TmpPlotPoint[2].X := 1/TmpPlotPoint[2].X;
        TmpPlotPoint[3].X := 1/TmpPlotPoint[3].X;
      end;
    end;

    // Make sure that the basis is linear
    case FYScale of
      stLog: begin
        TmpPlotPoint[1].Y := log10(TmpPlotPoint[1].Y);
        TmpPlotPoint[2].Y := log10(TmpPlotPoint[2].Y);
        TmpPlotPoint[3].Y := log10(TmpPlotPoint[3].Y);

        Y0 := log10(Y0);
      end;
      stLn: begin
        TmpPlotPoint[1].Y := ln(TmpPlotPoint[1].Y);
        TmpPlotPoint[2].Y := ln(TmpPlotPoint[2].Y);
        TmpPlotPoint[3].Y := ln(TmpPlotPoint[3].Y);

        Y0 := ln(Y0);
      end;
      stInverse: begin
        TmpPlotPoint[1].Y := 1/TmpPlotPoint[1].Y;
        TmpPlotPoint[2].Y := 1/TmpPlotPoint[2].Y;
        TmpPlotPoint[3].Y := 1/TmpPlotPoint[3].Y;

        Y0 := 1/Y0;
      end;
    end;

    // Make sure that the basis is in cartesian coordinates
    if (FCoords = csPolar) then
    begin
      TmpPlotPoint[1] := PolarToCartesian(TmpPlotPoint[1].X, TmpPlotPoint[1].Y - Y0);
      TmpPlotPoint[2] := PolarToCartesian(TmpPlotPoint[2].X, 0);
      TmpPlotPoint[3] := PolarToCartesian(TmpPlotPoint[3].X, TmpPlotPoint[3].Y - Y0);
    end;

    // Change the basis
    pnt := ChangeCoords(ImagePoints, TmpPlotPoint, p);

    // Make sure that the point is in the right coordinate system
    if (FCoords = csPolar) then
    begin
      pnt := CartesianToPolar(pnt);
      pnt.Y := Y0 + pnt.Y;
    end;

    // Make sure that X is in the right scale
    case FXScale of
      stLog: pnt.X := Power(10, pnt.X);
      stLn: pnt.X := Exp(pnt.X);
      stInverse: pnt.X := 1.0/pnt.X;
    end;

    // Make sure that Y is in the right scale
    case FYScale of
      stLog: pnt.Y := Power(10, pnt.Y);
      stLn: pnt.Y := Exp(pnt.Y);
      stInverse: pnt.Y := 1.0/pnt.Y;
    end;

    Result := pnt;
  end
  else
    Result := GetCurvePoint(0, 0);
end;

function TScale.FromImgToPlot(X, Y: Double): TCurvePoint;
begin
  Result := FromImgToPlot(GetCurvePoint(X, Y));
end;

function TScale.FromPlotToImg(p: TCurvePoint): TCurvePoint;
var
  pnt: TCurvePoint;
  TmpPlotPoint: TScalePoints;
  Y0: Double;
begin
  if AllPointsAreSet then
  begin
    TmpPlotPoint := PlotPoints;
    Y0 := PlotPoints[2].Y;
    pnt := p;

    // Make sure that X is linear
    case FXScale of
      stLog: begin
        TmpPlotPoint[1].X := log10(TmpPlotPoint[1].X);
        TmpPlotPoint[2].X := log10(TmpPlotPoint[2].X);
        TmpPlotPoint[3].X := log10(TmpPlotPoint[3].X);

        pnt.X := log10(pnt.X);
      end;
      stLn: begin
        TmpPlotPoint[1].X := ln(TmpPlotPoint[1].X);
        TmpPlotPoint[2].X := ln(TmpPlotPoint[2].X);
        TmpPlotPoint[3].X := ln(TmpPlotPoint[3].X);

        pnt.X := ln(pnt.X);
      end;
      stInverse: begin
        TmpPlotPoint[1].X := 1/TmpPlotPoint[1].X;
        TmpPlotPoint[2].X := 1/TmpPlotPoint[2].X;
        TmpPlotPoint[3].X := 1/TmpPlotPoint[3].X;

        pnt.X := 1.0/pnt.X;
      end;
    end;

    // Make sure that Y is linear
    case FYScale of
      stLog: begin
        TmpPlotPoint[1].Y := log10(TmpPlotPoint[1].Y);
        TmpPlotPoint[2].Y := log10(TmpPlotPoint[2].Y);
        TmpPlotPoint[3].Y := log10(TmpPlotPoint[3].Y);

        Y0 := log10(Y0);
        pnt.Y := log10(pnt.Y);
      end;
      stLn: begin
        TmpPlotPoint[1].Y := ln(TmpPlotPoint[1].Y);
        TmpPlotPoint[2].Y := ln(TmpPlotPoint[2].Y);
        TmpPlotPoint[3].Y := ln(TmpPlotPoint[3].Y);

        Y0 := ln(Y0);
        pnt.Y := ln(pnt.Y);
      end;
      stInverse: begin
        TmpPlotPoint[1].Y := 1/TmpPlotPoint[1].Y;
        TmpPlotPoint[2].Y := 1/TmpPlotPoint[2].Y;
        TmpPlotPoint[3].Y := 1/TmpPlotPoint[3].Y;

        Y0 := 1/Y0;
        pnt.Y := 1/pnt.Y;
      end;
    end;

    // Make sure that the basis and the point are in cartesian coordinates
    if (FCoords = csPolar) then
    begin
      TmpPlotPoint[1] := PolarToCartesian(TmpPlotPoint[1].X, TmpPlotPoint[1].Y - Y0);
      TmpPlotPoint[2] := PolarToCartesian(TmpPlotPoint[2].X, 0);
      TmpPlotPoint[3] := PolarToCartesian(TmpPlotPoint[3].X, TmpPlotPoint[3].Y - Y0);

      pnt := PolarToCartesian(pnt.X, pnt.Y - Y0);
    end;

    // Change the basis
    Result := ChangeCoords(TmpPlotPoint, ImagePoints, pnt);
  end
  else
    Result := GetCurvePoint(0, 0);
end;

function TScale.FromPlotToImg(X, Y: Double): TCurvePoint;
begin
  Result := FromPlotToImg(GetCurvePoint(X, Y));
end;

function TScale.Distance(P1, P2: TCurvePoint): Double;
begin
  if (CoordSystem = csCartesian) then
    Result := Sqrt(Power(P2.X - P1.X, 2) + Power(P2.Y - P1.Y, 2))
  else
    Result := Sqrt(P1.Y*P1.Y + P2.Y*P2.Y - 2*P1.Y*P2.Y*Cos(PI*(P2.X - P1.X)/180));
end;

function TScale.Nx(Pi: TCurvePoint): TCurvePoint;
begin
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + GetCurvePoint(1, 0)) - Pi)
end;

function TScale.Ny(Pi: TCurvePoint): TCurvePoint;
begin
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + GetCurvePoint(0, 1)) - Pi)
end;

function TScale.dx(Pi: TCurvePoint): Double;
begin
  Result := Distance(FromImgToPlot(Pi + Nx(Pi)), FromImgToPlot(Pi - Nx(Pi)))/2;
end;

function TScale.dy(Pi: TCurvePoint): Double;
begin
  Result := Distance(FromImgToPlot(Pi + Ny(Pi)), FromImgToPlot(Pi - Ny(Pi)))/2;
end;

function TScale.GetImagePoint(Index: Integer): TCurvePoint;
begin
  if (Index >= 1) and (Index <= 3) then
    Result := FPntImg[Index]
end;

function TScale.GetPlotPoint(Index: Integer): TCurvePoint;
begin
  if (Index >= 1) and (Index <= 3) then
    Result := FPntPlt[Index];
end;

function TScale.GetPointIsSet(Index: Integer): Boolean;
begin
  if (Index >= 1) and (Index <= 3) then
    Result := FImgPointIsSet[Index] and FPltPointIsSet[Index]
  else
    Result := False;
end;

function TScale.GetPointsAreSet: Boolean;
begin
  Result := FImgPointIsSet[1] and FImgPointIsSet[2] and FImgPointIsSet[3] and
            FPltPointIsSet[1] and FPltPointIsSet[2] and FPltPointIsSet[3];
end;

function TScale.GetIsValidScale: Boolean;
var
  b1, b2: TBasis;
begin
  Result := False;
  if AllPointsAreSet then
  begin
    b1 := PointsToBase(ImagePoints);
    b2 := PointsToBase(PlotPoints);

    Result := (not VectorsAreParallel(b1[1], b1[2])) and
              (not VectorsAreParallel(b2[1], b2[2]));
  end;
end;

procedure TScale.SetCoordSystem(const Value: TCoordSystem);
begin
  if (Value <> FCoords) then
  begin
    FCoords := Value;
    if (FCoords = csCartesian) then
    begin
      PlotPoint[1] := PolarToCartesian(PlotPoint[1]);
      PlotPoint[2] := PolarToCartesian(PlotPoint[2]);
      PlotPoint[3] := PolarToCartesian(PlotPoint[3]);
    end
    else
    begin
      PlotPoint[1] := CartesianToPolar(PlotPoint[1]);
      PlotPoint[2] := CartesianToPolar(PlotPoint[2]);
      PlotPoint[3] := CartesianToPolar(PlotPoint[3]);
    end;
  end;
end;

procedure TScale.SetImagePoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 1) and (Index <= 3) then
  begin
    FPntImg[Index] := Value;
    FImgPointIsSet[Index] := True;
  end;
end;

procedure TScale.SetPlotPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 1) and (Index <= 3) then
  begin
    FPntPlt[Index] := Value;
    FPltPointIsSet[Index] := True;
  end;
end;

function TScale.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, j: Integer;
  X, Y: Double;
  Child: TDOMNode;

function StrToCoordSystem(Value: String): TCoordSystem;
begin
  if (Value = 'POLAR') then
    Result := csPolar
  else
    Result := csCartesian;
end;

function StrToScaleType(Value: String): TScaleType;
begin
  if (Value = 'LOG') then
    Result := stLog
  else if (Value = 'LN') then
    Result := stLn
  else if (Value = 'INVERSE') then
    Result := stInverse
  else
    Result := stLinear;
end;

begin
  Result := False;
  try
    CoordSystem := csCartesian;
    XScale := stLinear;
    YScale := stLinear;
    XLabel := 'X';
    YLabel := 'Y';
    with Item.Attributes do
    begin
      for i := 0 to Length - 1 do
      begin
        if (Item[i].CompareName('Coordinates') = 0) then
          CoordSystem := StrToCoordSystem(UTF8Encode(UnicodeUpperCase(Item[i].NodeValue)));
        if (Item[i].CompareName('XScale') = 0) then
          XScale := StrToScaleType(UTF8Encode(UnicodeUpperCase(Item[i].NodeValue)));
        if (Item[i].CompareName('YScale') = 0) then
          YScale := StrToScaleType(UTF8Encode(UnicodeUpperCase(Item[i].NodeValue)));
        if (Item[i].CompareName('XLabel') = 0) then
          XLabel := UTF8Encode(Item[i].NodeValue);
        if (Item[i].CompareName('YLabel') = 0) then
          YLabel := UTF8Encode(Item[i].NodeValue);
      end;
    end;
    Child := Item.FirstChild;
    while Assigned(Child) do
    begin
      for i := 1 to 3 do
      begin
        // Image points
        if (Child.CompareName(UTF8Decode('ImagePoint' + IntToStr(i))) = 0) then
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

          ImagePoint[i] := GetCurvePoint(X, Y);
        end;

        // Plot points
        if (Child.CompareName(UTF8Decode('PlotPoint' + IntToStr(i))) = 0) then
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

          PlotPoint[i] := GetCurvePoint(X, Y);
        end;
      end;

      // Go for the next image point or plot point
      Child := Child.NextSibling;
    end;

    Result := True;
  except
    // Do nothing, just catch the error
  end;
end;

function TScale.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  PointNode: TDOMNode;

function CoordSystemToStr(Value: TCoordSystem): String;
begin
  if (Value = csPolar) then
    Result := 'Polar'
  else
    Result := 'Cartesian';
end;

function ScaleTypeToStr(Value: TScaleType): String;
begin
  case Value of
    stLog: Result := 'Log';
    stLn: Result := 'Ln';
    stInverse: Result := 'Inverse';
    else Result := 'Linear';
  end;
end;

begin
  try
    // Create scale node
    Result := Doc.CreateElement('scale');
    with TDOMElement(Result) do
    begin
      SetAttribute('Coordinates', UTF8Decode(CoordSystemToStr(CoordSystem)));
      SetAttribute('XScale', UTF8Decode(ScaleTypeToStr(XScale)));
      SetAttribute('YScale', UTF8Decode(ScaleTypeToStr(YScale)));
      SetAttribute('XLabel', UTF8Decode(XLabel));
      SetAttribute('YLabel', UTF8Decode(YLabel));
    end;
    // Image points
    for i := 1 to 3 do
    begin
      PointNode := Doc.CreateElement(UTF8Decode('ImagePoint' + IntToStr(i)));
      TDOMElement(PointNode).SetAttribute('X', UTF8Decode(FloatToStr(ImagePoint[i].X)));
      TDOMElement(PointNode).SetAttribute('Y', UTF8Decode(FloatToStr(ImagePoint[i].Y)));
      Result.Appendchild(PointNode);
    end;
    // Plot points
    for i := 1 to 3 do
    begin
      PointNode := Doc.CreateElement(UTF8Decode('PlotPoint' + IntToStr(i)));
      TDOMElement(PointNode).SetAttribute('X', UTF8Decode(FloatToStr(PlotPoint[i].X)));
      TDOMElement(PointNode).SetAttribute('Y', UTF8Decode(FloatToStr(PlotPoint[i].Y)));
      Result.Appendchild(PointNode);
    end;
  except
    Result := nil;
  end;
end;

//===============================| TScale |===============================//


//==============================| TPlotPoly |==============================//

constructor TPlotPoly.Create(NumVert: Integer = 4);
begin
  inherited Create;

  NumVertices := NumVert;

  Reset;
end;

destructor TPlotPoly.Destroy;
begin
  SetLength(FVertices, 0);

  inherited Destroy;
end;

procedure TPlotPoly.Reset;
var
  i: Integer;
begin
  for i := 0 to NumVertices - 1 do
    Vertex[i] := GetCurvePoint(-1, -1);
end;

function TPlotPoly.GetNumVertices: Integer;
begin
  Result :=  Length(FVertices);
end;

function TPlotPoly.GetVertex(Index: Integer): TCurvePoint;
begin
  if (Index >= 0) and (Index < NumVertices) then
    Result := FVertices[Index]
  else
    Result :=  GetCurvePoint(-1, -1);
end;

function TPlotPoly.GetPolygonPoints: ArrayOfTPointF;
var i: Integer;
begin
  Setlength(Result, NumVertices);
  for i := Low(Result) to High(Result) do
  begin
    Result[i].X := FVertices[i].X;
    Result[i].Y := FVertices[i].Y;
  end;
end;

function TPlotPoly.GetRect: TRect;
var
  i, Xo, Yo, Xf, Yf: Integer;
begin
  Result := TRect.Create(0, 0, 0, 0);

  if (NumVertices > 0) then
  begin
    Xo := Round(FVertices[0].X);
    Yo := Round(FVertices[0].Y);
    Xf := Round(FVertices[0].X);
    Yf := Round(FVertices[0].Y);
    for i := 1 to NumVertices -1 do
    begin
      if (FVertices[i].X < Xo) then Xo := Round(FVertices[i].X);
      if (FVertices[i].Y < Yo) then Yo := Round(FVertices[i].Y);
      if (FVertices[i].X > Xf) then Xf := Round(FVertices[i].X);
      if (FVertices[i].Y > Yf) then Yf := Round(FVertices[i].Y);
    end;

    Result := TRect.Create(Xo, Yo, Xf, Yf);
  end;
end;

procedure TPlotPoly.SetNumVertices(const Value: Integer);
begin
  SetLength(FVertices, Value);
end;

procedure TPlotPoly.SetVertex(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 0) and (Index < NumVertices) then
    FVertices[Index] := Value;
end;

function TPlotPoly.Contains(p: TCurvePoint): Boolean;
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

function TPlotPoly.ImportFromXML(Item: TDOMNode): Boolean;
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
    while Assigned(Child) do
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

          Vertex[i - 1] := GetCurvePoint(X, Y);
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

function TPlotPoly.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  VertexNode: TDOMNode;
begin
  try
    // Create scale node
    Result := Doc.CreateElement('PlotBox');
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

//==============================| TPlotPoly |==============================//


//==============================| TPlotQuad |==============================//

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

function TPlotQuad.GetPolygonPoints: ArrayOfTPointF;
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

      Result[t].X := (xp*FDMat[0, 0] + yp*FDMat[0, 1] + FDMat[0, 2])/zp;
      Result[t].Y := (xp*FDMat[1, 0] + yp*FDMat[1, 1] + FDMat[1, 2])/zp;
    end;
  end
  else
    Result := inherited GetPolygonPoints;
end;

procedure TPlotQuad.SetVertex(Index: Integer; const Value: TCurvePoint);
begin
  FRecalculateDirect := True;
  FRecalculateInverse := True;

  inherited SetVertex(Index, Value);
end;

function TPlotQuad.Contains(p: TCurvePoint): Boolean;
var
  l, m, u: Double;
  Mat: Array of Array of Double;
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
  Result := CartesianToPolar(GetCurvePoint(X, Y));
end;

function PolarToCartesian(p: TCurvePoint): TCurvePoint;
begin
  Result.X := p.Y*cos(arctan(1)*(p.X/45));
  Result.Y := p.Y*sin(arctan(1)*(p.X/45));
end;

function PolarToCartesian(X, Y: Double): TCurvePoint;
begin
  Result := PolarToCartesian(GetCurvePoint(X, Y));
end;

end.

