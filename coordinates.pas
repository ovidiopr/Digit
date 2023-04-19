unit coordinates;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, DOM;

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

    function Nx(Pi: TCurvePoint): TCurvePoint;
    function Ny(Pi: TCurvePoint): TCurvePoint;

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
  Reset;
end;

destructor TScale.Destroy;
begin
  inherited;
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

function TScale.Nx(Pi: TCurvePoint): TCurvePoint;
begin
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + GetCurvePoint(1, 0)) - Pi)
end;

function TScale.Ny(Pi: TCurvePoint): TCurvePoint;
begin
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + GetCurvePoint(0, 1)) - Pi)
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

