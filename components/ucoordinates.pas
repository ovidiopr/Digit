unit uCoordinates;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, Types;

type
  TCoordSystem = (csCartesian, csPolar);
  TScaleType = (stLinear, stLog, stLn, stInverse);

  TCurvePoint = record
    X, Y: Double;

    constructor Create(AX, AY: Double);

    class operator = (a, b: TCurvePoint): Boolean;
    class operator + (a, b: TCurvePoint): TCurvePoint;
    class operator - (a, b: TCurvePoint): TCurvePoint; overload;
    class operator - (a: TCurvePoint): TCurvePoint; overload;
    class operator * (a: Double; b: TCurvePoint): TCurvePoint;
    class operator / (a: TCurvePoint; b: Double): TCurvePoint;
    class operator := (a: TPoint) b: TCurvePoint;
    class operator := (a: TCurvePoint) b: TPoint;

    function DistanceTo(const APoint: TCurvePoint): Double;
    function ToStr(Fmt: String = '%.5g'): String; overload;
  end;

  TBasis = array [1..2] of TCurvePoint;
  TBasisChangeMtrx = array [1..2, 1..2] of Double;
  TScalePoints = array [1..3] of TCurvePoint;

const
  cp: TScalePoints = ((X: 0; Y: 1),
                      (X: 0; Y: 0),
                      (X: 1; Y: 0));
  cb: TBasis = ((X: 1; Y: 0),
                (X: 0; Y: 1));

function TCurvePointComparator(const a, b: TCurvePoint): Integer;
function Modulus(const v: TCurvePoint): Double; inline;
function Normalize(const v: TCurvePoint): TCurvePoint; inline;
function VectorsAreParallel(const p1, p2: TCurvePoint): Boolean; inline;
function PlotBox(const p: TScalePoints; CoordSystem: TCoordSystem): TRect;
function VirtualImagePoints(const ImgPoints: TScalePoints; CoordSystem: TCoordSystem): TScalePoints;
function BoxHeight(const p: TScalePoints): Double; inline;
function BoxWidth(const p: TScalePoints): Double; inline;
function PointsToBase(const p: TScalePoints): TBasis;
function SolveLinearSystem(const b: TBasis; const c: TCurvePoint): TCurvePoint;
function TransfMatrix(const b1, b2: TBasis): TBasisChangeMtrx;
function ChangeBasis(const b1, b2: TBasis; const p: TCurvePoint): TCurvePoint;
function ChangeCoords(const OldPoints, NewPoints: TScalePoints; const p: TCurvePoint): TCurvePoint;
function CartesianToPolar(const p: TCurvePoint): TCurvePoint; overload;
function CartesianToPolar(X, Y: Double): TCurvePoint; overload;
function PolarToCartesian(const p: TCurvePoint): TCurvePoint; overload;
function PolarToCartesian(X, Y: Double): TCurvePoint; overload;

implementation

{ TCurvePoint }

constructor TCurvePoint.Create(AX, AY: Double);
begin
  X := AX;
  Y := AY;
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

class operator TCurvePoint.- (a, b: TCurvePoint): TCurvePoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

class operator TCurvePoint.- (a: TCurvePoint): TCurvePoint;
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
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

function TCurvePoint.DistanceTo(const APoint: TCurvePoint): Double;
begin
  Result := Hypot(X - APoint.X, Y - APoint.Y);
end;

function TCurvePoint.ToStr(Fmt: String): String;
begin
  Result := Format(Fmt + ', ' + Fmt, [X, Y]);
end;

{ Utility Functions }

function TCurvePointComparator(const a, b: TCurvePoint): Integer;
begin
  Result := Sign(a.X - b.X);
end;

function Modulus(const v: TCurvePoint): Double;
begin
  Result := Hypot(v.X, v.Y);
end;

function Normalize(const v: TCurvePoint): TCurvePoint;
begin
  Result := v/Modulus(v);
end;

function VectorsAreParallel(const p1, p2: TCurvePoint): Boolean;
var
  n1, n2: TCurvePoint;
begin
  n1 := Normalize(p1);
  n2 := Normalize(p2);
  Result := (n1 = n2) or (n1 = -n2);
end;

function PlotBox(const p: TScalePoints; CoordSystem: TCoordSystem): TRect;
begin
  case CoordSystem of
    csCartesian:
      Result := TRect.Create(0, 0, Round(BoxWidth(p)), Round(BoxHeight(p)));
    csPolar:
      Result := TRect.Create(0, 0, 360, Round(Max(BoxWidth(p), BoxHeight(p))));
    else
      Result := TRect.Create(0, 0, 0, 0);
  end;
end;

function VirtualImagePoints(const ImgPoints: TScalePoints; CoordSystem: TCoordSystem): TScalePoints;
var
  PB: TRect;
begin
  PB := PlotBox(ImgPoints, CoordSystem);
  Result[1] := TCurvePoint.Create(0, PB.Height);
  Result[2] := TCurvePoint.Create(0, 0);
  Result[3] := TCurvePoint.Create(PB.Width, 0);
end;

function BoxHeight(const p: TScalePoints): Double;
begin
  Result := Modulus(p[1] - p[2]);
end;

function BoxWidth(const p: TScalePoints): Double;
begin
  Result := Modulus(p[3] - p[2]);
end;

function PointsToBase(const p: TScalePoints): TBasis;
begin
  Result[1] := p[3] - p[2];
  Result[2] := p[1] - p[2];
end;

function SolveLinearSystem(const b: TBasis; const c: TCurvePoint): TCurvePoint;
begin
  Result.X := (c.X*b[2].Y - c.Y*b[2].X)/(b[1].X*b[2].Y - b[1].Y*b[2].X);
  Result.Y := (c.X*b[1].Y - c.Y*b[1].X)/(b[2].X*b[1].Y - b[2].Y*b[1].X);
end;

function TransfMatrix(const b1, b2: TBasis): TBasisChangeMtrx;
var
  ab, cd: TCurvePoint;
begin
  ab := SolveLinearSystem(b2, b1[1]);
  cd := SolveLinearSystem(b2, b1[2]);
  Result[1, 1] := ab.X; Result[1, 2] := cd.X;
  Result[2, 1] := ab.Y; Result[2, 2] := cd.Y;
end;

function ChangeBasis(const b1, b2: TBasis; const p: TCurvePoint): TCurvePoint;
var
  BCM: TBasisChangeMtrx;
begin
  BCM := TransfMatrix(b1, b2);
  Result.X := BCM[1,1]*p.X + BCM[1,2]*p.Y;
  Result.Y := BCM[2,1]*p.X + BCM[2,2]*p.Y;
end;

function ChangeCoords(const OldPoints, NewPoints: TScalePoints; const p: TCurvePoint): TCurvePoint;
var
  OldBase, NewBase: TBasis;
  PntOldBase: TCurvePoint;
begin
  OldBase := PointsToBase(OldPoints);
  NewBase := PointsToBase(NewPoints);
  PntOldBase := SolveLinearSystem(OldBase, p - OldPoints[2]);
  Result := NewPoints[2] + PntOldBase.X*NewBase[1] + PntOldBase.Y*NewBase[2];
end;

function CartesianToPolar(const p: TCurvePoint): TCurvePoint;
begin
  Result.X := ArcTan2(p.Y, p.X)*180/Pi;
  if Result.X < 0 then Result.X := 360 + Result.X;
  Result.Y := Hypot(p.X, p.Y);
end;

function CartesianToPolar(X, Y: Double): TCurvePoint;
begin
  Result := CartesianToPolar(TCurvePoint.Create(X, Y));
end;

function PolarToCartesian(const p: TCurvePoint): TCurvePoint;
begin
  Result.X := p.Y*Cos(p.X*Pi/180);
  Result.Y := p.Y*Sin(p.X*Pi/180);
end;

function PolarToCartesian(X, Y: Double): TCurvePoint;
begin
  Result := PolarToCartesian(TCurvePoint.Create(X, Y));
end;

end.

