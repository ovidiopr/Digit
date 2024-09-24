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

const
  cp: TScalePoints = ((X: 0; Y: 1),
                      (X: 0; Y: 0),
                      (X: 1; Y: 0));
  cb: TBasis = ((X: 1; Y: 0),
                (X: 0; Y: 1));

  function TCurvePointComparator(const a, b: TCurvePoint): Integer;
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

