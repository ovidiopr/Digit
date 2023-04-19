unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, coordinates, typ, ipf, math, inv, Dialogs;

function BSpline(Points: Array of TCurvePoint; Degree: Integer; Xval: Double): Double;
procedure SavitzkyGolay(Kernel, Degree, Deriv: Integer; var Points: Array of TCurvePoint);

implementation


function WeightedAverage(X1, X2, Coeff: Double): Double;
begin
  Result := X1*(1 - Coeff) + X2*Coeff;
end;

function NextNumberSeq(const Points: Array of TCurvePoint; var StartIdx, EndIdx: Integer): Boolean;
begin
  StartIdx := EndIdx + 2;
  while (StartIdx <= High(Points)) and
        (IsNan(Points[StartIdx].X) or
         IsNan(Points[StartIdx].Y)) do
    inc(StartIdx);
  EndIdx := StartIdx;
  while (EndIdx + 1 <= High(Points)) and
    not (IsNan(Points[EndIdx + 1].X) or
         IsNan(Points[EndIdx + 1].y)) do
    inc(EndIdx);
  Result := StartIdx <= High(Points);
end;

// This function has been adapted from the function 'TBSplineSeries.Calculate'
// in the unit 'TAFuncSeries'
function BSpline(Points: Array of TCurvePoint; Degree: Integer; Xval: Double): Double;
var
  p: Array of TCurvePoint;
  startIndex: Integer;
  splineStart: Integer = 0;
  splineEnd: Integer = -2;
  pStart, pEnd: TCurvePoint;

  function CalcSpline(Pos: Double): TCurvePoint;
  var
    i, d: Integer;
    w, denom: Double;
  begin
    // Duplicate end points Degree times to fix spline to them.
    for i := 0 to Degree do
      p[i] := Points[EnsureRange(startIndex - Degree + i, splineStart, splineEnd)];
    // De Boor's algorithm, source points used as control points.
    // Parametric coordinate is equal to point index.
    for d := 1 to Degree do
    begin
      denom := Degree + 1 - d;
      for i := Degree downto d do
      begin
        w := (Pos + Degree - i)/denom;
        p[i].X := WeightedAverage(p[i - 1].X, p[i].X, w);
        p[i].Y := WeightedAverage(p[i - 1].Y, p[i].Y, w);
      end;
    end;
    Result := p[Degree];
  end;

  function Interpolate(Xint: Double): TCurvePoint;
  // calculates the B-Spline at n pivot points of the parametric coordinate t=0..1
  // and seeks the t for the requested x value (Xint) by means of
  // interpolating a cubic spline
  var
    i, n, ok: Integer;
    t: ArbFloat;
    pp: TCurvePoint;
    xval, yval, coeff: Array of ArbFloat;
  begin
    n := 10;
    SetLength(xval, n + 1);
    SetLength(yval, n + 1);
    SetLength(coeff, n + 1);
    // Calculate pivots
    for i:=0 to n do begin
      pp := CalcSpline(i/n);
      xval[i] := pp.X;
      yval[i] := i/n;
    end;
    ok := 0;
    // Calc interpolation spline coefficients
    ipfisn(N, xval[0], yval[0], coeff[0], ok);
    assert(ok = 1, 'Error: Calculation of spline coefficients failed.');

    // Calc interpolation spline value at Xint
    t := ipfspn(High(coeff), xval[0], yval[0], coeff[0], Xint, ok);
    assert(ok = 1, 'Error: Interpolation with spline failed.');

    // Calc B-Spline value at t
    Result := CalcSpline(t);
  end;

begin
  Result := NaN;

  if (Length(Points) > 0) then
  begin
    SetLength(p, Degree + 1);
    while NextNumberSeq(Points, splineStart, splineEnd) do
    begin
      startIndex := splineStart;
      pStart := CalcSpline(0.0);
      while startIndex <= splineEnd + Degree - 1 do
      begin
        pEnd := CalcSpline(1.0);
        // Find interval
        if (Xval = pStart.X) and (pStart.X = pEnd.X) then
          Result := pStart.Y
        else
        if InRange(Xval, pStart.X, pEnd.X) and (pStart.X <> pEnd.X) then
        begin
          // Calculate B-spline Y value by interpolation
          Result := Interpolate(Xval).Y;
          Exit;
        end;
        pStart := pEnd;
        inc(startIndex);
      end;
      Result := pEnd.Y;
    end;
  end;
end;


// This function implements the method described in:
// https://dsp.stackexchange.com/questions/1676/savitzky-golay-smoothing-filter-for-not-equally-spaced-data
// Feel free to use it at your own risk
procedure SavitzkyGolay(Kernel, Degree, Deriv: Integer; var Points: Array of TCurvePoint);
type
  TMatrix = Array of Array of ArbFloat;

function Index(idx, idx_ini, idx_end: Integer): Integer;
begin
  Result := idx;
  if (Result > idx_end) then
    Result := 2*idx_end - Result;
  if (Result < idx_ini) then
    Result := 2*idx_ini - Result;
end;

procedure MatMult(const A, B: TMatrix; var C: TMatrix; ic, kc, jc: Integer);
var
  i, j, k: integer;
begin
  for i := 0 to ic - 1 do
    for j := 0 to jc - 1 do
    begin
      C[i, j] := 0;
      for k := 0 to kc - 1 do
        C[i, j] := C[i, j] + A[i, k]*B[k, j];
    end;
end;

procedure MatInv(var A: TMatrix; Size: Integer);
const
  MatSize = 100;
var
  i, j, ok: integer;
  // This intermediate matrix is required because the inversion function
  // in NumLib only works when the indexes start in 1 (ie, cannot be dynamic)
  B: Array [1..MatSize, 1..MatSize] of ArbFloat;
begin
  assert(Size <= MatSize, 'Error: The degree of the polynomial is too high.');

  for i := 1 to Size do
    for j := 1 to Size do
      B[i, j] := A[i - 1, j - 1];

  // Calculate (B)-¹ in place
  invgen(Size, MatSize, B[1, 1], ok);

  assert(ok = 1, 'Error: The matrix is singular.');

  for i := 1 to Size do
    for j := 1 to Size do
      A[i - 1, j - 1] := B[i, j];
end;

var
  i, j, k, HalfKernel, Order, Size: Integer;
  r, t: Double;
  A, B, tA, tAA: TMatrix;
  Yold: Array of Double;
begin
  Size := Length(Points);
  assert(Size >= Kernel, 'Error: The curve does not have enough points for this kernel.');

  HalfKernel := Kernel div 2;
  // A polynomial of degree n has n + 1 coefficients
  Order := Degree + 1;

  SetLength(Yold, Size);
  for i := 0 to Size - 1 do
    Yold[i] := Points[i].Y;

  SetLength(A, Kernel, Order);
  SetLength(B, Order, Kernel);
  SetLength(tA, Order, Kernel);
  SetLength(tAA, Order, Order);

  // Do the smoothing thing
  for i := Low(Points) to High(Points) do
  begin
    // Build A and tA
    for j := -HalfKernel to HalfKernel do
    begin
      r := 1.0;
      t := Points[i].X - Points[Index(i + j, Low(Points), High(Points))].X;
      for k := 0 to Order - 1 do
      begin
        A[j + HalfKernel, k] := r;
        tA[k, j + HalfKernel] := r;
        r := r*t;
      end;
    end;

    // Calculate product tA.A
    MatMult(tA, A, tAA, Order, Kernel, Order);

    // Calculate (tA.A)-¹
    MatInv(tAA, Order);

    // Calculate product (tA.A)-¹.tA
    MatMult(tAA, tA, B, Order, Order, Kernel);

    // Compute the polynomial's value at the center of the sample
    Points[i].Y := 0.0;
    for j := -HalfKernel to HalfKernel do
      Points[i].Y := Points[i].Y + B[Deriv, j + HalfKernel]*
                     Yold[Index(i + j, Low(Points), High(Points))];
  end;
end;


end.

