unit curves;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses Classes, SysUtils, LazFileUtils, fgl, Graphics, coordinates, utils, DOM, math;

const
  HistItems = 20;

type
  TIntegerList = specialize TFPGList<Integer>;
  TFloatList = specialize TFPGList<Double>;

  TPointList = specialize TFPGList<TCurvePoint>;

  TCurve = class(TObject)
  protected
    { Protected declarations }
    FPoints: TPointList;
    FIsSorted: Boolean;
    FShowAsSymbols: Boolean;
  private
    { Private declarations }
    function GetCount: Integer;
    function GetXValues: TFloatList;
    function GetYValues: TFloatList;
    function GetX(Index: Integer): Double;
    function GetY(Index: Integer): Double;
    function GetPoint(Index: Integer): TCurvePoint;

    function GetMinX: Double;
    function GetMaxX: Double;
    function GetMinY: Double;
    function GetMaxY: Double;

    procedure SetX(Index: Integer; const Value: Double);
    procedure SetY(Index: Integer; const Value: Double);
    procedure SetPoint(Index: Integer; const Value: TCurvePoint);
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create;
    {@exclude}
    destructor Destroy; override;

    {Check if the value is within the range of X values.
        @param(XValue: Value to check.)
    }
    function IsInXRange(XValue: Double): Boolean;

    {Adds a point to the curve with the default values.
        @param(p: Point to add.)
    }
    procedure AddPoint(p: TCurvePoint); overload;
    {Adds a point to the curve with the specified values:
        @param(X: X value of the point.)
        @param(Y: Y value of the point.)
    }
    procedure AddPoint(Xv, Yv: Double); overload;
    {Inserts a point to the curve at the selected position with the specified values:
        @param(Position: Position of the point to be inserted.)
        @param(p: Point to insert.)
    }
    procedure InsertPoint(Position: Integer; p: TCurvePoint); overload;
    {Inserts a point to the curve at the selected position with the specified values:
        @param(Position: Position of the point to be inserted.)
        @param(X: X value of the point.)
        @param(Y: Y value of the point.)
    }
    procedure InsertPoint(Position: Integer; Xv, Yv: Double); overload;
    {Deletes the selected point in the curve:
        @param(PointNo: Number (position) of the point to be deleted.)
    }
    procedure DeletePoint(Index: Integer);
    {Deletes all points in the curve.}
    procedure Clear;
    {Deletes all points above a certain value:
        @param(Value: Upper limit.)
    }
    procedure DeletePointsAbove(Value: Double);
    {Deletes all points below a certain value:
        @param(Value: Lower limit.)
    }
    procedure DeletePointsBelow(Value: Double);
    {Deletes all points inside a certain region:
        @param(Region: Rectangular region.)
    }
    procedure DeletePointsInRegion(Region: TRect);
    {Group all points inside a certain region as one single point:
        @param(Region: Rectangular region.)
    }
    procedure GroupPointsInRegion(Region: TRect);
    {Sort the points in the curve.}
    procedure SortCurve;
    {Inverts the order of the points in the curve.}
    procedure InvertCurve;
    {Smooths the curve using the Savitzky-Golay algorithm:
        @param(k: Half size of the kernel: kernel = 2*k + 1.)
        @param(d: Degree of the polynomial, >= 2.)
    }
    procedure Smooth(k, d: Integer);
    {Interpolates new points using B-Splines:
        @param(n: Number of points.)
    }
    procedure Interpolate(n: Integer); overload;
    {Interpolates new points using B-Splines:
        @param(Xo: Lower limit.)
        @param(Xf: Upper limit.)
        @param(n: Number of points.)
    }
    procedure Interpolate(Xo, Xf: Double; n: Integer); overload;

    {Number of points in the curve}
    property Count: Integer read GetCount;
    {Is the curve sorted or not?}
    property IsSorted: Boolean read FIsSorted;
    {Show as symbols (the default is a line)}
    property ShowAsSymbols: Boolean read FShowAsSymbols write FShowAsSymbols;
    {Array containing X values in the curve}
    property XValues: TFloatList read GetXValues;
    {Array containing Y values in the curve}
    property YValues: TFloatList read GetYValues;
    {Array containing point values in the curve}
    property Points: TPointList read FPoints;
    {X value in the curve}
    property X[Index: Integer]: Double read GetX write SetX;
    {Y value in the curve}
    property Y[Index: Integer]: Double read GetY write SetY;
    {@definitionList(
        @itemLabel(Selected point:)
        @item(@bold(Index): Index of the point to be selected))
    }
    property Point[Index: Integer]: TCurvePoint read GetPoint write SetPoint; default;
  end;

  TStraightLine = class(TCurve)
  protected
    { Protected declarations }
    FColor: TColor;
  public
    { Public declarations }
    {@exclude}
    constructor Create;
    {Draws a line in the canvas from its first point to the last one:
        @param(Canvas: Canvas where the line will be drawn.)
    }
    procedure Draw(Canvas: TCanvas);

    {Return the line color}
    property Color: TColor read FColor write FColor;
  end;

  TIsland = class(TCurve)
  private
    { Private declarations }
    function GetMeanValue: TCurvePoint;
  public
    { Public declarations }
    property MeanValue: TCurvePoint read GetMeanValue;
    function Contains(p: TCurvePoint): Boolean; overload;
    function Contains(Xv, Yv: Double): Boolean; overload;
    {Adds a point to the curve with the default values.
        @param(Point: Point to add.)
    }
    procedure AddPoint(p: TCurvePoint); overload;
    {Adds a point to the curve with the specified values:
        @param(X: X value of the point.)
        @param(Y: Y value of the point.)
    }
    procedure AddPoint(Xv, Yv: Double); overload;
    {Inserts a point to the curve at the selected position with the default values:
        @param(Position: Position of the point to be inserted.)
    }
    procedure InsertPoint(Position: Integer; p: TCurvePoint); overload;
    {Inserts a point to the curve at the selected position with the specified values:
        @param(Position: Position of the point to be inserted.)
        @param(X: X value of the point.)
        @param(Y: Y value of the point.)
    }
    procedure InsertPoint(Position: Integer; Xv, Yv: Double); overload;
  end;

  TDigitCurve = class(TObject)
  protected
    { Protected declarations }
    FName: String;
    FCurves: Array [0..HistItems - 1] of TCurve;
    FCurveIndex: Integer;
    FValidCurves: Integer;
    FColor: TColor;

    FMarkers: TPointList;

    FStep: Integer;
    FInterval: Integer;
    FTolerance: Integer;
    FSpread: Integer;
  private
    function GetCount: Integer;
    function GetColor: TColor;
    function GetShowAsSymbols: Boolean;
    function GetCurve(Index: Integer): TCurve;
    function GetActiveCurve: TCurve;
    function GetColorIsSet: Boolean;
    function GetHasPoints: Boolean;
    function GetMarkerCount: Integer;
    function GetMarker(Index: Integer): TCurvePoint;

    procedure SetName(Value: String);
    procedure SetColor(Value: TColor);
    procedure SetShowAsSymbols(Value: Boolean);
    procedure SetCurveIndex(Value: Integer);
    procedure SetMarker(Index: Integer; const Value: TCurvePoint);
  public
    constructor Create(Name: String);
    destructor Destroy; override;

    procedure Reset;
    procedure Clear;
    procedure Draw(Canvas: TCanvas);
    function CurveRect: TRect;

    procedure CorrectCurve(Po, Pf: TCurvePoint; IsStep: Boolean = True);
    procedure DeletePointsInRegion(Region: TRect);
    procedure GroupPointsInRegion(Region: TRect);
    procedure AddToX(Value: Double);
    procedure AddToY(Value: Double);
    procedure MultiplyByX(Value: Double);
    procedure MultiplyByY(Value: Double);
    procedure DivideByX(Value: Double);
    procedure DivideByY(Value: Double);

    procedure AddMarker(P: TCurvePoint); overload;
    procedure AddMarker(Xp, Yp: Double); overload;
    procedure InsertMarker(Position: Integer; P: TCurvePoint); overload;
    procedure InsertMarker(Position: Integer; Xp, Yp: Double); overload;
    procedure DeleteMarker(Index: Integer);
    procedure ClearMarkers;
    procedure SortMarkers;

    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    procedure GoBack;
    procedure GoForward;
    procedure NextCurve(Copy: Boolean = True);
    procedure SetNewCurve(Value: TCurve);
    procedure UnsetActiveCurve;
    procedure UnsetAllCurves;

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    {Return the name of the curve}
    property Name: String read FName write SetName;
    {Return the number of curves (fixed)}
    property Count: Integer read GetCount;
    {Return the index of the active curve}
    property CurveIndex: Integer read FCurveIndex write SetCurveIndex;
    {Return the curve}
    property Curves[Index: Integer]: TCurve read GetCurve; default;
    {Return the active curve}
    property Curve: TCurve read GetActiveCurve;
    {Return the color of the active curve}
    property Color: TColor read GetColor write SetColor;
    {Return whether the active curve should be plotted as symbols or lines}
    property ShowAsSymbols: Boolean read GetShowAsSymbols write SetShowAsSymbols;

    property MarkerCount: Integer read GetMarkerCount;
    property Markers[Index: Integer]: TCurvePoint read GetMarker write SetMarker;

    property Step: Integer read FStep write FStep;
    property Interval: Integer read FInterval write FInterval;
    property Tolerance: Integer read FTolerance write FTolerance;
    property Spread: Integer read FSpread write FSpread;

    property ColorIsSet: Boolean read GetColorIsSet;
    property HasPoints: Boolean read GetHasPoints;
  end;

implementation


//=================================| TCurve |=================================//
constructor TCurve.Create;
begin
  FPoints := TPointList.Create;
  FPoints.Clear;
  //Don't waste time sorting an empty list
  FIsSorted := True;
  FShowAsSymbols := False;
end;

destructor  TCurve.Destroy;
begin
  FPoints.Free;
  inherited;
end;

function TCurve.IsInXRange(XValue: Double): Boolean;
begin
  Result := (Count > 0) and (XValue >= GetMinX) and (XValue <= GetMaxX);
end;

function TCurve.GetCount: Integer;
begin
  Result := FPoints.Count;
end;

procedure TCurve.AddPoint(p: TCurvePoint);
begin
  Points.Add(p);

  if (Count <= 1) then
    FIsSorted := True
  else
    FIsSorted := FIsSorted and (X[Count - 1] >= X[Count - 2]);
end;

procedure TCurve.AddPoint(Xv, Yv: Double);
begin
  AddPoint(GetCurvePoint(Xv, Yv));
end;

procedure TCurve.InsertPoint(Position: Integer; p: TCurvePoint);
begin
  Points.Insert(Position, p);

  if (Count <= 1) then
    FIsSorted := True
  else if Position = 0 then
    FIsSorted := FIsSorted and (X[0] < X[1])
  else if Position >= Count - 2 then
    FIsSorted := FIsSorted and (X[Count - 1] >= X[Count - 2])
  else
    FIsSorted := FIsSorted and (X[Position - 1] < X[Position]) and
                               (X[Position] < X[Position + 1]);
end;

procedure TCurve.InsertPoint(Position: Integer; Xv, Yv: Double);
begin
  InsertPoint(Position, GetCurvePoint(Xv, Yv));
end;

procedure TCurve.DeletePoint(Index: Integer);
begin
  Points.Delete(Index);
end;

procedure TCurve.Clear;
begin
  Points.Clear;
  FShowAsSymbols := False;
end;

procedure TCurve.SetX(Index: Integer; const Value: Double);
begin
  Point[Index] := GetCurvePoint(Value, Y[Index]);

  if (Count <= 1) then
    FIsSorted := True
  else if Index = 0 then
    FIsSorted := FIsSorted and (X[0] < X[1])
  else if Index >= Count - 2 then
    FIsSorted := FIsSorted and (X[Count - 1] >= X[Count - 2])
  else
    FIsSorted := FIsSorted and (X[Index - 1] < X[Index]) and
                               (X[Index] < X[Index + 1]);
end;

procedure TCurve.SetY(Index: Integer; const Value: Double);
begin
  Point[Index] := GetCurvePoint(X[Index], Value);
end;

procedure TCurve.SetPoint(Index: Integer; const Value: TCurvePoint);
begin
  FPoints[Index] := Value;

  if (Count <= 1) then
    FIsSorted := True
  else if Index = 0 then
    FIsSorted := FIsSorted and (X[0] < X[1])
  else if Index = Count - 1 then
    FIsSorted := FIsSorted and (X[Count - 1] >= X[Count - 2])
  else
    FIsSorted := FIsSorted and (X[Index - 1] < X[Index]) and
                               (X[Index] < X[Index + 1]);
end;

function TCurve.GetXValues: TFloatList;
var
  i:  Integer;
begin
  Result := TFloatList.Create;
  Result.Clear;

  for i := 0 to Count - 1 do
    Result.Add(X[i]);
end;

function TCurve.GetYValues: TFloatList;
var
  i:  Integer;
begin
  Result := TFloatList.Create;
  Result.Clear;

  for i := 0 to Count - 1 do
    Result.Add(Y[i]);
end;

function TCurve.GetX(Index: Integer): Double;
begin
  Result := Point[Index].X;
end;

function TCurve.GetY(Index: Integer): Double;
begin
  Result := Point[Index].Y;
end;

function TCurve.GetPoint(Index: Integer): TCurvePoint;
begin
  Result := FPoints[Index];
end;

function TCurve.GetMinX: Double;
var
  i: Integer;
begin
  if (Count > 0) then
  begin
    if IsSorted then
      Result := X[0]
    else
    begin
      Result := X[0];
      for i := 1 to Count - 1 do
        if (X[i] < Result) then
          Result := X[i];
    end;
  end
  else
    Result := 0;
end;

function TCurve.GetMaxX: Double;
var
  i: Integer;
begin
  if (Count > 0) then
  begin
    if IsSorted then
      Result := X[Count - 1]
    else
    begin
      Result := X[0];
      for i := 1 to Count - 1 do
        if (X[i] > Result) then
          Result := X[i];
    end;
  end
  else
    Result := 0;
end;

function TCurve.GetMinY: Double;
var
  i: Integer;
begin
  if (Count > 0) then
  begin
    Result := Y[0];
    for i := 1 to Count - 1 do
      if (Y[i] < Result) then
        Result := Y[i];
  end
  else
    Result := 0;
end;

function TCurve.GetMaxY: Double;
var
  i: Integer;
begin
  if (Count > 0) then
  begin
    Result := Y[0];
    for i := 1 to Count - 1 do
      if (Y[i] > Result) then
        Result := Y[i];
  end
  else
    Result := 0;
end;

{procedure TCurve.LoadFromFile(FileName: TFileName);
var
  i: Integer;
begin
  if FileExists(FileName) then
  begin
    Self.Clear;
    try
      FCSVDoc.LoadFromFile(FileName);

      for i := 0 to FCSVDoc.RowCount - 1 do
        Self.AddPoint(FCSVDoc.CellValues[FOptions.XCol, i],
                      FCSVDoc.CellValues[FOptions.YCol, i]);
     finally
     end;
  end;
end;

procedure TCurve.LoadFromFile(FileName: TFileName; Options: TCSVOptions);
begin
  FOptions := Options;

  FCSVDoc.Delimiter := FOptions.Delimiter;
  FCSVDoc.Comment := FOptions.Comment;
  FCSVDoc.QuoteChar := FOptions.Quotation;
  FCSVDoc.HeaderLines := FOptions.HeaderLines;
  FCSVDoc.DecimalChar := FOptions.Decimal;
  FCSVDoc.ThousandChar := FOptions.Thousand;

  LoadFromFile(FileName);
end;}

procedure TCurve.SortCurve;
begin
  Points.Sort(@TCurvePointComparator);
  FIsSorted := True;
end;

procedure TCurve.InvertCurve;
var
  i   : Integer;
  Tmp : TCurvePoint;
begin
  for i := 0 to (Count div 2) - 1 do
  begin
    Tmp := Point[i];
    Point[i] := Point[Count - 1 - i];
    Point[Count - 1 - i] := Tmp
  end;
  FIsSorted := False;
end;

procedure TCurve.DeletePointsAbove(Value: Double);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if X[i] > Value then
      DeletePoint(i);
end;

procedure TCurve.DeletePointsBelow(Value: Double);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if X[i] < Value then
      DeletePoint(i);
end;

procedure TCurve.DeletePointsInRegion(Region: TRect);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if Region.Contains(TPoint.Create(Round(X[i]), Round(Y[i]))) then
      DeletePoint(i);
end;

procedure TCurve.GroupPointsInRegion(Region: TRect);
var
  i, n: Integer;
  p: TCurvePoint;
begin
  n := 0;
  p := GetCurvePoint(0, 0);
  for i := Count - 1 downto 0 do
    if Region.Contains(TPoint.Create(Round(X[i]), Round(Y[i]))) then
    begin
      p := p + Point[i];
      inc(n);

      DeletePoint(i);
    end;

  if (n > 0) then
  begin
    AddPoint(p/n);
    SortCurve;
  end;
end;

procedure TCurve.Smooth(k, d: Integer);
var
 i: Integer;
 Pnts: Array of TCurvePoint;
begin
  if (k >= 1) and (d >= 2) then
  begin
    if not IsSorted then SortCurve;

    SetLength(Pnts, Count);
    for i := 0 to Count - 1 do
      Pnts[i] := Point[i];

    SavitzkyGolay(2*k + 1, d, 0, Pnts);
    Clear;
    for i := Low(Pnts) to High(Pnts) do
      Points.Add(Pnts[i]);
  end;
end;

procedure TCurve.Interpolate(n: Integer);
begin
  if (n > 2) then
    Interpolate(GetMinX, GetMaxX, n)
end;

procedure TCurve.Interpolate(Xo, Xf: Double; n: Integer);
const
 Degree = 3;
var
 i: Integer;
 dx, Xint, Yint: Double;

 Pnts: Array of TCurvePoint;
begin
  if (n > 2) then
  begin
    if not IsSorted then SortCurve;

    if (Xo < X[0]) then
      InsertPoint(0, Xo, Y[0]);

    if (Xf > X[Count - 1]) then
      AddPoint(Xf, Y[Count - 1]);

    SetLength(Pnts, Count);
    for i := 0 to Count - 1 do
      Pnts[i] := Point[i];

    Clear;
    dx := (Xf - Xo)/(n - 1);
    for i := 0 to n - 1 do
    begin
      Xint := Xo + i*dx;
      Yint :=  BSpline(Pnts, Degree, Xint);
      AddPoint(Xint, Yint);
    end;
  end;
end;



//=============================| TStraightLine |==============================//
constructor TStraightLine.Create;
begin
  inherited;
  FColor := -1;
end;

procedure TStraightLine.Draw(Canvas: TCanvas);
var
  i: Integer;
begin
  //Draw the straigh line in the image
  if (Points.Count >= 2) then
  begin
    if not IsSorted then
      SortCurve;

    with Canvas do
    begin
      Pen.Mode := pmCopy;
      Pen.Color := Color;

      MoveTo(Round(X[0]), Round(Y[0]));
      LineTo(Round(X[Points.Count - 1]), Round(Y[Points.Count - 1]));
    end;
  end;
end;




//================================| TIsland |=================================//
function TIsland.GetMeanValue: TCurvePoint;
var
  i: Integer;
begin
  Result := GetCurvePoint(0, 0);

  if (Count > 0) then
  begin
    for i := 0 to Count - 1 do
      Result := Result + Point[i];

    Result := Result/Count;
  end;
end;

function TIsland.Contains(p: TCurvePoint): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count > 0 then
  begin
    i := 0;
    repeat
      //Result := (Sqrt(Power(X[i] - p.X, 2) + Power(Y[i] - p.Y, 2)) < 1.5);
      Result := (Round(X[i]) = Round(p.X)) and (Round(Y[i]) = Round(p.Y));
      inc(i);
    until Result or (i >= Count);
  end;
end;

function TIsland.Contains(Xv, Yv: Double): Boolean;
begin
  Result := Contains(GetCurvePoint(Xv, Yv));
end;

procedure TIsland.AddPoint(p: TCurvePoint);
begin
  if not Contains(p) then
    inherited AddPoint(p);
end;

procedure TIsland.AddPoint(Xv, Yv: Double);
begin
  if not Contains(Xv, Yv) then
    inherited AddPoint(Xv, Yv);
end;

procedure TIsland.InsertPoint(Position: Integer; p: TCurvePoint);
begin
  if not Contains(p) then
    inherited InsertPoint(Position, p);
end;

procedure TIsland.InsertPoint(Position: Integer; Xv, Yv: Double);
begin
  if not Contains(Xv, Yv) then
    inherited InsertPoint(Position, Xv, Yv);
end;

//================================| TIsland |=================================//


//==============================| TDigitCurve |===============================//
constructor TDigitCurve.Create(Name: String);
var
  i: Integer;
begin
  FName := Name;

  for i := 0 to HistItems - 1 do
    FCurves[i] := TCurve.Create;

  FMarkers := TPointList.Create;

  Reset;
end;

destructor TDigitCurve.Destroy;
var
  i: Integer;
begin
  for i := 0 to HistItems - 1 do
    FCurves[i].Free;

  FMarkers.Free;

  inherited;
end;

procedure TDigitCurve.Reset;
var
  i: Integer;
begin
  for i := 0 to HistItems - 1 do
    FCurves[i].Clear;

  FMarkers.Clear;

  FCurveIndex := 0;
  FValidCurves := 1;
  FColor := -1;

  FStep := 1;
  FInterval := 50;
  FTolerance := 32;
  FSpread := 4;
end;

procedure TDigitCurve.Clear;
begin
  NextCurve(False);
end;

procedure TDigitCurve.CorrectCurve(Po, Pf: TCurvePoint; IsStep: Boolean = True);
var
  i, idXo, idXf: Integer;
  Delta: Double;
begin
  NextCurve(True);
  with Curve do
  begin
    if Po.X > Pf.X then
    begin
      idXf := 0;
      idXo := Count - 1;
    end
    else
    begin
      idXo := 0;
      idXf := Count - 1;
    end;
    for i := 0 to Count - 1 do
    begin
      if (i < (Count - 1)) then
      begin
         if (Po.X >= X[i]) and (Po.X < X[i + 1]) then
         begin
           Po.X := X[i];
           idXo := i;
         end;
         if (Pf.X > X[i]) and (Pf.X <= X[i + 1]) then
         begin
           Pf.X := X[i + 1];
           idXf := i + 1;
         end;
      end;
    end;
    //Correct step from right to left
    if Po.X > Pf.X then
    begin
      if IsStep then
      begin
        Delta := Y[idXf] - Pf.Y;
        for i := idXo downto 0 do
        begin
            if (i >= idXf) then
              Y[i] := Y[idXo] + (X[i] - X[idXo])*(Pf.Y - Y[idXo])/(X[idXf] - X[idXo])
            else
              Y[i] := Y[i] - Delta;
        end;
      end
      else
        for i := idXo downto idXf do
          Y[i] := Y[idXo] + (X[i] - X[idXo])*(Y[idXf] - Y[idXo])/(X[idXf] - X[idXo])
    end
    //Correct step from left to right
    else
    begin
      if IsStep then
      begin
        Delta := Y[idXf] - Pf.Y;
        for i := idXo to Count - 1 do
        begin
          if (i <= idXf) then
            Y[i] := Y[idXo] + (X[i] - X[idXo])*(Pf.Y - Y[idXo])/(X[idXf] - X[idXo])
          else
            Y[i] := Y[i] - Delta;
        end;
      end
      else
        for i := idXo to idXf do
          Y[i] := Y[idXo] + (X[i] - X[idXo])*(Y[idXf] - Y[idXo])/(X[idXf] - X[idXo])
    end;
  end;
end;

procedure TDigitCurve.DeletePointsInRegion(Region: TRect);
begin
  NextCurve(True);
  Curve.DeletePointsInRegion(Region);
end;

procedure TDigitCurve.GroupPointsInRegion(Region: TRect);
begin
  NextCurve(True);
  Curve.GroupPointsInRegion(Region);
end;

procedure TDigitCurve.AddToX(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.X[i] := Curve.X[i] + Value;
end;

procedure TDigitCurve.AddToY(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.Y[i] := Curve.Y[i] + Value;
end;

procedure TDigitCurve.MultiplyByX(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.X[i] := Curve.X[i]*Value;
end;

procedure TDigitCurve.MultiplyByY(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.Y[i] := Curve.Y[i]*Value;
end;

procedure TDigitCurve.DivideByX(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.X[i] := Value/Curve.X[i];
end;

procedure TDigitCurve.DivideByY(Value: Double);
var i: Integer;
begin
  NextCurve(True);
  for i := 0 to Curve.Count - 1 do
    Curve.Y[i] := Value/Curve.Y[i];
end;

procedure TDigitCurve.AddMarker(P: TCurvePoint);
begin
  FMarkers.Add(P);
end;

procedure TDigitCurve.AddMarker(Xp, Yp: Double);
begin
  FMarkers.Add(GetCurvePoint(Xp, Yp));
end;

procedure TDigitCurve.InsertMarker(Position: Integer; P: TCurvePoint);
begin
  FMarkers.Insert(Position, P);
end;

procedure TDigitCurve.InsertMarker(Position: Integer; Xp, Yp: Double);
begin
  FMarkers.Insert(Position, GetCurvePoint(Xp, Yp));
end;

procedure TDigitCurve.DeleteMarker(Index: Integer);
begin
  FMarkers.Delete(Index);
end;

procedure TDigitCurve.ClearMarkers;
begin
  FMarkers.Clear;
end;

procedure TDigitCurve.SortMarkers;
begin
  FMarkers.Sort(@TCurvePointComparator);
end;

function TDigitCurve.GetCount: Integer;
begin
  Result := HistItems;
end;

function TDigitCurve.GetColor: TColor;
begin
  Result := FColor
end;

function TDigitCurve.GetShowAsSymbols: Boolean;
begin
  Result := assigned(Curve) and Curve.ShowAsSymbols;
end;

function TDigitCurve.GetCurve(Index: Integer): TCurve;
begin
  if (Index >= 0) and (Index < FValidCurves) then
    Result := FCurves[Index]
  else
    Result := nil;
end;

function TDigitCurve.GetActiveCurve: TCurve;
begin
  Result := GetCurve(CurveIndex);
end;

function TDigitCurve.GetColorIsSet: Boolean;
begin
  Result := FColor >= 0;
end;

function TDigitCurve.GetHasPoints: Boolean;
begin
  Result := Curve.Count > 0;
end;

function TDigitCurve.GetMarkerCount: Integer;
begin
  Result := FMarkers.Count;
end;

function TDigitCurve.GetMarker(Index: Integer): TCurvePoint;
begin
  if (Index >= 0) and (Index < FMarkers.Count) then
    Result := FMarkers[Index]
  else
    Result := GetCurvePoint(0, 0);
end;

procedure TDigitCurve.SetName(Value: String);
begin
  FName := Value;
end;

procedure TDigitCurve.SetColor(Value: TColor);
begin
  FColor := Value;
end;

procedure TDigitCurve.SetShowAsSymbols(Value: Boolean);
begin
  if assigned(Curve) then
    Curve.ShowAsSymbols := Value;
end;

procedure TDigitCurve.SetCurveIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FValidCurves) then
    FCurveIndex := Value;
end;

procedure TDigitCurve.SetMarker(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 0) and (Index < FMarkers.Count) then
    FMarkers[Index] := Value;
end;

procedure TDigitCurve.Draw(Canvas: TCanvas);
var
  i: Integer;
begin
  //Draw the curve in the image
  if (Curve.Count > 0) then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNot;
      with Curve do
      begin
        MoveTo(Round(X[0]), Round(Y[0]));
        for i := 1 to Count - 1 do
          LineTo(Round(X[i]), Round(Y[i]));
      end;
    end;
  end;
end;

function TDigitCurve.CurveRect: TRect;
var
  i,
  Xo, Xf,
  Yo, Yf: Integer;
begin
  if (Curve.Count > 0) then
  begin
    Xo := Round(Curve.X[0]);
    Xf := Round(Curve.X[0]);
    Yo := Round(Curve.Y[0]);
    Yf := Round(Curve.Y[0]);

    for i := 1 to Curve.Count - 1 do
    begin
      if (Round(Curve.X[i]) < Xo) then
        Xo := Round(Curve.X[i]);
      if (Round(Curve.X[i]) > Xf) then
        Xf := Round(Curve.X[i]);
      if (Round(Curve.Y[i]) < Yo) then
        Yo := Round(Curve.Y[i]);
      if (Round(Curve.Y[i]) > Yf) then
        Yf := Round(Curve.Y[i]);
    end;

    Result := TRect.Create(Xo, Yo, Xf, Yf);
  end
  else
    Result := TRect.Create(0, 0, 0, 0);
end;

function TDigitCurve.CanGoBack: Boolean;
begin
  Result := (FValidCurves > 1) and (CurveIndex > 0);
end;

function TDigitCurve.CanGoForward: Boolean;
begin
  Result := (FValidCurves > 1) and (CurveIndex < (FValidCurves - 1));
end;

procedure TDigitCurve.GoBack;
begin
  if CanGoBack then
    dec(FCurveIndex);
end;

procedure TDigitCurve.GoForward;
begin
  if CanGoForward then
    inc(FCurveIndex);
end;

procedure TDigitCurve.NextCurve(Copy: Boolean = True);
var
  i: Integer;
begin
  // We have available slots, just use them
  if (CurveIndex < (HistItems - 1)) then
  begin
    inc(FCurveIndex);
    FValidCurves := FCurveIndex + 1;
  end
  // Set a new curve
  else
    SetNewCurve(TCurve.Create);

  Curve.Clear;
  Curve.ShowAsSymbols := Curves[CurveIndex - 1].ShowAsSymbols;
  // Set all the values in the next curve
  if Copy then
  begin
    for i := 0 to Curves[CurveIndex - 1].Count - 1 do
      Curve.AddPoint(Curves[CurveIndex - 1].Point[i]);
  end;
end;

procedure TDigitCurve.SetNewCurve(Value: TCurve);
var
  i: Integer;
begin
  // We have available slots, just use them
  if (CurveIndex < (HistItems - 1)) then
  begin
    inc(FCurveIndex);
    FValidCurves := FCurveIndex + 1;
    Curve.Free;
    FCurves[FCurveIndex] := Value;
  end
  // Make room for the new curve
  else
  begin
    FCurves[0].Free;
    for i := 1 to HistItems - 1 do
      FCurves[i - 1] := FCurves[i];
    FCurves[HistItems - 1] := Value;
    // Just in case
    FCurveIndex := HistItems - 1;
    FValidCurves := HistItems;
  end;
end;

procedure TDigitCurve.UnsetActiveCurve;
begin
  if (CurveIndex >= 0) then
  begin
    dec(FCurveIndex);
    FValidCurves := CurveIndex + 1;
  end;
end;

procedure TDigitCurve.UnsetAllCurves;
var
  i: Integer;
begin
  FCurveIndex := 0;
  FValidCurves := 1;
  for i := 0 to HistItems - 1 do
    Curves[i].Clear;
end;

function TDigitCurve.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, j,
  SavedMarkerCount,
  RealMarkerCount,
  SavedPointCount,
  RealPointCount: Integer;
  P: TCurvePoint;
  Child: TDOMNode;
begin
  Result := False;

  try
    //Reset curve
    Reset;

    SavedMarkerCount := 0;
    SavedPointCount := 0;
    for i := 0 to Item.Attributes.Length - 1 do
    begin
      if (Item.Attributes.Item[i].CompareName('Name') = 0) then
        Name := UTF8Encode(Item.Attributes.Item[i].NodeValue);
      if (Item.Attributes.Item[i].CompareName('Color') = 0) then
        Color := StrToInt(UTF8Encode('$' + Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('ShowAsSymbols') = 0) then
        ShowAsSymbols := StrToBool(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('Step') = 0) then
        Step := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('Interval') = 0) then
        Interval := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('Tolerance') = 0) then
        Tolerance := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('Spread') = 0) then
        Spread := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('MarkerCount') = 0) then
        SavedMarkerCount := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
      if (Item.Attributes.Item[i].CompareName('PointCount') = 0) then
        SavedPointCount := StrToInt(UTF8Encode(Item.Attributes.Item[i].NodeValue));
    end;

    RealPointCount := 0;
    RealMarkerCount := 0;
    Child := Item.FirstChild;
    while Assigned(Child) do
    begin
      // Curve markers and  points
      if (Child.CompareName('marker') = 0) or (Child.CompareName('point') = 0) then
      begin
        P.X := 0;
        P.Y := 0;
        for j := 0 to Child.Attributes.Length - 1 do
        begin
          if (Child.Attributes.Item[j].CompareName('X') = 0) then
            P.X := StrToFloat(UTF8Encode(Child.Attributes.Item[j].NodeValue));
          if (Child.Attributes.Item[j].CompareName('Y') = 0) then
            P.Y := StrToFloat(UTF8Encode(Child.Attributes.Item[j].NodeValue));
        end;

        if (Child.CompareName('marker') = 0) then
        begin
          AddMarker(P);
          inc(RealMarkerCount);
        end
        else
        begin
          FCurves[FCurveIndex].AddPoint(P);
          inc(RealPointCount);
        end;
      end;

      // Go for the next marker or point
      Child := Child.NextSibling;
    end;

    Result := True;
  except
    //Do nothing, just catch the exception
  end;

  assert(SavedPointCount = RealPointCount,
         'Error: The number of points found (' + IntToStr(RealPointCount) +
         ') is different from the expected (' + IntToStr(SavedPointCount) + ').');

  assert(SavedMarkerCount = RealMarkerCount,
         'Error: The number of markers found (' + IntToStr(RealMarkerCount) +
         ') is different from the expected (' + IntToStr(SavedMarkerCount) + ').');
end;

function TDigitCurve.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  PointNode, MarkerNode: TDOMNode;
begin
  Result := Doc.CreateElement('curve');
  TDOMElement(Result).SetAttribute('Name', UTF8Decode(Name));
  TDOMElement(Result).SetAttribute('Color', UTF8Decode(IntToHex(Color, 6)));
  TDOMElement(Result).SetAttribute('ShowAsSymbols', UTF8Decode(BoolToStr(ShowAsSymbols)));
  TDOMElement(Result).SetAttribute('Step', UTF8Decode(IntToStr(Step)));
  TDOMElement(Result).SetAttribute('Interval', UTF8Decode(IntToStr(Interval)));
  TDOMElement(Result).SetAttribute('Tolerance', UTF8Decode(IntToStr(Tolerance)));
  TDOMElement(Result).SetAttribute('Spread', UTF8Decode(IntToStr(Spread)));
  TDOMElement(Result).SetAttribute('MarkerCount', UTF8Decode(IntToStr(MarkerCount)));
  with FCurves[FCurveIndex] do
  begin
    TDOMElement(Result).SetAttribute('PointCount', UTF8Decode(IntToStr(Count)));
    for i := 0 to MarkerCount - 1 do
    begin
      MarkerNode := Doc.CreateElement('marker');
      TDOMElement(MarkerNode).SetAttribute('X', UTF8Decode(FloatToStr(Markers[i].X)));
      TDOMElement(MarkerNode).SetAttribute('Y', UTF8Decode(FloatToStr(Markers[i].Y)));
      Result.Appendchild(MarkerNode);
    end;
    for i := 0 to Count - 1 do
    begin
      PointNode := Doc.CreateElement('point');
      TDOMElement(PointNode).SetAttribute('X', UTF8Decode(FloatToStr(X[i])));
      TDOMElement(PointNode).SetAttribute('Y', UTF8Decode(FloatToStr(Y[i])));
      Result.Appendchild(PointNode);
    end;
  end;
end;

end.
