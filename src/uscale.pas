unit uscale;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Fgl, DOM, Math, ucoordinates, upolygons, ucurves;

type
  TScale = class(TObject)
  protected type
    TCurveList = specialize TFPGObjectList<TDigitCurve>;
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

    FOnChange: TNotifyEvent;

    function GetImagePoint(Index: Integer): TCurvePoint;
    function GetPlotPoint(Index: Integer): TCurvePoint;
    function GetPointIsSet(Index: Integer): Boolean;
    function GetPointsAreSet: Boolean;
    function GetIsValidScale: Boolean;

    procedure SetCoordSystem(const Value: TCoordSystem);
    procedure SetXScale(const Value: TScaleType);
    procedure SetYScale(const Value: TScaleType);
    procedure SetXLabel(const Value: String);
    procedure SetYLabel(const Value: String);
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
    property XScale: TScaleType read FXScale write SetXScale;
    property YScale: TScaleType read FYScale write SetYScale;
    property XLabel: String read FXLabel write SetXLabel;
    property YLabel: String read FYLabel write SetYLabel;
    property ImagePoints: TScalePoints read FPntImg;
    property PlotPoints: TScalePoints read FPntPlt;

    property ImagePoint[Index: Integer]: TCurvePoint read GetImagePoint write SetImagePoint;
    property PlotPoint[Index: Integer]: TCurvePoint read GetPlotPoint write SetPlotPoint;
    property PointIsSet[Index: Integer]: Boolean read GetPointIsSet;
    property AllPointsAreSet: Boolean read GetPointsAreSet;
    property IsValid: Boolean read GetIsValidScale;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPlot = class(TObject)
  protected type
    TCurveList = specialize TFPGObjectList<TDigitCurve>;
  private
    { Private declarations }
    FName: String;

    FScale: TScale;

    FBox: TPlotQuad;

    FCurves: TCurveList;
    FCurveIndex: Integer;

    FOnChange: TNotifyEvent;

    function GetCurveCount: Integer;
    function GetCurve(Index: Integer): TDigitCurve;
    function GetActiveCurve: TCurve;
    function GetDigitCurve: TDigitCurve;
    function GetPlotCurves(Index: Integer): TCurve;
    function GetPlotCurve: TCurve;

    procedure SetCurveIndex(Value: Integer);

    procedure SetName(Value: String);

    procedure ChildChange(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    {@exclude}
    constructor Create(Name: String);
    {@exclude}
    destructor Destroy; override;

    procedure Reset;

    function MoveCurve(FromIdx, ToIdx: Integer): Boolean;

    procedure AddCurve; overload;
    procedure AddCurve(Position: Integer); overload;
    procedure DeleteCurve; overload;
    procedure DeleteCurve(Index: Integer); overload;

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    {Return the name of the plot}
    property Name: String read FName write SetName;

    {Return the scale}
    property Scale: TScale read FScale;

    {Return the box}
    property Box: TPlotQuad read FBox;

    {Return the number of curves}
    property CurveCount: Integer read GetCurveCount;
    {Return the index of the active curve}
    property CurveIndex: Integer read FCurveIndex write SetCurveIndex;
    {Return the curve}
    property Curves[Index: Integer]: TDigitCurve read GetCurve; default;
    {Return the active curve (TCurve)}
    property Curve: TCurve read GetActiveCurve;
    {Return the active curve (TDigitCurve)}
    property DigitCurve: TDigitCurve read GetDigitCurve;
    {Return the given curve converted to plot scale}
    property PlotCurves[Index: Integer]: TCurve read GetPlotCurves;
    {Return the active curve converted to plot scale}
    property PlotCurve: TCurve read GetPlotCurve;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

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
    ImagePoint[i] := TCurvePoint.Create(0, 0);
    PlotPoint[i] := TCurvePoint.Create(0, 0);

    FImgPointIsSet[i] := False;
    FPltPointIsSet[i] := False;
  end;
end;

function TScale.FromImgToPlot(p: TCurvePoint): TCurvePoint;
var
  i: Integer;
  pnt: TCurvePoint;
  TmpPlotPoint: TScalePoints;
  Y0: Double;
begin
  if AllPointsAreSet then
  begin
    TmpPlotPoint := PlotPoints;
    Y0 := PlotPoints[2].Y;

    // Make sure that the basis is linear in X
    case XScale of
      stLog: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := log10(TmpPlotPoint[i].X);
      end;
      stLn: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := ln(TmpPlotPoint[i].X);
      end;
      stInverse: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := 1/TmpPlotPoint[i].X;
      end;
    end;

    // Make sure that the basis is linear in Y
    case YScale of
      stLog: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := log10(TmpPlotPoint[i].Y);

        Y0 := log10(Y0);
      end;
      stLn: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := ln(TmpPlotPoint[i].Y);

        Y0 := ln(Y0);
      end;
      stInverse: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := 1/TmpPlotPoint[i].Y;

        Y0 := 1/Y0;
      end;
    end;

    // Make sure that the basis is in cartesian coordinates
    if (CoordSystem = csPolar) then
    begin
      for i := 1 to 3 do
        TmpPlotPoint[i] := PolarToCartesian(TmpPlotPoint[i].X, TmpPlotPoint[i].Y - Y0);
    end;

    // Change the basis
    pnt := ChangeCoords(ImagePoints, TmpPlotPoint, p);

    // Make sure that the point is in the right coordinate system
    if (CoordSystem = csPolar) then
    begin
      pnt := CartesianToPolar(pnt);
      pnt.Y := Y0 + pnt.Y;
    end;

    // Make sure that X is in the right scale
    case XScale of
      stLog: pnt.X := Power(10, pnt.X);
      stLn: pnt.X := Exp(pnt.X);
      stInverse: pnt.X := 1.0/pnt.X;
    end;

    // Make sure that Y is in the right scale
    case YScale of
      stLog: pnt.Y := Power(10, pnt.Y);
      stLn: pnt.Y := Exp(pnt.Y);
      stInverse: pnt.Y := 1.0/pnt.Y;
    end;

    Result := pnt;
  end
  else
    Result := TCurvePoint.Create(0, 0);
end;

function TScale.FromImgToPlot(X, Y: Double): TCurvePoint;
begin
  Result := FromImgToPlot(TCurvePoint.Create(X, Y));
end;

function TScale.FromPlotToImg(p: TCurvePoint): TCurvePoint;
var
  i: Integer;
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
    case XScale of
      stLog: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := log10(TmpPlotPoint[i].X);

        pnt.X := log10(pnt.X);
      end;
      stLn: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := ln(TmpPlotPoint[i].X);

        pnt.X := ln(pnt.X);
      end;
      stInverse: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].X := 1/TmpPlotPoint[i].X;

        pnt.X := 1.0/pnt.X;
      end;
    end;

    // Make sure that Y is linear
    case YScale of
      stLog: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := log10(TmpPlotPoint[i].Y);

        Y0 := log10(Y0);
        pnt.Y := log10(pnt.Y);
      end;
      stLn: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := ln(TmpPlotPoint[i].Y);

        Y0 := ln(Y0);
        pnt.Y := ln(pnt.Y);
      end;
      stInverse: begin
        for i := 1 to 3 do
          TmpPlotPoint[i].Y := 1/TmpPlotPoint[i].Y;

        Y0 := 1/Y0;
        pnt.Y := 1/pnt.Y;
      end;
    end;

    // Make sure that the basis and the point are in cartesian coordinates
    if (CoordSystem = csPolar) then
    begin
      for i := 1 to 3 do
        TmpPlotPoint[i] := PolarToCartesian(TmpPlotPoint[i].X, TmpPlotPoint[i].Y - Y0);

      pnt := PolarToCartesian(pnt.X, pnt.Y - Y0);
    end;

    // Change the basis
    Result := ChangeCoords(TmpPlotPoint, ImagePoints, pnt);
  end
  else
    Result := TCurvePoint.Create(0, 0);
end;

function TScale.FromPlotToImg(X, Y: Double): TCurvePoint;
begin
  Result := FromPlotToImg(TCurvePoint.Create(X, Y));
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
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + TCurvePoint.Create(1, 0)) - Pi)
end;

function TScale.Ny(Pi: TCurvePoint): TCurvePoint;
begin
  Result := Normalize(FromPlotToImg(FromImgToPlot(Pi) + TCurvePoint.Create(0, 1)) - Pi)
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
  if (Index in [1..3]) then
    Result := FPntImg[Index]
end;

function TScale.GetPlotPoint(Index: Integer): TCurvePoint;
begin
  if (Index in [1..3]) then
    Result := FPntPlt[Index];
end;

function TScale.GetPointIsSet(Index: Integer): Boolean;
begin
  if (Index in [1..3]) then
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
var
  i: Integer;
begin
  if (Value <> FCoords) then
  begin
    FCoords := Value;
    if (FCoords = csCartesian) then
      for i := 1 to 3 do
        PlotPoint[i] := PolarToCartesian(PlotPoint[i])
    else
      for i := 1 to 3 do
        PlotPoint[i] := CartesianToPolar(PlotPoint[i]);

    //Box.PolarCoordinates := (FCoords = csPolar);
    // Notify the parent that the Scale has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TScale.SetXScale(const Value: TScaleType);
begin
  if (Value <> FXScale) then
  begin
    FXScale := Value;

    // Notify the parent that the Scale has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TScale.SetYScale(const Value: TScaleType);
begin
  if (Value <> FYScale) then
  begin
    FYScale := Value;

    // Notify the parent that the Scale has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TScale.SetXLabel(const Value: String);
begin
  if (Value <> FXLabel) then
  begin
    FXLabel := Value;

    // Notify the parent that the Scale has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TScale.SetYLabel(const Value: String);
begin
  if (Value <> FYLabel) then
  begin
    FYLabel := Value;

    // Notify the parent that the Scale has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TScale.SetImagePoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index in [1..3]) then
  begin
    FImgPointIsSet[Index] := True;

    if (FPntImg[Index] <> Value) then
    begin
      FPntImg[Index] := Value;

      // Notify the parent that the Scale has changed
      if assigned(OnChange) then
        OnChange(Self);
    end;
  end;
end;

procedure TScale.SetPlotPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index in [1..3]) then
  begin
    FPltPointIsSet[Index] := True;

    if (FPntPlt[Index] <> Value) then
    begin
      FPntPlt[Index] := Value;

      // Notify the parent that the Scale has changed
      if assigned(OnChange) then
        OnChange(Self);
    end;
  end;
end;

function TScale.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, j: Integer;
  SavedCurveCount,
  RealCurveCount: Integer;
  X, Y: Double;
  Child, CurveChild: TDOMNode;

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
    while assigned(Child) do
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

          ImagePoint[i] := TCurvePoint.Create(X, Y);
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

          PlotPoint[i] := TCurvePoint.Create(X, Y);
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
    Result := Nil;
  end;
end;

//===============================| TScale |================================//


//===============================| TPlot |=================================//
constructor TPlot.Create(Name: String);
begin
  FName := Name;

  FScale := TScale.Create;
  FScale.OnChange := @ChildChange;

  FBox := TPlotQuad.Create;
  FBox.OnChange := @ChildChange;

  FCurves := TCurveList.Create;

  inherited Create;

  Reset;
end;

destructor TPlot.Destroy;
begin
  FScale.Free;
  FBox.Free;
  FCurves.Free;

  inherited Destroy;
end;

procedure TPlot.Reset;
var
  TmpCurve: TDigitCurve;
begin
  FScale.Reset;

  FBox.Reset;

  FCurves.Clear;
  TmpCurve := TDigitCurve.Create('Curve1');
  TmpCurve.OnChange := @ChildChange;
  FCurves.Add(TmpCurve);

  FCurveIndex := 0;
end;

function TPlot.GetCurveCount: Integer;
begin
  Result := FCurves.Count;
end;

function TPlot.GetCurve(Index: Integer): TDigitCurve;
begin
  if (Index >= 0) and (Index < CurveCount) then
    Result := FCurves[Index]
  else
    Result := Nil;
end;

function TPlot.GetActiveCurve: TCurve;
begin
  Result := DigitCurve.Curve;
end;

function TPlot.GetDigitCurve: TDigitCurve;
begin
  Result := FCurves[CurveIndex];
end;

// Warning: any time that we call this function, we must free the curve
// created here, or we will have a memoory leak
function TPlot.GetPlotCurves(Index: Integer): TCurve;
var
  i: Integer;
begin
  Result := TCurve.Create;
  with FCurves[Index].Curve do
  begin
    for i := 0 to Count - 1 do
      Result.AddPoint(Scale.FromImgToPlot(Point[i]));
  end;
end;

function TPlot.GetPlotCurve: TCurve;
begin
  Result := GetPlotCurves(CurveIndex);
end;

procedure TPlot.SetCurveIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < FCurves.Count) and (Value <> CurveIndex) then
  begin
    // Change the active curve
    FCurveIndex := Value;

    // Notify the parent that the Plot has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPlot.SetName(Value: String);
begin
  if (Value <> FName) then
  begin
    FName := Value;

    // Notify the parent that the Plot has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPlot.ChildChange(Sender: TObject);
begin
  // Notify the parent that the child (Scale, Box, Curve) has changed
  if assigned(OnChange) then
    OnChange(Self);
end;

function TPlot.MoveCurve(FromIdx, ToIdx: Integer): Boolean;
begin
  Result := (FromIdx <> ToIdx) and
            (FromIdx >= 0) and (FromIdx < FCurves.Count) and
            (ToIdx >= 0) and (ToIdx < FCurves.Count);

  if Result then
  begin
    FCurves.Move(FromIdx, ToIdx);

    //if (CurveIndex = FromIdx) then
    //  FCurveIndex := ToIdx;

    // Notify the parent that the Plot has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TPlot.AddCurve;
var
  TmpCurve: TDigitCurve;
begin
  TmpCurve := TDigitCurve.Create('Curve' + IntToStr(FCurves.Count + 1));
  TmpCurve.OnChange := @ChildChange;

  FCurves.Add(TmpCurve);

  // Notify the parent that the Plot has changed
  if assigned(OnChange) then
    OnChange(Self);
end;

procedure TPlot.AddCurve(Position: Integer);
var
  TmpCurve: TDigitCurve;
begin
  if (Position >= 0) and (Position < FCurves.Count) then
  begin
    TmpCurve := TDigitCurve.Create('Curve' + IntToStr(Position) + 'b');
    TmpCurve.OnChange := @ChildChange;

    FCurves.Insert(Position, TmpCurve);

    // Notify the parent that the Plot has changed
    if assigned(OnChange) then
      OnChange(Self);
  end
  else
    AddCurve;

end;

procedure TPlot.DeleteCurve;
begin
  DeleteCurve(CurveIndex);
end;

procedure TPlot.DeleteCurve(Index: Integer);
begin
  if (Index >= 0) and (Index < CurveCount) then
  begin
    FCurves.Delete(Index);
    if (CurveIndex >= CurveCount) then
      CurveIndex := CurveCount - 1;

    // Notify the parent that the Plot has changed
    if assigned(OnChange) then
      OnChange(Self);
  end;
end;

function TPlot.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, j: Integer;
  X, Y: Double;
  SavedCurveCount,
  RealCurveCount: Integer;
  Child, CurveChild: TDOMNode;
begin
  Result := False;

  try
    Name := '';
    with Item.Attributes do
    begin
      for i := 0 to Length - 1 do
      begin
        if (Item[i].CompareName('Name') = 0) then
          Name := UTF8Encode(Item[i].NodeValue);
      end;
    end;
    Child := Item.FirstChild;
    while assigned(Child) do
    begin
      // Read scale parameters
      if (Child.CompareName('scale') = 0) then
        Scale.ImportFromXML(Child);

      // Read box parameters
      if (Child.CompareName('PlotBox') = 0) or
         (Child.CompareName('box') = 0) then
        Box.ImportFromXML(Child);

      // For older versions of the document
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

          Scale.ImagePoint[i] := TCurvePoint.Create(X, Y);
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

          Scale.PlotPoint[i] := TCurvePoint.Create(X, Y);
        end;
      end;

      // Read curves
      if (Child.CompareName('curves') = 0) then
      begin
        SavedCurveCount := 0;
        for i := 0 to Child.Attributes.Length - 1 do
          if (Child.Attributes.Item[i].CompareName('Count') = 0) then
            SavedCurveCount := StrToInt(UTF8Encode(Child.Attributes.Item[i].NodeValue));

        RealCurveCount := 0;
        CurveChild := Child.FirstChild;
        while assigned(CurveChild) do
        begin
          if (CurveChild.CompareName('curve') = 0) then
          begin
            inc(RealCurveCount);
            //Create all the needed curves
            while (RealCurveCount > CurveCount) do
              Self.AddCurve;

            Curves[RealCurveCount - 1].ImportFromXML(CurveChild);
          end;

          // Go for the next curve
          CurveChild := CurveChild.NextSibling;
        end;

        assert(SavedCurveCount = RealCurveCount,
               Format('Error: The number of saved curves (%d)' +
                      ' does not match the expected value (%d).',
                      [RealCurveCount, SavedCurveCount]));
      end;

      // Go for the next image point or plot point
      Child := Child.NextSibling;
    end;
    Box.PolarCoordinates := (Scale.CoordSystem = csPolar);

    Result := True;
  except
    // Do nothing, just catch the error
  end;
end;

function TPlot.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  CurveNode: TDOMNode;
begin
  try
    // Create plot node
    Result := Doc.CreateElement('plot');
    with TDOMElement(Result) do
      SetAttribute('Name', UTF8Decode(Name));

    // Add scale node
    Result.AppendChild(Scale.ExportToXML(Doc));

    // Add box node
    Result.AppendChild(Box.ExportToXML(Doc));

    // Add curves node
    CurveNode := Doc.CreateElement('curves');
    TDOMElement(CurveNode).SetAttribute('Count', UTF8Decode(IntToStr(CurveCount)));

    // Save curve nodes
    for i := 0 to CurveCount - 1 do
      CurveNode.Appendchild(Curves[i].ExportToXML(Doc));

    // Save curves node
    Result.AppendChild(CurveNode);
  except
    Result := Nil;
  end;
end;
//===============================| TPlot |=================================//


end.

