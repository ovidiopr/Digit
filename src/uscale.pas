unit uscale;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, Math, ucoordinates;

type
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

    FPlotBox: TPlotQuad;

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
    property PlotBox: TPlotQuad read FPlotBox;
    property IsValid: Boolean read GetIsValidScale;
  end;

implementation

//===============================| TScale |===============================//
constructor TScale.Create;
begin
  FPlotBox := TPlotQuad.Create;
  inherited Create;
  Reset;
end;

destructor TScale.Destroy;
begin
  FPlotBox.Free;

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

  FPlotBox.Reset;
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

    // Make sure that the basis is linear
    case FXScale of
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

    // Make sure that the basis is linear
    case FYScale of
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
    case FXScale of
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
    case FYScale of
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

    PlotBox.PolarCoordinates := (FCoords = csPolar);
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
        // Read PlotBox parameters
        if (Child.CompareName('PlotBox') = 0) then
          PlotBox.ImportFromXML(Child);

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

    // Add PlotBox node
    Result.AppendChild(PlotBox.ExportToXML(Doc));

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

//===============================| TScale |================================//


end.
