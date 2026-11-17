unit uMapImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Fgl, Controls, Graphics, ExtDlgs, DOM, XMLRead,
  XMLWrite, System.UITypes, BGRABitmap, BGRABitmapTypes, uSciDM3,
  uCoordinates, uMarker, uImageViewer, uMapScale, uMapFeatures, uMapRaster,
  uColorBar;

type
  TMapTool = (mtNone, mtSelect, mtScaleBar, mtReference, mtPoint, mtPolyline,
              mtPolygon, mtDistance, mtAngle, mtProfile, mtColorBar);

  TScaleBarPickedEvent = procedure(Sender: TObject; const P1, P2: TCurvePoint) of object;
  TReferencePickedEvent = procedure(Sender: TObject; const ImgPts: TScalePoints) of object;
  TFeatureEvent = procedure(Sender: TObject; Feature: TMapFeature) of object;

  TProfilePoint = record
    S, X, Y: Double;
    Value: Double;
    Z: Double;
    R, G, B: Byte;
  end;
  TProfileData = array of TProfilePoint;

  TFeatureSnapshots = specialize TFPGObjectList<TMapFeatureList>;

  // Calibrated 2D image (map, micrograph) with measurements and features
  TMapImage = class(TImageViewer)
  protected
    FMapScale: TMapScale;
    FFeatures: TMapFeatureList;
    FUndo: TFeatureSnapshots;
    FRedo: TFeatureSnapshots;
    FRaster: TMapRaster;
    FColorBar: TColorBar;
    FSourceFile: TFileName;

    FTool: TMapTool;
    FPending: TMapFeature;
    FRubber: TCurvePoint;
    FHasRubber: Boolean;
    FSelected: Integer;
    FDragUndoDone: Boolean;
    FFeatureColor: TColor;

    FOnScaleBarPicked: TScaleBarPickedEvent;
    FOnReferencePicked: TReferencePickedEvent;
    FOnColorBarPicked: TScaleBarPickedEvent;
    FOnFeatureAdded: TFeatureEvent;
    FOnSelectionChanged: TNotifyEvent;

    procedure SetTool(Value: TMapTool);
    procedure SetSelected(Value: Integer);
    function GetSelectedFeature: TMapFeature;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;
    function GetHasRaster: Boolean;
    procedure MapScaleChanged(Sender: TObject);

    function ToolKind(ATool: TMapTool): TFeatureKind;
    function ImagePoint(X, Y: Integer): TCurvePoint;
    procedure CompletePending;
    procedure PushUndo;
    function ReadDM3(const FileName: TFileName; LoadDisplay: Boolean): Boolean;

    procedure ImageSizeChanged; override;
    procedure ScaleContent(Factor: Double); override;
    procedure SyncMarkersFromModel; override;
    procedure DrawContent(const PaintRect: TRect); override;
    function MarkerRemoving(Marker: TMarker; RealDelete: Boolean): Boolean; override;
    function HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function HandleMouseMove(Shift: TShiftState; X, Y: Integer): Boolean; override;
    procedure AfterMarkerMove(Marker: TMarker; NewPos: TCurvePoint); override;
    procedure MarkerDropDone; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset; override;

    function LoadImageFile(const FileName: TFileName): Boolean;
    function LoadDM3(const FileName: TFileName): Boolean;

    procedure FinishFeature;
    procedure CancelFeature;
    procedure DeleteSelectedFeature;
    procedure ClearFeatures;
    procedure Undo;
    procedure Redo;

    function MapCoordAt(X, Y: Integer): TCurvePoint;
    function ImageValue(const P: TCurvePoint): Double;
    function ValueAt(X, Y: Integer): Double;
    function Profile(Feature: TMapFeature): TProfileData;
    procedure SaveProfileCSV(Feature: TMapFeature; const FileName: String);

    procedure SetColorBar(const P1, P2: TCurvePoint; V1, V2: Double;
                          ALogScale: Boolean; const AUnits: String);
    procedure ClearColorBar;
    function ColorValue(const P: TCurvePoint): Double;
    function ZAt(X, Y: Integer): Double;
    function SaveZGridCSV(Region: TRect; Step: Integer; const FileName: String): Boolean;

    function SaveToXML(FileName: TFileName): Boolean;
    function LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;

    property MapScale: TMapScale read FMapScale;
    property Features: TMapFeatureList read FFeatures;
    property Raster: TMapRaster read FRaster;
    property ColorBar: TColorBar read FColorBar;
    property HasRaster: Boolean read GetHasRaster;
    property SourceFile: TFileName read FSourceFile;
    property Tool: TMapTool read FTool write SetTool;
    property PendingFeature: TMapFeature read FPending;
    property SelectedIndex: Integer read FSelected write SetSelected;
    property SelectedFeature: TMapFeature read GetSelectedFeature;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  published
    property FeatureColor: TColor read FFeatureColor write FFeatureColor default clRed;
    property OnScaleBarPicked: TScaleBarPickedEvent read FOnScaleBarPicked write FOnScaleBarPicked;
    property OnReferencePicked: TReferencePickedEvent read FOnReferencePicked write FOnReferencePicked;
    property OnColorBarPicked: TScaleBarPickedEvent read FOnColorBarPicked write FOnColorBarPicked;
    property OnFeatureAdded: TFeatureEvent read FOnFeatureAdded write FOnFeatureAdded;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

implementation

const
  MaxUndo = 50;
  HitTolerance = 6;

// Empty CSV field for undefined values
function CSVFloat(Value: Double): String;
begin
  if IsNan(Value) then
    Result := ''
  else
    Result := XMLFloat(Value);
end;

//=============================|TMapImage|====================================//
constructor TMapImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FMapScale := TMapScale.Create;
  FFeatures := TMapFeatureList.Create;
  FUndo := TFeatureSnapshots.Create;
  FRedo := TFeatureSnapshots.Create;
  FColorBar := TColorBar.Create;
  FFeatureColor := clRed;
  FSelected := -1;
end;


destructor TMapImage.Destroy;
begin
  FPending.Free;
  FColorBar.Free;
  FRaster.Free;
  FRedo.Free;
  FUndo.Free;
  FFeatures.Free;
  FMapScale.Free;

  inherited Destroy;
end;


procedure TMapImage.Reset;
begin
  FMapScale.OnChange := nil;
  FMapScale.Clear;
  FMapScale.OnChange := @MapScaleChanged;

  FFeatures.Clear;
  FUndo.Clear;
  FRedo.Clear;
  FreeAndNil(FPending);
  FreeAndNil(FRaster);
  FColorBar.Clear;
  FSourceFile := '';
  FHasRubber := False;
  FSelected := -1;

  inherited Reset;
end;


procedure TMapImage.MapScaleChanged(Sender: TObject);
begin
  IsChanged := True;
  RepaintAll;
end;


function TMapImage.GetHasRaster: Boolean;
begin
  Result := Assigned(FRaster) and (FRaster.Width = PlotImg.Width) and
            (FRaster.Height = PlotImg.Height);
end;


function TMapImage.ImagePoint(X, Y: Integer): TCurvePoint;
begin
  Result := TCurvePoint.Create(X/Zoom, Y/Zoom);
end;


function TMapImage.MapCoordAt(X, Y: Integer): TCurvePoint;
begin
  Result := FMapScale.ImgToMap(ImagePoint(X, Y));
end;


function TMapImage.ImageValue(const P: TCurvePoint): Double;
begin
  if HasRaster then
    Result := FRaster.Sample(P.X, P.Y)
  else if ImageIsLoaded then
    Result := Luminance(PlotImg.GetPixel(Single(P.X), Single(P.Y)))
  else
    Result := NaN;
end;


function TMapImage.ValueAt(X, Y: Integer): Double;
begin
  Result := ImageValue(ImagePoint(X, Y));
end;


function TMapImage.ReadDM3(const FileName: TFileName; LoadDisplay: Boolean): Boolean;
var
  DM3: TSciDM3;
  Bmp: TBGRABitmap;
  Stream: TMemoryStream;
  AxX, AxY: TTriple;
begin
  Result := False;
  DM3 := TSciDM3.CreateFile(nil, FileName);
  try
    try
      DM3.ParseDM3;
      if not DM3.IsParsed then Exit;

      FreeAndNil(FRaster);
      FRaster := TMapRaster.Create;
      FRaster.LoadFromDM3(DM3);

      if LoadDisplay then
      begin
        // Display limits stored by DigitalMicrograph
        Bmp := FRaster.ToBitmap(DM3.LowLimit, DM3.HighLimit);
        Stream := TMemoryStream.Create;
        try
          Bmp.SaveToStreamAsPng(Stream);
          LoadImage(Stream);
        finally
          Stream.Free;
          Bmp.Free;
        end;
        FImageName := FileName;

        // Calibration from the file
        AxX := DM3.Axis[0];
        AxY := DM3.Axis[1];
        if (AxX.PixelSize > 0) and (AxY.PixelSize > 0) and (AxX.Units <> '') then
          FMapScale.SetAxes(TCurvePoint.Create(AxX.Origin, AxY.Origin),
                            TCurvePoint.Create(AxX.PixelSize, AxY.PixelSize), AxX.Units);
      end;

      FSourceFile := FileName;
      Result := True;
    except
      on E: Exception do
      begin
        FreeAndNil(FRaster);
        if Assigned(OnPrintMessage) then
          OnPrintMessage(Self, E.Message, mtError);
      end;
    end;
  finally
    DM3.Free;
  end;
end;


function TMapImage.LoadDM3(const FileName: TFileName): Boolean;
begin
  Reset;
  Result := ReadDM3(FileName, True);
  IsChanged := True;
end;


function TMapImage.LoadImageFile(const FileName: TFileName): Boolean;
begin
  if SameText(ExtractFileExt(FileName), '.dm3') then
    Exit(LoadDM3(FileName));

  Reset;
  ImageName := FileName;
  Result := ImageIsLoaded;
end;


procedure TMapImage.ImageSizeChanged;
begin
  FMapScale.ImageHeight := PlotImg.Height;
end;


procedure TMapImage.ScaleContent(Factor: Double);
var
  i: Integer;
begin
  for i := 0 to FFeatures.Count - 1 do
    FFeatures[i].ScalePoints(Factor);
  FMapScale.ScaleImage(Factor);
  FColorBar.ScaleImage(Factor);
  FreeAndNil(FPending);
  FUndo.Clear;
  FRedo.Clear;
  // Raw data no longer matches the resampled image
  FreeAndNil(FRaster);
end;


procedure TMapImage.SetTool(Value: TMapTool);
begin
  if (Value <> FTool) then
  begin
    FreeAndNil(FPending);
    FHasRubber := False;
    FTool := Value;
    if (FTool in [mtNone, mtSelect]) then
      Cursor := crDefault
    else
      Cursor := crCross;
    if (FTool <> mtSelect) then
      SetSelected(-1);
    RepaintAll;
  end;
end;


procedure TMapImage.SetSelected(Value: Integer);
begin
  if (Value < -1) or (Value >= FFeatures.Count) then
    Value := -1;
  if (Value <> FSelected) then
  begin
    FSelected := Value;
    SyncMarkersFromModel;
    RepaintAll;
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  end;
end;


function TMapImage.GetSelectedFeature: TMapFeature;
begin
  if (FSelected >= 0) and (FSelected < FFeatures.Count) then
    Result := FFeatures[FSelected]
  else
    Result := nil;
end;


function TMapImage.ToolKind(ATool: TMapTool): TFeatureKind;
begin
  case ATool of
    mtPoint: Result := fkPoint;
    mtPolyline, mtReference: Result := fkPolyline;
    mtPolygon: Result := fkPolygon;
    mtAngle: Result := fkAngle;
    mtProfile: Result := fkProfile;
    else Result := fkDistance;
  end;
end;


procedure TMapImage.PushUndo;
var
  Snapshot: TMapFeatureList;
begin
  Snapshot := TMapFeatureList.Create;
  Snapshot.Assign(FFeatures);
  FUndo.Add(Snapshot);
  if (FUndo.Count > MaxUndo) then
    FUndo.Delete(0);
  FRedo.Clear;
end;


function TMapImage.GetCanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;


function TMapImage.GetCanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;


procedure TMapImage.Undo;
var
  Current: TMapFeatureList;
begin
  if (FUndo.Count = 0) then Exit;
  Current := TMapFeatureList.Create;
  Current.Assign(FFeatures);
  FRedo.Add(Current);
  FFeatures.Assign(FUndo.Last);
  FUndo.Delete(FUndo.Count - 1);
  FSelected := -1;
  SyncMarkersFromModel;
  IsChanged := True;
  RepaintAll;
end;


procedure TMapImage.Redo;
var
  Current: TMapFeatureList;
begin
  if (FRedo.Count = 0) then Exit;
  Current := TMapFeatureList.Create;
  Current.Assign(FFeatures);
  FUndo.Add(Current);
  FFeatures.Assign(FRedo.Last);
  FRedo.Delete(FRedo.Count - 1);
  FSelected := -1;
  SyncMarkersFromModel;
  IsChanged := True;
  RepaintAll;
end;


procedure TMapImage.CompletePending;
var
  i: Integer;
  Pts: TScalePoints;
  F: TMapFeature;
begin
  if not Assigned(FPending) then Exit;
  FHasRubber := False;

  if not FPending.IsComplete then
  begin
    FreeAndNil(FPending);
    RepaintAll;
    Exit;
  end;

  case FTool of
    mtScaleBar: begin
      if Assigned(FOnScaleBarPicked) then
        FOnScaleBarPicked(Self, FPending[0], FPending[1]);
      FreeAndNil(FPending);
    end;
    mtColorBar: begin
      if Assigned(FOnColorBarPicked) then
        FOnColorBarPicked(Self, FPending[0], FPending[1]);
      FreeAndNil(FPending);
    end;
    mtReference: begin
      for i := 1 to 3 do
        Pts[i] := FPending[i - 1];
      if Assigned(FOnReferencePicked) then
        FOnReferencePicked(Self, Pts);
      FreeAndNil(FPending);
    end;
    else begin
      PushUndo;
      F := FPending;
      FPending := nil;
      F.Name := FFeatures.NextName(F.Kind);
      FFeatures.Add(F);
      IsChanged := True;
      if Assigned(FOnFeatureAdded) then
        FOnFeatureAdded(Self, F);
    end;
  end;

  RepaintAll;
end;


procedure TMapImage.FinishFeature;
begin
  if not Assigned(FPending) then Exit;
  // A double click may repeat the last vertex
  while (FPending.Count > 1) and
    (FPending[FPending.Count - 1].DistanceTo(FPending[FPending.Count - 2]) < 0.5) do
    FPending.DeletePoint(FPending.Count - 1);
  CompletePending;
end;


procedure TMapImage.CancelFeature;
begin
  FreeAndNil(FPending);
  FHasRubber := False;
  RepaintAll;
end;


procedure TMapImage.DeleteSelectedFeature;
begin
  if (FSelected < 0) then Exit;
  PushUndo;
  FFeatures.Delete(FSelected);
  FSelected := -1;
  SyncMarkersFromModel;
  IsChanged := True;
  RepaintAll;
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;


procedure TMapImage.ClearFeatures;
begin
  if (FFeatures.Count = 0) then Exit;
  PushUndo;
  FFeatures.Clear;
  FSelected := -1;
  SyncMarkersFromModel;
  IsChanged := True;
  RepaintAll;
end;


procedure TMapImage.SyncMarkersFromModel;
var
  i: Integer;
  F: TMapFeature;
  Marker: TMarker;
begin
  ClearMarkers;
  F := GetSelectedFeature;
  if not Assigned(F) then Exit;
  // One draggable marker per vertex of the selected feature
  for i := 0 to F.Count - 1 do
  begin
    Marker := TMarker.Create(CreateMarker(TPoint.Create(11, 11), 'q', F.Color, 2),
                             Zoom*F[i], False);
    Marker.DigitMarkerIndex := i;
    AddMarker(Marker, False);
  end;
end;


function TMapImage.MarkerRemoving(Marker: TMarker; RealDelete: Boolean): Boolean;
var
  F: TMapFeature;
  Idx: Integer;
begin
  Result := False;
  if not RealDelete then Exit;
  F := GetSelectedFeature;
  Idx := Marker.DigitMarkerIndex;
  if Assigned(F) and (Idx >= 0) and (Idx < F.Count) then
  begin
    Result := True;
    // Keep the minimum number of vertices of the feature
    if (F.Count <= F.MinPoints) then Exit;
    PushUndo;
    F.DeletePoint(Idx);
    Markers.Remove(Marker);
    SyncMarkersFromModel;
    IsChanged := True;
    RepaintAll;
  end;
end;


procedure TMapImage.AfterMarkerMove(Marker: TMarker; NewPos: TCurvePoint);
var
  F: TMapFeature;
  Idx: Integer;
begin
  F := GetSelectedFeature;
  Idx := Marker.DigitMarkerIndex;
  if Assigned(F) and (Idx >= 0) and (Idx < F.Count) then
  begin
    if not FDragUndoDone then
    begin
      PushUndo;
      FDragUndoDone := True;
    end;
    F[Idx] := NewPos/Zoom;
    RepaintAll;
  end;
end;


procedure TMapImage.MarkerDropDone;
begin
  FDragUndoDone := False;
end;


function TMapImage.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  i: Integer;
  P: TCurvePoint;
begin
  Result := False;
  P := ImagePoint(X, Y);

  case FTool of
    mtNone: Exit;
    mtSelect: begin
      // Vertex markers are dragged by the base class
      for i := 0 to Markers.Count - 1 do
        if Markers[i].HitTest(TPoint.Create(X, Y)) then
          Exit;
      SetSelected(FFeatures.FeatureAt(P, HitTolerance/Zoom));
      Result := True;
    end;
    else begin
      Result := True;
      // Second press of a double click: DblClick finishes the feature
      if (ssDouble in Shift) then Exit;

      if not Assigned(FPending) then
        FPending := TMapFeature.Create(ToolKind(FTool), '', FFeatureColor);
      FPending.AddPoint(P);

      if ((FPending.MaxPoints > 0) and (FPending.Count >= FPending.MaxPoints)) or
         ((FTool = mtReference) and (FPending.Count >= 3)) then
        CompletePending
      else
        RepaintAll;
    end;
  end;
end;


function TMapImage.HandleMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if Assigned(FPending) then
  begin
    // Rubber band from the last vertex
    FRubber := ImagePoint(X, Y);
    FHasRubber := True;
    RepaintAll;
    Result := True;
  end;
end;


procedure TMapImage.DblClick;
begin
  inherited DblClick;
  if Assigned(FPending) then
    FinishFeature;
end;


procedure TMapImage.DrawContent(const PaintRect: TRect);
var
  i: Integer;
  Last: TCurvePoint;
  c: TBGRAPixel;
begin
  // Colour bar sampling line
  if FColorBar.Defined or FColorBar.NeedsRebuild then
  begin
    c := BGRA(255, 0, 255);
    WhiteBoard.PenStyle := psDash;
    WhiteBoard.DrawLineAntialias(Zoom*FColorBar.StartPoint.X, Zoom*FColorBar.StartPoint.Y,
      Zoom*FColorBar.EndPoint.X, Zoom*FColorBar.EndPoint.Y, c, 1);
    WhiteBoard.PenStyle := psSolid;
  end;

  for i := 0 to FFeatures.Count - 1 do
    FFeatures[i].Draw(WhiteBoard, Zoom, FMapScale, i = FSelected);

  if Assigned(FPending) then
  begin
    FPending.Draw(WhiteBoard, Zoom, FMapScale, True);
    if FHasRubber and (FPending.Count > 0) then
    begin
      Last := Zoom*FPending[FPending.Count - 1];
      c := ColorToBGRA(FPending.Color);
      WhiteBoard.PenStyle := psDash;
      WhiteBoard.DrawLineAntialias(Last.X, Last.Y, Zoom*FRubber.X, Zoom*FRubber.Y, c, 1);
      WhiteBoard.PenStyle := psSolid;
    end;
  end;
end;


function TMapImage.Profile(Feature: TMapFeature): TProfileData;
var
  i, k, n, Count: Integer;
  P, Prev: TCurvePoint;
  M: TCurvePoint;
  S: Double;
  c: TBGRAPixel;
  Data: TProfileData;

  procedure AddSample(const Q: TCurvePoint);
  begin
    if (Count > 0) then
      S := S + FMapScale.Distance(Prev, Q);
    M := FMapScale.ImgToMap(Q);
    c := PlotImg.GetPixel(Single(Q.X), Single(Q.Y));
    if (Count >= Length(Data)) then
      SetLength(Data, 2*Count + 16);
    Data[Count].S := S;
    Data[Count].X := M.X;
    Data[Count].Y := M.Y;
    Data[Count].Value := ImageValue(Q);
    Data[Count].Z := ColorValue(Q);
    Data[Count].R := c.red;
    Data[Count].G := c.green;
    Data[Count].B := c.blue;
    Inc(Count);
    Prev := Q;
  end;

begin
  SetLength(Result, 0);
  SetLength(Data, 0);
  if (not Assigned(Feature)) or (Feature.Count < 2) or (not ImageIsLoaded) then Exit;

  Count := 0;
  S := 0;
  Prev := Feature[0];
  AddSample(Feature[0]);
  // About one sample per image pixel along each segment
  for i := 1 to Feature.Count - 1 do
  begin
    n := Max(1, Ceil(Feature[i - 1].DistanceTo(Feature[i])));
    for k := 1 to n do
    begin
      P := Feature[i - 1] + (k/n)*(Feature[i] - Feature[i - 1]);
      AddSample(P);
    end;
  end;
  SetLength(Data, Count);
  Result := Data;
end;


procedure TMapImage.SaveProfileCSV(Feature: TMapFeature; const FileName: String);
var
  i: Integer;
  Data: TProfileData;
  Lines: TStringList;
  ValueName: String;
begin
  Data := Profile(Feature);
  if HasRaster then
    ValueName := 'value'
  else
    ValueName := 'luminance';
  if HasRaster and (FRaster.Units <> '') then
    ValueName := ValueName + '_' + FRaster.Units;

  Lines := TStringList.Create;
  try
    Lines.Add(Format('s_%s,x_%s,y_%s,%s,R,G,B,z',
      [FMapScale.Units, FMapScale.Units, FMapScale.Units, ValueName]));
    for i := 0 to High(Data) do
      Lines.Add(Format('%s,%s,%s,%s,%d,%d,%d,%s',
        [XMLFloat(Data[i].S), XMLFloat(Data[i].X), XMLFloat(Data[i].Y),
         XMLFloat(Data[i].Value), Data[i].R, Data[i].G, Data[i].B, CSVFloat(Data[i].Z)]));
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;


procedure TMapImage.SetColorBar(const P1, P2: TCurvePoint; V1, V2: Double;
  ALogScale: Boolean; const AUnits: String);
begin
  FColorBar.Define(PlotImg, P1, P2, V1, V2, ALogScale, AUnits);
  IsChanged := True;
  RepaintAll;
end;


procedure TMapImage.ClearColorBar;
begin
  FColorBar.Clear;
  IsChanged := True;
  RepaintAll;
end;


function TMapImage.ColorValue(const P: TCurvePoint): Double;
var
  px, py: Integer;
begin
  Result := NaN;
  if not ImageIsLoaded then Exit;
  if FColorBar.NeedsRebuild then
    FColorBar.Rebuild(PlotImg);
  px := Round(P.X);
  py := Round(P.Y);
  if FColorBar.Defined and (px >= 0) and (py >= 0) and
     (px < PlotImg.Width) and (py < PlotImg.Height) then
    Result := FColorBar.ValueOfColor(PlotImg.GetPixel(px, py));
end;


function TMapImage.ZAt(X, Y: Integer): Double;
begin
  Result := ColorValue(ImagePoint(X, Y));
end;


function TMapImage.SaveZGridCSV(Region: TRect; Step: Integer; const FileName: String): Boolean;
var
  x, y, Rows, Row: Integer;
  M: TCurvePoint;
  Lines: TStringList;
begin
  Result := False;
  if not ImageIsLoaded then Exit;
  if FColorBar.NeedsRebuild then
    FColorBar.Rebuild(PlotImg);
  if not FColorBar.Defined then Exit;

  if Region.IsEmpty then
    Region := TRect.Create(0, 0, PlotImg.Width, PlotImg.Height);
  Region.Intersect(TRect.Create(0, 0, PlotImg.Width, PlotImg.Height));
  Step := Max(Step, 1);

  RunningAction := True;
  CancelAction := False;
  Lines := TStringList.Create;
  try
    Lines.Add(Format('x_%s,y_%s,z_%s', [FMapScale.Units, FMapScale.Units, FColorBar.Units]));
    Rows := Max(1, Region.Height div Step);
    Row := 0;
    y := Region.Top;
    while (y < Region.Bottom) do
    begin
      x := Region.Left;
      while (x < Region.Right) do
      begin
        M := FMapScale.ImgToMap(TCurvePoint.Create(x, y));
        Lines.Add(Format('%s,%s,%s', [XMLFloat(M.X), XMLFloat(M.Y),
          CSVFloat(FColorBar.ValueOfColor(PlotImg.GetPixel(x, y)))]));
        Inc(x, Step);
      end;
      Inc(y, Step);
      Inc(Row);
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, Min(100, Round(100*Row/Rows)), 'Extracting values...');
      if CheckCancelStatus then Exit;
    end;
    Lines.SaveToFile(FileName);
    Result := True;
  finally
    Lines.Free;
    RunningAction := False;
    if Assigned(OnHideProgress) then
      OnHideProgress(Self);
  end;
end;


function TMapImage.SaveToXML(FileName: TFileName): Boolean;
var
  XMLDoc: TXMLDocument;
  RootNode, DocNode, MapNode: TDOMElement;
begin
  Result := False;
  XMLDoc := TXMLDocument.Create;
  try
    RootNode := XMLDoc.CreateElement('digitization');
    RootNode.SetAttribute('version', '2.0');
    XMLDoc.AppendChild(RootNode);

    DocNode := XMLDoc.CreateElement('document');
    DocNode.SetAttribute('ImageIsLoaded', UTF8Decode(BoolToStr(ImageIsLoaded, True)));
    DocNode.SetAttribute('Type', 'map');
    if ImageIsLoaded then
      DocNode.AppendChild(ImageToXML(XMLDoc));
    RootNode.AppendChild(DocNode);

    MapNode := XMLDoc.CreateElement('map');
    MapNode.SetAttribute('SourceFile', UTF8Decode(FSourceFile));
    MapNode.AppendChild(FMapScale.ExportToXML(XMLDoc));
    MapNode.AppendChild(FFeatures.ExportToXML(XMLDoc));
    MapNode.AppendChild(FColorBar.ExportToXML(XMLDoc));
    RootNode.AppendChild(MapNode);

    WriteXMLFile(XMLDoc, FileName);
    FIsChanged := False;
    Result := True;
  finally
    XMLDoc.Free;
  end;
end;


function TMapImage.LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;
var
  XMLDoc: TXMLDocument;
  Child, DocChild, MapChild: TDOMNode;
  ImgPath, ImgName, Src: String;
  TmpOnChange: TNotifyEvent;
begin
  Result := False;
  TmpOnChange := OnChange;
  OnChange := nil;
  XMLDoc := nil;
  try
    Reset;
    ReadXMLFile(XMLDoc, FileName);
    ImgPath := '';
    ImgName := '';
    Src := '';

    Child := XMLDoc.DocumentElement.FirstChild;
    while Assigned(Child) do
    begin
      if (Child.CompareName('document') = 0) then
      begin
        DocChild := Child.FirstChild;
        while Assigned(DocChild) do
        begin
          if (DocChild.CompareName('image') = 0) then
            ImageFromXML(DocChild, ImgPath, ImgName);
          DocChild := DocChild.NextSibling;
        end;
      end;

      if (Child.CompareName('map') = 0) then
      begin
        Src := GetXMLAttr(Child, 'SourceFile');
        MapChild := Child.FirstChild;
        while Assigned(MapChild) do
        begin
          if (MapChild.CompareName('scale') = 0) then
            FMapScale.ImportFromXML(MapChild);
          if (MapChild.CompareName('features') = 0) then
            FFeatures.ImportFromXML(MapChild);
          if (MapChild.CompareName('colorbar') = 0) then
            FColorBar.ImportFromXML(MapChild);
          MapChild := MapChild.NextSibling;
        end;
      end;

      Child := Child.NextSibling;
    end;

    // No embedded image: look for the original file
    if not ImageIsLoaded then
    begin
      if FileExists(ImgPath + ImgName) then
        ImageName := ImgPath + ImgName
      else if FileExists(ExtractFilePath(FileName) + ImgName) then
        ImageName := ExtractFilePath(FileName) + ImgName
      else if Assigned(PictureDlg) then
      begin
        PictureDlg.InitialDir := ExtractFilePath(FileName);
        if PictureDlg.Execute then
          ImageName := PictureDlg.FileName;
      end;
    end
    else
      FImageName := ImgPath + ImgName;

    // Raw data comes back from the DM3 file when it is still there
    if (Src <> '') and FileExists(Src) then
      ReadDM3(Src, False)
    else
      FSourceFile := Src;

    FMapScale.ImageHeight := PlotImg.Height;
    SyncMarkersFromModel;
    RepaintAll;
    Result := True;
  finally
    XMLDoc.Free;
    OnChange := TmpOnChange;
    FIsChanged := False;
  end;
end;

end.
