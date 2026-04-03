unit uplotimage;

{$mode objfpc}{$H+}

interface

uses {$ifdef windows}Windows,{$endif} Forms, Classes, Controls, Graphics,
  ExtDlgs, Fgl, ComCtrls, SysUtils, DOM, XMLWrite, XMLRead, Dialogs, Types,
  Base64, BGRABitmap, BGRABitmapTypes, BGRAreadTiff, Math,
  uutils, ucurves, ucoordinates, uscale, umarker, ugrid, CurveDigitizer;

const
  ZoomLevels: array [1..17] of Double = (1/10, 1/8, 1/6, 1/5, 1/4, 1/3, 1/2,
    2/3, 1, 3/2, 2, 3, 4, 5, 6, 8, 10);

type
  TPlotImageState = (piSetCurve, piSetScale, piSetPlotBox, piSetGrid);
  TDragAction = (daNone, daVertex, daEdge, daAngle, daRotate);

  TSelectRegionEvent = procedure(Sender: TObject; RegionRect: TRect) of object;
  TStateChangeEvent = procedure(Sender: TObject; NewState: TPlotImageState) of object;
  TMarkerDraggedEvent = procedure(Sender: TObject; Marker: TMarker;
    Zoom: Boolean) of object;
  TZoomChangeEvent = procedure(Sender: TObject; Zoom: Double) of object;
  TScaleChangeEvent = procedure(Sender: TObject; Index: Integer) of object;
  TItemChangeEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;

  TPlotImage = class(TCustomControl)
  protected
  type
    TPlotList = specialize TFPGObjectList<TPlot>;
  protected
    FOldCursor: TCursor;

    InMouseMove: Boolean;
    MouseMovePos: TPoint;

    FRunningAction: Boolean;
    FCancelAction: Boolean;

    FZoom: Double;
    FOptions: TPlotOptions;
    FState: TPlotImageState;
    FDragAction: TDragAction;

    FImageName: TFileName;
    FPlotImg: TBGRABitmap;
    FZoomImg: TBGRABitmap;
    FWhiteBoard: TBGRABitmap;
    FGridMask: TGridMask;

    FMarkers: TMarkerList;
    FMarkerList: TCurve;
    FAxesMarkers: array [1..3] of TMarker;
    FBoxMarkers: array [1..4] of TMarker;
    FEdgeMarkers: array [1..4] of TMarker;
    FMarkerUnderCursor: TMarker;
    FActiveMarker: TMarker;
    FClickedMarker: TMarker;
    FClickedPoint: TPoint;
    FClickedCoord: TPoint;
    FDragging: Boolean;
    FSelectingRegion: Boolean;
    FSelectionRect: TRect;

    FPlots: TPlotList;
    FPlotIndex: Integer;

    FImageIsLoaded: Boolean;

    FIsChanged: Boolean;
    FOnChange: TNotifyEvent;

    FOnPrintMessage: TPrintMessageEvent;
    FOnShowProgress: TShowProgressEvent;
    FOnHideProgress: THideProgressEvent;
    FOnRegionSelected: TSelectRegionEvent;
    FOnStateChanged: TStateChangeEvent;
    FOnMarkerDragged: TMarkerDraggedEvent;
    FOnZoomChanged: TZoomChangeEvent;
    FOnActivePlotChanging: TItemChangeEvent;
    FOnActivePlotChanged: TItemChangeEvent;
    FOnActiveCurveChanging: TItemChangeEvent;
    FOnActiveCurveChanged: TItemChangeEvent;

    FDigitThread: TDigitizerThread;
    FStartPi: TCurvePoint;
    FCurrentDigitMode: TDigitization;

    procedure SetCancelAction(Value: Boolean);
    function CheckCancelStatus: Boolean;
    procedure OnDigitizeProgress(Percent: Integer; Messg: String = '');
    procedure OnDigitizeFinished(Sender: TObject);
    procedure ProcessDigitizedCurve(ACurve: TCurve);
    procedure DeferredFreeThread(Data: PtrInt);

    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    function GetAxesPoint(Index: Integer): TCurvePoint;
    function GetBoxVertex(Index: Integer): TCurvePoint;
    function GetAxesMarkers(Index: Integer): TMarker;
    function GetBoxMarkers(Index: Integer): TMarker;
    function GetEdgeMarkers(Index: Integer): TMarker;

    function GetPlotCount: Integer;
    function GetPlot(Index: Integer): TPlot;
    function GetActivePlot: TPlot;

    function GetCurveCount: Integer;
    function GetCurveIndex: Integer;

    function GetNumPoints: Integer;
    function GetPoint(Index: Integer): TCurvePoint;

    function GetLeftMarker: TMarker;
    function GetRightMarker: TMarker;
    function GetTopMarker: TMarker;
    function GetBottomMarker: TMarker;
    function GetMarkerList: TCurve;

    function GetColorIsSet: Boolean;
    function GetHasPoints: Boolean;
    function GetIsChanged: Boolean;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;

    function XAxisRect: TRect;
    function YAxisRect: TRect;

    procedure ResetZoomImage;
    procedure SetZoom(Value: Double);

    procedure SetAxesPoint(Index: Integer; const Value: TCurvePoint);
    procedure SetBoxVertex(Index: Integer; const Value: TCurvePoint);

    procedure SetMarkerUnderCursor(Marker: TMarker);
    procedure SetActiveMarker(Marker: TMarker);
    procedure SetImageName(Value: TFileName);
    procedure SetDragging(Value: Boolean);
    procedure SetSelectingRegion(Value: Boolean);

    procedure SetState(Value: TPlotImageState);

    procedure SetCurveIndex(Value: Integer);
    procedure SetPlotIndex(Value: Integer);
    procedure SetIsChanged(Value: Boolean);

    procedure SetPoint(Index: Integer; const Value: TCurvePoint);

    procedure ClearMarkers;
    procedure UpdateMarkersInCurve;
    procedure UpdateMarkersInImage;

    procedure RepaintRegion(UpdateArea: TRect);
    procedure RepaintAll;

    procedure RectIntersection(Po, Pf: TCurvePoint; var Pini, Pend: TCurvePoint);
    procedure XAxisCoords(var Pini, Pend: TCurvePoint);
    procedure YAxisCoords(var Pini, Pend: TCurvePoint);

    procedure LoadImage(FileName: TFileName); overload;
    procedure LoadImage(Stream: TStream); overload;

    procedure ChildChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;
    procedure SetPlotPointMarkers(ResetPoints: Boolean = False);

    function GetPixel(X, Y: Integer): Longint; overload;
    function GetPixel(X, Y: Double): Longint; overload;
    function GetPixel(P: TCurvePoint): Longint; overload;

    procedure FindCurvePoints;

    function FindNextPoint(var Pv: TCurvePoint; Interval: Integer; ScanX: Boolean = False): Boolean;
    procedure Digitize(DigitMode: TDigitization = digLineFollowing; UseThread: Boolean = True); overload;
    procedure Digitize(Pi: TCurvePoint; DigitMode: TDigitization; UseThread: Boolean = True); overload;
    procedure DigitizeMarkers;

    procedure BuildDigitizerContext(out Ctx: TDigitizerContext);

    procedure AdjustCurve(Noisy: Boolean = False);
    procedure ConvertCurveToSymbols;

    function ConvertCoords(P: TCurvePoint): TCurvePoint; overload;
    function ConvertCoords(X, Y: Double): TCurvePoint; overload;

    procedure SwitchGrid;
    procedure MergeGridMask;
    procedure ResetPlotBox;

    procedure PasteImage(Stream: TStream);

    procedure ZoomOriginal;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomFitBest;
    procedure ResizeToZoom;

    procedure AddMarker(Position: TPoint; NewMarker: Boolean = True); overload;
    procedure AddMarker(Marker: TMarker; NewMarker: Boolean = True); overload;
    procedure RepaintMarker(Marker: TMarker);
    procedure DeleteMarker(Marker: TMarker; RealDelete: Boolean = True); overload;
    procedure DeleteMarker(Index: Integer; RealDelete: Boolean = True); overload;
    procedure DeleteMarkerUnderCursor;
    procedure DeleteActiveMarker;
    procedure ShiftActiveMarker(Delta: TPoint);
    procedure RedrawMarkers;

    function MoveCurve(FromIdx, ToIdx: Integer): Boolean;
    function MovePlot(FromIdx, ToIdx: Integer): Boolean;

    procedure AddPlot; overload;
    procedure AddPlot(Position: Integer); overload;
    procedure DeletePlot; overload;
    procedure DeletePlot(Index: Integer); overload;

    procedure AddCurve; overload;
    procedure AddCurve(Position: Integer); overload;
    procedure DeleteCurve; overload;
    procedure DeleteCurve(Index: Integer); overload;
    procedure ClearCurve; overload;
    procedure ClearCurve(Index: Integer); overload;
    procedure UndoCurveChanges;
    procedure RedoCurveChanges;

    procedure SortCurve; overload;
    procedure SortCurve(Index: Integer); overload;
    procedure Smooth(k, d: Integer; Index: Integer); overload;
    procedure Smooth(k, d: Integer; AllCurves: Boolean = False); overload;
    procedure Interpolate(n, d: Integer; Index: Integer;
      IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(n, d: Integer; AllCurves: Boolean = False;
      IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(Xo, Xf: Double; n, d: Integer; Index: Integer;
      IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(Xo, Xf: Double; n, d: Integer;
      AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline); overload;
    procedure CorrectCurve(Po, Pf: TPoint; IsStep: Boolean = True); overload;
    procedure CorrectCurve(Po, Pf: TCurvePoint; IsStep: Boolean = True); overload;
    procedure CorrectCurve(Region: TRect; IsStep: Boolean = True); overload;
    procedure CorrectCurve(IsStep: Boolean = True); overload;
    procedure RemoveGrid(LineColor1, LineColor2, BckgndColor: TColor;
      Tolerance: Integer = 10; Threshold: Double = 0.5;
      FixCurve: Boolean = False; MaskSize: Integer = 5);
    procedure GroupPoints(Region: TRect);
    procedure DeletePoints(Region: TRect);
    procedure MoveCurveUp;
    procedure MoveCurveDown;
    procedure MoveCurveLeft;
    procedure MoveCurveRight;

    procedure ClearMarker(Marker: TMarker);
    procedure DrawMarker(Marker: TMarker);
    procedure MoveMarker(Marker: TMarker; Point: TPoint); overload;
    procedure MoveMarker(Marker: TMarker; X, Y: Double); overload;

    procedure UndistortImage;

    function GetZoomImage(w, h: Integer; Region: TRect): TBitmap;

    function SaveToXML(FileName: TFileName): Boolean;
    function LoadFromXML(FileName: TFileName;
      PictureDlg: TOpenPictureDialog = nil): Boolean;

    property Zoom: Double read FZoom write SetZoom;
    property Options: TPlotOptions read FOptions write FOptions;
    property State: TPlotImageState read FState write SetState;
    property ImageName: TFileName read FImageName write SetImageName;
    property GridMask: TGridMask read FGridMask;
    property WhiteBoard: TBGRABitmap read FWhiteBoard;
    property PlotImg: TBGRABitmap read FPlotImg;
    property ZoomImg: TBGRABitmap read FZoomImg;
    property Markers: TMarkerList read FMarkers;
    property AxesPoint[Index: Integer]: TCurvePoint read GetAxesPoint write SetAxesPoint;
    property AxesMarkers[Index: Integer]: TMarker read GetAxesMarkers;
    property BoxVertex[Index: Integer]: TCurvePoint read GetBoxVertex write SetBoxVertex;
    property BoxMarkers[Index: Integer]: TMarker read GetBoxMarkers;
    property EdgeMarkers[Index: Integer]: TMarker read GetEdgeMarkers;
    property MarkerUnderCursor: TMarker read FMarkerUnderCursor
      write SetMarkerUnderCursor;
    property ActiveMarker: TMarker read FActiveMarker write SetActiveMarker;
    property LeftMarker: TMarker read GetLeftMarker;
    property RightMarker: TMarker read GetRightMarker;
    property TopMarker: TMarker read GetTopMarker;
    property BottomMarker: TMarker read GetBottomMarker;
    property MarkerList: TCurve read GetMarkerList;
    property Dragging: Boolean read FDragging write SetDragging;
    property SelectingRegion: Boolean read FSelectingRegion write SetSelectingRegion;
    property SelectionRect: TRect read FSelectionRect;
    property RunningAction: Boolean read FRunningAction write FRunningAction;
    property CancelAction: Boolean read FCancelAction write SetCancelAction;

    {Return the number of plots}
    property PlotCount: Integer read GetPlotCount;
    {Return the index of the active plot}
    property PlotIndex: Integer read FPlotIndex write SetPlotIndex;
    {Return the plot}
    property Plots[Index: Integer]: TPlot read GetPlot; default;
    {Return the active plot}
    property Plot: TPlot read GetActivePlot;

    {Return the number of curves in the active plot}
    property CurveCount: Integer read GetCurveCount;
    {Return the index of the active curve}
    property CurveIndex: Integer read GetCurveIndex write SetCurveIndex;

    property ImageIsLoaded: Boolean read FImageIsLoaded write FImageIsLoaded;

    {Return the number of points in the active curve}
    property NumPoints: Integer read GetNumPoints;
    property Point[Index: Integer]: TCurvePoint read GetPoint write SetPoint;

    //property Scale: TScale read FScale;
    property ColorIsSet: Boolean read GetColorIsSet;
    property HasPoints: Boolean read GetHasPoints;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPrintMessage: TPrintMessageEvent
      read FOnPrintMessage write FOnPrintMessage;
    property OnShowProgress: TShowProgressEvent
      read FOnShowProgress write FOnShowProgress;
    property OnHideProgress: THideProgressEvent
      read FOnHideProgress write FOnHideProgress;
    property OnRegionSelected: TSelectRegionEvent
      read FOnRegionSelected write FOnRegionSelected;
    property OnStateChanged: TStateChangeEvent
      read FOnStateChanged write FOnStateChanged;
    property OnMarkerDragged: TMarkerDraggedEvent
      read FOnMarkerDragged write FOnMarkerDragged;
    property OnZoomChanged: TZoomChangeEvent read FOnZoomChanged write FOnZoomChanged;
    property OnActivePlotChanging: TItemChangeEvent
      read FOnActivePlotChanging write FOnActivePlotChanging;
    property OnActivePlotChanged: TItemChangeEvent
      read FOnActivePlotChanged write FOnActivePlotChanged;
    property OnActiveCurveChanging: TItemChangeEvent
      read FOnActiveCurveChanging write FOnActiveCurveChanging;
    property OnActiveCurveChanged: TItemChangeEvent
      read FOnActiveCurveChanged write FOnActiveCurveChanged;
  end;

implementation

function TMarkerComparator(const a, b: TMarker): Integer;
begin
  Result := Sign(a.Position.X - b.Position.X);
end;

//=============================|TPlotImage|===================================//
constructor TPlotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlotImg := TBGRABitmap.Create;
  FZoomImg := TBGRABitmap.Create;
  FWhiteBoard := TBGRABitmap.Create;

  FGridMask := TGridMask.Create(0, 0);
  FGridMask.OnChange := @ChildChange;

  FMarkers := TMarkerList.Create;
  FMarkerList := TCurve.Create;

  FPlots := TPlotList.Create;

  Reset;
end;

destructor TPlotImage.Destroy;
begin
  if Assigned(FDigitThread) then
  begin
    FDigitThread.Terminate;
    FDigitThread.WaitFor;
    FDigitThread.Free;
    FDigitThread := nil;
  end;

  FGridMask.Free;
  FWhiteBoard.Free;
  FZoomImg.Free;
  FPlotImg.Free;

  FMarkers.Free;
  FMarkerList.Free;

  FPlots.Free;

  inherited Destroy;
end;

procedure TPlotImage.Reset;
var
  i: Integer;
  TmpPlot: TPlot;
begin
  FState := piSetCurve;
  FDragAction := daNone;
  FZoom := 1;

  FPlots.Clear;
  TmpPlot := TPlot.Create('Plot1');
  TmpPlot.OnChange := @ChildChange;
  FPlots.Add(TmpPlot);

  FPlotIndex := 0;

  FMarkers.Clear;
  ActiveMarker := nil;
  MarkerUnderCursor := nil;
  for i := 1 to 3 do
    FAxesMarkers[i] := nil;
  for i := 1 to 4 do
  begin
    FBoxMarkers[i] := nil;
    FEdgeMarkers[i] := nil;
  end;

  FOldCursor := Cursor;

  FImageIsLoaded := False;

  FGridMask.IsValid := False;
  FGridMask.IsActive := False;

  InMouseMove := False;

  FRunningAction := False;
  FCancelAction := False;

  FIsChanged := False;
end;

procedure TPlotImage.SetPlotPointMarkers(ResetPoints: Boolean = False);
const
  span = 2;

  function PutInside(P: TCurvePoint; w, h: Integer; d: Integer = 0): TCurvePoint;
  begin
    Result := P;

    if (Result.X < d) then Result.X := d;
    if (Result.Y < d) then Result.Y := d;
    if (Result.X > w - d - 1) then Result.X := w - d - 1;
    if (Result.Y > h - d - 1) then Result.Y := h - d - 1;
  end;

begin
  if ImageIsLoaded then
  begin
    if ResetPoints then
      with PlotImg do
      begin
        AxesPoint[1] := TPoint.Create(span, span);
        AxesPoint[2] := TPoint.Create(span, Height - span - 1);
        AxesPoint[3] := TPoint.Create(Width - span - 1, Height - span - 1);

        Plot.Scale.PlotPoint[1] := TCurvePoint.Create(0, 1);
        Plot.Scale.PlotPoint[2] := TCurvePoint.Create(0, 0);
        Plot.Scale.PlotPoint[3] := TCurvePoint.Create(1, 0);

        BoxVertex[1] := TCurvePoint.Create(span, span);
        BoxVertex[2] := TCurvePoint.Create(Width - span - 1, span);
        BoxVertex[3] := TCurvePoint.Create(Width - span - 1, Height - span - 1);
        BoxVertex[4] := TCurvePoint.Create(span, Height - span - 1);
      end
    else
      with PlotImg do
      begin
        AxesPoint[1] := PutInside(AxesPoint[1], Width, Height, span);
        AxesPoint[2] := PutInside(AxesPoint[2], Width, Height, span);
        AxesPoint[3] := PutInside(AxesPoint[3], Width, Height, span);

        BoxVertex[1] := PutInside(BoxVertex[1], Width, Height, span);
        BoxVertex[2] := PutInside(BoxVertex[2], Width, Height, span);
        BoxVertex[3] := PutInside(BoxVertex[3], Width, Height, span);
        BoxVertex[4] := PutInside(BoxVertex[4], Width, Height, span);
      end;
  end;
end;

procedure TPlotImage.FindCurvePoints;
var
  Ctx: TDigitizerContext;
begin
  if Plot.DigitCurve.ValidPoints then Exit;

  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Finding points...');
  if Assigned(OnPrintMessage) then
    OnPrintMessage(Self, 'Finding points...', mtInformation);

  RunningAction := True;
  CancelAction := False;

  BuildDigitizerContext(Ctx);

  Ctx.OnProgress := @OnDigitizeProgress;
  Ctx.CheckTerminated := @CheckCancelStatus;

  Plot.DigitCurve.AllPoints.Clear;
  ScanCurvePoints(FPlotImg, Ctx, Plot.DigitCurve.AllPoints);

  if Assigned(OnHideProgress) then OnHideProgress(Self);
  if Assigned(OnPrintMessage) then
    if CancelAction then
      OnPrintMessage(Self, 'Action cancelled by user', mtWarning)
    else
      OnPrintMessage(Self, 'Done', mtConfirmation);

  Plot.DigitCurve.ValidPoints := not CancelAction;
  RunningAction := False;
end;

function TPlotImage.FindNextPoint(var Pv: TCurvePoint; Interval: Integer; ScanX: Boolean = False): Boolean;
var
  // Number of pixels that the line is expected to spread
  Spread: Integer;

  P: TCurvePoint;
  dX, dY: Integer;
  NoPnts: Integer;
  FoundUp, FoundDown: Boolean;

  function CheckPlotPoint(const Pi: TCurvePoint; var P: TCurvePoint;
  var nP: Integer): Boolean;
  begin
    Result := Plot.DigitCurve.AllPoints.Contains(Pi);
    if Result then
    begin
      Inc(nP);
      P := P + Pi;
    end;
  end;

begin
  Spread := Max(2, Plot.DigitCurve.Spread);

  P := TCurvePoint.Create(0, 0);
  NoPnts := 0;
  dY := 0;
  Result := False;
  // We scan, starting from Yv, until we find enough points
  repeat
    // We check Yv twice, to give it more weight
    FoundUp := CheckPlotPoint(Pv - dY*Plot.Scale.Ny(Pv), P, NoPnts);
    FoundDown := CheckPlotPoint(Pv + dY*Plot.Scale.Ny(Pv), P, NoPnts);

    dX := 1;
    while ScanX and (dX < Spread) do
    begin
      FoundUp := FoundUp or CheckPlotPoint(Pv - dX*Plot.Scale.Nx(Pv) -
        dY*Plot.Scale.Ny(Pv), P, NoPnts);
      FoundUp := FoundUp or CheckPlotPoint(Pv + dX*Plot.Scale.Nx(Pv) -
        dY*Plot.Scale.Ny(Pv), P, NoPnts);
      FoundDown := FoundDown or CheckPlotPoint(Pv - dX*Plot.Scale.Nx(Pv) +
        dY*Plot.Scale.Ny(Pv), P, NoPnts);
      FoundDown := FoundDown or CheckPlotPoint(Pv + dX*Plot.Scale.Nx(Pv) +
        dY*Plot.Scale.Ny(Pv), P, NoPnts);

      Inc(dX);
    end;

    Inc(dY);
  until (NoPnts >= 1 + 2*Spread) or (dY >= Interval) or
    ((NoPnts > 0) and (not FoundUp) and (not FoundDown));

  if (NoPnts > 0) then
  begin
    Result := True;
    Pv := P/NoPnts;
  end;
end;

procedure TPlotImage.Digitize(Pi: TCurvePoint; DigitMode: TDigitization; UseThread: Boolean);
var
  Ctx: TDigitizerContext;
  Seeds, SyncResultCurve: TCurve;
  i, nSteps: Integer;
  X0, X1: Double;
begin
  if Assigned(FDigitThread) then Exit; // Prevent multiple threads running

  RunningAction := True;
  CancelAction := False;
  FStartPi := Pi;
  FCurrentDigitMode := DigitMode;

  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Digitizing curve...');

  BuildDigitizerContext(Ctx);
  Ctx.Mode := DigitMode;

  Seeds := TCurve.Create;
  try
    if DigitMode = digColorTracing then
    begin
      Seeds.AddPoint(TCurvePoint.Create(0, 0)); // Dummy seed for color scan
    end
    else
    begin
      // Seed generation for spectrum/line algorithms
      if Markers.Count > 0 then
        for i := 0 to Markers.Count - 1 do
          Seeds.AddPoint(Markers[i].Position/Zoom)
      else
        Seeds.AddPoint(Pi);

      if (Seeds.Count > 2) and (Abs(Ctx.Step) > 0) then
      begin
        X0 := Seeds.Point[0].X;
        X1 := Seeds.Point[Seeds.Count - 1].X;
        nSteps := 1 + Abs(Round((X1 - X0)/Abs(Ctx.Step)));
        if nSteps > Seeds.Count then
          Seeds.Interpolate(nSteps, 3);
      end;
    end;

    if UseThread then
    begin
      // Asynchronous (background thread)
      FDigitThread := TDigitizerThread.Create(FPlotImg, Ctx, Seeds, @OnDigitizeFinished);
      FDigitThread.OnProgress := @OnDigitizeProgress;
      FDigitThread.Start;
    end
    else
    begin
      // Synchronous (main thread)
      SyncResultCurve := TCurve.Create;
      try
        // Attach callbacks for UI responsiveness during sync loop
        Ctx.OnProgress := @OnDigitizeProgress;
        Ctx.CheckTerminated := @CheckCancelStatus;

        // Call the core unit directly
        CurveDigitizer.DigitizeCurve(Seeds, FPlotImg, Ctx, SyncResultCurve);

        if CancelAction then
        begin
          if Assigned(OnPrintMessage) then
            OnPrintMessage(Self, 'Action cancelled by user', mtWarning);
        end
        else
        begin
          ProcessDigitizedCurve(SyncResultCurve);
        end;
      finally
        SyncResultCurve.Free;
        RunningAction := False;
        if Assigned(OnHideProgress) then OnHideProgress(Self);
        Invalidate;
      end;
    end;

  finally
    Seeds.Free; // Cleanup
  end;
end;

procedure TPlotImage.Digitize(DigitMode: TDigitization = digLineFollowing; UseThread: Boolean = True);
var
  Pi: TCurvePoint;
  i: Integer;
  BestX: Double;
  Candidate: TCurvePoint;
begin
  if DigitMode = digColorTracing then
  begin
    if Assigned(OnShowProgress) then
      OnShowProgress(Self, 0, 'Digitizing curve...');
    Digitize(TCurvePoint.Create(0, 0), DigitMode, UseThread);
    Exit;
  end;

  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Finding initial point...');

  FindCurvePoints;

  // Estimate the first point
  if (Markers.Count > 0) then
  begin
    if (Plot.DigitCurve.Step > 0) then
      Pi := LeftMarker.Position/Zoom
    else
      Pi := RightMarker.Position/Zoom;
  end
  else
  begin
    if Plot.DigitCurve.AllPoints.Count > 0 then
    begin
      if Plot.DigitCurve.Step > 0 then
        BestX := MaxDouble   // looking for minimum X
      else
        BestX := -MaxDouble; // looking for maximum X

      Pi := Plot.DigitCurve.AllPoints.Point[0];

      for i := 0 to Plot.DigitCurve.AllPoints.Count - 1 do
      begin
        Candidate := Plot.DigitCurve.AllPoints.Point[i];
        if (Plot.DigitCurve.Step > 0) and (Candidate.X < BestX) then
        begin
          BestX := Candidate.X;
          Pi := Candidate;
        end
        else if (Plot.DigitCurve.Step < 0) and (Candidate.X > BestX) then
        begin
          BestX := Candidate.X;
          Pi := Candidate;
        end;
      end;
    end
    else
    begin
      // No matching pixels found at all — fall back to the plot boundary
      if Plot.Scale.CoordSystem = csCartesian then
      begin
        if Plot.DigitCurve.Step > 0 then
          Pi := (Plot.Scale.ImagePoint[1] + Plot.Scale.ImagePoint[2])/2
        else
          Pi := Plot.Scale.ImagePoint[3] + (Plot.Scale.ImagePoint[1] - Plot.Scale.ImagePoint[2])/2;
      end
      else
        Pi := (Plot.Scale.ImagePoint[3] + Plot.Scale.ImagePoint[2])/2;
    end;
  end;

  Digitize(Pi, DigitMode, UseThread);
end;

procedure TPlotImage.DigitizeMarkers;
var
  i: Integer;
begin
  if (State = piSetCurve) and (Markers.Count > 0) then
  begin
    Plot.DigitCurve.NextCurve(False);
    Plot.Curve.ShowAsSymbols := True;

    for i := 0 to Markers.Count - 1 do
      Plot.Curve.AddPoint(Markers[i].Position/Zoom);

    SortCurve;

    IsChanged := True;
  end;
end;

procedure TPlotImage.BuildDigitizerContext(out Ctx: TDigitizerContext);
begin
  Ctx.Plot := Plot;
  Ctx.Color := Plot.DigitCurve.Color;
  Ctx.Tolerance := Plot.DigitCurve.Tolerance;
  Ctx.LineSpread := Plot.DigitCurve.Spread;
  Ctx.MaxGap := Plot.DigitCurve.Interval;
  Ctx.Mode := digLineFollowing;
  Ctx.Step := Plot.DigitCurve.Step;
  if (Plot.DigitCurve.Step > 0) then
    Ctx.ScanDirection := sdForward
  else
    Ctx.ScanDirection := sdBackward;
  Ctx.GridMask := GridMask.Mask;
  Ctx.GridMaskActive := GridMask.IsValid and GridMask.IsActive;
end;

procedure TPlotImage.AdjustCurve(Noisy: Boolean = False);
var
  Ctx: TDigitizerContext;
  TmpCurve: TCurve;
  i: Integer;
begin
  if not HasPoints then Exit;

  RunningAction := True;
  CancelAction := False;

  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Adjusting curve...');
  if Assigned(OnPrintMessage) then
    OnPrintMessage(Self, 'Adjusting curve...', mtInformation);

  BuildDigitizerContext(Ctx);
  // Attach callbacks for UI responsiveness during sync loop
  Ctx.OnProgress := @OnDigitizeProgress;
  Ctx.CheckTerminated := @CheckCancelStatus;

  // Work on a copy so we can save the undo state cleanly
  TmpCurve := TCurve.Create;
  try
    for i := 0 to Plot.Curve.Count - 1 do
      TmpCurve.AddPoint(Plot.Curve.Point[i]);

    // Each point is replaced by the centroid of the connected
    // same-colour island around it
    AdjustDigitizedCurve(FPlotImg, Ctx, TmpCurve, Noisy);

    // Store result as a new undo step
    Plot.DigitCurve.NextCurve(False);
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curve.AddPoint(TmpCurve.Point[i]);

    IsChanged := True;
  finally
    TmpCurve.Free;
    if Assigned(OnHideProgress) then OnHideProgress(Self);
    if Assigned(OnPrintMessage) then
      if CancelAction then
        OnPrintMessage(Self, 'Action cancelled by user', mtWarning)
      else
        OnPrintMessage(Self, 'Done', mtConfirmation);
    RunningAction := False;
  end;
end;

procedure TPlotImage.ConvertCurveToSymbols;
var
  Ctx: TDigitizerContext;
  TmpCurve: TCurve;
  i: Integer;
begin
  if not HasPoints then Exit;

  RunningAction := True;
  CancelAction := False;

  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Converting curve to symbols...');
  if Assigned(OnPrintMessage) then
    OnPrintMessage(Self, 'Converting curve to symbols...', mtInformation);

  BuildDigitizerContext(Ctx);
  // Attach callbacks for UI responsiveness during sync loop
  Ctx.OnProgress := @OnDigitizeProgress;
  Ctx.CheckTerminated := @CheckCancelStatus;

  TmpCurve := TCurve.Create;
  try
    for i := 0 to Plot.Curve.Count - 1 do
      TmpCurve.AddPoint(Plot.Curve.Point[i]);

    // Each contiguous blob touched by the curve becomes one symbol point
    ConvertCurveToSymbolPoints(FPlotImg, Ctx, TmpCurve);

    Plot.DigitCurve.NextCurve(False);
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curve.AddPoint(TmpCurve.Point[i]);

    SortCurve;
    Plot.Curve.ShowAsSymbols := True;
    IsChanged := True;
  finally
    TmpCurve.Free;
    if Assigned(OnHideProgress) then OnHideProgress(Self);
    if Assigned(OnPrintMessage) then
      if CancelAction then
        OnPrintMessage(Self, 'Action cancelled by user', mtWarning)
      else
        OnPrintMessage(Self, 'Done', mtConfirmation);
    RunningAction := False;
  end;
end;

function TPlotImage.GetPixel(X, Y: Integer): Longint;
var
  c: TBGRAPixel;
  XZ, YZ: Integer;
begin
  XZ := Round(X/Zoom);
  YZ := Round(Y/Zoom);
  if GridMask.IsValid and GridMask.IsActive then
  begin
    c := GridMask.Mask.GetPixel(XZ, YZ);

    if (c = BGRAPixelTransparent) then
      c := PlotImg.GetPixel(XZ, YZ);
  end
  else
    c := PlotImg.GetPixel(XZ, YZ);

  Result := ColorToRGB(c);
end;

function TPlotImage.GetPixel(X, Y: Double): Longint;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

function TPlotImage.GetPixel(P: TCurvePoint): Longint;
begin
  Result := GetPixel(P.X, P.Y);
end;

function TPlotImage.GetAxesPoint(Index: Integer): TCurvePoint;
begin
  Result := Plot.Scale.ImagePoint[Index];
end;

function TPlotImage.GetBoxVertex(Index: Integer): TCurvePoint;
begin
  Result := Plot.Box[Index - 1];
end;

function TPlotImage.GetAxesMarkers(Index: Integer): TMarker;
begin
  if (Index in [1..3]) and Assigned(FAxesMarkers[Index]) then
    Result := FAxesMarkers[Index]
  else
    Result := nil;
end;

function TPlotImage.GetBoxMarkers(Index: Integer): TMarker;
begin
  if (Index in [1..4]) and Assigned(FBoxMarkers[Index]) then
    Result := FBoxMarkers[Index]
  else
    Result := nil;
end;

function TPlotImage.GetEdgeMarkers(Index: Integer): TMarker;
begin
  if (Index in [1..4]) and Assigned(FEdgeMarkers[Index]) then
    Result := FEdgeMarkers[Index]
  else
    Result := nil;
end;

function TPlotImage.XAxisRect: TRect;
var
  Pini, Pend: TCurvePoint;
  Xmin, Xmax, Ymin, Ymax: Integer;
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[3]) then
  begin
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[3].Position, Pini, Pend);

    Xmin := Round(min(Pini.X, Pend.X));
    Xmax := Round(Max(Pini.X, Pend.X));
    Ymin := Round(min(Pini.Y, Pend.Y));
    Ymax := Round(Max(Pini.Y, Pend.Y));
    Result := TRect.Create(Xmin, Ymin, Xmax, Ymax);
    Result.Union(TRect.Create(Pend - TPoint.Create(6, 6), 12, 12));
  end
  else
    Result := TRect.Create(0, 0, 0, 0);
end;

function TPlotImage.YAxisRect: TRect;
var
  Pini, Pend: TCurvePoint;
  Xmin, Xmax, Ymin, Ymax: Integer;
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[1]) then
  begin
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[1].Position, Pini, Pend);

    Xmin := Round(min(Pini.X, Pend.X));
    Xmax := Round(Max(Pini.X, Pend.X));
    Ymin := Round(min(Pini.Y, Pend.Y));
    Ymax := Round(Max(Pini.Y, Pend.Y));
    Result := TRect.Create(Xmin, Ymin, Xmax, Ymax);
    Result.Union(TRect.Create(Pend - TPoint.Create(6, 6), 12, 12));
  end
  else
    Result := TRect.Create(0, 0, 0, 0);
end;

procedure TPlotImage.ResetZoomImage;
var
  Rect: TRect;
begin
  ZoomImg.SetSize(PlotImg.Width, PlotImg.Height);
  ZoomImg.PutImage(0, 0, PlotImg, dmSet);
  if GridMask.IsActive then
  begin
    Rect := TRect.Create(TPoint.Create(0, 0), PlotImg.Width, PlotImg.Height);
    ZoomImg.Canvas.CopyRect(Rect, GridMask.Mask.Canvas, Rect);
  end;
  if (Zoom <> 1) then
  begin
    ZoomImg.ResampleFilter := rfSpline;
    BGRAReplace(FZoomImg, ZoomImg.Resample(Width, Height, rmFineResample));
  end;
end;

procedure TPlotImage.SetZoom(Value: Double);
begin
  if (Value > 0) and (Value <> FZoom) then
  begin
    UpdateMarkersInCurve;

    FZoom := Value;

    Width := Round(FZoom*PlotImg.Width);
    Height := Round(FZoom*PlotImg.Height);

    ResetZoomImage;

    WhiteBoard.SetSize(Width, Height);
    WhiteBoard.PutImage(0, 0, ZoomImg, dmSet);

    UpdateMarkersInImage;

    // Notify the parent that the state has changed
    if Assigned(OnZoomChanged) then
      OnZoomChanged(Self, Value);

    Invalidate;
  end;
end;

procedure TPlotImage.SetAxesPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index in [1..3]) and (Plot.Scale.ImagePoint[Index] <> Value) then
  begin
    if Assigned(FAxesMarkers[Index]) then
      FAxesMarkers[Index].Position := Zoom*Value;

    Plot.Scale.ImagePoint[Index] := Value;

    IsChanged := True;
  end;
end;

procedure TPlotImage.SetBoxVertex(Index: Integer; const Value: TCurvePoint);
var
  Idx: Integer;
begin
  if (Index in [1..4]) and (Plot.Box[Index - 1] <> Value) then
  begin
    if Assigned(FBoxMarkers[Index]) then
      FBoxMarkers[Index].Position := Zoom*Value;

    Plot.Box[Index - 1] := Value;

    if Assigned(FEdgeMarkers[Index]) then
      FEdgeMarkers[Index].Position := Zoom*Plot.Box.Edge[Index - 1];

    Idx := Plot.Box.PrevVertIdx(Index - 1) + 1;
    if Assigned(FEdgeMarkers[Idx]) then
      FEdgeMarkers[Idx].Position := Zoom*Plot.Box.Edge[Idx - 1];

    IsChanged := True;
  end;
end;

function TPlotImage.GetLeftMarker: TMarker;
var
  i: Integer;
  X: Double;
begin
  Result := nil;
  if (State = piSetCurve) then
  begin
    X := Markers[0].Position.X;
    Result := Markers[0];
    for i := 1 to Markers.Count - 1 do
      if (X > Markers[i].Position.X) then
      begin
        X := Markers[i].Position.X;
        Result := Markers[i];
      end;
  end;
end;

function TPlotImage.GetRightMarker: TMarker;
var
  i: Integer;
  X: Double;
begin
  Result := nil;
  if (State = piSetCurve) then
  begin
    X := Markers[0].Position.X;
    Result := Markers[0];
    for i := 1 to Markers.Count - 1 do
      if (X < Markers[i].Position.X) then
      begin
        X := Markers[i].Position.X;
        Result := Markers[i];
      end;
  end;
end;

function TPlotImage.GetTopMarker: TMarker;
var
  i: Integer;
  Y: Double;
begin
  Result := nil;
  if (State = piSetCurve) then
  begin
    Y := Markers[0].Position.Y;
    Result := Markers[0];
    for i := 1 to Markers.Count - 1 do
      if (Y > Markers[i].Position.Y) then
      begin
        Y := Markers[i].Position.Y;
        Result := Markers[i];
      end;
  end;
end;

function TPlotImage.GetBottomMarker: TMarker;
var
  i: Integer;
  Y: Double;
begin
  Result := nil;
  if (State = piSetCurve) then
  begin
    Y := Markers[0].Position.Y;
    Result := Markers[0];
    for i := 1 to Markers.Count - 1 do
      if (Y < Markers[i].Position.Y) then
      begin
        Y := Markers[i].Position.Y;
        Result := Markers[i];
      end;
  end;
end;

function TPlotImage.GetMarkerList: TCurve;
var
  i: Integer;
begin
  FMarkerList.Clear;

  if (State = piSetCurve) then
  begin
    for i := 0 to Markers.Count - 1 do
      FMarkerList.AddPoint(Plot.Scale.FromImgToPlot(Markers[i].Position/Zoom));

    FMarkerList.SortCurve;
  end;

  Result := FMarkerList;
end;

procedure TPlotImage.SetImageName(Value: TFileName);
begin
  FImageName := Value;
  LoadImage(FImageName);
end;

procedure TPlotImage.SetDragging(Value: Boolean);
begin
  FDragging := Value;
  if FDragging then
  begin
    FOldCursor := Cursor;
    Cursor := crDrag;
  end
  else
    Cursor := FOldCursor;
end;


procedure TPlotImage.SetSelectingRegion(Value: Boolean);
begin
  FSelectingRegion := Value;
  if FSelectingRegion then
  begin
    FOldCursor := Cursor;
    Cursor := crCross;
  end
  else
    Cursor := FOldCursor;
end;

procedure TPlotImage.SwitchGrid;
begin
  GridMask.IsActive := not GridMask.IsActive;

  ResetZoomImage;

  WhiteBoard.SetSize(Width, Height);
  WhiteBoard.PutImage(0, 0, ZoomImg, dmSet);

  IsChanged := True;
end;

procedure TPlotImage.MergeGridMask;
var
  Rect: TRect;
begin
  if GridMask.IsActive then
  begin
    // Copy the mask over the original image
    Rect := TRect.Create(TPoint.Create(0, 0), PlotImg.Width, PlotImg.Height);
    PlotImg.Canvas.CopyRect(Rect, GridMask.Mask.Canvas, Rect);

    with GridMask do
    begin
      // Reset the mask
      SetSize(PlotImg.Width, PlotImg.Height);
      IsValid := False;
      IsActive := False;
    end;

    ResetZoomImage;

    WhiteBoard.SetSize(Width, Height);
    WhiteBoard.PutImage(0, 0, ZoomImg, dmSet);
  end;
end;

procedure TPlotImage.ResetPlotBox;
const
  span = 2;
begin
  Plot.Box[0] := TCurvePoint.Create(span, span);
  Plot.Box[1] := TCurvePoint.Create(PlotImg.Width - span - 1, span);
  Plot.Box[2] := TCurvePoint.Create(PlotImg.Width - span - 1,
    PlotImg.Height - span - 1);
  Plot.Box[3] := TCurvePoint.Create(span, PlotImg.Height - span - 1);

  if (State = piSetPlotBox) then
  begin
    UpdateMarkersInImage;
    Invalidate;
  end;
end;

procedure TPlotImage.PasteImage(Stream: TStream);
var
  TmpBmp: TBGRABitmap;
  NewStream: TMemoryStream;
begin
  try
    TmpBmp := TBGRABitmap.Create(Stream);
    NewStream := TMemoryStream.Create;

    NewStream.Clear;
    NewStream.Position := 0;
    TmpBmp.SaveToStreamAsPng(NewStream);

    LoadImage(NewStream);

  finally
    TmpBmp.Free;
    NewStream.Free;
  end;
end;

procedure TPlotImage.ZoomOriginal;
begin
  Zoom := 1;
end;

procedure TPlotImage.ZoomIn;
var
  i: Integer;
  Zoomed: Boolean;
begin
  Zoomed := False;
  for i := Low(ZoomLevels) to High(ZoomLevels) do
    if (ZoomLevels[i] > Zoom) then
    begin
      Zoom := ZoomLevels[i];
      Zoomed := True;
      Break;
    end;

  if (not Zoomed) then
    Zoom := 2*Zoom;
end;

procedure TPlotImage.ZoomOut;
var
  i: Integer;
  Zoomed: Boolean;
begin
  Zoomed := False;
  for i := High(ZoomLevels) downto Low(ZoomLevels) do
    if (ZoomLevels[i] < Zoom) then
    begin
      Zoom := ZoomLevels[i];
      Zoomed := True;
      Break;
    end;

  if (not Zoomed) then
    Zoom := Zoom/2;
end;

procedure TPlotImage.ZoomFitBest;
begin
  if ((PlotImg.Width/PlotImg.Height) > (Parent.ClientWidth/Parent.ClientHeight)) then
    Zoom := Parent.ClientWidth/PlotImg.Width
  else
    Zoom := Parent.ClientHeight/PlotImg.Height;
end;

procedure TPlotImage.ResizeToZoom;
var
  i, j, k, l: Integer;
begin
  if (Zoom <> 1) then
  begin
    for i := 0 to PlotCount - 1 do
    begin
      // Update the scales
      with Plots[i].Scale do
        for j := 1 to 3 do
          ImagePoint[j] := Zoom*ImagePoint[j];

      // Update the boxes
      with Plots[i].Box do
        for j := 0 to NumVertices - 1 do
          Vertex[j] := Zoom*Vertex[j];

      // Update the curves
      for j := 0 to Plots[i].CurveCount - 1 do
        for k := 0 to Plots[i].Curves[j].ValidCurves - 1 do
          with Plots[i].Curves[j].Curves[k] do
            for l := 0 to Count - 1 do
              Point[l] := Zoom*Point[l];
    end;

    PlotImg.ResampleFilter := rfSpline;
    BGRAReplace(FPlotImg, PlotImg.Resample(Width, Height, rmFineResample));

    GridMask.ResizeMask(Width, Height);

    Zoom := 1.0;

    RepaintAll;

    IsChanged := True;
  end;
end;

procedure TPlotImage.AddMarker(Position: TPoint; NewMarker: Boolean = True);
var
  BMP: TBGRABitmap;
begin
  BMP := CreateMarker(TPoint.Create(13, 13), 'x', Plot.DigitCurve.Color, 3);
  AddMarker(TMarker.Create(BMP, Position, False), NewMarker);
end;

procedure TPlotImage.AddMarker(Marker: TMarker; NewMarker: Boolean = True);
begin
  Markers.Insert(0, Marker);
  ActiveMarker := Marker;
  RepaintMarker(Marker);

  if NewMarker then
    IsChanged := True;
end;

procedure TPlotImage.RepaintMarker(Marker: TMarker);
begin
  RepaintRegion(Marker.Rect);
end;

procedure TPlotImage.DeleteMarker(Marker: TMarker; RealDelete: Boolean = True);
var
  i: Integer;
begin
  if Assigned(Marker) then
  begin
    if (FClickedMarker = Marker) then
    begin
      FClickedMarker := nil;
      Dragging := False;
    end;

    if ActiveMarker = Marker then
      ActiveMarker := nil;

    if (MarkerUnderCursor = Marker) then
      MarkerUnderCursor := nil;

    for i := 1 to 3 do
      if (FAxesMarkers[i] = Marker) then
        FAxesMarkers[i] := nil;

    for i := 1 to Plot.Box.NumVertices do
    begin
      if (FBoxMarkers[i] = Marker) then
        FBoxMarkers[i] := nil;
      if (FEdgeMarkers[i] = Marker) then
        FEdgeMarkers[i] := nil;
    end;

    Markers.Remove(Marker);

    if (not Assigned(ActiveMarker)) and (Markers.Count > 0) then
      ActiveMarker := Markers[0];

    if RealDelete then
      IsChanged := True;
  end;
end;

procedure TPlotImage.DeleteMarker(Index: Integer; RealDelete: Boolean = True);
begin
  if (Index >= 0) and (Index < Markers.Count) then
    DeleteMarker(Markers[Index], RealDelete);
end;

procedure TPlotImage.DeleteMarkerUnderCursor;
begin
  DeleteMarker(MarkerUnderCursor, True);
end;

procedure TPlotImage.DeleteActiveMarker;
begin
  DeleteMarker(ActiveMarker, True);
end;

procedure TPlotImage.ShiftActiveMarker(Delta: TPoint);
var
  i: Integer;
  NewPos: TCurvePoint;
  OldPos, P1, P2: TCurvePoint;
begin
  if Assigned(ActiveMarker) then
  begin
    NewPos := ActiveMarker.Position + Delta;

    // Check that the marker remains inside the image
    if ClientRect.Contains(NewPos) then
    begin
      for i := 1 to 3 do
        if (ActiveMarker = AxesMarkers[i]) then
          Plot.Scale.ImagePoint[i] := NewPos/Zoom;

      with Plot.Box do
      begin
        for i := 1 to NumVertices do
        begin
          if (ActiveMarker = BoxMarkers[i]) then
          begin
            RepaintRegion(Rect[Zoom]);
            Vertex[i - 1] := NewPos/Zoom;

            EdgeMarkers[i].Move(Zoom*Edge[i - 1]);
            EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(Zoom*Edge[PrevVertIdx(i - 1)]);
            RepaintRegion(Rect[Zoom]);
          end;

          if (ActiveMarker = EdgeMarkers[i]) then
          begin
            OldPos := Edge[i - 1];
            MoveEdge(i - 1, NewPos/Zoom);
            P1 := Zoom*Vertex[NextVertIdx(i - 1)];
            P2 := Zoom*Vertex[i - 1];

            // Check that no marker moves out of the image
            if ClientRect.Contains(P1) and ClientRect.Contains(P2) then
            begin
              RepaintRegion(Rect[Zoom]);

              BoxMarkers[NextVertIdx(i - 1) + 1].Move(P1);
              BoxMarkers[i].Move(P2);

              EdgeMarkers[NextVertIdx(i - 1) + 1].Move(Zoom*Edge[NextVertIdx(i - 1)]);
              EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(Zoom*Edge[PrevVertIdx(i - 1)]);

              RepaintRegion(Rect[Zoom]);

              NewPos := Zoom*Edge[i - 1];
            end
            else
            begin
              MoveEdge(i - 1, OldPos);
              NewPos := OldPos;
            end;
          end;
        end;
      end;

      if (NewPos <> ActiveMarker.Position) then
      begin
        RepaintMarker(ActiveMarker);
        ActiveMarker.Move(NewPos);
        RepaintMarker(ActiveMarker);

        IsChanged := True;
      end;
    end;
  end;
end;

procedure TPlotImage.RedrawMarkers;
begin
  UpdateMarkersInCurve;
  UpdateMarkersInImage;
end;

procedure TPlotImage.SetCancelAction(Value: Boolean);
begin
  FCancelAction := Value;
  // If the user hits cancel, forcefully flag the thread to terminate
  if FCancelAction and Assigned(FDigitThread) then
    FDigitThread.Terminate;
end;

function TPlotImage.CheckCancelStatus: Boolean;
begin
  Application.ProcessMessages;
  Result := FCancelAction;
end;

procedure TPlotImage.OnDigitizeProgress(Percent: Integer; Messg: String);
begin
  if Assigned(OnShowProgress) then
  if (Messg <> '') then
    OnShowProgress(Self, Percent, Messg)
  else
    OnShowProgress(Self, Percent, 'Processing...');
end;

procedure TPlotImage.ProcessDigitizedCurve(ACurve: TCurve);
var
  i, best: Integer;
  d, dmin: Double;
begin
  if (ACurve = nil) or (ACurve.Count = 0) then
  begin
    if Assigned(OnPrintMessage) then
      OnPrintMessage(Self, 'No curve detected.', mtWarning);
    Exit;
  end;

  if FCurrentDigitMode = digColorTracing then
  begin
    // --- Color Tracing Result Logic ---
    Plot.DigitCurve.NextCurve(False); // Start a new curve segment
    for i := 0 to ACurve.Count - 1 do
      Plot.Curve.AddPoint(ACurve.Point[i]);
  end
  else
  begin
    // --- Spectrum/Line Tracing Result Logic ---
    Plot.Curve.Clear;
    dmin := MaxDouble;
    best := 0;

    // Find the point closest to the starting selection
    for i := 0 to ACurve.Count - 1 do
    begin
      d := Sqr(ACurve.X[i] - FStartPi.X) + Sqr(ACurve.Y[i] - FStartPi.Y);
      if d < dmin then
      begin
        dmin := d;
        best := i;
      end;
    end;

    // Reorder points based on the best match found
    for i := best to ACurve.Count - 1 do
      Plot.Curve.AddPoint(ACurve.Point[i]);
    for i := 0 to best - 1 do
      Plot.Curve.AddPoint(ACurve.Point[i]);

    Plot.Curve.SortCurve;
  end;

  SortCurve;
  IsChanged := True;

  if Assigned(OnPrintMessage) then
    OnPrintMessage(Self, 'Done', mtConfirmation);
end;

procedure TPlotImage.DeferredFreeThread(Data: PtrInt);
begin
  TDigitizerThread(Data).Free;
end;

procedure TPlotImage.OnDigitizeFinished(Sender: TObject);
var
  TmpCurve: TCurve;
  LThread: TDigitizerThread;
begin
  if not (Sender is TDigitizerThread) then Exit;
  LThread := TDigitizerThread(Sender);
  TmpCurve := nil;
  try
    TmpCurve := LThread.TakeResultCurve;
    if CancelAction or LThread.IsTerminated then
    begin
      if Assigned(OnPrintMessage) then
        OnPrintMessage(Self, 'Action cancelled by user', mtWarning);
    end
    else
      ProcessDigitizedCurve(TmpCurve);
  finally
    TmpCurve.Free;
    Application.QueueAsyncCall(@DeferredFreeThread, PtrInt(LThread));
    FDigitThread := nil;
    if Assigned(OnHideProgress) then OnHideProgress(Self);
    RunningAction := False;
    Invalidate;
  end;
end;

procedure TPlotImage.Paint;
var
  i: Integer;
  {$ifdef windows}
  ClipRgn: HRGN;
  {$endif}
  PolyColor: TBGRAPixel;
  Rect, ClipRect, PaintRect: TRect;
  Pini, Pend: TCurvePoint;
begin
  inherited Paint;

  Rect := TRect.Create(TPoint.Create(0, 0), Width, Height);
  ClipRect := Canvas.ClipRect;
  PaintRect := Rect*ClipRect;

  with WhiteBoard.Canvas do
  begin
    {$ifdef windows}
    ClipRgn := CreateRectRgn(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom);
    SelectClipRgn(Handle, ClipRgn);
    {$endif}

    CopyMode := cmSrcCopy;
    CopyRect(PaintRect, ZoomImg.Canvas, PaintRect);
  end;

  case State of
    piSetCurve: begin
      if HasPoints then
        Plot.DigitCurve.Draw(WhiteBoard.Canvas, Zoom);
    end;
    piSetScale: begin
      if Options.ShowXAxis or Options.ShowYAxis then
      begin
        WhiteBoard.ArrowEndAsTriangle();
        WhiteBoard.ArrowEndSize := PointF(8, 2.5);
        WhiteBoard.ArrowStartOffset := 0;
        WhiteBoard.ArrowEndOffset := -7;

        WhiteBoard.PenStyle := psDash;

        if Options.ShowXAxis then
        begin
          XAxisCoords(Pini, Pend);
          WhiteBoard.DrawLineAntialias(Pini.X, Pini.Y, Pend.X, Pend.Y,
            Options.XAxisColor, 2);
        end;

        if Options.ShowYAxis then
        begin
          YAxisCoords(Pini, Pend);
          WhiteBoard.DrawLineAntialias(Pini.X, Pini.Y, Pend.X, Pend.Y,
            Options.YAxisColor, 2);
        end;

        WhiteBoard.PenStyle := psSolid;
      end;
    end;
    piSetPlotBox: begin
      with Plot.Box do
      begin
        if not (Rect[Zoom]*PaintRect).IsEmpty then
        begin
          if IsConvex then
          begin
            if IsCW then
              PolyColor := clBlue
            else
              PolyColor := clGreen;
          end
          else
            PolyColor := clRed;

          PolyColor.alpha := 80;
          //PolarCoordinates := (Plot.CoordSystem = csPolar);
          WhiteBoard.DrawPolygonAntialias(DrawPoints[Zoom], BGRABlack, 1, PolyColor);
        end;
      end;
    end;
  end;

  for i := Markers.Count - 1 downto 0 do
    Markers[i].Draw(WhiteBoard, PaintRect);

  if Assigned(ActiveMarker) and not (ActiveMarker.Rect*PaintRect).IsEmpty then
  begin
    with WhiteBoard.Canvas do
      DrawFocusRect(ActiveMarker.Rect);
  end;

  if Assigned(MarkerUnderCursor) and (MarkerUnderCursor <> ActiveMarker) and
    not (MarkerUnderCursor.Rect*PaintRect).IsEmpty then
  begin
    with WhiteBoard.Canvas do
      DrawFocusRect(MarkerUnderCursor.Rect);
  end;

  if SelectingRegion then
    WhiteBoard.Canvas.DrawFocusRect(SelectionRect);

  {$ifdef windows}
  with WhiteBoard.Canvas do
  begin
    SelectClipRgn(Handle, 0);
    DeleteObject(ClipRgn);
  end;
  {$endif}
  //WhiteBoard.Draw(Canvas, PaintRect, True);
  Canvas.CopyRect(PaintRect, WhiteBoard.Canvas, PaintRect);

  if not PaintRect.Contains(ClipRect) then
  begin
    Rect := ClientRect;
    Rect.Left := WhiteBoard.Width;
    Rect.Intersect(ClipRect);
    Canvas.CopyRect(Rect, ZoomImg.Canvas, Rect);

    Rect := ClientRect;
    Rect.Top := WhiteBoard.Height;
    Rect.Width := WhiteBoard.Width;
    Rect.Intersect(ClipRect);
    Canvas.CopyRect(Rect, ZoomImg.Canvas, Rect);
  end;
end;

procedure TPlotImage.Resize;
begin
  Invalidate;
  inherited Resize;
end;

procedure TPlotImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function IsInArray(M: TMarker; Arr: array of TMarker): Boolean;
  var
    j: Integer;
  begin
    for j := Low(Arr) to High(Arr) do
      if (M = Arr[j]) then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  i: Integer;
  Marker, HitMarker: TMarker;
begin
  if Button = mbLeft then
  begin
    HitMarker := nil;
    for i := 0 to Markers.Count - 1 do
    begin
      Marker := Markers[i];
      if Marker.HitTest(TPoint.Create(X, Y)) then
      begin
        HitMarker := Marker;
        FClickedPoint := TPoint.Create(X, Y);
        FClickedCoord := Marker.Rect.CenterPoint;
        Break;
      end;
    end;
    FClickedMarker := HitMarker;
    ActiveMarker := HitMarker;

    // Define the action to perform when dragging
    if (State = piSetPlotBox) and Assigned(FClickedMarker) then
    begin
      if IsInArray(FClickedMarker, FBoxMarkers) then
      begin
        if (ssAlt in Shift) or (ssCtrl in Shift) then
          FDragAction := daRotate
        else if (ssShift in Shift) or (not Plot.Box.IsConvex) then
          FDragAction := daVertex
        else
          FDragAction := daAngle;
      end
      else if IsInArray(FClickedMarker, FEdgeMarkers) then
        FDragAction := daEdge;
    end;

    // No marker under cursor, we are selecting a region
    if (HitMarker = nil) and (State = piSetCurve) and not (ssCtrl in Shift) then
    begin
      SelectingRegion := True;
      FSelectionRect := TRect.Create(X, Y, X, Y);
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPlotImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, j: Integer;
  Marker, HitMarker: TMarker;
  TmpRect: TRect;
  NewPos, OldPos, P1, P2: TCurvePoint;
begin
  MouseMovePos := TPoint.Create(X, Y);

  // Make sure that there are no previous events in the stack
  if InMouseMove then Exit;
  InMouseMove := True;
  // Empty message stack
  Application.ProcessMessages;

  inherited MouseMove(Shift, MouseMovePos.X, MouseMovePos.Y);

  if Assigned(FClickedMarker) then
  begin
    if not Dragging then
      if (Abs(MouseMovePos.X - FClickedPoint.X) > 5) or
        (Abs(MouseMovePos.Y - FClickedPoint.Y) > 5) then
        Dragging := True;

    if Dragging then
    begin
      RepaintMarker(FClickedMarker);

      if (State = piSetScale) then
      begin
        RepaintRegion(XAxisRect);
        RepaintRegion(YAxisRect);
      end;

      if ClientRect.Contains(MouseMovePos) then
        NewPos := FClickedCoord + MouseMovePos - FClickedPoint
      else
        NewPos := FClickedCoord;  //The mouse moves out of the image

      FClickedMarker.Move(NewPos);

      if (State = piSetScale) then
      begin
        RepaintRegion(XAxisRect);
        RepaintRegion(YAxisRect);
      end;

      if (State = piSetPlotBox) then
        with Plot.Box do
        begin
          // Rectangle containing all the modified region
          TmpRect := Rect[Zoom];
          for i := 1 to NumVertices do
            TmpRect.Union(BoxMarkers[i].Rect);
          for i := 1 to NumEdges do
            TmpRect.Union(EdgeMarkers[i].Rect);

          for i := 1 to NumVertices do
            if (FClickedMarker = BoxMarkers[i]) then
              Break;

          case FDragAction of
            daVertex, daAngle: begin
              // Move vertex
              OldPos := Vertex[i - 1];
              if (FDragAction = daAngle) and IsConvex then
              begin
                MoveVertex(i - 1, NewPos/Zoom);
                P1 := Zoom*Vertex[NextVertIdx(i - 1)];
                P2 := Zoom*Vertex[PrevVertIdx(i - 1)];

                // Check that no marker moves out of the image
                if ClientRect.Contains(P1) and ClientRect.Contains(P2) then
                begin
                  BoxMarkers[NextVertIdx(i - 1) + 1].Move(P1);
                  BoxMarkers[PrevVertIdx(i - 1) + 1].Move(P2);
                end
                else
                begin
                  MoveVertex(i - 1, OldPos);
                  NewPos := OldPos;
                end;
              end;

              // Update vertex
              Vertex[i - 1] := NewPos/Zoom;

              // Move edges
              for j := 1 to NumEdges do
                EdgeMarkers[j].Move(Zoom*Edge[j - 1]);
            end;
            daRotate: begin
              // Rotate
              Rotate(i - 1, NewPos/Zoom);

              // Check that no marker moves out of the image
              if not ClientRect.Contains(Zoom*Vertex[0]) or not
                ClientRect.Contains(Zoom*Vertex[1]) or not
                ClientRect.Contains(Zoom*Vertex[2]) or not
                ClientRect.Contains(Zoom*Vertex[3]) then
              begin
                CancelRotation;
              end;

              for j := 1 to NumVertices do
                if (i <> j) then
                  BoxMarkers[j].Move(Zoom*Vertex[j - 1]);

              for j := 1 to NumEdges do
                EdgeMarkers[j].Move(Zoom*Edge[j - 1]);

              NewPos := Zoom*Vertex[i - 1];
              FClickedMarker.Move(NewPos);
            end;
            daEdge: begin
              for i := 1 to NumEdges do
                if (FClickedMarker = EdgeMarkers[i]) then
                  Break;

              if IsConvex then
              begin
                OldPos := Edge[i - 1];
                MoveEdge(i - 1, NewPos/Zoom);
                P1 := Zoom*Vertex[NextVertIdx(i - 1)];
                P2 := Zoom*Vertex[i - 1];

                // Check that no marker moves out of the image
                if ClientRect.Contains(P1) and ClientRect.Contains(P2) and
                  IsConvex then
                begin
                  BoxMarkers[NextVertIdx(i - 1) + 1].Move(P1);
                  BoxMarkers[i].Move(P2);

                  EdgeMarkers[NextVertIdx(i - 1) + 1].Move(
                    Zoom*Edge[NextVertIdx(i - 1)]);
                  EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(
                    Zoom*Edge[PrevVertIdx(i - 1)]);
                end
                else
                  MoveEdge(i - 1, OldPos);
              end;

              NewPos := Zoom*Edge[i - 1];
              FClickedMarker.Move(NewPos);
            end;
          end;

          // Include the new positions in the modified rectangle
          TmpRect.Union(Rect[Zoom]);
          for i := 1 to NumVertices do
            TmpRect.Union(BoxMarkers[i].Rect);
          for i := 1 to NumEdges do
            TmpRect.Union(EdgeMarkers[i].Rect);

          RepaintRegion(TmpRect);
        end;

      RepaintMarker(FClickedMarker);
      MarkerUnderCursor := FClickedMarker;

      InMouseMove := False;

      Exit;
    end;
  end
  else
  begin
    if SelectingRegion and (State = piSetCurve) then
    begin
      TmpRect := FSelectionRect;
      FSelectionRect.Width := MouseMovePos.X - FSelectionRect.Left;
      FSelectionRect.Height := MouseMovePos.Y - FSelectionRect.Top;

      //Refresh the rectangle containing the old and new selection
      TmpRect.Union(FSelectionRect);
      RepaintRegion(TmpRect);

      InMouseMove := False;

      Exit;
    end;
  end;

  HitMarker := nil;
  for i := 0 to Markers.Count - 1 do
  begin
    Marker := Markers[i];
    if Marker.HitTest(MouseMovePos) then
    begin
      HitMarker := Marker;
      Break;
    end;
  end;
  MarkerUnderCursor := HitMarker;

  InMouseMove := False;
end;

procedure TPlotImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  WasDragging: Boolean;
begin
  // Make sure that there are no previous events in the stack
  InMouseMove := True;
  // Empty message stack
  Application.ProcessMessages;

  WasDragging := Dragging;

  if Button = mbLeft then
  begin
    if Dragging then
    begin
      Dragging := False;

      if (State = piSetScale) and Assigned(FClickedMarker) then
        for i := 1 to 3 do
          if (FClickedMarker = AxesMarkers[i]) then
            Plot.Scale.ImagePoint[i] := FClickedMarker.Position/Zoom;

      if Assigned(OnMarkerDragged) and Assigned(FClickedMarker) then
      begin
        // Notify that neighboring markers have moved
        if (State = piSetPlotBox) then
          with Plot.Box do
          begin
            for i := 1 to NumVertices do
              if (FClickedMarker = BoxMarkers[i]) then
                Break;

            case FDragAction of
              daAngle: begin
                OnMarkerDragged(Self, BoxMarkers[NextVertIdx(i - 1) + 1], False);
                OnMarkerDragged(Self, BoxMarkers[PrevVertIdx(i - 1) + 1], False);
              end;
              daRotate: begin
                for i := 1 to NumVertices do
                begin
                  BoxMarkers[i].Move(Zoom*Vertex[i - 1]);

                  if (BoxMarkers[i] <> FClickedMarker) then
                    OnMarkerDragged(Self, BoxMarkers[i], False);
                end;
              end;
              daEdge: begin
                for i := 1 to NumEdges do
                  if (FClickedMarker = EdgeMarkers[i]) then
                    Break;

                OnMarkerDragged(Self, BoxMarkers[NextVertIdx(i - 1) + 1], False);
                OnMarkerDragged(Self, BoxMarkers[i], False);
              end;
            end;
          end;

        // Notify that the marker has been dragged
        OnMarkerDragged(Self, FClickedMarker, True);
      end;

      if Plot.Box.Rotated then
        Plot.Box.ApplyRotation;

      FDragAction := daNone;

      IsChanged := True;
    end
    else
    begin
      if SelectingRegion and (State = piSetCurve) then
      begin
        FSelectionRect.Width := X - FSelectionRect.Left;
        FSelectionRect.Height := Y - FSelectionRect.Top;
        if (Zoom <> 1) then
          with FSelectionRect do
          begin
            Left := Round(Left/Zoom);
            Top := Round(Top/Zoom);
            Right := Round(Right/Zoom);
            Bottom := Round(Bottom/Zoom);
          end;

        // Notify the parent that the user has selected a region
        if Assigned(OnRegionSelected) then
          OnRegionSelected(Self, FSelectionRect);

        SelectingRegion := False;
        RepaintRegion(FSelectionRect);
      end;

      if Assigned(FClickedMarker) then
      begin
        Markers.Move(Markers.IndexOf(FClickedMarker), 0);
        RepaintMarker(FClickedMarker);
      end;
    end;

    FClickedMarker := nil;
  end;

  if not WasDragging then
    inherited MouseUp(Button, Shift, X, Y);

  InMouseMove := False;
end;

procedure TPlotImage.SetMarkerUnderCursor(Marker: TMarker);
var
  OldMarker: TMarker;
begin
  if MarkerUnderCursor <> Marker then
  begin
    OldMarker := MarkerUnderCursor;
    FMarkerUnderCursor := Marker;

    if Assigned(OldMarker) then
      RepaintMarker(OldMarker);

    if Assigned(MarkerUnderCursor) then
      RepaintMarker(MarkerUnderCursor);
  end;
end;

procedure TPlotImage.SetActiveMarker(Marker: TMarker);
var
  OldMarker: TMarker;
begin
  if ActiveMarker <> Marker then
  begin
    OldMarker := ActiveMarker;
    FActiveMarker := Marker;

    if Assigned(OldMarker) then
      RepaintMarker(OldMarker);

    if Assigned(ActiveMarker) then
      RepaintMarker(ActiveMarker);
  end;
end;


function TPlotImage.ConvertCoords(P: TCurvePoint): TCurvePoint;
begin
  Result := Plot.Scale.FromImgToPlot(P/Zoom);
end;

function TPlotImage.ConvertCoords(X, Y: Double): TCurvePoint;
begin
  Result := ConvertCoords(TCurvePoint.Create(X, Y));
end;

function TPlotImage.GetPlotCount: Integer;
begin
  Result := FPlots.Count;
end;

function TPlotImage.GetCurveCount: Integer;
begin
  Result := Plot.CurveCount;
end;

function TPlotImage.GetCurveIndex: Integer;
begin
  Result := Plot.CurveIndex;
end;

function TPlotImage.GetNumPoints: Integer;
begin
  Result := Plot.DigitCurve.Curve.Count;
end;

function TPlotImage.GetPlot(Index: Integer): TPlot;
begin
  if (Index >= 0) and (Index < PlotCount) then
    Result := FPlots[Index]
  else
    Result := nil;
end;

function TPlotImage.GetActivePlot: TPlot;
begin
  Result := GetPlot(PlotIndex);
end;

function TPlotImage.GetPoint(Index: Integer): TCurvePoint;
begin
  Result := Plot.Scale.FromImgToPlot(Plot.Curve.Point[Index]);
end;

function TPlotImage.GetColorIsSet: Boolean;
begin
  Result := Plot.DigitCurve.ColorIsSet;
end;

function TPlotImage.GetHasPoints: Boolean;
begin
  Result := Plot.DigitCurve.HasPoints;
end;

function TPlotImage.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

function TPlotImage.GetCanUndo: Boolean;
begin
  Result := Plot.DigitCurve.CanGoBack;
end;

function TPlotImage.GetCanRedo: Boolean;
begin
  Result := Plot.DigitCurve.CanGoForward;
end;

procedure TPlotImage.SetState(Value: TPlotImageState);
begin
  if (FState <> Value) then
  begin
    // Update the markers in the curve
    UpdateMarkersInCurve;

    // Change the state
    FState := Value;
    RepaintAll;

    // Update the markers in the image
    UpdateMarkersInImage;

    // Notify the parent that the state has changed
    if Assigned(OnStateChanged) then
      OnStateChanged(Self, Value);
  end;
end;

procedure TPlotImage.SetCurveIndex(Value: Integer);
var
  TmpRect: TRect;
  TmpOnChange: TNotifyEvent;
  OldValue: Integer;
begin
  if (Value >= 0) and (Value < CurveCount) and (Value <> CurveIndex) then
  begin
    TmpOnChange := OnChange;
    OnChange := nil;

    OldValue := Plot.CurveIndex;
    // Notify the parent that the active curve is about to change
    if Assigned(OnActiveCurveChanging) then
      OnActiveCurveChanging(Self, OldValue, Value);

    // First, update all the markers in the curve
    UpdateMarkersInCurve;

    // Now change the active curve
    TmpRect := Plot.DigitCurve.CurveRect(Zoom);
    Plot.CurveIndex := Value;
    TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
    if (State = piSetCurve) then
      RepaintRegion(TmpRect);

    // Finally, update all the new markers in the image
    UpdateMarkersInImage;

    OnChange := TmpOnChange;

    // Notify the parent that the active curve has changed
    if Assigned(OnActiveCurveChanged) then
      OnActiveCurveChanged(Self, OldValue, Value);
  end;
end;

procedure TPlotImage.SetPlotIndex(Value: Integer);
var
  TmpOnChange: TNotifyEvent;
  OldValue: Integer;
begin
  if (Value >= 0) and (Value < PlotCount) and (Value <> PlotIndex) then
  begin
    TmpOnChange := OnChange;
    OnChange := nil;

    OldValue := FPlotIndex;
    // Notify the parent that the active plot is about to change
    if Assigned(OnActivePlotChanging) then
      OnActivePlotChanging(Self, OldValue, Value);

    // Update all the markers in the curve
    UpdateMarkersInCurve;
    // Change the active Plot
    FPlotIndex := Value;
    // Update the active Curve
    //CurveIndex := Plots[Value].CurveIndex;
    RepaintAll;

    // Update all the new markers in the image
    UpdateMarkersInImage;

    OnChange := TmpOnChange;

    // Notify the parent that the active plot has changed
    if Assigned(OnActivePlotChanged) then
      OnActivePlotChanged(Self, OldValue, Value);
  end;
end;

procedure TPlotImage.SetIsChanged(Value: Boolean);
begin
  if (Value <> FIsChanged) then
    FIsChanged := Value;

  // Notify the parent that the PlotImage has changed
  if (Assigned(OnChange) and IsChanged) then
    OnChange(Self);
end;

procedure TPlotImage.SetPoint(Index: Integer; const Value: TCurvePoint);
var
  TmpRect: TRect;
begin
  with Plot do
    if (Scale.FromPlotToImg(Value) <> DigitCurve.Curve.Point[Index]) then
    begin
      TmpRect := DigitCurve.CurveRect(Zoom);
      DigitCurve.Curve.Point[Index] := Scale.FromPlotToImg(Value);
      TmpRect.Union(DigitCurve.CurveRect(Zoom));
      if (State = piSetCurve) then
        RepaintRegion(TmpRect);

      FIsChanged := True;
    end;
end;

procedure TPlotImage.ClearMarkers;
var
  i: Integer;
begin
  for i := Markers.Count - 1 downto 0 do
    DeleteMarker(i, False);
end;

procedure TPlotImage.UpdateMarkersInCurve;
var
  i: Integer;
begin
  if (State = piSetCurve) and Assigned(Plot) and Assigned(Plot.DigitCurve) then
  begin
    Plot.DigitCurve.ClearMarkers;
    for i := Markers.Count - 1 downto 0 do
      Plot.DigitCurve.AddMarker(Markers[i].Position/Zoom);

    Plot.DigitCurve.SortMarkers;
  end;
end;

procedure TPlotImage.UpdateMarkersInImage;
var
  i: Integer;
begin
  ClearMarkers;

  case State of
    piSetScale: begin

      FAxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+',
        Options.YAxisColor, 3), Zoom *
        Plot.Scale.ImagePoint[1], True);
      FAxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+',
        Options.OriginColor, 3), Zoom *
        Plot.Scale.ImagePoint[2], True);
      FAxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+',
        Options.XAxisColor, 3), Zoom *
        Plot.Scale.ImagePoint[3], True);

      AddMarker(FAxesMarkers[1], False);
      AddMarker(FAxesMarkers[2], False);
      AddMarker(FAxesMarkers[3], False);
    end;
    piSetPlotBox: begin
      with Plot.Box do
        for i := 1 to 4 do
        begin
          FBoxMarkers[i] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),
            '1', clBlack, 3),
            Zoom*Vertex[i - 1], True);
          AddMarker(FBoxMarkers[i], False);

          FEdgeMarkers[i] :=
            TMarker.Create(CreateMarker(TPoint.Create(13, 13),
            '0', clBlack, 3),
            Zoom*Edge[i - 1], True);
          AddMarker(FEdgeMarkers[i], False);
        end;
    end;
    piSetCurve: begin
      for i := 0 to Plot.DigitCurve.MarkerCount - 1 do
        AddMarker(Zoom*Plot.DigitCurve.Markers[i], False);
    end;
    piSetGrid: begin
      // For now, do nothing
    end;
  end;
end;

procedure TPlotImage.RepaintRegion(UpdateArea: TRect);
begin
  Canvas.ClipRect := UpdateArea;
  {$ifdef windows}
  InvalidateRect(Handle, UpdateArea, False);
  {$else}
  Invalidate;
  {$endif}
end;

procedure TPlotImage.RepaintAll;
begin
  RepaintRegion(ClientRect);
end;

procedure TPlotImage.RectIntersection(Po, Pf: TCurvePoint;
  var Pini, Pend: TCurvePoint);
var
  P: array [1..4] of TCurvePoint;
  Idx: array [1..2] of Integer;
  i, j: Integer;
  M, b: Double;
begin
  // Mostly vertical
  if Abs(Pf.Y - Po.Y) > Abs(Pf.X - Po.X) then
  begin
    M := (Pf.X - Po.X)/(Pf.Y - Po.Y);
    b := Pf.X - M*Pf.Y;

    // Intersection with the upper edge
    P[1].Y := 0;
    P[1].X := b;

    // Intersection with the bottom edge
    P[3].Y := ClientHeight - 1;
    P[3].X := M*P[3].Y + b;

    // Not completely vertical
    if (Abs(M) > 1e-5) then
    begin
      // Intersection with the right edge
      P[2].X := ClientWidth - 1;
      P[2].Y := (P[2].X - b)/M;

      // Intersection with the left edge
      P[4].X := 0;
      P[4].Y := -b/M;
    end
    // Completely vertical
    else
    begin
      // No intersection (lines are parallel)
      P[2] := TCurvePoint.Create(-1, -1);
      P[4] := TCurvePoint.Create(-1, -1);
    end;
  end
  // Mostly horizontal
  else
  begin
    M := (Pf.Y - Po.Y)/(Pf.X - Po.X);
    b := Pf.Y - M*Pf.X;

    // Not completely horizontal
    if (Abs(M) > 1e-5) then
    begin
      // Intersection with the upper edge
      P[1].Y := 0;
      P[1].X := -b/M;

      // Intersection with the bottom edge
      P[3].Y := ClientHeight - 1;
      P[3].X := (P[3].Y - b)/M;
    end
    // Completely horizontal
    else
    begin
      // No intersection (lines are parallel)
      P[1] := TCurvePoint.Create(-1, -1);
      P[3] := TCurvePoint.Create(-1, -1);
    end;

    // Intersection with the right edge
    P[2].X := ClientWidth - 1;
    P[2].Y := M*P[2].X + b;

    // Intersection with the left edge
    P[4].X := 0;
    P[4].Y := b;
  end;

  // Find the two points that belong to the rectangle
  j := 1;
  for i := 1 to 4 do
  begin
    if ClientRect.Contains(P[i]) then
      if (j <= 2) then
      begin
        Idx[j] := i;
        Inc(j);
      end
      else
      if (P[Idx[1]].DistanceTo(P[i]) > P[Idx[1]].DistanceTo(P[Idx[2]])) then
        Idx[2] := i;
  end;

  // Assign the points
  if (Sign(Pf.X - Po.X) = Sign(P[Idx[2]].X - P[Idx[1]].X)) and
    (Sign(Pf.Y - Po.Y) = Sign(P[Idx[2]].Y - P[Idx[1]].Y)) then
  begin
    Pini := P[Idx[1]];
    Pend := P[Idx[2]];
  end
  else
  begin
    Pini := P[Idx[2]];
    Pend := P[Idx[1]];
  end;
end;

procedure TPlotImage.XAxisCoords(var Pini, Pend: TCurvePoint);
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[3]) then
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[3].Position, Pini, Pend)
  else
    with Plot.Scale do
      RectIntersection(Zoom*ImagePoint[2], Zoom*ImagePoint[3], Pini, Pend);
end;

procedure TPlotImage.YAxisCoords(var Pini, Pend: TCurvePoint);
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[1]) then
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[1].Position, Pini, Pend)
  else
    with Plot.Scale do
      RectIntersection(Zoom*ImagePoint[2], Zoom*ImagePoint[1], Pini, Pend);
end;

procedure TPlotImage.LoadImage(FileName: TFileName);
var
  TmpBmp: TBGRABitmap;
  Stream: TMemoryStream;
begin
  FImageIsLoaded := False;

  if FileExists(FileName) then
  begin
    try
      Stream := TMemoryStream.Create;

      TmpBmp := TBGRABitmap.Create(FileName);

      Stream.Clear;
      Stream.Position := 0;
      TmpBmp.SaveToStreamAsPng(Stream);

      LoadImage(Stream);
    finally
      TmpBmp.Free;
      Stream.Free;
    end;
  end
  else
  begin
    Visible := False;
    Width := 0;
    Height := 0;
  end;
end;

procedure TPlotImage.LoadImage(Stream: TStream);
begin
  FImageIsLoaded := False;

  try
    Stream.Position := 0;
    PlotImg.LoadFromStream(Stream);
    PlotImg.ReplaceTransparent(Options.BgndColor);

    ZoomFitBest;

    Width := Round(Zoom*PlotImg.Width);
    Height := Round(Zoom*PlotImg.Height);

    GridMask.SetSize(PlotImg.Width, PlotImg.Height);

    FImageIsLoaded := True;

    GridMask.IsValid := False;
    GridMask.IsActive := False;

    ResetZoomImage;

    WhiteBoard.SetSize(Width, Height);
    WhiteBoard.PutImage(0, 0, ZoomImg, dmSet);

    FMarkers.Clear;

    Invalidate;
  finally
    IsChanged := True;
    Visible := ImageIsLoaded;
  end;
end;

procedure TPlotImage.ChildChange(Sender: TObject);
begin
  // Some child (Plot, GridMask) has changed
  IsChanged := True;
end;

function TPlotImage.MoveCurve(FromIdx, ToIdx: Integer): Boolean;
begin
  Result := Plot.MoveCurve(FromIdx, ToIdx);

  if Result then
  begin
    UpdateMarkersInImage;

    IsChanged := True;
  end;
end;

function TPlotImage.MovePlot(FromIdx, ToIdx: Integer): Boolean;
begin
  Result := (FromIdx <> ToIdx) and (FromIdx >= 0) and
    (FromIdx < FPlots.Count) and (ToIdx >= 0) and (ToIdx < FPlots.Count);

  if Result then
  begin
    FPlots.Move(FromIdx, ToIdx);

    UpdateMarkersInImage;

    IsChanged := True;
  end;
end;

procedure TPlotImage.AddPlot;
var
  TmpPlot: TPlot;
begin
  TmpPlot := TPlot.Create('Plot' + IntToStr(FPlots.Count + 1));
  TmpPlot.OnChange := @ChildChange;

  FPlots.Add(TmpPlot);

  IsChanged := True;
end;

procedure TPlotImage.AddPlot(Position: Integer);
var
  TmpPlot: TPlot;
begin
  if (Position >= 0) and (Position < FPlots.Count) then
  begin
    TmpPlot := TPlot.Create('Plot' + IntToStr(Position) + 'b');
    TmpPlot.OnChange := @ChildChange;

    FPlots.Insert(Position, TmpPlot);

    IsChanged := True;
  end
  else
    AddPlot;
end;

procedure TPlotImage.DeletePlot;
begin
  DeletePlot(PlotIndex);
end;

procedure TPlotImage.DeletePlot(Index: Integer);
begin
  if (Index >= 0) and (Index < PlotCount) then
  begin
    FPlots.Delete(Index);
    if (PlotIndex >= PlotCount) then
      PlotIndex := PlotCount - 1;

    IsChanged := True;
  end;
end;

procedure TPlotImage.AddCurve;
begin
  Plot.AddCurve;

  IsChanged := True;
end;

procedure TPlotImage.AddCurve(Position: Integer);
begin
  Plot.AddCurve(Position);

  IsChanged := True;
end;

procedure TPlotImage.DeleteCurve;
begin
  Plot.DeleteCurve;

  IsChanged := True;
end;

procedure TPlotImage.DeleteCurve(Index: Integer);
begin
  if (Index >= 0) and (Index < CurveCount) then
  begin
    Plot.DeleteCurve(Index);

    IsChanged := True;
  end;
end;

procedure TPlotImage.ClearCurve;
begin
  ClearCurve(CurveIndex);
end;

procedure TPlotImage.ClearCurve(Index: Integer);
begin
  if (Index >= 0) and (Index < CurveCount) then
  begin
    RepaintRegion(Plot.Curves[Index].CurveRect(Zoom));
    Plot.Curves[Index].Clear;

    IsChanged := True;
  end;
end;

procedure TPlotImage.UndoCurveChanges;
var
  TmpRect: TRect;
begin
  with Plot.DigitCurve do
    if CanGoBack then
    begin
      TmpRect := CurveRect(Zoom);
      GoBack;
      TmpRect.Union(CurveRect(Zoom));
      if (State = piSetCurve) then
        RepaintRegion(TmpRect);

      IsChanged := True;
    end;
end;

procedure TPlotImage.RedoCurveChanges;
var
  TmpRect: TRect;
begin
  with Plot.DigitCurve do
    if CanGoForward then
    begin
      TmpRect := CurveRect(Zoom);
      GoForward;
      TmpRect.Union(CurveRect(Zoom));
      if (State = piSetCurve) then
        RepaintRegion(TmpRect);

      IsChanged := True;
    end;
end;

procedure TPlotImage.SortCurve;
begin
  SortCurve(CurveIndex);
end;

procedure TPlotImage.SortCurve(Index: Integer);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := Plot.PlotCurves[Index];
    TmpCurve.SortCurve;

    Plot.Curves[Index].Curve.Clear;
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curves[Index].Curve.AddPoint(Plot.Scale.FromPlotToImg(TmpCurve.Point[i]));

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Smooth(k, d: Integer; Index: Integer);
var
  i: Integer;
  TmpRect: TRect;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := Plot.PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Smooth(k, d);

    if (Index = CurveIndex) and (State = piSetCurve) then
      TmpRect := Plot.DigitCurve.CurveRect(Zoom);

    Plot.Curves[Index].NextCurve(False);
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curves[Index].Curve.AddPoint(Plot.Scale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) and (State = piSetCurve) then
    begin
      TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
      RepaintRegion(TmpRect);
    end;

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Smooth(k, d: Integer; AllCurves: Boolean = False);
var
  i: Integer;
begin
  if not AllCurves then
    Smooth(k, d, CurveIndex)
  else
    for i := 0 to CurveCount - 1 do
      Smooth(k, d, i);
end;

procedure TPlotImage.Interpolate(n, d: Integer; Index: Integer;
  IntType: TInterpolation = itpBSpline);
var
  i: Integer;
  TmpRect: TRect;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := Plot.PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(n, d, Plot.Scale.XScale, IntType);

    if (Index = CurveIndex) and (State = piSetCurve) then
      TmpRect := Plot.DigitCurve.CurveRect(Zoom);

    Plot.Curves[Index].NextCurve(False);
    Plot.Curves[Index].ShowAsSymbols := False;
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curves[Index].Curve.AddPoint(Plot.Scale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) and (State = piSetCurve) then
    begin
      TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
      RepaintRegion(TmpRect);
    end;

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Interpolate(n, d: Integer; AllCurves: Boolean = False;
  IntType: TInterpolation = itpBSpline);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(n, d, CurveIndex, IntType)
  else
    for i := 0 to CurveCount - 1 do
      Interpolate(n, d, i, IntType);
end;

procedure TPlotImage.Interpolate(Xo, Xf: Double; n, d: Integer;
  Index: Integer; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
  TmpRect: TRect;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := Plot.PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(Xo, Xf, n, d, Plot.Scale.XScale, IntType);

    if (Index = CurveIndex) and (State = piSetCurve) then
      TmpRect := Plot.DigitCurve.CurveRect(Zoom);

    Plot.Curves[Index].NextCurve(False);
    Plot.Curves[Index].ShowAsSymbols := False;
    for i := 0 to TmpCurve.Count - 1 do
      Plot.Curves[Index].Curve.AddPoint(Plot.Scale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) and (State = piSetCurve) then
    begin
      TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
      RepaintRegion(TmpRect);
    end;

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Interpolate(Xo, Xf: Double; n, d: Integer;
  AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(Xo, Xf, n, d, CurveIndex, IntType)
  else
    for i := 0 to CurveCount - 1 do
      Interpolate(Xo, Xf, n, d, i, IntType);
end;

procedure TPlotImage.CorrectCurve(Po, Pf: TPoint; IsStep: Boolean = True);
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.CorrectCurve(Po, Pf, IsStep);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.CorrectCurve(Po, Pf: TCurvePoint; IsStep: Boolean = True);
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.CorrectCurve(Po, Pf, IsStep);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.CorrectCurve(Region: TRect; IsStep: Boolean = True);
begin
  CorrectCurve(Region.TopLeft, Region.BottomRight, IsStep);
end;

procedure TPlotImage.CorrectCurve(IsStep: Boolean = True);
begin
  CorrectCurve(SelectionRect, IsStep);
end;

procedure TPlotImage.RemoveGrid(LineColor1, LineColor2, BckgndColor: TColor;
  Tolerance: Integer = 10;
  Threshold: Double = 0.5;
  FixCurve: Boolean = False;
  MaskSize: Integer = 5);
var
  i: Integer;
  Boxes: TBoxArray;
begin
  try
    GridMask.MajorGridColor := LineColor1;
    GridMask.MinorGridColor := LineColor2;
    GridMask.BckgndColor := BckgndColor;
    GridMask.Tolerance := Tolerance;
    GridMask.Threshold := Threshold;
    GridMask.FixCurve := FixCurve;
    GridMask.MaskSize := MaskSize;

    SetLength(Boxes, PlotCount);
    for i := Low(Boxes) to High(Boxes) do
    begin
      Plots[i].Box.PolarCoordinates := (Plot.Scale.CoordSystem = csPolar);
      Boxes[i] := Plots[i].Box;
    end;

    if Plot.Scale.CoordSystem = csCartesian then
      GridMask.RemoveCartesianGrid(PlotImg, Boxes)
    else
      GridMask.RemovePolarGrid(PlotImg, Boxes, Plot.Scale.ImagePoint[2]);

    if GridMask.FixCurve then
    begin
      for i := 0 to CurveCount - 1 do
        GridMask.RebuildCurve(PlotImg, Boxes, Plot.Curves[i].Color);
    end;

    ResetZoomImage;
  finally
    SetLength(Boxes, 0);

    IsChanged := True;
  end;
end;

procedure TPlotImage.GroupPoints(Region: TRect);
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.GroupPointsInRegion(Region);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.DeletePoints(Region: TRect);
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.DeletePointsInRegion(Region);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveUp;
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.AddToY(-1);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveDown;
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.AddToY(1);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveLeft;
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.AddToX(-1);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveRight;
var
  TmpRect: TRect;
begin
  TmpRect := Plot.DigitCurve.CurveRect(Zoom);
  Plot.DigitCurve.AddToX(1);
  TmpRect.Union(Plot.DigitCurve.CurveRect(Zoom));
  if (State = piSetCurve) then
    RepaintRegion(TmpRect);

  IsChanged := True;
end;

procedure TPlotImage.ClearMarker(Marker: TMarker);
var
  i: Integer;
begin
  if (Markers.IndexOf(Marker) > -1) then
  begin
    with WhiteBoard.CanvasBGRA do
    begin
      CopyRect(Marker.Rect, ZoomImg, Marker.Rect);
    end;

    for i := Markers.Count - 1 downto 0 do
      if (Markers[i] <> Marker) and not (Markers[i].Rect*Marker.Rect).IsEmpty then
        Markers[i].Draw(WhiteBoard, Marker.Rect);
  end;
end;

procedure TPlotImage.DrawMarker(Marker: TMarker);
begin
  if (Markers.IndexOf(Marker) > -1) then
    Marker.Draw(WhiteBoard, Marker.Rect);
end;

procedure TPlotImage.MoveMarker(Marker: TMarker; Point: TPoint);
begin
  if (Markers.IndexOf(Marker) > -1) then
  begin
    ClearMarker(Marker);
    Marker.Move(Point);
    DrawMarker(Marker);

    IsChanged := True;
  end;
end;

procedure TPlotImage.MoveMarker(Marker: TMarker; X, Y: Double);
begin
  MoveMarker(Marker, TPoint.Create(Round(X), Round(Y)));
end;

procedure TPlotImage.UndistortImage;
var
  w, h: Integer;
  Stream: TMemoryStream;
  NewImg: TBGRABitmap;
begin
  try
    with Plot.Box do
    begin
      w := Round(Max(Vertex[0].DistanceTo(Vertex[1]),
        Vertex[2].DistanceTo(Vertex[3])));
      h := Round(Max(Vertex[0].DistanceTo(Vertex[3]),
        Vertex[1].DistanceTo(Vertex[2])));

      NewImg := TBGRABitmap.Create(w, h, BGRABlack);

      NewImg.FillPolyLinearMapping([PointF(0, 0), PointF(w - 1, 0),
        PointF(w - 1, h - 1), PointF(0, h - 1)],
        PlotImg, PolygonPoints[1], True);
    end;

    Stream := TMemoryStream.Create;
    Stream.Clear;
    Stream.Position := 0;

    NewImg.SaveToStreamAsPng(Stream);

    // Reset the plot
    Reset;
    // Load image
    LoadImage(Stream);
    // Reset markers
    SetPlotPointMarkers(True);
    State := piSetPlotBox;

    IsChanged := True;
  finally
    NewImg.Free;
    Stream.Free;
  end;
end;

function TPlotImage.GetZoomImage(w, h: Integer; Region: TRect): TBitmap;
var
  TmpZoomImg: TBGRABitmap;
begin
  try
    Result := TBitmap.Create;
    Result.SetSize(w, h);
    Result.PixelFormat := pf24bit;

    TmpZoomImg := TBGRABitmap.Create(Region.Width, Region.Height, clWhite);
    TmpZoomImg.CanvasBGRA.CopyRect(TRect.Create(0, 0, Region.Width, Region.Height),
      WhiteBoard, Region);

    //BGRAReplace(TmpZoomImg, TmpZoomImg.Resample(w, h, rmSimpleStretch));
    TmpZoomImg.ResampleFilter := rfSpline;
    BGRAReplace(TmpZoomImg, TmpZoomImg.Resample(w, h, rmFineResample));
    TmpZoomImg.Draw(Result.Canvas, 0, 0, True);
  finally
    TmpZoomImg.Free;
  end;
end;

function TPlotImage.SaveToXML(FileName: TFileName): Boolean;
var
  i: Integer;
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  XMLDoc: TXMLDocument;
  RootNode, DigitNode, ImageNode, PathNode, FileNode, DataNode,
  CDataNode, PlotNode: TDOMNode;
begin
  // Update markers in current curve
  UpdateMarkersInCurve;

  Result := False;
  try
    // Create a document
    XMLDoc := TXMLDocument.Create;
    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    // Create a root node
    RootNode := XMLDoc.CreateElement('digitization');
    TDOMElement(RootNode).SetAttribute('version', '2.0');
    XMLDoc.Appendchild(RootNode); // Save root node

    // Create document node
    RootNode := XMLDoc.DocumentElement;
    DigitNode := XMLDoc.CreateElement('document');
    // Create atributes to document node
    with TDOMElement(DigitNode) do
      SetAttribute('ImageIsLoaded', UTF8Decode(BoolToStr(ImageIsLoaded, True)));

    if ImageIsLoaded then
    begin
      ImageNode := XMLDoc.CreateElement('image');
      with TDOMElement(ImageNode) do
      begin
        SetAttribute('Width', UTF8Decode(IntToStr(Width)));
        SetAttribute('Height', UTF8Decode(IntToStr(Height)));
      end;

      PathNode := XMLDoc.CreateElement('path');
      PathNode.Appendchild(XMLDoc.CreateTextNode(
        UTF8Decode(ExtractFilePath(ImageName))));
      ImageNode.Appendchild(PathNode);

      FileNode := XMLDoc.CreateElement('name');
      FileNode.Appendchild(XMLDoc.CreateTextNode(
        UTF8Decode(ExtractFileName(ImageName))));
      ImageNode.Appendchild(FileNode);

      DataNode := XMLDoc.CreateElement('data');
      // Now encode the image and save it to the XML file
      Stream.Clear;
      Stream.Position := 0;

      PlotImg.SaveToStreamAsPng(Stream);

      // Extract Image contents as a common string
      SetString(Buffer, Stream.Memory, Stream.Size);
      // Encode the image contents in a Base64 string
      EncBuffer := EncodeStringBase64(Buffer);
      // Store the image on a CData-section node
      CDataNode := XMLDoc.CreateCDATASection(UTF8Decode(EncBuffer));
      DataNode.Appendchild(CDataNode);
      ImageNode.Appendchild(DataNode);

      DigitNode.Appendchild(ImageNode);
    end;

    // Add Grid node
    if GridMask.IsValid then
      DigitNode.Appendchild(GridMask.ExportToXML(XMLDoc));

    // Save document node
    RootNode.Appendchild(DigitNode);

    // Create plots node
    PlotNode := XMLDoc.CreateElement('plots');
    TDOMElement(PlotNode).SetAttribute('Count', UTF8Decode(IntToStr(PlotCount)));

    // Save plot nodes
    for i := 0 to PlotCount - 1 do
      PlotNode.Appendchild(Plots[i].ExportToXML(XMLDoc));

    // Save Plot node
    RootNode.Appendchild(PlotNode);

    WriteXMLFile(XMLDoc, FileName);
    Result := True;
  finally
    XMLDoc.Free;
    Stream.Free;

    FIsChanged := False;
  end;
end;

function TPlotImage.LoadFromXML(FileName: TFileName;
  PictureDlg: TOpenPictureDialog = nil): Boolean;
var
  i, w, h, SavedCurveCount, RealCurveCount, SavedPlotCount, RealPlotCount: Integer;
  DigitVersion: Double;
  Path, ImgName: TFileName;
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  XMLDoc: TXMLDocument;
  Child, DigitChild, ImageChild, CurveChild, PlotChild: TDOMNode;

  ImageLoaded: Boolean;
  PlotBoxLoaded: Boolean;

  TmpOnChange: TNotifyEvent;
begin
  TmpOnChange := OnChange;
  OnChange := nil;

  ImageIsLoaded := False;

  Result := False;
  try
    // Read the XML document
    ReadXMLFile(XMLDoc, FileName);
    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    with XMLDoc.DocumentElement.Attributes do
      for i := 0 to Length - 1 do
        if (Item[i].CompareName('version') = 0) then
          if not TryStrToFloat(UTF8Encode(Item[i].NodeValue), DigitVersion) then
            DigitVersion := 1.0;

    PlotBoxLoaded := (DigitVersion > 1);

    Child := XMLDoc.DocumentElement.FirstChild;
    while Assigned(Child) do
    begin
      if (Child.CompareName('document') = 0) then
      begin
        ImageLoaded := False;
        with Child.Attributes do
        begin
          for i := 0 to Length - 1 do
          begin
            if (Item[i].CompareName('ImageIsLoaded') = 0) then
              ImageLoaded := StrToBool(UTF8Encode(Item[i].NodeValue));
          end;
        end;

        DigitChild := Child.FirstChild;
        while Assigned(DigitChild) do
        begin
          if (DigitChild.CompareName('image') = 0) then
          begin
            w := 0;
            h := 0;
            with DigitChild.Attributes do
            begin
              for i := 0 to Length - 1 do
              begin
                if (Item[i].CompareName('Width') = 0) then
                  w := StrToInt(UTF8Encode(Item[i].NodeValue));
                if (Item[i].CompareName('Height') = 0) then
                  h := StrToInt(UTF8Encode(Item[i].NodeValue));
              end;
            end;

            Path := '';
            ImgName := '';

            ImageChild := DigitChild.FirstChild;
            while Assigned(ImageChild) do
            begin
              if (ImageChild.CompareName('path') = 0) then
                if Assigned(ImageChild.FirstChild) then
                  Path := UTF8Encode(ImageChild.FirstChild.NodeValue)
                else
                  Path := '';

              if (ImageChild.CompareName('name') = 0) then
                if Assigned(ImageChild.FirstChild) then
                  ImgName := UTF8Encode(ImageChild.FirstChild.NodeValue)
                else
                  ImgName := '';

              // It is the data image
              if (ImageChild.CompareName('data') = 0) and
                Assigned(ImageChild.FirstChild) then
              begin
                // Extract the image data as a Base64 string
                EncBuffer := UTF8Encode(ImageChild.FirstChild.NodeValue);
                // Convert it back to an ordinary string
                Buffer := DecodeStringBase64(EncBuffer);

                // Put the image data in the stream
                Stream.Clear;
                Stream.Write(Pointer(Buffer)^, Length(Buffer));
                Stream.Position := 0;

                // And load the stream in the TPicture
                LoadImage(Stream);
              end;

              ImageChild := ImageChild.NextSibling;
            end;
          end;
          // All image parameters have been read

          // Read Plot parameters (only for older versions)
          if (DigitChild.CompareName('scale') = 0) and
            (DigitVersion < 1.5) then
            Plot.Scale.ImportFromXML(DigitChild);

          // Read Box parameters (only for older versions)
          if (DigitChild.CompareName('PlotBox') = 0) and
            (DigitVersion <= 1.0) then
            with Plot.Box do
            begin
              ImportFromXML(DigitChild);
              PolarCoordinates := (Plot.Scale.CoordSystem = csPolar);
              PlotBoxLoaded := True;
            end;

          // Read Grid parameters
          if (DigitChild.CompareName('grid') = 0) then
            GridMask.ImportFromXML(DigitChild);

          // Go for the next element
          DigitChild := DigitChild.NextSibling;
        end;
      end;
      // All document parameters have been read

      // Read plots (only for newer versions of the document)
      if ((Child.CompareName('scales') = 0) and (DigitVersion >= 1.5)) or
        ((Child.CompareName('plots') = 0) and (DigitVersion >= 2)) then
      begin
        SavedPlotCount := 0;
        for i := 0 to Child.Attributes.Length - 1 do
          if (Child.Attributes.Item[i].CompareName('Count') = 0) then
            SavedPlotCount := StrToInt(UTF8Encode(Child.Attributes.Item[i].NodeValue));

        RealPlotCount := 0;
        PlotChild := Child.FirstChild;
        while Assigned(PlotChild) do
        begin
          if (PlotChild.CompareName('scale') = 0) or
            (PlotChild.CompareName('plot') = 0) then
          begin
            Inc(RealPlotCount);
            //Create all the needed scales (plots)
            while (RealPlotCount > PlotCount) do
              AddPlot;

            Plots[RealPlotCount - 1].ImportFromXML(PlotChild);
          end;

          // Go for the next scale (plot)
          PlotChild := PlotChild.NextSibling;
        end;

        assert(SavedPlotCount = RealPlotCount,
          Format('Error: The number of saved plots (%d)' +
          ' does not match the expected value (%d).',
          [RealPlotCount, SavedPlotCount]));
      end;

      // Read curves (only for older versions of the document)
      if (Child.CompareName('curves') = 0) and (DigitVersion < 1.5) then
      begin
        SavedCurveCount := 0;
        for i := 0 to Child.Attributes.Length - 1 do
          if (Child.Attributes.Item[i].CompareName('Count') = 0) then
            SavedCurveCount := StrToInt(UTF8Encode(Child.Attributes.Item[i].NodeValue));

        RealCurveCount := 0;
        CurveChild := Child.FirstChild;
        while Assigned(CurveChild) do
        begin
          if (CurveChild.CompareName('curve') = 0) then
          begin
            Inc(RealCurveCount);
            //Create all the needed curves
            while (RealCurveCount > CurveCount) do
              Self.AddCurve;

            Plot.Curves[RealCurveCount - 1].ImportFromXML(CurveChild);
          end;

          // Go for the next curve
          CurveChild := CurveChild.NextSibling;
        end;

        assert(SavedCurveCount = RealCurveCount, Format('Error: The number of saved curves (%d)' +
          ' does not match the expected value (%d).', [RealCurveCount, SavedCurveCount]));
      end;

      Child := Child.NextSibling;
    end;
    // All digitization parameters have been read

    //All is set, now
    if ImageIsLoaded then
    begin
      FImageName := Path + ImgName;
    end
    else
    begin
      // Load Image (only if it is necessary) and draw curve
      if ImageLoaded then
      begin
        if FileExists(Path + ImgName) then
          ImageName := Path + ImgName
        else
        if FileExists(ExtractFilePath(FileName) + ImgName) then
        begin
          ImageName := ExtractFilePath(FileName) + ImgName;
        end
        else
        begin
          if Assigned(PictureDlg) then
          begin
            PictureDlg.InitialDir := ExtractFilePath(FileName);
            if PictureDlg.Execute then
              ImageName := PictureDlg.FileName;
          end;
        end;
      end;
    end;

    FPlotIndex := 0;
    Plot.CurveIndex := 0;
    UpdateMarkersInImage;

    if ImageIsLoaded then
    begin
      ZoomFitBest;

      if not PlotBoxLoaded then
        ResetPlotBox;

      State := piSetCurve;
    end;

    // Reset the zoom
    ResetZoomImage;

    Result := True;
  finally
    XMLDoc.Free;
    Stream.Free;

    OnChange := TmpOnChange;
    FIsChanged := False;
  end;
end;

//=============================|TPlotImage|===================================//

end.
