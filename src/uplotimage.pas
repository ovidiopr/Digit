unit uplotimage;

{$mode objfpc}{$H+}

interface

uses {$ifdef windows}Windows,{$endif} Forms, Classes, Controls, Graphics,
     ExtDlgs, Fgl, ComCtrls, SysUtils, DOM, XMLWrite, XMLRead, Dialogs, Types,
     Base64, BGRABitmap, BGRABitmapTypes, BGRAreadTiff, Math,
     uutils, ucurves, ucoordinates, uscale, ugrid;

const
  ZoomLevels: Array [1..17] of Double = (1/10, 1/8, 1/6, 1/5, 1/4, 1/3, 1/2,
                                         2/3, 1, 3/2, 2, 3, 4, 5, 6, 8, 10);

type
  TPlotImageState = (piSetCurve, piSetScale, piSetPlotBox, piSetGrid);
  TDragAction = (daNone, daVertex, daEdge, daAngle, daRotate);

  TMarker = class
  protected
    FBitmap: TBGRABitmap;
    FRect: TRect;
    FPersistent: Boolean;
  private
    function GetPosition: TCurvePoint;

    procedure SetPosition(const Value: TCurvePoint);
  public
    constructor Create(Bitmap: TBGRABitmap; Coord: TPoint; Persistent: Boolean = False);
    destructor Destroy; override;

    function HitTest(Point: TPoint): Boolean;
    procedure Draw(Img: TBGRABitmap; Rectangle: TRect);
    procedure Move(Point: TPoint);
    procedure Shift(Delta: TPoint);

    property Bitmap: TBGRABitmap read FBitmap;
    property Rect: TRect read FRect;
    property Position: TCurvePoint read GetPosition write SetPosition;
    property IsPersistent: Boolean read FPersistent;
  end;

  TMarkerList = specialize TFPGObjectList<TMarker>;

  TShowProgressEvent = procedure(Sender: TObject; Progress: Cardinal; Msg: String) of Object;
  THideProgressEvent = procedure(Sender: TObject) of Object;
  TSelectRegionEvent = procedure(Sender: TObject; RegionRect: TRect) of Object;
  TStateChangeEvent = procedure(Sender: TObject; NewState: TPlotImageState) of Object;
  TMarkerDraggedEvent = procedure(Sender: TObject; Marker: TMarker; Zoom: Boolean) of Object;
  TZoomChangeEvent = procedure(Sender: TObject; Zoom: Double) of Object;

  TPlotImage = class(TCustomControl)
  protected type
    TCurveList = specialize TFPGObjectList<TDigitCurve>;
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
    FAxesMarkers: Array [1..3] of TMarker;
    FBoxMarkers: Array [1..4] of TMarker;
    FEdgeMarkers: Array [1..4] of TMarker;
    FMarkerUnderCursor: TMarker;
    FActiveMarker: TMarker;
    FClickedMarker: TMarker;
    FClickedPoint: TPoint;
    FClickedCoord: TPoint;
    FDragging: Boolean;
    FSelectingRegion: Boolean;
    FSelectionRect: TRect;

    FCurves: TCurveList;
    FCurveIndex: Integer;

    FScale: TScale;

    FImageIsLoaded: Boolean;

    FIsChanged: Boolean;
    FOnChange: TNotifyEvent;

    FOnShowProgress: TShowProgressEvent;
    FOnHideProgress: THideProgressEvent;
    FOnRegionSelected: TSelectRegionEvent;
    FOnStateChanged: TStateChangeEvent;
    FOnMarkerDragged: TMarkerDraggedEvent;
    FOnZoomChanged: TZoomChangeEvent;


    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    function GetAxesPoint(Index: Integer): TCurvePoint;
    function GetBoxVertex(Index: Integer): TCurvePoint;
    function GetAxesMarkers(Index: Integer): TMarker;
    function GetBoxMarkers(Index: Integer): TMarker;
    function GetEdgeMarkers(Index: Integer): TMarker;

    function GetCount: Integer;
    function GetCurve(Index: Integer): TDigitCurve;
    function GetActiveCurve: TCurve;
    function GetDigitCurve: TDigitCurve;
    function GetPlotCurves(Index: Integer): TCurve;
    function GetPlotCurve: TCurve;

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
    procedure SetIsChanged(Value: Boolean);

    procedure SetPoint(Index: Integer; const Value: TCurvePoint);

    procedure ClearMarkers;
    procedure UpdateMarkersInCurve;
    procedure UpdateMarkersInImage;

    procedure UpdateRegion(UpdateArea: TRect); overload;
    procedure UpdateRegion; overload;

    procedure EraseCurve(Curve: TDigitCurve);

    procedure RectIntersection(Po, Pf: TCurvePoint; var Pini, Pend: TCurvePoint);
    procedure XAxisCoords(var Pini, Pend: TCurvePoint);
    procedure YAxisCoords(var Pini, Pend: TCurvePoint);

    procedure LoadImage(FileName: TFileName); overload;
    procedure LoadImage(Stream: TStream); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;
    procedure SetPlotPointMarkers(ResetPoints: Boolean = False);

    function GetPixel(X, Y: Integer): LongInt; overload;
    function GetPixel(X, Y: Double): LongInt; overload;
    function GetPixel(P: TCurvePoint): LongInt; overload;

    procedure FindCurvePoints;

    function FindNextPoint(var Pv: TCurvePoint; Interval: Integer;
                           ScanX: Boolean = False): Boolean;
    procedure DigitizeSpectrum(Pi: TCurvePoint;
                               FillCurvePoints: Boolean = True); overload;
    procedure DigitizeSpectrum; overload;

    procedure DigitizeColor;
    procedure DigitizeMarkers;

    procedure FillIsland(Pi: TCurvePoint; var Island: TIsland;
                         JustInY: Boolean = True;
                         MoveUp: Boolean = True;
                         MoveDown: Boolean = True;
                         MaxPoints: Integer = 1000;
                         FillCurvePoints: Boolean = True); overload;
    procedure FillIsland(Xi, Yi: Double; var Island: TIsland;
                         JustInY: Boolean = True;
                         MoveUp: Boolean = True;
                         MoveDown: Boolean = True;
                         MaxPoints: Integer = 1000); overload;
    procedure AdjustCurve(Noisy: Boolean = False);
    procedure ConvertCurveToSymbols;

    function ConvertCoords(p: TCurvePoint): TCurvePoint; overload;
    function ConvertCoords(X, Y: Double): TCurvePoint; overload;

    procedure SwitchGrid;
    procedure ResetPlotBox;

    procedure PasteImage(Stream: TStream);

    procedure ZoomOriginal;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomFitBest;

    procedure AddMarker(Position: TPoint; NewMarker: Boolean = True); overload;
    procedure AddMarker(Marker: TMarker; NewMarker: Boolean = True); overload;
    procedure UpdateMarker(Marker: TMarker);
    procedure DeleteMarker(Marker: TMarker; RealDelete: Boolean = True); overload;
    procedure DeleteMarker(Index: Integer; RealDelete: Boolean = True); overload;
    procedure DeleteMarkerUnderCursor;
    procedure DeleteActiveMarker;
    procedure ShiftActiveMarker(Delta: TPoint);
    procedure RedrawMarkers;

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
    procedure Interpolate(n, d: Integer; Index: Integer; IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(n, d: Integer; AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(Xo, Xf: Double; n, d: Integer; Index: Integer; IntType: TInterpolation = itpBSpline); overload;
    procedure Interpolate(Xo, Xf: Double; n, d: Integer; AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline); overload;
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
    function LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;

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
    property MarkerUnderCursor: TMarker read FMarkerUnderCursor write SetMarkerUnderCursor;
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
    property CancelAction: Boolean read FCancelAction write FCancelAction;

    {Return the number of curves}
    property Count: Integer read GetCount;
    {Return the index of the active curve}
    property CurveIndex: Integer read FCurveIndex write SetCurveIndex;
    {Return the curve}
    property ucurves[Index: Integer]: TDigitCurve read GetCurve; default;
    {Return the active curve (TCurve)}
    property Curve: TCurve read GetActiveCurve;
    {Return the active curve (TDigitCurve)}
    property DigitCurve: TDigitCurve read GetDigitCurve;
    {Return the given curve converted to plot scale}
    property PlotCurves[Index: Integer]: TCurve read GetPlotCurves;
    {Return the active curve converted to plot scale}
    property PlotCurve: TCurve read GetPlotCurve;
    property ImageIsLoaded: Boolean read FImageIsLoaded write FImageIsLoaded;

    {Return the number of points in the active curve}
    property NumPoints: Integer read GetNumPoints;
    property Point[Index: Integer]: TCurvePoint read GetPoint write SetPoint;

    property Scale: TScale read FScale;
    property ColorIsSet: Boolean read GetColorIsSet;
    property HasPoints: Boolean read GetHasPoints;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnShowProgress: TShowProgressEvent read FOnShowProgress write FOnShowProgress;
    property OnHideProgress: THideProgressEvent read FOnHideProgress write FOnHideProgress;
    property OnRegionSelected: TSelectRegionEvent read FOnRegionSelected write FOnRegionSelected;
    property OnStateChanged: TStateChangeEvent read FOnStateChanged write FOnStateChanged;
    property OnMarkerDragged: TMarkerDraggedEvent read FOnMarkerDragged write FOnMarkerDragged;
    property OnZoomChanged: TZoomChangeEvent read FOnZoomChanged write FOnZoomChanged;
  end;

function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor; LineWith: Integer = 3): TBGRABitmap;

implementation

function TMarkerComparator(const a, b: TMarker): Integer;
begin
  Result := Sign(a.Position.X - b.Position.X);
end;

//==============================|Bitmaps|=====================================//
function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor; LineWith: Integer = 3): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Size.X, Size.Y, BGRAPixelTransparent);

  with Result do
  begin
    case Symbol of
      'x', 'X': begin
        DrawLineAntialias(0, 0, Width - 1, Height - 1, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, Height - 1, Width - 1, 0, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, 0, Width - 1, Height - 1, Color, LineWith);
        DrawLineAntialias(0, Height - 1, Width - 1, 0, Color, LineWith);
      end;
      '+': begin
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, LineWith);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, LineWith);
      end;
      '0': begin
        EllipseAntialias(Width div 2, Height div 2,
                         (Width - LineWith) div 2, (Height - LineWith) div 2,
                         Color, LineWith);

        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, 1);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, 1);
      end;
      '1': begin
        RectangleAntialias(1, 1, Width - 2, Height - 2, Color, LineWith);

        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, 1);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, 1);
      end;
      'c', 'C': begin
        EllipseAntialias(Width div 2, Height div 2,
                         (Width - LineWith) div 2, (Height - LineWith) div 2,
                         clWhite - Color, LineWith, Color);
      end;
      'o', 'O': begin
        EllipseAntialias(Width div 2, Height div 2,
                         (Width - LineWith) div 2, (Height - LineWith) div 2,
                         Color, LineWith);
      end;
      'r', 'R': begin
        RectangleAntialias(1, 1, Width - 2, Height - 2, clWhite - Color,
                           LineWith, Color);
      end;
      'q', 'Q': begin
        RectangleAntialias(1, 1, Width - 2, Height - 2, Color, LineWith);
      end;
    end;
  end;
end;
//==============================|Bitmaps|=====================================//

//==============================|TMarker|=====================================//
constructor TMarker.Create(Bitmap: TBGRABitmap; Coord: TPoint; Persistent: Boolean = False);
var
  Delta: TPoint;
begin
  inherited Create;
  FBitmap := Bitmap;
  Delta := TPoint.Create(Bitmap.Width div 2, Bitmap.Height div 2);
  FRect := TRect.Create(Coord - Delta, Bitmap.Width, Bitmap.Height);
  FPersistent := Persistent;
end;

destructor TMarker.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TMarker.Draw(Img: TBGRABitmap; Rectangle: TRect);
begin
  if not (Rectangle*Rect).IsEmpty then
    Img.PutImage(Rect.Left, Rect.Top, Bitmap, dmDrawWithTransparency);
end;

procedure TMarker.Move(Point: TPoint);
var
  Delta: TPoint;
begin
  Delta := TPoint.Create(Bitmap.Width div 2, Bitmap.Height div 2);
  FRect := TRect.Create(Point - Delta, Bitmap.Width, Bitmap.Height);
end;

procedure TMarker.Shift(Delta: TPoint);
begin
  Move(FRect.CenterPoint + Delta);
end;

function TMarker.GetPosition: TCurvePoint;
begin
  Result := TCurvePoint.Create(FRect.CenterPoint.X, FRect.CenterPoint.Y);
end;

function TMarker.HitTest(Point: TPoint): Boolean;
begin
  Result := Rect.Contains(Point);
  if Result then
  begin
    Point := Point - Rect.TopLeft;
    Result := Bitmap.GetPixel(Point.X, Point.Y) <> BGRAPixelTransparent;
  end;
end;

procedure TMarker.SetPosition(const Value: TCurvePoint);
begin
  Move(TPoint.Create(Round(Value.X), Round(Value.Y)));
end;
//==============================|TMarker|=====================================//

//=============================|TPlotImage|===================================//
constructor TPlotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlotImg := TBGRABitmap.Create;
  FZoomImg := TBGRABitmap.Create;
  FWhiteBoard := TBGRABitmap.Create;
  FGridMask := TGridMask.Create(0, 0);

  FMarkers := TMarkerList.Create;
  FMarkerList := TCurve.Create;

  FCurves := TCurveList.Create;
  FScale := TScale.Create;

  Reset;
end;

destructor TPlotImage.Destroy;
begin
  FGridMask.Free;
  FWhiteBoard.Free;
  FZoomImg.Free;
  FPlotImg.Free;

  FMarkers.Free;
  FMarkerList.Free;

  FCurves.Free;
  FScale.Free;

  inherited Destroy;
end;

procedure TPlotImage.Reset;
var
  i: Integer;
begin
  FState := piSetCurve;
  FDragAction := daNone;
  FZoom := 1;

  FCurves.Clear;
  FCurves.Add(TDigitCurve.Create('Curve1'));

  FCurveIndex := 0;

  FScale.Reset;

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

  function PutInside(p: TCurvePoint; w, h: Integer; d: Integer = 0): TCurvePoint;
  begin
    Result := p;

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

        Scale.PlotPoint[1] := TCurvePoint.Create(0, 1);
        Scale.PlotPoint[2] := TCurvePoint.Create(0, 0);
        Scale.PlotPoint[3] := TCurvePoint.Create(1, 0);

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
  // Maximum difference allowed (in shadows of grey)
  Tolerance: Integer;

  i, j: Integer;
  p, p1, p2: PBGRAPixel;
  C1: LongInt;
  R1, G1, B1: Byte;
begin
  // Only if necessary
  if (not DigitCurve.ValidPoints) then
  begin
    // Notify the parent that it must show the progress bar
    if Assigned(OnShowProgress) then
      OnShowProgress(Self, 0, 'Finding points...');

    RunningAction := True;
    CancelAction := False;

    Tolerance := DigitCurve.Tolerance;
    C1 := ColorToRGB(DigitCurve.Color);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);

    DigitCurve.AllPoints.Clear;
    for j := 0 to PlotImg.Height - 1 do
    begin
      p1 := GridMask.Mask.Scanline[j];
      p2 := PlotImg.Scanline[j];

      for i := 0 to PlotImg.Width - 1 do
      begin
        if Scale.PlotBox.Contains(TCurvePoint.Create(i, j)) then
        begin
          if GridMask.IsValid and GridMask.IsActive and (p1^.alpha > 0) then
            p := p1
          else
            p := p2;

          if AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) then
            DigitCurve.AllPoints.AddPoint(TCurvePoint.Create(i, j));
            //DigitCurve.AllPoints.AddPoint(Scale.FromImgToPlot(i, j));
        end;

        inc(p1);
        inc(p2);

        // Notify the parent that it must update the progress bar
        if Assigned(OnShowProgress) then
          OnShowProgress(Self, Round(100*(j*PlotImg.Width + i)/
                                     (PlotImg.Width*PlotImg.Height)),
                                     'Finding points...');

        Application.ProcessMessages;
        if CancelAction then Break;
      end;
      if CancelAction then Break;
    end;
    //DigitCurve.AllPoints.SortCurve;

    // Notify the parent that it must hide the progress bar
    if Assigned(OnHideProgress) then
      OnHideProgress(Self);

    DigitCurve.ValidPoints := not CancelAction;

    RunningAction := False;
  end;
end;

function TPlotImage.FindNextPoint(var Pv: TCurvePoint; Interval: Integer;
                                  ScanX: Boolean = False): Boolean;
var
  // Number of pixels that the line is expected to spread
  Spread: Integer;

  P: TCurvePoint;
  dX, dY: Integer;
  NoPnts: Integer;
  FoundUp, FoundDown: Boolean;

function CheckPlotPoint(const Pi: TCurvePoint; var P: TCurvePoint; var nP: Integer): Boolean;
begin
  Result := DigitCurve.AllPoints.Contains(Pi);
  if Result then
  begin
    inc(nP);
    P := P + Pi;
  end;
end;

begin
  Spread := DigitCurve.Spread;

  P := GetcurvePoint(0, 0);
  NoPnts := 0;
  dY := 0;
  Result := False;
  // We scan, starting from Yv, until we find enough points
  repeat
    // We check Yv twice, to give it more weight
    FoundUp := CheckPlotPoint(Pv - dY*Scale.Ny(Pv), P, NoPnts);
    FoundDown := CheckPlotPoint(Pv + dY*Scale.Ny(Pv), P, NoPnts);

    dX := 1;
    while ScanX and (dX < Spread) do
    begin
      FoundUp := FoundUp or CheckPlotPoint(Pv - dX*Scale.Nx(Pv) - dY*Scale.Ny(Pv), P, NoPnts);
      FoundUp := FoundUp or CheckPlotPoint(Pv + dX*Scale.Nx(Pv) - dY*Scale.Ny(Pv), P, NoPnts);
      FoundDown := FoundDown or CheckPlotPoint(Pv - dX*Scale.Nx(Pv) + dY*Scale.Ny(Pv), P, NoPnts);
      FoundDown := FoundDown or CheckPlotPoint(Pv + dX*Scale.Nx(Pv) + dY*Scale.Ny(Pv), P, NoPnts);

      inc(dX);
    end;

    inc(dY);
  until (NoPnts >= 1 + 2*Spread) or (dY >= Interval) or
        ((NoPnts > 0) and (not FoundUp) and (not FoundDown));

  if (NoPnts > 0) then
  begin
    Result := True;
    Pv := P/NoPnts;
  end;
end;

procedure TPlotImage.DigitizeSpectrum(Pi: TCurvePoint;
                                      FillCurvePoints: Boolean = True);
var
  i : Integer;
  Delta, L: Double;
  Pp: TCurvePoint;
  PNew: TCurvePoint;

  // Steps (in the image) between one point and the next
  PixelStep: Integer;
  // Interval of pixels to scan below and above the expected point
  Interval: Integer;

  ML: TCurve;

function SegmentLength(P, dP: TCurvePoint): Double;
var
  mx, my: Double;
begin
  if (Abs(dP.X) < 1e-4) then
    mx := 0
  else if (dP.X > 0) then
    mx := (Width - P.X)/dP.X
  else
    mx := -P.X/dP.X;

  if (Abs(dP.Y) < 1e-4) then
    my := 0
  else if (dP.Y > 0) then
    my := (Width - P.Y)/dP.Y
  else
    my := -P.Y/dP.Y;

  Result := Min(mx, my);
end;

begin
  RunningAction := True;
  CancelAction := False;

  if FillCurvePoints then
    FindCurvePoints;

  PixelStep := DigitCurve.Step;
  Interval := DigitCurve.Interval;

  // Notify the parent that it must show the progress bar
  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0, 'Digitizing spectrum...');

  DigitCurve.NextCurve(False);
  Curve.AddPoint(Pi);

  Delta := 0;
  if PixelStep > 0 then
    L := PlotImg.Width - Pi.X
  else
    L := Pi.X;

  ML := MarkerList;
  if (ML.Count > 2) then
    if (Scale.CoordSystem = csCartesian) then
      ML.Interpolate(1 + Abs(Round((Scale.FromPlotToImg(ML.Point[ML.Count - 1]).X -
                                    Scale.FromPlotToImg(ML.Point[0]).X)/PixelStep)), 3)
    else
      ML.Interpolate(1 + Abs(360 div PixelStep), 3);

  i := 0;

  // Move to the next and look for [Interval] points up and down
  repeat
    case ML.Count of
      0..2: begin
        // Not enough markers to guide the search
        // Find the closer point
        if (Scale.CoordSystem = csPolar) then
        begin
          Pp := CartesianToPolar(Pi - Scale.ImagePoint[2]);
          if (2*Pp.Y*Pp.Y < PixelStep*PixelStep) then
            Pp.Y := PixelStep;
          Pp.X := Pp.X - ArcSin(PixelStep/Sqrt(4*Pp.Y*Pp.Y - PixelStep*PixelStep))*90/ArcTan(1);
          Pi := Scale.ImagePoint[2] + PolarToCartesian(Pp);

          Delta := Delta + Sign(PixelStep)*ArcSin(PixelStep/Sqrt(4*Pp.Y*Pp.Y - PixelStep*PixelStep))*90/ArcTan(1);
        end
        else
        begin
          Pi := Pi + PixelStep*Scale.Nx(Pi);
          Delta := SegmentLength(Pi, Scale.Nx(Pi));
        end;

        PNew := Pi;
      end
      else
      begin
        // There are enough markers to guide the search
        inc(i);

        if (PixelStep > 0) then
          PNew := Scale.FromPlotToImg(ML.Point[i])
        else
          PNew := Scale.FromPlotToImg(ML.Point[ML.Count - 1 - i]);
      end;
    end;


    if FindNextPoint(PNew, Interval) then
    begin
      Pi := PNew;
      Curve.AddPoint(Pi);
    end;

    // Notify the parent that it must update the progress bar
    if Assigned(OnShowProgress) then
    begin
      case ML.Count of
        0..2: begin
          if (Scale.CoordSystem = csCartesian) then
          begin
            if PixelStep > 0 then
              OnShowProgress(Self, Round(100*Pi.X/L), 'Digitizing spectrum...')
            else
              OnShowProgress(Self, Round(100*(1 - Pi.X/L)), 'Digitizing spectrum...');
          end
          else
            OnShowProgress(Self, Round(100*Abs(Delta)/360), 'Digitizing spectrum...');
        end
        else
          OnShowProgress(Self, Round(i/(ML.Count - 1)), 'Digitizing spectrum...');
        end;
    end;

    Application.ProcessMessages;
    if CancelAction then Break;
  until (not Scale.PlotBox.Contains(Pi)) or
        ((Scale.CoordSystem = csPolar) and (Abs(Delta) > 360)) or
        ((ML.Count > 2) and (i >= ML.Count - 1));

  // Notify the parent that it must hide the progress bar
  if Assigned(OnHideProgress) then
    OnHideProgress(Self);

  SortCurve;

  IsChanged := True;

  RunningAction := False;
end;

procedure TPlotImage.DigitizeSpectrum;
var
  Pi : TCurvePoint;
begin
  FindCurvePoints;

  // Estimate the first point
  if (Markers.Count > 0) then
  begin
    if (DigitCurve.Step > 0) then
      Pi := LeftMarker.Position/Zoom
    else
      Pi := RightMarker.Position/Zoom;
  end
  else
  begin
    if (Scale.CoordSystem = csCartesian) then
    begin
      if (DigitCurve.Step > 0) then
        Pi := (Scale.ImagePoint[1] + Scale.ImagePoint[2])/2
      else
        Pi := Scale.ImagePoint[3] + (Scale.ImagePoint[1] - Scale.ImagePoint[2])/2;
    end
    else
      Pi := (Scale.ImagePoint[3] + Scale.ImagePoint[2])/2;

    while (not FindNextPoint(Pi, Round(Modulus(Pi)), True)) and
          (Scale.PlotBox.Contains(Pi)) do
      Pi := Pi + Sign(DigitCurve.Step)*Scale.Nx(Pi);
  end;


  DigitizeSpectrum(Pi, False);
end;

procedure TPlotImage.DigitizeColor;
var
  i, j: Integer;
  n: Array of Integer;
  Pi: TCurvePoint;
  Added: Boolean;
begin
  try
    //Identify all the points that have similar color
    FindCurvePoints;
    SetLength(n, DigitCurve.AllPoints.Count);

    // Notify the parent that it must show the progress bar
    if Assigned(OnShowProgress) then
      OnShowProgress(Self, 0, 'Digitizing spectrum...');

    DigitCurve.NextCurve(False);

    with Curve do
    begin
      AddPoint(DigitCurve.AllPoints.Point[0]);
      n[0] := 1;
      for i := 1 to DigitCurve.AllPoints.Count - 1 do
      begin
        // Notify the parent that it must update the progress bar
        if Assigned(OnShowProgress) then
          OnShowProgress(Self, Round(100*(i + 1)/DigitCurve.AllPoints.Count), 'Digitizing spectrum...');

        Added := False;
        Pi := DigitCurve.AllPoints.Point[i];
        for j := 0 to Count - 1 do
          if (2*Pi.DistanceTo(Point[j]) <= DigitCurve.Spread) then
          begin
            Point[j] := Point[j] + TCurvePoint.Create(Point[j].X, Pi.Y);
            inc(n[j]);
            Added := True;
          end;

        if not Added then
        begin
          AddPoint(Pi);
          n[Count - 1] := 1;
        end;
      end;

      for j := 0 to Count - 1 do
        if (n[j] > 1) then
          Point[j] := Point[j]/n[j];
    end;

    // Notify the parent that it must hide the progress bar
    if Assigned(OnHideProgress) then
      OnHideProgress(Self);

    SortCurve;
    //Curve.Interpolate(Curve.Count, itpBSpline);

    IsChanged := True;
  finally
    SetLength(n, 0);
  end;
end;

procedure TPlotImage.DigitizeMarkers;
var
  i: Integer;
begin
  if (State = piSetCurve) and (Markers.Count > 0) then
  begin
    DigitCurve.NextCurve(False);
    Curve.ShowAsSymbols := True;

    for i := 0 to Markers.Count - 1 do
      Curve.AddPoint(Markers[i].Position/Zoom);

    Curve.SortCurve;

    IsChanged := True;
  end;
end;

procedure TPlotImage.FillIsland(Pi: TCurvePoint; var Island: TIsland;
                                JustInY: Boolean = True;
                                MoveUp: Boolean = True;
                                MoveDown: Boolean = True;
                                MaxPoints: Integer = 1000;
                                FillCurvePoints: Boolean = True);
begin
  // Make sure that the program doesn't freezes due to bad configuration
  // We want an island, not a continent ;-)
  if (Island.Count < MaxPoints) then
  begin
    if FillCurvePoints then
      FindCurvePoints;

    if (not Island.Contains(Pi)) and DigitCurve.AllPoints.Contains(Pi) then
    begin
      Island.AddPoint(Pi);
      if MoveDown then
        FillIsland(Pi - Scale.Ny(Pi), Island, JustInY,
                   MoveUp, MoveDown, MaxPoints, False);
      if MoveUp then
        FillIsland(Pi + Scale.Ny(Pi), Island, JustInY,
                   MoveUp, MoveDown, MaxPoints, False);
      if (not JustInY) then
      begin
        FillIsland(Pi - Scale.Nx(Pi), Island, JustInY,
                   MoveUp, MoveDown, MaxPoints, False);
        FillIsland(Pi + Scale.Nx(Pi), Island, JustInY,
                   MoveUp, MoveDown, MaxPoints, False);
      end;
    end;
  end;
end;

procedure TPlotImage.FillIsland(Xi, Yi: Double; var Island: TIsland;
                                JustInY: Boolean = True;
                                MoveUp: Boolean = True;
                                MoveDown: Boolean = True;
                                MaxPoints: Integer = 1000);
begin
  FillIsland(TCurvePoint.Create(Xi, Yi), Island, JustInY, MoveUp, MoveDown, MaxPoints);
end;

procedure TPlotImage.AdjustCurve(Noisy: Boolean = False);
const
  min_diff = 0.01;
var
  i: Integer;
  Pi: TCurvePoint;
  NewPoints: TPointList;
  Island: TIsland;
  Up, Down: Boolean;
begin
  // Only if there is a curve
  if HasPoints then
  begin
    RunningAction := True;
    CancelAction := False;

    FindCurvePoints;
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      // Notify the parent that it must show the progress bar
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, 0, 'Adjusting curve...');

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          if Noisy then
          begin
            case i of
              0: begin
                // The point is '100*min_diff'% larger than its neighbour
                Up := (min_diff <= (Pi.Y - Curve.Point[1].Y)/abs(Pi.Y));
                // The point is '100*min_diff'% smaller than its neighbour
                Down := (-min_diff >= (Pi.Y - Curve.Point[1].Y)/abs(Pi.Y));
              end;
              else
              begin
                if (i = (Curve.Count - 1)) then
                begin
                  // The point is '100*min_diff'% larger than its neighbour
                  Up := (min_diff <= (Pi.Y - Curve.Point[i - 1].Y)/abs(Pi.Y));
                  // The point is '100*min_diff'% smaller than its neighbour
                  Down := (-min_diff >= (Pi.Y - Curve.Point[i - 1].Y)/abs(Pi.Y));
                end
                else
                begin
                  // The point is '100*min_diff'% larger than its neighbours
                  Up := (min_diff <= (Pi.Y - Curve.Point[i - 1].Y)/abs(Pi.Y)) and
                        (min_diff <= (Pi.Y - Curve.Point[i + 1].Y)/abs(Pi.Y));
                  // The point is '100*min_diff'% smaller than its neighbours
                  Down := (-min_diff >= (Pi.Y - Curve.Point[i - 1].Y)/abs(Pi.Y)) and
                          (-min_diff >= (Pi.Y - Curve.Point[i + 1].Y)/abs(Pi.Y));
                end;
              end;
            end;
            // The point is neither a maximum nor a minimum
            if (not Up) and (not Down) then
            begin
              Up := True;
              Down := True;
            end;
          end
          else
          begin
            Up := True;
            Down := True;
          end;
          Island.Clear;
          FillIsland(Pi, Island, True, Up, Down, 1000, False);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue)
          else
            NewPoints.Add(Pi);
        end;

        // Notify the parent that it must update the progress bar
        if Assigned(OnShowProgress) then
          OnShowProgress(Self, Round(100*(i + 1)/Curve.Count), 'Adjusting curve...');

        Application.ProcessMessages;
        if CancelAction then Break;
      end;

      DigitCurve.NextCurve(False);
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      // Notify the parent that it must hide the progress bar
      if Assigned(OnHideProgress) then
        OnHideProgress(Self);
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;

    RunningAction := False;
  end;
end;

procedure TPlotImage.ConvertCurveToSymbols;
var
  i: Integer;
  Pi: TCurvePoint;
  NewPoints: TPointList;
  Island: TIsland;
begin
  // Only if there is a curve
  if HasPoints then
  begin
    RunningAction := True;
    CancelAction := False;

    FindCurvePoints;
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      // Notify the parent that it must show the progress bar
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, 0, 'Converting curve to symbols...');

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          Island.Clear;
          FillIsland(Pi, Island, False, True, True, 1000, False);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue);
        end;

        // Notify the parent that it must update the progress bar
        if Assigned(OnShowProgress) then
          OnShowProgress(Self, Round(100*(i + 1)/Curve.Count),
                         'Converting curve to symbols...');

        Application.ProcessMessages;
        if CancelAction then Break;
      end;

      DigitCurve.NextCurve(False);
      Curve.ShowAsSymbols := True;
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      // Notify the parent that it must hide the progress bar
      if Assigned(OnHideProgress) then
        OnHideProgress(Self);
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;

    RunningAction := False;
  end;
end;

function TPlotImage.GetPixel(X, Y: Integer): LongInt;
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

function TPlotImage.GetPixel(X, Y: Double): LongInt;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

function TPlotImage.GetPixel(P: TCurvePoint): LongInt;
begin
  Result := GetPixel(P.X, P.Y);
end;

function TPlotImage.GetAxesPoint(Index: Integer): TCurvePoint;
begin
  Result := Scale.ImagePoint[Index];
end;

function TPlotImage.GetBoxVertex(Index: Integer): TCurvePoint;
begin
  Result := Scale.PlotBox[Index - 1];
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
  Xmin, Xmax,
  Ymin, Ymax: Integer;
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[3]) then
  begin
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[3].Position, Pini, Pend);

    Xmin := Round(min(Pini.X, Pend.X));
    Xmax := Round(max(Pini.X, Pend.X));
    Ymin := Round(min(Pini.Y, Pend.Y));
    Ymax := Round(max(Pini.Y, Pend.Y));
    Result := TRect.Create(Xmin, Ymin, Xmax, Ymax);
    Result.Union(TRect.Create(Pend - TPoint.Create(6, 6), 12, 12));
  end
  else
    Result := TRect.Create(0, 0, 0, 0);
end;

function TPlotImage.YAxisRect: TRect;
var
  Pini, Pend: TCurvePoint;
  Xmin, Xmax,
  Ymin, Ymax: Integer;
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[1]) then
  begin
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[1].Position, Pini, Pend);

    Xmin := Round(min(Pini.X, Pend.X));
    Xmax := Round(max(Pini.X, Pend.X));
    Ymin := Round(min(Pini.Y, Pend.Y));
    Ymax := Round(max(Pini.Y, Pend.Y));
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
    if assigned(OnZoomChanged) then
      OnZoomChanged(Self, Value);

    Invalidate;
  end;
end;

procedure TPlotImage.SetAxesPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index in [1..3]) and (Scale.ImagePoint[Index] <> Value) then
  begin
    if Assigned(FAxesMarkers[Index]) then
      FAxesMarkers[Index].Position := Zoom*Value;

    Scale.ImagePoint[Index] := Value;

    IsChanged := True;
  end;
end;

procedure TPlotImage.SetBoxVertex(Index: Integer; const Value: TCurvePoint);
var
  Idx: Integer;
begin
  if (Index in [1..4]) and
     (Scale.PlotBox[Index - 1] <> Value) then
  begin
    if Assigned(FBoxMarkers[Index]) then
      FBoxMarkers[Index].Position := Zoom*Value;

    Scale.PlotBox[Index - 1] := Value;

    if Assigned(FEdgeMarkers[Index]) then
      FEdgeMarkers[Index].Position := Zoom*Scale.PlotBox.Edge[Index - 1];

    Idx := Scale.PlotBox.PrevVertIdx(Index - 1) + 1;
    if Assigned(FEdgeMarkers[Idx]) then
      FEdgeMarkers[Idx].Position := Zoom*Scale.PlotBox.Edge[Idx - 1];

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
      FMarkerList.AddPoint(Scale.FromImgToPlot(Markers[i].Position/Zoom));

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

procedure TPlotImage.ResetPlotBox;
const
  span = 2;
begin
  Scale.PlotBox[0] := TCurvePoint.Create(span, span);
  Scale.PlotBox[1] := TCurvePoint.Create(PlotImg.Width - span - 1, span);
  Scale.PlotBox[2] := TCurvePoint.Create(PlotImg.Width - span - 1,
                                         PlotImg.Height - span - 1);
  Scale.PlotBox[3] := TCurvePoint.Create(span, PlotImg.Height - span - 1);

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

procedure TPlotImage.AddMarker(Position: TPoint; NewMarker: Boolean = True);
var
  BMP: TBGRABitmap;
begin
  BMP := CreateMarker(TPoint.Create(13, 13), 'x', DigitCurve.Color, 3);
  AddMarker(TMarker.Create(BMP, Position, False), NewMarker);
end;

procedure TPlotImage.AddMarker(Marker: TMarker; NewMarker: Boolean = True);
begin
  Markers.Insert(0, Marker);
  ActiveMarker := Marker;
  UpdateMarker(Marker);

  if NewMarker then
    IsChanged := True;
end;

procedure TPlotImage.UpdateMarker(Marker: TMarker);
begin
  UpdateRegion(Marker.Rect);
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

    for i := 1 to Scale.PlotBox.NumVertices do
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
  OldPos,
  P1, P2: TCurvePoint;
begin
  if Assigned(ActiveMarker) then
  begin
    NewPos := ActiveMarker.Position + Delta;

    // Check that the marker remains inside the image
    if ClientRect.Contains(NewPos) then
    begin
      for i := 1 to 3 do
        if (ActiveMarker = AxesMarkers[i]) then
          Scale.ImagePoint[i] := NewPos/Zoom;

      with Scale.PlotBox do
      begin
        for i := 1 to NumVertices do
        begin
          if (ActiveMarker = BoxMarkers[i]) then
          begin
            UpdateRegion(Rect[Zoom]);
            Vertex[i - 1] := NewPos/Zoom;

            EdgeMarkers[i].Move(Zoom*Edge[i - 1]);
            EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(Zoom*Edge[PrevVertIdx(i - 1)]);
            UpdateRegion(Rect[Zoom]);
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
              UpdateRegion(Rect[Zoom]);

              BoxMarkers[NextVertIdx(i - 1) + 1].Move(P1);
              BoxMarkers[i].Move(P2);

              EdgeMarkers[NextVertIdx(i - 1) + 1].Move(Zoom*Edge[NextVertIdx(i - 1)]);
              EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(Zoom*Edge[PrevVertIdx(i - 1)]);

              UpdateRegion(Rect[Zoom]);

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

      if (NewPos <> ActiveMArker.Position) then
      begin
        UpdateMarker(ActiveMarker);
        ActiveMarker.Move(NewPos);
        UpdateMarker(ActiveMarker);

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

    CopyMode:= cmSrcCopy;
    CopyRect(PaintRect, ZoomImg.Canvas, PaintRect);
  end;

  case State of
    piSetCurve: begin
      if HasPoints then
        DigitCurve.Draw(WhiteBoard.Canvas, Zoom);
    end;
    piSetScale: begin
      if Options.ShowXAxis or Options.ShowYAxis then
      begin
        WhiteBoard.ArrowEndAsTriangle();
        WhiteBoard.ArrowEndSize := PointF(8, 2.5);
        WhiteBoard.ArrowStartOffset:= 0;
        WhiteBoard.ArrowEndOffset:= -7;

        WhiteBoard.PenStyle := psDash;

        if Options.ShowXAxis then
        begin
          XAxisCoords(Pini, Pend);
          WhiteBoard.DrawLineAntialias(Pini.X, Pini.Y, Pend.X, Pend.Y, Options.XAxisColor, 2);
        end;

        if Options.ShowYAxis then
        begin
          YAxisCoords(Pini, Pend);
          WhiteBoard.DrawLineAntialias(Pini.X, Pini.Y, Pend.X, Pend.Y, Options.YAxisColor, 2);
        end;

        WhiteBoard.PenStyle := psSolid;
      end;
    end;
    piSetPlotBox: begin
      with Scale.PlotBox do
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
          //PolarCoordinates := (Scale.CoordSystem = csPolar);
          WhiteBoard.DrawPolygonAntialias(DrawPoints[Zoom], BGRABlack, 1, PolyColor);
        end;
      end;
    end;
  end;

  for i := Markers.Count - 1 downto 0 do
    Markers[i].Draw(WhiteBoard, PaintRect);

  if Assigned(ActiveMarker) and
     not (ActiveMarker.Rect*PaintRect).IsEmpty then
  begin
    with WhiteBoard.Canvas do
      DrawFocusRect(ActiveMarker.Rect);
  end;

  if Assigned(MarkerUnderCursor) and
     (MarkerUnderCursor <> ActiveMarker) and
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
  function IsInArray(M: TMarker; Arr: Array of TMarker): Boolean;
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
        else if (ssShift in Shift) or (not Scale.PlotBox.IsConvex) then
          FDragAction := daVertex
        else
          FDragAction := daAngle;
      end
      else if IsInArray(FClickedMarker, FEdgeMarkers) then
        FDragAction := daEdge;
    end;

    // No marker under cursor, we are selecting a region
    if (HitMarker = nil) and (State = piSetCurve) then
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
  Newpos,
  OldPos,
  P1, P2: TCurvePoint;
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
      UpdateMarker(FClickedMarker);

      if (State = piSetScale) then
      begin
        UpdateRegion(XAxisRect);
        UpdateRegion(YAxisRect);
      end;

      if ClientRect.Contains(MouseMovePos) then
        NewPos := FClickedCoord + MouseMovePos - FClickedPoint
      else
        NewPos := FClickedCoord;  //The mouse moves out of the image

      FClickedMarker.Move(NewPos);

      if (State = piSetScale) then
      begin
        UpdateRegion(XAxisRect);
        UpdateRegion(YAxisRect);
      end;

      if (State = piSetPlotBox) then
        with Scale.PlotBox do
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
              if not ClientRect.Contains(Zoom*Vertex[0]) or
                 not ClientRect.Contains(Zoom*Vertex[1]) or
                 not ClientRect.Contains(Zoom*Vertex[2]) or
                 not ClientRect.Contains(Zoom*Vertex[3]) then
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

                  EdgeMarkers[NextVertIdx(i - 1) + 1].Move(Zoom*Edge[NextVertIdx(i - 1)]);
                  EdgeMarkers[PrevVertIdx(i - 1) + 1].Move(Zoom*Edge[PrevVertIdx(i - 1)]);
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

          UpdateRegion(TmpRect);
        end;

      UpdateMarker(FClickedMarker);
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
      UpdateRegion(TmpRect);

      InMouseMove := False;

      Exit;
    end;
  end;

  HitMarker := nil;
  for i := 0 to Markers.Count - 1 do
  begin
    Marker := Markers[i];
    if Marker.HitTest(MouseMovePos) then
      HitMarker := Marker;
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
            Scale.ImagePoint[i] := FClickedMarker.Position/Zoom;

      if Assigned(OnMarkerDragged) and Assigned(FClickedMarker) then
      begin
        // Notify that neighboring markers have moved
        if (State = piSetPlotBox) then
          with Scale.PlotBox do
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

      if Scale.PlotBox.Rotated then
        Scale.PlotBox.ApplyRotation;

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
        UpdateRegion;
      end;

      if Assigned(FClickedMarker) then
      begin
        Markers.Move(Markers.IndexOf(FClickedMarker), 0);
        UpdateMarker(FClickedMarker);
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
      UpdateMarker(OldMarker);

    if Assigned(MarkerUnderCursor) then
      UpdateMarker(MarkerUnderCursor);
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
      UpdateMarker(OldMarker);

    if Assigned(ActiveMarker) then
      UpdateMarker(ActiveMarker);
  end;
end;


function TPlotImage.ConvertCoords(p: TCurvePoint): TCurvePoint;
begin
  Result := FScale.FromImgToPlot(p/Zoom);
end;

function TPlotImage.ConvertCoords(X, Y: Double): TCurvePoint;
begin
  Result := ConvertCoords(TCurvePoint.Create(X, Y));
end;

function TPlotImage.GetCount: Integer;
begin
  Result := FCurves.Count;
end;

function TPlotImage.GetNumPoints: Integer;
begin
  Result := DigitCurve.Curve.Count;
end;

function TPlotImage.GetCurve(Index: Integer): TDigitCurve;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FCurves[Index]
  else
    Result := nil;
end;

function TPlotImage.GetActiveCurve: TCurve;
begin
  Result := DigitCurve.Curve;
end;

// Warning: any time that we call this function, we must free the curve
// created here, or we will have a memoory leak
function TPlotImage.GetPlotCurves(Index: Integer): TCurve;
var
  i: Integer;
begin
  Result := TCurve.Create;
  with FCurves[Index].Curve do
  begin
    for i := 0 to Count - 1 do
      Result.AddPoint(FScale.FromImgToPlot(Point[i]));
  end;
end;

function TPlotImage.GetPlotCurve: TCurve;
begin
  Result := GetPlotCurves(CurveIndex);
end;

function TPlotImage.GetPoint(Index: Integer): TCurvePoint;
begin
  Result := FScale.FromImgToPlot(FCurves[CurveIndex].Curve.Point[Index]);
end;

function TPlotImage.GetDigitCurve: TDigitCurve;
begin
  Result := FCurves[CurveIndex];
end;

function TPlotImage.GetColorIsSet: Boolean;
begin
  Result := DigitCurve.ColorIsSet;
end;

function TPlotImage.GetHasPoints: Boolean;
begin
  Result := DigitCurve.HasPoints;
end;

function TPlotImage.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

function TPlotImage.GetCanUndo: Boolean;
begin
  Result := DigitCurve.CanGoBack;
end;

function TPlotImage.GetCanRedo: Boolean;
begin
  Result := DigitCurve.CanGoForward;
end;

procedure TPlotImage.SetState(Value: TPlotImageState);
begin
  if (FState <> Value) then
  begin
    // Update the markers in the curve
    UpdateMarkersInCurve;

    // Change the state
    FState := Value;

    // Update the markers in the image
    UpdateMarkersInImage;

    // Notify the parent that the state has changed
    if assigned(OnStateChanged) then
      OnStateChanged(Self, Value);
  end;
end;

procedure TPlotImage.SetCurveIndex(Value: Integer);
var
  TmpOnChange: TNotifyEvent;
begin
  if (Value >= 0) and (Value < FCurves.Count) and (Value <> CurveIndex) then
  begin
    TmpOnChange := OnChange;
    OnChange := nil;

    // First, update all the markers in the curve
    UpdateMarkersInCurve;
    // Now change the active curve
    FCurveIndex := Value;
    // Finally, update all the new markers in the image
    UpdateMarkersInImage;

    OnChange := TmpOnChange;
  end;
end;

procedure TPlotImage.SetIsChanged(Value: Boolean);
begin
  FIsChanged := Value;

  if (Assigned(FOnChange) and FIsChanged) then
    FOnChange(Self);
end;

procedure TPlotImage.SetPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (FScale.FromPlotToImg(Value) <> DigitCurve.Curve.Point[Index]) then
  begin
    EraseCurve(DigitCurve);
    DigitCurve.Curve.Point[Index] := FScale.FromPlotToImg(Value);
    DigitCurve.Draw(Canvas);

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
  if (State = piSetCurve) then
  begin
    DigitCurve.ClearMarkers;
    for i := Markers.Count - 1 downto 0 do
      DigitCurve.AddMarker(Markers[i].Position/Zoom);

    DigitCurve.SortMarkers;
  end;
end;

procedure TPlotImage.UpdateMarkersInImage;
var
  i: Integer;
begin
  ClearMarkers;

  case State of
    piSetScale: begin

      FAxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', Options.YAxisColor, 3),
                                        Zoom*Scale.ImagePoint[1], True);
      FAxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clGreen, 3),
                                        Zoom*Scale.ImagePoint[2], True);
      FAxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', Options.XAxisColor, 3),
                                        Zoom*Scale.ImagePoint[3], True);

      AddMarker(FAxesMarkers[1], False);
      AddMarker(FAxesMarkers[2], False);
      AddMarker(FAxesMarkers[3], False);
    end;
    piSetPlotBox: begin
      with Scale.PlotBox do
        for i := 1 to 4 do
        begin
          FBoxMarkers[i] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),
                                           '1', clBlack, 3),
                                           Zoom*Vertex[i - 1], True);
          AddMarker(FBoxMarkers[i], False);

          FEdgeMarkers[i] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),
                                            '0', clBlack, 3),
                                            Zoom*Edge[i - 1], True);
          AddMarker(FEdgeMarkers[i], False);
        end;
    end;
    piSetCurve: begin
      for i := 0 to DigitCurve.MarkerCount - 1 do
        AddMarker(Zoom*DigitCurve.Markers[i], False);
    end;
    piSetGrid: begin
      // For now, do nothing
    end;
  end;
end;

procedure TPlotImage.UpdateRegion(UpdateArea: TRect);
begin
  Canvas.ClipRect := UpdateArea;
  {$ifdef windows}
  InvalidateRect(Handle, UpdateArea, False);
  {$else}
  Invalidate;
  {$endif}
end;

procedure TPlotImage.UpdateRegion;
begin
  UpdateRegion(SelectionRect);
end;

procedure TPlotImage.EraseCurve(Curve: TDigitCurve);
begin
  Curve.Draw(Canvas);
end;

procedure TPlotImage.RectIntersection(Po, Pf: TCurvePoint;
                                      var Pini, Pend: TCurvePoint);
var
  P: Array [1..4] of TCurvePoint;
  Idx: Array [1..2] of Integer;
  i, j: Integer;
  m, b: Double;
begin
  // Mostly vertical
  if abs(Pf.Y - Po.Y) > abs(Pf.X - Po.X) then
  begin
    m := (Pf.X - Po.X)/(Pf.Y - Po.Y);
    b := Pf.X - m*Pf.Y;

    // Intersection with the upper edge
    P[1].Y := 0;
    P[1].X := b;

    // Intersection with the bottom edge
    P[3].Y := ClientHeight - 1;
    P[3].X := m*P[3].Y + b;

    // Not completely vertical
    if (abs(m) > 1e-5) then
    begin
      // Intersection with the right edge
      P[2].X := ClientWidth - 1;
      P[2].Y := (P[2].X - b)/m;

      // Intersection with the left edge
      P[4].X := 0;
      P[4].Y := -b/m;
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
    m := (Pf.Y - Po.Y)/(Pf.X - Po.X);
    b := Pf.Y - m*Pf.X;

    // Not completely horizontal
    if (abs(m) > 1e-5) then
    begin
      // Intersection with the upper edge
      P[1].Y := 0;
      P[1].X := -b/m;

      // Intersection with the bottom edge
      P[3].Y := ClientHeight - 1;
      P[3].X := (P[3].Y - b)/m;
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
    P[2].Y := m*P[2].X + b;

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
        inc(j);
      end
      else
        if (P[Idx[1]].DistanceTo(P[i]) > P[Idx[1]].DistanceTo(P[Idx[2]])) then
          Idx[2] := i;
  end;

  // Assign the points
  if (sign(Pf.X - Po.X) = sign(P[Idx[2]].X - P[Idx[1]].X)) and
     (sign(Pf.Y - Po.Y) = sign(P[Idx[2]].Y - P[Idx[1]].Y)) then
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
    with Scale do
      RectIntersection(Zoom*ImagePoint[2], Zoom*ImagePoint[3], Pini, Pend);
end;

procedure TPlotImage.YAxisCoords(var Pini, Pend: TCurvePoint);
begin
  if Assigned(FAxesMarkers[2]) and Assigned(FAxesMarkers[1]) then
    RectIntersection(AxesMarkers[2].Position, AxesMarkers[1].Position, Pini, Pend)
  else
    with Scale do
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

procedure TPlotImage.AddCurve;
begin
  FCurves.Add(TDigitCurve.Create('Curve' + IntToStr(FCurves.Count + 1)));

  IsChanged := True;
end;

procedure TPlotImage.AddCurve(Position: Integer);
begin
  if (Position >= 0) and (Position < Fcurves.Count) then
    Fcurves.Insert(Position, TDigitCurve.Create('Curve' + IntToStr(Position) + 'b'))
  else
    FCurves.Add(TDigitCurve.Create('Curve' + IntToStr(FCurves.Count + 1)));

  IsChanged := True;
end;

procedure TPlotImage.DeleteCurve;
begin
  DeleteCurve(CurveIndex);
end;

procedure TPlotImage.DeleteCurve(Index: Integer);
begin
  if (Index >= 0) and (Index < FCurves.Count) then
  begin
    FCurves.Delete(Index);
    if (CurveIndex >= FCurves.Count) then
      FCurveIndex := FCurves.Count - 1;

    IsChanged := True;
  end;
end;

procedure TPlotImage.ClearCurve;
begin
  ClearCurve(CurveIndex);
end;

procedure TPlotImage.ClearCurve(Index: Integer);
begin
  if (Index >= 0) and (Index < FCurves.Count) then
  begin
    EraseCurve(FCurves[Index]);
    FCurves[Index].Clear;

    IsChanged := True;
  end;
end;

procedure TPlotImage.UndoCurveChanges;
begin
  if DigitCurve.CanGoBack then
  begin
    EraseCurve(DigitCurve);
    DigitCurve.GoBack;
    DigitCurve.Draw(Canvas);

    IsChanged := True;
  end;
end;

procedure TPlotImage.RedoCurveChanges;
begin
  if DigitCurve.CanGoForward then
  begin
    EraseCurve(DigitCurve);
    DigitCurve.GoForward;
    DigitCurve.Draw(Canvas);

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
    TmpCurve := PlotCurves[Index];
    TmpCurve.SortCurve;

    FCurves[Index].Curve.Clear;
    for i := 0 to TmpCurve.Count - 1 do
      FCurves[Index].Curve.AddPoint(Scale.FromPlotToImg(TmpCurve.Point[i]));

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Smooth(k, d: Integer; Index: Integer);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Smooth(k, d);

    if (Index = CurveIndex) then
      EraseCurve(DigitCurve);

    FCurves[Index].NextCurve(False);
    for i := 0 to TmpCurve.Count - 1 do
      FCurves[Index].Curve.AddPoint(FScale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) then
      DigitCurve.Draw(Canvas);

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
    for i := 0 to Count - 1 do
      Smooth(k, d, i);
end;

procedure TPlotImage.Interpolate(n, d: Integer; Index: Integer; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(n, d, IntType);

    if (Index = CurveIndex) then
      EraseCurve(DigitCurve);

    FCurves[Index].NextCurve(False);
    FCurves[Index].ShowAsSymbols := False;
    for i := 0 to TmpCurve.Count - 1 do
      FCurves[Index].Curve.AddPoint(FScale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) then
      DigitCurve.Draw(Canvas);

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Interpolate(n, d: Integer; AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(n, d, CurveIndex, IntType)
  else
    for i := 0 to Count - 1 do
      Interpolate(n, d, i, IntType);
end;

procedure TPlotImage.Interpolate(Xo, Xf: Double; n, d: Integer; Index: Integer; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(Xo, Xf, n, d, IntType);

    if (Index = CurveIndex) then
      EraseCurve(DigitCurve);

    FCurves[Index].NextCurve(False);
    FCurves[Index].ShowAsSymbols := False;
    for i := 0 to TmpCurve.Count - 1 do
      FCurves[Index].Curve.AddPoint(FScale.FromPlotToImg(TmpCurve.Point[i]));

    if (Index = CurveIndex) then
      DigitCurve.Draw(Canvas);

    IsChanged := True;
  finally
    TmpCurve.Free;
  end;
end;

procedure TPlotImage.Interpolate(Xo, Xf: Double; n, d: Integer; AllCurves: Boolean = False; IntType: TInterpolation = itpBSpline);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(Xo, Xf, n, d, CurveIndex,IntType)
  else
    for i := 0 to Count - 1 do
      Interpolate(Xo, Xf, n, d, i, IntType);
end;

procedure TPlotImage.CorrectCurve(Po, Pf: TPoint; IsStep: Boolean = True);
begin
  EraseCurve(DigitCurve);
  DigitCurve.CorrectCurve(Po, Pf, IsStep);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.CorrectCurve(Po, Pf: TCurvePoint; IsStep: Boolean = True);
begin
  EraseCurve(DigitCurve);
  DigitCurve.CorrectCurve(Po, Pf, IsStep);
  DigitCurve.Draw(Canvas);

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
begin
  try
    GridMask.MajorGridColor := LineColor1;
    GridMask.MinorGridColor := LineColor2;
    GridMask.BckgndColor := BckgndColor;
    GridMask.Tolerance := Tolerance;
    GridMask.Threshold := Threshold;
    GridMask.FixCurve := FixCurve;
    GridMask.MaskSize := MaskSize;

    //Scale.PlotBox.PolarCoordinates := (Scale.CoordSystem = csPolar);

    if Scale.CoordSystem = csCartesian then
      GridMask.RemoveCartesianGrid(PlotImg, Scale.PlotBox)
    else
      GridMask.RemovePolarGrid(PlotImg, Scale.PlotBox, Scale.ImagePoint[2]);

    if GridMask.FixCurve then
    begin
      for i := 0 to FCurves.Count - 1 do
        GridMask.RebuildCurve(PlotImg, Scale.PlotBox, FCurves[i].Color);
    end;

    ResetZoomImage;
  finally
    IsChanged := True;
  end;
end;

procedure TPlotImage.GroupPoints(Region: TRect);
begin
  EraseCurve(DigitCurve);
  DigitCurve.GroupPointsInRegion(Region);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.DeletePoints(Region: TRect);
begin
  EraseCurve(DigitCurve);
  DigitCurve.DeletePointsInRegion(Region);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveUp;
begin
  EraseCurve(DigitCurve);
  DigitCurve.AddToY(-1);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveDown;
begin
  EraseCurve(DigitCurve);
  DigitCurve.AddToY(1);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveLeft;
begin
  EraseCurve(DigitCurve);
  DigitCurve.AddToX(-1);
  DigitCurve.Draw(Canvas);

  IsChanged := True;
end;

procedure TPlotImage.MoveCurveRight;
begin
  EraseCurve(DigitCurve);
  DigitCurve.AddToX(1);
  DigitCurve.Draw(Canvas);

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
  MoveMarker(Marker,TPoint.Create(Round(X), Round(Y)));
end;

procedure TPlotImage.UndistortImage;
var
  w, h: Integer;
  Stream: TMemoryStream;
  NewImg: TBGRABitmap;
begin
  try
    with Scale.PlotBox do
    begin
      w := Round(max(Vertex[0].DistanceTo(Vertex[1]),
                     Vertex[2].DistanceTo(Vertex[3])));
      h := Round(max(Vertex[0].DistanceTo(Vertex[3]),
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
  RootNode, DigitNode,
  ImageNode, PathNode,
  FileNode, DataNode,
  CDataNode, CurveNode: TDOMNode;
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
    TDOMElement(RootNode).SetAttribute('version', '1.1');
    XMLDoc.Appendchild(RootNode); // Save root node

    // Create document node
    RootNode:= XMLDoc.DocumentElement;
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
      PathNode.Appendchild(XMLDoc.CreateTextNode(UTF8Decode(ExtractFilePath(ImageName))));
      ImageNode.Appendchild(PathNode);

      FileNode := XMLDoc.CreateElement('name');
      FileNode.Appendchild(XMLDoc.CreateTextNode(UTF8Decode(ExtractFileName(ImageName))));
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
      DataNode.AppendChild(CDataNode);
      ImageNode.AppendChild(DataNode);

      DigitNode.Appendchild(ImageNode);
    end;

    // Add scale node
    DigitNode.AppendChild(Scale.ExportToXML(XMLDoc));

    // Add PlotBox node
    //DigitNode.AppendChild(Scale.PlotBox.ExportToXML(XMLDoc));

    // Add Grid node
    if GridMask.IsValid then
      DigitNode.AppendChild(GridMask.ExportToXML(XMLDoc));

    // Save document node
    RootNode.Appendchild(DigitNode);

    // Create curves node
    RootNode:= XMLDoc.DocumentElement;

    CurveNode := XMLDoc.CreateElement('curves');
    TDOMElement(CurveNode).SetAttribute('Count', UTF8Decode(IntToStr(Count)));

    // Save curve nodes
    for i := 0 to Count - 1 do
      CurveNode.Appendchild(ucurves[i].ExportToXML(XMLDoc));

    // Save curves node
    RootNode.AppendChild(CurveNode);

    WriteXMLFile(XMLDoc, FileName);
    Result := True;
  finally
    XMLDoc.Free;
    Stream.Free;

    FIsChanged := False;
  end;
end;

function TPlotImage.LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;
var
  i, w, h,
  SavedCurveCount,
  RealCurveCount: Integer;
  DigitVersion: Double;
  Path, ImgName: TFileName;
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  XMLDoc: TXMLDocument;
  Child, DigitChild,
  ImageChild, CurveChild: TDOMNode;

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

          // Read scale parameters
          if (DigitChild.CompareName('scale') = 0) then
            Scale.ImportFromXML(DigitChild);

          // Read PlotBox parameters
          if (DigitChild.CompareName('PlotBox') = 0) and
             (DigitVersion <= 1.0) then
            with Scale.PlotBox do
            begin
              ImportFromXML(DigitChild);
              PolarCoordinates := (Scale.CoordSystem = csPolar);
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

      // Read curves
      if (Child.CompareName('curves') = 0) then
      begin
        //ImportCurvesFromXML(Child);
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
            inc(RealCurveCount);
            //Create all the needed curves
            while (RealCurveCount > Count) do
              Self.AddCurve;

            FCurves[RealCurveCount - 1].ImportFromXML(CurveChild);
          end;

          // Go for the next curve
          CurveChild := CurveChild.NextSibling;
        end;
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
        if FileExists(Path  + ImgName) then
          ImageName := Path + ImgName
        else
          if FileExists(ExtractFilePath(FileName) + ImgName) then
          begin
            ImageName := ExtractFilePath(FileName) + ImgName;
          end
          else
          begin
            if assigned(PictureDlg) then
            begin
              PictureDlg.InitialDir := ExtractFilePath(FileName);
              if PictureDlg.Execute then
                ImageName := PictureDlg.FileName;
            end;
          end;
      end;
    end;

    FCurveIndex := 0;
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
