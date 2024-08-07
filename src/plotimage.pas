unit plotimage;

{$mode objfpc}{$H+}

interface

uses {$ifdef windows}Windows,{$endif} Forms, Classes, Controls, Graphics,
     ExtDlgs, Fgl, ComCtrls, SysUtils, DOM, XMLWrite, XMLRead, math, curves,
     coordinates, Dialogs, Types, Base64, BGRABitmap, BGRABitmapTypes,
     BGRAreadTiff;

type
  TPlotImageState = (piSetCurve, piSetScale, piSetPlotBox, piSetGrid);

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

  TGridMask = class
  protected
    FMask: TBGRABitmap;

    FMajorGridColor: TColor;
    FMinorGridColor: TColor;
    FBckgndColor: TColor;
    FTolerance: Integer;
    FThreshold: Double;
    FFixCurve: Boolean;
    FMaskSize: Integer;

    FIsValid: Boolean;
    FIsActive: Boolean;
  private
  public
    constructor Create(Width, Height: Integer);
    destructor Destroy; override;

    procedure SetSize(Width, Height: Integer);

    procedure RemoveCartesianGrid(PlotImg: TBGRABitmap; PlotBox: TPlotQuad);
    procedure RemovePolarGrid(PlotImg: TBGRABitmap; PlotBox: TPlotQuad;
                              Pc: TCurvePoint);

    procedure RebuildCurve(PlotImg: TBGRABitmap; PlotBox: TPlotQuad;
                           CurveColor: TColor);

    function ImportFromXML(Item: TDOMNode): Boolean;
    function ExportToXML(Doc: TXMLDocument): TDOMNode;

    property Mask: TBGRABitmap read FMask;
    property MajorGridColor: TColor read FMajorGridColor write FMajorGridColor;
    property MinorGridColor: TColor read FMinorGridColor write FMinorGridColor;
    property BckgndColor: TColor read FBckgndColor write FBckgndColor;
    property Tolerance: Integer read FTolerance write FTolerance;
    property Threshold: Double read FThreshold write FThreshold;
    property FixCurve: Boolean read FFixCurve write FFixCurve;
    property MaskSize: Integer read FMaskSize write FMaskSize;
    property IsValid: Boolean read FIsValid write FIsValid;
    property IsActive: Boolean read FIsActive write FIsActive;
  end;

  TMarkerList = specialize TFPGObjectList<TMarker>;

  TShowProgressEvent = procedure(Sender: TObject; Progress: Cardinal) of Object;
  THideProgressEvent = procedure(Sender: TObject) of Object;
  TSelectRegionEvent = procedure(Sender: TObject; RegionRect: TRect) of Object;
  TStateChangeEvent = procedure(Sender: TObject; NewState: TPlotImageState) of Object;
  TMarkerDraggedEvent = procedure(Sender: TObject; Marker: TMarker) of Object;

  TPlotImage = class(TCustomControl)
  protected type
    TCurveList = specialize TFPGObjectList<TDigitCurve>;
  protected
    FOldCursor: TCursor;

    InMouseMove: Boolean;
    MouseMovePos: TPoint;

    FState: TPlotImageState;

    FImageName: TFileName;
    FPlotImg: TBGRABitmap;
    FWhiteBoard: TBGRABitmap;
    FGridMask: TGridMask;

    FMarkers: TMarkerList;
    FMarkerList: TCurve;
    FAxesMarkers: Array [1..3] of TMarker;
    FBoxMarkers: Array [1..4] of TMarker;
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
    FAllCurvePoints: TIsland;

    FScale: TScale;
    FPlotBox: TPlotQuad;

    FImageIsLoaded: Boolean;

    FIsChanged: Boolean;
    FOnChange: TNotifyEvent;

    FOnShowProgress: TShowProgressEvent;
    FOnHideProgress: THideProgressEvent;
    FOnRegionSelected: TSelectRegionEvent;
    FOnStateChanged: TStateChangeEvent;
    FOnMarkerDragged: TMarkerDraggedEvent;

    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    function GetPixel(X, Y: Integer): LongInt; overload;
    function GetPixel(X, Y: Double): LongInt; overload;
    function GetPixel(P: TCurvePoint): LongInt; overload;

    function GetAxesPoint(Index: Integer): TCurvePoint;
    function GetBoxVertex(Index: Integer): TCurvePoint;
    function GetAxesMarkers(Index: Integer): TMarker;
    function GetBoxMarkers(Index: Integer): TMarker;

    function GetCount: Integer;
    function GetCurve(Index: Integer): TDigitCurve;
    function GetActiveCurve: TCurve;
    function GetDigitCurve: TDigitCurve;
    function GetPlotCurves(Index: Integer): TCurve;
    function GetPlotCurve: TCurve;

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

    procedure ClearMarkers;
    procedure UpdateMarkersInCurve;
    procedure UpdateMarkersInImage;

    procedure UpdateRegion(UpdateArea: TRect); overload;
    procedure UpdateRegion; overload;

    procedure EraseCurve(Curve: TDigitCurve);

    procedure LoadImage(FileName: TFileName); overload;
    procedure LoadImage(Stream: TStream); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;

    procedure FindCurvePoints;

    function FindNextPoint(var Pv: TCurvePoint; Interval: Integer;
                           ScanX: Boolean = False): Boolean;
    procedure DigitizeSpectrum(Pi: TCurvePoint;
                               FillCurvePoints: Boolean = True); overload;
    procedure DigitizeSpectrum; overload;

    procedure DigitizeMarkers;

    procedure FillIsland(Pi: TCurvePoint; var Island: TIsland;
                         JustInY: Boolean = True;
                         MaxPoints: Integer = 1000;
                         FillCurvePoints: Boolean = True); overload;
    procedure FillIsland(Xi, Yi: Double; var Island: TIsland;
                         JustInY: Boolean = True;
                         MaxPoints: Integer = 1000); overload;
    procedure AdjustCurve;
    procedure ConvertCurveToSymbols;

    function ConvertCoords(p: TCurvePoint): TCurvePoint; overload;
    function ConvertCoords(X, Y: Double): TCurvePoint; overload;

    procedure SwitchGrid;

    procedure PasteImage(Stream: TStream);

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
    procedure Interpolate(n: Integer; Index: Integer); overload;
    procedure Interpolate(n: Integer; AllCurves: Boolean = False); overload;
    procedure Interpolate(Xo, Xf: Double; n: Integer; Index: Integer); overload;
    procedure Interpolate(Xo, Xf: Double; n: Integer; AllCurves: Boolean = False); overload;
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

    function GetZoomImage(w, h: Integer; Region: TRect): TBitmap;

    function SaveToXML(FileName: TFileName): Boolean;
    function LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;

    property State: TPlotImageState read FState write SetState;
    property ImageName: TFileName read FImageName write SetImageName;
    property GridMask: TGridMask read FGridMask;
    property WhiteBoard: TBGRABitmap read FWhiteBoard;
    property PlotImg: TBGRABitmap read FPlotImg;
    property Markers: TMarkerList read FMarkers;
    property AxesPoint[Index: Integer]: TCurvePoint read GetAxesPoint write SetAxesPoint;
    property AxesMarkers[Index: Integer]: TMarker read GetAxesMarkers;
    property BoxVertex[Index: Integer]: TCurvePoint read GetBoxVertex write SetBoxVertex;
    property BoxMarkers[Index: Integer]: TMarker read GetBoxMarkers;
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

    {Return the number of curves}
    property Count: Integer read GetCount;
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
    property ImageIsLoaded: Boolean read FImageIsLoaded write FImageIsLoaded;

    property Scale: TScale read FScale;
    property PlotBox: TPlotQuad read FPlotBox;
    property ColorIsSet: Boolean read GetColorIsSet;
    property HasPoints: Boolean read GetHasPoints;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;

    property AllCurvePoints: TIsland read FAllCurvePoints;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnShowProgress: TShowProgressEvent read FOnShowProgress write FOnShowProgress;
    property OnHideProgress: THideProgressEvent read FOnHideProgress write FOnHideProgress;
    property OnRegionSelected: TSelectRegionEvent read FOnRegionSelected write FOnRegionSelected;
    property OnStateChanged: TStateChangeEvent read FOnStateChanged write FOnStateChanged;
    property OnMarkerDragged: TMarkerDraggedEvent read FOnMarkerDragged write FOnMarkerDragged;
  end;

function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor; LineWith: Integer = 3): TBGRABitmap;

implementation

uses utils;

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
        DrawLineAntialias(0, 0, Width - 1, Height - 1, Color, LineWith);
        DrawLineAntialias(0, Height - 1, Width - 1, 0, Color, LineWith);
      end;
      '+': begin
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
  Result := GetCurvePoint(FRect.CenterPoint.X, FRect.CenterPoint.Y);
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

//=============================|TGridMask|====================================//
constructor TGridMask.Create(Width, Height: Integer);
begin
  inherited Create;

  FMask := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);

  FMajorGridColor := clBlack;
  FMinorGridColor := clGray;
  FBckgndColor := clWhite;
  FTolerance := 10;
  FThreshold := 0.5;

  FIsValid := False;
  FIsActive := False;
end;

destructor TGridMask.Destroy;
begin
  FMask.Free;

  inherited Destroy;
end;

procedure TGridMask.SetSize(Width, Height: Integer);
begin
  FMask.SetSize(Width, Height);
  FMask.FillTransparent;
end;

procedure TGridMask.RemoveCartesianGrid(PlotImg: TBGRABitmap; PlotBox: TPlotQuad);
const
  angle_step = 1;
var
  t_idx: Integer;
  i, j: Integer;
  x, y: Double;
  diag_len: Integer;

  p: PBGRAPixel;

  grid_points: Array of Array of Boolean;
  accumulator: THough2DMap;

  num_thetas: Integer;
  theta: Double;
  theta_count: Array of Integer;
  sin_theta, cos_theta: Array of Double;

  rho: Integer;

  C1, C2: LongInt;
  R1, G1, B1 : Byte;
  R2, G2, B2 : Byte;

  hori_count,
  vert_count,
  hori_index,
  vert_index: Integer;
  max_count_hori,
  max_count_vert: Integer;
begin
  try
    C1 := ColorToRGB(MajorGridColor);
    C2 := ColorToRGB(MinorGridColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);
    R2 := Red(C2);
    G2 := Green(C2);
    B2 := Blue(C2);

    with PlotImg do
    begin
      // Diagonal length of image
      diag_len := Round(Sqrt(Width*Width + Height*Height));

      // Put the canvas values in an array for faster access
      SetLength(grid_points, Width, Height);
      for j := 0 to Height - 1 do
      begin
        p := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          grid_points[i, j] := PlotBox.Contains(GetCurvePoint(i, j)) and
                              (AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) or
                               AreSimilar(p^.red, p^.green, p^.blue, R2, G2, B2, Tolerance));
          inc(p);
        end;
      end;
    end;

    // Cache some reusable values
    num_thetas := 1 + Round(180/angle_step);
    SetLength(sin_theta, num_thetas);
    SetLength(cos_theta, num_thetas);
    SetLength(theta_count, num_thetas);
    for t_idx := Low(sin_theta) to High(sin_theta) do
    begin
      theta := -90.0 + t_idx*angle_step;
      sin_theta[t_idx] := sin(theta*PI/180.0);
      cos_theta[t_idx] := cos(theta*PI/180.0);
      theta_count[t_idx] := 0;
    end;

    // Hough accumulator array of theta vs rho
    SetLength(accumulator, 2*diag_len, num_thetas);
    for i := Low(accumulator) to High(accumulator) do
      for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        accumulator[i, j] := 0;

    max_count_hori := 0;
    max_count_vert := 0;
    // Vote in the hough accumulator
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
          for t_idx := Low(sin_theta) to High(sin_theta) do
          begin
            // Calculate rho, diag_len is added for a positive index
            rho := diag_len + Round(i*cos_theta[t_idx] + j*sin_theta[t_idx]);
            inc(accumulator[rho, t_idx]);

            if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
            begin
              // Horizontal line
              if (accumulator[rho, t_idx] > max_count_hori) then
                max_count_hori := accumulator[rho, t_idx];
            end
            else
            begin
              // Vertical line
              if (accumulator[rho, t_idx] > max_count_vert) then
                max_count_vert := accumulator[rho, t_idx];
            end;
          end;

    // Histogram of angles
    for i := Low(accumulator) to High(accumulator) do
      for t_idx := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
        begin
          // Horizontal line
          if (accumulator[i, t_idx] > Threshold*max_count_hori) then
            inc(theta_count[t_idx]);
        end
        else
        begin
          // Vertical line
          if (accumulator[i, t_idx] > Threshold*max_count_vert) then
            inc(theta_count[t_idx]);
        end;

    // Determine angle for horizontal and vertical grid lines
    hori_count := 0;
    vert_count := 0;
    hori_index := 0;
    vert_index := 0;
    for i := Low(accumulator) to High(accumulator) do
      for t_idx := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        if (abs(sin_theta[t_idx]) > abs(cos_theta[t_idx])) then
        begin
          // Mostly horizontal line
          if (accumulator[i, t_idx] > Threshold*max_count_hori) then
          begin
            if (theta_count[t_idx] > hori_count) then
            begin
              hori_count := theta_count[t_idx];
              hori_index := t_idx;
            end;
          end;
        end
        else
        begin
          // Mostly vertical line
          if (accumulator[i, t_idx] > Threshold*max_count_vert) then
          begin
            if (theta_count[t_idx] > vert_count) then
            begin
              vert_count := theta_count[t_idx];
              vert_index := t_idx;
            end;
          end;
        end;

    //Draw all the points in the mask
    try
      Mask.FillTransparent;

      // Reset the status of the mask
      IsValid := False;
      IsActive := False;

      // Now, draw the lines
      R1 := Red(BckgndColor);
      G1 := Green(BckgndColor);
      B1 := Blue(BckgndColor);

      for i := Low(accumulator) to High(accumulator) do
      begin
        rho := i - diag_len;
        // Draw horizontal lines
        if (accumulator[i, hori_index] > Threshold*max_count_hori) then
        begin
          // Fill the line with all the points that meet the color criteria
          for j := 0 to Mask.Width - 1 do
          begin
            x := j;
            y := (rho - x*cos_theta[hori_index])/sin_theta[hori_index];

            if (y >= 0) and (Round(y) < Mask.Height) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
        end;

        // Draw vertical lines
        if (accumulator[i, vert_index] > Threshold*max_count_vert) then
        begin
          // Fill the line with all the points that meet the color criteria
          for j := 0 to Mask.Height - 1 do
          begin
            y := j;
            x := (rho - y*sin_theta[vert_index])/cos_theta[vert_index];

            if (x >= 0) and (Round(x) < Mask.Width) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
        end;
      end;
    finally
      Mask.InvalidateBitmap;

      IsValid := True;
      IsActive := True;
    end;
  finally
    // Release all the dynamic arrays
    SetLength(grid_points, 0);
    SetLength(sin_theta, 0);
    SetLength(cos_theta, 0);
    SetLength(theta_count, 0);
    SetLength(accumulator, 0);
  end;
end;

procedure TGridMask.RemovePolarGrid(PlotImg: TBGRABitmap; PlotBox: TPlotQuad; Pc: TCurvePoint);
const
  angle_step = 0.5;
var
  t_idx: Integer;
  i, j, k: Integer;
  x, y: Double;
  diag_len: Integer;

  p: PBGRAPixel;

  grid_points: Array of Array of Boolean;
  accumulator: THough2DMap;

  num_thetas: Integer;
  theta: Double;
  sin_theta, cos_theta: Array of Double;

  rho: Integer;

  C1, C2: LongInt;
  R1, G1, B1 : Byte;
  R2, G2, B2 : Byte;

  max_count: Integer;

  accum_radius: Array of Integer;
  max_radius_count: Integer;
  radius_max_count: Integer;
begin
  try
    C1 := ColorToRGB(MajorGridColor);
    C2 := ColorToRGB(MinorGridColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);
    R2 := Red(C2);
    G2 := Green(C2);
    B2 := Blue(C2);

    with PlotImg do
    begin
      // Diagonal length of image
      diag_len := Round(Sqrt(Width*Width + Height*Height));

      // Put the canvas values in an array for faster access
      SetLength(grid_points, Width, Height);
      for j := 0 to Height - 1 do
      begin
        p := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          //grid_points[i, j] := p^.red or (p^.green shl 8) or (p^.blue shl 16);
          grid_points[i, j] := PlotBox.Contains(GetCurvePoint(i, j)) and
                              (AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) or
                               AreSimilar(p^.red, p^.green, p^.blue, R2, G2, B2, Tolerance));
          inc(p);
        end;
      end;
    end;

    // Cache some reusable values
    num_thetas := 1 + Round(180/angle_step);
    SetLength(sin_theta, num_thetas);
    SetLength(cos_theta, num_thetas);
    for t_idx := Low(sin_theta) to High(sin_theta) do
    begin
      theta := -90.0 + t_idx*angle_step;
      sin_theta[t_idx] := sin(theta*PI/180.0);
      cos_theta[t_idx] := cos(theta*PI/180.0);
    end;

    // Hough accumulator array of theta vs rho
    SetLength(accumulator, 2*diag_len, num_thetas);
    for i := Low(accumulator) to High(accumulator) do
      for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        accumulator[i, j] := 0;

    max_count := 0;
    // Vote in the hough accumulator
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
          for t_idx := Low(sin_theta) to High(sin_theta) do
          begin
            // Calculate rho, diag_len is added for a positive index
            rho := diag_len + Round(i*cos_theta[t_idx] + j*sin_theta[t_idx]);
            inc(accumulator[rho, t_idx]);

            if (accumulator[rho, t_idx] > max_count) then
              max_count := accumulator[rho, t_idx];
          end;

    // Find circles (all should be concentric, and centered in the origin)
    SetLength(accum_radius, diag_len);
    max_radius_count := 0;
    // Vote in the hough accumulator (for circles)
    for i := Low(grid_points) to High(grid_points) do
      for j := Low(grid_points[Low(grid_points)]) to High(grid_points[Low(grid_points)]) do
        if grid_points[i, j] then
        begin
          // Calculate radius
          rho := Round(Sqrt((i - Pc.X)*(i - Pc.X) + (j - Pc.Y)*(j - Pc.Y)));
          inc(accum_radius[rho]);

          if (accum_radius[rho] > max_radius_count) then
          begin
            radius_max_count := rho;
            max_radius_count := accum_radius[rho];
          end;
        end;

    //Draw all the points in the mask
    try
      Mask.FillTransparent;

      // Reset the status of the mask
      IsValid := False;
      IsActive := False;

      R1 := Red(BckgndColor);
      G1 := Green(BckgndColor);
      B1 := Blue(BckgndColor);

      // Now, draw the lines
      for i := Low(accumulator) to High(accumulator) do
        for j := Low(accumulator[Low(accumulator)]) to High(accumulator[Low(accumulator)]) do
        begin
          rho := i - diag_len;
          // Draw lines
          if (accumulator[i, j] >= Threshold*max_count) and
             ((Pc.X*cos_theta[j] + Pc.Y*sin_theta[j] - rho) <= 2) then
          begin
            if (abs(sin_theta[j]) > abs(cos_theta[j])) then
            begin
              // Horizontal line
              // Fill the line with all the points that meet the color criteria
              for k := 0 to Mask.Width - 1 do
              begin
                x := k;
                y := (rho - x*cos_theta[j])/sin_theta[j];

                if (y >= 0) and (Round(y) < Mask.Height) and
                   grid_points[Round(x), Round(y)] then
                begin
                  p := Mask.Scanline[Round(y)];
                  inc(p, Round(x));
                  if (p^.alpha = 0) then
                  begin
                    p^.red := R1;
                    p^.green := G1;
                    p^.blue := B1;
                    p^.alpha := 255;
                    // Mask.SetPixel(Round(x), Round(y), BckgndColor);
                  end;
                end;
              end;
            end
            else
            begin
              // Vertical line
              // Fill the line with all the points that meet the color criteria
              for k := 0 to Mask.Height - 1 do
              begin
                y := k;
                x := (rho - y*sin_theta[j])/cos_theta[j];

                if (x >= 0) and (Round(x) < Mask.Width) and
                   grid_points[Round(x), Round(y)] then
                begin
                  p := Mask.Scanline[Round(y)];
                  inc(p, Round(x));
                  if (p^.alpha = 0) then
                  begin
                    p^.red := R1;
                    p^.green := G1;
                    p^.blue := B1;
                    p^.alpha := 255;
                    // Mask.SetPixel(Round(x), Round(y), BckgndColor);
                  end;
                end;
              end;
            end;
          end;
        end;

      // Now, draw the circles
      for i := 1 to High(accum_radius) do
      begin
        // Draw circles
        if ((radius_max_count*accum_radius[i] div i) >= Threshold*max_radius_count) then
          for j := 0 to Round(2*PI*i) do
          begin
            x := Pc.X + i*cos(j/i);
            y := Pc.Y + i*sin(j/i);;

            if (x >= 0) and (Round(x) < Mask.Width) and
               (y >= 0) and (Round(y) < Mask.Height) and
               grid_points[Round(x), Round(y)] then
            begin
              p := Mask.Scanline[Round(y)];
              inc(p, Round(x));
              if (p^.alpha = 0) then
              begin
                p^.red := R1;
                p^.green := G1;
                p^.blue := B1;
                p^.alpha := 255;
                // Mask.SetPixel(Round(x), Round(y), BckgndColor);
              end;
            end;
          end;
      end;
    finally
      Mask.InvalidateBitmap;

      IsValid := True;
      IsActive := True;
    end;
  finally
    // Release all the dynamic arrays
    SetLength(grid_points, 0);
    SetLength(sin_theta, 0);
    SetLength(cos_theta, 0);
    SetLength(accumulator, 0);
    SetLength(accum_radius, 0);
  end;
end;

procedure TGridMask.RebuildCurve(PlotImg: TBGRABitmap; PlotBox: TPlotQuad;
                                 CurveColor: TColor);
var
  i, j, k, l: Integer;
  p: PBGRAPixel;

  curve_points: Array of Array of Boolean;

  C1: LongInt;
  R1, G1, B1 : Byte;

  top_pixels: TIsland;
  bottom_pixels: TIsland;
  left_pixels: TIsland;
  right_pixels: TIsland;

  function is_masked(x, y: Integer): Boolean;
  begin
    p := Mask.Scanline[y];
    inc(p, x);
    Result := (p^.alpha > 0);
  end;

begin
  try
    C1 := ColorToRGB(CurveColor);

    R1 := Red(C1);
    G1 := Green(C1);
    B1 := Blue(C1);

    // Put the canvas values in an array for faster access
    SetLength(curve_points, PlotImg.Width, PlotImg.Height);
    for j := 0 to PlotImg.Height - 1 do
    begin
      p := PlotImg.Scanline[j];
      for i := 0 to PlotImg.Width - 1 do
      begin
        curve_points[i, j] := PlotBox.Contains(GetCurvePoint(i, j)) and
                              AreSimilar(p^.red, p^.green, p^.blue,
                                         R1, G1, B1, Tolerance);

        inc(p);
      end;
    end;

    top_pixels := TIsland.Create;
    bottom_pixels := TIsland.Create;
    left_pixels := TIsland.Create;
    right_pixels := TIsland.Create;

    for i := 0 to PlotImg.Width - 1 do
      for j := 0 to PlotImg.Height - 1 do
      begin
        top_pixels.Clear;
        bottom_pixels.Clear;
        left_pixels.Clear;
        right_pixels.Clear;

        if curve_points[i, j] and is_masked(i, j) then
        begin
          // Find the pixels at the edges
          for k := max(1, i - MaskSize) to min(PlotImg.Width - 2, i + MaskSize) do
            for l := max(1, j - MaskSize) to min(PlotImg.Height - 2, j + MaskSize) do
            begin
              if curve_points[k, l] and (not is_masked(k, l)) then
              begin
                if is_masked(k + 1, l) then
                  left_pixels.AddPoint(k, l);
                if is_masked(k - 1, l) then
                  right_pixels.AddPoint(k, l);
                if is_masked(k, l + 1) then
                  bottom_pixels.AddPoint(k, l);
                if is_masked(k, l - 1) then
                  top_pixels.AddPoint(k, l);
                if is_masked(k + 1, l + 1) then
                begin
                  left_pixels.AddPoint(k, l);
                  bottom_pixels.AddPoint(k, l);
                end;
                if is_masked(k + 1, l - 1) then
                begin
                  left_pixels.AddPoint(k, l);
                  top_pixels.AddPoint(k, l);
                end;
                if is_masked(k - 1, l - 1) then
                begin
                  right_pixels.AddPoint(k, l);
                  top_pixels.AddPoint(k, l);
                end;
                if is_masked(k - 1, l + 1) then
                begin
                  right_pixels.AddPoint(k, l);
                  bottom_pixels.AddPoint(k, l);
                end;
              end;
            end;
          // Connect the edge pixels
          for k := 0 to top_pixels.Count - 1 do
            for l := 0 to bottom_pixels.Count - 1 do
            begin
              Mask.DrawLine(Round(top_pixels.Point[k].X),
                            Round(top_pixels.Point[k].Y),
                            Round(bottom_pixels.Point[l].X),
                            Round(bottom_pixels.Point[l].Y),
                            BGRAPixelTransparent,//BGRA(R1, G1, B1, 255),
                            True, dmSet);
            end;

          for k := 0 to left_pixels.Count - 1 do
            for l := 0 to right_pixels.Count - 1 do
            begin
              //ShowMessage(IntToStr(k) + ', ' + IntToStr(l) + ', ' + IntToStr(i) + ', ' + IntToStr(j));
              //ShowMessage(FloatToStr(left_pixels.Point[k].X) + ', ' +
              //            FloatToStr(left_pixels.Point[k].Y) + ', ' +
              //            FloatToStr(right_pixels.Point[l].X) + ', ' +
              //            FloatToStr(right_pixels.Point[l].Y));
              Mask.DrawLine(Round(left_pixels.Point[k].X),
                            Round(left_pixels.Point[k].Y),
                            Round(right_pixels.Point[l].X),
                            Round(right_pixels.Point[l].Y),
                            BGRAPixelTransparent,//BGRA(R1, G1, B1, 255),
                            True, dmSet);
            end;
        end;
      end;
  finally
    SetLength(curve_points, 0);

    top_pixels.Free;
    bottom_pixels.Free;
    left_pixels.Free;
    right_pixels.Free;
  end;
end;

function TGridMask.ImportFromXML(Item: TDOMNode): Boolean;
var
  i, w, h: Integer;
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  Child: TDOMNode;
begin
  Result := False;
  try
    IsValid := False;
    IsActive := False;

    FixCurve := False;

    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    w := 0;
    h := 0;
    with Item.Attributes do
    begin
      for i := 0 to Length - 1 do
      begin
        if (Item[i].CompareName('Width') = 0) then
          w := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Height') = 0) then
          h := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Active') = 0) then
          IsActive := StrToBool(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MajorGridColor') = 0) then
          MajorGridColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MinorGridColor') = 0) then
          MinorGridColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('BckgndColor') = 0) then
          BckgndColor := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Tolerance') = 0) then
          Tolerance := StrToInt(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('Threshold') = 0) then
          Threshold := StrToFloat(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('FixCurve') = 0) then
          FixCurve := StrToBool(UTF8Encode(Item[i].NodeValue));
        if (Item[i].CompareName('MaskSize') = 0) then
          MaskSize := StrToInt(UTF8Encode(Item[i].NodeValue));
      end;
    end;

    Child := Item.FirstChild;
    while Assigned(Child) do
    begin
      // Grid data
      if (Child.CompareName('data') = 0) then
      begin
        // Extract the image data as a Base64 string
        EncBuffer := UTF8Encode(Child.FirstChild.NodeValue);
        // Convert it back to an ordinary string
        Buffer := DecodeStringBase64(EncBuffer);

        // Put the image data in the stream
        Stream.Clear;
        Stream.Write(Pointer(Buffer)^, Length(Buffer));
        Stream.Position := 0;

        // And load the stream in the GridMask
        Mask.LoadFromStream(Stream);
      end;

      Child := Child.NextSibling;
    end;

    IsValid := True;

    Result := True;
  finally
    Stream.Free;
  end;
end;

function TGridMask.ExportToXML(Doc: TXMLDocument): TDOMNode;
var
  Stream: TMemoryStream;
  Buffer: String; // common string with the jpg info
  EncBuffer: String; // it's Base64 equivalent
  DataNode,
  CDataNode: TDOMNode;
begin
  try
    // Create the stream to save the image
    Stream := TMemoryStream.Create;

    Result := Doc.CreateElement('grid');
    with TDOMElement(Result) do
    begin
      SetAttribute('Width', UTF8Decode(IntToStr(Mask.Width)));
      SetAttribute('Height', UTF8Decode(IntToStr(Mask.Height)));
      SetAttribute('Active', UTF8Decode(BoolToStr(IsActive, True)));
      SetAttribute('MajorGridColor', UTF8Decode('$' + IntToHex(MajorGridColor, 6)));
      SetAttribute('MinorGridColor', UTF8Decode('$' + IntToHex(MinorGridColor, 6)));
      SetAttribute('BckgndColor', UTF8Decode('$' + IntToHex(BckgndColor, 6)));
      SetAttribute('Tolerance', UTF8Decode(IntToStr(Tolerance)));
      SetAttribute('Threshold', UTF8Decode(FloatToStr(Threshold)));
      SetAttribute('FixCurve', UTF8Decode(BoolToStr(FixCurve, True)));
      SetAttribute('MaskSize', UTF8Decode(IntToStr(MaskSize)));
    end;

    DataNode := Doc.CreateElement('data');
    // Now encode the grid image and save it to the XML file
    Stream.Clear;
    Stream.Position := 0;

    Mask.SaveToStreamAsPng(Stream);

    // Extract Image contents as a common string
    SetString(Buffer, Stream.Memory, Stream.Size);
    // Encode the image contents in a Base64 string
    EncBuffer := EncodeStringBase64(Buffer);
    // Store the image on a CData-section node
    CDataNode := Doc.CreateCDATASection(UTF8Decode(EncBuffer));
    DataNode.AppendChild(CDataNode);

    Result.AppendChild(DataNode);
  finally
  end;
end;

//=============================|TGridMask|====================================//

//=============================|TPlotImage|===================================//
constructor TPlotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlotImg := TBGRABitmap.Create;
  FWhiteBoard := TBGRABitmap.Create;
  FGridMask := TGridMask.Create(0, 0);

  FMarkers := TMarkerList.Create;
  FMarkerList := TCurve.Create;

  FCurves := TCurveList.Create;
  FAllCurvePoints := TIsland.Create;
  FScale := TScale.Create;
  FPlotBox := TPlotQuad.Create;

  Reset;
end;

destructor TPlotImage.Destroy;
begin
  FGridMask.Free;
  FWhiteBoard.Free;
  FPlotImg.Free;

  FMarkers.Free;
  FMarkerList.Free;

  FCurves.Free;
  FAllCurvePoints.Free;
  FScale.Free;
  FPlotBox.Free;

  inherited Destroy;
end;

procedure TPlotImage.Reset;
var
  i: Integer;
begin
  FState := piSetCurve;

  FCurves.Clear;
  FCurves.Add(TDigitCurve.Create('Curve1'));

  FCurveIndex := 0;

  FScale.Reset;
  FPlotBox.Reset;

  FMarkers.Clear;
  ActiveMarker := nil;
  MarkerUnderCursor := nil;
  for i := 1 to 3 do
    FAxesMarkers[i] := nil;
  for i := 1 to 4 do
    FBoxMarkers[i] := nil;

  FOldCursor := Cursor;

  FImageIsLoaded := False;

  FGridMask.IsValid := False;
  FGridMask.IsActive := False;

  InMouseMove := false;

  FIsChanged := False;
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
  // Notify the parent that must show the progress bar
  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0);

  Tolerance := DigitCurve.Tolerance;
  C1 := ColorToRGB(DigitCurve.Color);

  R1 := Red(C1);
  G1 := Green(C1);
  B1 := Blue(C1);

  AllCurvePoints.Clear;
  for j := 0 to PlotImg.Height - 1 do
  begin
    p1 := GridMask.Mask.Scanline[j];
    p2 := PlotImg.Scanline[j];

    for i := 0 to PlotImg.Width - 1 do
    begin
      if PlotBox.Contains(GetCurvePoint(i, j)) then
      begin
        if GridMask.IsValid and GridMask.IsActive and (p1^.alpha > 0) then
          p := p1
        else
          p := p2;

        if AreSimilar(p^.red, p^.green, p^.blue, R1, G1, B1, Tolerance) then
          AllCurvePoints.AddPoint(GetCurvePoint(i, j));
          //AllCurvePoints.AddPoint(Scale.FromImgToPlot(i, j));
      end;

      inc(p1);
      inc(p2);

      // Notify the parent that must update the progress bar
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, Round(100*(j*PlotImg.Width + i)/
                                   (PlotImg.Width*PlotImg.Height)));

      Application.ProcessMessages;
    end;
  end;
  //AllCurvePoints.SortCurve;

  // Notify the parent that must hide the progress bar
  if Assigned(OnHideProgress) then
    OnHideProgress(Self);
end;

//function TPlotImage.FindNextPoint(var Pv: TCurvePoint; Interval: Integer;
//                                  ScanX: Boolean = False): Boolean;
//var
//  // Number of pixels that the line is expected to spread
//  Spread: Integer;
//
//  Nx, Ny, d: Double;
//  dk: Array of Double;
//  i, j, k: Integer;
//  ki: Array of Integer;
//  Pi, Pvp, Pip: TCurvePoint;
//  P: Array of TCurvePoint;
//  NoPnts: Integer;
//begin
//  if ScanX then
//    Spread := 0
//  else
//    Spread := DigitCurve.Spread;
//
//  Nx := Interval*Abs(Scale.Ny(Pv).X);
//  Ny := Interval*Abs(Scale.Ny(Pv).Y);
//  Pvp := Scale.FromImgToPlot(Pv);
//  d := max(1, Modulus(Pvp));
//
//  NoPnts := 0;
//  Result := False;
//
//  SetLength(ki, 1 + 2*Spread);
//  SetLength(dk, 1 + 2*Spread);
//  SetLength(P, 1 + 2*Spread);
//  try
//    // We scan all the potential curve points,
//    // trying to find all that are within the spread range
//    for i := AllCurvePoints.Count - 1 downto 0 do
//    begin
//      Pi := AllCurvePoints.Point[i];
//      Pip := Scale.FromImgToPlot(Pi);
//      //if Distance(Pv, Pi, not ScanX) <= Interval then
//      //ShowMessage(FloatToStr(Scale.Ny(Pv).X) + ', ' + FloatToStr(Scale.Ny(Pv).Y));
//      if (Abs(Pv.X - Pi.X) <= Nx) and (Abs(Pv.Y - Pi.Y) <= Ny) then//and
//         //(Abs((Pvp.X - Pip.X)/d) < 1e-2) then
//      begin
//        ShowMessage(FloatToStr(Abs(Pvp.X - Pip.X)/d) + ': (' +
//                    FloatToStr(Pvp.X) + ', ' + FloatToStr(Pvp.Y) + '), (' +
//                    FloatToStr(Pip.X) + ', ' + FloatToStr(Pip.Y) + ')');
//        if (NoPnts < 1 + 2*Spread) then
//        begin
//          ki[NoPnts] := i;
//          dk[NoPnts] := Scale.Distance(Pv, Pi);
//          P[NoPnts] := Pi;
//          inc(NoPnts);
//        end
//        else
//        begin
//          k := 0;
//          for j := 1 to NoPnts - 1 do
//            if (dk[j] > dk[j - 1]) then
//              k := j;
//
//          if (dk[k] > Scale.Distance(Pv, Pi)) then
//          begin
//            ki[k] := i;
//            dk[k] := Scale.Distance(Pv, Pi);
//            P[k] := Pi;
//          end;
//        end;
//      end;
//    end;
//
//    if (NoPnts > 0) then
//    begin
//      Pi := GetcurvePoint(0, 0);
//      for i := 0 to NoPnts - 1 do
//      begin
//        Pi := Pi + P[i];
//        AllCurvePoints.DeletePoint(ki[i]);
//      end;
//
//      Pv := Pi/NoPnts;
//      Result := True;
//    end;
//  finally
//    SetLength(ki, 0);
//    SetLength(dk, 0);
//    SetLength(P, 0);
//  end;
//end;

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
  Result := AllCurvePoints.Contains(Pi);
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

//procedure TPlotImage.DigitizeSpectrum(Pi: TCurvePoint;
//                                      ProgressBar: TProgressBar = nil;
//                                      FillCurvePoints: Boolean = True);
//var
//  i: Integer;
//  Delta, L: Double;
//  Pv, Pn: TCurvePoint;
//  Ppv, Ppn: TCurvePoint;
//  PNew: TCurvePoint;
//  NoPnts: Integer;
//
//  // Steps (in the image) between one point and the next
//  PixelStep: Integer;
//  // Interval of pixels to scan below and above the expected point
//  Interval: Integer;
//
//  ML: TCurve;
//begin
//  if FillCurvePoints then
//    FindCurvePoints(ProgressBar);
//
//  PixelStep := DigitCurve.Step;
//  Interval := DigitCurve.Interval;
//
//  if Assigned(ProgressBar) then
//  begin
//    ProgressBar.Visible := True;
//    ProgressBar.Position := 0;
//  end;
//
//  DigitCurve.NextCurve(False);
//  Curve.AddPoint(Pi);
//
//  if PixelStep > 0 then
//    i := 0
//  else
//    i := AllCurvePoints.Count - 1;
//
//  L := AllCurvePoints.Count;
//  PNew := GetcurvePoint(0, 0);
//  NoPnts := 0;
//  repeat
//    Ppn := AllCurvePoints.Point[i];
//    Pn := Scale.FromPlotToImg(Ppn);
//
//    if (Abs(Pn.Y - Pi.Y) <= Interval) then
//    begin
//
//    end;
//
//    inc(i, sign(PixelStep));
//
//    if (i < 0) then i := AllCurvePoints.Count - 1;
//    if (i >= AllCurvePoints.Count) then i := 0;
//
//    if Assigned(ProgressBar) then
//      ProgressBar.Position := Round(100*(L - AllCurvePoints.Count)/L);
//
//    Application.ProcessMessages;
//  until (AllCurvePoints.Count = 0);
//
//
//
//
//
//
//
//
//
//  repeat
//    if (NoPnts = 0) then
//    begin
//      if () then
//      begin
//        Pi := Pp;
//        PNew := Pp;
//        NoPnts := 1;
//      end;
//    end
//    else
//    begin
//      if ((PixelStep > 0) and (PNew.X >= Pp.X)) or
//         ((PixelStep < 0) and (PNew.X <= Pp.X)) then
//      begin
//        if (Abs(Pi.X - AllCurvePoints.Point[i].X) < Scale.dx(Pi)) and
//           () then
//        begin
//          PNew := PNew + AllCurvePoints.Point[i];
//          inc(NoPnts);
//        end
//        else
//        begin
//          if (NoPnts > 0) then
//          begin
//            Curve.AddPoint(PNew/NoPnts);
//            PNew := GetcurvePoint(0, 0);
//            NoPnts := 0;
//         end;
//        end;
//      end;
//      AllCurvePoints.DeletePoint(i);
//      inc(i, sign(-PixelStep));
//    end;
//
//    inc(i, sign(PixelStep));
//
//    if (i < 0) then i := AllCurvePoints.Count - 1;
//    if (i >= AllCurvePoints.Count) then i := 0;
//
//    if Assigned(ProgressBar) then
//      ProgressBar.Position := Round(100*(L - AllCurvePoints.Count)/L);
//
//    Application.ProcessMessages;
//  until (AllCurvePoints.Count = 0);
//
//
//
//
//
//  Delta := 0;
//  if PixelStep > 0 then
//    L := Width - Pi.X
//  else
//    L := Pi.X;
//
//  ML := MarkerList;
//  if (ML.Count > 2) then
//    if (Scale.CoordSystem = csCartesian) then
//      ML.Interpolate(1 + Abs(Round((Scale.FromPlotToImg(ML.Point[ML.Count - 1]).X -
//                                    Scale.FromPlotToImg(ML.Point[0]).X)/PixelStep)))
//    else
//      ML.Interpolate(1 + Abs(360 div PixelStep));
//
//  i := 0;
//
//  // Move to the next and look for [Interval] points up and down
//  repeat
//    case ML.Count of
//      0..2: begin
//        // Not enough markers to guide the search
//        // Find the closer point
//        if (Scale.CoordSystem = csPolar) then
//        begin
//          Pp := CartesianToPolar(Pi - Scale.ImagePoint[2]);
//          if (2*Pp.Y*Pp.Y < PixelStep*PixelStep) then
//            Pp.Y := PixelStep;
//          Pp.X := Pp.X - ArcSin(PixelStep/Sqrt(4*Pp.Y*Pp.Y - PixelStep*PixelStep))*90/ArcTan(1);
//          Pi := Scale.ImagePoint[2] + PolarToCartesian(Pp);
//
//          Delta := Delta + Sign(PixelStep)*ArcSin(PixelStep/Sqrt(4*Pp.Y*Pp.Y - PixelStep*PixelStep))*90/ArcTan(1);
//        end
//        else
//        begin
//          Pi := Pi + PixelStep*Scale.Nx(Pi);
//          Delta := SegmentLength(Pi, Scale.Nx(Pi));
//        end;
//
//        PNew := Pi;
//      end
//      else
//      begin
//        // There are enough markers to guide the search
//        inc(i);
//
//        if (PixelStep > 0) then
//          PNew := Scale.FromPlotToImg(ML.Point[i])
//        else
//          PNew := Scale.FromPlotToImg(ML.Point[ML.Count - 1 - i]);
//      end;
//    end;
//
//
//    if FindNextPoint(PNew, Interval) then
//    begin
//      Pi := PNew;
//      Curve.AddPoint(Pi);
//    end;
//
//    if Assigned(ProgressBar) then
//    begin
//      case ML.Count of
//        0..2: begin
//          if (Scale.CoordSystem = csCartesian) then
//          begin
//            if PixelStep > 0 then
//              ProgressBar.Position := Round(100*Pi.X/L)
//            else
//              ProgressBar.Position := Round(100*(1 - Pi.X/L));
//          end
//          else
//            ProgressBar.Position := Round(100*Abs(Delta)/360);
//        end
//        else
//          ProgressBar.Position := Round(i/(ML.Count - 1));
//        end;
//    end;
//
//    Application.ProcessMessages;
//  until (not PlotBox.Contains(Pi)) or
//        ((Scale.CoordSystem = csPolar) and (Abs(Delta) > 360)) or
//        ((ML.Count > 2) and (i >= ML.Count - 1));
//
//  if Assigned(ProgressBar) then
//    ProgressBar.Visible := False;
//
//  SortCurve;
//
//  IsChanged := True;
//end;

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
  if FillCurvePoints then
    FindCurvePoints;

  PixelStep := DigitCurve.Step;
  Interval := DigitCurve.Interval;

  // Notify the parent that must show the progress bar
  if Assigned(OnShowProgress) then
    OnShowProgress(Self, 0);

  DigitCurve.NextCurve(False);
  Curve.AddPoint(Pi);

  //for i := 0 to AllCurvePoints.Count - 1 do
  //  Curve.AddPoint(Scale.FromImgToPlot(AllCurvePoints.Point[i]));
  //Curve.SortCurve;
  //for i := 0 to Curve.Count - 1 do
  //  Curve.Point[i] := Scale.FromPlotToImg(Curve.Point[i]);
  //Exit;

  Delta := 0;
  if PixelStep > 0 then
    L := Width - Pi.X
  else
    L := Pi.X;

  ML := MarkerList;
  if (ML.Count > 2) then
    if (Scale.CoordSystem = csCartesian) then
      ML.Interpolate(1 + Abs(Round((Scale.FromPlotToImg(ML.Point[ML.Count - 1]).X -
                                    Scale.FromPlotToImg(ML.Point[0]).X)/PixelStep)))
    else
      ML.Interpolate(1 + Abs(360 div PixelStep));

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

    // Notify the parent that must update the progress bar
    if Assigned(OnShowProgress) then
    begin
      case ML.Count of
        0..2: begin
          if (Scale.CoordSystem = csCartesian) then
          begin
            if PixelStep > 0 then
              OnShowProgress(Self, Round(100*Pi.X/L))
            else
              OnShowProgress(Self, Round(100*(1 - Pi.X/L)));
          end
          else
            OnShowProgress(Self, Round(100*Abs(Delta)/360));
        end
        else
          OnShowProgress(Self, Round(i/(ML.Count - 1)));
        end;
    end;

    Application.ProcessMessages;
  until (not PlotBox.Contains(Pi)) or
        ((Scale.CoordSystem = csPolar) and (Abs(Delta) > 360)) or
        ((ML.Count > 2) and (i >= ML.Count - 1));

  // Notify the parent that must hide the progress bar
  if Assigned(OnHideProgress) then
    OnHideProgress(Self);

  SortCurve;

  IsChanged := True;
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
      Pi := LeftMarker.Position
    else
      Pi := RightMarker.Position;
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
          (PlotBox.Contains(Pi)) do
      Pi := Pi + Sign(DigitCurve.Step)*Scale.Nx(Pi);
  end;


  DigitizeSpectrum(Pi, False);
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
      Curve.AddPoint(Markers[i].Position);

    Curve.SortCurve;

    IsChanged := True;
  end;
end;

procedure TPlotImage.FillIsland(Pi: TCurvePoint; var Island: TIsland;
                                JustInY: Boolean = True;
                                MaxPoints: Integer = 1000;
                                FillCurvePoints: Boolean = True);
begin
  // Make sure that the program doesn't freezes due to bad configuration
  // We want an island, not a continent ;-)
  if (Island.Count < MaxPoints) then
  begin
    if FillCurvePoints then
      FindCurvePoints;

    if (not Island.Contains(Pi)) and AllCurvePoints.Contains(Pi) then
    begin
      Island.AddPoint(Pi);
      FillIsland(Pi - Scale.Ny(Pi), Island, JustInY, MaxPoints, False);
      FillIsland(Pi + Scale.Ny(Pi), Island, JustInY, MaxPoints, False);
      if (not JustInY) then
      begin
        FillIsland(Pi - Scale.Nx(Pi), Island, JustInY, MaxPoints, False);
        FillIsland(Pi + Scale.Nx(Pi), Island, JustInY, MaxPoints, False);
      end;
    end;
  end;
end;

procedure TPlotImage.FillIsland(Xi, Yi: Double; var Island: TIsland;
                                JustInY: Boolean = True;
                                MaxPoints: Integer = 1000);
begin
  FillIsland(GetCurvePoint(Xi, Yi), Island, JustInY, MaxPoints);
end;

procedure TPlotImage.AdjustCurve;
var
  i: Integer;
  Pi: TCurvePoint;
  NewPoints: TPointList;
  Island: TIsland;
begin
  // Only if there is a curve
  if HasPoints then
  begin
    FindCurvePoints;
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      // Notify the parent that must show the progress bar
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, 0);

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        // Notify the parent that must update the progress bar
        if Assigned(OnShowProgress) then
        begin
          OnShowProgress(Self, Round(100*(i + 1)/Curve.Count));
          Application.ProcessMessages;
        end;

        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          Island.Clear;
          FillIsland(Pi, Island, True, 1000, False);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue)
          else
            NewPoints.Add(Pi);
        end;
      end;

      DigitCurve.NextCurve(False);
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      // Notify the parent that must hide the progress bar
      if Assigned(OnHideProgress) then
        OnHideProgress(Self);
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;
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
    FindCurvePoints;
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      // Notify the parent that must show the progress bar
      if Assigned(OnShowProgress) then
        OnShowProgress(Self, 0);

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        // Notify the parent that must update the progress bar
        if Assigned(OnShowProgress) then
        begin
          OnShowProgress(Self, Round(100*(i + 1)/Curve.Count));
          Application.ProcessMessages;
        end;

        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          Island.Clear;
          FillIsland(Pi, Island, False, 1000, False);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue);
        end;
      end;

      DigitCurve.NextCurve(False);
      Curve.ShowAsSymbols := True;
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      // Notify the parent that must hide the progress bar
      if Assigned(OnHideProgress) then
        OnHideProgress(Self);
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;
  end;
end;

function TPlotImage.GetPixel(X, Y: Integer): LongInt;
var
  c: TBGRAPixel;
begin
    if GridMask.IsValid and GridMask.IsActive then
    begin
      c := GridMask.Mask.GetPixel(X, Y);

      if (c = BGRAPixelTransparent) then
        c := PlotImg.GetPixel(X, Y);
    end
    else
      c := PlotImg.GetPixel(X, Y);

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
  Result := PlotBox.Vertex[Index - 1];
end;

function TPlotImage.GetAxesMarkers(Index: Integer): TMarker;
begin
  if (Index >= 1) and (Index <= 3) and Assigned(FAxesMarkers[Index]) then
    Result := FAxesMarkers[Index]
  else
    Result := nil;
end;

function TPlotImage.GetBoxMarkers(Index: Integer): TMarker;
begin
  if (Index >= 1) and (Index <= 4) and Assigned(FBoxMarkers[Index]) then
    Result := FBoxMarkers[Index]
  else
    Result := nil;
end;

procedure TPlotImage.SetAxesPoint(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 1) and (Index <= 3) and (Scale.ImagePoint[Index] <> Value) then
  begin
    if Assigned(FAxesMarkers[Index]) then
      FAxesMarkers[Index].Position := Value;

    Scale.ImagePoint[Index] := Value;

    IsChanged := True;
  end;
end;

procedure TPlotImage.SetBoxVertex(Index: Integer; const Value: TCurvePoint);
begin
  if (Index >= 1) and (Index <= 4) and (PlotBox.Vertex[Index - 1] <> Value) then
  begin
    if Assigned(FBoxMarkers[Index]) then
      FBoxMarkers[Index].Position := Value;

    PlotBox.Vertex[Index - 1] := Value;

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
      FMarkerList.AddPoint(Scale.FromImgToPlot(Markers[i].Position));

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
  IsChanged := True;
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

    for i := 1 to PlotBox.NumVertices do
      if (FBoxMarkers[i] = Marker) then
        FBoxMarkers[i] := nil;

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
begin
  if Assigned(ActiveMarker) then
  begin
    UpdateMarker(ActiveMarker);
    ActiveMarker.Shift(Delta);
    UpdateMarker(ActiveMarker);

    for i := 1 to 3 do
      if (ActiveMarker = AxesMarkers[i]) then
        Scale.ImagePoint[i] := AxesMarkers[i].Position;

    for i := 1 to PlotBox.NumVertices do
    begin
      UpdateRegion(PlotBox.Rect);
      if (ActiveMarker = BoxMarkers[i]) then
        PlotBox.Vertex[i - 1] := BoxMarkers[i].Position;
      UpdateRegion(PlotBox.Rect);
    end;

    IsChanged := True;
  end;
end;

procedure TPlotImage.RedrawMarkers;
begin
  UpdateMarkersInImage;
end;

procedure TPlotImage.Paint;
var
  PolyColor: TBGRAPixel;
  Rect, ClipRect, PaintRect: TRect;
  i: Integer;
  {$ifdef windows}
  ClipRgn: HRGN;
  {$endif}
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
    CopyRect(PaintRect, PlotImg.Bitmap.Canvas, PaintRect);

    if GridMask.IsActive then
      CopyRect(PaintRect, GridMask.Mask.Canvas, PaintRect);
  end;

  if HasPoints and (State = piSetCurve) then
    DigitCurve.Draw(WhiteBoard.Canvas);

  if (State = piSetPlotBox) and
     not (PlotBox.Rect*PaintRect).IsEmpty then
  begin
    PolyColor := clBlue;
    PolyColor.alpha := 80;
    PlotBox.PolarCoordinates := (Scale.CoordSystem = csPolar);
    WhiteBoard.DrawPolygonAntialias(PlotBox.PolygonPoints, BGRABlack, 1, PolyColor);
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
    Canvas.CopyRect(Rect, PlotImg.Canvas, Rect);
    if GridMask.IsActive then
      Canvas.CopyRect(Rect, GridMask.Mask.Canvas, Rect);

    Rect := ClientRect;
    Rect.Top := WhiteBoard.Height;
    Rect.Width := WhiteBoard.Width;
    Rect.Intersect(ClipRect);
    Canvas.CopyRect(Rect, PlotImg.Canvas, Rect);
    if GridMask.IsActive then
      Canvas.CopyRect(Rect, GridMask.Mask.Canvas, Rect);
  end;
end;

procedure TPlotImage.Resize;
begin
  Invalidate;
  inherited Resize;
end;

procedure TPlotImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Marker, HitMarker: TMarker;
begin
  inherited MouseDown(Button, Shift, X, Y);
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

    // No marker under cursor, we are selecting a region
    if (HitMarker = nil) and (State = piSetCurve) then
    begin
      SelectingRegion := True;
      FSelectionRect := TRect.Create(X, Y, X, Y);
    end;
  end;
end;

procedure TPlotImage.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  xm: Array [1..4] of Integer = (4, 3, 2, 1);
  ym: Array [1..4] of Integer = (2, 1, 4, 3);
  //
  // 1 |--------------| 2
  //   |              |
  //   |              |
  //   |              |
  //   |              |
  // 4 |--------------| 3
  //
var
  i: Integer;
  Marker, HitMarker: TMarker;
  TmpRect: TRect;
  Newpos: TPoint;
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
      if (State = piSetPlotBox) then
        UpdateRegion(PlotBox.Rect);

      if ClientRect.Contains(MouseMovePos) then
        NewPos := FClickedCoord + MouseMovePos - FClickedPoint
      else
        NewPos := FClickedCoord;  //The mouse moves out of the image

      FClickedMarker.Move(NewPos);

      if (State = piSetPlotBox) then
      begin
        for i := 1 to PlotBox.NumVertices do
          if (FClickedMarker = BoxMarkers[i]) then
          begin
            PlotBox.Vertex[i - 1] := NewPos;
            if (ssShift in Shift) then
            begin
              BoxMarkers[xm[i]].Move(GetCurvePoint(NewPos.X, BoxMarkers[xm[i]].Position.Y));
              BoxMarkers[ym[i]].Move(GetCurvePoint(BoxMarkers[ym[i]].Position.X, NewPos.Y));

              PlotBox.Vertex[xm[i] - 1] := BoxMarkers[xm[i]].Position;
              PlotBox.Vertex[ym[i] - 1] := BoxMarkers[ym[i]].Position;
            end;
          end;
      end;

      UpdateMarker(FClickedMarker);
      if (State = piSetPlotBox) then
        UpdateRegion(PlotBox.Rect);
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
      UnionRect(TmpRect, TmpRect, FSelectionRect);
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
const
  xm: Array [1..4] of Integer = (4, 3, 2, 1);
  ym: Array [1..4] of Integer = (2, 1, 4, 3);
  //
  // 1 |--------------| 2
  //   |              |
  //   |              |
  //   |              |
  //   |              |
  // 4 |--------------| 3
  //
var
  i: Integer;
begin
  if not Dragging then
  begin
    if Button = mbLeft then
    begin
      if SelectingRegion and (State = piSetCurve) then
      begin
        FSelectionRect.Width := X - FSelectionRect.Left;
        FSelectionRect.Height := Y - FSelectionRect.Top;

        // Notify the parent that the user has selected a region
        if Assigned(OnRegionSelected) then
          OnRegionSelected(Self, FSelectionRect);

        SelectingRegion := False;
        UpdateRegion;
      end;
    end;

    inherited MouseUp(Button, Shift, X, Y);
  end;

  if Button = mbLeft then
  begin
    if Dragging then
    begin
      if (State = piSetScale) and Assigned(FClickedMarker) then
        for i := 1 to 3 do
          if (FClickedMarker = AxesMarkers[i]) then
            Scale.ImagePoint[i] := FClickedMarker.Position;

      Dragging := False;

      // Notify the parent that the marker has been dragged
      if Assigned(OnMarkerDragged) and Assigned(FClickedMarker) then
      begin
        OnMarkerDragged(Self, FClickedMarker);
        // Notify that neighboring markers have also moved
        if (State = piSetPlotBox) and (ssShift in Shift) then
          for i := 1 to PlotBox.NumVertices do
            if (FClickedMarker = BoxMarkers[i]) then
            begin
              OnMarkerDragged(Self, BoxMarkers[xm[i]]);
              OnMarkerDragged(Self, BoxMarkers[ym[i]]);
            end;
      end;

      IsChanged := True;
    end
    else
    begin
      if Assigned(FClickedMarker) then
      begin
        Markers.Move(Markers.IndexOf(FClickedMarker), 0);
        UpdateMarker(FClickedMarker);
      end;
    end;
    FClickedMarker := nil;
  end;
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
  Result := FScale.FromImgToPlot(p);
end;

function TPlotImage.ConvertCoords(X, Y: Double): TCurvePoint;
begin
  Result := ConvertCoords(GetCurvePoint(X, Y));
end;

function TPlotImage.GetCount: Integer;
begin
  Result := FCurves.Count;
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
      DigitCurve.AddMarker(Markers[i].Position);

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

      FAxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clRed, 3),
                                        Scale.ImagePoint[1], True);
      FAxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clGreen, 3),
                                        Scale.ImagePoint[2], True);
      FAxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clRed, 3),
                                        Scale.ImagePoint[3], True);

      AddMarker(AxesMarkers[1], False);
      AddMarker(AxesMarkers[2], False);
      AddMarker(AxesMarkers[3], False);
    end;
    piSetPlotBox: begin
      FBoxMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'1', clBlack, 3),
                                       PlotBox.Vertex[0], True);
      FBoxMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'1', clBlack, 3),
                                       PlotBox.Vertex[1], True);
      FBoxMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'1', clBlack, 3),
                                       PlotBox.Vertex[2], True);
      FBoxMarkers[4] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'1', clBlack, 3),
                                       PlotBox.Vertex[3], True);

      AddMarker(BoxMarkers[1], False);
      AddMarker(BoxMarkers[2], False);
      AddMarker(BoxMarkers[3], False);
      AddMarker(BoxMarkers[4], False);
    end;
    piSetCurve: begin
      for i := 0 to DigitCurve.MarkerCount - 1 do
        AddMarker(DigitCurve.Markers[i], False);
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
    PlotImg.ReplaceTransparent(clWhite);

    WhiteBoard.SetSize(PlotImg.Width, PlotImg.Height);
    WhiteBoard.PutImage(0, 0, PlotImg, dmSet);

    GridMask.SetSize(PlotImg.Width, PlotImg.Height);

    Width := PlotImg.Width;
    Height := PlotImg.Height;

    FImageIsLoaded := True;

    GridMask.IsValid := False;
    GridMask.IsActive := False;

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

procedure TPlotImage.Interpolate(n: Integer; Index: Integer);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(n);

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

procedure TPlotImage.Interpolate(n: Integer; AllCurves: Boolean = False);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(n, CurveIndex)
  else
    for i := 0 to Count - 1 do
      Interpolate(n, i);
end;

procedure TPlotImage.Interpolate(Xo, Xf: Double; n: Integer; Index: Integer);
var
  i: Integer;
  TmpCurve: TCurve;
begin
  try
    TmpCurve := PlotCurves[Index];

    TmpCurve.SortCurve;
    TmpCurve.Interpolate(Xo, Xf, n);

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

procedure TPlotImage.Interpolate(Xo, Xf: Double; n: Integer; AllCurves: Boolean = False);
var
  i: Integer;
begin
  if not AllCurves then
    Interpolate(Xo, Xf, n, CurveIndex)
  else
    for i := 0 to Count - 1 do
      Interpolate(Xo, Xf, n, i);
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

    PlotBox.PolarCoordinates := (Scale.CoordSystem = csPolar);

    if Scale.CoordSystem = csCartesian then
      GridMask.RemoveCartesianGrid(PlotImg, PlotBox)
    else
      GridMask.RemovePolarGrid(PlotImg, PlotBox, Scale.ImagePoint[2]);

    if GridMask.FixCurve then
    begin
      for i := 0 to FCurves.Count - 1 do
        GridMask.RebuildCurve(PlotImg, PlotBox, FCurves[i].Color);
    end;
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
      CopyRect(Marker.Rect, PlotImg, Marker.Rect);

      if GridMask.IsActive then
        CopyRect(Marker.Rect, GridMask.Mask, Marker.Rect);
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

function TPlotImage.GetZoomImage(w, h: Integer; Region: TRect): TBitmap;
var
  ZoomImg: TBGRABitmap;
begin
  try
    Result := TBitmap.Create;
    Result.SetSize(w, h);
    Result.PixelFormat := pf24bit;

    ZoomImg := TBGRABitmap.Create(Region.Width, Region.Height, clWhite);
    ZoomImg.CanvasBGRA.CopyRect(TRect.Create(0, 0, Region.Width, Region.Height),
                                WhiteBoard, Region);

    BGRAReplace(ZoomImg, ZoomImg.Resample(w, h, rmSimpleStretch));
    ZoomImg.Draw(Result.Canvas, 0, 0, True);
  finally
    ZoomImg.Free;
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
    TDOMElement(RootNode).SetAttribute('version', '1.0');
    XMLDoc.Appendchild(RootNode); // Save root node

    // Create document node
    RootNode:= XMLDoc.DocumentElement;
    DigitNode := XMLDoc.CreateElement('document');
    // Create atributes to document node
    with TDOMElement(DigitNode) do
    begin
      SetAttribute('ImageIsLoaded', UTF8Decode(BoolToStr(ImageIsLoaded, True)));
    end;

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
    DigitNode.AppendChild(PlotBox.ExportToXML(XMLDoc));

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
      CurveNode.Appendchild(Curves[i].ExportToXML(XMLDoc));

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
  PlotBoxLoaded := False;

  Result := False;
  try
    // Read the XML document
    ReadXMLFile(XMLDoc, FileName);
    // Create the stream to save the image
    Stream := TMemoryStream.Create;

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
                Path := UTF8Encode(ImageChild.FirstChild.NodeValue);

              if (ImageChild.CompareName('name') = 0) then
                ImgName := UTF8Encode(ImageChild.FirstChild.NodeValue);

              // It is the data image
              if (ImageChild.CompareName('data') = 0) then
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
          if (DigitChild.CompareName('PlotBox') = 0) then
          begin
            PlotBox.ImportFromXML(DigitChild);
            PlotBox.PolarCoordinates := (Scale.CoordSystem = csPolar);
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

    if ImageIsLoaded then
    begin
      if not PlotBoxLoaded then
      begin
        PlotBox.Vertex[0] := GetCurvePoint(0, 0);
        PlotBox.Vertex[1] := GetCurvePoint(0, Height);
        PlotBox.Vertex[2] := GetCurvePoint(Width, Height);
        PlotBox.Vertex[3] := GetCurvePoint(Width, 0);
      end;

      State := piSetCurve;
    end;

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
