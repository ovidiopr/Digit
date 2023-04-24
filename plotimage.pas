unit plotimage;

{$mode objfpc}{$H+}

interface

uses {$ifdef windows}Windows,{$endif} Forms, Classes, Controls, Graphics,
     ExtDlgs, Fgl, ComCtrls, SysUtils, DOM, XMLWrite, XMLRead, math, curves,
     coordinates, Dialogs, Types, Base64, intfgraphics,
     BGRABitmap, BGRABitmapTypes;

type
  TSelectRegionEvent = procedure(Sender: TObject; RegionRect: TRect) of Object;

  TMarker = class
  protected
    FBitmap: TBGRABitmap;
    FRect: TRect;
    FPersistent: Boolean;
  private
    function GetPosition: TCurvePoint;
  public
    constructor Create(Bitmap: TBGRABitmap; Coord: TPoint; Persistent: Boolean = False);
    destructor Destroy; override;

    function HitTest(Point: TPoint): Boolean;
    procedure Draw(Canvas: TCanvas; Rectangle: TRect);
    procedure Move(Point: TPoint);
    procedure Shift(Delta: TPoint);

    property Bitmap: TBGRABitmap read FBitmap;
    property Rect: TRect read FRect;
    property Position: TCurvePoint read GetPosition;
    property IsPersistent: Boolean read FPersistent;
  end;

  TMarkerList = specialize TFPGObjectList<TMarker>;

  TPlotImage = class(TCustomControl)
  protected type
    TCurveList = specialize TFPGObjectList<TDigitCurve>;
  protected
    FOldCursor: TCursor;

    FImageName: TFileName;
    FPlotImg: TBGRABitmap;
    FBlackBoard: TBitmap;
    FGridMask: TBGRABitmap;

    FMarkers: TMarkerList;
    FMarkerList: TCurve;
    FAxesMarkers: Array [1..3] of TMarker;
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
    FValidGrid: Boolean;
    FSubstractGrid: Boolean;

    FIsChanged: Boolean;
    FOnChange: TNotifyEvent;

    FOnRegionSelected: TSelectRegionEvent;

    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  private
    function GetPixel(X, Y: Integer): LongInt; overload;
    function GetPixel(X, Y: Double): LongInt; overload;
    function GetPixel(P: TCurvePoint): LongInt; overload;

    function GetAxesMarkers(Index: Integer): TMarker;

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

    procedure SetMarkerUnderCursor(Marker: TMarker);
    procedure SetActiveMarker(Marker: TMarker);
    procedure SetAxesMarkers(Index: Integer; const Value: TMarker);
    procedure SetImageName(Value: TFileName);
    procedure SetDragging(Value: Boolean);
    procedure SetSelectingRegion(Value: Boolean);

    procedure SetCurveIndex(Value: Integer);
    procedure SetIsChanged(Value: Boolean);
    procedure SetSubstractGrid(Value: Boolean);

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

    function FindNextPoint(var Pv: TCurvePoint; Interval: Integer; ScanX: Boolean = False): Boolean;
    procedure DigitizeSpectrum(Pi: TCurvePoint; ProgressBar: TProgressBar = nil); overload;
    procedure DigitizeSpectrum(ProgressBar: TProgressBar = nil); overload;

    procedure FillIsland(Pi: TCurvePoint; var Island: TIsland; JustInY: Boolean = True; MaxPoints: Integer = 1000); overload;
    procedure FillIsland(Xi, Yi: Double; var Island: TIsland; JustInY: Boolean = True; MaxPoints: Integer = 1000); overload;
    procedure AdjustCurve(ProgressBar: TProgressBar = nil);
    procedure ConvertCurveToSymbols(ProgressBar: TProgressBar = nil);

    function ConvertCoords(p: TCurvePoint): TCurvePoint; overload;
    function ConvertCoords(X, Y: Double): TCurvePoint; overload;

    procedure AddMarker(Position: TPoint); overload;
    procedure AddMarker(Marker: TMarker); overload;
    procedure UpdateMarker(Marker: TMarker);
    procedure DeleteMarker(Marker: TMarker); overload;
    procedure DeleteMarker(Index: Integer); overload;
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
                         Tolerance: Integer = 10; Threshold: Double = 0.5);
    procedure GroupPoints(Region: TRect);
    procedure DeletePoints(Region: TRect);
    procedure MoveCurveUp;
    procedure MoveCurveDown;
    procedure MoveCurveLeft;
    procedure MoveCurveRight;

    procedure MoveMarker(Marker: TMarker; Point: TPoint); overload;
    procedure MoveMarker(Marker: TMarker; X, Y: Double); overload;

    function SaveToXML(FileName: TFileName): Boolean;
    function LoadFromXML(FileName: TFileName; PictureDlg: TOpenPictureDialog = nil): Boolean;

    property ImageName: TFileName read FImageName write SetImageName;
    property GridMask: TBGRABitmap read FGridMask;
    property BlackBoard: TBitmap read FBlackBoard;
    property PlotImg: TBGRABitmap read FPlotImg;
    property Markers: TMarkerList read FMarkers;
    property AxesMarkers[Index: Integer]: TMarker read GetAxesMarkers write SetAxesMarkers;
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
    property ValidGrid: Boolean read FValidGrid write FValidGrid;
    property SubstractGrid: Boolean read FSubstractGrid write SetSubstractGrid;

    property Scale: TScale read FScale write FScale;
    property ColorIsSet: Boolean read GetColorIsSet;
    property HasPoints: Boolean read GetHasPoints;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRegionSelected: TSelectRegionEvent read FOnRegionSelected write FOnRegionSelected;
  end;

function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor; LineWith: Integer = 3): TBGRABitmap;

implementation

function AreSimilarColors(C1, C2: LongInt; Tolerance: Byte): Boolean;
begin
  //Check for the trivial case
  if C1 = C2 then
    Result := True
  else
    Result := (Abs(Red(C1) - Red(C2)) +
               Abs(Green(C1) - Green(C2)) +
               Abs(Blue(C1) - Blue(C2)) <= 3*Tolerance);
end;

function TMarkerComparator(const a, b: TMarker): Integer;
begin
  Result := Sign(a.Position.X - b.Position.X);
end;

//==============================|Bitmaps|=====================================//
function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor; LineWith: Integer = 3): TBGRABitmap;
begin
  // Make sure that the marker is not transparent
  if (color = clBlack) then
    color := RGBToColor(1, 1, 1);

  Result := TBGRABitmap.Create(Size.X, Size.Y, clBlack);

  with Result do
  begin
    FillTransparent;
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
      'c', 'C': begin
        EllipseAntialias(Width div 2, Height div 2,
                         (Width - LineWith) div 2, (Height - LineWith) div 2,
                         Color, LineWith, Color);
      end;
      'o', 'O': begin
        EllipseAntialias(Width div 2, Height div 2,
                         (Width - LineWith) div 2, (Height - LineWith) div 2,
                         Color, LineWith);
      end;
      'r', 'R': begin
        RectangleAntialias(1, 1, Width - 2, Height - 2, Color, LineWith, Color);
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

procedure TMarker.Draw(Canvas: TCanvas; Rectangle: TRect);
begin
  if not (Rectangle*Rect).IsEmpty then
    Canvas.Draw(Rect.Left, Rect.Top, Bitmap.Bitmap);
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
//==============================|TMarker|=====================================//

//=============================|TPlotImage|===================================//
constructor TPlotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlotImg := TBGRABitmap.Create;
  FBlackBoard := TBitmap.Create;
  FGridMask := TBGRABitmap.Create;
  FGridMask.FillTransparent;

  FMarkers := TMarkerList.Create;
  FMarkerList := TCurve.Create;

  FCurves := TCurveList.Create;
  FScale := TScale.Create;

  Reset;
end;

destructor TPlotImage.Destroy;
begin
  FGridMask.Free;
  FBlackBoard.Free;
  FPlotImg.Free;

  FMarkers.Free;
  FMarkerList.Free;

  FCurves.Free;
  FScale.Free;

  inherited Destroy;
end;

procedure TPlotImage.Reset;
begin
  FCurves.Clear;
  FCurves.Add(TDigitCurve.Create('Curve1'));

  FCurveIndex := 0;

  FScale.Reset;

  FOldCursor := Cursor;

  FImageIsLoaded := False;
  FValidGrid := False;
  FSubstractGrid := False;

  FIsChanged := False;
end;


function TPlotImage.FindNextPoint(var Pv: TCurvePoint; Interval: Integer; ScanX: Boolean = False): Boolean;
var
  // Maximum difference allowed (in shadows of grey)
  Tolerance: Integer;
  // Number of pixels that the line is expected to spread
  Spread: Integer;

  P: TCurvePoint;
  dX, dY: Integer;
  C1, C2: LongInt;
  NoPnts: Integer;
  FoundUp, FoundDown: Boolean;

function CheckPlotPoint(const Pi: TCurvePoint; var P: TCurvePoint; var nP: Integer): Boolean;
begin
  Result := False;
  if GetClientRect.Contains(Pi) then
  begin
    // Point to the pixel location
    C2 := GetPixel(Pi);
    if AreSimilarColors(C1, C2, Tolerance) then
    begin
      inc(nP);
      P := P + Pi;
      Result := True;
    end;
  end;
end;

begin
  Tolerance := DigitCurve.Tolerance;
  Spread := DigitCurve.Spread;

  C1 := ColorToRGB(DigitCurve.Color);

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
        ((NoPnts > 0) and (not FoundUp) and ( not FoundDown));

  if (NoPnts > 0) then
  begin
    Result := True;
    Pv := P/NoPnts;
  end;
end;

procedure TPlotImage.DigitizeSpectrum(Pi: TCurvePoint; ProgressBar: TProgressBar = nil);
var
  i : Integer;
  Delta, L, mx, my: Double;
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
  PixelStep := DigitCurve.Step;
  Interval := DigitCurve.Interval;

  if assigned(ProgressBar) then
  begin
    ProgressBar.Visible := True;
    ProgressBar.Position := 0;
  end;

  DigitCurve.NextCurve(False);
  Curve.AddPoint(Pi);

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

    if assigned(ProgressBar) then
    begin
      case ML.Count of
        0..2: begin
          if (Scale.CoordSystem = csCartesian) then
          begin
            if PixelStep > 0 then
              ProgressBar.Position := Round(100*Pi.X/L)
            else
              ProgressBar.Position := Round(100*(1 - Pi.X/L));
          end
          else
            ProgressBar.Position := Round(100*Abs(Delta)/360);
        end
        else
          ProgressBar.Position := Round(i/(ML.Count - 1));
        end;
      Application.ProcessMessages;
    end;
  until ((Scale.CoordSystem = csCartesian) and (not ClientRect.Contains(Pi))) or
        ((Scale.CoordSystem = csPolar) and (Abs(Delta) > 360)) or
        ((ML.Count > 2) and (i >= ML.Count - 1));

  if assigned(ProgressBar) then
    ProgressBar.Visible := False;

  SortCurve;

  IsChanged := True;
end;

procedure TPlotImage.DigitizeSpectrum(ProgressBar: TProgressBar = nil);
var
  Pi : TCurvePoint;
begin
  // Estimate the first point
  if (Markers.Count > 3) then
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
          (ClientRect.Contains(Pi)) do
      Pi := Pi + Sign(DigitCurve.Step)*Scale.Nx(Pi);
  end;


  DigitizeSpectrum(Pi, ProgressBar);
end;

procedure TPlotImage.FillIsland(Pi: TCurvePoint; var Island: TIsland; JustInY: Boolean = True; MaxPoints: Integer = 1000);
var
  // Maximum difference allowed (in shadows of grey)
  Tolerance: Integer;
  C1, C2: LongInt;
begin
  // Make sure that the program doesn't freezes due to bad configuration
  // We want an island, not a continent ;-)
  if (Island.Count < MaxPoints) then
  begin
    Tolerance := DigitCurve.Tolerance;

    C1 := ColorToRGB(DigitCurve.Color);
    C2 := GetPixel(Pi);
    if (not Island.Contains(Pi)) and AreSimilarColors(C1, C2, Tolerance) then
    begin
      Island.AddPoint(Pi);
      FillIsland(Pi - Scale.Ny(Pi), Island, JustInY);
      FillIsland(Pi + Scale.Ny(Pi), Island, JustInY);
      if (not JustInY) then
      begin
        FillIsland(Pi - Scale.Nx(Pi), Island, JustInY);
        FillIsland(Pi + Scale.Nx(Pi), Island, JustInY);
      end;
    end;
  end;
end;

procedure TPlotImage.FillIsland(Xi, Yi: Double; var Island: TIsland; JustInY: Boolean = True; MaxPoints: Integer = 1000);
begin
  FillIsland(GetCurvePoint(Xi, Yi), Island, JustInY);
end;

procedure TPlotImage.AdjustCurve(ProgressBar: TProgressBar = nil);
var
  i: Integer;
  Pi, Pv: TCurvePoint;
  NewPoints: TPointList;
  Island: TIsland;
begin
  // Only if there is a curve
  if HasPoints then
  begin
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      if assigned(ProgressBar) then
      begin
        ProgressBar.Visible := True;
        ProgressBar.Position := 0;
      end;

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        if assigned(ProgressBar) then
        begin
          ProgressBar.Position := Round(100*(i + 1)/Curve.Count);
          Application.ProcessMessages;
        end;

        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          Island.Clear;
          FillIsland(Pi, Island, True);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue)
          else
            NewPoints.Add(Pi);
        end;
      end;

      DigitCurve.NextCurve(False);
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      if assigned(ProgressBar) then
        ProgressBar.Visible := False;
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;
  end;
end;

procedure TPlotImage.ConvertCurveToSymbols(ProgressBar: TProgressBar = nil);
var
  i: Integer;
  Pi: TCurvePoint;
  NewPoints: TPointList;
  Island: TIsland;
begin
  // Only if there is a curve
  if HasPoints then
  begin
    try
      NewPoints := TPointList.Create;
      NewPoints.Clear;

      if assigned(ProgressBar) then
      begin
        ProgressBar.Visible := True;
        ProgressBar.Position := 0;
      end;

      Island := TIsland.Create;
      Island.Clear;
      for i := 0 to Curve.Count - 1 do
      begin
        if assigned(ProgressBar) then
        begin
          ProgressBar.Position := Round(100*(i + 1)/Curve.Count);
          Application.ProcessMessages;
        end;

        Pi := Curve.Point[i];
        if (not Island.Contains(Pi)) then
        begin
          Island.Clear;
          FillIsland(Pi, Island, False);
          if (Island.Count > 0) then
            NewPoints.Add(Island.MeanValue);
        end;
      end;

      DigitCurve.NextCurve(False);
      Curve.ShowAsSymbols := True;
      for i := 0 to NewPoints.Count - 1 do
        Curve.AddPoint(NewPoints[i]);

      if assigned(ProgressBar) then
        ProgressBar.Visible := False;
    finally
      NewPoints.Free;
      Island.Free;

      IsChanged := True;
    end;
  end;
end;




function TPlotImage.GetPixel(X, Y: Integer): LongInt;
begin
    if ValidGrid and SubstractGrid then
      Result := ColorToRGB(GridMask.GetPixel(X, Y))
    else
      Result := 0;

    if (Result = 0) then
      Result := ColorToRGB(PlotImg.GetPixel(X, Y));
end;

function TPlotImage.GetPixel(X, Y: Double): LongInt;
begin
  Result := GetPixel(Round(X), Round(Y));
end;

function TPlotImage.GetPixel(P: TCurvePoint): LongInt;
begin
  Result := GetPixel(P.X, P.Y);
end;

function TPlotImage.GetAxesMarkers(Index: Integer): TMarker;
begin
  if (Index >= 1) and (Index <= 3) then
    Result := FAxesMarkers[Index]
  else
    Result := nil;
end;

procedure TPlotImage.SetAxesMarkers(Index: Integer; const Value: TMarker);
begin
  if (Index >= 1) and (Index <= 3) then
  begin
    if assigned(FAxesMarkers[Index]) then
      DeleteMarker(FAxesMarkers[Index]);

    FAxesMarkers[Index] := Value;
    AddMarker(FAxesMarkers[Index]);

    Scale.ImagePoint[Index] := FAxesMarkers[Index].Position;

    IsChanged := True;
  end;
end;

function TPlotImage.GetLeftMarker: TMarker;
var
  i: Integer;
  X: Double;
begin
  X := BlackBoard.Width;
  Result := nil;
  for i := 0 to Markers.Count - 1 do
    if (not Markers[i].IsPersistent) and (X > Markers[i].Position.X) then
    begin
      X := Markers[i].Position.X;
      Result := Markers[i];
    end;
end;

function TPlotImage.GetRightMarker: TMarker;
var
  i: Integer;
  X: Double;
begin
  X := 0;
  Result := nil;
  for i := 0 to Markers.Count - 1 do
    if (not Markers[i].IsPersistent) and (X < Markers[i].Position.X) then
    begin
      X := Markers[i].Position.X;
      Result := Markers[i];
    end;
end;

function TPlotImage.GetTopMarker: TMarker;
var
  i: Integer;
  Y: Double;
begin
  Y := BlackBoard.Height;
  Result := nil;
  for i := 0 to Markers.Count - 1 do
    if (not Markers[i].IsPersistent) and (Y > Markers[i].Position.Y) then
    begin
      Y := Markers[i].Position.Y;
      Result := Markers[i];
    end;
end;

function TPlotImage.GetBottomMarker: TMarker;
var
  i: Integer;
  Y: Double;
begin
  Y := 0;
  Result := nil;
  for i := 0 to Markers.Count - 1 do
    if (not Markers[i].IsPersistent) and (Y < Markers[i].Position.Y) then
    begin
      Y := Markers[i].Position.Y;
      Result := Markers[i];
    end;
end;

function TPlotImage.GetMarkerList: TCurve;
var
  i: Integer;
begin
  FMarkerList.Clear;

  for i := 0 to Markers.Count - 1 do
    if (not Markers[i].IsPersistent) then
      FMarkerList.AddPoint(Scale.FromImgToPlot(Markers[i].Position));
  FMarkerList.SortCurve;

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

procedure TPlotImage.AddMarker(Position: TPoint);
var
  BMP: TBGRABitmap;
begin
  BMP := CreateMarker(TPoint.Create(13, 13), 'x', DigitCurve.Color, 3);
  AddMarker(TMarker.Create(BMP, Position, False));
end;

procedure TPlotImage.AddMarker(Marker: TMarker);
begin
  Markers.Insert(0, Marker);
  ActiveMarker := Marker;
  UpdateMarker(Marker);

  IsChanged := True;
end;

procedure TPlotImage.UpdateMarker(Marker: TMarker);
begin
  {$ifdef windows}
  InvalidateRect(Handle, Marker.Rect, False);
  {$else}
  Invalidate;
  {$endif}
end;

procedure TPlotImage.DeleteMarker(Marker: TMarker);
var
  i: Integer;
begin
  if assigned(Marker) and (not Marker.IsPersistent) then
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

    Markers.Remove(Marker);

    if (not assigned(ActiveMarker)) and (Markers.Count > 0) then
      ActiveMarker := Markers[0];

    IsChanged := True;
  end;
end;

procedure TPlotImage.DeleteMarker(Index: Integer);
begin
  if (Index >= 0) and (Index < Markers.Count) then
    DeleteMarker(Markers[Index]);
end;

procedure TPlotImage.DeleteMarkerUnderCursor;
begin
  DeleteMarker(MarkerUnderCursor);
end;

procedure TPlotImage.DeleteActiveMarker;
begin
  DeleteMarker(ActiveMarker);
end;

procedure TPlotImage.ShiftActiveMarker(Delta: TPoint);
var
  i: Integer;
begin
  if assigned(ActiveMarker) then
  begin
    UpdateMarker(ActiveMarker);
    ActiveMarker.Shift(Delta);
    UpdateMarker(ActiveMarker);

    for i := 1 to 3 do
      if (ActiveMarker = AxesMarkers[i]) then
        Scale.ImagePoint[i] := AxesMarkers[i].Position;

    IsChanged := True;
  end;
end;

procedure TPlotImage.RedrawMarkers;
begin
  UpdateMarkersInImage;
end;

procedure TPlotImage.Paint;
var
  Rect, ClipRect, PaintRect: TRect;
  i: Integer;
  {$ifdef windows}
  ClipRgn: HRGN;
  {$endif}
begin
  inherited Paint;
  Rect := TRect.Create(TPoint.Create(0, 0), BlackBoard.Width, BlackBoard.Height);
  ClipRect := Canvas.ClipRect;
  PaintRect := Rect*ClipRect;
  with BlackBoard.Canvas do
  begin
    {$ifdef windows}
    ClipRgn := CreateRectRgn(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom);
    SelectClipRgn(Handle, ClipRgn);
    {$endif}
    CopyMode:= cmSrcCopy;
    CopyRect(PaintRect, PlotImg.Canvas, PaintRect);

    if SubstractGrid then
      //CopyRect(PaintRect, GridMask.Canvas, PaintRect);
      Draw(0, 0, GridMask.Bitmap);
  end;

  if HasPoints then
    DigitCurve.Draw(BlackBoard.Canvas);

  for i := Markers.Count - 1 downto 0 do
    Markers[i].Draw(BlackBoard.Canvas, PaintRect);

  if Assigned(ActiveMarker) and
     not (ActiveMarker.Rect*PaintRect).IsEmpty then
  begin
    with BlackBoard.Canvas do
      DrawFocusRect(ActiveMarker.Rect);
  end;

  if Assigned(MarkerUnderCursor) and
     (MarkerUnderCursor <> ActiveMarker) and
     not (MarkerUnderCursor.Rect*PaintRect).IsEmpty then
  begin
    with BlackBoard.Canvas do
      DrawFocusRect(MarkerUnderCursor.Rect);
  end;

  if SelectingRegion then
    BlackBoard.Canvas.DrawFocusRect(SelectionRect);

  {$ifdef windows}
  with BlackBoard.Canvas do
  begin
    SelectClipRgn(Handle, 0);
    DeleteObject(ClipRgn);
  end;
  {$endif}
  Canvas.CopyRect(PaintRect, BlackBoard.Canvas, PaintRect);
  if not PaintRect.Contains(ClipRect) then
  begin
    Rect := ClientRect;
    Rect.Left := BlackBoard.Width;
    Rect.Intersect(ClipRect);
    Canvas.CopyRect(Rect, PlotImg.Canvas, Rect);

    Rect := ClientRect;
    Rect.Top := BlackBoard.Height;
    Rect.Width := BlackBoard.Width;
    Rect.Intersect(ClipRect);
    Canvas.CopyRect(Rect, PlotImg.Canvas, Rect);
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
    if (HitMarker = nil) then
    begin
      SelectingRegion := True;
      FSelectionRect := TRect.Create(X, Y, X, Y);
    end;
  end;
end;

procedure TPlotImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Marker, HitMarker: TMarker;
  TmpRect: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(FClickedMarker) then
  begin
    if not Dragging then
      if (Abs(X - FClickedPoint.X) > 5) or (Abs(Y - FClickedPoint.Y) > 5) then
        Dragging := True;

    if Dragging then
    begin
      UpdateMarker(FClickedMarker);

      if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
        FClickedMarker.Move(FClickedCoord + TPoint.Create(X, Y) - FClickedPoint)
      else //The mouse moves out of the image
        FClickedMarker.Move(FClickedCoord);

      UpdateMarker(FClickedMarker);
      MarkerUnderCursor := FClickedMarker;
      Exit;
    end;
  end
  else
    if SelectingRegion then
    begin
      TmpRect := FSelectionRect;
      FSelectionRect.Width := X - FSelectionRect.Left;
      FSelectionRect.Height := Y - FSelectionRect.Top;

      //Refresh the rectangle containing the old and new selection
      UnionRect(TmpRect, TmpRect, FSelectionRect);
      UpdateRegion(TmpRect);
    end;

  HitMarker := nil;
  for i := 0 to Markers.Count - 1 do
  begin
    Marker := Markers[i];
    if Marker.HitTest(TPoint.Create(X, Y)) then
      HitMarker := Marker;
  end;
  MarkerUnderCursor := HitMarker;
end;

procedure TPlotImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if not Dragging then
  begin
    if Button = mbLeft then
    begin
      if SelectingRegion then
      begin
        FSelectionRect.Width := X - FSelectionRect.Left;
        FSelectionRect.Height := Y - FSelectionRect.Top;

        // Notify the parent that the user has selected a region
        if assigned(OnRegionSelected) then
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
      for i := 1 to 3 do
        if (FClickedMarker = AxesMarkers[i]) then
          Scale.ImagePoint[i] := FClickedMarker.Position;
      Dragging := False;

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

procedure TPlotImage.SetSubstractGrid(Value: Boolean);
begin
  if FSubstractGrid <> Value then
  begin
    FSubstractGrid := Value;
    Invalidate;
  end;
end;

procedure TPlotImage.UpdateMarkersInCurve;
var
  i: Integer;
begin
  DigitCurve.ClearMarkers;
  for i := Markers.Count - 1 downto 0 do
    if (not Markers[i].IsPersistent) then
      DigitCurve.AddMarker(Markers[i].Position);

  DigitCurve.SortMarkers;

  IsChanged := True;
end;

procedure TPlotImage.UpdateMarkersInImage;
var
  i: Integer;
begin
  for i := Markers.Count - 1 downto 0 do
    if (not Markers[i].IsPersistent) then
      DeleteMarker(i);

  for i := 0 to DigitCurve.MarkerCount - 1 do
    AddMarker(DigitCurve.Markers[i]);
end;

procedure TPlotImage.UpdateRegion(UpdateArea: TRect);
begin
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
  TmpPic: TPicture;
  Stream: TMemoryStream;
begin
  FImageIsLoaded := False;

  if FileExists(FileName) then
  begin
    try
      Stream := TMemoryStream.Create;

      TmpPic := TPicture.Create;
      TmpPic.LoadFromFile(FileName);

      Stream.Clear;
      Stream.Position := 0;
      TmpPic.SaveToStream(Stream);

      LoadImage(Stream);
    finally
      TmpPic.Free;
      Stream.Free;
    end;
  end
  else
  begin
    BlackBoard.FreeImage;
    Width := 0;
    Height := 0;
  end;
end;

procedure TPlotImage.LoadImage(Stream: TStream);
var
  TmpPic: TPicture;
begin
  FImageIsLoaded := False;

  try
    Stream.Position := 0;
    TmpPic := TPicture.Create;
    TmpPic.LoadFromStream(Stream);

    with PlotImg do
    begin
      // This trick is required because the TPicture has problems updating
      // .JPG and .TIFF images when they are loaded directly. Moreover,
      // the digitization becomes painfully slow in this case.
      // Ovidio (2021/04/08)
      SetSize(TmpPic.Width, TmpPic.Height);
      // Clean the Canvas (in case there was loaded a previous image)
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      Canvas.Rectangle(0, 0, Width, Height);
      //Now draw the image
      Canvas.Draw(0, 0, TmpPic.Bitmap);
    end;

    BlackBoard.SetSize(PlotImg.Width, PlotImg.Height);

    GridMask.SetSize(PlotImg.Width, PlotImg.Height);
    GridMask.FillTransparent;

    Width := BlackBoard.Width;
    Height := BlackBoard.Height;

    FImageIsLoaded := True;
    FValidGrid := False;
    FSubstractGrid := False;

    FMarkers.Clear;
    Invalidate;
  finally
    TmpPic.Free;

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
  TmpCurve := PlotCurves[Index];
  TmpCurve.SortCurve;

  FCurves[Index].Curve.Clear;
  for i := 0 to TmpCurve.Count - 1 do
    FCurves[Index].Curve.AddPoint(Scale.FromPlotToImg(TmpCurve.Point[i]));

  IsChanged := True;
end;

procedure TPlotImage.Smooth(k, d: Integer; Index: Integer);
var
  i: Integer;
  TmpCurve: TCurve;
begin
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
                                Tolerance: Integer = 10; Threshold: Double = 0.5);
const
  angle_step = 1;
var
  t_idx: Integer;
  i, j: Integer;
  x, y: Double;
  diag_len: Integer;

  img: Array of Array of LongInt;
  accumulator: Array of Array of Integer;

  num_thetas: Integer;
  theta: Double;
  theta_count: Array of Integer;
  sin_theta, cos_theta: Array of Double;

  C1, C2: LongInt;
  rho: Integer;

  hori_count,
  vert_count,
  hori_index,
  vert_index: Integer;
  vert_line,
  hori_line: TStraightLine;
  max_count_hori,
  max_count_vert: Integer;
begin
  C1 := ColorToRGB(LineColor1);
  C2 := ColorToRGB(LineColor2);

  with PlotImg do
  begin
    // Diagonal length of image
    diag_len := Round(Sqrt(Width*Width + Height*Height));

    // Put the canvas values in an array for faster access
    SetLength(img, Width, Height);
    for j := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        img[i, j] := ColorToRGB(GetPixel(i, j));
  end;

  // Cache some reusable values
  num_thetas := 1 + Round(180/angle_step);
  SetLength(sin_theta, num_thetas);
  SetLength(cos_theta, num_thetas);
  SetLength(theta_count, num_thetas);
  for t_idx := Low(sin_theta) to High(sin_theta) do
  begin
    theta := -90.0 + t_idx*angle_step;
    sin_theta[t_idx] := sin(theta*3.14159/180.0);
    cos_theta[t_idx] := cos(theta*3.14159/180.0);
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
  for i := Low(img) to High(img) do
    for j := Low(img[Low(img)]) to High(img[Low(img)]) do
      if AreSimilarColors(C1, img[i, j], Tolerance) or
         AreSimilarColors(C2, img[i, j], Tolerance) then
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

  //Create all the required TStraightLine objects
  try
    hori_line := TStraightLine.Create;
    hori_line.Color := BckgndColor;

    vert_line := TStraightLine.Create;
    vert_line.Color := BckgndColor;

    GridMask.FillTransparent;

    // Reset the status of the mask
    FValidGrid := False;
    FSubstractGrid := False;

    // Now, draw the lines
    for i := Low(accumulator) to High(accumulator) do
    begin
      rho := i - diag_len;
      // Draw horizontal lines
      if (accumulator[i, hori_index] > Threshold*max_count_hori) then
      begin
        hori_line.Clear;

        // Fill the line with all the points that meet the color criteria
        for j := 0 to GridMask.Width - 1 do
        begin
          x := j;
          y := (rho - x*cos_theta[hori_index])/sin_theta[hori_index];

          if (y >= 0) and (Round(y) < GridMask.Height) then
            if AreSimilarColors(C1, img[Round(x), Round(y)], Tolerance) or
               AreSimilarColors(C2, img[Round(x), Round(y)], Tolerance) then
            begin
              hori_line.AddPoint(x, y);
            end;
        end;

        // Draw the line
        hori_line.Draw(GridMask.Canvas);
      end;

      // Draw vertical lines
      if (accumulator[i, vert_index] > Threshold*max_count_vert) then
      begin
        vert_line.Clear;

        // Fill the line with all the points that meet the color criteria
        for j := 0 to GridMask.Height - 1 do
        begin
          y := j;
          x := (rho - y*sin_theta[vert_index])/cos_theta[vert_index];

          if (x >= 0) and (Round(x) < GridMask.Width) then
            if AreSimilarColors(C1, img[Round(x), Round(y)], Tolerance) or
               AreSimilarColors(C2, img[Round(x), Round(y)], Tolerance) then
            begin
              vert_line.AddPoint(x, y);
            end;
        end;

        // Draw the line
        vert_line.Draw(GridMask.Canvas);
      end;
    end;
  finally
    hori_line.Free;
    vert_line.Free;

    ValidGrid := True;
    SubstractGrid := True;

    IsChanged := True;
  end;

  // Release all the dynamic arrays
  SetLength(img, 0);
  SetLength(sin_theta, 0);
  SetLength(cos_theta, 0);
  SetLength(theta_count, 0);
  SetLength(accumulator, 0);
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

procedure TPlotImage.MoveMarker(Marker: TMarker; Point: TPoint);
begin
  if (FMarkers.IndexOf(Marker) > -1) then
  begin
    UpdateMarker(Marker);
    Marker.Move(Point);
    UpdateMarker(Marker);

    IsChanged := True;
  end;
end;

procedure TPlotImage.MoveMarker(Marker: TMarker; X, Y: Double);
begin
  MoveMarker(Marker,TPoint.Create(Round(X), Round(Y)));
end;

function TPlotImage.SaveToXML(FileName: TFileName): Boolean;
var
  i: Integer;
  PNG: TPortableNetworkGraphic;
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
    PNG := TPortableNetworkGraphic.Create;

    // Create a root node
    RootNode := XMLDoc.CreateElement('digitization');
    TDOMElement(RootNode).SetAttribute('version', '1.0');
    XMLDoc.Appendchild(RootNode); // Save root node

    // Create document node
    RootNode:= XMLDoc.DocumentElement;
    DigitNode := XMLDoc.CreateElement('document');
    // Create atributes to document node
    TDOMElement(DigitNode).SetAttribute('ImageIsLoaded', UTF8Decode(BoolToStr(ImageIsLoaded, True)));

    if ImageIsLoaded then
    begin
      ImageNode := XMLDoc.CreateElement('image');
      TDOMElement(ImageNode).SetAttribute('Width', UTF8Decode(IntToStr(Width)));
      TDOMElement(ImageNode).SetAttribute('Height', UTF8Decode(IntToStr(Height)));

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

      PNG.Assign(PlotImg.Bitmap);
      PNG.SaveToStream(Stream);

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
    PNG.Free;

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

    Child := XMLDoc.DocumentElement.FirstChild;
    while Assigned(Child) do
    begin
      if (Child.CompareName('document') = 0) then
      begin
        ImageLoaded := False;
        for i := 0 to Child.Attributes.Length - 1 do
          if (Child.Attributes.Item[i].CompareName('ImageIsLoaded') = 0) then
            ImageLoaded := StrToBool(UTF8Encode(Child.Attributes.Item[i].NodeValue));

        DigitChild := Child.FirstChild;
        while Assigned(DigitChild) do
        begin
          if (DigitChild.CompareName('image') = 0) then
          begin
            w := 0;
            h := 0;
            for i := 0 to DigitChild.Attributes.Length - 1 do
            begin
              if (DigitChild.Attributes.Item[i].CompareName('Width') = 0) then
                w := StrToInt(UTF8Encode(DigitChild.Attributes.Item[i].NodeValue));
              if (DigitChild.Attributes.Item[i].CompareName('Height') = 0) then
                h := StrToInt(UTF8Encode(DigitChild.Attributes.Item[i].NodeValue));
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

          // Go for the next image or scale
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

    if ImageIsLoaded then
    begin
      AxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clRed, 3),
                                       Scale.ImagePoint[1], True);
      AxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clGreen, 3),
                                       Scale.ImagePoint[2], True);
      AxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clRed, 3),
                                       Scale.ImagePoint[3], True);
      UpdateMarkersInImage;
    end;

    FCurveIndex := 0;

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

