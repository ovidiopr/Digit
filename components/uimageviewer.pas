unit uImageViewer;

{$mode objfpc}{$H+}

interface

uses {$ifdef windows}Windows,{$endif} Forms, Classes, Controls, Graphics,
  SysUtils, Types, BGRABitmap, BGRABitmapTypes, BGRAreadTiff,
  uUtils, uCoordinates, uMarker;

const
  ZoomLevels: array [1..17] of Double = (1/10, 1/8, 1/6, 1/5, 1/4, 1/3, 1/2,
                                         2/3, 1, 3/2, 2, 3, 4, 5, 6, 8, 10);

type
  TSelectRegionEvent = procedure(Sender: TObject; RegionRect: TRect) of object;
  TMarkerDraggedEvent = procedure(Sender: TObject; Marker: TMarker; Zoom: Boolean) of object;
  TZoomChangeEvent = procedure(Sender: TObject; Zoom: Double) of object;

  // Zoomable image with markers, region selection and change tracking
  TImageViewer = class(TCustomControl)
  protected
    FOldCursor: TCursor;

    InMouseMove: Boolean;
    MouseMovePos: TPoint;

    FRunningAction: Boolean;
    FCancelAction: Boolean;
    FLastProcessMessages: QWord;

    FZoom: Double;
    FOptions: TPlotOptions;

    FImageName: TFileName;
    FPlotImg: TBGRABitmap;
    FZoomImg: TBGRABitmap;
    FWhiteBoard: TBGRABitmap;

    FMarkers: TMarkerList;
    FMarkerUnderCursor: TMarker;
    FActiveMarker: TMarker;
    FClickedMarker: TMarker;
    FClickedPoint: TPoint;
    FClickedCoord: TCurvePoint;
    FDragging: Boolean;
    FSelectingRegion: Boolean;
    FSelectionRect: TRect;

    FImageIsLoaded: Boolean;
    FIsChanged: Boolean;

    FOnChange: TNotifyEvent;
    FOnPrintMessage: TPrintMessageEvent;
    FOnShowProgress: TShowProgressEvent;
    FOnHideProgress: THideProgressEvent;
    FOnRegionSelected: TSelectRegionEvent;
    FOnMarkerDragged: TMarkerDraggedEvent;
    FOnZoomChanged: TZoomChangeEvent;

    class function GetControlClassDefaultSize: TSize; override;

    procedure SetCancelAction(Value: Boolean); virtual;
    function CheckCancelStatus: Boolean;

    procedure Paint; override;
    procedure Resize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function GetIsChanged: Boolean;
    procedure SetIsChanged(Value: Boolean);
    procedure SetZoom(Value: Double);
    procedure SetImageName(Value: TFileName);
    procedure SetMarkerUnderCursor(Marker: TMarker);
    procedure SetActiveMarker(Marker: TMarker);
    procedure SetDragging(Value: Boolean);
    procedure SetSelectingRegion(Value: Boolean);

    procedure ResetZoomImage;
    procedure ClearMarkers;
    procedure RepaintRegion(UpdateArea: TRect);
    procedure RepaintAll;

    procedure LoadImage(FileName: TFileName); overload;
    procedure LoadImage(Stream: TStream); overload;

    // Hooks for descendants
    function GetSourcePixel(X, Y: Integer): TBGRAPixel; virtual;
    procedure OverlayZoomImage; virtual;
    procedure ImageSizeChanged; virtual;
    procedure ScaleContent(Factor: Double); virtual;
    procedure SyncMarkersToModel; virtual;
    procedure SyncMarkersFromModel; virtual;
    procedure DrawContent(const PaintRect: TRect); virtual;
    procedure DrawForeground(const PaintRect: TRect); virtual;
    function MarkerRemoving(Marker: TMarker; RealDelete: Boolean): Boolean; virtual;
    procedure RepaintForMarkerMove(Marker: TMarker); virtual;

    function HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function HandleMouseMove(Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure MarkerClicked(Marker: TMarker; Shift: TShiftState); virtual;
    procedure BeforeMarkerMove(Marker: TMarker); virtual;
    procedure AfterMarkerMove(Marker: TMarker; NewPos: TCurvePoint); virtual;
    procedure MarkerDropped(Marker: TMarker); virtual;
    procedure MarkerDropDone; virtual;
    procedure MouseHover(X, Y: Integer); virtual;
    function CanStartRegion(Shift: TShiftState): Boolean; virtual;
    function CanSelectRegion: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure Reset; virtual;

    function GetPixel(X, Y: Integer): Longint; overload;
    function GetPixel(X, Y: Double): Longint; overload;
    function GetPixel(P: TCurvePoint): Longint; overload;

    procedure PasteImage(Stream: TStream);

    procedure ZoomOriginal;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ZoomFitBest;
    procedure ResizeToZoom;

    procedure AddMarker(Marker: TMarker; NewMarker: Boolean = True); overload;
    procedure RepaintMarker(Marker: TMarker);
    procedure DeleteMarker(Marker: TMarker; RealDelete: Boolean = True); overload;
    procedure DeleteMarker(Index: Integer; RealDelete: Boolean = True); overload;
    procedure DeleteMarkerUnderCursor;
    procedure DeleteActiveMarker;
    procedure RedrawMarkers;

    procedure ClearMarker(Marker: TMarker);
    procedure DrawMarker(Marker: TMarker);
    procedure MoveMarker(Marker: TMarker; Point: TCurvePoint); overload;
    procedure MoveMarker(Marker: TMarker; X, Y: Double); overload;

    function GetZoomImage(w, h: Integer; Region: TRect): TBitmap;

    property Zoom: Double read FZoom write SetZoom;
    property Options: TPlotOptions read FOptions write FOptions;
    property ImageName: TFileName read FImageName write SetImageName;
    property WhiteBoard: TBGRABitmap read FWhiteBoard;
    property PlotImg: TBGRABitmap read FPlotImg;
    property Image: TBGRABitmap read FPlotImg;
    property ZoomImg: TBGRABitmap read FZoomImg;
    property Markers: TMarkerList read FMarkers;
    property MarkerUnderCursor: TMarker read FMarkerUnderCursor write SetMarkerUnderCursor;
    property ActiveMarker: TMarker read FActiveMarker write SetActiveMarker;
    property Dragging: Boolean read FDragging write SetDragging;
    property SelectingRegion: Boolean read FSelectingRegion write SetSelectingRegion;
    property SelectionRect: TRect read FSelectionRect;
    property RunningAction: Boolean read FRunningAction write FRunningAction;
    property CancelAction: Boolean read FCancelAction write SetCancelAction;
    property ImageIsLoaded: Boolean read FImageIsLoaded write FImageIsLoaded;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnResize;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPrintMessage: TPrintMessageEvent read FOnPrintMessage write FOnPrintMessage;
    property OnShowProgress: TShowProgressEvent read FOnShowProgress write FOnShowProgress;
    property OnHideProgress: THideProgressEvent read FOnHideProgress write FOnHideProgress;
    property OnRegionSelected: TSelectRegionEvent read FOnRegionSelected write FOnRegionSelected;
    property OnMarkerDragged: TMarkerDraggedEvent read FOnMarkerDragged write FOnMarkerDragged;
    property OnZoomChanged: TZoomChangeEvent read FOnZoomChanged write FOnZoomChanged;
  end;

implementation

//============================|TImageViewer|==================================//
constructor TImageViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPlotImg := TBGRABitmap.Create;
  FZoomImg := TBGRABitmap.Create;
  FWhiteBoard := TBGRABitmap.Create;

  FMarkers := TMarkerList.Create;

  // Visible size in the form designer only
  if (csDesigning in ComponentState) then
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
end;


destructor TImageViewer.Destroy;
begin
  FWhiteBoard.Free;
  FZoomImg.Free;
  FPlotImg.Free;

  FMarkers.Free;

  inherited Destroy;
end;


procedure TImageViewer.AfterConstruction;
begin
  inherited AfterConstruction;
  // Descendants have created their objects by now
  Reset;
end;


class function TImageViewer.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 150;
end;


procedure TImageViewer.Reset;
begin
  FZoom := 1;

  // Drop references before the markers are freed
  FClickedMarker := nil;
  FActiveMarker := nil;
  FMarkerUnderCursor := nil;
  FMarkers.Clear;

  FOldCursor := Cursor;

  FImageIsLoaded := False;

  InMouseMove := False;

  FRunningAction := False;
  FCancelAction := False;

  FIsChanged := False;
end;


function TImageViewer.GetPixel(X, Y: Integer): Longint;
begin
  Result := ColorToRGB(GetSourcePixel(Round(X/Zoom), Round(Y/Zoom)));
end;


function TImageViewer.GetSourcePixel(X, Y: Integer): TBGRAPixel;
begin
  Result := PlotImg.GetPixel(X, Y);
end;


procedure TImageViewer.ResetZoomImage;
begin
  ZoomImg.SetSize(PlotImg.Width, PlotImg.Height);
  ZoomImg.PutImage(0, 0, PlotImg, dmSet);
  OverlayZoomImage;
  if (Zoom <> 1) then
  begin
    ZoomImg.ResampleFilter := rfSpline;
    BGRAReplace(FZoomImg, ZoomImg.Resample(Width, Height, rmFineResample));
  end;
end;


procedure TImageViewer.SetZoom(Value: Double);
begin
  if (Value > 0) and (Value <> FZoom) then
  begin
    SyncMarkersToModel;

    FZoom := Value;

    Width := Round(FZoom*PlotImg.Width);
    Height := Round(FZoom*PlotImg.Height);

    ResetZoomImage;

    WhiteBoard.SetSize(Width, Height);
    WhiteBoard.PutImage(0, 0, ZoomImg, dmSet);

    SyncMarkersFromModel;

    // Notify the parent that the state has changed
    if Assigned(OnZoomChanged) then
      OnZoomChanged(Self, Value);

    Invalidate;
  end;
end;


procedure TImageViewer.ResizeToZoom;
begin
  if (Zoom <> 1) then
  begin
    ScaleContent(Zoom);

    PlotImg.ResampleFilter := rfSpline;
    BGRAReplace(FPlotImg, PlotImg.Resample(Width, Height, rmFineResample));

    Zoom := 1.0;

    RepaintAll;

    IsChanged := True;
  end;
end;


procedure TImageViewer.LoadImage(Stream: TStream);
begin
  FImageIsLoaded := False;

  try
    Stream.Position := 0;
    PlotImg.LoadFromStream(Stream);
    PlotImg.ReplaceTransparent(Options.BgndColor);

    ZoomFitBest;

    Width := Round(Zoom*PlotImg.Width);
    Height := Round(Zoom*PlotImg.Height);

    ImageSizeChanged;

    FImageIsLoaded := True;

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


procedure TImageViewer.RedrawMarkers;
begin
  SyncMarkersToModel;
  SyncMarkersFromModel;
end;


procedure TImageViewer.SetCancelAction(Value: Boolean);
begin
  FCancelAction := Value;
end;


procedure TImageViewer.RepaintForMarkerMove(Marker: TMarker);
begin
  RepaintMarker(Marker);
end;


procedure TImageViewer.DeleteMarker(Marker: TMarker; RealDelete: Boolean = True);
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

    // Descendant bookkeeping; True means the marker is already gone
    if MarkerRemoving(Marker, RealDelete) then
      Exit;

    Markers.Remove(Marker);

    if (not Assigned(ActiveMarker)) and (Markers.Count > 0) then
      ActiveMarker := Markers[0];

    if RealDelete then
      IsChanged := True;
  end;
end;


procedure TImageViewer.Paint;
var
  i: Integer;
  {$ifdef windows}
  ClipRgn: HRGN;
  {$endif}
  Rect, ClipRect, PaintRect: TRect;
begin
  inherited Paint;

  // Placeholder in the form designer
  if (csDesigning in ComponentState) and not ImageIsLoaded then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Style := psDash;
    Canvas.Rectangle(ClientRect);
    Exit;
  end;

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

  DrawContent(PaintRect);

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

  DrawForeground(PaintRect);

  if SelectingRegion then
    WhiteBoard.Canvas.DrawFocusRect(SelectionRect);

  {$ifdef windows}
  with WhiteBoard.Canvas do
  begin
    SelectClipRgn(Handle, 0);
    DeleteObject(ClipRgn);
  end;
  {$endif}
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


procedure TImageViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Marker, HitMarker: TMarker;
begin
  if Button = mbLeft then
  begin
    // Descendant may take over the click
    if HandleMouseDown(Button, Shift, X, Y) then
    begin
      inherited MouseDown(Button, Shift, X, Y);
      Exit;
    end;

    HitMarker := nil;
    for i := 0 to Markers.Count - 1 do
    begin
      Marker := Markers[i];
      if Marker.HitTest(TPoint.Create(X, Y)) then
      begin
        HitMarker := Marker;
        FClickedPoint := TPoint.Create(X, Y);
        FClickedCoord := Marker.Position;
        Break;
      end;
    end;
    FClickedMarker := HitMarker;
    ActiveMarker := HitMarker;

    if Assigned(FClickedMarker) then
      MarkerClicked(FClickedMarker, Shift);

    // No marker under cursor: start a region selection
    if (HitMarker = nil) and CanStartRegion(Shift) then
    begin
      SelectingRegion := True;
      FSelectionRect := TRect.Create(X, Y, X, Y);
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;


procedure TImageViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Marker, HitMarker: TMarker;
  TmpRect: TRect;
  NewPos: TCurvePoint;
begin
  MouseMovePos := TPoint.Create(X, Y);

  // Make sure that there are no previous events in the stack
  if InMouseMove then Exit;
  InMouseMove := True;
  try
    // Empty message stack
    Application.ProcessMessages;

    inherited MouseMove(Shift, MouseMovePos.X, MouseMovePos.Y);

    if HandleMouseMove(Shift, MouseMovePos.X, MouseMovePos.Y) then
      Exit;

    // Guard against FClickedMarker having been freed by a re-entrant event
    if Assigned(FClickedMarker) and (Markers.IndexOf(FClickedMarker) < 0) then
    begin
      FClickedMarker := nil;
      Dragging := False;
    end;

    if Assigned(FClickedMarker) then
    begin
      if not Dragging then
        if (Abs(MouseMovePos.X - FClickedPoint.X) > 5) or
          (Abs(MouseMovePos.Y - FClickedPoint.Y) > 5) then
          Dragging := True;

      if Dragging then
      begin
        // Erase old marker position
        RepaintForMarkerMove(FClickedMarker);
        BeforeMarkerMove(FClickedMarker);

        if ClientRect.Contains(MouseMovePos) then
          NewPos := FClickedCoord + MouseMovePos - FClickedPoint
        else
          NewPos := FClickedCoord;  //The mouse moves out of the image

        FClickedMarker.Move(NewPos);
        AfterMarkerMove(FClickedMarker, NewPos);

        // Draw marker in new position
        RepaintForMarkerMove(FClickedMarker);
        MarkerUnderCursor := FClickedMarker;

        Exit;
      end;
    end
    else
    begin
      if SelectingRegion and CanSelectRegion then
      begin
        TmpRect := FSelectionRect;
        FSelectionRect.Width := MouseMovePos.X - FSelectionRect.Left;
        FSelectionRect.Height := MouseMovePos.Y - FSelectionRect.Top;

        //Refresh the rectangle containing the old and new selection
        TmpRect.Union(FSelectionRect);
        RepaintRegion(TmpRect);

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

    MouseHover(MouseMovePos.X, MouseMovePos.Y);
  finally
    InMouseMove := False;
  end;
end;


procedure TImageViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  WasDragging: Boolean;
begin
  // Make sure that there are no previous events in the stack
  InMouseMove := True;
  try
    // Empty message stack
    Application.ProcessMessages;

    if HandleMouseUp(Button, Shift, X, Y) then
    begin
      inherited MouseUp(Button, Shift, X, Y);
      Exit;
    end;

    WasDragging := Dragging;

    if Button = mbLeft then
    begin
      if Dragging then
      begin
        Dragging := False;

        MarkerDropped(FClickedMarker);

        // Notify that the marker has been dragged
        if Assigned(OnMarkerDragged) and Assigned(FClickedMarker) then
          OnMarkerDragged(Self, FClickedMarker, True);

        MarkerDropDone;

        IsChanged := True;
      end
      else
      begin
        if SelectingRegion and CanSelectRegion then
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
          // Ensure that the region is correct
          FSelectionRect := CorrectRectangle(FSelectionRect);

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
  finally
    InMouseMove := False;
  end;
end;


// Default hooks: plain image viewer
procedure TImageViewer.OverlayZoomImage;
begin
end;


procedure TImageViewer.ImageSizeChanged;
begin
end;


procedure TImageViewer.ScaleContent(Factor: Double);
begin
end;


procedure TImageViewer.SyncMarkersToModel;
begin
end;


procedure TImageViewer.SyncMarkersFromModel;
begin
end;


procedure TImageViewer.DrawContent(const PaintRect: TRect);
begin
end;


procedure TImageViewer.DrawForeground(const PaintRect: TRect);
begin
end;


function TImageViewer.MarkerRemoving(Marker: TMarker; RealDelete: Boolean): Boolean;
begin
  Result := False;
end;


function TImageViewer.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;


function TImageViewer.HandleMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;


function TImageViewer.HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
end;


procedure TImageViewer.MarkerClicked(Marker: TMarker; Shift: TShiftState);
begin
end;


procedure TImageViewer.BeforeMarkerMove(Marker: TMarker);
begin
end;


procedure TImageViewer.AfterMarkerMove(Marker: TMarker; NewPos: TCurvePoint);
begin
end;


procedure TImageViewer.MarkerDropped(Marker: TMarker);
begin
end;


procedure TImageViewer.MarkerDropDone;
begin
end;


procedure TImageViewer.MouseHover(X, Y: Integer);
begin
end;


function TImageViewer.CanStartRegion(Shift: TShiftState): Boolean;
begin
  Result := False;
end;


function TImageViewer.CanSelectRegion: Boolean;
begin
  Result := True;
end;


function TImageViewer.GetPixel(X, Y: Double): Longint;
begin
  Result := GetPixel(Round(X), Round(Y));
end;


function TImageViewer.GetPixel(P: TCurvePoint): Longint;
begin
  Result := GetPixel(P.X, P.Y);
end;


procedure TImageViewer.ZoomOriginal;
begin
  Zoom := 1;
end;


procedure TImageViewer.ZoomIn;
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


procedure TImageViewer.ZoomOut;
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


procedure TImageViewer.ZoomFitBest;
begin
  // Guard against no parent or an image that has not been loaded yet
  if not Assigned(Parent) then Exit;
  if (PlotImg.Width = 0) or (PlotImg.Height = 0) then Exit;

  if ((PlotImg.Width/PlotImg.Height) > (Parent.ClientWidth/Parent.ClientHeight)) then
    Zoom := Parent.ClientWidth/PlotImg.Width
  else
    Zoom := Parent.ClientHeight/PlotImg.Height;
end;


procedure TImageViewer.PasteImage(Stream: TStream);
var
  TmpBmp: TBGRABitmap;
  NewStream: TMemoryStream;
begin
  TmpBmp := nil;
  NewStream := nil;
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


procedure TImageViewer.SetImageName(Value: TFileName);
begin
  FImageName := Value;
  LoadImage(FImageName);
end;


function TImageViewer.GetZoomImage(w, h: Integer; Region: TRect): TBitmap;
var
  TmpZoomImg: TBGRABitmap;
begin
  TmpZoomImg := nil;
  try
    Result := TBitmap.Create;
    Result.SetSize(w, h);
    Result.PixelFormat := pf24bit;

    TmpZoomImg := TBGRABitmap.Create(Region.Width, Region.Height, clWhite);
    TmpZoomImg.CanvasBGRA.CopyRect(TRect.Create(0, 0, Region.Width, Region.Height), WhiteBoard, Region);

    //BGRAReplace(TmpZoomImg, TmpZoomImg.Resample(w, h, rmSimpleStretch));
    TmpZoomImg.ResampleFilter := rfSpline;
    BGRAReplace(TmpZoomImg, TmpZoomImg.Resample(w, h, rmFineResample));
    TmpZoomImg.Draw(Result.Canvas, 0, 0, True);
  finally
    TmpZoomImg.Free;
  end;
end;


procedure TImageViewer.RepaintMarker(Marker: TMarker);
begin
  RepaintRegion(Marker.Rect);
end;


procedure TImageViewer.DeleteMarkerUnderCursor;
begin
  DeleteMarker(MarkerUnderCursor, True);
end;


procedure TImageViewer.DeleteActiveMarker;
begin
  DeleteMarker(ActiveMarker, True);
end;


procedure TImageViewer.ClearMarkers;
var
  i: Integer;
begin
  for i := Markers.Count - 1 downto 0 do
    DeleteMarker(i, False);
end;


procedure TImageViewer.ClearMarker(Marker: TMarker);
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


procedure TImageViewer.DrawMarker(Marker: TMarker);
begin
  if (Markers.IndexOf(Marker) > -1) then
    Marker.Draw(WhiteBoard, Marker.Rect);
end;


procedure TImageViewer.MoveMarker(Marker: TMarker; Point: TCurvePoint);
begin
  if (Markers.IndexOf(Marker) > -1) then
  begin
    ClearMarker(Marker);
    Marker.MoveSilent(Point);
    DrawMarker(Marker);

    IsChanged := True;
  end;
end;


procedure TImageViewer.MoveMarker(Marker: TMarker; X, Y: Double);
begin
  MoveMarker(Marker, TCurvePoint.Create(X, Y));
end;


procedure TImageViewer.SetMarkerUnderCursor(Marker: TMarker);
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


procedure TImageViewer.SetActiveMarker(Marker: TMarker);
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


procedure TImageViewer.SetDragging(Value: Boolean);
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


procedure TImageViewer.SetSelectingRegion(Value: Boolean);
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


procedure TImageViewer.RepaintRegion(UpdateArea: TRect);
begin
  Canvas.ClipRect := UpdateArea;
  {$ifdef windows}
  InvalidateRect(Handle, UpdateArea, False);
  {$else}
  Invalidate;
  {$endif}
end;


procedure TImageViewer.RepaintAll;
begin
  RepaintRegion(ClientRect);
end;


function TImageViewer.CheckCancelStatus: Boolean;
begin
  // Only pump the message queue every 100 ms to keep the UI responsive
  if (GetTickCount64 - FLastProcessMessages) > 100 then
  begin
    Application.ProcessMessages;
    FLastProcessMessages := GetTickCount64;
  end;
  Result := FCancelAction;
end;


function TImageViewer.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;


procedure TImageViewer.SetIsChanged(Value: Boolean);
begin
  if (Value <> FIsChanged) then
    FIsChanged := Value;

  // Notify the parent that the PlotImage has changed
  if FIsChanged and Assigned(OnChange) then
    OnChange(Self);
end;


procedure TImageViewer.Resize;
begin
  Invalidate;
  inherited Resize;
end;


procedure TImageViewer.DeleteMarker(Index: Integer; RealDelete: Boolean = True);
begin
  if (Index >= 0) and (Index < Markers.Count) then
    DeleteMarker(Markers[Index], RealDelete);
end;


procedure TImageViewer.AddMarker(Marker: TMarker; NewMarker: Boolean = True);
begin
  Markers.Insert(0, Marker);
  ActiveMarker := Marker;
  RepaintMarker(Marker);

  if NewMarker then
    IsChanged := True;
end;


procedure TImageViewer.LoadImage(FileName: TFileName);
var
  TmpBmp: TBGRABitmap;
  Stream: TMemoryStream;
begin
  FImageIsLoaded := False;

  if FileExists(FileName) then
  begin
    TmpBmp := nil;
    Stream := nil;
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

end.
