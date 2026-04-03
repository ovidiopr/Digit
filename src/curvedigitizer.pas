unit CurveDigitizer;

{$mode objfpc}{$H+}

{ =============================================================================
  CurveDigitizer.pas  –  All algorithms for extracting curves from plot images
  =============================================================================

  This unit is deliberately free of any UI framework dependency and knows
  nothing about TPlotImage.  Callers build a TDigitizerContext and supply
  a TBGRABitmap.

  Public surface
  ──────────────
  ScanCurvePoints          – collect every matching pixel (respects grid mask)
  DigitizeCurve            – main entry: dispatch by TDigitization
  AdjustDigitizedCurve     – snap each curve point to its island centroid
  ConvertCurveToSymbolPoints – condense a line curve into discrete blob centres
  PixelMatchesCurve        – single-pixel predicate (useful for callers that
                              need to probe individual pixels with mask support)

  Grid-mask contract
  ──────────────────
  When TDigitizerContext.GridMaskActive is True and GridMask is non-nil,
  every pixel lookup first checks the mask bitmap.  A non-transparent mask
  pixel (alpha > 0) overrides the corresponding source pixel.  This is the
  same composite rule used by the existing RemoveGrid/SwitchGrid workflow
  in TPlotImage.
  ============================================================================= }

interface

uses
  Classes, SysUtils, Math, Graphics, BGRABitmap, BGRABitmapTypes,
  uutils, uscale, ucurves, ucoordinates, upolygons;

type
  TScanDirection = (sdForward, sdBackward);

  TScanProgressEvent = procedure(Percent: Integer; Messg: String) of object;
  TScanCheckTerminated = function: Boolean of object;

  // All parameters that drive the digitization process
  TDigitizerContext = record
    Mode: TDigitization;
    Plot: TPlot;
    Color: TColor;
    ScanDirection: TScanDirection;
    Tolerance: Integer;
    LineSpread: Integer;  // Half-width for sub-pixel centroiding (Spread)
    MaxGap: Integer;      // Max gap to bridge/scan interval (Interval)
    Step: Integer;
    // Grid-mask overlay – when active, non-transparent mask pixels replace
    // the corresponding source pixels during every colour comparison.
    GridMask: TBGRABitmap;
    GridMaskActive: Boolean;

    OnProgress: TScanProgressEvent;
    CheckTerminated: TScanCheckTerminated;
  end;

  // 2-D boolean array indexed as [Y][X]: True = pixel already processed
  TVisitedMap = array of array of Boolean;

  TDigitizerThread = class(TThread)
    private
      FImage: TBGRABitmap;
      FOwnsImage: Boolean;
      FGridMaskCopy: TBGRABitmap;
      FContext: TDigitizerContext;
      FSeeds: TCurve;
      FResultCurve: TCurve;

      FatalThreadException: Exception;
      FatalThreadMessage: String;

      // Thread-safe progress handling
      FOnProgress: TScanProgressEvent;
      FCurrentProgress: Integer;
      FCurrentMessage: String;
      procedure SyncProgress;
      procedure DoProgress(Percent: Integer; Messg: String);
      function DoCheckTerminated: Boolean;
      procedure SyncFatalError;
    protected
      procedure Execute; override;
    public
      constructor Create(AImage: TBGRABitmap; const ACtx: TDigitizerContext; ASeeds: TCurve; ACallback: TNotifyEvent);
      destructor Destroy; override;

      function TakeResultCurve: TCurve;

      property ResultCurve: TCurve read FResultCurve;
      property IsTerminated: Boolean read DoCheckTerminated;
      property OnProgress: TScanProgressEvent read FOnProgress write FOnProgress;
    end;

  // Thin abstraction over the image so algorithms stay testable without a
  // real TBGRABitmap
  IPixelSource = interface
    function Pixel(X, Y: Integer): TBGRAPixel;
    function Width: Integer;
    function Height: Integer;
    function Contains(P: TCurvePoint): Boolean;
  end;

  // Wraps a TBGRABitmap plus optional grid-mask as an IPixelSource.
  // When a mask pixel has alpha > 0 it replaces the corresponding source
  // pixel in every Pixel() call.
  TBGRABitmapSource = class(TInterfacedObject, IPixelSource)
  private
    FImg: TBGRABitmap;
    FMaskedImg: TBGRABitmap; // Holds the pre-composited image to speed up scans
    FQuad: TPlotQuad;
  public
    constructor Create(AImg: TBGRABitmap; const ACtx: TDigitizerContext);
    destructor Destroy; override;
    function Pixel(X, Y: Integer): TBGRAPixel;
    function Width: Integer;
    function Height: Integer;
    function Contains(P: TCurvePoint): Boolean;
  end;

{ --------------------------------------------------------------------------- }
{ Public API                                                                  }
{ --------------------------------------------------------------------------- }

{ Returns True when the image pixel at (X, Y) matches Ctx.Color.
  The grid mask is applied when Ctx.GridMaskActive = True.
  Coordinates are in image (pixel) space.  The plot-box boundary is NOT
  checked here – use this for point probes; full scans use PixelBelongsToCurve
  internally. }
function PixelMatchesCurve(Image: TBGRABitmap; const Ctx: TDigitizerContext; X, Y: Integer): Boolean;

{ Scan every pixel inside Ctx.Plot.Box that matches Ctx.Color within
  Ctx.Tolerance.  Results are appended to AllPoints (caller must Clear first
  if a fresh scan is desired).  The grid mask is applied automatically. }
procedure ScanCurvePoints(Image: TBGRABitmap; const Ctx: TDigitizerContext; AllPoints: TCurve);

{ Main digitization entry point.
  Seeds  – one or more starting positions in image (pixel) coordinates.
  Image  – the original plot image (mask carried inside Ctx).
  Curve  – receives the digitized points in image coordinates.
  Returns True when at least one point was found. }
function DigitizeCurve(Seeds: TCurve; Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve): Boolean;

{ Post-process: for every point in Curve find the connected island of
  same-coloured pixels around it and replace the point with the island
  centroid.  All islands share a single TVisitedMap so no pixel is counted
  twice.

  When Noisy = True the island is allowed to grow only in the vertical
  direction of a detected local extremum, which suppresses isolated noise
  spikes while keeping genuine curve peaks intact. }
procedure AdjustDigitizedCurve(Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve; Noisy: Boolean = False);

{ Post-process: replace every connected blob that the curve touches with its
  2-D centroid.  Designed to convert a continuous traced line into a set of
  discrete data-point symbols. }
procedure ConvertCurveToSymbolPoints(Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve);


implementation

{ =========================================================================== }
{ Color helpers                                                               }
{ =========================================================================== }

function ColorsAreSimilar(C1: TBGRAPixel; C2: TColor; Tol: Integer): Boolean; overload;
begin
  Result := AreSimilar(C1.red, C1.green, C1.blue, Red(C2), Green(C2), Blue(C2), Tol);
end;

function ColorsAreSimilar(C1: TColor; C2: TBGRAPixel; Tol: Integer): Boolean; overload;
begin
  Result := ColorsAreSimilar(C2, C1, Tol);
end;


{ =========================================================================== }
{ TDigitizerThread                                                            }
{ =========================================================================== }

constructor TDigitizerThread.Create(AImage: TBGRABitmap; const ACtx: TDigitizerContext; ASeeds: TCurve; ACallback: TNotifyEvent);
var
  i: Integer;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FImage := AImage.Duplicate as TBGRABitmap;
  FOwnsImage := True;
  FContext := ACtx;
  if ACtx.GridMaskActive and Assigned(ACtx.GridMask) then
  begin
    FGridMaskCopy := ACtx.GridMask.Duplicate as TBGRABitmap;
    FContext.GridMask := FGridMaskCopy;
  end
  else
  begin
    FGridMaskCopy := nil;
    FContext.GridMask := nil;
  end;
  OnTerminate := ACallback;
  FResultCurve := TCurve.Create;
  FSeeds := TCurve.Create;
  if Assigned(ASeeds) then
    for i := 0 to ASeeds.Count - 1 do
      FSeeds.AddPoint(ASeeds.Point[i]);
end;

destructor TDigitizerThread.Destroy;
begin
  FSeeds.Free;
  if assigned(FResultCurve) then
    FResultCurve.Free;
    if FOwnsImage then

  FImage.Free;
  FGridMaskCopy.Free;

  inherited Destroy;
end;

function TDigitizerThread.TakeResultCurve: TCurve;
begin
  Result := FResultCurve;
  FResultCurve := nil;
end;

procedure TDigitizerThread.SyncProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FCurrentProgress, FCurrentMessage);
end;

procedure TDigitizerThread.DoProgress(Percent: Integer; Messg: String);
begin
  FCurrentProgress := Percent;
  FCurrentMessage := Messg;
  TThread.Queue(Self, @SyncProgress);
end;

function TDigitizerThread.DoCheckTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TDigitizerThread.SyncFatalError;
begin
  raise EThread.CreateFmt('Digitizer thread error (%s): %s',
        [FatalThreadException.ClassName, FatalThreadMessage]);
end;

procedure TDigitizerThread.Execute;
begin
  FContext.OnProgress := @DoProgress;
  FContext.CheckTerminated := @DoCheckTerminated;
  FResultCurve.Clear;
  DigitizeCurve(FSeeds, FImage, FContext, FResultCurve);
end;

{ =========================================================================== }
{ TBGRABitmapSource                                                           }
{ =========================================================================== }

constructor TBGRABitmapSource.Create(AImg: TBGRABitmap; const ACtx: TDigitizerContext);
var
  x, y: Integer;
  pImg, pMask: PBGRAPixel;
  AMask: TBGRABitmap;
  AMaskActive: Boolean;
begin
  FQuad := ACtx.Plot.Box;
  AMask := ACtx.GridMask;
  AMaskActive := ACtx.GridMaskActive;

  // Instead of checking the mask in the Pixel() function millions of times,
  // we generate a single image with the grid removed upfront.
  if AMaskActive and Assigned(AMask) and (AMask.Width = AImg.Width) and (AMask.Height = AImg.Height) then
  begin
    FMaskedImg := AImg.Duplicate as TBGRABitmap;
    for y := 0 to FMaskedImg.Height - 1 do
    begin
      pImg := FMaskedImg.Scanline[y];
      pMask := AMask.Scanline[y];
      for x := 0 to FMaskedImg.Width - 1 do
      begin
        if pMask^.alpha > 0 then
          pImg^ := pMask^;
        Inc(pImg);
        Inc(pMask);
      end;
    end;
    FImg := FMaskedImg;
  end
  else
  begin
    FMaskedImg := nil;
    FImg := AImg;
  end;
end;

destructor TBGRABitmapSource.Destroy;
begin
  if Assigned(FMaskedImg) then
    FMaskedImg.Free;
  inherited Destroy;
end;

function TBGRABitmapSource.Pixel(X, Y: Integer): TBGRAPixel;
begin
  if (X < 0) or (Y < 0) or (X >= FImg.Width) or (Y >= FImg.Height) then
    Exit(BGRAPixelTransparent);

  // No alpha checks needed here anymore since the mask is pre-composited
  Result := FImg.Scanline[Y][X];
end;

function TBGRABitmapSource.Width: Integer;
begin
  Result := FImg.Width;
end;

function TBGRABitmapSource.Height: Integer;
begin
  Result := FImg.Height;
end;

function TBGRABitmapSource.Contains(P: TCurvePoint): Boolean;
begin
  Result := FQuad.Contains(P);
end;

{ =========================================================================== }
{ Pixel acceptance predicates                                                 }
{ =========================================================================== }

// Pixel is inside the image bounds, inside the plot box, and colour-
// matches the target with the given tolerance
function PixelBelongsToCurve(Src: IPixelSource; X, Y: Integer; const Ctx: TDigitizerContext): Boolean;
begin
  if (X < 0) or (Y < 0) or (X >= Src.Width) or (Y >= Src.Height) then
    Exit(False);

  if not ColorsAreSimilar(Src.Pixel(X, Y), Ctx.Color, Ctx.Tolerance) then
    Exit(False);

  Result := Src.Contains(TCurvePoint.Create(X, Y));
end;

// Same check, works directly on the raw TBGRABitmap + mask
function PixelMatchesCurve(Image: TBGRABitmap; const Ctx: TDigitizerContext; X, Y: Integer): Boolean;
var
  Src: IPixelSource;
begin
  Src := TBGRABitmapSource.Create(Image, Ctx);
  Result := PixelBelongsToCurve(Src, X, Y, Ctx);
end;

{ =========================================================================== }
{ ScanCurvePoints                                                             }
{ =========================================================================== }

procedure ScanCurvePoints(Image: TBGRABitmap; const Ctx: TDigitizerContext; AllPoints: TCurve);
var
  Src: IPixelSource;
  x, y, TotalH, LastProgress, CurrentProgress: Integer;
  BBox: TRect;
begin
  AllPoints.Clear;
  if (Image = nil) or (Ctx.Plot.Box = nil) then Exit;

  BBox := Ctx.Plot.Box.Rect[1.0];
  Src := TBGRABitmapSource.Create(Image, Ctx);

  TotalH := Max(1, BBox.Bottom - BBox.Top);
  LastProgress := -1;

  for y := BBox.Top to BBox.Bottom do
  begin
    // Check for Cancel
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    // Report Progress
    if Assigned(Ctx.OnProgress) then
    begin
      CurrentProgress := Round(((y - BBox.Top)/TotalH)*100);
      if CurrentProgress <> LastProgress then
      begin
        Ctx.OnProgress(CurrentProgress, 'Scanning curve points...');
        LastProgress := CurrentProgress;
      end;
    end;

    for x := BBox.Left to BBox.Right do
      if PixelBelongsToCurve(Src, x, y, Ctx) then
        AllPoints.AddPoint(TCurvePoint.Create(x, y));
  end;
end;

{ =========================================================================== }
{ ALGORITHM 1 – Line tracing (connected-component grow with gap bridging)     }
{ =========================================================================== }

procedure GrowCurveRegion(Src: IPixelSource; Seeds: TCurve; const Ctx: TDigitizerContext; out Region: TVisitedMap);
const
  DX: array[0..7] of Integer = (-1, 0, 1, -1, 1, -1, 0, 1);
  DY: array[0..7] of Integer = (-1,-1,-1,  0, 0,  1, 1, 1);
var
  StackX, StackY, LastDX, LastDY: array of Integer;
  Top, i, s, X, Y, nx, ny,
  px, py, dist, w, j, tx, ty: Integer;
  FoundNext: Boolean;
  TotalPixels, Processed, LastProgress, CurrentProgress: Integer;
begin
  SetLength(Region,  Src.Height, Src.Width);
  SetLength(StackX,  Src.Width*Src.Height);
  SetLength(StackY,  Length(StackX));
  SetLength(LastDX,  Length(StackX));
  SetLength(LastDY,  Length(StackX));
  Top := -1;

  TotalPixels := Max(1, Src.Width*Src.Height);
  Processed := 0;
  LastProgress := -1;

  for s := 0 to Seeds.Count - 1 do
  begin
    X := Round(Seeds[s].X);
    Y := Round(Seeds[s].Y);
    if (X >= 0) and (Y >= 0) and (X < Src.Width) and (Y < Src.Height)
       and not Region[Y][X] then
    begin
      Inc(Top);
      StackX[Top] := X;  StackY[Top] := Y;
      LastDX[Top] := 0;  LastDY[Top] := 0;
      Region[Y][X] := True;
    end;
  end;

  while Top >= 0 do
  begin
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    X := StackX[Top]; Y := StackY[Top];
    nx := LastDX[Top]; ny := LastDY[Top];
    Dec(Top);
    FoundNext := False;

    Inc(Processed);
    if Assigned(Ctx.OnProgress) then
    begin
      CurrentProgress := Min(99, Processed*100 div TotalPixels);
      if CurrentProgress <> LastProgress then
      begin
        Ctx.OnProgress(CurrentProgress, 'Growing curve region...');
        LastProgress := CurrentProgress;
      end;
    end;

    for i := 0 to 7 do
    begin
      tx := X + DX[i]; ty := Y + DY[i];
      if PixelBelongsToCurve(Src, tx, ty, Ctx) and not Region[ty][tx] then
      begin
        Region[ty][tx] := True;
        Inc(Top);
        StackX[Top] := tx; StackY[Top] := ty;
        LastDX[Top] := DX[i]; LastDY[Top] := DY[i];
        FoundNext := True;
      end;
    end;

    // Gap bridging: if the trace just ended, try to jump forward along the
    // last known direction, spreading perpendicularly by ±w pixels.
    if not FoundNext and ((nx <> 0) or (ny <> 0)) and (Ctx.MaxGap > 0) then
    begin
      px := -ny; py := nx;   // perpendicular to travel direction
      for dist := 2 to Ctx.MaxGap do
      begin
        w := (dist div 3) + 1;
        for j := -w to w do
        begin
          tx := X + (nx*dist) + (px*j);
          ty := Y + (ny*dist) + (py*j);
          if PixelBelongsToCurve(Src, tx, ty, Ctx) and not Region[ty][tx] then
          begin
            Region[ty][tx] := True;
            Inc(Top);
            StackX[Top] := tx; StackY[Top] := ty;
            LastDX[Top] := nx; LastDY[Top] := ny;
            FoundNext := True;
            Break;
          end;
        end;
        if FoundNext then Break;
      end;
    end;
  end;
end;

{ =========================================================================== }
{ ALGORITHM 2 – Symbol tracing (blob grow + centroid jump)                    }
{ =========================================================================== }

procedure GrowSymbolRegion(Src: IPixelSource; Seeds: TCurve; const Ctx: TDigitizerContext; out Region: TVisitedMap);
const
  DX: array[0..7] of Integer = (-1, 0, 1, -1, 1, -1, 0, 1);
  DY: array[0..7] of Integer = (-1,-1,-1,  0, 0,  1, 1, 1);
var
  StackX, StackY: array of Integer;
  Top, X, Y, tx, ty, s, i,
  SumX, SumY, Count,
  CX, CY, dist, rdx, rdy,
  BestX, BestY, BestDX, BestDY,
  deltaX, deltaY: Integer;
  Found: Boolean;
  TotalPixels, Processed, LastProgress, CurrentProgress: Integer;
begin
  SetLength(Region, Src.Height, Src.Width);
  SetLength(StackX, Src.Width*Src.Height);
  SetLength(StackY, Length(StackX));
  Top := -1;

  TotalPixels := Max(1, Src.Width*Src.Height);
  Processed := 0;
  LastProgress := -1;

  for Y := 0 to Src.Height - 1 do
    for X := 0 to Src.Width - 1 do
      Region[Y][X] := False;

  for s := 0 to Seeds.Count - 1 do
  begin
    X := Round(Seeds[s].X);
    Y := Round(Seeds[s].Y);
    if (X >= 0) and (Y >= 0) and (X < Src.Width) and (Y < Src.Height)
       and not Region[Y][X] then
    begin
      Region[Y][X] := True;
      Inc(Top);
      StackX[Top] := X;
      StackY[Top] := Y;
    end;
  end;

  while Top >= 0 do
  begin
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    X := StackX[Top]; Y := StackY[Top];
    Dec(Top);

    Inc(Processed);
    if Assigned(Ctx.OnProgress) then
    begin
      CurrentProgress := Min(99, Processed*100 div TotalPixels);
      if CurrentProgress <> LastProgress then
      begin
        Ctx.OnProgress(CurrentProgress, 'Growing symbol region');
        LastProgress := CurrentProgress;
      end;
    end;

    // Flood-fill this blob
    SumX := 0; SumY := 0; Count := 0;
    Inc(Top);
    StackX[Top] := X; StackY[Top] := Y;

    while Top >= 0 do
    begin
      X := StackX[Top]; Y := StackY[Top];
      Dec(Top);
      Inc(SumX, X); Inc(SumY, Y); Inc(Count);

      for i := 0 to 7 do
      begin
        tx := X + DX[i]; ty := Y + DY[i];
        if PixelBelongsToCurve(Src, tx, ty, Ctx) and not Region[ty][tx] then
        begin
          Region[ty][tx] := True;
          Inc(Top);
          StackX[Top] := tx; StackY[Top] := ty;
        end;
      end;
    end;

    if Count = 0 then Continue;

    CX := SumX div Count; CY := SumY div Count;

    // Jump from the blob centroid to the nearest unvisited matching pixel
    // within MaxGap distance, preferring the closest one.
    Found := False; BestDX := MaxInt; BestDY := MaxInt;
    for dist := 2 to Ctx.MaxGap do
      for rdy := -dist to dist do
        for rdx := -dist to dist do
          if (Abs(rdx) = dist) or (Abs(rdy) = dist) then
          begin
            tx := CX + rdx; ty := CY + rdy;
            if PixelBelongsToCurve(Src, tx, ty, Ctx) and not Region[ty][tx] then
            begin
              deltaX := Abs(rdx); deltaY := Abs(rdy);
              if (deltaX < BestDX) or
                 ((deltaX = BestDX) and (deltaY < BestDY)) then
              begin
                BestDX := deltaX; BestDY := deltaY;
                BestX := tx;     BestY := ty;
                Found := True;
              end;
            end;
          end;

    if Found then
    begin
      Region[BestY][BestX] := True;
      Inc(Top);
      StackX[Top] := BestX; StackY[Top] := BestY;
    end;
  end;
end;

{ =========================================================================== }
{ ALGORITHM 3 – Line following (column-by-column centroid scan)               }
{ =========================================================================== }
procedure FollowLine(Src: IPixelSource; Seeds: TCurve; Pi: TCurvePoint;
                     const Ctx: TDigitizerContext; Curve: TCurve);
var
  PNew: TCurvePoint;
  Delta: Double;
  ML: TCurve;
  i, Step, Interval: Integer;
  Pp: TCurvePoint;
  BBox: TRect;
  TotalSteps, LastProgress, CurrentProgress: Integer;

  // Search vertically around P for a band of matching pixels.
  // Returns the sub-pixel centroid via P, or False if nothing found.
  function FindNextPoint(var P: TCurvePoint): Boolean;
  var
    yOffset: Integer;
    AccP: TCurvePoint;
    nP: Integer;
    Pix: TBGRAPixel;
  begin
    AccP := TCurvePoint.Create(0, 0);
    nP := 0;
    yOffset := 0;

    repeat
      if (Round(P.Y) - yOffset >= 0) and
         Src.Contains(TCurvePoint.Create(P.X, P.Y - yOffset)) then
      begin
        Pix := Src.Pixel(Round(P.X), Round(P.Y - yOffset));
        if ColorsAreSimilar(Pix, Ctx.Color, Ctx.Tolerance) then
        begin
          AccP := AccP + TCurvePoint.Create(P.X, P.Y - yOffset);
          Inc(nP);
        end;
      end;

      if (Round(P.Y) + yOffset < Src.Height) and
         Src.Contains(TCurvePoint.Create(P.X, P.Y + yOffset)) then
      begin
        Pix := Src.Pixel(Round(P.X), Round(P.Y + yOffset));
        if ColorsAreSimilar(Pix, Ctx.Color, Ctx.Tolerance) then
        begin
          AccP := AccP + TCurvePoint.Create(P.X, P.Y + yOffset);
          Inc(nP);
        end;
      end;

      Inc(yOffset);
    until (nP >= 1 + 2*Interval) or (yOffset > Interval);

    if nP > 0 then
    begin
      P := AccP/nP;
      Result := True;
    end
    else
      Result := False;
  end;

begin
  Curve.Clear;
  Curve.AddPoint(Pi);

  Delta := 0;
  ML := Seeds;
  i := 0;

  Step := Ctx.Step;
  Interval := Ctx.MaxGap;

  BBox := Ctx.Plot.Box.Rect[1.0];
  if ML.Count > 2 then
    TotalSteps := Max(1, ML.Count - 1)
  else
    TotalSteps := Max(1, Abs(Round(BBox.Right - BBox.Left)));
  LastProgress := -1;

  repeat
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    if Assigned(Ctx.OnProgress) then
    begin
      if ML.Count > 2 then
        CurrentProgress := Min(99, i*100 div TotalSteps)
      else if Ctx.Plot.Scale.CoordSystem = csPolar then
        CurrentProgress := Min(99, Abs(Round(Delta))*100 div 360)
      else
        CurrentProgress := Min(99, Abs(Round(Pi.X - BBox.Left))*100 div TotalSteps);
      if CurrentProgress <> LastProgress then
      begin
        Ctx.OnProgress(CurrentProgress, 'Detecting curve...');
        LastProgress := CurrentProgress;
      end;
    end;

    if ML.Count <= 2 then
    begin
      // No guide markers: step by one pixel in the scan direction
      if Ctx.Plot.Scale.CoordSystem = csPolar then
      begin
        Pp := CartesianToPolar(Pi - Ctx.Plot.Scale.ImagePoint[2]);
        if (2*Pp.Y*Pp.Y < Step*Step) then Pp.Y := Step;
        Pp.X := Pp.X - ArcSin(Step/Sqrt(4*Pp.Y*Pp.Y - Step*Step))*90/ArcTan(1);
        Pi := Ctx.Plot.Scale.ImagePoint[2] + PolarToCartesian(Pp);
        Delta := Delta + Sign(Step)*
                 ArcSin(Step/Sqrt(4*Pp.Y*Pp.Y - Step*Step))*90/ArcTan(1);
      end
      else
        Pi := Pi + Step*Ctx.Plot.Scale.Nx(Pi);

      PNew := Pi;
    end
    else
    begin
      // Follow the guide markers
      Inc(i);
      if Step > 0 then
        PNew := ML.Point[i]
      else
        PNew := ML.Point[ML.Count - 1 - i];
    end;

    if FindNextPoint(PNew) then
    begin
      Pi := PNew;
      Curve.AddPoint(Pi);
    end;

    if not Ctx.Plot.Box.Contains(Pi) then Break;
    if (Ctx.Plot.Scale.CoordSystem = csPolar) and (Abs(Delta) > 360) then Break;
    if (ML.Count > 2) and (i >= ML.Count - 1) then Break;

  until False;
end;

{ =========================================================================== }
{ ALGORITHM 4 – Color trace (scan + greedy nearest-neighbour clustering)      }
{ =========================================================================== }

// Collect all matching pixels and group them by proximity (distance <= half
// the LineSpread value).  Each group is averaged to its centroid.
procedure ClusterByColor(Src: IPixelSource; const Ctx: TDigitizerContext; Curve: TCurve);
var
  x, y, k, TotalH, LastProgress, CurrentProgress: Integer;
  BBox: TRect;
  N: array of Integer;
  Pi: TCurvePoint;
  Added: Boolean;
  Spread: Integer;
begin
  Curve.Clear;
  Spread := Max(1, Ctx.LineSpread);
  SetLength(N, 0);

  BBox := Ctx.Plot.Box.Rect[1.0];
  TotalH := Max(1, BBox.Bottom - BBox.Top);
  LastProgress := -1;

  for y := BBox.Top to BBox.Bottom do
  begin
    // Check for Cancel
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    // Report Progress
    if Assigned(Ctx.OnProgress) then
    begin
      CurrentProgress := Round(((y - BBox.Top)/TotalH)*100);
      if CurrentProgress <> LastProgress then
      begin
        Ctx.OnProgress(CurrentProgress, 'Detecting curve...');
        LastProgress := CurrentProgress;
      end;
    end;

    for x := BBox.Left to BBox.Right do
    begin
      if PixelBelongsToCurve(Src, x, y, Ctx) then
      begin
        Pi := TCurvePoint.Create(x, y);
        Added := False;

        for k := 0 to Curve.Count - 1 do
          if 2.0*Pi.DistanceTo(Curve.Point[k]) <= Spread then
          begin
            Curve.Point[k] := Curve.Point[k] + Pi;
            Inc(N[k]);
            Added := True;
            Break;
          end;

        if not Added then
        begin
          SetLength(N, Length(N) + 1);
          N[High(N)] := 1;
          Curve.AddPoint(Pi);
        end;
      end;
    end;
  end;

  for k := 0 to Curve.Count - 1 do
    if N[k] > 1 then
      Curve.Point[k] := Curve.Point[k]/N[k];

  SetLength(N, 0);
end;

{ =========================================================================== }
{ Curve extraction from a region bitmap (sub-pixel averaging)                 }
{ =========================================================================== }
procedure ExtractOrderedCurve(const Region: TVisitedMap; Curve: TCurve; const Ctx: TDigitizerContext);
var
  X, Y, dx, dy, nx, ny: Integer;
  SumX, SumY: Double;
  Count: Integer;
  WorkMap: TVisitedMap;
  TotalY, LastProgress, CurrentProgress: Integer;
begin
  Curve.Clear;
  if Length(Region) = 0 then Exit;

  TotalY := Max(1, High(Region));
  LastProgress := -1;

  if Ctx.LineSpread <= 0 then
  begin
    // No averaging: emit every set pixel
    for Y := 0 to High(Region) do
    begin
      if Assigned(Ctx.OnProgress) then
      begin
        CurrentProgress := Y*100 div TotalY;
        if CurrentProgress <> LastProgress then
        begin
          Ctx.OnProgress(CurrentProgress, 'Extracting curve...');
          LastProgress := CurrentProgress;
        end;
      end;

      for X := 0 to High(Region[Y]) do
        if Region[Y][X] then
          Curve.AddPoint(TCurvePoint.Create(X, Y));
    end;
  end
  else
  begin
    // Average over a (2·LineSpread + 1)^2 neighbourhood to get sub-pixel
    // centroids; mark pixels as used to avoid double-counting.
    SetLength(WorkMap, Length(Region));
    for Y := 0 to High(Region) do
      WorkMap[Y] := Copy(Region[Y]);

    for Y := 0 to High(WorkMap) do
    begin
      if Assigned(Ctx.OnProgress) then
      begin
        CurrentProgress := Y*100 div TotalY;
        if CurrentProgress <> LastProgress then
        begin
          Ctx.OnProgress(CurrentProgress, 'Extracting curve...');
          LastProgress := CurrentProgress;
        end;
      end;

      for X := 0 to High(WorkMap[Y]) do
        if WorkMap[Y][X] then
        begin
          SumX := 0; SumY := 0; Count := 0;
          for dy := -Ctx.LineSpread to Ctx.LineSpread do
            for dx := -Ctx.LineSpread to Ctx.LineSpread do
            begin
              nx := X + dx; ny := Y + dy;
              if (ny >= 0) and (ny <= High(WorkMap)) and
                 (nx >= 0) and (nx <= High(WorkMap[ny])) then
                if WorkMap[ny][nx] then
                begin
                  SumX := SumX + nx; SumY := SumY + ny;
                  Inc(Count);
                  WorkMap[ny][nx] := False;
                end;
            end;
          if Count > 0 then
            Curve.AddPoint(TCurvePoint.Create(SumX/Count, SumY/Count));
        end;
    end;
  end;

  Curve.SortCurve;
end;

{ =========================================================================== }
{ Private: non-recursive flood-fill for island operations                     }
{ =========================================================================== }
{ Fills Island with all connected pixels that pass PixelBelongsToCurve,
  starting from (Xi, Yi).  Already-visited pixels (Visited map) are skipped
  and new ones are marked as visited.

  JustInY = True:  only expand up/down (ignores horizontal neighbours).
  MoveUp  = False: do not expand toward lower Y (image-space "up").
  MoveDown= False: do not expand toward higher Y. }

procedure FillIslandAt(Src: IPixelSource; Xi, Yi: Integer; const Ctx: TDigitizerContext;
                       JustInY, MoveUp, MoveDown: Boolean; MaxPoints: Integer;
                       Island: TCurve; var Visited: TVisitedMap);
var
  StackX, StackY: array of Integer;
  Top, cx, cy,
  nx, ny, k: Integer;
  NDirs: Integer;
  DX: array[0..3] of Integer;
  DY: array[0..3] of Integer;
begin
  if not PixelBelongsToCurve(Src, Xi, Yi, Ctx) then Exit;
  if Visited[Yi][Xi] then Exit;

  // Build the direction table for this call
  if JustInY then
  begin
    NDirs := 2;
    DX[0] := 0; DY[0] := -1;   // up
    DX[1] := 0; DY[1] := 1;    // down
  end
  else
  begin
    NDirs := 4;
    DX[0] := 0; DY[0] := -1;   // up
    DX[1] := 0; DY[1] := 1;    // down
    DX[2] := -1; DY[2] := 0;   // left
    DX[3] := 1; DY[3] := 0;    // right
  end;

  SetLength(StackX, MaxPoints + 1);
  SetLength(StackY, MaxPoints + 1);
  Top := 0;
  StackX[0] := Xi;
  StackY[0] := Yi;
  Visited[Yi][Xi] := True;
  Island.AddPoint(TCurvePoint.Create(Xi, Yi));

  while (Top >= 0) and (Island.Count < MaxPoints) do
  begin
    if Assigned(Ctx.CheckTerminated) and Ctx.CheckTerminated() then Exit;

    cx := StackX[Top];
    cy := StackY[Top];
    Dec(Top);

    for k := 0 to NDirs - 1 do
    begin
      if (DY[k] < 0) and not MoveUp   then Continue;
      if (DY[k] > 0) and not MoveDown then Continue;

      nx := cx + DX[k];
      ny := cy + DY[k];

      if (nx < 0) or (ny < 0) or
         (nx >= Src.Width) or (ny >= Src.Height) then Continue;
      if Visited[ny][nx] then Continue;
      if not PixelBelongsToCurve(Src, nx, ny, Ctx) then Continue;

      Visited[ny][nx] := True;
      Island.AddPoint(TCurvePoint.Create(nx, ny));

      if Island.Count < MaxPoints then
      begin
        Inc(Top);
        if Top >= Length(StackX) then
        begin
          SetLength(StackX, Length(StackX)*2);
          SetLength(StackY, Length(StackY)*2);
        end;
        StackX[Top] := nx;
        StackY[Top] := ny;
      end;
    end;
  end;
end;

{ =========================================================================== }
{ AdjustDigitizedCurve                                                        }
{ =========================================================================== }
procedure AdjustDigitizedCurve(Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve; Noisy: Boolean = False);
const
  MinDiff   = 0.01;
  MaxIsland = 1000;
var
  Src: IPixelSource;
  Visited: TVisitedMap;
  NewCurve: TCurve;
  Island: TCurve;
  i, j: Integer;
  Pi, Prev,
  Next: TCurvePoint;
  Up, Down: Boolean;
  SumX,
  SumY: Double;
  h: Integer;
  LastProgress, CurrentProgress: Integer;
begin
  if Curve.Count = 0 then Exit;

  Src := TBGRABitmapSource.Create(Image, Ctx);

  // Allocate and zero the visited map
  h := Src.Height;
  SetLength(Visited, h);
  for i := 0 to h - 1 do
  begin
    SetLength(Visited[i], Src.Width);
    FillChar(Visited[i][0], Src.Width, 0);
  end;

  LastProgress := -1;
  CurrentProgress := -1;

  NewCurve := TCurve.Create;
  Island := TCurve.Create;
  try
    for i := 0 to Curve.Count - 1 do
    begin
      if Assigned(Ctx.OnProgress) then
      begin
        CurrentProgress := i*100 div Max(1, Curve.Count - 1);
        if CurrentProgress <> LastProgress then
        begin
          Ctx.OnProgress(CurrentProgress, 'Adjusting curve...');
          LastProgress := CurrentProgress;
        end;
      end;

      Pi := Curve.Point[i];

      // Skip if already claimed by a previous island
      if (Round(Pi.Y) >= 0) and (Round(Pi.Y) < Src.Height) and
         (Round(Pi.X) >= 0) and (Round(Pi.X) < Src.Width) and
         Visited[Round(Pi.Y)][Round(Pi.X)] then
        Continue;

      if Noisy then
      begin
        // Determine whether this point is a local vertical extremum and
        // restrict island growth to the appropriate direction(s).
        case i of
          0:
          begin
            if Curve.Count > 1 then
              Next := Curve.Point[1]
            else
              Next := Pi;
            Up := (MinDiff <= (Pi.Y - Next.Y)/Max(Abs(Pi.Y), 1e-9));
            Down := (-MinDiff >= (Pi.Y - Next.Y)/Max(Abs(Pi.Y), 1e-9));
          end;
          else
          begin
            Prev := Curve.Point[i - 1];
            if i < Curve.Count - 1 then
              Next := Curve.Point[i + 1]
            else
              Next := Prev;
            Up := (MinDiff <= (Pi.Y - Prev.Y)/Max(Abs(Pi.Y), 1e-9)) and
                    (MinDiff <= (Pi.Y - Next.Y)/Max(Abs(Pi.Y), 1e-9));
            Down := (-MinDiff >= (Pi.Y - Prev.Y)/Max(Abs(Pi.Y), 1e-9)) and
                    (-MinDiff >= (Pi.Y - Next.Y)/Max(Abs(Pi.Y), 1e-9));
          end;
        end;
        // If neither a max nor a min, allow expansion in both directions
        if not Up and not Down then
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
      FillIslandAt(Src, Round(Pi.X), Round(Pi.Y), Ctx, True, Up, Down,
                   MaxIsland, Island, Visited);

      if Island.Count > 0 then
      begin
        SumX := 0; SumY := 0;
        for j := 0 to Island.Count - 1 do
        begin
          SumX := SumX + Island.Point[j].X;
          SumY := SumY + Island.Point[j].Y;
        end;
        NewCurve.AddPoint(TCurvePoint.Create(SumX/Island.Count, SumY/Island.Count));
      end
      else
        NewCurve.AddPoint(Pi); // No island found: keep original
    end;

    // Replace curve contents with the adjusted points
    Curve.Clear;
    for i := 0 to NewCurve.Count - 1 do
      Curve.AddPoint(NewCurve.Point[i]);

  finally
    NewCurve.Free;
    Island.Free;
    SetLength(Visited, 0);
  end;
end;

{ =========================================================================== }
{ ConvertCurveToSymbolPoints                                                  }
{ =========================================================================== }
procedure ConvertCurveToSymbolPoints(Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve);
const
  MaxIsland = 1000;
var
  Src: IPixelSource;
  Visited: TVisitedMap;
  NewCurve: TCurve;
  Island: TCurve;
  i, j, h: Integer;
  Pi: TCurvePoint;
  SumX, SumY: Double;
  LastProgress, CurrentProgress: Integer;
begin
  if Curve.Count = 0 then Exit;

  Src := TBGRABitmapSource.Create(Image, Ctx);

  h := Src.Height;
  SetLength(Visited, h);
  for i := 0 to h - 1 do
  begin
    SetLength(Visited[i], Src.Width);
    FillChar(Visited[i][0], Src.Width, 0);
  end;

  LastProgress := -1;
  CurrentProgress := -1;

  NewCurve := TCurve.Create;
  Island := TCurve.Create;
  try
    for i := 0 to Curve.Count - 1 do
    begin
      if Assigned(Ctx.OnProgress) then
      begin
        CurrentProgress := i*100 div Max(1, Curve.Count - 1);
        if CurrentProgress <> LastProgress then
        begin
          Ctx.OnProgress(CurrentProgress, 'Converting curve to symbols...');
          LastProgress := CurrentProgress;
        end;
      end;

      Pi := Curve.Point[i];

      if (Round(Pi.Y) >= 0) and (Round(Pi.Y) < Src.Height) and
         (Round(Pi.X) >= 0) and (Round(Pi.X) < Src.Width) and
         Visited[Round(Pi.Y)][Round(Pi.X)] then
        Continue;

      Island.Clear;
      FillIslandAt(Src, Round(Pi.X), Round(Pi.Y), Ctx, False, True, True,
                   MaxIsland, Island, Visited);

      if Island.Count > 0 then
      begin
        SumX := 0; SumY := 0;
        for j := 0 to Island.Count - 1 do
        begin
          SumX := SumX + Island.Point[j].X;
          SumY := SumY + Island.Point[j].Y;
        end;
        NewCurve.AddPoint(TCurvePoint.Create(SumX/Island.Count, SumY/Island.Count));
      end;
    end;

    Curve.Clear;
    for i := 0 to NewCurve.Count - 1 do
      Curve.AddPoint(NewCurve.Point[i]);

  finally
    NewCurve.Free;
    Island.Free;
    SetLength(Visited, 0);
  end;
end;

{ =========================================================================== }
{ DigitizeCurve – main dispatch                                               }
{ =========================================================================== }
function DigitizeCurve(Seeds: TCurve; Image: TBGRABitmap; const Ctx: TDigitizerContext; Curve: TCurve): Boolean;
var
  Region: TVisitedMap;
  Src: IPixelSource;
  FirstSeed: TCurvePoint;
  TempSeeds: TCurve;
  i: Integer;
begin
  Result := False;
  if (Image = nil) or (Curve = nil) or
     (Seeds = nil) or (Seeds.Count = 0) then Exit;

  // Every algorithm gets a source that transparently composites the grid mask
  Src := TBGRABitmapSource.Create(Image, Ctx);

  // Ensure we select the correct left-most or right-most marker as the start
  TempSeeds := TCurve.Create;
  try
    for i := 0 to Seeds.Count - 1 do
      TempSeeds.AddPoint(Seeds.Point[i]);

    TempSeeds.SortCurve; // Sort ascending by X

    if Ctx.ScanDirection = sdForward then
      FirstSeed := TempSeeds.Point[0]
    else
      FirstSeed := TempSeeds.Point[TempSeeds.Count - 1];

    case Ctx.Mode of
      digLineFollowing:
        FollowLine(Src, TempSeeds, FirstSeed, Ctx, Curve);
      digLineTracing:
        begin
          GrowCurveRegion(Src, TempSeeds, Ctx, Region);
          ExtractOrderedCurve(Region, Curve, Ctx);
        end;
      digSymbolTracing:
        begin
          GrowSymbolRegion(Src, TempSeeds, Ctx, Region);
          ExtractOrderedCurve(Region, Curve, Ctx);
        end;
      digColorTracing:
        ClusterByColor(Src, Ctx, Curve);
    end;
  finally
    TempSeeds.Free;
  end;

  Result := Curve.Count > 0;
end;

end.
