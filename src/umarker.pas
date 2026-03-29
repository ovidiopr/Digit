unit umarker;

{$mode objfpc}{$H+}

{ ============================================================================
  umarker.pas  –  Marker bitmaps used to annotate the plot image
  ============================================================================

  Responsibilities:
    • TMarker       – a small overlay bitmap pinned to a pixel coordinate
    • TMarkerList   – an ownership list of TMarker instances
    • CreateMarker  – factory that renders a named symbol into a TBGRABitmap

  TMarker is deliberately UI-agnostic: it knows nothing about TPlotImage,
  zoom, or state.  Callers are responsible for coordinate translation.
  ============================================================================ }

interface

uses
  Classes, Fgl, Graphics, Types, BGRABitmap, BGRABitmapTypes, ucoordinates;

type
  { ------------------------------------------------------------------ }
  { TMarker                                                            }
  { ------------------------------------------------------------------ }
  TMarker = class
  protected
    FBitmap: TBGRABitmap;
    FRect: TRect;
    FPersistent: Boolean;
  private
    function GetPosition: TCurvePoint;
    procedure SetPosition(const Value: TCurvePoint);
  public
    { Create a marker whose centre lies at Coord.
      The marker takes ownership of Bitmap and frees it on Destroy.
      Persistent markers are never deleted during a ClearMarkers sweep –
      they represent fixed UI anchors such as axis reference points. }
    constructor Create(Bitmap: TBGRABitmap; Coord: TPoint;
      Persistent: Boolean = False);
    destructor Destroy; override;

    { Returns True when Point falls within the marker's bounding rect
      AND the pixel at that location is not transparent. }
    function HitTest(Point: TPoint): Boolean;

    { Blit the marker onto Img, but only if it intersects Rectangle. }
    procedure Draw(Img: TBGRABitmap; Rectangle: TRect);

    { Move the marker centre to Point. }
    procedure Move(Point: TPoint);

    { Shift the marker centre by Delta. }
    procedure Shift(Delta: TPoint);

    property Bitmap: TBGRABitmap read FBitmap;
    property Rect: TRect read FRect;
    property Position: TCurvePoint read GetPosition write SetPosition;
    property IsPersistent: Boolean read FPersistent;
  end;

  TMarkerList = specialize TFPGObjectList<TMarker>;

  { ------------------------------------------------------------------ }
  { Factory                                                            }
  { ------------------------------------------------------------------ }

{ Create a Size × Size BGRA bitmap containing the given Symbol.
  Supported symbols:
    'x'/'X'  – diagonal cross
    '+'         – orthogonal cross
    '0'         – circle with hairlines
    '1'         – square with hairlines
    'c'/'C'  – filled circle (inverted outline)
    'o'/'O'  – hollow circle
    'r'/'R'  – filled square (inverted outline)
    'q'/'Q'  – hollow square

  The caller owns the returned bitmap. }
function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor;
  LineWith: Integer = 3): TBGRABitmap;


implementation

{ ============================================================================
  CreateMarker
  ============================================================================ }

function CreateMarker(Size: TPoint; Symbol: Char; Color: TColor;
  LineWith: Integer = 3): TBGRABitmap;
begin
  Result := TBGRABitmap.Create(Size.X, Size.Y, BGRAPixelTransparent);

  with Result do
    case Symbol of
      'x', 'X':
      begin
        DrawLineAntialias(0, 0, Width - 1, Height - 1, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, Height - 1, Width - 1, 0, InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, 0, Width - 1, Height - 1, Color, LineWith);
        DrawLineAntialias(0, Height - 1, Width - 1, 0, Color, LineWith);
      end;

      '+':
      begin
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2,
          InvertColor(Color), LineWith + 2);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1,
          InvertColor(Color), LineWith + 2);
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, LineWith);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, LineWith);
      end;

      '0':
      begin
        EllipseAntialias(Width div 2, Height div 2,
          (Width - LineWith) div 2,
          (Height - LineWith) div 2,
          Color, LineWith);
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, 1);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, 1);
      end;

      '1':
      begin
        RectangleAntialias(1, 1, Width - 2, Height - 2, Color, LineWith);
        DrawLineAntialias(0, Height div 2, Width - 1, Height div 2, Color, 1);
        DrawLineAntialias(Width div 2, 0, Width div 2, Height - 1, Color, 1);
      end;

      'c', 'C':
        EllipseAntialias(Width div 2, Height div 2,
          (Width - LineWith) div 2,
          (Height - LineWith) div 2,
          clWhite - Color, LineWith, Color);

      'o', 'O':
        EllipseAntialias(Width div 2, Height div 2,
          (Width - LineWith) div 2,
          (Height - LineWith) div 2,
          Color, LineWith);

      'r', 'R':
        RectangleAntialias(1, 1, Width - 2, Height - 2,
          clWhite - Color, LineWith, Color);

      'q', 'Q':
        RectangleAntialias(1, 1, Width - 2, Height - 2, Color, LineWith);
    end;
end;

{ ============================================================================
  TMarker
  ============================================================================ }

constructor TMarker.Create(Bitmap: TBGRABitmap; Coord: TPoint;
  Persistent: Boolean = False);
var
  Delta: TPoint;
begin
  inherited Create;
  FBitmap := Bitmap;
  FPersistent := Persistent;
  Delta := TPoint.Create(Bitmap.Width div 2, Bitmap.Height div 2);
  FRect := TRect.Create(Coord - Delta, Bitmap.Width, Bitmap.Height);
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

end.
