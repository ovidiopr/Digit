# Image viewer components and 2D-map support

Status: design, branch `componentes`. Tabbed multi-document UI is out of scope
(see "Deferred" at the end).

## 1. Goals

1. Turn `TPlotImage` into an installable Lazarus component (palette page `Digit`).
2. Split it into a generic image viewer plus a plot-specific subclass.
3. Add `TMapImage` for 2D images (maps, TEM/SEM micrographs) with:
   calibrated measurement, colour-bar value extraction, feature digitizing and
   particle/grain statistics.
4. Keep the existing plot workflow and project files working unchanged.

## 2. Class hierarchy and units

```
TCustomControl
 └─ TImageViewer          uimageviewer.pas   image, zoom, whiteboard, markers, selection, XML image
     ├─ TPlotImage         uplotimage.pas     plots, curves, axes, plot box, grid mask, digitizer
     └─ TMapImage          umapimage.pas      calibration, features, colour bar, particles
```

Support units (no LCL controls, testable on their own):

| Unit | Content |
|---|---|
| `umapscale.pas` | `TMapScale`: image px <-> map coordinates (scale bar or 3 reference points) |
| `umapfeatures.pas` | `TMapFeature`, `TMapFeatureList`: points, polylines, polygons, measurements |
| `ucolorbar.pas` | `TColorBar`: colour -> value LUT, CIELab nearest match |
| `uparticles.pas` | threshold, Otsu, morphology, labelling, per-particle statistics |
| `digitimagereg.pas` | `Register` procedure (palette page `Digit`) |

## 3. Package

* `components/digitimage.lpk`, type RunAndDesignTime, required by `src/digit.lpi`
  (with `DefaultFilename` pointing at the package so `lazbuild` finds it without installing).
* Units used by the viewers move from `src/` to `components/` with `git mv` (history kept):
  `ucoordinates, uutils, ucurves, upolygons, uscale, umarker, ugrid, ucurvedigitizer, uplotimage`.
  They are removed from `digit.lpi`, otherwise FPC would find each unit twice.
* Requires: `LCL`, `BGRABitmapPack`; numlib units (`typ, ipf, spe, inv`) come from FPC.
* `.github/workflows/release.yml` gains `lazbuild --add-package-link components/digitimage.lpk`.
* The main form keeps creating the viewer at run time; no change to `umain.lfm` is needed
  for the component step.

## 4. Member allocation (from the current 142 methods)

### 4.1 Moves to `TImageViewer`

Fields: `FOldCursor, InMouseMove, MouseMovePos, FRunningAction, FCancelAction, FZoom,
FOptions, FImageName, FPlotImg, FZoomImg, FWhiteBoard, FMarkers, FMarkerUnderCursor,
FActiveMarker, FClickedMarker, FClickedPoint, FClickedCoord, FDragging, FSelectingRegion,
FSelectionRect, FImageIsLoaded, FIsChanged, FLastProcessMessages`.

Events: `OnChange, OnPrintMessage, OnShowProgress, OnHideProgress, OnRegionSelected,
OnMarkerDragged, OnZoomChanged`.

Methods: `GetPixel (3), SetZoom, ResetZoomImage, ZoomOriginal, ZoomIn, ZoomOut, ZoomFitBest,
ResizeToZoom, LoadImage (2), PasteImage, SetImageName, GetZoomImage, AddMarker(TMarker),
RepaintMarker, DeleteMarker (2), DeleteMarkerUnderCursor, DeleteActiveMarker, ClearMarkers,
ClearMarker, DrawMarker, MoveMarker (2), RedrawMarkers, SetMarkerUnderCursor, SetActiveMarker,
SetDragging, SetSelectingRegion, RepaintRegion, RepaintAll, CheckCancelStatus, SetCancelAction,
GetIsChanged, SetIsChanged, Paint, Resize, MouseDown/Move/Up (generic part), Reset (generic part),
SaveToXML/LoadFromXML (frame + image element)`.

Property names (`PlotImg`, `ZoomImg`, `WhiteBoard`, ...) stay as they are so `umain.pas`
compiles without changes; `Image` is added as an alias of `PlotImg`.

### 4.2 Stays in `TPlotImage`

`FState, FDragAction, FGridMask, FMarkerList, FAxesMarkers, FBoxMarkers, FEdgeMarkers,
FPlots, FPlotIndex, FDigitThread, FStartPi, FCurrentDigitMode`, edition-mode/drag fields,
`OnStateChanged, OnActivePlot*, OnActiveCurve*`, and every plot/curve/axes/box/grid/digitize
method (`Digitize*, AdjustCurve, Smooth, Interpolate, CorrectCurve, RemoveGrid, UndistortImage,
Add/Delete/Move Plot and Curve, Undo/Redo, Axes/Box properties, ConvertCoords`, ...).

### 4.3 Virtual hooks in `TImageViewer`

| Hook | Default | `TPlotImage` override |
|---|---|---|
| `GetSourcePixel(X, Y)` | `PlotImg` pixel | grid mask over image |
| `OverlayZoomImage` | none | copies the grid mask |
| `ImageSizeChanged` | none | resizes and resets the grid mask |
| `ScaleContent(Factor)` (in `ResizeToZoom`) | none | scales, boxes, curve points, grid mask |
| `SyncMarkersToModel` / `SyncMarkersFromModel` | none | `UpdateMarkersInCurve/InImage` |
| `DrawContent(PaintRect)` (under markers) | none | state-dependent curve/axes/box |
| `DrawForeground(PaintRect)` (over markers) | none | hover dot |
| `MarkerRemoving(Marker, RealDelete)` | False | axes/box/curve marker bookkeeping |
| `RepaintForMarkerMove(Marker)` | repaint marker | full repaint with the virtual curve |
| `HandleMouseDown/Move/Up` | False | drag of curve points |
| `MarkerClicked`, `BeforeMarkerMove`, `AfterMarkerMove` | none | box drag actions, axes repaint |
| `MarkerDropped`, `MarkerDropDone` | none | scale points, box notifications, rotation |
| `MouseHover(X, Y)` | none | hover index |
| `CanStartRegion(Shift)`, `CanSelectRegion` | False, True | curve state and edition mode |
| `Reset` (virtual, called from `AfterConstruction`) | viewer fields | plots, state, drag fields |

XML reading and writing stays in `TPlotImage` for now; shared image-node helpers move
to `TImageViewer` when `TMapImage` needs them.

## 5. Project file

Root stays `<digitization version="2.0">`. The `<document>` node gets `Type="plot|map"`;
a missing attribute means `plot`, so all existing files load as before. `TMapImage`
writes a `<map>` node next to `<document>` (scale, colour bar, features, particle settings
and results). `TImageViewer.DocumentTypeOf(FileName)` reads only the attribute, so the main
form can create the right viewer before loading.

## 6. `TMapImage`

Coordinates are stored in image pixels (unzoomed), like curve points; markers live in
zoomed pixels and are rebuilt from the model, as in `TPlotImage`.

### 6.1 Calibration (`TMapScale`)

* Scale bar: two image points + length + unit; isotropic; origin at image top-left,
  Y down by default (option `YUp`).
* Reference points: three image points with map coordinates; affine with rotation and
  anisotropy, reusing `ChangeCoords` from `ucoordinates.pas`.
* From file: DigitalMicrograph files (`.dm3`) are read with LazScience `TSciDM3`
  (`uSciDM3`). `Axis[0]`, `Axis[1]` give per-axis origin, pixel size and units, so the
  map is calibrated on load: `x = (i - Ox) Sx`, `y = (j - Oy) Sy`. `SptUnits` gives the
  intensity units. The user can still recalibrate by hand.
* Uncalibrated images work in pixels.
* Lengths and areas are computed after transforming points to map coordinates,
  so anisotropic calibrations are handled correctly.

### 6.1.1 Raw intensity data

For DM3 files `TMapImage` keeps a `Single` raster with the raw values (counts, float),
besides the 8-bit display image. Line profiles, thresholds and histograms use the raw
values when present, and luminance otherwise. The project file stores the path of the
DM3 file and its calibration; the raw raster is re-read from that file when it exists,
otherwise the embedded PNG is used.

Dependencies: `digitimage.lpk` requires `LazScience`; `release.yml` clones
`github.com/ovidiopr/LazScience` and adds its package link.

### 6.2 Features and measurements (`TMapFeature`)

Kinds: point, polyline, polygon, distance, angle, profile. Each has name, colour,
points (image px) and is drawn on the whiteboard. Derived values: length, angle,
area (shoelace), perimeter, centroid. Undo/redo by snapshot of the feature list.
Export: CSV of features (one row per vertex) and of measurement summaries.

### 6.3 Line profile

Sampled along the polyline every image pixel with bilinear interpolation. Columns:
arc length (map units), x, y, luminance, R, G, B, and z when a colour bar is set.

### 6.4 Colour bar (`TColorBar`)

* User picks both ends of the bar and gives their values; linear or log scale.
* The bar is sampled (averaged across a small width) into a LUT of (CIELab, value).
* Pixel value = value of the nearest LUT entry; if dE exceeds the tolerance the result
  is NaN (text, axes, contour lines).
* Exact nearest-entry search with a last-colour memo (the planned 32x32x32 cache was
  dropped: its quantisation error was comparable to the colour steps of a bar).
* Outputs: z under cursor, z profile, z(x,y) grid over a region with chosen step (CSV).

### 6.5 Particles and grains (`uparticles.pas`)

1. Region (whole image or selection) -> luminance.
2. Threshold: manual or Otsu; dark or bright objects.
3. Optional opening/closing (radius in px) and hole filling.
4. 8-connected labelling; minimum area; optional exclusion of edge particles.
5. Per particle: centroid, area, equivalent diameter, perimeter (Moore tracing of the
   outer contour, Vossepoel-Smeulders estimator 0.980 ne + 1.406 no - 0.091 nc),
   max/min Feret (convex hull of the pixel corners, in map coordinates), aspect ratio,
   circularity 4 pi A / P^2. Perimeter in map units uses sqrt(pixel area), exact for
   isotropic calibrations.
6. Summary (count, mean, sd, median), size histogram, outlines on the whiteboard,
   per-particle CSV. Runs in the main thread with the existing progress/cancel events.

## 7. Main form (single document)

* `TDocKind = (dkPlot, dkMap)`. Exactly one of `PlotImage` / `MapImage` exists.
* New actions: New map, Open image as map. A Map menu and toolbar are created in code.
* Plot actions are disabled (and plot-only toolbars hidden) in map mode.
* Status bar: calibrated x, y (and z with a colour bar).
* Small dialogs: calibration input, colour-bar values, particle settings, results
  (grid + histogram, TAChart).

## 8. Using the map mode

* File > New map from image... (or drop a .dm3 file) opens an image or DM3 file as a map.
  DM3 files are calibrated from the file and keep their raw values.
* The side panel replaces the plot inputs: pick a tool, click on the image.
  Distance and angle finish by themselves; polylines, polygons and profiles finish with a
  double click, Enter or Map > Finish drawing; Esc cancels.
* Scale bar, reference points and colour bar ask for their values after the last click.
* Select / edit: click a feature to select it, drag its square markers to move vertices,
  Ctrl+Del deletes it. Undo/Redo in the Edit menu apply to features.
* Particle region: drag a rectangle to analyse it, or press Analyse image.
* Status bar: pixel position, map coordinates, intensity (raw for DM3) and z.
* Projects are saved as .dig with `<document Type="map">`.

Checked outside Pascal (Python prototypes of the same algorithms): affine reference
calibration and its inverse, colour-bar lookup on a synthetic viridis map (mean error 0.13 %
of the range, contour pixels rejected), contour tracing on 555 random components, and
perimeter/Feret values on discs, an ellipse and a square.

## 9. Deferred: tabbed documents

Later: shell form + `TBaseFrame` with `TPlotFrame` / `TMapFrame`. Notes for that work:
menu items must be re-pointed to the active frame's actions on every tab change, and
action lists of hidden frames must be disabled so their shortcuts and `OnUpdate`
handlers do not fire.
