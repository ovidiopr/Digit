# Digit

`Digit` is a [digitizer](https://en.wikipedia.org/wiki/Digitization), a tool to extract the original data from images representing plots. According to [Wikipedia](https://en.wikipedia.org/wiki/Digitizer_(disambiguation)), *a digitizer is a machine that converts an analog object, image or signal into a digital (i.e. computer-readable) format*. Quite often (e.g., in scientific papers), information is represented as plots, which are convenient for humans but useless for computers, which need the raw data. Unfortunately, both forms of representation are rarely available at the same time, so a tool is needed to extract data from plots. `Digit` is such a tool.

Digitization is a four-step process:

1. **Import the plot image**. `Digit` can accept most image formats (e.g., GIF, BMP, PNG, JPEG and TIFF), and the image can be loaded from a file or pasted from the clipboard.
2. **Define the axis system**. `Digit` can handle plots in Cartesian or polar coordinates, axes with linear, logarithmic, and reciprocal scales, and non-orthogonal axes. Additionally, `Digit` can deal with skewed, tilted or distorted plots, and multiple plots in one image.
3. **Digitize the data**. `Digit` has [several algorithms](ALGORITHMS.md) for automatic data digitization by detecting lines or colors and for manual digitization via markers that can be added with a mouse click.
4. **Export the data values**. In `Digit` the digitized data can be saved in CSV format or copied to the clipboard, to use them in any other application.

## Usage

`Digit`'s philosophy is to present the user with several tools with reasonable defaults, and get out of the way. For basic digitization, the only mandatory steps are to define (i) the scale and (ii) the curve color. In most cases this is sufficient, but in more complex cases some user tweaks may be required.

Among the options available to the user for digitization are *grid removal*, for the rare cases in which the grid interferes with the digitalization and the *plot box* (an area containing the data) definition, to avoid false detections. The most important options for processing the digitized data are *resampling*, to reduce the number of data, and *smoothing*, to remove the noise introduced by digitization. There are also tools to adjust digitized curves, convert them to scatter plots, or remove spurious data.

## Features

`Digit` has numerous features, among which we can mention:

* Can import most common image file formats, including GIF, BMP, PNG, JPEG and TIFF.
* Images can be pasted from the clipboard.
* Plots can be zoomed, cropped and rotated.
* Can handle skewed, tilted or distorted plots.
* Can handle multiple plots in one image.
* Can handle plots in Cartesian or polar coordinates.
* Can handle axes with linear, logarithmic, and reciprocal scales, and non-orthogonal axes.
* Tool for grid removal.
* Automatic digitization of line and scatter plots.
* Manual digitization via markers that can be added with a mouse click.
* Markers can be used also to guide the automatic digitization.
* Multiple data sets can be defined.
* Automatic sorting of data values.
* Tools for resampling and smoothing data values.
* Data can be exported as CSV or via clipboard.
* Digitizations are saved as XML files to facilitate interoperability with similar programs.
* Available for Windows, Mac, Linux, and can be compiled on any OS with support for [Lazarus](https://www.lazarus-ide.org/) and [FPC](https://www.freepascal.org/).

## Edition modes

Edition modes let the user interact directly with the digitized curve to correct or refine the data after digitization. The active edition mode is selected from the toolbar; only one mode can be active at a time.

### Add markers (`mdMarkers`)

In this mode a left-click on the image places a new marker at the clicked position. Markers serve a dual purpose: they are recorded as data points themselves, and they act as seed or guide points for the automatic digitization algorithms (see [ALGORITHMS.md](ALGORITHMS.md)). An existing marker can be repositioned by dragging it, and it can be removed with a right-click. This is the default edition mode and the one to use when manual digitization is needed.

### Select curve color (`mdColor`)

A left-click on any pixel of the image samples its color and sets it as the active curve color. The color is used by all automatic digitization algorithms to decide which pixels belong to the curve. A tolerance parameter (configured in the *Curve* panel, see below) controls how similar a pixel's color must be to the sampled value in order to be accepted. Clicking again simply resamples; no confirmation is required. This mode is typically used once at the beginning of a digitization session, or whenever the user switches to a different curve in the same image.

### Correct steps (`mdSteps`)

This mode is designed to deal with curves that contain a vertical discontinuity — a sudden jump in Y that is an artifact of the digitization rather than a true feature of the data. The user draws a vertical line on the image by clicking and dragging. All curve points to the right of that line are shifted vertically by the amount needed to make the curve continuous at the cut, while the points to the left remain unchanged. The shift is computed automatically from the difference between the last point on the left and the first point on the right of the cut. The operation is undoable.

### Correct segments (`mdSegments`)

Similar to `mdSteps`, but more surgical: the user draws a vertical cut and only the portion of the curve immediately adjacent to the cut is adjusted, leaving the rest of the curve in place on both sides. This is useful when a short segment of the curve has been digitized at the wrong vertical level and needs to be snapped into alignment with its neighbors without propagating any shift to the remainder of the data. The operation is undoable.

### Group points (`mdGroup`)

The user draws a selection rectangle by clicking and dragging. All curve points that fall inside the rectangle are replaced by a single point whose coordinates are the average of the group. This is useful for reducing noise or redundant points in a dense region of the curve — for example where the digitizer has produced a cluster of nearly identical points around a local feature. Points outside the rectangle are not affected. The operation is undoable.

### Delete points (`mdDelete`)

The user draws a selection rectangle by clicking and dragging. All curve points that fall inside the rectangle are removed from the curve. Points outside the rectangle are not affected. This is the fastest way to remove a region of spurious detections — such as a tick mark, a legend symbol, or a noise cluster — without touching the rest of the curve. The operation is undoable.

### Drag curve points (`mdDragPoints`)

When this mode is active, `Digit` overlays the digitized curve on the image and lets the user adjust it by clicking and dragging individual points. A small circle marks the point closest to the cursor, giving a visual preview of which point will be affected before the click.

When the user drags a point, the displacement is propagated to neighboring points using a Gaussian weighting based on their arc-length distance along the curve (i.e., distance measured along the rope, not in a straight line). This means that the influence falls off smoothly and naturally with distance, independently of whether the curve is densely or sparsely sampled: nearby points are pulled almost as much as the dragged one, while distant points are barely affected. The width of the Gaussian is configurable in the *Curve* panel.

Two modifier keys alter this behavior:

* **Shift**: Only the dragged point moves; all other points remain exactly in place. This is useful for fixing a single outlier without disturbing the surrounding curve.
* **Alt**: The dragged point (and its neighbors, weighted as usual) moves only along the Y axis (or the ρ axis in polar coordinates), keeping the X (or θ) coordinate fixed in plot space. This is useful for correcting the amplitude of a curve without shifting its phase or time axis.

The two modifiers can be combined: **Shift + Alt** moves only the dragged point, and only along Y/ρ.

Every drag operation is recorded as a single undo step, so it can be reversed cleanly regardless of how many points were affected.

---

## Configuration modes

Configuration modes expose the panels and controls used to set up the digitization. They are selected via the tab strip at the top of the side panel; unlike edition modes they do not change what the mouse does on the image, but they do control which overlay elements are visible.

### Curve configuration (`tsCurve`)

This panel collects all parameters that govern how the curve is detected. The key settings are:

- **Curve color**: The target color sampled with the `mdColor` edition mode (see above). A color swatch shows the current value and can also be edited directly.
- **Tolerance**: How much a pixel's color may differ from the target and still be accepted. A larger value makes detection more permissive but increases the risk of picking up background pixels; a smaller value is more selective but may miss anti-aliased or noisy edges.
- **Algorithm-specific parameters**: Step size, maximum gap, line spread, and similar settings whose meaning depends on the chosen algorithm (see [ALGORITHMS.md](ALGORITHMS.md)).

### Scale configuration (`tsScale`)

This panel is used to define the coordinate system of the plot. The user places three scale markers on the image — defining the origin, the X axis and the Y axis — and enters the corresponding data values. `Digit` computes the mapping between image pixels and plot coordinates from those three reference points. The available settings include:

- **Coordinate system**: Cartesian or polar.
- **Axis scale**: Linear, logarithmic, or reciprocal, independently for each axis.
- **Reference point values**: The data values assigned to each of the three scale markers.

Non-orthogonal, skewed, or tilted axes are handled automatically: `Digit` derives the full affine transform from the three marker positions, so the axes do not need to be aligned with the image edges.

### Plot box configuration (`tsBox`)

This panel controls the plot box — the quadrilateral region that delimits the data area of the image (see [README — Plot box](#plot-box) for the full interaction reference). In addition to cropping out labels and legends, the plot box has a second, important use: **correcting image skewness**. When a plot has been scanned at a slight angle, or photographed with perspective distortion, the four corners of the plot box can be placed precisely on the inner corners of the axis frame. `Digit` then uses the box geometry to straighten the coordinate mapping, so the extracted data values are correct even though the image itself is not perfectly aligned.

The panel provides numeric fields for the position of each corner, a color picker for the box overlay, and a toggle to enable or disable the box as a clipping region for the automatic algorithms.

### Grid removal (`tsGrid`)

This panel provides tools to suppress the grid lines that are frequently present in scientific plots. Grid removal works by creating a **grid mask**: a separate layer in which grid pixels are painted over with a neutral color so that they become invisible to the digitization algorithms. The mask is applied transparently — the original image is never modified.

The controls available in this panel are:

- **Grid color**: The color of the grid lines to remove, sampled from the image in the same way as the curve color.
- **Tolerance**: How liberally grid-colored pixels are identified.
- **Mask opacity**: How strongly the mask covers the underlying pixels, allowing partial suppression for grids that partially overlap the curve.
- **Preview toggle**: Shows the image with the mask applied so the user can verify that grid lines are suppressed without removing any part of the curve.

Grid removal is particularly important when using the Line Tracing or Symbol Tracing algorithms, which are sensitive to connected components: an un-masked grid line that crosses the curve can cause the algorithm to follow the grid instead of the curve.


## Plot box

The plot box is a quadrilateral region that tells `Digit` where the actual data area of the plot is. Defining it is optional but recommended whenever the image contains elements outside the data area (axis labels, legends, surrounding white space, etc.) that would otherwise confuse the automatic digitization algorithms.

The plot box is displayed as a filled, semi-transparent quadrilateral. Its color indicates its validity: green when the vertices are in a valid clockwise convex order, blue when they are in a valid counter-clockwise convex order, and red when the quadrilateral is not convex (which is not permitted for automatic digitization).

### Vertices and edges

The plot box is defined by four corner markers (vertices) and four midpoint markers (one per edge). Both types of marker can be dragged to reshape the box.

**Dragging an edge marker** translates the entire edge — and the two vertices at its ends — perpendicularly to the edge direction. The adjacent edges adjust automatically to keep the quadrilateral closed. The move is cancelled if it would push any vertex outside the image boundary or make the quadrilateral non-convex.

**Dragging a vertex marker** has two sub-modes depending on the modifier key held at the moment of the click:

* *No modifier (angle-preserving mode)* — the two edges adjacent to the dragged vertex are moved in a way that attempts to preserve the interior angles at the neighboring vertices. This keeps the overall shape of the box consistent and is the default behavior when the box is convex.
* *Shift (free move)* — the vertex is moved freely without any constraint on the neighboring vertices. The adjacent edges follow, but the neighboring vertices stay in place. This is also the only mode available when the box is not convex.

### Rotation

Holding **Alt** or **Ctrl** while clicking a vertex puts the box in rotation mode. Dragging the vertex then rotates the entire quadrilateral rigidly around its center, keeping all side lengths and interior angles unchanged. The rotation is applied only when the mouse button is released; if the rotated box would extend outside the image boundary, the rotation is cancelled and the box returns to its original orientation.

### Polar coordinates

When the active plot uses polar coordinates, the plot box is interpreted accordingly: it defines the region in polar space that contains the data, rather than a rectangular area of the image. The same vertex and edge controls apply, but the shape they define is mapped onto the polar grid of the plot.

