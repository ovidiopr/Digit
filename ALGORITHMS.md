# Curve Digitization Algorithms

`Digit` provides four algorithms for extracting curve data from plot
images. Each one is suited to a different type of source image and curve style.
All algorithms share a common pipeline:

1. A **pixel acceptance predicate** tests each candidate pixel against a target
   colour (within a configurable tolerance) and confirms it lies inside the
   plot bounding box. When a grid mask is active, non-transparent mask pixels
   silently replace the underlying image pixels before any colour comparison,
   so grid lines are invisible to all algorithms.
2. The chosen **algorithm** traces or clusters the matching pixels.
3. An optional **post-processing** step refines the result (sub-pixel
   centroiding or symbol condensation).

---

## Algorithm 1 — Line Tracing (`digLineTracing`)

**Best for:** Continuous or near-continuous lines where the full connected
shape needs to be captured before ordering — for example anti-aliased curves,
thick lines, or lines with mild noise.

### How it works

**Phase 1 — Region growing (`GrowCurveRegion`)**

An iterative, stack-based 8-connected flood fill starts from one or more seed
pixels. At each step the algorithm pops a pixel from the stack, marks it as
visited, and pushes all unvisited 8-neighbours that pass the colour test.

When the fill reaches a dead end (no matching unvisited neighbour exists) and a
travel direction is known from the previous step, the algorithm attempts **gap
bridging**: it looks ahead along the last known direction for up to `MaxGap`
pixels, spreading perpendicularly by a small margin (1 pixel per 3 pixels of
distance) to account for slight curves. If a matching pixel is found within
that search cone it is added to the stack and the fill continues from there,
effectively jumping over gaps such as white grid lines or short interruptions
in the curve.

The result is a boolean 2-D region map: `True` at every pixel that belongs to
the curve.

**Phase 2 — Ordered extraction (`ExtractOrderedCurve`)**

The region map is converted to an ordered list of curve points:

- If `LineSpread` is zero, every set pixel is emitted directly.
- If `LineSpread` is positive, the map is scanned and each set pixel seeds a
  centroid average over a `(2·LineSpread + 1)²` neighbourhood. Pixels
  consumed by one centroid are cleared so they cannot contribute to another,
  giving sub-pixel accuracy for thick or anti-aliased lines.

The resulting points are sorted by X (or angle in polar mode) to produce a
left-to-right ordered curve.

### Key parameters

| Parameter | Effect |
|-----------|--------|
| `Tolerance` | How far a pixel's colour may deviate from the target |
| `MaxGap` | Maximum gap in pixels the fill will jump over |
| `LineSpread` | Half-width of the sub-pixel averaging neighbourhood (0 = off) |

---

## Algorithm 2 — Symbol Tracing (`digSymbolTracing`)

**Best for:** Scatter plots or charts where data points are represented by
discrete symbols (circles, squares, crosses, etc.) rather than a continuous
line.

### How it works

**Phase 1 — Symbol region growing (`GrowSymbolRegion`)**

Like Line Tracing, the algorithm begins with a stack-based 8-connected flood
fill from seed pixels. However, the fill is applied blob by blob rather than
as a single connected component:

1. A pixel is popped from the outer stack and its entire connected blob is
   flood-filled in an inner loop, accumulating the sum of X and Y coordinates
   of every pixel in the blob.
2. The **centroid** of the blob is computed from that sum.
3. From the centroid, the algorithm searches outward in expanding rings up to
   `MaxGap` pixels for the nearest unvisited matching pixel. That pixel becomes
   the seed for the next blob.

This blob-to-blob jumping ensures that each discrete symbol is captured as a
unit, and that the jump between symbols follows the natural reading order of
the data series.

**Phase 2 — Ordered extraction**

Identical to Line Tracing: the boolean region map produced by Phase 1 is
converted to sub-pixel centroids via `ExtractOrderedCurve` and sorted.

### Key parameters

| Parameter | Effect |
|-----------|--------|
| `Tolerance` | Colour matching tolerance |
| `MaxGap` | Maximum distance to search for the next symbol after a blob ends |
| `LineSpread` | Sub-pixel averaging half-width for the extraction phase |

---

## Algorithm 3 — Line Following (`digLineFollowing`)

**Best for:** Clean, thin, continuous lines without noise or thick strokes —
the classic case of a precisely drawn curve on a white background.

### How it works

The algorithm advances along the curve **one step at a time** rather than
growing a region first:

1. Starting from a seed point, it steps in the scan direction (left-to-right
   or right-to-left) by `Step` pixels along the curve's local normal direction.
   In polar coordinate mode the step is converted to an angular increment.
2. At each new X position (or angle), it searches **vertically** by up to
   `MaxGap` pixels above and below the current Y estimate, accumulating all
   matching pixels it finds.
3. The **sub-pixel centroid** of those matching pixels becomes the new curve
   point, and the algorithm steps forward again.

If guide markers are present (more than two seeds), the algorithm follows the
interpolated marker positions as X waypoints instead of stepping by a fixed
pixel interval, giving finer control over the sampling density in non-uniform
regions.

The algorithm supports both **Cartesian** (step along X) and **polar**
(step along angle) coordinate systems natively.

### Key parameters

| Parameter | Effect |
|-----------|--------|
| `Step` | Pixels per step along the scan direction (positive = left→right) |
| `MaxGap` | Vertical search radius at each step position |
| `Tolerance` | Colour matching tolerance |

---

## Algorithm 4 — Color Tracing (`digColorTracing`)

**Best for:** Situations where the curve colour is distinctive but the curve
itself has no clear geometric structure — for example a filled area, a band of
colour, or multiple disconnected patches that all belong to the same data
series.

### How it works

The algorithm performs a **full-image scan** followed by **greedy
nearest-neighbour clustering**:

1. Every pixel inside the plot bounding box is tested against the target
   colour. No seeds or connectivity are required.
2. Each matching pixel is compared to all cluster centres accumulated so far.
   If the pixel lies within `LineSpread / 2` pixels of an existing centre, its
   coordinates are added to that centre's running sum (accumulation). Otherwise
   a new cluster is started.
3. After the scan, each cluster centre is divided by its pixel count to produce
   the final centroid, which becomes a curve point.

This is equivalent to a single-pass approximate k-means where the cluster
radius is fixed rather than iteratively refined. The scan proceeds row by row,
so clusters tend to follow the natural reading order of the image.

No seeds are needed: the caller passes a dummy seed `(0, 0)` and the algorithm
ignores it.

### Key parameters

| Parameter | Effect |
|-----------|--------|
| `Tolerance` | Colour matching tolerance — typically needs to be generous for this mode |
| `LineSpread` | Clustering radius: pixels closer than `LineSpread / 2` are merged into one point |

---

## Post-processing

Two optional post-processing steps can be applied to the output of any
algorithm:

### `AdjustDigitizedCurve`

For each point in the curve, finds the connected island of same-coloured pixels
surrounding it and replaces the point with the island's 2-D centroid. A shared
visited map prevents any pixel from being counted twice. When `Noisy` mode is
enabled, island growth is restricted to the vertical direction of locally
detected extrema, suppressing noise spikes while preserving genuine curve peaks.

### `ConvertCurveToSymbolPoints`

Performs a full 4-connected flood fill from each curve point, collects all
pixels in the connected blob, and replaces the original point with the blob's
centroid. Designed to condense the output of Line Tracing or Line Following
into discrete symbol centres when the digitized curve turns out to represent a
scatter plot rather than a continuous line.

---

## Choosing an algorithm

| Situation | Recommended algorithm |
|-----------|-----------------------|
| Thin, clean, continuous line | Line Following |
| Thick, anti-aliased, or noisy continuous line | Line Tracing |
| Scatter plot with symbol markers | Symbol Tracing |
| Filled area or colour band without clear geometry | Color Tracing |
| Continuous line that needs to become discrete points | Line Following or Line Tracing + `ConvertCurveToSymbolPoints` |
