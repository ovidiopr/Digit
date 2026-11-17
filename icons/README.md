# Icons

SVG sources of the image list `ImageList` (TExtSVGImageList) in `src/umain.lfm`.
The file prefix is the image index; menus and action lists draw them at 16 px, the
toolbars at 32 px (`ImagesWidth = 32`).

To change an icon, edit the file and use Replace in the image list editor (double click
`ImageList` in the main form). New icons go at the end so existing indexes stay valid.

Sources: `new` icons were drawn for Digit; the others come from the GIMP and eXperience
sets in IconImages, with editor metadata removed.

| Index | File | Used by | Source |
|---|---|---|---|
| 0 | `00_new.svg` | FileNew | new |
| 1 | `01_open.svg` | FileOpen, tsPlot | new |
| 2 | `02_save.svg` | FileSave, LogSave, sep01, tsLog | new |
| 3 | `03_save_as.svg` | FileSaveAs | new |
| 4 | `04_import.svg` | FileImportDigit | new |
| 5 | `05_load_image.svg` | FileImage | new |
| 6 | `06_export.svg` | FileExport, PlotExport | new |
| 7 | `07_exit.svg` | FileExit | new |
| 8 | `08_undo.svg` | EditUndo | new |
| 9 | `09_redo.svg` | EditRedo | new |
| 10 | `10_copy.svg` | EditCopyCurve | new |
| 11 | `11_preferences-system.svg` | ToolPlotOptions | IconImages/GIMP/preferences-system.svg |
| 12 | `12_gimp-cursor.svg` | ModeCursor | IconImages/GIMP/gimp-cursor.svg |
| 13 | `13_add_markers.svg` | ModeMarkers | new |
| 14 | `14_gtk-select-color.svg` | ToolDigitColorTracing | IconImages/GIMP/gtk-select-color.svg |
| 15 | `15_correct_steps.svg` | ModeSteps | new |
| 16 | `16_correct_segment.svg` | ModeSegment | new |
| 17 | `17_line_following.svg` | DigitizeFromHereItem, ToolDigitLineFollowing, ToolDigitizeItem, tsCurve | new |
| 18 | `18_adjust_curve.svg` | ToolAdjustCurve | new |
| 19 | `19_digitize_markers.svg` | ToolDigitMarkers, sep04 | new |
| 20 | `20_smooth.svg` | EditShowLine, ToolSmooth | new |
| 21 | `21_symbols.svg` | EditShowSymbols, ToolConvertToSymbols | new |
| 22 | `22_edit-delete.svg` | MarkersDelete, ToolClear | IconImages/GIMP/edit-delete.svg |
| 23 | `23_up.svg` | MarkersMoveUp, ToolCurveUp | new |
| 24 | `24_down.svg` | MarkersMoveDown, ToolCurveDown | new |
| 25 | `25_add_item.svg` | ToolCurveAdd, ToolPlotAdd | new |
| 26 | `26_delete_item.svg` | ToolCurveDelete, ToolPlotDelete | new |
| 27 | `27_gtk-edit.svg` | ToolCurveName, ToolPlotName | IconImages/GIMP/gtk-edit.svg |
| 28 | `28_about.svg` | HelpAbout | new |
| 29 | `29_set_scale.svg` | PlotScale | new |
| 30 | `30_left.svg` | MarkersMoveLeft, ToolCurveLeft | new |
| 31 | `31_right.svg` | MarkersMoveRight, ToolCurveRight | new |
| 32 | `32_arrow_right.svg` | (not used) | new |
| 33 | `33_arrow_left.svg` | (not used) | new |
| 34 | `34_gimp-tool-zoom.svg` | (not used) | IconImages/GIMP/gimp-tool-zoom.svg |
| 35 | `35_gimp-histogram.svg` | (not used) | IconImages/GIMP/gimp-histogram.svg |
| 36 | `36_group_points.svg` | ModeGroupPoints | new |
| 37 | `37_gimp-tool-eraser.svg` | ModeDeletePoints | IconImages/GIMP/gimp-tool-eraser.svg |
| 38 | `38_grid.svg` | GridRemoval, tsGrid | new |
| 39 | `39_gtk-ok.svg` | GridShowHide, GridShowHide (set in code) | IconImages/GIMP/gtk-ok.svg |
| 40 | `40_process-stop.svg` | ToolCancelAction, GridShowHide (set in code) | IconImages/GIMP/process-stop.svg |
| 41 | `41_scale_points.svg` | ToolCorrectDistortion, tsScale | new |
| 42 | `42_gimp-tool-rect-select.svg` | ToolResetBox, tsBox | IconImages/GIMP/gimp-tool-rect-select.svg |
| 43 | `43_gimp-tool-color-picker.svg` | ModeBackgroundColor, ModeColor, ModeMajorGridColor, ModeMinorGridColor, btnBackgroundColor, btnMajorGridColor, btnMinorGridColor | IconImages/GIMP/gimp-tool-color-picker.svg |
| 44 | `44_image_edit.svg` | (not used) | new |
| 45 | `45_gtk-paste.svg` | EditPasteImage | IconImages/eXperience/normal/stock/gtk-paste.svg |
| 46 | `46_page_ruler.svg` | (not used) | new |
| 47 | `47_drafting.svg` | (not used) | new |
| 48 | `48_bsplines.svg` | ToolBSplines, ToolResampleItem | new |
| 49 | `49_splines.svg` | ToolSplines | new |
| 50 | `50_linear.svg` | ToolLinear | new |
| 51 | `51_polynomial.svg` | ToolPolynomial | new |
| 52 | `52_polynomial2.svg` | (not used) | new |
| 53 | `53_noisy.svg` | ToolAdjustNoise | new |
| 54 | `54_zoom-original.svg` | EditZoomOriginal | IconImages/GIMP/zoom-original.svg |
| 55 | `55_zoom-in.svg` | EditZoomIn | IconImages/GIMP/zoom-in.svg |
| 56 | `56_zoom-out.svg` | EditZoomOut | IconImages/GIMP/zoom-out.svg |
| 57 | `57_zoom-fit-best.svg` | EditZoomFit | IconImages/GIMP/zoom-fit-best.svg |
| 58 | `58_gimp-resize.svg` | EditZoomResize | IconImages/gimp-resize.svg |
| 59 | `59_merge-mask.svg` | GridMergeMask | IconImages/merge-mask.svg |
| 60 | `60_edit-clear.svg` | LogClear | IconImages/GIMP/edit-clear.svg |
| 61 | `61_line_tracing.svg` | ToolDigitLineTracing | new |
| 62 | `62_symbol_tracing.svg` | ToolDigitSymbolTracing | new |
| 63 | `63_gimp-path.svg` | ModeDragPoints | IconImages/GIMP/gimp-path.svg |
| 64 | `64_edit-delete.svg` | (not used) | IconImages/GIMP/edit-delete.svg |
| 65 | `65_gimp-tool-pencil.svg` | (not used) | IconImages/GIMP/gimp-tool-pencil.svg |
| 66 | `66_media-record.svg` | (not used) | IconImages/GIMP/media-record.svg |
| 67 | `67_select_region.svg` | (not used) | new |
| 68 | `68_map_select.svg` | Map tool: Select / edit | new |
| 69 | `69_map_scale_bar.svg` | Map tool: Scale bar | new |
| 70 | `70_map_reference.svg` | Map tool: Reference points | new |
| 71 | `71_map_point.svg` | Map tool: Point | new |
| 72 | `72_map_polyline.svg` | Map tool: Polyline | new |
| 73 | `73_map_polygon.svg` | Map tool: Polygon (area) | new |
| 74 | `74_map_distance.svg` | Map tool: Distance | new |
| 75 | `75_map_angle.svg` | Map tool: Angle | new |
| 76 | `76_map_profile.svg` | Map tool: Line profile | new |
| 77 | `77_map_color_bar.svg` | Map tool: Colour bar | new |
| 78 | `78_map_particles.svg` | Map tool: Particle region | new |
