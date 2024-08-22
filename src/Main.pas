unit Main;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
  {$DEFINE TIFF_CLIPBOARD_FORMAT}
{$ELSE}
  {$DEFINE BMP_CLIPBOARD_FORMAT}
  {$DEFINE PNG_CLIPBOARD_FORMAT}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, ExtCtrls, ComCtrls, TATypes, TASeries, TAGraph,
  ClipBrd, ActnList, ValEdit, Spin, ExtDlgs, MaskEdit, BCTrackbarUpdown,
  FileUtil, restore, TAChartUtils, uchartscale, IniFiles, BGRABitmapTypes,
  GraphType, Grids, utils, coordinates, curves, plotimage;

type
  TMouseMode = (mdCursor, mdMarkers, mdColor, mdSteps, mdSegments,
                mdGroup, mdDelete, mdMajorGridColor, mdMinorGridColor,
                mdBackgroundColor);

  { TDigitMainForm }
  TDigitMainForm = class(TForm)
    DigitizeColorItem: TMenuItem;
    ToolDigitizeItem: TMenuItem;
    ToolDigitColorItem: TMenuItem;
    ToolDigitColor: TAction;
    tbResetBox: TToolButton;
    ToolResetBox: TAction;
    ToolCorrectDistortion: TAction;
    tbBox: TToolBar;
    tbCorrectDistortion: TToolButton;
    ToolAdjustNoise: TAction;
    BSplinesItem: TMenuItem;
    btnAdjustNoise: TToolButton;
    ToolSplinesItem: TMenuItem;
    ToolBSplinesItem: TMenuItem;
    ResampleMenu: TPopupMenu;
    SplinesItem: TMenuItem;
    ToolSplines: TAction;
    DigitizeLineItem: TMenuItem;
    DigitizeMarkersItem: TMenuItem;
    DigitMainItem: TMenuItem;
    ToolDigitMarkersItem: TMenuItem;
    ToolDigitMarkers: TAction;
    chbRebuildCurve: TCheckBox;
    EditPasteImage: TAction;
    btnBackground: TColorButton;
    btnBackgroundColor: TSpeedButton;
    btnColor: TColorButton;
    btnMajorGrid: TColorButton;
    btnMajorGridColor: TSpeedButton;
    btnMinorGrid: TColorButton;
    btnMinorGridColor: TSpeedButton;
    btnAddCurve: TToolButton;
    btnAdjustCurve: TToolButton;
    btnClear: TToolButton;
    btnDelCurve: TToolButton;
    btnDigitize: TToolButton;
    btnEditName: TToolButton;
    btnExportPlot: TToolButton;
    btnMoveDown: TToolButton;
    btnMoveUp: TToolButton;
    btnSetScale: TToolButton;
    cbbCoords: TComboBox;
    cbbXScale: TComboBox;
    cbbYScale: TComboBox;
    EditIX1: TBCTrackbarUpdown;
    EditIX2: TBCTrackbarUpdown;
    EditIX3: TBCTrackbarUpdown;
    EditIY1: TBCTrackbarUpdown;
    EditIY2: TBCTrackbarUpdown;
    EditIY3: TBCTrackbarUpdown;
    EditPX1: TEdit;
    EditPX2: TEdit;
    EditPX3: TEdit;
    EditPY1: TEdit;
    EditPY2: TEdit;
    EditPY3: TEdit;
    EditVX1: TBCTrackbarUpdown;
    EditVX2: TBCTrackbarUpdown;
    EditVX3: TBCTrackbarUpdown;
    EditVX4: TBCTrackbarUpdown;
    EditVY1: TBCTrackbarUpdown;
    EditVY2: TBCTrackbarUpdown;
    EditVY3: TBCTrackbarUpdown;
    EditVY4: TBCTrackbarUpdown;
    edtGridThreshold: TBCTrackbarUpdown;
    edtGridMask: TBCTrackbarUpdown;
    edtGridTolerance: TBCTrackbarUpdown;
    edtInterval: TBCTrackbarUpdown;
    edtSpread: TBCTrackbarUpdown;
    edtStep: TBCTrackbarUpdown;
    edtTolerance: TBCTrackbarUpdown;
    edtX: TEdit;
    edtY: TEdit;
    gbCoord: TGroupBox;
    gbPoint1: TGroupBox;
    gbPoint2: TGroupBox;
    gbPoint3: TGroupBox;
    gbVertex1: TGroupBox;
    gbVertex2: TGroupBox;
    gbVertex3: TGroupBox;
    gbVertex4: TGroupBox;
    gbX: TGroupBox;
    gbY: TGroupBox;
    lblBackground: TLabel;
    lblColor: TLabel;
    lblGridThreshold: TLabel;
    lblGridMask: TLabel;
    lblGridTolerance: TLabel;
    lblImg1: TLabel;
    lblImg2: TLabel;
    lblImg3: TLabel;
    lblInterval: TLabel;
    lblMajorGrid: TLabel;
    lblMinorGrid: TLabel;
    lblPlt1: TLabel;
    lblPlt2: TLabel;
    lblPlt3: TLabel;
    lblSpace1: TLabel;
    lblSpace2: TLabel;
    lblSpace3: TLabel;
    lblSpread: TLabel;
    lblStep: TLabel;
    lblTolerance: TLabel;
    lblVX1: TLabel;
    lblVX2: TLabel;
    lblVX3: TLabel;
    lblVX4: TLabel;
    lblVY1: TLabel;
    lblVY2: TLabel;
    lblVY3: TLabel;
    lblVY4: TLabel;
    lblX: TLabel;
    lblX1: TLabel;
    lblX2: TLabel;
    lblX3: TLabel;
    lblXScale: TLabel;
    lblY: TLabel;
    lblY1: TLabel;
    lblY2: TLabel;
    lblY3: TLabel;
    lblYScale: TLabel;
    MainPlot: TChart;
    EditPasteImageItem: TMenuItem;
    N7: TMenuItem;
    ModeBackgroundColor: TAction;
    ModeMinorGridColor: TAction;
    ModeMajorGridColor: TAction;
    GridShowHide: TAction;
    GridRemoval: TAction;
    ModeDeletePointsItem: TMenuItem;
    ModeGroupPointsItem: TMenuItem;
    ModeDeletePoints: TAction;
    ModeGroupPoints: TAction;
    MarkerUpItem: TMenuItem;
    MarkerDownItem: TMenuItem;
    MarkerLeftItem: TMenuItem;
    MarkerRightItem: TMenuItem;
    MarkerDelItem: TMenuItem;
    N6: TMenuItem;
    btnGroupPointsmode: TToolButton;
    btnDeletePointsMode: TToolButton;
    MainPanel: TPanel;
    PageControl: TPageControl;
    pcInput: TPageControl;
    DigitizeMenu: TPopupMenu;
    rgDirection: TRadioGroup;
    ScrollBox: TScrollBox;
    sbScale: TScrollBox;
    sbPlotBox: TScrollBox;
    sbCurve: TScrollBox;
    sbGrid: TScrollBox;
    sep07: TToolButton;
    SpeedButton1: TSpeedButton;
    BtnPasteImage: TToolButton;
    tsPlotBox: TTabSheet;
    tsScale: TTabSheet;
    tbPlot: TToolBar;
    tbGrid: TToolBar;
    tbRemoveGrid: TToolButton;
    tbShowHideGrid: TToolButton;
    tcCurves: TTabControl;
    sep08: TToolButton;
    tsGrid: TTabSheet;
    tsCurve: TTabSheet;
    ToolRightItem: TMenuItem;
    ToolLeftItem: TMenuItem;
    ToolCurveRight: TAction;
    ToolCurveLeft: TAction;
    MarkersMenu: TMenuItem;
    tsPicture: TTabSheet;
    tsPlot: TTabSheet;
    ZoomImage: TImage;
    MarkersDelete: TAction;
    MarkersMoveRight: TAction;
    MarkersMoveLeft: TAction;
    MarkersMoveDown: TAction;
    MarkersMoveUp: TAction;
    btnColorMode: TToolButton;
    btnCursorMode: TToolButton;
    btnMarkersMode: TToolButton;
    btnResample: TToolButton;
    btnSegmentMode: TToolButton;
    btnSmooth: TToolButton;
    btnStepsMode: TToolButton;
    btnSymbols: TToolButton;
    FileImportDigit: TAction;
    EditCopyCurve: TAction;
    EditCopyItem: TMenuItem;
    FileImportDigitItem: TMenuItem;
    N5: TMenuItem;
    btnCopy: TToolButton;
    btnImport: TToolButton;
    sep02: TToolButton;
    sep03: TToolButton;
    ToolScaleOptionsItem: TMenuItem;
    N4: TMenuItem;
    sep06: TToolButton;
    ToolScaleOptions: TAction;
    LargeImageList: TImageList;
    CurveMenu: TMenuItem;
    CurveAddItem: TMenuItem;
    CurveDelItem: TMenuItem;
    CurveNameItem: TMenuItem;
    ChartScaleItem: TMenuItem;
    ChartExportItem: TMenuItem;
    PlotMenu: TMenuItem;
    sep05: TToolButton;
    btnAbout: TToolButton;
    btnScaleOptions: TToolButton;
    ToolSymbolsItem: TMenuItem;
    ToolResampleItem: TMenuItem;
    N3: TMenuItem;
    ToolAdjustItem: TMenuItem;
    ModeSegmentItem: TMenuItem;
    ModeStepItem: TMenuItem;
    ModeColorItem: TMenuItem;
    ModeMarkersItem: TMenuItem;
    ModeCursorItem: TMenuItem;
    ModeMenu: TMenuItem;
    ModeSegment: TAction;
    PlotExport: TAction;
    PlotScale: TAction;
    SavePlotDlg: TSaveDialog;
    ToolConvertToSymbols: TAction;
    ToolAdjustCurve: TAction;
    seXo: TFloatSpinEdit;
    seXf: TFloatSpinEdit;
    lblXInterval: TLabel;
    lblXInterval1: TLabel;
    OpenPictureDlg: TOpenPictureDialog;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    ToolBSplines: TAction;
    seInterpPoints: TSpinEdit;
    seSGKernel: TSpinEdit;
    gbSmooth: TGroupBox;
    gbResample: TGroupBox;
    gbData: TGroupBox;
    lblSmoothDegree: TLabel;
    lblBSPoints: TLabel;
    lblSmoothKernel: TLabel;
    InputPanel: TPanel;
    LeftSplitter: TSplitter;
    ModeSteps: TAction;
    ModeColor: TAction;
    ModeMarkers: TAction;
    ModeCursor: TAction;
    pnlData: TPanel;
    seSGDegree: TSpinEdit;
    RightSplitter: TSplitter;
    ToolCurveName: TAction;
    ToolCurveDelete: TAction;
    ToolCurveAdd: TAction;
    DigitizeFromHereItem: TMenuItem;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    DigitPopupMenu: TPopupMenu;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    Edit1: TMenuItem;
    EditUndoItem: TMenuItem;
    ActionList: TActionList;
    FileNew: TAction;
    FileSave: TAction;
    FileExit: TAction;
    FileOpen: TAction;
    FileSaveAs: TAction;
    HelpAbout: TAction;
    tbMain: TToolBar;
    BtnOpen: TToolButton;
    BtnSave: TToolButton;
    sep01: TToolButton;
    BtnUndo: TToolButton;
    BtnNew: TToolButton;
    ImageList: TImageList;
    EditUndo: TAction;
    DigitMenu: TMenuItem;
    ToolDigitLine: TAction;
    ToolDigitLineItem: TMenuItem;
    ToolSmooth: TAction;
    sep04: TToolButton;
    BtnImage: TToolButton;
    BtnExport: TToolButton;
    FileImage: TAction;
    FileExport: TAction;
    N2: TMenuItem;
    FileLoadImageItem: TMenuItem;
    FileExportItem: TMenuItem;
    ToolCurveUp: TAction;
    ToolCurveDown: TAction;
    ToolClear: TAction;
    ToolSmoothItem: TMenuItem;
    ToolUpItem: TMenuItem;
    ToolDownItem: TMenuItem;
    ToolDeleteItem: TMenuItem;
    EditRedo: TAction;
    BtnRedo: TToolButton;
    EditRedoItem: TMenuItem;
    SaveDataDlg: TSaveDialog;
    OpenProjectDlg: TOpenDialog;
    SaveProjectDlg: TSaveDialog;
    leData: TValueListEditor;
    procedure btnBackgroundColorChanged(Sender: TObject);
    procedure btnMajorGridColorChanged(Sender: TObject);
    procedure btnMinorGridColorChanged(Sender: TObject);
    procedure cbbCoordsChange(Sender: TObject);
    procedure cbbXScaleChange(Sender: TObject);
    procedure cbbYScaleChange(Sender: TObject);
    procedure chbRebuildCurveChange(Sender: TObject);
    procedure DigitizeFromHereItemClick(Sender: TObject);
    procedure EditCopyCurveExecute(Sender: TObject);
    procedure EditIX1Change(Sender: TObject; AByUser: boolean);
    procedure EditIY1Change(Sender: TObject; AByUser: boolean);
    procedure EditPasteImageExecute(Sender: TObject);
    procedure EditPX1EditingDone(Sender: TObject);
    procedure EditPY1EditingDone(Sender: TObject);
    procedure EditVX1Change(Sender: TObject; AByUser: boolean);
    procedure EditVY1Change(Sender: TObject; AByUser: boolean);
    procedure edtGridMaskChange(Sender: TObject; AByUser: boolean);
    procedure edtGridThresholdChange(Sender: TObject; AByUser: boolean);
    procedure edtGridToleranceChange(Sender: TObject; AByUser: boolean);
    procedure edtXEditingDone(Sender: TObject);
    procedure edtYEditingDone(Sender: TObject);
    procedure FileImportDigitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileImageExecute(Sender: TObject);
    procedure FileExportExecute(Sender: TObject);
    procedure gbResampleResize(Sender: TObject);
    procedure GridRemovalExecute(Sender: TObject);
    procedure GridShowHideExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure leDataValidateEntry(Sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure ModeBackgroundColorExecute(Sender: TObject);
    procedure ModeMajorGridColorExecute(Sender: TObject);
    procedure ModeMinorGridColorExecute(Sender: TObject);
    procedure pcInputChange(Sender: TObject);
    procedure PlotImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlotImageMouseLeave(Sender: TObject);
    procedure PlotImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PlotImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlotImageResize(Sender: TObject);
    procedure PlotImageChange(Sender: TObject);
    procedure PlotImageShowProgress(Sender: TObject; Progress: Cardinal);
    procedure PlotImageHideProgress(Sender: TObject);
    procedure PlotImageRegionSelected(Sender: TObject; RegionRect: TRect);
    procedure PlotImageStateChanged(Sender: TObject; NewState: TPlotImageState);
    procedure PlotImageMarkerDragged(Sender: TObject; Marker: TMarker; Zoom: Boolean);
    procedure InputPanelResize(Sender: TObject);
    procedure MainPlotMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MarkersDeleteExecute(Sender: TObject);
    procedure MarkersMoveDownExecute(Sender: TObject);
    procedure MarkersMoveLeftExecute(Sender: TObject);
    procedure MarkersMoveRightExecute(Sender: TObject);
    procedure MarkersMoveUpExecute(Sender: TObject);
    procedure ModeColorExecute(Sender: TObject);
    procedure ModeCursorExecute(Sender: TObject);
    procedure ModeSegmentExecute(Sender: TObject);
    procedure ModeMarkersExecute(Sender: TObject);
    procedure ModeStepsExecute(Sender: TObject);
    procedure ModeDeletePointsExecute(Sender: TObject);
    procedure ModeGroupPointsExecute(Sender: TObject);
    procedure PlotExportExecute(Sender: TObject);
    procedure PlotScaleExecute(Sender: TObject);
    procedure tcCurvesChange(Sender: TObject);
    procedure ToolAdjustCurveExecute(Sender: TObject);
    procedure ToolAdjustNoiseExecute(Sender: TObject);
    procedure ToolConvertToSymbolsExecute(Sender: TObject);
    procedure ToolCorrectDistortionExecute(Sender: TObject);
    procedure ToolCurveAddExecute(Sender: TObject);
    procedure ToolCurveDeleteExecute(Sender: TObject);
    procedure ToolCurveLeftExecute(Sender: TObject);
    procedure ToolCurveNameExecute(Sender: TObject);
    procedure ToolCurveRightExecute(Sender: TObject);
    procedure ToolDigitColorExecute(Sender: TObject);
    procedure ToolDigitLineExecute(Sender: TObject);
    procedure ToolDigitMarkersExecute(Sender: TObject);
    procedure ToolBSplinesExecute(Sender: TObject);
    procedure ToolResetBoxExecute(Sender: TObject);
    procedure ToolScaleOptionsExecute(Sender: TObject);
    procedure ToolSmoothExecute(Sender: TObject);
    procedure ToolCurveUpExecute(Sender: TObject);
    procedure ToolCurveDownExecute(Sender: TObject);
    procedure ToolClearExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure ToolSplinesExecute(Sender: TObject);
  private
    { Private declarations }
    FIsSaved: Boolean;

    PlotImage: TPlotImage;

    function CheckSaveStatus: Boolean;

    function GetIsSaved: Boolean;
    function GetCurveCount: Integer;
    function GetManualZoom: Boolean;
    function GetZoomXmin: Double;
    function GetZoomXmax: Double;
    function GetZoomYmin: Double;
    function GetZoomYmax: Double;
    function GetIniName: TFileName;

    function GetCoords: TCoordSystem;
    function GetXScale: TScaleType;
    function GetYScale: TScaleType;
    function GetXLabel: String;
    function GetYLabel: String;
    function GetImagePoint(Index: Integer): TCurvePoint;
    function GetPlotPoint(Index: Integer): TCurvePoint;

    procedure SetDigitFileName(Value: TFileName);
    procedure SetIsSaved(Value: Boolean); overload;
    procedure SetIsSaved(Value, UpdateTable: Boolean); overload;
    procedure SetMouseMode(Value: TMouseMode);
    procedure SetCurveCount(Value: Integer);
    procedure SetManualZoom(Value: Boolean);
    procedure SetZoomXmin(Value: Double);
    procedure SetZoomXmax(Value: Double);
    procedure SetZoomYmin(Value: Double);
    procedure SetZoomYmax(Value: Double);

    procedure SetCoords(Value: TCoordSystem);
    procedure SetXScale(Value: TScaleType);
    procedure SetYScale(Value: TScaleType);
    procedure SetXLabel(Value: String);
    procedure SetYLabel(Value: String);
    procedure SetImagePoint(Index: Integer; const Value: TCurvePoint);
    procedure SetPlotPoint(Index: Integer; const Value: TCurvePoint);

    procedure SavePreferences;
    procedure RestorePreferences;

    procedure UpdateZoomImage(X, Y: Integer); overload;
    procedure UpdateZoomImage(P: TPoint); overload;
    procedure ClearZoomImage;

    procedure CurveToGUI;
    procedure GUIToCurve;
  public
    { Public declarations }
    TmpPoint: TPoint;
    FMouseMode: TMouseMode;
    FDigitFileName: TFileName;
    ExportFileName: TFileName;
    function XLine(Y, Shift: Integer): Integer;

    procedure OpenProject(FileName: TFileName);
    procedure SaveProject(FileName: TFileName);
    procedure ImportProject(FileName: TFileName);
    procedure InsertImage(FileName: TFileName);
    procedure Initialize;

    procedure UpdateControls;
    procedure PlotCurve;
    procedure UpdateValues;
    procedure UpdateView;

    procedure SavePlot(FName: TFileName; FType: Integer);

    property DigitFileName: TFileName read FDigitFileName write SetDigitFileName;
    property IsSaved: Boolean read GetIsSaved write SetIsSaved;
    property MouseMode: TMouseMode read FMouseMode write SetMouseMode;
    property CurveCount: Integer read GetCurveCount write SetCurveCount;
    property ManualZoom: Boolean read GetManualZoom write SetManualZoom;
    property ZoomXmin: Double read GetZoomXmin write SetZoomXmin;
    property ZoomXmax: Double read GetZoomXmax write SetZoomXmax;
    property ZoomYmin: Double read GetZoomYmin write SetZoomYmin;
    property ZoomYmax: Double read GetZoomYmax write SetZoomYmax;

    property CoordSystem: TCoordSystem read GetCoords write SetCoords;
    property XScale: TScaleType read GetXScale write SetXScale;
    property YScale: TScaleType read GetYScale write SetYScale;
    property XLabel: String read GetXLabel write SetXLabel;
    property YLabel: String read GetYLabel write SetYLabel;
    property ImagePoint[Index: Integer]: TCurvePoint read GetImagePoint write SetImagePoint;
    property PlotPoint[Index: Integer]: TCurvePoint read GetPlotPoint write SetPlotPoint;
  end;

const
  AppFullName = 'Digit';

var
  DigitMainForm: TDigitMainForm;
  MustOpen: Boolean;
  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  tiffClipboardFormat: TClipboardFormat;
  {$ENDIF}
  {$IFDEF PNG_CLIPBOARD_FORMAT}
  pngClipboardFormat: TClipboardFormat;
  {$ENDIF}

implementation

{$R *.lfm}

uses About, uoptions;

//Beginning of general functions


function ColorDelta(C1, C2: LongInt): Real;
begin
  Result := Sqrt((C1 - C2)*(C1 - C2));
end;

function TDigitMainForm.XLine(Y, Shift: Integer): Integer;
begin
  Result := Round(PlotImage.Scale.ImagePoint[2].X + Shift + (Y - PlotImage.Scale.ImagePoint[2].Y)*
                  (PlotImage.Scale.ImagePoint[1].X - PlotImage.Scale.ImagePoint[2].X)/
                  (PlotImage.Scale.ImagePoint[1].Y - PlotImage.Scale.ImagePoint[2].Y));
end;

procedure TDigitMainForm.UpdateControls;
begin
  with PlotImage do
  begin
    //pcInput.Enabled := ImageIsLoaded;
    tsCurve.Enabled := ImageIsLoaded;
    tsScale.Enabled := ImageIsLoaded;
    tsPlotBox.Enabled := ImageIsLoaded;
    tsGrid.Enabled := ImageIsLoaded;

    FileSave.Enabled :=  ImageIsLoaded and (not IsSaved);
    FileSaveAs.Enabled := ImageIsLoaded;

    FileExport.Enabled := HasPoints;

    ModeMarkers.Enabled := ImageIsLoaded and (State = piSetCurve);
    ModeColor.Enabled := ImageIsLoaded and (State = piSetCurve);
    ModeSteps.Enabled := (State = piSetCurve) and HasPoints;
    ModeSegment.Enabled := (State = piSetCurve) and HasPoints;
    ModeGroupPoints.Enabled := (State = piSetCurve) and HasPoints;
    ModeDeletePoints.Enabled := (State = piSetCurve) and HasPoints;
    ModeMajorGridColor.Enabled := ImageIsLoaded and (State = piSetGrid);
    ModeMinorGridColor.Enabled := ImageIsLoaded and (State = piSetGrid);
    ModeBackgroundColor.Enabled := ImageIsLoaded and (State = piSetGrid);

    ToolDigitLine.Enabled := Scale.IsValid and ColorIsSet and (State = piSetCurve);
    ToolDigitColor.Enabled := Scale.IsValid and ColorIsSet and (State = piSetCurve);
    ToolDigitMarkers.Enabled := ToolDigitLine.Enabled and (Markers.Count > 0);
    ToolAdjustCurve.Enabled := (State = piSetCurve) and HasPoints;
    ToolAdjustNoise.Enabled := (State = piSetCurve) and HasPoints;
    ToolBSplines.Enabled := (State = piSetCurve) and HasPoints;
    ToolSplines.Enabled := (State = piSetCurve) and HasPoints;
    ToolSmooth.Enabled := (State = piSetCurve) and HasPoints;
    ToolConvertToSymbols.Enabled := (State = piSetCurve) and HasPoints;
    ToolCurveUp.Enabled := (State = piSetCurve) and HasPoints;
    ToolCurveDown.Enabled := (State = piSetCurve) and HasPoints;
    ToolCurveLeft.Enabled := (State = piSetCurve) and HasPoints;
    ToolCurveRight.Enabled := (State = piSetCurve) and HasPoints;
    ToolClear.Enabled := (State = piSetCurve) and HasPoints;

    ToolCurveAdd.Enabled := ImageIsLoaded and (State = piSetCurve);
    ToolCurveDelete.Enabled := ImageIsLoaded and (State = piSetCurve) and (Count > 1);
    ToolCurveName.Enabled := ImageIsLoaded and (State = piSetCurve) and (Count > 0);

    ToolScaleOptions.Enabled := ImageIsLoaded;

    MarkersMoveUp.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveDown.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveLeft.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveRight.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersDelete.Enabled := ImageIsLoaded and assigned(ActiveMarker) and not ActiveMarker.IsPersistent;

    ToolCorrectDistortion.Enabled := ImageIsLoaded and (State = piSetPlotBox) and PlotImage.PlotBox.IsConvex;
    ToolResetBox.Enabled := ImageIsLoaded and (State = piSetPlotBox);

    GridRemoval.Enabled := ImageIsLoaded and (State = piSetGrid) and not (GridMask.IsValid and GridMask.IsActive);
    GridShowHide.Enabled := ImageIsLoaded and (State = piSetGrid) and GridMask.IsValid;

    PlotExport.Enabled := (State = piSetCurve) and HasPoints;
    PlotScale.Enabled := (State = piSetCurve) and HasPoints;

    EditUndo.Enabled := (State = piSetCurve) and CanUndo;
    EditRedo.Enabled := (State = piSetCurve) and CanRedo;
    EditCopyCurve.Enabled := (State = piSetCurve) and HasPoints;
  end;
end;

procedure TDigitMainForm.PlotCurve;
var
  i, j: Integer;
  PtCv: TCurve;
begin
  //Draw the plot in the Chart
  for i := 0 to PlotImage.Count - 1 do
  begin
    with TLineSeries(MainPlot.Series.Items[i]) do
    begin
      Clear;
      if (PlotImage.Scale.IsValid and PlotImage.Curves[i].HasPoints) then
      begin
        SeriesColor := PlotImage.Curves[i].Color;
        Pointer.Style := psCircle;
        Pointer.Brush.Color := PlotImage.Curves[i].Color;
        Pointer.Pen.Color := clBlack;
        ShowLines := not PlotImage.Curves[i].ShowAsSymbols;
        ShowPoints := PlotImage.Curves[i].ShowAsSymbols;
        try
          PtCv := PlotImage.PlotCurves[i];
          for j := 0 to PtCv.Count - 1 do
            AddXY(PtCv.X[j], PtCv.Y[j]);
        finally
          PtCv.Free;
        end;
      end;
    end;
  end;
end;

procedure TDigitMainForm.UpdateValues;
var
  i: Integer;
begin
  // Avoid loops
  leData.OnValidateEntry := Nil;
  leData.EditorMode:=False;

  // Copy the values to the table
  leData.Strings.Clear;
  with PlotImage do
  begin
    if (Scale.IsValid and HasPoints) then
      for i := 0 to NumPoints - 1 do
        leData.InsertRow(Format('%.5g', [Point[i].X]),
                         Format('%.5g', [Point[i].Y]), True);
  end;
  leData.Row := 0;

  // Restore data validation
  leData.OnValidateEntry := @leDataValidateEntry;
end;

procedure TDigitMainForm.SavePlot(FName: TFileName; FType: Integer);
begin
  case FType of
    1: MainPlot.SaveToFile(TPortableNetworkGraphic, ChangeFileExt(FName, '.png')); // PNG - our default
    2: MainPlot.SaveToFile(TJPEGImage, ChangeFileExt(FName, '.jpg')); // JPEG
    3: MainPlot.SaveToFile(TPixmap, ChangeFileExt(FName, '.xpm')); // XPM
    4: MainPlot.SaveToFile(TPortableAnyMapGraphic, ChangeFileExt(FName, '.ppm')); // PPM
    5: MainPlot.SaveToBitmapFile(ChangeFileExt(FName, '.bmp')); // bitmap
  end;
end;

procedure TDigitMainForm.UpdateView;
begin
  //The image is visible, update it first
  if (PageControl.ActivePageIndex = 0) then
    Application.ProcessMessages;

  PlotCurve;
  UpdateValues;
end;

procedure TDigitMainForm.OpenProject(FileName: TFileName);
begin
  Initialize;
  if PlotImage.LoadFromXML(FileName, OpenPictureDlg) then
  begin
    leData.TitleCaptions[0] := PlotImage.Scale.XLabel;
    leData.TitleCaptions[1] := PlotImage.Scale.YLabel;
    leData.Invalidate;
    MainPlot.BottomAxis.Title.Caption := PlotImage.Scale.XLabel;
    MainPlot.LeftAxis.Title.Caption := PlotImage.Scale.YLabel;

    if PlotImage.ImageIsLoaded then
    begin
      // Update the limits in the relevant controls
      EditIX1.MaxValue := PlotImage.Width - 1;
      EditIY1.MaxValue := PlotImage.Height - 1;
      EditIX2.MaxValue := PlotImage.Width - 1;
      EditIY2.MaxValue := PlotImage.Height - 1;
      EditIX3.MaxValue := PlotImage.Width - 1;
      EditIY3.MaxValue := PlotImage.Height - 1;

      EditVX1.MaxValue := PlotImage.Width - 1;
      EditVY1.MaxValue := PlotImage.Height - 1;
      EditVX2.MaxValue := PlotImage.Width - 1;
      EditVY2.MaxValue := PlotImage.Height - 1;
      EditVX3.MaxValue := PlotImage.Width - 1;
      EditVY3.MaxValue := PlotImage.Height - 1;
      EditVX4.MaxValue := PlotImage.Width - 1;
      EditVY4.MaxValue := PlotImage.Height - 1;
    end;
    CurveToGUI;

    DigitFileName := FileName;
    IsSaved := True;
  end;
end;

procedure TDigitMainForm.SaveProject(FileName: TFileName);
begin
  //Save digitization
  if PlotImage.SaveToXML(FileName) then
  begin
    DigitFileName := FileName;
    IsSaved := True;
  end;
end;

procedure TDigitMainForm.ImportProject(FileName: TFileName);
var
  F: TextFile;
  S: String;
  C: Char;
  Co: TColor;
  i: Integer;
  ImageName: TFileName;
  ImageLoaded,
  SavedPoints: Boolean;
  X, Y: Double;
begin
  //Check that the file exists
  if not FileExists(FileName) then
  begin
    MessageDlg('The file ' + ExtractFileName(FileName) + ' doesn''t exists.', mtError, [mbOk], 0);
    Exit;
  end;

  //Load digitization
  PlotImage.Reset;
  ImageLoaded := False;

  //Start loading file
  AssignFile(F, FileName);
  Reset(F);
  Readln(F, S);
  ImageName := S;
  Readln(F, S);
  PlotImage.Scale.XLabel := S;
  Readln(F, S);
  PlotImage.Scale.YLabel := S;
  Read(F, C);
  PlotImage.ImageIsLoaded := C = '1';
  Read(F, C);
  //PlotImage.PointIsSet[1] := C = '1';
  Read(F, C);
  //PlotImage.PointIsSet[2] := C = '1';
  Read(F, C);
  //PlotImage.PointIsSet[3] := C = '1';
  Read(F, C);
  //PlotImage.ColorIsSet := C = '1';
  Readln(F, C);
  SavedPoints := C = '1';
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.ImagePoint[1] := TCurvePoint.Create(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.ImagePoint[2] := TCurvePoint.Create(X, Y);
  Read(F, X);  Readln(F, Y);
  PlotImage.Scale.ImagePoint[3] := TCurvePoint.Create(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.PlotPoint[1] := TCurvePoint.Create(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.PlotPoint[2] := TCurvePoint.Create(X, Y);
  Read(F, X);  Readln(F, Y);
  PlotImage.Scale.PlotPoint[3] := TCurvePoint.Create(X, Y);
  Readln(F, Co);
  btnColor.ButtonColor := Co;
  PlotImage.DigitCurve.Color := Co;
  PlotImage.Curve.Clear;
  if SavedPoints then
  begin
    Readln(F, X);
    while not EOF(F) do
    begin
      Readln(F, Y);
      PlotImage.Curve.AddPoint(X, Y);
      X := X + 1;
    end;
  end;
  CloseFile(F);
  //Now load Image and draw curve
  if PlotImage.ImageIsLoaded then
  begin
    if FileExists(ImageName) then
    begin
      PlotImage.ImageName := ImageName;
    end
    else
      if FileExists(ExtractFilePath(FileName) + ExtractFileName(ImageName)) then
      begin
        PlotImage.ImageName := ExtractFilePath(FileName) + ExtractFileName(ImageName);
      end
      else
      begin
        OpenPictureDlg.InitialDir := ExtractFilePath(FileName);
        if OpenPictureDlg.Execute then
          PlotImage.ImageName := OpenPictureDlg.FileName;
      end;
  end;

  if PlotImage.ImageIsLoaded then
    with PlotImage do
    begin
      AxesPoint[1] := Scale.ImagePoint[1];
      AxesPoint[2] := Scale.ImagePoint[2];
      AxesPoint[3] := Scale.ImagePoint[3];
    end;

  IsSaved := False;
end;

procedure TDigitMainForm.InsertImage(FileName: TFileName);
var
  ResetPoints: Boolean;
begin
  ResetPoints := not (PlotImage.ImageIsLoaded and FileExists(DigitFileName));

  PlotImage.State := piSetCurve;
  PlotImage.ImageName := FileName;

  if PlotImage.ImageIsLoaded then
    PlotImage.SetPlotPointMarkers(ResetPoints);

  if not FileExists(DigitFileName) then
    DigitFileName := ChangeFileExt(FileName, '.dig');

  IsSaved := False;
end;

procedure TDigitMainForm.Initialize;
begin
  //Initiate everything to create a new digitization
  PlotImage.ImageName := '';
  PlotImage.Visible := False;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
  StatusBar.Panels[3].Text := '';
  MouseMode := mdCursor;
  ModeCursor.Checked := True;
  seSGKernel.Value := 5;
  seSGDegree.Value := 3;
  seInterpPoints.Value := 101;
  seXo.Value := 0;
  ClearZoomImage;
  btnColor.ButtonColor := clBtnFace;
  rgDirection.ItemIndex := 0;
  DigitFileName := 'noname.dig';
  ExportFileName := '';
  PlotImage.Reset;
  IsSaved := True;
  leData.Strings.Clear;
  pcInput.ActivePageIndex := Integer(PlotImage.State);

  case PlotImage.Options.DefaultItp of
    itpSpline:
      btnResample.Action := ToolSplines;
    else
      btnResample.Action := ToolBSplines;
  end;

  case PlotImage.Options.DefaultDig of
    digColor:
      btnDigitize.Action := ToolDigitColor;
    digMarkers:
      btnDigitize.Action := ToolDigitMarkers;
    else
      btnDigitize.Action := ToolDigitLine;
  end;
end;
//End of general functions

//Beginning of form functions
procedure TDigitMainForm.FormActivate(Sender: TObject);
begin
  MustOpen := True;
  UpdateControls;
  pcInput.ActivePageIndex := Integer(PlotImage.State);

  InputPanelResize(Self);
end;

procedure TDigitMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //Verify that the file has been saved before closing
  CanClose := CheckSaveStatus;
end;

procedure TDigitMainForm.DigitizeFromHereItemClick(Sender: TObject);
begin
  if PlotImage.Scale.IsValid then
  begin
    GUIToCurve;

    PlotImage.DigitizeSpectrum(TmpPoint);

    CurveToGUI;
  end;
end;

procedure TDigitMainForm.cbbCoordsChange(Sender: TObject);
begin
  case TCoordSystem(cbbCoords.ItemIndex) of
    csCartesian: begin
      gbX.Caption := 'X:';
      lblX1.Caption := 'X:';
      lblX2.Caption := 'X:';
      lblX3.Caption := 'X:';
      cbbXScale.Items.Strings[3] := 'Inverse (1/X)';
      gbY.Caption := 'Y:';
      lblY1.Caption := 'Y:';
      lblY2.Caption := 'Y:';
      lblY3.Caption := 'Y:';
      cbbYScale.Items.Strings[3] := 'Inverse (1/Y)';
    end;
    csPolar: begin
      gbX.Caption := 'θ:';
      lblX1.Caption := 'θ:';
      lblX2.Caption := 'θ:';
      lblX3.Caption := 'θ:';
      cbbXScale.Items.Strings[3] := 'Inverse (1/θ)';
      gbY.Caption := 'ρ:';
      lblY1.Caption := 'ρ:';
      lblY2.Caption := 'ρ:';
      lblY3.Caption := 'ρ:';
      cbbYScale.Items.Strings[3] := 'Inverse (1/ρ)';
    end;
  end;

  if (PlotImage.Scale.CoordSystem <> TCoordSystem(cbbCoords.ItemIndex)) then
  begin
    PlotImage.Scale.CoordSystem := TCoordSystem(cbbCoords.ItemIndex);
    PlotImage.PlotBox.PolarCoordinates := (PlotImage.Scale.CoordSystem = csPolar);
  end;
end;

procedure TDigitMainForm.btnMajorGridColorChanged(Sender: TObject);
begin
  if (PlotImage.GridMask.MajorGridColor <> btnMajorGrid.ButtonColor) then
  begin
    PlotImage.GridMask.MajorGridColor := btnMajorGrid.ButtonColor;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.btnBackgroundColorChanged(Sender: TObject);
begin
  if (PlotImage.GridMask.BckgndColor <> btnBackground.ButtonColor) then
  begin
    PlotImage.GridMask.BckgndColor := btnBackground.ButtonColor;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.btnMinorGridColorChanged(Sender: TObject);
begin
  if (PlotImage.GridMask.MinorGridColor <> btnMinorGrid.ButtonColor) then
  begin
    PlotImage.GridMask.MinorGridColor := btnMinorGrid.ButtonColor;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.cbbXScaleChange(Sender: TObject);
begin
  if (PlotImage.Scale.XScale <> TScaleType(cbbXScale.ItemIndex)) then
    PlotImage.Scale.XScale := TScaleType(cbbXScale.ItemIndex);
end;

procedure TDigitMainForm.cbbYScaleChange(Sender: TObject);
begin
  if (PlotImage.Scale.YScale <> TScaleType(cbbYScale.ItemIndex)) then
    PlotImage.Scale.YScale := TScaleType(cbbYScale.ItemIndex);
end;

procedure TDigitMainForm.chbRebuildCurveChange(Sender: TObject);
begin
  if (PlotImage.GridMask.FixCurve <> chbRebuildCurve.Checked) then
  begin
    PlotImage.GridMask.FixCurve := chbRebuildCurve.Checked;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.EditCopyCurveExecute(Sender: TObject);
begin
  leData.CopyToClipboard(False);
end;

procedure TDigitMainForm.EditIX1Change(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
    with TBCTrackbarUpdown(Sender) do
      PlotImage.AxesPoint[Tag] := ImagePoint[Tag];
end;

procedure TDigitMainForm.EditIY1Change(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
    with TBCTrackbarUpdown(Sender) do
      PlotImage.AxesPoint[Tag] := ImagePoint[Tag];
end;

procedure TDigitMainForm.EditPasteImageExecute(Sender: TObject);
var
  i: integer;
  ResetPoints: Boolean;

  function GetImgFromClipboard: Boolean;
  var
    Stream: TMemoryStream;
  begin
    Result := False;

    Stream := TMemoryStream.Create;
    Clipboard.GetFormat(Clipboard.Formats[i], Stream);
    Stream.Position := 0;
    try
      Result := True;

      ResetPoints := not PlotImage.ImageIsLoaded;

      with PlotImage do
      begin
        if ImageIsLoaded and
           (MessageDlg('You are about to replace the current plot image.' +
                       ' This action cannot be undone. Continue?',
                      mtWarning, [mbYes, mbNo], 0) = mrNo) then Exit;

        PasteImage(Stream);
        SetPlotPointMarkers(ResetPoints);
      end;
    except
      on ex: Exception do
        Result := False;
    end;
    Stream.Free;
  end;

begin
  {$IFDEF TIFF_CLIPBOARD_FORMAT}
  for i := 0 to Clipboard.FormatCount - 1 do
    if Clipboard.Formats[i] = tiffClipboardFormat then
      if GetImgFromClipboard then Exit;
  {$ENDIF}

  {$IFDEF PNG_CLIPBOARD_FORMAT}
  for i := 0 to Clipboard.FormatCount - 1 do
    if Clipboard.Formats[i] = pngClipboardFormat then
      if GetImgFromClipboard then Exit;
  {$ENDIF}

  for i := 0 to Clipboard.FormatCount - 1 do
    if (Clipboard.Formats[i] = PredefinedClipboardFormat(pcfBitmap)) then
      if GetImgFromClipboard then Exit;
end;

procedure TDigitMainForm.EditPX1EditingDone(Sender: TObject);
var
  Value: Double;
begin
  with TEdit(Sender) do
  begin
    if TryStrToFloat(Text, Value) and
       (Value <> PlotImage.Scale.PlotPoint[Tag].X) then
    begin
      PlotImage.Scale.PlotPoint[Tag] := PlotPoint[Tag];
      PlotImage.IsChanged := True;
    end
    else
      Text := FloatToStr(PlotImage.Scale.PlotPoint[Tag].X);
  end;
end;

procedure TDigitMainForm.EditPY1EditingDone(Sender: TObject);
var
  Value: Double;
begin
  with TEdit(Sender) do
  begin
    if TryStrToFloat(Text, Value) and
       (Value <> PlotImage.Scale.PlotPoint[Tag].Y) then
    begin
      PlotImage.Scale.PlotPoint[Tag] := PlotPoint[Tag];
      PlotImage.IsChanged := True;
    end
    else
      Text := FloatToStr(PlotImage.Scale.PlotPoint[Tag].Y);
  end;
end;

procedure TDigitMainForm.EditVX1Change(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
    with TBCTrackbarUpdown(Sender) do
      PlotImage.BoxVertex[Tag] := TCurvePoint.Create(Value, PlotImage.BoxVertex[Tag].Y);
end;

procedure TDigitMainForm.EditVY1Change(Sender: TObject; AByUser: boolean);
begin
  if AByUser then
    with TBCTrackbarUpdown(Sender) do
      PlotImage.BoxVertex[Tag] := TCurvePoint.Create(PlotImage.BoxVertex[Tag].X, Value);
end;

procedure TDigitMainForm.edtGridMaskChange(Sender: TObject; AByUser: boolean);
begin
  if AByUser and (PlotImage.GridMask.MaskSize <> edtGridMask.Value) then
  begin
    PlotImage.GridMask.MaskSize := edtGridMask.Value;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.edtGridThresholdChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser and (PlotImage.GridMask.Threshold <> edtGridThreshold.Value/100) then
  begin
    PlotImage.GridMask.Threshold := edtGridThreshold.Value/100;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.edtGridToleranceChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser and (PlotImage.GridMask.Tolerance <> edtGridTolerance.Value) then
  begin
    PlotImage.GridMask.Tolerance := edtGridTolerance.Value;
    PlotImage.IsChanged := True;
  end;
end;

procedure TDigitMainForm.edtXEditingDone(Sender: TObject);
begin
  if (PlotImage.Scale.XLabel <> edtX.Text) then
  begin
    PlotImage.Scale.XLabel := edtX.Text;
    leData.TitleCaptions[0] := edtX.Text;
    leData.Invalidate;

    MainPlot.BottomAxis.Title.Caption := edtX.Text;
  end;
end;

procedure TDigitMainForm.edtYEditingDone(Sender: TObject);
begin
  if (PlotImage.Scale.YLabel <> edtY.Text) then
  begin
    PlotImage.Scale.YLabel := edtY.Text;
    leData.TitleCaptions[1] := edtY.Text;
    leData.Invalidate;

    MainPlot.LeftAxis.Title.Caption := edtY.Text;
  end;
end;

procedure TDigitMainForm.FileImportDigitExecute(Sender: TObject);
var
  FileName: TFileName;
begin
  //Load digitization
  if OpenProjectDlg.Execute then
  begin
    FileName := OpenProjectDlg.FileName;
    ImportProject(FileName);
    DigitFileName := FileName;
  end;
end;

procedure TDigitMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  //Open dragged files (only the first one)
  if (Length(FileNames) > 0) then
  begin
    //Verify that the open digitization has been saved
    if CheckSaveStatus then
    begin
      if (UpperCase(ExtractFileExt(FileNames[0])) = '.DIG') then
      begin
        // It is a digitization file, open it
        OpenProject(FileNames[0]);
      end
      else
      begin
        // It should be an image, try to insert it
        Initialize;
        InsertImage(FileNames[0]);
      end;
    end;
  end;
end;

procedure TDigitMainForm.FormShow(Sender: TObject);
var
  i     : Byte;
  S     : String;
begin
  if MustOpen then
  begin
    S := '';
    for i := 1 to ParamCount do
    begin
      S := S + ParamStr(i);
      if UpperCase(Copy(ParamStr(i), Length(ParamStr(i)) - 3, 1)) = '.' then
      begin
        if FileExists(S)then
        begin
          OpenProject(S);
          S := '';
        end;
      end
      else S := S + ' ';
    end;
    MustOpen := False;
  end;
  UpdateControls;

  {$ifdef darwin}
  // This fixes a bug in MacOs (tcCurves is not following the Align 'alClient')
  InputPanel.Width := InputPanel.Width + 1;
  InputPanel.Width := InputPanel.Width - 1;
  {$endif}
end;

procedure TDigitMainForm.FormCreate(Sender: TObject);
begin
  GlobalWinRestorer := TWinRestorer.Create(GetIniName, WHATSAVE_ALL);
  GlobalWinRestorer.RestoreWin(Self, [size, location, state]);
  RestorePreferences;

  PlotImage := TPlotImage.Create(ScrollBox);
  with PlotImage do
  begin
    Parent := ScrollBox;
    PopupMenu := DigitPopupMenu;

    OnMouseDown := @PlotImageMouseDown;
    OnMouseMove := @PlotImageMouseMove;
    OnMouseUp := @PlotImageMouseUp;
    OnMouseLeave := @PlotImageMouseLeave;
    OnResize := @PlotImageResize;
    OnChange := @PlotImageChange;
    OnShowProgress := @PlotImageShowProgress;
    OnHideProgress := @PlotImageHideProgress;
    OnRegionSelected := @PlotImageRegionSelected;
    OnStateChanged := @PlotImageStateChanged;
    OnMarkerDragged := @PlotImageMarkerDragged;

    Options.LoadFromFile(GetIniName);
  end;

  Initialize;

  // Update shortcuts to MacOS style
  {$IFDEF DARWIN}
  // File shortcuts
  FileNew.ShortCut := scMeta + VK_N; // Command + N
  FileOpen.ShortCut := scMeta + VK_O; // Command + O
  FileSave.ShortCut := scMeta + VK_S; // Command + S
  FileImage.ShortCut := scMeta + VK_I; // Command + I
  FileExport.ShortCut := scMeta + VK_E; // Command + E
  FileExit.ShortCut := scMeta + scAlt + VK_X; // Command + Alt + X
  // Edit shortcuts
  EditUndo.ShortCut := scMeta + VK_Z; // Command + Z
  EditRedo.ShortCut := scMeta + VK_R; // Command + R
  EditCopyCurve.ShortCut := scMeta + VK_C; // Command + C
  EditPasteImage.ShortCut := scMeta + VK_V; // Command + V
  // Tools shortcuts
  ToolCurveAdd.ShortCut := scMeta + VK_T; // Command + T
  ToolCurveDelete.ShortCut := scMeta + VK_W; // Command + W
  {$ENDIF}
end;

procedure TDigitMainForm.FormClose(Sender: TObject;
  var AAction: TCloseAction);
begin
  GlobalWinRestorer.SaveWin(Self, [size, location, state]);
  GlobalWinRestorer.Free;
  SavePreferences;

  PlotImage.Options.SaveToFile(GetIniName);
  PlotImage.Free;
end;
//End of form functions

//Beginning of action functions
procedure TDigitMainForm.FileNewExecute(Sender: TObject);
begin
  //Verify that the file has been saved
  if CheckSaveStatus then
  begin
    //Create a new digitization
    Initialize;
    DigitFileName := 'noname.dig';
  end;
end;

procedure TDigitMainForm.FileOpenExecute(Sender: TObject);
begin
  //Load digitization (checking previously that the digitization is saved)
  if CheckSaveStatus and OpenProjectDlg.Execute then
    OpenProject(OpenProjectDlg.FileName);
end;

procedure TDigitMainForm.FileSaveExecute(Sender: TObject);
begin
  //Save digitization
  if FileExists(SaveProjectDlg.FileName) or SaveProjectDlg.Execute then
    SaveProject(SaveProjectDlg.FileName);
end;

procedure TDigitMainForm.FileSaveAsExecute(Sender: TObject);
begin
  //Save digitization with a different name
  if SaveProjectDlg.Execute then
    SaveProject(SaveProjectDlg.FileName);
end;

procedure TDigitMainForm.FileImageExecute(Sender: TObject);
begin
  //Load the image
  OpenPictureDlg.FileName := PlotImage.ImageName;
  if OpenPictureDlg.Execute then
    InsertImage(OpenPictureDlg.FileName);
end;

procedure TDigitMainForm.FileExportExecute(Sender: TObject);
var
  i: Integer;
  F: TextFile;
  PtCv: TCurve;
begin
  //Export digitization to a .csv file
  SaveDataDlg.FileName := ExtractFilePath(DigitFileName) + PlotImage.DigitCurve.Name + '.csv';
  if SaveDataDlg.Execute then
  begin
    AssignFile(F, SaveDataDlg.FileName);
    Rewrite(F);
    Writeln(F, '"' + PlotImage.Scale.XLabel + '","' + PlotImage.Scale.YLabel + '"');
    try
      PtCv := PlotImage.PlotCurve;
      for i := 0 to PtCv.Count - 1 do
        Writeln(F, PtCv.Point[i].ToStr('%.5g'));
    finally
      PtCv.Free;
    end;
    CloseFile(F);
  end;
end;

procedure TDigitMainForm.gbResampleResize(Sender: TObject);
begin
  SeXo.Width := (SeXo.Width + SeXf.Width) div 2;
  leData.ColWidths[0] := leData.Width div 2;
end;

procedure TDigitMainForm.GridRemovalExecute(Sender: TObject);
begin
  PlotImage.RemoveGrid(btnMajorGrid.ButtonColor, btnMinorGrid.ButtonColor,
                       btnBackground.ButtonColor,
                       edtGridTolerance.Value, edtGridThreshold.Value/100.0,
                       chbRebuildCurve.Checked, edtGridMask.Value);
  CurveToGUI;
end;

procedure TDigitMainForm.GridShowHideExecute(Sender: TObject);
begin
  PlotImage.SwitchGrid;
  GridShowHide.Checked := PlotImage.GridMask.IsActive;
  GridRemoval.Enabled := not PlotImage.GridMask.IsActive;

  if GridShowHide.Checked then
    GridShowHide.ImageIndex := 40
  else
    GridShowHide.ImageIndex := 39;
end;

procedure TDigitMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TDigitMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDigitMainForm.leDataValidateEntry(Sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  X, Y: Double;
begin
  if (aCol = 1) then
  begin
    if TryStrToFloat(NewValue, Y) then
    begin
      with leData do
        if (PlotImage.NumPoints >= Row) and (NewValue <> OldValue) then
        begin
          X := PlotImage.Point[Row - 1].X;
          PlotImage.Point[Row - 1] := TCurvePoint.Create(X, Y);

          NewValue := Format('%.5g', [Y]);

          // Inform that the curve changed, but don't update the table
          SetIsSaved(False, False);
        end;
    end
    else
    begin
      // If the value entered is not a float, then set the old value
      NewValue := OldValue;
      raise EAbort.Create('Invalid value.');
    end;
  end;
end;

procedure TDigitMainForm.ModeBackgroundColorExecute(Sender: TObject);
begin
  MouseMode := mdBackgroundColor;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeMajorGridColorExecute(Sender: TObject);
begin
  MouseMode := mdMajorGridColor;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeMinorGridColorExecute(Sender: TObject);
begin
  MouseMode := mdMinorGridColor;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.pcInputChange(Sender: TObject);
begin
  PlotImage.State := TPlotImageState(pcInput.ActivePageIndex);
  UpdateControls;

  // Update relevant fields
  case TPlotImageState(pcInput.ActivePageIndex) of
    piSetCurve: CurveToGUI;
    piSetScale: begin
      cbbCoords.ItemIndex := Integer(PlotImage.Scale.CoordSystem);
      cbbXScale.ItemIndex := Integer(PlotImage.Scale.XScale);
      edtX.Text := PlotImage.Scale.XLabel;
      cbbYScale.ItemIndex := Integer(PlotImage.Scale.YScale);
      edtY.Text := PlotImage.Scale.YLabel;

      ImagePoint[1] := PlotImage.Scale.ImagePoint[1];
      PlotPoint[1] := PlotImage.Scale.PlotPoint[1];
      ImagePoint[2] := PlotImage.Scale.ImagePoint[2];
      PlotPoint[2] := PlotImage.Scale.PlotPoint[2];
      ImagePoint[3] := PlotImage.Scale.ImagePoint[3];
      PlotPoint[3] := PlotImage.Scale.PlotPoint[3];
    end;
    piSetPlotBox: begin
      EditVX1.Value := Round(PlotImage.PlotBox.Vertex[0].X);
      EditVY1.Value := Round(PlotImage.PlotBox.Vertex[0].Y);
      EditVX2.Value := Round(PlotImage.PlotBox.Vertex[1].X);
      EditVY2.Value := Round(PlotImage.PlotBox.Vertex[1].Y);
      EditVX3.Value := Round(PlotImage.PlotBox.Vertex[2].X);
      EditVY3.Value := Round(PlotImage.PlotBox.Vertex[2].Y);
      EditVX4.Value := Round(PlotImage.PlotBox.Vertex[3].X);
      EditVY4.Value := Round(PlotImage.PlotBox.Vertex[3].Y);
    end;
    piSetGrid: begin
      btnMajorGrid.ButtonColor := PlotImage.GridMask.MajorGridColor;
      btnMinorGrid.ButtonColor := PlotImage.GridMask.MinorGridColor;
      btnBackground.ButtonColor := PlotImage.GridMask.BckgndColor;
      edtGridTolerance.Value := PlotImage.GridMask.Tolerance;
      edtGridThreshold.Value := Round(100*PlotImage.GridMask.Threshold);
      chbRebuildCurve.Checked := PlotImage.GridMask.FixCurve;
      edtGridMask.Value := PlotImage.GridMask.MaskSize;
    end;
  end;

end;

function TDigitMainForm.CheckSaveStatus: Boolean;
begin
  Result := True;

  if (not IsSaved) then
    case MessageDlg('The file ' + ExtractFileName(DigitFileName) +
                    ' was modified. Do you want to save it?',
                    mtInformation, [mbYes, mbNo, mbCancel], 0) of

      mrYes: FileSaveExecute(DigitMainForm);
      mrCancel: Result := False;
    end;
end;

function TDigitMainForm.GetIsSaved: Boolean;
begin
  Result := FIsSaved;
end;

function TDigitMainForm.GetCurveCount: Integer;
begin
  Result := PlotImage.Count;
end;

function TDigitMainForm.GetManualZoom: Boolean;
begin
  Result := MainPlot.Extent.UseXMin or MainPlot.Extent.UseXMax or
            MainPlot.Extent.UseYMin or MainPlot.Extent.UseYMax;
end;

function TDigitMainForm.GetZoomXmin: Double;
begin
  Result := MainPlot.LogicalExtent.a.X;
end;

function TDigitMainForm.GetZoomXmax: Double;
begin
  Result := MainPlot.LogicalExtent.b.X;
end;

function TDigitMainForm.GetZoomYmin: Double;
begin
  Result := MainPlot.LogicalExtent.a.Y;
end;

function TDigitMainForm.GetZoomYmax: Double;
begin
  Result := MainPlot.LogicalExtent.b.Y;
end;

function TDigitMainForm.GetIniName: TFileName;
var
  Path : TFileName;
begin
  Result := AppFullName + '.ini';

  Path := GetUserDir + '.digit' + PathDelim;
  if not ForceDirectories(Path) then
    Path := '';

  Result := Path + Result;
end;

function TDigitMainForm.GetCoords: TCoordSystem;
begin
  case cbbCoords.ItemIndex of
    0, 1: Result := TCoordSystem(cbbCoords.ItemIndex);
    else
      Result := csCartesian;
  end;
end;

function TDigitMainForm.GetXScale: TScaleType;
begin
  case cbbXScale.ItemIndex of
    0, 1, 2, 3: Result := TScaleType(cbbXScale.ItemIndex);
    else
      Result := stLinear;
  end;
end;

function TDigitMainForm.GetYScale: TScaleType;
begin
  case cbbYScale.ItemIndex of
    0, 1, 2, 3: Result := TScaleType(cbbYScale.ItemIndex);
    else
      Result := stLinear;
  end;
end;

function TDigitMainForm.GetXLabel: String;
begin
  Result := edtX.Text;
end;

function TDigitMainForm.GetYLabel: String;
begin
  Result := edtY.Text;
end;

function TDigitMainForm.GetImagePoint(Index: Integer): TCurvePoint;
begin
  case Index of
    1: Result := TCurvePoint.Create(EditIX1.Value, EditIY1.Value);
    2: Result := TCurvePoint.Create(EditIX2.Value, EditIY2.Value);
    3: Result := TCurvePoint.Create(EditIX3.Value, EditIY3.Value);
    else Result := TCurvePoint.Create(0, 0);
  end;
end;

function TDigitMainForm.GetPlotPoint(Index: Integer): TCurvePoint;
begin
  case Index of
    1: Result := TCurvePoint.Create(StrToFloat(EditPX1.Text),
                               StrToFloat(EditPY1.Text));
    2: Result := TCurvePoint.Create(StrToFloat(EditPX2.Text),
                               StrToFloat(EditPY2.Text));
    3: Result := TCurvePoint.Create(StrToFloat(EditPX3.Text),
                               StrToFloat(EditPY3.Text));
    else Result := TCurvePoint.Create(0, 0);
  end;
end;

procedure TDigitMainForm.SetDigitFileName(Value: TFileName);
begin
  FDigitFileName := Value;
  if (FDigitFileName = '') then
    Caption := AppFullName
  else
    Caption := AppFullName + ' - ' + ExtractFileName(FDigitFileName);

  OpenProjectDlg.FileName := FDigitFileName;
  SaveProjectDlg.FileName := FDigitFileName;

  if DirectoryExists(ExtractFilePath(FDigitFileName)) then
  begin
    OpenProjectDlg.InitialDir := ExtractFilePath(FDigitFileName);
    SaveProjectDlg.InitialDir := ExtractFilePath(FDigitFileName);
  end;
end;

procedure TDigitMainForm.SetIsSaved(Value: Boolean);
begin
  SetIsSaved(Value, True)
end;

procedure TDigitMainForm.SetIsSaved(Value, UpdateTable: Boolean);
begin
  if (FDigitFileName = '') then
    Caption := AppFullName
  else
    Caption := AppFullName + ' - ' + ExtractFileName(FDigitFileName);

  if not Value then
    Caption := Caption + '*';

  FIsSaved := Value;

  CurveCount := PlotImage.Count;

  if UpdateTable then UpdateView;
  UpdateControls;
end;

procedure TDigitMainForm.SetMouseMode(Value: TMouseMode);
begin
  FMouseMode := Value;
  case Value of
    mdCursor: PlotImage.Cursor := crDefault;
    mdColor,
    mdMajorGridColor,
    mdMinorGridColor,
    mdBackgroundColor: PlotImage.Cursor := crHandPoint;
    mdMarkers,
    mdSteps,
    mdSegments,
    mdGroup,
    mdDelete: PlotImage.Cursor := crCross;
  end;
end;

procedure TDigitMainForm.SetCurveCount(Value: Integer);
var
  i: Integer;
  TmpSeries: TLineSeries;
begin
  assert(Value = PlotImage.Count, 'Error: The number of curves is incorrect.');

  // Create or delete the required series
  if (MainPlot.Series.Count > Value) then
    for i := MainPlot.Series.Count - 1 downto Value do
    begin
      TmpSeries := TLineSeries(MainPlot.Series[i]);
      MainPlot.Series.List.Remove(TmpSeries);
      TmpSeries.Free;

      tcCurves.Tabs.Delete(i);
    end
  else
    for i := MainPlot.Series.Count to Value - 1 do
    begin
      TmpSeries := TLineSeries.Create(MainPlot);
      MainPlot.AddSeries(TmpSeries);

      tcCurves.Tabs.Add(PlotImage.Curves[i].Name);
    end;

  // Update tabs and series
  for i := 0 to PlotImage.Count - 1 do
  begin
    TmpSeries := TLineSeries(MainPlot.Series[i]);
    TmpSeries.Title := PlotImage.Curves[i].Name;
    TmpSeries.SeriesColor := PlotImage.Curves[i].Color;
    TmpSeries.LinePen.Width := 2;
    TmpSeries.Clear;

    tcCurves.Tabs.Strings[i] := PlotImage.Curves[i].Name;
  end;
end;


procedure TDigitMainForm.SetManualZoom(Value: Boolean);
begin
  with MainPlot.Extent do
  begin
    UseXMin := Value;
    UseXMax := Value;
    UseYMin := Value;
    UseYMax := Value;
  end;
end;

procedure TDigitMainForm.SetZoomXmin(Value: Double);
begin
  MainPlot.Extent.XMin := Value;
end;

procedure TDigitMainForm.SetZoomXmax(Value: Double);
begin
  MainPlot.Extent.XMax := Value;
end;

procedure TDigitMainForm.SetZoomYmin(Value: Double);
begin
  MainPlot.Extent.YMin := Value;
end;

procedure TDigitMainForm.SetZoomYmax(Value: Double);
begin
  MainPlot.Extent.YMax := Value;
end;

procedure TDigitMainForm.SetCoords(Value: TCoordSystem);
begin
  case Value of
    csCartesian,
    csPolar: cbbCoords.ItemIndex := Integer(Value);
    else
      cbbCoords.ItemIndex := -1;
  end;
  cbbCoordsChange(cbbCoords);
end;

procedure TDigitMainForm.SetXScale(Value: TScaleType);
begin
  case Value of
    stLinear,
    stLog,
    stLn,
    stInverse: cbbXScale.ItemIndex := Integer(Value);
    else
      cbbXScale.ItemIndex := -1;
  end;
end;

procedure TDigitMainForm.SetYScale(Value: TScaleType);
begin
  case Value of
    stLinear,
    stLog,
    stLn,
    stInverse: cbbYScale.ItemIndex := Integer(Value);
    else
      cbbYScale.ItemIndex := -1;
  end;
end;

procedure TDigitMainForm.SetXLabel(Value: String);
begin
  edtX.Text := Value;
end;

procedure TDigitMainForm.SetYLabel(Value: String);
begin
  edtY.Text := Value;
end;

procedure TDigitMainForm.SetImagePoint(Index: Integer; const Value: TCurvePoint);
begin
  case Index of
    1: begin
      EditIX1.Value := Round(Value.X);
      EditIY1.Value := Round(Value.Y);
    end;
    2: begin
      EditIX2.Value := Round(Value.X);
      EditIY2.Value := Round(Value.Y);
    end;
    3: begin
      EditIX3.Value := Round(Value.X);
      EditIY3.Value := Round(Value.Y);
    end;
  end;
end;

procedure TDigitMainForm.SetPlotPoint(Index: Integer; const Value: TCurvePoint);
begin
  case Index of
    1: begin
      EditPX1.Text := FloatToStr(Value.X);
      EditPY1.Text := FloatToStr(Value.Y);
    end;
    2: begin
      EditPX2.Text := FloatToStr(Value.X);
      EditPY2.Text := FloatToStr(Value.Y);
    end;
    3: begin
      EditPX3.Text := FloatToStr(Value.X);
      EditPY3.Text := FloatToStr(Value.Y);
    end;
  end;
end;

procedure TDigitMainForm.SavePreferences;
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniName);
  try
    //Save panel widths
    Ini.WriteInteger('Panels', 'InputPanel', InputPanel.Width);
    Ini.WriteInteger('Panels', 'DataPanel', pnlData.Width);
  finally
    Ini.Free;
  end;
end;

procedure TDigitMainForm.RestorePreferences;
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniName);
  try
    //Restore panel widths
    InputPanel.Width := Ini.ReadInteger('Panels', 'InputPanel', InputPanel.Width);
    pnlData.Width := Ini.ReadInteger('Panels', 'DataPanel', pnlData.Width);
  finally
    Ini.Free;
    InputPanelResize(Self);
  end;
end;

procedure TDigitMainForm.UpdateZoomImage(X, Y: Integer);
const
  span = 20;
var
  ImgRect: TRect;
  TempBmp: TBitmap;
  Xo, Xf, Yo, Yf, Xc, Yc: Integer;
begin
  // Make sure that the image is updated
  Application.ProcessMessages;

  if (X < Span) then
  begin
    Xo := 0;
    Xf := 2*span + 1;
  end
  else if (X > (PlotImage.Width - span - 1)) then
  begin
    Xf := PlotImage.Width;
    Xo := Xf - 2*span - 1;
  end
  else
  begin
    Xo := X - span;
    Xf := X + span + 1;
  end;

  if (Y < Span) then
  begin
    Yo := 0;
    Yf := 2*span + 1;
  end
  else if (Y > (PlotImage.Height - span - 1)) then
  begin
    Yf := PlotImage.Height;
    Yo := Yf - 2*span - 1;
  end
  else
  begin
    Yo := Y - span;
    Yf := Y + span + 1;
  end;

  ImgRect := Rect(Xo, Yo, Xf, Yf);

  try
    TempBmp := PlotImage.GetZoomImage(ZoomImage.Width, ZoomImage.Height, ImgRect);
    with TempBmp do
    begin
      Xc := Width*(X - Xo) div (2*span);
      Yc := Height*(Y - Yo) div (2*span);

      Canvas.Pen.Mode := pmNot;
      Canvas.Line(Xc - 10, Yc, Xc + 11, Yc);
      Canvas.Line(Xc, Yc - 10, Xc, Yc + 11);
    end;

    ZoomImage.Picture.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;
end;

procedure TDigitMainForm.UpdateZoomImage(P: TPoint);
begin
  UpdateZoomImage(P.X, P.Y);
end;

procedure TDigitMainForm.ClearZoomImage;
begin
  with ZoomImage.Canvas do
  begin
    Brush.Color:= clBtnFace;
    Pen.Mode := pmCopy;
    Pen.Color := clBtnFace;
    Pen.Style := psSolid;
    Rectangle(0, 0, ZoomImage.Width, ZoomImage.Height);
  end;
end;

procedure TDigitMainForm.CurveToGUI;
var
  TmpCurve: TCurve;
begin
  with PlotImage.DigitCurve do
  begin
    btnColor.ButtonColor := Color;
    if (Step > 0) then
    begin
      edtStep.Value := Step;
      rgDirection.ItemIndex := 0;
    end
    else
    begin
      edtStep.Value := -Step;
      rgDirection.ItemIndex := 1;
    end;
    edtInterval.Value := Interval;
    edtTolerance.Value := Tolerance;
    edtSpread.Value := Spread;

    try
      TmpCurve := PlotImage.PlotCurve;
      if (TmpCurve.Count > 1) then
      begin
        seInterpPoints.Value := TmpCurve.Count;
        seXo.Value := TmpCurve.X[0];
        seXf.Value := TmpCurve.X[TmpCurve.Count - 1];
      end
      else
      begin
        seInterpPoints.Value := 101;
        seXo.Value := 0;
        seXf.Value := 0;
      end;
    finally
      TmpCurve.Free;
    end;
  end;

  GridShowHide.Checked := PlotImage.GridMask.IsValid and PlotImage.GridMask.IsActive;
  if GridShowHide.Checked then
    GridShowHide.ImageIndex := 40
  else
    GridShowHide.ImageIndex := 39;
end;

procedure TDigitMainForm.GUIToCurve;
begin
  with PlotImage.DigitCurve do
  begin
    Color := btnColor.ButtonColor;
    if (rgDirection.ItemIndex = 0) then
      Step := edtStep.Value
    else
      Step := -edtStep.Value;
    Interval := edtInterval.Value;
    Tolerance := edtTolerance.Value;
    Spread := edtSpread.Value;
  end;
end;

procedure TDigitMainForm.PlotImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft: begin
      case MouseMode of
        //We are adding markers
        mdMarkers: begin
          PlotImage.AddMarker(TPoint.Create(X, Y));
          UpdateZoomImage(X, Y);
        end;
        //We are selecting the color
        mdColor: begin
          btnColor.ButtonColor := PlotImage.PlotImg.GetPixel(X, Y);
          PlotImage.DigitCurve.Color := PlotImage.PlotImg.GetPixel(X, Y);
          PlotImage.RedrawMarkers;
        end;
        mdMajorGridColor: btnMajorGrid.ButtonColor := PlotImage.PlotImg.GetPixel(X, Y);
        mdMinorGridColor: btnMinorGrid.ButtonColor := PlotImage.PlotImg.GetPixel(X, Y);
        mdBackgroundColor: btnBackground.ButtonColor := PlotImage.PlotImg.GetPixel(X, Y);
      end;

      UpdateControls;
    end;
    mbRight: begin
      DigitizeFromHereItem.Enabled := PlotImage.Scale.IsValid and PlotImage.ColorIsSet;
      TmpPoint.X := X;
      TmpPoint.Y := Y;
    end;
  end;
end;

procedure TDigitMainForm.PlotImageMouseLeave(Sender: TObject);
begin
  ClearZoomImage;
  //ZoomImage.Visible := False;

  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
end;

procedure TDigitMainForm.PlotImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TCurvePoint;
//var
//  i: Integer;
//  a, b, c, Pn: TCurvePoint;
//  X1, Y1,
//  sin_a,
//  cos_a: Double;
begin
  //ZoomImage.Visible := True;
  UpdateZoomImage(X, Y);

  StatusBar.Panels[1].Text := IntToStr(X) + ', ' + IntToStr(Y);
  if PlotImage.Scale.IsValid then
  begin
    Pt := PlotImage.ConvertCoords(X, Y);
    StatusBar.Panels[2].Text := Pt.ToStr('%.3g');
  end
  else
    StatusBar.Panels[2].Text := '';



  //Pn := TCurvePoint.Create(X, Y);
  //c := PlotImage.PlotBox.Center;
  //a := PlotImage.PlotBox[0] - c;
  //b := Pn - c;
  //
  //if (Modulus(a) > 0) and (Modulus(b) > 0) then
  //begin
  //  cos_a := (a.X*b.X + a.Y*b.Y)/Modulus(a)/Modulus(b);
  //  sin_a := sign(a.x*(pn.Y - c.Y) - a.y*(pn.X - c.X))*sqrt(1 - cos_a*cos_a);
  //end;
  //StatusBar.Panels[3].Text := Format('%.g', [ArcSin(sin_a)*45/ArcTan(1)]);
  //StatusBar.Panels[4].Text := Format('%.g, %.g', [sin_a, cos_a]);
end;

procedure TDigitMainForm.PlotImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateZoomImage(X, Y);
end;

procedure TDigitMainForm.PlotImageResize(Sender: TObject);
begin
  with TPlotImage(Sender) do
  begin
    // Update the limits in the relevant controls
    EditIX1.MaxValue := Width - 1;
    EditIY1.MaxValue := Height - 1;
    EditIX2.MaxValue := Width - 1;
    EditIY2.MaxValue := Height - 1;
    EditIX3.MaxValue := Width - 1;
    EditIY3.MaxValue := Height - 1;

    EditVX1.MaxValue := Width - 1;
    EditVY1.MaxValue := Height - 1;
    EditVX2.MaxValue := Width - 1;
    EditVY2.MaxValue := Height - 1;
    EditVX3.MaxValue := Width - 1;
    EditVY3.MaxValue := Height - 1;
    EditVX4.MaxValue := Width - 1;
    EditVY4.MaxValue := Height - 1;
  end;
end;

procedure TDigitMainForm.PlotImageChange(Sender: TObject);
begin
  // Only update the table when the scale changes
  SetIsSaved(False, PlotImage.State in [piSetCurve, piSetScale]);
end;

procedure TDigitMainForm.PlotImageShowProgress(Sender: TObject; Progress: Cardinal);
begin
  ProgressBar.Visible := True;
  ProgressBar.Position := Progress;
end;

procedure TDigitMainForm.PlotImageHideProgress(Sender: TObject);
begin
  ProgressBar.Visible := False;
end;

procedure TDigitMainForm.PlotImageRegionSelected(Sender: TObject; RegionRect: TRect);
begin
  case MouseMode of
    mdSteps: PlotImage.CorrectCurve(RegionRect, True);
    mdSegments: PlotImage.CorrectCurve(RegionRect, False);
    mdGroup: PlotImage.GroupPoints(RegionRect);
    mdDelete: PlotImage.DeletePoints(RegionRect);
  end;
end;

procedure TDigitMainForm.PlotImageStateChanged(Sender: TObject; NewState: TPlotImageState);
begin
  if (pcInput.ActivePageIndex <> Integer(NewState)) then
  begin
    pcInput.ActivePageIndex := Integer(NewState);
  end;
end;

procedure TDigitMainForm.PlotImageMarkerDragged(Sender: TObject; Marker: TMarker; Zoom: Boolean);

  // Avoid an update loop, values come from PlotImage
  procedure UpdateEditors(Point: TCurvePoint; EditX, EditY: TBCTrackbarUpdown);
  var
    Ex, Ey: TTrackbarUpDownChangeEvent;
  begin
    Ex := EditX.OnChange;
    Ey := EditY.OnChange;
    EditX.OnChange := Nil;
    EditY.OnChange := Nil;

    EditX.Value := Round(Point.X);
    EditY.Value := Round(Point.Y);

    EditX.OnChange := Ex;
    EditY.OnChange := Ey;
  end;

begin
  case PlotImage.State of
    piSetScale: begin
      if (Marker = PlotImage.AxesMarkers[1]) then
        UpdateEditors(PlotImage.Scale.ImagePoint[1], EditIX1, EditIY1);

      if (Marker = PlotImage.AxesMarkers[2]) then
        UpdateEditors(PlotImage.Scale.ImagePoint[2], EditIX2, EditIY2);

      if (Marker = PlotImage.AxesMarkers[3]) then
        UpdateEditors(PlotImage.Scale.ImagePoint[3], EditIX3, EditIY3);
    end;
    piSetPlotBox: begin
      if (Marker = PlotImage.BoxMarkers[1]) then
        UpdateEditors(PlotImage.BoxVertex[1], EditVX1, EditVY1);

      if (Marker = PlotImage.BoxMarkers[2]) then
        UpdateEditors(PlotImage.BoxVertex[2], EditVX2, EditVY2);

      if (Marker = PlotImage.BoxMarkers[3]) then
        UpdateEditors(PlotImage.BoxVertex[3], EditVX3, EditVY3);

      if (Marker = PlotImage.BoxMarkers[4]) then
        UpdateEditors(PlotImage.BoxVertex[4], EditVX4, EditVY4);
    end;
  end;

  if Zoom then
    UpdateZoomImage(Marker.Position);
end;

procedure TDigitMainForm.InputPanelResize(Sender: TObject);
begin
  ZoomImage.Height := InputPanel.Width;
  {$ifdef linux}
  edtStep.Invalidate;
  edtInterval.Invalidate;
  edtTolerance.Invalidate;
  edtSpread.Invalidate;

  EditIX1.Invalidate;
  EditIY1.Invalidate;
  EditIX2.Invalidate;
  EditIY2.Invalidate;
  EditIX3.Invalidate;
  EditIY3.Invalidate;

  edtGridTolerance.Invalidate;
  edtGridThreshold.Invalidate;
  edtGridMask.Invalidate;
  {$endif}
end;

procedure TDigitMainForm.MainPlotMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TDoublePoint;
begin
  StatusBar.Panels[1].Text := '';
  Pt := MainPlot.ImageToGraph(TPoint.Create(X, Y));
  StatusBar.Panels[2].Text := Format('%.3g, %.3g', [Pt.X, Pt.Y]);
end;

procedure TDigitMainForm.MarkersDeleteExecute(Sender: TObject);
var
  P: TPoint;
begin
  with PlotImage do
  begin
    DeleteActiveMarker;
    P := ScreenToClient(Mouse.CursorPos);
    if BoundsRect.Contains(P) then
      UpdateZoomImage(P);
  end;
end;

procedure TDigitMainForm.MarkersMoveDownExecute(Sender: TObject);
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(0, 1));

    PlotImageMarkerDragged(Self, ActiveMarker, True);
  end;
end;

procedure TDigitMainForm.MarkersMoveLeftExecute(Sender: TObject);
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(-1, 0));

    PlotImageMarkerDragged(Self, ActiveMarker, True);
  end;
end;

procedure TDigitMainForm.MarkersMoveRightExecute(Sender: TObject);
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(1, 0));

    PlotImageMarkerDragged(Self, ActiveMarker, True);
  end;
end;

procedure TDigitMainForm.MarkersMoveUpExecute(Sender: TObject);
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(0, -1));

    PlotImageMarkerDragged(Self, ActiveMarker, True);
  end;
end;

procedure TDigitMainForm.ModeColorExecute(Sender: TObject);
begin
  MouseMode := mdColor;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeCursorExecute(Sender: TObject);
begin
  MouseMode := mdCursor;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeSegmentExecute(Sender: TObject);
begin
  MouseMode := mdSegments;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeMarkersExecute(Sender: TObject);
begin
  MouseMode := mdMarkers;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeStepsExecute(Sender: TObject);
begin
  MouseMode := mdSteps;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeDeletePointsExecute(Sender: TObject);
begin
  MouseMode := mdDelete;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.ModeGroupPointsExecute(Sender: TObject);
begin
  MouseMode := mdGroup;
  TAction(Sender).Checked := True;
end;

procedure TDigitMainForm.PlotExportExecute(Sender: TObject);
begin
  SavePlotDlg.FilterIndex := 1;
  SavePlotDlg.FileName := ChangeFileExt(ExtractFileName(DigitFileName), '.png');
  if SavePlotDlg.Execute then
    SavePlot(SavePlotDlg.FileName, SavePlotDlg.FilterIndex);
end;

procedure TDigitMainForm.PlotScaleExecute(Sender: TObject);
begin
  with ChartScaleDlg do
  begin
    if Execute(ManualZoom, ZoomXmin, ZoomXmax, ZoomYmin, ZoomYmax) then
    begin
      ManualZoom := Zoom.ManualZoom;

      ZoomXmin := Zoom.Xmin;
      ZoomXmax := Zoom.Xmax;
      ZoomYmin := Zoom.Ymin;
      ZoomYmax := Zoom.Ymax;
    end;
  end;
end;

procedure TDigitMainForm.tcCurvesChange(Sender: TObject);
begin
  //First, update the active curve
  GUIToCurve;
  //Change active curve
  PlotImage.CurveIndex := tcCurves.TabIndex;
  //Finally, update the control values
  CurveToGUI;
  //Update the control values
  UpdateView;
  UpdateControls;
end;

procedure TDigitMainForm.ToolAdjustCurveExecute(Sender: TObject);
begin
  GUIToCurve;
  PlotImage.AdjustCurve;
  PlotImage.Invalidate;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolAdjustNoiseExecute(Sender: TObject);
begin
  GUIToCurve;
  PlotImage.AdjustCurve(True);
  PlotImage.Invalidate;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolConvertToSymbolsExecute(Sender: TObject);
begin
  GUIToCurve;
  PlotImage.ConvertCurveToSymbols;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolCorrectDistortionExecute(Sender: TObject);
begin
  // Almost ready, but not yet
  if (MessageDlg('You are about to crop the current plot image.' +
                 ' This action cannot be undone and will reset the' +
                 ' digitization. Continue?',
                 mtWarning, [mbYes, mbNo], 0) = mrYes) then
    PlotImage.UndistortImage;
end;

procedure TDigitMainForm.ToolCurveAddExecute(Sender: TObject);
begin
  PlotImage.AddCurve;
  tcCurves.TabIndex := PlotImage.Count - 1;
  tcCurvesChange(Self);
end;

procedure TDigitMainForm.ToolCurveDeleteExecute(Sender: TObject);
begin
  PlotImage.DeleteCurve;
  tcCurves.TabIndex := PlotImage.CurveIndex;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolCurveLeftExecute(Sender: TObject);
begin
  //Move the curve one pixel left
  PlotImage.MoveCurveLeft;
end;

procedure TDigitMainForm.ToolCurveNameExecute(Sender: TObject);
var
  NewName: String;
begin
  NewName := InputBox('Curve name', 'New name:', PlotImage.DigitCurve.Name);
  if (NewName <> PlotImage.DigitCurve.Name) then
  begin
    PlotImage.DigitCurve.Name := NewName;
    tcCurves.Tabs.Strings[tcCurves.TabIndex] := NewName;
    TLineSeries(MainPlot.Series[tcCurves.TabIndex]).Title := NewName;
  end;
end;

procedure TDigitMainForm.ToolCurveRightExecute(Sender: TObject);
begin
  //Move the curve one pixel right
  PlotImage.MoveCurveRight;
end;

procedure TDigitMainForm.ToolDigitColorExecute(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  // TODO
  TmpOpt := PlotImage.Options;
  TmpOpt.DefaultDig := digColor;
  PlotImage.Options := TmpOpt;
  btnDigitize.Action := ToolDigitColor;
end;

procedure TDigitMainForm.ToolDigitLineExecute(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  GUIToCurve;
  //Digitize curve
  PlotImage.DigitizeSpectrum;
  CurveToGUI;

  TmpOpt := PlotImage.Options;
  TmpOpt.DefaultDig := digLine;
  PlotImage.Options := TmpOpt;
  btnDigitize.Action := ToolDigitLine;
end;

procedure TDigitMainForm.ToolDigitMarkersExecute(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  GUIToCurve;
  //Fill curve from markers
  PlotImage.DigitizeMarkers;
  CurveToGUI;

  TmpOpt := PlotImage.Options;
  TmpOpt.DefaultDig := digMarkers;
  PlotImage.Options := TmpOpt;
  btnDigitize.Action := ToolDigitMarkers;
end;

procedure TDigitMainForm.ToolBSplinesExecute(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  GUIToCurve;
  //Replace the curve by interpolated values
  PlotImage.Interpolate(seXo.Value, seXf.Value, seInterpPoints.Value, False, itpBSpline);
  CurveToGUI;

  TmpOpt := PlotImage.Options;
  TmpOpt.DefaultItp := itpBSpline;
  PlotImage.Options := TmpOpt;
  btnResample.Action := ToolBSplines;
end;

procedure TDigitMainForm.ToolResetBoxExecute(Sender: TObject);
const
  span = 6;
begin
  with PlotImage do
  begin
    BoxVertex[1] := TCurvePoint.Create(span, span);
    BoxVertex[2] := TCurvePoint.Create(Width - span - 1, span);
    BoxVertex[3] := TCurvePoint.Create(Width - span - 1, Height - span - 1);
    BoxVertex[4] := TCurvePoint.Create(span, Height - span - 1);
  end;
end;

procedure TDigitMainForm.ToolScaleOptionsExecute(Sender: TObject);
var
  i: Integer;
  WasChanged: Boolean;
begin
  if OptionsDlg.Execute(PlotImage.Scale) then
  begin
    WasChanged := False;
    with PlotImage.Scale do
    begin
      if (CoordSystem <> OptionsDlg.CoordSystem) then
      begin
        CoordSystem := OptionsDlg.CoordSystem;
        WasChanged := True;
      end;
      if (XScale <> OptionsDlg.XScale) or (YScale <> OptionsDlg.YScale) then
      begin
        XScale := OptionsDlg.XScale;
        YScale := OptionsDlg.YScale;
        WasChanged := True;
      end;

      if (XLabel <> OptionsDlg.XLabel) or (YLabel <> OptionsDlg.YLabel) then
      begin
        XLabel := OptionsDlg.XLabel;
        YLabel := OptionsDlg.YLabel;

        leData.TitleCaptions[0] := XLabel;
        leData.TitleCaptions[1] := YLabel;
        leData.Invalidate;

        MainPlot.BottomAxis.Title.Caption := XLabel;
        MainPlot.LeftAxis.Title.Caption := YLabel;

        WasChanged := True;
      end;

      for i := 1 to 3 do
      begin
        if (ImagePoint[i] <> OptionsDlg.ImagePoint[i]) then
        begin
          ImagePoint[i] := OptionsDlg.ImagePoint[i];
          PlotImage.MoveMarker(PlotImage.AxesMarkers[i], OptionsDlg.ImagePoint[i]);
          WasChanged := True;
        end;
        if (PlotPoint[i] <> OptionsDlg.PlotPoint[i]) then
        begin
          PlotPoint[i] := OptionsDlg.PlotPoint[i];
          WasChanged := True;
        end;
      end;
    end;

    if WasChanged then
    begin
      GUIToCurve;

      IsSaved := False;
    end;
  end;
end;

procedure TDigitMainForm.ToolSmoothExecute(Sender: TObject);
begin
  //Smooth the curve
  PlotImage.Smooth(seSGKernel.Value, seSGDegree.Value);
end;

procedure TDigitMainForm.ToolCurveUpExecute(Sender: TObject);
begin
  //Move the curve one pixel up
  PlotImage.MoveCurveUp;
end;

procedure TDigitMainForm.ToolCurveDownExecute(Sender: TObject);
begin
  //Move the curve one pixel down
  PlotImage.MoveCurveDown;
end;

procedure TDigitMainForm.ToolClearExecute(Sender: TObject);
begin
  GUIToCurve;
  //Clear the curve
  TLineSeries(MainPlot.Series[PlotImage.CurveIndex]).Clear;
  PlotImage.ClearCurve;
  CurveToGUI;
end;

procedure TDigitMainForm.EditUndoExecute(Sender: TObject);
begin
  GUIToCurve;
  //Undo the last change to the curve
  PlotImage.UndoCurveChanges;
  CurveToGUI;
end;


procedure TDigitMainForm.EditRedoExecute(Sender: TObject);
begin
  GUIToCurve;
  //Redo the last change to the curve
  PlotImage.RedoCurveChanges;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolSplinesExecute(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  GUIToCurve;
  //Replace the curve by interpolated values
  PlotImage.Interpolate(seXo.Value, seXf.Value, seInterpPoints.Value, False, itpSpline);
  CurveToGUI;

  TmpOpt := PlotImage.Options;
  TmpOpt.DefaultItp := itpSpline;
  PlotImage.Options := TmpOpt;
  btnResample.Action := ToolSplines;
end;

//End of the action functions

initialization

{$IFDEF TIFF_CLIPBOARD_FORMAT}
  tiffClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.tiff'{$ELSE}'image/tiff'{$ENDIF});
{$ENDIF}
{$IFDEF PNG_CLIPBOARD_FORMAT}
  pngClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.png'{$ELSE}{$IFDEF WINDOWS}'PNG'{$ELSE}'image/png'{$ENDIF}{$ENDIF});
{$ENDIF}

end.
