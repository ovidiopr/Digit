unit uMain;

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
  TAChartAxis, TATransformations, TACustomSource, TAChartUtils, ClipBrd,
  ActnList, ValEdit, Spin, ExtDlgs, MaskEdit, BCTrackbarUpdown, FileUtil,
  BGRABitmapTypes, GraphType, Grids, ExtTabCtrl, Math, Types,
  uSciRestorer, uChartScale, uUtils, uCoordinates, uCurves, uMarker, uPlotImage,
  uImageViewer, uMapImage, uMapPanel, ExtPageCtrl, uDocFrame, uPlotFrame,
  uMapFrame, uExtSVGImageList;

type
  { TDigitMainForm }
  TDigitMainForm = class(TForm)
    EditShowSymbols: TAction;
    EditShowLine: TAction;
    DigitLineTraceItem: TMenuItem;
    DigitSymbolTraceItem: TMenuItem;
    DragPointsItem: TMenuItem;
    EditShowLinetem: TMenuItem;
    EditShowSymbolsItem: TMenuItem;
    SegmentItem: TMenuItem;
    N8: TMenuItem;
    StepsItem: TMenuItem;
    ModeDragPointsItem: TMenuItem;
    DragMenu: TPopupMenu;
    tbWidth: TBCTrackbarUpdown;
    ToolDigitSymbolTraceItem: TMenuItem;
    ToolDigitLineTraceItem: TMenuItem;
    ToolDigitLineTracing: TAction;
    ToolDigitSymbolTracing: TAction;
    LogClear: TAction;
    LogSave: TAction;
    GridMergeMask: TAction;
    EditZoomResize: TAction;
    EditZoomResizeItem: TMenuItem;
    PlotNameItem: TMenuItem;
    PlotDeleteItem: TMenuItem;
    PlotAddItem: TMenuItem;
    SaveLogDlg: TSaveDialog;
    Separator2: TMenuItem;
    btnResize: TToolButton;
    ToolPlotDelete: TAction;
    ToolPlotName: TAction;
    ToolPlotAdd: TAction;
    LinearItem: TMenuItem;
    ToolQuadraticItem: TMenuItem;
    ToolLinearItem: TMenuItem;
    QuadraticItem: TMenuItem;
    ToolPolynomial: TAction;
    ToolLinear: TAction;
    ToolCancelAction: TAction;
    tbZoom: TBCTrackbarUpdown;
    EditZoomFit: TAction;
    EditZoomOut: TAction;
    EditZoomIn: TAction;
    EditZoomOriginal: TAction;
    DigitColorItem: TMenuItem;
    EditZoomOriginalItem: TMenuItem;
    EditZoomInItem: TMenuItem;
    EditZoomOutItem: TMenuItem;
    EditZoomFitItem: TMenuItem;
    N7: TMenuItem;
    sep09: TToolButton;
    btnZoomOriginal: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnZoomFit: TToolButton;
    ToolDigitizeItem: TMenuItem;
    ToolDigitColorItem: TMenuItem;
    ToolDigitColorTracing: TAction;
    ToolResetBox: TAction;
    ToolCorrectDistortion: TAction;
    ToolAdjustNoise: TAction;
    BSplinesItem: TMenuItem;
    ToolSplinesItem: TMenuItem;
    ToolBSplinesItem: TMenuItem;
    ResampleMenu: TPopupMenu;
    SplinesItem: TMenuItem;
    ToolSplines: TAction;
    DigitLineFollowItem: TMenuItem;
    DigitMarkersItem: TMenuItem;
    DigitMainItem: TMenuItem;
    ToolDigitMarkersItem: TMenuItem;
    ToolDigitMarkers: TAction;
    EditPasteImage: TAction;
    EditPasteImageItem: TMenuItem;
    ModeBackgroundColor: TAction;
    ModeMinorGridColor: TAction;
    ModeMajorGridColor: TAction;
    GridShowHide: TAction;
    GridRemoval: TAction;
    ModeDeletePointsItem: TMenuItem;
    ModeGroupPointsItem: TMenuItem;
    ModeDeletePoints: TAction;
    ModeGroupPoints: TAction;
    ModeDragPoints: TAction;
    ModeDragCurveItem: TMenuItem;
    MarkerUpItem: TMenuItem;
    MarkerDownItem: TMenuItem;
    MarkerLeftItem: TMenuItem;
    MarkerRightItem: TMenuItem;
    MarkerDelItem: TMenuItem;
    N6: TMenuItem;
    btnGroupPointsmode: TToolButton;
    btnDeletePointsMode: TToolButton;
    DigitizeMenu: TPopupMenu;
    BtnPasteImage: TToolButton;
    ToolRightItem: TMenuItem;
    ToolLeftItem: TMenuItem;
    ToolCurveRight: TAction;
    ToolCurveLeft: TAction;
    MarkersMenu: TMenuItem;
    MarkersDelete: TAction;
    MarkersMoveRight: TAction;
    MarkersMoveLeft: TAction;
    MarkersMoveDown: TAction;
    MarkersMoveUp: TAction;
    btnColorMode: TToolButton;
    btnCursorMode: TToolButton;
    btnMarkersMode: TToolButton;
    btnResample: TToolButton;
    btnSmooth: TToolButton;
    btnCorrectPoints: TToolButton;
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
    ToolPlotOptions: TAction;
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
    OpenPictureDlg: TOpenPictureDialog;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    ToolBSplines: TAction;
    ModeSteps: TAction;
    ModeColor: TAction;
    ModeMarkers: TAction;
    ModeCursor: TAction;
    ToolCurveName: TAction;
    ToolCurveDelete: TAction;
    ToolCurveAdd: TAction;
    DigitizeFromHereItem: TMenuItem;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    DigitPopupMenu: TPopupMenu;
    HelpMenu: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    EditMenu: TMenuItem;
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
    ImageList: TExtSVGImageList;
    EditUndo: TAction;
    DigitMenu: TMenuItem;
    ToolDigitLineFollowing: TAction;
    ToolDigitLineFollowItem: TMenuItem;
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
    procedure DigitizeFromHereItemClick(Sender: TObject);
    procedure EditCopyCurveExecute(Sender: TObject);
    procedure EditPasteImageExecute(Sender: TObject);
    procedure EditShowLineExecute(Sender: TObject);
    procedure EditShowLineUpdate(Sender: TObject);
    procedure EditShowSymbolsExecute(Sender: TObject);
    procedure EditShowSymbolsUpdate(Sender: TObject);
    procedure EditZoomFitExecute(Sender: TObject);
    procedure EditZoomInExecute(Sender: TObject);
    procedure EditZoomOriginalExecute(Sender: TObject);
    procedure EditZoomOutExecute(Sender: TObject);
    procedure EditZoomResizeExecute(Sender: TObject);
    procedure FileImportDigitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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
    procedure GridMergeMaskExecute(Sender: TObject);
    procedure GridRemovalExecute(Sender: TObject);
    procedure GridShowHideExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);
    procedure LogClearExecute(Sender: TObject);
    procedure LogSaveExecute(Sender: TObject);
    procedure ModeBackgroundColorExecute(Sender: TObject);
    procedure ModeBackgroundColorUpdate(Sender: TObject);
    procedure ModeColorUpdate(Sender: TObject);
    procedure ModeCursorUpdate(Sender: TObject);
    procedure ModeDeletePointsUpdate(Sender: TObject);
    procedure ModeDragPointsUpdate(Sender: TObject);
    procedure ModeGroupPointsUpdate(Sender: TObject);
    procedure ModeMajorGridColorExecute(Sender: TObject);
    procedure ModeMajorGridColorUpdate(Sender: TObject);
    procedure ModeMarkersUpdate(Sender: TObject);
    procedure ModeMinorGridColorExecute(Sender: TObject);
    procedure ModeMinorGridColorUpdate(Sender: TObject);
    procedure ModeSegmentUpdate(Sender: TObject);
    procedure ModeStepsUpdate(Sender: TObject);
    procedure MarkersDeleteExecute(Sender: TObject);
    procedure MarkersMoveExecute(Sender: TObject);
    procedure ModeColorExecute(Sender: TObject);
    procedure ModeCursorExecute(Sender: TObject);
    procedure ModeSegmentExecute(Sender: TObject);
    procedure ModeMarkersExecute(Sender: TObject);
    procedure ModeStepsExecute(Sender: TObject);
    procedure ModeDeletePointsExecute(Sender: TObject);
    procedure ModeGroupPointsExecute(Sender: TObject);
    procedure ModeDragPointsExecute(Sender: TObject);
    procedure PlotExportExecute(Sender: TObject);
    procedure PlotScaleExecute(Sender: TObject);
    procedure tbWidthChange(Sender: TObject; AByUser: boolean);
    procedure tbZoomChange(Sender: TObject; AByUser: Boolean);
    procedure ToolAdjustCurveExecute(Sender: TObject);
    procedure ToolAdjustNoiseExecute(Sender: TObject);
    procedure ToolCancelActionExecute(Sender: TObject);
    procedure ToolConvertToSymbolsExecute(Sender: TObject);
    procedure ToolCorrectDistortionExecute(Sender: TObject);
    procedure ToolCurveAddExecute(Sender: TObject);
    procedure ToolCurveDeleteExecute(Sender: TObject);
    procedure ToolCurveLeftExecute(Sender: TObject);
    procedure ToolCurveNameExecute(Sender: TObject);
    procedure ToolCurveRightExecute(Sender: TObject);
    procedure ToolDigitColorTracingExecute(Sender: TObject);
    procedure ToolDigitLineFollowingExecute(Sender: TObject);
    procedure ToolDigitLineTracingExecute(Sender: TObject);
    procedure ToolDigitMarkersExecute(Sender: TObject);
    procedure ToolBSplinesExecute(Sender: TObject);
    procedure ToolDigitSymbolTracingExecute(Sender: TObject);
    procedure ToolLinearExecute(Sender: TObject);
    procedure ToolPlotAddExecute(Sender: TObject);
    procedure ToolPlotDeleteExecute(Sender: TObject);
    procedure ToolPlotNameExecute(Sender: TObject);
    procedure ToolPolynomialExecute(Sender: TObject);
    procedure ToolResetBoxExecute(Sender: TObject);
    procedure ToolPlotOptionsExecute(Sender: TObject);
    procedure ToolSmoothExecute(Sender: TObject);
    procedure ToolCurveUpExecute(Sender: TObject);
    procedure ToolCurveDownExecute(Sender: TObject);
    procedure ToolClearExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure ToolSplinesExecute(Sender: TObject);
  private
    { Private declarations }
    Documents: TExtPageCtrl;
    NewDocMenu: TPopupMenu;
    MapOpenDlg: TOpenDialog;
    FAddingDocument: Boolean;
    Restorer: TSciRestorer;
    MapToolActions: array [TMapTool] of TAction;
    MapToolButtons: array [TMapTool] of TToolButton;
    MapToolSep: TToolButton;

    procedure CreateDocumentUI;
    function DocumentOf(APage: TExtPage): TDocumentFrame;
    function PageOf(Doc: TDocumentFrame): TExtPage;
    function FindDocument(const FileName: TFileName): TDocumentFrame;
    function AddDocument(AKind: TDocumentKind): TDocumentFrame;
    procedure CloseDocument(Doc: TDocumentFrame);
    procedure ReplaceBlank(Blank, Doc: TDocumentFrame);
    procedure ActivateDocument(Doc: TDocumentFrame);
    procedure SetDocKind(Value: TDocumentKind; HasDocument: Boolean);
    procedure DisableDocumentActions;
    procedure UpdateCaption;
    procedure DocumentLayout(Doc: TDocumentFrame; Save: Boolean);

    procedure DocumentsTabChanging(Sender: TObject; OldIndex, NewIndex: Integer; var Allow: Boolean);
    procedure DocumentsTabChanged(Sender: TObject; NewIndex: Integer);
    procedure DocumentsTabDeleting(Sender: TObject; Index: Integer; var Allow: Boolean);
    procedure DocumentsTabDeleted(Sender: TObject);
    procedure DocumentsAddClick(Sender: TObject);
    procedure NewPlotClick(Sender: TObject);
    procedure MapNewClick(Sender: TObject);
    procedure MapToolExecute(Sender: TObject);
    procedure MapToolUpdate(Sender: TObject);
    procedure MapFinishClick(Sender: TObject);
    procedure MapCancelClick(Sender: TObject);
    procedure MapDeleteClick(Sender: TObject);

    function GetIniName: TFileName;
  public
    { Public declarations }
    PlotOptions: TPlotOptions;
    MapMenu: TMenuItem;

    function ActiveDocument: TDocumentFrame;
    function ActivePlot: TPlotFrame;
    function ActiveMap: TMapFrame;
    function Viewer: TImageViewer;

    function NewPlot: TPlotFrame;
    procedure NewMap(const FileName: TFileName);
    procedure OpenFile(const FileName: TFileName);
    function SaveDocument(Doc: TDocumentFrame; AsNew: Boolean): Boolean;
    procedure DocumentChanged(Doc: TDocumentFrame);

    procedure UpdateControls;
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

uses uAbout;

const
  // Keys used by the former window restorer, so saved layouts survive
  RestorerSection = 'WindowsRestorer';
  RestorerKey = 'TDigitMainForm';

  // Map tool icons follow the plot icons in the image lists
  MapIconFirst = 68;

  MapToolCaptions: array [TMapTool] of String = ('None', 'Select / edit',
    'Scale bar', 'Reference points', 'Point', 'Polyline', 'Polygon (area)',
    'Distance', 'Angle', 'Line profile', 'Colour bar', 'Particle region');

//Beginning of document functions
function TDigitMainForm.DocumentOf(APage: TExtPage): TDocumentFrame;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(APage) then Exit;

  for i := 0 to APage.ControlCount - 1 do
    if (APage.Controls[i] is TDocumentFrame) then
      Exit(TDocumentFrame(APage.Controls[i]));
end;

function TDigitMainForm.PageOf(Doc: TDocumentFrame): TExtPage;
begin
  Result := nil;
  if Assigned(Doc) and (Doc.Parent is TExtPage) then
    Result := TExtPage(Doc.Parent);
end;

function TDigitMainForm.ActiveDocument: TDocumentFrame;
begin
  if Assigned(Documents) then
    Result := DocumentOf(Documents.ActivePage)
  else
    Result := nil;
end;

function TDigitMainForm.ActivePlot: TPlotFrame;
var
  Doc: TDocumentFrame;
begin
  Doc := ActiveDocument;
  if Assigned(Doc) and (Doc.Kind = dkPlot) then
    Result := TPlotFrame(Doc)
  else
    Result := nil;
end;

function TDigitMainForm.ActiveMap: TMapFrame;
var
  Doc: TDocumentFrame;
begin
  Doc := ActiveDocument;
  if Assigned(Doc) and (Doc.Kind = dkMap) then
    Result := TMapFrame(Doc)
  else
    Result := nil;
end;

function TDigitMainForm.Viewer: TImageViewer;
var
  Doc: TDocumentFrame;
begin
  Doc := ActiveDocument;
  if Assigned(Doc) then
    Result := Doc.Viewer
  else
    Result := nil;
end;

function TDigitMainForm.FindDocument(const FileName: TFileName): TDocumentFrame;
var
  i: Integer;
  Doc: TDocumentFrame;
begin
  Result := nil;
  for i := 0 to Documents.PageCount - 1 do
  begin
    Doc := DocumentOf(Documents.Page[i]);
    if Assigned(Doc) and FileExists(Doc.DigitFileName) and
       SameFileName(ExpandFileName(Doc.DigitFileName), ExpandFileName(FileName)) then
      Exit(Doc);
  end;
end;

procedure TDigitMainForm.CreateDocumentUI;

  function AddItem(AParent: TMenuItem; const ACaption: String;
    AOnClick: TNotifyEvent; ATag: Integer = 0): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;
    Result.Tag := ATag;
    AParent.Add(Result);
  end;

var
  T: TMapTool;
  Item, ToolsItem: TMenuItem;
  A: TAction;
  Btn: TToolButton;
begin
  // Document tabs fill the space between the toolbar and the status bar
  Documents := TExtPageCtrl.Create(Self);
  Documents.Parent := Self;
  Documents.Align := alClient;
  Documents.ShowHint := True;
  Documents.TabOptions := Documents.TabOptions + [etoShowAddButton, etoShowCloseButton];
  Documents.OnTabChanging := @DocumentsTabChanging;
  Documents.OnTabChanged := @DocumentsTabChanged;
  Documents.OnTabDeleting := @DocumentsTabDeleting;
  Documents.OnTabDeleted := @DocumentsTabDeleted;
  Documents.OnAddButtonClick := @DocumentsAddClick;

  // Menu shown by the add button
  NewDocMenu := TPopupMenu.Create(Self);
  AddItem(NewDocMenu.Items, 'New plot', @NewPlotClick);
  AddItem(NewDocMenu.Items, 'New map from image...', @MapNewClick);
  Documents.AddMenu := NewDocMenu;

  MapOpenDlg := TOpenDialog.Create(Self);
  MapOpenDlg.Title := 'Open image as map';
  MapOpenDlg.Filter := 'Images and DigitalMicrograph files|*.dm3;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.gif|' +
    'DigitalMicrograph (*.dm3)|*.dm3|All files (*.*)|*.*';

  // Several files can be opened at once, one tab each
  OpenProjectDlg.Options := OpenProjectDlg.Options + [ofAllowMultiSelect];

  // File menu entry, always available
  Item := TMenuItem.Create(Self);
  Item.Caption := 'New map from image...';
  Item.OnClick := @MapNewClick;
  FileMenu.Insert(FileMenu.IndexOf(FileNewItem) + 1, Item);

  // Map menu, only for map documents
  MapMenu := TMenuItem.Create(Self);
  MapMenu.Caption := '&Map';
  MainMenu.Items.Insert(MainMenu.Items.IndexOf(HelpMenu), MapMenu);
  // Map tools are actions, shared by the menu and the toolbar
  ToolsItem := AddItem(MapMenu, 'Tool', nil);
  for T := Succ(mtNone) to High(TMapTool) do
  begin
    A := TAction.Create(Self);
    A.ActionList := ActionList;
    A.Category := 'Map';
    A.Caption := MapToolCaptions[T];
    A.Hint := MapToolCaptions[T];
    A.Tag := Ord(T);
    A.ImageIndex := MapIconFirst + Ord(T) - 1;
    A.OnExecute := @MapToolExecute;
    A.OnUpdate := @MapToolUpdate;
    MapToolActions[T] := A;

    Item := TMenuItem.Create(Self);
    Item.Action := A;
    Item.RadioItem := True;
    ToolsItem.Add(Item);
  end;

  // Toolbar buttons sit where the plot mode buttons are (only one set is visible)
  MapToolSep := TToolButton.Create(Self);
  MapToolSep.Style := tbsSeparator;
  MapToolSep.Left := sep02.Left + 1;
  MapToolSep.Parent := tbMain;
  MapToolSep.Visible := False;
  for T := Succ(mtNone) to High(TMapTool) do
  begin
    Btn := TToolButton.Create(Self);
    Btn.Left := sep02.Left + 2 + Ord(T);
    Btn.Parent := tbMain;
    Btn.Style := tbsCheck;
    Btn.Action := MapToolActions[T];
    Btn.Visible := False;
    MapToolButtons[T] := Btn;
  end;
  AddItem(MapMenu, '-', nil);
  AddItem(MapMenu, 'Finish drawing', @MapFinishClick).ShortCut := Menus.ShortCut(VK_RETURN, []);
  AddItem(MapMenu, 'Cancel drawing', @MapCancelClick).ShortCut := Menus.ShortCut(VK_ESCAPE, []);
  AddItem(MapMenu, 'Delete selected feature', @MapDeleteClick).ShortCut := Menus.ShortCut(VK_DELETE, [ssCtrl]);
  MapMenu.Visible := False;
end;

function TDigitMainForm.AddDocument(AKind: TDocumentKind): TDocumentFrame;
var
  Page: TExtPage;
begin
  FAddingDocument := True;
  try
    Page := Documents.AddPage('noname.dig');
    if (AKind = dkMap) then
      Result := TMapFrame.Create(Page)
    else
      Result := TPlotFrame.Create(Page);
    Result.Parent := Page;
    Result.Align := alClient;

    DocumentLayout(Result, False);

    Documents.PageIndex := Documents.IndexOfPage(Page);
  finally
    FAddingDocument := False;
  end;
end;

procedure TDigitMainForm.CloseDocument(Doc: TDocumentFrame);
var
  Page: TExtPage;
begin
  Page := PageOf(Doc);
  if Assigned(Page) then
    Documents.DeletePage(Documents.IndexOfPage(Page));
end;

procedure TDigitMainForm.ReplaceBlank(Blank, Doc: TDocumentFrame);
begin
  // An untouched empty tab gives way to the opened document
  if Assigned(Blank) and (Blank <> Doc) and Blank.IsBlank then
    CloseDocument(Blank);
  ActivateDocument(ActiveDocument);
end;

function TDigitMainForm.NewPlot: TPlotFrame;
begin
  Result := TPlotFrame(AddDocument(dkPlot));
  Result.Initialize;
  ActivateDocument(Result);
end;

procedure TDigitMainForm.NewMap(const FileName: TFileName);
var
  Blank: TDocumentFrame;
  Doc: TMapFrame;
begin
  Blank := ActiveDocument;
  Doc := TMapFrame(AddDocument(dkMap));
  ActivateDocument(Doc);
  Doc.LoadImageFile(FileName);
  ReplaceBlank(Blank, Doc);
end;

procedure TDigitMainForm.OpenFile(const FileName: TFileName);
var
  Blank, Doc: TDocumentFrame;
begin
  if not FileExists(FileName) then
  begin
    MessageDlg('The file ' + ExtractFileName(FileName) + ' does not exist.',
      mtError, [mbOK], 0);
    Exit;
  end;

  // Already open, just show it
  Doc := FindDocument(FileName);
  if Assigned(Doc) then
  begin
    Documents.PageIndex := Documents.IndexOfPage(PageOf(Doc));
    ActivateDocument(Doc);
    Exit;
  end;

  Blank := ActiveDocument;
  if SameText(ExtractFileExt(FileName), '.dig') then
  begin
    if (TImageViewer.DocumentTypeOf(FileName) = 'map') then
    begin
      Doc := AddDocument(dkMap);
      ActivateDocument(Doc);
      TMapFrame(Doc).OpenProject(FileName);
    end
    else
    begin
      Doc := AddDocument(dkPlot);
      ActivateDocument(Doc);
      TPlotFrame(Doc).OpenProject(FileName);
    end;
  end
  else if SameText(ExtractFileExt(FileName), '.dm3') then
  begin
    Doc := AddDocument(dkMap);
    ActivateDocument(Doc);
    TMapFrame(Doc).LoadImageFile(FileName);
  end
  else
  begin
    // It should be an image, digitize it
    Doc := AddDocument(dkPlot);
    TPlotFrame(Doc).Initialize;
    ActivateDocument(Doc);
    TPlotFrame(Doc).InsertImage(FileName);
  end;

  ReplaceBlank(Blank, Doc);
end;

function TDigitMainForm.SaveDocument(Doc: TDocumentFrame; AsNew: Boolean): Boolean;
begin
  Result := False;
  if not Assigned(Doc) then Exit;

  SaveProjectDlg.FileName := Doc.DigitFileName;
  if DirectoryExists(ExtractFilePath(Doc.DigitFileName)) then
    SaveProjectDlg.InitialDir := ExtractFilePath(Doc.DigitFileName);

  if ((not AsNew) and FileExists(Doc.DigitFileName)) or SaveProjectDlg.Execute then
    Doc.SaveProject(SaveProjectDlg.FileName);

  Result := Doc.IsSaved;
end;

procedure TDigitMainForm.DocumentChanged(Doc: TDocumentFrame);
var
  Page: TExtPage;
begin
  Page := PageOf(Doc);
  if Assigned(Page) then
  begin
    if Doc.IsSaved then
      Page.Tab.Caption := Doc.Title
    else
      Page.Tab.Caption := Doc.Title + '*';
    Page.Tab.Hint := Doc.DigitFileName;
  end;

  if (Doc = ActiveDocument) then
    UpdateCaption;
end;

procedure TDigitMainForm.UpdateCaption;
var
  Doc: TDocumentFrame;
begin
  Doc := ActiveDocument;
  if not Assigned(Doc) then
  begin
    Caption := AppFullName;
    Exit;
  end;

  if (Doc.DigitFileName = '') then
    Caption := AppFullName
  else
    Caption := AppFullName + ' - ' + ExtractFileName(Doc.DigitFileName);

  if not Doc.IsSaved then
    Caption := Caption + '*';

  OpenProjectDlg.FileName := Doc.DigitFileName;
  SaveProjectDlg.FileName := Doc.DigitFileName;
  if DirectoryExists(ExtractFilePath(Doc.DigitFileName)) then
  begin
    OpenProjectDlg.InitialDir := ExtractFilePath(Doc.DigitFileName);
    SaveProjectDlg.InitialDir := ExtractFilePath(Doc.DigitFileName);
  end;
end;

procedure TDigitMainForm.DocumentLayout(Doc: TDocumentFrame; Save: Boolean);
var
  i, j: Integer;
  Other: TDocumentFrame;
  Panels: TFPList;
  Tags: array of PtrInt;

  procedure CollectPanels(Container: TWinControl);
  var
    k: Integer;
  begin
    for k := 0 to Container.ControlCount - 1 do
    begin
      if (Container.Controls[k] is TWinControl) then
        CollectPanels(TWinControl(Container.Controls[k]));
      if (Container.Controls[k] is TPanel) then
        Panels.Add(Container.Controls[k]);
    end;
  end;

begin
  if not (Assigned(Doc) and Assigned(Restorer)) then Exit;

  // The restorer walks the whole form, other documents opt out with Tag = -1
  Panels := TFPList.Create;
  try
    for i := 0 to Documents.PageCount - 1 do
    begin
      Other := DocumentOf(Documents.Page[i]);
      if Assigned(Other) and (Other <> Doc) then
        CollectPanels(Other);
    end;

    SetLength(Tags, Panels.Count);
    for j := 0 to Panels.Count - 1 do
    begin
      Tags[j] := TPanel(Panels[j]).Tag;
      TPanel(Panels[j]).Tag := -1;
    end;
    try
      if Save then
        Restorer.SaveForm(Self, RestorerKey, [svPanels])
      else
        Restorer.RestoreForm(Self, RestorerKey, [svPanels]);
    finally
      for j := 0 to Panels.Count - 1 do
        TPanel(Panels[j]).Tag := Tags[j];
    end;
  finally
    Panels.Free;
  end;
end;

procedure TDigitMainForm.ActivateDocument(Doc: TDocumentFrame);
begin
  if Assigned(Doc) then
    SetDocKind(Doc.Kind, True)
  else
    SetDocKind(dkPlot, False);

  UpdateCaption;

  if Assigned(Doc) then
  begin
    // Shared by every document kind
    EditPasteImage.Enabled := True;
    LogClear.Enabled := True;
    LogSave.Enabled := True;
    Doc.Activate;
  end
  else
    UpdateControls;
end;

procedure TDigitMainForm.SetDocKind(Value: TDocumentKind; HasDocument: Boolean);
var
  IsMap: Boolean;
  i: Integer;
  T: TMapTool;
begin
  IsMap := (Value = dkMap);

  // Menus
  ModeMenu.Visible := not IsMap;
  PlotMenu.Visible := not IsMap;
  CurveMenu.Visible := not IsMap;
  DigitMenu.Visible := not IsMap;
  MarkersMenu.Visible := not IsMap;
  EditCopyItem.Visible := not IsMap;
  EditShowLinetem.Visible := not IsMap;
  EditShowSymbolsItem.Visible := not IsMap;
  MapMenu.Visible := IsMap;
  // Hidden shortcuts must not fire in plot mode
  for i := 0 to MapMenu.Count - 1 do
    MapMenu.Items[i].Enabled := IsMap and HasDocument;

  // Plot-only toolbar buttons
  BtnExport.Visible := not IsMap;
  btnScaleOptions.Visible := not IsMap;
  sep06.Visible := not IsMap;
  btnCopy.Visible := not IsMap;
  btnImport.Visible := not IsMap;
  btnCursorMode.Visible := not IsMap;
  btnMarkersMode.Visible := not IsMap;
  btnColorMode.Visible := not IsMap;
  btnCorrectPoints.Visible := not IsMap;
  sep02.Visible := not IsMap;
  sep03.Visible := not IsMap;
  btnResample.Visible := not IsMap;
  btnSmooth.Visible := not IsMap;
  btnSymbols.Visible := not IsMap;
  btnGroupPointsmode.Visible := not IsMap;
  btnDeletePointsMode.Visible := not IsMap;
  tbWidth.Visible := not IsMap;

  // Map tool buttons
  MapToolSep.Visible := IsMap;
  for T := Succ(mtNone) to High(TMapTool) do
    MapToolButtons[T].Visible := IsMap;
end;

procedure TDigitMainForm.DisableDocumentActions;
var
  i: Integer;
  Cat: String;
begin
  // Without documents only creating or opening one makes sense
  for i := 0 to ActionList.ActionCount - 1 do
  begin
    Cat := TAction(ActionList.Actions[i]).Category;
    if (Cat = 'Tools') or (Cat = 'Mode') or (Cat = 'Markers') or
       (Cat = 'Grid') or (Cat = 'Chart') or (Cat = 'Log') or (Cat = 'Edit') then
      TAction(ActionList.Actions[i]).Enabled := False;
  end;
  EditPasteImage.Enabled := True;

  FileSave.Enabled := False;
  FileSaveAs.Enabled := False;
  FileExport.Enabled := False;
  FileImportDigit.Enabled := True;
  tbZoom.Enabled := False;
  tbWidth.Enabled := False;
end;

procedure TDigitMainForm.UpdateControls;
var
  Doc: TDocumentFrame;
begin
  Doc := ActiveDocument;
  if Assigned(Doc) then
    Doc.UpdateControls
  else
    DisableDocumentActions;
end;

procedure TDigitMainForm.DocumentsTabChanging(Sender: TObject; OldIndex,
  NewIndex: Integer; var Allow: Boolean);
begin
  // Long actions must finish (or be cancelled) first
  Allow := not (Assigned(Viewer) and Viewer.RunningAction);
end;

procedure TDigitMainForm.DocumentsTabChanged(Sender: TObject; NewIndex: Integer);
begin
  if not FAddingDocument then
    ActivateDocument(ActiveDocument);
end;

procedure TDigitMainForm.DocumentsTabDeleting(Sender: TObject; Index: Integer;
  var Allow: Boolean);
var
  Doc: TDocumentFrame;
begin
  Doc := DocumentOf(Documents.Page[Index]);
  if not Assigned(Doc) then Exit;

  Allow := Doc.CanClose;
  if Allow then
    DocumentLayout(Doc, True);
end;

procedure TDigitMainForm.DocumentsTabDeleted(Sender: TObject);
begin
  ActivateDocument(ActiveDocument);
end;

procedure TDigitMainForm.DocumentsAddClick(Sender: TObject);
var
  P: TPoint;
begin
  // Plot or map, the user picks
  P := Mouse.CursorPos;
  NewDocMenu.PopUp(P.X, P.Y);
end;

procedure TDigitMainForm.NewPlotClick(Sender: TObject);
begin
  NewPlot;
end;
//End of document functions

//Beginning of form functions
procedure TDigitMainForm.FormActivate(Sender: TObject);
begin
  MustOpen := True;
  if Assigned(ActivePlot) then
    with ActivePlot do
    begin
      pcInput.ActivePageIndex := Integer(PlotImage.State);
      InputPanelResize(Self);
    end;
  UpdateControls;
end;

procedure TDigitMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
  Doc: TDocumentFrame;
begin
  //Verify that every document has been saved before closing
  CanClose := True;
  for i := Documents.PageCount - 1 downto 0 do
  begin
    Doc := DocumentOf(Documents.Page[i]);
    if not Assigned(Doc) then Continue;

    if (not Doc.IsSaved) and (Doc <> ActiveDocument) then
      Documents.PageIndex := i;
    if not Doc.CanClose then
    begin
      CanClose := False;
      Exit;
    end;
  end;
end;

procedure TDigitMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: Integer;
begin
  //Open dragged files, one tab each
  for i := Low(FileNames) to High(FileNames) do
    OpenFile(FileNames[i]);
end;

procedure TDigitMainForm.FormShow(Sender: TObject);
var
  i: Byte;
  S: String;
begin
  if MustOpen then
  begin
    S := '';
    for i := 1 to ParamCount do
    begin
      S := S + ParamStr(i);
      if UpperCase(Copy(ParamStr(i), Length(ParamStr(i)) - 3, 1)) = '.' then
      begin
        if FileExists(S) then
        begin
          OpenFile(S);
          S := '';
        end;
      end
      else
        S := S + ' ';
    end;
    MustOpen := False;
  end;

  // Always start with a document
  if (Documents.PageCount = 0) then
    NewPlot;

  UpdateControls;
end;

procedure TDigitMainForm.FormCreate(Sender: TObject);
var
  Opt: TPlotOptions;
begin
  // Window geometry here, panel sizes per document
  Restorer := TSciRestorer.Create(Self);
  Restorer.AutoSaveRestoreOwner := False;
  Restorer.IniLocation := ilCustomDir;
  Restorer.IniFileName := GetIniName;
  Restorer.IniSection := RestorerSection;
  Restorer.DefaultWhatSave := [svSize, svLocation, svState];
  Restorer.RestoreForm(Self, RestorerKey);

  FAddingDocument := False;
  Opt.LoadFromFile(GetIniName);
  PlotOptions := Opt;

  CreateDocumentUI;
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
  ToolPlotAdd.ShortCut := scMeta + VK_P; // Command + P
  ToolPlotDelete.ShortCut := scMeta + VK_D; // Command + D
  {$ENDIF}
end;

procedure TDigitMainForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  Restorer.SaveForm(Self, RestorerKey);
  DocumentLayout(ActiveDocument, True);

  PlotOptions.SaveToFile(GetIniName);
end;
//End of form functions

//Beginning of action functions
procedure TDigitMainForm.FileNewExecute(Sender: TObject);
begin
  //Create a new digitization in its own tab
  NewPlot;
end;

procedure TDigitMainForm.FileOpenExecute(Sender: TObject);
var
  i: Integer;
begin
  //Load digitizations, one tab each
  if OpenProjectDlg.Execute then
    for i := 0 to OpenProjectDlg.Files.Count - 1 do
      OpenFile(OpenProjectDlg.Files[i]);
end;

procedure TDigitMainForm.FileSaveExecute(Sender: TObject);
begin
  //Save digitization
  SaveDocument(ActiveDocument, False);
end;

procedure TDigitMainForm.FileSaveAsExecute(Sender: TObject);
begin
  //Save digitization with a different name
  SaveDocument(ActiveDocument, True);
end;

procedure TDigitMainForm.FileImageExecute(Sender: TObject);
var
  Doc: TPlotFrame;
begin
  // Map documents start again from the new image
  if Assigned(ActiveMap) then
  begin
    MapOpenDlg.FileName := ActiveMap.MapImage.ImageName;
    if ActiveMap.CanClose and MapOpenDlg.Execute then
      ActiveMap.LoadImageFile(MapOpenDlg.FileName);
    Exit;
  end;

  //Load the image
  Doc := ActivePlot;
  if Assigned(Doc) then
    OpenPictureDlg.FileName := Doc.PlotImage.ImageName;
  if OpenPictureDlg.Execute then
  begin
    if not Assigned(Doc) then
      Doc := NewPlot;
    Doc.InsertImage(OpenPictureDlg.FileName);
  end;
end;

procedure TDigitMainForm.FileImportDigitExecute(Sender: TObject);
var
  Doc: TPlotFrame;
begin
  //Load digitization
  if OpenProjectDlg.Execute then
  begin
    Doc := ActivePlot;
    if not Assigned(Doc) then
      Doc := NewPlot;
    Doc.ImportProject(OpenProjectDlg.FileName);
    Doc.DigitFileName := OpenProjectDlg.FileName;
  end;
end;

procedure TDigitMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDigitMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TDigitMainForm.EditPasteImageExecute(Sender: TObject);
var
  i: Integer;

  function GetImgFromClipboard: Boolean;
  var
    Stream: TMemoryStream;
    Doc: TDocumentFrame;
  begin
    Result := False;

    Stream := TMemoryStream.Create;
    try
      Clipboard.GetFormat(Clipboard.Formats[i], Stream);
      Stream.Position := 0;
      try
        Result := True;

        Doc := ActiveDocument;
        if not Assigned(Doc) then
          Doc := NewPlot;
        Doc.PasteImage(Stream);
      except
        on ex: Exception do
          Result := False;
      end;
    finally
      Stream.Free;
    end;
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

procedure TDigitMainForm.EditUndoExecute(Sender: TObject);
begin
  if Assigned(ActiveDocument) then
    ActiveDocument.Undo;
end;

procedure TDigitMainForm.EditRedoExecute(Sender: TObject);
begin
  if Assigned(ActiveDocument) then
    ActiveDocument.Redo;
end;

procedure TDigitMainForm.EditZoomFitExecute(Sender: TObject);
begin
  if Assigned(Viewer) then
    Viewer.ZoomFitBest;
end;

procedure TDigitMainForm.EditZoomInExecute(Sender: TObject);
begin
  if Assigned(Viewer) then
    Viewer.ZoomIn;
end;

procedure TDigitMainForm.EditZoomOriginalExecute(Sender: TObject);
begin
  if Assigned(Viewer) then
    Viewer.ZoomOriginal;
end;

procedure TDigitMainForm.EditZoomOutExecute(Sender: TObject);
begin
  if Assigned(Viewer) then
    Viewer.ZoomOut;
end;

procedure TDigitMainForm.EditZoomResizeExecute(Sender: TObject);
begin
  if Assigned(Viewer) and (MessageDlg('You are about to resize the current image.' +
    ' This action cannot be undone. Continue?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
    Viewer.ResizeToZoom;
end;

procedure TDigitMainForm.tbZoomChange(Sender: TObject; AByUser: Boolean);
begin
  if AByUser and Assigned(Viewer) then
    with TBCTrackbarUpdown(Sender) do
      Viewer.Zoom := Value/100;
end;

procedure TDigitMainForm.ToolCancelActionExecute(Sender: TObject);
begin
  if Assigned(Viewer) and not Viewer.CancelAction then
    Viewer.CancelAction := True;
end;

procedure TDigitMainForm.LogClearExecute(Sender: TObject);
begin
  if Assigned(ActiveDocument) then
    ActiveDocument.LogBox.Clear;
end;

procedure TDigitMainForm.LogSaveExecute(Sender: TObject);
begin
  if Assigned(ActiveDocument) and SaveLogDlg.Execute then
    ActiveDocument.LogBox.Items.SaveToFile(SaveLogDlg.FileName);
end;

function TDigitMainForm.GetIniName: TFileName;
var
  Path: TFileName;
begin
  Result := AppFullName + '.ini';

  Path := GetUserDir + '.digit' + PathDelim;
  if not ForceDirectories(Path) then
    Path := '';

  Result := Path + Result;
end;
//End of the action functions

//Beginning of map functions
procedure TDigitMainForm.MapNewClick(Sender: TObject);
begin
  if MapOpenDlg.Execute then
    NewMap(MapOpenDlg.FileName);
end;

procedure TDigitMainForm.MapToolExecute(Sender: TObject);
var
  T: TMapTool;
begin
  if not Assigned(ActiveMap) then Exit;

  // A second click on the active tool releases it, as the plot modes do
  T := TMapTool(TComponent(Sender).Tag);
  if (ActiveMap.MapImage.Tool = T) then
    ActiveMap.MapImage.Tool := mtNone
  else
    ActiveMap.MapImage.Tool := T;
  ActiveMap.UpdateControls;
end;

procedure TDigitMainForm.MapToolUpdate(Sender: TObject);
var
  Map: TMapFrame;
begin
  Map := ActiveMap;
  TAction(Sender).Enabled := Assigned(Map) and Map.MapImage.ImageIsLoaded;
  TAction(Sender).Checked := Assigned(Map) and
    (Map.MapImage.Tool = TMapTool(TAction(Sender).Tag));
end;

procedure TDigitMainForm.MapFinishClick(Sender: TObject);
begin
  if not Assigned(ActiveMap) then Exit;
  ActiveMap.MapImage.FinishFeature;
  ActiveMap.UpdateControls;
end;

procedure TDigitMainForm.MapCancelClick(Sender: TObject);
begin
  if not Assigned(ActiveMap) then Exit;
  ActiveMap.MapImage.CancelFeature;
  ActiveMap.UpdateControls;
end;

procedure TDigitMainForm.MapDeleteClick(Sender: TObject);
begin
  if not Assigned(ActiveMap) then Exit;
  ActiveMap.MapImage.DeleteSelectedFeature;
  ActiveMap.UpdateControls;
end;
//End of map functions

//Beginning of plot actions, they act on the active plot

procedure TDigitMainForm.DigitizeFromHereItemClick(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.DigitizeFromHereItemClick(Sender);
end;

procedure TDigitMainForm.EditCopyCurveExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.EditCopyCurveExecute(Sender);
end;

procedure TDigitMainForm.EditShowLineExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.EditShowLineExecute(Sender);
end;

procedure TDigitMainForm.EditShowLineUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.EditShowLineUpdate(Sender);
end;

procedure TDigitMainForm.EditShowSymbolsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.EditShowSymbolsExecute(Sender);
end;

procedure TDigitMainForm.EditShowSymbolsUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.EditShowSymbolsUpdate(Sender);
end;

procedure TDigitMainForm.FileExportExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.FileExportExecute(Sender);
end;

procedure TDigitMainForm.GridMergeMaskExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.GridMergeMaskExecute(Sender);
end;

procedure TDigitMainForm.GridRemovalExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.GridRemovalExecute(Sender);
end;

procedure TDigitMainForm.GridShowHideExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.GridShowHideExecute(Sender);
end;

procedure TDigitMainForm.MarkersDeleteExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.MarkersDeleteExecute(Sender);
end;

procedure TDigitMainForm.MarkersMoveExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.MarkersMoveExecute(Sender);
end;

procedure TDigitMainForm.ModeBackgroundColorExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeBackgroundColorExecute(Sender);
end;

procedure TDigitMainForm.ModeBackgroundColorUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeBackgroundColorUpdate(Sender);
end;

procedure TDigitMainForm.ModeColorExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeColorExecute(Sender);
end;

procedure TDigitMainForm.ModeColorUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeColorUpdate(Sender);
end;

procedure TDigitMainForm.ModeCursorExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeCursorExecute(Sender);
end;

procedure TDigitMainForm.ModeCursorUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeCursorUpdate(Sender);
end;

procedure TDigitMainForm.ModeDeletePointsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeDeletePointsExecute(Sender);
end;

procedure TDigitMainForm.ModeDeletePointsUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeDeletePointsUpdate(Sender);
end;

procedure TDigitMainForm.ModeDragPointsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeDragPointsExecute(Sender);
end;

procedure TDigitMainForm.ModeDragPointsUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeDragPointsUpdate(Sender);
end;

procedure TDigitMainForm.ModeGroupPointsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeGroupPointsExecute(Sender);
end;

procedure TDigitMainForm.ModeGroupPointsUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeGroupPointsUpdate(Sender);
end;

procedure TDigitMainForm.ModeMajorGridColorExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMajorGridColorExecute(Sender);
end;

procedure TDigitMainForm.ModeMajorGridColorUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMajorGridColorUpdate(Sender);
end;

procedure TDigitMainForm.ModeMarkersExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMarkersExecute(Sender);
end;

procedure TDigitMainForm.ModeMarkersUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMarkersUpdate(Sender);
end;

procedure TDigitMainForm.ModeMinorGridColorExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMinorGridColorExecute(Sender);
end;

procedure TDigitMainForm.ModeMinorGridColorUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeMinorGridColorUpdate(Sender);
end;

procedure TDigitMainForm.ModeSegmentExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeSegmentExecute(Sender);
end;

procedure TDigitMainForm.ModeSegmentUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeSegmentUpdate(Sender);
end;

procedure TDigitMainForm.ModeStepsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeStepsExecute(Sender);
end;

procedure TDigitMainForm.ModeStepsUpdate(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ModeStepsUpdate(Sender);
end;

procedure TDigitMainForm.PlotExportExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.PlotExportExecute(Sender);
end;

procedure TDigitMainForm.PlotScaleExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.PlotScaleExecute(Sender);
end;

procedure TDigitMainForm.ToolAdjustCurveExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolAdjustCurveExecute(Sender);
end;

procedure TDigitMainForm.ToolAdjustNoiseExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolAdjustNoiseExecute(Sender);
end;

procedure TDigitMainForm.ToolBSplinesExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolBSplinesExecute(Sender);
end;

procedure TDigitMainForm.ToolClearExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolClearExecute(Sender);
end;

procedure TDigitMainForm.ToolConvertToSymbolsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolConvertToSymbolsExecute(Sender);
end;

procedure TDigitMainForm.ToolCorrectDistortionExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCorrectDistortionExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveAddExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveAddExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveDeleteExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveDeleteExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveDownExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveDownExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveLeftExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveLeftExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveNameExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveNameExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveRightExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveRightExecute(Sender);
end;

procedure TDigitMainForm.ToolCurveUpExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolCurveUpExecute(Sender);
end;

procedure TDigitMainForm.ToolDigitColorTracingExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolDigitColorTracingExecute(Sender);
end;

procedure TDigitMainForm.ToolDigitLineFollowingExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolDigitLineFollowingExecute(Sender);
end;

procedure TDigitMainForm.ToolDigitLineTracingExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolDigitLineTracingExecute(Sender);
end;

procedure TDigitMainForm.ToolDigitMarkersExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolDigitMarkersExecute(Sender);
end;

procedure TDigitMainForm.ToolDigitSymbolTracingExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolDigitSymbolTracingExecute(Sender);
end;

procedure TDigitMainForm.ToolLinearExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolLinearExecute(Sender);
end;

procedure TDigitMainForm.ToolPlotAddExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolPlotAddExecute(Sender);
end;

procedure TDigitMainForm.ToolPlotDeleteExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolPlotDeleteExecute(Sender);
end;

procedure TDigitMainForm.ToolPlotNameExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolPlotNameExecute(Sender);
end;

procedure TDigitMainForm.ToolPlotOptionsExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolPlotOptionsExecute(Sender);
end;

procedure TDigitMainForm.ToolPolynomialExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolPolynomialExecute(Sender);
end;

procedure TDigitMainForm.ToolResetBoxExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolResetBoxExecute(Sender);
end;

procedure TDigitMainForm.ToolSmoothExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolSmoothExecute(Sender);
end;

procedure TDigitMainForm.ToolSplinesExecute(Sender: TObject);
begin
  if Assigned(ActivePlot) then
    ActivePlot.ToolSplinesExecute(Sender);
end;

procedure TDigitMainForm.tbWidthChange(Sender: TObject; AByUser: boolean);
begin
  if Assigned(ActivePlot) then
    ActivePlot.tbWidthChange(Sender, AByUser);
end;
//End of plot actions

initialization

  {$IFDEF TIFF_CLIPBOARD_FORMAT}
tiffClipboardFormat := RegisterClipboardFormat({$IFDEF DARWIN}'public.tiff'{$ELSE}'image/tiff'{$ENDIF});
  {$ENDIF}
  {$IFDEF PNG_CLIPBOARD_FORMAT}
  pngClipboardFormat := RegisterClipboardFormat(
    {$IFDEF DARWIN}
'public.png'
    {$ELSE}
    {$IFDEF WINDOWS}
'PNG'
    {$ELSE}
    'image/png'
    {$ENDIF}
    {$ENDIF}
    );
  {$ENDIF}

end.
