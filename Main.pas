unit Main;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, ExtCtrls, ComCtrls, TATypes, TASeries, TAGraph,
  ClipBrd, ActnList, ValEdit, Spin, ExtDlgs, MaskEdit, BCTrackbarUpdown,
  FileUtil, restore, TAChartUtils, coordinates, curves, plotimage, uchartscale,
  IniFiles;

type
  TMouseMode = (mdCursor, mdMarkers, mdColor, mdSteps, mdSegments,
                mdGroup, mdDelete);

  { TDigitMainForm }
  TDigitMainForm = class(TForm)
    GridShowHide: TAction;
    GridRemoval: TAction;
    edtSpread: TBCTrackbarUpdown;
    edtTolerance: TBCTrackbarUpdown;
    edtInterval: TBCTrackbarUpdown;
    edtStep: TBCTrackbarUpdown;
    edtGridTolerance: TBCTrackbarUpdown;
    btnColor: TColorButton;
    btnMinorGrid: TColorButton;
    btnBackground: TColorButton;
    btnMajorGrid: TColorButton;
    edtGridThreshold: TBCTrackbarUpdown;
    lblGridTolerance: TLabel;
    lblColor: TLabel;
    lblBackground: TLabel;
    lblGridThreshold: TLabel;
    lblMinorGrid: TLabel;
    lblInterval: TLabel;
    lblMajorGrid: TLabel;
    lblSpread: TLabel;
    lblStep: TLabel;
    lblTolerance: TLabel;
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
    pcInput: TPageControl;
    rgDirection: TRadioGroup;
    tbGrid: TToolBar;
    tbRemoveGrid: TToolButton;
    tbShowHideGrid: TToolButton;
    tsGrid: TTabSheet;
    tsDigit: TTabSheet;
    ToolRightItem: TMenuItem;
    ToolLeftItem: TMenuItem;
    ToolCurveRight: TAction;
    ToolCurveLeft: TAction;
    btnAddCurve: TToolButton;
    btnAdjustCurve: TToolButton;
    btnClear: TToolButton;
    btnDelCurve: TToolButton;
    btnDigitize: TToolButton;
    btnEditName: TToolButton;
    btnMoveDown: TToolButton;
    btnMoveUp: TToolButton;
    MarkersMenu: TMenuItem;
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
    EditCopy: TAction;
    EditCopyItem: TMenuItem;
    FileImportDigitItem: TMenuItem;
    N5: TMenuItem;
    btnCopy: TToolButton;
    btnImport: TToolButton;
    ScrollBox: TScrollBox;
    sep02: TToolButton;
    sep03: TToolButton;
    sep07: TToolButton;
    tbDigit: TToolBar;
    tcCurves: TTabControl;
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
    btnExportPlot: TToolButton;
    btnSetScale: TToolButton;
    ToolConvertToSymbols: TAction;
    ToolAdjustCurve: TAction;
    seXo: TFloatSpinEdit;
    seXf: TFloatSpinEdit;
    lblXInterval: TLabel;
    lblXInterval1: TLabel;
    OpenPictureDlg: TOpenPictureDialog;
    ProgressBar: TProgressBar;
    StatusBar: TStatusBar;
    ToolResample: TAction;
    seInterpPoints: TSpinEdit;
    seSGKernel: TSpinEdit;
    gbSmooth: TGroupBox;
    gbResample: TGroupBox;
    gbData: TGroupBox;
    lblSmoothDegree: TLabel;
    lblBSPoints: TLabel;
    lblSmoothKernel: TLabel;
    MainPanel: TPanel;
    LeftSplitter: TSplitter;
    ModeSteps: TAction;
    ModeColor: TAction;
    ModeMarkers: TAction;
    ModeCursor: TAction;
    pnlData: TPanel;
    seSGDegree: TSpinEdit;
    RightSplitter: TSplitter;
    tbPlot: TToolBar;
    ToolCurveName: TAction;
    ToolCurveDelete: TAction;
    ToolCurveAdd: TAction;
    DigitizeFromHereItem: TMenuItem;
    MainPlot: TChart;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    PageControl: TPageControl;
    DigitPopupMenu: TPopupMenu;
    tsPicture: TTabSheet;
    tsPlot: TTabSheet;
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
    MainToolBar: TToolBar;
    BtnOpen: TToolButton;
    BtnSave: TToolButton;
    sep01: TToolButton;
    BtnUndo: TToolButton;
    BtnNew: TToolButton;
    ImageList: TImageList;
    EditUndo: TAction;
    DigitMenu: TMenuItem;
    ToolDigit: TAction;
    ToolDigitizeItem: TMenuItem;
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
    procedure DigitizeFromHereItemClick(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
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
    procedure PlotImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlotImageMouseLeave(Sender: TObject);
    procedure PlotImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure PlotImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PlotImageChange(Sender: TObject);
    procedure PlotImageRegionSelected(Sender: TObject; RegionRect: TRect);
    procedure MainPanelResize(Sender: TObject);
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
    procedure ToolConvertToSymbolsExecute(Sender: TObject);
    procedure ToolCurveAddExecute(Sender: TObject);
    procedure ToolCurveDeleteExecute(Sender: TObject);
    procedure ToolCurveLeftExecute(Sender: TObject);
    procedure ToolCurveNameExecute(Sender: TObject);
    procedure ToolCurveRightExecute(Sender: TObject);
    procedure ToolDigitExecute(Sender: TObject);
    procedure ToolResampleExecute(Sender: TObject);
    procedure ToolScaleOptionsExecute(Sender: TObject);
    procedure ToolSmoothExecute(Sender: TObject);
    procedure ToolCurveUpExecute(Sender: TObject);
    procedure ToolCurveDownExecute(Sender: TObject);
    procedure ToolClearExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
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

    procedure SetDigitFileName(Value: TFileName);
    procedure SetIsSaved(Value: Boolean);
    procedure SetMouseMode(Value: TMouseMode);
    procedure SetCurveCount(Value: Integer);
    procedure SetManualZoom(Value: Boolean);
    procedure SetZoomXmin(Value: Double);
    procedure SetZoomXmax(Value: Double);
    procedure SetZoomYmin(Value: Double);
    procedure SetZoomYmax(Value: Double);

    procedure SavePreferences;
    procedure RestorePreferences;

    procedure UpdateZoomImage(X, Y: Integer);
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
  end;

const
  AppFullName = 'Digit';

var
  DigitMainForm: TDigitMainForm;
  MustOpen: Boolean;

implementation

{$R *.lfm}

uses About, scaledialog;

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
    FileSave.Enabled :=  ImageIsLoaded and (not IsSaved);
    FileSaveAs.Enabled := ImageIsLoaded;

    FileExport.Enabled := HasPoints;

    ModeMarkers.Enabled := ImageIsLoaded;
    ModeColor.Enabled := ImageIsLoaded;
    ModeSteps.Enabled := HasPoints;
    ModeSegment.Enabled := HasPoints;
    ModeGroupPoints.Enabled := HasPoints;
    ModeDeletePoints.Enabled := HasPoints;

    ToolDigit.Enabled := Scale.IsValid and ColorIsSet;
    ToolAdjustCurve.Enabled := HasPoints;
    ToolResample.Enabled := HasPoints;
    ToolSmooth.Enabled := HasPoints;
    ToolConvertToSymbols.Enabled := HasPoints;
    ToolCurveUp.Enabled := HasPoints;
    ToolCurveDown.Enabled := HasPoints;
    ToolClear.Enabled := HasPoints;

    ToolCurveAdd.Enabled := ImageIsLoaded;
    ToolCurveDelete.Enabled := ImageIsLoaded and (Count > 1);
    ToolCurveName.Enabled := ImageIsLoaded and (Count > 0);

    ToolScaleOptions.Enabled := ImageIsLoaded;

    MarkersMoveUp.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveDown.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveLeft.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersMoveRight.Enabled := ImageIsLoaded and assigned(ActiveMarker);
    MarkersDelete.Enabled := ImageIsLoaded and assigned(ActiveMarker) and not ActiveMarker.IsPersistent;

    GridRemoval.Enabled := ImageIsLoaded;
    GridShowHide.Enabled := ImageIsLoaded and ValidGrid;

    PlotExport.Enabled := HasPoints;
    PlotScale.Enabled := HasPoints;

    EditUndo.Enabled := CanUndo;
    EditRedo.Enabled := CanRedo;
    EditCopy.Enabled := HasPoints;
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
        PtCv := PlotImage.PlotCurves[i];
        for j := 0 to PtCv.Count - 1 do
          AddXY(PtCv.X[j], PtCv.Y[j]);
      end;
    end;
  end;
end;

procedure TDigitMainForm.UpdateValues;
var
  i: Integer;
  PtCv: TCurve;
begin
  //Copy the values to the table
  leData.Strings.Clear;
  if (PlotImage.Scale.IsValid and PlotImage.HasPoints) then
  begin
    PtCv := PlotImage.PlotCurve;
    for i := 0 to PtCv.Count - 1 do
      leData.InsertRow(Format('%.5g', [PtCv.X[i]]), Format('%.5g', [PtCv.Y[i]]), True);
  end;
  leData.Row := 1;
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
  PlotImage.Scale.ImagePoint[1] := GetCurvePoint(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.ImagePoint[2] := GetCurvePoint(X, Y);
  Read(F, X);  Readln(F, Y);
  PlotImage.Scale.ImagePoint[3] := GetCurvePoint(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.PlotPoint[1] := GetCurvePoint(X, Y);
  Read(F, X);  Read(F, Y);
  PlotImage.Scale.PlotPoint[2] := GetCurvePoint(X, Y);
  Read(F, X);  Readln(F, Y);
  PlotImage.Scale.PlotPoint[3] := GetCurvePoint(X, Y);
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
      AxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clRed, 3),
                                       Scale.ImagePoint[1], True);
      AxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clGreen, 3),
                                       Scale.ImagePoint[2], True);
      AxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13), '+', clRed, 3),
                                       Scale.ImagePoint[3], True);
    end;

  IsSaved := False;
end;

procedure TDigitMainForm.InsertImage(FileName: TFileName);
begin
  PlotImage.ImageName := FileName;

  with PlotImage do
  begin
    AxesMarkers[1] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clRed, 3),
                                     TPoint.Create(6, 6), True);
    AxesMarkers[2] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clGreen, 3),
                                     TPoint.Create(6, Height - 6), True);
    AxesMarkers[3] := TMarker.Create(CreateMarker(TPoint.Create(13, 13),'+', clRed, 3),
                                     TPoint.Create(Width - 6, Height - 6), True);

    Scale.PlotPoint[1] := GetCurvePoint(0, 1);
    Scale.PlotPoint[2] := GetCurvePoint(0, 0);
    Scale.PlotPoint[3] := GetCurvePoint(1, 0);
  end;

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
end;
//End of general functions

//Beginning of form functions
procedure TDigitMainForm.FormActivate(Sender: TObject);
begin
  MustOpen := True;
  UpdateControls;
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

    PlotImage.DigitizeSpectrum(TmpPoint, ProgressBar);

    CurveToGUI;
  end;
end;

procedure TDigitMainForm.EditCopyExecute(Sender: TObject);
begin
  leData.CopyToClipboard(False);
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
  MainPanel.Width := MainPanel.Width + 1;
  MainPanel.Width := MainPanel.Width - 1;
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
    OnChange := @PlotImageChange;
    OnRegionSelected := @PlotImageRegionSelected;
  end;

  Initialize;
end;

procedure TDigitMainForm.FormClose(Sender: TObject;
  var AAction: TCloseAction);
begin
  GlobalWinRestorer.SaveWin(Self, [size, location, state]);
  GlobalWinRestorer.Free;
  SavePreferences;

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
    PtCv := PlotImage.PlotCurve;
    for i := 0 to PtCv.Count - 1 do
      Writeln(F, Format('%.5g,%.5g', [PtCv.Point[i].X, PtCv.Point[i].Y]));
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
                       edtGridTolerance.Value, edtGridThreshold.Value/100.0);
  CurveToGUI;
end;

procedure TDigitMainForm.GridShowHideExecute(Sender: TObject);
begin
  PlotImage.SubstractGrid := not PlotImage.SubstractGrid;
  GridShowHide.Checked := PlotImage.SubstractGrid;

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

function TDigitMainForm.CheckSaveStatus: Boolean;
begin
  Result := True;

  if (not IsSaved) then
    case MessageDlg('The file ' + ExtractFileName(DigitFileName) +
                    ' was modified. Do you want to save it?',
                    mtInformation,[mbYes, mbNo, mbCancel], 0) of

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
  if (FDigitFileName = '') then
    Caption := AppFullName
  else
    Caption := AppFullName + ' - ' + ExtractFileName(FDigitFileName);

  if not Value then
    Caption := Caption + '*';

  FIsSaved := Value;

  CurveCount := PlotImage.Count;

  UpdateView;
  UpdateControls;
end;

procedure TDigitMainForm.SetMouseMode(Value: TMouseMode);
begin
  FMouseMode := Value;
  case Value of
    mdCursor: PlotImage.Cursor := crDefault;
    mdColor: PlotImage.Cursor := crHandPoint;
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

procedure TDigitMainForm.SavePreferences;
var Ini: TIniFile;
begin
  Ini := TIniFile.Create(GetIniName);
  try
    //Save panel widths
    Ini.WriteInteger('Panels', 'InputPanel', MainPanel.Width);
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
    MainPanel.Width := Ini.ReadInteger('Panels', 'InputPanel', MainPanel.Width);
    pnlData.Width := Ini.ReadInteger('Panels', 'DataPanel', pnlData.Width);
  finally
    Ini.Free;
  end;
end;

procedure TDigitMainForm.UpdateZoomImage(X, Y: Integer);
const
  span = 20;
var
  ZoomRect, ImgRect: TRect;
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

  TempBmp := TBitmap.Create;
  try
    with TempBmp do
    begin
      PixelFormat := pf24bit;
      Width := ZoomImage.Width;
      Height := ZoomImage.Height;
      ZoomRect := Rect(0, 0, Width, Height);
      Canvas.CopyRect(ZoomRect, PlotImage.Bitmap.Canvas, ImgRect);

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

    if (PlotImage.PlotCurve.Count > 1) then
    begin
      seInterpPoints.Value := PlotImage.PlotCurve.Count;
      seXo.Value := PlotImage.PlotCurve.X[0];
      seXf.Value := PlotImage.PlotCurve.X[PlotImage.PlotCurve.Count - 1];
    end
    else
    begin
      seInterpPoints.Value := 101;
      seXo.Value := 0;
      seXf.Value := 0;
    end;
  end;

  GridShowHide.Checked := PlotImage.ValidGrid and PlotImage.SubstractGrid;
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
          btnColor.ButtonColor := PlotImage.Picture.Bitmap.Canvas.Pixels[X, Y];
          PlotImage.DigitCurve.Color := PlotImage.Picture.Bitmap.Canvas.Pixels[X, Y];
          PlotImage.RedrawMarkers;
        end;
      end;

      UpdateControls;
    end;
    mbRight: begin
      Self.DigitizeFromHereItem.Enabled := PlotImage.Scale.IsValid and PlotImage.ColorIsSet;
      TmpPoint.X := X;
      TmpPoint.Y := Y;
    end;
  end;
end;

procedure TDigitMainForm.PlotImageMouseLeave(Sender: TObject);
begin
  ClearZoomImage;

  StatusBar.Panels[1].Text := '';
  StatusBar.Panels[2].Text := '';
end;

procedure TDigitMainForm.PlotImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TCurvePoint;
begin
  UpdateZoomImage(X, Y);

  StatusBar.Panels[1].Text := IntToStr(X) + ', ' + IntToStr(Y);
  if PlotImage.Scale.IsValid then
  begin
    Pt := PlotImage.ConvertCoords(X, Y);
    StatusBar.Panels[2].Text := Format('%.3g, %.3g', [Pt.X, Pt.Y]);
  end
  else
    StatusBar.Panels[2].Text := '';
end;

procedure TDigitMainForm.PlotImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateZoomImage(X, Y);
end;

procedure TDigitMainForm.PlotImageChange(Sender: TObject);
begin
  IsSaved := False;
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

procedure TDigitMainForm.MainPanelResize(Sender: TObject);
begin
  ZoomImage.Height := ZoomImage.Width;
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
      UpdateZoomImage(P.X, P.Y);
  end;
end;

procedure TDigitMainForm.MarkersMoveDownExecute(Sender: TObject);
var
  P: TPoint;
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(0, 1));
    P := ScreenToClient(Mouse.CursorPos);
    if BoundsRect.Contains(P) then
      UpdateZoomImage(P.X, P.Y);
  end;
end;

procedure TDigitMainForm.MarkersMoveLeftExecute(Sender: TObject);
var
  P: TPoint;
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(-1, 0));
    P := ScreenToClient(Mouse.CursorPos);
    if BoundsRect.Contains(P) then
      UpdateZoomImage(P.X, P.Y);
  end;
end;

procedure TDigitMainForm.MarkersMoveRightExecute(Sender: TObject);
var
  P: TPoint;
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(1, 0));
    P := ScreenToClient(Mouse.CursorPos);
    if BoundsRect.Contains(P) then
      UpdateZoomImage(P.X, P.Y);
  end;
end;

procedure TDigitMainForm.MarkersMoveUpExecute(Sender: TObject);
var
  P: TPoint;
begin
  with PlotImage do
  begin
    ShiftActiveMarker(TPoint.Create(0, -1));
    P := ScreenToClient(Mouse.CursorPos);
    if BoundsRect.Contains(P) then
      UpdateZoomImage(P.X, P.Y);
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
  PlotImage.AdjustCurve(ProgressBar);
  PlotImage.Invalidate;
  CurveToGUI;
end;

procedure TDigitMainForm.ToolConvertToSymbolsExecute(Sender: TObject);
begin
  GUIToCurve;
  PlotImage.ConvertCurveToSymbols(ProgressBar);
  CurveToGUI;
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

procedure TDigitMainForm.ToolDigitExecute(Sender: TObject);
begin
  GUIToCurve;
  //Digitize curve
  PlotImage.DigitizeSpectrum(ProgressBar);
  CurveToGUI;
end;

procedure TDigitMainForm.ToolResampleExecute(Sender: TObject);
begin
  GUIToCurve;
  //Replace the curve by interpolated values
  PlotImage.Interpolate(seXo.Value, seXf.Value, seInterpPoints.Value);
  CurveToGUI;
end;

procedure TDigitMainForm.ToolScaleOptionsExecute(Sender: TObject);
var
  i: Integer;
  WasChanged: Boolean;
begin
  if ScaleDlg.Execute(PlotImage.Scale) then
  begin
    WasChanged := False;
    with PlotImage.Scale do
    begin
      if (CoordSystem <> ScaleDlg.CoordSystem) then
      begin
        CoordSystem := ScaleDlg.CoordSystem;
        WasChanged := True;
      end;
      if (XScale <> ScaleDlg.XScale) or (YScale <> ScaleDlg.YScale) then
      begin
        XScale := ScaleDlg.XScale;
        YScale := ScaleDlg.YScale;
        WasChanged := True;
      end;

      if (XLabel <> ScaleDlg.XLabel) or (YLabel <> ScaleDlg.YLabel) then
      begin
        XLabel := ScaleDlg.XLabel;
        YLabel := ScaleDlg.YLabel;

        leData.TitleCaptions[0] := XLabel;
        leData.TitleCaptions[1] := YLabel;
        leData.Invalidate;

        MainPlot.BottomAxis.Title.Caption := XLabel;
        MainPlot.LeftAxis.Title.Caption := YLabel;

        WasChanged := True;
      end;

      for i := 1 to 3 do
      begin
        if (ImagePoint[i] <> ScaleDlg.ImagePoint[i]) then
        begin
          ImagePoint[i] := ScaleDlg.ImagePoint[i];
          PlotImage.MoveMarker(PlotImage.AxesMarkers[i], ScaleDlg.ImagePoint[i]);
          WasChanged := True;
        end;
        if (PlotPoint[i] <> ScaleDlg.PlotPoint[i]) then
        begin
          PlotPoint[i] := ScaleDlg.PlotPoint[i];
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
//End of the action functions

end.
