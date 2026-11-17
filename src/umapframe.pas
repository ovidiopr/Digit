unit uMapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, uDocFrame, uImageViewer, uCoordinates,
  uMapImage, uMapPanel;

type
  { TMapFrame }
  // Map document, its controls are built in code
  TMapFrame = class(TDocumentFrame)
  private
    { Private declarations }
    FLeftPanel: TPanel;
    FSplitter: TSplitter;
    FZoomImage: TImage;
    FPages: TPageControl;
    FImageSheet: TTabSheet;
    FLogSheet: TTabSheet;
    FScrollBox: TScrollBox;
    FLog: TListBox;

    procedure LeftPanelResize(Sender: TObject);
    procedure MapImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MapImageMouseLeave(Sender: TObject);
    procedure MapImageChange(Sender: TObject);
    procedure MapImagePrintMessage(Sender: TObject; Msg: String; MsgType: TMsgDlgType);
    procedure MapImageZoomChanged(Sender: TObject; Zoom: Double);
    procedure MapLoaded;
  public
    { Public declarations }
    MapImage: TMapImage;
    MapPanel: TMapPanel;

    constructor Create(AOwner: TComponent); override;

    function Kind: TDocumentKind; override;
    function Viewer: TImageViewer; override;
    function ZoomView: TImage; override;
    function LogBox: TListBox; override;

    function LoadImageFile(const FileName: TFileName): Boolean;
    function OpenProject(FileName: TFileName): Boolean;
    procedure SaveProject(FileName: TFileName); override;
    procedure UpdateControls; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure PasteImage(Stream: TStream); override;
  end;

implementation

{$R *.lfm}

uses
  uMain;

constructor TMapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Side panel: map tools on top, zoom inset at the bottom
  FLeftPanel := TPanel.Create(Self);
  FLeftPanel.Name := 'MapLeftPanel';
  FLeftPanel.Caption := '';
  FLeftPanel.BevelOuter := bvNone;
  FLeftPanel.Width := 220;
  FLeftPanel.Align := alLeft;
  FLeftPanel.Parent := Self;
  FLeftPanel.OnResize := @LeftPanelResize;

  FZoomImage := TImage.Create(Self);
  FZoomImage.Align := alBottom;
  FZoomImage.Height := FLeftPanel.Width;
  FZoomImage.Parent := FLeftPanel;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Align := alLeft;
  FSplitter.Left := FLeftPanel.Width + 1;
  FSplitter.Parent := Self;

  // Image and message log
  FPages := TPageControl.Create(Self);
  FPages.Align := alClient;
  FPages.Parent := Self;

  FImageSheet := FPages.AddTabSheet;
  FImageSheet.Caption := 'Image';
  FLogSheet := FPages.AddTabSheet;
  FLogSheet.Caption := 'Log';
  FPages.ActivePage := FImageSheet;

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.Align := alClient;
  FScrollBox.HorzScrollBar.Page := 1;
  FScrollBox.VertScrollBar.Page := 1;
  FScrollBox.ParentBackground := False;
  FScrollBox.Parent := FImageSheet;

  FLog := TListBox.Create(Self);
  FLog.Align := alClient;
  FLog.Style := lbOwnerDrawFixed;
  FLog.OnDrawItem := @msgAreaDrawItem;
  FLog.Parent := FLogSheet;

  MapImage := TMapImage.Create(FScrollBox);
  with MapImage do
  begin
    Parent := FScrollBox;
    Visible := False;
    Options := DigitMainForm.PlotOptions;
    OnMouseMove := @MapImageMouseMove;
    OnMouseUp := @MapImageMouseUp;
    OnMouseLeave := @MapImageMouseLeave;
    OnChange := @MapImageChange;
    OnPrintMessage := @MapImagePrintMessage;
    OnShowProgress := @ViewerShowProgress;
    OnHideProgress := @ViewerHideProgress;
    OnZoomChanged := @MapImageZoomChanged;
  end;

  MapPanel := TMapPanel.Create(Self);
  MapPanel.Parent := FLeftPanel;
  MapPanel.Align := alClient;
  MapPanel.MapImage := MapImage;
end;

function TMapFrame.Kind: TDocumentKind;
begin
  Result := dkMap;
end;

function TMapFrame.Viewer: TImageViewer;
begin
  Result := MapImage;
end;

function TMapFrame.ZoomView: TImage;
begin
  Result := FZoomImage;
end;

function TMapFrame.LogBox: TListBox;
begin
  Result := FLog;
end;

procedure TMapFrame.MapLoaded;
begin
  MapImage.Visible := MapImage.ImageIsLoaded;
  MapPanel.ParamsFromMap;
  UpdateControls;
end;

function TMapFrame.LoadImageFile(const FileName: TFileName): Boolean;
begin
  MapImage.Options := DigitMainForm.PlotOptions;
  Result := MapImage.LoadImageFile(FileName);
  if Result then
  begin
    DigitFileName := ChangeFileExt(FileName, '.dig');
    IsSaved := False;
  end;
  MapLoaded;
end;

function TMapFrame.OpenProject(FileName: TFileName): Boolean;
begin
  MapImage.Options := DigitMainForm.PlotOptions;
  Result := MapImage.LoadFromXML(FileName, DigitMainForm.OpenPictureDlg);
  if Result then
  begin
    DigitFileName := FileName;
    IsSaved := True;
  end;
  MapLoaded;
end;

procedure TMapFrame.SaveProject(FileName: TFileName);
begin
  if MapImage.SaveToXML(FileName) then
  begin
    DigitFileName := FileName;
    IsSaved := True;
  end;
end;

procedure TMapFrame.UpdateControls;
var
  i: Integer;
  ImageLoaded: Boolean;
  Cat: String;
begin
  // Only the active document drives the shell controls
  if not IsActive then Exit;

  ImageLoaded := MapImage.ImageIsLoaded;

  // Plot actions do not apply to maps
  for i := 0 to DigitMainForm.ActionList.ActionCount - 1 do
  begin
    Cat := TAction(DigitMainForm.ActionList.Actions[i]).Category;
    if (Cat = 'Tools') or (Cat = 'Mode') or (Cat = 'Markers') or
       (Cat = 'Grid') or (Cat = 'Chart') then
      TAction(DigitMainForm.ActionList.Actions[i]).Enabled := False;
  end;
  DigitMainForm.ToolCancelAction.Enabled := MapImage.RunningAction;

  DigitMainForm.FileSave.Enabled := ImageLoaded and (not IsSaved);
  DigitMainForm.FileSaveAs.Enabled := ImageLoaded;
  DigitMainForm.FileExport.Enabled := False;
  DigitMainForm.FileImportDigit.Enabled := False;

  DigitMainForm.EditUndo.Enabled := MapImage.CanUndo;
  DigitMainForm.EditRedo.Enabled := MapImage.CanRedo;
  DigitMainForm.EditCopyCurve.Enabled := False;
  DigitMainForm.EditShowLine.Enabled := False;
  DigitMainForm.EditShowSymbols.Enabled := False;

  DigitMainForm.tbZoom.Enabled := ImageLoaded;
  DigitMainForm.EditZoomOriginal.Enabled := ImageLoaded and (MapImage.Zoom <> 1);
  DigitMainForm.EditZoomIn.Enabled := ImageLoaded;
  DigitMainForm.EditZoomOut.Enabled := ImageLoaded;
  DigitMainForm.EditZoomFit.Enabled := ImageLoaded;
  DigitMainForm.EditZoomResize.Enabled := ImageLoaded and (MapImage.Zoom <> 1);

  for i := 0 to DigitMainForm.MapMenu.Count - 1 do
    if (DigitMainForm.MapMenu.Items[i].Count > 0) then
      DigitMainForm.MapMenu.Items[i].Items[Ord(MapImage.Tool)].Checked := True;

  MapPanel.UpdateControls;
end;

procedure TMapFrame.Undo;
begin
  MapImage.Undo;
  UpdateControls;
end;

procedure TMapFrame.Redo;
begin
  MapImage.Redo;
  UpdateControls;
end;

procedure TMapFrame.PasteImage(Stream: TStream);
begin
  // Maps just replace the image
  if MapImage.ImageIsLoaded and (MessageDlg(
    'You are about to replace the current image.' +
    ' This action cannot be undone. Continue?', mtWarning, [mbYes, mbNo], 0) = mrNo)
  then Exit;

  MapImage.Reset;
  MapImage.PasteImage(Stream);
  IsSaved := False;
  MapLoaded;
end;

procedure TMapFrame.LeftPanelResize(Sender: TObject);
begin
  FZoomImage.Height := FLeftPanel.Width;
end;

procedure TMapFrame.MapImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  P: TCurvePoint;
  z: Double;
  S: String;
begin
  UpdateZoomImage(X, Y);

  DigitMainForm.StatusBar.Panels[1].Text := Format('%.1f, %.1f px', [X/MapImage.Zoom, Y/MapImage.Zoom]);
  if MapImage.MapScale.IsCalibrated then
  begin
    P := MapImage.MapCoordAt(X, Y);
    DigitMainForm.StatusBar.Panels[2].Text := Format('%.4g, %.4g %s', [P.X, P.Y, MapImage.MapScale.Units]);
  end
  else
    DigitMainForm.StatusBar.Panels[2].Text := '';

  S := Format('I = %.4g', [MapImage.ValueAt(X, Y)]);
  z := MapImage.ZAt(X, Y);
  if not IsNan(z) then
    S := S + Format('   z = %.4g %s', [z, MapImage.ColorBar.Units]);
  DigitMainForm.StatusBar.Panels[3].Text := S;
end;

procedure TMapFrame.MapImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateControls;
end;

procedure TMapFrame.MapImageMouseLeave(Sender: TObject);
begin
  ClearZoomImage;

  DigitMainForm.StatusBar.Panels[1].Text := '';
  DigitMainForm.StatusBar.Panels[2].Text := '';
end;

procedure TMapFrame.MapImageChange(Sender: TObject);
begin
  IsSaved := False;
end;

procedure TMapFrame.MapImagePrintMessage(Sender: TObject; Msg: String;
  MsgType: TMsgDlgType);
begin
  PrintUserMessage(Msg, MsgType);
end;

procedure TMapFrame.MapImageZoomChanged(Sender: TObject; Zoom: Double);
begin
  DigitMainForm.tbZoom.Value := Round(Zoom*100);
  UpdateControls;
end;

end.
