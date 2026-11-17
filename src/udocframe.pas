unit uDocFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCLType, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, uImageViewer;

type
  TDocumentKind = (dkPlot, dkMap);

  { TDocumentFrame }
  // Base of the documents shown in the main window tabs
  TDocumentFrame = class(TFrame)
    procedure msgAreaDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
    FDigitFileName: TFileName;
    FIsSaved: Boolean;
  protected
    procedure SetDigitFileName(Value: TFileName); virtual;
    procedure SetIsSaved(Value: Boolean); virtual;
    procedure StoreSaved(Value: Boolean);
    procedure DocumentChanged;
    procedure ShowCancelButton(AVisible: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    function Kind: TDocumentKind; virtual; abstract;
    function Viewer: TImageViewer; virtual; abstract;
    function ZoomView: TImage; virtual; abstract;
    function LogBox: TListBox; virtual; abstract;

    function Title: String;
    function IsActive: Boolean;
    function IsBlank: Boolean; virtual;
    function CanClose: Boolean; virtual;

    procedure SaveProject(FileName: TFileName); virtual; abstract;
    procedure UpdateControls; virtual; abstract;
    procedure Activate; virtual;
    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure PasteImage(Stream: TStream); virtual; abstract;

    procedure PrintUserMessage(Msg: String; MsgType: TMsgDlgType = mtInformation);
    procedure UpdateZoomImage(X, Y: Integer); overload;
    procedure UpdateZoomImage(P: TPoint); overload;
    procedure ClearZoomImage;
    procedure ViewerShowProgress(Sender: TObject; Progress: Cardinal; Msg: String);
    procedure ViewerHideProgress(Sender: TObject);

    property DigitFileName: TFileName read FDigitFileName write SetDigitFileName;
    property IsSaved: Boolean read FIsSaved write SetIsSaved;
  end;

implementation

{$R *.lfm}

uses
  uMain;

constructor TDocumentFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDigitFileName := '';
  FIsSaved := True;
end;

procedure TDocumentFrame.SetDigitFileName(Value: TFileName);
begin
  FDigitFileName := Value;
  DocumentChanged;
end;

procedure TDocumentFrame.SetIsSaved(Value: Boolean);
begin
  StoreSaved(Value);
  UpdateControls;
end;

procedure TDocumentFrame.StoreSaved(Value: Boolean);
begin
  FIsSaved := Value;
  DocumentChanged;
end;

procedure TDocumentFrame.DocumentChanged;
begin
  // The shell updates the tab and window captions
  if Assigned(DigitMainForm) then
    DigitMainForm.DocumentChanged(Self);
end;

procedure TDocumentFrame.ShowCancelButton(AVisible: Boolean);
begin
  // Nothing by default
end;

function TDocumentFrame.Title: String;
begin
  if (FDigitFileName = '') then
    Result := 'noname.dig'
  else
    Result := ExtractFileName(FDigitFileName);
end;

function TDocumentFrame.IsActive: Boolean;
begin
  Result := Assigned(DigitMainForm) and (DigitMainForm.ActiveDocument = Self);
end;

function TDocumentFrame.IsBlank: Boolean;
begin
  Result := IsSaved and (not Viewer.ImageIsLoaded);
end;

function TDocumentFrame.CanClose: Boolean;
begin
  Result := not Viewer.RunningAction;
  if not Result then Exit;

  if (not IsSaved) then
    case MessageDlg('The file ' + Title +
        ' was modified. Do you want to save it?', mtInformation,
        [mbYes, mbNo, mbCancel], 0) of

      mrYes: Result := DigitMainForm.SaveDocument(Self, False);
      mrCancel: Result := False;
    end;
end;

procedure TDocumentFrame.Activate;
begin
  DigitMainForm.StatusBar.Panels[1].Text := '';
  DigitMainForm.StatusBar.Panels[2].Text := '';
  DigitMainForm.StatusBar.Panels[3].Text := '';
  DigitMainForm.tbZoom.Value := Round(Viewer.Zoom*100);

  UpdateControls;
end;

procedure TDocumentFrame.Undo;
begin
  // Nothing by default
end;

procedure TDocumentFrame.Redo;
begin
  // Nothing by default
end;

procedure TDocumentFrame.PrintUserMessage(Msg: String; MsgType: TMsgDlgType = mtInformation);
begin
  with LogBox.Items do
  begin
    case MsgType of
      mtWarning: // Amarillo $07FCFD
        AddObject(TimeToStr(Time) + ': ' + Msg, TObject(PtrUInt($07FCFD)));
      mtError:  // Rojo $5B53FF
        AddObject(TimeToStr(Time) + ': ' + Msg, TObject(PtrUInt($5B53FF)));
      mtConfirmation:  // Verde $51D450
        AddObject(TimeToStr(Time) + ': ' + Msg, TObject(PtrUInt($51D450)));
      else
        AddObject(TimeToStr(Time) + ': ' + Msg, TObject(PtrUInt(LogBox.Color)));
    end;
  end;

  LogBox.ItemIndex := LogBox.Items.Count - 1;
end;

procedure TDocumentFrame.msgAreaDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  lb: TListBox absolute Control;
  ts: TTextStyle;
begin
  lb.Canvas.Brush.Color := TColor(PtrUInt(lb.Items.Objects[Index]));
  lb.Canvas.FillRect(ARect);

  ts := lb.Canvas.TextStyle;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlCenter;
  lb.Canvas.Pen.Color := clBlack;
  lb.Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top, lb.Items[Index], ts);
end;

procedure TDocumentFrame.UpdateZoomImage(X, Y: Integer);
const
  span = 20;
var
  ImgRect: TRect;
  TempBmp: TBitmap;
  Xo, Xf, Yo, Yf, Xc, Yc: Integer;
begin
  // No ProcessMessages here, the viewer already flushed the queue before this event

  if (X < span) then
  begin
    Xo := 0;
    Xf := 2*span + 1;
  end
  else if (X > (Viewer.Width - span - 1)) then
  begin
    Xf := Viewer.Width;
    Xo := Xf - 2*span - 1;
  end
  else
  begin
    Xo := X - span;
    Xf := X + span + 1;
  end;

  if (Y < span) then
  begin
    Yo := 0;
    Yf := 2*span + 1;
  end
  else if (Y > (Viewer.Height - span - 1)) then
  begin
    Yf := Viewer.Height;
    Yo := Yf - 2*span - 1;
  end
  else
  begin
    Yo := Y - span;
    Yf := Y + span + 1;
  end;

  ImgRect := Rect(Xo, Yo, Xf, Yf);

  try
    TempBmp := Viewer.GetZoomImage(ZoomView.Width, ZoomView.Height, ImgRect);
    with TempBmp do
    begin
      Xc := Width*(X - Xo) div (2*span);
      Yc := Height*(Y - Yo) div (2*span);

      Canvas.Pen.Mode := pmNot;
      Canvas.Line(Xc - 10, Yc, Xc + 11, Yc);
      Canvas.Line(Xc, Yc - 10, Xc, Yc + 11);
    end;

    ZoomView.Picture.Assign(TempBmp);
  finally
    TempBmp.Free;
  end;
end;

procedure TDocumentFrame.UpdateZoomImage(P: TPoint);
begin
  UpdateZoomImage(P.X, P.Y);
end;

procedure TDocumentFrame.ClearZoomImage;
begin
  with ZoomView.Canvas do
  begin
    Brush.Color := clBtnFace;
    Pen.Mode := pmCopy;
    Pen.Color := clBtnFace;
    Pen.Style := psSolid;
    Rectangle(0, 0, ZoomView.Width, ZoomView.Height);
  end;
end;

procedure TDocumentFrame.ViewerShowProgress(Sender: TObject;
  Progress: Cardinal; Msg: String);
begin
  ShowCancelButton(True);
  DigitMainForm.ToolCancelAction.Enabled := True;
  DigitMainForm.ProgressBar.Visible := True;
  DigitMainForm.ProgressBar.Position := Progress;
  DigitMainForm.StatusBar.Panels[4].Text := Format('%s (Esc to cancel)', [Msg]);
  Application.ProcessMessages;
end;

procedure TDocumentFrame.ViewerHideProgress(Sender: TObject);
begin
  ShowCancelButton(False);
  DigitMainForm.ToolCancelAction.Enabled := False;
  DigitMainForm.ProgressBar.Visible := False;
  DigitMainForm.StatusBar.Panels[4].Text := '';
end;

end.
