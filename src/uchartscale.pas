unit uchartscale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ValEdit;

type

  TMyZoom = record
    ManualZoom : Boolean;
    Xmin       : Double;
    Xmax       : Double;
    Ymin       : Double;
    Ymax       : Double;
  end;

  { TChartScaleDlg }

  TChartScaleDlg = class(TForm)
    cbManualScale: TCheckBox;
    BtnCancel: TBitBtn;
    BtnAccept: TBitBtn;
    BtnPanel: TPanel;
    txtValueEditor: TValueListEditor;
    procedure cbManualScaleChange(Sender: TObject);
  private
    { private declarations }
    FZoom: TMyZoom;
  public
    { public declarations }
    function Execute(ManualZoom: Boolean; Xmin, Xmax, Ymin, Ymax: Double): Boolean;

    property Zoom: TMyZoom read FZoom write FZoom;
  end;

var
  ChartScaleDlg: TChartScaleDlg;

implementation

{$R *.lfm}

procedure TChartScaleDlg.cbManualScaleChange(Sender: TObject);
begin
  txtValueEditor.Enabled := cbManualScale.Checked;
end;

function TChartScaleDlg.Execute(ManualZoom: Boolean; Xmin, Xmax, Ymin, Ymax: Double): Boolean;
begin
  cbManualScale.Checked := ManualZoom;
  cbManualScaleChange(Self);

  txtValueEditor.Values['X min'] := FloatToStr(XMin);
  txtValueEditor.Values['X max'] := FloatToStr(XMax);
  txtValueEditor.Values['Y min'] := FloatToStr(YMin);
  txtValueEditor.Values['Y max'] := FloatToStr(YMax);

  Result := (ShowModal = mrOK);

  if Result then
  begin
    FZoom.ManualZoom := cbManualScale.Checked;

    FZoom.XMin := StrToFloat(txtValueEditor.Values['X min']);
    FZoom.XMax := StrToFloat(txtValueEditor.Values['X max']);
    FZoom.YMin := StrToFloat(txtValueEditor.Values['Y min']);
    FZoom.YMax := StrToFloat(txtValueEditor.Values['Y max']);
  end;
end;

end.

