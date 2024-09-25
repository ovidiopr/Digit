unit uoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, uutils;

type
  { TOptionsDlg }

  TOptionsDlg = class(TForm)
    btnDefaults: TBitBtn;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    btnXAxisColor: TColorButton;
    btnOriginColor: TColorButton;
    btnYAxisColor: TColorButton;
    btnBgndColor: TColorButton;
    cbbDigitType: TComboBox;
    cbbInterpType: TComboBox;
    chbShowXAxis: TCheckBox;
    chbShowYAxis: TCheckBox;
    gbX: TGroupBox;
    gbX1: TGroupBox;
    gbY: TGroupBox;
    lblInterpType: TLabel;
    lblDigitType: TLabel;
    lblXAxis: TLabel;
    lblBgndColor: TLabel;
    lblOrigin: TLabel;
    lblYAxis: TLabel;
    pcScale: TPageControl;
    pnButtons: TPanel;
    tsAxes: TTabSheet;
    tsDefaults: TTabSheet;
    procedure btnDefaultsClick(Sender: TObject);
  private
    { private declarations }
    function GetOptions: TPlotOptions;

    procedure SetOptions(Value: TPlotOptions);
  public
    { public declarations }
    function Execute(Opt: TPlotOptions): Boolean;

    property Options: TPlotOptions read GetOptions write SetOptions;
  end;

var
  OptionsDlg: TOptionsDlg;

implementation

{$R *.lfm}

function TOptionsDlg.GetOptions: TPlotOptions;
begin
  Result.BgndColor := btnBgndColor.ButtonColor;

  case cbbDigitType.ItemIndex of
    0..2: Result.DefaultDig := TDigitization(cbbDigitType.ItemIndex);
    else
      Result.DefaultDig := digLine;
  end;

  case cbbInterpType.ItemIndex of
    0..3: Result.DefaultItp := TInterpolation(cbbInterpType.ItemIndex);
    else
      Result.DefaultItp := itpBSpline;
  end;

  Result.ShowXAxis := chbShowXAxis.Checked;
  Result.ShowYAxis := chbShowYAxis.Checked;
  Result.OriginColor := btnOriginColor.ButtonColor;
  Result.XAxisColor := btnXAxisColor.ButtonColor;
  Result.YAxisColor := btnYAxisColor.ButtonColor;
end;

procedure TOptionsDlg.SetOptions(Value: TPlotOptions);
begin
  btnBgndColor.ButtonColor := Value.BgndColor;

  case Value.DefaultDig of
    digLine,
    digColor,
    digMarkers: cbbDigitType.ItemIndex := Integer(Value.DefaultDig);
    else
      cbbDigitType.ItemIndex := -1;
  end;

  case Value.DefaultItp of
    itpBSpline,
    itpSpline,
    itpLinear,
    itpPoly: cbbInterpType.ItemIndex := Integer(Value.DefaultItp);
    else
      cbbInterpType.ItemIndex := -1;
  end;

  chbShowXAxis.Checked := Value.ShowXAxis;
  chbShowYAxis.Checked := Value.ShowYAxis;
  btnOriginColor.ButtonColor := Value.OriginColor;
  btnXAxisColor.ButtonColor := Value.XAxisColor;
  btnYAxisColor.ButtonColor := Value.YAxisColor;
end;

procedure TOptionsDlg.btnDefaultsClick(Sender: TObject);
var
  TmpOpt: TPlotOptions;
begin
  Options := TmpOpt;
end;

function TOptionsDlg.Execute(Opt: TPlotOptions): Boolean;
begin
  Options := Opt;

  Result := (ShowModal = mrOK);
end;

end.

