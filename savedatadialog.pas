unit SaveDataDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ButtonPanel, EditBtn, Spin, StdCtrls;

type

  { TSaveDataDlg }

  TSaveDataDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    seNoPoints: TSpinEdit;
    txtFileName: TFileNameEdit;
    seStartingPoint: TFloatSpinEdit;
    seEndPoint: TFloatSpinEdit;
    lblStartingPoint: TLabel;
    lblNoPoints: TLabel;
    lblFileName: TLabel;
    lblEndPoint: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SaveDataDlg: TSaveDataDlg;

implementation

{$R *.lfm}

end.

