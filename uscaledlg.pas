unit uscaledlg;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type

  { TFScaleDlg }

  TFScaleDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditIX1: TEdit;
    EditIY1: TEdit;
    EditGX1: TEdit;
    EditGY1: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    EditIX2: TEdit;
    EditIY2: TEdit;
    EditGX2: TEdit;
    EditGY2: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    EditIX3: TEdit;
    EditIY3: TEdit;
    EditGX3: TEdit;
    EditGY3: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FScaleDlg: TFScaleDlg;

implementation

uses Main;

{$R *.lfm}

procedure TFScaleDlg.FormActivate(Sender: TObject);
begin
  case DigitMainForm.ActivePoint of
    0: begin
      EditIX1.SelectAll;
      EditIX1.SetFocus;
    end;
    1: begin
      EditIX2.SelectAll;
      EditIX2.SetFocus;
    end;
    else begin
      EditIX3.SelectAll;
      EditIX3.SetFocus;
    end;
  end;
end;

end.
