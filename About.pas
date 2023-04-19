unit About;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Comments: TMemo;
    OKButton: TButton;
    TxtPanel: TPanel;
    BtnPanel: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

end.
 
