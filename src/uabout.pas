unit uabout;

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  { TAboutBox }
  TAboutBox = class(TForm)
    AboutImage: TImage;
    lblDesc: TLabel;
    lblProgName: TLabel;
    lblAuthor: TLabel;
    btnOK: TButton;
    Bevel1: TBevel;
    lblVersion: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  fileinfo,
  winpeimagereader, {need this for reading exe info}
  elfreader, {needed for reading ELF executables}
  machoreader; {needed for reading MACH-O executables}

function GetCompilationTime: TDateTime;
var
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.TimeSeparator := ':';
  fs.DecimalSeparator := '.';
  fs.ShortDateFormat := 'yyyy"/"mm"/"dd';
  fs.LongTimeFormat := 'hh":"nn":"ss';
  fs.DateSeparator := '/';
  Result := StrToDateTime({$I %DATE%} + ' ' + {$I %TIME%}, fs)
end; 

function GetProgramVersion: String;
var
  info: TFileVersionInfo;
begin
  info := TFileVersionInfo.Create(Nil);
  try
    info.ReadFileInfo;
    Result := Format('%s [built %s]',
                     [info.VersionStrings.Values['ProductVersion'],
                      FormatDateTime('yyyy-mm-dd', GetCompilationTime)]);
  finally
    info.Free;
  end;
end;

procedure TAboutBox.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  lblVersion.Caption := 'Version ' + GetProgramVersion;
end;

end.
