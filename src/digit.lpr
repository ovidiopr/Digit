program digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols,
  coordinates in 'coordinates.pas',
  umain {DigitMainForm},
  uabout {AboutBox},
  curves in 'curves.pas',
  restore,
  uchartscale, uoptions, utils, plotimage;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Digit';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigitMainForm, DigitMainForm);
  Application.CreateForm(TChartScaleDlg, ChartScaleDlg);
  Application.CreateForm(TOptionsDlg, OptionsDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
