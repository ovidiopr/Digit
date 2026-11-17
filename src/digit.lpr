program digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, tachartlazaruspkg, lazcontrols,
  uMain {DigitMainForm},
  uAbout {AboutBox},
  uRestore, uChartScale, uOptions;

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
