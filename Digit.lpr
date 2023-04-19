program Digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg,
  coordinates in 'coordinates.pas',
  Main in 'Main.pas' {DigitMainForm},
  About in 'About.pas' {AboutBox},
  curves in 'curves.pas',
  restore,
  uchartscale, scaledialog, utils, plotimage;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigitMainForm, DigitMainForm);
  Application.CreateForm(TChartScaleDlg, ChartScaleDlg);
  Application.CreateForm(TScaleDlg, ScaleDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
