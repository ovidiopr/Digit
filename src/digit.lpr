program digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, tachartlazaruspkg, lazcontrols,
  uCoordinates,
  uMain {DigitMainForm},
  uAbout {AboutBox},
  uCurves, uRestore, uChartScale, uOptions, uUtils, uPlotImage,
  uGrid, uScale, uPolygons, uCurveDigitizer, uMarker;

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
