program digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces,
  Forms, tachartlazaruspkg, lazcontrols,
  ucoordinates,
  umain {DigitMainForm},
  uabout {AboutBox},
  ucurves,
  urestore,
  uchartscale, uoptions, uutils, uplotimage, ugrid, uscale, upolygons,
CurveDigitizer, umarker;

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
