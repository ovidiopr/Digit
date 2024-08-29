program digit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols,
  ucoordinates,
  umain {DigitMainForm},
  uabout {AboutBox},
  ucurves,
  urestore,
  uchartscale, uoptions, uutils, uplotimage, ugrid, uscale;

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
