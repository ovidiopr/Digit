{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DigitImage;

{$warn 5023 off : no warning about unused units}
interface

uses
  uCoordinates, uUtils, uCurves, uPolygons, uScale, uMarker, uGrid,
  uCurveDigitizer, uImageViewer, uPlotImage, uMapScale, uMapFeatures, uMapRaster,
  uMapImage, DigitImageReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DigitImageReg', @DigitImageReg.Register);
end;

initialization
  RegisterPackage('DigitImage', @Register);
end.
