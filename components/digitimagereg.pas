unit DigitImageReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, uPlotImage, uMapImage;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digit', [TPlotImage, TMapImage]);
end;

end.
