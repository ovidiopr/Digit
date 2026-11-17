unit DigitImageReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, uPlotImage;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Digit', [TPlotImage]);
end;

end.
