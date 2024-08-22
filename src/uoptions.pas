unit uoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, coordinates;

type
  { TOptionsDlg }

  TOptionsDlg = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    cbbCoords: TComboBox;
    cbbXScale: TComboBox;
    cbbYScale: TComboBox;
    EditIX1: TEdit;
    EditIX2: TEdit;
    EditIX3: TEdit;
    EditIY1: TEdit;
    EditIY2: TEdit;
    EditIY3: TEdit;
    EditPX1: TEdit;
    EditPX2: TEdit;
    EditPX3: TEdit;
    EditPY1: TEdit;
    EditPY2: TEdit;
    EditPY3: TEdit;
    edtX: TEdit;
    edtY: TEdit;
    gbX: TGroupBox;
    gbY: TGroupBox;
    lblImg1: TLabel;
    lblImg2: TLabel;
    lblImg3: TLabel;
    lblP1: TLabel;
    lblP2: TLabel;
    lblP3: TLabel;
    lblPlt1: TLabel;
    lblPlt2: TLabel;
    lblPlt3: TLabel;
    lblType: TLabel;
    lblX: TLabel;
    lblX1: TLabel;
    lblX2: TLabel;
    lblX3: TLabel;
    lblXScale: TLabel;
    lblY: TLabel;
    lblY1: TLabel;
    lblY2: TLabel;
    lblY3: TLabel;
    lblYScale: TLabel;
    pcScale: TPageControl;
    pnButtons: TPanel;
    tsBasis: TTabSheet;
    tsCoordinates: TTabSheet;
    procedure cbbCoordsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    function GetCoords: TCoordSystem;
    function GetXScale: TScaleType;
    function GetYScale: TScaleType;
    function GetXLabel: String;
    function GetYLabel: String;
    function GetImagePoint(Index: Integer): TCurvePoint;
    function GetPlotPoint(Index: Integer): TCurvePoint;

    procedure SetCoords(Value: TCoordSystem);
    procedure SetXScale(Value: TScaleType);
    procedure SetYScale(Value: TScaleType);
    procedure SetXLabel(Value: String);
    procedure SetYLabel(Value: String);
    procedure SetImagePoint(Index: Integer; const Value: TCurvePoint);
    procedure SetPlotPoint(Index: Integer; const Value: TCurvePoint);
  public
    { public declarations }
    function Execute(Scale: TScale; ActivePoint: Integer = 0;
                     X: Integer = 0; Y:Integer = 0): Boolean;

    property CoordSystem: TCoordSystem read GetCoords write SetCoords;
    property XScale: TScaleType read GetXScale write SetXScale;
    property YScale: TScaleType read GetYScale write SetYScale;
    property XLabel: String read GetXLabel write SetXLabel;
    property YLabel: String read GetYLabel write SetYLabel;
    property ImagePoint[Index: Integer]: TCurvePoint read GetImagePoint write SetImagePoint;
    property PlotPoint[Index: Integer]: TCurvePoint read GetPlotPoint write SetPlotPoint;
  end;

var
  OptionsDlg: TOptionsDlg;

implementation

{$R *.lfm}

function TOptionsDlg.GetCoords: TCoordSystem;
begin
  case cbbCoords.ItemIndex of
    0, 1: Result := TCoordSystem(cbbCoords.ItemIndex);
    else
      Result := csCartesian;
  end;
end;

function TOptionsDlg.GetXScale: TScaleType;
begin
  case cbbXScale.ItemIndex of
    0, 1, 2, 3: Result := TScaleType(cbbXScale.ItemIndex);
    else
      Result := stLinear;
  end;
end;

function TOptionsDlg.GetYScale: TScaleType;
begin
  case cbbYScale.ItemIndex of
    0, 1, 2, 3: Result := TScaleType(cbbYScale.ItemIndex);
    else
      Result := stLinear;
  end;
end;

function TOptionsDlg.GetXLabel: String;
begin
  Result := edtX.Text;
end;

function TOptionsDlg.GetYLabel: String;
begin
  Result := edtY.Text;
end;

function TOptionsDlg.GetImagePoint(Index: Integer): TCurvePoint;
begin
  case Index of
    1: Result := TCurvePoint.Create(StrToFloat(EditIX1.Text),
                               StrToFloat(EditIY1.Text));
    2: Result := TCurvePoint.Create(StrToFloat(EditIX2.Text),
                               StrToFloat(EditIY2.Text));
    3: Result := TCurvePoint.Create(StrToFloat(EditIX3.Text),
                               StrToFloat(EditIY3.Text));
    else Result := TCurvePoint.Create(0, 0);
  end;
end;

function TOptionsDlg.GetPlotPoint(Index: Integer): TCurvePoint;
begin
  case Index of
    1: Result := TCurvePoint.Create(StrToFloat(EditPX1.Text),
                               StrToFloat(EditPY1.Text));
    2: Result := TCurvePoint.Create(StrToFloat(EditPX2.Text),
                               StrToFloat(EditPY2.Text));
    3: Result := TCurvePoint.Create(StrToFloat(EditPX3.Text),
                               StrToFloat(EditPY3.Text));
    else Result := TCurvePoint.Create(0, 0);
  end;
end;

procedure TOptionsDlg.SetCoords(Value: TCoordSystem);
begin
  case Value of
    csCartesian,
    csPolar: cbbCoords.ItemIndex := Integer(Value);
    else
      cbbCoords.ItemIndex := -1;
  end;
  cbbCoordsChange(cbbCoords);
end;

procedure TOptionsDlg.SetXScale(Value: TScaleType);
begin
  case Value of
    stLinear,
    stLog,
    stLn,
    stInverse: cbbXScale.ItemIndex := Integer(Value);
    else
      cbbXScale.ItemIndex := -1;
  end;
end;

procedure TOptionsDlg.SetYScale(Value: TScaleType);
begin
  case Value of
    stLinear,
    stLog,
    stLn,
    stInverse: cbbYScale.ItemIndex := Integer(Value);
    else
      cbbYScale.ItemIndex := -1;
  end;
end;

procedure TOptionsDlg.SetXLabel(Value: String);
begin
  edtX.Text := Value;
end;

procedure TOptionsDlg.SetYLabel(Value: String);
begin
  edtY.Text := Value;
end;

procedure TOptionsDlg.SetImagePoint(Index: Integer; const Value: TCurvePoint);
begin
  case Index of
    1: begin
      EditIX1.Text := FloatToStr(Value.X);
      EditIY1.Text := FloatToStr(Value.Y);
    end;
    2: begin
      EditIX2.Text := FloatToStr(Value.X);
      EditIY2.Text := FloatToStr(Value.Y);
    end;
    3: begin
      EditIX3.Text := FloatToStr(Value.X);
      EditIY3.Text := FloatToStr(Value.Y);
    end;
  end;
end;

procedure TOptionsDlg.SetPlotPoint(Index: Integer; const Value: TCurvePoint);
begin
  case Index of
    1: begin
      EditPX1.Text := FloatToStr(Value.X);
      EditPY1.Text := FloatToStr(Value.Y);
    end;
    2: begin
      EditPX2.Text := FloatToStr(Value.X);
      EditPY2.Text := FloatToStr(Value.Y);
    end;
    3: begin
      EditPX3.Text := FloatToStr(Value.X);
      EditPY3.Text := FloatToStr(Value.Y);
    end;
  end;
end;

procedure TOptionsDlg.cbbCoordsChange(Sender: TObject);
begin
  if (TCoordSystem(cbbCoords.ItemIndex ) = csCartesian) then
  begin
    gbX.Caption := 'X:';
    lblX1.Caption := 'X:';
    lblX2.Caption := 'X:';
    lblX3.Caption := 'X:';
    cbbXScale.Items.Strings[3] := 'Inverse (1/X)';
    gbY.Caption := 'Y:';
    lblY1.Caption := 'Y:';
    lblY2.Caption := 'Y:';
    lblY3.Caption := 'Y:';
    cbbYScale.Items.Strings[3] := 'Inverse (1/Y)';
  end
  else
  begin
    gbX.Caption := 'θ:';
    lblX1.Caption := 'θ:';
    lblX2.Caption := 'θ:';
    lblX3.Caption := 'θ:';
    cbbXScale.Items.Strings[3] := 'Inverse (1/θ)';
    gbY.Caption := 'ρ:';
    lblY1.Caption := 'ρ:';
    lblY2.Caption := 'ρ:';
    lblY3.Caption := 'ρ:';
    cbbYScale.Items.Strings[3] := 'Inverse (1/ρ)';
  end;
end;


procedure TOptionsDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  CoordSystem := csCartesian;
  XScale := stLinear;
  YScale := stLinear;
  XLabel := 'X';
  YLabel := 'Y';

  for i := 1 to 3 do
  begin
    ImagePoint[i] := TCurvePoint.Create(0, 0);
    PlotPoint[i] := TCurvePoint.Create(0, 0);
  end;
end;

function TOptionsDlg.Execute(Scale: TScale; ActivePoint: Integer = 0;
                           X: Integer = 0; Y:Integer = 0): Boolean;
var
  i: Integer;
  EditBasis: Boolean;
begin
  EditBasis := (ActivePoint >=1) and (ActivePoint <= 3);

  CoordSystem := Scale.CoordSystem;
  XScale := Scale.XScale;
  YScale := Scale.YScale;
  XLabel := Scale.XLabel;
  YLabel := Scale.YLabel;

  for i := 1 to 3 do
  begin
    ImagePoint[i] := Scale.ImagePoint[i];
    PlotPoint[i] := Scale.PlotPoint[i];
  end;

  if EditBasis then
    ImagePoint[ActivePoint] := TCurvePoint.Create(X, Y);

  EditIX1.Enabled := (Scale.PointIsSet[1] and (not EditBasis)) or (ActivePoint = 1);
  EditIY1.Enabled := (Scale.PointIsSet[1] and (not EditBasis)) or (ActivePoint = 1);
  EditPX1.Enabled := (Scale.PointIsSet[1] and (not EditBasis)) or (ActivePoint = 1);
  EditPY1.Enabled := (Scale.PointIsSet[1] and (not EditBasis)) or (ActivePoint = 1);
  EditIX2.Enabled := (Scale.PointIsSet[2] and (not EditBasis)) or (ActivePoint = 2);
  EditIY2.Enabled := (Scale.PointIsSet[2] and (not EditBasis)) or (ActivePoint = 2);
  EditPX2.Enabled := (Scale.PointIsSet[2] and (not EditBasis)) or (ActivePoint = 2);
  EditPY2.Enabled := (Scale.PointIsSet[2] and (not EditBasis)) or (ActivePoint = 2);
  EditIX3.Enabled := (Scale.PointIsSet[3] and (not EditBasis)) or (ActivePoint = 3);
  EditIY3.Enabled := (Scale.PointIsSet[3] and (not EditBasis)) or (ActivePoint = 3);
  EditPX3.Enabled := (Scale.PointIsSet[3] and (not EditBasis)) or (ActivePoint = 3);
  EditPY3.Enabled := (Scale.PointIsSet[3] and (not EditBasis)) or (ActivePoint = 3);

  if EditBasis then
    pcScale.TabIndex := 1
  else
    pcScale.TabIndex := 0;

  tsCoordinates.Enabled := not EditBasis;

  Result := (ShowModal = mrOK);
end;

end.

