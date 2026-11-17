unit uMapPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Types, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Spin, Dialogs, Graphics, TAGraph, TASeries, uCoordinates, uMapScale,
  uMapFeatures, uParticles, uMapImage;

type
  // Side panel with the map tools, calibration, features and analyses
  TMapPanel = class(TScrollBox)
  private
    FMap: TMapImage;
    FUpdating: Boolean;

    gbCalib: TGroupBox;
    lblCalib: TLabel;
    chkYUp: TCheckBox;
    btnClearCalib: TButton;

    gbFeatures: TGroupBox;
    lvFeatures: TListView;
    btnFinish: TButton;
    btnDelete: TButton;
    btnExportVertices: TButton;
    btnExportSummary: TButton;
    btnExportProfile: TButton;

    gbColorBar: TGroupBox;
    lblColorBar: TLabel;
    seStep: TSpinEdit;
    btnExportGrid: TButton;
    btnClearColorBar: TButton;

    gbParticles: TGroupBox;
    chkAuto: TCheckBox;
    seThreshold: TFloatSpinEdit;
    chkDark: TCheckBox;
    seOpen: TSpinEdit;
    seClose: TSpinEdit;
    chkFill: TCheckBox;
    seMinArea: TSpinEdit;
    chkEdges: TCheckBox;
    chkShow: TCheckBox;
    btnAnalyze: TButton;
    btnExportParticles: TButton;
    lblResults: TLabel;
    chHist: TChart;
    srHist: TBarSeries;

    SaveDlg: TSaveDialog;

    procedure SetMap(Value: TMapImage);
    function NewGroup(const ACaption: String): TGroupBox;
    function NewButton(AParent: TWinControl; const ACaption: String; AOnClick: TNotifyEvent): TButton;
    function NewCheck(AParent: TWinControl; const ACaption: String; AChecked: Boolean): TCheckBox;
    function NewSpin(AParent: TWinControl; const ALabel: String; AMin, AMax, AValue: Integer): TSpinEdit;
    function AskFile(const ATitle, AName: String): Boolean;

    procedure YUpClick(Sender: TObject);
    procedure ClearCalibClick(Sender: TObject);
    procedure FeatureSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FinishClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure ExportVerticesClick(Sender: TObject);
    procedure ExportSummaryClick(Sender: TObject);
    procedure ExportProfileClick(Sender: TObject);
    procedure ExportGridClick(Sender: TObject);
    procedure ClearColorBarClick(Sender: TObject);
    procedure AnalyzeClick(Sender: TObject);
    procedure ExportParticlesClick(Sender: TObject);
    procedure ShowOutlinesClick(Sender: TObject);

    procedure MapScaleBarPicked(Sender: TObject; const P1, P2: TCurvePoint);
    procedure MapReferencePicked(Sender: TObject; const ImgPts: TScalePoints);
    procedure MapColorBarPicked(Sender: TObject; const P1, P2: TCurvePoint);
    procedure MapFeatureAdded(Sender: TObject; Feature: TMapFeature);
    procedure MapSelectionChanged(Sender: TObject);
    procedure MapRegionSelected(Sender: TObject; RegionRect: TRect);

    procedure AutoClick(Sender: TObject);
    procedure ParamsToMap;
    procedure ShowResults;
    procedure RunAnalysis(const Region: TRect);
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateControls;
    procedure ParamsFromMap;

    property MapImage: TMapImage read FMap write SetMap;
  end;

// Small modal form; prompts starting with '?' are check boxes
function AskValues(const ACaption: String; const Prompts: array of String;
  var Values: array of String): Boolean;

implementation

function AskValues(const ACaption: String; const Prompts: array of String;
  var Values: array of String): Boolean;
var
  Dlg: TForm;
  Grid, Buttons: TPanel;
  Lbl: TLabel;
  Ctrls: array of TControl;
  i: Integer;
  Btn: TButton;
begin
  Result := False;
  Dlg := TForm.CreateNew(nil);
  try
    Dlg.Caption := ACaption;
    Dlg.BorderStyle := bsDialog;
    Dlg.Position := poMainFormCenter;
    Dlg.AutoSize := True;

    Grid := TPanel.Create(Dlg);
    Grid.Parent := Dlg;
    Grid.Align := alTop;
    Grid.BevelOuter := bvNone;
    Grid.AutoSize := True;
    Grid.BorderSpacing.Around := 8;
    Grid.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    Grid.ChildSizing.ControlsPerLine := 2;
    Grid.ChildSizing.HorizontalSpacing := 8;
    Grid.ChildSizing.VerticalSpacing := 6;

    SetLength(Ctrls, Length(Prompts));
    for i := 0 to High(Prompts) do
    begin
      Lbl := TLabel.Create(Dlg);
      Lbl.Parent := Grid;
      if (Copy(Prompts[i], 1, 1) = '?') then
      begin
        Lbl.Caption := '';
        Ctrls[i] := TCheckBox.Create(Dlg);
        TCheckBox(Ctrls[i]).Caption := Copy(Prompts[i], 2, MaxInt);
        TCheckBox(Ctrls[i]).Checked := SameText(Values[i], 'True');
      end
      else
      begin
        Lbl.Caption := Prompts[i];
        Ctrls[i] := TEdit.Create(Dlg);
        TEdit(Ctrls[i]).Text := Values[i];
        Ctrls[i].Width := 160;
      end;
      Ctrls[i].Parent := Grid;
    end;

    Buttons := TPanel.Create(Dlg);
    Buttons.Parent := Dlg;
    Buttons.Align := alTop;
    Buttons.Top := Grid.Top + Grid.Height + 1;
    Buttons.BevelOuter := bvNone;
    Buttons.AutoSize := True;
    Buttons.BorderSpacing.Around := 8;
    Buttons.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    Buttons.ChildSizing.ControlsPerLine := 2;
    Buttons.ChildSizing.HorizontalSpacing := 8;

    Btn := TButton.Create(Dlg);
    Btn.Parent := Buttons;
    Btn.Caption := 'OK';
    Btn.ModalResult := mrOK;
    Btn.Default := True;

    Btn := TButton.Create(Dlg);
    Btn.Parent := Buttons;
    Btn.Caption := 'Cancel';
    Btn.ModalResult := mrCancel;
    Btn.Cancel := True;

    if (Dlg.ShowModal = mrOK) then
    begin
      for i := 0 to High(Prompts) do
        if (Ctrls[i] is TCheckBox) then
          Values[i] := BoolToStr(TCheckBox(Ctrls[i]).Checked, True)
        else
          Values[i] := TEdit(Ctrls[i]).Text;
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

//==============================|TMapPanel|===================================//
constructor TMapPanel.Create(AOwner: TComponent);
var
  Lbl: TLabel;
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  HorzScrollBar.Visible := False;
  VertScrollBar.Tracking := True;

  SaveDlg := TSaveDialog.Create(Self);
  SaveDlg.Filter := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
  SaveDlg.DefaultExt := 'csv';
  SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];

  // Groups are stacked top-down in reverse creation order
  gbParticles := NewGroup('Particles / grains');
  chkAuto := NewCheck(gbParticles, 'Automatic threshold (Otsu)', True);
  chkAuto.OnClick := @AutoClick;
  Lbl := TLabel.Create(Self);
  Lbl.Parent := gbParticles;
  Lbl.Caption := 'Threshold';
  seThreshold := TFloatSpinEdit.Create(Self);
  seThreshold.Parent := gbParticles;
  seThreshold.MinValue := -1e9;
  seThreshold.MaxValue := 1e9;
  seThreshold.DecimalPlaces := 2;
  seThreshold.Value := 128;
  chkDark := NewCheck(gbParticles, 'Dark particles', True);
  seOpen := NewSpin(gbParticles, 'Opening radius (px)', 0, 20, 0);
  seClose := NewSpin(gbParticles, 'Closing radius (px)', 0, 20, 0);
  seMinArea := NewSpin(gbParticles, 'Minimum area (px)', 1, 1000000, 10);
  chkFill := NewCheck(gbParticles, 'Fill holes', True);
  chkEdges := NewCheck(gbParticles, 'Exclude edge particles', True);
  chkShow := NewCheck(gbParticles, 'Show outlines', True);
  chkShow.OnClick := @ShowOutlinesClick;
  btnAnalyze := NewButton(gbParticles, 'Analyse image', @AnalyzeClick);
  btnExportParticles := NewButton(gbParticles, 'Export CSV...', @ExportParticlesClick);
  lblResults := TLabel.Create(Self);
  lblResults.Parent := gbParticles;
  lblResults.WordWrap := True;
  lblResults.Caption := '';
  chHist := TChart.Create(Self);
  chHist.Parent := gbParticles;
  chHist.Height := 140;
  chHist.Width := 200;
  chHist.BottomAxis.Title.Caption := 'Equivalent diameter';
  chHist.BottomAxis.Title.Visible := True;
  srHist := TBarSeries.Create(chHist);
  srHist.BarWidthPercent := 90;
  chHist.AddSeries(srHist);

  gbColorBar := NewGroup('Colour bar');
  lblColorBar := TLabel.Create(Self);
  lblColorBar.Parent := gbColorBar;
  lblColorBar.WordWrap := True;
  seStep := NewSpin(gbColorBar, 'Grid step (px)', 1, 1000, 4);
  btnExportGrid := NewButton(gbColorBar, 'Export z grid...', @ExportGridClick);
  btnClearColorBar := NewButton(gbColorBar, 'Clear', @ClearColorBarClick);

  gbFeatures := NewGroup('Features and measurements');
  lvFeatures := TListView.Create(Self);
  lvFeatures.Parent := gbFeatures;
  lvFeatures.ViewStyle := vsReport;
  lvFeatures.ReadOnly := True;
  lvFeatures.RowSelect := True;
  lvFeatures.HideSelection := False;
  lvFeatures.Height := 160;
  lvFeatures.Width := 200;
  lvFeatures.Columns.Add.Caption := 'Name';
  lvFeatures.Columns.Add.Caption := 'Value';
  lvFeatures.Columns[0].Width := 80;
  lvFeatures.Columns[1].Width := 200;
  lvFeatures.OnSelectItem := @FeatureSelect;
  btnFinish := NewButton(gbFeatures, 'Finish drawing', @FinishClick);
  btnDelete := NewButton(gbFeatures, 'Delete selected', @DeleteClick);
  btnExportVertices := NewButton(gbFeatures, 'Export vertices...', @ExportVerticesClick);
  btnExportSummary := NewButton(gbFeatures, 'Export summary...', @ExportSummaryClick);
  btnExportProfile := NewButton(gbFeatures, 'Export profile...', @ExportProfileClick);

  gbCalib := NewGroup('Calibration');
  lblCalib := TLabel.Create(Self);
  lblCalib.Parent := gbCalib;
  lblCalib.WordWrap := True;
  chkYUp := NewCheck(gbCalib, 'Y axis up (scale bar)', False);
  chkYUp.OnClick := @YUpClick;
  btnClearCalib := NewButton(gbCalib, 'Clear calibration', @ClearCalibClick);
end;

function TMapPanel.NewGroup(const ACaption: String): TGroupBox;
begin
  Result := TGroupBox.Create(Self);
  Result.Parent := Self;
  Result.Align := alTop;
  Result.Top := 0;
  Result.Caption := ACaption;
  Result.AutoSize := True;
  Result.BorderSpacing.Around := 4;
  Result.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  Result.ChildSizing.ControlsPerLine := 1;
  Result.ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  Result.ChildSizing.LeftRightSpacing := 6;
  Result.ChildSizing.TopBottomSpacing := 4;
  Result.ChildSizing.VerticalSpacing := 4;
end;

function TMapPanel.NewButton(AParent: TWinControl; const ACaption: String;
  AOnClick: TNotifyEvent): TButton;
begin
  Result := TButton.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.OnClick := AOnClick;
end;

function TMapPanel.NewCheck(AParent: TWinControl; const ACaption: String;
  AChecked: Boolean): TCheckBox;
begin
  Result := TCheckBox.Create(Self);
  Result.Parent := AParent;
  Result.Caption := ACaption;
  Result.Checked := AChecked;
end;

function TMapPanel.NewSpin(AParent: TWinControl; const ALabel: String;
  AMin, AMax, AValue: Integer): TSpinEdit;
var
  Lbl: TLabel;
begin
  Lbl := TLabel.Create(Self);
  Lbl.Parent := AParent;
  Lbl.Caption := ALabel;
  Result := TSpinEdit.Create(Self);
  Result.Parent := AParent;
  Result.MinValue := AMin;
  Result.MaxValue := AMax;
  Result.Value := AValue;
end;

function TMapPanel.AskFile(const ATitle, AName: String): Boolean;
begin
  SaveDlg.Title := ATitle;
  SaveDlg.FileName := AName;
  Result := SaveDlg.Execute;
end;

procedure TMapPanel.SetMap(Value: TMapImage);
begin
  FMap := Value;
  if Assigned(FMap) then
  begin
    FMap.OnScaleBarPicked := @MapScaleBarPicked;
    FMap.OnReferencePicked := @MapReferencePicked;
    FMap.OnColorBarPicked := @MapColorBarPicked;
    FMap.OnFeatureAdded := @MapFeatureAdded;
    FMap.OnSelectionChanged := @MapSelectionChanged;
    FMap.OnRegionSelected := @MapRegionSelected;
  end;
  UpdateControls;
end;

procedure TMapPanel.UpdateControls;
var
  i: Integer;
  Item: TListItem;
  F: TMapFeature;
  S: TMapScale;
begin
  if not Assigned(FMap) then Exit;
  FUpdating := True;
  try
    S := FMap.MapScale;
    case S.Mode of
      mcScaleBar:
        lblCalib.Caption := Format('Scale bar: %.4g %s over %.1f px',
          [S.BarLength, S.Units, S.BarStart.DistanceTo(S.BarEnd)]);
      mcReference:
        lblCalib.Caption := Format('Reference points (%s)', [S.Units]);
      mcAxes:
        lblCalib.Caption := Format('From file: %.4g x %.4g %s/px',
          [S.PixelSize.X, S.PixelSize.Y, S.Units]);
      else
        lblCalib.Caption := 'Not calibrated (pixels)';
    end;
    if S.IsCalibrated then
      lblCalib.Caption := lblCalib.Caption + LineEnding +
        Format('1 px = %.4g %s', [Sqrt(S.PixelArea), S.Units]);
    chkYUp.Checked := S.YUp;
    chkYUp.Enabled := (S.Mode = mcScaleBar);

    // Feature list
    lvFeatures.Items.BeginUpdate;
    try
      lvFeatures.Items.Clear;
      for i := 0 to FMap.Features.Count - 1 do
      begin
        F := FMap.Features[i];
        Item := lvFeatures.Items.Add;
        Item.Caption := F.Name;
        Item.SubItems.Add(F.Summary(S));
      end;
      if (FMap.SelectedIndex >= 0) and (FMap.SelectedIndex < lvFeatures.Items.Count) then
        lvFeatures.Items[FMap.SelectedIndex].Selected := True;
    finally
      lvFeatures.Items.EndUpdate;
    end;
    btnFinish.Enabled := Assigned(FMap.PendingFeature);
    btnDelete.Enabled := FMap.SelectedIndex >= 0;
    btnExportVertices.Enabled := FMap.Features.Count > 0;
    btnExportSummary.Enabled := FMap.Features.Count > 0;
    btnExportProfile.Enabled := Assigned(FMap.SelectedFeature) and (FMap.SelectedFeature.Count > 1);

    // Colour bar
    with FMap.ColorBar do
      if Defined or NeedsRebuild then
      begin
        lblColorBar.Caption := Format('%.4g to %.4g %s', [ValueStart, ValueEnd, Units]);
        if LogScale then
          lblColorBar.Caption := lblColorBar.Caption + ' (log)';
      end
      else
        lblColorBar.Caption := 'Not defined: pick both ends with the Colour bar tool';
    btnExportGrid.Enabled := FMap.ColorBar.Defined or FMap.ColorBar.NeedsRebuild;
    btnClearColorBar.Enabled := btnExportGrid.Enabled;

    btnAnalyze.Enabled := FMap.ImageIsLoaded;
    btnExportParticles.Enabled := FMap.Particles.Count > 0;
    seThreshold.Enabled := not chkAuto.Checked;
  finally
    FUpdating := False;
  end;
end;

procedure TMapPanel.YUpClick(Sender: TObject);
begin
  if FUpdating or (not Assigned(FMap)) then Exit;
  FMap.MapScale.YUp := chkYUp.Checked;
  UpdateControls;
end;

procedure TMapPanel.ClearCalibClick(Sender: TObject);
begin
  if not Assigned(FMap) then Exit;
  FMap.MapScale.Clear;
  UpdateControls;
end;

procedure TMapPanel.FeatureSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if FUpdating or (not Assigned(FMap)) or (not Selected) then Exit;
  FMap.SelectedIndex := Item.Index;
end;

procedure TMapPanel.FinishClick(Sender: TObject);
begin
  if Assigned(FMap) then
    FMap.FinishFeature;
  UpdateControls;
end;

procedure TMapPanel.DeleteClick(Sender: TObject);
begin
  if Assigned(FMap) then
    FMap.DeleteSelectedFeature;
  UpdateControls;
end;

procedure TMapPanel.ExportVerticesClick(Sender: TObject);
begin
  if Assigned(FMap) and AskFile('Export feature vertices', 'features.csv') then
    FMap.Features.SaveVerticesCSV(SaveDlg.FileName, FMap.MapScale);
end;

procedure TMapPanel.ExportSummaryClick(Sender: TObject);
begin
  if Assigned(FMap) and AskFile('Export measurement summary', 'measurements.csv') then
    FMap.Features.SaveSummaryCSV(SaveDlg.FileName, FMap.MapScale);
end;

procedure TMapPanel.ExportProfileClick(Sender: TObject);
begin
  if Assigned(FMap) and Assigned(FMap.SelectedFeature) and
     AskFile('Export line profile', FMap.SelectedFeature.Name + '.csv') then
    FMap.SaveProfileCSV(FMap.SelectedFeature, SaveDlg.FileName);
end;

procedure TMapPanel.ExportGridClick(Sender: TObject);
begin
  if Assigned(FMap) and AskFile('Export z grid', 'zgrid.csv') then
    FMap.SaveZGridCSV(TRect.Create(0, 0, 0, 0), seStep.Value, SaveDlg.FileName);
end;

procedure TMapPanel.ClearColorBarClick(Sender: TObject);
begin
  if Assigned(FMap) then
    FMap.ClearColorBar;
  UpdateControls;
end;

procedure TMapPanel.AutoClick(Sender: TObject);
begin
  seThreshold.Enabled := not chkAuto.Checked;
end;

procedure TMapPanel.ParamsToMap;
var
  P: TParticleParams;
begin
  P := FMap.Particles.Params;
  P.AutoThreshold := chkAuto.Checked;
  P.Threshold := seThreshold.Value;
  P.DarkObjects := chkDark.Checked;
  P.OpenRadius := seOpen.Value;
  P.CloseRadius := seClose.Value;
  P.FillHoles := chkFill.Checked;
  P.MinArea := seMinArea.Value;
  P.ExcludeEdges := chkEdges.Checked;
  FMap.Particles.Params := P;
end;

procedure TMapPanel.ParamsFromMap;
var
  P: TParticleParams;
begin
  P := FMap.Particles.Params;
  chkAuto.Checked := P.AutoThreshold;
  seThreshold.Value := P.Threshold;
  chkDark.Checked := P.DarkObjects;
  seOpen.Value := P.OpenRadius;
  seClose.Value := P.CloseRadius;
  chkFill.Checked := P.FillHoles;
  seMinArea.Value := P.MinArea;
  chkEdges.Checked := P.ExcludeEdges;
  chkShow.Checked := FMap.ShowParticles;
  seThreshold.Enabled := not chkAuto.Checked;
end;

procedure TMapPanel.RunAnalysis(const Region: TRect);
begin
  if not Assigned(FMap) then Exit;
  ParamsToMap;
  Screen.Cursor := crHourGlass;
  try
    FMap.AnalyzeParticles(Region);
  finally
    Screen.Cursor := crDefault;
  end;
  ShowResults;
  UpdateControls;
end;

procedure TMapPanel.AnalyzeClick(Sender: TObject);
begin
  RunAnalysis(TRect.Create(0, 0, 0, 0));
end;

procedure TMapPanel.ShowResults;
var
  i, Bins: Integer;
  St: TParticleStats;
  Edges, Counts: TDoubleDynArray;
  U: String;
begin
  srHist.Clear;
  St := FMap.Particles.DiameterStats;
  U := FMap.MapScale.Units;
  if (St.Count = 0) then
  begin
    lblResults.Caption := 'No particles found';
    Exit;
  end;
  lblResults.Caption := Format('N = %d, threshold = %.4g' + LineEnding +
    'Eq. diameter = %.4g ± %.4g %s' + LineEnding + 'Median = %.4g %s, range %.4g-%.4g',
    [St.Count, FMap.Particles.UsedThreshold, St.Mean, St.SD, U, St.Median, U,
     St.MinValue, St.MaxValue]);
  chHist.BottomAxis.Title.Caption := 'Equivalent diameter (' + U + ')';
  Bins := EnsureRange(Round(Sqrt(St.Count)), 5, 30);
  FMap.Particles.DiameterHistogram(Bins, Edges, Counts);
  for i := 0 to Bins - 1 do
    srHist.AddXY((Edges[i] + Edges[i + 1])/2, Counts[i]);
end;

procedure TMapPanel.ExportParticlesClick(Sender: TObject);
begin
  if Assigned(FMap) and AskFile('Export particles', 'particles.csv') then
    FMap.Particles.SaveCSV(SaveDlg.FileName, FMap.MapScale);
end;

procedure TMapPanel.ShowOutlinesClick(Sender: TObject);
begin
  if FUpdating or (not Assigned(FMap)) then Exit;
  FMap.ShowParticles := chkShow.Checked;
end;

procedure TMapPanel.MapScaleBarPicked(Sender: TObject; const P1, P2: TCurvePoint);
var
  V: array [0..1] of String;
begin
  V[0] := '100';
  V[1] := 'nm';
  if AskValues('Scale bar', ['Length', 'Units'], V) and (ParseFloat(V[0]) > 0) then
    FMap.MapScale.SetScaleBar(P1, P2, ParseFloat(V[0]), Trim(V[1]));
  FMap.Tool := mtSelect;
  UpdateControls;
end;

procedure TMapPanel.MapReferencePicked(Sender: TObject; const ImgPts: TScalePoints);
var
  V: array [0..6] of String;
  MapPts: TScalePoints;
  i: Integer;
begin
  for i := 0 to 5 do
    V[i] := '0';
  V[6] := 'm';
  if AskValues('Reference points',
    ['Point 1: x', 'Point 1: y', 'Point 2: x', 'Point 2: y', 'Point 3: x', 'Point 3: y', 'Units'], V) then
  begin
    for i := 1 to 3 do
      MapPts[i] := TCurvePoint.Create(ParseFloat(V[2*i - 2]), ParseFloat(V[2*i - 1]));
    FMap.MapScale.SetReference(ImgPts, MapPts, Trim(V[6]));
    if not FMap.MapScale.IsValid then
      MessageDlg('The three reference points are collinear.', mtWarning, [mbOK], 0);
  end;
  FMap.Tool := mtSelect;
  UpdateControls;
end;

procedure TMapPanel.MapColorBarPicked(Sender: TObject; const P1, P2: TCurvePoint);
var
  V: array [0..3] of String;
begin
  V[0] := '0';
  V[1] := '1';
  V[2] := '';
  V[3] := 'False';
  if AskValues('Colour bar', ['Value at first point', 'Value at second point', 'Units',
    '?Logarithmic scale'], V) then
    FMap.SetColorBar(P1, P2, ParseFloat(V[0]), ParseFloat(V[1]),
                     SameText(V[3], 'True'), Trim(V[2]));
  FMap.Tool := mtSelect;
  UpdateControls;
end;

procedure TMapPanel.MapFeatureAdded(Sender: TObject; Feature: TMapFeature);
begin
  UpdateControls;
end;

procedure TMapPanel.MapSelectionChanged(Sender: TObject);
begin
  UpdateControls;
end;

procedure TMapPanel.MapRegionSelected(Sender: TObject; RegionRect: TRect);
begin
  RunAnalysis(RegionRect);
end;

end.
