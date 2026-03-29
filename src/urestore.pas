unit urestore;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Forms, Controls, ExtCtrls, Types, Math, IniFiles,
  LCLIntf, LCLType, LCLProc;

type
  EWinRestorer = class(Exception);

  TWhatSave = (svDefault, svSize, svLocation, svState, svPanels);
  STWhatSave = set of TWhatSave;

  TWinRestorer = class(TObject)
  private
    mIniFile: TFileName;
    mIniSect: string;
    mDefaultWhat: STWhatSave;

    procedure EnsureFormVisible(F: TForm);
    function ScaleValue(Value, FromDPI, ToDPI: Integer): Integer;
    procedure SavePanels(Form: TForm; Ini: TCustomIniFile; const Section, FormName: string);
    procedure RestorePanels(Form: TForm; Ini: TCustomIniFile; const Section, FormName: string);
  public
    constructor Create(const IniName: TFileName; DefaultWhatSave: STWhatSave);
    procedure SaveWin(TheForm: TForm; What: STWhatSave = [svDefault]);
    procedure RestoreWin(TheForm: TForm; What: STWhatSave = [svDefault]);
    property IniFileName: TFileName read mIniFile;
  end;

const
  WhatSave_All = [svSize, svLocation, svState, svPanels];

var
  GlobalWinRestorer: TWinRestorer = nil;

implementation

{ --------------------------------------------------------------------------- }

constructor TWinRestorer.Create(const IniName: TFileName; DefaultWhatSave: STWhatSave);
begin
  inherited Create;

  if svDefault in DefaultWhatSave then
    raise EWinRestorer.Create('DefaultWhatSave must not include svDefault.');

  mIniFile := IniName;
  mIniSect := 'WindowsRestorer';
  mDefaultWhat := DefaultWhatSave;
end;

function TWinRestorer.ScaleValue(Value, FromDPI, ToDPI: Integer): Integer;
begin
  if (FromDPI <= 0) or (ToDPI <= 0) then Exit(Value);
  Result := Round((Int64(Value) * ToDPI) div FromDPI);
end;

procedure TWinRestorer.EnsureFormVisible(F: TForm);
var
  R: TRect;
  M: TMonitor;
  L, T: Integer;
begin
  R := Rect(F.Left, F.Top, F.Left + F.Width, F.Top + F.Height);
  M := Screen.MonitorFromRect(R, mdNearest);
  if M = nil then M := Screen.PrimaryMonitor;

  L := F.Left;
  T := F.Top;

  if L + F.Width < M.Left then L := M.Left;
  if T + F.Height < M.Top then T := M.Top;

  if L > M.Left + M.Width - 50 then L := M.Left + Max(0, M.Width - F.Width);
  if T > M.Top + M.Height - 50 then T := M.Top + Max(0, M.Height - F.Height);

  if F.Width > M.Width then F.Width := M.Width;
  if F.Height > M.Height then F.Height := M.Height;

  F.Left := L;
  F.Top := T;
end;

{ --------------------------------------------------------------------------- }
{ PANEL SAVE/RESTORE                                                          }
{ --------------------------------------------------------------------------- }

procedure TWinRestorer.SavePanels(Form: TForm; Ini: TCustomIniFile;
  const Section, FormName: string);
var
  i: Integer;
  P: TPanel;
  Key: string;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[i] is TPanel then
    begin
      P := TPanel(Form.Components[i]);

      if P.Align in [alLeft, alRight] then
      begin
        Key := FormName + '_Panel_' + P.Name + '_Width';
        Ini.WriteInteger(Section, Key, P.Width);
      end
      else if P.Align in [alTop, alBottom] then
      begin
        Key := FormName + '_Panel_' + P.Name + '_Height';
        Ini.WriteInteger(Section, Key, P.Height);
      end;
    end;
  end;
end;

procedure TWinRestorer.RestorePanels(Form: TForm; Ini: TCustomIniFile;
  const Section, FormName: string);
var
  i: Integer;
  P: TPanel;
  SavedSize: Integer;
  Key: string;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[i] is TPanel then
    begin
      P := TPanel(Form.Components[i]);

      if P.Align in [alLeft, alRight] then
      begin
        Key := FormName + '_Panel_' + P.Name + '_Width';
        SavedSize := Ini.ReadInteger(Section, Key, P.Width);
        P.Width := SavedSize;
      end
      else if P.Align in [alTop, alBottom] then
      begin
        Key := FormName + '_Panel_' + P.Name + '_Height';
        SavedSize := Ini.ReadInteger(Section, Key, P.Height);
        P.Height := SavedSize;
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }
{ MAIN SAVE }
{ --------------------------------------------------------------------------- }

procedure TWinRestorer.SaveWin(TheForm: TForm; What: STWhatSave);
var
  Ini: TIniFile;
  Section, FormName: string;
  UseSet: STWhatSave;
begin
  if not Assigned(TheForm) then Exit;

  if svDefault in What then
    UseSet := mDefaultWhat
  else
    UseSet := What;

  Ini := TIniFile.Create(mIniFile);
  try
    Section := mIniSect;
    FormName := TheForm.ClassName;

    Ini.WriteInteger(Section, FormName + '_DPI', TheForm.PixelsPerInch);

    if svSize in UseSet then
    begin
      Ini.WriteInteger(Section, FormName + '_Width',  TheForm.Width);
      Ini.WriteInteger(Section, FormName + '_Height', TheForm.Height);
    end;

    if svLocation in UseSet then
    begin
      Ini.WriteInteger(Section, FormName + '_Left', TheForm.Left);
      Ini.WriteInteger(Section, FormName + '_Top',  TheForm.Top);
    end;

    if svState in UseSet then
    begin
      case TheForm.WindowState of
        wsMinimized: Ini.WriteInteger(Section, FormName + '_WindowState', 1);
        wsNormal:    Ini.WriteInteger(Section, FormName + '_WindowState', 2);
        wsMaximized: Ini.WriteInteger(Section, FormName + '_WindowState', 3);
      end;
    end;

    if svPanels in UseSet then
      SavePanels(TheForm, Ini, Section, FormName);

  finally
    Ini.Free;
  end;
end;

{ --------------------------------------------------------------------------- }
{ MAIN RESTORE }
{ --------------------------------------------------------------------------- }

procedure TWinRestorer.RestoreWin(TheForm: TForm; What: STWhatSave);
var
  Ini: TIniFile;
  Section, FormName: string;
  UseSet: STWhatSave;
  SavedDPI, CurDPI: Integer;
  l, t, w, h, stateCode: Integer;
begin
  if not Assigned(TheForm) then Exit;

  if svDefault in What then
    UseSet := mDefaultWhat
  else
    UseSet := What;

  Ini := TIniFile.Create(mIniFile);
  try
    Section := mIniSect;
    FormName := TheForm.ClassName;

    CurDPI   := TheForm.PixelsPerInch;
    SavedDPI := Ini.ReadInteger(Section, FormName + '_DPI', CurDPI);

    l := TheForm.Left;
    t := TheForm.Top;
    w := TheForm.Width;
    h := TheForm.Height;

    if svSize in UseSet then
    begin
      w := Ini.ReadInteger(Section, FormName + '_Width', w);
      h := Ini.ReadInteger(Section, FormName + '_Height', h);
    end;

    if svLocation in UseSet then
    begin
      l := Ini.ReadInteger(Section, FormName + '_Left', l);
      t := Ini.ReadInteger(Section, FormName + '_Top', t);
    end;

    if (SavedDPI <> CurDPI) then
    begin
      l := ScaleValue(l, SavedDPI, CurDPI);
      t := ScaleValue(t, SavedDPI, CurDPI);
      w := ScaleValue(w, SavedDPI, CurDPI);
      h := ScaleValue(h, SavedDPI, CurDPI);
    end;

    TheForm.SetBounds(l, t, w, h);

    if svPanels in UseSet then
      RestorePanels(TheForm, Ini, Section, FormName);

    EnsureFormVisible(TheForm);

    if svState in UseSet then
    begin
      stateCode := Ini.ReadInteger(Section, FormName + '_WindowState', 2);
      case stateCode of
        1: TheForm.WindowState := wsMinimized;
        2: TheForm.WindowState := wsNormal;
        3: TheForm.WindowState := wsMaximized;
      end;
    end;

  finally
    Ini.Free;
  end;
end;

end.

