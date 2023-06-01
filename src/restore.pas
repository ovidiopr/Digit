unit restore;

{$mode objfpc}{$H+}

interface

uses SysUtils, Forms, LCLIntf, LCLType;

type
  EWinRestorer = class( Exception);
  TWhatSave = (default, Size, Location, State);
  STWhatSave = set of TWhatSave;
  TWinRestorer = class(TObject)
    protected
      mIniFile: TFilename;
      mIniSect: String[80];
      mIsInitialized: Boolean;
      mDefaultWhat: STWhatSave;
    public
     constructor Create(IniName: TFileName; DefaultWhatSave: STWhatSave);
     procedure SaveWin(TheForm: TForm; What: STWhatSave);
     procedure RestoreWin(TheForm: TForm; What: STWhatSave);
     property IniFileName: String read mIniFile;
   end;

const
  WhatSave_All = [Size, Location, State];

var
  GlobalWinRestorer: TWinRestorer;

implementation

uses IniFiles;

constructor TWinRestorer.Create(IniName: TFileName; DefaultWhatSave: STWhatSave);
begin
  inherited Create;
  if default in DefaultWhatSave then
    raise EWinRestorer.Create(
     'Attempt to initialize default window position paramaters with set ' +
     ' containing [default] item.  ' +
     'Default params may contain only members of [size, location, state].  ')
  else
    mDefaultWhat := DefaultWhatSave;

  mIniFile := IniName;
  mIniSect := 'WindowsRestorer';
end;

procedure TWinRestorer.RestoreWin(TheForm: TForm; What: STWhatSave);
var
  FormNm, SectionNm : string[80];
  Ini               : TIniFile;
  n,l,t,w,h         : Integer;
begin
  Ini := TIniFile.Create( mIniFile);
  try
    SectionNm := mIniSect;
    FormNm := TheForm.ClassName;
    if default in What then What := mDefaultWhat;
    n := 1;
    if State in What then
      n := Ini.ReadInteger( SectionNm, FormNm + '_WindowState', 0);
      case n of
        1: begin
          TheForm.WindowState := wsMinimized;
        end;
        2: begin
          TheForm.WindowState := wsNormal;
          with TheForm do
          begin
            l := Left;
            t := Top;
            h := Height;
            w := Width;
          end;
          if Size in What then begin
            w := Ini.ReadInteger( SectionNm, FormNm + '_Width', w);
            h := Ini.ReadInteger( SectionNm, FormNm + '_Height', h);
          end;
          if Location in What then begin
            t := Ini.ReadInteger( SectionNm, FormNm + '_Top', t);
            l := Ini.ReadInteger( SectionNm, FormNm + '_Left', l);
          end;
          TheForm.SetBounds(l,t,w,h);
        end;
        3: begin
          TheForm.WindowState := wsMaximized;
        end;
      end;
  finally
    Ini.Free;
  end;
end;

procedure TWinRestorer.SaveWin(TheForm: TForm; What: STWhatSave);
var FormNm, SectionNm: string[80];   w : STWhatsave; Ini: TIniFile;
begin
  Ini := TIniFile.Create( mIniFile);
  try
    SectionNm := mIniSect;
    FormNm := TheForm.ClassName;
    if default in What then w := mDefaultWhat else w := mDefaultWhat;
    if Size in w then begin
      Ini.WriteInteger( SectionNm, FormNm + '_Width', TheForm.Width);
      Ini.WriteInteger( SectionNm, FormNm + '_Height', TheForm.Height);
    end;
    if Location in w then begin
      Ini.WriteInteger( SectionNm, FormNm + '_Top', TheForm.Top);
      Ini.WriteInteger( SectionNm, FormNm + '_Left', TheForm.Left);
    end;
    if State in w then
      case TheForm.WindowState of
        wsMinimized : Ini.WriteInteger( SectionNm, FormNm + '_WindowState', 1);
        wsNormal    : Ini.WriteInteger( SectionNm, FormNm + '_WindowState', 2);
        wsMaximized : Ini.WriteInteger( SectionNm, FormNm + '_WindowState', 3);
      end;
  finally
    Ini.Free;
  end;
end;

initialization
end.

