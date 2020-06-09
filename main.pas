unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, utils, fpjson, jsonparser, fileutil, md5;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    EditModsPath: TEdit;
    EditModsURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Logger: TMemo;
    ProgressBar1: TProgressBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure EditModsPathChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  version: string;

implementation

{$R *.lfm}

{ TForm1 }
var
  requests: TRequests;
  md5obj: TJSONObject;

procedure TForm1.FormCreate(Sender: TObject);
var
  config: TJSONObject;
begin

  requests.get('https://raw.githubusercontent.com/Augmeneco/AugModsChecker/master/version');
  logwrite(Format('|%s|',[requests.text]));

  if FileExists('config.json') then
  begin
    config := TJSONObject(GetJSON(readfile('config.json')));
    EditModsPath.Text := config['modspath'].AsString;
    EditModsURL.Text := config['modsurl'].AsString;
    logWrite('Loaded config.json');
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    EditModsPath.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  git: TGit;
begin
  if not DirectoryExists(EditModsPath.text) then
  begin
    ShowMessage('There is no such folder');
    exit;
  end;
  if EditModsURL.text = '' then
  begin
    ShowMessage('You must enter the server link');
    exit;
  end;
  git := TGit.Create(EditModsURL.text);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  config: TJSONObject;
begin
  config := TJSONObject.Create;
  config.Add('modspath',EditModsPath.Text);
  config.Add('modsurl',EditModsURL.Text);
  writefile('config.json',config.FormatJSON());
  logwrite('Saved config.json');
end;

procedure GenMD5(path: string);
var
  f_list: TStringList;
  md5, filename: string;
  farray: TStringArray;
  i: integer;
begin
  f_list := FindAllFiles(path,'*', true );
  logwrite('[ Creating md5list.json ]'+LineEnding+'Found '+inttostr(f_list.count)+' files:');
  md5obj.add('augmc_version',version);
  for i:=0 to f_list.Count-1 do
  begin
    filename := f_list[i];
    farray := filename.split('\');
    filename := farray[Length(farray)-1];
    md5 := MD5Print(MD5File(f_list[i]));
    md5obj.Add(filename,md5);
    Form1.Logger.Lines.Add(#9+filename);
  end;
  writefile('md5list.json',md5obj.FormatJSON());
  logwrite('Saved md5list.json');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  GenMD5(EditModsPath.text);
end;

procedure TForm1.EditModsPathChange(Sender: TObject);
var
  AppDataDir: string;
begin
  if veryBadToLower(EditModsPath.Text) = 'minecraft' then
  begin
    AppDataDir := GetEnvironmentVariable('appdata');
    if DirectoryExists(AppDataDir+'\.minecraft') = True then
      EditModsPath.Text := AppDataDir+'\.minecraft\mods'
    else
    begin
      ShowMessage('You don''t have Minecraft installed!');
      EditModsPath.Text := '';
    end;
  end;
end;

begin
  requests := TRequests.Create;
  md5obj := TJSONObject.Create;
  version := '1.1.3';
end.

