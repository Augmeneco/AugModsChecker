unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libcurl, strutils, fpjson, jsonparser, dateutils, md5, RegExpr, fileutil;

function readfile(fnam: string): string;
function cuterandom(min,max: integer): Integer;
function veryBadToLower(str: String): String;
function encodeUrl(url: string): string;
function URLArgsEncode(args: array of string): string;
function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;
function writeFunction(pBuff: Pointer; size: Integer; nmemb: Integer; pUserData: Pointer): Integer;
procedure writefile(fnam: string; txt: string);
procedure logWrite(str: String);

type
  TRequests = class
  public
    text: string;
    error: string;
    jsondata: TJSONData;
    bs: TBytesStream;
    hCurl: PCURL;
    code: CURLcode;
    function Get(url: string): string;
    procedure Download(url: string; filename: string);
    function Post(url: string; args: array of string; photo: string = ''): string;
    function Json(): TJSONData;
    procedure Free;
    constructor Create;
end;

type
  TGit = class(TThread)
  public
    URL: string;
    MD5List: TJSONObject;
    hasmd5: Boolean;
    procedure Execute; override;
    procedure GetFilesList;
    procedure CompareFiles(tree: TJSONArray;comp_path: string);
    procedure UpdateFiles(comp_path: string;flist: TJSONArray);
    constructor Create(name: string);
end;

implementation
uses
  main;
var
  requests: TRequests;
  ignore: TStringList;

procedure logWrite(str: String);
var
  logTime: TDateTime;
begin
  logTime := now();
  Form1.Logger.Lines.Add(format('[%s] %s',
                 [formatDateTime('hh:nn:ss', logTime),
                  str]));
end;

function writeFunction(pBuff: Pointer; size: Integer; nmemb: Integer; pUserData: Pointer): Integer;
begin
  TStream(pUserData).write(pBuff^, size*nmemb);
  writeFunction := size*nmemb;
end;

procedure TRequests.Free;
begin
  bs.Free;
  curl_easy_cleanup(hCurl);
end;

constructor TRequests.Create;
begin
  hCurl := curl_easy_init();
  if assigned(hCurl) then
  begin
    curl_easy_setopt(hCurl, CURLOPT_VERBOSE, [True]);
    curl_easy_setopt(hCurl, CURLOPT_SSL_VERIFYHOST, [False]);
    curl_easy_setopt(hCurl, CURLOPT_SSL_VERIFYPEER, [False]);
    //curl_easy_setopt(hCurl, CURLOPT_URL, [PChar('https://api.vk.com/method/users.get')]);
    curl_easy_setopt(hCurl, CURLOPT_NOSIGNAL, [True]);
    curl_easy_setopt(hCurl, CURLOPT_VERBOSE, [0]);
    curl_easy_setopt(hCurl, CURLOPT_WRITEFUNCTION, [@writeFunction]);
    curl_easy_setopt(hCurl, CURLOPT_USERAGENT, [PChar('User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:76.0) Gecko/20100101 Firefox/76.0')]);
    //code := curl_easy_perform(hCurl);
    //ShowMessage(String(curl_easy_strerror(code)));
    //ShowMessage(String(bs.Bytes));
  end;
  //curl_easy_cleanup(hCurl);
  //bs.Free;
end;

function TRequests.Json(): TJSONData;
begin
  Result := GetJSON(text);
end;

function TRequests.Get(url: string): string;
begin
  bs := TBytesStream.Create();
  curl_easy_setopt(hCurl, CURLOPT_WRITEDATA, [Pointer(bs)]);
  curl_easy_setopt(hCurl, CURLOPT_URL, [PChar(url)]);
  //curl_easy_setopt(hCurl, CURLOPT_PROXY, [PChar('socks5://localhost:9050')]);
  code := curl_easy_perform(hCurl);
  error := String(curl_easy_strerror(code));
  text := String(Pchar(String(bs.Bytes)));

  if error <> 'No error' then
     raise Exception.create(error);

  Result := text;
end;

function TRequests.Post(url: string; args: array of string; photo: string = ''): string;
var
  FirstPost, LastPost:pcurl_httppost;
begin
  bs := TBytesStream.Create();
  curl_easy_setopt(hCurl, CURLOPT_WRITEDATA, [Pointer(bs)]);
  curl_easy_setopt(hCurl, CURLOPT_POST, [1]);
  curl_easy_setopt(hCurl, CURLOPT_URL, [PChar(url)]);
  curl_easy_setopt(hCurl, CURLOPT_POSTFIELDS, [PChar( URLArgsEncode(args) )]);

  if photo <> '' then
  begin
    FirstPost:=nil;
    LastPost:=nil;
    curl_formadd(@FirstPost, @LastPost,
      [CURLFORM_COPYNAME,    'photo',
      CURLFORM_FILE,        Pchar(photo),
      CURLFORM_CONTENTTYPE, 'image/jpeg',
      CURLFORM_END]
    );
    curl_easy_setopt(hCurl, CURLOPT_HTTPPOST, [FirstPost]);
  end;

  code := curl_easy_perform(hCurl);
  error := String(curl_easy_strerror(code));
  text := String(Pchar(String(bs.Bytes)));

  curl_easy_setopt(hCurl, CURLOPT_POST, [0]);
  curl_easy_setopt(hCurl, CURLOPT_POSTFIELDS, [PChar('')]);

  if error <> 'No error' then
     raise Exception.create(error);

  Result := text;
end;

procedure TRequests.Download(url: string; filename: string);
Var
  f : TFileStream;
  DhCurl : pCurl;

begin
  F:=TFileStream.Create(filename,fmCreate);
    DhCurl:= curl_easy_init;
      curl_easy_setopt(DhCurl, CURLOPT_USERAGENT, [PChar('User-Agent: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:76.0) Gecko/20100101 Firefox/76.0')]);
      curl_easy_setopt(DhCurl, CURLOPT_SSL_VERIFYHOST, [False]);
      curl_easy_setopt(DhCurl, CURLOPT_SSL_VERIFYPEER, [False]);
      curl_easy_setopt(DhCurl,CURLOPT_VERBOSE, [True]);
      curl_easy_setopt(DhCurl,CURLOPT_URL,[pchar(url)]);
      curl_easy_setopt(DhCurl,CURLOPT_WRITEFUNCTION,[@DoWrite]);
      curl_easy_setopt(DhCurl,CURLOPT_WRITEDATA,[Pointer(F)]);
      curl_easy_perform(DhCurl);
      curl_easy_cleanup(DhCurl);
  F.Free;
end;

Function DoWrite(Ptr : Pointer; Size : size_t; nmemb: size_t; Data : Pointer) : size_t;cdecl;

begin
  Result:=TStream(Data).Write(Ptr^,Size*nmemb);
end;

function URLArgsEncode(args: array of string): string;
var
  arg1: integer = 0;
  arg2: integer = 1;
  arg: string = '';
begin
  while True do
  begin
    arg += Format('%s=%s',[args[arg1],encodeUrl(args[arg2])]);
    inc(arg1,2);
    inc(arg2,2);
    if arg2 > Length(args) then Break;
    arg += '&';
  end;
  Result := arg;
end;

function readfile(fnam: string): string;
var
  text: TStringList;
begin
  text := TStringList.Create;
  text.LoadFromFile(fnam);
  Result := text.Text;
end;

procedure writefile(fnam: string; txt: string);
var
  strm: TFileStream;
  n: longint;
begin
  strm := TFileStream.Create(fnam, fmCreate);
  n := Length(txt);
  try
    strm.Position := 0;
    strm.Write(txt[1], n);
  finally
    strm.Free;
  end;
end;

function cuterandom(min,max: integer): Integer;
begin
  cuterandom := random(max-min+1)+min;
end;

function veryBadToLower(str: String): String;
const
  convLowers: Array [0..87] of String = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
      'v', 'w', 'x', 'y', 'z', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
      'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж',
      'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы',
      'ь', 'э', 'ю', 'я');
  convUppers: Array [0..87] of String = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
      'V', 'W', 'X', 'Y', 'Z', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
      'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж',
      'З', 'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ъ',
      'Ь', 'Э', 'Ю', 'Я');
var
  i: Integer;
begin
  result := str;
  for i := 0 to 87 do
    result := stringReplace(result, convUppers[i], convLowers[i], [rfReplaceAll]);
end;

function encodeUrl(url: string): string;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-'];
begin
  //Init
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    //Check if we have a safe char
    if url[x] in SafeMask then
    begin
      //Append all other chars
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      //Append space
      sBuff := sBuff + '+';
    end
    else
    begin
      //Convert to hex
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

procedure TGit.GetFilesList;
var
  sha: string;
  tree: TJSONArray;
begin
  main.Form1.Logger.Lines.Add('');
  requests.Get(
    Format('https://api.github.com/repos/%s/commits',[url])
  );
  sha := TJSONArray(requests.Json()).Objects[0]['sha'].AsString;

  requests.Get(
    Format('https://api.github.com/repos/%s/git/trees/%s',[url,sha])
  );
  tree := TJSONObject(requests.json()).Arrays['tree'];
  logwrite(Format('Parsed %d files from %s',[tree.Count,URL]));
  CompareFiles(tree,main.Form1.EditModsPath.text);
end;

constructor TGit.Create(name: string);
begin
  hasmd5 := True;
  URL := name;
  FreeOnTerminate := True;
  inherited Create(false);
  requests.get(Format('https://raw.githubusercontent.com/%s/master/md5list.json',[URL]));
  if requests.text = '404: Not Found' then
  begin
    logwrite('md5 list not founded on mods server!');
    hasmd5 := False;
  end
  else
    MD5List := TJSONObject(requests.Json());
  //MD5List := TJSONObject(GetJSON(readfile('md5list.json')));
end;

procedure TGit.CompareFiles(tree: TJSONArray; comp_path: string);
var
  i,i1: Integer;
  dir_tree, FilesList: TJSONArray;
  fobj, tmpobj: TJSONObject;
  md5_orig, md5, fname, dir_name: string;
  all_files: TStringList;
  farray: TStringArray;
begin
  FilesList := TJSONArray.Create;
  //main.Form1.Logger.Lines.Add('');
  logwrite('Starting compare files in '+comp_path);
  for i:=0 to tree.Count-1 do
  begin
    fobj := tree.Objects[i];
    if fobj['type'].AsString = 'tree' then
    begin
      dir_name := comp_path +'\'+fobj['path'].AsString;
      requests.Get(
        fobj['url'].AsString
      );
      dir_tree := TJSONObject(requests.json()).Arrays['tree'];
      logwrite('Recursively checking a '+dir_name+' folder');
      CreateDir(dir_name);
      CompareFiles(dir_tree,dir_name);
    end;
    if ignore.IndexOf(fobj['path'].AsString) <> -1 then
       continue;

    tmpobj := TJSONObject.Create;
    tmpobj.add('name',fobj['path'].AsString);
    tmpobj.add('url',Format('https://raw.githubusercontent.com/%s/master/%s',[url,fobj['path'].AsString]));

    all_files := FindAllFiles(comp_path,'*', False);
    for i1:=0 to all_files.Count-1 do
    begin
      farray := all_files[i1].split('\');
      fname := farray[Length(farray)-1];
      if MD5List.IndexOfName(fname) = -1 then
      begin
        logWrite('Delete unnecessary file '+fname);
        DeleteFile(all_files[i1]);
      end;
    end;

    if not FileExists(comp_path+'\'+fobj['path'].AsString) then
    begin
      logWrite('Compare '+fobj['path'].AsString);
       if MD5List.IndexOfName(fobj['path'].AsString) = -1 then
          continue;

       FilesList.add(tmpobj);
       continue;
    end;
    md5_orig := MD5List[fobj['path'].AsString].AsString;
    md5 := MD5Print(MD5File(comp_path+'\'+fobj['path'].AsString));

    if md5_orig <> md5 then
       FilesList.add(tmpobj);
  end;
  main.Form1.Logger.Lines.Add('');
  logWrite(inttostr(FilesList.Count)+' mods will be downloaded');
  main.Form1.ProgressBar1.Max := FilesList.Count;
  UpdateFiles(comp_path,FilesList);
end;

procedure TGit.UpdateFiles(comp_path: string; flist: TJSONArray);
var
  i: integer;
  fobj: TJSONObject;
begin
  main.Form1.Logger.Lines.Add('');
  for i:=0 to flist.Count-1 do
  begin
    fobj := flist.Objects[i];

    if FileExists(comp_path+'\'+fobj['name'].AsString) then
       DeleteFile(comp_path+'\'+fobj['name'].AsString);

    logwrite('Download '+fobj['name'].AsString);
    requests.Download(
      fobj['url'].AsString,
      comp_path+'\'+fobj['name'].AsString
    );
    main.Form1.ProgressBar1.StepIt;
  end;
  main.Form1.ProgressBar1.Position:=0;
end;

procedure TGit.Execute;
begin
  if hasmd5 then
     GetFilesList;
end;


begin
  requests := TRequests.Create;
  ignore := TStringList.Create;
  ignore.Add('.gitignore');
  ignore.Add('.git');  //TODO: add ignorelist.json to servers
end.














