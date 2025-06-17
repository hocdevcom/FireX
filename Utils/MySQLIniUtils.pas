unit MySQLIniUtils;

interface

uses
  System.SysUtils, System.Classes;

procedure CreateMySQLIniIfMissing(const IniPath, DataDir: string);
function ExtractVersionNumber(const FullName: string): string;
function IsVersionGreaterOrEqual(const V1, V2: string): Boolean;

implementation

uses
  System.IOUtils, System.RegularExpressions, System.StrUtils, Math;

function CompareVersion(const V1, V2: string): Integer;
var
  Parts1, Parts2: TArray<string>;
  I, N1, N2: Integer;
begin
  Parts1 := V1.Split(['.']);
  Parts2 := V2.Split(['.']);

  for I := 0 to Max(High(Parts1), High(Parts2)) do
  begin
    N1 := IfThen(I <= High(Parts1), StrToIntDef(Parts1[I], 0), 0);
    N2 := IfThen(I <= High(Parts2), StrToIntDef(Parts2[I], 0), 0);
    if N1 <> N2 then
      Exit(N1 - N2);
  end;

  Result := 0;
end;

function IsVersionGreaterOrEqual(const V1, V2: string): Boolean;
begin
  Result := CompareVersion(V1, V2) >= 0;
end;

function ExtractVersionNumber(const FullName: string): string;
var
  Parts: TArray<string>;
  Part: string;
begin
  Result := '';
  Parts := FullName.Split(['-']);
  for Part in Parts do
    if TRegEx.IsMatch(Part, '^\d+\.\d+\.\d+$') then
      Exit(Part);
end;

procedure CreateMySQLIniIfMissing(const IniPath, DataDir: string);
var
  IniFile: TStringList;
  AuthPluginLine: string;
  MySQLVersion: string;
begin
  if FileExists(IniPath) then Exit;

  IniFile := TStringList.Create;
  try
    // Extract version: e.g. mysql-8.0.29-winx64 → 8.0.29
    MySQLVersion := ExtractVersionNumber(ExtractFileName(ExtractFileDir(IniPath)));

    // Determine if we need to comment out default_authentication_plugin line
    if IsVersionGreaterOrEqual(MySQLVersion, '8.0.27') then
      AuthPluginLine := '#default_authentication_plugin=mysql_native_password'
    else
      AuthPluginLine := 'default_authentication_plugin=mysql_native_password';

    IniFile.Text :=
      '[client]' + sLineBreak +
      '#password=your_password' + sLineBreak +
      'port=3306' + sLineBreak +
      'socket=/tmp/mysql.sock' + sLineBreak + sLineBreak +

      '[mysqld]' + sLineBreak +
      'port=3306' + sLineBreak +
      'socket=/tmp/mysql.sock' + sLineBreak +
      'key_buffer_size=256M' + sLineBreak +
      'max_allowed_packet=512M' + sLineBreak +
      'table_open_cache=256' + sLineBreak +
      'sort_buffer_size=1M' + sLineBreak +
      'read_buffer_size=1M' + sLineBreak +
      'read_rnd_buffer_size=4M' + sLineBreak +
      'myisam_sort_buffer_size=64M' + sLineBreak +
      'thread_cache_size=8' + sLineBreak + sLineBreak +

      'secure-file-priv=""' + sLineBreak +
      'explicit_defaults_for_timestamp=1' + sLineBreak +
      'datadir="' + StringReplace(DataDir, '\', '/', [rfReplaceAll]) + '"' + sLineBreak +
      AuthPluginLine + sLineBreak + sLineBreak +

      '[mysqldump]' + sLineBreak +
      'quick' + sLineBreak +
      'max_allowed_packet=512M';

    IniFile.SaveToFile(IniPath);
  finally
    IniFile.Free;
  end;
end;

end.

