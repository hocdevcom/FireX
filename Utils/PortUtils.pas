unit PortUtils;

interface

uses
  System.SysUtils, System.IOUtils, System.Classes, WriteToLog;

type
  TPortUtils = class
  public
    class function GetApachePort(const ApacheDir, ApacheVersion: string): string;
    class function GetMySQLPort(const MySQLDir, MySQLVersion: string): string;
    class function SetApachePort(const ApacheDir, ApacheVersion, NewPort: string): Boolean; static;
    class function SetMySQLPort(const MySQLDir, MySQLVersion, NewPort: string): Boolean; static;
  end;

var
  SelectedApacheVersion, SelectedMySQLVersion: string;
  FApacheDir, FMySQLDir: string;

implementation

uses
  System.StrUtils;

class function TPortUtils.GetApachePort(const ApacheDir, ApacheVersion: string): string;
var
  Lines: TStringList;
  Line, ListenValue: string;
  I: Integer;
  FilePath: string;
  parts: TArray<string>;
begin
  Result := '80'; // mặc định
  FilePath := TPath.Combine(ApacheDir, ApacheVersion, 'conf', 'httpd.conf');
  Lines := TStringList.Create;
  try
    if FileExists(FilePath) then
    begin
      Lines.LoadFromFile(FilePath);
      for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        if (Line <> '') and (not Line.StartsWith('#')) and (LowerCase(Copy(Line, 1, 6)) = 'listen') then
        begin
          ListenValue := Trim(Copy(Line, 7, Length(Line))); // cắt phần "Listen"
          parts := ListenValue.Split([':']);
          if Length(parts) > 1 then
            Result := Trim(parts[High(parts)])
          else
            Result := Trim(ListenValue);
          Exit;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

class function TPortUtils.GetMySQLPort(const MySQLDir, MySQLVersion: string): string;
var
  Lines: TStringList;
  Line: string;
  I: Integer;
  FilePath: string;
begin
  Result := '3306'; // mặc định
  FilePath := TPath.Combine(MySQLDir, MySQLVersion, 'my.ini');
  Lines := TStringList.Create;
  try
    if FileExists(FilePath) then
    begin
      Lines.LoadFromFile(FilePath);
      for I := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[I]);
        if Line.ToLower.StartsWith('port') and Line.Contains('=') then
        begin
          Result := Trim(StringReplace(Line, 'port=', '', [rfReplaceAll, rfIgnoreCase]));
          Exit;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

class function TPortUtils.SetApachePort(const ApacheDir, ApacheVersion, NewPort: string): Boolean;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  FilePath: string;
begin
  Result := False;
  FilePath := TPath.Combine(ApacheDir, ApacheVersion, 'conf', 'httpd.conf');
  //WriteToLogMessage(FilePath);
  if not FileExists(FilePath) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath);
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if (Line <> '') and (not Line.StartsWith('#')) and (LowerCase(Copy(Line, 1, 6)) = 'listen') then
      begin
        Lines[I] := 'Listen ' + NewPort;
        Lines.SaveToFile(FilePath);
        Result := True;
        Exit;
      end;
    end;

    // Nếu không tìm thấy dòng Listen, thêm vào cuối
    Lines.Add('Listen ' + NewPort);
    Lines.SaveToFile(FilePath);
    Result := True;
  finally
    Lines.Free;
  end;
end;

class function TPortUtils.SetMySQLPort(const MySQLDir, MySQLVersion, NewPort: string): Boolean;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  FilePath: string;
begin
  Result := False;
  FilePath := TPath.Combine(MySQLDir, MySQLVersion, 'my.ini');
  if not FileExists(FilePath) then Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FilePath);
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line.ToLower.StartsWith('port') and Line.Contains('=') then
      begin
        Lines[I] := 'port=' + NewPort;
        Lines.SaveToFile(FilePath);
        Result := True;
        Exit;
      end;
    end;

    // Nếu không tìm thấy port, thêm vào cuối
    Lines.Add('port=' + NewPort);
    Lines.SaveToFile(FilePath);
    Result := True;
  finally
    Lines.Free;
  end;
end;


initialization
  SelectedApacheVersion := '';
  SelectedMySQLVersion := '';
  FApacheDir := '';
  FMySQLDir := '';

end.
