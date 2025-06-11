unit PhpIniUpdater;

interface

uses
  System.SysUtils, System.Classes, Vcl.Dialogs, WriteToLog;

procedure UpdatePhpIni(const LaragonDir, SelectedVersion, SelectedPhpVersion: string);

implementation

uses
  System.IOUtils;

procedure UpdatePhpIni(const LaragonDir, SelectedVersion, SelectedPhpVersion: string);
var
  PhpIniPath, PhpBaseDir, ProdIniPath, DevIniPath: string;
  PhpIniFile: TStringList;
  i: Integer;
  Line: string;
begin
  try
    PhpBaseDir := IncludeTrailingPathDelimiter(LaragonDir) + 'bin\php\' + SelectedPhpVersion + '\';
    PhpIniPath := IncludeTrailingPathDelimiter(LaragonDir) + 'bin\php\' + SelectedPhpVersion + '\php.ini';
    ProdIniPath := PhpBaseDir + 'php.ini-production';
    DevIniPath := PhpBaseDir + 'php.ini-development';

    WriteToLogMessage('Đường dẫn tới php.ini: ' + PhpIniPath);

    // Nếu không tồn tại php.ini, cố gắng sao chép từ php.ini-production hoặc php.ini-development
    if not FileExists(PhpIniPath) then
    begin
      if FileExists(ProdIniPath) then
      begin
        TFile.Copy(ProdIniPath, PhpIniPath);
        WriteToLogMessage('Đã sao chép từ php.ini-production');
      end
      else if FileExists(DevIniPath) then
      begin
        TFile.Copy(DevIniPath, PhpIniPath);
        WriteToLogMessage('Đã sao chép từ php.ini-development');
      end
      else
      begin
        ShowMessage('Không tìm thấy file php.ini: ' + PhpIniPath);
        Exit;
      end;
    end;

    PhpIniFile := TStringList.Create;
    try
      PhpIniFile.LoadFromFile(PhpIniPath);

      for i := 0 to PhpIniFile.Count - 1 do
      begin
        Line := PhpIniFile[i];

        if Pos('curl.cainfo', Line) = 1 then
          PhpIniFile[i] := 'curl.cainfo = "' + StringReplace(LaragonDir + 'etc\ssl\cacert.pem"', '\', '/', [rfReplaceAll])
        else if Pos('error_log', Line) = 1 then
          PhpIniFile[i] := 'error_log = "' + StringReplace(LaragonDir + 'tmp\php_errors.log"', '\', '/', [rfReplaceAll])
        else if Pos('include_path', Line) = 1 then
          PhpIniFile[i] := 'include_path = ".;' + StringReplace(LaragonDir + 'etc\php\pear"', '\', '/', [rfReplaceAll])
        else if Pos('extension_dir', Line) = 1 then
          PhpIniFile[i] := 'extension_dir = "' + StringReplace(LaragonDir + 'bin\php\' + SelectedPhpVersion + '\ext"', '\', '/', [rfReplaceAll])
        else if Pos('session.save_path', Line) = 1 then
          PhpIniFile[i] := 'session.save_path = "' + StringReplace(LaragonDir + 'tmp"', '\', '/', [rfReplaceAll]);
      end;

      PhpIniFile.SaveToFile(PhpIniPath);

    finally
      PhpIniFile.Free;
    end;

  except
    on E: Exception do
      ShowMessage('Lỗi khi cập nhật php.ini: ' + E.Message);
  end;
end;

end.

