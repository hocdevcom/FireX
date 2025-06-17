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
  ExtensionDirFound: Boolean;
  LastCommentLineIndex: Integer;
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
    ExtensionDirFound := False;
    LastCommentLineIndex := -1;
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
        else if Pos('session.save_path', Line) = 1 then
          PhpIniFile[i] := 'session.save_path = "' + StringReplace(LaragonDir + 'tmp"', '\', '/', [rfReplaceAll])

        else if Pos('extension_dir', Line) = 1 then
        begin
          // Nếu đã có dòng extension_dir mà không bị comment, thì cập nhật
          ExtensionDirFound := True;
          PhpIniFile[i] := 'extension_dir = "' + StringReplace(LaragonDir + 'bin\php\' + SelectedPhpVersion + '\ext"', '\', '/', [rfReplaceAll]);
        end
        else if Pos(';extension_dir', Line) = 1 then
        begin
          // Nếu chỉ thấy dòng comment ;extension_dir, lưu chỉ số dòng comment cuối cùng
          LastCommentLineIndex := i;
        end;
      end;

      // Nếu không tìm thấy dòng extension_dir, nhưng tìm thấy dòng comment ;extension_dir, thì thay thế dòng comment cuối cùng
      if not ExtensionDirFound and (LastCommentLineIndex >= 0) then
      begin
        PhpIniFile[LastCommentLineIndex] := 'extension_dir = "' + StringReplace(LaragonDir + 'bin\php\' + SelectedPhpVersion + '\ext"', '\', '/', [rfReplaceAll]);
        ExtensionDirFound := True;
      end;

      // Nếu không tìm thấy cả extension_dir và dòng comment ;extension_dir, thêm dòng vào cuối file
      if not ExtensionDirFound then
        PhpIniFile.Add('extension_dir = "' + StringReplace(LaragonDir + 'bin\php\' + SelectedPhpVersion + '\ext"', '\', '/', [rfReplaceAll]));

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

