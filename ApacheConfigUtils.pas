unit ApacheConfigUtils;

interface

uses
  Vcl.Forms, System.SysUtils, System.Classes, Vcl.Dialogs, WriteToLog;

procedure UpdateApacheConfigs(const ApacheBinDir: string; const SelectedVersion: string);

implementation

procedure UpdateApacheConfigs(const ApacheBinDir: string; const SelectedVersion: string);
var
  HttpdConfPath, ModPhpConfPath, LaragonDir, ApacheFullPath: string;
  HttpdConfFile, ModPhpConfFile: TStringList;
  i: Integer;
  Line, NewLine: string;
  PosApacheDir: Integer;
begin
  try
    // Lấy thư mục gốc của ứng dụng (LaragonDir)
    LaragonDir := ExtractFilePath(Application.ExeName);

    // Kiểm tra nếu SelectedVersion không rỗng
    if Trim(SelectedVersion) = '' then
    begin
      ShowMessage('Chưa chọn phiên bản Apache!');
      Exit;
    end;

    // Cập nhật httpd.conf
    HttpdConfPath := IncludeTrailingPathDelimiter(LaragonDir) + 'bin\apache\' + SelectedVersion + '\conf\httpd.conf';

    // Ghi đường dẫn vào log để kiểm tra
    WriteToLogMessage('Đường dẫn tới httpd.conf: ' + HttpdConfPath);

    // Kiểm tra nếu file cấu hình tồn tại
    if not FileExists(HttpdConfPath) then
    begin
      ShowMessage('Không tìm thấy file httpd.conf: ' + HttpdConfPath);
      Exit;
    end;

    HttpdConfFile := TStringList.Create;
    try
      // Đọc file cấu hình httpd.conf
      HttpdConfFile.LoadFromFile(HttpdConfPath);

      // Tạo đường dẫn đầy đủ cho Apache
      ApacheFullPath := IncludeTrailingPathDelimiter(LaragonDir) + 'bin/apache/' + SelectedVersion;

      // Đảm bảo dấu gạch chéo xuôi cho đường dẫn ApacheFullPath
      ApacheFullPath := StringReplace(ApacheFullPath, '\', '/', [rfReplaceAll]);

      // Duyệt qua từng dòng trong file httpd.conf và thay thế các đường dẫn
      for i := 0 to HttpdConfFile.Count - 1 do
      begin
        Line := HttpdConfFile[i];

        if (Trim(Line) <> '') and (Copy(Trim(Line), 1, 1) <> '#') then
        begin
          // Kiểm tra nếu dòng là <Directory />
          if Pos('<Directory />', Line) > 0 then
          begin
            // Không sửa nếu gặp <Directory />
            Continue;
          end
          // Kiểm tra và thay thế SRVROOT và ServerRoot
          else if Pos('Define SRVROOT', Line) > 0 then
          begin
            NewLine := 'Define SRVROOT "' + ApacheFullPath + '"';
            HttpdConfFile[i] := NewLine;
          end
          else if Pos('ServerRoot', Line) > 0 then
          begin
            NewLine := 'ServerRoot "' + ApacheFullPath + '"';
            HttpdConfFile[i] := NewLine;
          end
          // Sửa DocumentRoot
          else if Pos('DocumentRoot', Line) > 0 then
          begin
            NewLine := 'DocumentRoot "' + StringReplace(LaragonDir + 'www', '\', '/', [rfReplaceAll]) + '"';
            HttpdConfFile[i] := NewLine;
          end
          // Sửa Directory
          else if Pos('<Directory', Line) > 0 then
          begin
            if Pos('${SRVROOT}/cgi-bin', Line) > 0 then
              Continue
            else
            begin
              NewLine := '<Directory "' + StringReplace(LaragonDir + 'www', '\', '/', [rfReplaceAll]) + '">';
              HttpdConfFile[i] := NewLine;
            end;
          end
          // Chỉ thay thế nếu dòng chứa "Include" hoặc "IncludeOptional"
          else if (Pos('/etc/apache2', Line) > 0) and ((Pos('Include ', Line) = 1) or (Pos('IncludeOptional ', Line) = 1)) then
          begin
            PosApacheDir := Pos('/etc/apache2', Line);
            if PosApacheDir > 0 then
            begin
              NewLine := LaragonDir + Copy(Line, PosApacheDir, MaxInt);
              NewLine := StringReplace(NewLine, '\', '/', [rfReplaceAll]);
              NewLine := StringReplace(NewLine, '//', '/', [rfReplaceAll]);
              if Pos('IncludeOptional', Line) > 0 then
                HttpdConfFile[i] := 'IncludeOptional "' + NewLine
              else if Pos('Include', Line) > 0 then
                HttpdConfFile[i] := 'Include "' + NewLine;
            end;
          end;
        end;
      end;

      // Lưu lại file httpd.conf
      HttpdConfFile.SaveToFile(HttpdConfPath);

    finally
      HttpdConfFile.Free;
    end;

    // Cập nhật mod_php.conf
    ModPhpConfPath := IncludeTrailingPathDelimiter(LaragonDir) + 'etc/apache2/mod_php.conf';

    WriteToLogMessage('Đường dẫn tới mod_php.conf: ' + ModPhpConfPath);

    if not FileExists(ModPhpConfPath) then
    begin
      ShowMessage('Không tìm thấy file mod_php.conf: ' + ModPhpConfPath);
      Exit;
    end;

    ModPhpConfFile := TStringList.Create;
    try
      // Đọc file cấu hình mod_php.conf
      ModPhpConfFile.LoadFromFile(ModPhpConfPath);

      // Duyệt qua từng dòng trong file mod_php.conf
      for i := 0 to ModPhpConfFile.Count - 1 do
      begin
        Line := ModPhpConfFile[i];

        // Thay thế đường dẫn LoadModule php_module
        if Pos('LoadModule php_module', Line) > 0 then
        begin
          NewLine := 'LoadModule php_module "' + StringReplace(LaragonDir + 'bin/php/php-8.3.14-Win32-vs16-x64/php8apache2_4.dll', '\', '/', [rfReplaceAll]) + '"';
          ModPhpConfFile[i] := NewLine;
        end
        // Thay thế đường dẫn PHPIniDir
        else if Pos('PHPIniDir', Line) > 0 then
        begin
          NewLine := 'PHPIniDir "' + StringReplace(LaragonDir + 'bin/php/php-8.3.14-Win32-vs16-x64', '\', '/', [rfReplaceAll]) + '"';
          ModPhpConfFile[i] := NewLine;
        end;
      end;

      // Lưu lại file mod_php.conf
      ModPhpConfFile.SaveToFile(ModPhpConfPath);

    finally
      ModPhpConfFile.Free;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Lỗi khi xử lý Apache config: ' + E.Message);
    end;
  end;
end;

end.
