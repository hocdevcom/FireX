unit ApacheConfig;

interface

uses
  Vcl.Forms, System.SysUtils, System.Classes, Vcl.Dialogs, PhpIniUpdater, WriteToLog;

procedure UpdateApacheConfigs(const FApacheDir: string; const SelectedVersion: string; const SelectedPhpVersion: string);

implementation

procedure UpdateApacheConfigs(const FApacheDir: string; const SelectedVersion: string; const SelectedPhpVersion: string);
var
  HttpdConfPath, ModPhpConfPath, LaragonDir, ApacheFullPath, NewLine: string;
  HttpdConfFile, ModPhpConfFile: TStringList;
  i: Integer;
  Line: string;
  IncludePaths: array[0..3] of string;
  IncludeLine: string;
  NeedEmptyLine: Boolean;
  IncludeAlreadyExists: Boolean;
  
  // Hàm chuẩn hóa đường dẫn về dấu /
  function NormalizePath(const Path: string): string;
  begin
    Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
  end;

  // Hàm kiểm tra và thêm Include nếu cần thiết
  procedure AddIncludeIfMissing(var FileList: TStringList; const IncludeLine: string);
  var
    Line: string;
    IncludeExists: Boolean;
  begin
    IncludeExists := False;
    for Line in FileList do
    begin
      if Line = IncludeLine then
      begin
        IncludeExists := True;
        Break;
      end;
    end;
    if not IncludeExists then
      FileList.Add(IncludeLine);
  end;

begin
  try
    // Lấy thư mục gốc của ứng dụng (LaragonDir)
    LaragonDir := ExtractFilePath(Application.ExeName);

    if Trim(SelectedVersion) = '' then
    begin
      ShowMessage('Chưa chọn phiên bản Apache!');
      Exit;
    end;

    if Trim(SelectedPhpVersion) = '' then
    begin
      ShowMessage('Chưa chọn phiên bản PHP!');
      Exit;
    end;

    // Cập nhật httpd.conf
    HttpdConfPath := LaragonDir + 'bin\apache\' + SelectedVersion + '\conf\httpd.conf';
    if not FileExists(HttpdConfPath) then
    begin
      ShowMessage('Không tìm thấy file httpd.conf: ' + HttpdConfPath);
      Exit;
    end;

    HttpdConfFile := TStringList.Create;
    try
      HttpdConfFile.LoadFromFile(HttpdConfPath);
      ApacheFullPath := NormalizePath(LaragonDir + 'bin/apache/' + SelectedVersion);

      // Duyệt qua từng dòng và thay thế nhanh chóng
      for i := 0 to HttpdConfFile.Count - 1 do
      begin
        Line := HttpdConfFile[i];

        // Cập nhật các dòng có chứa 'ServerRoot', 'Define SRVROOT', 'DocumentRoot', và <Directory>
        if (Pos('ServerRoot', Trim(Line)) = 1) and (Line[1] <> '#') then
          HttpdConfFile[i] := 'ServerRoot "' + ApacheFullPath + '"'
        else if (Pos('Define SRVROOT', Trim(Line)) = 1) and (Line[1] <> '#') then
          HttpdConfFile[i] := 'Define SRVROOT "' + ApacheFullPath + '"'
        else if (Pos('DocumentRoot', Trim(Line)) = 1) and (Line[1] <> '#') then
          HttpdConfFile[i] := 'DocumentRoot "' + NormalizePath(LaragonDir + 'www') + '"'
        else if (Pos('<Directory', Trim(Line)) = 1) and (Line[1] <> '#') then
        begin
          // Nếu <Directory /> hoặc <Directory "${SRVROOT}/cgi-bin"> thì không sửa
          if (Pos('<Directory />', Line) = 1) or (Pos('<Directory "${SRVROOT}/cgi-bin">', Line) = 1) then
            Continue; // Không thay đổi

          HttpdConfFile[i] := '<Directory "' + NormalizePath(LaragonDir + 'www') + '">';
        end
        else if (Pos('ServerName', Trim(Line)) = 1) and (Line[1] <> '#') then
          HttpdConfFile[i] := 'ServerName LaraX'
        else if (Pos('Include', Line) > 0) and (Line[1] <> '#') then
        begin
          // Kiểm tra và thay thế include với đường dẫn đúng
          if (Pos('Include', Line) > 0) and (Line[1] <> '#') then
          begin
            Line := NormalizePath(Line);

            if Pos('/etc/apache2/', Line) > 0 then
            begin
              var StartQuote := Pos('"', Line);
              var EndQuote := LastDelimiter('"', Line);

              if (StartQuote > 0) and (EndQuote > StartQuote) then
              begin
                var FullPath := Copy(Line, StartQuote + 1, EndQuote - StartQuote - 1);
                var RelPos := Pos('/etc/apache2/', FullPath);

                if RelPos > 0 then
                begin
                  var RelativePath := Copy(FullPath, RelPos + 1, MaxInt); // Bỏ dấu / đầu
                  var CleanLaragon := NormalizePath(IncludeTrailingPathDelimiter(LaragonDir));
                  var NewFullPath := CleanLaragon + RelativePath;
                  HttpdConfFile[i] := Copy(Line, 1, StartQuote) + NewFullPath + '"';
                end;
              end;
            end;
          end;
        end;
      end;

      // Danh sách các Include cần kiểm tra/cập nhật
      IncludePaths[0] := 'IncludeOptional "' + NormalizePath(LaragonDir + 'etc/apache2/alias/*.conf') + '"';
      IncludePaths[1] := 'IncludeOptional "' + NormalizePath(LaragonDir + 'etc/apache2/sites-enabled/*.conf') + '"';
      IncludePaths[2] := 'Include "' + NormalizePath(LaragonDir + 'etc/apache2/httpd-ssl.conf') + '"';
      IncludePaths[3] := 'Include "' + NormalizePath(LaragonDir + 'etc/apache2/mod_php.conf') + '"';

      IncludeAlreadyExists := False;

      // Kiểm tra xem có dòng Include nào đã tồn tại chưa
      for i := 0 to High(IncludePaths) do
      begin
        IncludeLine := NormalizePath(IncludePaths[i]);

        for var j := 0 to HttpdConfFile.Count - 1 do
        begin
          Line := NormalizePath(Trim(HttpdConfFile[j]));
          if Line = IncludeLine then
          begin
            IncludeAlreadyExists := True;
            Break;
          end;
        end;

        if IncludeAlreadyExists then
          Break;
      end;

      // Nếu chưa có dòng Include nào và dòng cuối không trống => thêm dòng trống
      NeedEmptyLine := not IncludeAlreadyExists and
                       ((HttpdConfFile.Count = 0) or (Trim(HttpdConfFile[HttpdConfFile.Count - 1]) <> ''));

      if NeedEmptyLine then
        HttpdConfFile.Add('');

      // Thêm từng dòng Include nếu chưa có
      for i := 0 to High(IncludePaths) do
      begin
        IncludeLine := NormalizePath(IncludePaths[i]);
        var Found := False;

        for var j := 0 to HttpdConfFile.Count - 1 do
        begin
          Line := NormalizePath(Trim(HttpdConfFile[j]));
          if Line = IncludeLine then
          begin
            Found := True;
            Break;
          end;
        end;

        if not Found then
          HttpdConfFile.Add(IncludeLine);
      end;


      HttpdConfFile.SaveToFile(HttpdConfPath);

    finally
      HttpdConfFile.Free;
    end;

    // Cập nhật mod_php.conf
    ModPhpConfPath := LaragonDir + 'etc\apache2\mod_php.conf';
    if not FileExists(ModPhpConfPath) then
    begin
      ShowMessage('Không tìm thấy file mod_php.conf: ' + ModPhpConfPath);
      Exit;
    end;

    ModPhpConfFile := TStringList.Create;
    try
      ModPhpConfFile.LoadFromFile(ModPhpConfPath);

      // Thay thế LoadModule php_module và PHPIniDir trong mod_php.conf
      for i := 0 to ModPhpConfFile.Count - 1 do
      begin
        Line := ModPhpConfFile[i];

        if (Pos('LoadModule php_module', Line) = 1) and (Line[1] <> '#') then
          ModPhpConfFile[i] := 'LoadModule php_module "' + NormalizePath(LaragonDir + 'bin/php/' + SelectedPhpVersion + '/php8apache2_4.dll') + '"'
        else if (Pos('PHPIniDir', Line) = 1) and (Line[1] <> '#') then
          ModPhpConfFile[i] := 'PHPIniDir "' + NormalizePath(LaragonDir + 'bin/php/' + SelectedPhpVersion) + '"';
      end;

      ModPhpConfFile.SaveToFile(ModPhpConfPath);

    finally
      ModPhpConfFile.Free;
    end;

  except
    on E: Exception do
      ShowMessage('Lỗi khi xử lý Apache config: ' + E.Message);
  end;

  // Cập nhật php.ini sau khi cập nhật các file Apache
  UpdatePhpIni(LaragonDir, SelectedVersion, SelectedPhpVersion);
end;

end.
