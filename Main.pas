unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Math, System.RegularExpressions,
  System.StrUtils, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, SelectVersion,
  ServicesStop, AppConfig, MySQLIniUtils, TlHelp32, ShellAPI, System.IOUtils,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.ButtonGroup, System.ImageList,
  Vcl.ImgList, System.UITypes, Vcl.Buttons, VersionWatcher, WriteToLog,
  ServicesConfig, TrayIconHandler, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Settings, PortUtils;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    PopApache: TMenuItem;
    Version1: TMenuItem;
    TrayIcon1: TTrayIcon;
    PopQuit: TMenuItem;
    PopPHP: TMenuItem;
    Version2: TMenuItem;
    BtnStart: TBitBtn;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    BtnWeb: TBitBtn;
    BtnDB: TBitBtn;
    BtnTer: TBitBtn;
    BtnRoot: TBitBtn;
    Version3: TMenuItem;
    VirtualImageList2: TVirtualImageList;
    ImgSettings: TImage;
    SubExt: TMenuItem;
    procedure btRootClick(Sender: TObject);
    procedure btWebClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApacheVersionMenuClick(Sender: TObject);
    procedure PhpVersionMenuClick(Sender: TObject);
    procedure MySQLVersionMenuClick(Sender: TObject);
    procedure PopQuitClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnDBClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ImgSettingsMouseEnter(Sender: TObject);
    procedure ImgSettingsMouseLeave(Sender: TObject);
    procedure ImgSettingsClick(Sender: TObject);
    procedure OnPortChanged;
  private
    lbApache, lbApacheStatus: TLabel;
    lbMySQL, lbMySQLStatus: TLabel;
    lbApachePort, lbMySQLPort: TLabel;
    FApacheChangeHandle: THandle;
    FPhpChangeHandle: THandle;
    PIDFilePath: string;
    procedure UncheckVersionMenu(VersionMenu: TMenuItem);
    procedure SelectVersion(Menu: TMenuItem; const NewVersion: string; var SelectedVersion: string; const LabelToUpdate: TLabel = nil);
    function IsApachePhpCompatible(const ApacheVersion, PhpVersion: string; ShowWarning: Boolean = True): Boolean;
    procedure StartApache(const ApachePath: string);
    procedure StartMySQL(const MySQLExePath: string);
    procedure StopServices;
    procedure UpdateApacheChange(IsRunning: Boolean);
    procedure UpdateMySQLChange(IsRunning: Boolean);
    procedure InitializeTrayIcon;
    procedure UpdateWindowTitle;
    procedure CreateStatusLabel(const ACaption: string; ALeft, ATop: Integer;
  out ALabel, AStatus, APortLabel: TLabel; PortValue: string);
    procedure CheckSelectedVersionInMenu(Menu: TMenuItem; const Version: string);
    procedure UpdateExtensionsMenu;
    procedure PHPExtensionClick(Sender: TObject);
  public
    SelectedApacheVersion: string;
    SelectedPhpVersion: string;
    SelectedMySQLVersion: string;
    FApacheDir, FPHPDir, FMySQLDir: string;
    function GetBinPath(const SubDir: string): string;
  end;

var
  Form1: TForm1;
  Mutex: THandle;

const
  MutexName = '{7E215C93-8F62-442F-89F2-BC1E9ECA6297}';
  AppBaseTitle = 'FireX 1.0.0';
  SYMBOL_STOP = '■';
  SYMBOL_RUN = '▶';

implementation

{$R *.dfm}

procedure SetImageFromIndex(Image: TImage; ImgList: TVirtualImageList; Index: Integer);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    ImgList.GetBitmap(Index, bmp);
    Image.Picture.Bitmap.Assign(bmp);
    Image.Refresh;  // Cập nhật lại hiển thị
  finally
    bmp.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ApachePort, MySQLPort: string;
begin
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    ShowMessage('Ứng dụng đã đang chạy!');
    Application.Terminate;
    Exit;
  end;

  Application.Icon := Self.Icon;
  InitializeTrayIcon;

  FApacheDir := GetBinPath('apache');
  FPHPDir := GetBinPath('php');
  FMySQLDir := GetBinPath('mysql');

  if not DirectoryExists(FApacheDir) then
  begin
    ShowMessage('Thư mục bin\apache không tồn tại!');
    Exit;
  end;

  UpdateApacheVersionMenu(FApacheDir);
  UpdatePhpVersionMenu(FPHPDir);
  UpdateMySQLVersionMenu(FMySQLDir);

  SelectedApacheVersion := '';
  SelectedPhpVersion := '';
  SelectedMySQLVersion := '';

  if Config.IsVersionValid(Config.ApacheVersion, FApacheDir) then
    SelectedApacheVersion := Config.ApacheVersion;

  if Config.IsVersionValid(Config.PhpVersion, FPHPDir) then
    SelectedPhpVersion := Config.PhpVersion;

  if Config.IsVersionValid(Config.MySQLVersion, FMySQLDir) then
    SelectedMySQLVersion := Config.MySQLVersion;

  ApachePort := TPortUtils.GetApachePort(FApacheDir, SelectedApacheVersion);
  MySQLPort := TPortUtils.GetMySQLPort(FMySQLDir, SelectedMySQLVersion);

  CreateStatusLabel('Apache', 130, 50, lbApache, lbApacheStatus, lbApachePort, ApachePort);
  CreateStatusLabel('MySQL', 130, 80, lbMySQL, lbMySQLStatus, lbMySQLPort, MySQLPort);
  SetImageFromIndex(ImgSettings, VirtualImageList2, 0);

  if SelectedApacheVersion <> '' then
  begin
    lbApache.Caption := 'Apache ' + ExtractVersionNumber(SelectedApacheVersion);
    CheckSelectedVersionInMenu(Version1, SelectedApacheVersion);
  end;

  if SelectedMySQLVersion <> '' then
  begin
    lbMySQL.Caption := 'MySQL ' + ExtractVersionNumber(SelectedMySQLVersion);
    CheckSelectedVersionInMenu(Version3, SelectedMySQLVersion);
  end;

  if SelectedPhpVersion <> '' then
  begin
    CheckSelectedVersionInMenu(Version2, SelectedPhpVersion);
    UpdateWindowTitle;
    UpdateExtensionsMenu;
  end;

  TThread.CreateAnonymousThread(WatchVersionDirs).Start;
  UpdateApacheChange(ServicesRunning('httpd.exe'));
  UpdateMySQLChange(ServicesRunning('mysqld.exe'));
end;

procedure TForm1.UpdateExtensionsMenu;
var
  FPHPIniPath: string;
  IniFile: TStringList;
  I: Integer;
  ExtName, LineTrimmed: string;
  ExtMenuItem: TMenuItem;
begin
  if SelectedPhpVersion = '' then Exit;

  // Xóa các item cũ
  while SubExt.Count > 0 do
    SubExt.Delete(0);

  FPHPIniPath := IncludeTrailingPathDelimiter(FPHPDir) + SelectedPhpVersion + '\php.ini';
  if not FileExists(FPHPIniPath) then Exit;

  IniFile := TStringList.Create;
  try
    IniFile.LoadFromFile(FPHPIniPath);

    for I := 0 to IniFile.Count - 1 do
    begin
      LineTrimmed := Trim(IniFile[I]);

      // Kiểm tra dòng bắt đầu bằng extension= hoặc ;extension=
      if (Pos('extension=', LineTrimmed) = 1) or (Pos(';extension=', LineTrimmed) = 1) then
      begin
        // Cắt phần sau dấu =
        ExtName := Trim(Copy(LineTrimmed, Pos('=', LineTrimmed) + 1));

        // Chỉ lấy tên tới dấu cách đầu tiên (nếu có)
        if Pos(' ', ExtName) > 0 then
          ExtName := Copy(ExtName, 1, Pos(' ', ExtName) - 1);

        // Loại bỏ phần comment sau dấu ;
        if Pos(';', ExtName) > 0 then
          ExtName := Copy(ExtName, 1, Pos(';', ExtName) - 1);

        ExtName := Trim(ExtName);

        if ExtName <> '' then
        begin
          //WriteToLogMessage('Parsed extension in menu: ' + ExtName);

          ExtMenuItem := TMenuItem.Create(SubExt);
          ExtMenuItem.Caption := ExtName;
          ExtMenuItem.AutoCheck := True;
          ExtMenuItem.Checked := (Pos(';', LineTrimmed) <> 1);
          ExtMenuItem.OnClick := PHPExtensionClick;
          SubExt.Add(ExtMenuItem);
        end;
      end;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.PHPExtensionClick(Sender: TObject);
var
  IniFile: TStringList;
  FPHPIniPath, Line, ExtName, LineTrimmed, ExtPart: string;
  I, EqPos: Integer;
  MenuItem: TMenuItem;
begin
  if not (Sender is TMenuItem) then Exit;
  MenuItem := TMenuItem(Sender);
  ExtName := StringReplace(MenuItem.Caption, '&', '', [rfReplaceAll]);

  //WriteToLogMessage('Clicked extension: ' + ExtName);

  FPHPIniPath := IncludeTrailingPathDelimiter(FPHPDir) + SelectedPhpVersion + '\php.ini';
  if not FileExists(FPHPIniPath) then
  begin
    //WriteToLogMessage('php.ini not found: ' + FPHPIniPath);
    Exit;
  end;

  IniFile := TStringList.Create;
  try
    IniFile.LoadFromFile(FPHPIniPath);

    for I := 0 to IniFile.Count - 1 do
    begin
      Line := IniFile[I];
      LineTrimmed := TrimLeft(Line);

      if (Pos('extension=', LineTrimmed) = 1) or (Pos(';extension=', LineTrimmed) = 1) then
      begin
        EqPos := Pos('=', LineTrimmed);
        if EqPos > 0 then
        begin
          ExtPart := Trim(Copy(LineTrimmed, EqPos + 1, MaxInt));

          // Cắt bỏ phần sau tên extension
          if Pos(' ', ExtPart) > 0 then
            ExtPart := Copy(ExtPart, 1, Pos(' ', ExtPart) - 1)
          else if Pos(';', ExtPart) > 0 then
            ExtPart := Copy(ExtPart, 1, Pos(';', ExtPart) - 1);

          ExtPart := Trim(ExtPart);

          if SameText(ExtPart, ExtName) then
          begin
            //WriteToLogMessage('  => Match found: ' + LineTrimmed);

            if Pos(';', LineTrimmed) = 1 then
            begin
              // Bỏ comment (;) để bật extension
              IniFile[I] := Copy(Line, Pos(';', Line) + 1, MaxInt);
              MenuItem.Checked := True;  // Đánh dấu tích
              //WriteToLogMessage('  => Extension enabled');
            end
            else
            begin
              // Thêm comment (;) để tắt extension
              IniFile[I] := ';' + Line;
              MenuItem.Checked := False; // Bỏ tích
              //WriteToLogMessage('  => Extension disabled');
            end;

            Break;
          end;
        end;
      end;
    end;

    IniFile.SaveToFile(FPHPIniPath);
    //WriteToLogMessage('php.ini saved');
  finally
    IniFile.Free;
  end;
end;


function TForm1.IsApachePhpCompatible(const ApacheVersion, PhpVersion: string; ShowWarning: Boolean = True): Boolean;
var
  ApacheVerNum, PhpVerNum: string;
  ApacheIsNew, PhpIsNew: Boolean;
begin
  Result := True;
  if (ApacheVersion = '') or (PhpVersion = '') then Exit;

  ApacheVerNum := ExtractVersionNumber(ApacheVersion);
  PhpVerNum := ExtractVersionNumber(PhpVersion);

  ApacheIsNew := IsVersionGreaterOrEqual(ApacheVerNum, '2.4.62');
  PhpIsNew := IsVersionGreaterOrEqual(PhpVerNum, '8.4.0');

  // Nếu không cùng "loại" (mới/cũ), là không tương thích
  if ApacheIsNew xor PhpIsNew then
  begin
    Result := False;
    if ShowWarning then
    begin
    MessageDlg('⚠ Apache và PHP không tương thích' + sLineBreak +
               '-  Apache ≥ 2.4.62 yêu cầu PHP ≥ 8.4.0' + sLineBreak +
               '-  Apache < 2.4.62 yêu cầu PHP < 8.4.0',
               mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TForm1.CheckSelectedVersionInMenu(Menu: TMenuItem; const Version: string);
var
  I: Integer;
begin
  if Version = '' then Exit;

  for I := 0 to Menu.Count - 1 do
  begin
    if SameText(Menu.Items[I].Hint, Version) then
    begin
      Menu.Items[I].Checked := True;
      Break;
    end;
  end;
end;

function TForm1.GetBinPath(const SubDir: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\' + SubDir;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  FrmPhu: TForm2;
begin
  FrmPhu := TForm2.Create(Self);

  // Căn giữa form phụ theo form chính (dùng Self)
  FrmPhu.Left := Self.Left + (Self.Width - FrmPhu.Width) div 2;
  FrmPhu.Top := Self.Top + (Self.Height - FrmPhu.Height) div 2;

  FrmPhu.Show;
end;

procedure TForm1.BtnDBClick(Sender: TObject);
var
  HeidiSQLPath: string;
begin
  HeidiSQLPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\heidisql\heidisql.exe';

  if FileExists(HeidiSQLPath) then
  begin
    ShellExecute(0, 'open', PChar(HeidiSQLPath), nil, nil, SW_SHOWNORMAL);
  end
  else
  begin
    ShowMessage('HeidiSQL executable not found at: ' + HeidiSQLPath);
  end;
end;

procedure TForm1.BtnStartClick(Sender: TObject);
var
  ApachePath, MySQLPath: string;
begin
  if SelectedApacheVersion = '' then
  begin
    ShowMessage('Chưa chọn phiên bản Apache!');
    Exit;
  end;

  if SelectedPhpVersion = '' then
  begin
    ShowMessage('Chưa chọn phiên bản PHP!');
    Exit;
  end;

  ApachePath := IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\bin\httpd.exe';
  MySQLPath := IncludeTrailingPathDelimiter(FMySQLDir) + SelectedMySQLVersion + '\bin\mysqld.exe';

  if not FileExists(ApachePath) then
  begin
    ShowMessage('Không tìm thấy Apache!: ' + ApachePath);
    //WriteToLogMessage('Không tìm thấy Apache! Đường dẫn: ' + ApachePath);
    Exit;
  end;

  if ServicesRunning(['httpd.exe', 'mysqld.exe']) then
  begin
    StopServices;
  end
  else
  begin
    // Kiểm tra tương thích 2 chiều giữa Apache và PHP (có cảnh báo nếu sai)
    if not IsApachePhpCompatible(SelectedApacheVersion, SelectedPhpVersion, True) then
      Exit;

    // Cập nhật lại file cấu hình
    ServicesConfig.UpdateApacheConfigs(
      IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion,
      SelectedApacheVersion,
      SelectedPhpVersion,
      SelectedMySQLVersion
    );

    StartApache(ApachePath);

    if ServicesRunning('httpd.exe') then
      StartMySQL(MySQLPath);
  end;
end;

procedure TForm1.btRootClick(Sender: TObject);
var
  FolderPath: string;
begin
  FolderPath := ExtractFilePath(Application.ExeName) + 'www';
  if not DirectoryExists(FolderPath) then
    ForceDirectories(FolderPath);
  ShellExecute(0, 'open', PChar(FolderPath), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.btWebClick(Sender: TObject);
var
  ApachePort: string;
begin
  ApachePort := lbApachePort.Caption;
  if ApachePort <> '' then
    ShellExecute(0, 'open', PChar('http://localhost:' + ApachePort), nil, nil, SW_SHOWNORMAL)
  else
    ShellExecute(0, 'open', 'http://localhost', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.CreateStatusLabel(const ACaption: string; ALeft, ATop: Integer;
  out ALabel, AStatus, APortLabel: TLabel; PortValue: string);
begin
  ALabel := TLabel.Create(Self);
  ALabel.Parent := Self;
  ALabel.Left := ALeft;
  ALabel.Top := ATop;
  ALabel.Enabled := False;
  ALabel.Caption := ACaption;
  ALabel.Font.Size := 11;
  ALabel.AutoSize := True;

  AStatus := TLabel.Create(Self);
  AStatus.Parent := Self;
  AStatus.Left := ALabel.Left + 120;
  AStatus.Top := ATop;
  AStatus.Enabled := False;
  AStatus.Caption := SYMBOL_STOP;
  AStatus.Font.Size := 11;
  AStatus.AutoSize := True;

  APortLabel := TLabel.Create(Self);
  APortLabel.Parent := Self;
  APortLabel.Left := AStatus.Left + 100;
  APortLabel.Top := ATop;
  AStatus.Enabled := False;
  APortLabel.Tag := StrToIntDef(PortValue, 0);
  APortLabel.Caption := PortValue;
  APortLabel.Font.Size := 11;
  APortLabel.AutoSize := True;
end;

procedure TForm1.UpdateApacheChange(IsRunning: Boolean);
begin
  if IsRunning then
  begin
    BtnStart.Caption := 'Stop';
    BtnStart.ImageIndex := 1;
    lbApache.Enabled := True;
    lbApacheStatus.Enabled := True;
    lbApachePort.Enabled := True;
    lbApacheStatus.Caption := SYMBOL_RUN;
    lbApacheStatus.Font.Color := clGreen;
    BtnDB.Enabled := True;
  end
  else
  begin
    BtnStart.Caption := 'Start';
    BtnStart.ImageIndex := 0;
    lbApache.Enabled := False;
    lbApachePort.Enabled := False;
    lbApacheStatus.Enabled := False;
    lbApacheStatus.Caption := SYMBOL_STOP;
    BtnDB.Enabled := False;
  end;
end;

procedure TForm1.UpdateMySQLChange(IsRunning: Boolean);
begin
  if IsRunning then
  begin
    lbMySQL.Enabled := True;
    lbMySQLStatus.Enabled := True;
    lbMySQLPort.Enabled := True;
    lbMySQLStatus.Caption := SYMBOL_RUN;
    lbMySQLStatus.Font.Color := clGreen;
  end
  else
  begin
    lbMySQL.Enabled := False;
    lbMySQLStatus.Enabled := False;
    lbMySQLPort.Enabled := False;
    lbMySQLStatus.Caption := SYMBOL_STOP;
  end;
end;

procedure TForm1.StartApache(const ApachePath: string);
var
  RetryCount: Integer;
  PidFile, ErrorLog: string;
begin
  PidFile := TPath.Combine(IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\logs', 'httpd.pid');
  if TFile.Exists(PidFile) then
    TFile.Delete(PidFile);

  ErrorLog := TPath.Combine(IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\logs', 'error.log');
  if TFile.Exists(ErrorLog) then
    TFile.Delete(ErrorLog);

  try
    if ShellExecute(0, 'open', PChar(ApachePath), nil, nil, SW_HIDE) <= 32 then
      raise Exception.Create('ShellExecute failed');
  except
    on E: Exception do
    begin
      ShowMessage('Lỗi khởi động Apache: ' + E.Message);
      //WriteToLogMessage('ShellExecute exception: ' + E.Message);
      Exit;
    end;
  end;

  RetryCount := 0;
  while (not ServicesRunning('httpd.exe')) and (RetryCount < 10) do
  begin
    Sleep(200);
    Inc(RetryCount);
  end;

  if ServicesRunning('httpd.exe') then
  begin
    UpdateApacheChange(True);
    //WriteToLogMessage('Apache started: ' + ApachePath);
  end
  else
  begin
    ShowMessage('Không thể khởi động Apache!');
    //WriteToLogMessage('Apache failed to start: ' + ApachePath);
  end;
end;

procedure TForm1.StopServices;
begin
  if FileExists(PIDFilePath) then
    KillServicesByPID(FApacheDir, FMySQLDir)
  else
    KillServices;

  if not ServicesRunning(['httpd.exe', 'mysqld.exe']) then
  begin
    UpdateApacheChange(False);
    UpdateMySQLChange(False);
  end
  else
  begin
    ShowMessage('Không thể dừng dịch vụ!');
  end;
end;

procedure TForm1.StartMySQL(const MySQLExePath: string);
var
  Params: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  PID: DWORD;
  MySQLVersionShort, MySQLDataDir, MySQLIniPath, PIDFile, LogFile, IbdataFile: string;
  Initialized: Boolean;
begin
  if not FileExists(MySQLExePath) then
  begin
    ShowMessage('Không tìm thấy mysqld.exe: ' + MySQLExePath);
    //WriteToLogMessage('Không tìm thấy mysqld.exe: ' + MySQLExePath);
    Exit;
  end;

  MySQLVersionShort := TRegEx.Replace(SelectedMySQLVersion, '^mysql-(\d+)\.(\d+)\..*$', 'mysql-$1.$2');
  MySQLIniPath := IncludeTrailingPathDelimiter(FMySQLDir) + SelectedMySQLVersion + '\my.ini';
  if IsVersionGreaterOrEqual(ExtractVersionNumber(SelectedMySQLVersion), '8.0.27') then
    MySQLDataDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'data\' + MySQLVersionShort
  else
    MySQLDataDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'data\' + MySQLVersionShort + '_old';

  CreateMySQLIniIfMissing(MySQLIniPath, MySQLDataDir);
  PIDFile := TPath.Combine(MySQLDataDir, 'mysqld.pid');
  LogFile := TPath.Combine(MySQLDataDir, 'mysqld.log');
  IbdataFile := TPath.Combine(MySQLDataDir, 'ibdata1');

  if not DirectoryExists(MySQLDataDir) then
    ForceDirectories(MySQLDataDir);

  Initialized := FileExists(IbdataFile);
  if not Initialized then
  begin
    //WriteToLogMessage('Đang khởi tạo MySQL data directory...');

    Params := Format('--initialize-insecure --basedir="%s" --datadir="%s"', [
      IncludeTrailingPathDelimiter(FMySQLDir) + SelectedMySQLVersion,
      MySQLDataDir
    ]);

    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(SEInfo);
    SEInfo.Wnd := Application.Handle;
    SEInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    SEInfo.lpVerb := 'open';
    SEInfo.lpFile := PChar(MySQLExePath);
    SEInfo.lpParameters := PChar(Params);
    SEInfo.nShow := SW_HIDE;

    if ShellExecuteEx(@SEInfo) then
    begin
      WaitForSingleObject(SEInfo.hProcess, INFINITE);
      CloseHandle(SEInfo.hProcess);
      //WriteToLogMessage('MySQL đã được khởi tạo thành công.');
    end
    else
    begin
      ShowMessage('Lỗi khởi tạo MySQL bằng --initialize-insecure');
      //WriteToLogMessage('Lỗi khi chạy mysqld --initialize-insecure');
      Exit;
    end;
  end;

  if TFile.Exists(PIDFile) then TFile.Delete(PIDFile);
  if TFile.Exists(LogFile) then TFile.Delete(LogFile);

  Params := Format('--log-error="%s" --pid-file="%s"', [
    LogFile,
    PIDFile
  ]);

  FillChar(SEInfo, SizeOf(SEInfo), 0);
  SEInfo.cbSize := SizeOf(SEInfo);
  SEInfo.Wnd := Application.Handle;
  SEInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  SEInfo.lpVerb := 'open';
  SEInfo.lpFile := PChar(MySQLExePath);
  SEInfo.lpParameters := PChar(Params);
  SEInfo.nShow := SW_HIDE;

  if ShellExecuteEx(@SEInfo) then
  begin
    WaitForInputIdle(SEInfo.hProcess, 5000);
    GetExitCodeProcess(SEInfo.hProcess, ExitCode);
    CloseHandle(SEInfo.hProcess);

    if TFile.Exists(PIDFile) then
    begin
      PID := StrToIntDef(Trim(TFile.ReadAllText(PIDFile)), 0);
      if PID > 0 then
        WriteToLogMessage('MySQL đã khởi động, PID: ' + IntToStr(PID))
      else
        WriteToLogMessage('Không đọc được PID từ file: ' + PIDFile);
    end;
    UpdateMySQLChange(True);
    //WriteToLogMessage('MySQL đã được khởi động: ' + MySQLExePath);
  end
  else
  begin
    ShowMessage('Lỗi khi khởi động MySQL.');
    //WriteToLogMessage('ShellExecuteEx thất bại cho MySQL');
  end;
end;

procedure TForm1.ImgSettingsClick(Sender: TObject);
var
  FrmPhu: TForm2;
begin
  FrmPhu := TForm2.Create(Self);
  try
    FrmPhu.Left := Self.Left + (Self.Width - FrmPhu.Width) div 2;
    FrmPhu.Top := Self.Top + (Self.Height - FrmPhu.Height) div 2;

    // Gán sự kiện để reload port khi có thay đổi
    FrmPhu.OnPortChanged := OnPortChanged;
    FrmPhu.ShowModal;
  finally
    FrmPhu.Free;
  end;
end;


procedure TForm1.ImgSettingsMouseEnter(Sender: TObject);
begin
  SetImageFromIndex(ImgSettings, VirtualImageList2, 1);
end;

procedure TForm1.ImgSettingsMouseLeave(Sender: TObject);
begin
  SetImageFromIndex(ImgSettings, VirtualImageList2, 0);
end;

procedure TForm1.InitializeTrayIcon;
begin
  Form1.PopupMenu := PopupMenu1;
  TrayIcon1.OnClick := TTrayIconHandler.TrayIconClick;
  TrayIcon1.OnMouseDown := TTrayIconHandler.TrayIconMouseDown;
end;

procedure TForm1.UpdateWindowTitle;
var
  NewCaption: string;
begin
  if SelectedPhpVersion <> '' then
  begin
    if Pos('NTS', UpperCase(SelectedPhpVersion)) > 0 then
    begin
      NewCaption := AppBaseTitle + ' | ' + SelectedPhpVersion + ' (NTS)';
    end
    else
    begin
      NewCaption := AppBaseTitle + ' | ' + SelectedPhpVersion + ' (TS)';
    end;

    Form1.Caption := NewCaption;
    SetWindowText(Application.Handle, PChar('FireX'));
  end
  else
  begin
    Form1.Caption := AppBaseTitle;
    SetWindowText(Application.Handle, PChar('FireX'));
  end;
end;


procedure TForm1.UncheckVersionMenu(VersionMenu: TMenuItem);
var
  I: Integer;
begin
  for I := 0 to VersionMenu.Count - 1 do
    VersionMenu.Items[I].Checked := False;
end;

procedure TForm1.SelectVersion(Menu: TMenuItem; const NewVersion: string; var SelectedVersion: string; const LabelToUpdate: TLabel = nil);
var
  I: Integer;
begin
  if (NewVersion = '') or (NewVersion = SelectedVersion) then
    Exit;

  UncheckVersionMenu(Menu);

  for I := 0 to Menu.Count - 1 do
    if SameText(Menu.Items[I].Hint, NewVersion) then
    begin
      Menu.Items[I].Checked := True;
      Break;
    end;

  SelectedVersion := NewVersion;

  if Assigned(LabelToUpdate) then
    LabelToUpdate.Caption := Menu.Caption + ' ' + ExtractVersionNumber(NewVersion);

  Config.UpdateConfig(SelectedPhpVersion, SelectedApacheVersion, SelectedMySQLVersion);
end;

procedure TForm1.ApacheVersionMenuClick(Sender: TObject);
var
  ApacheMenuItem: TMenuItem;
  ApachePath, NewApacheVersion, ApachePort: string;
  WasRunning: Boolean;
begin
  if not (Sender is TMenuItem) then Exit;
  ApacheMenuItem := Sender as TMenuItem;
  NewApacheVersion := ApacheMenuItem.Hint;

  if (NewApacheVersion = '') or
     ((NewApacheVersion = SelectedApacheVersion) and
      DirectoryExists(IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion)) then
    Exit;

  SelectVersion(Version1, NewApacheVersion, SelectedApacheVersion, lbApache);

  // Cập nhật lại port
  ApachePort := TPortUtils.GetApachePort(FApacheDir, SelectedApacheVersion);
  lbApachePort.Caption := ApachePort;

  WasRunning := ServicesRunning('httpd.exe');
  if WasRunning then StopServices;

  ServicesConfig.UpdateApacheConfigs(
    IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion,
    SelectedApacheVersion,
    SelectedPhpVersion,
    SelectedMySQLVersion
  );

  if WasRunning then
  begin
    if not IsApachePhpCompatible(SelectedApacheVersion, SelectedPhpVersion, True) then
    begin
      ShowMessage('Apache và PHP không tương thích, Apache sẽ không được khởi động lại.');
      Exit;
    end;

    ApachePath := IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\bin\httpd.exe';
    if FileExists(ApachePath) then
    begin
      StartApache(ApachePath);
      MessageDlgPos('Apache đã được khởi động lại với phiên bản: ' + SelectedApacheVersion,
                    mtInformation, [mbOK], 0, Left + 150, Top + 150);
    end
    else
      ShowMessage('Không tìm thấy Apache: ' + ApachePath);
  end;
end;

procedure TForm1.PhpVersionMenuClick(Sender: TObject);
var
  PhpMenuItem: TMenuItem;
  ApachePath, NewPhpVersion: string;
  WasRunning: Boolean;
begin
  if not (Sender is TMenuItem) then Exit;
  PhpMenuItem := Sender as TMenuItem;
  NewPhpVersion := PhpMenuItem.Hint;

  if (NewPhpVersion = '') or (NewPhpVersion = SelectedPhpVersion) then Exit;

  SelectVersion(Version2, NewPhpVersion, SelectedPhpVersion);
  UpdateWindowTitle;

  WasRunning := ServicesRunning('httpd.exe');
  if WasRunning then StopServices;

  ServicesConfig.UpdateApacheConfigs(
    IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion,
    SelectedApacheVersion,
    SelectedPhpVersion,
    SelectedMySQLVersion
  );

  if WasRunning then
  begin
    // Kiểm tra tương thích trước khi Start lại Apache
    if not IsApachePhpCompatible(SelectedApacheVersion, SelectedPhpVersion, True) then
    begin
      ShowMessage('Phiên bản PHP không tương thích, Apache sẽ không được khởi động lại.');
      Exit;
    end;

    ApachePath := IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\bin\httpd.exe';
    if FileExists(ApachePath) then
    begin
      StartApache(ApachePath);
      MessageDlgPos('Apache đã được khởi động lại với PHP: ' + SelectedPhpVersion,
                    mtInformation, [mbOK], 0, Left + 150, Top + 150);
    end
    else
      ShowMessage('Không tìm thấy Apache: ' + ApachePath);
  end;
  // Cập nhật Extensions
  UpdateExtensionsMenu;
end;

procedure TForm1.MySQLVersionMenuClick(Sender: TObject);
var
  MySQLMenuItem: TMenuItem;
  NewMySQLVersion, MySQLPort: string;
begin
  if not (Sender is TMenuItem) then Exit;
  MySQLMenuItem := Sender as TMenuItem;
  NewMySQLVersion := MySQLMenuItem.Hint;

  if (NewMySQLVersion = '') or (NewMySQLVersion = SelectedMySQLVersion) then Exit;

  SelectVersion(Version3, NewMySQLVersion, SelectedMySQLVersion, lbMySQL);

  // Cập nhật lại port
  MySQLPort := TPortUtils.GetMySQLPort(FMySQLDir, SelectedMySQLVersion);
  lbMySQLPort.Caption := MySQLPort;
end;

procedure TForm1.PopQuitClick(Sender: TObject);
begin
  if ServicesRunning(['httpd.exe', 'mysqld.exe']) then
    StopServices;
  Application.Terminate;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, SC_MOVE or HTCAPTION, 0);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Mutex <> 0 then
    CloseHandle(Mutex);

  if (FApacheChangeHandle <> 0) and (FApacheChangeHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(FApacheChangeHandle);

  if (FPhpChangeHandle <> 0) and (FPhpChangeHandle <> INVALID_HANDLE_VALUE) then
    CloseHandle(FPhpChangeHandle);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  Form1.Visible := False;
  TrayIcon1.Visible := True;
end;

procedure TForm1.OnPortChanged;
var
  ApachePort, MySQLPort: string;
begin
  ApachePort := TPortUtils.GetApachePort(FApacheDir, SelectedApacheVersion);
  MySQLPort := TPortUtils.GetMySQLPort(FMySQLDir, SelectedMySQLVersion);
  lbApachePort.Caption := ApachePort;
  lbMySQLPort.Caption := MySQLPort;
end;


end.
