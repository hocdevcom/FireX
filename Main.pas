unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.StrUtils, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  TlHelp32, ShellAPI, System.IOUtils, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  Vcl.ButtonGroup, System.ImageList, Vcl.ImgList, System.UITypes, Vcl.Buttons,
  WriteToLog, ApacheConfig, TrayIconHandler,
  SelectVersion, StopApache, AppConfig, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  Vcl.ImageCollection;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    N11, bb1, cc1, N111, N211, N221, Root1, N1, Laragonini1, ools1, N2: TMenuItem;
    Apache1: TMenuItem;
    Version1: TMenuItem;
    TrayIcon1: TTrayIcon;
    ut1: TMenuItem;
    PHP1: TMenuItem;
    Version2: TMenuItem;
    BtnStart: TBitBtn;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    BtnWeb: TBitBtn;
    BtnDB: TBitBtn;
    BtnTer: TBitBtn;
    BtnRoot: TBitBtn;
    MySQL1: TMenuItem;
    Version3: TMenuItem;
    procedure btRootClick(Sender: TObject);
    procedure btWebClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ApacheVersionMenuClick(Sender: TObject);
    procedure PhpVersionMenuClick(Sender: TObject);
    procedure MySQLVersionMenuClick(Sender: TObject);
    procedure ut1Click(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
  private
    { Private declarations }
    lbApache, lbApacheStatus: TLabel;
    lbMySQL, lbMySQLStatus: TLabel;
    FApacheChangeHandle: THandle;
    FPhpChangeHandle: THandle;
    PIDFilePath, FApacheDir, FPHPDir, FMySQLDir: string;
    procedure WatchVersionDirs;
    procedure ProcessDirectoryChange;
    procedure UncheckVersionMenu(VersionMenu: TMenuItem);
    procedure SelectVersion(Menu: TMenuItem; const NewVersion: string; var SelectedVersion: string; const LabelToUpdate: TLabel = nil);
    procedure StartApache(const ApachePath: string);
    procedure StopApache;
    procedure UpdateUIAfterApacheChange(IsRunning: Boolean);
    procedure CheckVCRedist;
    procedure InitializeTrayIcon;
    procedure UpdateWindowTitle;
    procedure CreateStatusLabel(const ACaption: string; ALeft, ATop: Integer; out ALabel, AStatus: TLabel);
    procedure CheckSelectedVersionInMenu(Menu: TMenuItem; const Version: string);
    function GetBinPath(const SubDir: string): string;
  public
    { Public declarations }
    SelectedApacheVersion: string;
    SelectedPhpVersion: string;
    SelectedMySQLVersion: string;
  end;

var
  Form1: TForm1;
  Mutex: THandle;
const
  MutexName = '{7E215C93-8F62-442F-89F2-BC1E9ECA6297}';
  AppBaseTitle = 'LaraX 1.0.0';
  SYMBOL_STOP = '■';
  SYMBOL_RUN = '▶';

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    ShowMessage('Ứng dụng đã đang chạy!');
    Application.Terminate;
    Exit;
  end;

  CreateStatusLabel('Apache:', 140, 57, lbApache, lbApacheStatus);
  CreateStatusLabel('MySQL:', 140, 80, lbMySQL, lbMySQLStatus);

  InitializeTrayIcon;
  CheckVCRedist;

  FApacheDir := GetBinPath('apache');
  FPHPDir := GetBinPath('php');
  FMySQLDir := GetBinPath('mysql');

  if not DirectoryExists(FApacheDir) then
  begin
    ShowMessage('Thư mục bin\apache không tồn tại!');
    Exit;
  end;

  // Cập nhật menu trước
  UpdateApacheVersionMenu(FApacheDir);
  UpdatePhpVersionMenu(FPHPDir);
  UpdateMySQLVersionMenu(FMySQLDir);

  // Kiểm tra và chọn phiên bản từ config
  SelectedApacheVersion := '';
  SelectedPhpVersion := '';
  SelectedMySQLVersion := '';

  // Load cấu hình từ file INI và kiểm tra tồn tại
  if Config.IsVersionValid(Config.ApacheVersion, FApacheDir) then
    SelectedApacheVersion := Config.ApacheVersion;

  if Config.IsVersionValid(Config.PhpVersion, FPHPDir) then
    SelectedPhpVersion := Config.PhpVersion;

  if Config.IsVersionValid(Config.MySQLVersion, FMySQLDir) then
    SelectedMySQLVersion := Config.MySQLVersion;

  // Hiển thị phiên bản đã chọn (nếu có)
  if SelectedApacheVersion <> '' then
  begin
    lbApache.Caption := 'Apache: ' + SelectedApacheVersion;
    CheckSelectedVersionInMenu(Version1, SelectedApacheVersion);
  end;

  if SelectedMySQLVersion <> '' then
  begin
    lbMySQL.Caption := 'MySQL: ' + SelectedMySQLVersion;
    CheckSelectedVersionInMenu(Version3, SelectedMySQLVersion);
  end;

  // Chỉ cập nhật title nếu có PHP version hợp lệ
  if SelectedPhpVersion <> '' then
  begin
    CheckSelectedVersionInMenu(Version2, SelectedPhpVersion);
    UpdateWindowTitle;
  end;

  // Bắt đầu theo dõi thư mục
  TThread.CreateAnonymousThread(WatchVersionDirs).Start;
  UpdateUIAfterApacheChange(IsApacheRunning);
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

procedure TForm1.BtnStartClick(Sender: TObject);
var
  ApachePath: string;
begin
  if SelectedApacheVersion = '' then
  begin
    ShowMessage('Chưa chọn phiên bản Apache!');
    Exit;
  end;

  ApachePath := IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion + '\bin\httpd.exe';

  if not FileExists(ApachePath) then
  begin
    ShowMessage('Không tìm thấy Apache!: ' + ApachePath);
    WriteToLogMessage('Không tìm thấy Apache! Đường dẫn: ' + ApachePath);
    Exit;
  end;

  if IsApacheRunning then
    StopApache
  else
  begin
    ApacheConfig.UpdateApacheConfigs(IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion, SelectedApacheVersion, SelectedPhpVersion);
    StartApache(ApachePath);
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
begin
  ShellExecute(0, 'open', 'http://localhost', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.CreateStatusLabel(const ACaption: string; ALeft, ATop: Integer; out ALabel, AStatus: TLabel);
begin
  ALabel := TLabel.Create(Self);
  ALabel.Parent := Self;
  ALabel.Left := ALeft;
  ALabel.Top := ATop;
  ALabel.Caption := ACaption;
  ALabel.Font.Style := [];     //[fsBold];
  ALabel.AutoSize := True;

  AStatus := TLabel.Create(Self);
  AStatus.Parent := Self;
  AStatus.Left := ALabel.Left + 250;
  AStatus.Top := ATop;
  AStatus.Caption := SYMBOL_STOP;
  AStatus.Font.Color := clRed;
  AStatus.Font.Style := [];    //[fsBold];
  AStatus.AutoSize := True;
end;

procedure TForm1.UpdateUIAfterApacheChange(IsRunning: Boolean);
begin
  if IsRunning then
  begin
    BtnStart.Caption := 'Stop';
    BtnStart.ImageIndex := 1;
    lbApacheStatus.Caption := SYMBOL_RUN;
    lbApacheStatus.Font.Color := clGreen;
    lbMySQLStatus.Caption := SYMBOL_RUN;  // Giả sử MySQL chạy cùng Apache
    lbMySQLStatus.Font.Color := clGreen;

  end
  else
  begin
    BtnStart.Caption := 'Start';
    BtnStart.ImageIndex := 0;
    lbApacheStatus.Caption := SYMBOL_STOP;
    lbApacheStatus.Font.Color := clRed;
    lbMySQLStatus.Caption := SYMBOL_STOP;
    lbMySQLStatus.Font.Color := clRed;

  end;
end;

procedure TForm1.StartApache(const ApachePath: string);
var
  RetryCount: Integer;
  PidFile, ErrorLog: string;
begin
  // Cập nhật đường dẫn Apache với SelectedApacheVersion
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
      WriteToLogMessage('ShellExecute exception: ' + E.Message);
      Exit;
    end;
  end;

  RetryCount := 0;
  while (not IsApacheRunning) and (RetryCount < 10) do
  begin
    Sleep(200);
    Inc(RetryCount);
  end;

  if IsApacheRunning then
  begin
    UpdateUIAfterApacheChange(True);
    WriteToLogMessage('Apache started: ' + ApachePath);
  end
  else
  begin
    ShowMessage('Không thể khởi động Apache!');
    WriteToLogMessage('Apache failed to start: ' + ApachePath);
  end;
end;

procedure TForm1.StopApache;
begin
  if FileExists(PIDFilePath) then
    KillApacheByPID(FApacheDir)
  else
    KillApache;

  if not IsApacheRunning then
  begin
    UpdateUIAfterApacheChange(False);
    WriteToLogMessage('Apache stopped.');
  end
  else
  begin
    ShowMessage('Không thể dừng Apache!');
    WriteToLogMessage('Failed to stop Apache.');
  end;
end;

procedure TForm1.CheckVCRedist;
var
  SysDir: array[0..MAX_PATH] of Char;
begin
  GetSystemDirectory(SysDir, MAX_PATH);
  if not FileExists(IncludeTrailingPathDelimiter(SysDir) + 'vcruntime140.dll') then
  begin
    WriteToLogMessage('Visual C++ Redistributable not found');
    ShowMessage('Visual C++ Redistributable is missing. Please install it from Microsoft''s website.');
    ShellExecute(0, 'open', 'https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist', nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TForm1.InitializeTrayIcon;
begin
  Form1.PopupMenu := PopupMenu1;
  TrayIcon1.OnClick := TTrayIconHandler.TrayIconClick;
  TrayIcon1.OnMouseDown := TTrayIconHandler.TrayIconMouseDown;
end;

procedure TForm1.UpdateWindowTitle;
begin
  if SelectedPhpVersion <> '' then
  begin
    if Pos('NTS', UpperCase(SelectedPhpVersion)) > 0 then
      Form1.Caption := AppBaseTitle + ' | ' + SelectedPhpVersion + ' (NTS)'
    else
      Form1.Caption := AppBaseTitle + ' | ' + SelectedPhpVersion + ' (TS)';
  end
  else
    Form1.Caption := AppBaseTitle;
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

procedure TForm1.UncheckVersionMenu(VersionMenu: TMenuItem);
var
  I: Integer;
begin
  for I := 0 to VersionMenu.Count - 1 do
    (VersionMenu.Items[I] as TMenuItem).Checked := False;
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
    LabelToUpdate.Caption := Menu.Caption + ': ' + NewVersion;

  Config.UpdateConfig(SelectedPhpVersion, SelectedApacheVersion, SelectedMySQLVersion);
end;

procedure TForm1.ApacheVersionMenuClick(Sender: TObject);
var
  ApacheMenuItem: TMenuItem;
  ApachePath, NewApacheVersion: string;
  WasRunning: Boolean;
begin
  if not (Sender is TMenuItem) then Exit;
  ApacheMenuItem := Sender as TMenuItem;

  NewApacheVersion := ApacheMenuItem.Hint;

  // Nếu không chọn gì hoặc đã là phiên bản hiện tại và thư mục còn tồn tại thì thoát
  if (NewApacheVersion = '') or
     ((NewApacheVersion = SelectedApacheVersion) and
      DirectoryExists(IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion)) then
    Exit;

  SelectVersion(Version1, NewApacheVersion, SelectedApacheVersion, lbApache);

  // Nếu đang chạy thì dừng lại
  WasRunning := IsApacheRunning;
  if WasRunning then StopApache;

  // Cập nhật cấu hình apache
  ApacheConfig.UpdateApacheConfigs(
    IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion,
    SelectedApacheVersion,
    SelectedPhpVersion
  );

  // Nếu cần thì khởi động lại Apache
  if WasRunning then
  begin
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

  // Nếu Apache đang chạy thì dừng lại
  WasRunning := IsApacheRunning;
  if WasRunning then StopApache;

  // Cập nhật cấu hình Apache
  ApacheConfig.UpdateApacheConfigs(
    IncludeTrailingPathDelimiter(FApacheDir) + SelectedApacheVersion,
    SelectedApacheVersion,
    SelectedPhpVersion
  );

  // Khởi động lại nếu cần
  if WasRunning then
  begin
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
end;

procedure TForm1.MySQLVersionMenuClick(Sender: TObject);
var
  MySQLMenuItem: TMenuItem;
  NewMySQLVersion: string;
begin
  if not (Sender is TMenuItem) then Exit;
  MySQLMenuItem := Sender as TMenuItem;
  NewMySQLVersion := MySQLMenuItem.Hint;

  if (NewMySQLVersion = '') or (NewMySQLVersion = SelectedMySQLVersion) then Exit;

  SelectVersion(Version3, NewMySQLVersion, SelectedMySQLVersion, lbMySQL);
end;


procedure TForm1.ut1Click(Sender: TObject);
begin
  if IsApacheRunning then
    StopApache;
  Application.Terminate;
end;

procedure TForm1.WatchVersionDirs;
var
  ChangeHandles: array[0..2] of THandle;
  WaitResult: DWORD;
begin
  // Lấy đường dẫn thư mục
  FApacheDir := GetBinPath('apache');
  FPHPDir    := GetBinPath('php');
  FMySQLDir  := GetBinPath('mysql');

  // Tạo các handle theo dõi thư mục
  ChangeHandles[0] := FindFirstChangeNotification(PChar(FApacheDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);
  ChangeHandles[1] := FindFirstChangeNotification(PChar(FPHPDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);
  ChangeHandles[2] := FindFirstChangeNotification(PChar(FMySQLDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);

  if (ChangeHandles[0] = INVALID_HANDLE_VALUE) or
     (ChangeHandles[1] = INVALID_HANDLE_VALUE) or
     (ChangeHandles[2] = INVALID_HANDLE_VALUE) then
  begin
    // Nếu có handle nào lỗi thì thoát
    Exit;
  end;

  try
    while True do
    begin
      WaitResult := WaitForMultipleObjects(3, @ChangeHandles[0], False, INFINITE);

      case WaitResult of
        WAIT_OBJECT_0:
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;
                UpdateApacheVersionMenu(FApacheDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[0]) then Break;
          end;

        WAIT_OBJECT_0 + 1:
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;
                UpdatePhpVersionMenu(FPHPDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[1]) then Break;
          end;

        WAIT_OBJECT_0 + 2:
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;
                UpdateMySQLVersionMenu(FMySQLDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[2]) then Break;
          end;

      else
        // Có lỗi xảy ra
        Break;
      end;
    end;
  finally
    CloseHandle(ChangeHandles[0]);
    CloseHandle(ChangeHandles[1]);
    CloseHandle(ChangeHandles[2]);
  end;
end;

procedure TForm1.ProcessDirectoryChange;
begin
  UpdateApacheVersionMenu(FApacheDir);
  UpdatePhpVersionMenu(FPHPDir);
  UpdateMySQLVersionMenu(FMySQLDir);
end;

end.
