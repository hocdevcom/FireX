unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  TlHelp32, ShellAPI, System.IOUtils, Vcl.ExtCtrls, Vcl.Imaging.pngimage, WriteToLog, ApacheConfigUtils,
  Vcl.ButtonGroup, System.ImageList, Vcl.ImgList, System.UITypes, Vcl.Buttons, TrayIconHandler, ApacheVersionUtils;

type
  TForm1 = class(TForm)
    btWeb: TButton;
    btDatabase: TButton;
    btTerminal: TButton;
    btRoot: TButton;
    PopupMenu1: TPopupMenu;
    N11: TMenuItem;
    bb1: TMenuItem;
    cc1: TMenuItem;
    N111: TMenuItem;
    N211: TMenuItem;
    N221: TMenuItem;
    Root1: TMenuItem;
    N1: TMenuItem;
    Laragonini1: TMenuItem;
    ools1: TMenuItem;
    N2: TMenuItem;
    Apache1: TMenuItem;
    Version1: TMenuItem;
    ImageList1: TImageList;
    TrayIcon1: TTrayIcon;
    ut1: TMenuItem;
    btStarAll: TBitBtn;
    lbApacheVer: TLabel;
    Label1: TLabel;
    LbStatus: TLabel;
    PHP1: TMenuItem;
    Version2: TMenuItem;
    procedure btRootClick(Sender: TObject);
    procedure btWebClick(Sender: TObject);
    procedure btStarAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VersionMenuClick(Sender: TObject);
    procedure PhpVersionMenuClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ut1Click(Sender: TObject);
  private
    { Private declarations }
    FChangeHandleApache: THandle;
    FChangeHandlePhp: THandle;
    procedure WatchVersionDirs;
    procedure ProcessDirectoryChange;
    function IsApacheRunning: Boolean;
    procedure KillApache;
    procedure UncheckAllVersionMenu;
    procedure UncheckAllPhpVersionMenu;
  public
    { Public declarations }
    SelectedVersion: string;
    SelectedPhpVersion: string;
  end;

var
  Form1: TForm1;
  Mutex: THandle;
const
  MutexName = '{7E215C93-8F62-442F-89F2-BC1E9ECA6297}';
  AppBaseTitle = 'Lara BETA 1.0.0';

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  ApachePath, Params: string;
  SysDir: array[0..MAX_PATH] of Char;
  UserResponse: Integer;
begin
  // Kiểm tra sự tồn tại của file vcruntime140.dll
  GetSystemDirectory(SysDir, MAX_PATH);
  if not FileExists(IncludeTrailingPathDelimiter(SysDir) + 'vcruntime140.dll') then
  begin
    // Hiển thị hộp thoại với 2 nút: Truy cập hoặc Không
    UserResponse := MessageDlg('Thiếu file vcruntime140.dll!' + sLineBreak + 'Hãy cài đặt Visual C++ Redistributable.' + sLineBreak +
                               'Bạn muốn truy cập trang cài đặt?', mtWarning, [mbYes, mbNo], 0);

    // Kiểm tra nút người dùng chọn
    if UserResponse = mrYes then
    begin
      ShellExecute(0, 'open', 'https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist', nil, nil, SW_SHOWNORMAL);
      Exit;
    end
    else
    begin
      // Nếu người dùng chọn "Không", không tiếp tục với quá trình khởi động Apache
      Exit;
    end;
  end;

  if IsApacheRunning then
  begin
    KillApache;

    if not IsApacheRunning then
    begin
      btStarAll.Caption := 'Start';
      LbStatus.Caption := 'Fail';
      LbStatus.Font.Color := clRed;
      btStarAll.Images := ImageList1;
      btStarAll.ImageIndex := 1;
      WriteToLogMessage('Apache stopped.');
    end
    else
    begin
      ShowMessage('Không thể dừng Apache!');
      WriteToLogMessage('Failed to stop Apache.');
    end;
  end
  else
  begin
    if SelectedVersion = '' then
    begin
      ShowMessage('Chưa chọn phiên bản Apache!');
      Exit;
    end;

    ApachePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                  'bin\apache\' + SelectedVersion + '\bin\httpd.exe';

    if not FileExists(ApachePath) then
    begin
      ShowMessage('Không tìm thấy Apache!: ' + ApachePath);
      WriteToLogMessage('Không tìm thấy Apache! Đường dẫn: ' + ApachePath);
      Exit;
    end;

    ApacheConfigUtils.UpdateApacheConfigs(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                    'bin\apache\' + SelectedVersion, SelectedVersion, SelectedPhpVersion);

    Params := '-d "' + IncludeTrailingPathDelimiter(ExtractFilePath(ApachePath)) + '.."';

    if ShellExecute(0, 'open', PChar(ApachePath), PChar(Params), nil, SW_HIDE) <= 32 then
    begin
      ShowMessage('Không thể khởi động Apache!');
      WriteToLogMessage('Apache failed to start: ' + ApachePath);
      Exit;
    end;

    var RetryCount := 0;
    while (not IsApacheRunning) and (RetryCount < 10) do
    begin
      Sleep(200);
      Inc(RetryCount);
    end;

    if IsApacheRunning then
    begin
      btStarAll.Caption := 'Stop';
      LbStatus.Caption := 'Success';
      LbStatus.Font.Color := clBlue;
      btStarAll.Images := ImageList1;
      btStarAll.ImageIndex := 0;
      WriteToLogMessage('Apache started: ' + ApachePath);
    end
    else
    begin
      ShowMessage('Không thể khởi động Apache!');
      WriteToLogMessage('Apache failed to start: ' + ApachePath);
    end;
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

procedure TForm1.KillApache;
var
  RetryCount: Integer;
begin
  if ShellExecute(0, 'open', 'taskkill.exe', '/IM httpd.exe /F', nil, SW_HIDE) <= 32 then
  begin
    WriteToLogMessage('Lỗi khi gửi lệnh taskkill.');
    Exit;
  end;

  RetryCount := 0;
  while IsApacheRunning and (RetryCount < 10) do
  begin
    Sleep(300);
    Inc(RetryCount);
  end;
end;

procedure TForm1.btStarAllClick(Sender: TObject);
var
  ApachePath, Params: string;
begin
  if IsApacheRunning then
  begin
    KillApache;

    if not IsApacheRunning then
    begin
      btStarAll.Caption := 'Start';
      LbStatus.Caption := 'Fail';
      LbStatus.Font.Color := clRed;
      WriteToLogMessage('Apache stopped.');
    end
    else
    begin
      ShowMessage('Không thể dừng Apache!');
      WriteToLogMessage('Failed to stop Apache.');
    end;
  end
  else
  begin
    if SelectedVersion = '' then
    begin
      ShowMessage('Chưa chọn phiên bản Apache!');
      Exit;
    end;

    ApachePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                  'bin\apache\' + SelectedVersion + '\bin\httpd.exe';

    if not FileExists(ApachePath) then
    begin
      ShowMessage('Không tìm thấy Apache!: ' + ApachePath);
      WriteToLogMessage('Không tìm thấy Apache! Đường dẫn: ' + ApachePath);
      Exit;
    end;

    ApacheConfigUtils.UpdateApacheConfigs(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                    'bin\apache\' + SelectedVersion, SelectedVersion, SelectedPhpVersion);

    Params := '-d "' + IncludeTrailingPathDelimiter(ExtractFilePath(ApachePath)) + '.."';

    if ShellExecute(0, 'open', PChar(ApachePath), PChar(Params), nil, SW_HIDE) <= 32 then
    begin
      ShowMessage('Không thể khởi động Apache!');
      WriteToLogMessage('Apache failed to start: ' + ApachePath);
      Exit;
    end;

    var RetryCount := 0;
    while (not IsApacheRunning) and (RetryCount < 10) do
    begin
      Sleep(200);
      Inc(RetryCount);
    end;

    if IsApacheRunning then
    begin
      btStarAll.Caption := 'Stop';
      LbStatus.Caption := 'Success';
      LbStatus.Font.Color := clBlue;
      WriteToLogMessage('Apache started: ' + ApachePath);
    end
    else
    begin
      ShowMessage('Không thể khởi động Apache!');
      WriteToLogMessage('Apache failed to start: ' + ApachePath);
    end;
  end;
end;

procedure TForm1.btWebClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://localhost', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ApacheBinDir: string;
  PHPBinDir: string;
begin
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if (Mutex = 0) then
  begin
    ShowMessage('Không thể tạo Mutex!');
    Exit;
  end;

  begin
    Form1.PopupMenu := PopupMenu1;
    TrayIcon1.OnClick := TTrayIconHandler.TrayIconClick;
    TrayIcon1.OnMouseDown := TTrayIconHandler.TrayIconMouseDown;
  end;

  if (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    ShowMessage('Ứng dụng đã đang chạy!');
    Application.Terminate;
    Exit;
  end;

  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';
  PHPBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\php';

  if DirectoryExists(ApacheBinDir) then
  begin
    UpdateVersionMenu(ApacheBinDir);
    UpdatePhpVersionMenu(PHPBinDir);

    // { thêm mới }: theo dõi thư mục thay đổi trong thread riêng
    TThread.CreateAnonymousThread(
      procedure
      begin
        WatchVersionDirs;
      end).Start;
  end
  else
    ShowMessage('Thư mục bin\apache không tồn tại!');

  // Kiểm tra trạng thái của Apache và cập nhật nút Start
  if IsApacheRunning then
  begin
    btStarAll.Caption := 'Stop'; // Nếu Apache đang chạy, đổi nút thành Stop
    btStarAll.Images := ImageList1;
    btStarAll.ImageIndex := 0;
    LbStatus.Caption := 'Success';
    LbStatus.Font.Color := clBlue;
  end
  else
  begin
    btStarAll.Caption := 'Start'; // Nếu Apache không chạy, giữ nút là Start
    btStarAll.Images := ImageList1;
    btStarAll.ImageIndex := 1;
    LbStatus.Caption := 'Fail';
    LbStatus.Font.Color := clRed;
  end;

  // Đặt giá trị mặc định cho lbApacheVer dựa trên SelectedVersion
  if SelectedVersion <> '' then
  begin
    lbApacheVer.Caption := 'Apache: ' + SelectedVersion;
    Form1.Caption := AppBaseTitle + ' | ' + SelectedPhpVersion;
  end
  else
  begin
    lbApacheVer.Caption := 'Chưa chọn phiên bản PHP';
    Form1.Caption := AppBaseTitle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Mutex <> 0 then
    CloseHandle(Mutex);

  if FChangeHandleApache <> 0 then CloseHandle(FChangeHandleApache);
  if FChangeHandlePhp <> 0 then CloseHandle(FChangeHandlePhp);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Ngăn không cho đóng form
  CanClose := False;

  // Ẩn form khỏi màn hình và Taskbar
  Form1.Visible := False;  // Ẩn form hoàn toàn
  TrayIcon1.Visible := True;  // Hiển thị tray icon
end;

procedure TForm1.UncheckAllVersionMenu;
var
  I: Integer;
begin
  for I := 0 to Version1.Count - 1 do
    (Version1.Items[I] as TMenuItem).Checked := False;
end;

procedure TForm1.UncheckAllPhpVersionMenu;
var
  I: Integer;
begin
  for I := 0 to Version2.Count - 1 do
    (Version2.Items[I] as TMenuItem).Checked := False;
end;

procedure TForm1.VersionMenuClick(Sender: TObject);
var
  VersionMenuItem: TMenuItem;
  ApachePath, Params: string;
begin
  UncheckAllVersionMenu;

  VersionMenuItem := Sender as TMenuItem;
  VersionMenuItem.Checked := True;

  if VersionMenuItem.Hint <> '' then
  begin
    // Cập nhật lbPhpVer thành tên phiên bản PHP
    lbApacheVer.Caption := 'Apache: ' + VersionMenuItem.Hint;

    // Kiểm tra nếu người dùng đã chọn lại phiên bản đang sử dụng
    if VersionMenuItem.Hint = SelectedVersion then
    begin
      Exit; // Nếu chọn lại phiên bản đang sử dụng thì không làm gì cả
    end;

    SelectedVersion := VersionMenuItem.Hint;

    // Nếu Apache đang chạy, dừng và khởi động lại với phiên bản mới
    if IsApacheRunning then
    begin
      KillApache;

      if not IsApacheRunning then
      begin
        WriteToLogMessage('Apache stopped due to version change.');
        ShowMessage('Apache đã dừng!');
      end
      else
      begin
        ShowMessage('Không thể dừng Apache!');
        WriteToLogMessage('Failed to stop Apache during version change.');
        Exit;
      end;

      UpdateApacheConfigs(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
					'bin\apache\' + SelectedVersion, SelectedVersion, SelectedPhpVersion);

      ApachePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
                    'bin\apache\' + SelectedVersion + '\bin\httpd.exe';

      if not FileExists(ApachePath) then
      begin
        ShowMessage('Không tìm thấy Apache!: ' + ApachePath);
        WriteToLogMessage('Không tìm thấy Apache! Đường dẫn: ' + ApachePath);
        Exit;
      end;

      Params := '-d "' + IncludeTrailingPathDelimiter(ExtractFilePath(ApachePath)) + '.."';

      if ShellExecute(0, 'open', PChar(ApachePath), PChar(Params), nil, SW_HIDE) <= 32 then
      begin
        ShowMessage('Không thể khởi động Apache!');
        WriteToLogMessage('Apache failed to start: ' + ApachePath);
        Exit;
      end;

      var RetryCount := 0;
      while (not IsApacheRunning) and (RetryCount < 10) do
      begin
        Sleep(200);
        Inc(RetryCount);
      end;

      if IsApacheRunning then
      begin
        ShowMessage('Apache đã được khởi động lại!');
        WriteToLogMessage('Apache started: ' + ApachePath);
      end
      else
      begin
        ShowMessage('Không thể khởi động Apache!');
        WriteToLogMessage('Apache failed to start: ' + ApachePath);
      end;
    end
    else
    begin
      WriteToLogMessage('Apache is not running. Version changed to: ' + SelectedVersion);
    end;
  end
  else
    ShowMessage('Không có thông tin phiên bản!');
end;

procedure TForm1.PhpVersionMenuClick(Sender: TObject);
begin
  UncheckAllPhpVersionMenu;

  (Sender as TMenuItem).Checked := True;
  SelectedPhpVersion := (Sender as TMenuItem).Hint;
  Form1.Caption := AppBaseTitle + ' | ' + SelectedPhpVersion;

  //lbApacheVer.Caption := 'PHP: ' + SelectedPhpVersion;
  WriteToLogMessage('Selected PHP version: ' + SelectedPhpVersion);
end;

function TForm1.IsApacheRunning: Boolean;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := False;
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then Exit;

  ProcEntry.dwSize := SizeOf(TProcessEntry32);
  if Process32First(Snapshot, ProcEntry) then
  begin
    repeat
      if SameText(ExtractFileName(ProcEntry.szExeFile), 'httpd.exe') then
      begin
        Result := True;
        Break;
      end;
    until not Process32Next(Snapshot, ProcEntry);
  end;
  CloseHandle(Snapshot);
end;

procedure TForm1.ut1Click(Sender: TObject);
begin
  Application.Terminate;
end;

// { thêm mới }: theo dõi thư mục bin\apache
procedure TForm1.WatchVersionDirs;
var
  ApacheBinDir, PhpBinDir: string;
  ApacheChangeHandle, PhpChangeHandle: THandle;
begin
  // Đường dẫn thư mục Apache và PHP
  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';
  PhpBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\php';

  // Theo dõi thư mục Apache
  ApacheChangeHandle := FindFirstChangeNotification(
    PChar(ApacheBinDir),
    False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME
  );

  if ApacheChangeHandle = INVALID_HANDLE_VALUE then
  begin
    WriteToLogMessage('Không thể theo dõi thư mục bin\apache.');
  end
  else
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          while WaitForSingleObject(ApacheChangeHandle, INFINITE) = WAIT_OBJECT_0 do
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;  // Gọi lại hàm xử lý thay đổi thư mục Apache
                UpdateVersionMenu(ApacheBinDir);  // Cập nhật menu Apache khi có thay đổi
              end
            );
            if not FindNextChangeNotification(ApacheChangeHandle) then Break;
          end;
        finally
          if ApacheChangeHandle <> INVALID_HANDLE_VALUE then
            CloseHandle(ApacheChangeHandle);
        end;
      end
    ).Start;
  end;

  // Theo dõi thư mục PHP
  PhpChangeHandle := FindFirstChangeNotification(
    PChar(PhpBinDir),
    False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME
  );

  if PhpChangeHandle = INVALID_HANDLE_VALUE then
  begin
    WriteToLogMessage('Không thể theo dõi thư mục bin\php.');
  end
  else
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          while WaitForSingleObject(PhpChangeHandle, INFINITE) = WAIT_OBJECT_0 do
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;  // Gọi lại hàm xử lý thay đổi thư mục PHP
                UpdatePhpVersionMenu(PhpBinDir);  // Cập nhật menu PHP khi có thay đổi
              end
            );
            if not FindNextChangeNotification(PhpChangeHandle) then Break;
          end;
        finally
          if PhpChangeHandle <> INVALID_HANDLE_VALUE then
            CloseHandle(PhpChangeHandle);
        end;
      end
    ).Start;
  end;
end;

// { thêm mới }: xử lý khi thư mục thay đổi
procedure TForm1.ProcessDirectoryChange;
var
  ApacheBinDir: string;
begin
  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';
  UpdateVersionMenu(ApacheBinDir);
end;

end.

