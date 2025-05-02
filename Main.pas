unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus,
  TlHelp32, ShellAPI, System.IOUtils, Vcl.ExtCtrls, Vcl.Imaging.pngimage, WriteToLog, ApacheConfigUtils,
  Vcl.ButtonGroup, System.ImageList, Vcl.ImgList, Vcl.Buttons;

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
    GroupBox1: TGroupBox;
    lbApacheVer: TLabel;
    imgApacheStatus: TImage;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    Image1: TImage;
    BitBtn1: TBitBtn;
    btStarAll: TBitBtn;
    BitBtn2: TBitBtn;
    ImageList1: TImageList;
    TrayIcon1: TTrayIcon;
    ut1: TMenuItem;
    procedure btRootClick(Sender: TObject);
    procedure btWebClick(Sender: TObject);
    procedure btStarAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VersionMenuClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIcon1Click(Sender: TObject);
    procedure ut1Click(Sender: TObject);
    procedure TrayIcon1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    SelectedVersion: string;
    FChangeHandle: THandle;
    procedure UpdateVersionMenu;
    procedure WatchApacheDir;
    procedure ProcessDirectoryChange;
    function IsApacheRunning: Boolean;
    procedure KillApache;
    procedure UncheckAllVersionMenu;
    function ExtractApacheVersion(const FullName: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Mutex: THandle;
const
  MutexName = '{7E215C93-8F62-442F-89F2-BC1E9ECA6297}';

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  ApachePath, Params: string;
begin
  if IsApacheRunning then
  begin
    KillApache;

    if not IsApacheRunning then
    begin
      btStarAll.Caption := 'Start';
      imgApacheStatus.Picture.LoadFromFile('red.png');
      imgApacheStatus.Visible := True;
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
                    'bin\apache\' + SelectedVersion, SelectedVersion);

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
      imgApacheStatus.Picture.LoadFromFile('green.png');
      imgApacheStatus.Visible := True;
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
      imgApacheStatus.Picture.LoadFromFile('red.png');
      imgApacheStatus.Visible := True;
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
                    'bin\apache\' + SelectedVersion, SelectedVersion);

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
      imgApacheStatus.Picture.LoadFromFile('green.png');
      imgApacheStatus.Visible := True;
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
begin
  Mutex := CreateMutex(nil, True, PChar(MutexName));
  if (Mutex = 0) then
  begin
    ShowMessage('Không thể tạo Mutex!');
    Exit;
  end;

  begin
    Form1.PopupMenu := PopupMenu1;
  end;

  if (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    ShowMessage('Ứng dụng đã đang chạy!');
    Application.Terminate;
    Exit;
  end;

  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';

  if DirectoryExists(ApacheBinDir) then
  begin
    UpdateVersionMenu; // { thêm mới }: cập nhật menu khi khởi động

    // { thêm mới }: theo dõi thư mục thay đổi trong thread riêng
    TThread.CreateAnonymousThread(
      procedure
      begin
        WatchApacheDir;
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
    imgApacheStatus.Picture.LoadFromFile('green.png');
    imgApacheStatus.Visible := True;
  end
  else
  begin
    btStarAll.Caption := 'Start'; // Nếu Apache không chạy, giữ nút là Start
    btStarAll.Images := ImageList1;
    btStarAll.ImageIndex := 1;
    imgApacheStatus.Picture.LoadFromFile('red.png');
    imgApacheStatus.Visible := True;
  end;

  // Đặt giá trị mặc định cho lbApacheVer dựa trên SelectedVersion
  if SelectedVersion <> '' then
    lbApacheVer.Caption := 'Apache: ' + ExtractApacheVersion(SelectedVersion)
  else
    lbApacheVer.Caption := 'Chưa chọn phiên bản PHP';
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Mutex <> 0 then
    CloseHandle(Mutex);

  // { thêm mới }: hủy theo dõi thư mục
  if FChangeHandle <> 0 then
    CloseHandle(FChangeHandle);
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
    lbApacheVer.Caption := 'Apache: ' + ExtractApacheVersion(VersionMenuItem.Hint);

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
					'bin\apache\' + SelectedVersion, SelectedVersion);

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

// { thêm mới }: cập nhật lại menu phiên bản từ thư mục bin\apache
procedure TForm1.UpdateVersionMenu;
var
  ApacheBinDir: string;
  SearchRec: TSearchRec;
  Versions: TStringList;
  VersionMenuItem: TMenuItem;
begin
  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';
  Versions := TStringList.Create;
  try
    if FindFirst(ApacheBinDir + '\*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory = faDirectory) and
           (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          Versions.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    Versions.Sort;
    Version1.Clear;

    for var S in Versions do
    begin
      VersionMenuItem := TMenuItem.Create(Version1);
      VersionMenuItem.Caption := S;
      VersionMenuItem.Hint := S;
      VersionMenuItem.OnClick := VersionMenuClick;
      Version1.Add(VersionMenuItem);
    end;

    if Version1.Count > 0 then
    begin
      (Version1.Items[0] as TMenuItem).Checked := True;
      SelectedVersion := (Version1.Items[0] as TMenuItem).Hint;
    end;
  finally
    Versions.Free;
  end;
end;

procedure TForm1.ut1Click(Sender: TObject);
begin
  Application.Terminate;
end;

// { thêm mới }: theo dõi thư mục bin\apache
procedure TForm1.WatchApacheDir;
var
  ApacheBinDir: string;
begin
  ApacheBinDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\apache';

  FChangeHandle := FindFirstChangeNotification(
    PChar(ApacheBinDir),
    False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME
  );

  if FChangeHandle = INVALID_HANDLE_VALUE then
  begin
    WriteToLogMessage('Không thể theo dõi thư mục bin\apache.');
    Exit;
  end;

  try
    while WaitForSingleObject(FChangeHandle, INFINITE) = WAIT_OBJECT_0 do
    begin
      TThread.Synchronize(nil, ProcessDirectoryChange);
      if not FindNextChangeNotification(FChangeHandle) then Break;
    end;
  finally
    if FChangeHandle <> INVALID_HANDLE_VALUE then
      CloseHandle(FChangeHandle);
  end;
end;

// { thêm mới }: xử lý khi thư mục thay đổi
procedure TForm1.ProcessDirectoryChange;
begin
  UpdateVersionMenu;
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  // Hiển thị form từ tray
  Form1.Visible := True;  // Hiển thị lại form
  Form1.WindowState := wsNormal;  // Khôi phục trạng thái bình thường
  Form1.ShowInTaskbar := True;  // Hiển thị lại form trong Taskbar
  // TrayIcon1.Visible := False;
end;

procedure TForm1.TrayIcon1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TForm1.ExtractApacheVersion(const FullName: string): string;
var
  StartPos, EndPos: Integer;
  VersionStr: string;
begin
  // Tìm vị trí bắt đầu của số phiên bản
  StartPos := Pos('httpd-', FullName);
  if StartPos > 0 then
  begin
    VersionStr := Copy(FullName, StartPos + 6, Length(FullName)); // Bỏ 'httpd-'
    EndPos := Pos('-', VersionStr); // Tìm dấu '-' tiếp theo
    if EndPos > 0 then
      Result := Copy(VersionStr, 1, EndPos - 1)
    else
      Result := VersionStr;
  end
  else
    Result := ''; // Trả về chuỗi rỗng nếu không tìm thấy
end;

end.
