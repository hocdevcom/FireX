unit StopApache;

interface

uses
  Winapi.Windows, TlHelp32, System.SysUtils, ShellAPI, System.IOUtils;

function IsApacheRunning: Boolean;
procedure KillApache;
procedure KillApacheByPID(const FApacheDir: string);

implementation

function IsApacheRunning: Boolean;
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

procedure KillApache;
var
  RetryCount: Integer;
begin
  if ShellExecute(0, 'open', 'taskkill.exe', '/IM httpd.exe /F', nil, SW_HIDE) <= 32 then
  begin
    // Ghi log hoặc thông báo lỗi khi không thể gửi lệnh
    Exit;
  end;

  RetryCount := 0;
  while IsApacheRunning and (RetryCount < 10) do
  begin
    Sleep(300);
    Inc(RetryCount);
  end;
end;

// Đọc PID từ file httpd.pid và kill tiến trình đó
procedure KillApacheByPID(const FApacheDir: string);
var
  PIDFilePath: string;
  PIDStr: string;
  PID: DWORD;
  ProcHandle: THandle;
begin
  PIDFilePath := TPath.Combine(FApacheDir, 'logs\httpd.pid');
  if not TFile.Exists(PIDFilePath) then
  begin
    Exit; // Không tìm thấy file PID
  end;

  PIDStr := Trim(TFile.ReadAllText(PIDFilePath));
  PID := StrToIntDef(PIDStr, 0);
  if PID = 0 then
  begin
    Exit; // PID không hợp lệ
  end;

  ProcHandle := OpenProcess(PROCESS_TERMINATE, False, PID);
  if ProcHandle <> 0 then
  begin
    if TerminateProcess(ProcHandle, 0) then
    begin
      // Thành công
    end;
    CloseHandle(ProcHandle);
  end;
end;

end.

