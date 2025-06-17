unit ServicesStop;

interface

uses
  Winapi.Windows, TlHelp32, System.SysUtils, ShellAPI, System.IOUtils;

function ServicesRunning(const ProcessName: string): Boolean; overload;
function ServicesRunning(const ProcessNames: array of string): Boolean; overload;
procedure KillServices;
procedure KillServicesByPID(const FApacheDir, FMySQLDir: string);

implementation

function ServicesRunning(const ProcessName: string): Boolean; overload;
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
      if SameText(ExtractFileName(ProcEntry.szExeFile), ProcessName) then
      begin
        Result := True;
        Break;
      end;
    until not Process32Next(Snapshot, ProcEntry);
  end;

  CloseHandle(Snapshot);
end;

function ServicesRunning(const ProcessNames: array of string): Boolean; overload;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
  Found: array of Boolean;
  I: Integer;
begin
  Result := False;
  SetLength(Found, Length(ProcessNames));

  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then Exit;

  ProcEntry.dwSize := SizeOf(TProcessEntry32);
  if Process32First(Snapshot, ProcEntry) then
  begin
    repeat
      for I := 0 to High(ProcessNames) do
      begin
        if not Found[I] and SameText(ExtractFileName(ProcEntry.szExeFile), ProcessNames[I]) then
        begin
          Found[I] := True;
        end;
      end;
    until not Process32Next(Snapshot, ProcEntry);
  end;

  CloseHandle(Snapshot);

  // Kiểm tra tất cả phải được tìm thấy
  Result := True;
  for I := 0 to High(Found) do
    if not Found[I] then
    begin
      Result := False;
      Break;
    end;
end;

procedure KillServices;
var
  RetryCount: Integer;
begin
  // Tắt Apache
  if ShellExecute(0, 'open', 'taskkill.exe', '/IM httpd.exe /F', nil, SW_HIDE) <= 32 then
  begin
    // Ghi log hoặc thông báo lỗi khi không thể gửi lệnh tắt Apache
    Exit;
  end;

  // Tắt MySQL
  if ShellExecute(0, 'open', 'taskkill.exe', '/IM mysqld.exe /F', nil, SW_HIDE) <= 32 then
  begin
    // Ghi log hoặc thông báo lỗi khi không thể gửi lệnh tắt MySQL
    Exit;
  end;

  RetryCount := 0;
  // Kiểm tra trạng thái của cả Apache và MySQL
  while ServicesRunning(['httpd.exe', 'mysqld.exe']) and (RetryCount < 10) do
  begin
    Sleep(300);  // Chờ 300ms trước khi kiểm tra lại
    Inc(RetryCount);
  end;
end;

// Đọc PID từ file httpd.pid và kill tiến trình đó
procedure KillServicesByPID(const FApacheDir, FMySQLDir: string);
var
  PIDFilePath: string;
  PIDStr: string;
  PID: DWORD;
  ProcHandle: THandle;
begin
  // Xử lý Apache
  PIDFilePath := TPath.Combine(FApacheDir, 'logs\httpd.pid');
  if TFile.Exists(PIDFilePath) then
  begin
    PIDStr := Trim(TFile.ReadAllText(PIDFilePath));
    PID := StrToIntDef(PIDStr, 0);
    if PID <> 0 then
    begin
      ProcHandle := OpenProcess(PROCESS_TERMINATE, False, PID);
      if ProcHandle <> 0 then
      begin
        TerminateProcess(ProcHandle, 0);
        CloseHandle(ProcHandle);
      end;
    end;
  end;

  // Xử lý MySQL
  PIDFilePath := TPath.Combine(FMySQLDir, 'data\mysql-8.4\mysqld.pid');
  if TFile.Exists(PIDFilePath) then
  begin
    PIDStr := Trim(TFile.ReadAllText(PIDFilePath));
    PID := StrToIntDef(PIDStr, 0);
    if PID <> 0 then
    begin
      ProcHandle := OpenProcess(PROCESS_TERMINATE, False, PID);
      if ProcHandle <> 0 then
      begin
        TerminateProcess(ProcHandle, 0);
        CloseHandle(ProcHandle);
      end;
    end;
  end;
end;



end.

