unit VersionWatcher;

interface

procedure WatchVersionDirs;

implementation

uses
  Winapi.Windows, System.Classes, Vcl.Forms,
  Main, SelectVersion; // SelectVersion chứa các hàm UpdateXVersionMenu

procedure ProcessDirectoryChange;
begin
  UpdateApacheVersionMenu(Form1.FApacheDir);
  UpdatePhpVersionMenu(Form1.FPHPDir);
  UpdateMySQLVersionMenu(Form1.FMySQLDir);
end;

procedure WatchVersionDirs;
var
  ChangeHandles: array[0..2] of THandle;
  WaitResult: DWORD;
begin
  // Lấy đường dẫn thư mục
  Form1.FApacheDir := Form1.GetBinPath('apache');
  Form1.FPHPDir    := Form1.GetBinPath('php');
  Form1.FMySQLDir  := Form1.GetBinPath('mysql');

  // Tạo các handle theo dõi thư mục
  ChangeHandles[0] := FindFirstChangeNotification(PChar(Form1.FApacheDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);
  ChangeHandles[1] := FindFirstChangeNotification(PChar(Form1.FPHPDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);
  ChangeHandles[2] := FindFirstChangeNotification(PChar(Form1.FMySQLDir), False,
    FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME);

  if (ChangeHandles[0] = INVALID_HANDLE_VALUE) or
     (ChangeHandles[1] = INVALID_HANDLE_VALUE) or
     (ChangeHandles[2] = INVALID_HANDLE_VALUE) then
    Exit;

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
                UpdateApacheVersionMenu(Form1.FApacheDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[0]) then Break;
          end;

        WAIT_OBJECT_0 + 1:
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;
                UpdatePhpVersionMenu(Form1.FPHPDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[1]) then Break;
          end;

        WAIT_OBJECT_0 + 2:
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                ProcessDirectoryChange;
                UpdateMySQLVersionMenu(Form1.FMySQLDir);
              end);
            if not FindNextChangeNotification(ChangeHandles[2]) then Break;
          end;

      else
        Break;
      end;
    end;
  finally
    CloseHandle(ChangeHandles[0]);
    CloseHandle(ChangeHandles[1]);
    CloseHandle(ChangeHandles[2]);
  end;
end;

end.

