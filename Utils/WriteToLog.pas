unit WriteToLog;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Dialogs;

procedure WriteToLogMessage(const Msg: string);

implementation

procedure WriteToLogMessage(const Msg: string);
var
  LogFileName: string;
  LogFile: TStreamWriter;
begin
  // Tạo đường dẫn tệp log
  LogFileName := ExtractFilePath(Application.ExeName) + 'Log.txt';
  
  // Đảm bảo thư mục tồn tại
  if not DirectoryExists(ExtractFilePath(LogFileName)) then
    ForceDirectories(ExtractFilePath(LogFileName));

  try
    LogFile := TStreamWriter.Create(LogFileName, True, TEncoding.UTF8);
    try
      LogFile.WriteLine(Format('%s - %s', [DateTimeToStr(Now), Msg]));
    finally
      LogFile.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Không thể ghi vào tệp log: ' + E.Message);
  end;
end;

end.
