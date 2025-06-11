unit AppConfig;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils;

type
  TAppConfig = class
  private
    FIniFilePath: string;
    FPhpVersion: string;
    FApacheVersion: string;
    FMySQLVersion: string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor Create;
    property PhpVersion: string read FPhpVersion write FPhpVersion;
    property ApacheVersion: string read FApacheVersion write FApacheVersion;
    property MySQLVersion: string read FMySQLVersion write FMySQLVersion;
    procedure UpdateConfig(const APhpVersion, AApacheVersion, AMySQLVersion: string);
    function IsVersionValid(const Version, BaseDir: string): Boolean;
  end;

var
  Config: TAppConfig;

implementation

constructor TAppConfig.Create;
begin
  FIniFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'bin\lara.ini';
  LoadFromFile;
end;

procedure TAppConfig.LoadFromFile;
var
  IniFile: TIniFile;
begin
  if not FileExists(FIniFilePath) then
  begin
    // Tạo file mới với giá trị mặc định nếu không tồn tại
    FPhpVersion := '';
    FApacheVersion := '';
    FMySQLVersion := '';
    SaveToFile;
    Exit;
  end;

  IniFile := TIniFile.Create(FIniFilePath);
  try
    FPhpVersion := IniFile.ReadString('php', 'Version', '');
    FApacheVersion := IniFile.ReadString('apache', 'Version', '');
    FMySQLVersion := IniFile.ReadString('mysql', 'Version', '');
  finally
    IniFile.Free;
  end;
end;

procedure TAppConfig.SaveToFile;
var
  IniFile: TIniFile;
begin
  // Đảm bảo thư mục bin tồn tại
  ForceDirectories(ExtractFilePath(FIniFilePath));

  IniFile := TIniFile.Create(FIniFilePath);
  try
    IniFile.WriteString('php', 'Version', FPhpVersion);
    IniFile.WriteString('apache', 'Version', FApacheVersion);
    IniFile.WriteString('mysql', 'Version', FMySQLVersion);
  finally
    IniFile.Free;
  end;
end;

procedure TAppConfig.UpdateConfig(const APhpVersion, AApacheVersion, AMySQLVersion: string);
begin
  FPhpVersion := APhpVersion;
  FApacheVersion := AApacheVersion;
  FMySQLVersion := AMySQLVersion;
  SaveToFile;
end;

// Thêm hàm kiểm tra phiên bản có tồn tại không
function TAppConfig.IsVersionValid(const Version, BaseDir: string): Boolean;
begin
  Result := (Version <> '') and DirectoryExists(IncludeTrailingPathDelimiter(BaseDir) + Version);
end;

initialization
  Config := TAppConfig.Create;

finalization
  Config.Free;

end.
