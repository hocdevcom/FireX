unit Settings;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, PortUtils,
  Vcl.StdCtrls, Vcl.NumberBox, SysUtils, AppConfig, WriteToLog, Vcl.ExtCtrls;

type
  TPortChangedEvent = procedure of object;

  TForm2 = class(TForm)
    PageControl1: TPageControl;
    Tabchung: TTabSheet;
    Tabcong: TTabSheet;
    NumberBox2: TNumberBox;
    NumberBox1: TNumberBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FOnPortChanged: TPortChangedEvent;
  public
    SelectedApacheVersion: string;
    SelectedMySQLVersion: string;
    FApacheDir, FMySQLDir: string;
    function GetBinPath(const SubDir: string): string;
    property OnPortChanged: TPortChangedEvent read FOnPortChanged write FOnPortChanged;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
var
  ApachePort, MySQLPort: string;
begin
  PageControl1.OwnerDraw := False;
  PageControl1.TabStop := False; // Loại bỏ nét đứt trên tab
  PageControl1.OnDrawTab := nil;

  FApacheDir := GetBinPath('apache');
  FMySQLDir := GetBinPath('mysql');

  SelectedApacheVersion := '';
  SelectedMySQLVersion := '';

  if Config.IsVersionValid(Config.ApacheVersion, FApacheDir) then
    SelectedApacheVersion := Config.ApacheVersion;
  if Config.IsVersionValid(Config.MySQLVersion, FMySQLDir) then
    SelectedMySQLVersion := Config.MySQLVersion;

  ApachePort := TPortUtils.GetApachePort(FApacheDir, SelectedApacheVersion);
  MySQLPort := TPortUtils.GetMySQLPort(FMySQLDir, SelectedMySQLVersion);

  NumberBox1.Value := StrToInt(ApachePort);
  NumberBox2.Value := StrToInt(MySQLPort);

  // Gán sự kiện OnClose trong phần khai báo của form (không cần gán lại trong FormCreate)
  Self.OnClose := FormClose;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Áp dụng thay đổi khi đóng form
  TPortUtils.SetApachePort(FApacheDir, SelectedApacheVersion, IntToStr(Trunc(NumberBox1.Value))); // Chuyển đổi kiểu nếu cần
  TPortUtils.SetMySQLPort(FMySQLDir, SelectedMySQLVersion, IntToStr(Trunc(NumberBox2.Value))); // Chuyển đổi kiểu nếu cần

  // Nếu cần thông báo sự thay đổi cổng, gọi sự kiện sau khi đóng form
  if Assigned(FOnPortChanged) then
    FOnPortChanged;
end;


function TForm2.GetBinPath(const SubDir: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'bin\' + SubDir;
end;

end.

