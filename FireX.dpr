program FireX;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  MySQLIniUtils in 'Utils\MySQLIniUtils.pas',
  AppConfig in 'Utils\AppConfig.pas',
  PhpIniUpdater in 'Utils\PhpIniUpdater.pas',
  SelectVersion in 'Utils\SelectVersion.pas',
  ServicesConfig in 'Utils\ServicesConfig.pas',
  ServicesStop in 'Utils\ServicesStop.pas',
  TrayIconHandler in 'Utils\TrayIconHandler.pas',
  VersionWatcher in 'Utils\VersionWatcher.pas',
  WriteToLog in 'Utils\WriteToLog.pas',
  Settings in 'Settings.pas' {Form2},
  PortUtils in 'Utils\PortUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
