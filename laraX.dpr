program laraX;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  WriteToLog in 'WriteToLog.pas',
  TrayIconHandler in 'TrayIconHandler.pas',
  ApacheConfig in 'ApacheConfig.pas',
  SelectVersion in 'SelectVersion.pas',
  StopApache in 'StopApache.pas',
  PhpIniUpdater in 'PhpIniUpdater.pas',
  AppConfig in 'AppConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
