unit TrayIconHandler;

interface

uses
  System.Classes, Vcl.Menus, Vcl.Forms, Vcl.Controls, Winapi.Windows, Winapi.Messages, Vcl.Dialogs;

type
  TTrayIconHandler = class
  public
    class procedure TrayIconClick(Sender: TObject);
    class procedure TrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

uses
  Main, System.UITypes;

class procedure TTrayIconHandler.TrayIconClick(Sender: TObject);
begin
  if Assigned(Form1) then
  begin
    Form1.Show;
    Form1.WindowState := wsNormal;
    Form1.BringToFront;
  end;
end;

class procedure TTrayIconHandler.TrayIconMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    Form1.PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

end.

