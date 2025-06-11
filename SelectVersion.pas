unit SelectVersion;

interface

procedure UpdateApacheVersionMenu(const ApacheBinPath: string);
procedure UpdatePhpVersionMenu(const PHPBinPath: string);
procedure UpdateMySQLVersionMenu(const MySQLBinPath: string);


implementation

uses
  System.SysUtils, System.Classes, Vcl.Menus, AppConfig,
  Main; // Truy cập Form1 và Version1

procedure UpdateVersionMenu(const BinPath: string; Menu: TMenuItem; OnClick: TNotifyEvent);
var
  SearchRec: TSearchRec;
  Versions: TStringList;
  VersionMenuItem: TMenuItem;
  SelectedVersion: string;
begin
  Versions := TStringList.Create;
  try
    if FindFirst(BinPath + '\*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        if ((SearchRec.Attr and faDirectory) = faDirectory) and
           (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          Versions.Add(SearchRec.Name);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;

    Versions.Sort;
    Menu.Clear;

    // Chọn lại đúng phiên bản từ lara.ini nếu còn tồn tại
    if Menu = Form1.Version1 then
      SelectedVersion := Config.ApacheVersion
    else if Menu = Form1.Version2 then
      SelectedVersion := Config.PhpVersion
    else if Menu = Form1.Version3 then
      SelectedVersion := Config.MySQLVersion
    else
      SelectedVersion := '';

    for var S in Versions do
    begin
      VersionMenuItem := TMenuItem.Create(Menu);
      VersionMenuItem.Caption := S;
      VersionMenuItem.Hint := S;
      VersionMenuItem.OnClick := OnClick;

      // Nếu trùng với phiên bản trong lara.ini thì đánh dấu check
      if S = SelectedVersion then
        VersionMenuItem.Checked := True;

      Menu.Add(VersionMenuItem);
    end;
  finally
    Versions.Free;
  end;
end;

procedure UpdateApacheVersionMenu(const ApacheBinPath: string);
begin
  UpdateVersionMenu(ApacheBinPath, Form1.Version1, Form1.ApacheVersionMenuClick);
end;

procedure UpdatePhpVersionMenu(const PHPBinPath: string);
begin
  UpdateVersionMenu(PHPBinPath, Form1.Version2, Form1.PhpVersionMenuClick);
end;

procedure UpdateMySQLVersionMenu(const MySQLBinPath: string);
begin
  UpdateVersionMenu(MySQLBinPath, Form1.Version3, Form1.MySQLVersionMenuClick);
end;

end.

