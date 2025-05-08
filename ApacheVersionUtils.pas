unit ApacheVersionUtils;

interface

procedure UpdateVersionMenu(const ApacheBinPath: string);
procedure UpdatePhpVersionMenu(const PHPBinPath: string);


implementation

uses
  System.SysUtils, System.Classes, Vcl.Menus,
  Main; // Truy cập Form1 và Version1

procedure UpdateVersionMenu(const ApacheBinPath: string);
var
  SearchRec: TSearchRec;
  Versions: TStringList;
  VersionMenuItem: TMenuItem;
begin
  Versions := TStringList.Create;
  try
    if FindFirst(ApacheBinPath + '\*', faDirectory, SearchRec) = 0 then
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
    Form1.Version1.Clear;

    for var S in Versions do
    begin
      VersionMenuItem := TMenuItem.Create(Form1.Version1);
      VersionMenuItem.Caption := S;
      VersionMenuItem.Hint := S;
      VersionMenuItem.OnClick := Form1.VersionMenuClick;
      Form1.Version1.Add(VersionMenuItem);
    end;

    if Form1.Version1.Count > 0 then
    begin
      (Form1.Version1.Items[0] as TMenuItem).Checked := True;
      Form1.SelectedVersion := (Form1.Version1.Items[0] as TMenuItem).Hint;
    end;
  finally
    Versions.Free;
  end;
end;

procedure UpdatePhpVersionMenu(const PHPBinPath: string);
var
  SearchRec: TSearchRec;
  Versions: TStringList;
  VersionMenuItem: TMenuItem;
begin
  Versions := TStringList.Create;
  try
    if FindFirst(PHPBinPath + '\*', faDirectory, SearchRec) = 0 then
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
    Form1.Version2.Clear;

    for var S in Versions do
    begin
      VersionMenuItem := TMenuItem.Create(Form1.Version2);
      VersionMenuItem.Caption := S;
      VersionMenuItem.Hint := S;
      VersionMenuItem.OnClick := Form1.PhpVersionMenuClick;
      Form1.Version2.Add(VersionMenuItem);
    end;

    if Form1.Version2.Count > 0 then
    begin
      (Form1.Version2.Items[0] as TMenuItem).Checked := True;
      Form1.SelectedPhpVersion := (Form1.Version2.Items[0] as TMenuItem).Hint;
    end;
  finally
    Versions.Free;
  end;
end;


end.

