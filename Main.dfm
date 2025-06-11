object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 418
  ClientWidth = 671
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object BtnStart: TBitBtn
    Left = 11
    Top = 376
    Width = 120
    Height = 30
    Caption = 'Start'
    Default = True
    ImageIndex = 0
    ImageName = 'play'
    Images = VirtualImageList1
    TabOrder = 0
    OnClick = BtnStartClick
  end
  object BtnWeb: TBitBtn
    Left = 142
    Top = 376
    Width = 120
    Height = 30
    Caption = 'Web'
    ImageIndex = 2
    ImageName = 'web'
    Images = VirtualImageList1
    TabOrder = 1
    OnClick = btWebClick
  end
  object BtnDB: TBitBtn
    Left = 274
    Top = 376
    Width = 120
    Height = 30
    Caption = 'Database'
    ImageIndex = 3
    ImageName = 'database'
    Images = VirtualImageList1
    TabOrder = 2
  end
  object BtnTer: TBitBtn
    Left = 406
    Top = 376
    Width = 120
    Height = 30
    Caption = 'Terminal'
    ImageIndex = 4
    ImageName = 'terminal'
    Images = VirtualImageList1
    TabOrder = 3
  end
  object BtnRoot: TBitBtn
    Left = 538
    Top = 376
    Width = 120
    Height = 30
    Caption = 'Root'
    ImageIndex = 5
    ImageName = 'folder'
    Images = VirtualImageList1
    TabOrder = 4
    OnClick = btRootClick
  end
  object PopupMenu1: TPopupMenu
    Left = 564
    Top = 328
    object PopLaraX: TMenuItem
      Caption = 'LaraX'
      object PopWeb: TMenuItem
        Caption = 'Web'
      end
      object PopDB: TMenuItem
        Caption = 'Database'
      end
      object PopTer: TMenuItem
        Caption = 'Terminal'
      end
      object PopRoot: TMenuItem
        Caption = 'Root'
      end
      object PopLine2: TMenuItem
        Caption = '-'
      end
      object PopINI: TMenuItem
        Caption = 'Lara.ini'
      end
    end
    object PopWWW: TMenuItem
      Caption = 'www'
    end
    object PopQuick: TMenuItem
      Caption = 'Quick app'
    end
    object PopTool: TMenuItem
      Caption = 'Tools'
    end
    object PopLine1: TMenuItem
      Caption = '-'
    end
    object PopPHP: TMenuItem
      Caption = 'PHP'
      object Version2: TMenuItem
        Caption = 'Version'
      end
    end
    object PopApache: TMenuItem
      Caption = 'Apache'
      object Version1: TMenuItem
        Caption = 'Version'
      end
    end
    object PopMySQL: TMenuItem
      Caption = 'MySQL'
      object Version3: TMenuItem
        Caption = 'Version'
      end
    end
    object PopQuit: TMenuItem
      Caption = 'Exit'
      OnClick = PopQuitClick
    end
  end
  object TrayIcon1: TTrayIcon
    Left = 632
    Top = 328
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'play'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000004249444154789C
              6360A006483CF8FF3F3978901B00026F7EFCFF5F769202030819429401F80C21
              DA005C86D0CF80379478E10D2581F88692684C1CF094489201940000D8207CC7
              36DBA03A0000000049454E44AE426082}
          end>
      end
      item
        Name = 'stop'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000002749444154789C
              6360A006F86718FA9F1C3C9C0DC005460DF84FC730F837E0E9E01FDD0DA00400
              00E33D216F77535FEE0000000049454E44AE426082}
          end>
      end
      item
        Name = 'web'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000014C49444154789C
              A553B14E03310CCD021F036C7C0124EDD691810F0046567EA048200AA5C45721
              5106060636BA21CAC04C25900A7B073EC0BE74A328E8E5EECA956B8B2896ACF8
              EC679FEDBC28F5434A915BD196EB86F855930CA0C1B65C474C4D938D1BBF68AC
              9026FE34247E926ACB434D72066C21595B7900A81425E07253FC7ED7F96AD78D
              7CD9A9493A63458C1542A0D212BFF7E80268A71DFBABF741D0ED761C7C88552E
              D28EAC34F2330FCB11FB5ACFF9DDBB047CF8E246050E9E93A28801032C72D6AC
              5B5686F874DACCBF2B1F2B63F96DDE02DA724F1912878FCB7ED2723912BF79FD
              3D7FA6F021061BD8740F311618FFB540AB3FC86E43FE3F82263E99BB00F191C2
              55CC7B8DEBCD78297001F49C45A4ADDB2291F036C6A94CD2C9D31527685C7D2A
              52D958B95F3DF70B931E5303ADCD20CE07FE5C48CE0B76028661C329475CB02D
              D74633E7E40BCE607401C6122AD60000000049454E44AE426082}
          end>
      end
      item
        Name = 'database'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000015C49444154789C
              DD90CF4EC24010C679021F42DFC2F830FA005E14FB87E2418FB06C970B3762E9
              B6D4887F4E5488072D0A09164589068D7810A3486224C18B7219337B326991C4
              7872922FF932F3ED2F331B89FC6525A2C60C91F82AD39C735DB55F89648E50E8
              99963FC31955B2D3A18F996A2DD398F5BEBB71F2513BE840EBB40F37AD8110FA
              DAC13DE00C3344B197028094CC477EA50B9DEBE18FF2BD0720321F8501201D77
              60CFA882D8A0FE6D833A6ED0C10D4486C81C0280E44A0E5AF51E141D1F0CEA42
              66AD00BA6209A137E83EB89BBEC8603614D0A83C4E3CA1E175C703D2710776B2
              C7502DDF053E117B38C3CC58C095DF87D2561372BA0B99F56DA0AA25841E7BE5
              425364FEEB093466BD786E7BE2095EB10D54B57B010091737344E60393958718
              BAA83DC3EDE540083DF64C561AA614FE4624733600C04A2FF2A9A4642C302D7F
              A46BF61391CC4F147A16B70F1351731E33A18F7F5B5F4A68F4FF00122F530000
              000049454E44AE426082}
          end>
      end
      item
        Name = 'terminal'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000004D49444154789C
              6360A002F84F2166184606900A701A50C6C0C0D041AE015A0C0C0CBFA1FC0672
              5D10826448073906842219504FAA01EA58BC8011EA845C50826633C906301009
              06514AA4080000B75C9453C8AA49720000000049454E44AE426082}
          end>
      end
      item
        Name = 'folder'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
              61000000097048597300000B1300000B1301009A9C180000006849444154789C
              6360A0144CAB72F98F8EA7D7B8F49264C0ED0DA9707C754DF2FFF98D9E5F6654
              BBF49165C0ED0DA9FF8F2D8CF93FB7CEE32F36D7A163066C828B9ABDFF9F5F9B
              F5FFF3D996FF5FCFB5E2C47003BEE251346A40EB68189C23320C28C114E46308
              0000A71D61753F0E81080000000049454E44AE426082}
          end>
      end>
    Left = 472
    Top = 328
  end
  object VirtualImageList1: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'play'
        Name = 'play'
      end
      item
        CollectionIndex = 1
        CollectionName = 'stop'
        Name = 'stop'
      end
      item
        CollectionIndex = 2
        CollectionName = 'web'
        Name = 'web'
      end
      item
        CollectionIndex = 3
        CollectionName = 'database'
        Name = 'database'
      end
      item
        CollectionIndex = 4
        CollectionName = 'terminal'
        Name = 'terminal'
      end
      item
        CollectionIndex = 5
        CollectionName = 'folder'
        Name = 'folder'
      end>
    ImageCollection = ImageCollection1
    Left = 376
    Top = 328
  end
end
