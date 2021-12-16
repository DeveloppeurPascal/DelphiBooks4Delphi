object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object ListBox1: TListBox
    AlignWithMargins = True
    Left = 10
    Top = 84
    Width = 604
    Height = 347
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    ItemHeight = 15
    TabOrder = 0
    ExplicitTop = 51
    ExplicitHeight = 380
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ActivityIndicator1: TActivityIndicator
    Left = 24
    Top = 64
    IndicatorSize = aisXLarge
    IndicatorType = aitSectorRing
  end
  object SearchBox1: TSearchBox
    AlignWithMargins = True
    Left = 10
    Top = 51
    Width = 604
    Height = 23
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alTop
    TabOrder = 3
    TextHint = 'Saisissez le titre ou l'#39'auteur du livre recherch'#233
    OnInvokeSearch = SearchBox1InvokeSearch
    ExplicitLeft = 18
    ExplicitTop = 59
  end
  object FDMemTable1: TFDMemTable
    FilterOptions = [foCaseInsensitive]
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 536
    Top = 112
    object FDMemTable1Titre: TStringField
      FieldName = 'titre'
      Size = 200
    end
    object FDMemTable1auteurs: TStringField
      FieldName = 'auteurs'
      Size = 200
    end
    object FDMemTable1date: TStringField
      FieldName = 'date'
    end
  end
end
