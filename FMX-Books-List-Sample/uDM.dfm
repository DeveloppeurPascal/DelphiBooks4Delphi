object dm: Tdm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 516
  Width = 759
  object DelphiBookAPI: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'https://delphi-books.com/api'
    Params = <>
    Left = 104
    Top = 64
  end
  object BooksAllRequest: TRESTRequest
    AssignedValues = [rvConnectTimeout, rvReadTimeout]
    Client = DelphiBookAPI
    Params = <>
    Resource = 'b/all.json'
    Response = BooksAllResponse
    Left = 224
    Top = 64
  end
  object BooksAllResponse: TRESTResponse
    ContentType = 'application/json'
    Left = 320
    Top = 64
  end
  object BooksAllDSAdapter: TRESTResponseDataSetAdapter
    Dataset = BooksMemTable
    FieldDefs = <>
    Response = BooksAllResponse
    TypesMode = JSONOnly
    Left = 424
    Top = 64
  end
  object BooksMemTable: TFDMemTable
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvUpdateChngFields, uvUpdateMode, uvLockMode, uvLockPoint, uvLockWait, uvRefreshMode, uvFetchGeneratorsPoint, uvCheckRequired, uvCheckReadOnly, uvCheckUpdatable]
    UpdateOptions.LockWait = True
    UpdateOptions.FetchGeneratorsPoint = gpNone
    UpdateOptions.CheckRequired = False
    StoreDefs = True
    Left = 520
    Top = 64
    object BooksMemTableid: TFloatField
      FieldName = 'id'
    end
    object BooksMemTablename: TWideStringField
      FieldName = 'name'
      Size = 255
    end
    object BooksMemTableurl: TWideStringField
      FieldName = 'url'
      Size = 255
    end
    object BooksMemTablethumb: TWideStringField
      FieldName = 'thumb'
      Size = 255
    end
    object BooksMemTablelang: TWideStringField
      FieldName = 'lang'
      Size = 2
    end
    object BooksMemTablepubdate: TWideStringField
      FieldName = 'pubdate'
      Size = 10
    end
  end
end
