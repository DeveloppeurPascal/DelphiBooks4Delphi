unit uDM;

interface

uses
  System.SysUtils, System.Classes, REST.Types, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, REST.Response.Adapter, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  Tdm = class(TDataModule)
    DelphiBookAPI: TRESTClient;
    BooksAllRequest: TRESTRequest;
    BooksAllResponse: TRESTResponse;
    BooksAllDSAdapter: TRESTResponseDataSetAdapter;
    BooksMemTable: TFDMemTable;
    BooksMemTableid: TFloatField;
    BooksMemTablename: TWideStringField;
    BooksMemTableurl: TWideStringField;
    BooksMemTablethumb: TWideStringField;
    BooksMemTablelang: TWideStringField;
    BooksMemTablepubdate: TWideStringField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  dm: Tdm;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  BooksAllRequest.Execute;
end;

end.
