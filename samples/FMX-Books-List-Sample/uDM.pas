(* C2PP
  ***************************************************************************

  Delphi Books API client for Delphi

  Copyright 2020-2025 Patrick PREMARTIN under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  Client library and samples programs showing how to use Delphi-Books.com
  open data API in Delphi and Pascal languages.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://delphibooks4delphi.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/DelphiBooks4Delphi

  ***************************************************************************
  File last update : 2025-02-09T11:03:50.267+01:00
  Signature : 3480e73251b0ff8e49771b5d59beb3149afdf8a1
  ***************************************************************************
*)

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
