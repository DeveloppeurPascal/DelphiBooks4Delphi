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
  Signature : a4554c3b134f41495f874f7285ab372f3fb626cb
  ***************************************************************************
*)

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uDM,
  System.Rtti, FMX.Grid.Style, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  FMX.Bind.Grid, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.Layouts,
  FMX.ExtCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid;

type
  TfrmMain = class(TForm)
    sgBooks: TStringGrid;
    ivCover: TImageViewer;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    procedure sgBooksSelChanged(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses u_download, System.IOUtils;

procedure TfrmMain.sgBooksSelChanged(Sender: TObject);
var
  img_url, img_local: string;
begin
  img_local := tpath.combine(tpath.GetTempPath,
    dm.BooksMemTable.FieldByName('id').AsString + '.png');
  if tfile.Exists(img_local) then
  begin
    ivCover.Bitmap.LoadFromFile(img_local);
    ivCover.BestFit;
  end
  else
  begin
    ivCover.Bitmap.Clear(talphacolors.white);
    if not dm.BooksMemTable.FieldByName('thumb').AsString.IsEmpty then
    begin
      img_url := dm.BooksMemTable.FieldByName('thumb').AsString;
      log.d('From %s to %s', [img_url, img_local]);
      tdownload_file.download(img_url, img_local,
        procedure
        begin
          ivCover.Bitmap.LoadFromFile(img_local);
          ivCover.BestFit;
        end);
    end;
  end;
end;

initialization

{$IFDEF DEBUG}
  reportmemoryleaksonshutdown := true;
{$ENDIF}

end.
