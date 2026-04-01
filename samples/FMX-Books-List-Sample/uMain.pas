(* C2PP
  ***************************************************************************

  Delphi Books API client for Delphi
  Copyright (c) 2020-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  File last update : 2025-05-26T15:42:07.255+02:00
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
