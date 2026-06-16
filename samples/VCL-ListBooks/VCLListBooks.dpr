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
  https://delphi-books.com/opendata.html

  Project site :
  https://codeberg.org/Delphi-Books/DelphiBooks4Delphi

  ***************************************************************************
  File last update : 2026-06-15T19:32:13.622+02:00
  Signature : 00d72884642d7ca5495a88a5fd28783dbd966b78
  ***************************************************************************
*)

program VCLListBooks;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  u_download in '..\..\lib-externes\librairies\src\u_download.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
