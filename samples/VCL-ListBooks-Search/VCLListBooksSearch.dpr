/// <summary>
/// ***************************************************************************
///
/// Delphi Books API client for Delphi
///
/// Copyright 2020-2025 Patrick PREMARTIN under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Client library and samples programs showing how to use Delphi-Books.com
/// open data API in Delphi and Pascal languages.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://delphibooks4delphi.developpeur-pascal.fr/
///
/// Project site :
/// https://github.com/DeveloppeurPascal/DelphiBooks4Delphi
///
/// ***************************************************************************
/// File last update : 2025-02-05T20:59:20.281+01:00
/// Signature : a6a79485f638de17387846cc68f28d9f2f1c6929
/// ***************************************************************************
/// </summary>

program VCLListBooksSearch;

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
