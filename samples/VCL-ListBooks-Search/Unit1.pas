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
  Signature : 80d751e95e20faf093f00409e6b26a71839a990b
  ***************************************************************************
*)

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.WinXCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Vcl.Grids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    ActivityIndicator1: TActivityIndicator;
    FDMemTable1: TFDMemTable;
    SearchBox1: TSearchBox;
    FDMemTable1Titre: TStringField;
    FDMemTable1auteurs: TStringField;
    FDMemTable1date: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SearchBox1InvokeSearch(Sender: TObject);
  private
    /// <summary>
    /// Compteur lié à l'animation
    /// </summary>
    FAnimationAttenteCompteur: integer;

    /// <summary>
    /// Parcourt la liste des livres fournies par delphi-books.com et demande leurs infos
    /// </summary>
    procedure TraiterListeLivres(NomFichierJSON: string);

    /// <summary>
    /// Reçoit les détails d'un livre et les ajoute à la liste à l'écran
    /// </summary>
    procedure TraiterLivre(NomFichierJSON: string);

    /// <summary>
    /// Active l'animation d'attente
    /// </summary>
    procedure LanceAnimationAttente;

    /// <summary>
    /// Désactive l'animation d'attente
    /// </summary>
    procedure ArreteAnimationAttente(Callback: TProc = nil);

    /// <summary>
    /// Active l'accès à la liste des livres
    /// </summary>
    procedure ActiveLaListeDesLivres;

    /// <summary>
    /// Effectue la recherche des livres depuis la table en mémoire et l'affiche dans la listbox
    /// </summary>
    procedure LancerRecherche;
    procedure DownloadFile(const ID: integer);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses u_download, System.IOUtils, System.JSON, System.Threading;

procedure TForm1.Button1Click(Sender: TObject);
var
  TempFileName: string;
begin
  Button1.Enabled := false;
  LanceAnimationAttente;
  TempFileName := tdownload_file.temporaryFileName('listelivres');
  tdownload_file.download('https://delphi-books.com/api/b/all.json',
    TempFileName,
    procedure
    begin // chargement ok
      try
        TraiterListeLivres(TempFileName);
      finally
        ArreteAnimationAttente(ActiveLaListeDesLivres);
      end;
    end,
    procedure
    begin // problème réseau ou pas de réponse
      ArreteAnimationAttente(
        procedure
        begin // Attention : c'est fait que si c'est le dernier thread qui plante
          Button1.Enabled := true;
        end);
      raise exception.Create('Récupération de la liste des livres impossible.');
    end);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimationAttenteCompteur := 0;
  ArreteAnimationAttente;
  ListBox1.visible := false;
  SearchBox1.visible := false;
end;

procedure TForm1.TraiterListeLivres(NomFichierJSON: string);
begin
  FDMemTable1.open;
  // FDMemTable1.EmptyDataSet;
  // Pas Clear qui supprime aussi la structure des champs saisis dans l'IDE

  LanceAnimationAttente;

  ttask.run( // tthread.CreateAnonymousThread( ... ).Start;
    procedure
    var
      jsa: tjsonarray;
      jso: tjsonobject;
      jsv: tjsonvalue;
      ID: integer;
    begin
      try
        try
          // Traite la liste de livres reçue en tableau JSON
          jsa := tjsonobject.ParseJSONValue(tfile.ReadAllText(NomFichierJSON))
            as tjsonarray;
          if assigned(jsa) then
            try
              if (jsa.Count > 0) then
                for jsv in jsa do
                begin
                  jso := jsv as tjsonobject;
                  if assigned(jso) then
                  begin
                    ID := jso.GetValue<integer>('id');
                    tthread.Synchronize(nil,
                      procedure
                      begin
                        LanceAnimationAttente;
                      end);
                    DownloadFile(ID);
                  end;
                end;
            finally
              jsa.Free;
            end;
        finally
          tfile.Delete(NomFichierJSON);
        end;
      finally
        tthread.Synchronize(nil,
          procedure
          begin
            ArreteAnimationAttente(ActiveLaListeDesLivres);
          end);
      end;
    end);
end;

procedure TForm1.TraiterLivre(NomFichierJSON: string);
begin
  LanceAnimationAttente;
  ttask.run(
    procedure
    var
      jsa: tjsonarray;
      jso_livre, jso_auteur: tjsonobject;
      jsv: tjsonvalue;
      titre: string;
      auteurs: string;
      NomAuteur: string;
      DatePublication: string;
    begin
      try
        try
          jso_livre := tjsonobject.ParseJSONValue
            (tfile.ReadAllText(NomFichierJSON)) as tjsonobject;
          if assigned(jso_livre) then
            try
              titre := jso_livre.GetValue<string>('name');
              jsa := jso_livre.GetValue<tjsonarray>('authors');
              auteurs := '';
              if assigned(jsa) and (jsa.Count > 0) then
                for jsv in jsa do
                begin
                  jso_auteur := jsv as tjsonobject;
                  if assigned(jso_auteur) then
                  begin
                    NomAuteur := jso_auteur.GetValue<string>('name');
                    if auteurs.Length > 0 then
                      auteurs := auteurs + ', ';
                    auteurs := auteurs + NomAuteur;
                  end;
                end;
              DatePublication := jso_livre.GetValue<string>('pubdate');
              tthread.Synchronize(nil,
                procedure
                begin
                  FDMemTable1.append;
                  FDMemTable1.fieldbyname('titre').AsString := titre;
                  FDMemTable1.fieldbyname('auteurs').AsString := auteurs;
                  FDMemTable1.fieldbyname('date').AsString := DatePublication;
                  FDMemTable1.Post;
                end);
            finally
              jso_livre.Free;
            end;
        finally
          tfile.Delete(NomFichierJSON);
        end;
      finally
        tthread.Synchronize(nil,
          procedure
          begin
            ArreteAnimationAttente(ActiveLaListeDesLivres);
          end);
      end;
    end);
end;

procedure TForm1.LanceAnimationAttente;
begin
  inc(FAnimationAttenteCompteur);
  if not ActivityIndicator1.visible then
    ActivityIndicator1.visible := true;
  if not ActivityIndicator1.animate then
    ActivityIndicator1.animate := true;
end;

procedure TForm1.LancerRecherche;
var
  TexteRecherche: string;
begin
  TexteRecherche := SearchBox1.Text;
  if (TexteRecherche.IsEmpty) then
    FDMemTable1.Filtered := false
  else
  begin
    TexteRecherche := '%' + TexteRecherche + '%';
    FDMemTable1.Filter := '(titre like ' + TexteRecherche.QuotedString +
      ') or (auteurs like ' + TexteRecherche.QuotedString + ')';
    // showmessage(FDMemTable1.Filter);
    FDMemTable1.Filtered := true;
  end;
  ListBox1.Clear;
  FDMemTable1.First;
  while not FDMemTable1.Eof do
  begin
    if FDMemTable1.fieldbyname('auteurs').AsString.Length > 0 then
      ListBox1.Items.Add(FDMemTable1.fieldbyname('titre').AsString + ' (' +
        FDMemTable1.fieldbyname('auteurs').AsString + ') - ' +
        FDMemTable1.fieldbyname('date').AsString)
    else
      ListBox1.Items.Add(FDMemTable1.fieldbyname('titre').AsString + ' (' +
        FDMemTable1.fieldbyname('date').AsString);
    FDMemTable1.next;
  end;
end;

procedure TForm1.DownloadFile(const ID: integer);
var
  TempFileName: string;
begin
  // Demande les infos liées à un livre au serveur
  TempFileName := tdownload_file.temporaryFileName('livre-' + ID.ToString);
  tdownload_file.download('https://delphi-books.com/api/b/' + ID.ToString +
    '.json', TempFileName,
    procedure
    begin // chargement ok
      try
        TraiterLivre(TempFileName);
      finally
        ArreteAnimationAttente(ActiveLaListeDesLivres);
      end;
    end,
    procedure
    begin // problème réseau ou pas de réponse
      ArreteAnimationAttente(
        procedure
        begin // Attention : c'est fait que si c'est le dernier thread qui plante
          Button1.Enabled := true;
        end);
      raise exception.Create('Récupération d''un livre impossible.');
    end);
end;

procedure TForm1.SearchBox1InvokeSearch(Sender: TObject);
begin
  LancerRecherche;
end;

procedure TForm1.ActiveLaListeDesLivres;
begin
  SearchBox1.visible := true;
  SearchBox1.SetFocus;
  ListBox1.visible := true;
  LancerRecherche;
end;

procedure TForm1.ArreteAnimationAttente(Callback: TProc);
begin
  if (FAnimationAttenteCompteur > 0) then
    dec(FAnimationAttenteCompteur);
  if (FAnimationAttenteCompteur < 1) then
  begin
    ActivityIndicator1.animate := false;
    ActivityIndicator1.visible := false;
    if assigned(Callback) then
      Callback;
  end;
end;

end.
