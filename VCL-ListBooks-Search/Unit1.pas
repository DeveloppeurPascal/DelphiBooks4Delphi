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
    /// Compteur li� � l'animation
    /// </summary>
    FAnimationAttenteCompteur: integer;

    /// <summary>
    /// Parcourt la liste des livres fournies par delphi-books.com et demande leurs infos
    /// </summary>
    procedure TraiterListeLivres(NomFichierJSON: string);

    /// <summary>
    /// Re�oit les d�tails d'un livre et les ajoute � la liste � l'�cran
    /// </summary>
    procedure TraiterLivre(NomFichierJSON: string);

    /// <summary>
    /// Active l'animation d'attente
    /// </summary>
    procedure LanceAnimationAttente;

    /// <summary>
    /// D�sactive l'animation d'attente
    /// </summary>
    procedure ArreteAnimationAttente(Callback: TProc = nil);

    /// <summary>
    /// Active l'acc�s � la liste des livres
    /// </summary>
    procedure ActiveLaListeDesLivres;

    /// <summary>
    /// Effectue la recherche des livres depuis la table en m�moire et l'affiche dans la listbox
    /// </summary>
    procedure LancerRecherche;
  public
    { D�clarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses u_download, System.IOUtils, System.JSON, System.Threading;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := false;
  LanceAnimationAttente;
  tdownload_file.download('https://delphi-books.com/api/b/all.json',
    tdownload_file.temporaryFileName('listelivres'),
    procedure(NomFichierTemp: string)
    begin // chargement ok
      try
        TraiterListeLivres(NomFichierTemp);
      finally
        ArreteAnimationAttente(ActiveLaListeDesLivres);
      end;
    end,
    procedure
    begin // probl�me r�seau ou pas de r�ponse
      ArreteAnimationAttente(
        procedure
        begin // Attention : c'est fait que si c'est le dernier thread qui plante
          Button1.Enabled := true;
        end);
      raise exception.Create('R�cup�ration de la liste des livres impossible.');
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
      id: integer;
    begin
      try
        try
          // Traite la liste de livres re�ue en tableau JSON
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
                    id := jso.GetValue<integer>('id');
                    tthread.Synchronize(nil,
                      procedure
                      begin
                        LanceAnimationAttente;
                      end);
                    // Demande les infos li�es � un livre au serveur
                    tdownload_file.download('https://delphi-books.com/api/b/' +
                      id.ToString + '.json',
                      tdownload_file.temporaryFileName('livre-' + id.ToString),
                      procedure(NomFichierTemp: string)
                      begin // chargement ok
                        try
                          TraiterLivre(NomFichierTemp);
                        finally
                          ArreteAnimationAttente(ActiveLaListeDesLivres);
                        end;
                      end,
                      procedure
                      begin // probl�me r�seau ou pas de r�ponse
                        ArreteAnimationAttente(
                          procedure
                          begin // Attention : c'est fait que si c'est le dernier thread qui plante
                            Button1.Enabled := true;
                          end);
                        raise exception.Create
                          ('R�cup�ration d''un livre impossible.');
                      end);
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
