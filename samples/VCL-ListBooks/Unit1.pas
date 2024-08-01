unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.WinXCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    ActivityIndicator1: TActivityIndicator;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    /// <summary>
    /// Compteur lié à l'animation
    /// </summary>
    FAnimationAttenteCompteur: integer;

    /// <summary>
    /// Parcourt la liste des livres fournies par delphi-books.com et demande leurs infos
    /// </summary>
    procedure TraiterListeLivres(NomFichierJSON: string);
    procedure DownloadFile(const id: integer);

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
  TempFileName := tdownload_file.temporaryFileName('listelivres');
  Button1.Enabled := false;
  LanceAnimationAttente;
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

procedure TForm1.DownloadFile(const id: integer);
var
  TempFileName: string;
begin
  TempFileName := tdownload_file.temporaryFileName('livre-' + id.ToString);
  tdownload_file.download('https://delphi-books.com/api/b/' + id.ToString +
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAnimationAttenteCompteur := 0;
  ArreteAnimationAttente;
  ListBox1.visible := false;
end;

procedure TForm1.TraiterListeLivres(NomFichierJSON: string);
begin
  // Vide la liste à l'écran
  ListBox1.Clear;
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
                    id := jso.GetValue<integer>('id');
                    tthread.Synchronize(nil,
                      procedure
                      begin
                        LanceAnimationAttente;
                      end);
                    // Demande les infos liées à un livre au serveur
                    DownloadFile(id);
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
                  if auteurs.Length > 0 then
                    ListBox1.Items.Add(titre + ' (' + auteurs + ') - ' +
                      DatePublication)
                  else
                    ListBox1.Items.Add(titre + ' - ' + DatePublication);
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

procedure TForm1.ActiveLaListeDesLivres;
begin
  ListBox1.visible := true;
  // ListBox1.Sorted := true; // TODO : erreur systeme 5 lors du tri, à voir ultérieurement
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
