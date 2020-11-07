program FMXBooksListSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {frmMain},
  uDM in 'uDM.pas' {dm: TDataModule},
  u_download in 'u_download.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(Tdm, dm);
  Application.Run;
end.
