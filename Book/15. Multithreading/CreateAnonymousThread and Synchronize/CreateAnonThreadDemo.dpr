program CreateAnonThreadDemo;

uses
  Vcl.Forms,
  DownloadAnonForm in 'DownloadAnonForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
