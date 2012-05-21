unit DownloadForm;
{
  Downloading a file on the internet to a stream using Indy (Delphi's default
  sockets framework) is easy, but 'blocking' - the Indy method in question
  doesn't return until it has finished downloading, leading to a frozen UI if
  called by a button's OnClick handler. This example demonstrates how to use a
  custom thread to download in the background, specifically via a custom TThread
  descendant that has its OnTerminate event assigned.

  In terms of the implementation, note that the Data property on our TThread
  descendant should *not* be accessed by the main thread until the secondary
  thread has terminated. This we enforce by not keeping hold of a reference to
  the thread object.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, IdHTTP,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Objects, FMX.Layouts;

type
  TfrmDownloadTest = class(TForm)
    imgOutput: TImage;
    Layout1: TLayout;
    Label1: TLabel;
    edtURL: TEdit;
    btnUnthreadedDL: TButton;
    btnAbout: TButton;
    Rectangle1: TRectangle;
    btnThreadedDL: TButton;
    lblInfo: TLabel;
    procedure btnThreadedDLClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnUnthreadedDLClick(Sender: TObject);
  strict private
    procedure DownloadCompleted(Sender: TObject);
  end;

var
  frmDownloadTest: TfrmDownloadTest;

implementation

{$R *.fmx}

type
  TDownloadThread = class(TThread)
  strict private
    FData: TCustomMemoryStream;
    FIdHttp: TIdHTTP;
    FURL: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;
    property Data: TCustomMemoryStream read FData;
  end;

constructor TDownloadThread.Create(const AURL: string);
begin
  inherited Create(True); //create suspended
  FData := TMemoryStream.Create;
  FIdHttp := TIdHTTP.Create(nil);
  FURL := AURL;
end;

destructor TDownloadThread.Destroy;
begin
  FIdHTTP.Free;
  FData.Free;
  inherited;
end;

procedure TDownloadThread.Execute;
begin
  FIdHttp.Get(FURL, FData);
end;

{ TfrmDownloadInBkg }

procedure TfrmDownloadTest.btnUnthreadedDLClick(Sender: TObject);
var
  IdHttp: TIdHTTP;
  Stream: TMemoryStream;
begin
  if edtURL.Text = '' then Exit;
  Stream := nil;
  IdHttp := TIdHTTP.Create(nil);
  try
    Stream := TMemoryStream.Create;
    IdHttp.Get(edtURL.Text, Stream);
    Stream.Position := 0;
    imgOutput.Bitmap.LoadFromStream(Stream);
  finally
    IdHttp.Free;
    Stream.Free;
  end;
  lblInfo.Visible := False;
end;

procedure TfrmDownloadTest.btnThreadedDLClick(Sender: TObject);
var
  Thread: TDownloadThread;
begin
  if edtURL.Text = '' then Exit;
  btnThreadedDL.Enabled := False;
  btnUnthreadedDL.Enabled := False;
  Thread := TDownloadThread.Create(edtURL.Text);
  Thread.FreeOnTerminate := True;
  Thread.OnTerminate := DownloadCompleted;
  Thread.Start;
end;

procedure TfrmDownloadTest.btnAboutClick(Sender: TObject);
begin
  MessageDlg('Demonstrates use of the OnTerminate event of TThread. When you use the ' +
    'non-threaded version, the application will ''hang'' while the data is downloaded. ' +
    'With the threaded version, in contrast, the application will remain responsive, meaning ' +
    'you will be able to show this dialog box, for example.', TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmDownloadTest.DownloadCompleted(Sender: TObject);
var
  Thread: TDownloadThread;
begin
  Thread := Sender as TDownloadThread;
  if Thread.FatalException <> nil then
    Application.HandleException(Thread.FatalException)
  else
    try
      Thread.Data.Position := 0;
      imgOutput.Bitmap.LoadFromStream(Thread.Data);
    except
      Application.HandleException(ExceptObject)
    end;
  lblInfo.Visible := False;
  btnThreadedDL.Enabled := True;
  btnUnthreadedDL.Enabled := True;
end;

end.
