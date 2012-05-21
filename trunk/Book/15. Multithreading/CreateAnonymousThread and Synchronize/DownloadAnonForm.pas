unit DownloadAnonForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnLoadUnthreaded: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    RichEdit1: TRichEdit;
    RichEdit3: TRichEdit;
    RichEdit4: TRichEdit;
    RichEdit5: TRichEdit;
    btnLoadThreaded: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    TabSheet2: TTabSheet;
    RichEdit2: TRichEdit;
    TabSheet6: TTabSheet;
    RichEdit6: TRichEdit;
    procedure btnLoadThreadedClick(Sender: TObject);
    procedure btnLoadUnthreadedClick(Sender: TObject);
  private
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses IdHttp;

procedure StartDownloadThread(const URL: string; Dest: TCustomMemo);
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    var
      IdHttp: TIdHTTP;
      HTML: string;
    begin
      IdHttp := TIdHTTP.Create(nil);
      try
        HTML := IdHttp.Get(URL);
      finally
        IdHttp.Free;
      end;
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Dest.Text := HTML;
        end);
    end);
  Thread.Start;
end;

procedure TForm1.btnLoadThreadedClick(Sender: TObject);
var
  I: Integer;
  Memo: TCustomMemo;
  Page: TTabSheet;
begin
  for I := 0 to PageControl1.PageCount - 1 do
  begin
    Page := PageControl1.Pages[I];
    Memo := (Page.Controls[0] as TCustomMemo);
    Memo.Text := 'Downloading...';
    StartDownloadThread('http://www.' + Page.Caption + '/', Memo);
  end;
end;

procedure TForm1.btnLoadUnthreadedClick(Sender: TObject);
var
  I: Integer;
  Memo: TCustomMemo;
  Page: TTabSheet;
  IdHttp: TIdHTTP;
begin
  IdHttp := TIdHttp.Create(nil);
  try
    for I := 0 to PageControl1.PageCount - 1 do
    begin
      Page := PageControl1.Pages[I];
      Memo := (Page.Controls[0] as TCustomMemo);
      Memo.Text := 'Downloading...';
      Memo.Update;
      Memo.Text := IdHttp.Get('http://www.' + Page.Caption + '/');
    end;
  finally
    IdHttp.Free;
  end;
end;

end.
