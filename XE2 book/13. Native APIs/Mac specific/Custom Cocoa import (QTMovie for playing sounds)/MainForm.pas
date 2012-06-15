unit MainForm;
{
  At my time of writing, XE2 does not supply an import unit for QTKit, which includes
  classes such as QTMovie. Defining Delphi wrappers using the RTL's Objective-C to
  Delphi bridge can be very easy though. This unit implements and uses a minimal import
  of the QTMovie class, which despite its name, can play sound as well as video files.
  
  As shown here, it is not necessary to translate an Objective-C class wholesale - you 
  need only translate the methods you wish to use.

  In the actual translations, the interface GUIDs used are arbitary, along with the 
  order methods are declared. For any given object parameter, you can also just use 
  Pointer if you don't care for having the parameter 'wrapped' by a Delphi interface.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation;

type
  //declare wrapper interface for class methods
  QTMovieClass = interface(NSObjectClass)
    function movieWithFile(fileName: NSString; error: PPointer = nil): Pointer; cdecl;
  end;

  //declare wrapper interface for instance methods
  QTMovie = interface(NSObject)
    ['{754E82A2-0135-4805-A7FE-D3A7B49ACC37}']
    procedure play; cdecl;
    procedure stop; cdecl;
  end;

  //declare Delphi factory class
  TQTMovie = class(TOCGenericImport<QTMovieClass, QTMovie>);

  TfrmQTMovie = class(TForm)
    btnFindFile: TButton;
    OpenDialog1: TOpenDialog;
    btnExit: TButton;
    procedure btnFindFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
    FMovie: QTMovie;
    procedure ReleaseMovie;
  end;

var
  frmQTMovie: TfrmQTMovie;

implementation

{$R *.fmx}

procedure TfrmQTMovie.btnFindFileClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  ReleaseMovie;
  FMovie := TQTMovie.Wrap(TQTMovie.OCClass.movieWithFile(NSSTR(OpenDialog1.FileName)));
  FMovie.retain; //QTMovie requirement - not ordinarily necessary
  FMovie.play;
end;

procedure TfrmQTMovie.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmQTMovie.FormDestroy(Sender: TObject);
begin
  ReleaseMovie;
end;

procedure TfrmQTMovie.ReleaseMovie;
begin
  if FMovie <> nil then
  begin
    FMovie.release;
    FMovie := nil;
  end;
end;

end.
