unit MainForm;
{
  Simple demo of a purely procedual DLL or dylib being loaded at runtime. Note you need to build
  the library project (MyUtils) first!
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit;

type
  TForm1 = class(TForm)
    btnAddThem: TButton;
    nbxLeft: TNumberBox;
    Label1: TLabel;
    nbxRight: TNumberBox;
    lblResult: TLabel;
    procedure btnAddThemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLibHandle: HMODULE;
    { Declare a procedural pointer to the library func, being careful to get the calling
      convention right }
    FTestFunc: function (A, B: Int32): Int32; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  end;

var
  Form1: TForm1;

implementation

{$IFDEF MSWINDOWS}
uses Winapi.Windows;
{$ENDIF}

{$R *.fmx}

const
  LibName = {$IFDEF MSWINDOWS}'MyUtils.dll'{$ELSE}'libMyUtils.dylib'{$ENDIF};
  ProcPrefix = {$IFDEF MACOS}'_'{$ELSE}''{$ENDIF};

procedure TForm1.btnAddThemClick(Sender: TObject);
begin
  if FLibHandle = 0 then
  begin
    FLibHandle := LoadLibrary(LibName);
    if FLibHandle = 0 then
    begin
      ShowMessage('Failed to find library');
      Exit;
    end;
    FTestFunc := GetProcAddress(FLibHandle, ProcPrefix + 'AddThem');
    if not Assigned(FTestFunc) then
    begin
      ShowMessage('Failed to find function');
      FreeLibrary(FLibHandle);
      FLibHandle := 0;
      Exit;
    end;
  end;
  lblResult.Text := IntToStr(FTestFunc(Trunc(nbxLeft.Value), Trunc(nbxRight.Value)));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);
end;

end.
