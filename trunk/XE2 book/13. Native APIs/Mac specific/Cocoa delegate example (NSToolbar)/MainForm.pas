unit MainForm;
{
  The Cocoa toolbar control is fixed to the top of the form and (for a few OS X
  releases now) visually integrates with the title bar. To use it, you need to
  create a delegate object. In this project, the CCR.CocoaToolbar unit does the
  the necessary.

  The button glyphs have been set up via Project|Deployment - after selecting
  'All configurations - OS X platform' from the combo box at the top, click the
  button one in from the left to add new files to the outputted application
  'bundle'. Once added, click on the 'Remote path' cell, press F2, and change
  the value from Contents\MacOS to Contents\Resources. (The glyphs I've used
  come from Mark James' 'Silk' set - http://www.famfamfam.com/lab/icons/silk/.)

  Unfortunately, one thing I have not been able to get working is the built-in
  toolbar customisation dialog. More exactly, if you uncomment the two lines
  below that call setAllowsUserCustomization and setAutosavesConfiguration, they
  will work up until the point the user goes to show the customisation dialog.
  Go to show it regardless, and the application will hang.

  Chris Rolliston, July 2012
  http://delphifoundations.com/
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.Menus,
  CCR.CocoaToolbar;

type
  TfrmNSToolbarDemo = class(TForm)
    memEditor: TMemo;
    dlgOpen: TOpenDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    itmFileOpen: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure itmFileOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHelper: TNSToolbarHelper;
  end;

var
  frmNSToolbarDemo: TfrmNSToolbarDemo;

implementation

uses Macapi.AppKit;

{$R *.fmx}

procedure TfrmNSToolbarDemo.FormCreate(Sender: TObject);
var
  Item: TNSToolbarHelperItem;
begin
  itmFileOpen.ShortCut := scCommand or Ord('O');
  { Both toolbar and toolbar items have textual identifiers in the Cocoa system,
    and my wrapper code reflects that. }
  FHelper := TNSToolbarHelper.Create('DocumentToolbar');
  //open command - use the corresponding menu item's OnClick handler
  Item := FHelper.AddItem('FileOpen');
  Item.SetCaptionImageAndHint('Open', 'Open.png', 'Open file');
  Item.OnClick := itmFileOpenClick;
  //copy command - ensure the button is enabled and disabled as appropriate
  Item := FHelper.AddItem('EditCopy');
  Item.SetCaptionImageAndHint('Copy', 'Copy.png', 'Copy selected text');
  Item.OnClick :=
    procedure (Sender: TObject)
    begin
      memEditor.CopyToClipboard;
    end;
  Item.OnUpdate :=
    procedure (Sender: TNSToolbarHelperItem; var EnableItem: Boolean)
    begin
      EnableItem := (memEditor.SelLength > 0);
    end;
  //add a flexible spacer to right align the remaining button
  FHelper.AddFlexibleSpaceItem;
  //add a button onto the end of the toolbar
  Item := FHelper.AddItem('Info');
  Item.SetCaptionImageAndHint('Info', 'Info.png', 'App information');
  Item.OnClick :=
    procedure (Sender: TObject)
    begin
      MessageDlg('FireMonkey NSToolbar Demo', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    end;
  //configure the toolbar appropriately, and attach it to the form - requires XE3 hotfix 1!!!
  {$IF CompilerVersion >= 24}
  FHelper.Toolbar.setAllowsUserCustomization(True);
  FHelper.Toolbar.setAutosavesConfiguration(True);
  {$IFEND}
  FHelper.Toolbar.setSizeMode(NSToolbarSizeModeSmall);
  FHelper.Attach(Self);
end;

procedure TfrmNSToolbarDemo.FormDestroy(Sender: TObject);
begin
  FHelper.Free;
end;

procedure TfrmNSToolbarDemo.itmFileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then memEditor.Lines.LoadFromFile(dlgOpen.FileName);
end;

end.
