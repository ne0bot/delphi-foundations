unit MainForm;
{
  Plugins are put in the same directory as the executable, and must have the word
  'Plugin' in their file name. Each plugin may then implement one or more 'frames'
  which become one or more tabs on this form.

  A plugin 'frame' is a form that implements the IPluginFrame interface, as
  defined in the PluginIntf unit. RTTI is used to discover what frames a plugin
  provides - in itself, a plugin need not explicitly export anything.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.TabControl;

type
  TfrmMain = class(TForm)
    TabControl: TTabControl;
    TabItem1: TTabItem;
    lsbPlugins: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lsbPluginsChangeCheck(Sender: TObject);
  strict private
    procedure LoadPluginList;
    function LoadPluginTabs(const AFileName: string): HMODULE;
  end;

var
  frmMain: TfrmMain;

implementation

uses System.Rtti, PluginIntf, PluginManager;

{$R *.fmx}

type
  TCustomFormClass = class of TCustomForm;

function IsPluginFrameClass(AType: TRttiType; var AFormClass: TCustomFormClass): Boolean;
var
  LClass: TClass;
begin
  if not (AType is TRttiInstanceType) then Exit(False);
  LClass := TRttiInstanceType(AType).MetaclassType;
  Result := LClass.InheritsFrom(TCustomForm) and Supports(LClass, IPluginFrame);
  if Result then
    AFormClass := TCustomFormClass(LClass);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  LoadPluginList;
end;

procedure TfrmMain.lsbPluginsChangeCheck(Sender: TObject);
var
  Item: TListBoxItem;
begin
  Item := (Sender as TListBoxItem);
  if Item.IsChecked then
    Item.Tag := NativeInt(LoadPluginTabs(Item.TagString))
  else
  begin
    UnloadPlugin(HMODULE(Item.Tag));
    TabControl.Realign;
  end;
end;

procedure TfrmMain.LoadPluginList;
var
  Description: string;
  NewItem: TListBoxItem;
  Path: string;
  SearchRec: TSearchRec;
begin
  Path := ExtractFilePath(ParamStr(0));
  if FindFirst(Path + '*Plugin*', 0, SearchRec) <> 0 then Exit;
  try
    repeat
      if TryGetPluginDescription(Path + SearchRec.Name, Description) then
      begin
        NewItem := TListBoxItem.Create(lsbPlugins);
        NewItem.Text := Description;
        NewItem.TagString := Path + SearchRec.Name;
        lsbPlugins.AddObject(NewItem);
      end;
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
  lsbPlugins.Sorted := True;
end;

function TfrmMain.LoadPluginTabs(const AFileName: string): HMODULE;
var
  Context: TRttiContext;
  Frame: TCustomForm;
  FrameClass: TCustomFormClass;
  LType: TRttiType;
  Package: TRttiPackage;
  Tab: TTabItem;
begin
  Result := LoadPlugin(AFileName);
  try
    { Cycle through the RTTI system's packages list to find the one we've just loaded. }
    for Package in Context.GetPackages do
      if Package.Handle = Result then
      begin
        { Cycle through the package's types looking for implementors of the
          IPluginFrameinterface defined in the shared package. }
        for LType in Package.GetTypes do
          if IsPluginFrameClass(LType, FrameClass) then
          begin
            { For each frame, create a new tab to host its contents. In the case of
              a VCL application, we could require an actual TFrame object, or failing
              that, embed the form directly. FireMonkey has neither frames proper nor
              supports embedded forms, so instead we ask the implementing form to
              nominate a base control that will get embedded. }
            Tab := TTabItem.Create(TabControl);
            Frame := FrameClass.Create(Tab);
            Tab.Text := ' ' + Frame.Caption;
            (Frame as IPluginFrame).GetBaseControl.Parent := Tab;
            { Associate the tab with the plugin - since it owns the 'frame' form,
              and that form owns its own components, freeing the tab will have the
              effect of freeing all the actual plugin objects too. }
            RegisterPluginComponent(Result, Tab);
            TabControl.AddObject(Tab);
            Tab.Width := Tab.Canvas.TextWidth(Tab.Text + 'w');
          end;
        Break;
      end;
  except
    UnloadPlugin(Result);
    raise;
  end;
end;

end.
