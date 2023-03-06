unit FNotebook;

interface

uses
  htmlcomp, htmlpars,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, IOUtils,
  htmledit, System.ImageList,
  Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ActnCtrls, Vcl.Controls, Vcl.Tabs,
  Vcl.Graphics, Vcl.Forms, Vcl.Dialogs,
  System.Generics.Collections,
  VCL.TMSFNCTypes, JvExComCtrls, JvToolBar, Vcl.StdCtrls, JvExStdCtrls,
  JvHtControls, Vcl.FileCtrl, FlCtrlEx,
  clRamLog, clRichLog, Vcl.Menus, AdvMemo, advmjson,
  Chilkat_v9_5_0_TLB;

type
  ZeroBasedInteger = integer;
  OneBasedInteger = integer;

  TNotebook = class(TForm)
    HtTabSet1: THtTabSet;
    HtmlEditor1: THtmlEditor;
    tSave: TTimer;
    actman: TActionManager;
    actPageNext: TAction;
    actPagePrev: TAction;
    ActionToolBar2: TActionToolBar;
    actNotebookNext: TAction;
    Action3: TAction;
    unvis: TPanel;
    imgcol: TImageCollection;
    imglst: TVirtualImageList;
    StatusBar1: TStatusBar;
    actPageExport: TAction;
    FileListBoxEx1: TFileListBoxEx;
    filePopupMenu: THtPopupMenu;
    actPageRename: TAction;
    Rename1: TMenuItem;
    actPageDelete: TAction;
    Delete1: TMenuItem;
    lstPages: TListBox;
    actGlobalAbout: TAction;
    actNotebookRename: TAction;
    lstNotebookNames: TListBox;
    actPageSave: TAction;
    Save1: TMenuItem;
    NextTab1: TMenuItem;
    N1: TMenuItem;
    exportDlg: TSaveDialog;
    importDlg: TOpenDialog;
    actPageImport: TAction;
    ImportPage1: TMenuItem;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure HtTabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure actTabsNextExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tSaveTimer(Sender: TObject);
    procedure actPagePrevExecute(Sender: TObject);
    procedure actPageNextExecute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure actNotebookNextExecute(Sender: TObject);
    procedure actPageExportExecute(Sender: TObject);
    procedure HtmlEditor1UrlClick(Sender: TElement);
    procedure FileListBoxEx1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure actPageRenameExecute(Sender: TObject);
    procedure actPageDeleteExecute(Sender: TObject);
    procedure lstPagesClick(Sender: TObject);
    procedure lstPagesDblClick(Sender: TObject);
    procedure lstPagesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstPagesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actGlobalAboutExecute(Sender: TObject);
    procedure actNotebookRenameExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstPagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actPageSaveExecute(Sender: TObject);
    procedure actPageImportExecute(Sender: TObject);
  private
    { Private declarations }
    metaFile: string;
    bDebugMode: boolean;
    zbiCurrentNotebook: ZeroBasedInteger;
    zbiCurrentPage: ZeroBasedInteger;
    AppDataDir: string;
    richLog: TRichLog;
    ramLog: TRamLog;
    loadingMode: string;
    notebookNames: TStringList;
    procedure Log(message: string; alsoShowMessage: boolean = false);
    procedure LoadPage(zbiNewNotebook: ZeroBasedInteger;
                       zbiNewPage: ZeroBasedInteger = 0);
    function GetNotebookDir(zbiNotebook: ZeroBasedInteger = -1): string;
    function GetPageFilename(zbiPage: ZeroBasedInteger): string;
    procedure SavePage;
    procedure NextPage;
    procedure PrevPage;
    procedure ExportPage;
    procedure ImportPage;
    procedure LaunchBrowser(URL: string);
    procedure RefreshPagesListBox(findFilename: string = '<:NONE:>');
    procedure EnterLoadingMode(modeName: string);
    procedure LeaveLoadingMode(modeName: string);
//    function GetJsonMeta(var json: HCkJsonObject; fieldName: string; var success: boolean): string;
//    procedure SetJsonMeta(json: HCkJsonObject; fieldName: string; fieldValue: string);
    procedure LoadNotebookNames;
  public
    { Public declarations }
  end;

var
  Notebook: TNotebook;

implementation

{$R *.dfm}

uses ShellApi, System.StrUtils, System.DateUtils, System.JSON, FAbout;

resourcestring
  StrUnableToLoadChikc = 'Unable to load chikcat.dll, error recieved was: "%s"';
  StrDefaultPage1_Begin = '<html><body><p><b><span style="color: #1F497D">';
  StrDefaultPage1_Line2 = '<p>Welcome to the mindfulvector Notebook program!</span></b>&nbsp;<br/><small>aka <b>mvNotebook</b>!</small></p>';
  StrDefaultPage1_Line3 = '<p>Each Notebook tab above is a directory under Documents\mvNotebook. '+
    'Each one can contain unlimited pages.</p><p>It currently can switch between 10 notebooks, '+
    'auto saves every 5 minutes as well as on tab change and on close.</p>';
  StrDefaultPage1_Line4 = '<p>Rich text formatting is fairly robust, and '+
  'accessed by selecting a piece of text then using the '+
  '<span style="background-color: #FFFF00">popup</span>.</p>';
  StrDefaultPage1_Line5 = '<p>Additional features will be added soon, such as '+
  'file exporting, syncronization, and more.</p><p>&nbsp;</p>';
  StrDefaultPage1_Line6 = '<p style="text-align:justify;">Example: '+
  '<span style="background-color: #FFFF00">Select this text</span> with your '+
  'mouse to see the formatting toolbar</p><p>&nbsp;</p>';
  StrDefaultPage1_End = '</body></html>';

const STATUSPANEL_STATE: integer = 0;
const STATUSPANEL_CURRENT_LOCATION: integer = 1;

procedure TNotebook.actPageDeleteExecute(Sender: TObject);
var
  currentFilename, newFilename: string;
  buttonSelected : Integer;
begin
  Log('actPageDeleteExecute ====================================================');
  if lstPages.ItemIndex = -1 then
  begin
    Log('No file selected, ItemIndex is -1');
    Exit;
  end;

  EnterLoadingMode('actPageDelete');
  currentFilename := GetPageFilename(lstPages.ItemIndex);
  buttonSelected := MessageDlg('Do you want to delete the file: "' + currentFilename + '" from the current notebook?',
    mtCustom, [mbYes, mbNo], 0);

  if buttonSelected = mrYes then
  begin
    currentFilename := GetNotebookDir + currentFilename;
    if DeleteFile(currentFilename) then
    begin
      if lstPages.ItemIndex > 0 then
        lstPages.ItemIndex := lstPages.ItemIndex - 1
      else if lstPages.Count > 0 then
          lstPages.ItemIndex := lstPages.ItemIndex + 1
        else
          lstPages.ItemIndex := -1;

      Log(Format('lstPages.ItemIndex( new value )=%d', [lstPages.ItemIndex]));

      HtmlEditor1.Visible := false;
      Log('File has been deleted!', true);
    end else begin
      Log('Unable to delete file "'+currentFilename+'"', true);
    end;
  end;

  LeaveLoadingMode('actPageDelete');
  if lstPages.ItemIndex > -1 then
    LoadPage(zbiCurrentNotebook, lstPages.ItemIndex)
  else
    LoadPage(zbiCurrentNotebook, 0);

  RefreshPagesListBox;

  // Mismatch in UI, so just hide the editor until user selects page they
  // want
  HtmlEditor1.Visible := false;
end;

procedure TNotebook.actPageExportExecute(Sender: TObject);
begin
  Log('actPageExportExecute ====================================================');
  ExportPage;
end;

procedure TNotebook.actPageNextExecute(Sender: TObject);
begin
  Log('actPageNextExecute ======================================================');
  NextPage;
end;

procedure TNotebook.actPagePrevExecute(Sender: TObject);
begin
  Log('actPagePrevExecute ======================================================');
  PrevPage;
end;

procedure TNotebook.actPageRenameExecute(Sender: TObject);
var
  currentFilename, newFilenameBasename, newFileFullPath: string;
begin
  Log('actPageRenameExecute ====================================================');
  currentFilename := GetPageFilename(lstPages.ItemIndex);
  newFilenameBasename := InputBox('Rename page', 'Enter new name:', currentFilename);
  newFilenameBasename := newFilenameBasename.Trim;
  if (newFilenameBasename.Length > 0) and (newFilenameBasename <> currentFilename)  then
  begin
    if not EndsStr('.html', newFilenameBasename) then
      newFilenameBasename := newFilenameBasename + '.html';

    currentFilename := GetNotebookDir + currentFilename;
    newFileFullPath := GetNotebookDir + newFilenameBasename;
    Log('Rename file "'+currentFilename+'" to "'+newFileFullPath+'"');
    if RenameFile(currentFilename, newFileFullPath) then
    begin
      Log('File renamed successfully');
      RefreshPagesListBox(newFilenameBasename);
    end else begin
      Log('Unable to rename file "'+currentFilename+'" to "'+newFileFullPath+'"', true);
    end;
  end;
end;

procedure TNotebook.actPageSaveExecute(Sender: TObject);
begin
  SavePage;
end;

procedure TNotebook.actGlobalAboutExecute(Sender: TObject);
begin
  AboutBox.ShowModal();

//  ShowMessage('soNotebook created by Stone Orb Software. '+#13#10+
//  'Copyright 2023. All rights reserved. '+#13#10+
//  #13#10+
//  'The source code for this program is open source, '+#13#10+
//  'but this binary file is not!'+#13#10+
//  #13#10+
//  'This program is the work of a single individual, '+#13#10+
//  'mindfulvector, who asks for your help to earn a '+#13#10+
//  'living if you have found this software useful.'+#13#10+
//  #13#10+
//  'Please do not share this file, and instead direct '+#13#10+
//  'interested persons to the itch.io site where they '+#13#10+
//  'can purchase their own copy for a very small amount. '+#13#10+
//  #13#10+
//  'Thank you for your support!');
end;

procedure TNotebook.actPageImportExecute(Sender: TObject);
begin
  if importDlg.Execute then
  begin
    if not CopyFile(PChar(importDlg.FileName),  PChar(GetNotebookDir+'\' + ExtractFileName(importDlg.FileName)), true) then
      ShowMessage('File not imported; does it already exist or was the source disk removed?')
    else
      RefreshPagesListBox;
  end;
end;

procedure TNotebook.Action3Execute(Sender: TObject);
begin
  Log('Action3Execute ==========================================================');
  HtTabSet1.SelectNext(true);
end;

procedure TNotebook.actNotebookNextExecute(Sender: TObject);
begin
  Log('actNotebookNextExecute ==================================================');
  SavePage;
  HtTabSet1.SelectNext(false);
end;

procedure TNotebook.actNotebookRenameExecute(Sender: TObject);
var
  newNotebookName: string;
  currNotebookName: string;
begin
  Log('actNotebookRenameExecute ================================================');

  currNotebookName := notebookNames[zbiCurrentNotebook];
  Log('currNotebookName: '+currNotebookName);
  newNotebookName := InputBox('Rename notebook',
    Format('Enter name for notebook #%d', [zbiCurrentNotebook+1]), currNotebookName);
  newNotebookName := newNotebookName.Trim;
  if (newNotebookName.Length > 0) then
  begin
    Log(Format('Rename notebook #%d to "%s"', [zbiCurrentNotebook+1, newNotebookName]));
    notebookNames[zbiCurrentNotebook] := newNotebookName;
    notebookNames.SaveToFile(metaFile);
    LoadNotebookNames;
  end;
end;

procedure TNotebook.actTabsNextExecute(Sender: TObject);
begin
  Log('actTabsNextExecute ======================================================');
  SavePage;
  HtTabSet1.SelectNext(True);
end;

procedure TNotebook.NextPage;
begin
  Log('NextPage ----------------------------------------------------------------');
  SavePage;
  LoadPage(zbiCurrentNotebook, zbiCurrentPage + 1);
end;

procedure TNotebook.ExportPage;
begin
  Log('ExportPage --------------------------------------------------------------');
  exportDlg.InitialDir := ExtractFilePath(metaFile);
  if exportDlg.Execute then
  begin
    HtmlEditor1.SaveToFile(exportDlg.FileName);
  end;
end;

procedure TNotebook.ImportPage;
begin
  Log('ImportPage --------------------------------------------------------------');
  ShowMessage('Import page!');
end;

procedure TNotebook.PrevPage;
begin
  Log('PrevPage ----------------------------------------------------------------');
  SavePage;
  if zbiCurrentPage > 0 then
  begin
    LoadPage(zbiCurrentNotebook, zbiCurrentPage - 1);
  end;
end;

procedure TNotebook.Log(message: string; alsoShowMessage: boolean = false);
begin
  ramLog.AddInfo('[' + DateTimeToStr(Now) + '] ' + message);
  ramLog.SaveAsTextFile(AppDataDir+'log.txt');
  if bDebugMode then
    StatusBar1.Panels[STATUSPANEL_STATE].Text := message;
  if alsoShowMessage then
    ShowMessage(message);
end;

procedure TNotebook.lstPagesClick(Sender: TObject);
begin
  Log('lstPagesClick ===========================================================');
  Log(Format('lstPagesClick ItemIndex=%d', [lstPages.ItemIndex]));
  if loadingMode.Length > 0 then begin
    Log(Format('lstPagesClick ignored due to loading mode "%s"', [loadingMode]));
    Exit;
  end;

  FileListBoxEx1.ItemIndex := lstPages.ItemIndex;
  FileListBoxEx1Change(FileListBoxEx1);
end;

procedure TNotebook.lstPagesDblClick(Sender: TObject);
begin
  Log('lstPagesDblClick =========================================================');
  Log(Format('lstPagesDblClick ItemIndex=%d', [lstPages.ItemIndex]));
  if loadingMode.Length > 0 then begin
    Log(Format('lstPagesDblClick ignored due to loading mode "%s"', [loadingMode]));
    Exit;
  end;
  FileListBoxEx1.ItemIndex := lstPages.ItemIndex;
  FileListBoxEx1Change(FileListBoxEx1);
end;

procedure TNotebook.lstPagesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Log('lstPagesKeyDown =========================================================');
  Log(Format('lstPagesKeyDown( IGNORED! ) ItemIndex=%d', [lstPages.ItemIndex]));
{   Log(Format('lstPagesKeyUp ItemIndex=%d', [lstPages.ItemIndex]));
  FileListBoxEx1.ItemIndex := lstPages.ItemIndex;
  FileListBoxEx1Change(FileListBoxEx1);
}
end;

procedure TNotebook.lstPagesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Log('lstPagesKeyUp ===========================================================');
  if loadingMode.Length > 0 then Exit;
  Log(Format('lstPagesKeyUp ItemIndex=%d', [lstPages.ItemIndex]));
  FileListBoxEx1.ItemIndex := lstPages.ItemIndex;
  FileListBoxEx1Change(FileListBoxEx1);
end;

procedure TNotebook.lstPagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Log('lstPagesMouseDown =======================================================');
  Log(Format('lstPagesMouseDown ItemIndex=%d', [lstPages.ItemIndex]));
  if loadingMode.Length > 0 then begin
    Log(Format('lstPagesMouseDown ignored due to loading mode "%s"', [loadingMode]));
    Exit;
  end;
end;

procedure TNotebook.LoadPage(zbiNewNotebook: ZeroBasedInteger;
                          zbiNewPage: ZeroBasedInteger = 0);
var
  notebookDir: string;
  pageFile: string;
begin
  Log('LoadPage ----------------------------------------------------------------');
  Log(Format('LoadPage(%d, %d)', [zbiNewNotebook, zbiNewPage]));

  // Temporarily set the new notebook and page for directory creation
  zbiCurrentNotebook := zbiNewNotebook;
  zbiCurrentPage := zbiNewPage;

  // Ensure new notebook directory exists
  notebookDir := GetNotebookDir;
  notebookDir := Format('%sNotebook%d\', [AppDataDir, zbiNewNotebook + 1]);
  TDirectory.CreateDirectory(notebookDir);
  Log(Format('LoadPage::notebookDir=%s', [notebookDir]));

  if not FileListBoxEx1.Directory.ToUpper.Equals(notebookDir.ToUpper) then
  begin
    Log('New directory selected, loading file list: "'
        +FileListBoxEx1.Directory.ToUpper + '" != "'
        +notebookDir.ToUpper + '"');
    FileListBoxEx1.Directory := notebookDir;
  end;

  // Determine the page file
  pageFile := notebookDir + GetPageFilename(zbiNewPage);
  Log(Format('LoadPage::pageFile=%s', [pageFile]));

  EnterLoadingMode('LoadPage');

  // Check if the page file we calculated above exists or not, then either
  // load it or clear the editor. On first start of the program, load
  // default text into Notebook 1, Page 1.
  if TFile.Exists(pageFile) then
  begin
    Log(Format('LoadPage::LoadFromFile=%s', [pageFile]));
    HtmlEditor1.LoadFromFile(pageFile);
    HtmlEditor1.Visible := true;
  end
  else
  begin
    Log('LoadPage::No file found, clear editor');
    HtmlEditor1.Visible := true;
    HtmlEditor1.SelectAll;
    HtmlEditor1.DeleteSelection;
    if (zbiNewNotebook = 0) and (zbiNewPage = 0) then
    begin
      Log('LoadPage::Initialize default Notebook 1, Page 1 content');
      HtmlEditor1.LoadFromString(
StrDefaultPage1_Begin+
StrDefaultPage1_Line2+
StrDefaultPage1_Line3+
StrDefaultPage1_Line4+
StrDefaultPage1_Line5+
StrDefaultPage1_Line6+
StrDefaultPage1_End)
    end;

  end;

  Log(Format('LoadPage::Set flags to NORMAL mode: '+
    'zbiCurrentNotebook=%d, zbiCurrentPage=%d',
    [zbiCurrentNotebook, zbiCurrentPage]));

  // Now set current notebook since we have finished loading
  zbiCurrentNotebook := zbiNewNotebook;
  zbiCurrentPage := zbiNewPage;


  Log(Format('LoadPage::lstPages.ItemIndex( currently is )=%d', [lstPages.ItemIndex]));
  FileListBoxEx1.ItemIndex := zbiNewPage;
  lstPages.ItemIndex := zbiNewPage;
  Log(Format('LoadPage::lstPages.ItemIndex( new value is )=%d', [lstPages.ItemIndex]));


  Statusbar1.Panels[STATUSPANEL_CURRENT_LOCATION].Text := (
    Format('Notebook %d, page %d',
      [zbiCurrentNotebook + 1, zbiCurrentPage + 1]));

  Log(Format('LoadPage::Updated statusbar panel #%d text for current page: "%s"',
    [STATUSPANEL_CURRENT_LOCATION,
      Statusbar1.Panels[STATUSPANEL_CURRENT_LOCATION].Text]));

  // We must leave this loading mode because RefreshPagesListBox also
  // uses a loading mode, which would cause a hault when we attempt
  // to leave our loading mode since it would clear the mode flag.
  // We could use a stack of modes instead but we are effectively done
  // anyway with our own loading so we can pass it on to that proc now.
  LeaveLoadingMode('LoadPage');

  if lstPages.ItemIndex > -1 then
    RefreshPagesListBox(lstPages.Items[lstPages.ItemIndex])
  else
    RefreshPagesListBox();



end;

function TNotebook.GetNotebookDir(zbiNotebook: ZeroBasedInteger): string;
begin
  if zbiNotebook = -1 then zbiNotebook := zbiCurrentNotebook;

  // Ensure current notebook dir exists
  Result := Format('%sNotebook%d\', [AppDataDir, zbiNotebook + 1]);
  TDirectory.CreateDirectory(Result);
  Log(Format('GetNotebookDir(zbiNotebook=%d)=%s', [zbiNotebook, Result]));
end;

function TNotebook.GetPageFilename(zbiPage: ZeroBasedInteger): string;
var
  newFile: TextFile;
  fullFilename: string;
begin
  Log(Format('GetPageFilename(zbiPage=%d)', [zbiPage]));
  if (FileListBoxEx1.Count = 0) or (zbiPage > FileListBoxEx1.Count - 1) then
  begin
    Log(Format('Index out of range %d-%d, initialize new page', [0, FileListBoxEx1.Count-1]));
    Result := Format('New Page [%x].html', [HourOf(Now)*86400+MinuteOf(Now)*3600+SecondOf(Now)]);
    fullFilename := GetNotebookDir + Result;
    // Create the file
    AssignFile(newFile, fullFilename);
    Rewrite(newFile);
    CloseFile(newFile);
    Append(newFile);
    CloseFile(newFile);
  end else begin
    Result := FileListBoxEx1.Items[zbiPage];
  end;
  Log(Format('GetPageFilename(zbiPage=%d)=%s', [zbiPage, Result]));
end;

procedure TNotebook.SavePage;
var
  notebookDir: string;
  pageFile: string;
begin
  Log('SavePage ----------------------------------------------------------------');

  Log(Format('SavePage zbiCurrentPage=%d', [zbiCurrentPage]));
  if loadingMode.Length = 0 then
  begin
    if HtmlEditor1.Visible then
    begin
      notebookDir := GetNotebookDir;
      pageFile := notebookDir + GetPageFilename(zbiCurrentPage);
      // Save current page to directory above
      Log(Format('SavePage::notebookDir=%s', [notebookDir]));
      Log(Format('SavePage::pageFile=%s', [pageFile]));
      HtmlEditor1.SavetoFile(pageFile);
    end else begin
      Log('Loading mode "<:DISABLED EDITOR:>" in effect, IGNORE SavePage command');
    end;
  end else begin
    Log(Format('Loading mode "%s" in effect, IGNORE SavePage command', [loadingMode]));
  end;
end;

procedure TNotebook.StatusBar1Click(Sender: TObject);
begin
  Log('StatusBar1Click');
  if richLog.Visible then
  begin
    bDebugMode := false;
    richLog.Visible := false;
    FileListBoxEx1.Visible := false;
    //memNotebookMeta.Visible := false;
  end else begin
    bDebugMode := not bDebugMode;
  end;
  if bDebugMode then
    Log('Debug mode enabled. Click to disable. Double click to show full log view.')
  else
    Log('Debug mode disabled. Click to re-enable.');

end;

procedure TNotebook.StatusBar1DblClick(Sender: TObject);
begin
  Log('StatusBar1DblClick');
  richLog.Visible := true;
  FileListBoxEx1.Visible := true;
  lstNotebookNames.Visible := true;
  bDebugMode := true;
  if richLog.Visible then
    Log('Full log view enabled. Click status bar to disable')
  else
    Log('Full log view disabled.');
end;

procedure TNotebook.LaunchBrowser(URL: string);
begin
  Log('LaunchBrowser');
  URL := StringReplace(URL, '"', '%22', [rfReplaceAll]);
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TNotebook.RefreshPagesListBox(findFilename: string = '<:NONE:>');
var
  zbiCurrentNotebookWas, zbiCurrentPageWas: integer;
  currentPageNameWas: string;
  i: integer;
  bFoundPage: boolean;
begin
  Log(Format('RefreshPagesListBox zbiCurrentNotebook=%d, zbiCurrentPage=%d',
    [zbiCurrentNotebook, zbiCurrentPage]));
  zbiCurrentNotebookWas := zbiCurrentNotebook;
  zbiCurrentPageWas := zbiCurrentPage;
  if zbiCurrentPage > -1 then
    currentPageNameWas := findFilename
  else
    currentPageNameWas := '<:NONE:>';

  EnterLoadingMode('RefreshPagesListBox');

  // First, refresh the backing file list
  // Silly mask that'll never match to
  FileListBoxEx1.Mask := '*.refresh';
  // force a refresh when we set real one
  FileListBoxEx1.Mask := '*.html';
  // Now, refresh the sorted view list
  lstPages.Items := FileListBoxEx1.Items;

  zbiCurrentNotebook := zbiCurrentNotebookWas;

  // Instead of restoring zbiCurrentPageWas, search for the filename we had
  // selected in case it has moved position
  for i := 0 to FileListBoxEx1.Count -1 do
  begin
    if FileListBoxEx1.Items[i].Equals(currentPageNameWas) then
    begin
      Log(Format('Found page "%s" at index %d', [currentPageNameWas, i]));
      zbiCurrentPage := i;
      bFoundPage := true;
      Break;
    end;
  end;
  if bFoundPage then
    lstPages.ItemIndex := zbiCUrrentPage
  else
    Log(Format('Page "%s" was not found in the new pages list. Was deleted?', [currentPageNameWas]));


  Log(Format('RefreshPagesListBox restoring indicies to zbiCurrentNotebook=%d, zbiCurrentPage=%d',
    [zbiCurrentNotebook, zbiCurrentPage]));

  LeaveLoadingMode('RefreshPagesListBox');
end;

procedure TNotebook.EnterLoadingMode(modeName: string);
begin
  // To notify other events that we are loading, set loadingMode
  // so that other events don't accidentally replace something when
  // we start modifying values in a moment
  if not loadingMode.Equals('') then begin
    Log(Format('Mode mismatch while entering loading mode "%s"!'#13#10+
      'Expected "%s" but we are in mode "%s". Program must exit.',
    [modeName, '', loadingMode]), true);
    Halt;
  end;

  loadingMode := modeName;
  Log(Format('<<<<<<<<<< loadingMode="%s"', [loadingMode]));
end;

procedure TNotebook.LeaveLoadingMode(modeName: string);
begin
  if loadingMode.Equals(modeName) then
  begin
    Log(Format('RESET loadingMode="%s" >>>>>', [loadingMode]));
    loadingMode := '';
    Log(Format('loadingMode="%s"', [loadingMode]));
  end else begin
    Log(Format('Mode mismatch while exiting loading mode!'#13#10+
      'Expected "%s" but we are in mode "%s". Program must exit.',
    [modeName, loadingMode]), true);
    Halt;
  end;
end;

//function TNotebook.GetJsonMeta(var json: HCkJsonObject; fieldName: string; var success: boolean): string;
//begin
//  // Load once if the same reference is used
//  if json = nil then
//  begin
//    json := CkJsonObject_Create;
//    CkJsonObject_putEmitCompact(json, False);
//    success := CkJsonObject_Load(json, PChar(memNotebookMeta.Lines.Text));
//    if (success <> True) then
//    begin
//      Log('Error loading JSON meta data:' + CkJsonObject__lastErrorText(json));
//    end;
//  end;
//
//  if json <> nil then
//  begin
//    Result := CkJsonObject__stringOf(json, PChar(fieldName));
//  end;
//end;
//
//procedure TNotebook.SetJsonMeta(json: HCkJsonObject; fieldName: string; fieldValue: string);
//begin
//  CkJsonObject_SetStringOf(json, PChar(fieldName), PChar(fieldValue));
//  CkJsonObject_putEmitCompact(json, False);
//  // If bare-LF line endings are desired, turn off EmitCrLf
//  // Otherwise CRLF line endings are emitted.
//  CkJsonObject_putEmitCrLf(json, False);
//  // Emit the formatted JSON:
//  memNotebookMeta.Lines.Text := CkJsonObject__emit(json);
//  Log(Format('Saving meta data file to "%s"', [metaFile]));
//  memNotebookMeta.SaveToJSONFile(metaFile);
//end;

procedure TNotebook.LoadNotebookNames;
var
  success: Boolean;
  newNotebookName: string;
  currNotebookName: string;
  fieldName: string;
  fieldValue: string;
  i: Integer;
begin
  Log('LoadNotebookNames =======================================================');
  lstNotebookNames.Items := notebookNames;
  // Field to fetch notebook name from in JSON meta object
  for i := 1 to 10 do
  begin
    HtTabSet1.Tabs[i-1] := notebookNames[i-1]
  end;
end;

procedure TNotebook.tSaveTimer(Sender: TObject);
begin
  Log('tSaveTimer');
  SavePage;
end;

procedure TNotebook.FileListBoxEx1Change(Sender: TObject);
begin
  // @TODO: Should this event be on lstPages instead? Not sure...

  // Loading mode is not zero len if we are loading a list of pages or
  // other such thing and should ignore events. ItemIndex is -1 if
  // there is no selection.
  if (loadingMode.Length = 0) and (FileListBoxEx1.ItemIndex > -1) then
  begin
    Log('FileListBoxEx1Change, not ignored');
    if zbiCurrentPage <> FileListBoxEx1.ItemIndex then
    begin
      // This would clobber a file if we had just deleted or renamed one and
      // the sort order changed
      SavePage;
    end;
    LoadPage(zbiCurrentNotebook, FileListBoxEx1.ItemIndex);
  end else begin
    Log('FileListBoxEx1Change, ignored due to loading');
  end;
end;

procedure TNotebook.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Log('FormClose ===============================================================');
  SavePage;
end;

procedure TNotebook.FormCreate(Sender: TObject);
var
  //glob: HCkGlobal;
  success: boolean;
begin
  richLog := TRichLog.Create(self);
  richLog.SetParentComponent(self);
  richLog.Width := 200;
  richLog.Height := 200;
  richLog.ReadOnly := true;
  richLog.Visible := false;
  richLog.Font.Name := 'Consolas';
  richLog.Font.Size := 12;
  ramLog := TRamLog.Create;
  ramLog.RichLog := richLog;

  
  // Crete database directory (needed to write log output)
  AppDataDir := Format('%s\mvNotebook\', [TPath.GetDocumentsPath]);
  TDirectory.CreateDirectory(AppDataDir);

  Log('Starting app');

  // If off, log lines don't appear in status bar
  bDebugMode := false;

  {
  @Todo: Convert to ActiveX activation
  glob := CkGlobal_Create();
  success := CkGlobal_UnlockBundle(glob,'Anything for 30-day trial');
  if (success <> True) then
  begin
    ShowMessage(Format(StrUnableToLoadChikc, [CkGlobal__lastErrorText(glob)]));
    Halt;
  end;
  }
  zbiCurrentNotebook := 0;
  zbiCurrentPage := 0;

  metaFile := AppDataDir + 'notebooks.txt';
  notebookNames := TStringList.Create;
  if FileExists(metaFile) then
  begin
    Log(Format('Loading meta data file from "%s"', [metaFile]));
    notebookNames.LoadFromFile(metaFile);
  end;
  while notebookNames.Count < 10 do
  begin
    notebookNames.Add(Format('Notebook %d', [notebookNames.Count+1]));
  end;
  LoadNotebookNames;
  LoadPage(0);
end;

procedure TNotebook.FormResize(Sender: TObject);
begin
  richLog.Top := StatusBar1.Top - richLog.Height;
  richLog.Width := self.Width;
  FileListBoxEx1.Top := richLog.Top - FileListBoxEx1.Height - 5;
  lstNotebookNames.Top := FileListBoxEx1.Top;
  StatusBar1.Panels[STATUSPANEL_STATE].Width := Round(self.Width * 0.75);
end;

procedure TNotebook.FormShow(Sender: TObject);
begin
  //memNotebookMeta.Visible := false;
end;

procedure TNotebook.HtmlEditor1UrlClick(Sender: TElement);
begin
  Log('HtmlEditor1UrlClick -----------------------------------------------------');
  LaunchBrowser(Sender['href']);
end;

procedure TNotebook.HtTabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  Log('HtTabSet1Change =========================================================');
  SavePage;
  // Takes only the tab to load for now, defaults to page 1
  LoadPage(NewTab);
end;

end.
