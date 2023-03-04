unit CJsonObject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvMemo, advmjson;

type
  TJsonObjectMv = class(TFrame)
    memNotebookMeta: TAdvMemo;
    jsonStyle: TAdvJSONMemoStyler;
  private
    { Private declarations }
    dataFile: string;
    procedure Log(message: string; alsoShowMessage: boolean = false);
  public
    { Public declarations }
    procedure Loaded;
    function SetDataFile(AFile: string): boolean;
    function GetString(APath, ADefault: string): string;
    function SetString(APath, AValue: string): boolean;
  end;

implementation

{$R *.dfm}
uses VCL.TMSFNCTypes, System.StrUtils, JsonObject { Chilkat };

procedure TJsonObjectMv.Loaded;
begin
  ShowMessage('Loaded!');
end;
procedure TJsonObjectMv.Log(message: string; alsoShowMessage: boolean = false);
begin
//  ramLog.AddInfo('[' + DateTimeToStr(Now) + '] ' + message);
//  ramLog.SaveAsTextFile(AppDataDir+'log.txt');
//  if bDebugMode then
//    StatusBar1.Panels[STATUSPANEL_STATE].Text := message;
//  if alsoShowMessage then
//    ShowMessage(message);
end;

function TJsonObjectMv.SetDataFile(AFile: string): boolean;
begin
  if FileExists(AFile) then
  begin
    dataFile := AFile;
    Log(Format('Loading JsonObjectMv from file "%s"', [dataFile]));
    //memNotebookMeta.LoadFromJSONFile(metaFile);
  end;
end;

function TJsonObjectMv.GetString(APath, ADefault: string): string;
var
  json: HCkJsonObject;
  newNotebookName: string;
  currNotebookName: string;
begin
  Result := ADefault;
  json := CkJsonObject_Create();
  CkJsonObject_putEmitCompact(json,False);

  if not CkJsonObject_Load(json, PChar(memNotebookMeta.Lines.Text)) then
  begin
    Log('Error loading JSON meta data:'+CkJsonObject__lastErrorText(json));
    Exit;
  end;

  if CkJsonObject_HasMember(json, PChar(APath)) then
    Result := CkJsonObject__stringOf(json, PChar(APath));
end;

function TJsonObjectMv.SetString(APath, AValue: string): boolean;
var
  json: HCkJsonObject;
  newNotebookName: string;
  currNotebookName: string;
begin
  Result := false;
  json := CkJsonObject_Create();
  CkJsonObject_putEmitCompact(json,False);

  if not CkJsonObject_Load(json, PChar(memNotebookMeta.Lines.Text)) then
  begin
    Log('Error loading JSON meta data:'+CkJsonObject__lastErrorText(json));
    Exit;
  end;

  CkJsonObject_SetStringOf(json, PChar(APath), PChar(AValue));

  CkJsonObject_putEmitCompact(json, false);
  CkJsonObject_putEmitCrLf(json, false);

  memNotebookMeta.Lines.Text := CkJsonObject__emit(json);
    Log(Format('Saving meta data file to "%s"', [dataFile]));

  memNotebookMeta.SaveToJSONFile(dataFile);

  Result := true;
end;

end.
