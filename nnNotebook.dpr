program nnNotebook;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  FNotebook in 'FNotebook.pas' {Notebook},
  CJsonObject in 'CJsonObject.pas' {JsonObjectMv: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNotebook, Notebook);
  Application.Run;
end.
