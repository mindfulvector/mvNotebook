program mvNotebook;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  FNotebook in 'FNotebook.pas' {Notebook},
  CJsonObject in 'CJsonObject.pas' {JsonObjectMv: TFrame},
  FAbout in 'FAbout.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNotebook, Notebook);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.