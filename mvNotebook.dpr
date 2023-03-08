program mvNotebook;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  FNotebook in 'FNotebook.pas' {Notebook},
  FAbout in 'FAbout.pas' {AboutBox},
  MPublisher in 'MPublisher.pas',
  MNotebook in 'MNotebook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNotebook, Notebook);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
