unit NotebookTests;

interface

uses
  DUnitX.TestFramework, FNotebook;

type
  [TestFixture]
  TNotebookTests = class
  private
    notebook: TNotebook;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure LoadsFileCorrectly;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    procedure SavesFileCorrectly;
  end;

implementation

uses
  System.SysUtils;

procedure TNotebookTests.Setup;
begin
  notebook := TNotebook.Create(nil);
end;

procedure TNotebookTests.TearDown;
begin
  FreeAndNil(notebook);
end;

procedure TNotebookTests.LoadsFileCorrectly;
begin

end;

procedure TNotebookTests.SavesFileCorrectly;
var
  fileName: string;
begin
  fileName := notebook.PutPage(9, 0, 'test string');
  Assert.AreNotEqual('', fileName, false, 'Did not return a filename, file was not saved.');
end;

initialization
  TDUnitX.RegisterTestFixture(TNotebookTests);

end.
