unit NotebookTests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TNotebookTests = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    [TestCase('Page 1234','<b>This is the content of the file</b>')]
    procedure LoadsFileCorrectly(const AFilename: string; const AFileContent: string);
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('Page 1234','<b>This is the content of the file</b>')]
    [TestCase('Page 5678','Here is some file content')]
    procedure SavesFileCorrectly(const AFilename: string; const AFileContent: string);
  end;

implementation

procedure TNotebookTests.Setup;
begin

end;

procedure TNotebookTests.TearDown;
begin

end;

procedure TNotebookTests.LoadsFileCorrectly(const AFilename: string; const AFileContent: string);
begin

end;

procedure TNotebookTests.SavesFileCorrectly(const AFilename: string; const AFileContent: string);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TNotebookTests);

end.
