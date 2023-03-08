unit MPublisher;

interface

uses Classes;

type

  TPublisher = class(TObject)
  private
    sites: TStringList;
  public
{    procedure SetNotebookProvider();
    function AddOrUpdateSite(ASiteName: string;
                     AHost: string;
                     AUsername: string;
                     APassword: string): boolean;
    function DeleteSite(ASiteName: string): boolean;
    function PublishToSite(ASiteName: string): boolean;
}
  end;

implementation

end.
