unit ormbr.driver.link.sqlite;

interface

uses
  Classes;

type
  {$IF CompilerVersion > 23}
  [ComponentPlatformsAttribute(pidWin32 or
                               pidWin64 or
                               pidOSX32 or
                               pidiOSSimulator or
                               pidiOSDevice or
                               pidAndroid)]
  {$IFEND}
  TORMBrDriverLinkSQLite = class(TComponent)
  end;

implementation

end.
