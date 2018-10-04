/*------------------------------------------------------------------------

  Name: getVersionInfo.p
  Desc: Give back latest buildnr from DataDigger on GitHub

------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcBranch  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcBuildNr AS CHARACTER NO-UNDO.

IF LOOKUP(pcBranch,'master,develop') = 0 THEN RETURN.

RUN getRemoteFile.p
  ( INPUT  SUBSTITUTE('https://raw.githubusercontent.com/patrickTingen/DataDigger/&1/build.i', pcBranch)
  , OUTPUT pcBuildNr
  ).
  