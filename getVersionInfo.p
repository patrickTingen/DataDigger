/*------------------------------------------------------------------------

  Name: getVersionInfo.p
  Desc: Give back latest versions from DataDigger on GitHub

  Notes:
    The version nr is increased when it is ready for production, the
    build nr is increaded when something is ready for beta testing.
------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcBranch  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcVersion AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcBuildNr AS CHARACTER NO-UNDO.

IF LOOKUP(pcBranch,'master,develop') = 0 THEN RETURN.

RUN getRemoteFile.p(SUBSTITUTE('https://raw.githubusercontent.com/patrickTingen/DataDigger/&1/version.i', pcBranch), OUTPUT pcVersion).
RUN getRemoteFile.p(SUBSTITUTE('https://raw.githubusercontent.com/patrickTingen/DataDigger/&1/build.i'  , pcBranch), OUTPUT pcBuildNr).
