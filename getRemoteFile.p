/*------------------------------------------------------------------------

  Name: getRemoteFile.p
  Desc: Return a remotely hosted file as longchar

------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcRemoteFile AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcContents   AS LONGCHAR  NO-UNDO.

{&_proparse_prolint-nowarn(varusage)}
DEFINE VARIABLE iResult   AS INT64     NO-UNDO.
DEFINE VARIABLE cTempFile AS CHARACTER NO-UNDO.

/* Figure out a temp name */
#GetName:
REPEAT:
  cTempFile = SUBSTITUTE('&1_remote-file-&2.txt', SESSION:TEMP-DIRECTORY, ETIME).
  IF SEARCH(cTempFile) = ? THEN LEAVE #GetName.
END.

/* Download */
RUN DeleteURLCacheEntry (INPUT pcRemoteFile).

{&_proparse_prolint-nowarn(varusage)}
IF SESSION:CPINTERNAL = 'UTF8' 
  THEN RUN urlDownloadToFileW (0, pcRemoteFile, cTempFile, 0, 0, OUTPUT iResult).
  ELSE RUN urlDownloadToFileA (0, pcRemoteFile, cTempFile, 0, 0, OUTPUT iResult).

/* Read */
IF SEARCH(cTempFile) <> ? THEN COPY-LOB FILE cTempFile TO pcContents.
pcContents = TRIM(pcContents).

/* Cleanup */
OS-DELETE VALUE(cTempFile).
