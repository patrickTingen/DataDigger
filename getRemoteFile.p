/*------------------------------------------------------------------------

  Name: getRemoteFile.p
  Desc: Return a remotely hosted file as longchar

------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pcRemoteFile AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcContents   AS LONGCHAR  NO-UNDO.

PROCEDURE URLDownloadToFileA EXTERNAL "URLMON.DLL" :
  DEFINE INPUT PARAMETER pCaller    AS LONG.
  DEFINE INPUT PARAMETER szURL      AS CHARACTER.
  DEFINE INPUT PARAMETER szFilename AS CHARACTER.
  DEFINE INPUT PARAMETER dwReserved AS LONG.
  DEFINE INPUT PARAMETER lpfnCB     AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE. /* URLDownloadToFileA */

PROCEDURE DeleteUrlCacheEntry EXTERNAL "WININET.DLL" :
  DEFINE INPUT PARAMETER lbszUrlName AS CHARACTER.
END PROCEDURE. /* DeleteUrlCacheEntry */


/* Main 
*/
DEFINE VARIABLE iResult   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTempFile AS CHARACTER   NO-UNDO.

/* Figure out a temp name */
#GetName:
REPEAT:
  cTempFile = SUBSTITUTE('&1_remote-file-&2.txt', SESSION:TEMP-DIRECTORY, ETIME).
  IF SEARCH(cTempFile) = ? THEN LEAVE #GetName.
END.

/* Download */
RUN DeleteURLCacheEntry (INPUT pcRemoteFile).
RUN urlDownloadToFileA (0, pcRemoteFile, cTempFile, 0, 0, OUTPUT iResult).

/* Read */
IF SEARCH(cTempFile) <> ? THEN COPY-LOB FILE cTempFile TO pcContents.
pcContents = TRIM(pcContents).

/* Cleanup */
FINALLY:
  OS-DELETE VALUE(cTempFile).
END FINALLY.
