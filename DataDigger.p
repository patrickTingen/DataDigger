/*------------------------------------------------------------------------

  File : DataDigger.p
  Desc : Launcher for DataDigger in Edit mode

  ----------------------------------------------------------------------*/

DEFINE VARIABLE cProgramDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDictDb     AS CHARACTER NO-UNDO.

/* Where are we running from? */
FILE-INFO:FILE-NAME = THIS-PROCEDURE:FILE-NAME.
IF FILE-INFO:FULL-PATHNAME = ? THEN
  FILE-INFO:FILE-NAME = REPLACE(THIS-PROCEDURE:FILE-NAME, '.p', '.r').

cProgramDir = REPLACE(FILE-INFO:FULL-PATHNAME,"\","/").
cProgramDir = SUBSTRING(cProgramDir,1,R-INDEX(cProgramDir,'/')).

/* Save dictdb alias */
IF NUM-DBS > 0 THEN cDictDb = LDBNAME('dictdb').

/* Start the actual DataDigger program */
RUN VALUE(cProgramDir + "DataDigger2.p") (INPUT FALSE).

/* Restore dictdb to avoid ADM errors */
IF NUM-DBS > 0 THEN 
DO:
  IF cDictDb = ? THEN 
    DELETE ALIAS dictdb.
  ELSE 
  IF CONNECTED(cDictDb) THEN
    CREATE ALIAS dictdb FOR DATABASE VALUE(cDictDb).
END.
  
