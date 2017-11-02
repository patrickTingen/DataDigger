/*------------------------------------------------------------------------

  Name: DataReader.p
  Desc: Launcher for DataDigger in ReadOnly mode

  ----------------------------------------------------------------------*/
DEFINE VARIABLE gcProgramDir AS CHARACTER NO-UNDO.

/* Where are we running from? */
FILE-INFO:FILE-NAME = THIS-PROCEDURE:FILE-NAME.
IF FILE-INFO:FULL-PATHNAME = ? THEN
  FILE-INFO:FILE-NAME = REPLACE(THIS-PROCEDURE:FILE-NAME, '.p', '.r').

gcProgramDir = REPLACE(FILE-INFO:FULL-PATHNAME,"\","/").
gcProgramDir = SUBSTRING(gcProgramDir,1,R-INDEX(gcProgramDir,'/')).

/* Start the actual DataDigger program */
RUN VALUE(gcProgramDir + "DataDigger2.p") (INPUT TRUE).
