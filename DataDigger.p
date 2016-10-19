/*------------------------------------------------------------------------
    File        : DataDigger.p
    Purpose     : Launcher for DataDigger in Edit mode
  ----------------------------------------------------------------------*/

DEFINE VARIABLE gcProgramDir AS CHARACTER NO-UNDO.

/* Where are we running from? */
gcProgramDir = THIS-PROCEDURE:FILE-NAME.
gcProgramDir = REPLACE(gcProgramDir,"\","/").
gcProgramDir = SUBSTRING(gcProgramDir,1,R-INDEX(gcProgramDir,'/')).

/* Start the actual DataDigger program */
RUN VALUE(gcProgramDir + "DataDigger2.p") (INPUT FALSE).