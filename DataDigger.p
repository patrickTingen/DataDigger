/*------------------------------------------------------------------------
    File        : DataDigger.p
    Purpose     : Launcher for DataDigger in Edit mode
  ----------------------------------------------------------------------*/

DEFINE VARIABLE gcProgramDir AS CHARACTER NO-UNDO.

/* Where are we running from? */
/* gcProgramDir = SUBSTRING(THIS-PROCEDURE:FILE-NAME,1,R-INDEX(THIS-PROCEDURE:FILE-NAME,'\')). */
gcProgramDir = SUBSTRING(REPLACE(THIS-PROCEDURE:FILE-NAME,"\","/"),1,R-INDEX(THIS-PROCEDURE:FILE-NAME,'/')).

/* Start the actual DataDigger program */
RUN VALUE(gcProgramDir + "DataDigger2.p") (INPUT FALSE).