/*------------------------------------------------------------------------

  Name: startDiggerLib.p
  Desc: Start DiggerLib if it has not already been started

  ----------------------------------------------------------------------*/

DEFINE VARIABLE hDiggerLib AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCustomLib AS HANDLE    NO-UNDO.
DEFINE VARIABLE iProcArch  AS INTEGER  NO-UNDO.

/* Call out to see if the libraries have been started 
*/
PUBLISH 'DataDiggerLib' (OUTPUT hDiggerLib).

IF NOT VALID-HANDLE(hDiggerLib) THEN
DO:
  /* Start main library 
  */
  DO ON ERROR UNDO, LEAVE
   ON STOP UNDO, LEAVE:
   /* this file won't compile and won't run on version < 11.3,
      only 11.3 and higher can be 64 bit
      this will give an error or stop condition on versions under 11.3
      catch this error and assume 32 bit as the first 64 bit client is 11.3
   */
   IF SEARCH("getProcessArchitecture.r") <> ?
     THEN RUN 'getProcessArchitecture'(OUTPUT iProcArch) NO-ERROR.
  END.
  /* progress r-files are not bit dependent,
     however calls to the Windows API require other variable types
     select the correct veriabled types by starting the correct file */
  IF iProcArch = 64
      THEN RUN DataDiggerLib64.p PERSISTENT SET hDiggerLib.
      ELSE RUN DataDiggerLib32.p PERSISTENT SET hDiggerLib.
  SESSION:ADD-SUPER-PROCEDURE(hDiggerLib,SEARCH-TARGET).

  /* Populate the ttConfig table. Must only be done when the lib is started 
  ** because it is persistent. A second run would overwrite them. 
  */
  RUN loadSettings. 

  /* Start customizations in myDataDigger.p 
  */
  IF SEARCH('myDataDigger.p') <> ? THEN
  DO:
    RUN myDataDigger.p PERSISTENT SET hCustomLib.
    SESSION:ADD-SUPER-PROCEDURE(hCustomLib, SEARCH-TARGET).
  
    /* Register all hooks */
    SUBSCRIBE PROCEDURE hCustomLib TO "customBorderColor"     ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customDump"            ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customFormat"          ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customFrameColor"      ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customGetFilterValue"  ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customQuery"           ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customSaveFilterValue" ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "customShowField"       ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "DataDigger"            ANYWHERE.
    SUBSCRIBE PROCEDURE hCustomLib TO "query"                 ANYWHERE RUN-PROCEDURE "QueryOpen".
    SUBSCRIBE PROCEDURE hCustomLib TO "setWindowTitle"        ANYWHERE.
  END.

END.
