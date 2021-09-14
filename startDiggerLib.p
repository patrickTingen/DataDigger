/*------------------------------------------------------------------------

  Name: startDiggerLib.p
  Desc: Start DiggerLib if it has not already been started

  ----------------------------------------------------------------------*/

DEFINE VARIABLE hDiggerLib AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCustomLib AS HANDLE    NO-UNDO.

/* Call out to see if the libraries have been started 
*/
PUBLISH 'DataDiggerLib' (OUTPUT hDiggerLib).

IF NOT VALID-HANDLE(hDiggerLib) THEN
DO:
  /* Start main library 
  */
  RUN DataDiggerLib.p PERSISTENT SET hDiggerLib.
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
