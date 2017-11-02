&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame
/*------------------------------------------------------------------------

  Name: dCloneDatabase.w
  Desc: Create an empty copy of a database

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ DataDigger.i }

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER pcDatabase AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER pcOptions  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcNewDatabase AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiDir Btn_OK Btn_Cancel tgConnect ~
btnChooseDumpFile fiLabel
&Scoped-Define DISPLAYED-OBJECTS fiDir tgConnect fiLabel

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnChooseDumpFile
     LABEL "..."
     SIZE-PIXELS 20 BY 21.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY
     LABEL "Cancel"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO
     LABEL "OK"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE fiDir AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     SIZE-PIXELS 362 BY 21 TOOLTIP "the dir where you want to create the clone database" NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U
      VIEW-AS TEXT
     SIZE-PIXELS 358 BY 19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX
     SIZE-PIXELS 410 BY 110.

DEFINE VARIABLE tgConnect AS LOGICAL INITIAL YES
     LABEL "&Connect after cloning"
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 213 BY 17 TOOLTIP "connect the database after cloning" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiDir AT Y 28 X 2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     Btn_OK AT Y 77 X 245
     Btn_Cancel AT Y 77 X 325
     tgConnect AT Y 52 X 12 WIDGET-ID 10
     btnChooseDumpFile AT Y 28 X 380 WIDGET-ID 8
     fiLabel AT Y 9 X 2 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     RECT-1 AT Y 0 X 0 WIDGET-ID 4
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
         SIDE-LABELS NO-UNDERLINE THREE-D
         SIZE-PIXELS 423 BY 146
         TITLE "Create an empty Clone Database"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Create an empty Clone Database */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnChooseDumpFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChooseDumpFile Dialog-Frame
ON CHOOSE OF btnChooseDumpFile IN FRAME Dialog-Frame /* ... */
DO:
  DEFINE VARIABLE cDir AS CHARACTER  NO-UNDO.

  cDir = fiDir:screen-value.

  SYSTEM-DIALOG GET-DIR cDir
    INITIAL-DIR cDir
    RETURN-TO-START-DIR.

  DO WITH FRAME {&frame-name}:
    fiDir:screen-value = cDir.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:

  /* Save settings */
  setRegistry("DataDigger","CloneDB:dir" ,fiDir:screen-value).
  setRegistry("DataDigger","CloneDB:connect",STRING(tgConnect:checked)).

  /* Create full folder structure */
  RUN createFolder(fiDir:SCREEN-VALUE).

  RUN cloneDatabase
    ( pcDatabase          /* database to clone */
    , fiDir:SCREEN-VALUE  /* target folder */
    , tgConnect:CHECKED   /* connect after cloning */
    , OUTPUT pcNewDatabase
    ).

  /* Clear cache files for the new database to avoid
   * messages about restarting DataDigger
   */
  RUN clearCache(pcNewDatabase).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN initializeObject.
  RUN enable_UI.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearCache Dialog-Frame
PROCEDURE clearCache :
  /* Delete old cache files of the newly created database.
  *
  * Note: If you create a local db with a name that has been used before, DD
  *       will see a difference in the schema. To avoid this, remove old cache
  */
  DEFINE INPUT PARAMETER pcDbName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cFile AS CHARACTER NO-UNDO EXTENT 3.

  PUBLISH "debugInfo" (3, SUBSTITUTE("Clearing disk cache")).

  INPUT FROM OS-DIR(getProgramdir() + "cache").
  REPEAT:
    IMPORT cFile.
    IF cFile[1] MATCHES SUBSTITUTE('db.&1.*.xml', pcDbName) THEN OS-DELETE VALUE( cFile[2]).
  END.
  INPUT CLOSE.

END PROCEDURE. /* clearCache */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cloneDatabase Dialog-Frame
PROCEDURE cloneDatabase :
  /* Clone the current database
  */
  DEFINE INPUT PARAMETER pcDbName        AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcFolder        AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER plStayConnected AS LOGICAL     NO-UNDO.
  DEFINE OUTPUT PARAMETER pcLogicalName  AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cCommand     AS CHARACTER   NO-UNDO EXTENT 5.
  DEFINE VARIABLE cCmd         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDlc         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lDelete      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cDf          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hProc        AS HANDLE      NO-UNDO.

  IF NUM-DBS = 0 THEN DO:
    MESSAGE 'No databases connected' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
  END.

  IF CONNECTED(pcDbName) <> TRUE THEN DO:
    MESSAGE 'Database' pcDbName 'not connected' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN.
  END.

  cDlc = OS-GETENV('DLC':U).
  IF cDlc = ? THEN
    ASSIGN cDlc = SEARCH('empty.db':U)
           cDlc = SUBSTRING(cDlc,1,LENGTH(cDlc,'CHARACTER':U) - 9,'CHARACTER':U).

  /* Point to the proper database */
  CREATE ALIAS 'dictdb' FOR DATABASE VALUE(pcDbName).

  /* Exist? */
  IF SEARCH(SUBSTITUTE('&1\&2.db', pcFolder, pcDbName)) <> ? THEN
  DO:
    MESSAGE 'Database already exists. Replace?'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lDelete.
    IF lDelete <> TRUE THEN RETURN.

    /* Delete existing database */
    cCommand[1] = SUBSTITUTE('cd /d &1', pcFolder).
    cCommand[2] = SUBSTITUTE('echo y|&1\bin\prodel &2', cDlc, pcDbName).
    cCmd = SUBSTITUTE('&1 && &2', cCommand[1], cCommand[2]).
    OS-COMMAND SILENT VALUE(cCmd).
  END.

  /* If the new db is already connected, disconnect it first, before
   * we are going to replace it.
   */
  pcLogicalName = SUBSTITUTE('my&1&2', CAPS(SUBSTRING(pcDbName,1,1)), LOWER(SUBSTRING(pcDbName,2))).
  IF CONNECTED(pcLogicalName) THEN DISCONNECT VALUE(pcLogicalName).

  /* Create structure */
  RUN createStructureFile(pcDbName,pcFolder).

  /* Create empty db */
  cCommand[1] = SUBSTITUTE('cd /d &1', pcFolder).
  cCommand[2] = SUBSTITUTE('&1\bin\prostrct create &2', cDlc, pcDbName).
  cCommand[3] = SUBSTITUTE('&1\bin\procopy &1\empty &2', cDlc, pcDbName).
  cCmd = SUBSTITUTE('&1 && &2 && &3', cCommand[1], cCommand[2], cCommand[3]).
  OS-COMMAND SILENT VALUE(cCmd).

  /* Do a silent dump df of old db */
  cDf = SUBSTITUTE('&1\&2.df', pcFolder, pcDbName).
  RUN prodict/dump_df.p PERSISTENT SET hProc (INPUT 'ALL', INPUT cDf, INPUT '1252').
  RUN setSilent IN hProc(YES) NO-ERROR. /* setSilent not avail in all versions */
  RUN doDump IN hProc.
  DELETE PROCEDURE hProc.

  /* Connect new db */
  CONNECT VALUE(SUBSTITUTE('-db &1\&2.db -ld &3 -1', pcFolder, pcDbName, pcLogicalName)).

  /* Load in new db */
  CREATE ALIAS 'dictdb' FOR DATABASE VALUE(pcLogicalName).
  RUN prodict/load_df.p (INPUT SUBSTITUTE('&1\&2.df', pcFolder, pcDbName)).

  /* Let it remain connected or not */
  IF NOT plStayConnected THEN DISCONNECT VALUE(pcLogicalName).

END PROCEDURE. /* cloneDatabase */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createStructureFile Dialog-Frame
PROCEDURE createStructureFile :
  /* Create a .st file for the currently connected database
  */
  DEFINE INPUT PARAMETER pcDbName AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcFolder   AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.

  CREATE BUFFER hBuffer FOR TABLE SUBSTITUTE('&1._Area',pcDbName).
  IF NOT VALID-HANDLE(hBuffer) THEN NEXT.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE('FOR EACH _Area WHERE (_Area-type = 3 AND _Area-number > 1) OR  (_Area-type = 6 AND _Area-number > 1)').
  IF NOT hQuery:QUERY-OPEN THEN RETURN.

  OUTPUT TO VALUE(SUBSTITUTE('&1\&2.st',pcFolder, pcDbName)).
  PUT UNFORMATTED
      '#' SKIP
      '# Structure file for database ' pcDbName SKIP
      '# Generated from DataDigger on ' STRING(TODAY) SKIP
      .

  #Area:
  REPEAT:
    hQuery:GET-NEXT(NO-LOCK).
    IF hQuery:QUERY-OFF-END THEN LEAVE #Area.

    IF hBuffer::_Area-type = 3 THEN
      PUT UNFORMATTED '#' SKIP
                      'b .' SKIP.
    ELSE
      PUT UNFORMATTED '#' SKIP
                      SUBSTITUTE( 'd "&1":&2,&3 .'
                                , hBuffer::_Area-name
                                , hBuffer::_Area-number
                                , hBuffer::_Area-clustersize)
                                SKIP.
  END.
  OUTPUT CLOSE.
  DELETE OBJECT hBuffer NO-ERROR.

END PROCEDURE. /* createStructureFile */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiDir tgConnect fiLabel
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 fiDir Btn_OK Btn_Cancel tgConnect btnChooseDumpFile fiLabel
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame
PROCEDURE initializeObject :
  /* Initialize global vars
  */
  DEFINE VARIABLE iOption  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cOption  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSetting AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    /* Set default font */
    FRAME {&frame-name}:font = getFont('Default').

    /* set title */
    fiLabel = SUBSTITUTE('Create an empty clone of database &1 in', pcDatabase).
    DISPLAY fiLabel.

    /* Process startup options */
    DO iOption = 1 TO NUM-ENTRIES(pcOptions):
      cOption  = ENTRY(iOption,pcOptions).
      cSetting = ENTRY(1,cOption,"=").
      cValue   = ENTRY(2,cOption,"=").

      CASE cSetting:
        WHEN "x" THEN FRAME {&FRAME-NAME}:x = INTEGER(cValue).
        WHEN "y" THEN FRAME {&FRAME-NAME}:y = INTEGER(cValue).
      END CASE.
    END.

    fiDir = getRegistry("DataDigger","CloneDB:dir").
    IF fiDir = ? THEN fiDir = SESSION:TEMP-DIRECTORY.

    cSetting = getRegistry("DataDigger","CloneDB:connect").
    IF cSetting = ? THEN cSetting = "yes".
    tgConnect = LOGICAL(cSetting).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

