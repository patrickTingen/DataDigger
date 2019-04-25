&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  Name: dDumpDf.w
  Desc: Dump definitions of table or complete database

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

{ DataDigger.i }

DEFINE INPUT  PARAMETER pcDatabase AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcTable    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcOptions  AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiDir Btn_OK Btn_Cancel tgOpenFile ~
btnChooseDumpFile rsDump 
&Scoped-Define DISPLAYED-OBJECTS fiDir tgOpenFile rsDump 

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
     LABEL "&Folder" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 320 BY 21 TOOLTIP "the dir where you want to dump the .df file to" NO-UNDO.

DEFINE VARIABLE rsDump AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Table [table]", "table",
"&Database [db]", "db",
"&All connected Databases", "all"
     SIZE-PIXELS 307 BY 75 TOOLTIP "what should be dumped" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  
     SIZE-PIXELS 410 BY 205.

DEFINE VARIABLE tgOpenFile AS LOGICAL INITIAL no 
     LABEL "&Open DF after dump" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 195 BY 17 TOOLTIP "open the DF file right after dumping" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiDir AT Y 95 X 50 COLON-ALIGNED WIDGET-ID 2
     Btn_OK AT Y 166 X 245
     Btn_Cancel AT Y 166 X 325
     tgOpenFile AT Y 121 X 60 WIDGET-ID 10
     btnChooseDumpFile AT Y 95 X 380 WIDGET-ID 8
     rsDump AT Y 10 X 60 NO-LABEL WIDGET-ID 12
     "Dump:" VIEW-AS TEXT
          SIZE-PIXELS 40 BY 13 AT Y 15 X 13 WIDGET-ID 16
     RECT-1 AT Y 0 X 0 WIDGET-ID 4
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 425 BY 243
         TITLE "Dump Definitions"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Dump Definitions */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnChooseDumpFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChooseDumpFile Dialog-Frame
ON CHOOSE OF btnChooseDumpFile IN FRAME Dialog-Frame /* ... */
DO:
  DEFINE VARIABLE cDir  AS CHARACTER  NO-UNDO.

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
  DEFINE VARIABLE cDir  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

  /* Create full folder structure */
  RUN createFolder(fiDir:SCREEN-VALUE).
  cDir = RIGHT-TRIM(fiDir:SCREEN-VALUE,"\").

  /* Do the dump, using built in procedure */
  CASE rsDump:SCREEN-VALUE:
    WHEN 'table' THEN RUN DumpDF(pcTable, SUBSTITUTE('&1\&2.df',cDir,pcTable   ), tgOpenFile:CHECKED, INPUT-OUTPUT cList).
    WHEN 'db'    THEN RUN DumpDF('ALL'  , SUBSTITUTE('&1\&2.df',cDir,pcDatabase), tgOpenFile:CHECKED, INPUT-OUTPUT cList).
    WHEN 'all'   THEN DO i = 1 TO NUM-DBS:
                        CREATE ALIAS dictdb FOR DATABASE VALUE(LDBNAME(i)).
                        RUN DumpDF('ALL', SUBSTITUTE('&1\&2.df',cDir,LDBNAME(i)), tgOpenFile:CHECKED, INPUT-OUTPUT cList).
                      END.
  END CASE.
  
  IF tgOpenFile:CHECKED THEN
  DO i = 1 TO NUM-ENTRIES(cList):
    OS-COMMAND NO-WAIT START VALUE(ENTRY(i,cList)).
  END.

  /* Save settings */
  setRegistry("DataDigger:DumpDF","WhatToDump",rsDump:SCREEN-VALUE).
  setRegistry("DataDigger:DumpDF","DumpDir" ,fiDir:SCREEN-VALUE).
  setRegistry("DataDigger:DumpDF","OpenFile",STRING(tgOpenFile:CHECKED)).

  RUN showHelp('DumpCompleted','').
END.

PROCEDURE DumpDF:
    DEFINE INPUT PARAMETER pcWhat  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcFile  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER plOpen  AS LOGICAL     NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER pcList AS CHARACTER NO-UNDO.

    /* suppress 'Dump of definitions completed.' */
    OUTPUT TO nul.

    RUN prodict/dump_df.p(pcWhat, pcFile, '').

    OUTPUT CLOSE. 
    IF plOpen THEN pcList = TRIM(SUBSTITUTE('&1,&2',pcList,pcFile),',').

END PROCEDURE. /* DumpDF */

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

  RUN initObject.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fiDir tgOpenFile rsDump 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 fiDir Btn_OK Btn_Cancel tgOpenFile btnChooseDumpFile rsDump 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject Dialog-Frame 
PROCEDURE initObject :
/* initialize global vars
  */
  DEFINE VARIABLE iOption  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cOption  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSetting AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    /* Set default font */
    FRAME {&FRAME-NAME}:FONT = getFont('Default').

    DO iOption = 1 TO NUM-ENTRIES(pcOptions):
      cOption  = ENTRY(iOption,pcOptions).
      cSetting = ENTRY(1,cOption,"=").
      cValue   = ENTRY(2,cOption,"=").

      CASE cSetting:
        WHEN "x" THEN FRAME {&FRAME-NAME}:X = INTEGER(cValue).
        WHEN "y" THEN FRAME {&FRAME-NAME}:Y = INTEGER(cValue).
      END CASE.
    END.

    /* Set name in radioset */
    rsDump:RADIO-BUTTONS = REPLACE(rsDump:RADIO-BUTTONS,'[table]',pcTable).
    rsDump:RADIO-BUTTONS = REPLACE(rsDump:RADIO-BUTTONS,'[db]',pcDatabase).

    fiDir = getRegistry("DataDigger:DumpDF","DumpDir").
    IF fiDir = ? THEN fiDir = SESSION:TEMP-DIRECTORY.

    cSetting = getRegistry("DataDigger:DumpDF","OpenFile").
    IF cSetting = ? THEN cSetting = "yes".
    tgOpenFile = LOGICAL(cSetting).

    cSetting = getRegistry("DataDigger:DumpDF","WhatToDump").
    IF cSetting = ? THEN cSetting = 'table'.
    rsDump:SCREEN-VALUE = cSetting.
  END.

  RUN enable_UI.

END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME