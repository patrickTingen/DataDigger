&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  Name: dNewGroup.w
  Desc: Ask name for new group of favourites

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ DataDigger.i }
DEFINE TEMP-TABLE ttTableOk LIKE ttTable.

/* Parameters Definitions --- */

&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT  PARAMETER TABLE FOR ttTable.
  DEFINE OUTPUT PARAMETER TABLE FOR ttTableOk.
&ELSE

  DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
  RUN datadiggerlib.p PERSISTENT SET hLib.
  THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib,SEARCH-TARGET).
  RUN fillTT.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME brTables

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTable

/* Definitions for BROWSE brTables                                      */
&Scoped-define FIELDS-IN-QUERY-brTables ttTable.lFavourite ttTable.cTableName ttTable.cDatabase   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTables   
&Scoped-define SELF-NAME brTables
&Scoped-define QUERY-STRING-brTables FOR EACH ttTable     WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'       AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE)
&Scoped-define OPEN-QUERY-brTables OPEN QUERY {&SELF-NAME}   FOR EACH ttTable     WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'       AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE).
&Scoped-define TABLES-IN-QUERY-brTables ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-brTables ttTable


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brTables}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiTableFilter Btn_OK brTables btnSelectAll ~
btnDeselectAll 
&Scoped-Define DISPLAYED-OBJECTS fiTableFilter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeselectAll 
     LABEL "&Deselect all" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "deselect all tables currently in the browse"
     BGCOLOR 8 .

DEFINE BUTTON btnSelectAll 
     LABEL "Select &all" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "select all tables currently in the browse"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE fiTableFilter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 310 BY 21 TOOLTIP "filter the list of tables" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTables FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTables Dialog-Frame _FREEFORM
  QUERY brTables DISPLAY
      ttTable.lFavourite    COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
ttTable.cTableName    COLUMN-LABEL "Table"
ttTable.cDatabase     COLUMN-LABEL "DB"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 12.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiTableFilter AT Y 10 X 5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     Btn_OK AT Y 265 X 340
     brTables AT ROW 2.67 COL 4 WIDGET-ID 200
     btnSelectAll AT Y 75 X 340 WIDGET-ID 6
     btnDeselectAll AT Y 110 X 340 WIDGET-ID 8
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 435 BY 333
         TITLE "Edit favourites group" WIDGET-ID 100.


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
/* BROWSE-TAB brTables Btn_OK Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTables
/* Query rebuild information for BROWSE brTables
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH ttTable
    WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'
      AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brTables */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Edit favourites group */
DO:

  /* Apply changes */
  RUN copyData.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit favourites group */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTables
&Scoped-define SELF-NAME brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables Dialog-Frame
ON DEFAULT-ACTION OF brTables IN FRAME Dialog-Frame
OR 'ENTER' OF brTables 
OR ' ' OF brTables 
DO:
  ttTable.lFavourite = NOT ttTable.lFavourite.
  BROWSE brTables:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables Dialog-Frame
ON OFF-HOME OF brTables IN FRAME Dialog-Frame
DO:
  APPLY 'ENTRY' TO fiTableFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeselectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeselectAll Dialog-Frame
ON CHOOSE OF btnDeselectAll IN FRAME Dialog-Frame /* Deselect all */
OR 'CTRL-D' OF brTables
DO:
  RUN selectTables(NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll Dialog-Frame
ON CHOOSE OF btnSelectAll IN FRAME Dialog-Frame /* Select all */
OR 'CTRL-A' OF brTables
DO:
  RUN selectTables(YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTableFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTableFilter Dialog-Frame
ON CURSOR-DOWN OF fiTableFilter IN FRAME Dialog-Frame
DO:
  APPLY 'ENTRY' to brTables.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTableFilter Dialog-Frame
ON VALUE-CHANGED OF fiTableFilter IN FRAME Dialog-Frame
DO:
  {&open-query-brTables}
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

  /* Create initial set */
  RUN copyData.

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyData Dialog-Frame 
PROCEDURE copyData :
/* Copy the table data 
*/
  DEFINE BUFFER bTable   FOR ttTable.
  DEFINE BUFFER bTableOk FOR ttTableOk.

  EMPTY TEMP-TABLE bTableOk.

  FOR EACH bTable:
    CREATE bTableOk.
    BUFFER-COPY bTable TO bTableOk.
  END.

END PROCEDURE. /* copyData */

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
  DISPLAY fiTableFilter 
      WITH FRAME Dialog-Frame.
  ENABLE fiTableFilter Btn_OK brTables btnSelectAll btnDeselectAll 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTT Dialog-Frame 
PROCEDURE fillTT :
/* Fill the tt for testing in the UIB
*/
  DEFINE BUFFER bTable       FOR ttTable.
  DEFINE BUFFER bTableFilter FOR ttTableFilter.
  
  RUN getTables(INPUT TABLE bTableFilter, OUTPUT TABLE bTable).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectTables Dialog-Frame 
PROCEDURE selectTables :
/* Select all or none of the tables
*/
  DEFINE INPUT PARAMETER plSelect AS LOGICAL NO-UNDO.

  DEFINE BUFFER bTable FOR ttTable.
  DEFINE VARIABLE rTable     AS ROWID   NO-UNDO.
  DEFINE VARIABLE iBrowseRow AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    IF AVAILABLE ttTable THEN 
    DO:
      rTable = ROWID(ttTable).
      /* Position in browse */
      DO iBrowseRow = 1 TO brTables:NUM-ENTRIES:
        IF brTables:IS-ROW-SELECTED(iBrowseRow) THEN LEAVE. 
      END.
    END.

    FOR EACH bTable 
      WHERE bTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'
        AND (bTable.lShowInList = TRUE OR bTable.lFavourite = TRUE):
      bTable.lFavourite = plSelect.
    END. 
  
    {&open-query-brTables}
  
    IF rTable <> ? THEN 
    DO:
      IF iBrowseRow > 0 THEN brTables:SET-REPOSITIONED-ROW(iBrowseRow,"CONDITIONAL") .      
      brTables:QUERY:REPOSITION-TO-ROWID(rTable).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

