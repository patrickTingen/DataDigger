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

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

{ DataDigger.i }

&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT-OUTPUT PARAMETER pcGroup AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttTable.
  DEFINE INPUT PARAMETER pcGroupList AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER plOk       AS LOGICAL NO-UNDO.
&ELSE

  DEFINE VARIABLE pcGroup     AS CHARACTER NO-UNDO INITIAL 'myFavourites'.
  DEFINE VARIABLE pcGroupList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hLib        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE plOk        AS LOGICAL   NO-UNDO.
  
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
&Scoped-define QUERY-STRING-brTables FOR EACH ttTable     WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'       AND (cbDatabase:SCREEN-VALUE = ? OR ttTable.cDatabase = cbDatabase:SCREEN-VALUE)       AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE)
&Scoped-define OPEN-QUERY-brTables OPEN QUERY {&SELF-NAME}   FOR EACH ttTable     WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'       AND (cbDatabase:SCREEN-VALUE = ? OR ttTable.cDatabase = cbDatabase:SCREEN-VALUE)       AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE).
&Scoped-define TABLES-IN-QUERY-brTables ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-brTables ttTable


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-brTables}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnCancel btnDelete btnEdit fiGroupname ~
fiTableFilter cbDatabase brTables btnSelectAll btnDeselectAll Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS fiGroupname fiTableFilter cbDatabase 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnDelete  NO-FOCUS FLAT-BUTTON
     LABEL "del" 
     SIZE-PIXELS 20 BY 21 TOOLTIP "delete this group".

DEFINE BUTTON btnDeselectAll 
     LABEL "&Deselect all" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "deselect all tables currently in the browse"
     BGCOLOR 8 .

DEFINE BUTTON btnEdit  NO-FOCUS FLAT-BUTTON
     LABEL "edit" 
     SIZE-PIXELS 20 BY 21 TOOLTIP "edit the name of the group".

DEFINE BUTTON btnSelectAll 
     LABEL "Select &all" 
     SIZE-PIXELS 75 BY 24 TOOLTIP "select all tables currently in the browse"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE cbDatabase AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE-PIXELS 125 BY 21 NO-UNDO.

DEFINE VARIABLE fiGroupname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 290 BY 21 NO-UNDO.

DEFINE VARIABLE fiTableFilter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 185 BY 21 TOOLTIP "filter the list of tables" NO-UNDO.

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
    WITH NO-ROW-MARKERS SEPARATORS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 62 BY 12
          &ELSE SIZE-PIXELS 310 BY 255 &ENDIF FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BtnCancel AT Y 270 X 330 WIDGET-ID 12
     btnDelete AT Y 5 X 385 WIDGET-ID 18
     btnEdit AT Y 5 X 295 WIDGET-ID 16
     fiGroupname AT Y 5 X 5 NO-LABEL WIDGET-ID 14
     fiTableFilter AT Y 47 X 5 NO-LABEL WIDGET-ID 2
     cbDatabase AT Y 47 X 180 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     brTables AT Y 70 X 5 WIDGET-ID 200
     btnSelectAll AT Y 137 X 330 WIDGET-ID 6
     btnDeselectAll AT Y 172 X 330 WIDGET-ID 8
     Btn_OK AT Y 300 X 330
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 424 BY 366
         TITLE "Edit favourites group"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brTables cbDatabase Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiGroupname IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fiGroupname:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN fiTableFilter IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTables
/* Query rebuild information for BROWSE brTables
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
  FOR EACH ttTable
    WHERE ttTable.cTableName MATCHES '*' + fiTableFilter:SCREEN-VALUE + '*'
      AND (cbDatabase:SCREEN-VALUE = ? OR ttTable.cDatabase = cbDatabase:SCREEN-VALUE)
      AND (ttTable.lShowInList = TRUE OR ttTable.lFavourite = TRUE).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brTables */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ALT-T OF FRAME Dialog-Frame /* Edit favourites group */
DO:
  APPLY 'ENTRY' TO fiTableFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Edit favourites group */
DO:
  DEFINE VARIABLE lMerge  AS LOGICAL NO-UNDO.

  IF pcGroup <> fiGroupname:SCREEN-VALUE 
    AND CAN-DO(pcGroupList, fiGroupname:SCREEN-VALUE) THEN
  DO:
    MESSAGE 'There is another group with this name. Do you want to merge the groups?'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lMerge.

    IF lMerge <> YES THEN RETURN NO-APPLY.
  END.

  plOk = TRUE.
  pcGroup = fiGroupname:SCREEN-VALUE.

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
OR ' ' OF brTables 
OR 'RETURN' OF brTables
DO:
  ttTable.lFavourite = NOT ttTable.lFavourite.
  BROWSE brTables:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables Dialog-Frame
ON MOUSE-SELECT-CLICK OF brTables IN FRAME Dialog-Frame
DO:
  /* Alternative way for toggling the selection box
   * If we just enable it, focus comes solely on the 
   * toggle box when we jump from filterbox to browse
   */
  DEFINE VARIABLE iMouseX AS INTEGER NO-UNDO.
  DEFINE VARIABLE iMouseY AS INTEGER NO-UNDO.
  DEFINE VARIABLE iRow    AS INTEGER NO-UNDO.
  DEFINE VARIABLE hBrowse AS HANDLE  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    hBrowse = BROWSE brTables:HANDLE.
    RUN getMouseXY(INPUT hBrowse:FRAME, OUTPUT iMouseX, OUTPUT iMouseY).

    IF    iMouseY > hBrowse:Y
      AND iMouseY < hBrowse:Y + hBrowse:HEIGHT-PIXELS
      AND iMouseX > hBrowse:X 
      AND iMouseX < (hBrowse:X + hBrowse:GET-BROWSE-COLUMN(2):X) THEN
    DO:
      iRow = TRUNCATE((iMouseY - brTables:Y) / (hBrowse:ROW-HEIGHT-PIXELS + 4),0).

      IF hBrowse:NUM-ITERATIONS > 0 AND iRow > hBrowse:NUM-ITERATIONS THEN RETURN.
      IF iRow < 1 THEN RETURN.

      /* Get the record in the buffer */
      IF hBrowse:QUERY:NUM-RESULTS > 0 THEN
      DO:
        hBrowse:SELECT-ROW(iRow).
        hBrowse:FETCH-SELECTED-ROW(1).
        APPLY 'default-action' TO hBrowse.
      END.
    END.
  END.
  
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


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete Dialog-Frame
ON CHOOSE OF btnDelete IN FRAME Dialog-Frame /* del */
DO:
  DEFINE VARIABLE lDelete AS LOGICAL NO-UNDO.
  DEFINE BUFFER bTable FOR ttTable.

  MESSAGE 'Are you sure you want to delete this group?'
    VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lDelete.

  IF lDelete THEN
  DO:
    FOR EACH bTable WHERE (bTable.lShowInList = TRUE OR bTable.lFavourite = TRUE):
      bTable.lFavourite = NO.
    END. 

    APPLY 'go' TO FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeselectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeselectAll Dialog-Frame
ON CHOOSE OF btnDeselectAll IN FRAME Dialog-Frame /* Deselect all */
OR 'CTRL-D' OF brTables
DO:
  RUN selectTables(fiTableFilter:SCREEN-VALUE, NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEdit Dialog-Frame
ON CHOOSE OF btnEdit IN FRAME Dialog-Frame /* edit */
OR mouse-select-click OF fiGroupname
DO:
  fiGroupname:PRIVATE-DATA = fiGroupname:SCREEN-VALUE.
  fiGroupname:READ-ONLY = FALSE.
  APPLY 'ENTRY' TO fiGroupname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll Dialog-Frame
ON CHOOSE OF btnSelectAll IN FRAME Dialog-Frame /* Select all */
OR 'CTRL-A' OF brTables
DO:
  RUN selectTables(fiTableFilter:SCREEN-VALUE, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiGroupname
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiGroupname Dialog-Frame
ON RETURN OF fiGroupname IN FRAME Dialog-Frame
DO:
  SELF:CLEAR-SELECTION(). 
  fiGroupname:READ-ONLY = TRUE.
  APPLY 'entry' TO fiTableFilter.
  RETURN NO-APPLY.
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
, cbDatabase
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

  /* Set databases */
  cbDatabase:LIST-ITEMS = ',' + getDatabaseList().
  cbDatabase:SCREEN-VALUE = getRegistry('DataDigger','Database') NO-ERROR.

  btnEdit:LOAD-IMAGE(getImagePath('edit.gif')).
  btnDelete:LOAD-IMAGE(getImagePath('delete.gif')).
  fiGroupName = pcGroup.

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME} FOCUS fiTableFilter.

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
  DISPLAY fiGroupname fiTableFilter cbDatabase 
      WITH FRAME Dialog-Frame.
  ENABLE BtnCancel btnDelete btnEdit fiGroupname fiTableFilter cbDatabase 
         brTables btnSelectAll btnDeselectAll Btn_OK 
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
  DEFINE INPUT PARAMETER pcFilter   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER plSelect   AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE rTable     AS ROWID   NO-UNDO.
  DEFINE VARIABLE iBrowseRow AS INTEGER NO-UNDO.
  DEFINE BUFFER bTable FOR ttTable.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    IF AVAILABLE ttTable THEN 
    DO:
      rTable = ROWID(ttTable).
      /* Position in browse */
      #FindRow:
      DO iBrowseRow = 1 TO brTables:NUM-ENTRIES:
        IF brTables:IS-ROW-SELECTED(iBrowseRow) THEN LEAVE #FindRow. 
      END.
    END.

    FOR EACH bTable 
      WHERE bTable.cTableName MATCHES '*' + pcFilter + '*'
        AND (cbDatabase:SCREEN-VALUE = ? OR bTable.cDatabase = cbDatabase:SCREEN-VALUE)
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

