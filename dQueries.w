&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame
/*------------------------------------------------------------------------

  Name: dQueries.w
  Desc: Let user maintain previously used queries

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{ DataDigger.i }

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER  pcDatabase     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  pcTable        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  pcCurrentQuery AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER piQueryNr      AS INTEGER NO-UNDO INITIAL ?.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE giQueryOffset  AS INTEGER NO-UNDO.
DEFINE VARIABLE ghEditor       AS HANDLE  EXTENT 5 NO-UNDO.
DEFINE VARIABLE ghDelButton    AS HANDLE  EXTENT 5 NO-UNDO.
DEFINE VARIABLE giQuery        AS INTEGER EXTENT 5 NO-UNDO.
DEFINE VARIABLE ghActiveEditor AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDelQuery-1 EdQuery-1 btUp EdQuery-2 btDown ~
EdQuery-3 EdQuery-4 EdQuery-5 BtnOK BtnCancel btDelQuery-2 btDelQuery-3 ~
btDelQuery-4 btDelQuery-5
&Scoped-Define DISPLAYED-OBJECTS EdQuery-1 EdQuery-2 EdQuery-3 EdQuery-4 ~
EdQuery-5

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-EdQuery-1
       MENU-ITEM m_Edit         LABEL "Edit"
       MENU-ITEM m_Delete       LABEL "Delete"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btDelQuery-1  NO-FOCUS FLAT-BUTTON
     LABEL "Del"
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-2  NO-FOCUS FLAT-BUTTON
     LABEL "Del"
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-3  NO-FOCUS FLAT-BUTTON
     LABEL "Del"
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-4  NO-FOCUS FLAT-BUTTON
     LABEL "Del"
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDelQuery-5  NO-FOCUS FLAT-BUTTON
     LABEL "Del"
     SIZE-PIXELS 23 BY 23 TOOLTIP "delete this query".

DEFINE BUTTON btDown
     LABEL "Down"
     SIZE-PIXELS 30 BY 24 TOOLTIP "go down".

DEFINE BUTTON BtnCancel AUTO-END-KEY
     LABEL "&Cancel"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT
     LABEL "OK"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btUp
     LABEL "Up"
     SIZE-PIXELS 30 BY 24 TOOLTIP "go up".

DEFINE VARIABLE EdQuery-1 AS CHARACTER
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-2 AS CHARACTER
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     BGCOLOR 8 FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-3 AS CHARACTER
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-4 AS CHARACTER
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     BGCOLOR 8 FONT 0 NO-UNDO.

DEFINE VARIABLE EdQuery-5 AS CHARACTER
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 415 BY 75
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btDelQuery-1 AT Y 5 X 420 WIDGET-ID 56
     EdQuery-1 AT Y 5 X 5 NO-LABEL WIDGET-ID 2
     btUp AT Y 5 X 480 WIDGET-ID 52
     EdQuery-2 AT Y 80 X 5 NO-LABEL WIDGET-ID 4
     btDown AT Y 5 X 510 WIDGET-ID 54
     EdQuery-3 AT Y 155 X 5 NO-LABEL WIDGET-ID 6
     EdQuery-4 AT Y 230 X 5 NO-LABEL WIDGET-ID 12
     EdQuery-5 AT Y 305 X 5 NO-LABEL WIDGET-ID 10
     BtnOK AT Y 322 X 465 WIDGET-ID 48
     BtnCancel AT Y 352 X 465
     btDelQuery-2 AT Y 80 X 420 WIDGET-ID 58
     btDelQuery-3 AT Y 155 X 420 WIDGET-ID 60
     btDelQuery-4 AT Y 230 X 420 WIDGET-ID 62
     btDelQuery-5 AT Y 305 X 420 WIDGET-ID 64
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
         SIDE-LABELS NO-UNDERLINE THREE-D
         SIZE-PIXELS 560 BY 420
         TITLE "Select query"
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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

ASSIGN
       EdQuery-1:POPUP-MENU IN FRAME Dialog-Frame       = MENU POPUP-MENU-EdQuery-1:HANDLE
       EdQuery-1:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN
       EdQuery-2:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN
       EdQuery-3:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN
       EdQuery-4:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN
       EdQuery-5:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Select query */
DO:
  RUN saveQuery( ghActiveEditor:PRIVATE-DATA
               , ghActiveEditor:SCREEN-VALUE ).

  RUN saveQueryTable( INPUT table ttQuery
                    , INPUT pcDatabase
                    , INPUT pcTable
                    ).

  piQueryNr = INTEGER(ghActiveEditor:PRIVATE-DATA) NO-ERROR.
  IF ERROR-STATUS:ERROR
    OR NOT CAN-FIND(ttQuery
              WHERE ttQuery.cDatabase = pcDatabase
                AND ttQuery.cTable    = pcTable
                AND ttQuery.iQueryNr  = piQueryNr ) THEN piQueryNr = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON LEAVE OF FRAME Dialog-Frame /* Select query */
DO:
  SELF:bgcolor = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select query */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-1 Dialog-Frame
ON CHOOSE OF btDelQuery-1 IN FRAME Dialog-Frame /* Del */
OR 'shift-del' OF EdQuery-1
DO:
  RUN deleteQuery( ghEditor[1]:PRIVATE-DATA ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-2 Dialog-Frame
ON CHOOSE OF btDelQuery-2 IN FRAME Dialog-Frame /* Del */
OR 'shift-del' OF EdQuery-2
DO:
  RUN deleteQuery(giQueryOffset + 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-3 Dialog-Frame
ON CHOOSE OF btDelQuery-3 IN FRAME Dialog-Frame /* Del */
OR 'shift-del' OF EdQuery-3
DO:
  RUN deleteQuery(giQueryOffset + 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-4 Dialog-Frame
ON CHOOSE OF btDelQuery-4 IN FRAME Dialog-Frame /* Del */
OR 'shift-del' OF EdQuery-4
DO:
  RUN deleteQuery(giQueryOffset + 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelQuery-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelQuery-5 Dialog-Frame
ON CHOOSE OF btDelQuery-5 IN FRAME Dialog-Frame /* Del */
OR 'shift-del' OF EdQuery-5
DO:
  RUN deleteQuery(giQueryOffset + 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDown Dialog-Frame
ON CHOOSE OF btDown IN FRAME Dialog-Frame /* Down */
DO:
  APPLY 'cursor-down' TO ghActiveEditor.
  APPLY 'entry' TO ghActiveEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUp Dialog-Frame
ON CHOOSE OF btUp IN FRAME Dialog-Frame /* Up */
DO:
  APPLY 'cursor-up' TO ghActiveEditor.
  APPLY 'entry' TO ghActiveEditor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-1 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON CURSOR-UP OF EdQuery-1 IN FRAME Dialog-Frame
DO:
  giQueryOffset = giQueryOffset - 1.
  IF giQueryOffset < 1 THEN giQueryOffset = 1.

  RUN showQueries.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON ENTRY OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-2, edQuery-3, edQuery-4, edQuery-5 DO:

  SELF:read-only = NO.
  SELF:bgcolor = 14.

  ghActiveEditor = SELF:handle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON LEAVE OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-3, edQuery-5
DO:
  IF SELF:modified THEN
    RUN saveQuery( SELF:private-data, SELF:screen-value ).

  SELF:read-only = YES.
  SELF:bgcolor = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF EdQuery-1 IN FRAME Dialog-Frame
, edQuery-2, edQuery-3, edQuery-4, edQuery-5
OR 'return' OF edQuery-1, edQuery-2, edQuery-3, edQuery-4, edQuery-5

DO:
  APPLY 'GO' TO FRAME {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-2 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON CURSOR-UP OF EdQuery-2 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-2 Dialog-Frame
ON LEAVE OF EdQuery-2 IN FRAME Dialog-Frame
, edQuery-4
DO:
  IF SELF:modified THEN
    RUN saveQuery( SELF:private-data, SELF:screen-value ).

  SELF:read-only = YES.
  SELF:bgcolor = 8.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-3 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-3 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-3 Dialog-Frame
ON CURSOR-UP OF EdQuery-3 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-4 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-4 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-4 Dialog-Frame
ON CURSOR-UP OF EdQuery-4 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EdQuery-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-5 Dialog-Frame
ON CURSOR-DOWN OF EdQuery-5 IN FRAME Dialog-Frame
DO:

  RUN saveQuery( ghActiveEditor:PRIVATE-DATA
               , ghActiveEditor:SCREEN-VALUE ).

  giQueryOffset = giQueryOffset + 1.
  IF giQueryOffset > 10 THEN giQueryOffset = 10.

  RUN showQueries.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EdQuery-5 Dialog-Frame
ON CURSOR-UP OF EdQuery-5 IN FRAME Dialog-Frame
DO:
  APPLY 'entry' TO edQuery-4.
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

  RUN enable_UI.
  RUN initializeObject.

  WAIT-FOR GO OF FRAME {&frame-name} FOCUS ghActiveEditor.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteQuery Dialog-Frame
PROCEDURE deleteQuery :
/*
 * Delete query from list
 */
  DEFINE INPUT  PARAMETER piQueryNum AS INTEGER     NO-UNDO.
  DEFINE BUFFER bQuery FOR ttQuery.

  FIND bQuery
    WHERE bQuery.cDatabase = pcDatabase
      AND bQuery.cTable    = pcTable
      AND bQuery.iQueryNr  = piQueryNum NO-ERROR.

  IF AVAILABLE bQuery THEN
  DO:
    DELETE bQuery.

    RUN renumberQueries.
    RUN showQueries.
  END.

END PROCEDURE. /* deleteQuery */

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
  DISPLAY EdQuery-1 EdQuery-2 EdQuery-3 EdQuery-4 EdQuery-5
      WITH FRAME Dialog-Frame.
  ENABLE btDelQuery-1 EdQuery-1 btUp EdQuery-2 btDown EdQuery-3 EdQuery-4
         EdQuery-5 BtnOK BtnCancel btDelQuery-2 btDelQuery-3 btDelQuery-4
         btDelQuery-5
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame
PROCEDURE initializeObject :
  /* Set global vars and prepare widgets
  */
  DEFINE VARIABLE iBox AS INTEGER NO-UNDO.

  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&frame-name}:

    /* Set default font */
    FRAME {&frame-name}:font = getFont('Default').

    /* Get all queries */
    RUN getQueryTable( OUTPUT table ttQuery ).

    /* Init handles */
    ghEditor[1] = edQuery-1:handle.
    ghEditor[2] = edQuery-2:handle.
    ghEditor[3] = edQuery-3:handle.
    ghEditor[4] = edQuery-4:handle.
    ghEditor[5] = edQuery-5:handle.

    ghDelButton[1] = btDelQuery-1:handle.
    ghDelButton[2] = btDelQuery-2:handle.
    ghDelButton[3] = btDelQuery-3:handle.
    ghDelButton[4] = btDelQuery-4:handle.
    ghDelButton[5] = btDelQuery-5:handle.

    /* Init images */
    btUp:load-image(getImagePath('Up.gif')).
    btDown:load-image(getImagePath('Down.gif')).

    DO iBox = 1 TO 5:
      ghEditor[iBox]:FONT = getFont('Fixed').
      ghDelButton[iBox]:LOAD-IMAGE(getImagePath('Clear.gif') ).
    END.

    /* Transform query to internal format */
    pcCurrentQuery = REPLACE(pcCurrentQuery,CHR(1),'~n').
    pcCurrentQuery = REPLACE(pcCurrentQuery, '~n', {&QUERYSEP}).

    FRAME {&frame-name}:title = SUBSTITUTE('Select query for &1.&2'
                                          , pcDatabase
                                          , pcTable
                                          ).
    RUN showQueries.
    giQueryOffset = 1.
    ghActiveEditor = ghEditor[1].

    /* Point to current query */
    FIND FIRST bQuery
      WHERE bQuery.cDatabase = pcDatabase
        AND bQuery.cTable    = pcTable
        AND bQuery.cQueryTxt = pcCurrentQuery
            NO-ERROR.

    IF AVAILABLE bQuery THEN
    DO:
      IF bQuery.iQueryNr > 5 THEN
      DO:
        giQueryOffset = bQuery.iQueryNr - 4.
        ghActiveEditor = ghEditor[5].
        RUN showQueries.
      END.
      ELSE
      DO:
        ghActiveEditor = ghEditor[bQuery.iQueryNr].
      END.
    END.

    /* For some reasons, these #*$&# scrollbars keep coming back */
    RUN showScrollBars(FRAME {&frame-name}:handle, NO, NO). /* KILL KILL KILL */
  END.

END PROCEDURE. /* initializeObject. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumberQueries Dialog-Frame
PROCEDURE renumberQueries :
  /* Renumber all queries to avoid gaps
  */
  DEFINE VARIABLE iQuery AS INTEGER     NO-UNDO.
  DEFINE BUFFER bQuery FOR ttQuery.

  /* Renumber ttQuery temp-table */
  iQuery = 0.

  #QueryLoop:
  REPEAT PRESELECT EACH bQuery
    WHERE bQuery.cDatabase = pcDatabase
      AND bQuery.cTable    = pcTable
       BY bQuery.iQueryNr:

    FIND NEXT bQuery NO-ERROR.
    IF NOT AVAILABLE bQuery THEN LEAVE #QueryLoop.

    ASSIGN
      iQuery          = iQuery + 1
      bQuery.iQueryNr = iQuery.
  END.

END PROCEDURE. /* renumberQueries */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveQuery Dialog-Frame
PROCEDURE saveQuery :
  /* Save the query to INI
  */
  DEFINE INPUT PARAMETER piQueryNum AS INTEGER     NO-UNDO.
  DEFINE INPUT PARAMETER pcQueryTxt AS CHARACTER   NO-UNDO.

  DEFINE BUFFER bQuery FOR ttQuery.

  /* New query? */
  IF piQueryNum = 0 THEN
  DO:
    FIND LAST bQuery
      WHERE bQuery.cDatabase = pcDatabase
        AND bQuery.cTable    = pcTable NO-ERROR.

    IF AVAILABLE bQuery THEN
      piQueryNum = bQuery.iQueryNr.
    ELSE
      piQueryNum = 1.

    CREATE bQuery.
    ASSIGN bQuery.cDatabase = pcDatabase
           bQuery.cTable    = pcTable
           bQuery.iQueryNr  = piQueryNum
           bQuery.cQueryTxt = pcQueryTxt.
  END.
  ELSE
  DO:
    FIND bQuery
      WHERE bQuery.cDatabase = pcDatabase
        AND bQuery.cTable    = pcTable
        AND bQuery.iQueryNr  = piQueryNum NO-ERROR.

    bQuery.cQueryTxt = pcQueryTxt.
  END.

  RUN renumberQueries.
  RUN showQueries.

END PROCEDURE. /* saveQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtons Dialog-Frame
PROCEDURE setButtons :
  /* Set sensitivity of the buttons
  */
  DEFINE VARIABLE iQuery AS INTEGER NO-UNDO.

  DO iQuery = 1 TO 5:
    ghDelButton[iQuery]:SENSITIVE = (ghEditor[iQuery]:SCREEN-VALUE <> '').
  END.

END PROCEDURE. /* setButtons */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showQueries Dialog-Frame
PROCEDURE showQueries :
  /* Show the queries in their editor box
  */
  DEFINE VARIABLE iQuery AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLoop  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cQuery AS CHARACTER   NO-UNDO.

  DEFINE BUFFER bQuery FOR ttQuery.

  #Query:
  FOR EACH bQuery
    WHERE bQuery.cDatabase = pcDatabase
      AND bQuery.cTable    = pcTable
      AND bQuery.iQueryNr  >= giQueryOffset:

    iQuery = iQuery + 1.

    cQuery = REPLACE(bQuery.cQueryTxt,CHR(1),'~n').
    ghEditor[iQuery]:SCREEN-VALUE = cQuery.
    ghEditor[iQuery]:PRIVATE-DATA = STRING(bQuery.iQueryNr).

    IF iQuery = 5 THEN LEAVE #Query.
  END. /* #Query */

  DO iLoop = iQuery + 1 TO 5:
    ghEditor[iLoop]:SCREEN-VALUE = ''.
    ghEditor[iLoop]:PRIVATE-DATA = ''.
  END.

  RUN setButtons.

END PROCEDURE. /* showQueries */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

