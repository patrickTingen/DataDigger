&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*
-------------------------------------------------------------------
Copyright (c) 2001 and later Netsetup B.V.
-------------------------------------------------------------------
Name         : wDebugger.w
Purpose      : Debugger for DataDigger

15/04/13 PT    Changed to 'light' version for DataDigger, originating
               from the DWP Debugger.

----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.    */
/*--------------------------------------------------------------------*/

CREATE WIDGET-POOL.

{ DataDigger.i }

/* Local Variable Definitions */
DEFINE VARIABLE giPrevTime AS INTEGER     NO-UNDO. /* remember last TIME */.
DEFINE VARIABLE gnPrevProg AS CHARACTER   NO-UNDO. /* program-name(2) */.

/* Temptable to keep track of all published messages. */
DEFINE TEMP-TABLE ttMessage NO-UNDO
  FIELD dtEvent   AS DATETIME
  FIELD iTime     AS INTEGER
  FIELD iLevel    AS INTEGER
  FIELD cProgram  AS CHARACTER
  FIELD cMessage  AS CHARACTER
  INDEX iPrim IS PRIMARY /* not unique! */ iTime.

DEFINE TEMP-TABLE ttTimer NO-UNDO
  FIELD cTimerId   AS CHARACTER
  FIELD tStartTime AS DATETIME
  FIELD iNumStarts AS INTEGER
  FIELD tTotalTime AS INTEGER
  INDEX ttTimerPrim IS PRIMARY UNIQUE cTimerId.

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll":
   DEFINE INPUT  PARAMETER piWindowHwnd AS LONG NO-UNDO.
   DEFINE RETURN PARAMETER piResult     AS LONG NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiLevelFrom fiLevelTo fiFilterText btnFilter ~
btnClear edMessageBox fiFindString btnTimers
&Scoped-Define DISPLAYED-OBJECTS fiLevelFrom fiLevelTo fiFilterText ~
edMessageBox fiFindString

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegistry C-Win
FUNCTION getRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRegistry C-Win
FUNCTION setRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    , pcValue   AS CHARACTER
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear
     LABEL "&Clear All"
     SIZE-PIXELS 70 BY 21 TOOLTIP "clear all messages".

DEFINE BUTTON btnFilter
     LABEL "&Filter Now"
     SIZE-PIXELS 70 BY 21 TOOLTIP "(re)apply the filter".

DEFINE BUTTON btnTimers
     LABEL "&Timers"
     SIZE-PIXELS 70 BY 21 TOOLTIP "show timers".

DEFINE VARIABLE edMessageBox AS CHARACTER
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 780 BY 225
     FONT 2 NO-UNDO.

DEFINE VARIABLE fiFilterText AS CHARACTER FORMAT "X(256)":U
     LABEL "Te&xt"
     VIEW-AS FILL-IN
     SIZE-PIXELS 120 BY 21 TOOLTIP "filter on text" NO-UNDO.

DEFINE VARIABLE fiFindString AS CHARACTER FORMAT "X(256)":U
     LABEL "&Find"
     VIEW-AS FILL-IN
     SIZE-PIXELS 165 BY 21 TOOLTIP "Find text (CTRL-F) use F9 / SHIFT-F9 to search again" NO-UNDO.

DEFINE VARIABLE fiLevelFrom AS INTEGER FORMAT ">>9":U INITIAL 0
     LABEL "&Level from"
     VIEW-AS FILL-IN
     SIZE-PIXELS 35 BY 21 TOOLTIP "select the lower limit of the levelfilter"
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiLevelTo AS INTEGER FORMAT ">>9":U INITIAL 999
     LABEL "&to"
     VIEW-AS FILL-IN
     SIZE-PIXELS 35 BY 21 TOOLTIP "select the upper limit of the levelfilter"
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiLevelFrom AT Y 2 X 50 COLON-ALIGNED
     fiLevelTo AT Y 2 X 105 COLON-ALIGNED
     fiFilterText AT Y 2 X 180 COLON-ALIGNED
     btnFilter AT Y 2 X 315
     btnClear AT Y 2 X 708
     edMessageBox AT Y 25 X 0 NO-LABEL
     fiFindString AT Y 2 X 425 COLON-ALIGNED WIDGET-ID 14
     btnTimers AT Y 2 X 620 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT X 0 Y 0
         SIZE-PIXELS 780 BY 250
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Debugger"
         HEIGHT-P           = 250
         WIDTH-P            = 780
         MAX-HEIGHT-P       = 702
         MAX-WIDTH-P        = 1024
         VIRTUAL-HEIGHT-P   = 702
         VIRTUAL-WIDTH-P    = 1024
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN
       edMessageBox:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Debugger */
OR ENDKEY OF {&window-name} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Debugger */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Debugger */
DO:
  RUN resizeWindow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME DEFAULT-FRAME /* Clear All */
DO:
  RUN initializeDebugger.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter C-Win
ON CHOOSE OF btnFilter IN FRAME DEFAULT-FRAME /* Filter Now */
OR 'return' OF fiLevelFrom, fiLevelTo, fiFilterText
DO:
  RUN applyFilter.
  RUN saveSettings.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTimers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTimers C-Win
ON CHOOSE OF btnTimers IN FRAME DEFAULT-FRAME /* Timers */
DO:
  DEFINE BUFFER bfTimer FOR ttTimer.

  PUBLISH "debugInfo"(1, SUBSTITUTE("&1 &2 &3 &4"
                                      , STRING("Timer"  ,"x(30)")
                                      , STRING("    Num","x(8)")
                                      , STRING("    Avg","x(8)")
                                      , STRING("  Total","x(8)")
                                      )
                        ).
  PUBLISH "debugInfo"(1, SUBSTITUTE("&1 &2 &3 &4"
                                      , STRING(FILL("-",30),"x(30)")
                                      , STRING(FILL("-", 8),"x(8)")
                                      , STRING(FILL("-", 8),"x(8)")
                                      , STRING(FILL("-", 8),"x(8)")
                                      )
                        ).
  FOR EACH bfTimer:
    PUBLISH "debugInfo"(1, SUBSTITUTE("&1 &2 &3 &4"
                                        , STRING(bfTimer.cTimerId,"x(30)")
                                        , STRING(bfTimer.iNumStarts,">>>>>>>9")
                                        , STRING(INTEGER(bfTimer.tTotalTime / bfTimer.iNumStarts),">>>>>>>9")
                                        , STRING(bfTimer.tTotalTime,">>>>>>>9")
                                        )
                          ).
  END. /* case picCommand: */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edMessageBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edMessageBox C-Win
ON END-ERROR OF edMessageBox IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edMessageBox C-Win
ON F9 OF edMessageBox IN FRAME DEFAULT-FRAME
OR "F9" OF fiFindString
DO:
  RUN findText(fiFindString:screen-value IN FRAME {&frame-name},"next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edMessageBox C-Win
ON SHIFT-F9 OF edMessageBox IN FRAME DEFAULT-FRAME
OR "SHIFT-F9" OF fiFindString
DO:
  RUN findText(fiFindString:screen-value IN FRAME {&frame-name},"prev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFindString
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFindString C-Win
ON RETURN OF fiFindString IN FRAME DEFAULT-FRAME /* Find */
DO:
  RUN findText(fiFindString:screen-value IN FRAME {&frame-name},"next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevelFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevelFrom C-Win
ON SHIFT-DEL OF fiLevelFrom IN FRAME DEFAULT-FRAME /* Level from */
, fiLevelTo, fiFilterText
DO:
  fiLevelFrom:SCREEN-VALUE  = "0".
  fiLevelTo:SCREEN-VALUE    = "999".
  fiFilterText:SCREEN-VALUE = "".

  RUN saveSettings.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&window-name}
       THIS-PROCEDURE:CURRENT-WINDOW = {&window-name}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  RUN saveSettings.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Set the max window size to the display size */
ASSIGN {&window-name}:MAX-WIDTH  = ?
       {&window-name}:MAX-HEIGHT = ?.

SUBSCRIBE TO "dwp_stop" ANYWHERE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN initializeDebugger.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyFilter C-Win
PROCEDURE applyFilter :
/*------------------------------------------------------------------------------
  Name: applyFilter
  Desc: Refilter all messages
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iReturnCode AS INTEGER    NO-UNDO.

  /* Avoid flashing */
  RUN lockWindowUpdate (INPUT FRAME {&frame-name}:hwnd, OUTPUT iReturnCode).

  /* clear viewport */
  DO WITH FRAME {&frame-name}:
    edMessageBox:screen-value = "".
  END.

  /* Reset vars that hold previous values */
  ASSIGN
    giPrevTime = 0
    gnPrevProg = ''
    .

  /* Run thru all messages and hand 'em over to showMessage */
  FOR EACH ttMessage:
    RUN showMessage
      ( INPUT ttMessage.iTime
      , INPUT ttMessage.iLevel
      , INPUT ttMessage.cProgram
      , INPUT ttMessage.cMessage
      ).
  END.

  /* Unlock window */
  RUN lockWindowUpdate (INPUT 0, OUTPUT iReturnCode).

  /* Set focus to editor */
  APPLY 'entry' TO edMessageBox IN FRAME {&frame-name}.

END PROCEDURE. /* applyFilter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE debugInfo C-Win
PROCEDURE debugInfo :
/*------------------------------------------------------------------------------
  Name       : debugInfo
  Purpose    : Save a message in the temp-table
  Parameters : piLevel    : level of the message
               pcMessage  : the message itself
  ------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piLevel    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER pcMessage  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iReturnCode AS INTEGER    NO-UNDO.

  /* Avoid flashing */
  RUN lockWindowUpdate (INPUT FRAME {&frame-name}:hwnd, OUTPUT iReturnCode).

  /* Only internal procedure name.
   * Otherwise only program name, no path
   */
  CREATE ttMessage.
  ASSIGN
    ttMessage.iTime     = MTIME(NOW)
    ttMessage.iLevel    = piLevel
    ttMessage.cProgram  = ENTRY(1, PROGRAM-NAME(2)," ")
    ttMessage.cMessage  = pcMessage
    .

  /* Make sure it shows up in the editor */
  RUN showMessage
    ( INPUT ttMessage.iTime
    , INPUT ttMessage.iLevel
    , INPUT ttMessage.cProgram
    , INPUT ttMessage.cMessage
    ).

  /* Unlock window */
  RUN lockWindowUpdate (INPUT 0, OUTPUT iReturnCode).

END PROCEDURE. /* debugInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fiLevelFrom fiLevelTo fiFilterText edMessageBox fiFindString
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiLevelFrom fiLevelTo fiFilterText btnFilter btnClear edMessageBox
         fiFindString btnTimers
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findText C-Win
PROCEDURE findText :
/*------------------------------------------------------------------------------
  Name : findText
  Desc : Find a text in the editor
  Parm : pcString = text you are looking for
         pcDirection = "next" | "prev" for direction
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcString    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcDirection AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iFlags AS INTEGER NO-UNDO.

  IF pcDirection = "NEXT" THEN iFlags = FIND-NEXT-OCCURRENCE. /* 1 */
  IF pcDirection = "PREV" THEN iFlags = FIND-PREV-OCCURRENCE. /* 2 */

  ASSIGN iFlags = iFlags + 16 /* wrap */ + 32. /* find-select */

  IF edMessageBox:SEARCH(pcString,iFlags) IN FRAME {&frame-name} = FALSE THEN
    MESSAGE pcString "not found" VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE. /* findText */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDebugger C-Win
PROCEDURE initializeDebugger :
/*------------------------------------------------------------------------------
  Name       : initializeDebugger
  Purpose    : Initializes the window, sets all settings to default
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE iValue AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

  /* Default settings */
  DO WITH FRAME {&frame-name}:

    EMPTY TEMP-TABLE ttMessage.
    EMPTY TEMP-TABLE ttTimer.

    /* Redisplay filterfields */
    fiLevelFrom:screen-value  = "0".
    fiLevelTo:screen-value    = "999".
    fiFilterText:screen-value = "".
    edMessageBox:screen-value = "".

    /* Reset vars that hold previous values */
    giPrevTime = 0.
    gnPrevProg = "".

    /* Subscribe to all events */
    SUBSCRIBE TO "debugInfo" ANYWHERE RUN-PROCEDURE "debugInfo".
    SUBSCRIBE TO "query"        ANYWHERE RUN-PROCEDURE "debugInfo".
    SUBSCRIBE TO "timerCommand" ANYWHERE RUN-PROCEDURE "timerCommand".

    RUN enable_UI.

    /* Window x-position */
    iValue = INTEGER(getRegistry("Debugger", "Window:x" )).
    IF iValue = ? THEN iValue = 200.

    /* Keep DD on primary monitor ? (Rob Willoughby) */
    IF LOGICAL(getRegistry("DataDigger","StartOnPrimaryMonitor")) = TRUE
      AND (iValue < 0 OR iValue > SESSION:WORK-AREA-WIDTH-PIXELS) THEN iValue = 200.

    ASSIGN c-win:X = iValue NO-ERROR.

    /* Window y-position */
    iValue = INTEGER(getRegistry("Debugger", "Window:y" )).
    IF iValue < 0 OR iValue = ? OR iValue > SESSION:WORK-AREA-HEIGHT-PIXELS THEN iValue = 200.
    ASSIGN c-win:Y = iValue NO-ERROR.

    /* Window height */
    iValue = INTEGER(getRegistry("Debugger", "Window:height" )).
    IF iValue = ? OR iValue = 0 THEN iValue = 600.
    ASSIGN c-win:HEIGHT-PIXELS = iValue NO-ERROR.

    /* Window width */
    iValue = INTEGER(getRegistry("Debugger", "Window:width" )).
    IF iValue = ? OR iValue = 0 THEN iValue = 800.
    ASSIGN c-win:WIDTH-PIXELS = iValue NO-ERROR.

    /* Filter settings */
    iValue = INTEGER(getRegistry("Debugger", "LevelFrom" )).
    IF iValue = ? OR iValue < 0 THEN iValue = 0.
    fiLevelFrom:SCREEN-VALUE = STRING(iValue).

    iValue = INTEGER(getRegistry("Debugger", "LevelTo")).
    IF iValue = ? OR iValue < 0 OR iValue > 999 THEN iValue = 999.
    fiLevelTo:SCREEN-VALUE = STRING(iValue).

    cValue = getRegistry("Debugger", "FilterText").
    IF cValue = ? THEN cValue = "".
    fiFilterText:SCREEN-VALUE = cValue.

    RUN resizeWindow.
    RUN applyFilter.
  END.

END PROCEDURE. /* initializeDebugger */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow C-Win
PROCEDURE resizeWindow :
/*------------------------------------------------------------------------------
  Name       : resizeWindow
  Purpose    : Resizes the editor inside the window.
  Parameters : <none>
  ------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:

    /* Adjust frame */
    ASSIGN
      FRAME {&frame-name}:virtual-width  = {&window-name}:width
      FRAME {&frame-name}:virtual-height = {&window-name}:height
      FRAME {&frame-name}:width          = {&window-name}:width
      FRAME {&frame-name}:height         = {&window-name}:height
      no-error.

    /* Set all widgets */
    ASSIGN
      edMessageBox:width-pixels  = FRAME {&frame-name}:width-pixels
      edMessageBox:height-pixels = FRAME {&frame-name}:height-pixels - edMessageBox:y - 1
      .

    RUN saveSettings.
  END.

END PROCEDURE. /* resizeWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSettings C-Win
PROCEDURE saveSettings :
/*------------------------------------------------------------------------
  Name         : saveSettings
  Description  : Save size and position of the window.

  ----------------------------------------------------------------------
  03-12-2013 pti Created
  ----------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    /* Save window size/pos */
    setRegistry("Debugger", "Window:x"     , STRING(c-win:X) ).
    setRegistry("Debugger", "Window:y"     , STRING(c-win:Y) ).
    setRegistry("Debugger", "Window:height", STRING(c-win:HEIGHT-PIXELS) ).
    setRegistry("Debugger", "Window:width" , STRING(c-win:WIDTH-PIXELS) ).

    /* Save filter settings */
    setRegistry("Debugger", "LevelFrom" , fiLevelFrom:screen-value   ).
    setRegistry("Debugger", "LevelTo"   , fiLevelTo:screen-value     ).
    setRegistry("Debugger", "FilterText", fiFilterText:screen-value  ).

  END.

END PROCEDURE. /* saveSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showMessage C-Win
PROCEDURE showMessage :
/*------------------------------------------------------------------------------
  Name       : showMessage
  Purpose    : Displays a message in the editor when it passes the filter
  Parameters : piTime     : time in mSec of the message
               piLevel    : level of the message
               pcProgram  : the publisher
               pcMessage  : the message itself
  ------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER piTimeMsec AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER piLevel    AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER pcProgram  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pcMessage  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cMessage    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLevel      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTextStart  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iTime       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iTextPart   AS INTEGER     NO-UNDO.

  DEFINE VARIABLE iLevelFrom  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLevelTo    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFilterText AS CHARACTER   NO-UNDO.

  /* Get filter values */
  DO WITH FRAME {&frame-name}:
    ASSIGN
      iLevelFrom  = INTEGER(fiLevelFrom:screen-value)
      iLevelTo    = INTEGER(fiLevelTo:screen-value)
      cFilterText = fiFilterText:screen-value
      .

    /* sanity checks */
    IF iLevelFrom > iLevelTo THEN iLevelTo = iLevelFrom.
    IF iLevelFrom = 0 AND iLevelTo = 0 THEN iLevelTo = 999.
    fiLevelTo:screen-value = STRING(iLevelTo).
  END.

  /* Use time representation in Seconds rather than milliseconds */
  ASSIGN iTime = piTimeMsec / 1000.

  /* Respect Capture level and text/program filter */
  IF    piLevel >= iLevelFrom
    AND piLevel <= iLevelTo
    AND (   pcMessage MATCHES "*" + cFilterText + "*"
         OR pcProgram MATCHES "*" + cFilterText + "*" ) THEN
  DO WITH FRAME {&frame-name}:

    /* remove trailing newlines */
    cMessage = RIGHT-TRIM(pcMessage,"~n").

    /* Add messages to the tail */
    edMessageBox:move-to-eof().

    /* Add spaces to program name to make them all equally long */
    IF LENGTH(pcProgram) < 24 THEN pcProgram = STRING(pcProgram,"X(24)").

    /* Insert new line when changing procedure or time difference more than 1 second */
    IF   pcProgram <> gnPrevProg OR iTime <> giPrevTime THEN
      edMessageBox:insert-string("~n").

    ASSIGN
      cLevel = SUBSTITUTE("&1:&2: ", STRING(piLevel,"999"), pcProgram ).

    /* Insert time only once per second */
    IF iTime <> giPrevTime THEN
      edMessageBox:insert-string(SUBSTITUTE("&1 &2" ,STRING(iTime,"HH:MM:SS"), cLevel)).
    ELSE
      edMessageBox:insert-string(SUBSTITUTE("         &1", cLevel)).

    /* Respect newlines inserted by the developer */
    DO iTextPart = 1 TO NUM-ENTRIES(cMessage,"~n"):

      IF iTextPart = 1 THEN
        cTextStart = "".
      ELSE
        cTextStart = FILL(" ",13).

      /* Insert line */
      edMessageBox:insert-string(SUBSTITUTE("&1&2~n"
                                           , cTextStart
                                           , TRIM(ENTRY(iTextPart,cMessage,"~n"))
                                           )
                                ).
    END.

    ASSIGN
      giPrevTime = iTime
      gnPrevProg = pcProgram
      .
  END.
END PROCEDURE. /* showMessage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timerCommand C-Win
PROCEDURE timerCommand :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER picCommand AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER picTimerId AS CHARACTER NO-UNDO.

  DEFINE VARIABLE dtNow AS DATETIME NO-UNDO INITIAL NOW.

  /* Find the timer. Might not exist yet */
  FIND ttTimer WHERE ttTimer.cTimerId = picTimerId NO-ERROR.

  IF NOT AVAILABLE ttTimer THEN DO:
    CREATE ttTimer.
    ASSIGN ttTimer.cTimerId = picTimerId.
  END.

  CASE picCommand:
    WHEN "start" THEN
    DO:
      ASSIGN
        ttTimer.tStartTime = dtNow
        ttTimer.iNumStarts = ttTimer.iNumStarts + 1.

      PUBLISH "debugInfo"(123, SUBSTITUTE("Timer [&1] started", picTimerId)).
    END. /* start */

    WHEN "stop" THEN
    DO:
      ASSIGN
        ttTimer.tTotalTime = ttTimer.tTotalTime + (dtNow - ttTimer.tStartTime)
        .

      PUBLISH "debugInfo"(124, SUBSTITUTE("Timer [&1] stopped, time:&2 ms  num:&3  avg:&4 ms"
                                             , picTimerId
                                             , dtNow - ttTimer.tStartTime
                                             , ttTimer.iNumStarts
                                             , INTEGER(ttTimer.tTotalTime / ttTimer.iNumStarts)
                                             )).
    END. /* stop */
  END CASE. /* case picCommand: */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegistry C-Win
FUNCTION getRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    ) :
/*------------------------------------------------------------------------
  Name         : getRegistry
  Description  : Get a value from DataDigger.ini Not from personal ini!
  ----------------------------------------------------------------------*/
  DEFINE VARIABLE cRegistryValue AS CHARACTER   NO-UNDO.

  USE 'DataDigger'.
  GET-KEY-VALUE SECTION pcSection KEY pcKey VALUE cRegistryValue.
  USE "".

  RETURN cRegistryValue.
END FUNCTION. /* getRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRegistry C-Win
FUNCTION setRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    , pcValue   AS CHARACTER
    ) :
/*------------------------------------------------------------------------
  Name         : getRegistry
  Description  : Get a value from DataDigger.ini Not from personal ini!
  ----------------------------------------------------------------------*/
  USE 'DataDigger'.
  PUT-KEY-VALUE SECTION pcSection KEY pcKey VALUE pcValue NO-ERROR.
  USE "".

  RETURN "".
END FUNCTION. /* setRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME