&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wAbout 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ datadigger.i }

DEFINE TEMP-TABLE ttBlock NO-UNDO
  FIELD cBlockId AS CHARACTER 
  FIELD cDesc    AS CHARACTER
  FIELD iScore   AS INTEGER
  FIELD hButton  AS HANDLE 
  FIELD iLine    AS INTEGER
  FIELD x1       AS INTEGER
  FIELD x2       AS INTEGER
  FIELD y1       AS INTEGER
  FIELD y2       AS INTEGER
  INDEX iPrim IS PRIMARY cBlockId
  .

DEFINE TEMP-TABLE ttScores NO-UNDO
  FIELD iRank AS INTEGER
  FIELD cName AS CHARACTER
  FIELD cTime AS CHARACTER
  INDEX iPrim IS PRIMARY iRank
  .

DEFINE VARIABLE cGameStatus AS CHARACTER NO-UNDO.

/* For debugging in the UIB */
&IF DEFINED(UIB_is_Running) <> 0 &THEN

RUN startDiggerLib.

PROCEDURE startDiggerLib :
/* Start DiggerLib if it has not already been started
 */
  DEFINE VARIABLE hDiggerLib AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cDiggerLib AS CHARACTER NO-UNDO.

  /* Call out to see if the lib has been started */
  PUBLISH 'DataDiggerLib' (OUTPUT hDiggerLib).

  IF NOT VALID-HANDLE(hDiggerLib) THEN
  DO:
    /* gcProgramDir = SUBSTRING(THIS-PROCEDURE:FILE-NAME,1,R-INDEX(THIS-PROCEDURE:FILE-NAME,'\')). */
    cDiggerLib = THIS-PROCEDURE:FILE-NAME.
    cDiggerLib = REPLACE(cDiggerLib,"\","/").
    cDiggerLib = SUBSTRING(cDiggerLib,1,R-INDEX(cDiggerLib,'/')) + 'DataDiggerLib.p'.
    IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'd:\data\progress\DataDigger\DataDiggerLib.p'.
    IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'd:\data\dropbox\DataDigger\src\DataDiggerLib.p'.
    IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'c:\data\dropbox\DataDigger\src\DataDiggerLib.p'.
    RUN VALUE(cDiggerLib) PERSISTENT SET hDiggerLib.
    SESSION:ADD-SUPER-PROCEDURE(hDiggerLib,SEARCH-TARGET).
  END.

END PROCEDURE. /* startDiggerLib */

&ENDIF

DEFINE VARIABLE giBallX       AS INTEGER NO-UNDO INITIAL -5.
DEFINE VARIABLE giBallY       AS INTEGER NO-UNDO INITIAL -5.
DEFINE VARIABLE giGameStarted AS INTEGER NO-UNDO.
DEFINE VARIABLE giOldMouseX   AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDataDigger BtnOK edChangelog btnTabAbout ~
btnTabChanges 
&Scoped-Define DISPLAYED-OBJECTS edChangelog fiDataDigger-1 fiDataDigger-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD inRange wAbout 
FUNCTION inRange RETURNS LOGICAL
  ( piValue AS INTEGER
  , piMin   AS INTEGER
  , piMax   AS INTEGER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pointInRect wAbout 
FUNCTION pointInRect RETURNS LOGICAL
  ( piX AS INTEGER
  , piY AS INTEGER
  , phRect AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD rangeIntersect wAbout 
FUNCTION rangeIntersect RETURNS LOGICAL
  ( piMin1 AS INTEGER 
  , piMax1 AS INTEGER
  , piMin2 AS INTEGER 
  , piMax2 AS INTEGER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD rectIntersect wAbout 
FUNCTION rectIntersect RETURNS LOGICAL
  ( phRect1 AS HANDLE 
  , phRect2 AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wAbout AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDataDigger  NO-FOCUS FLAT-BUTTON
     LABEL "D" 
     SIZE 6 BY 1.43.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnTabAbout  NO-FOCUS FLAT-BUTTON
     LABEL "About" 
     SIZE 19 BY 1.24.

DEFINE BUTTON btnTabChanges  NO-FOCUS FLAT-BUTTON
     LABEL "Changes" 
     SIZE 19 BY 1.24.

DEFINE VARIABLE edChangelog AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 625 BY 335
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiDataDigger-1 AS CHARACTER FORMAT "X(256)":U INITIAL "DataDigger ~{&&version} - ~{&&edition}" 
      VIEW-AS TEXT 
     SIZE-PIXELS 275 BY 13
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiDataDigger-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Build ~{&&build}" 
      VIEW-AS TEXT 
     SIZE-PIXELS 155 BY 13
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiTime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rcBall
     EDGE-PIXELS 8    ROUNDED 
     SIZE 3 BY .62
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE rcBar
     EDGE-PIXELS 0    ROUNDED 
     SIZE 14 BY .43
     BGCOLOR 1 .

DEFINE BUTTON btGotIt 
     LABEL "I &Got it" 
     SIZE-PIXELS 75 BY 24.

DEFINE VARIABLE edHint AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE-PIXELS 145 BY 65
     BGCOLOR 14 FGCOLOR 9  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDataDigger AT ROW 1.24 COL 2 WIDGET-ID 82
     fiTime AT ROW 1.24 COL 92 COLON-ALIGNED WIDGET-ID 294
     BtnOK AT Y 5 X 545 WIDGET-ID 48
     edChangelog AT Y 70 X 0 NO-LABEL WIDGET-ID 72
     btnTabAbout AT ROW 3.19 COL 1 WIDGET-ID 78
     btnTabChanges AT ROW 3.19 COL 20 WIDGET-ID 80
     fiDataDigger-1 AT Y 5 X 35 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiDataDigger-2 AT Y 20 X 35 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     rcBar AT ROW 3.14 COL 52 WIDGET-ID 84
     rcBall AT ROW 2.43 COL 58 WIDGET-ID 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.6 BY 19.33 WIDGET-ID 100.

DEFINE FRAME frHint
     edHint AT Y 10 X 25 NO-LABEL WIDGET-ID 2
     btGotIt AT Y 80 X 65 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT X 187 Y 173
         SIZE-PIXELS 200 BY 110
         BGCOLOR 14  WIDGET-ID 600.


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
  CREATE WINDOW wAbout ASSIGN
         HIDDEN             = YES
         TITLE              = "About the DataDigger"
         HEIGHT             = 19.33
         WIDTH              = 126.6
         MAX-HEIGHT         = 54
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 54
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wAbout
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frHint:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edChangelog:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiDataDigger-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDataDigger-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTime IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiTime:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE rcBall IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rcBall:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE rcBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rcBar:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FRAME frHint
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frHint:HIDDEN           = TRUE.

ASSIGN 
       edHint:READ-ONLY IN FRAME frHint        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wAbout)
THEN wAbout:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 83
       HEIGHT          = 1.43
       WIDTH           = 6
       WIDGET-ID       = 292
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: BallTimer */
      CtrlFrame:MOVE-AFTER(BtnOK:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wAbout wAbout
ON END-ERROR OF wAbout /* About the DataDigger */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:

  APPLY 'CLOSE' TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wAbout wAbout
ON WINDOW-CLOSE OF wAbout /* About the DataDigger */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wAbout wAbout
ON WINDOW-RESIZED OF wAbout /* About the DataDigger */
DO:
  
  FRAME {&FRAME-NAME}:HEIGHT-PIXELS = wAbout:HEIGHT-PIXELS.
  FRAME {&FRAME-NAME}:WIDTH-PIXELS = wAbout:WIDTH-PIXELS.

  RUN showScrollBars(FRAME {&FRAME-NAME}:HANDLE, NO, NO). /* KILL KILL KILL */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME wAbout
ON F11 OF FRAME DEFAULT-FRAME
DO:
  chCtrlFrame:BallTimer:INTERVAL = max(1,chCtrlFrame:BallTimer:INTERVAL - 10).
  wAbout:TITLE = STRING(chCtrlFrame:BallTimer:INTERVAL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME wAbout
ON F12 OF FRAME DEFAULT-FRAME
DO:
  chCtrlFrame:BallTimer:INTERVAL = chCtrlFrame:BallTimer:INTERVAL + 10.
  wAbout:TITLE = STRING(chCtrlFrame:BallTimer:INTERVAL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frHint
&Scoped-define SELF-NAME btGotIt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGotIt wAbout
ON CHOOSE OF btGotIt IN FRAME frHint /* I Got it */
DO:
  cGameStatus = 'running'.
  FRAME frHint:VISIBLE = FALSE.

  /* Enable ball mover */
  chCtrlFrame:BallTimer:ENABLED = TRUE.

  /* Start timer */
  giGameStarted = MTIME.
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataDigger wAbout
ON CHOOSE OF btnDataDigger IN FRAME DEFAULT-FRAME /* D */
DO:
  RUN showLog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK wAbout
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTabAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabAbout wAbout
ON CHOOSE OF btnTabAbout IN FRAME DEFAULT-FRAME /* About */
or 'ctrl-1' of frame {&frame-name} anywhere
DO:
  run setPage(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTabChanges
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabChanges wAbout
ON CHOOSE OF btnTabChanges IN FRAME DEFAULT-FRAME /* Changes */
or 'ctrl-2' of frame {&frame-name} anywhere
DO:
  run setPage(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wAbout OCX.Tick
PROCEDURE CtrlFrame.BallTimer.Tick .
/*------------------------------------------------------------------------------
    Name : BallTimer.ocx.tick
    Desc : Move the ball
  ------------------------------------------------------------------------------*/

  RUN setBar.
  RUN moveBall.
  RUN setTime.

END PROCEDURE. /* OCX.Tick */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wAbout 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FRAME {&FRAME-NAME}:HIDDEN = YES.
  RUN enable_UI.
  RUN initializeObject.
  FRAME {&FRAME-NAME}:HIDDEN = NO.
  
  RUN fadeWindow(0,240).
  WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS edChangelog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildBlocks wAbout 
PROCEDURE buildBlocks :
/* Build blocks on the screen via button widgets
 */
 DEFINE BUFFER bfBlock FOR ttBlock.
 
 &GLOBAL-DEFINE Border      30
 &GLOBAL-DEFINE RowMargin   10
 &GLOBAL-DEFINE BlockMargin  0
 
 DEFINE VARIABLE xx AS INTEGER NO-UNDO.
 DEFINE VARIABLE yy AS INTEGER NO-UNDO.
 DEFINE VARIABLE ii AS INTEGER NO-UNDO.

 DEFINE VARIABLE iBlockLine  AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iNumLines   AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iNumBlocks  AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iTotalWidth AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iSpace      AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iRest       AS INTEGER     NO-UNDO.

 xx = {&border}.
 yy = 90.
 iBlockLine = 1.

 FOR EACH bfBlock BY bfBlock.iScore DESCENDING:

   CREATE BUTTON bfBlock.hButton
     ASSIGN
       X             = 1
       Y             = 1
       LABEL         = bfBlock.cBlockId
       FRAME         = FRAME {&FRAME-NAME}:HANDLE
       SENSITIVE     = TRUE
       VISIBLE       = TRUE
       WIDTH-PIXELS  = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(bfBlock.cBlockId, getFont('default')) + 15
       HEIGHT-PIXELS = 30
       .
   ON 'CHOOSE' OF bfBlock.hButton PERSISTENT RUN showDesc(bfBlock.cDesc).

   /* Round width up to nearest multiple of 20 */
   bfBlock.hButton:WIDTH-PIXELS = (TRUNCATE(bfBlock.hButton:WIDTH-PIXELS / 20,0) + 1) * 20.

   /* See where it fits */
   IF xx + bfBlock.hButton:WIDTH-PIXELS > (FRAME {&FRAME-NAME}:WIDTH-PIXELS - {&border}) THEN
   DO:
     xx = {&border}.
     yy = yy + bfBlock.hButton:HEIGHT-PIXELS + {&RowMargin}.
     iBlockLine = iBlockLine + 1.
     iNumLines = iBlockLine.
   END.

   bfBlock.hButton:X = xx.
   bfBlock.hButton:Y = yy.
   bfBlock.iLine = iBlockLine.
   xx = xx + bfBlock.hButton:WIDTH-PIXELS + {&BlockMargin}.

 END.

 SESSION:DEBUG-ALERT = TRUE.

 /* Justify blocks */
 DO ii = 1 TO iNumLines:

   /* How much buttons on a row */
   iTotalWidth = 0.
   iNumBlocks = 0.
   FOR EACH bfBlock WHERE bfBlock.iLine = ii:
     iTotalWidth = iTotalWidth + bfBlock.hButton:WIDTH-PIXELS.
     iNumBlocks = iNumBlocks + 1.
   END.

   /* Extra space */
   IF iNumBlocks > 0 THEN
     iSpace = (FRAME {&FRAME-NAME}:WIDTH-PIXELS - (2 * {&border}) - iTotalWidth) / (iNumBlocks ).
   ELSE 
     iSpace = 0.

   iRest = FRAME {&FRAME-NAME}:WIDTH-PIXELS - (2 * {&border}) - iTotalWidth - (iNumBlocks * iSpace).

   /* Redraw buttons */
   xx = {&border}.
   FOR EACH bfBlock WHERE bfBlock.iLine = ii:

     bfBlock.hButton:X = 1. /* to avoid errors while resizing */
     bfBlock.hButton:WIDTH-PIXELS = bfBlock.hButton:WIDTH-PIXELS + iSpace + iRest.
     iRest = 0.
     bfBlock.hButton:X = xx.
     xx = xx + bfBlock.hButton:WIDTH-PIXELS.

     /* Register exact position */
     ASSIGN 
       bfBlock.x1 = bfBlock.hButton:X
       bfBlock.y1 = bfBlock.hButton:Y 
       bfBlock.x2 = bfBlock.hButton:X + bfBlock.hButton:WIDTH-PIXELS
       bfBlock.y2 = bfBlock.hButton:Y + bfBlock.hButton:HEIGHT-PIXELS
       .
   END.
 END.

/*  TEMP-TABLE bfBlock:WRITE-XML('file', 'c:\temp\TempTable.xml',YES,'utf-8', ?). */

END PROCEDURE. /* buildBlocks */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wAbout  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wAbout.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wAbout.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wAbout  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wAbout)
  THEN DELETE WIDGET wAbout.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wAbout  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY edChangelog fiDataDigger-1 fiDataDigger-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW wAbout.
  ENABLE btnDataDigger BtnOK edChangelog btnTabAbout btnTabChanges 
      WITH FRAME DEFAULT-FRAME IN WINDOW wAbout.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY edHint 
      WITH FRAME frHint IN WINDOW wAbout.
  ENABLE edHint btGotIt 
      WITH FRAME frHint IN WINDOW wAbout.
  {&OPEN-BROWSERS-IN-QUERY-frHint}
  VIEW wAbout.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fadeWindow wAbout 
PROCEDURE fadeWindow :
DEFINE INPUT PARAMETER piStartValue AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piEndValue   AS INTEGER NO-UNDO.

  DEFINE VARIABLE iTranparency AS INTEGER NO-UNDO.

  IF piEndValue > piStartValue THEN 
  DO iTranparency = piStartValue TO piEndValue by 24:
    RUN setTransparency( INPUT FRAME {&FRAME-NAME}:HANDLE, iTranparency).
    RUN justWait(20).
  END.

  ELSE
  DO iTranparency = piStartValue TO piEndValue by -24:
    RUN setTransparency( INPUT FRAME {&FRAME-NAME}:HANDLE, iTranparency).
    RUN justWait(20).
  END.

END PROCEDURE. /* fadeWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wAbout 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&FRAME-NAME}:

    fiDataDigger-1:SCREEN-VALUE = "DataDigger {&version} - {&edition}".
    fiDataDigger-2:SCREEN-VALUE = 'Build {&build}'.
    
    FRAME {&FRAME-NAME}:FONT = getFont('Default').
    fiDataDigger-1:FONT      = getFont('Fixed').
    fiDataDigger-2:FONT      = getFont('Fixed').
    edChangelog:FONT         = getFont('Fixed').
    fiTime:FONT              = getFont('Fixed').

    btnDataDigger:LOAD-IMAGE(getImagePath('DataDigger24x24.gif')).
    
    RUN setPage(1).
    RUN setTransparency(INPUT FRAME {&FRAME-NAME}:HANDLE, 1).
    
    /* For some reasons, these #*$&# scrollbars keep coming back */
    RUN showScrollBars(FRAME {&FRAME-NAME}:HANDLE, NO, NO). /* KILL KILL KILL */

  END.

END PROCEDURE. /* initializeObject. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE justWait wAbout 
PROCEDURE justWait :
/* Wait a few miliseconds 
 */
  DEFINE INPUT  PARAMETER piWait AS INTEGER NO-UNDO.
  DEFINE VARIABLE iStart AS INTEGER NO-UNDO.
   
  iStart = ETIME.
  DO WHILE ETIME < iStart + piWait: 
    PROCESS EVENTS.
  END. 

END PROCEDURE. /* justWait */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBall wAbout 
PROCEDURE moveBall :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE BUFFER bfBlock FOR ttBlock.

  FRAME {&FRAME-NAME}:BGCOLOR = ?.

  /* Turn off events when we're running */
  IF cGameStatus <> 'running' THEN RETURN.

  DO WITH FRAME {&FRAME-NAME}:

    /* Gonna hit the top or bottom? */
    IF (rcBall:Y + giBallY) < 0 
      OR (rcBall:Y + rcBall:HEIGHT-PIXELS + giBallY) > FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN 
    DO:
      /* flash when bottom is hit */
      IF rcBall:Y > rcBar:Y THEN FRAME {&FRAME-NAME}:BGCOLOR = 14.
      giBallY = giBallY * -1.
      RETURN.
    END.

    /* Gonna hit the wall? */
    IF (rcBall:X + giBallX) < 0 
      OR (rcBall:X + giBallX) > (FRAME {&FRAME-NAME}:WIDTH-PIXELS - rcBall:WIDTH-PIXELS) THEN 
    DO:
      giBallX = giBallX * -1.
      RETURN.
    END.

    rcBall:Y = rcBall:Y + giBallY.
    FOR EACH bfBlock 
      WHERE ( rcBall:Y >= bfBlock.y1 AND rcBall:Y <= bfBlock.y2 )
         OR ( rcBall:Y + rcBall:HEIGHT-PIXELS >= bfBlock.y1 AND rcBall:Y + rcBall:HEIGHT-PIXELS <= bfBlock.y2 ):

      IF rectIntersect(rcBall:HANDLE, bfBlock.hButton) THEN
      DO:
        giBallY = giBallY * -1.
        DELETE OBJECT bfBlock.hButton.
        DELETE bfBlock.
        RETURN. 
      END.
    END.

    rcBall:X = rcBall:X + giBallX.
    FOR EACH bfBlock 
      WHERE ( rcBall:X >= bfBlock.x1 AND rcBall:X <= bfBlock.x2 )
         OR ( rcBall:X + rcBall:WIDTH-PIXELS >= bfBlock.x1 AND rcBall:X + rcBall:WIDTH-PIXELS <= bfBlock.x2 ):

      IF rectIntersect(rcBall:HANDLE, bfBlock.hButton) THEN
      DO:
        giBallX = giBallX * -1.
        DELETE OBJECT bfBlock.hButton.
        DELETE bfBlock.
        RETURN. 
      END.
    END.

    /* hit the bat? */
    IF rectIntersect(rcBall:HANDLE, rcBar:HANDLE) THEN 
    DO:
      giBallY = giBallY * -1.

      /* Right side ball hits left side of bat */
      IF rcBall:X + 15 < rcBar:X + 20 THEN giBallX = -3 - (RANDOM(1,3) * 2).
      ELSE 

      /* Left side of ball hits right side of bat */
      IF rcBall:X > rcBar:X + 50 THEN giBallX = 3 + (RANDOM(1,3) * 2).
    END.
  END.

END PROCEDURE. /* moveBall */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBar wAbout 
PROCEDURE moveBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piMove AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF    rcBar:X + piMove > 0 
      AND rcBar:X + piMove < (FRAME {&FRAME-NAME}:WIDTH-PIXELS - rcBar:WIDTH-PIXELS - 10) THEN
      rcBar:X = rcBar:X + piMove.
  END.

END PROCEDURE. /* moveBar */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE playGame wAbout 
PROCEDURE playGame :
/* Play some game ...
 */  

  DO WITH FRAME {&FRAME-NAME}:

    /* Enable cursor movement of bar */
    ON 'cursor-right' OF FRAME {&FRAME-NAME} ANYWHERE PERSISTENT RUN moveBar(+20).
    ON 'cursor-left' OF FRAME {&FRAME-NAME} ANYWHERE PERSISTENT RUN moveBar(-20).

    /* Wait for game to start via 'I get it' button */
    REPEAT WHILE cGameStatus = '':
      PROCESS EVENTS. 
    END.

    /* Game is on! */
    #Game:
    REPEAT:
      IF NOT FRAME {&FRAME-NAME}:VISIBLE THEN LEAVE #Game.

      PROCESS EVENTS. 
/*       RUN justWait(5). */
      

    END.

  END.

END PROCEDURE. /* playGame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prepareWindow wAbout 
PROCEDURE prepareWindow :
/* Grow window to desired size and position
  */
  DEFINE VARIABLE iStep     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartH   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartW   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartX   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartY   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartEdH AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartEdW AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndH     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndW     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndY     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndX     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndEdH   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iEndEdW   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNumSteps AS INTEGER     NO-UNDO INITIAL 50.

  DO WITH FRAME {&FRAME-NAME}:
    btnTabAbout:VISIBLE = NO.
    btnTabChanges:VISIBLE = NO.
    BtnOK:VISIBLE = NO.

    ASSIGN 
      iStartH = wAbout:HEIGHT-PIXELS
      iStartW = wAbout:WIDTH-PIXELS
      iStartX = wAbout:X
      iStartY = wAbout:Y
      iEndH   = 800
      iEndW   = 1100
      iEndY   = (SESSION:HEIGHT-PIXELS - iEndH) / 2
      iEndX   = (SESSION:WIDTH-PIXELS - iEndW) / 2

      /* editor box */
      iStartEdH = edChangelog:HEIGHT-PIXELS
      iEndEdH   = 10
      iStartEdW = edChangelog:WIDTH-PIXELS - 40
      iEndEdW   = 80
      .

    DO iStep = 1 TO iNumSteps:
      /* Move vertically */
      wAbout:X             = iStartX + ((iEndX - iStartX)) / iNumSteps * iStep.
      wAbout:Y             = iStartY + ((iEndY - iStartY)) / iNumSteps * iStep.
      wAbout:HEIGHT-PIXELS = iStartH + ((iEndH - iStartH)) / iNumSteps * iStep.
      wAbout:WIDTH-PIXELS  = iStartW + ((iEndW - iStartW)) / iNumSteps * iStep.
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = wAbout:HEIGHT-PIXELS.
      FRAME {&FRAME-NAME}:WIDTH-PIXELS = wAbout:WIDTH-PIXELS.

      edChangelog:HEIGHT-PIXELS = iStartEdH + ((iEndEdH - iStartEdH)) / iNumSteps * iStep.
      edChangelog:Y             = wAbout:HEIGHT-PIXELS - edChangelog:HEIGHT-PIXELS - 40.

      edChangelog:WIDTH-PIXELS = iStartEdW + ((iEndEdW - iStartEdW)) / iNumSteps * iStep.
      edChangelog:X            = (wAbout:WIDTH-PIXELS - edChangelog:WIDTH-PIXELS) / 2.

      RUN justWait(5).
    END.

    edChangelog:VISIBLE = FALSE.
    edChangelog:SENSITIVE = FALSE.
    fiTime:VISIBLE = TRUE.

  END.

END PROCEDURE. /* prepareWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readAboutFile wAbout 
PROCEDURE readAboutFile :
/* Build blocks with names of all contributors 
 **/
  DEFINE VARIABLE cLine     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cName     AS CHARACTER   NO-UNDO.
  DEFINE BUFFER bfBlock FOR ttBlock.

  INPUT FROM 'DataDigger.txt'.
  SEEK INPUT TO 300. /* random nr after header */
  
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine BEGINS 'DataDigger' THEN NEXT. /* lines with version name */
    IF NOT cLine MATCHES '*(*)' THEN NEXT. /* does not end with brackets */
    cName = TRIM( ENTRY(NUM-ENTRIES(cLine,'('),cLine,'(' ), ')').
    IF cName = '' THEN NEXT.  /* blank name */

    FIND bfBlock WHERE bfBlock.cBlockId = cName NO-ERROR.
    IF NOT AVAILABLE bfBlock THEN
    DO:
      CREATE bfBlock.
      ASSIGN bfBlock.cBlockId = cName. 
    END.

    ASSIGN 
      bfBlock.cDesc  = TRIM(bfBlock.cDesc + '~n' + REPLACE(cLine,'(' + cName + ')', ''), '~n')
      bfBlock.iScore = bfBlock.iScore + 1.
  END.
  
  INPUT CLOSE. 

END PROCEDURE. /* readAboutFile */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBall wAbout 
PROCEDURE setBall :
DEFINE VARIABLE xx   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE yy   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dx   AS DECIMAL NO-UNDO INIT 1. /* hor speed */
  DEFINE VARIABLE dy   AS DECIMAL NO-UNDO INIT 0. /* ver speed */
  DEFINE VARIABLE elas AS DECIMAL NO-UNDO INIT .85. /* perc speed left after bounce */
  DEFINE VARIABLE grav AS DECIMAL NO-UNDO INIT .2. /* gravity acceleration */

  DO WITH FRAME {&FRAME-NAME}:
    rcBar:X = 280.
    rcBar:Y = 766.
    rcBar:VISIBLE = TRUE.

    rcBall:X = 1.
    rcBall:Y = 575.
    rcBall:VISIBLE = TRUE.

    yy = rcBall:Y.
    xx = rcBall:X.
  END.

  REPEAT:
    /* Normal flow */
    dy = dy + grav.
    xx = xx + dx.
    yy = yy + dy.
    
    /* Bounce at bottom of frame */
    IF xx < 280 AND yy > (FRAME {&FRAME-NAME}:HEIGHT-PIXELS - rcBall:HEIGHT-PIXELS) THEN
    DO:
      yy = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - rcBall:HEIGHT-PIXELS.
      dy = -1 * dy.
      dy = dy * elas.
      IF xx > 305 THEN LEAVE. 
    END.

    /* Bounce at the bat */
    IF xx > 280 AND yy > (rcBar:Y - rcBall:HEIGHT-PIXELS) THEN
    DO:
      yy = rcBar:Y - rcBall:HEIGHT-PIXELS.
      dy = -1 * dy.
      dy = dy * elas * elas * elas.
      dx = dx * elas.
      IF xx > 305 THEN LEAVE. 
    END.

    rcBall:X = xx.
    rcBall:Y = yy.

    RUN justWait(12).
  END. 

  rcBall:X = 305.

  /* Move ball and bat to center */
  REPEAT WHILE rcBall:X < (FRAME {&FRAME-NAME}:WIDTH-PIXELS / 2):
    rcBall:X = rcBall:X + 5.
    rcBar:X = rcBar:X + 5.
    RUN justWait(12).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBar wAbout 
PROCEDURE setBar :
DEFINE VARIABLE iMouseX    AS INTEGER NO-UNDO.
  DEFINE VARIABLE iMouseY    AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    
    RUN getMouseXY(INPUT FRAME {&FRAME-NAME}:HANDLE, OUTPUT iMouseX, OUTPUT iMouseY).

    IF giOldMouseX <> iMouseX
      AND iMouseX > (rcBar:WIDTH-PIXELS / 2) 
      AND iMouseX < (FRAME {&FRAME-NAME}:WIDTH-PIXELS - (rcBar:WIDTH-PIXELS / 2)) THEN 
    DO:
      rcBar:X = iMouseX - (rcBar:WIDTH-PIXELS / 2).
      giOldMouseX = iMouseX.
    END.                   
  END.

END PROCEDURE. /* setBar */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPage wAbout 
PROCEDURE setPage :
/*------------------------------------------------------------------------
  Name         : setPage
  Description  : Activate either the About or the Changes tab

  ----------------------------------------------------------------------
  7-9-2012 pti Created
  ----------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piPage AS INTEGER     NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    edChangelog:SCREEN-VALUE = "".

    CASE piPage:
      WHEN 1 THEN DO:
        btnTabAbout  :LOAD-IMAGE( getImagePath('tab_about_active.gif'    )).
        btnTabChanges:LOAD-IMAGE( getImagePath('tab_changes_inactive.gif' )).

        edChangeLog:INSERT-FILE(getProgramDir() + 'DataDiggerAbout.txt').
        edChangeLog:CURSOR-OFFSET = 1.
      END.
  
      WHEN 2 THEN DO:
        btnTabAbout  :LOAD-IMAGE( getImagePath('tab_about_inactive.gif'    )).
        btnTabChanges:LOAD-IMAGE( getImagePath('tab_changes_active.gif' )).

        edChangeLog:INSERT-FILE(getProgramDir() + 'DataDigger.txt').
        edChangeLog:CURSOR-OFFSET = 1.
      END.                                          
    END CASE. /* piPage */
  END.
  
END PROCEDURE. /* setPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTime wAbout 
PROCEDURE setTime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fiTime:SCREEN-VALUE = STRING((MTIME - giGameStarted) / 1000,'>>>9.9').    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showDesc wAbout 
PROCEDURE showDesc :
/* Show contributions of a user
 */
  DEFINE INPUT PARAMETER pcText AS CHARACTER NO-UNDO.

  MESSAGE pcText VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* showDesc */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showHint wAbout 
PROCEDURE showHint :
/* Show a hint to the user to play
 */
   
  DO WITH FRAME frHint:

    FRAME frHint:Y = 500.
    FRAME frHint:X = (FRAME {&FRAME-NAME}:WIDTH-PIXELS - FRAME frHint:WIDTH-PIXELS) / 2.
    FRAME frHint:VISIBLE = TRUE. 

    edHint:SCREEN-VALUE = "Ah, come on...~n~nYou know what to do. ~nGo bounce 'em all!". 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showLog wAbout 
PROCEDURE showLog :
/* Play arkanoid-like game 
 */ 

  RUN prepareWindow.
  RUN readAboutFile.
  RUN buildBlocks.
  RUN setBall.
  RUN showHint.
  RUN playGame.
  
END PROCEDURE. /* showLog */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION inRange wAbout 
FUNCTION inRange RETURNS LOGICAL
  ( piValue AS INTEGER
  , piMin   AS INTEGER
  , piMax   AS INTEGER
  ) :

  RETURN (piValue >= piMin) AND (piValue <= piMax).

END FUNCTION. /* inRange */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pointInRect wAbout 
FUNCTION pointInRect RETURNS LOGICAL
  ( piX AS INTEGER
  , piY AS INTEGER
  , phRect AS HANDLE ) :

  RETURN inRange(piX, phRect:X, phRect:X + phRect:WIDTH-PIXELS) 
     AND inRange(piY, phRect:Y, phRect:Y + phRect:HEIGHT-PIXELS).

END FUNCTION. /* pointInRect */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION rangeIntersect wAbout 
FUNCTION rangeIntersect RETURNS LOGICAL
  ( piMin1 AS INTEGER 
  , piMax1 AS INTEGER
  , piMin2 AS INTEGER 
  , piMax2 AS INTEGER
  ) :

  RETURN MAXIMUM(piMin1,piMax1) >= MINIMUM(piMin2,piMax2) 
     AND MINIMUM(piMin1,piMax1) <= MAXIMUM(piMin2,piMax2).

END FUNCTION. /* rangeIntersect */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION rectIntersect wAbout 
FUNCTION rectIntersect RETURNS LOGICAL
  ( phRect1 AS HANDLE 
  , phRect2 AS HANDLE ) :

  RETURN 
    rangeIntersect( phRect1:X, phRect1:X + phRect1:WIDTH-PIXELS
                  , phRect2:X, phRect2:X + phRect2:WIDTH-PIXELS)
    AND

    rangeIntersect( phRect1:Y, phRect1:Y + phRect1:HEIGHT-PIXELS
                  , phRect2:Y, phRect2:Y + phRect2:HEIGHT-PIXELS).

END FUNCTION. /* rectIntersect */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

