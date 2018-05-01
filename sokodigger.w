&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wSokoDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wSokoDigger 
/*------------------------------------------------------------------------

  Name : SokoDigger.w
  Desc : Sokoban easter egg for DataDigger :)

  Notes:
    This is a version that includes levels from the great site
    
    http://sokoban-jd.blogspot.nl by Jordi Domènech
              
    Which hosts more information on Sokoban than you'll ever 
    need to know. If you want to play more or different levels,
    just download a level file there, place it in the DD folder
    and load it (see initObject).

 -----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

{DataDigger.i}

DEFINE TEMP-TABLE ttBlock NO-UNDO
  FIELD iPosX   AS INTEGER
  FIELD iPosY   AS INTEGER
  FIELD cType   AS CHARACTER
  FIELD hBlock  AS HANDLE
  FIELD lSolid  AS LOGICAL
  .

DEFINE TEMP-TABLE ttMove NO-UNDO
  FIELD iMoveNr    AS INTEGER
  FIELD cDirection AS CHARACTER /* direction */
  FIELD iDeltaX    AS INTEGER
  FIELD iDeltaY    AS INTEGER
  FIELD rBlock     AS RECID
  .

DEFINE TEMP-TABLE ttLevel NO-UNDO
  FIELD iLevelNr AS INTEGER
  FIELD cData    AS CHARACTER
  .

DEFINE TEMP-TABLE ttImage NO-UNDO
  FIELD cType   AS CHARACTER
  FIELD hImage  AS HANDLE
  .

DEFINE VARIABLE giBlockWidth     AS INTEGER   NO-UNDO.
DEFINE VARIABLE giBlockHeight    AS INTEGER   NO-UNDO.
DEFINE VARIABLE giMaxWidth       AS INTEGER   NO-UNDO.
DEFINE VARIABLE giMaxHeight      AS INTEGER   NO-UNDO.
DEFINE VARIABLE giNumMoves       AS INTEGER   NO-UNDO.
DEFINE VARIABLE giCurrentLevel   AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE glLevelComplete  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giLockCounter    AS INTEGER   NO-UNDO.

PROCEDURE SendMessageA EXTERNAL "user32.dll":
  DEFINE INPUT  PARAMETER hwnd   AS long NO-UNDO.
  DEFINE INPUT  PARAMETER wmsg   AS long NO-UNDO.
  DEFINE INPUT  PARAMETER wparam AS long NO-UNDO.
  DEFINE INPUT  PARAMETER lparam AS long NO-UNDO.
  DEFINE RETURN PARAMETER rc     AS long NO-UNDO.
END PROCEDURE.

PROCEDURE RedrawWindow EXTERNAL "user32.dll":
  DEFINE INPUT PARAMETER v-hwnd  AS LONG NO-UNDO.
  DEFINE INPUT PARAMETER v-rect  AS LONG NO-UNDO.
  DEFINE INPUT PARAMETER v-rgn   AS LONG NO-UNDO.
  DEFINE INPUT PARAMETER v-flags AS LONG NO-UNDO.
  DEFINE RETURN PARAMETER v-ret  AS LONG NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frMain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSavedImage wSokoDigger 
FUNCTION getSavedImage RETURNS HANDLE
  ( pcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wSokoDigger AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU mainMenu MENUBAR
       MENU-ITEM m_Undo         LABEL "&Undo"         
       MENU-ITEM m_Redo         LABEL "&Redo"         
       MENU-ITEM m_Restart      LABEL "Re&start"      
       MENU-ITEM m_Skip         LABEL "S&kip Level"   .


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.8 BY 20.95 WIDGET-ID 100.


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
  CREATE WINDOW wSokoDigger ASSIGN
         HIDDEN             = YES
         TITLE              = "SokoDigger"
         HEIGHT             = 20.95
         WIDTH              = 98.8
         MAX-HEIGHT         = 100
         MAX-WIDTH          = 200
         VIRTUAL-HEIGHT     = 100
         VIRTUAL-WIDTH      = 200
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mainMenu:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wSokoDigger
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wSokoDigger)
THEN wSokoDigger:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wSokoDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSokoDigger wSokoDigger
ON END-ERROR OF wSokoDigger /* SokoDigger */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSokoDigger wSokoDigger
ON F12 OF wSokoDigger /* SokoDigger */
ANYWHERE DO:
  RUN dumpData. /* debug */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSokoDigger wSokoDigger
ON WINDOW-CLOSE OF wSokoDigger /* SokoDigger */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSokoDigger wSokoDigger
ON WINDOW-RESIZED OF wSokoDigger /* SokoDigger */
DO:
  DEFINE VARIABLE lSuppress AS LOGICAL NO-UNDO.
  lSuppress = SESSION:SUPPRESS-WARNINGS.

  RUN lockWindow({&WINDOW-NAME}:HANDLE, YES).
  SESSION:SUPPRESS-WARNINGS = YES.

  RUN resizeWindow.
  RUN drawButtons.
  RUN movePlayer(0,0).

  RUN lockWindow({&WINDOW-NAME}:HANDLE, NO).
  SESSION:SUPPRESS-WARNINGS = lSuppress.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain wSokoDigger
ON BACKSPACE OF FRAME frMain
ANYWHERE
DO:
  IF glLevelComplete THEN RETURN NO-APPLY.
  RUN undoMove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain wSokoDigger
ON CURSOR-DOWN OF FRAME frMain
ANYWHERE
DO:
  IF glLevelComplete THEN RETURN NO-APPLY.
  RUN processKeystroke(INPUT 'down', OUTPUT glLevelComplete).
  IF glLevelComplete THEN RUN levelCompleted.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain wSokoDigger
ON CURSOR-LEFT OF FRAME frMain
ANYWHERE
DO:
  IF glLevelComplete THEN RETURN NO-APPLY.
  RUN processKeystroke(INPUT 'left', OUTPUT glLevelComplete).
  IF glLevelComplete THEN RUN levelCompleted.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain wSokoDigger
ON CURSOR-RIGHT OF FRAME frMain
ANYWHERE
DO:
  IF glLevelComplete THEN RETURN NO-APPLY.
  RUN processKeystroke(INPUT 'right', OUTPUT glLevelComplete).
  IF glLevelComplete THEN RUN levelCompleted.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain wSokoDigger
ON CURSOR-UP OF FRAME frMain
ANYWHERE
DO:
  IF glLevelComplete THEN RETURN NO-APPLY.
  RUN processKeystroke(INPUT 'up', OUTPUT glLevelComplete).
  IF glLevelComplete THEN RUN levelCompleted.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Redo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Redo wSokoDigger
ON CHOOSE OF MENU-ITEM m_Redo /* Redo */
OR 'ctrl-y' OF FRAME frMain ANYWHERE
DO:
  IF NOT glLevelComplete THEN RUN redoMove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Restart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Restart wSokoDigger
ON CHOOSE OF MENU-ITEM m_Restart /* Restart */
OR 'f5' OF FRAME frMain ANYWHERE
DO:
  DEFINE VARIABLE lRestart AS LOGICAL NO-UNDO INITIAL YES.
  MESSAGE 'Start again?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lRestart.
  IF lRestart THEN RUN startLevel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Skip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Skip wSokoDigger
ON CHOOSE OF MENU-ITEM m_Skip /* Skip Level */
DO:
  RUN skipLevel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Undo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Undo wSokoDigger
ON CHOOSE OF MENU-ITEM m_Undo /* Undo */
OR 'ctrl-z' OF FRAME frMain ANYWHERE
DO:
  IF NOT glLevelComplete THEN RUN undoMove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wSokoDigger 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:
  /* Make sure all settings are saved */
  RUN flushRegistry.
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* For debugging in the UIB */
&IF DEFINED(UIB_is_Running) <> 0 &THEN
  RUN startDiggerLib.p.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN initObject.
  RUN startLevel.

  APPLY 'entry' TO FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcBlockSize wSokoDigger 
PROCEDURE calcBlockSize :
/* Calc the blocksize by examining all levels
*/
  DEFINE VARIABLE iPosY AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPosX AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMinX AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMinY AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMaxX AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMaxY AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cLine   AS CHARACTER NO-UNDO.

  DEFINE BUFFER bLevel FOR ttLevel.

  /* Get min/max coordinates of the levels */
  FOR EACH bLevel:
    DO iPosY = 1 TO NUM-ENTRIES(bLevel.cData,'|'):
      cLine = ENTRY(iPosY,bLevel.cData,'|').
  
      DO iPosX = 1 TO LENGTH(cLine):
        IF SUBSTRING(cLine,iPosX,1) = ' ' THEN NEXT.
    
        ASSIGN
          iMinX = MINIMUM(iMinX, iPosX)
          iMaxX = MAXIMUM(iMaxX, iPosX)
          iMinY = MINIMUM(iMinY, iPosY)
          iMaxY = MAXIMUM(iMaxY, iPosY).
      END.
    END.
  END.

  /* Calculate new size for buttons */
  ASSIGN
    giMaxWidth    = (iMaxX - iMinX)
    giMaxHeight   = (iMaxY - iMinY)
    giBlockWidth  = TRUNCATE(FRAME frMain:WIDTH-PIXELS  / giMaxWidth ,0)
    giBlockHeight = TRUNCATE(FRAME frMain:HEIGHT-PIXELS / giMaxHeight,0)
    .

END PROCEDURE. /* calcBlockSize */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createField wSokoDigger 
PROCEDURE createField :
/* Create a field in the tt for an element on the board.
  */
  DEFINE INPUT PARAMETER pcType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piPosX AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER piPosY AS INTEGER   NO-UNDO.

  DEFINE BUFFER bBlock FOR ttBlock.

  /* create a record for each element */
  CREATE bBlock.
  ASSIGN bBlock.iPosX = piPosX
         bBlock.iPosY = piPosY
         bBlock.cType = pcType.

  /* create a widget for each element */
  bBlock.hBlock = getSavedImage(bBlock.cType).

  /* if it is Solid, we cannot walk thru it */
  bBlock.lSolid = NOT CAN-DO('target',pcType).

END PROCEDURE. /* createField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wSokoDigger  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wSokoDigger)
  THEN DELETE WIDGET wSokoDigger.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE drawButtons wSokoDigger 
PROCEDURE drawButtons :
/* Draw the buttons of the level according to current sizes.
  */
  DEFINE BUFFER bBlock FOR ttBlock.

/*   FOR EACH bBlock:                        */
/*     ASSIGN bBlock.hBlock:VISIBLE = FALSE. */
/*   END. /* for each bBlock */              */

  FOR EACH bBlock:
    RUN drawElement(RECID(bBlock), NO).
  END. /* for each bBlock */

  FOR EACH bBlock:
    ASSIGN bBlock.hBlock:VISIBLE = TRUE.
  END. /* for each bBlock */


END PROCEDURE. /* drawButtons */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE drawElement wSokoDigger 
PROCEDURE drawElement :
/* Draw a single element.
  */
  DEFINE INPUT PARAMETER prBlock     AS RECID   NO-UNDO.
  DEFINE INPUT PARAMETER plShowBlock AS LOGICAL NO-UNDO.

  DEFINE BUFFER bBlock FOR ttBlock.

  FIND bBlock WHERE RECID(bBlock) = prBlock.

  ASSIGN
    bBlock.hBlock:X             = ( bBlock.iPosX - 1) * giBlockWidth + 1 
    bBlock.hBlock:Y             = ( bBlock.iPosY - 1) * giBlockHeight + 1
    bBlock.hBlock:WIDTH-PIXELS  = giBlockWidth
    bBlock.hBlock:HEIGHT-PIXELS = giBlockHeight.

END PROCEDURE. /* drawElement */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpData wSokoDigger 
PROCEDURE dumpData :
DEFINE VARIABLE cLine AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cChar AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile AS CHARACTER   NO-UNDO.

  cFile = SESSION:TEMP-DIR + 'SokoDigger.txt'.
  OUTPUT TO VALUE(cFile).

  FOR EACH ttBlock BREAK BY ttBlock.iPosY BY ttBlock.iPosX:

    IF FIRST-OF(ttBlock.iPosY) THEN cLine = FILL(' ',20).

    /* Signs: @ = player  $ = box          * = box on targetplace
    **        # = wall    . = targetplace  + = player on targetplace 
    */
    CASE ttBlock.cType:
      WHEN 'wall'   THEN cChar = '#'.
      WHEN 'box'    THEN cChar = '$'.
      WHEN 'box-ok' THEN cChar = '*'.
      WHEN 'target' THEN cChar = '.'.
      WHEN 'player' THEN cChar = '@'.
    END CASE.

    SUBSTRING(cLine,ttBlock.iPosX,1) = cChar.
    IF LAST-OF(ttBlock.iPosY) THEN PUT UNFORMATTED cLine SKIP.
  END.

  OUTPUT CLOSE. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wSokoDigger  _DEFAULT-ENABLE
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
  VIEW FRAME frMain IN WINDOW wSokoDigger.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW wSokoDigger.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject wSokoDigger 
PROCEDURE initObject :
/* Initialize variables and calculate block width and height.
  */
  DEFINE VARIABLE iColor AS INTEGER NO-UNDO.

  RUN enable_ui.

  /* Set max dimensions for the window to that of the monitor */
  THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
  {&WINDOW-NAME}:MAX-WIDTH-PIXELS = SESSION:WIDTH-PIXELS.
  {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS.

  /* Set bg color of frame to a kind of sand color */
  iColor = COLOR-TABLE:NUM-ENTRIES.
  COLOR-TABLE:NUM-ENTRIES = iColor + 1.
  COLOR-TABLE:SET-DYNAMIC    (iColor,TRUE).
  COLOR-TABLE:SET-RED-VALUE  (iColor, 222).
  COLOR-TABLE:SET-GREEN-VALUE(iColor, 214).
  COLOR-TABLE:SET-BLUE-VALUE (iColor, 173).
  FRAME frMain:BGCOLOR = iColor.

  /* uib */
  IF LOOKUP(PROPATH,'c:\Data\DropBox\progress\Sokoban') = 0 THEN
    PROPATH = PROPATH + ',c:\Data\DropBox\progress\Sokoban'.
  
  RUN readLevelFile(SEARCH('SokoDigger.txt')).
  RUN calcBlockSize.

  /* Get last completed level */
  giCurrentLevel = INTEGER(getRegistry('DataDigger','SokobanLevel')) NO-ERROR.
  IF giCurrentLevel = ? THEN giCurrentLevel = 1.

END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levelCompleted wSokoDigger 
PROCEDURE levelCompleted :
/* Level has been completed.
  */
  MESSAGE "Congratulations, you have completed level" giCurrentLevel "in" giNumMoves " moves." VIEW-AS ALERT-BOX INFORMATION.

  giCurrentLevel = giCurrentLevel + 1.
  RUN startLevel.

END PROCEDURE. /* levelCompleted */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lockWindow wSokoDigger 
PROCEDURE lockWindow :
/* Lock / unlock updates that Windows does to windows.
  */
  DEFINE INPUT PARAMETER phWindow AS HANDLE  NO-UNDO.
  DEFINE INPUT PARAMETER plLock   AS LOGICAL NO-UNDO.

  DEFINE VARIABLE iRet AS INTEGER NO-UNDO.

  /* Locking / unlocking windows */
  &GLOBAL-DEFINE WM_SETREDRAW     11
  &GLOBAL-DEFINE RDW_ALLCHILDREN 128
  &GLOBAL-DEFINE RDW_ERASE         4
  &GLOBAL-DEFINE RDW_INVALIDATE    1

  IF NOT VALID-HANDLE(phWindow) THEN RETURN.

  /* Keep track of locking order like: lock, lock, unlock, unlock
   * then onlu unlock at the last unlock */
  giLockCounter = giLockCounter + (IF plLock THEN 1 ELSE -1).

  IF phWindow:HWND <> ? THEN 
  DO:
    IF giLockCounter > 0 THEN
      RUN SendMessageA(phWindow:HWND, {&WM_SETREDRAW}, 0, 0, OUTPUT iRet).
    ELSE
    DO:
      RUN SendMessageA(phWindow:HWND, {&WM_SETREDRAW}, 1, 0, OUTPUT iRet).
      RUN RedrawWindow(phWindow:HWND, 0, 0, {&RDW_ALLCHILDREN} + {&RDW_ERASE} + {&RDW_INVALIDATE}, OUTPUT iRet).
    END.
  END.
END PROCEDURE. /* lockWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveBlock wSokoDigger 
PROCEDURE moveBlock :
/* Move a block and check if it is placed on a target field.
  */
  DEFINE INPUT PARAMETER prBox  AS RECID   NO-UNDO.
  DEFINE INPUT PARAMETER piDifX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piDifY AS INTEGER NO-UNDO.

  DEFINE BUFFER bBox    FOR ttBlock.
  DEFINE BUFFER bTarget FOR ttBlock.

  FIND bBox WHERE RECID(bBox) = prBox.

  /* Was there a target place underneath the box? */
  FIND bTarget
    WHERE bTarget.iPosX = bBox.iPosX
      AND bTarget.iPosY = bBox.iPosY
      AND bTarget.cType = 'target'
          NO-ERROR.
  IF AVAILABLE bTarget THEN 
  DO:
    bTarget.hBlock:VISIBLE = TRUE.

    RUN saveImage(bBox.cType,bBox.hBlock).
    bBox.cType = 'box'.
    bBox.hBlock = getSavedImage(bBox.cType).
    bBox.hBlock:VISIBLE = TRUE.
  END.

  /* move the box */
  ASSIGN
    bBox.iPosX = bBox.iPosX + piDifX
    bBox.iPosY = bBox.iPosY + piDifY.

  /* Box moved onto target */
  FIND bTarget
    WHERE bTarget.iPosX = bBox.iPosX
      AND bTarget.iPosY = bBox.iPosY
      AND bTarget.cType = 'target'
          NO-ERROR.
  IF AVAILABLE bTarget THEN 
  DO: 
    bTarget.hBlock:VISIBLE = FALSE.

    RUN saveImage(bBox.cType,bBox.hBlock).
    bBox.cType = 'box-ok'.
    bBox.hBlock = getSavedImage(bBox.cType).
    bBox.hBlock:VISIBLE = TRUE.
  END. 

  /* draw box */
  RUN drawElement(prBox, YES).

END PROCEDURE. /* moveBlock */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movePlayer wSokoDigger 
PROCEDURE movePlayer :
/* Move the player to a new position
 */
  DEFINE INPUT PARAMETER piMoveX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER piMoveY AS INTEGER NO-UNDO.

  DEFINE BUFFER bPlayer FOR ttBlock.

  DO WITH FRAME {&FRAME-NAME}:

    /* Move the player to the new position */
    FIND bPlayer WHERE bPlayer.cType = 'player'.
    ASSIGN 
      bPlayer.iPosX = bPlayer.iPosX + piMoveX
      bPlayer.iPosY = bPlayer.iPosY + piMoveY
      .

    /* Place the player's image */
    ASSIGN
      bPlayer.hBlock:VISIBLE       = FALSE
      bPlayer.hBlock:X             = (bPlayer.iPosX - 1) * giBlockWidth + 1
      bPlayer.hBlock:Y             = (bPlayer.iPosY - 1) * giBlockHeight + 1
      bPlayer.hBlock:WIDTH-PIXELS  = giBlockWidth
      bPlayer.hBlock:HEIGHT-PIXELS = giBlockHeight
      bPlayer.hBlock:VISIBLE       = TRUE
      .
  END.

END PROCEDURE. /* movePlayer */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processKeystroke wSokoDigger 
PROCEDURE processKeystroke :
/* Check if the keystroke is a legal move and if the game is completed.
  */
  DEFINE INPUT  PARAMETER pcKeyLabel  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER plCompleted AS LOGICAL   NO-UNDO.

  DEFINE BUFFER bMove   FOR ttMove.
  DEFINE BUFFER bBlock  FOR ttBlock.
  DEFINE BUFFER bBlock2 FOR ttBlock.

  DEFINE VARIABLE lValidMove AS LOGICAL INITIAL ? NO-UNDO.
  DEFINE VARIABLE iNewX      AS INTEGER           NO-UNDO.
  DEFINE VARIABLE iNewY      AS INTEGER           NO-UNDO.

  /* Delete 'future' moves in case the player undoes a number of moves,
  ** redoes some of the undone moves and before he has redone all undone
  ** moves, he gives new keystrokes. All moves which at that point have not
  ** been redone, will be deleted.
  */
  FOR EACH bMove WHERE bMove.iMoveNr > giNumMoves:
    DELETE bMove.
  END.

  /* Register the move. */
  CREATE bMove.
  ASSIGN giNumMoves       = giNumMoves + 1
         bMove.iMoveNr    = giNumMoves
         bMove.cDirection = pcKeyLabel.

  /* Calculate steps */
  CASE pcKeyLabel:
    WHEN 'up'    THEN ASSIGN bMove.iDeltaY = -1.
    WHEN 'down'  THEN ASSIGN bMove.iDeltaY = 1.
    WHEN 'right' THEN ASSIGN bMove.iDeltaX = 1.
    WHEN 'left'  THEN ASSIGN bMove.iDeltaX = -1.
  END CASE.

  /* Calculate new position of the player */
  FIND bBlock WHERE bBlock.cType = 'player'.
  ASSIGN
    iNewX = bBlock.iPosX + bMove.iDeltaX
    iNewY = bBlock.iPosY + bMove.iDeltaY.

  /* Check if move is allowed. Determine wether the position the player
  ** wants to go to is a blank place. In that case, the keystroke need
  ** no further processing.
  */
  FIND bBlock
    WHERE bBlock.iPosX  = iNewX
      AND bBlock.iPosY  = iNewY
      AND bBlock.lSolid = TRUE
          NO-ERROR.

  IF NOT AVAILABLE bBlock THEN
    ASSIGN lValidMove = TRUE.
  ELSE
  DO:
    /* So, not a blank place. Check if it is a block and if the block can be moved
    ** by checking if the place beneath the block is a blank one.
    */
    IF CAN-DO('box,box-ok',bBlock.cType)
      AND NOT CAN-FIND(bBlock
                 WHERE bBlock.iPosX  = iNewX + bMove.iDeltaX
                   AND bBlock.iPosY  = iNewY + bMove.iDeltaY
                   AND bBlock.lSolid = TRUE) THEN
    DO:
      RUN moveBlock(RECID(bBlock), bMove.iDeltaX, bMove.iDeltaY).
      ASSIGN lValidMove = TRUE.

      /* Register the move of the block */
      ASSIGN bMove.rBlock = RECID(bBlock).
    END. /* block */
    ELSE
      ASSIGN lValidMove = FALSE.
  END.

  /* if the move is valid, move the player to the right place */
  IF lValidMove THEN
    RUN movePlayer(bMove.iDeltaX, bMove.iDeltaY).
  ELSE
  DO:
    DELETE bMove. /* otherwise cancel the move */
    ASSIGN giNumMoves = giNumMoves - 1.
  END.

  /*
  ** Check if the level has been completed.
  */
  FIND FIRST bBlock
    WHERE bBlock.cType BEGINS 'box'
      AND NOT CAN-FIND(bBlock2 WHERE bBlock2.iPosX  = bBlock.iPosX
                                 AND bBlock2.iPosY  = bBlock.iPosY
                                 AND bBlock2.cType  = 'target' ) NO-ERROR.
  ASSIGN plCompleted = (NOT AVAILABLE bBlock).

END PROCEDURE. /* processKeystroke */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readLevelFile wSokoDigger 
PROCEDURE readLevelFile :
/* Read a sokoban level file into ttLevel
*/
  DEFINE INPUT PARAMETER pcLevelFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cLine   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iLineNr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLevNum AS INTEGER   NO-UNDO.

  DEFINE BUFFER bLevel FOR ttLevel.

  FIND LAST bLevel NO-ERROR.
  ASSIGN iLevNum = (IF AVAILABLE bLevel THEN bLevel.iLevelNr ELSE 0).

  INPUT FROM VALUE(pcLevelFile).

  #ReadCollection:
  REPEAT:
    /* New level */
    iLevNum = iLevNum + 1.
    CREATE bLevel.
    ASSIGN bLevel.iLevelNr = iLevNum.

    /* Read until level starts */
    REPEAT ON ENDKEY UNDO, LEAVE #ReadCollection:
      IMPORT UNFORMATTED cLine.
      IF cLine MATCHES '*#*' THEN LEAVE. 
    END.
  
    /* Read level data */
    REPEAT ON ENDKEY UNDO, LEAVE #ReadCollection:
      bLevel.cData = bLevel.cData + cLine + '|'.
      IMPORT UNFORMATTED cLine.
      IF NOT cLine MATCHES '*#*' THEN LEAVE. 
    END.

    /* Read level info */
    REPEAT ON ENDKEY UNDO, LEAVE #ReadCollection:
      IF cLine = '' THEN LEAVE. 
      IMPORT UNFORMATTED cLine.
    END.
  END. 

  DELETE bLevel.
  INPUT CLOSE. 

END PROCEDURE. /* readLevelFile */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redoMove wSokoDigger 
PROCEDURE redoMove :
/* Redo the next move in the bMove table
  */
  DEFINE VARIABLE iDifX AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iDifY AS INTEGER     NO-UNDO.

  DEFINE BUFFER bMove FOR ttMove.

  FIND bMove WHERE bMove.iMoveNr = giNumMoves + 1 NO-ERROR.
  IF NOT AVAILABLE bMove THEN RETURN.
  ASSIGN giNumMoves = giNumMoves + 1.

  /* Calculate new coordinates. */
  CASE bMove.cDirection:
    WHEN 'up'    THEN ASSIGN iDifY = -1.
    WHEN 'down'  THEN ASSIGN iDifY = 1.
    WHEN 'right' THEN ASSIGN iDifX = 1.
    WHEN 'left'  THEN ASSIGN iDifX = -1.
  END CASE.

  /* Move the player */
  RUN movePlayer(iDifX,iDifY).

  /* If a block was moved, move it back. */
  IF bMove.rBlock <> ? THEN
    RUN moveBlock ( bMove.rBlock, iDifX, iDifY).

END PROCEDURE. /* redoMove */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow wSokoDigger 
PROCEDURE resizeWindow :
/* Calculate dimensions of frame and buttons, based on parent window.
  */
  DEFINE VARIABLE hParent AS HANDLE NO-UNDO.
  DEFINE VARIABLE hFrame  AS HANDLE NO-UNDO.

  /* Adjust frame to fit in window */
  hFrame = FRAME {&FRAME-NAME}:HANDLE.
  hParent = {&WINDOW-NAME}:HANDLE.

  /* Adjust screen size to make blocks fit nicely */
  hParent:WIDTH-PIXELS  = (INTEGER(hParent:WIDTH-PIXELS / 20) * 20).
  hParent:HEIGHT-PIXELS = (INTEGER(hParent:HEIGHT-PIXELS / 20) * 20).

  ASSIGN
    hFrame:SCROLLABLE            = TRUE

    hFrame:VIRTUAL-WIDTH-PIXELS  = hParent:WIDTH-PIXELS
    hFrame:VIRTUAL-HEIGHT-PIXELS = hParent:HEIGHT-PIXELS

    hFrame:WIDTH-PIXELS          = hParent:WIDTH-PIXELS
    hFrame:VIRTUAL-WIDTH-PIXELS  = hParent:WIDTH-PIXELS

    hFrame:HEIGHT-PIXELS         = hParent:HEIGHT-PIXELS
    hFrame:VIRTUAL-HEIGHT-PIXELS = hParent:HEIGHT-PIXELS

    hFrame:SCROLLABLE            = FALSE
    .

  /* Calc hor/ver sizes of a block */
  RUN calcBlockSize.

END PROCEDURE. /* resizeWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveImage wSokoDigger 
PROCEDURE saveImage :
/* Place image on the image stack for later re-use
*/  
  DEFINE INPUT PARAMETER pcType  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER phImage AS HANDLE    NO-UNDO.
  
  DEFINE BUFFER bImage FOR ttImage.

  IF NOT VALID-HANDLE(phImage) THEN RETURN. 
  ASSIGN phImage:VISIBLE = FALSE.

  FIND FIRST bImage WHERE bImage.hImage = phImage NO-ERROR.
  IF NOT AVAILABLE bImage THEN 
  DO:
    CREATE bImage.
    ASSIGN 
      bImage.cType   = pcType
      bImage.hImage  = phImage.

    /* Resize and reposition the image */
    ASSIGN 
      phImage:X             = 1
      phImage:Y             = 1
      phImage:WIDTH-PIXELS  = 10
      phImage:HEIGHT-PIXELS = 10
      .
  END.
  
END PROCEDURE. /* saveImage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showLevel wSokoDigger 
PROCEDURE showLevel :
/* Read a level from an ascii file and put it in the temp-table.
  */
  DEFINE INPUT PARAMETER piLevel  AS INTEGER NO-UNDO.

  DEFINE VARIABLE cFile    AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iPosY    AS INTEGER            NO-UNDO.
  DEFINE VARIABLE iPosX    AS INTEGER            NO-UNDO.
  DEFINE VARIABLE cLine    AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE cElement AS CHARACTER          NO-UNDO.
  DEFINE VARIABLE iMinX    AS INTEGER INITIAL 20 NO-UNDO.
  DEFINE VARIABLE iMaxX    AS INTEGER INITIAL 0  NO-UNDO.
  DEFINE VARIABLE iMinY    AS INTEGER INITIAL 20 NO-UNDO.
  DEFINE VARIABLE iMaxY    AS INTEGER INITIAL 0  NO-UNDO.

  DEFINE BUFFER bMove  FOR ttMove.
  DEFINE BUFFER bBlock FOR ttBlock.
  DEFINE BUFFER bLevel FOR ttLevel.

  /*
  ** Layout sokoban XSB-file:
  **
  ** Levelsize max 20 x 20 blocks
  ** Signs: @ = player  $ = box          * = box on targetplace
  **        # = wall    . = targetplace  + = player on targetplace
  **
  ** Example:
  **
  **         #####            
  **         #   #            
  **         #$  #            
  **       ###  $##           
  **       #  $ $ #           
  **     ### # ## #   ######  
  **     #   # ## #####  ..#  
  **     # $  $          ..#  
  **     ##### ### #@##  ..#  
  **         #     #########  
  **         #######          
  */
  FIND bLevel WHERE bLevel.iLevelnr = piLevel NO-ERROR.
  IF NOT AVAILABLE bLevel THEN 
  DO:
    MESSAGE 'Cannot find level' piLevel VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    RETURN. 
  END.

  /* Clear the temp-table, save images */
  FOR EACH bBlock:
    RUN saveImage(bBlock.cType, bBlock.hBlock).
    DELETE bBlock.
  END.

  /* Clear the move-table */
  EMPTY TEMP-TABLE bMove.

  /* Load blocks from bLevel */
  DO iPosY = 1 TO NUM-ENTRIES(bLevel.cData,'|'):
    cLine = ENTRY(iPosY,bLevel.cData,'|').

    DO iPosX = 1 TO LENGTH(cLine):
      cElement = SUBSTRING(cLine,iPosX,1).
      IF cElement = ' ' THEN NEXT.

      /* element found */
      CASE cElement:
        /* player */
        WHEN '@' THEN RUN createField('player',iPosX,iPosY).

        /* player on targetplace */
        WHEN '+' THEN
        DO:
          RUN createField('target',iPosX,iPosY).
          RUN createField('player',iPosX,iPosY).
        END.

        /* block */
        WHEN '$' THEN RUN createField('box',iPosX,iPosY).

        /* wall */
        WHEN '#' THEN RUN createField('wall',iPosX,iPosY).

        /* target */
        WHEN '.' THEN RUN createField('target',iPosX,iPosY).

        /* block on targetplace */
        WHEN '*' THEN
        DO:
          RUN createField('target',iPosX,iPosY).
          RUN createField('box-ok',iPosX,iPosY).
        END.
      END CASE. /* cElement */
    END. /* iPosY */
  END. /* iPosX */

  /* Get min/max coordinates of the level */
  FOR EACH bBlock:
    ASSIGN
      iMinX = MINIMUM(iMinX, bBlock.iPosX)
      iMaxX = MAXIMUM(iMaxX, bBlock.iPosX)
      iMinY = MINIMUM(iMinY, bBlock.iPosY)
      iMaxY = MAXIMUM(iMaxY, bBlock.iPosY).
  END.

  /* Center the level */
  FOR EACH bBlock:
    ASSIGN
      bBlock.iPosX = bBlock.iPosX + ROUND((giMaxWidth  - (iMaxX - iMinX + 1)) / 2,0)
      bBlock.iPosY = bBlock.iPosY + ROUND((giMaxHeight - (iMaxY - iMinY + 1)) / 2,0).
  END.

  ASSIGN {&WINDOW-NAME}:TITLE = 'Sokoban level ' + STRING(giCurrentLevel).

END PROCEDURE. /* showLevel */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skipLevel wSokoDigger 
PROCEDURE skipLevel :
/* Goto next level
  */
  
  IF giNumMoves < 5 THEN
  DO:
    MESSAGE "Oh come on, you're barely started" VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN. 
  END.
  
  ASSIGN giCurrentLevel = giCurrentLevel + 1.
  RUN startLevel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startLevel wSokoDigger 
PROCEDURE startLevel :
/* Start a level
  */
  ASSIGN
    glLevelComplete = FALSE
    giNumMoves      = 0.

  setRegistry('DataDigger','SokobanLevel', STRING(giCurrentLevel)).

  RUN lockWindow({&WINDOW-NAME}:HANDLE, YES).
  SESSION:SET-WAIT-STATE('general').

  RUN showLevel(INPUT giCurrentLevel).
  RUN resizeWindow.
  RUN drawButtons.
  RUN movePlayer(0,0).

  RUN lockWindow({&WINDOW-NAME}:HANDLE, NO).
  SESSION:SET-WAIT-STATE('').

END PROCEDURE. /* startLevel */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoMove wSokoDigger 
PROCEDURE undoMove :
/* Undo the last move.
  */
  DEFINE BUFFER bMove FOR ttMove.

  FIND bMove WHERE bMove.iMoveNr = giNumMoves NO-ERROR.
  IF NOT AVAILABLE bMove THEN RETURN.
  ASSIGN giNumMoves = giNumMoves - 1.

  /* Move the player */
  RUN movePlayer(bMove.iDeltaX * -1, bMove.iDeltaY * -1).

  /* If a block was moved, move it back. */
  IF bMove.rBlock <> ? THEN
    RUN moveBlock ( bMove.rBlock, bMove.iDeltaX * -1, bMove.iDeltaY * -1).

END PROCEDURE. /* undoMove */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSavedImage wSokoDigger 
FUNCTION getSavedImage RETURNS HANDLE
  ( pcType AS CHARACTER ) :
  
  DEFINE VARIABLE hImage AS HANDLE NO-UNDO.
  DEFINE BUFFER bImage FOR ttImage.

  FIND FIRST bImage WHERE bImage.cType = pcType NO-ERROR.
  IF AVAILABLE bImage THEN 
  DO:
    hImage = bImage.hImage.
    DELETE bImage.
    RETURN hImage.
  END.

  ELSE 
  DO:
    CREATE IMAGE hImage
    ASSIGN
      FRAME          = FRAME {&FRAME-NAME}:HANDLE
      SENSITIVE      = FALSE
      VISIBLE        = FALSE
      STRETCH-TO-FIT = TRUE
      TRANSPARENT    = TRUE
      .
  
    hImage:LOAD-IMAGE(SUBSTITUTE('image/default_&1.gif', pcType)).

    RETURN hImage.
  END.

END FUNCTION. /* getSavedImage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

