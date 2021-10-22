&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  Name: generate-Assign-Statement.w
  Desc: Generate assign statement for current file

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.
{ DataDigger.i }

&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT PARAMETER pcDatabase AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcTable    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR ttField.
  DEFINE INPUT PARAMETER TABLE FOR ttIndex.
&ELSE
  DEFINE VARIABLE pcDatabase AS CHARACTER   NO-UNDO INITIAL 'sports'.
  DEFINE VARIABLE pcTable    AS CHARACTER   NO-UNDO INITIAL 'sales-rep'.
  
  DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
  RUN datadiggerlib.p PERSISTENT SET hLib.
  THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hLib,SEARCH-TARGET).
  
  RUN getDummyScheme.p(OUTPUT TABLE ttField, OUTPUT TABLE ttIndex).
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-5 edDefinition cbIndent ~
tgSelectedOnly tgLowerCase tgIncludeDb rsBufferName fiBuffer btnCopy 
&Scoped-Define DISPLAYED-OBJECTS edDefinition cbIndent tgSelectedOnly ~
tgLowerCase tgIncludeDb rsBufferName fiBuffer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCopy 
     LABEL "&Copy to clipboard" 
     SIZE-PIXELS 185 BY 24.

DEFINE VARIABLE cbIndent AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "2 Spaces","2",
                     "3 Spaces","3",
                     "4 Spaces","4",
                     "Tab","Tab"
     DROP-DOWN-LIST
     SIZE-PIXELS 80 BY 21 NO-UNDO.

DEFINE VARIABLE edDefinition AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 700 BY 340
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiBuffer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 135 BY 21 NO-UNDO.

DEFINE VARIABLE rsBufferName AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Use table name as-is", 1,
"bCustomer", 2,
"bfCustomer", 3,
"bufCustomer", 4,
"b-customer", 5,
"Other", 99
     SIZE-PIXELS 166 BY 115 TOOLTIP "specify buffer name for the table" NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 180 BY 160.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 185 BY 110.

DEFINE VARIABLE tgIncludeDb AS LOGICAL INITIAL no 
     LABEL "Include &DB Name" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 165 BY 17 TOOLTIP "include DB name in the code" NO-UNDO.

DEFINE VARIABLE tgLowerCase AS LOGICAL INITIAL no 
     LABEL "Keywords in &Lower Case" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 165 BY 17 TOOLTIP "use lower case for keywords" NO-UNDO.

DEFINE VARIABLE tgSelectedOnly AS LOGICAL INITIAL no 
     LABEL "&Selected fields only" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "export only the fields that are selected in the main window" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     edDefinition AT Y 5 X 205 NO-LABEL  
     cbIndent AT Y 17 X 60 COLON-ALIGNED NO-LABEL   
     tgSelectedOnly AT Y 49 X 25  
     tgLowerCase AT Y 69 X 25   
     tgIncludeDb AT Y 89 X 25   
     rsBufferName AT Y 145 X 24 NO-LABEL   
     fiBuffer AT Y 260 X 30 COLON-ALIGNED NO-LABEL   
     btnCopy AT Y 320 X 15   
     "Buffer name" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 123 X 25   
     "Indent:" VIEW-AS TEXT
          SIZE-PIXELS 40 BY 20 AT Y 18 X 25   
     RECT-6 AT Y 5 X 15   
     RECT-5 AT Y 130 X 15   
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183.4 BY 16.81    .


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
         TITLE              = "Generate Assign statement"
         HEIGHT             = 16.95
         WIDTH              = 183.4
         MAX-HEIGHT         = 40
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 40
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generate Assign statement */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate Assign statement */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN saveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Generate Assign statement */
DO:

  RUN windowResized.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME frMain /* Copy to clipboard */
OR 'go' OF FRAME {&FRAME-NAME} ANYWHERE
DO:
  
  CLIPBOARD:VALUE = edDefinition:SCREEN-VALUE.

  APPLY "END-ERROR":U TO FRAME frMain.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBuffer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBuffer C-Win
ON VALUE-CHANGED OF fiBuffer IN FRAME frMain
DO:
  RUN generateCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsBufferName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsBufferName C-Win
ON VALUE-CHANGED OF rsBufferName IN FRAME frMain
DO:
  fiBuffer:SENSITIVE = (SELF:SCREEN-VALUE = '99').
  fiBuffer:MOVE-TO-TOP().
  RUN generateCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSelectedOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSelectedOnly C-Win
ON VALUE-CHANGED OF tgSelectedOnly IN FRAME frMain /* Selected fields only */
, tgLowerCase, cbIndent, tgIncludeDb
DO:
  RUN generateCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
  
  SESSION:DEBUG-ALERT = YES.

  RUN enable_UI.
  RUN initObject.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY edDefinition cbIndent tgSelectedOnly tgLowerCase tgIncludeDb 
          rsBufferName fiBuffer 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE RECT-6 RECT-5 edDefinition cbIndent tgSelectedOnly tgLowerCase 
         tgIncludeDb rsBufferName fiBuffer btnCopy 
      WITH FRAME frMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generateCode C-Win 
PROCEDURE generateCode :
/* generate the code
*/  
  DEFINE VARIABLE cText     AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cMask     AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cIndent   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMaxName  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cTable    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iExt      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cExt      AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.

  DO WITH FRAME frMain:
    ASSIGN cbIndent tgLowerCase tgSelectedOnly tgIncludeDb rsBufferName fiBuffer.
    
    CASE cbIndent:
      WHEN 'tab' THEN cIndent = '~t'.
      WHEN '2'   THEN cIndent = '  '.
      WHEN '3'   THEN cIndent = '   '.
      WHEN '4'   THEN cIndent = '    '.
    END CASE.

    CASE rsBufferName:
      WHEN  1 THEN cTable = pcTable.
      WHEN  2 THEN cTable = SUBSTITUTE('b&1&2'  , CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  3 THEN cTable = SUBSTITUTE('bf&1&2' , CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  4 THEN cTable = SUBSTITUTE('buf&1&2', CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  5 THEN cTable = SUBSTITUTE('b-&1&2' , LOWER(pcTable)).
      WHEN 99 THEN cTable = (IF fiBuffer <> '' THEN fiBuffer ELSE pcTable).
    END CASE.

    /* Prefix with db name */
    IF tgIncludeDb THEN cTable = pcDatabase + '.' + cTable.

    cText = SUBSTITUTE('/* Assign &1 ~n */~n', cTable ).
    cText = cText + (IF tgLowerCase:CHECKED THEN 'assign' ELSE 'ASSIGN').
            
    /* Get max lengths */
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
      iMaxName = MAXIMUM(iMaxName, LENGTH(bField.cFieldName) + (IF bField.iExtent = 0 THEN 0 ELSE LENGTH(STRING(bField.iExtent)) + 2)).
    END. 

    /* Add the fields */
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        

      DO iExt = (IF bField.iExtent = 0 THEN 0 ELSE 1) TO (IF bField.iExtent = 0 THEN 0 ELSE bField.iExtent):

        cMask = '&1~n&2&3.&4 = '.
        cExt  = (IF iExt = 0 THEN '' ELSE SUBSTITUTE('[&1]', iExt)).
        cText = SUBSTITUTE(cMask
                          , cText 
                          , cIndent
                          , cTable
                          , STRING(bField.cFieldName + cExt, FILL('X',iMaxName))
                          ).
      END.
    END. 

    cMask = '&1~n&2.~n'.
    cText = SUBSTITUTE(cMask, cText, cIndent).

    edDefinition:SCREEN-VALUE = cText.
  END.
  
END PROCEDURE. /* generateCode */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject C-Win 
PROCEDURE initObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iSetting AS INTEGER NO-UNDO.
  
  DO WITH FRAME frMain:
    
    /* Prepare window and frame */
    FRAME {&FRAME-NAME}:FONT = getFont('Default').
    edDefinition:FONT = getFont('Fixed').

    {&WINDOW-NAME}:MAX-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS.
    {&WINDOW-NAME}:MAX-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS.

    /* Init the settings in case they do not yet exist */
    /* Window size */
    IF getRegistry('DataDigger:GenerateAssign', 'Window:width' ) = ? THEN setRegistry('DataDigger:GenerateAssign', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).  
    IF getRegistry('DataDigger:GenerateAssign', 'Window:height') = ? THEN setRegistry('DataDigger:GenerateAssign', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ). 

    /* User settings */
    IF getRegistry('DataDigger:GenerateAssign', 'Indent')        = ? THEN setRegistry('DataDigger:GenerateAssign', 'Indent','2').
    IF getRegistry('DataDigger:GenerateAssign', 'SelectedOnly')  = ? THEN setRegistry('DataDigger:GenerateAssign', 'SelectedOnly','no').
    IF getRegistry('DataDigger:GenerateAssign', 'LowerCase')     = ? THEN setRegistry('DataDigger:GenerateAssign', 'LowerCase','no').
    IF getRegistry('DataDigger:GenerateAssign', 'IncludeDbName') = ? THEN setRegistry('DataDigger:GenerateAssign', 'IncludeDbName','no').
    IF getRegistry('DataDigger:GenerateAssign', 'BufferName')    = ? THEN setRegistry('DataDigger:GenerateAssign', 'BufferName','1').

    /* Get user settings */
    cbIndent:SCREEN-VALUE     = getRegistry('DataDigger:GenerateAssign', 'Indent').
    tgSelectedOnly:CHECKED    = LOGICAL(getRegistry('DataDigger:GenerateAssign', 'SelectedOnly')).
    tgLowerCase:CHECKED       = LOGICAL(getRegistry('DataDigger:GenerateAssign', 'LowerCase')).
    tgIncludeDb:CHECKED       = LOGICAL(getRegistry('DataDigger:GenerateAssign', 'IncludeDbName')).
    rsBufferName:SCREEN-VALUE = getRegistry('DataDigger:GenerateAssign', 'BufferName').
    
    IF getRegistry('DataDigger:GenerateAssign', 'BufferNameCustom') <> ? THEN 
      fiBuffer:SCREEN-VALUE   = getRegistry('DataDigger:GenerateAssign', 'BufferNameCustom').

    APPLY 'VALUE-CHANGED' TO cbIndent.
    APPLY 'VALUE-CHANGED' TO rsBufferName.
    
    /* Restore window size */
    iSetting = INTEGER(getRegistry('DataDigger:GenerateAssign', 'Window:width' )) NO-ERROR.
    IF iSetting <> ? THEN {&WINDOW-NAME}:WIDTH-PIXELS = iSetting.

    iSetting = INTEGER(getRegistry('DataDigger:GenerateAssign', 'Window:height')) NO-ERROR.
    IF iSetting <> ? THEN {&WINDOW-NAME}:HEIGHT-PIXELS = iSetting.

    RUN windowResized.
    
  END.
  
END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSettings C-Win 
PROCEDURE saveSettings :
/* Save settings */
    
  DO WITH FRAME {&FRAME-NAME}:
    /* Window size (don't save x,y pos because you might place it on a second monitor and 
     * later try to restore it when your second monitor is gone) 
     */
    setRegistry('DataDigger:GenerateAssign', 'Window:width'    , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).
    setRegistry('DataDigger:GenerateAssign', 'Window:height'   , STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ).
                                                               
    /* User settings */                                        
    setRegistry('DataDigger:GenerateAssign', 'Indent'          , cbIndent:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateAssign', 'SelectedOnly'    , STRING(tgSelectedOnly:CHECKED )).
    setRegistry('DataDigger:GenerateAssign', 'LowerCase'       , STRING(tgLowerCase:CHECKED    )).
    setRegistry('DataDigger:GenerateAssign', 'IncludeDbName'   , STRING(tgIncludeDb:CHECKED    )).
    setRegistry('DataDigger:GenerateAssign', 'BufferName'      , rsBufferName:SCREEN-VALUE      ).

    IF fiBuffer:SCREEN-VALUE <> '' THEN 
      setRegistry('DataDigger:GenerateAssign', 'BufferNameCustom', fiBuffer:SCREEN-VALUE ).
    ELSE
      setRegistry('DataDigger:GenerateAssign', 'BufferNameCustom', ? ).
  END.

END PROCEDURE. /* saveSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE windowResized C-Win 
PROCEDURE windowResized :
/* Window has been resized, adjust editor 
  */
  FRAME {&FRAME-NAME}:SCROLLABLE = YES.
  
  {&WINDOW-NAME}:WIDTH-PIXELS  = MAXIMUM(200, {&WINDOW-NAME}:WIDTH-PIXELS).
  {&WINDOW-NAME}:HEIGHT-PIXELS = MAXIMUM(180, {&WINDOW-NAME}:HEIGHT-PIXELS).

  FRAME {&FRAME-NAME}:WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS .
  FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS.

  edDefinition:WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS - 210.
  edDefinition:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 13.

  FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS .
  FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.

  FRAME {&FRAME-NAME}:SCROLLABLE = NO.

END PROCEDURE. /* windowResized */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


