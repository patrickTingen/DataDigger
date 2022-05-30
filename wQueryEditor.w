&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/* Header goes here
*/
{DataDigger.i}

CREATE WIDGET-POOL.

DEFINE INPUT        PARAMETER phParentWindow AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER TABLE FOR ttField.
DEFINE INPUT-OUTPUT PARAMETER pcQuery        AS CHARACTER NO-UNDO.
DEFINE       OUTPUT PARAMETER plOk           AS LOGICAL   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frWhere

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBegins rctQueryButtons cbAndOr cbFields ~
cbOperator ficValue btnInsert ficWhere btnOr btnOK btnCancel btnAnd ~
btnBracket btnContains btnEq btnGT btnLT btnMatches btnNE btnQt btnToday 
&Scoped-Define DISPLAYED-OBJECTS cbAndOr cbFields cbOperator ficValue ~
ficWhere 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnBegins rctQueryButtons cbAndOr cbFields cbOperator ~
ficValue btnInsert btnOr btnAnd btnBracket btnContains btnEq btnGT btnLT ~
btnMatches btnNE btnQt btnToday 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnd  NO-FOCUS
     LABEL "and" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnBegins  NO-FOCUS
     LABEL "begins" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnBracket  NO-FOCUS
     LABEL "()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnCancel DEFAULT 
     LABEL "Cancel" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnContains  NO-FOCUS
     LABEL "contains" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnEq  NO-FOCUS
     LABEL "=" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnGT  NO-FOCUS
     LABEL ">" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnInsert 
     LABEL "+" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "insert the expression into the where field".

DEFINE BUTTON btnLT  NO-FOCUS
     LABEL "<" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnMatches  NO-FOCUS
     LABEL "matches" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnNE  NO-FOCUS
     LABEL "<>" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnOr  NO-FOCUS
     LABEL "or" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnQt  NO-FOCUS
     LABEL "~"~"" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 21 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnToday  NO-FOCUS
     LABEL "today" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 21 TOOLTIP "insert this text into the where field".

DEFINE VARIABLE cbAndOr AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Where" 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE-PIXELS 50 BY 21 TOOLTIP "preceding AND or OR for the expression" NO-UNDO.

DEFINE VARIABLE cbFields AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     DROP-DOWN-LIST
     SIZE-PIXELS 186 BY 21 TOOLTIP "field used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE cbOperator AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","=","<>",">",">=","<","<=","begins","matches","contains" 
     DROP-DOWN-LIST
     SIZE-PIXELS 85 BY 21 TOOLTIP "operator used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE ficWhere AS CHARACTER 
     CONTEXT-HELP-ID 1050
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 501 BY 175 TOOLTIP "alt-cursor-up / down to view/hide query editor"
     FONT 2 NO-UNDO.

DEFINE VARIABLE ficValue AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS FILL-IN 
     SIZE-PIXELS 210 BY 23 TOOLTIP "the literal value for the expression"
     FONT 2 NO-UNDO.

DEFINE RECTANGLE rctQueryButtons
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 610 BY 190.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frWhere
     btnBegins AT Y 123 X 17
     cbAndOr AT Y 5 X 46 COLON-ALIGNED
     cbFields AT Y 5 X 100 COLON-ALIGNED NO-LABEL
     cbOperator AT Y 5 X 286 COLON-ALIGNED NO-LABEL
     ficValue AT Y 5 X 371 COLON-ALIGNED NO-LABEL
     btnInsert AT Y 5 X 595
     ficWhere AT Y 35 X 110 NO-LABEL
     btnOr AT Y 101 X 57
     btnOK AT Y 231 X 460
     btnCancel AT Y 231 X 540
     btnAnd AT Y 101 X 17
     btnBracket AT Y 79 X 17
     btnContains AT Y 145 X 17
     btnEq AT Y 35 X 17
     btnGT AT Y 57 X 57
     btnLT AT Y 57 X 17
     btnMatches AT Y 167 X 17
     btnNE AT Y 35 X 57
     btnQt AT Y 79 X 57
     btnToday AT Y 189 X 17
     "or ESC to undo your changes" VIEW-AS TEXT
          SIZE-PIXELS 340 BY 15 AT Y 243 X 9
          FGCOLOR 7 
     "Use ALT-UP to take the query back to the main window" VIEW-AS TEXT
          SIZE-PIXELS 340 BY 15 AT Y 227 X 10
          FGCOLOR 7 
     rctQueryButtons AT Y 30 X 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.8 BY 13.71
         TITLE "Query Editor".


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Query Editor"
         HEIGHT             = 13.76
         WIDTH              = 126
         MAX-HEIGHT         = 14.05
         MAX-WIDTH          = 136
         VIRTUAL-HEIGHT     = 14.05
         VIRTUAL-WIDTH      = 136
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frWhere
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frWhere:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAnd IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnBegins IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnBracket IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnContains IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnEq IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnGT IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnInsert IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnLT IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnMatches IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnNE IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnOr IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnQt IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnToday IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbAndOr IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbFields IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbOperator IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR FILL-IN ficValue IN FRAME frWhere
   1                                                                    */
ASSIGN 
       ficWhere:RETURN-INSERTED IN FRAME frWhere  = TRUE.

/* SETTINGS FOR RECTANGLE rctQueryButtons IN FRAME frWhere
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Query Editor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 

DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Query Editor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE" TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frWhere C-Win
ON ALT-W OF FRAME frWhere /* Query Editor */
ANYWHERE DO:
  APPLY 'entry' TO cbAndOr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frWhere C-Win
ON GO OF FRAME frWhere /* Query Editor */
OR 'ALT-CURSOR-UP' OF FRAME frWhere ANYWHERE
DO:
  pcQuery = formatQueryString(ficWhere:SCREEN-VALUE, NO).
  plOk = TRUE.

  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnd C-Win
ON CHOOSE OF btnAnd IN FRAME frWhere /* and */
, btnOr, btnEq, btnNe, btnGt, btnLt, btnToday, btnMatches, btnContains, btnBegins
DO:
  /* No text selected */
  IF ficWhere:SELECTION-TEXT = "" THEN
    ficWhere:INSERT-STRING(SUBSTITUTE(' &1 ', SELF:LABEL)).
  ELSE
    ficWhere:REPLACE-SELECTION-TEXT(SUBSTITUTE(' &1 ', SELF:LABEL)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBracket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBracket C-Win
ON CHOOSE OF btnBracket IN FRAME frWhere /* () */
DO:
  /* No text selected */
  IF ficWhere:SELECTION-TEXT = "" THEN
  DO:
    ficWhere:INSERT-STRING(SUBSTITUTE(' &1 ', SELF:LABEL)).
    ficWhere:CURSOR-OFFSET = ficWhere:CURSOR-OFFSET - 2.
  END.
  ELSE
    ficWhere:REPLACE-SELECTION-TEXT(SUBSTITUTE(' ( &1 ) ', ficWhere:SELECTION-TEXT)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME frWhere /* Cancel */
OR 'LEAVE' OF {&WINDOW-NAME} 
DO:
  plOk = NO.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnInsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnInsert C-Win
ON CHOOSE OF btnInsert IN FRAME frWhere /* + */
OR "return" OF ficValue
DO:
  DEFINE BUFFER bField FOR ttField.
  DEFINE VARIABLE cField AS CHARACTER NO-UNDO.

  FIND bField WHERE bField.cFullName = cbFields:SCREEN-VALUE NO-ERROR.
  IF NOT AVAILABLE bField THEN RETURN.
  cField = bField.cFullName.

  IF cField = 'RECID' OR cField = 'ROWID'
    THEN cField = SUBSTITUTE('&1(&2)', cField, bField.cTablename ).

  ficWhere:INSERT-STRING(LEFT-TRIM(SUBSTITUTE('&1 &2 &3 &4&5'
                                        , (IF cbAndOr:SCREEN-VALUE = ? THEN '' ELSE cbAndOr:SCREEN-VALUE)
                                        , cField
                                        , cbOperator:SCREEN-VALUE
                                        , IF bField.cDataType = 'character' THEN QUOTER(ficValue:SCREEN-VALUE) ELSE ficValue:SCREEN-VALUE
                                        , CHR(13)
                                        )
                              )
                         ).

  APPLY "entry" TO cbAndOr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnQt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQt C-Win
ON CHOOSE OF btnQt IN FRAME frWhere /* "" */
DO:
  /* No text selected */
  IF ficWhere:SELECTION-TEXT = "" THEN
  DO:
    ficWhere:INSERT-STRING(SUBSTITUTE(' &1 ', SELF:LABEL)).
    ficWhere:CURSOR-OFFSET = ficWhere:CURSOR-OFFSET - 2.
  END.
  ELSE
    ficWhere:REPLACE-SELECTION-TEXT(SUBSTITUTE('"&1"', ficWhere:SELECTION-TEXT)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAndOr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAndOr C-Win
ON RETURN OF cbAndOr IN FRAME frWhere /* Where */
DO:
  APPLY 'entry' TO cbFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFields C-Win
ON RETURN OF cbFields IN FRAME frWhere
DO:
  APPLY 'entry' TO cbOperator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOperator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOperator C-Win
ON RETURN OF cbOperator IN FRAME frWhere
DO:
  APPLY 'entry' TO ficValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ficValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficValue C-Win
ON ENTRY OF ficValue IN FRAME frWhere
DO:
  IF SELF:screen-value = "" THEN
    SELF:screen-value = getLinkInfo(cbFields:screen-value).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ficWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficWhere C-Win
ON CTRL-A OF ficWhere IN FRAME frWhere
DO:
  SELF:SET-SELECTION(1,LENGTH(SELF:SCREEN-VALUE) + NUM-ENTRIES(SELF:SCREEN-VALUE,'~n')).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficWhere C-Win
ON CTRL-D OF ficWhere IN FRAME frWhere
DO:
  DEFINE VARIABLE i AS INTEGER     NO-UNDO.
  i = SELF:CURSOR-OFFSET.
  SELF:SET-SELECTION(0,0).
  SELF:CURSOR-OFFSET = i.
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
   
  RUN initObject.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE FOCUS ficWhere.
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
  /* Hide all frames. */
  HIDE FRAME frWhere.
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
  DISPLAY cbAndOr cbFields cbOperator ficValue ficWhere 
      WITH FRAME frWhere.
  ENABLE btnBegins rctQueryButtons cbAndOr cbFields cbOperator ficValue 
         btnInsert ficWhere btnOr btnOK btnCancel btnAnd btnBracket btnContains 
         btnEq btnGT btnLT btnMatches btnNE btnQt btnToday 
      WITH FRAME frWhere.
  {&OPEN-BROWSERS-IN-QUERY-frWhere}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject C-Win 
PROCEDURE initObject :
/* Init
*/
  DEFINE VARIABLE iDefaultFont AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iFixedFont   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFieldList   AS CHARACTER   NO-UNDO.
  
  DO WITH FRAME frWhere:
  
    iDefaultFont = getFont("Default").
    iFixedFont = getFont("Fixed").
    
    {&_proparse_ prolint-nowarn(overflow)}
    ASSIGN 
      FRAME frWhere:FONT = iDefaultFont
      FRAME frWhere:X    = (phParentWindow:WIDTH-PIXELS - FRAME frWhere:WIDTH-PIXELS) / 2
      FRAME frWhere:Y    = (phParentWindow:HEIGHT-PIXELS - FRAME frWhere:HEIGHT-PIXELS) / 2.

    cbAndOr:FONT     = iFixedFont.
    cbFields:FONT    = iFixedFont.
    cbOperator:FONT  = iFixedFont.
    ficValue:FONT    = iFixedFont.
    ficWhere:FONT    = iFixedFont.
    btnEq:FONT       = iFixedFont.
    btnNe:FONT       = iFixedFont.
    btnGt:FONT       = iFixedFont.
    btnLt:FONT       = iFixedFont.
    btnBracket:FONT  = iFixedFont.
    btnQt:FONT       = iFixedFont.
    btnAnd:FONT      = iFixedFont.
    btnOr:FONT       = iFixedFont.
    btnBegins:FONT   = iFixedFont.
    btnContains:FONT = iFixedFont.
    btnMatches:FONT  = iFixedFont.
    btnToday:FONT    = iFixedFont.
    
    btnInsert:LOAD-IMAGE(getImagePath("Add.gif")).
    ficValue:TOOLTIP = ficValue:TOOLTIP + "~n~n(CTRL-ENTER) execute".
    ficWhere:TOOLTIP = ficWhere:TOOLTIP + "~n~n(CTRL-ENTER) execute".
    
    /* Get a list of all fields (extents NOT expanded) */
    FOR EACH ttField:
      cFieldList = cFieldList + ',' + ttField.cFullname.
    END.

    /* Set list of fields in field combo */
    cbFields:LIST-ITEMS     = cFieldList.
    cbAndOr:SCREEN-VALUE    = ENTRY(1,cbAndOr:LIST-ITEMS).
    cbFields:SCREEN-VALUE   = ENTRY(1,cbFields:LIST-ITEMS).
    cbOperator:SCREEN-VALUE = ENTRY(1,cbOperator:LIST-ITEMS).
    
    RUN enable_UI.

    pcQuery = formatQueryString(pcQuery, YES).
    ficWhere:SCREEN-VALUE  = pcQuery.
    ficWhere:CURSOR-OFFSET = LENGTH(pcQuery) + NUM-ENTRIES(pcQuery,'~n') + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

