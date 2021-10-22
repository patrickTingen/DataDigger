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
  DEFINE VARIABLE pcTable    AS CHARACTER   NO-UNDO INITIAL 'customer'.
  
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
tgLowerCase tgIncludeDb tgDisableTriggers tgSuppressValidation ~
tgShowCounter rsBufferName fiBuffer btnCopy 
&Scoped-Define DISPLAYED-OBJECTS edDefinition cbIndent tgLowerCase ~
tgIncludeDb tgDisableTriggers tgSuppressValidation tgShowCounter ~
rsBufferName fiBuffer 

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
     SIZE-PIXELS 555 BY 560
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
     SIZE-PIXELS 185 BY 155.

DEFINE VARIABLE tgDisableTriggers AS LOGICAL INITIAL no 
     LABEL "Disable &Triggers" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "disable triggers" NO-UNDO.

DEFINE VARIABLE tgIncludeDb AS LOGICAL INITIAL no 
     LABEL "Include &DB Name" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 165 BY 17 TOOLTIP "include DB name in the code" NO-UNDO.

DEFINE VARIABLE tgLowerCase AS LOGICAL INITIAL no 
     LABEL "Keywords in &Lower Case" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 165 BY 17 TOOLTIP "use lower case for keywords" NO-UNDO.

DEFINE VARIABLE tgShowCounter AS LOGICAL INITIAL no 
     LABEL "Show Delete &Counter" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "show counter while deleting" NO-UNDO.

DEFINE VARIABLE tgSuppressValidation AS LOGICAL INITIAL no 
     LABEL "Suppress Delete &Validation" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "suppress delete validations from the dictionary" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     edDefinition AT Y 5 X 205 NO-LABEL  
     cbIndent AT Y 17 X 60 COLON-ALIGNED NO-LABEL   
     tgLowerCase AT Y 48 X 25   
     tgIncludeDb AT Y 68 X 25   
     tgDisableTriggers AT Y 90 X 25   
     tgSuppressValidation AT Y 112 X 25   
     tgShowCounter AT Y 134 X 25   
     rsBufferName AT Y 207 X 24 NO-LABEL   
     fiBuffer AT Y 322 X 30 COLON-ALIGNED NO-LABEL   
     btnCopy AT Y 357 X 15   
     "Buffer name" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 185 X 25   
     "Indent:" VIEW-AS TEXT
          SIZE-PIXELS 40 BY 20 AT Y 18 X 25   
     RECT-6 AT Y 5 X 15   
     RECT-5 AT Y 192 X 15   
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 153.4 BY 27.14    .


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
         TITLE              = "Generate Bulk Delete"
         HEIGHT             = 27.14
         WIDTH              = 153.6
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
ON END-ERROR OF C-Win /* Generate Bulk Delete */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate Bulk Delete */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN saveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Generate Bulk Delete */
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


&Scoped-define SELF-NAME cbIndent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbIndent C-Win
ON VALUE-CHANGED OF cbIndent IN FRAME frMain
, tgLowerCase, cbIndent, tgIncludeDb, tgDisableTriggers, tgSuppressValidation, tgShowCounter
DO:
  RUN generateCode.
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
  DISPLAY edDefinition cbIndent tgLowerCase tgIncludeDb tgDisableTriggers 
          tgSuppressValidation tgShowCounter rsBufferName fiBuffer 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE RECT-6 RECT-5 edDefinition cbIndent tgLowerCase tgIncludeDb 
         tgDisableTriggers tgSuppressValidation tgShowCounter rsBufferName 
         fiBuffer btnCopy 
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
  DEFINE VARIABLE cText   AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cHeader AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cIndent AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBuffer AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTable  AS CHARACTER NO-UNDO.
  
  DO WITH FRAME frMain:
    ASSIGN cbIndent tgLowerCase tgIncludeDb rsBufferName fiBuffer 
           tgDisableTriggers tgSuppressValidation tgShowCounter.
    
    CASE cbIndent:
      WHEN 'tab' THEN cIndent = '~t'.
      WHEN '2'   THEN cIndent = '  '.
      WHEN '3'   THEN cIndent = '   '.
      WHEN '4'   THEN cIndent = '    '.
    END CASE.

    CASE rsBufferName:
      WHEN  1 THEN cBuffer = pcTable.
      WHEN  2 THEN cBuffer = SUBSTITUTE('b&1&2'  , CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  3 THEN cBuffer = SUBSTITUTE('bf&1&2' , CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  4 THEN cBuffer = SUBSTITUTE('buf&1&2', CAPS(SUBSTRING(pcTable,1,1)), LOWER(SUBSTRING(pcTable,2))).
      WHEN  5 THEN cBuffer = SUBSTITUTE('b-&1&2' , LOWER(pcTable)).
      WHEN 99 THEN cBuffer = (IF fiBuffer <> '' THEN fiBuffer ELSE pcTable).
    END CASE.

    /* Prefix with db name */
    cTable = (IF tgIncludeDb THEN pcDatabase + '.' ELSE '') + pcTable.

    cHeader = "/*----------------------------------------------------------------------     ~n" 
            + "    File        : delete_<table>                                           ~n"
            + "    Description : Bulk delete for table <table>                            ~n"
            + "                                                                           ~n"
            + "    History:                                                               ~n"
            + "    <today> <userid> Created                                               ~n"
            + "                                                                           ~n"
            + "  ----------------------------------------------------------------------   ~n" 
            + "            This file was generated with the DataDigger                    ~n" 
            + "  ----------------------------------------------------------------------*/ ~n~n".

    IF tgDisableTriggers THEN 
      cText = "DISABLE TRIGGERS FOR LOAD OF <db.table>.~n~n".

    IF tgShowCounter THEN 
      cText = cText + "PAUSE 0 BEFORE-HIDE.~n~n".

    cText = cText
          + "/* RUN delete_<table>. */"
          + "                                                     ~n" 
          + "                                                     ~n" 
          + "PROCEDURE delete_<table>:                            ~n" 
          + "<i>DEFINE BUFFER <buffer> FOR <db.table>.            ~n" 
          + "<i>DEFINE VARIABLE iDeleted AS INTEGER NO-UNDO.      ~n" 
          + "                                                     ~n" 
          + "<i>REPEAT WHILE CAN-FIND(FIRST <buffer>):            ~n" 
          + "                                                     ~n" 
          + "<i><i>DO TRANSACTION:                                ~n" 
          + "                                                     ~n" 
          + "<i><i><i>FOR EACH <buffer>:                          ~n" 
          + "<i><i><i><i>DELETE <buffer> <validate>.              ~n" 
          + "<i><i><i><i>iDeleted = iDeleted + 1.                 ~n" 
          + "<i><i><i><i>IF iDeleted MOD 1000 = 0 THEN LEAVE.     ~n" 
          + "<i><i><i>END.                                        ~n".

    IF tgShowCounter THEN cText = cText 
          + "                                                     ~n" 
          + "<i><i><i>MESSAGE iDeleted '<table> records deleted'. ~n" 
          + "<i><i><i>PROCESS EVENTS.                             ~n".

    cText = cText 
          + "                                                     ~n" 
          + "<i><i>END. /* trans */                               ~n" 
          + "<i>END.                                              ~n" 
          + "END PROCEDURE. /* delete_<table> */                  ~n~n".


    IF tgSuppressValidation THEN 
      cText = REPLACE(cText, '<validate>', 'VALIDATE(YES,"")').
    ELSE 
      cText = REPLACE(cText, '<validate>', '').

    IF tgLowerCase THEN 
    DO:
      cText = LOWER(cText).
      cText = REPLACE(cText, 'ideleted', 'iDeleted').
    END.

    cText = cHeader + cText. 

    cText = REPLACE(cText, '<i>'       , cIndent).
    cText = REPLACE(cText, '<table>'   , pcTable).
    cText = REPLACE(cText, '<db.table>', cTable).
    cText = REPLACE(cText, '<buffer>'  , cBuffer).
    cText = REPLACE(cText, '<today>'   , STRING(TODAY,'99-99-9999')).
    cText = REPLACE(cText, '<userid>'  , getUserName() ).

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
    IF getRegistry('DataDigger:GenerateBulkDelete', 'Window:width' ) = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).  
    IF getRegistry('DataDigger:GenerateBulkDelete', 'Window:height') = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ). 

    /* User settings */
    IF getRegistry('DataDigger:GenerateBulkDelete', 'Indent')             = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'Indent','2').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'SelectedOnly')       = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'SelectedOnly','no').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'LowerCase')          = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'LowerCase','no').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'IncludeDbName')      = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'IncludeDbName','no').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'DisableTriggers')    = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'DisableTriggers','no').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'SuppressValidation') = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'SuppressValidation','no').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'ShowCounter')        = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'ShowCounter','yes').
    IF getRegistry('DataDigger:GenerateBulkDelete', 'BufferName')    = ? THEN setRegistry('DataDigger:GenerateBulkDelete', 'BufferName','1').

    /* Get user settings */
    cbIndent    :SCREEN-VALUE = getRegistry('DataDigger:GenerateBulkDelete', 'Indent').
    rsBufferName:SCREEN-VALUE = getRegistry('DataDigger:GenerateBulkDelete', 'BufferName').
    tgLowerCase         :CHECKED = LOGICAL(getRegistry('DataDigger:GenerateBulkDelete', 'LowerCase')).
    tgIncludeDb         :CHECKED = LOGICAL(getRegistry('DataDigger:GenerateBulkDelete', 'IncludeDbName')).
    tgDisableTriggers   :CHECKED = LOGICAL(getRegistry('DataDigger:GenerateBulkDelete', 'DisableTriggers')).
    tgSuppressValidation:CHECKED = LOGICAL(getRegistry('DataDigger:GenerateBulkDelete', 'SuppressValidation')).
    tgShowCounter       :CHECKED = LOGICAL(getRegistry('DataDigger:GenerateBulkDelete', 'ShowCounter')).
    
    IF getRegistry('DataDigger:GenerateBulkDelete', 'BufferNameCustom') <> ? THEN 
      fiBuffer:SCREEN-VALUE   = getRegistry('DataDigger:GenerateBulkDelete', 'BufferNameCustom').

    APPLY 'VALUE-CHANGED' TO cbIndent.
    APPLY 'VALUE-CHANGED' TO rsBufferName.
    
    /* Restore window size */
    iSetting = INTEGER(getRegistry('DataDigger:GenerateBulkDelete', 'Window:width' )) NO-ERROR.
    IF iSetting <> ? THEN {&WINDOW-NAME}:WIDTH-PIXELS = iSetting.

    iSetting = INTEGER(getRegistry('DataDigger:GenerateBulkDelete', 'Window:height')) NO-ERROR.
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
    setRegistry('DataDigger:GenerateBulkDelete', 'Window:width'    , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).
    setRegistry('DataDigger:GenerateBulkDelete', 'Window:height'   , STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ).
                                                               
    /* User settings */                                              
    setRegistry('DataDigger:GenerateBulkDelete', 'Indent'            , cbIndent:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateBulkDelete', 'LowerCase'         , STRING(tgLowerCase:CHECKED    )).
    setRegistry('DataDigger:GenerateBulkDelete', 'IncludeDbName'     , STRING(tgIncludeDb:CHECKED    )).
    setRegistry('DataDigger:GenerateBulkDelete', 'BufferName'        , rsBufferName:SCREEN-VALUE      ).
    setRegistry('DataDigger:GenerateBulkDelete', 'DisableTriggers'   , STRING(tgDisableTriggers:CHECKED)).
    setRegistry('DataDigger:GenerateBulkDelete', 'SuppressValidation', STRING(tgSuppressValidation:CHECKED)).
    setRegistry('DataDigger:GenerateBulkDelete', 'ShowCounter'       , STRING(tgShowCounter:CHECKED)).

    IF fiBuffer:SCREEN-VALUE <> '' THEN 
      setRegistry('DataDigger:GenerateBulkDelete', 'BufferNameCustom', fiBuffer:SCREEN-VALUE ).
    ELSE
      setRegistry('DataDigger:GenerateBulkDelete', 'BufferNameCustom', ? ).
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


