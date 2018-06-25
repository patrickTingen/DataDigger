&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ DataDigger.i }

/* Parameters Definitions ---                                           */

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
  
  RUN fillTT.
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
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 edDefinition tgSelectedOnly ~
tgLowerCase cbIndent rsPrefix fiPrefix rsDashes fiReplace btnSave 
&Scoped-Define DISPLAYED-OBJECTS edDefinition tgSelectedOnly tgLowerCase ~
cbIndent rsPrefix fiPrefix rsDashes fiReplace 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getClassName C-Win 
FUNCTION getClassName RETURNS CHARACTER
  ( pcName AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHungarian C-Win 
FUNCTION getHungarian RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piLength   AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNameString C-Win 
FUNCTION getNameString RETURNS CHARACTER
  ( pcFieldName AS CHARACTER
  , pcDataType  AS CHARACTER
  , piLength    AS INTEGER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTypeString C-Win 
FUNCTION getTypeString RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piExtent   AS INTEGER
  , piLength   AS INTEGER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     LABEL "&Save class" 
     SIZE-PIXELS 160 BY 24.

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
     SIZE-PIXELS 710 BY 505
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPrefix AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 25 BY 21 NO-UNDO.

DEFINE VARIABLE fiReplace AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 25 BY 21 NO-UNDO.

DEFINE VARIABLE rsDashes AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "As-is", 0,
"Remove dashes", 1,
"Replace with", 2
     SIZE-PIXELS 120 BY 55 TOOLTIP "specify field prefix" NO-UNDO.

DEFINE VARIABLE rsPrefix AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&None", 0,
"Hungarian-&1", 1,
"Hungarian-&2", 2,
"&Fixed", 3
     SIZE-PIXELS 120 BY 80 TOOLTIP "specify field prefix" NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 100.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 78.

DEFINE VARIABLE tgLowerCase AS LOGICAL INITIAL no 
     LABEL "Keywords in &Lower Case" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "use lower case for keywords" NO-UNDO.

DEFINE VARIABLE tgSelectedOnly AS LOGICAL INITIAL no 
     LABEL "&Selected fields only" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "export only the fields that are selected in the main window" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     edDefinition AT Y 5 X 185 NO-LABEL WIDGET-ID 2
     tgSelectedOnly AT Y 10 X 15 WIDGET-ID 8
     tgLowerCase AT Y 30 X 15 WIDGET-ID 22
     cbIndent AT Y 55 X 60 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     rsPrefix AT Y 120 X 30 NO-LABEL WIDGET-ID 28
     fiPrefix AT Y 180 X 125 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     rsDashes AT Y 240 X 30 NO-LABEL WIDGET-ID 62
     fiReplace AT Y 276 X 125 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     btnSave AT Y 330 X 15 WIDGET-ID 36
     "Dashes" VIEW-AS TEXT
          SIZE-PIXELS 65 BY 13 AT Y 220 X 25 WIDGET-ID 70
     "Field Prefix" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 100 X 25 WIDGET-ID 48
     "Indent:" VIEW-AS TEXT
          SIZE-PIXELS 40 BY 20 AT Y 56 X 25 WIDGET-ID 68
     RECT-4 AT Y 105 X 15 WIDGET-ID 46
     RECT-5 AT Y 227 X 15 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180 BY 24.67 WIDGET-ID 100.


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
         TITLE              = "Generate Active Record Class"
         HEIGHT             = 24.76
         WIDTH              = 180
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
ON END-ERROR OF C-Win /* Generate TempTable Include */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generate TempTable Include */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN saveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Generate TempTable Include */
DO:

  RUN windowResized.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME frMain /* Save class */
OR 'go' OF FRAME {&FRAME-NAME} ANYWHERE
DO:
  DEFINE VARIABLE lOkay     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cText     AS LONGCHAR   NO-UNDO. 
  
  cFileName = getClassName(pcTable) + '.cls'.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS ".cls Class file (*.cls)" "*.cls",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1
    ASK-OVERWRITE
    USE-FILENAME
    CREATE-TEST-FILE
    DEFAULT-EXTENSION ".cls"
    SAVE-AS
    UPDATE lOkay.

  IF NOT lOkay THEN
    RETURN NO-APPLY.
    
  cText = edDefinition:SCREEN-VALUE IN FRAME frMain.
  COPY-LOB cText TO FILE cFileName.
  
  APPLY "END-ERROR":U TO FRAME frMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDashes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDashes C-Win
ON VALUE-CHANGED OF rsDashes IN FRAME frMain
DO:
  fiReplace:SENSITIVE = (SELF:SCREEN-VALUE = '2').
  fiReplace:MOVE-TO-TOP().
  RUN generateClass.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsPrefix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsPrefix C-Win
ON VALUE-CHANGED OF rsPrefix IN FRAME frMain
DO:
  
  fiPrefix:SENSITIVE = (SELF:SCREEN-VALUE = '3').
  fiPrefix:MOVE-TO-TOP().
  RUN generateClass.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSelectedOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSelectedOnly C-Win
ON VALUE-CHANGED OF tgSelectedOnly IN FRAME frMain /* Selected fields only */
, tgLowerCase, cbIndent, fiPrefix, fiReplace
DO:
  RUN generateClass.
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
  DISPLAY edDefinition tgSelectedOnly tgLowerCase cbIndent rsPrefix fiPrefix 
          rsDashes fiReplace 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE RECT-4 RECT-5 edDefinition tgSelectedOnly tgLowerCase cbIndent 
         rsPrefix fiPrefix rsDashes fiReplace btnSave 
      WITH FRAME frMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTT C-Win 
PROCEDURE fillTT :
/* Fill tt for testing in UIB
  */
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bIndex FOR ttIndex.
  
  CREATE bField. ASSIGN bField.cFieldName = 'rep-nr'      bField.lShow = TRUE bField.cDataType = 'INTEGER'   bField.cFormat = '>>>9'  bField.cLabel = 'Rep nr'.
  CREATE bField. ASSIGN bField.cFieldName = 'rep-name'    bField.lShow = TRUE bField.cDataType = 'CHARACTER' bField.cFormat = 'x(30)' bField.cLabel = 'Rep name'.
  CREATE bField. ASSIGN bField.cFieldName = 'region'      bField.lShow = FALSE bField.cDataType = 'CHARACTER' bField.cFormat = 'x(8)'  bField.cLabel = 'Region'.
  CREATE bField. ASSIGN bField.cFieldName = 'month-quota' bField.lShow = FALSE bField.cDataType = 'INTEGER'   bField.cFormat = '->,>>>,>>9' bField.cLabel = 'Rep name' bField.iExtent = 12.
         
  CREATE bIndex. ASSIGN bIndex.cIndexName  = 'iPrim'   bIndex.cIndexFlags = 'P U' bIndex.cFieldList  = 'rep-nr'.
  CREATE bIndex. ASSIGN bIndex.cIndexName  = 'iRegion' bIndex.cIndexFlags = ''    bIndex.cFieldList  = 'region,rep-name'.   

END PROCEDURE. /* fillTT */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generateClass C-Win 
PROCEDURE generateClass :
DEFINE VARIABLE cText         AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cPrefix       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cName         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cHeader       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIndent       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMaxName      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iMaxType      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  
  DO WITH FRAME frMain:
    
    ASSIGN rsPrefix cbIndent.
    
    CASE cbIndent:
      WHEN 'tab' THEN cIndent = '~t'.
      WHEN '2'   THEN cIndent = '  '.
      WHEN '3'   THEN cIndent = '   '.
      WHEN '4'   THEN cIndent = '    '.
    END CASE.

    cHeader = SUBSTITUTE('/*----------------------------------------------------------------------*/ ~n' )
            + SUBSTITUTE('    File        : &1 ~n', getClassName(pcTable) )
            + SUBSTITUTE('    Description : Active Record Class definition for &1.&2 ~n', pcDatabase, pcTable )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('    History: ~n' )
            + SUBSTITUTE('    &1 &2 Created ~n', STRING(TODAY,'99/99/9999'), getUserName() )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('  ----------------------------------------------------------------------*/ ~n' )
            + SUBSTITUTE('/*          This .i file was generated with the DataDigger              */ ~n' )
            + SUBSTITUTE('/*----------------------------------------------------------------------*/ ~n' )
            .
            
    cMask = '&1~nBLOCK-LEVEL ON ERROR UNDO, THROW.~n'.
    IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
    cText = SUBSTITUTE(cMask, cHeader).

    cText = SUBSTITUTE('&1~n&2 data.&3.&4'
                      , cText
                      , (IF tgLowerCase:CHECKED THEN 'class' ELSE 'CLASS')
                      , pcDatabase
                      , getClassName(pcTable)
                      ).
                      
    /* Get max lengths */
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
        
      iMaxName   = MAXIMUM(iMaxName  , LENGTH(bField.cFieldName)).
      iMaxType   = MAXIMUM(iMaxType  , LENGTH(TRIM(getTypeString(bField.cDataType, bField.iExtent, 100)) )).
    END. 
    
    /* Inc name lenth when using prefix */
    CASE rsPrefix:
      WHEN 1 THEN iMaxName = iMaxName + 1.
      WHEN 2 THEN iMaxName = iMaxName + 2.
      WHEN 3 THEN iMaxName = iMaxName + LENGTH(fiPrefix:SCREEN-VALUE).
    END CASE.

    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
      
      cMask = '&1DEFINE PUBLIC PROPERTY &2 AS &3 NO-UNDO. GET. SET.'.
      IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
      
      cText = cText + '~n' + SUBSTITUTE(cMask
                                       , cIndent
                                       , getNameString(bField.cFieldName, bField.cDataType, iMaxName)
                                       , getTypeString(bField.cDataType, bField.iExtent, iMaxType)
                                       ).
    END.
    
    cText = cText + '~n' + (IF tgLowerCase:CHECKED THEN 'end class.' ELSE 'END CLASS.'). 
 
    edDefinition:SCREEN-VALUE = cText.
  END.
  
END PROCEDURE. /* generateClass */

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
    IF getRegistry('DataDigger:GenerateActiveRecord', 'Window:width' ) = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).  
    IF getRegistry('DataDigger:GenerateActiveRecord', 'Window:height') = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ). 

    /* User settings */
    IF getRegistry('DataDigger:GenerateActiveRecord', 'SelectedOnly')  = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'SelectedOnly','no').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'UseLowerCase')  = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'UseLowerCase','no').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'Indent')        = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'Indent','2').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'AddPrefix')     = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'AddPrefix','0').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'FixedPrefix')   = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'FixedPrefix','').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'Dashes')        = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'Dashes','0').
    IF getRegistry('DataDigger:GenerateActiveRecord', 'DashReplace')   = ? THEN setRegistry('DataDigger:GenerateActiveRecord', 'DashReplace','').

    /* Get user settings */
    tgSelectedOnly:CHECKED = LOGICAL(getRegistry('DataDigger:GenerateActiveRecord', 'SelectedOnly')).
    tgLowerCase:CHECKED    = LOGICAL(getRegistry('DataDigger:GenerateActiveRecord', 'UseLowerCase')).
    cbIndent:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'Indent').    
    rsPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'AddPrefix').
    fiPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'FixedPrefix').
    rsDashes:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'Dashes').
    fiReplace:SCREEN-VALUE = getRegistry('DataDigger:GenerateActiveRecord', 'DashReplace').
    APPLY 'VALUE-CHANGED' TO rsPrefix.
    APPLY 'VALUE-CHANGED' TO rsDashes.
    
    /* Restore window size */
    iSetting = INTEGER(getRegistry('DataDigger:GenerateActiveRecord', 'Window:width' )) NO-ERROR.
    IF iSetting <> ? THEN {&WINDOW-NAME}:WIDTH-PIXELS = iSetting.

    iSetting = INTEGER(getRegistry('DataDigger:GenerateTT', 'Window:height')) NO-ERROR.
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
    setRegistry('DataDigger:GenerateActiveRecord', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).
    setRegistry('DataDigger:GenerateActiveRecord', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ).

    /* User settings */
    setRegistry('DataDigger:GenerateActiveRecord', 'SelectedOnly' , STRING(tgSelectedOnly:CHECKED) ).
    setRegistry('DataDigger:GenerateActiveRecord', 'UseLowerCase' , STRING(tgLowerCase:CHECKED   ) ).
    setRegistry('DataDigger:GenerateActiveRecord', 'Indent'       , cbIndent:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateActiveRecord', 'SerialReplace', fiReplace:SCREEN-VALUE         ). 
    setRegistry('DataDigger:GenerateActiveRecord', 'AddPrefix'    , rsPrefix:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateActiveRecord', 'FixedPrefix'  , fiPrefix:SCREEN-VALUE          ).
    
    
    /* Get user settings */
    tgSelectedOnly:CHECKED = LOGICAL(getRegistry('DataDigger:GenerateActiveRecord', 'SelectedOnly')).
    tgLowerCase:CHECKED    = LOGICAL(getRegistry('DataDigger:GenerateActiveRecord', 'UseLowerCase')).
    cbIndent:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'Indent').    
    rsPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'AddPrefix').
    fiPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'FixedPrefix').
    rsDashes:SCREEN-VALUE  = getRegistry('DataDigger:GenerateActiveRecord', 'Dashes').
    fiReplace:SCREEN-VALUE = getRegistry('DataDigger:GenerateActiveRecord', 'DashReplace').
    
    
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

  edDefinition:WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS - 190.
  edDefinition:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 15.

  FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS .
  FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.

  FRAME {&FRAME-NAME}:SCROLLABLE = NO.

END PROCEDURE. /* windowResized */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getClassName C-Win 
FUNCTION getClassName RETURNS CHARACTER
  ( pcName AS CHARACTER
  ) :

  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rsDashes fiReplace.
    
    CASE rsDashes:
      WHEN 0 THEN cReturnValue = pcName.
      WHEN 1 THEN 
        DO i = 1 TO NUM-ENTRIES(pcName,'-'):
          cReturnValue = cReturnValue + CAPS(SUBSTRING(ENTRY(i,pcName,'-'),1,1)) + LC(SUBSTRING(ENTRY(i,pcName,'-'),2)).
        END.
      WHEN 2 THEN cReturnValue = REPLACE(pcName,'-',fiReplace:SCREEN-VALUE).
    END CASE.  
  END.

  RETURN cReturnValue + 'Record'.

END FUNCTION. /* getClassName */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHungarian C-Win 
FUNCTION getHungarian RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piLength   AS INTEGER ) :
/* 
 * return a Hungarian prefix for a field 
 */
  CASE pcDataType:
    WHEN 'character'   THEN RETURN (IF piLength = 1 THEN 'c' ELSE 'ch').
    WHEN 'longchar'    THEN RETURN (IF piLength = 1 THEN 'c' ELSE 'lc').
    WHEN 'integer'     THEN RETURN (IF piLength = 1 THEN 'i' ELSE 'in').
    WHEN 'int64'       THEN RETURN (IF piLength = 1 THEN 'i' ELSE 'in').
    WHEN 'decimal'     THEN RETURN (IF piLength = 1 THEN 'd' ELSE 'de').
    WHEN 'date'        THEN RETURN (IF piLength = 1 THEN 't' ELSE 'da').
    WHEN 'datetime'    THEN RETURN (IF piLength = 1 THEN 't' ELSE 'dt').
    WHEN 'datetime-tz' THEN RETURN (IF piLength = 1 THEN 't' ELSE 'dz').
    WHEN 'logical'     THEN RETURN (IF piLength = 1 THEN 'l' ELSE 'lg').
    WHEN 'raw'         THEN RETURN (IF piLength = 1 THEN 'r' ELSE 'rw').
    WHEN 'recid'       THEN RETURN (IF piLength = 1 THEN 'r' ELSE 're').
  END CASE.

END FUNCTION. /* getHungarian */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNameString C-Win 
FUNCTION getNameString RETURNS CHARACTER
  ( pcFieldName AS CHARACTER
  , pcDataType  AS CHARACTER
  , piLength    AS INTEGER
  ) :

  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrefix      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rsDashes fiReplace rsPrefix fiPrefix.
    
    CASE rsDashes:
      WHEN 0 THEN cReturnValue = pcFieldName.
      WHEN 1 THEN 
        DO i = 1 TO NUM-ENTRIES(pcFieldName,'-'):
          cReturnValue = cReturnValue + CAPS(SUBSTRING(ENTRY(i,pcFieldName,'-'),1,1)) + LC(SUBSTRING(ENTRY(i,pcFieldName,'-'),2)).
        END.
      WHEN 2 THEN cReturnValue = REPLACE(pcFieldName,'-',fiReplace:SCREEN-VALUE).
    END CASE.

    /* Capitalize field name before prefix is added */
    IF rsPrefix > 0 THEN OVERLAY(cReturnValue,1,1) = CAPS(SUBSTRING(cReturnValue,1,1)).
    
    CASE rsPrefix:
      WHEN 0 THEN cReturnValue = cReturnValue.
      WHEN 1 THEN cReturnValue = getHungarian(pcDataType,1) + cReturnValue.
      WHEN 2 THEN cReturnValue = getHungarian(pcDataType,2) + cReturnValue.
      WHEN 3 THEN cReturnValue = fiPrefix:SCREEN-VALUE + cReturnValue.
    END CASE.

    /* Fix length */
    IF piLength > 0 THEN
    DO:
      cMask = SUBSTITUTE('X(&1)', piLength ).
      cReturnValue = STRING(cReturnValue, cMask).
    END.

  END.

  RETURN cReturnValue.

END FUNCTION. /* getNameString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTypeString C-Win 
FUNCTION getTypeString RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piExtent   AS INTEGER
  , piLength   AS INTEGER
  ) :

  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask        AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    IF piExtent = 0 THEN
      cReturnValue = pcDataType.
    ELSE 
      cReturnValue = SUBSTITUTE('&1 extent &2', pcDataType, piExtent).

    cReturnValue = (IF tgLowerCase:CHECKED THEN LC(cReturnValue) ELSE CAPS(cReturnValue)).
    cMask = SUBSTITUTE('X(&1)', piLength).
    cReturnValue = STRING(cReturnValue, cMask).

  END.

  RETURN cReturnValue.

END FUNCTION. /* getTypeString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
