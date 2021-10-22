&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  Name: generate-TempTable-Include.w
  Desc: Generate include file with TT definition for current file

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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 RECT-5 edDefinition ~
tgSelectedOnly tgLowerCase cbIndent tgSerial rsSerial fiReplace tgFormat ~
tgLabel tgShortTypes rsPrefix fiPrefix tgCamelCasing rsIndex btnSave 
&Scoped-Define DISPLAYED-OBJECTS edDefinition tgSelectedOnly tgLowerCase ~
cbIndent tgSerial rsSerial fiReplace tgFormat tgLabel tgShortTypes rsPrefix ~
fiPrefix tgCamelCasing rsIndex 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCasedString C-Win 
FUNCTION getCasedString RETURNS CHARACTER
  ( pcName AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFormatString C-Win 
FUNCTION getFormatString RETURNS CHARACTER
  ( pcFormat AS CHARACTER
  , piLength AS INTEGER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHungarian C-Win 
FUNCTION getHungarian RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piLength   AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabelString C-Win 
FUNCTION getLabelString RETURNS CHARACTER
  ( pcLabel  AS CHARACTER
  , piLength AS INTEGER
  )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSerialString C-Win 
FUNCTION getSerialString RETURNS CHARACTER
  ( pcName   AS CHARACTER 
  , piLength AS INTEGER  )  FORWARD.

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
     LABEL "&Save include" 
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
     SIZE-PIXELS 710 BY 506
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPrefix AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 80 BY 21 NO-UNDO.

DEFINE VARIABLE fiReplace AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 25 BY 21 NO-UNDO.

DEFINE VARIABLE rsIndex AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "N&one", 0,
"&Primary", 1,
"&All", 2
     SIZE-PIXELS 95 BY 52 NO-UNDO.

DEFINE VARIABLE rsPrefix AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&None", 0,
"Hungarian-&1", 1,
"Hungarian-&2", 2,
"&Fixed", 3
     SIZE-PIXELS 120 BY 68 TOOLTIP "specify field prefix" NO-UNDO.

DEFINE VARIABLE rsSerial AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "As-is", 0,
"Remove dashes", 1,
"Replace with", 2
     SIZE-PIXELS 120 BY 55 TOOLTIP "specify field prefix" NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 78.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 70.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 114.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 78.

DEFINE VARIABLE tgCamelCasing AS LOGICAL INITIAL no 
     LABEL "&Use CamelCasing" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 140 BY 17 TOOLTIP "concatenate parts that are separated with dashes and underscores" NO-UNDO.

DEFINE VARIABLE tgFormat AS LOGICAL INITIAL no 
     LABEL "&Format" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 110 BY 17 TOOLTIP "add the format of the field to the definition" NO-UNDO.

DEFINE VARIABLE tgLabel AS LOGICAL INITIAL no 
     LABEL "&Label" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 130 BY 17 TOOLTIP "add the label of the field to the definition" NO-UNDO.

DEFINE VARIABLE tgLowerCase AS LOGICAL INITIAL no 
     LABEL "Keywords in &Lower Case" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "use lower case for keywords" NO-UNDO.

DEFINE VARIABLE tgSelectedOnly AS LOGICAL INITIAL no 
     LABEL "&Selected fields only" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 160 BY 17 TOOLTIP "export only the fields that are selected in the main window" NO-UNDO.

DEFINE VARIABLE tgSerial AS LOGICAL INITIAL no 
     LABEL "Serialization" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 110 BY 17 NO-UNDO.

DEFINE VARIABLE tgShortTypes AS LOGICAL INITIAL no 
     LABEL "&Short datatypes" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 130 BY 17 TOOLTIP "use abbreviated datatypes (e.g. INT instead of INTEGER)" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     edDefinition AT Y 4 X 185 NO-LABEL  
     tgSelectedOnly AT Y 10 X 15  
     tgLowerCase AT Y 30 X 15   
     cbIndent AT Y 55 X 60 COLON-ALIGNED NO-LABEL   
     tgSerial AT Y 86 X 25   
     rsSerial AT Y 106 X 30 NO-LABEL   
     fiReplace AT Y 142 X 125 COLON-ALIGNED NO-LABEL   
     tgFormat AT Y 197 X 30   
     tgLabel AT Y 217 X 30   
     tgShortTypes AT Y 237 X 30   
     rsPrefix AT Y 287 X 30 NO-LABEL   
     fiPrefix AT Y 338 X 75 COLON-ALIGNED NO-LABEL   
     tgCamelCasing AT Y 367 X 30   
     rsIndex AT Y 414 X 30 NO-LABEL   
     btnSave AT Y 486 X 15   
     "Indent:" VIEW-AS TEXT
          SIZE-PIXELS 40 BY 20 AT Y 56 X 25   
     "Field Prefix" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 270 X 25   
     "Indexes" VIEW-AS TEXT
          SIZE-PIXELS 50 BY 13 AT Y 395 X 25   
     "Field Options" VIEW-AS TEXT
          SIZE-PIXELS 90 BY 13 AT Y 178 X 25   
     RECT-2 AT Y 185 X 15   
     RECT-3 AT Y 401 X 15   
     RECT-4 AT Y 275 X 15   
     RECT-5 AT Y 93 X 15   
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 180 BY 24.67    .


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
         TITLE              = "Generate TempTable Include"
         HEIGHT             = 24.71
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
ON CHOOSE OF btnSave IN FRAME frMain /* Save include */
OR 'go' OF FRAME {&FRAME-NAME} ANYWHERE
DO:
  DEFINE VARIABLE lOkay     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cText     AS LONGCHAR   NO-UNDO. 
  
  cFileName = TRIM(STRING('tt' + LC(pcTable), 'xx!x(20)')) + '.i'.

  SYSTEM-DIALOG GET-FILE cFilename
    FILTERS ".i Include file (*.i)" "*.i",
            "Any File (*.*)" "*.*"
    INITIAL-FILTER 1
    ASK-OVERWRITE
    USE-FILENAME
    CREATE-TEST-FILE
    DEFAULT-EXTENSION ".i"
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


&Scoped-define SELF-NAME rsPrefix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsPrefix C-Win
ON VALUE-CHANGED OF rsPrefix IN FRAME frMain
DO:
  
  fiPrefix:VISIBLE = (SELF:SCREEN-VALUE = '3').
  fiPrefix:MOVE-TO-TOP().
  RUN generateCode.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsSerial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsSerial C-Win
ON VALUE-CHANGED OF rsSerial IN FRAME frMain
DO:
  fiReplace:SENSITIVE = (SELF:SCREEN-VALUE = '2').
  fiReplace:MOVE-TO-TOP().
  RUN generateCode.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSelectedOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSelectedOnly C-Win
ON VALUE-CHANGED OF tgSelectedOnly IN FRAME frMain /* Selected fields only */
, tgLowerCase, cbIndent, fiReplace, tgFormat, tgLabel, tgShortTypes, fiPrefix, tgCamelCasing, rsIndex
DO:
  RUN generateCode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSerial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSerial C-Win
ON VALUE-CHANGED OF tgSerial IN FRAME frMain /* Serialization */
DO:
  rsSerial :SENSITIVE = SELF:CHECKED.
  fiReplace:SENSITIVE = (SELF:CHECKED AND rsSerial:SCREEN-VALUE = '2').
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
  DISPLAY edDefinition tgSelectedOnly tgLowerCase cbIndent tgSerial rsSerial 
          fiReplace tgFormat tgLabel tgShortTypes rsPrefix fiPrefix 
          tgCamelCasing rsIndex 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-4 RECT-5 edDefinition tgSelectedOnly tgLowerCase 
         cbIndent tgSerial rsSerial fiReplace tgFormat tgLabel tgShortTypes 
         rsPrefix fiPrefix tgCamelCasing rsIndex btnSave 
      WITH FRAME frMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generateCode C-Win 
PROCEDURE generateCode :
/* Generate the code for the include
*/
  DEFINE VARIABLE cText         AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cMask         AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cUnique       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cHeader       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTable        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIndent       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMaxName      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iMaxLabel     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iMaxFormat    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iMaxType      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bIndex FOR ttIndex.
  
  DO WITH FRAME frMain:
    
    ASSIGN 
      tgSelectedOnly tgLowerCase cbIndent
      tgSerial rsSerial fiReplace
      tgFormat tgLabel tgShortTypes
      rsPrefix fiPrefix 
      rsIndex  .

    cTable = TRIM(STRING('tt' + LC(pcTable), 'xx!x(30)')).
    
    CASE cbIndent:
      WHEN 'tab' THEN cIndent = '~t'.
      WHEN '2'   THEN cIndent = '  '.
      WHEN '3'   THEN cIndent = '   '.
      WHEN '4'   THEN cIndent = '    '.
    END CASE.

    cHeader = SUBSTITUTE('/*---------------------------------------------------------------------- ~n' )
            + SUBSTITUTE('    File        : &1.i ~n', cTable )
            + SUBSTITUTE('    Description : TT definition for &1.&2 ~n', pcDatabase, pcTable )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('    History: ~n' )
            + SUBSTITUTE('    &1 &2 Created ~n', STRING(TODAY,'99-99-9999'), getUserName() )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('  ---------------------------------------------------------------------- ~n' )
            + SUBSTITUTE('            This file was generated with the DataDigger                  ~n' )
            + SUBSTITUTE('  ----------------------------------------------------------------------*/ ~n' )
            .
            
    cMask = '&1~nDEFINE TEMP-TABLE &2 NO-UNDO&3&4'.
    IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
    cText = SUBSTITUTE(cMask
                      , cHeader
                      , cTable
                      , (IF tgSerial THEN ' ' ELSE '')
                      , TRIM(getSerialString(pcTable, LENGTH(cTable)))
                      ).
    
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
        
      iMaxName   = MAXIMUM(iMaxName  , LENGTH(bField.cFieldName)).
      iMaxType   = MAXIMUM(iMaxType  , LENGTH(getTypeString(bField.cDataType, bField.iExtent, 0) )).
      iMaxFormat = MAXIMUM(iMaxFormat, LENGTH(bField.cFormat)).
      IF bField.cLabel <> ? THEN iMaxLabel = MAXIMUM(iMaxLabel, LENGTH(bField.cLabel)).
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
      
      cMask = '&1~n&2FIELD &3 AS &4 &5 &6 &7'.
      IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
      
      cText = SUBSTITUTE(cMask
                        , cText
                        , cIndent
                        , getNameString(bField.cFieldName, bField.cDataType, iMaxName)
                        , getTypeString(bField.cDataType, bField.iExtent, iMaxType) 
                        , getFormatString(bField.cFormat, iMaxFormat)
                        , getLabelString(bField.cLabel, iMaxLabel)
                        , getSerialString(bField.cFieldName, iMaxName)
                        ).
    END.
  
    /* indexes */
    IF rsIndex > 0 THEN
    DO:
      cText = cText + '~n'.
      
      #IndexLoop:
      FOR EACH bIndex BY bIndex.cIndexFlags MATCHES '*P*' DESCENDING:
        /* Skip others if we only want the primary index */
        IF rsIndex = 1 AND NOT bIndex.cIndexFlags MATCHES '*P*' THEN NEXT #IndexLoop. 
        
        IF bIndex.cIndexFlags MATCHES '*P*' THEN
          cMask = '&1~n&2INDEX &3 IS PRIMARY &4'.
        ELSE 
          cMask = '&1~n&2INDEX &3 &4'.
          
        IF tgLowerCase:CHECKED THEN cMask = LC(cMask).

        IF bIndex.cIndexFlags MATCHES '*u*' THEN 
          cUnique = (IF NOT bIndex.cIndexFlags MATCHES '*P*' THEN 'IS UNIQUE' ELSE 'UNIQUE').
        ELSE 
          cUnique = ''.
        IF tgLowerCase:CHECKED THEN cUnique = LC(cUnique).

        cText = SUBSTITUTE(cMask
                          , cText
                          , cIndent
                          , bIndex.cIndexName
                          , cUnique
                          ).
        DO i = 1 TO NUM-ENTRIES(bIndex.cFieldList):
          FIND bField WHERE bField.cFieldName = ENTRY(i, bIndex.cFieldList) NO-ERROR.
          IF AVAILABLE bField THEN 
            ASSIGN 
              cMask = '&1 &2'
              cText = SUBSTITUTE(cMask, cText, TRIM(getNameString(bField.cFieldName, bField.cDataType, iMaxName))).
        END.

      END.
    END.
    cMask = '&1~n&2.'.
    cText = SUBSTITUTE(cMask, cText, cIndent).
 
    edDefinition:SCREEN-VALUE = cText.
  END.
  
END PROCEDURE.

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
    IF getRegistry('DataDigger:GenerateTT', 'Window:width' ) = ? THEN setRegistry('DataDigger:GenerateTT', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).  
    IF getRegistry('DataDigger:GenerateTT', 'Window:height') = ? THEN setRegistry('DataDigger:GenerateTT', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ). 

    /* User settings */
    IF getRegistry('DataDigger:GenerateTT', 'SelectedOnly')  = ? THEN setRegistry('DataDigger:GenerateTT', 'SelectedOnly','no').
    IF getRegistry('DataDigger:GenerateTT', 'UseLowerCase')  = ? THEN setRegistry('DataDigger:GenerateTT', 'UseLowerCase','no').
    IF getRegistry('DataDigger:GenerateTT', 'Indent')        = ? THEN setRegistry('DataDigger:GenerateTT', 'Indent','2').
    IF getRegistry('DataDigger:GenerateTT', 'Serialize')     = ? THEN setRegistry('DataDigger:GenerateTT', 'Serialize','no').
    IF getRegistry('DataDigger:GenerateTT', 'Serialization') = ? THEN setRegistry('DataDigger:GenerateTT', 'Serialization','0').
    IF getRegistry('DataDigger:GenerateTT', 'SerialReplace') = ? THEN setRegistry('DataDigger:GenerateTT', 'SerialReplace','').
    IF getRegistry('DataDigger:GenerateTT', 'AddFormat')     = ? THEN setRegistry('DataDigger:GenerateTT', 'AddFormat','no').
    IF getRegistry('DataDigger:GenerateTT', 'AddLabel')      = ? THEN setRegistry('DataDigger:GenerateTT', 'AddLabel','no').
    IF getRegistry('DataDigger:GenerateTT', 'ShortTypes')    = ? THEN setRegistry('DataDigger:GenerateTT', 'ShortTypes','no').
    IF getRegistry('DataDigger:GenerateTT', 'AddPrefix')     = ? THEN setRegistry('DataDigger:GenerateTT', 'AddPrefix','0').
    IF getRegistry('DataDigger:GenerateTT', 'FixedPrefix')   = ? THEN setRegistry('DataDigger:GenerateTT', 'FixedPrefix','').
    IF getRegistry('DataDigger:GenerateTT', 'CamelCasing')   = ? THEN setRegistry('DataDigger:GenerateTT', 'CamelCasing','no' ).
    IF getRegistry('DataDigger:GenerateTT', 'Indexes')       = ? THEN setRegistry('DataDigger:GenerateTT', 'Indexes','0').

    /* Get user settings */
    tgSelectedOnly:CHECKED = LOGICAL(getRegistry('DataDigger:GenerateTT', 'SelectedOnly')).
    tgLowerCase:CHECKED    = LOGICAL(getRegistry('DataDigger:GenerateTT', 'UseLowerCase')).
    cbIndent:SCREEN-VALUE  = getRegistry('DataDigger:GenerateTT', 'Indent').
    tgSerial:CHECKED       = LOGICAL(getRegistry('DataDigger:GenerateTT', 'Serialize')).
    rsSerial:SCREEN-VALUE  = getRegistry('DataDigger:GenerateTT', 'Serialization').
    fiReplace:SCREEN-VALUE = getRegistry('DataDigger:GenerateTT', 'SerialReplace').
    IF fiReplace:SCREEN-VALUE = ? OR fiReplace:SCREEN-VALUE = '?' THEN fiReplace:SCREEN-VALUE = ''.
    tgFormat:CHECKED       = LOGICAL(getRegistry('DataDigger:GenerateTT', 'AddFormat')).
    tgLabel:CHECKED        = LOGICAL(getRegistry('DataDigger:GenerateTT', 'AddLabel')).
    tgShortTypes:CHECKED   = LOGICAL(getRegistry('DataDigger:GenerateTT', 'ShortTypes')).
    rsPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateTT', 'AddPrefix').
    fiPrefix:SCREEN-VALUE  = getRegistry('DataDigger:GenerateTT', 'FixedPrefix').
    IF fiPrefix:SCREEN-VALUE = ? OR fiPrefix:SCREEN-VALUE = '?' THEN fiPrefix:SCREEN-VALUE = ''.

    tgCamelCasing:CHECKED  = LOGICAL(getRegistry('DataDigger:GenerateTT', 'CamelCasing')).
    rsIndex:SCREEN-VALUE   = getRegistry('DataDigger:GenerateTT', 'Indexes').
    APPLY 'VALUE-CHANGED' TO tgSerial.
    APPLY 'VALUE-CHANGED' TO rsSerial.
    APPLY 'VALUE-CHANGED' TO rsPrefix.
    
    /* Restore window size */
    iSetting = INTEGER(getRegistry('DataDigger:GenerateTT', 'Window:width' )) NO-ERROR.
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
    setRegistry('DataDigger:GenerateTT', 'Window:width' , STRING({&WINDOW-NAME}:WIDTH-PIXELS) ).
    setRegistry('DataDigger:GenerateTT', 'Window:height', STRING({&WINDOW-NAME}:HEIGHT-PIXELS) ).

    /* User settings */
    setRegistry('DataDigger:GenerateTT', 'SelectedOnly' , STRING(tgSelectedOnly:CHECKED) ).
    setRegistry('DataDigger:GenerateTT', 'UseLowerCase' , STRING(tgLowerCase:CHECKED   ) ).
    setRegistry('DataDigger:GenerateTT', 'Indent'       , cbIndent:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateTT', 'Serialize'    , STRING(tgSerial:CHECKED      ) ). 
    setRegistry('DataDigger:GenerateTT', 'Serialization', rsSerial:SCREEN-VALUE          ). 
    setRegistry('DataDigger:GenerateTT', 'SerialReplace', fiReplace:SCREEN-VALUE         ). 
    setRegistry('DataDigger:GenerateTT', 'AddFormat'    , STRING(tgFormat:CHECKED      ) ).
    setRegistry('DataDigger:GenerateTT', 'AddLabel'     , STRING(tgLabel:CHECKED       ) ).
    setRegistry('DataDigger:GenerateTT', 'ShortTypes'   , STRING(tgShortTypes:CHECKED  ) ).
    setRegistry('DataDigger:GenerateTT', 'AddPrefix'    , rsPrefix:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateTT', 'FixedPrefix'  , fiPrefix:SCREEN-VALUE          ).
    setRegistry('DataDigger:GenerateTT', 'CamelCasing'  , STRING(tgCamelCasing:CHECKED ) ).
    setRegistry('DataDigger:GenerateTT', 'Indexes'      , rsIndex:SCREEN-VALUE           ).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCasedString C-Win 
FUNCTION getCasedString RETURNS CHARACTER
  ( pcName AS CHARACTER ):

  DEFINE VARIABLE i     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cName AS CHARACTER   NO-UNDO.

  pcName = REPLACE(pcName,'-',CHR(1)).
  pcName = REPLACE(pcName,'_',CHR(1)).

  DO i = 1 TO NUM-ENTRIES(pcName,CHR(1)):
   cName = cName + CAPS(SUBSTRING(ENTRY(i,pcName,CHR(1)),1,1)) + LC(SUBSTRING(ENTRY(i,pcName,CHR(1)),2)).
  END.

  RETURN cName.

END FUNCTION. /* getCasedString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFormatString C-Win 
FUNCTION getFormatString RETURNS CHARACTER
  ( pcFormat AS CHARACTER
  , piLength AS INTEGER
  ) :

  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask        AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    IF NOT tgFormat:CHECKED THEN RETURN ''.
  
    cReturnValue = SUBSTITUTE('&1 "&2"'
                             , (IF tgLowerCase:CHECKED THEN 'format' ELSE 'FORMAT')
                             , pcFormat
                             ).
    cMask = SUBSTITUTE('X(&1)', piLength + 9).
    cReturnValue = STRING(cReturnValue, cMask).

  END.

  RETURN cReturnValue.

END FUNCTION. /* getFormatString */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabelString C-Win 
FUNCTION getLabelString RETURNS CHARACTER
  ( pcLabel  AS CHARACTER
  , piLength AS INTEGER
  ) :

  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask        AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    IF NOT tgLabel:CHECKED THEN RETURN ''.
    IF pcLabel = ? THEN pcLabel = ''.

    cReturnValue = SUBSTITUTE('&1 "&2"'
                             , (IF tgLowerCase:CHECKED THEN 'label' ELSE 'LABEL')
                             , pcLabel
                             ).
    cMask = SUBSTITUTE('X(&1)', piLength + 8).
    cReturnValue = STRING(cReturnValue, cMask).

  END.

  RETURN cReturnValue.

END FUNCTION. /* getLabelString */

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

  DO WITH FRAME {&FRAME-NAME}:

    CASE rsPrefix:
      WHEN 0 THEN cPrefix = ''.
      WHEN 1 THEN cPrefix = getHungarian(pcDataType,1).
      WHEN 2 THEN cPrefix = getHungarian(pcDataType,2).
      WHEN 3 THEN cPrefix = fiPrefix:SCREEN-VALUE.
    END CASE.

    cReturnValue = pcFieldName.

    IF tgCamelCasing:CHECKED THEN cReturnValue = getCasedString(cReturnValue).

    IF rsPrefix > 0 THEN OVERLAY(cReturnValue,1,1) = CAPS(SUBSTRING(cReturnValue,1,1)).
    cReturnValue = cPrefix + cReturnValue.

    cMask = SUBSTITUTE('X(&1)', piLength ).
    cReturnValue = STRING(cReturnValue, cMask).

  END.

  RETURN cReturnValue.

END FUNCTION. /* getNameString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSerialString C-Win 
FUNCTION getSerialString RETURNS CHARACTER
  ( pcName   AS CHARACTER 
  , piLength AS INTEGER  ) :

  DEFINE VARIABLE cSerialName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask        AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN rsSerial fiReplace.

    IF NOT tgSerial:CHECKED THEN RETURN ''.

    CASE rsSerial:
      WHEN 0 THEN cSerialName = pcName.
      WHEN 1 THEN cSerialName = getCasedString(pcName).
      WHEN 2 THEN cSerialName = REPLACE(pcName,'-',fiReplace:SCREEN-VALUE).
    END CASE.

    cReturnValue = SUBSTITUTE('&1 "&2"'
                             , (IF tgLowerCase:CHECKED THEN 'serialize-name' ELSE 'SERIALIZE-NAME')
                             , cSerialName
                             ).
    cMask = SUBSTITUTE('X(&1)', piLength + 17).
    cReturnValue = STRING(cReturnValue, cMask).
  END.

  RETURN cReturnValue.

END FUNCTION. /* getSerialString */

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

    IF tgShortTypes THEN 
    DO:
      CASE pcDataType:
        WHEN 'INTEGER'   THEN pcDataType = 'int'.
        WHEN 'CHARACTER' THEN pcDataType = 'char'.
        WHEN 'DECIMAL'   THEN pcDataType = 'dec'.
        WHEN 'LOGICAL'   THEN pcDataType = 'log'.
      END CASE.
    END.

    IF piExtent = 0 THEN
      cReturnValue = pcDataType.
    ELSE 
      cReturnValue = SUBSTITUTE('&1 extent &2', pcDataType, piExtent).

    cReturnValue = (IF tgLowerCase:CHECKED THEN LC(cReturnValue) ELSE CAPS(cReturnValue)).

    cMask = SUBSTITUTE('X(&1)', piLength).
    cReturnValue = (IF piLength > 0 THEN STRING(cReturnValue, cMask) ELSE cReturnValue).

  END.

  RETURN cReturnValue.

END FUNCTION. /* getTypeString */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


