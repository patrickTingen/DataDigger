&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME frMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS frMain 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{ DataDigger.i }

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT PARAMETER pcDatabase AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcTable    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER TABLE FOR ttField.
  DEFINE INPUT PARAMETER TABLE FOR ttIndex.
&ELSE
  DEFINE VARIABLE pcDatabase AS CHARACTER   NO-UNDO INITIAL 'sports'.
  DEFINE VARIABLE pcTable    AS CHARACTER   NO-UNDO INITIAL 'salesrep'.
  
  DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
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
&Scoped-define FRAME-NAME frMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 rsIndex rsPrefix ~
tgLowerCase fiPrefix btnSave tgLabel tgFormat tgSelectedOnly edDefinition ~
cbIndent 
&Scoped-Define DISPLAYED-OBJECTS rsIndex rsPrefix tgLowerCase fiPrefix ~
tgLabel tgFormat tgSelectedOnly edDefinition cbIndent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHungarian frMain 
FUNCTION getHungarian RETURNS CHARACTER
  ( pcDataType AS CHARACTER
  , piLength   AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     LABEL "&Save include" 
     SIZE-PIXELS 160 BY 24.

DEFINE VARIABLE cbIndent AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Indent" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "2 Spaces","2",
                     "3 Spaces","3",
                     "4 Spaces","4",
                     "Tab","Tab"
     DROP-DOWN-LIST
     SIZE-PIXELS 80 BY 21 NO-UNDO.

DEFINE VARIABLE edDefinition AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE-PIXELS 800 BY 470
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiPrefix AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 80 BY 21 NO-UNDO.

DEFINE VARIABLE rsIndex AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "N&one", 0,
"&Primary", 1,
"&All", 2
     SIZE-PIXELS 95 BY 55 NO-UNDO.

DEFINE VARIABLE rsPrefix AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&None", 0,
"Hungarian-&1", 1,
"Hungarian-&2", 2,
"&Fixed", 3
     SIZE-PIXELS 120 BY 80 TOOLTIP "specify field prefix" NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 68.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 73.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 160 BY 100.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     rsIndex AT Y 355 X 30 NO-LABEL WIDGET-ID 42
     rsPrefix AT Y 220 X 30 NO-LABEL WIDGET-ID 28
     tgLowerCase AT Y 30 X 15 WIDGET-ID 22
     fiPrefix AT Y 280 X 75 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     btnSave AT Y 455 X 15 WIDGET-ID 36
     tgLabel AT Y 140 X 30 WIDGET-ID 12
     tgFormat AT Y 120 X 30 WIDGET-ID 10
     tgSelectedOnly AT Y 10 X 15 WIDGET-ID 8
     edDefinition AT Y 10 X 185 NO-LABEL WIDGET-ID 2
     cbIndent AT Y 55 X 40 COLON-ALIGNED WIDGET-ID 50
     "Field Prefix" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 200 X 25 WIDGET-ID 48
     "Indexes" VIEW-AS TEXT
          SIZE-PIXELS 50 BY 13 AT Y 335 X 25 WIDGET-ID 40
     "Field Options" VIEW-AS TEXT
          SIZE-PIXELS 75 BY 13 AT Y 95 X 25 WIDGET-ID 26
     RECT-2 AT Y 102 X 15 WIDGET-ID 24
     RECT-3 AT Y 342 X 15 WIDGET-ID 38
     RECT-4 AT Y 205 X 15 WIDGET-ID 46
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE-PIXELS 1003 BY 523
         TITLE "Generate TempTable Include" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX frMain
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frMain:SCROLLABLE       = FALSE
       FRAME frMain:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME frMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frMain frMain
ON WINDOW-CLOSE OF FRAME frMain /* Generate TempTable Include */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave frMain
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsPrefix frMain
ON VALUE-CHANGED OF rsPrefix IN FRAME frMain
DO:
  
  fiPrefix:VISIBLE = (SELF:SCREEN-VALUE = '3').
  fiPrefix:MOVE-TO-TOP().
  RUN generateInclude.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSelectedOnly
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSelectedOnly frMain
ON VALUE-CHANGED OF tgSelectedOnly IN FRAME frMain /* Selected fields only */
, tgLowerCase, cbIndent, tgFormat, tgLabel, fiPrefix, rsIndex
DO:
  RUN generateInclude.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK frMain 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  RUN init-object.
  
  RUN generateInclude.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI frMain  _DEFAULT-DISABLE
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
  HIDE FRAME frMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI frMain  _DEFAULT-ENABLE
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
  DISPLAY rsIndex rsPrefix tgLowerCase fiPrefix tgLabel tgFormat tgSelectedOnly 
          edDefinition cbIndent 
      WITH FRAME frMain.
  ENABLE RECT-2 RECT-3 RECT-4 rsIndex rsPrefix tgLowerCase fiPrefix btnSave 
         tgLabel tgFormat tgSelectedOnly edDefinition cbIndent 
      WITH FRAME frMain.
  VIEW FRAME frMain.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTT frMain 
PROCEDURE fillTT :
/* Fill tt for testing in UIB
  */
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bIndex FOR ttIndex.
  
  CREATE bField. ASSIGN bField.cFieldName = 'repNr'      bField.lShow = TRUE bField.cDataType = 'INTEGER'   bField.cFormat = '>>>9'  bField.cLabel = 'Rep nr'.
  CREATE bField. ASSIGN bField.cFieldName = 'repName'    bField.lShow = TRUE bField.cDataType = 'CHARACTER' bField.cFormat = 'x(30)' bField.cLabel = 'Rep name'.
  CREATE bField. ASSIGN bField.cFieldName = 'region'     bField.lShow = TRUE bField.cDataType = 'CHARACTER' bField.cFormat = 'x(8)'  bField.cLabel = 'Region'.
  CREATE bField. ASSIGN bField.cFieldName = 'monthQuota' bField.lShow = TRUE bField.cDataType = 'INTEGER'   bField.cFormat = '->,>>>,>>9' bField.cLabel = 'Rep name' bField.iExtent = 12.
         
  CREATE bIndex. ASSIGN bIndex.cIndexName  = 'iPrim'   bIndex.cIndexFlags = 'P U' bIndex.cFieldList  = 'repNr'.
  CREATE bIndex. ASSIGN bIndex.cIndexName  = 'iRegion' bIndex.cIndexFlags = ''    bIndex.cFieldList  = 'region,repName'.   

END PROCEDURE. /* fillTT */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generateInclude frMain 
PROCEDURE generateInclude :
DEFINE VARIABLE cText         AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE cType         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrefix       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cName         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMask         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cHeader       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTable        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFormatName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFormatType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFormatLabel  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFormatFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIndent       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iName         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iLabel        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iFormat       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iType         AS INTEGER   NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bIndex FOR ttIndex.
  
  DO WITH FRAME frMain:
    
    ASSIGN rsPrefix rsIndex cbIndent.
    cTable = TRIM(STRING('tt' + LC(pcTable), 'xx!x(20)')).
    
    CASE cbIndent:
      WHEN 'tab' THEN cIndent = '~t'.
      WHEN '2'   THEN cIndent = '  '.
      WHEN '3'   THEN cIndent = '   '.
      WHEN '4'   THEN cIndent = '    '.
    END CASE.

    cHeader = SUBSTITUTE('/*----------------------------------------------------------------------*/ ~n' )
            + SUBSTITUTE('    File        : &1 ~n', cTable )
            + SUBSTITUTE('    Description : TT definition for &1.&2 ~n', pcDatabase, pcTable )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('    History: ~n' )
            + SUBSTITUTE('    &1 &2 Created ~n', STRING(TODAY,'99/99/9999'), getUserName() )
            + SUBSTITUTE(' ~n' )
            + SUBSTITUTE('  ----------------------------------------------------------------------*/ ~n' )
            + SUBSTITUTE('/*          This .i file was generated with the DataDigger              */ ~n' )
            + SUBSTITUTE('/*----------------------------------------------------------------------*/ ~n' )
            .
            
    cMask = '&1~nDEFINE TEMP-TABLE &2 NO-UNDO RCODE-INFORMATION'.
    IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
    cText = SUBSTITUTE(cMask, cHeader, cTable).
    
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
        
      IF bField.iExtent = 0 THEN
        cType = bField.cDataType.
      ELSE 
        cType = SUBSTITUTE('&1 &2 &3', bField.cDataType, (IF tgLowerCase:CHECKED THEN 'extent' ELSE 'EXTENT'), bField.iExtent).
      
      iName   = MAXIMUM(iName  , LENGTH(bField.cFieldName)).
      iType   = MAXIMUM(iType  , LENGTH(cType)).
      iFormat = MAXIMUM(iFormat, LENGTH(bField.cFormat)).
      IF bField.cLabel <> ? THEN iLabel  = MAXIMUM(iLabel , LENGTH(bField.cLabel)).
    END.
    
    CASE rsPrefix:
      WHEN 1 THEN iName = iName + 1.
      WHEN 2 THEN iName = iName + 2.
      WHEN 3 THEN iName = iName + LENGTH(fiPrefix:SCREEN-VALUE).
    END CASE.
    
    ASSIGN 
      cFormatName   = SUBSTITUTE('X(&1)', iName)
      cFormatType   = SUBSTITUTE('X(&1)', iType)
      cFormatLabel  = SUBSTITUTE('X(&1)', iLabel + 8)
      cFormatFormat = SUBSTITUTE('X(&1)', iFormat + 9).
      
    FOR EACH bField
      WHERE bField.cFieldName <> 'RECID'
        AND bField.cFieldName <> 'ROWID'
        AND (NOT tgSelectedOnly:CHECKED OR bField.lShow):        
    
      IF bField.iExtent = 0 THEN
        cType = bField.cDataType.
      ELSE 
        cType = SUBSTITUTE('&1 &2 &3', bField.cDataType, (IF tgLowerCase:CHECKED THEN 'extent' ELSE 'EXTENT'), bField.iExtent).
      
      CASE rsPrefix:
        WHEN 0 THEN cPrefix = ''.
        WHEN 1 THEN cPrefix = getHungarian(bField.cDataType,1).
        WHEN 2 THEN cPrefix = getHungarian(bField.cDataType,2).
        WHEN 3 THEN cPrefix = fiPrefix:SCREEN-VALUE.
      END CASE.
      
      cName = bField.cFieldName.
      IF rsPrefix > 0 THEN OVERLAY(cName,1,1) = CAPS(SUBSTRING(cName,1,1)).
      cName = cPrefix + cName.
      
      cMask = '&1~n&2FIELD &3 AS &4 &5 &6'.
      IF tgLowerCase:CHECKED THEN cMask = LC(cMask).
      
      cText = SUBSTITUTE(cMask
                        , cText
                        , cIndent
                        , STRING(cName, cFormatName)
                        , (IF tgLowerCase:CHECKED THEN LC(STRING(cType, cFormatType)) ELSE CAPS(STRING(cType, cFormatType)))
                        , (IF tgFormat:CHECKED 
                            THEN STRING(SUBSTITUTE('&1 "&2"', (IF tgLowerCase:CHECKED THEN 'format' ELSE 'FORMAT'), bField.cFormat), cFormatFormat) 
                            ELSE '')
                        , (IF bField.cLabel <> ? AND tgLabel:CHECKED 
                            THEN STRING(SUBSTITUTE('&1 "&2"', (IF tgLowerCase:CHECKED THEN 'label' ELSE 'LABEL'), bField.cLabel), cFormatLabel) 
                            ELSE '')
                        ).
    END.
  
    /* indexes */
    IF rsIndex > 0 THEN
    DO:
      cText = cText + '~n'.
      
      FOR EACH bIndex BY bIndex.cIndexFlags MATCHES '*P*' DESCENDING:
        /* Skip others if we only want the primary index */
        IF rsIndex = 1 AND NOT bIndex.cIndexFlags MATCHES '*P*' THEN NEXT. 
        
        IF bIndex.cIndexFlags MATCHES '*P*' THEN
          cMask = '&1~n&2INDEX &3 IS PRIMARY &4 &5'.
        ELSE 
          cMask = '&1~n&2INDEX &3 &4 &5'.
          
        IF tgLowerCase:CHECKED THEN cMask = LC(cMask).

        cText = SUBSTITUTE(cMask
                          , cText
                          , cIndent
                          , bIndex.cIndexName
                          , TRIM(STRING(bIndex.cIndexFlags MATCHES '*u*','UNIQUE/'))
                          , REPLACE(bIndex.cFieldList,',',' ')
                          ).
      END.
    END.
    cText = SUBSTITUTE('&1~n&2.', cText, cIndent).
 
    edDefinition:SCREEN-VALUE = cText.
    
    /* Save settings */
    setRegistry('GenerateTT', 'SelectedOnly', STRING(tgSelectedOnly:CHECKED) ).
    setRegistry('GenerateTT', 'UseLowerCase', STRING(tgLowerCase:CHECKED   ) ).
    setRegistry('GenerateTT', 'Indent'      , cbIndent:SCREEN-VALUE          ).
    setRegistry('GenerateTT', 'AddFormat'   , STRING(tgFormat:CHECKED      ) ).
    setRegistry('GenerateTT', 'AddLabel'    , STRING(tgLabel:CHECKED       ) ).
    setRegistry('GenerateTT', 'AddPrefix'   , rsPrefix:SCREEN-VALUE          ).
    setRegistry('GenerateTT', 'FixedPrefix' , fiPrefix:SCREEN-VALUE          ).
    setRegistry('GenerateTT', 'Indexes'     , rsIndex:SCREEN-VALUE           ).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-object frMain 
PROCEDURE init-object :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cSetting AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lSetting AS LOGICAL   NO-UNDO.
  
  DO WITH FRAME frMain:
    
    /* Init the settings in case they do not yet exist */
    IF getRegistry('GenerateTT', 'SelectedOnly') = ? THEN setRegistry('GenerateTT', 'SelectedOnly','no').
    IF getRegistry('GenerateTT', 'UseLowerCase') = ? THEN setRegistry('GenerateTT', 'UseLowerCase','no').
    IF getRegistry('GenerateTT', 'Indent')       = ? THEN setRegistry('GenerateTT', 'Indent','2').
    IF getRegistry('GenerateTT', 'AddFormat')    = ? THEN setRegistry('GenerateTT', 'AddFormat','no').
    IF getRegistry('GenerateTT', 'AddLabel')     = ? THEN setRegistry('GenerateTT', 'AddLabel','no').
    IF getRegistry('GenerateTT', 'AddPrefix')    = ? THEN setRegistry('GenerateTT', 'AddPrefix','0').
    IF getRegistry('GenerateTT', 'FixedPrefix')  = ? THEN setRegistry('GenerateTT', 'FixedPrefix','').
    IF getRegistry('GenerateTT', 'Indexes')      = ? THEN setRegistry('GenerateTT', 'Indexes','0').
    
    /* Get settings */
    tgSelectedOnly:CHECKED = LOGICAL(getRegistry('GenerateTT', 'SelectedOnly')).
    tgLowerCase:CHECKED    = LOGICAL(getRegistry('GenerateTT', 'UseLowerCase')).
    cbIndent:SCREEN-VALUE  = getRegistry('GenerateTT', 'Indent').
    tgFormat:CHECKED       = LOGICAL(getRegistry('GenerateTT', 'AddFormat')).
    tgLabel:CHECKED        = LOGICAL(getRegistry('GenerateTT', 'AddLabel')).
    rsPrefix:SCREEN-VALUE  = getRegistry('GenerateTT', 'AddPrefix').
    fiPrefix:SCREEN-VALUE  = getRegistry('GenerateTT', 'FixedPrefix').
    rsIndex:SCREEN-VALUE   = getRegistry('GenerateTT', 'Indexes').
    
    APPLY 'VALUE-CHANGED' TO rsPrefix.
    
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHungarian frMain 
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

