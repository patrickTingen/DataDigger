&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  Name: wSettingsTab2.w
  Desc: Settings tab for Appearance

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

DEFINE INPUT  PARAMETER phParent    AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER phRectangle AS HANDLE      NO-UNDO.

/* Local Variable Definitions ---                                       */

{ DataDigger.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rsTitleBarDbName AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "No Name", "none",
"Logical Name", "ldbname",
"Physical Name", "pdbname"
     SIZE-PIXELS 155 BY 55 TOOLTIP "which database name to use in title bar" NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 400 BY 114.

DEFINE VARIABLE tgTitleStartsWithTableName AS LOGICAL INITIAL no 
     LABEL "Start Title With Table Name" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 275 BY 17 TOOLTIP "use table name as first element in the title" NO-UNDO.

DEFINE BUTTON btnDefaultFont 
     LABEL "&Default Font" 
     SIZE-PIXELS 150 BY 24.

DEFINE BUTTON btnFixedFont 
     LABEL "&Fixed Font" 
     SIZE-PIXELS 150 BY 24.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 400 BY 70.

DEFINE VARIABLE tgAutoFont AS LOGICAL INITIAL no 
     LABEL "&Automatically set fonts" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 215 BY 17 NO-UNDO.

DEFINE BUTTON btnFavouriteTable 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnfiCustomFormat 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnfiCustomOrder 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnFieldFilter 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnIndexInactive 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnPrimIndex 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnWarningBox 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE VARIABLE fiCustomFormat AS CHARACTER FORMAT "X(256)":U INITIAL "CustomFormat" 
     LABEL "User-defined format" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiCustomOrder AS CHARACTER FORMAT "X(256)":U INITIAL "CustomOrder" 
     LABEL "User-defined order" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiFavouriteTable AS CHARACTER FORMAT "X(256)":U INITIAL "Favourite Table" 
     LABEL "Favourite Table" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiFieldFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Matched by Field Filter" 
     LABEL "Table matches Field Filter" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiIndexInactive AS CHARACTER FORMAT "X(256)":U INITIAL "IndexInactive" 
     LABEL "Inactive Index" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiPrimIndex AS CHARACTER FORMAT "X(256)":U INITIAL "PrimIndex" 
     LABEL "Primary Index" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiWarningBox AS CHARACTER FORMAT "X(256)":U INITIAL "WarningBox" 
     LABEL "Changed format" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 400 BY 224.

DEFINE VARIABLE tgHighlightFavouriteTables AS LOGICAL INITIAL yes 
     LABEL "Highlight Favourite tables" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 180 BY 17 NO-UNDO.

DEFINE BUTTON btnEven 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnFilterBox 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnOdd 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnQueryError 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnRecordCountComplete 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnRecordCountIncomplete 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE BUTTON btnRecordCountSelected 
     LABEL "Set" 
     SIZE-PIXELS 50 BY 21.

DEFINE VARIABLE fiEvenRow AS CHARACTER FORMAT "X(256)":U INITIAL "Data Row: even" 
     LABEL "Even rows" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiFilterBox AS CHARACTER FORMAT "X(256)":U INITIAL "FilterBox" 
     LABEL "Box color when filter used" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiOddRow AS CHARACTER FORMAT "X(256)":U INITIAL "Data Row: odd" 
     LABEL "Odd rows" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiQueryError AS CHARACTER FORMAT "X(256)":U INITIAL "QueryError" 
     LABEL "Error on opening query" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiRecordCountComplete AS CHARACTER FORMAT "X(256)":U INITIAL "RecordCount:Complete" 
     LABEL "All records fetched" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiRecordCountIncomplete AS CHARACTER FORMAT "X(256)":U INITIAL "RecordCount:Incomplete" 
     LABEL "Not all records fetched" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE fiRecordCountSelected AS CHARACTER FORMAT "X(256)":U INITIAL "RecordCount:Selected" 
     LABEL "Nr of selected records" 
     VIEW-AS FILL-IN NATIVE 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 400 BY 224.

DEFINE VARIABLE tgUseSystemColors AS LOGICAL INITIAL no 
     LABEL "Use &System Colors" 
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 150 BY 17 NO-UNDO.

DEFINE VARIABLE fiExample AS CHARACTER FORMAT "X(256)":U 
     LABEL "Example" 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 170 BY 21 NO-UNDO.

DEFINE VARIABLE rsColumnLabelTemplate AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Field name", "&1",
"Order + Field name", "&2. &1",
"Label", "&3",
"Order + Label", "&2. &3"
     SIZE-PIXELS 175 BY 75 TOOLTIP "template for column labels on the data browse" NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 400 BY 138.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 184 BY 31.38    .

DEFINE FRAME FRAME-Z
     rsColumnLabelTemplate AT Y 30 X 85 NO-LABEL    
     fiExample AT Y 111 X 75 COLON-ALIGNED    
     "Data Browse Column Label Template" VIEW-AS TEXT
          SIZE-PIXELS 240 BY 17 AT Y 2 X 15    
     RECT-19 AT Y 11 X 5    
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 10 Y 155
         SIZE-PIXELS 415 BY 170
         TITLE "2"     .

DEFINE FRAME FRAME-V
     tgUseSystemColors AT Y 25 X 170    
     fiOddRow AT Y 45 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnOdd AT Y 45 X 344    
     fiEvenRow AT Y 68 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnEven AT Y 68 X 344    
     fiFilterBox AT Y 102 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnFilterBox AT Y 102 X 344    
     fiQueryError AT Y 127 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnQueryError AT Y 127 X 344    
     fiRecordCountComplete AT Y 152 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnRecordCountComplete AT Y 152 X 344    
     fiRecordCountIncomplete AT Y 178 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnRecordCountIncomplete AT Y 178 X 344    
     fiRecordCountSelected AT Y 204 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnRecordCountSelected AT Y 204 X 344    
     "Data / query highlighting colors" VIEW-AS TEXT
          SIZE-PIXELS 225 BY 17 AT Y 2 X 15    
     RECT-17 AT Y 11 X 5    
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 10 Y 340
         SIZE-PIXELS 415 BY 265
         TITLE "2"     .

DEFINE FRAME FRAME-T
     tgHighlightFavouriteTables AT Y 20 X 170    
     fiFavouriteTable AT Y 39 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnFavouriteTable AT Y 39 X 344    
     fiFieldFilter AT Y 74 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnFieldFilter AT Y 74 X 344    
     fiCustomOrder AT Y 100 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnfiCustomOrder AT Y 100 X 344    
     fiCustomFormat AT Y 126 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnfiCustomFormat AT Y 126 X 344    
     fiWarningBox AT Y 152 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnWarningBox AT Y 152 X 344    
     fiIndexInactive AT Y 179 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnIndexInactive AT Y 179 X 344    
     fiPrimIndex AT Y 205 X 160 COLON-ALIGNED     NO-TAB-STOP 
     btnPrimIndex AT Y 205 X 344    
     "Schema highlighting colors" VIEW-AS TEXT
          SIZE-PIXELS 175 BY 17 AT Y 2 X 15    
     RECT-14 AT Y 11 X 5    
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 465 Y 205
         SIZE-PIXELS 415 BY 275
         TITLE "2"     .

DEFINE FRAME FRAME-J
     tgAutoFont AT Y 25 X 88    
     btnDefaultFont AT Y 46 X 88   
     btnFixedFont AT Y 46 X 245    
     "Fonts" VIEW-AS TEXT
          SIZE-PIXELS 55 BY 17 AT Y 2 X 15    
     "Set fonts:" VIEW-AS TEXT
          SIZE-PIXELS 73 BY 13 AT Y 51 X 10    
     RECT-20 AT Y 11 X 5    
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 10 Y 15
         SIZE-PIXELS 415 BY 115
         TITLE "2"     .

DEFINE FRAME FRAME-AB
     rsTitleBarDbName AT Y 28 X 85 NO-LABEL    
     tgTitleStartsWithTableName AT Y 95 X 85    
     "Database Name in Title Bar" VIEW-AS TEXT
          SIZE-PIXELS 180 BY 17 AT Y 2 X 15    
     RECT-22 AT Y 11 X 5    
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 465 Y 40
         SIZE-PIXELS 415 BY 150
         TITLE "2"     .


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
         TITLE              = "<insert window title>"
         HEIGHT             = 31.71
         WIDTH              = 185.8
         MAX-HEIGHT         = 40.52
         MAX-WIDTH          = 235.6
         VIRTUAL-HEIGHT     = 40.52
         VIRTUAL-WIDTH      = 235.6
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-AB:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-J:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-T:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-V:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Z:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-T:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-V:HANDLE)
       XXTABVALXX = FRAME FRAME-Z:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-T:HANDLE)
       XXTABVALXX = FRAME FRAME-AB:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-Z:HANDLE)
       XXTABVALXX = FRAME FRAME-J:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-AB:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-AB
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-AB:HIDDEN           = TRUE.

ASSIGN 
       rsTitleBarDbName:PRIVATE-DATA IN FRAME FRAME-AB     = 
                "DataDigger,TitleBarDbName".

ASSIGN 
       tgTitleStartsWithTableName:PRIVATE-DATA IN FRAME FRAME-AB     = 
                "DataDigger,TitleStartsWithTableName".

/* SETTINGS FOR FRAME FRAME-J
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-J:HIDDEN           = TRUE.

ASSIGN 
       btnDefaultFont:PRIVATE-DATA IN FRAME FRAME-J     = 
                "DataDigger:Fonts,default".

ASSIGN 
       btnFixedFont:PRIVATE-DATA IN FRAME FRAME-J     = 
                "DataDigger:Fonts,fixed".

ASSIGN 
       tgAutoFont:PRIVATE-DATA IN FRAME FRAME-J     = 
                "DataDigger:Fonts,AutoSetFont".

/* SETTINGS FOR FRAME FRAME-T
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-T:HIDDEN           = TRUE.

ASSIGN 
       fiCustomFormat:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiCustomFormat:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,CustomFormat".

ASSIGN 
       fiCustomOrder:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiCustomOrder:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,CustomOrder".

ASSIGN 
       fiFavouriteTable:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiFavouriteTable:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,FavouriteTable".

ASSIGN 
       fiFieldFilter:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiFieldFilter:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,FieldFilter".

ASSIGN 
       fiIndexInactive:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiIndexInactive:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,IndexInactive".

ASSIGN 
       fiPrimIndex:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiPrimIndex:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,PrimIndex".

ASSIGN 
       fiWarningBox:READ-ONLY IN FRAME FRAME-T        = TRUE
       fiWarningBox:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,WarningBox".

ASSIGN 
       tgHighlightFavouriteTables:PRIVATE-DATA IN FRAME FRAME-T     = 
                "DataDigger:Colors,FavouriteTable:HiLite".

/* SETTINGS FOR FRAME FRAME-V
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-V:HIDDEN           = TRUE.

ASSIGN 
       fiEvenRow:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiEvenRow:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,DataRow:even".

ASSIGN 
       fiFilterBox:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiFilterBox:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,FilterBox".

ASSIGN 
       fiOddRow:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiOddRow:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,DataRow:odd".

ASSIGN 
       fiQueryError:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiQueryError:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,QueryError".

ASSIGN 
       fiRecordCountComplete:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiRecordCountComplete:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,RecordCount:Complete".

ASSIGN 
       fiRecordCountIncomplete:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiRecordCountIncomplete:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,RecordCount:Incomplete".

ASSIGN 
       fiRecordCountSelected:READ-ONLY IN FRAME FRAME-V        = TRUE
       fiRecordCountSelected:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,RecordCount:Selected".

ASSIGN 
       tgUseSystemColors:PRIVATE-DATA IN FRAME FRAME-V     = 
                "DataDigger:Colors,DataRow:UseSystem".

/* SETTINGS FOR FRAME FRAME-Z
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-Z:HIDDEN           = TRUE.

ASSIGN 
       fiExample:READ-ONLY IN FRAME FRAME-Z        = TRUE.

ASSIGN 
       rsColumnLabelTemplate:PRIVATE-DATA IN FRAME FRAME-Z     = 
                "DataDigger,ColumnLabelTemplate".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-J
&Scoped-define SELF-NAME btnDefaultFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefaultFont C-Win
ON CHOOSE OF btnDefaultFont IN FRAME FRAME-J /* Default Font */
DO:
  DEFINE VARIABLE iFontNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE lOk     AS LOGICAL NO-UNDO.

  iFontNr = INTEGER(getRegistry('DataDigger:Fonts', 'Default')).

  RUN adecomm/_chsfont.p
    ( INPUT "Choose your default font"
    , INPUT iFontNr
    , INPUT-OUTPUT iFontNr
    , OUTPUT lOk
    ).
  IF lOk THEN SELF:FONT = iFontNr.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-V
&Scoped-define SELF-NAME btnEven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEven C-Win
ON CHOOSE OF btnEven IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiEvenRow
DO:
  RUN chooseColor(fiEvenRow:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-T
&Scoped-define SELF-NAME btnFavouriteTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFavouriteTable C-Win
ON CHOOSE OF btnFavouriteTable IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiFavouriteTable
DO:
  RUN chooseColor(fiFavouriteTable:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfiCustomFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfiCustomFormat C-Win
ON CHOOSE OF btnfiCustomFormat IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiCustomFormat
DO:
  RUN chooseColor(fiCustomFormat:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfiCustomOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfiCustomOrder C-Win
ON CHOOSE OF btnfiCustomOrder IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiCustomOrder
DO:
  RUN chooseColor(fiCustomOrder:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFieldFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFieldFilter C-Win
ON CHOOSE OF btnFieldFilter IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiFieldFilter
DO:
  RUN chooseColor(fiFieldFilter:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-V
&Scoped-define SELF-NAME btnFilterBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterBox C-Win
ON CHOOSE OF btnFilterBox IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiFilterBox
DO:
  RUN chooseColor(fiFilterBox:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-J
&Scoped-define SELF-NAME btnFixedFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFixedFont C-Win
ON CHOOSE OF btnFixedFont IN FRAME FRAME-J /* Fixed Font */
DO:
  DEFINE VARIABLE iFontNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE lOk     AS LOGICAL NO-UNDO.

  iFontNr = INTEGER(getRegistry('DataDigger:Fonts', 'Fixed')).

  RUN adecomm/_chsfont.p
    ( INPUT "Choose your fixed font"
    , INPUT iFontNr
    , INPUT-OUTPUT iFontNr
    , OUTPUT lOk
    ).
  IF lOk THEN SELF:FONT = iFontNr.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-T
&Scoped-define SELF-NAME btnIndexInactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnIndexInactive C-Win
ON CHOOSE OF btnIndexInactive IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiIndexInactive
DO:
  RUN chooseColor(fiIndexInactive:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-V
&Scoped-define SELF-NAME btnOdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOdd C-Win
ON CHOOSE OF btnOdd IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiOddRow
DO:
  RUN chooseColor(fiOddRow:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-T
&Scoped-define SELF-NAME btnPrimIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrimIndex C-Win
ON CHOOSE OF btnPrimIndex IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiPrimIndex
DO:
  RUN chooseColor(fiPrimIndex:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-V
&Scoped-define SELF-NAME btnQueryError
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQueryError C-Win
ON CHOOSE OF btnQueryError IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiQueryError
DO:
  RUN chooseColor(fiQueryError:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRecordCountComplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecordCountComplete C-Win
ON CHOOSE OF btnRecordCountComplete IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiRecordCountComplete
DO:
  RUN chooseColor(fiRecordCountComplete:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRecordCountIncomplete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecordCountIncomplete C-Win
ON CHOOSE OF btnRecordCountIncomplete IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiRecordCountIncomplete
DO:
  RUN chooseColor(fiRecordCountIncomplete:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRecordCountSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRecordCountSelected C-Win
ON CHOOSE OF btnRecordCountSelected IN FRAME FRAME-V /* Set */
OR MOUSE-SELECT-CLICK OF fiRecordCountSelected
DO:
  RUN chooseColor(fiRecordCountSelected:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-T
&Scoped-define SELF-NAME btnWarningBox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWarningBox C-Win
ON CHOOSE OF btnWarningBox IN FRAME FRAME-T /* Set */
OR MOUSE-SELECT-CLICK OF fiWarningBox
DO:
  RUN chooseColor(fiWarningBox:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Z
&Scoped-define SELF-NAME rsColumnLabelTemplate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsColumnLabelTemplate C-Win
ON VALUE-CHANGED OF rsColumnLabelTemplate IN FRAME FRAME-Z
DO:

  fiExample:screen-value =
    SUBSTITUTE(SELF:screen-value
              , "cust-num"
              , "1"
              , "Customer Number"
              ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-J
&Scoped-define SELF-NAME tgAutoFont
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgAutoFont C-Win
ON VALUE-CHANGED OF tgAutoFont IN FRAME FRAME-J /* Automatically set fonts */
DO:

  btnDefaultFont:sensitive = NOT SELF:checked.
  btnFixedFont  :sensitive = NOT SELF:checked.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-T
&Scoped-define SELF-NAME tgHighlightFavouriteTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgHighlightFavouriteTables C-Win
ON VALUE-CHANGED OF tgHighlightFavouriteTables IN FRAME FRAME-T /* Highlight Favourite tables */
DO:

  fiFavouriteTable :SENSITIVE = SELF:CHECKED.
  btnFavouriteTable:SENSITIVE = SELF:CHECKED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-V
&Scoped-define SELF-NAME tgUseSystemColors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgUseSystemColors C-Win
ON VALUE-CHANGED OF tgUseSystemColors IN FRAME FRAME-V /* Use System Colors */
DO:

  fiOddRow :SENSITIVE = NOT SELF:CHECKED.
  fiEvenRow:SENSITIVE = NOT SELF:CHECKED.
  btnOdd   :SENSITIVE = NOT SELF:CHECKED.
  btnEven  :SENSITIVE = NOT SELF:CHECKED.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Handle reparenting, startup etc */
{frameLib.i}


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chooseColor C-Win 
PROCEDURE chooseColor :
/* Set color for a field, assign colors to widget
  */
  DEFINE INPUT PARAMETER phWidget AS HANDLE NO-UNDO.

  DEFINE VARIABLE iBG  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFG  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iSep AS INTEGER NO-UNDO.
  DEFINE VARIABLE lOk  AS LOGICAL NO-UNDO.
  
  iBg = phWidget:BGCOLOR.
  iFg = phWidget:FGCOLOR.

  RUN adecomm/_chscolr.p (  "Choose color for " + phWidget:SCREEN-VALUE
                          , ""
                          , FALSE
                          , ?
                          , ?
                          , ?
                          , INPUT-OUTPUT iBG
                          , INPUT-OUTPUT iFG
                          , INPUT-OUTPUT iSep
                          , OUTPUT lOk
                          ).
  IF lOk THEN ASSIGN phWidget:FGCOLOR = iFg phWidget:BGCOLOR = iBg.
  /*
    ipTitle        - Title for the dialog
    cipMessage     - Message to write at bottom of screen
    lipSeparator   - Indicates if separator colors need to display for browse widgets
    iipDfltBgColor - color number to use as default background
    iipDfltFgColor - color number to use as default foreground
    iipDfltSpColor - color number to use as default separator color
    iiopBgColor    - background color number
    iiopFgColor    - foreground color number
    iiopSpColor    - separator color number
    lOk     - FALSE if cancelled dialog box.
  */  

END PROCEDURE. /* chooseColor */

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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY tgAutoFont 
      WITH FRAME FRAME-J IN WINDOW C-Win.
  ENABLE RECT-20 tgAutoFont btnDefaultFont btnFixedFont 
      WITH FRAME FRAME-J IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-J}
  DISPLAY rsTitleBarDbName tgTitleStartsWithTableName 
      WITH FRAME FRAME-AB IN WINDOW C-Win.
  ENABLE RECT-22 rsTitleBarDbName tgTitleStartsWithTableName 
      WITH FRAME FRAME-AB IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-AB}
  DISPLAY rsColumnLabelTemplate fiExample 
      WITH FRAME FRAME-Z IN WINDOW C-Win.
  ENABLE RECT-19 rsColumnLabelTemplate fiExample 
      WITH FRAME FRAME-Z IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Z}
  DISPLAY tgHighlightFavouriteTables fiFavouriteTable fiFieldFilter 
          fiCustomOrder fiCustomFormat fiWarningBox fiIndexInactive fiPrimIndex 
      WITH FRAME FRAME-T IN WINDOW C-Win.
  ENABLE RECT-14 tgHighlightFavouriteTables fiFavouriteTable btnFavouriteTable 
         fiFieldFilter btnFieldFilter fiCustomOrder btnfiCustomOrder 
         fiCustomFormat btnfiCustomFormat fiWarningBox btnWarningBox 
         fiIndexInactive btnIndexInactive fiPrimIndex btnPrimIndex 
      WITH FRAME FRAME-T IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-T}
  DISPLAY tgUseSystemColors fiOddRow fiEvenRow fiFilterBox fiQueryError 
          fiRecordCountComplete fiRecordCountIncomplete fiRecordCountSelected 
      WITH FRAME FRAME-V IN WINDOW C-Win.
  ENABLE RECT-17 tgUseSystemColors fiOddRow btnOdd fiEvenRow btnEven 
         fiFilterBox btnFilterBox fiQueryError btnQueryError 
         fiRecordCountComplete btnRecordCountComplete fiRecordCountIncomplete 
         btnRecordCountIncomplete fiRecordCountSelected btnRecordCountSelected 
      WITH FRAME FRAME-V IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-V}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
