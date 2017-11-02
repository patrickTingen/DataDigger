&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wSettings
/*------------------------------------------------------------------------

        Name: wSettings.w
        Desc: Container window for settings tabs

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{ DataDigger.i }

/* Parameters Definitions ---                                           */
&if '{&file-name}' matches '*.ab' &then
  DEFINE VARIABLE pcSettingsFile AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE plSuccess      AS LOGICAL     NO-UNDO.
  pcSettingsFile = 'd:\Data\DropBox\DataDigger\DataDigger-nljrpti.ini'.
&else
  DEFINE INPUT  PARAMETER pcSettingsFile AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT PARAMETER plSuccess      AS LOGICAL     NO-UNDO.
&endif.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE gcPageButtons    AS CHARACTER NO-UNDO.
DEFINE VARIABLE giLastActivePage AS INTEGER   NO-UNDO.
DEFINE VARIABLE giWinX           AS INTEGER   NO-UNDO.
DEFINE VARIABLE giWinY           AS INTEGER   NO-UNDO.
DEFINE VARIABLE giCurrentPage    AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttFrame NO-UNDO RCODE-INFORMATION
  FIELD cFrame   AS CHARACTER
  FIELD hFrame   AS HANDLE
  FIELD iOrder   AS INTEGER
  FIELD cTags    AS CHARACTER
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSettings rcSettings ficSettingsFile ~
btnRawEdit fiSearch btPage1 btPage2 btPage3 BtnCancel-2 BtnOK
&Scoped-Define DISPLAYED-OBJECTS ficSettingsFile fiSearch

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wSettings AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel-2 AUTO-END-KEY DEFAULT
     LABEL "Cancel"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT
     LABEL "OK"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnRawEdit
     LABEL "&Raw Edit"
     SIZE-PIXELS 60 BY 21 TOOLTIP "direct editing of the settings file".

DEFINE BUTTON btnSettings  NO-FOCUS FLAT-BUTTON
     LABEL ""
     SIZE-PIXELS 125 BY 35.

DEFINE BUTTON btPage1
     LABEL "Behavior"
     SIZE-PIXELS 125 BY 35.

DEFINE BUTTON btPage2
     LABEL "Appearance"
     SIZE-PIXELS 125 BY 35.

DEFINE BUTTON btPage3
     LABEL "Backup"
     SIZE-PIXELS 125 BY 35.

DEFINE VARIABLE ficSettingsFile AS CHARACTER FORMAT "X(256)":U
     LABEL "Settings file"
     VIEW-AS FILL-IN
     SIZE-PIXELS 460 BY 21 NO-UNDO.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U
     VIEW-AS FILL-IN
     SIZE-PIXELS 125 BY 23
     FONT 4 NO-UNDO.

DEFINE RECTANGLE rcSettings
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE-PIXELS 610 BY 402.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSettings AT Y 10 X 20 WIDGET-ID 24
     ficSettingsFile AT Y 15 X 215 COLON-ALIGNED WIDGET-ID 54 NO-TAB-STOP
     btnRawEdit AT Y 15 X 690 WIDGET-ID 90
     fiSearch AT Y 64 X 10 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     btPage1 AT Y 104 X 20 WIDGET-ID 8
     btPage2 AT Y 139 X 20 WIDGET-ID 14
     btPage3 AT Y 174 X 20 WIDGET-ID 10
     BtnCancel-2 AT Y 470 X 575 WIDGET-ID 98
     BtnOK AT Y 470 X 660 WIDGET-ID 94
     "CTRL-ALT-S also opens this window" VIEW-AS TEXT
          SIZE-PIXELS 240 BY 20 AT Y 475 X 15 WIDGET-ID 100
          FGCOLOR 7
     rcSettings AT Y 60 X 150 WIDGET-ID 92
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT X 0 Y 0
         SIZE-PIXELS 761 BY 508
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel-2 WIDGET-ID 100.

DEFINE FRAME frSettings
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT X 156 Y 64
         SCROLLABLE SIZE-PIXELS 1600 BY 3900
         TITLE "" WIDGET-ID 200.


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
  CREATE WINDOW wSettings ASSIGN
         HIDDEN             = YES
         TITLE              = "DataDigger Settings"
         HEIGHT-P           = 510
         WIDTH-P            = 769
         MAX-HEIGHT-P       = 562
         MAX-WIDTH-P        = 769
         VIRTUAL-HEIGHT-P   = 562
         VIRTUAL-WIDTH-P    = 769
         RESIZE             = NO
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
/* SETTINGS FOR WINDOW wSettings
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frSettings:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN
       ficSettingsFile:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME frSettings
                                                                        */
ASSIGN
       FRAME frSettings:HEIGHT           = 18.57
       FRAME frSettings:WIDTH            = 120.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wSettings)
THEN wSettings:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSettings wSettings
ON CTRL-PAGE-DOWN OF wSettings /* DataDigger Settings */
ANYWHERE DO:

  DO WITH FRAME {&FRAME-NAME}:

    IF FOCUS:NAME = 'fiSearch' THEN
      APPLY 'entry' TO btPage1.
    ELSE
    CASE giCurrentPage:
      WHEN 1 THEN APPLY 'entry' TO btPage2.
      WHEN 2 THEN APPLY 'entry' TO btPage3.
    END CASE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSettings wSettings
ON CTRL-PAGE-UP OF wSettings /* DataDigger Settings */
ANYWHERE DO:

  DO WITH FRAME {&FRAME-NAME}:

    IF FOCUS:NAME = 'btPage1' THEN
      APPLY 'entry' TO fiSearch.
    ELSE
    CASE giCurrentPage:
      WHEN 2 THEN APPLY 'entry' TO btPage1.
      WHEN 3 THEN APPLY 'entry' TO btPage2.
    END CASE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSettings wSettings
ON END-ERROR OF wSettings /* DataDigger Settings */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wSettings wSettings
ON WINDOW-CLOSE OF wSettings /* DataDigger Settings */
OR "LEAVE" OF wSettings
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK wSettings
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
OR GO OF wSettings ANYWHERE
DO:
  SESSION:SET-WAIT-STATE("general").
  RUN saveSettings.
  SESSION:SET-WAIT-STATE("").

  RUN saveConfigFileSorted.

  plSuccess = TRUE.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRawEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRawEdit wSettings
ON CHOOSE OF btnRawEdit IN FRAME DEFAULT-FRAME /* Raw Edit */
DO:
  /* Start default editor for ini file */
  OS-COMMAND NO-WAIT START VALUE( pcSettingsFile ).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettings wSettings
ON CHOOSE OF btnSettings IN FRAME DEFAULT-FRAME
DO:
  fiSearch:screen-value = ''.
  APPLY 'entry' TO btPage1.
  APPLY 'choose' TO btPage1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPage1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPage1 wSettings
ON CURSOR-DOWN OF btPage1 IN FRAME DEFAULT-FRAME /* Behavior */
, fiSearch ,btPage2
DO:

  CASE SELF:name:
    WHEN 'fiSearch' THEN APPLY 'entry' TO btPage1.
    WHEN 'btPage1'  THEN APPLY 'entry' TO btPage2.
    WHEN 'btPage2'  THEN APPLY 'entry' TO btPage3.
  END CASE.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPage1 wSettings
ON CURSOR-UP OF btPage1 IN FRAME DEFAULT-FRAME /* Behavior */
,btPage2, btPage3
DO:

  CASE SELF:name:
    WHEN 'btPage1' THEN APPLY 'entry' TO fiSearch.
    WHEN 'btPage2' THEN APPLY 'entry' TO btPage1.
    WHEN 'btPage3' THEN APPLY 'entry' TO btPage2.
  END CASE.

  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPage1 wSettings
ON ENTRY OF btPage1 IN FRAME DEFAULT-FRAME /* Behavior */
,btPage2, btPage3
DO:

  RUN setPage( INTEGER(SUBSTRING(SELF:NAME,7,1)) ).

  RUN SetScrollPos ( INPUT FRAME frSettings:HWND
                   , INPUT 1 /* Indicates this function should operate on the vertical scrollbar attached to the frame */
                   , INPUT 1 /* Scrollbar row position */
                   , INPUT 1 /* Causes the scrollbar to be re-drawn to reflect the changed position */
                   ).
  RUN PostMessageA( INPUT FRAME frSettings:HWND
                  , INPUT 277
                  , INPUT 4 + 65536 * 1
                  , INPUT 0
                  ).
END.

PROCEDURE SetScrollPos EXTERNAL "USER32.DLL":
  DEFINE INPUT PARAMETER pHwnd   AS LONG  NO-UNDO.
  DEFINE INPUT PARAMETER pNBar   AS SHORT NO-UNDO.
  DEFINE INPUT PARAMETER pNPos   AS SHORT NO-UNDO.
  DEFINE INPUT PARAMETER pRedraw AS SHORT NO-UNDO.
END PROCEDURE.

PROCEDURE PostMessageA EXTERNAL "USER32.DLL":
  DEFINE INPUT  PARAMETER pHwnd    AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER pMsg     AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER pWparam  AS LONG NO-UNDO.
  DEFINE INPUT  PARAMETER pLparam  AS LONG NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch wSettings
ON ENTRY OF fiSearch IN FRAME DEFAULT-FRAME
DO:
  IF SELF:screen-value <> '' THEN
  DO:
    RUN setPage(0).
    RUN showFrames(SELF:screen-value).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch wSettings
ON VALUE-CHANGED OF fiSearch IN FRAME DEFAULT-FRAME
OR 'return' OF fiSearch
DO:
  IF SELF:screen-value <> '' THEN
  DO:
    RUN setPage(0).
    RUN showFrames(SELF:screen-value).
  END.
  ELSE
  DO:
    RUN setPage(giLastActivePage).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wSettings


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

  giWinX = ACTIVE-WINDOW:X.
  giWinY = ACTIVE-WINDOW:Y.

  RUN initializeObject. /* Collect frames and set values from ini */
  APPLY 'entry' TO btPage1.
  VIEW wSettings.

  RUN showScrollBars(FRAME {&frame-name}:handle, NO, NO).

  wSettings:X = giWinX + 50.
  wSettings:Y = giWinY + 50.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectFrames wSettings
PROCEDURE collectFrames :
  /* Collect all frames that have been instantiated
  */
  DEFINE INPUT PARAMETER phParent AS HANDLE NO-UNDO.

  DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
  DEFINE BUFFER ttFrame FOR ttFrame.

  IF NOT CAN-QUERY(phParent,'first-child') THEN RETURN.

  hWidget = phParent:FIRST-CHILD.

  DO WHILE VALID-HANDLE(hWidget):

    /* Collect frames at a lower level */
    RUN collectFrames(hWidget).

    IF hWidget:TYPE = 'FRAME' THEN
    DO:

      CREATE ttFrame.
      ASSIGN ttFrame.hFrame = hWidget
             ttFrame.cFrame = hWidget:NAME
             ttFrame.cTags  = 'page' + hWidget:TITLE
             ttFrame.iOrder = INTEGER(hWidget:TITLE) * 1000 + hWidget:Y
             .

      hWidget:TITLE = ?.
      hWidget:BOX = NO.
    END.

    hWidget = hWidget:NEXT-SIBLING.
  END.

END PROCEDURE. /* collectFrames */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wSettings  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wSettings)
  THEN DELETE WIDGET wSettings.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wSettings  _DEFAULT-ENABLE
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
  DISPLAY ficSettingsFile fiSearch
      WITH FRAME DEFAULT-FRAME IN WINDOW wSettings.
  ENABLE btnSettings rcSettings ficSettingsFile btnRawEdit fiSearch btPage1
         btPage2 btPage3 BtnCancel-2 BtnOK
      WITH FRAME DEFAULT-FRAME IN WINDOW wSettings.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME frSettings IN WINDOW wSettings.
  {&OPEN-BROWSERS-IN-QUERY-frSettings}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wSettings
PROCEDURE initializeObject :
  /* Init global vars and frames
  */
  DEFINE VARIABLE hWidget    AS HANDLE  NO-UNDO.
  DEFINE VARIABLE iMaxHeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE iScreen    AS INTEGER NO-UNDO.
  DEFINE VARIABLE hProg      AS HANDLE  NO-UNDO EXTENT 3.

  /* Load decoration stuff */
  DO WITH FRAME {&frame-name}:
    btnSettings:load-image(getImagePath('Settings_txt.gif')).
  END.

  /* Hide all signs of the existence of the frame that holds the actual setting frames */
  FRAME frSettings:title = ?.
  FRAME frSettings:box = NO.
  FRAME {&frame-name}:font = getFont("Default").

  /* Collect the page buttons at the left of the screen */
  hWidget = FRAME {&frame-name}:handle:first-child:first-child.
  DO WHILE VALID-HANDLE(hWidget):
    IF hWidget:NAME BEGINS 'btPage' THEN DO:
      gcPageButtons = TRIM(gcPageButtons + ',' + string(hWidget),',').
      hWidget:PRIVATE-DATA = hWidget:LABEL. /* save original label */
    END.
    hWidget = hWidget:NEXT-SIBLING.
  END.

  DO iScreen = 1 TO 3:
    RUN value(SUBSTITUTE('&1\wSettingsTab&2.w', getProgramDir(), iScreen)) PERSISTENT SET hProg[iScreen]
      ( INPUT FRAME frSettings:handle
      , INPUT rcSettings:handle
      ).
  END.

  /* Collect all frames in the window */
  RUN collectFrames(INPUT FRAME frSettings:handle).

  /* process the content on the frames */
  FOR EACH ttFrame:

    ASSIGN
      iMaxHeight = 0
      hWidget    = ttFrame.hFrame:first-child:first-child.

    /* Collect all labels on the frame */
    DO WHILE VALID-HANDLE(hWidget):
      iMaxHeight = MAXIMUM(iMaxHeight, hWidget:Y + hWidget:HEIGHT-PIXELS).
      IF CAN-SET(hWidget,'font') THEN hWidget:FONT = getFont('DEFAULT').
      hWidget = hWidget:NEXT-SIBLING.
    END.

    /* Adjust height of frame */
    ttFrame.hFrame:height-pixels         = iMaxHeight + 4.
    ttFrame.hFrame:virtual-height-pixels = ttFrame.hFrame:height-pixels.
    ttFrame.hFrame:virtual-width-pixels  = ttFrame.hFrame:width-pixels.
  END.

  RUN enable_UI.
  RUN loadSettings.

  DO WITH FRAME {&frame-name}:
    ficSettingsFile:screen-value = pcSettingsFile.
  END.

  /* Run local inits */
  DO iScreen = 1 TO 3:
    RUN localInitialize IN hProg[iScreen] NO-ERROR.
  END.

END PROCEDURE. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadSettings wSettings
PROCEDURE loadSettings :
  /* Walk the frames and load settings for all widgets
  */
  DEFINE VARIABLE hWidget    AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cValue     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lValue     AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iColor     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cSection   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSetting   AS CHARACTER   NO-UNDO.

  FOR EACH ttFrame:

    hWidget = ttFrame.hFrame:first-child:first-child.

    DO WHILE VALID-HANDLE(hWidget):

      /* Collect tags */
      IF CAN-QUERY(hWidget,'label') THEN
        ttFrame.cTags = SUBSTITUTE('&1 &2', ttFrame.cTags, hWidget:LABEL).

      IF CAN-QUERY(hWidget,'tooltip') THEN
        ttFrame.cTags = SUBSTITUTE('&1 &2', ttFrame.cTags, hWidget:TOOLTIP).

      IF hWidget:TYPE = 'literal' THEN
        ttFrame.cTags = SUBSTITUTE('&1 &2', ttFrame.cTags, hWidget:SCREEN-VALUE).

      IF CAN-QUERY(hWidget,'private-data') THEN
        ttFrame.cTags = SUBSTITUTE('&1 &2', ttFrame.cTags, ENTRY(NUM-ENTRIES(hWidget:PRIVATE-DATA),hWidget:PRIVATE-DATA)).

      /* Get value from INI file and set it in the widget */
      IF hWidget:PRIVATE-DATA <> ?
        AND num-entries(hWidget:PRIVATE-DATA) = 2 THEN
      DO:
        cSection = ENTRY(1,hWidget:PRIVATE-DATA).
        cSetting = ENTRY(2,hWidget:PRIVATE-DATA).
        cValue   = getRegistry(cSection, cSetting).
        IF cValue = ? THEN cValue = "".

        IF hWidget:TYPE = 'BUTTON' THEN
        DO:
          IF cSection = 'DataDigger:fonts' THEN
            hWidget:FONT = INTEGER(cValue) NO-ERROR.
        END.

        ELSE
        IF hWidget:TYPE = 'TOGGLE-BOX' THEN
        DO:
          lValue = LOGICAL(getRegistry(cSection, cSetting)) NO-ERROR.
          IF lValue = ? THEN lValue = FALSE.
          hWidget:CHECKED = lValue.
        END.

        ELSE
        IF hWidget:TYPE = 'FILL-IN'
          AND cSection = 'DataDigger:colors' THEN
        DO:
          /* Try to get :FG */
          iColor = getColor(cSetting + ':FG' ).
          IF iColor <> ? THEN hWidget:FGCOLOR = iColor NO-ERROR.

          /* Try to get :BG */
          iColor = getColor(cSetting + ':BG' ).
          IF iColor <> ? THEN hWidget:BGCOLOR = iColor NO-ERROR.

          hWidget:SCREEN-VALUE = cSetting.
        END.

        ELSE
          hWidget:SCREEN-VALUE = cValue.

        /* For some reason, applying "VALUE-CHANGED" toggles
         * the value of the checkbox, so do it twice :)
         */
        APPLY "VALUE-CHANGED" TO hWidget.
        APPLY "VALUE-CHANGED" TO hWidget.
      END.

      hWidget = hWidget:NEXT-SIBLING.
    END. /* f/e ttFrame */

    /* Correct tags, remove strange characters */
    ttFrame.cTags = REPLACE(ttFrame.cTags,'&','').
    ttFrame.cTags = REPLACE(ttFrame.cTags,'?','').
  END.

END PROCEDURE. /* loadSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSettings wSettings
PROCEDURE saveSettings :
  /* Write settings back to the ini file
  */
  DEFINE VARIABLE hWidget  AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cSection AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSetting AS CHARACTER NO-UNDO.

  FOR EACH ttFrame:

    hWidget = ttFrame.hFrame:FIRST-CHILD:FIRST-CHILD.

    DO WHILE VALID-HANDLE(hWidget):

      /* Get value from INI file and set it in the widget */
      IF hWidget:PRIVATE-DATA <> ?
        AND NUM-ENTRIES(hWidget:PRIVATE-DATA) = 2 THEN
      DO:
        cSection = ENTRY(1,hWidget:PRIVATE-DATA).
        cSetting = ENTRY(2,hWidget:PRIVATE-DATA).

        IF hWidget:TYPE = 'BUTTON' THEN
        DO:
          IF cSection = 'DataDigger:fonts' THEN
            setRegistry(cSection, cSetting, STRING(hWidget:FONT)).
        END.

        ELSE
        IF hWidget:TYPE = 'TOGGLE-BOX' THEN
        DO:
          setRegistry(cSection, cSetting, STRING(hWidget:CHECKED)).
        END.

        ELSE
        IF hWidget:TYPE = 'FILL-IN'
          AND cSection = 'DataDigger:colors' THEN
        DO:
          setRegistry(cSection, cSetting + ':FG', STRING(hWidget:FGCOLOR)).
          setRegistry(cSection, cSetting + ':BG', STRING(hWidget:BGCOLOR)).
        END.

        ELSE
          setRegistry(cSection, cSetting, hWidget:SCREEN-VALUE).
      END.

      hWidget = hWidget:NEXT-SIBLING.
    END. /* f/e ttFrame */
  END. /* while valid-handle */

END PROCEDURE. /* saveSettings */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPage wSettings
PROCEDURE setPage :
  /* Show a page
  */
  DEFINE INPUT PARAMETER piPageNr AS INTEGER NO-UNDO.

  DEFINE VARIABLE hButton AS HANDLE  NO-UNDO.
  DEFINE VARIABLE iPage   AS INTEGER NO-UNDO.

  /* Remember the last active page */
  IF piPageNr <> 0 THEN
    ASSIGN giLastActivePage = piPageNr
           giCurrentPage = piPageNr.

  DO iPage = 1 TO NUM-ENTRIES(gcPageButtons):
    hButton = WIDGET-HANDLE( ENTRY(iPage,gcPageButtons) ).

    /* Normal sizes */
    ASSIGN
      hButton:X = 20
      hButton:Y = 60 + (iPage * 35)
      hButton:WIDTH-PIXELS = 125
      hButton:HEIGHT-PIXELS = 35
      hButton:LABEL = hButton:PRIVATE-DATA.
      .

    /* Selected button */
    IF iPage = piPageNr THEN
    DO:
      ASSIGN
        hButton:X = hButton:X - 10
        hButton:Y = hButton:Y - 5
        hButton:WIDTH-PIXELS = hButton:WIDTH-PIXELS + 10
        hButton:HEIGHT-PIXELS = hButton:HEIGHT-PIXELS + 10
        hButton:LABEL = CAPS(hButton:PRIVATE-DATA)
        .
      hButton:MOVE-TO-TOP().

      RUN showFrames('Page' + string(piPageNr)).
    END.
  END.

END PROCEDURE. /* setPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showFrames wSettings
PROCEDURE showFrames :
  /* Show all subframes containing a certain tag
  */
  DEFINE INPUT PARAMETER pcTag AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iRow AS INTEGER     NO-UNDO.

  RUN LockWindow (INPUT wSettings:HANDLE, INPUT YES).

  /* Show the first setting on y=42 */
  iRow = 15.

  /* Make the frame large enough to hold all settings */
  FRAME frSettings:virtual-height-pixels = 6720.
  FRAME frSettings:height-pixels = 390.

  /* Make frames visible based on whether the tags match */
  FOR EACH ttFrame TABLE-SCAN BY ttFrame.iOrder:

    ttFrame.hFrame:visible = ( ttFrame.cTags MATCHES '*' + pcTag + '*' ).

    IF ttFrame.hFrame:visible THEN
      ASSIGN
        ttFrame.hFrame:x = 1
        ttFrame.hFrame:y = iRow
        iRow = iRow + ttFrame.hFrame:height-pixels + 2.
  END.

  FRAME frSettings:width-pixels = rcSettings:width-pixels IN FRAME {&frame-name} - 10.
  FRAME frSettings:virtual-height-pixels = MAXIMUM(iRow,390).

  RUN showScrollBars( FRAME frSettings:handle
                    , NO
                    , (FRAME frSettings:virtual-height > FRAME frSettings:height)
                    ).

  RUN LockWindow (INPUT wSettings:HANDLE, INPUT NO).

END PROCEDURE. /* showFrames */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

