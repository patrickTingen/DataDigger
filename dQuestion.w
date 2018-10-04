&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame
/*------------------------------------------------------------------------

  Name: dQuestion.w
  Desc: Show window to user and give back which button was pressed

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_is_Running) NE 0  &THEN
  DEFINE VARIABLE pcTitle         AS CHARACTER  NO-UNDO INITIAL 'Disconnect user'.
  DEFINE VARIABLE pcMessage       AS CHARACTER  NO-UNDO INITIAL 'Do you want to disconnect user "&1" from database "&2"?'.
  DEFINE VARIABLE pcButtons       AS CHARACTER  NO-UNDO INITIAL '&Yes,&No'.
  DEFINE VARIABLE plCanHide       AS LOGICAL    NO-UNDO INITIAL TRUE.
  DEFINE VARIABLE piButton        AS INTEGER    NO-UNDO INIT ?.
  DEFINE VARIABLE plDontShowAgain AS LOGICAL    NO-UNDO INIT ?.
&ELSE
  DEFINE INPUT PARAMETER  pcTitle         AS CHARACTER  NO-UNDO INITIAL 'Disconnect user'.
  DEFINE INPUT PARAMETER  pcMessage       AS CHARACTER  NO-UNDO INITIAL 'Do you want to disconnect user "&1" from database "&2"?'.
  DEFINE INPUT PARAMETER  pcButtons       AS CHARACTER  NO-UNDO INITIAL '&Yes,&No'.
  DEFINE INPUT PARAMETER  plCanHide       AS LOGICAL    NO-UNDO INITIAL TRUE.
  DEFINE OUTPUT PARAMETER piButton        AS INTEGER    NO-UNDO INITIAL ?.
  DEFINE OUTPUT PARAMETER plDontShowAgain AS LOGICAL    NO-UNDO INITIAL ?.
&ENDIF

{ DataDigger.i }

/* Allow testing */
&IF DEFINED(UIB_is_running) <> 0 &THEN

  DEFINE VARIABLE hDiggerLib AS HANDLE NO-UNDO.
  RUN DataDiggerLib.p PERSISTENT SET hDiggerLib.
  SESSION:ADD-SUPER-PROCEDURE(hDiggerLib, SEARCH-TARGET).

  /* Load or create personalized ini files */
  DEFINE VARIABLE cEnvironment AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cProgDir     AS CHARACTER NO-UNDO.

  cEnvironment = SUBSTITUTE('DataDigger-&1', getUserName()).
  OUTPUT to value(cProgDir + cEnvironment + '.ini') append.
  OUTPUT close.
  LOAD cEnvironment DIR cProgDir BASE-KEY 'ini' NO-ERROR.

  cEnvironment = 'DataDigger'.
  OUTPUT to value(cProgDir + cEnvironment + '.ini') append.
  OUTPUT close.
  LOAD cEnvironment DIR cProgDir BASE-KEY 'ini' NO-ERROR.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EdMessage BtnYes
&Scoped-Define DISPLAYED-OBJECTS tgDontShowAgain

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY
     LABEL "Cancel"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnNo AUTO-GO
     LABEL "&No"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON BtnYes AUTO-GO
     LABEL "&Yes"
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE VARIABLE EdMessage AS CHARACTER
     CONTEXT-HELP-ID 0
     VIEW-AS EDITOR NO-BOX
     SIZE-PIXELS 325 BY 74 NO-UNDO.

DEFINE IMAGE imgQuestion TRANSPARENT
     SIZE-PIXELS 32 BY 36.

DEFINE VARIABLE tgDontShowAgain AS LOGICAL INITIAL NO
     LABEL "&Don't show this message again"
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 215 BY 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     EdMessage AT Y 11 X 65 NO-LABEL NO-TAB-STOP
     BtnYes AT Y 115 X 145
     BtnNo AT Y 115 X 227
     BtnCancel AT Y 115 X 309
     tgDontShowAgain AT Y 145 X 5
     imgQuestion AT Y 11 X 10
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
         SIDE-LABELS NO-UNDERLINE THREE-D
         SIZE-PIXELS 402 BY 204
         TITLE "Question"
         DEFAULT-BUTTON BtnYes CANCEL-BUTTON BtnCancel.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BtnCancel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnNo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EdMessage IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN
       EdMessage:AUTO-RESIZE IN FRAME Dialog-Frame      = TRUE
       EdMessage:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR IMAGE imgQuestion IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tgDontShowAgain IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Question */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel Dialog-Frame
ON CHOOSE OF BtnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  piButton = 3.
  plDontShowAgain = tgDontShowAgain:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel Dialog-Frame
ON CURSOR-LEFT OF BtnCancel IN FRAME Dialog-Frame /* Cancel */
DO:
  APPLY "ENTRY" TO btnNo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNo Dialog-Frame
ON CHOOSE OF BtnNo IN FRAME Dialog-Frame /* No */
DO:
  piButton = 2.
  plDontShowAgain = tgDontShowAgain:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNo Dialog-Frame
ON CURSOR-LEFT OF BtnNo IN FRAME Dialog-Frame /* No */
DO:
  APPLY "ENTRY" TO btnYes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNo Dialog-Frame
ON CURSOR-RIGHT OF BtnNo IN FRAME Dialog-Frame /* No */
DO:
  APPLY "ENTRY" TO btnCancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnYes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnYes Dialog-Frame
ON CHOOSE OF BtnYes IN FRAME Dialog-Frame /* Yes */
DO:
  piButton = 1.
  plDontShowAgain = tgDontShowAgain:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnYes Dialog-Frame
ON CURSOR-RIGHT OF BtnYes IN FRAME Dialog-Frame /* Yes */
DO:
  APPLY "ENTRY" TO btnNo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ? THEN
  FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:

  RUN initializeObject.
  RUN enable_UI.

  /* Toggle for Don't Show Again is optional */
  tgDontShowAgain:visible = plCanHide.
  tgDontShowAgain:sensitive = plCanHide.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY tgDontShowAgain
      WITH FRAME Dialog-Frame.
  ENABLE EdMessage BtnYes
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame
PROCEDURE initializeObject :
/*
 * Init vars and frame
 */
  DEFINE VARIABLE iNumButtons   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dMargin       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iVertMargin   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cYesLabel     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNoLabel      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCancelLabel  AS CHARACTER  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    /* Get fonts */
    FRAME {&frame-name}:font = getFont('Default').

    iNumButtons  = NUM-ENTRIES(pcButtons).
    IF pcButtons = '' THEN pcButtons = 'OK'.

    /* Show Question-image or DD-logo */
    IF pcMessage MATCHES "*?" THEN
      imgQuestion:load-image( getImagePath('Question.gif')) NO-ERROR.
    ELSE
      imgQuestion:load-image( getImagePath("DataDigger24x24.gif")) NO-ERROR.

    /* Replace fake NEWLINES with chr(10) */
    pcMessage = REPLACE(pcMessage,'~~n',CHR(10)).

    /* Strip leading spaces */
    pcMessage = TRIM(pcMessage).

    PUBLISH "debugInfo" (1, pcMessage).

    /* Make some room in the frame for moving around with widgets */
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS * 2.

    ASSIGN
      FRAME {&FRAME-NAME}:TITLE  = IF pcTitle > '' THEN pcTitle ELSE FRAME {&FRAME-NAME}:TITLE
      edMessage:SCREEN-VALUE     = RIGHT-TRIM(pcMessage,CHR(10))
      edMessage:INNER-LINES      = edMessage:NUM-LINES
      dMargin                    = imgQuestion:COLUMN /* Use the editor Y as margin template */
      iVertMargin                = edMessage:Y
      btnYes:Y                   = edMessage:Y + edMessage:HEIGHT-PIXELS + iVertMargin
      btnNo:Y                    = btnYes:Y
      btnCancel:y                = btnYes:Y
      .

    /* Toggle for Don't Show Again is optional */
    IF plCanHide THEN
    DO:
      tgDontShowAgain:visible = plCanHide.
      tgDontShowAgain:sensitive = plCanHide.
      tgDontShowAgain:y = btnYes:Y + btnYes:height-pixels + iVertMargin.

      /* Add border top and bottom to ensure min heigth  */
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = tgDontShowAgain:Y + tgDontShowAgain:HEIGHT-PIXELS
                                        + FRAME {&FRAME-NAME}:BORDER-TOP-PIXELS
                                        + FRAME {&FRAME-NAME}:BORDER-BOTTOM-PIXELS
                                        + INTEGER(iVertMargin / 2) NO-ERROR.
    END.
    ELSE
    DO:
      tgDontShowAgain:visible = plCanHide.
      tgDontShowAgain:sensitive = plCanHide.
      tgDontShowAgain:y = 1.

      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = btnYes:Y + btnYes:height-pixels
                                        + FRAME {&FRAME-NAME}:BORDER-TOP-PIXELS
                                        + FRAME {&FRAME-NAME}:BORDER-BOTTOM-PIXELS
                                   + INTEGER(iVertMargin / 2) NO-ERROR.

    END.

    ASSIGN
      btnNo:HIDDEN           = iNumButtons < 2
      btnCancel:HIDDEN       = iNumButtons < 3
      btnNo:SENSITIVE        = NOT btnNo:HIDDEN
      btnCancel:SENSITIVE    = NOT btnCancel:HIDDEN

      cYesLabel              = ENTRY(1,pcButtons)
      cNoLabel               = ENTRY(2,pcButtons) WHEN iNumButtons >= 2
      cCancelLabel           = ENTRY(3,pcButtons) WHEN iNumButtons >= 3

      btnYes:LABEL           = IF cYesLabel > '':U THEN cYesLabel
                                  ELSE IF iNumButtons = 1 THEN 'OK' ELSE btnYes:LABEL
      btnNo:LABEL            = IF cNoLabel > '':U THEN cNoLabel ELSE btnNo:LABEL
      btnCancel:LABEL        = IF cCancelLabel > '':U THEN cCancelLabel ELSE btnCancel:LABEL
      btnYes:width           = MAX(btnYes:width,FONT-TABLE:GET-TEXT-WIDTH(btnYes:LABEL) + 1.5)
      btnNo:width            = MAX(btnNo:width,FONT-TABLE:GET-TEXT-WIDTH(btnNo:LABEL) + 1.5)
      btnCancel:width        = MAX(btnCancel:width,FONT-TABLE:GET-TEXT-WIDTH(btnCancel:LABEL) + 1.5)
      btnCancel:COLUMN       = FRAME {&FRAME-NAME}:WIDTH - (btnCancel:width + dMargin)
      btnNo:COLUMN           = IF btnCancel:HIDDEN
                                 THEN FRAME {&FRAME-NAME}:width - (btnNo:width + dMargin)
                                 ELSE btnCancel:COLUMN - (btnNo:width + (dMargin / 2))
      btnYes:COLUMN          = MAX(1, IF btnNo:HIDDEN
                                 THEN FRAME {&FRAME-NAME}:width - (btnYes:width + dMargin)
                                 ELSE btnNo:COLUMN - (btnYes:width + (dMargin / 2)) ).

    /* For some reasons, these #*$&# scrollbars keep coming back */
    RUN showScrollBars(FRAME {&frame-name}:handle, NO, NO). /* KILL KILL KILL */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
