/*------------------------------------------------------------------------

  File : ShowMessage.p
  Desc : Show a user defined message in a new window.

  ----------------------------------------------------------------------*/
  
&IF "{&file-name}" MATCHES "*.cmp" &THEN
  DEFINE VARIABLE pcTitle   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcMessage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE phWindow  AS HANDLE NO-UNDO.
&ELSE
  DEFINE INPUT  PARAMETER pcTitle   AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pcMessage AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER phWindow  AS HANDLE NO-UNDO.
&ENDIF

DEFINE VARIABLE cMessage    AS CHARACTER   NO-UNDO FORMAT "x(256)".
DEFINE VARIABLE iFont       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidth      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iImageWidth AS INTEGER     NO-UNDO.
DEFINE VARIABLE winMessage  AS HANDLE      NO-UNDO.

DEFINE IMAGE imgMessage SIZE 4.8 BY 1.14.
DEFINE FRAME infoFrame
  imgMessage AT ROW 1.3 COL 2 WIDGET-ID 2
  cMessage VIEW-AS FILL-IN SIZE 1 BY 1 AT ROW 1.4 COLUMN 1.5 NO-LABEL
  WITH 1 DOWN NO-BOX OVERLAY SIDE-LABELS THREE-D AT COLUMN 1 ROW 1 SIZE-PIXELS 50 BY 40.

/* *************************  Create Window  ************************** */
CREATE WINDOW winMessage ASSIGN
  TITLE         = pcTitle
  WIDTH-PIXELS  = 260
  HEIGHT-PIXELS = 40
  STATUS-AREA   = NO
  MESSAGE-AREA  = NO
  MIN-BUTTON    = NO
  MAX-BUTTON    = NO
  SENSITIVE     = YES.

winMessage:LOAD-ICON("image\default_DataDigger.ico").

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN
  CURRENT-WINDOW                = winMessage.
  THIS-PROCEDURE:CURRENT-WINDOW = winMessage.
  DEFAULT-WINDOW                = winMessage.

/* Find a decent font */
#FindFont:
DO iFont = 0 TO FONT-TABLE:NUM-ENTRIES - 1:
  IF    FONT-TABLE:GET-TEXT-WIDTH-PIXELS('DataDigger',iFont) = 54
    AND FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFont) = 13 THEN
  DO:
    FRAME infoFrame:font = iFont.
    LEAVE #FindFont.
  END.
END.

/* How wide should the text be? */
cMessage = pcMessage.
iWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(cMessage,iFont) + cMessage:x + 10.
iWidth = MAXIMUM(iWidth,150).

imgMessage:LOAD-IMAGE('image\default_DataDigger24x24.gif').
cMessage:X = imgMessage:X + imgMessage:WIDTH-PIXELS + 5.
winMessage:WIDTH-PIXELS = cMessage:X + iWidth + 5.
cMessage:WIDTH-PIXELS = iWidth.
cMessage:SCREEN-VALUE = cMessage.
FRAME infoFrame:WIDTH-PIXELS = winMessage:WIDTH-PIXELS.

/* Center the window */
winMessage:X = (SESSION:WORK-AREA-WIDTH-PIXELS - winMessage:WIDTH-PIXELS) / 2.
winMessage:Y = (SESSION:WORK-AREA-HEIGHT-PIXELS - winMessage:HEIGHT-PIXELS) / 2.

/* Showtime! */
VIEW FRAME infoFrame IN WINDOW winMessage.
VIEW winMessage.

/* Avoid input blocking error */
IF PROGRAM-NAME(2) <> "getDataserver.p" THEN PROCESS EVENTS.

phWindow = winMessage:HANDLE.

