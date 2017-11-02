/*------------------------------------------------------------------------

  File : ShowMessage.p
  Desc : Show a user defined message in a new window.

  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcTitle   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER pcMessage AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER phWindow  AS HANDLE NO-UNDO.

DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO FORMAT "x(256)".
DEFINE VARIABLE iFont      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE winMessage AS HANDLE      NO-UNDO.

DEFINE FRAME infoFrame
cMessage VIEW-AS FILL-IN SIZE 1 BY 1 AT ROW 1.5 COLUMN 1.5 NO-LABEL
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

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN
  CURRENT-WINDOW              = winMessage.
  THIS-PROCEDURE:CURRENT-WINDOW = winMessage.
  default-window = winMessage.

/* Find a decent font */
DO iFont = 0 TO FONT-TABLE:NUM-ENTRIES - 1:
  IF    FONT-TABLE:GET-TEXT-WIDTH-PIXELS('DataDigger',iFont) = 54
    AND FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(iFont) = 13 THEN
  DO:
    FRAME infoFrame:font = iFont.
    LEAVE.
  END.
END.

/* How wide should the text be? */
iWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(pcMessage,iFont) + cMessage:x + 30.
iWidth = MAXIMUM(iWidth,150).

winMessage:WIDTH-PIXELS = iWidth .
cMessage:width-pixels = iWidth - 10.
cMessage:screen-value = pcMessage.
FRAME infoFrame:width-pixels = iWidth.

/* Center the window */
winMessage:X = (SESSION:WORK-AREA-WIDTH-PIXELS - winMessage:WIDTH-PIXELS) / 2.
winMessage:Y = (SESSION:WORK-AREA-HEIGHT-PIXELS - winMessage:HEIGHT-PIXELS) / 2.

/* Showtime! */
VIEW FRAME infoFrame IN WINDOW winMessage.
VIEW winMessage.

PROCESS EVENTS.

phWindow = winMessage:HANDLE.
