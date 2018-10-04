&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include
/*------------------------------------------------------------------------

	Name: frameLib.i
	Desc: Generic code that is needed to reparent frames

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
	 Type: Include
	 Allow:
	 Frames: 0
	 Add Fields to: Neither
	 Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB)
	CREATE WINDOW Include ASSIGN
				 HEIGHT             = 13.05
				 WIDTH              = 60.
/* END WINDOW DEFINITION */
																																				*/
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include


/* ***************************  Main Block  *************************** */

RUN initFrames.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initFrames Include
PROCEDURE initFrames :
	/* Initialize frames
	*/
	DELETE WIDGET {&WINDOW-NAME}.
	{&WINDOW-NAME} = CURRENT-WINDOW.

	RUN reparentFrames(INPUT FRAME DEFAULT-FRAME:HANDLE, INPUT phParent).

	RUN enable_UI.

	/* Adjust the size of the frame to the rectange (if provided) */
	IF VALID-HANDLE(phRectangle) THEN
		RUN setFrame ( INPUT phRectangle:X + 2
								 , INPUT phRectangle:Y + 2
								 , INPUT phRectangle:WIDTH-PIXELS - 4
								 , INPUT phRectangle:HEIGHT-PIXELS - 4
								 ).

END PROCEDURE. /* initFrames */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reparentFrames Include
PROCEDURE reparentFrames :
	/* Reparent all frames
	*/
	DEFINE INPUT  PARAMETER phOldParent AS HANDLE NO-UNDO.
	DEFINE INPUT  PARAMETER phNewParent AS HANDLE NO-UNDO.

	/* Attach all frames on the main frame to the parent */
	DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

	REPEAT:
		hWidget = phOldParent:FIRST-CHILD:first-child.
		IF NOT VALID-HANDLE(hWidget) THEN LEAVE.
		IF hWidget:TYPE = 'FRAME' THEN hWidget:FRAME = phNewParent.
	END.

END PROCEDURE. /* reparentFrames */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFrame Include
PROCEDURE setFrame :
	/* Position the frame to a specified location with a specified size
	*/
	DEFINE INPUT PARAMETER piFrame-x AS INTEGER NO-UNDO.
	DEFINE INPUT PARAMETER piFrame-y AS INTEGER NO-UNDO.
	DEFINE INPUT PARAMETER piFrame-w AS INTEGER NO-UNDO.
	DEFINE INPUT PARAMETER piFrame-h AS INTEGER NO-UNDO.

	IF piFrame-w <> ? THEN
	DO:
		FRAME {&frame-name}:WIDTH-PIXELS = piFrame-w.
		FRAME {&frame-name}:VIRTUAL-WIDTH-PIXELS  = piFrame-w.
	END.

	IF piFrame-h <> ? THEN
	DO:
		FRAME {&frame-name}:HEIGHT-PIXELS = piFrame-h.
		FRAME {&frame-name}:VIRTUAL-HEIGHT-PIXELS = piFrame-h.
	END.

	IF piFrame-x <> ? THEN FRAME {&frame-name}:X = piFrame-x.
	IF piFrame-y <> ? THEN FRAME {&frame-name}:Y = piFrame-y.

END PROCEDURE. /* setFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewFrame Include
PROCEDURE viewFrame :
	/* Show or hide the frame
	*/
	DEFINE INPUT PARAMETER plView AS LOGICAL NO-UNDO.

	FRAME {&frame-name}:HIDDEN = NOT plView.
	IF plView THEN APPLY 'entry' TO FRAME {&frame-name}.

END PROCEDURE. /* viewFrame */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
