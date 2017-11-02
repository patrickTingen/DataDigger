&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame
/*------------------------------------------------------------------------

	Name: wConnections.w
	Desc: Maintain connections for DataDigger

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_running)=0 &THEN
	DEFINE INPUT PARAMETER pcCommand   AS CHARACTER NO-UNDO.
	DEFINE INPUT PARAMETER pcAttribute AS CHARACTER NO-UNDO.
	DEFINE OUTPUT PARAMETER pcResult   AS CHARACTER NO-UNDO.
&ELSE
	DEFINE VARIABLE pcCommand   AS CHARACTER NO-UNDO INITIAL "UI".
	DEFINE VARIABLE pcAttribute AS CHARACTER NO-UNDO INITIAL "".
	DEFINE VARIABLE pcResult    AS CHARACTER NO-UNDO INITIAL "".
&ENDIF

{ DataDigger.i }

DEFINE VARIABLE gcRecordState AS CHARACTER NO-UNDO INITIAL 'nodata'.

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
&Scoped-define BROWSE-NAME brConnections

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttConnection

/* Definitions for BROWSE brConnections                                 */
&Scoped-define FIELDS-IN-QUERY-brConnections ttConnection.cLogicalName ttConnection.cDescription ttConnection.lConnected
&Scoped-define ENABLED-FIELDS-IN-QUERY-brConnections
&Scoped-define SELF-NAME brConnections
&Scoped-define QUERY-STRING-brConnections FOR EACH ttConnection
&Scoped-define OPEN-QUERY-brConnections OPEN QUERY {&SELF-NAME} FOR EACH ttConnection.
&Scoped-define TABLES-IN-QUERY-brConnections ttConnection
&Scoped-define FIRST-TABLE-IN-QUERY-brConnections ttConnection


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
		~{&OPEN-QUERY-brConnections}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBrowse btnDelete btnTest btnConnect ~
btnDisconnect cbSection btnAdd brConnections btnClone btnEdit btnSave ~
btnUndo
&Scoped-Define DISPLAYED-OBJECTS fiLogicalName cbSection fiDescription ~
fiDatabaseName edParameters

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNewConnectionNr Dialog-Frame
FUNCTION getNewConnectionNr RETURNS INTEGER
	( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd  NO-FOCUS FLAT-BUTTON
		 LABEL "&Add"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "add a connection".

DEFINE BUTTON btnBrowse
		 LABEL "..."
		 SIZE-PIXELS 25 BY 24 TOOLTIP "find database".

DEFINE BUTTON btnClone  NO-FOCUS FLAT-BUTTON
		 LABEL "&Clone"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "clone connection".

DEFINE BUTTON btnConnect DEFAULT  NO-FOCUS FLAT-BUTTON
		 LABEL "&Con"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "connect selected database"
		 BGCOLOR 8 .

DEFINE BUTTON btnDelete  NO-FOCUS FLAT-BUTTON
		 LABEL "&Delete"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "delete the currently selected connection".

DEFINE BUTTON btnDisconnect  NO-FOCUS FLAT-BUTTON
		 LABEL "&Dis"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "disconnect selected database".

DEFINE BUTTON btnEdit  NO-FOCUS FLAT-BUTTON
		 LABEL "&Edit"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "change settings of currently selected connection".

DEFINE BUTTON btnSave DEFAULT  NO-FOCUS FLAT-BUTTON
		 LABEL "&Save"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "save changes"
		 BGCOLOR 8 .

DEFINE BUTTON btnTest DEFAULT
		 LABEL "&Test"
		 SIZE-PIXELS 60 BY 24 TOOLTIP "test the currently selected connection"
		 BGCOLOR 8 .

DEFINE BUTTON btnUndo DEFAULT  NO-FOCUS FLAT-BUTTON
		 LABEL "&Undo"
		 SIZE-PIXELS 25 BY 24 TOOLTIP "cancel changes"
		 BGCOLOR 8 .

DEFINE VARIABLE cbSection AS CHARACTER
		 LABEL "Section in INI"
		 VIEW-AS COMBO-BOX SORT INNER-LINES 10
		 LIST-ITEMS "Item 1"
		 DROP-DOWN
		 SIZE-PIXELS 160 BY 21 TOOLTIP "the section in the INI file to save the settings to"
		 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE edParameters AS CHARACTER
		 VIEW-AS EDITOR SCROLLBAR-VERTICAL
		 SIZE-PIXELS 470 BY 180 TOOLTIP "the connection parameters for this database"
		 FGCOLOR 1 FONT 2 NO-UNDO.

DEFINE VARIABLE fiDatabaseName AS CHARACTER FORMAT "X(256)":U
		 LABEL "DB/PF name"
		 VIEW-AS FILL-IN
		 SIZE-PIXELS 375 BY 21 TOOLTIP "the physical database name or PF file name to connect"
		 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiDescription AS CHARACTER FORMAT "X(256)":U
		 LABEL "Description"
		 VIEW-AS FILL-IN
		 SIZE-PIXELS 400 BY 21 TOOLTIP "the description of this connection"
		 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiLogicalName AS CHARACTER FORMAT "X(256)":U
		 LABEL "Logical Name"
		 VIEW-AS FILL-IN
		 SIZE-PIXELS 160 BY 21 TOOLTIP "the logical name for this connection (no spaces)"
		 FGCOLOR 1  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brConnections FOR
			ttConnection SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brConnections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brConnections Dialog-Frame _FREEFORM
	QUERY brConnections DISPLAY
			ttConnection.cLogicalName
ttConnection.cDescription
ttConnection.lConnected
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
		WITH NO-ROW-MARKERS SEPARATORS
					&IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 60 BY 15
					&ELSE SIZE-PIXELS 299 BY 324 &ENDIF FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
		 btnBrowse AT Y 90 X 760 WIDGET-ID 56
		 btnDelete AT Y 5 X 565 WIDGET-ID 14
		 btnTest AT Y 325 X 725 WIDGET-ID 42
		 btnConnect AT Y 5 X 315 WIDGET-ID 48
		 fiLogicalName AT Y 40 X 375 COLON-ALIGNED WIDGET-ID 4
		 btnDisconnect AT Y 5 X 340 WIDGET-ID 52
		 cbSection AT Y 40 X 615 COLON-ALIGNED WIDGET-ID 54
		 btnAdd AT Y 5 X 390 WIDGET-ID 2
		 fiDescription AT Y 65 X 375 COLON-ALIGNED WIDGET-ID 38
		 fiDatabaseName AT Y 92 X 375 COLON-ALIGNED WIDGET-ID 6
		 brConnections AT Y 1 X 1 WIDGET-ID 200
		 btnClone AT Y 5 X 415 WIDGET-ID 46
		 edParameters AT Y 145 X 315 NO-LABEL WIDGET-ID 10
		 btnEdit AT Y 5 X 440 WIDGET-ID 12
		 btnSave AT Y 5 X 490 WIDGET-ID 22
		 btnUndo AT Y 5 X 515 WIDGET-ID 24
		 "Parameters:" VIEW-AS TEXT
					SIZE-PIXELS 120 BY 13 AT Y 130 X 315 WIDGET-ID 18
		WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
				 SIDE-LABELS NO-UNDERLINE THREE-D
				 SIZE-PIXELS 798 BY 382
				 TITLE "Database Connections" DROP-TARGET WIDGET-ID 100.


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
	 FRAME-NAME Custom                                                    */
/* BROWSE-TAB brConnections fiDatabaseName Dialog-Frame */
ASSIGN
			 FRAME Dialog-Frame:SCROLLABLE       = FALSE
			 FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN
			 brConnections:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
			 brConnections:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR EDITOR edParameters IN FRAME Dialog-Frame
	 NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDatabaseName IN FRAME Dialog-Frame
	 NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDescription IN FRAME Dialog-Frame
	 NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLogicalName IN FRAME Dialog-Frame
	 NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brConnections
/* Query rebuild information for BROWSE brConnections
		 _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttConnection.
		 _END_FREEFORM
		 _Query            is OPENED
*/  /* BROWSE brConnections */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON DROP-FILE-NOTIFY OF FRAME Dialog-Frame /* Database Connections */
DO:
	RUN addConnections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON MOUSE-SELECT-CLICK OF FRAME Dialog-Frame /* Database Connections */
DO:
	DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

	hWidget = getWidgetUnderMouse(INPUT FRAME dialog-frame:handle).

	IF NOT VALID-HANDLE(hWidget) THEN RETURN.

	IF LOOKUP(hWidget:NAME, "fiLogicalName,fiDescription,cbSection,fiDatabaseName,edParameters") > 0 THEN
		RUN btnEditChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Database Connections */
DO:
	APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brConnections
&Scoped-define SELF-NAME brConnections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConnections Dialog-Frame
ON START-SEARCH OF brConnections IN FRAME Dialog-Frame
DO:
	RUN openConnectionQuery(INPUT SELF:current-column:name,?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brConnections Dialog-Frame
ON VALUE-CHANGED OF brConnections IN FRAME Dialog-Frame
DO:
	RUN viewConnection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd Dialog-Frame
ON CHOOSE OF btnAdd IN FRAME Dialog-Frame /* Add */
OR "insert-mode" OF brConnections
DO:

	RUN btnAddChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowse Dialog-Frame
ON CHOOSE OF btnBrowse IN FRAME Dialog-Frame /* ... */
DO:

	DEFINE VARIABLE lOkay     AS LOGICAL    NO-UNDO.
	DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.

	cFileName = fiDatabaseName:screen-value.

	SYSTEM-DIALOG GET-FILE cFilename
		FILTERS "Databases (*.db)" "*.db",
						"All Files (*.*)" "*.*"
		INITIAL-FILTER 1
		MUST-EXIST
		USE-FILENAME
		DEFAULT-EXTENSION ".db"
		UPDATE lOkay.

	IF NOT lOkay THEN
		RETURN.

	DO WITH FRAME {&frame-name}:
		fiDatabaseName:screen-value = cFileName.
	END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClone Dialog-Frame
ON CHOOSE OF btnClone IN FRAME Dialog-Frame /* Clone */
OR "shift-ins" OF brConnections
OR "alt-o" OF brConnections
DO:

	RUN btnCloneChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnConnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnConnect Dialog-Frame
ON CHOOSE OF btnConnect IN FRAME Dialog-Frame /* Con */
DO:

	RUN btnConnectChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete Dialog-Frame
ON CHOOSE OF btnDelete IN FRAME Dialog-Frame /* Delete */
OR "delete-character" OF brConnections
DO:

	RUN btnDeleteChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDisconnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDisconnect Dialog-Frame
ON CHOOSE OF btnDisconnect IN FRAME Dialog-Frame /* Dis */
DO:

	RUN btnDisconnectChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEdit Dialog-Frame
ON CHOOSE OF btnEdit IN FRAME Dialog-Frame /* Edit */
OR "DEFAULT-ACTION" OF brConnections
DO:
	RUN btnEditChoose.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
OR "RETURN" OF fiLogicalName, fiDescription, fiDatabaseName, cbSection
DO:

	RUN btnSaveChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest Dialog-Frame
ON CHOOSE OF btnTest IN FRAME Dialog-Frame /* Test */
DO:

	RUN btnTestChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUndo Dialog-Frame
ON CHOOSE OF btnUndo IN FRAME Dialog-Frame /* Undo */
DO:

	RUN btnUndoChoose.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLogicalName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLogicalName Dialog-Frame
ON ANY-PRINTABLE OF fiLogicalName IN FRAME Dialog-Frame /* Logical Name */
DO:
	IF LASTKEY = 32 THEN
	DO:
		APPLY '_'.
		RETURN NO-APPLY.
	END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON END-ERROR OF FRAME Dialog-Frame
OR ENDKEY OF FRAME Dialog-Frame ANYWHERE
DO:
	IF fiLogicalName:sensitive THEN
	DO:
		RUN btnUndoChoose.
		RETURN NO-APPLY.
	END.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
	 ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

	RUN initializeObject.

	CASE pcCommand:
		WHEN 'connect'        THEN RUN connectDatabase(INPUT pcAttribute, OUTPUT pcResult).
		WHEN 'getConnections' THEN RUN getConnections(OUTPUT pcResult).
		WHEN 'UI'             THEN DO:
			RUN enable_UI.
			RUN openConnectionQuery(?,?).
			APPLY "value-changed" TO brConnections IN FRAME {&frame-name}.
			APPLY "ENTRY" TO brConnections IN FRAME {&frame-name}.
			WAIT-FOR GO OF FRAME {&FRAME-NAME}.
		END.
	END CASE.

END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addConnections Dialog-Frame
PROCEDURE addConnections :
	/* Add dropped files as connections
	*/
	DEFINE VARIABLE iFile AS INTEGER   NO-UNDO.
	DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
	DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
	DEFINE VARIABLE rReposition AS ROWID NO-UNDO.
	DEFINE VARIABLE cContents AS LONGCHAR NO-UNDO.

	DO WITH FRAME dialog-frame:

		/* If we are in edit-mode, accept only the first dropped file */
		IF gcRecordState = "edit"
			AND SELF:get-dropped-file(1) MATCHES "*~.pf" THEN
			fiDatabaseName:screen-value = SELF:get-dropped-file(1).

		/* Otherwise, add all files as new connections */
		ELSE
		DO iFile = 1 TO SELF:num-dropped-files:
			cFile = SELF:get-dropped-file(iFile).
			IF NOT cFile MATCHES "*~.pf" AND NOT cFile MATCHES "*~.db" THEN NEXT.

			cName = ENTRY(NUM-ENTRIES(cFile,"\"),cFile,"\").

			IF NOT CAN-FIND(FIRST ttConnection WHERE ttConnection.cDatabaseName = cName) THEN
			DO:
				CREATE ttConnection.
				ASSIGN
					ttConnection.iConnectionNr = getNewConnectionNr()
					ttConnection.cLogicalName  = ENTRY(1,cName,".")
					ttConnection.cLogicalName  = REPLACE(ttConnection.cLogicalName,' ','_')
					ttConnection.cDescription  = ENTRY(1,cName,".")
					ttConnection.cDatabaseName = cFile
					ttConnection.cSection      = ttConnection.cLogicalName
					ttConnection.lConnected    = FALSE
					.

				/* Different settings for PF and DB files */
				IF cName MATCHES "*~.db" THEN
					ttConnection.cParameters = "-1".
				ELSE
				DO:
					COPY-LOB FROM FILE cFile TO cContents NO-CONVERT.
					ttConnection.cParameters = STRING(cContents).
				END.

				/* Save to registry */
				RUN saveConnection(buffer ttConnection).

				IF rReposition = ? THEN rReposition = ROWID(ttConnection).
			END.
		END.

		IF rReposition <> ? THEN
		DO:
			RUN openConnectionQuery(?,?).
			brConnections:query:reposition-to-rowid(rReposition) NO-ERROR.
			APPLY "VALUE-CHANGED" TO brConnections.
		END.
	END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnAddChoose Dialog-Frame
PROCEDURE btnAddChoose :
	/* Set screen to add-mode
	*/
	gcRecordState = 'new'.

	CREATE ttConnection.
	ASSIGN ttConnection.iConnectionNr = getNewConnectionNr().

	RUN viewConnection.
	RUN setToolbar.

	APPLY "ENTRY" TO fiLogicalName IN FRAME dialog-frame.

END PROCEDURE. /* btnAddChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnCloneChoose Dialog-Frame
PROCEDURE btnCloneChoose :
	/* Duplicate a connection and go to update-mode
	*/
	DEFINE BUFFER bfOriginalConnection FOR ttConnection.

	gcRecordState = 'new'.

	FIND bfOriginalConnection
		WHERE ROWID(bfOriginalConnection) = rowid(ttConnection).

	CREATE ttConnection.
	BUFFER-COPY bfOriginalConnection TO ttConnection
		ASSIGN ttConnection.iConnectionNr = getNewConnectionNr()
					 ttConnection.lConnected    = FALSE.

	RUN viewConnection.
	RUN setToolbar.
	APPLY "ENTRY" TO fiLogicalName IN FRAME dialog-frame.

END PROCEDURE. /* btnCloneChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnConnectChoose Dialog-Frame
PROCEDURE btnConnectChoose :
	/* Try to connect to the current connection
	*/
	DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
	DEFINE VARIABLE cError AS CHARACTER   NO-UNDO INITIAL 'Errors:'.
	DEFINE VARIABLE lAlreadyConnected AS LOGICAL NO-UNDO.

	DO WITH FRAME dialog-frame:
		/* Warn if already connected */
		lAlreadyConnected = (LOOKUP( fiLogicalName:screen-value, getDatabaseList()) > 0).

		/* Try to establish a connection */
		SESSION:SET-WAIT-STATE('general').
		RUN connectDatabase(INPUT fiLogicalName:screen-value, OUTPUT cError).
		SESSION:SET-WAIT-STATE('').

		/* Refresh connection status */
		ttConnection.lConnected = (LOOKUP( ttConnection.cLogicalName, getDatabaseList() ) > 0).
		brConnections:refresh().
		RUN viewConnection.

		/* If no success, show why */
		IF ERROR-STATUS:ERROR THEN
		DO:
			DO iError = 1 TO ERROR-STATUS:NUM-MESSAGES:
				cError = SUBSTITUTE('&1~n&2 (&3)'
													 , cError
													 , ERROR-STATUS:GET-MESSAGE(iError)
													 , ERROR-STATUS:GET-NUMBER(iError)
													 ).
			END.
			MESSAGE cError VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
			APPLY "ENTRY" TO brConnections.
			RETURN NO-APPLY.
		END.

		ELSE
		/* Success, but report if db was already connected */
		IF lAlreadyConnected THEN
		DO:
			MESSAGE 'Database already connected' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
			APPLY "ENTRY" TO brConnections.
			RETURN NO-APPLY.
		END.

		APPLY "ENTRY" TO brConnections.
	END.

END PROCEDURE. /* btnConnectChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnDeleteChoose Dialog-Frame
PROCEDURE btnDeleteChoose :
	/* Delete a connection
	*/
	DEFINE VARIABLE lOk         AS LOGICAL NO-UNDO INITIAL TRUE.
	DEFINE VARIABLE iConn       AS INTEGER NO-UNDO.
	DEFINE VARIABLE rConnection AS ROWID NO-UNDO.
	DEFINE VARIABLE rDelete     AS ROWID NO-UNDO.

	DO WITH FRAME dialog-frame:
		MESSAGE 'Delete this connection?' VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lOk.

		IF lOk = TRUE THEN
		DO:
			/* Delete from registry */
			iConn = ttConnection.iConnectionNr.
			setRegistry('Connections', SUBSTITUTE('&1-ldbname'    , STRING(iConn,'999')), ? ).
			setRegistry('Connections', SUBSTITUTE('&1-description', STRING(iConn,'999')), ? ).
			setRegistry('Connections', SUBSTITUTE('&1-pdbname'    , STRING(iConn,'999')), ? ).
			setRegistry('Connections', SUBSTITUTE('&1-parameters' , STRING(iConn,'999')), ? ).

			/* Remember record to delete */
			rDelete = brConnections:query:get-buffer-handle(1):rowid.

			/* And try to find the "next" connection, from the query's point of view */
			IF brConnections:query:get-next THEN
				rConnection = brConnections:query:get-buffer-handle(1):rowid.

			/* Find back record to delete */
			brConnections:query:reposition-to-rowid(rDelete) NO-ERROR.
			DELETE ttConnection.

			/* Point browse to next connection */
			brConnections:query:reposition-to-rowid(rConnection) NO-ERROR.

			/* And reopen */
			RUN openConnectionQuery(?,?).
			RUN viewConnection.
		END.

		APPLY "ENTRY" TO brConnections.
	END.

END PROCEDURE. /* btnDeleteChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnDisconnectChoose Dialog-Frame
PROCEDURE btnDisconnectChoose :
	/* Disconnect db
	*/
	DEFINE VARIABLE cCurrentDb  AS CHARACTER   NO-UNDO.
	DEFINE VARIABLE lDisconnect AS LOGICAL     NO-UNDO.

	cCurrentDb = fiLogicalName:screen-value IN FRAME dialog-frame.

	MESSAGE SUBSTITUTE('Are you sure you want to disconnect database "&1"?', cCurrentDb)
		VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lDisconnect.
	IF lDisconnect <> YES THEN RETURN.

	DISCONNECT value(cCurrentDb).
	removeConnection(cCurrentDb).

	ttConnection.lConnected = (LOOKUP( ttConnection.cLogicalName, getDatabaseList() ) > 0).
	brConnections:refresh().
	RUN viewConnection.

	APPLY "ENTRY" TO brConnections.

END PROCEDURE. /* btnDisconnectChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnEditChoose Dialog-Frame
PROCEDURE btnEditChoose :
	/* Set screen to edit-mode
	*/
	DO WITH FRAME dialog-frame:
		gcRecordState = 'edit'.
		RUN setToolbar.
		APPLY "ENTRY" TO fiLogicalName IN FRAME dialog-frame.
	END.

END PROCEDURE. /* btnEditChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnSaveChoose Dialog-Frame
PROCEDURE btnSaveChoose :
	/* Save the current connection
	*/
	DO WITH FRAME dialog-frame:

		/* No spaces in logical name */
		fiLogicalName:screen-value = REPLACE(fiLogicalName:screen-value,' ','_').

		ASSIGN
			ttConnection.cLogicalName  = fiLogicalName:screen-value
			ttConnection.cDescription  = fiDescription:screen-value
			ttConnection.cDatabaseName = fiDatabaseName:screen-value
			ttConnection.cSection      = cbSection:screen-value
			ttConnection.cParameters   = edParameters:screen-value
			.
		IF ttConnection.cSection = ? THEN ttConnection.cSection = ttConnection.cLogicalName.

		/* Save to registry */
		RUN saveConnection(buffer ttConnection).

		RUN openConnectionQuery(?,?).
		RUN viewConnection.
		APPLY "ENTRY" TO brConnections.

	END.

END PROCEDURE. /* btnSaveChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnTestChoose Dialog-Frame
PROCEDURE btnTestChoose :
	/* Test the current connection
	*/
	DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
	DEFINE VARIABLE cError AS CHARACTER   NO-UNDO INITIAL 'Errors:'.
	DEFINE VARIABLE lAlreadyConnected AS LOGICAL NO-UNDO.

	DO WITH FRAME dialog-frame:
		lAlreadyConnected = (LOOKUP( fiLogicalName:screen-value, getDatabaseList()) > 0).

		/* Try to establish a connection */
		SESSION:SET-WAIT-STATE('general').
		CONNECT value(fiDatabaseName:screen-value)
						VALUE(edParameters:screen-value)
						VALUE(SUBSTITUTE(' -ld &1', fiLogicalName:screen-value))
			NO-ERROR.
		SESSION:SET-WAIT-STATE('').

		/* If no success, show why */
		IF ERROR-STATUS:ERROR THEN
		DO:
			DO iError = 1 TO ERROR-STATUS:NUM-MESSAGES:
				cError = SUBSTITUTE('&1~n&2 (&3)'
													 , cError
													 , ERROR-STATUS:GET-MESSAGE(iError)
													 , ERROR-STATUS:GET-NUMBER(iError)
													 ).
			END.
			MESSAGE cError VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
		END.
		ELSE
		DO:
			/* Otherwise disconnect the db since it's only a test */
			IF NOT lAlreadyConnected THEN
				DISCONNECT value(LDBNAME(NUM-DBS)).
			MESSAGE 'Connection successful' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
		END.
	END.

END PROCEDURE. /* btnTestConnection */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnUndoChoose Dialog-Frame
PROCEDURE btnUndoChoose :
	/* Undo changes and go back to display-mode
	*/
	DO WITH FRAME dialog-frame:

		IF gcRecordState = 'new' THEN DELETE ttConnection.

		RUN openConnectionQuery(?,?).
		RUN viewConnection.

		IF CAN-FIND(FIRST ttConnection) THEN
			gcRecordState = 'display'.
		ELSE
			gcRecordState = 'nodata'.

		RUN setToolbar.

		APPLY "ENTRY" TO brConnections.
	END.

END PROCEDURE. /* btnUndoChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectDatabase Dialog-Frame
PROCEDURE connectDatabase :
	/* Try to connect to a given database.
	*/
	DEFINE INPUT PARAMETER pcConnection AS CHARACTER NO-UNDO.
	DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.

	DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
	DEFINE BUFFER bConnection FOR ttConnection.

	/* Find the connection */
	FIND bConnection WHERE bConnection.cLogicalName = pcConnection NO-LOCK NO-ERROR.
	IF NOT AVAILABLE bConnection THEN
	DO:
		ASSIGN pcError = 'No such connection known'.
		RETURN.
	END.

	/* Try to establish a connection */
	SESSION:SET-WAIT-STATE('general').

	IF bConnection.cDatabaseName MATCHES "*~.pf" THEN
		CONNECT value("-pf " + bConnection.cDatabaseName) NO-ERROR.
	ELSE
	DO:
		CONNECT value(bConnection.cDatabaseName)
						VALUE(bConnection.cParameters)
						VALUE(SUBSTITUTE(' -ld &1', bConnection.cLogicalName)) NO-ERROR.

		IF NOT ERROR-STATUS:ERROR THEN
			addConnection(bConnection.cLogicalName, bConnection.cSection).
	END.

	SESSION:SET-WAIT-STATE('').

	/* If no success, show why */
	IF ERROR-STATUS:ERROR THEN
	DO:
		pcError = 'Error connecting database:'.

		DO iError = 1 TO ERROR-STATUS:NUM-MESSAGES:
			pcError = SUBSTITUTE('&1~n&2 (&3)'
												 , pcError
												 , ERROR-STATUS:GET-MESSAGE(iError)
												 , ERROR-STATUS:GET-NUMBER(iError)
												 ).
		END.
		RETURN.
	END.

END PROCEDURE. /* connectDatabase */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
	HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
	DISPLAY fiLogicalName cbSection fiDescription fiDatabaseName edParameters
			WITH FRAME Dialog-Frame.
	ENABLE btnBrowse btnDelete btnTest btnConnect btnDisconnect cbSection btnAdd
				 brConnections btnClone btnEdit btnSave btnUndo
			WITH FRAME Dialog-Frame.
	VIEW FRAME Dialog-Frame.
	{&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConnections Dialog-Frame
PROCEDURE getConnections :
	/* Return a comma separated list of all connections
	*/
	DEFINE OUTPUT PARAMETER pcConnectionList AS CHARACTER NO-UNDO.

	DEFINE BUFFER bConnection FOR ttConnection.

	FOR EACH bConnection BY bConnection.cLogicalName:
		pcConnectionList = pcConnectionList + bConnection.cLogicalName + ','.
	END.

	pcConnectionList = TRIM(pcConnectionList,',').

END PROCEDURE. /* getConnections */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Dialog-Frame
PROCEDURE initializeObject :
	/* Prepare the program.
	*/
	EMPTY TEMP-TABLE ttConnection.

	DEFINE VARIABLE iConnection AS INTEGER   NO-UNDO.
	DEFINE VARIABLE iFixedFont  AS INTEGER   NO-UNDO.
	DEFINE VARIABLE cSetting    AS CHARACTER NO-UNDO.

	IF pcCommand = 'UI' THEN
	DO WITH FRAME {&frame-name}:
		btnAdd       :load-image(getImagePath('Add.gif')).
		btnClone     :load-image(getImagePath('Clone.gif')).
		btnEdit      :load-image(getImagePath('Edit.gif')).
		btnDelete    :load-image(getImagePath('Delete.gif')).
		btnSave      :load-image(getImagePath('Save.gif')).
		btnUndo      :load-image(getImagePath('Clear.gif')).
		btnConnect   :load-image(getImagePath('Execute.gif')).
		btnDisconnect:load-image(getImagePath('Stop.gif')).

		iFixedFont = getFont('Fixed').
		fiLogicalName :font = iFixedFont.
		fiDescription :font = iFixedFont.
		fiDatabaseName:font = iFixedFont.
		edParameters  :font = iFixedFont.
		cbSection     :font = iFixedFont.
	END.

	/* Arbitrarily test for max 999 connections
	 * connection numbers need to be sequential
	 */
	connectionLoop:
	DO iConnection = 1 TO 999:

		/* Find the ID of the connection. */
		cSetting = getRegistry('Connections', SUBSTITUTE('&1-ldbname',STRING(iConnection,'999'))).
		IF cSetting = ? THEN NEXT connectionLoop.

		CREATE ttConnection.
		ttConnection.iConnectionNr = iConnection.
		ttConnection.cLogicalName  = cSetting.
		ttConnection.cDescription  = getRegistry('Connections', SUBSTITUTE('&1-description',STRING(iConnection,'999'))).
		ttConnection.cDatabaseName = getRegistry('Connections', SUBSTITUTE('&1-pdbname'    ,STRING(iConnection,'999'))).
		ttConnection.cParameters   = getRegistry('Connections', SUBSTITUTE('&1-parameters' ,STRING(iConnection,'999'))).
		ttConnection.cParameters   = REPLACE(ttConnection.cParameters,CHR(1),'~n').
		ttConnection.cSection      = getRegistry('Connections', SUBSTITUTE('&1-section'    ,STRING(iConnection,'999'))).
		ttConnection.lConnected    = (LOOKUP( ttConnection.cLogicalName, getDatabaseList() ) > 0).

		/* Protect against blank value */
		IF ttConnection.cDescription  = ? THEN ttConnection.cDescription  = "".
		IF ttConnection.cDatabaseName = ? THEN ttConnection.cDatabaseName = "".
		IF ttConnection.cParameters   = ? THEN ttConnection.cParameters   = "".
		IF ttConnection.cSection      = ? THEN ttConnection.cSection      = ttConnection.cLogicalName.
	END.

	/* Get sort for Connections */
	DO WITH FRAME {&frame-name}:

		/* Get fonts */
		FRAME {&frame-name}:font = getFont('Default').
		edParameters:font = getFont('Fixed').

		cSetting = getRegistry('DataDigger','ColumnSortConnections').
		IF cSetting <> ? THEN
			brConnections:set-sort-arrow(INTEGER(ENTRY(1,cSetting)), LOGICAL(ENTRY(2,cSetting)) ).
	END.

	RUN openConnectionQuery(?,?).

END PROCEDURE. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openConnectionQuery Dialog-Frame
PROCEDURE openConnectionQuery :
	/* Undo changes and go back to display-mode
	*/
	DEFINE INPUT PARAMETER pcSortField  AS CHARACTER   NO-UNDO.
	DEFINE INPUT PARAMETER plAscending  AS LOGICAL     NO-UNDO.

	DEFINE VARIABLE rCurrentRecord   AS ROWID       NO-UNDO.
	DEFINE VARIABLE lAscending       AS LOGICAL     NO-UNDO.
	DEFINE VARIABLE cOldSort         AS CHARACTER   NO-UNDO.
	DEFINE VARIABLE cNewSort         AS CHARACTER   NO-UNDO.
	DEFINE VARIABLE cQuery           AS CHARACTER   NO-UNDO.
	DEFINE VARIABLE hQuery           AS HANDLE      NO-UNDO.
	DEFINE VARIABLE cConnectionList  AS CHARACTER   NO-UNDO.

	DO WITH FRAME {&frame-name}:

		/* Protect routine against invalid input */
		IF pcSortField = '' THEN pcSortField = ?.

		/* Remember record we're on */
		IF brConnections:num-selected-rows > 0 THEN
			rCurrentRecord = brConnections:query:get-buffer-handle(1):rowid.

		/* Find out what the current sort is */
		RUN getColumnSort(INPUT brConnections:handle, OUTPUT cOldSort, OUTPUT lAscending).

		/* If no new sortfield is provided, we don't want to change the sort.
		 * This happens when we press the filter button.
		 */
		IF pcSortField = ? THEN
			ASSIGN
				cNewSort   = cOldSort
				lAscending = lAscending. /* dont change order */
		ELSE
		IF pcSortField = cOldSort THEN
			ASSIGN
				cNewSort   = cOldSort
				lAscending = NOT lAscending. /* invert order */
		ELSE
			/* New field */
			ASSIGN
				cNewSort   = pcSortField
				lAscending = TRUE.

		/* Sort direction might be overruled */
		IF plAscending <> ? THEN lAscending = plAscending.

		/* Wich column should have what arrow? */
		RUN setSortArrow(brConnections:handle, cNewSort, lAscending).

		/* Rebuild the query */
		IF VALID-HANDLE(brConnections:query) THEN DO:
			brConnections:query:query-close().
			DELETE OBJECT brConnections:query.
		END.

		CREATE QUERY hQuery.
		hQuery:SET-BUFFERS(BUFFER ttConnection:handle).

		/* Build the query */
		cQuery = 'for each ttConnection where true'.
		cQuery = SUBSTITUTE("&1 by &2 &3", cQuery, cNewSort, STRING(lAscending,'/descending')).
		hQuery:QUERY-PREPARE(cQuery).
		hQuery:QUERY-OPEN().
		hQuery:GET-FIRST.

		/* Attach query to the browse */
		brConnections:query IN FRAME {&frame-name} = hQuery.

		/* Jump back to selected row */
		IF NOT hQuery:QUERY-OFF-END
			AND CAN-FIND(ttConnection WHERE ROWID(ttConnection) = rCurrentRecord) THEN
		DO:
			hQuery:REPOSITION-TO-ROWID(rCurrentRecord) NO-ERROR.
			brConnections:select-focused-row().
		END.

		IF AVAILABLE ttConnection THEN
			gcRecordState = 'display'.
		ELSE
			gcRecordState = 'nodata'.

		/* Collect all db names for "share settings" combo */
		RUN getConnections(OUTPUT cConnectionList).
		cbSection:list-items = " ," + cConnectionList.
	END.

	RUN setToolbar.

END PROCEDURE. /* openConnectionQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveConnection Dialog-Frame
PROCEDURE saveConnection :
	/* Save a connection to the INI file
	*/
	DEFINE PARAMETER BUFFER ttConnection FOR ttConnection.

	DEFINE VARIABLE iConn AS INTEGER NO-UNDO.

	iConn = ttConnection.iConnectionNr.

	setRegistry('Connections', SUBSTITUTE('&1-ldbname'    , STRING(iConn,'999')), ttConnection.cLogicalName  ).
	setRegistry('Connections', SUBSTITUTE('&1-description', STRING(iConn,'999')), ttConnection.cDescription  ).
	setRegistry('Connections', SUBSTITUTE('&1-pdbname'    , STRING(iConn,'999')), ttConnection.cDatabaseName ).
	setRegistry('Connections', SUBSTITUTE('&1-parameters' , STRING(iConn,'999')), REPLACE(ttConnection.cParameters,'~n',CHR(1)) ).
	setRegistry('Connections', SUBSTITUTE('&1-section'    , STRING(iConn,'999')), ttConnection.cSection  ).

END PROCEDURE. /* saveConnection */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setToolbar Dialog-Frame
PROCEDURE setToolbar :
	/* Set the state of the icons on the toolbar
	*/
	DO WITH FRAME {&frame-name}:
		DISABLE btnAdd btnClone btnEdit btnDelete btnTest btnSave btnUndo
						fiLogicalName fiDescription fiDatabaseName cbSection edParameters btnBrowse
						.

		CASE gcRecordState:
			WHEN 'nodata'  THEN ENABLE btnAdd  .
			WHEN 'display' THEN ENABLE btnAdd  btnClone btnEdit btnDelete btnTest.
			WHEN 'edit'    THEN ENABLE btnTest btnSave btnUndo btnDelete fiLogicalName fiDescription fiDatabaseName cbSection edParameters btnBrowse.
			WHEN 'new'     THEN ENABLE btnTest btnSave btnUndo fiLogicalName fiDescription fiDatabaseName cbSection edParameters btnBrowse.
		END CASE.

		brConnections:sensitive = (gcRecordState = 'display').
	END.

END PROCEDURE. /* setToolbar */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewConnection Dialog-Frame
PROCEDURE viewConnection :
	/* Show the details of the connection on the screen.
	*/
	DO WITH FRAME {&frame-name}:

		IF AVAILABLE ttConnection THEN
		DO:
			ASSIGN
				fiLogicalName:screen-value   = ttConnection.cLogicalName
				fiDescription:screen-value   = ttConnection.cDescription
				fiDatabaseName:screen-value  = ttConnection.cDatabaseName
				edParameters:screen-value    = ttConnection.cParameters
				cbSection:screen-value       = ttConnection.cSection
				btnDisconnect:sensitive      = ttConnection.lConnected
				btnConnect:sensitive         = NOT ttConnection.lConnected
				btnBrowse:sensitive          = FALSE
				.
		END.
		ELSE
			ASSIGN
				fiLogicalName:screen-value   = ""
				fiDescription:screen-value   = ""
				fiDatabaseName:screen-value  = ""
				edParameters:screen-value    = ""
				cbSection:screen-value       = ?
				btnDisconnect:sensitive      = FALSE
				btnConnect:sensitive         = FALSE
				btnBrowse:sensitive          = FALSE
				.
	END.

END PROCEDURE. /* viewConnection */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNewConnectionNr Dialog-Frame
FUNCTION getNewConnectionNr RETURNS INTEGER
	( /* parameter-definitions */ ) :

	/* Return a nr for the new connection.
	*/
	DEFINE VARIABLE iNewNr AS INTEGER NO-UNDO.

	DO iNewNr = 1 TO 999:
		IF NOT CAN-FIND(ttConnection WHERE ttConnection.iConnectionNr = iNewNr) THEN LEAVE.
	END.

	/* No nrs avail */
	IF iNewNr = 999 THEN
		MESSAGE "Out of connection numbers! ~nPlease contact patrick@tingen.net" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
	ELSE
	IF iNewNr > 900 THEN
		MESSAGE "Almost out of connection numbers! ~nPlease contact patrick@tingen.net" VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

	RETURN iNewNr.   /* Function return value. */

END FUNCTION. /* getNewConnectionNr */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

