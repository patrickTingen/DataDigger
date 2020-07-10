&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  Name: DataDigger2.p
  Desc: Recompile and start datadigger

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Constants ***************************** */

/* GetDriveType return values */
&GLOBAL-DEFINE DRIVE_UNKNOWN     0
&GLOBAL-DEFINE DRIVE_NO_ROOT_DIR 1
&GLOBAL-DEFINE DRIVE_REMOVABLE   2
&GLOBAL-DEFINE DRIVE_FIXED       3
&GLOBAL-DEFINE DRIVE_REMOTE      4
&GLOBAL-DEFINE DRIVE_CDROM       5
&GLOBAL-DEFINE DRIVE_RAMDISK     6


/* ***************************  Definitions  ************************** */
/* This one is also defined in dataReader.p and set to TRUE there */
&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT PARAMETER plReadOnlyDigger AS LOGICAL NO-UNDO.
&ELSE
  DEFINE VARIABLE plReadOnlyDigger AS LOGICAL NO-UNDO.
&ENDIF

DEFINE VARIABLE giNumDiggers AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcProgramDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttOsFile NO-UNDO RCODE-INFORMATION
  FIELD cFileName     AS CHARACTER FORMAT 'x(30)'
  FIELD cFileType     AS CHARACTER FORMAT 'x(8)'
  FIELD iFileSize     AS INTEGER   FORMAT '>>,>>>,>>9 '
  FIELD dtModified    AS DATETIME  FORMAT '99-99-9999 HH:MM:SS '
  FIELD cModified     AS CHARACTER FORMAT 'x(20)'
  FIELD cFullPathname AS CHARACTER FORMAT 'x(60)'
  FIELD cBaseName     AS CHARACTER FORMAT 'x(40)'
  FIELD cStatus       AS CHARACTER FORMAT 'x(20)'
  INDEX iPrim IS PRIMARY cBaseName cFileType
  INDEX iType cFileType
  .

PROCEDURE GetDriveTypeA EXTERNAL "kernel32.dll":
  DEFINE INPUT  PARAMETER lpRootPathName AS CHARACTER.
  DEFINE RETURN PARAMETER iType          AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDriveType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDriveType Procedure 
FUNCTION getDriveType RETURNS CHARACTER ( pcDrive AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcessorArchitecture) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcessorArchitecture Procedure 
FUNCTION getProcessorArchitecture RETURNS INTEGER() FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProwin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProwin Procedure 
FUNCTION getProwin RETURNS CHARACTER() FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRegistry Procedure 
FUNCTION getRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTimeStamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTimeStamp Procedure 
FUNCTION getTimeStamp RETURNS CHARACTER
  ( INPUT pDateTime AS DATETIME )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isFolderWritable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isFolderWritable Procedure 
FUNCTION isFolderWritable RETURNS LOGICAL
  ( INPUT pcFolderName AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isRecompileNeeded) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isRecompileNeeded Procedure 
FUNCTION isRecompileNeeded RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRegistry Procedure 
FUNCTION setRegistry RETURNS CHARACTER
  ( pcSection AS CHARACTER 
  , pcSetting AS CHARACTER
  , pcValue   AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 21.38
         WIDTH              = 46.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN initializeObject.

/* Notifications of starts and stops of DataDigger windows */
SUBSCRIBE TO 'DataDigger' ANYWHERE.

/* When we start, do a check whether we need to recompile */
RUN recompileDataDigger.

/* Start the main window */
RUN VALUE(gcProgramDir + 'wDataDigger.w') PERSISTENT (INPUT plReadOnlyDigger).

/* Sit back and relax */
IF NOT THIS-PROCEDURE:PERSISTENT THEN
DO:
  WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF SESSION:FIRST-PROCEDURE:FILE-NAME MATCHES '*DataDiggerLib.p' THEN QUIT.
END.

ELSE
DO:
  ON CLOSE OF THIS-PROCEDURE
  DO:
    DEFINE VARIABLE hDiggerLib AS HANDLE NO-UNDO.

    DELETE OBJECT THIS-PROCEDURE NO-ERROR.
    PUBLISH 'DataDiggerClose'.

    /* Kill the library */
    PUBLISH 'DataDiggerLib' (OUTPUT hDiggerLib).
    IF VALID-HANDLE(hDiggerLib) THEN
    DO:
      APPLY 'close' TO hDiggerLib.
      DELETE OBJECT hDiggerLib NO-ERROR.
    END.

    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-createDummyDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createDummyDb Procedure 
PROCEDURE createDummyDb :
/* Create an empty dummy db
*/
  DEFINE OUTPUT PARAMETER pcDummyDb AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDatabase AS CHARACTER NO-UNDO.

  #FindName:
  REPEAT:
    pcDummyDb = "DD_" + STRING(ETIME).
    cDatabase = SESSION:TEMP-DIR + pcDummyDb + ".db".
    FILE-INFORMATION:FILE-NAME = cDatabase.
    IF FILE-INFORMATION:FULL-PATHNAME = ? THEN LEAVE #FindName.
  END.

  CREATE DATABASE cDatabase FROM "EMPTY" REPLACE NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN CONNECT VALUE(cDatabase) -1.

END PROCEDURE. /* createDummyDb */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DataDigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataDigger Procedure 
PROCEDURE DataDigger :
/* Notifications of starts and stops of DataDigger windows
  */
  DEFINE INPUT PARAMETER piChange AS INTEGER NO-UNDO.

  giNumDiggers = giNumDiggers + piChange.
  IF giNumDiggers = 0 THEN APPLY 'close' TO THIS-PROCEDURE.

END PROCEDURE. /* DataDigger */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteDummyDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDummyDb Procedure 
PROCEDURE deleteDummyDb :
/* Create the empty dummy db
*/
  DEFINE INPUT PARAMETER pcDummyDb AS CHARACTER NO-UNDO.

  DISCONNECT VALUE(pcDummyDb) NO-ERROR.

  OS-DELETE VALUE(SESSION:TEMP-DIR + pcDummyDb + ".lg").
  OS-DELETE VALUE(SESSION:TEMP-DIR + pcDummyDb + ".b1").
  OS-DELETE VALUE(SESSION:TEMP-DIR + pcDummyDb + ".d1").
  OS-DELETE VALUE(SESSION:TEMP-DIR + pcDummyDb + ".db").
  OS-DELETE VALUE(SESSION:TEMP-DIR + pcDummyDb + ".st").

END PROCEDURE. /* deleteDummyDb */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSourceFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSourceFiles Procedure 
PROCEDURE getSourceFiles :
/* Read all source files with date/time stamp
  */
  DEFINE INPUT  PARAMETER pcDirectory AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER TABLE FOR ttOsFile.

  DEFINE VARIABLE cExtension AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile      AS CHARACTER   NO-UNDO EXTENT 3.

  DEFINE BUFFER bOsFile FOR ttOsFile.

  /* Read contents of progdir */
  EMPTY TEMP-TABLE bOsFile.
  INPUT FROM OS-DIR (pcDirectory).

  fileLoop:
  REPEAT:
    IMPORT cFile[1 FOR 3]. /* File FullPath Attributes */

    /* Only files */
    IF NOT cFile[3] BEGINS 'F' THEN NEXT fileLoop.
    cExtension = ENTRY(NUM-ENTRIES(cFile[1], '.'),cFile[1],'.').

    /* Check if we see image files. These do not belong here, so just
     * move them to their own directory (and create that one if needed)
     */
    IF LOOKUP(cExtension,'gif,ico') > 0 THEN
    DO:
      OS-CREATE-DIR "image".
      OS-COPY VALUE(cFile[2]) VALUE("image\" + cFile[1]).
      OS-DELETE VALUE(cFile[2]).
      NEXT fileLoop.
    END.

    /* Only valid file types (src + obj) */
    IF LOOKUP(cExtension,'r,i,w,p,cls') = 0 THEN NEXT fileLoop.

    /* get info modified */
    FILE-INFO:FILE-NAME = cFile[2].

    /* Create it */
    CREATE bOsFile.
    ASSIGN
      bOsFile.dtModified    = DATETIME(STRING(FILE-INFO:FILE-MOD-DATE) + ' ' + STRING(FILE-INFO:FILE-MOD-TIME,'hh:mm:ss'))
      bOsFile.cModified     = getTimeStamp(bOsFile.dtModified)
      bOsFile.cFileName     = cFile[1]
      bOsFile.iFileSize     = FILE-INFO:FILE-SIZE
      bOsFile.cBaseName     = SUBSTRING(cFile[1],1,R-INDEX(cFile[1],'.') - 1)
      bOsFile.cFullPathname = cFile[2]
      bOsFile.cFileType     = cExtension
      .
  END.
  INPUT CLOSE.

END PROCEDURE. /* getSourceFiles */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Procedure 
PROCEDURE initializeObject :
/* Initialize all kind of things.
  */
  /* Are we at least 10.1B ? */
  IF PROVERSION < "10.1B" THEN
  DO:
    MESSAGE "You need at least Progress 10.1B to run DataDigger" SKIP(1)
            "The program will now quit."
            VIEW-AS ALERT-BOX INFORMATION.
    QUIT.
  END.

  /* Where are we running from? */
  &IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
    FILE-INFO:FILE-NAME = THIS-PROCEDURE:FILE-NAME.
    IF FILE-INFO:FULL-PATHNAME = ? THEN
      FILE-INFO:FILE-NAME = REPLACE(THIS-PROCEDURE:FILE-NAME, '.p', '.r').
  
    gcProgramDir = REPLACE(FILE-INFO:FULL-PATHNAME,"\","/").
    gcProgramDir = SUBSTRING(gcProgramDir,1,R-INDEX(gcProgramDir,'/')).
  &ELSE
    /* Debugging */
    gcProgramDir = 'c:\Data\DropBox\DataDigger\Src\'.
  &ENDIF

  /* Add program dir to propath (if not already in) */
  IF SEARCH('datadigger.txt') = ? THEN
    PROPATH = gcProgramDir + ',' + PROPATH.

  /* If the help-ini does not exist, refuse to start */
  IF SEARCH(gcProgramDir + "DataDiggerHelp.ini") = ? THEN
  DO:
    MESSAGE "The file DataDiggerHelp.ini is missing." 
      SKIP "Please download and reinstall DataDigger again" 
      SKIP(1) "The program will now quit"
           VIEW-AS ALERT-BOX INFORMATION.
    OS-COMMAND NO-WAIT START VALUE("https://datadigger.wordpress.com/download/").
    QUIT.
  END.

  /* If the general ini file does not exist, create it */
  IF SEARCH(gcProgramDir + "DataDigger.ini") = ? THEN
  DO:
    OUTPUT TO VALUE(gcProgramDir + "DataDigger.ini").
    OUTPUT CLOSE.
  END.

  /* In any case, load it */
  LOAD 'DataDigger' DIR gcProgramDir BASE-KEY 'ini' NO-ERROR.

  /* Check some basic settings */
  IF getRegistry("DataDigger", "AutoCompile")   = ? THEN setRegistry("DataDigger", "AutoCompile"  , "yes").
  IF getRegistry("DataDigger", "StartDebugger") = ? THEN setRegistry("DataDigger", "StartDebugger", "no" ).
  IF getRegistry("DataDigger", "WorkFolder")    = ? THEN setRegistry("DataDigger", "WorkFolder"   , " " ).

  PUBLISH "DD:Timer" ("start", "Startup").

END PROCEDURE. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recompileDataDigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recompileDataDigger Procedure 
PROCEDURE recompileDataDigger :
/* Recompile all files and restart if needed
  */
  DEFINE VARIABLE cSetting   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lRecompile AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cDummyDb   AS CHARACTER   NO-UNDO.

  /* You can specify in the settings that you do not want to compile
   * This can be useful when you use DD in multiple environments
   * with different versions or codepages
   */
  cSetting = getRegistry("DataDigger", "AutoCompile").
  IF LOGICAL(cSetting) = FALSE THEN RETURN.

  /* Find out if a recompile is needed (change in source files / newer version / older version) */
  lRecompile = isRecompileNeeded().

  IF lRecompile THEN
  DO:
    /* Startup parameter -s should be set */
    IF INDEX(SESSION:STARTUP-PARAMETERS, "-s ") = 0 THEN
    DO:
      MESSAGE "You have not specified the -s startup parameter. DataDigger will not compile without this." SKIP(1)
              "Please set it to at least 128 and then try again."
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      STOP.
    END.

    /* If no databases connected, then create an empty db 
     * so getVersion.p can be compiled */
    IF NUM-DBS = 0 THEN 
    DO:
      RUN createDummyDb(OUTPUT cDummyDb).
      
      IF NUM-DBS = 0 THEN 
      DO:
        MESSAGE "Cannot create dummy database in folder" cDummyDb SKIP 
                "DataDigger needs at least 1 connected db to compile." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        OS-COMMAND NO-WAIT START 'https://github.com/patrickTingen/DataDigger/wiki/Problem-CannotCreateDummyDB'.
        STOP.
      END.
    END.

    RUN recompileSelf.

    IF cDummyDb <> '' THEN 
      RUN deleteDummyDb(cDummyDb).

    /* Force a restart of the library */
    RUN startDiggerLib(INPUT lRecompile).
  END.

END PROCEDURE. /* recompileDataDigger */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-recompileSelf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recompileSelf Procedure 
PROCEDURE recompileSelf :
/* Recompile all DataDigger procedures
  */
  DEFINE VARIABLE cBuildNr           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDiggerDriveType   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cExpectedDateTime  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFileList          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLogFile           AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLogicalDbName     AS CHARACTER   NO-UNDO.  
  DEFINE VARIABLE cMemory            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cProgressDriveType AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSystem            AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVersionInfo       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cWorkfolder        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hWindow            AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iCount             AS INTEGER     NO-UNDO.  
  DEFINE VARIABLE iFile              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lCompileError      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE lCoreFileError     AS LOGICAL     NO-UNDO.
  
  DEFINE BUFFER bOsFile FOR ttOsFile.

  /* Get progress version info */
  IF SEARCH("version") <> ? THEN
  DO:
    INPUT FROM VALUE(SEARCH("version")).
    IMPORT UNFORMATTED cVersionInfo.
    INPUT CLOSE.
  END.

  /* Get DD version */
  IF SEARCH(gcProgramDir + 'build.i') <> ? THEN
  DO:
    INPUT FROM VALUE(SEARCH(gcProgramDir + 'build.i')).
    IMPORT UNFORMATTED cBuildNr.
    INPUT CLOSE.
  END.

  /* See if there is a special .p for this particular build */
  IF SEARCH("precompile.p") <> ? THEN
  DO:
    OS-DELETE VALUE(SEARCH("precompile.r")).
    RUN VALUE(SEARCH("precompile.p")) NO-ERROR.
  END.

  /* Get Windows version info */
  RUN adecomm\_winsys.p(OUTPUT cSystem, OUTPUT cMemory).

  /* Check whether there are any sources. If you distribute DD without
   * sources, you definitely do not want to delete object files!
   */
  IF NOT CAN-FIND(FIRST bOsFile
                  WHERE bOsFile.cFileType = "p"
                     OR bOsFile.cFileType = "cls"
                     OR bOsFile.cFileType = "w") THEN
  DO:
    MESSAGE "No source files found. Compiling aborted.".
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE bOsFile.
    DELETE WIDGET hWindow.
    RETURN.
  END.

  /* Start the timer. We want the message to appear at least a certain time
   * to avoid flashing of windows
   */
  ETIME(YES).
  RUN showMessage.p("DataDigger", "Please wait while DataDigger is recompiled.", OUTPUT hWindow).
  SESSION:SET-WAIT-STATE("general").

  /* Open log */
  cLogFile = gcProgramDir + "DataDigger.log".
  OUTPUT TO VALUE(cLogFile).
  PUT UNFORMATTED "DataDigger recompile as of " STRING(NOW,"99-99-9999 HH:MM:SS").

  FILE-INFO:FILE-NAME = getProwin().
  cProgressDriveType = getDriveType(ENTRY(1,FILE-INFO:FULL-PATHNAME,"\")).
  cDiggerDriveType   = getDriveType(ENTRY(1,gcProgramDir,"\")).

  cWorkfolder = getRegistry("DataDigger", "WorkFolder").
  IF cWorkfolder = ? OR cWorkfolder = '' THEN cWorkfolder = gcProgramDir.

  PUT UNFORMATTED SKIP(1) "DataDigger Buildnr   : " cBuildNr.
  PUT UNFORMATTED SKIP(1) "ENVIRONMENT".
  PUT UNFORMATTED SKIP(0) "  Progress version   : " PROVERSION " " PROGRESS " " SESSION:CLIENT-TYPE.
  PUT UNFORMATTED SKIP(0) "  Version info file  : " cVersionInfo.
  PUT UNFORMATTED SKIP(0) "  Progress path      : " FILE-INFO:FULL-PATHNAME.
  PUT UNFORMATTED SKIP(0) "  Drive type         : " cProgressDriveType.
  PUT UNFORMATTED SKIP(0) "  ProPath            : " PROPATH.
  PUT UNFORMATTED SKIP(0) "  Windows version    : " SESSION:WINDOW-SYSTEM " " cSystem + ", " getProcessorArchitecture() "bit".
  PUT UNFORMATTED SKIP(0) "  System memory      : " cMemory.
  PUT UNFORMATTED SKIP(0) "  Display size       : " SESSION:WORK-AREA-WIDTH-PIXELS " x " SESSION:WORK-AREA-HEIGHT-PIXELS.
  PUT UNFORMATTED SKIP(0) "  Logged in as       : " OS-GETENV("username").

  PUT UNFORMATTED SKIP(1) "SESSION INFO".
  PUT UNFORMATTED SKIP(0) "  Program dir        : " gcProgramDir.
  PUT UNFORMATTED SKIP(0) "  Work folder        : " cWorkfolder.
  PUT UNFORMATTED SKIP(0) "  Drive type         : " cDiggerDriveType.
  PUT UNFORMATTED SKIP(0) "  Codepage           : " SESSION:CPINTERNAL.
  PUT UNFORMATTED SKIP(0) "  Character size     : " SESSION:PIXELS-PER-COLUMN " x " SESSION:PIXELS-PER-ROW.
  PUT UNFORMATTED SKIP(0) "  Startup parameters : " SESSION:STARTUP-PARAMETERS.
  PUT UNFORMATTED SKIP(0) "  Temp directory     : " SESSION:TEMP-DIRECTORY.
  PUT UNFORMATTED SKIP(0) "  System alert boxes : " SESSION:SYSTEM-ALERT-BOXES.
  PUT UNFORMATTED SKIP(0) "  Three-D            : " SESSION:THREE-D.
  PUT UNFORMATTED SKIP(0) "  V6Display          : " SESSION:V6DISPLAY.

  PUT UNFORMATTED SKIP(1) "CURRENT FILES".
  FOR EACH bOsFile {&TABLE-SCAN}:
    cExpectedDateTime = getRegistry("DataDigger:files", bOsFile.cFileName).

    DISPLAY
      bOsFile.cFileName  COLUMN-LABEL "File name" AT 3
      bOsFile.iFileSize  COLUMN-LABEL "Size "
      bOsFile.dtModified COLUMN-LABEL "File date"
      cExpectedDateTime  COLUMN-LABEL "Expected" FORMAT "x(20)"
      bOsFile.cStatus    COLUMN-LABEL "Status"
      WITH WIDTH 120 STREAM-IO NO-BOX.
  END.

  /* Delete old .r files */
  PUT UNFORMATTED SKIP(1) "Deleting old .r files".
  FOR EACH bOsFile WHERE bOsFile.cFileType = "r":
    OS-DELETE VALUE(bOsFile.cFullPathname).
  END.

  /* Clean up obsolete source names from the ini */
  cFileList = getRegistry("DataDigger:files", "").
  DO iFile = 1 TO NUM-ENTRIES(cFileList):
    IF SEARCH(gcProgramDir + ENTRY(iFile,cFileList)) = ? THEN
      setRegistry("DataDigger:files", ENTRY(iFile,cFileList), ?).
  END.

  /* Recompile sources */
  PUT UNFORMATTED SKIP(1) "RECOMPILING".

  /* Set the standard dictdb alias to the first Progress db */
  #SetDictDb:
  DO iCount = 1 TO NUM-DBS:
    IF DBTYPE(iCount) = "PROGRESS" THEN
    DO:
      cLogicalDbName = LDBNAME(iCount).
      CREATE ALIAS dictdb FOR DATABASE VALUE(cLogicalDbName).
      LEAVE #SetDictDb.
    END.
  END.

  FOR EACH bOsFile
    WHERE bOsFile.cFileType = "p"
       OR bOsFile.cFileType = "cls"
       OR bOsFile.cFileType = "w":

    MESSAGE "  Compiling:" bOsFile.cFullPathName.
    
    /* Do a strict compile for all DD sources on OpenEdge >= 11.7
     * Exception is myDataDigger.p of the user
     */
    &IF PROVERSION >= '11.7' &THEN 
      IF bOsFile.cFileName <> "myDataDigger.p" THEN
        COMPILE VALUE(bOsFile.cFullPathName) SAVE OPTIONS "require-full-names, require-field-qualifiers".
      ELSE 
        COMPILE VALUE(bOsFile.cFullPathName) SAVE.
    &ELSE
      COMPILE VALUE(bOsFile.cFullPathName) SAVE.    
    &ENDIF
    
    IF COMPILER:ERROR THEN
    DO:
      ASSIGN lCompileError = TRUE.
      IF bOsFile.cFileName <> "myDataDigger.p" THEN lCoreFileError = TRUE.
    END.
  END.

  IF cLogicalDbName <> "" THEN
    DELETE ALIAS dictdb.

  /* Reread dir to catch new date/times of .r files */
  RUN getSourceFiles(INPUT gcProgramDir, OUTPUT TABLE bOsFile).

  /* Save date/time of all files in INI-file */
  FOR EACH bOsFile {&TABLE-SCAN}:
    setRegistry("DataDigger:files", bOsFile.cFileName, bOsFile.cModified).
  END.

  IF NOT lCompileError THEN
    PUT UNFORMATTED SKIP(1) "All files successfully compiled.".
  ELSE
  DO:
    PUT UNFORMATTED
      SKIP(1) "Error while recompiling (see above)" .

    IF SEARCH("myDataDigger.p") <> ? AND SEARCH("myDataDigger.r") = ? THEN
      PUT UNFORMATTED
        SKIP(1) "Apparantly, something is broken in your custom code :)"
        SKIP    "DataDigger will now start without your customizations"
        SKIP    "Fix the errors or rename myDataDigger.p otherwise DD will "
        SKIP    "try to compile it each time it starts.".

    IF lCoreFileError THEN
      PUT UNFORMATTED
        SKIP(1) "There is an error in one of DD's own files."
        SKIP    "If you have not messed with DataDigger, please send this"
        SKIP    "logfile to patrick@tingen.net".

    PUT UNFORMATTED
      SKIP(1) "Sorry for the inconvenience ..."
      SKIP    " ".
  END.

  /* Close the log */
  OUTPUT CLOSE.

  /* Show the window at least some time, otherwise it will flash, which is annoying */
  REPEAT WHILE ETIME < 1000:
    PROCESS EVENTS.
  END.

  SESSION:SET-WAIT-STATE("").

  IF lCompileError THEN
  DO:
    MESSAGE "An error occurred while recompiling. ~n~nPlease check 'DataDigger.log' in the DataDigger directory."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    OS-COMMAND NO-WAIT START VALUE(cLogFile).
  END.

  /* Clean up */
  EMPTY TEMP-TABLE bOsFile.
  DELETE WIDGET hWindow.

END PROCEDURE. /* recompileSelf */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startDiggerLib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startDiggerLib Procedure 
PROCEDURE startDiggerLib :
/* Start DiggerLib if it has not already been started
  */
  DEFINE INPUT PARAMETER plForcedRestart AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hDiggerLib AS HANDLE NO-UNDO.

  /* Call out to see if the lib has been started for this build nr */
  PUBLISH 'DiggerLib' (OUTPUT hDiggerLib).

  /* If we MUST restart (after recompile), or if there is a new version, kill the library */
  IF plForcedRestart THEN
  DO:
    /* Publish a close to all open digger windows. The one that issues
     * this publish will not be closed because we are not subscribed yet.
     */
    PUBLISH 'DataDiggerClose'.
    DELETE PROCEDURE hDiggerLib NO-ERROR.
    hDiggerLib = ?.
  END.

  /* Now, start the lib */
  IF NOT VALID-HANDLE(hDiggerLib) THEN
  DO:
    RUN VALUE(gcProgramDir + 'DataDiggerLib.p') PERSISTENT SET hDiggerLib.
    SESSION:ADD-SUPER-PROCEDURE(hDiggerLib, SEARCH-TARGET).
  END.

END PROCEDURE. /* startDiggerLib */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDriveType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDriveType Procedure 
FUNCTION getDriveType RETURNS CHARACTER ( pcDrive AS CHARACTER ):
  /* Return the type of drive
  */
  DEFINE VARIABLE iType AS INTEGER NO-UNDO.

  RUN GetDriveTypeA(INPUT pcDrive, OUTPUT iType).

  CASE iType:
    WHEN {&DRIVE_UNKNOWN}     THEN RETURN ?.
    WHEN {&DRIVE_NO_ROOT_DIR} THEN RETURN ?.
    WHEN {&DRIVE_REMOVABLE}   THEN RETURN "Removable".
    WHEN {&DRIVE_FIXED}       THEN RETURN "Fixed".
    WHEN {&DRIVE_REMOTE}      THEN RETURN "Remote".
    WHEN {&DRIVE_CDROM}       THEN RETURN "CD-Rom".
    WHEN {&DRIVE_RAMDISK}     THEN RETURN "Ramdisk".
  END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcessorArchitecture) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcessorArchitecture Procedure 
FUNCTION getProcessorArchitecture RETURNS INTEGER():
  /* Return whether the CPU is 32 or 64 bit
  */
  DEFINE VARIABLE ival1 AS INT64 NO-UNDO.
  DEFINE VARIABLE ival2 AS INT64 NO-UNDO.
  DEFINE VARIABLE mdata AS MEMPTR NO-UNDO.

  ival1 = 0x12345678abcdef12.
  SET-POINTER-VALUE(mdata) = ival1.
  ival2 = GET-POINTER-VALUE(mdata).

  RETURN (IF ival1 = ival2 THEN 64 ELSE 32).
END FUNCTION. /* getProcessorArchitecture */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProwin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProwin Procedure 
FUNCTION getProwin RETURNS CHARACTER():
  /* Return the prowin executable name
  */

  DEFINE VARIABLE cProwin64 AS CHARACTER NO-UNDO INIT "prowin.exe".
  DEFINE VARIABLE cProwin32 AS CHARACTER NO-UNDO INIT "prowin32.exe".

  FILE-INFO:FILE-NAME = cProwin64.
  IF FILE-INFO:FULL-PATHNAME > "" THEN RETURN cProwin64.

  FILE-INFO:FILE-NAME = cProwin32.
  IF FILE-INFO:FULL-PATHNAME > "" THEN RETURN cProwin32.

  RETURN "".
END FUNCTION. /* getProwin */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRegistry Procedure 
FUNCTION getRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    ) :
  /* Get a value from DataDigger.ini Not from personal ini!
  */
  DEFINE VARIABLE cRegistryValue AS CHARACTER NO-UNDO.

  USE 'DataDigger'.
  GET-KEY-VALUE SECTION pcSection KEY pcKey VALUE cRegistryValue.
  USE "".

  RETURN cRegistryValue.
END FUNCTION. /* getRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTimeStamp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTimeStamp Procedure 
FUNCTION getTimeStamp RETURNS CHARACTER
  ( INPUT pDateTime AS DATETIME ) :
  /* Return a timestamp in the form "YYYY-MM-DD HH:MM:SS"
  */
  RETURN
    SUBSTITUTE('&1-&2-&3 &4'
              , STRING(YEAR(pDateTime),'9999')
              , STRING(MONTH(pDateTime),'99')
              , STRING(DAY(pDateTime),'99')
              , STRING( INTEGER( TRUNCATE( MTIME( pDateTime ) / 1000, 0 ) ),'HH:MM:SS' )
              ).

END FUNCTION. /* getTimeStamp */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isFolderWritable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isFolderWritable Procedure 
FUNCTION isFolderWritable RETURNS LOGICAL
  ( INPUT pcFolderName AS CHARACTER ):
  /* Check whether a folder is writable (PKB #S000021408)
  */
  DEFINE VARIABLE cTestFolder AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER     NO-UNDO.

  /* Strip trailing slash */
  cTestFolder = RIGHT-TRIM(RIGHT-TRIM(pcFolderName,"~\"),"/") + "~\foo".

  /* Need to create a test folder, but first we want to ensure that it doesn't already exist.
   * Use a counter to name the file if necessary.
   */
  FILE-INFO:FILE-NAME = cTestFolder.

  DO WHILE FILE-INFO:FULL-PATHNAME <> ?:
    iCount = iCount + 1.
    FILE-INFO:FILE-NAME = cTestFolder + STRING(iCount).
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    cTestFolder = cTestFolder + STRING(iCount).
  END.

  OS-CREATE-DIR VALUE(cTestFolder).
  IF OS-ERROR = 0 THEN
  DO:
    /* The file was created so we know the folder is writable.
     * Now delete the test folder and return TRUE.
     */
    OS-DELETE VALUE(cTestFolder).
    RETURN TRUE.
  END.
  ELSE
    RETURN FALSE.

END FUNCTION. /* isFolderWritable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isRecompileNeeded) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isRecompileNeeded Procedure 
FUNCTION isRecompileNeeded RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
  /* Check if any of the files of DataDigger has changed and recompile is needed
  */
  DEFINE VARIABLE cRegistryValue AS CHARACTER NO-UNDO.

  DEFINE BUFFER bOsFile FOR ttOsFile.

  /* If we run a limited version of Progress or if the program dir
   * is not writable, then we simply return that no recompile is needed.
   */
  IF LOOKUP(PROGRESS, 'Full,Query') = 0
    OR NOT isFolderWritable(gcProgramDir) THEN RETURN FALSE.

  /* Read all files from program dir. */
  RUN getSourceFiles(INPUT gcProgramDir, OUTPUT TABLE ttOsFile).

  /* Has any of the source files changed since the last run? */
  FOR EACH bOsFile
    WHERE bOsFile.cFileType = "i"
       OR bOsFile.cFileType = "p"
       OR bOsFile.cFileType = "w"
       OR bOsFile.cFileType = "cls":

    cRegistryValue = getRegistry('DataDigger:files', bOsFile.cFileName).

    IF cRegistryValue = ? THEN bOsFile.cStatus  = 'Status unknown'.
    ELSE
    IF cRegistryValue <> bOsFile.cModified THEN bOsFile.cStatus  = 'File modified'.
  END.

  /* Does every source has an object? */
  FOR EACH bOsFile
    WHERE bOsFile.cFileType = "p"
       OR bOsFile.cFileType = "w"
       OR bOsFile.cFileType = "cls":

    IF NOT CAN-FIND(ttOsFile WHERE ttOsFile.cBaseName = bOsFile.cBaseName
                               AND ttOsFile.cFileType = 'R') THEN bOsFile.cStatus = 'File has no .r'.
  END.

  /* Need to recompile? */
  RETURN CAN-FIND(FIRST bOsFile WHERE bOsFile.cStatus <> '').

END FUNCTION. /* isRecompileNeeded */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRegistry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRegistry Procedure 
FUNCTION setRegistry RETURNS CHARACTER
  ( pcSection AS CHARACTER 
  , pcSetting AS CHARACTER
  , pcValue   AS CHARACTER ) :

  USE 'DataDigger.ini' NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN PUT-KEY-VALUE SECTION pcSection KEY pcSetting VALUE pcValue NO-ERROR.
  USE "".
              
  RETURN "".   /* Function return value. */

END FUNCTION. /* setRegistry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF