&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*  convertSettings.p
 * 
 *  One-time conversions for new versions of DataDigger
 */

DEFINE INPUT PARAMETER piOldVersion AS INTEGER NO-UNDO.

{DataDigger.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

SESSION:SET-WAIT-STATE("general").

RUN VALUE(SUBSTITUTE('ConvertFrom-&1', piOldVersion)) NO-ERROR.
RUN flushRegistry.

SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-convertFrom-19) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-19 Procedure 
PROCEDURE convertFrom-19 :
/* v19 -> 20
*/
  DEFINE BUFFER bfConfig FOR ttConfig.

  /* Obsolete files */
  OS-DELETE VALUE(SEARCH("getNewVersion.p")).
  OS-DELETE VALUE(SEARCH("getNewVersion.r")).
  OS-DELETE VALUE(SEARCH("frLoadMapping.w")).
  OS-DELETE VALUE(SEARCH("frLoadMapping.r")).
  OS-DELETE VALUE(SEARCH("DataDigger.chm")).
  OS-DELETE VALUE(SEARCH("image/default_ReleaseNotes.gif")).
  OS-DELETE VALUE(SEARCH("image/default_FilterCombo.gif")).
  OS-DELETE VALUE(SEARCH("image/default_FilterComboRed.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Star.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tables.gif")).
  OS-DELETE VALUE(SEARCH("image/default_PrevQuery.gif")).
  OS-DELETE VALUE(SEARCH("image/default_NextQuery.gif")).
  OS-DELETE VALUE(SEARCH("image/default_ViewAsList.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Pinned.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Unpinned.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Undock.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Dock.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Favorites_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Favorites_Inactive.gif")).

  /* Erase widths of table browse */
  setRegistry('DataDigger','ColumnWidth:cTableName',?).
  setRegistry('DataDigger','ColumnWidth:cDatabase',?).
  setRegistry('DataDigger','ColumnWidth:iNumQueries',?).

  /* Setting for last active page not used anymore */
  setRegistry("DataDigger", "ActivePage",?).

  /* Remove last used table from settings */
  FOR EACH bfConfig WHERE bfConfig.cSection BEGINS 'DB:' BREAK BY bfConfig.cSection:
    IF FIRST-OF(bfConfig.cSection) THEN setRegistry(bfConfig.cSection,'table',?).
  END.

  /* Updates (are in the general .ini file */
  USE 'DataDigger.ini' NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
  DO:
    PUT-KEY-VALUE SECTION "DataDigger:Update" KEY "UpdateCheck"     VALUE ? NO-ERROR.
    PUT-KEY-VALUE SECTION "DataDigger:Update" KEY "UpdateUrl"       VALUE ? NO-ERROR.
    PUT-KEY-VALUE SECTION "DataDigger:Update" KEY "UpdateChannel"   VALUE ? NO-ERROR.
    PUT-KEY-VALUE SECTION "DataDigger:Update" KEY "UpdateLastCheck" VALUE ? NO-ERROR.
    PUT-KEY-VALUE SECTION "DataDigger:Update" KEY ""                VALUE ? NO-ERROR.
  END.
  USE "".

END PROCEDURE. /* 19 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convertFrom-20) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-20 Procedure 
PROCEDURE convertFrom-20 :
/* v20 -> 21
*/
  OS-DELETE VALUE(SEARCH("wEdit.wrx")).

END PROCEDURE. /* 20 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convertFrom-21) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-21 Procedure 
PROCEDURE convertFrom-21 :
/* v21 -> 22
*/
 
 /* No conversions needed */

END PROCEDURE. /* 21 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convertFrom-22) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-22 Procedure 
PROCEDURE convertFrom-22 :
/* v22 -> 23
*/

  OS-DELETE VALUE(SEARCH("dAbout.w")).
  OS-DELETE VALUE(SEARCH("dAbout.r")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_About_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_About_Inactive.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Changes_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Changes_Inactive.gif")).

END PROCEDURE. /* 22 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convertFrom-23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-23 Procedure 
PROCEDURE convertFrom-23 :
/* v23 -> 24
*/
  DEFINE VARIABLE cValue AS CHARACTER   NO-UNDO.
  DEFINE BUFFER bfConfig FOR ttConfig.

  /* Settings removed */
  setRegistry("DataDigger", "AddDataColumnForRecid", ?).
  setRegistry("DataDigger", "AddDataColumnForRowid", ?).
  setRegistry("DataDigger:Cache","Settings",?).
  setRegistry("DataDigger:Colors", "QueryCounter:FG", ?).
  setRegistry("DataDigger:Colors", "QueryInfo:FG", ?).

  /* DumpDf settings now in their own section */
  setRegistry("DataDigger", "DumpDF:dir" , ?).
  setRegistry("DataDigger", "DumpDF:open", ?).

  /* Answer to confirm delete should not be saved when NO or CANCEL */
  setRegistry("DataDigger:Help", "ConfirmDelete:hidden", ?).

  /* Table browse is now slightly wider, so erase old column widths */
  setRegistry("DataDigger", "ColumnWidth:cTableName" , ?).
  setRegistry("DataDigger", "ColumnWidth:cDatabase"  , ?).
  setRegistry("DataDigger", "ColumnWidth:iNumQueries", ?).

  /* dHint.w is not used */
  OS-DELETE VALUE(SEARCH("dHint.w")).
  OS-DELETE VALUE(SEARCH("dHint.r")).

  /* A typo in previous versions prevented these from deletion */
  /* DD19 */
  OS-DELETE VALUE(SEARCH("getNewVersion.p")).
  OS-DELETE VALUE(SEARCH("getNewVersion.r")).
  OS-DELETE VALUE(SEARCH("frLoadMapping.w")).
  OS-DELETE VALUE(SEARCH("frLoadMapping.r")).
  OS-DELETE VALUE(SEARCH("DataDigger.chm")).
  OS-DELETE VALUE(SEARCH("image/default_ReleaseNotes.gif")).
  OS-DELETE VALUE(SEARCH("image/default_FilterCombo.gif")).
  OS-DELETE VALUE(SEARCH("image/default_FilterComboRed.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Star.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tables.gif")).
  OS-DELETE VALUE(SEARCH("image/default_PrevQuery.gif")).
  OS-DELETE VALUE(SEARCH("image/default_NextQuery.gif")).
  OS-DELETE VALUE(SEARCH("image/default_ViewAsList.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Pinned.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Unpinned.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Undock.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Dock.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Favorites_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Favorites_Inactive.gif")).

  /* DD20 */
  OS-DELETE VALUE(SEARCH("wEdit.wrx")).

  /* DD22 */
  OS-DELETE VALUE(SEARCH("dAbout.w")).
  OS-DELETE VALUE(SEARCH("dAbout.r")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_About_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_About_Inactive.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Changes_Active.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Tab_Changes_Inactive.gif")).

  /* Obsolete stuff */
  OS-DELETE VALUE(SEARCH("image/default_ViewAsEditor.gif.jpg")). /* actually still from DD19 */
  OS-DELETE VALUE(SEARCH("DataDiggerAbout.txt")).
  OS-DELETE VALUE(SEARCH("wAbout.wrx")).
  OS-DELETE VALUE(SEARCH("dChooseFont.w")).  /* replaced with adecomm/_chsfont.p */
  OS-DELETE VALUE(SEARCH("dChooseColor.w")). /* replaced with adecomm/_chscolr.p */
  OS-DELETE VALUE(SEARCH("image/default_AboutTitle.gif")).
  OS-DELETE VALUE(SEARCH("image/default_AboutTitle2.gif")).
  OS-DELETE VALUE(SEARCH("image/default_AboutTitle3.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Paddle.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Download.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Download_ins.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Upload.gif")).
  OS-DELETE VALUE(SEARCH("image/default_ResizeHor.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Ok.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Back.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Compare.gif")).
  OS-DELETE VALUE(SEARCH("image/default_DataDigger16x16.gif")).
  OS-DELETE VALUE(SEARCH("image/default_Forward.gif")).

  /* Setting '<PROGDIR>' renamed to '<WORKDIR>' */
  cValue = getRegistry("DataDigger:Backup", "BackupDir").
  setRegistry("DataDigger:Backup", "BackupDir", REPLACE(cValue,'<PROGDIR>', '<WORKDIR>')).

  cValue = getRegistry("DataDigger:Backup", "BackupFileTemplate").
  setRegistry("DataDigger:Backup", "BackupFileTemplate", REPLACE(cValue,'<PROGDIR>', '<WORKDIR>')).

  cValue = getRegistry("DumpAndLoad", "DumpDir").
  setRegistry("DumpAndLoad", "DumpDir", REPLACE(cValue,'<PROGDIR>', '<WORKDIR>')).

  cValue = getRegistry("DumpAndLoad", "DumpFileTemplate").
  setRegistry("DumpAndLoad", "DumpFileTemplate", REPLACE(cValue,'<PROGDIR>', '<WORKDIR>')).

  /* Remove usage info, except for numUsed */
  RUN getRegistryTable(OUTPUT TABLE bfConfig).
  FOR EACH bfConfig WHERE bfConfig.cSection = "DataDigger:Hints":
    IF NOT bfConfig.cSetting MATCHES '*' THEN
      setRegistry(bfConfig.cSection,bfConfig.cSetting,?).
  END.

  /* Move old favourites to group 'myFavourites' */
  FOR EACH bfConfig WHERE bfConfig.cSection BEGINS 'DB:'
                      AND bfConfig.cSetting MATCHES '*:favourite':
    RUN setFavourite(ENTRY(2,bfConfig.cSection,':'), ENTRY(1,bfConfig.cSetting,':'), 'myFavourites', TRUE).
    setRegistry(bfConfig.cSection,bfConfig.cSetting,?).
  END.
  EMPTY TEMP-TABLE bfConfig.

  /* Old setting got re-created due to bug in DD23 */
  setRegistry("DumpAndLoad", "DumpFileDir", ?).

END PROCEDURE. /* 23 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convertFrom-24) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convertFrom-24 Procedure 
PROCEDURE convertFrom-24 :
/* v24 -> 25
*/
  DEFINE BUFFER bfConfig FOR ttConfig.
  DEFINE BUFFER btFavGroup FOR ttFavGroup.

  DEFINE VARIABLE i          AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTableName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cGroupName AS CHARACTER   NO-UNDO.

  /* Get all settings */
  RUN getRegistryTable(OUTPUT TABLE bfConfig).

  /* Convert usage info */
  FOR EACH bfConfig WHERE bfConfig.cSection = "DataDigger:Usage":
    IF NOT bfConfig.cSetting MATCHES '*' THEN
    DO:
      setRegistry("DataDigger:Hints", bfConfig.cSetting, "yes").
      setRegistry(bfConfig.cSection, bfConfig.cSetting,?).
    END.
  END.

  /* Convert favourites */
  EMPTY TEMP-TABLE ttFavGroup. 
  FOR EACH bfConfig WHERE bfConfig.cSetting MATCHES '*:Favourites*':

    cTableName = ENTRY(1,bfConfig.cSetting,':').

    /* Customer:Favourites=myFavourites,Cust-stuff */
    DO i = 1 TO NUM-ENTRIES(bfConfig.cValue):

      cGroupName = ENTRY(i,bfConfig.cValue).

      FIND btFavGroup WHERE btFavGroup.cGroup = cGroupName NO-ERROR.
      IF NOT AVAILABLE btFavGroup THEN CREATE btFavGroup.
      ASSIGN btFavGroup.cGroup = cGroupName.

      IF LOOKUP(cTableName, btFavGroup.cTables) = 0 THEN 
        ASSIGN btFavGroup.cTables = TRIM(SUBSTITUTE('&1,&2', btFavGroup.cTables, cTableName),',').
    END.

    setRegistry(bfConfig.cSection, bfConfig.cSetting, ?). /* remove old setting */
  END.

  /* [DataDigger:Favourites]
   * myFavourites=Customer,Order-Line,Order,Salesrep
   * Cust-stuff=Customer,State
   */
  FOR EACH btFavGroup:
    setRegistry('DataDigger:Favourites', btFavGroup.cGroup, btFavGroup.cTables).
  END.

  /* Obsolete files */
  OS-DELETE VALUE(SEARCH("dEditGroup.wrx")).

END PROCEDURE. /* 24 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

