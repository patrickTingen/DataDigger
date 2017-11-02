&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include
/*------------------------------------------------------------------------

  Name : DataDigger.i
  Desc : Definitions / forward defs for DataDigger programs

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE version {version.i}
&GLOBAL-DEFINE edition Easter Egg
&GLOBAL-DEFINE build {build.i}

&GLOBAL-DEFINE QUERYSEP CHR(1, SESSION:CPINTERNAL, "UTF-8")
&GLOBAL-DEFINE timerStart PUBLISH "timerCommand" ("start", ENTRY(1,PROGRAM-NAME(1)," ")).
&GLOBAL-DEFINE timerStop  PUBLISH "timerCommand" ("stop" , ENTRY(1,PROGRAM-NAME(1)," ")).

/* Constant values for update channels */
&GLOBAL-DEFINE CHECK-MANUAL 0
&GLOBAL-DEFINE CHECK-STABLE 1
&GLOBAL-DEFINE CHECK-BETA   2

/* Constant for collecting statistics */
&GLOBAL-DEFINE PINGBACKURL https://goo.gl/24deK3

DEFINE VARIABLE gcThisProcedure AS CHARACTER   NO-UNDO.

/* TT for field data to link DataDiggers to each other */
DEFINE TEMP-TABLE ttLinkInfo NO-UNDO RCODE-INFORMATION
  FIELD cField AS CHARACTER
  FIELD cValue AS CHARACTER
  INDEX idxPrim IS PRIMARY cField
  .

/* TT for the tables of a db */
DEFINE TEMP-TABLE ttTable NO-UNDO RCODE-INFORMATION
  FIELD cDatabase     AS CHARACTER LABEL "DB"        FORMAT "X(12)"
  FIELD cTableName    AS CHARACTER LABEL "Table"     FORMAT "X(32)"
  FIELD cCrc          AS CHARACTER LABEL "CRC"
  FIELD cCacheId      AS CHARACTER LABEL "CacheId"
  FIELD lShowInList   AS LOGICAL   LABEL "" /* for getTablesWithField */
  FIELD cTableDesc    AS CHARACTER LABEL "Desc"
  FIELD cFields       AS CHARACTER LABEL "Fields"
  FIELD lHidden       AS LOGICAL   LABEL ""
  FIELD lFrozen       AS LOGICAL   LABEL ""
  FIELD iNumQueries   AS INTEGER   LABEL "#"         FORMAT "zzzzz"
  FIELD tLastUsed     AS DATETIME  LABEL "Last Used" FORMAT "99/99/9999 HH:MM:SS"
  FIELD lFavourite    AS LOGICAL   LABEL ""
  FIELD lCached       AS LOGICAL   LABEL "" /* for preCaching */
  FIELD iFileNumber   AS INTEGER   LABEL "_File-Number"
  FIELD cCategory     AS CHARACTER LABEL "Category"
  INDEX idxPrim IS PRIMARY cDatabase cTableName
  INDEX idxSec cTableName
  .
DEFINE TEMP-TABLE ttTableXml NO-UNDO XML-NODE-NAME "ttTable" LIKE ttTable RCODE-INFORMATION .

/* TT for the saved queries of a table */
DEFINE TEMP-TABLE ttQuery NO-UNDO RCODE-INFORMATION
  FIELD cDatabase AS CHARACTER
  FIELD cTable    AS CHARACTER
  FIELD iQueryNr  AS INTEGER
  FIELD cQueryTxt AS CHARACTER
  INDEX idxQueryPrim IS PRIMARY iQueryNr
  INDEX idxTable cDatabase cTable
  .

/* TT for the fields of a table */
DEFINE TEMP-TABLE ttField NO-UNDO RCODE-INFORMATION
  FIELD cTableCacheId AS CHARACTER /* unique name for db / table / table-crc */
  FIELD cDatabase     AS CHARACTER
  FIELD cTableName    AS CHARACTER
  FIELD cFieldName    AS CHARACTER                   LABEL "Name"      FORMAT "X(40)"

  FIELD cFullName     AS CHARACTER                   LABEL "Name"      FORMAT "X(40)"    /* fieldname incl extent     */
  FIELD cXmlNodeName  AS CHARACTER                   LABEL "Xml Name"  FORMAT "X(40)"    /* name for usage in XML     */
  FIELD iOrder        AS DECIMAL                     LABEL "Order"     FORMAT ">>>>>9"   /* user defined order        */
  FIELD lShow         AS LOGICAL                     LABEL ""                            /* toggle box                */
  FIELD cDataType     AS CHARACTER                   LABEL "Type"      FORMAT "X(16)"
  FIELD cInitial      AS CHARACTER                   LABEL "Initial"                     /* initial value from dict   */
  FIELD cFormat       AS CHARACTER                   LABEL "Format"    FORMAT "X(80)"    /* user defined format       */
  FIELD cFormatOrg    AS CHARACTER                   LABEL "Format"                      /* original format           */
  FIELD cLabel        AS CHARACTER                   LABEL "Label"     FORMAT "X(24)"
  FIELD iOrderOrg     AS DECIMAL                                                         /* original order            */
  FIELD iExtent       AS INTEGER                     LABEL "Extent"    FORMAT ">>>>9"
  FIELD lPrimary      AS LOGICAL                     LABEL "Prim"                        /* part of prim index?       */
  FIELD lMandatory    AS LOGICAL                     LABEL "Man"                         /* mandatory?                */
  FIELD lUniqueIdx    AS LOGICAL                     LABEL "Uni"                         /* part of unique index?     */

  /* New fields as per v19 */
  FIELD cColLabel     AS CHARACTER                   LABEL "Column Label" FORMAT "x(24)"
  FIELD iDecimals     AS INTEGER                     LABEL "Decimals"     FORMAT ">>9"
  FIELD iFieldRpos    AS INTEGER                     LABEL "R-pos"        FORMAT ">>>>9"
  FIELD cValExp       AS CHARACTER                   LABEL "Val Expr"     FORMAT "x(80)"
  FIELD cValMsg       AS CHARACTER                   LABEL "Val Message"  FORMAT "x(80)"
  FIELD cHelp         AS CHARACTER                   LABEL "Help msg"     FORMAT "x(80)"
  FIELD cDesc         AS CHARACTER                   LABEL "Description"  FORMAT "x(80)"
  FIELD cViewAs       AS CHARACTER                   LABEL "View-As"      FORMAT "x(40)"

  /* These fields must be moved to ttColumn */
/*  FIELD lDataField    AS LOGICAL                                                         /* show in data browse */ */
  FIELD cFilterValue  AS CHARACTER
  FIELD cNewValue     AS CHARACTER                   LABEL "New value" FORMAT "x(256)"
  FIELD cOldValue     AS CHARACTER                   LABEL "Old value" FORMAT "x(256)"
  FIELD hColumn       AS HANDLE

  INDEX idxPrim IS PRIMARY cTableCacheId
  INDEX idxName cFieldName
  INDEX idxOrder iOrder /* for fields browse */
  INDEX idxSec   cTableCacheId cFieldName iOrder
  .

/* TT for the fields of a table with extent fields extracted
 *
 * Relations:  [ttTable] -< [ttField] -< [ttColumn]
 * For non-extents the relation between ttField and ttColumn
 * will be 1:1. For extent fields it will be 1:n
 */
DEFINE TEMP-TABLE ttColumn NO-UNDO RCODE-INFORMATION
  FIELD cTableCacheId AS CHARACTER /* unique name for db / table / table-crc */
  FIELD cDatabase     AS CHARACTER
  FIELD cTableName    AS CHARACTER
  FIELD cFieldName    AS CHARACTER          LABEL "Name"      FORMAT "X(40)"
  FIELD iExtent       AS INTEGER            LABEL "Extent"    FORMAT ">>>>9"

  FIELD cFullName     AS CHARACTER          LABEL "Name"      FORMAT "X(40)"    /* fieldname incl extent     */
/*  FIELD lDataField    AS LOGICAL            /* show in data browse */ */
  FIELD cFilterValue  AS CHARACTER          /* for setting shadow color */
  FIELD cNewValue     AS CHARACTER          LABEL "New value" FORMAT "X(256)" /* for wEdit */
  FIELD cOldValue     AS CHARACTER          LABEL "Old value" FORMAT "X(256)" /* for wEdit */
  FIELD lShow         AS LOGICAL                                              /* for wEdit */
  FIELD iOrder        AS DECIMAL            LABEL "Order"     FORMAT ">>>>>9" /* user defined order        */
  FIELD cLabel        AS CHARACTER          LABEL "Label"     FORMAT "X(24)"
  FIELD iColumnNr     AS INTEGER            /* order in the databrowse */
  FIELD hColumn       AS HANDLE             /* handle to the column in the databrowse */
  FIELD hFilter       AS HANDLE             /* handle to the filter on top of the databrowse */
  INDEX idxPrim IS PRIMARY cTableCacheId
  INDEX idxField cFieldName
  INDEX idxColNr iColumnNr
  INDEX idxSort  cTableCacheId cFieldName iColumnNr
  INDEX idxTable cDatabase cTablename
  .

/* TTs Used for preCaching */
DEFINE TEMP-TABLE ttFieldCache NO-UNDO LIKE ttField RCODE-INFORMATION
  INDEX idxTable IS PRIMARY cTableName
  .
DEFINE TEMP-TABLE ttColumnCache NO-UNDO LIKE ttColumn RCODE-INFORMATION
  .

DEFINE DATASET dsFields FOR ttField, ttColumn.
DEFINE DATASET dsFieldCache FOR ttFieldCache, ttColumnCache.

/* TT for the indexfields of a table */
DEFINE TEMP-TABLE ttIndex NO-UNDO RCODE-INFORMATION
  FIELD cIndexName   AS CHARACTER          LABEL "Name"        FORMAT "x(20)"
  FIELD cIndexFlags  AS CHARACTER          LABEL "Flags"       FORMAT "x(14)"
  FIELD cIndexFields AS CHARACTER          LABEL "Fields"      FORMAT "x(160)"
  FIELD cFieldList   AS CHARACTER          LABEL "Field List"  FORMAT "x(80)"
  FIELD lIndexActive AS LOGICAL
  .

/* TT for counting windowLocks  (WindowsUpdateLock) */
DEFINE TEMP-TABLE ttWindowLock NO-UNDO RCODE-INFORMATION
  FIELD hWindow      AS HANDLE
  FIELD iLockCounter AS INTEGER
  INDEX idxPrim IS PRIMARY hWindow
  .

/* TT for filters on top of data browse */
DEFINE TEMP-TABLE ttFilter NO-UNDO RCODE-INFORMATION
  FIELD cFieldName AS CHARACTER
  FIELD hFilter    AS HANDLE
  FIELD hColumn    AS HANDLE
  FIELD hBrowse    AS HANDLE
  FIELD lVisible   AS LOGICAL
  FIELD lModified  AS LOGICAL
  INDEX idxBrowse hBrowse
  INDEX idxField  cFieldName
  INDEX idxFilter hFilter
  .

/* TT for filter on database tables */
DEFINE TEMP-TABLE ttTableFilter NO-UNDO RCODE-INFORMATION
  FIELD cTableNameShow  AS CHARACTER
  FIELD cTableNameHide  AS CHARACTER
  FIELD cTableFieldShow AS CHARACTER
  FIELD cTableFieldHide AS CHARACTER
  FIELD lShowNormal     AS LOGICAL INITIAL TRUE
  FIELD lShowSchema     AS LOGICAL
  FIELD lShowVst        AS LOGICAL
  FIELD lShowSql        AS LOGICAL
  FIELD lShowOther      AS LOGICAL
  FIELD lShowHidden     AS LOGICAL
  FIELD lShowFrozen     AS LOGICAL
  .

/* TT For currently connected databases */
DEFINE TEMP-TABLE ttDatabase NO-UNDO RCODE-INFORMATION
  FIELD cLogicalName  AS CHARACTER COLUMN-LABEL "Logical Name" FORMAT "x(20)"
  FIELD cSection      AS CHARACTER COLUMN-LABEL "Section"      FORMAT "x(20)"
  FIELD cCacheStamp   AS CHARACTER COLUMN-LABEL "CacheStamp"   FORMAT "x(24)"
  INDEX idxPrim IS PRIMARY UNIQUE cLogicalName
  .

/* TT for favourites */
DEFINE TEMP-TABLE ttConnection NO-UNDO RCODE-INFORMATION
  FIELD iConnectionNr AS INTEGER
  FIELD cLogicalName  AS CHARACTER COLUMN-LABEL "Logical Name" FORMAT "x(20)"
  FIELD cDescription  AS CHARACTER COLUMN-LABEL "Description"  FORMAT "x(28)"
  FIELD cDatabaseName AS CHARACTER COLUMN-LABEL "Database"     FORMAT "x(20)"
  FIELD cParameters   AS CHARACTER
  FIELD lConnected    AS LOGICAL   COLUMN-LABEL "Con"
  FIELD cSection      AS CHARACTER COLUMN-LABEL "Section"      FORMAT "x(20)"
  INDEX idxPrim IS PRIMARY UNIQUE iConnectionNr
  .

/* TT for Query Tester */
DEFINE TEMP-TABLE ttTestQuery NO-UNDO RCODE-INFORMATION
  FIELD iId        AS INTEGER LABEL "Seq" COLUMN-LABEL "Seq" FORMAT ">,>>9"
  FIELD cProgName  AS CHARACTER
  FIELD cQueryTxt  AS CHARACTER
  FIELD cIndexInfo AS CHARACTER
  INDEX idxId IS PRIMARY UNIQUE iId DESCENDING
  .

/* TT for ini-file settings */
DEFINE TEMP-TABLE ttConfig NO-UNDO RCODE-INFORMATION
  FIELD cSection AS CHARACTER
  FIELD cSetting AS CHARACTER
  FIELD cValue   AS CHARACTER
  INDEX idxPrim IS PRIMARY cSection cSetting.


/* TT for sorting options in user query */
DEFINE TEMP-TABLE ttQuerySort NO-UNDO RCODE-INFORMATION
  FIELD iGroup     AS INTEGER /* 1:query, 2:browse */
  FIELD iSortNr    AS INTEGER
  FIELD cSortField AS CHARACTER
  FIELD lAscending AS LOGICAL
  FIELD iExt       AS INTEGER
  INDEX iPrim IS PRIMARY iGroup iSortNr
  .

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
         HEIGHT             = 6
         WIDTH              = 35.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include


/* ***************************  Main Block  *************************** */

/* Global-defines for testing purposes */
&if defined(invar) = 0 &then
   &if defined(UIB_is_Running) ne 0  &then
      &global-define invar  variable
      &global-define iovar  variable
      &global-define outvar variable
   &else
      &global-define invar   input parameter
      &global-define iovar   input-output parameter
      &global-define outvar  output parameter
   &endif
&endif

/* Forward defs */
FUNCTION addConnection RETURNS LOGICAL
  ( pcDatabase AS CHARACTER
  , pcSection  AS CHARACTER ) IN SUPER.

FUNCTION formatQueryString RETURNS CHARACTER
  ( INPUT pcQueryString AS CHARACTER
  , INPUT plExpanded    AS LOGICAL ) IN SUPER.

FUNCTION getDatabaseList RETURNS CHARACTER IN SUPER.

FUNCTION getEscapedData RETURNS CHARACTER
  ( INPUT pcTarget AS CHARACTER
  , INPUT pcString AS CHARACTER ) IN SUPER.

FUNCTION getColor RETURNS INTEGER
  ( INPUT pcName AS CHARACTER ) IN SUPER.

FUNCTION getColumnLabel RETURNS CHARACTER
  ( INPUT phFieldBuffer AS HANDLE ) IN SUPER.

FUNCTION getFont RETURNS INTEGER
  ( pcFontName AS CHARACTER ) IN SUPER.

FUNCTION getImagePath RETURNS CHARACTER
  ( pcImage AS CHARACTER ) IN SUPER.

FUNCTION getIndexFields RETURNS CHARACTER
  ( INPUT pcDatabaseName AS CHARACTER
  , INPUT pcTableName    AS CHARACTER
  , INPUT pcFlags        AS CHARACTER
  ) IN SUPER.

FUNCTION getKeyList      RETURNS CHARACTER IN SUPER.

FUNCTION getLinkInfo     RETURNS CHARACTER
  ( INPUT pcFieldName AS CHARACTER
  ) IN SUPER.

FUNCTION getMaxLength    RETURNS INTEGER
  ( pcSection AS CHARACTER
  ) IN SUPER.

FUNCTION getOsErrorDesc RETURNS CHARACTER
  ( INPUT piOsError AS INTEGER
  ) IN SUPER.

FUNCTION getProgramDir RETURNS CHARACTER IN SUPER.

FUNCTION getQuery RETURNS CHARACTER
  ( INPUT pcDatabase AS CHARACTER
  , INPUT pcTable    AS CHARACTER
  , INPUT piQuery    AS INTEGER
  ) IN SUPER.

FUNCTION getReadableQuery RETURNS CHARACTER
  ( INPUT pcQuery AS CHARACTER
  ) IN SUPER.

FUNCTION getRegistry RETURNS CHARACTER
  ( pcSection AS CHARACTER
  , pcKey     AS CHARACTER
  ) IN SUPER.

FUNCTION getStackSize RETURNS INTEGER
  () IN SUPER.

FUNCTION getTableList RETURNS CHARACTER
  ( INPUT  pcDatabaseFilter   AS CHARACTER
  , INPUT  pcTableFilter      AS CHARACTER
  ) IN SUPER.

FUNCTION getUsername RETURNS CHARACTER IN SUPER.

FUNCTION getWidgetUnderMouse RETURNS HANDLE
  ( INPUT phWidget AS HANDLE ) IN SUPER.

FUNCTION isDefaultFontsChanged RETURNS LOGICAL IN SUPER.

FUNCTION isFileLocked RETURNS LOGICAL
  ( pcFileName AS CHARACTER ) IN SUPER.

FUNCTION isBrowseChanged RETURNS LOGICAL
  ( INPUT phWidget AS HANDLE ) IN SUPER.

FUNCTION isMouseOver RETURNS LOGICAL
  ( INPUT phWidget AS HANDLE ) IN SUPER.

FUNCTION isTableFilterUsed RETURNS LOGICAL
  ( INPUT TABLE ttTableFilter )  IN SUPER.

FUNCTION isWidgetChanged RETURNS LOGICAL
  ( INPUT phWidget AS HANDLE ) IN SUPER.

FUNCTION readFile RETURNS LONGCHAR
  ( INPUT pcFilename AS CHARACTER) IN SUPER.

FUNCTION removeConnection RETURNS LOGICAL
  ( pcDatabase AS CHARACTER ) IN SUPER.

FUNCTION resolveOsVars RETURNS CHARACTER
  ( pcString AS CHARACTER ) IN SUPER.

FUNCTION setLinkInfo RETURNS LOGICAL
  ( INPUT pcFieldName AS CHARACTER
  , INPUT pcValue     AS CHARACTER
  ) IN SUPER.

FUNCTION setRegistry RETURNS CHARACTER
    ( pcSection AS CHARACTER
    , pcKey     AS CHARACTER
    , pcValue   AS CHARACTER
    ) IN SUPER.

FUNCTION isValidCodePage RETURNS LOGICAL
  (pcCodepage AS CHARACTER) IN SUPER.

/* Initialize */
gcThisProcedure = THIS-PROCEDURE:FILE-NAME.
gcThisProcedure = ENTRY(NUM-ENTRIES(gcThisProcedure,"\"),gcThisProcedure,"\").
gcThisProcedure = ENTRY(1,gcThisProcedure,".").

SUBSCRIBE TO gcThisProcedure ANYWHERE RUN-PROCEDURE "getProcHandle".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getProcHandle Include
PROCEDURE getProcHandle :
/*
 * Name : getProcHandle
 * Desc : Return the handle of the procedure this include is in
 */
DEFINE OUTPUT PARAMETER phHandle AS HANDLE NO-UNDO.
phHandle = THIS-PROCEDURE:HANDLE.

END PROCEDURE. /* getProcHandle */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
