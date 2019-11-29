/*------------------------------------------------------------------------
  Name : getSchema.p
  Desc : Get the schema of the dictdb database

  Notes:
    This is in a separate .p because the alias is set for the dictdb.
    The reason this is done is because reading the schema statically
    is much faster than reading it via a dynamic query (factor 4 or 5)

  Input parameter ttTable should be passed BY-REFERENCE
  ----------------------------------------------------------------------*/

{DataDigger.i}

DEFINE INPUT PARAMETER TABLE FOR ttTable.

DEFINE BUFFER bDb    FOR dictdb._Db.
DEFINE BUFFER bFile  FOR dictdb._File.
DEFINE BUFFER bField FOR dictdb._Field.
DEFINE BUFFER bTable FOR ttTable.

FIND FIRST bTable NO-ERROR.
FIND FIRST bDb NO-LOCK NO-ERROR.

FOR EACH bDb   NO-LOCK
  , EACH bFile NO-LOCK
   WHERE bFile._Db-recid    = RECID(bDb)
     AND bFile._File-Number < 32768
     AND (IF bDb._Db-slave THEN bFile._For-Type = 'TABLE' ELSE TRUE)
  :

  CREATE bTable.
  ASSIGN
    bTable.cSchemaHolder = (IF bDb._Db-slave THEN LDBNAME('dictdb') ELSE '')            /* [JAG 01-11-2019] */
    bTable.cDatabase   = (IF bDb._Db-slave THEN bDb._Db-name ELSE LDBNAME('dictdb'))
    bTable.cTableName  = bFile._file-name
    bTable.cTableDesc  = TRIM( (IF bFile._file-label <> ? AND bFile._file-label <> '' THEN bFile._file-label + ', ' ELSE '')
                             + (IF bFile._desc <> ? AND bFile._desc <> '' AND bFile._desc <> bFile._file-label THEN bFile._desc ELSE '')
                             , ' ,')
    bTable.cTableLabel = bFile._file-label
    bTable.lHidden     = bFile._hidden
    bTable.lFrozen     = bFile._frozen
    bTable.cCrc        = STRING(bFile._crc)
    bTable.cCacheId    = SUBSTITUTE('&1.&2.&3', bTable.cDatabase, bFile._file-name, bFile._crc)
    bTable.iFileNumber = bFile._file-number
    .

  /* Based on table name and -number, return the category for a table
   *
   * Application tables   : _file-number > 0   AND _file-number < 32000
   * Schema tables        : _file-number > -80 AND _file-number < 0
   * Virtual system tables: _file-number < -16384
   * SQL catalog tables   : _file-name BEGINS "_sys"
   * Other tables         : _file-number >= -16384 AND _file-number <= -80
   */
  IF bFile._file-name BEGINS '_sys'                                   THEN bTable.cCategory = 'SQL'.
  ELSE IF bFile._file-number > 0       AND bFile._file-number < 32000 THEN bTable.cCategory = 'Normal'.
  ELSE IF bFile._file-number > -80     AND bFile._file-number < 0     THEN bTable.cCategory = 'Schema'.
  ELSE IF bFile._file-number >= -16384 AND bFile._file-number <= -80  THEN bTable.cCategory = 'Other'.
  ELSE IF bFile._file-number < -16384                                 THEN bTable.cCategory = 'VST'.

  FOR EACH bField 
    WHERE bField._File-recid = RECID(bFile) NO-LOCK:
    bTable.cFields = bTable.cFields + ',' + bField._Field-name.
  END.
  bTable.cFields = TRIM(bTable.cFields,',').
END.
