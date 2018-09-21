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

FIND FIRST ttTable NO-ERROR.
FIND FIRST dictdb._Db NO-ERROR.

FOR EACH dictdb._File NO-LOCK
  WHERE dictdb._File._File-Number < 32768
    AND (IF dictdb._Db._Db-slave THEN dictdb._File._For-Type = 'TABLE' ELSE TRUE)
  :

  CREATE ttTable.
  ASSIGN
    ttTable.cDatabase   = (IF dictdb._Db._Db-slave THEN dictdb._Db._Db-name ELSE LDBNAME('dictdb'))
    ttTable.cTableName  = dictdb._File._file-name                                            
    ttTable.cTableDesc  = (IF dictdb._File._file-label <> ? AND dictdb._File._file-label <> '' THEN dictdb._File._file-label ELSE dictdb._File._desc)
    ttTable.lHidden     = dictdb._File._hidden
    ttTable.lFrozen     = dictdb._File._frozen 
    ttTable.cCrc        = STRING(dictdb._File._crc)     
    ttTable.cCacheId    = SUBSTITUTE('&1.&2.&3', ttTable.cDatabase, dictdb._File._file-name, dictdb._File._crc)
    ttTable.iFileNumber = dictdb._File._file-number
    .

  /* Based on table name and -number, return the category for a table
   *
   * Application tables   : _file-number > 0   AND _file-number < 32000
   * Schema tables        : _file-number > -80 AND _file-number < 0
   * Virtual system tables: _file-number < -16384
   * SQL catalog tables   : _file-name BEGINS "_sys"
   * Other tables         : _file-number >= -16384 AND _file-number <= -80
   */
  IF dictdb._File._file-name BEGINS '_sys'                                          THEN ttTable.cCategory = 'SQL'.
  ELSE IF dictdb._File._file-number > 0       AND dictdb._File._file-number < 32000 THEN ttTable.cCategory = 'Normal'.
  ELSE IF dictdb._File._file-number > -80     AND dictdb._File._file-number < 0     THEN ttTable.cCategory = 'Schema'.
  ELSE IF dictdb._File._file-number >= -16384 AND dictdb._File._file-number <= -80  THEN ttTable.cCategory = 'Other'.
  ELSE IF dictdb._File._file-number < -16384                                        THEN ttTable.cCategory = 'VST'.

  FOR EACH dictdb._Field OF dictdb._File NO-LOCK:
    ttTable.cFields = SUBSTITUTE('&1,&2',ttTable.cFields, dictdb._Field._Field-name).
  END.
  ttTable.cFields = TRIM(ttTable.cFields,',').
END.
