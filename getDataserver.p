/*------------------------------------------------------------------------

  Name: getDataserver.p
  Desc: Fetch dataserver info and connect 

------------------------------------------------------------------------*/

{ DataDigger.i }

/* If set to YES, then support >1 dataservers per schemaholder */
&scoped-define support-more-dataservers yes

DEFINE INPUT        PARAMETER pcLDbNameSchema AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piDataserverNr  AS INTEGER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttDataserver.

DEFINE VARIABLE cDbComm               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserName             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPassword             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cForceUserName        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cForcePassword        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLogNameDS            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPhysNameDS           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDatabaseType         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStatus               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAddParams            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cConnectedDatabases   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDontShow             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lDontShowSchemaHr     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE hWindow               AS HANDLE     NO-UNDO.
DEFINE VARIABLE iStartTime            AS INT64      NO-UNDO.
DEFINE VARIABLE iDataserverCount      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iItem                 AS INTEGER    NO-UNDO.

DEFINE BUFFER bDb FOR dictdb._db.

#GetDataserverInfo:
FOR EACH bDb 
  WHERE bDb._db-slave = YES NO-LOCK 
     BY bDb._db-name:

  /* Avoid error: "Could not create buffer object for table TPROGRESS._Db. (7334)" */
  FIND FIRST ttDataserver WHERE ttDataserver.cLDbNameDataserver = bDb._db-name NO-ERROR.
  
  IF AVAILABLE ttDataserver
    AND NOT ttDataserver.lConnected
    AND CONNECTED(ttDataserver.cLDbNameDataserver) THEN ttDataserver.lConnected = YES.
  
  IF AVAILABLE ttDataserver
    AND ttDataserver.lConnected = YES THEN RETURN.
  
  ASSIGN
    iDataserverCount  = iDataserverCount + 1
    cUserName         = ""
    cPassword         = ""
    cLogNameDS        = bDb._db-name
    cPhysNameDS       = bDb._db-addr
    cDatabaseType     = bDb._db-type
    cDbComm           = bDb._db-comm
    lDontShowSchemaHr = NO.

  RUN removeWhiteSpace(INPUT-OUTPUT cDbComm).
  RUN getParameter(INPUT "-db", INPUT cDbComm, INPUT-OUTPUT cPhysNameDS).
  RUN getParameter(INPUT "-ld", INPUT cDbComm, INPUT-OUTPUT cLogNameDS).
  RUN getParameter(INPUT "-U" , INPUT cDbComm, INPUT-OUTPUT cUserName).
  RUN getParameter(INPUT "-P" , INPUT cDbComm, INPUT-OUTPUT cPassword).

  IF cUserName = "" THEN
    ASSIGN
      cUserName = getUserName()
      cUserName = (IF CAN-DO("AS400", cDatabaseType) THEN /* AS400 can only connect with userid in caps */
                     CAPS(cUserName)
                   ELSE
                     cUserName).

  USE "DataDigger".
  GET-KEY-VALUE SECTION "DataDigger:dataservers" KEY pcLDbNameSchema + ":username" VALUE cForceUserName.
  GET-KEY-VALUE SECTION "DataDigger:dataservers" KEY pcLDbNameSchema + ":password" VALUE cForcePassword.
  GET-KEY-VALUE SECTION "DataDigger:dataservers" KEY pcLDbNameSchema + ":addparms" VALUE cAddParams.
  GET-KEY-VALUE SECTION "DataDigger:dataservers" KEY pcLDbNameSchema + ":dontshow" VALUE cDontShow.
  USE "".

  IF cForceUserName <> ? AND cForceUserName <> "" THEN cUserName = cForceUserName.
  IF cForcePassword <> ? AND cForcePassword <> "" THEN cPassword = cForcePassword.
 
  IF cAddParams = ? THEN cAddParams = "".
  
  IF LOOKUP(cDontShow, "yes,true") > 0 THEN lDontShowSchemaHr = YES.
  
  RUN removeParameter(INPUT "-db", INPUT YES, INPUT-OUTPUT  cDbComm).
  RUN removeParameter(INPUT "-ld", INPUT YES, INPUT-OUTPUT  cDbComm).
  RUN removeParameter(INPUT "-U" , INPUT YES, INPUT-OUTPUT  cDbComm).
  RUN removeParameter(INPUT "-P" , INPUT YES, INPUT-OUTPUT  cDbComm).
  
  FIND ttDataserver  
    WHERE ttDataserver.cLDbNameSchema = pcLDbNameSchema
      AND ttDataserver.cLDbNameDataserver = cLogNameDS NO-ERROR.

  IF NOT AVAILABLE ttDataserver THEN
  DO:
    CREATE ttDataserver.
    ASSIGN
      piDataserverNr                   = piDataserverNr + 1
      ttDataserver.iServerNr          = piDataserverNr
      ttDataserver.cLDbNameSchema     = pcLDbNameSchema
      ttDataserver.cLDbNameDataserver = cLogNameDS
      ttDataserver.cPDbNameDataserver = cPhysNameDS
      ttDataserver.cDbType            = cDatabaseType
      ttDataserver.cConnectString     = TRIM(SUBSTITUTE(  "-db &1 -ld &2 &3 -U &4 -P &5 &6"
                                                        , cPhysNameDS
                                                        , cLogNameDS
                                                        , cDbComm
                                                        , cUserName
                                                        , cPassword
                                                        , cAddParams
                                                        ))
      ttDataserver.lDontShowSchemaHr  = lDontShowSchemaHr
      .
  END. /* IF NOT AVAILABLE ttDataserver */
END. /* FOR EACH bDb  */

&if "{&support-more-dataservers}" <> "yes" &then
  IF iDataserverCount > 1 THEN
  DO:
    FOR EACH ttDataserver WHERE ttDataserver.iServerNr <> 1:
      DELETE ttDataserver.
    END.
      
    FIND ttDataserver WHERE ttDataserver.iServerNr = 1.   /* Die is er */

    MESSAGE
      SUBSTITUTE( TRIM(
                  "For schemaholder '&1' are &2 dataservers defined."   + "~n" +
                  "Currently there is support for max 1 dataserver per" + "~n" +
                  "schemaholder. The first dataserver (alphabetically)" + "~n" +
                  "will be used, which is '&3'."                        + "~n" +
                  "", "~n")
                , pcLDbNameSchema
                , iDataserverCount
                , ttDataserver.cLDbNameDataserver
                )
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.
&endif

FOR EACH ttDataserver BY ttDataserver.iServerNr:
  IF NOT CONNECTED(ttDataserver.cLDbNameDataserver) THEN
  DO:
    ASSIGN
      cStatus = SUBSTITUTE(  "Connecting &1 (&2) ..."
                           , ttDataserver.cLDbNameDataserver
                           , ttDataserver.cDbType
                           ).
    
    RUN showMessage.p("DataDigger", cStatus, OUTPUT hWindow).
    
    /* Enforce small delay */
    iStartTime = ETIME.
    REPEAT WHILE ETIME < iStartTime + 1000: /* small delay */ END.

    CONNECT VALUE(ttDataserver.cConnectString) NO-ERROR.

    IF   ERROR-STATUS:GET-MESSAGE(1) <> ?
     AND ERROR-STATUS:GET-MESSAGE(1) <> ""
     AND (IF ERROR-STATUS:GET-NUMBER(1) = 43 AND program-name(3) BEGINS "btnDisconnectChoose " THEN
            NO
          ELSE
            YES)
    THEN  
    DO:
      MESSAGE
        SUBSTITUTE( TRIM(
                    "For schemaholder '&1' &2 dataserver '&3'"  + "~n" +
                    "could not be connected. Error returned:"   + "~n" +
                    ""                                          + "~n" +
                    ERROR-STATUS:GET-MESSAGE(1)                 + "~n" +
                    ""                                          + "~n" +
                    "Dataserver connection string:"             + "~n" +
                    ""                                          + "~n" +
                    "&4"                                        + "~n" +
                    "", "~n")
                  , pcLDbNameSchema
                  , ttDataserver.cDbType
                  , ttDataserver.cLDbNameDataserver
                  , ttDataserver.cConnectString
                  )
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.

    DELETE WIDGET hWindow.
  END.
  
  ttDataserver.lConnected = CONNECTED(ttDataserver.cLDbNameDataserver).
END. /* FOR EACH */

if program-name(3) begins "btnDisconnectChoose " then
do:
  do iItem = 1 to num-dbs:
    cConnectedDatabases = trim(cConnectedDatabases + "," + ldbname(iItem), ",").
  end.
  
  for each ttDataserver by ttDataserver.iServerNr:
    if not can-do(cConnectedDatabases, ttDataserver.cLDbNameSchema) then
    do:
      piDataserverNr = piDataserverNr - 1.
      delete ttDataserver.
    end.
  end.
end.

PROCEDURE removeParameter:
  DEFINE INPUT        PARAMETER pcParam       AS CHARACTER  NO-UNDO.
  DEFINE INPUT        PARAMETER plCheckFirst  AS LOGICAL    NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcDbComm      AS CHARACTER  NO-UNDO.

  IF plCheckFirst AND LOOKUP(pcParam, pcDbComm, " ") = 0 THEN RETURN.

  BLOCKLoopRemove:
  REPEAT:
    IF LOOKUP(pcParam, pcDbComm, " ") > 0 THEN
    DO:
      ENTRY(LOOKUP(pcParam, pcDbComm, " ") + 1, pcDbComm, " ") = "".
      ENTRY(LOOKUP(pcParam, pcDbComm, " "), pcDbComm, " ") = "".
    END.
    ELSE
      LEAVE BLOCKLoopRemove.
  END.

  RUN removeWhiteSpace (INPUT-OUTPUT pcDbComm).
  
END PROCEDURE. /* removeParameter */


PROCEDURE getParameter:
  DEFINE INPUT        PARAMETER pcParam   AS CHARACTER  NO-UNDO.
  DEFINE INPUT        PARAMETER pcDbComm  AS CHARACTER  NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcVar     AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE iPos AS INTEGER    NO-UNDO.

  iPos = LOOKUP(pcParam, pcDbComm, " ").
  IF iPos > 0 THEN pcVar = ENTRY(iPos + 1, pcDbComm, " ").

END PROCEDURE. /* getParameter */


PROCEDURE removeWhiteSpace:
  DEFINE INPUT-OUTPUT PARAMETER pcDbComm AS CHARACTER NO-UNDO.

  pcDbComm = TRIM(pcDbComm).

  BLOCKLoopDouble:
  REPEAT:
    IF INDEX(pcDbComm, "  ") > 0 THEN
      pcDbComm = REPLACE(pcDbComm, "  ", " ").
    ELSE
      LEAVE BLOCKLoopDouble.
  END.
END PROCEDURE. /* removeWhiteSpace */