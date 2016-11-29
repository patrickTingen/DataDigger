  
  /* TT for sorting options in user query */
  DEFINE TEMP-TABLE ttQuerySort NO-UNDO RCODE-INFORMATION
    FIELD iSortNr     AS INTEGER
    FIELD cSortField  AS CHARACTER
    FIELD lDescending AS LOGICAL
    INDEX iPrim IS PRIMARY iSortNr
    .
  

PROCEDURE getQuerySorting:
  DEFINE INPUT  PARAMETER pcQuery AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iWord     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWord     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cField    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iNumSorts AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cWhere    AS CHARACTER   NO-UNDO.

  /* Clear old sorting */
  EMPTY TEMP-TABLE ttQuerySort.

  /* Restructure query, remove double spaces */
  DO iWord = 1 TO NUM-ENTRIES(pcQuery,' ') - 1:
    cWord = ENTRY(iWord,pcQuery,' ').
    IF cWord <> '' THEN cWhere = TRIM(cWhere + ' ' + cWord).
  END. 

  IndexLoop:
  DO iWord = 1 TO NUM-ENTRIES(cWhere,' ') - 1:
    cWord = ENTRY(iWord,cWhere,' ').

    IF cWord = 'BY' THEN
    DO:
      iNumSorts = iNumSorts + 1.

      CREATE ttQuerySort.
      ASSIGN 
        ttQuerySort.iSortNr     = iNumSorts
        ttQuerySort.cSortField  = ENTRY(iWord + 1,cWhere,' ')
        ttQuerySort.lDescending =     ( NUM-ENTRIES(cWhere,' ') >= iWord + 2 ) 
                                  AND ( ENTRY(iWord + 2,cWhere,' ') BEGINS "DESC" ).
    END.
  END. 
END PROCEDURE. 

RUN getQuerySorting("for   each      customer   by          name by city descending by sales-rep   ").

DEFINE VARIABLE cSort AS CHARACTER   NO-UNDO.

FOR EACH ttQuerySort BY ttQuerySort.iSortNr:
  cSort = SUBSTITUTE('&1~n&2 &3', cSort, ttQuerySort.cSortField, STRING(ttQuerySort.lDescending,'descending/')).
END.
MESSAGE cSort
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

