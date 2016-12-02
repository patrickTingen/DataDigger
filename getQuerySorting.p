
/* TT for sorting options in user query */
DEFINE TEMP-TABLE ttQuerySort NO-UNDO RCODE-INFORMATION
  FIELD iSortNr     AS INTEGER
  FIELD cSortField  AS CHARACTER
  FIELD lDescending AS LOGICAL
  INDEX iPrim IS PRIMARY iSortNr
  .

PROCEDURE getQuerySorting:
  /* Extract sorting from user query */
  DEFINE INPUT PARAMETER pcQuery AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cPart AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPart AS INTEGER     NO-UNDO.

  /* Clear old sorting */
  EMPTY TEMP-TABLE ttQuerySort.
  
  /* Split query on the word ' BY ' */
  pcQuery = REPLACE(pcQuery,' BY ', '|').

  IndexLoop:
  DO iPart = 2 TO NUM-ENTRIES(pcQuery,'|'):
    cPart = TRIM(ENTRY(iPart,pcQuery,'|')).
    RUN addQuerySort(ENTRY(1,cPart,' '), (cPart MATCHES '* DESC*')).
  END.
END PROCEDURE. /* getQuerySorting */


PROCEDURE addQuerySort:
  /* Add a user query to tt with sortings */
  DEFINE INPUT PARAMETER pcField      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER plDescending AS LOGICAL     NO-UNDO.
  
  DEFINE VARIABLE iNumSorts AS INTEGER     NO-UNDO.
  DEFINE BUFFER bfQuerySort FOR ttQuerySort.

  IF pcField = '' OR pcField = ? THEN RETURN.
  
  /* Determine nr of sorts */
  FIND LAST bfQuerySort NO-ERROR.
  IF AVAILABLE bfQuerySort THEN iNumSorts = bfQuerySort.iSortNr.

  CREATE bfQuerySort.
  ASSIGN 
    bfQuerySort.iSortNr     = iNumSorts + 1
    bfQuerySort.cSortField  = pcField
    bfQuerySort.lDescending = plDescending. 

END PROCEDURE. /* addQuerySort */ 


RUN getQuerySorting("for   each      customer   by          name by city descending by sales-rep   ").

DEFINE VARIABLE cSort AS CHARACTER   NO-UNDO.

FOR EACH ttQuerySort BY ttQuerySort.iSortNr:
  cSort = SUBSTITUTE('&1~n&2 &3', cSort, ttQuerySort.cSortField, STRING(ttQuerySort.lDescending,'descending/') ).
END.

MESSAGE cSort
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
