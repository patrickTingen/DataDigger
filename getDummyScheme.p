/*------------------------------------------------------------------------

  Name: getDummyScheme.p
  Desc: Generate some dummy data for testing generate procedures

  ----------------------------------------------------------------------*/

{ DataDigger.i }

DEFINE OUTPUT PARAMETER TABLE FOR ttField.
DEFINE OUTPUT PARAMETER TABLE FOR ttIndex.

RUN createFields.
RUN createIndexes.

PROCEDURE createFields:

  CREATE ttField. 
  ASSIGN 
    ttField.cFieldName = 'rep-nr'      
    ttField.lShow      = TRUE 
    ttField.cDataType  = 'INTEGER'   
    ttField.cFormat    = '>>>9'  
    ttField.cLabel     = 'Rep nr'.
  
  CREATE ttField. 
  ASSIGN 
    ttField.cFieldName = 'rep-name'    
    ttField.lShow      = TRUE 
    ttField.cDataType  = 'CHARACTER' 
    ttField.cFormat    = 'x(30)' 
    ttField.cLabel     = 'Rep name'.
  
  CREATE ttField. 
  ASSIGN 
    ttField.cFieldName = 'region'      
    ttField.lShow      = FALSE 
    ttField.cDataType  = 'CHARACTER' 
    ttField.cFormat    = 'x(8)'  
    ttField.cLabel     = 'Region'.
  
  CREATE ttField. 
  ASSIGN 
    ttField.cFieldName = 'month-quota' 
    ttField.lShow      = FALSE 
    ttField.cDataType  = 'INTEGER'   
    ttField.cFormat    = '->,>>>,>>9' 
    ttField.cLabel     = 'Rep name' 
    ttField.iExtent    = 12.

END PROCEDURE. /* createFields */


PROCEDURE createIndexes:

  CREATE ttIndex. 
  ASSIGN 
    ttIndex.cIndexName  = 'iPrim'   
    ttIndex.cIndexFlags = 'P U' 
    ttIndex.cFieldList  = 'rep-nr'.
  
  CREATE ttIndex. 
  ASSIGN 
    ttIndex.cIndexName  = 'iRegion' 
    ttIndex.cIndexFlags = ''    
    ttIndex.cFieldList  = 'region,rep-name'.   

END PROCEDURE. /* createIndexes */
