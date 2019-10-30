 /*------------------------------------------------------------------------
    File        : test_getOsErrorDesc.cls
    Purpose     : Test function getOsErrorDesc in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_getOsErrorDesc:
  
  FUNCTION getOsErrorDesc RETURNS CHARACTER
    ( piOsError AS INTEGER ) IN SUPER. 
    
  DEFINE VARIABLE ghLib AS HANDLE NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
    SESSION:ADD-SUPER-PROCEDURE(ghLib, SEARCH-TARGET).
  END METHOD.  
  
  @Test.
  METHOD PUBLIC VOID noNumber(  ):
    Assert:Equals('No error', getOsErrorDesc(0)). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID unknownValue(  ):
    Assert:Equals('Unmapped error', getOsErrorDesc(?)). 
  END METHOD.

  @Test.
  METHOD PUBLIC VOID negativeNumber(  ):
    Assert:Equals('Unmapped error', getOsErrorDesc(-1)). 
  END METHOD.

  @Test.
  METHOD PUBLIC VOID highNumber(  ):
    Assert:Equals('Unmapped error', getOsErrorDesc(9999)). 
  END METHOD.

  @Test.
  METHOD PUBLIC VOID fileExists(  ):  
    Assert:Equals('File exists', getOsErrorDesc(10)). 
  END METHOD.

  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
  END METHOD.
    
END CLASS.