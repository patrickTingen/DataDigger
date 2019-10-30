 /*------------------------------------------------------------------------
    File        : test_isValidCodepage.cls
    Purpose     : Test function isValidCodepage in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_isValidCodepage:
  
  FUNCTION isValidCodePage RETURNS LOGICAL
    (pcCodepage AS CHARACTER) IN SUPER. 
    
  DEFINE VARIABLE ghLib AS HANDLE NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
    SESSION:ADD-SUPER-PROCEDURE(ghLib, SEARCH-TARGET).
  END METHOD.  
  
  @Test.
  METHOD PUBLIC VOID emptyString(  ):
    Assert:IsTrue(isValidCodepage('')). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID windowsCodepage(  ):
    Assert:IsTrue(isValidCodepage('1252')). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID unknownValue(  ):
    Assert:IsTrue(isValidCodepage(?)). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID strangeValue(  ):
    Assert:isFalse(isValidCodepage('penguin')). 
  END METHOD.

  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
  END METHOD.
    
END CLASS.