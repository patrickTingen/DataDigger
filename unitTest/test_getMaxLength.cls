 /*------------------------------------------------------------------------
    File        : test_etMaxLength.cls
    Purpose     : Test function getMaxLength in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_getMaxLength:
  
  FUNCTION getMaxLength RETURNS INTEGER
    ( cFieldList AS CHARACTER ) IN SUPER. 
    
  DEFINE VARIABLE ghLib AS HANDLE NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
    SESSION:ADD-SUPER-PROCEDURE(ghLib, SEARCH-TARGET).
  END METHOD.  
  
  @Test.
  METHOD PUBLIC VOID emptyString(  ):
    Assert:Equals(0, getMaxLength('')). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID equalLengths(  ):
    Assert:Equals(5, getMaxLength('alpha,bravo')). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID oneLongest(  ):
    Assert:Equals(7, getMaxLength('alpha,charlie,bravo')). 
  END METHOD.
    
  @Test.
  METHOD PUBLIC VOID unknownValue(  ):
    Assert:Equals(0, getMaxLength(?)). 
  END METHOD.

  @Test.
  METHOD PUBLIC VOID longString(  ):
    Assert:Equals(1000, getMaxLength( FILL('x',1000) )). 
  END METHOD.

  
  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
  END METHOD.
    
END CLASS.