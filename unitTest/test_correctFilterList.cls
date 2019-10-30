 /*------------------------------------------------------------------------
    File        : test_correctFilterList.cls
    Purpose     : Test procedure correctFilterList in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_correctFilterList:
  
  DEFINE VARIABLE ghLib AS HANDLE    NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
  END METHOD.

  
  @Test.
  METHOD PUBLIC VOID justPositiveEntries():  
    /* Move negative entries from positive list to negative
      */
    DEFINE VARIABLE cPositive AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNegative AS CHARACTER NO-UNDO.
    
    cPositive = 'Alpha,Bravo,Charlie,Delta'.
    cNegative = ''.
    RUN correctFilterList IN ghLib (INPUT-OUTPUT cPositive, INPUT-OUTPUT cNegative).
    
    Assert:Equals('Alpha,Bravo,Charlie,Delta', cPositive). 
    Assert:Equals('', cNegative). 
  END METHOD. 


  @Test.
  METHOD PUBLIC VOID negativeEntriesInPositiveList():  
    /* Move negative entries from positive list to negative
      */
    DEFINE VARIABLE cPositive AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNegative AS CHARACTER NO-UNDO.
    
    cPositive = '!Alpha,!Bravo,!Charlie,!Delta'.
    cNegative = ''.
    RUN correctFilterList IN ghLib (INPUT-OUTPUT cPositive, INPUT-OUTPUT cNegative).
    
    Assert:Equals('', cPositive). 
    Assert:Equals('Alpha,Bravo,Charlie,Delta', cNegative). 
  END METHOD. 


  @Test.
  METHOD PUBLIC VOID justNegativeEntries():  
    /* Move negative entries from positive list to negative
      */
    DEFINE VARIABLE cPositive AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNegative AS CHARACTER NO-UNDO.
    
    cPositive = ''.
    cNegative = 'Alpha,Bravo,Charlie,Delta'.
    RUN correctFilterList IN ghLib (INPUT-OUTPUT cPositive, INPUT-OUTPUT cNegative).
    
    Assert:Equals('', cPositive). 
    Assert:Equals('Alpha,Bravo,Charlie,Delta', cNegative). 
  END METHOD. 


  @Test.
  METHOD PUBLIC VOID mixedEntries():  
    /* Move negative entries from positive list to negative
      */
    DEFINE VARIABLE cPositive AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNegative AS CHARACTER NO-UNDO.
    
    cPositive = 'Alpha,!Bravo,!Charlie,Delta'.
    cNegative = 'Foo,Bar'.
    RUN correctFilterList IN ghLib (INPUT-OUTPUT cPositive, INPUT-OUTPUT cNegative).
    
    Assert:Equals('Alpha,Delta', cPositive). 
    Assert:Equals('Foo,Bar,Bravo,Charlie', cNegative). 
  END METHOD. 


  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
  END METHOD.
    
END CLASS.