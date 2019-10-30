 /*------------------------------------------------------------------------
    File        : test_createFolder.cls
    Purpose     : Test procedure createFolder in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_createFolder:
  
  DEFINE VARIABLE ghLib        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE gcMainFolder AS CHARACTER NO-UNDO.
  DEFINE VARIABLE gcSubFolder  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE gcTempFolder AS CHARACTER NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
    
    REPEAT:
      gcTempFolder = RIGHT-TRIM(SESSION:TEMP-DIRECTORY,'\').
      gcMainFolder = RIGHT-TRIM(SUBSTITUTE('&1\test-&2', gcTempFolder, RANDOM(1,100)) ,'\').
      gcSubFolder  = RIGHT-TRIM(SUBSTITUTE('&1\test-&2', gcMainFolder, RANDOM(1,100)) ,'\').
      
      FILE-INFORMATION:FILE-NAME = gcSubFolder.
      IF FILE-INFORMATION:FULL-PATHNAME = ? THEN LEAVE.
    END.  
  END METHOD.

  
  @Test.
  METHOD PUBLIC VOID createSingleFolder(  ):
      
    RUN createFolder IN ghLib (gcMainFolder).
    FILE-INFORMATION:FILE-NAME = gcMainFolder.
    Assert:Equals(gcMainFolder, FILE-INFORMATION:FULL-PATHNAME).
     
  END METHOD.


  @Test.
  METHOD PUBLIC VOID createSubFolder(  ):
      
    RUN createFolder IN ghLib (gcSubFolder).
    FILE-INFORMATION:FILE-NAME = gcSubFolder.
    Assert:Equals(gcSubFolder, FILE-INFORMATION:FULL-PATHNAME).
     
  END METHOD.

  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
    OS-DELETE VALUE(gcMainFolder) RECURSIVE. 
  END METHOD.
    
END CLASS.