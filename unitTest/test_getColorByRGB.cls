 /*------------------------------------------------------------------------
    File        : test_getColorByRGB.cls
    Purpose     : Test function getColorByRGB in DataDiggerLib.p  
 
    Author(s)   : Patrick.Tingen
    Created     : Thu Oct 17 15:19:49 CEST 2019
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS test_getColorByRGB:
 
  FUNCTION getColorByRGB RETURNS INTEGER
    ( piRed   AS INTEGER
    , piGreen AS INTEGER
    , piBlue  AS INTEGER
    ) IN SUPER. 
    
  DEFINE VARIABLE ghLib AS HANDLE NO-UNDO.
 
  @Before.
  METHOD PUBLIC VOID setUp(  ):
    RUN datadiggerlib.p PERSISTENT SET ghLib.
    SESSION:ADD-SUPER-PROCEDURE(ghLib, SEARCH-TARGET).
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID findBlack(  ):
    Assert:Equals(0, getColorByRGB(0,0,0)). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID findRed(  ):  
    Assert:Equals(12, getColorByRGB(255,0,0)). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID findGreen(  ):  
    Assert:Equals(10, getColorByRGB(0,255,0)). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID findBlue(  ):  
    Assert:Equals(9, getColorByRGB(0,0,255)). 
  END METHOD.
  
  @Test.
  METHOD PUBLIC VOID findGray(  ):  
    Assert:Equals(8, getColorByRGB(192,192,192)). 
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID addWrongColor(  ):
    DEFINE VARIABLE iNumColors AS INTEGER NO-UNDO.
    iNumColors = COLOR-TABLE:NUM-ENTRIES.  
    Assert:Equals(iNumColors, getColorByRGB(999,999,999)). 
  END METHOD.  

  @Test.
  METHOD PUBLIC VOID addNewColor(  ):  
    DEFINE VARIABLE iRed   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iGreen AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlue  AS INTEGER NO-UNDO.
    DEFINE VARIABLE i      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iColor AS INTEGER NO-UNDO.
    
    #FindColor:
    REPEAT:
      iRed   = RANDOM(0,255).
      iGreen = RANDOM(0,255).
      iBlue  = RANDOM(0,255).
    
      /* See if already exists */
      DO i = 0 TO COLOR-TABLE:NUM-ENTRIES - 1:
        IF    COLOR-TABLE:GET-RED-VALUE(i)   = iRed
          AND COLOR-TABLE:GET-GREEN-VALUE(i) = iGreen
          AND COLOR-TABLE:GET-BLUE-VALUE(i)  = iBlue THEN NEXT #FindColor.
      END.
    
      LEAVE #FindColor.
    END.
    
    /* Create the new color */
    iColor = COLOR-TABLE:NUM-ENTRIES.    
    Assert:Equals(iColor, getColorByRGB(iRed, iGreen, iBlue)). 
  END METHOD.  

  @After.
  METHOD PUBLIC VOID tearDown():
    DELETE OBJECT ghLib NO-ERROR.
  END METHOD.
    
END CLASS.