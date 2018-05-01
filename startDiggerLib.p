/* Start DiggerLib if it has not already been started
 */
DEFINE VARIABLE hDiggerLib AS HANDLE    NO-UNDO.
DEFINE VARIABLE cDiggerLib AS CHARACTER NO-UNDO.

/* Call out to see if the lib has been started */
PUBLISH 'DataDiggerLib' (OUTPUT hDiggerLib).

IF NOT VALID-HANDLE(hDiggerLib) THEN
DO:
  cDiggerLib = THIS-PROCEDURE:FILE-NAME.
  cDiggerLib = REPLACE(cDiggerLib,"\","/").
  cDiggerLib = SUBSTRING(cDiggerLib,1,R-INDEX(cDiggerLib,'/')) + 'DataDiggerLib.p'.
  
  IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'd:\data\progress\DataDigger\DataDiggerLib.p'.
  IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'd:\data\dropbox\DataDigger\src\DataDiggerLib.p'.
  IF SEARCH(cDiggerLib) = ? THEN cDiggerLib = 'c:\data\dropbox\DataDigger\src\DataDiggerLib.p'.
  
  RUN VALUE(cDiggerLib) PERSISTENT SET hDiggerLib.
  SESSION:ADD-SUPER-PROCEDURE(hDiggerLib,SEARCH-TARGET).
END.
