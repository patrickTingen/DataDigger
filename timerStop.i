/* 
** Name: timerStop.i
** Desc: Statement for debugging DataDigger. 
**       In separate include to keep Sonar from complaining
*/
FINALLY: 
  PUBLISH "DD:Timer" ("stop", ENTRY(1,PROGRAM-NAME(1)," ")). 
END FINALLY.
