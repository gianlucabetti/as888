000100080904CREATE PROCEDURE GAITRAOBJ/TRTCM9R1(
000200080904IN  LIBFIL      CHAR   (10),
000300080904IN  LIBOBJ      CHAR   (10),
000400080904IN  IDPROG      CHAR   ( 7),
000500080904IN  STATO       CHAR   ( 1),
000600080904IN  MESSAGGIO   CHAR  (128),
000700080904OUT ESITO       CHAR   ( 1)
000800080904)
000900080904RESULT SETS 1 LANGUAGE RPGLE NOT DETERMINISTIC
001000080904READS SQL DATA EXTERNAL PARAMETER STYLE GENERAL;
