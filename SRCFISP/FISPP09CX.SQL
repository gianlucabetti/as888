000100171113DROP PROCEDURE GAITRAOBJ/FISPP09C;
000200061017
000300171113CREATE PROCEDURE GAITRAOBJ/FISPP09C(
000400171113  IN CODICEPC CHAR ( 1),
000500171113  IN FILIALE CHAR( 3),
000600171113  IN INIZIO CHAR ( 1),
000700171113  IN CATFOGLIO CHAR ( 1),
000800171113  IN NUMFOGLIO CHAR ( 7),
000900171117  IN TIPOPC CHAR ( 2),
001000171113  INOUT TICKET CHAR (17),
001100171113  OUT RTNRCDCNT INT,
001200171113  OUT RTNESITO INT,
001300171113  OUT RTNOPCODE CHAR (10 ),
001400171113  OUT RTNSTATUS INT )
001500171113RESULT SETS 1 LANGUAGE RPGLE NOT DETERMINISTIC MODIFIES SQL DATA EXTERNAL
001600061017PARAMETER STYLE GENERAL;
001700061017
