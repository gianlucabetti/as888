000100061017
000200170522DROP PROCEDURE GAITRAOBJ/FISPG55R;
000300070118
000400170522CREATE PROCEDURE GAITRAOBJ/FISPG55R(
000500170522OUT
000600061017RTNESITO INT , OUT RTNOPCODE CHAR (10 ), OUT RTNSTATUS INT ) RESULT
000700170522SETS 1 LANGUAGE RPGLE NOT DETERMINISTIC READS SQL DATA EXTERNAL
000800061017PARAMETER STYLE GENERAL;
000900061017
