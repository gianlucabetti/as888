000100061017
000200131127DROP PROCEDURE GAIAUTTST/FISP54R;
000300070118
000400131127CREATE PROCEDURE GAIAUTTST/FISP54R(IN PROFILO CHAR (10 ),
000500061031IN IDDISPOSITIVO CHAR(35 ), OUT
000600061017RTNESITO INT , OUT RTNOPCODE CHAR (10 ), OUT RTNSTATUS INT ) RESULT
000700101108SETS 16 LANGUAGE RPGLE NOT DETERMINISTIC READS SQL DATA EXTERNAL
000800061017PARAMETER STYLE GENERAL;
000900061017
