000100061228DROP PROCEDURE GAIAUTTST/FISP18R;
000200061017
000300061228CREATE PROCEDURE GAIAUTTST/FISP18R(IN PROFILO CHAR ( 10),
000400061031IN IDDISPOSITIVO CHAR(35 ), IN
000500061228DATI CHAR (250 ), OUT RTNESITO INT , OUT RTNOPCODE CHAR ( 10), OUT
000600061030RTNSTATUS INT ) LANGUAGE RPGLE NOT DETERMINISTIC NO SQL EXTERNAL
000700061030PARAMETER STYLE GENERAL
000800061017
