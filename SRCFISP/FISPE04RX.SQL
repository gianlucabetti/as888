000100100302DROP PROCEDURE GAITRAOBJ/FISPE04R;
000200061017
000300080417CREATE PROCEDURE
000400100302  GAITRAOBJ/FISPE04R (
000500100325    IN  PIN_TYPENV    CHAR (02 ),
000600100325    IN  PIN_TYPEXE    CHAR (01 ),
000700100108    OUT RTNESITO      INT,
000800080417    OUT RTNOPCODE     CHAR (10 ),
000900080417    OUT RTNSTATUS     INT )
001000080418  RESULT SETS 1
001100080417  LANGUAGE RPGLE
001200080417  NOT DETERMINISTIC
001300100518  MODIFIES SQL DATA
001400080417  EXTERNAL
001500080417  PARAMETER STYLE GENERAL;
001600061017
