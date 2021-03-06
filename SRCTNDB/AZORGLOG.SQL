000100150910--  Generazione SQL
000200150910--  Versione:                  V7R1M0 100423
000300150910--  Generata su:               10/09/15 17:06:23
000400150910--  Database relazionale:      AS888
000500150910--  Opzioni standard:          DB2 for i
000600150910
000700150910CREATE VIEW GAITRAGRPS/AZORGLOG (
000800150910  ORGFIL )
000900150910  AS
001000150910  SELECT ORGFIL FROM GAITRAGRPS/AZORG00F WHERE SUBSTR(ORGDE3, 11, 3) = 'LOG'
001100150910  RCDFMT AZORGLOG   ;
001200150910
001300150910LABEL ON TABLE GAITRAGRPS/AZORGLOG
001400150910  IS 'Filiali di logistica.' ;
001500150910
001600150910LABEL ON COLUMN GAITRAGRPS/AZORGLOG
001700150910( ORGFIL IS 'COD.FIL.' ) ;
001800150910
001900150910LABEL ON COLUMN GAITRAGRPS/AZORGLOG
002000150910( ORGFIL TEXT IS 'COD.FILIALE' ) ;
002100150910
002200150910GRANT ALTER , DELETE , INSERT , REFERENCES , SELECT , UPDATE
002300150910ON GAITRAGRPS/AZORGLOG TO QPGMR WITH GRANT OPTION ;
002400150910
