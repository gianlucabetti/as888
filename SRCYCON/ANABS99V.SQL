000100170613--  Generazione SQL
000200170613--  Versione:                  V7R2M0 140418
000300170613--  Generata su:               13/06/17 17:15:00
000400170613--  Database relazionale:      AS888
000500170613--  Opzioni standard:          DB2 for i
000600170613
000700170613CREATE VIEW PJBARGRU/ANABS99V (
000800170613  PROFILO ,
000900170613  SOCIETA )
001000170613  AS
001100170613  SELECT ANUTN00F.UTNPRF AS PROFILO, ANABS00F.ABSSOC AS SOCIETA
001200170613  	FROM PJBARGRU/ANUTN00F ANUTN00F
001300170613  	JOIN PJBARGRU/ANABS00F ANABS00F ON ANUTN00F.UTNGRP = ANABS00F.ABSGRP AND '' = ANABS00F.ABSPRF
001400170613  	UNION
001500170613  	SELECT ANUTN00F.UTNPRF AS PROFILO, ANABS00F.ABSSOC AS SOCIETA
001600170613  	FROM PJBARGRU/ANUTN00F ANUTN00F
001700170613  	JOIN PJBARGRU/ANABS00F ANABS00F ON ANUTN00F.UTNPRF = ANABS00F.ABSPRF
001800170613  RCDFMT ANABS99V   ;
001900170613
002000170613LABEL ON TABLE PJBARGRU/ANABS99V
002100170613  IS 'Societ� autorizzate per profilo utente.' ;
002200170613
002300170613LABEL ON COLUMN PJBARGRU/ANABS99V
002400170613( PROFILO IS 'Profilo' ,
002500170613  SOCIETA IS 'Societ�' ) ;
002600170613
002700170613LABEL ON COLUMN PJBARGRU/ANABS99V
002800170613( PROFILO TEXT IS 'Profilo ....................' ,
002900170613  SOCIETA TEXT IS 'Societ� ....................' ) ;
003000170613
003100170613GRANT SELECT
003200170613ON PJBARGRU/ANABS99V TO PUBLIC ;
003300170613
003400170613GRANT ALTER , REFERENCES , SELECT
003500170613ON PJBARGRU/ANABS99V TO QPGMR WITH GRANT OPTION ;
003600170613
