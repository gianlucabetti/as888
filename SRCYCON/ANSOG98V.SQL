000100170105--  Generazione SQL
000200170105--  Versione:                  V7R1M0 100423
000300170105--  Generata su:               05/01/17 16:34:28
000400170105--  Database relazionale:      AS888
000500170105--  Opzioni standard:          DB2 for i
000600170105
000700170105CREATE VIEW PJBARGRU/ANSOG98V (
000800170105  PARTITA_IVA FOR COLUMN SOGPARTIVA ,
000900170105  SOGDES )
001000170105  AS
001100170105  WITH A (PARTIVA, DTULMO) AS (SELECT SOGPARTIVA, MAX(SOGDTULMO) FROM PJBARGRU/ANSOG00F ANSOG00F JOI
001200170105N PJBARGRU/ANFRN00F ANFRN00F ON ANSOG00F.SOGSOGG = ANFRN00F.FRNSOGG AND '' = ANFRN00F.FRNTPIND AND '
001300170105' = ANFRN00F.FRNCDIND WHERE
001400170105  			SOGPARTIVA NOT IN ('', '$$') GROUP BY SOGPARTIVA) SELECT SOGPARTIVA AS PARTITA_IVA, MAX(SOGDES)
001500170105 AS RAGIONE_SOCIALE FROM PJBARGRU/ANSOG00F ANSOG00F JOIN A A ON ANSOG00F.SOGPARTIVA = A.PARTIVA AND
001600170105ANSOG00F.SOGDTULMO = A.DTULMO GROUP BY ANSOG00F.SOGPARTIVA
001700170105  RCDFMT ANSOG98V   ;
001800170105
001900170105LABEL ON TABLE PJBARGRU/ANSOG98V
002000170105  IS 'Una riga per partita IVA solo fornitori.' ;
002100170105
002200170105LABEL ON COLUMN PJBARGRU/ANSOG98V
002300170105( PARTITA_IVA IS 'Partita IVA' ) ;
002400170105
002500170105LABEL ON COLUMN PJBARGRU/ANSOG98V
002600170105( PARTITA_IVA TEXT IS 'Partita IVA ................' ) ;
002700170105
002800170105GRANT SELECT
002900170105ON PJBARGRU/ANSOG98V TO PUBLIC ;
003000170105
003100170105GRANT ALTER , REFERENCES , SELECT
003200170105ON PJBARGRU/ANSOG98V TO QPGMR WITH GRANT OPTION ;
003300170105
