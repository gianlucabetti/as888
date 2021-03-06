000100170105--  Generazione SQL
000200170105--  Versione:                  V7R1M0 100423
000300170105--  Generata su:               05/01/17 16:31:06
000400170105--  Database relazionale:      AS888
000500170105--  Opzioni standard:          DB2 for i
000600170105
000700170105CREATE VIEW PJBARSOC/NDREG5000V (
000800170105  V_DATA_INSERT FOR COLUMN V_DAT00001 ,
000900170105  V_SOCIETA ,
001000170105  V_SERIE ,
001100170105  V_DATA ,
001200170105  V_NUMERO ,
001300170105  V_CAUSALE ,
001400170105  V_IMPORTO ,
001500170105  A_SOCIETA ,
001600170105  A_SERIE ,
001700170105  A_DATA ,
001800170105  A_NUMERO ,
001900170105  A_CAUSALE ,
002000170105  A_IMPORTO )
002100170105  AS
002200170105  SELECT VREG.REGDTIMM AS V_DATA_INSERT, VREG.REGSOCIETA AS V_SOCIETA, VREG.REGSERREG AS V_SERIE, VR
002300170105EG.REGDTREG AS V_DATA, VREG.REGNRREG AS V_NUMERO, VREG.REGCAUSTES AS V_CAUSALE, DEC(VREG.REGIMPORTO,
002400170105 15, 2) AS V_IMPORTO, VMOV.MOVSOCVAL AS A_SOCIETA, AREG.REGSERREG AS A_SERIE, AREG.REGDTREG AS A_DAT
002500170105A, AREG.REGNRREG AS A_NUMERO, AREG.REGCAUSTES AS A_CAUSALE, DEC(AREG.REGIMPORTO, 15, 2) AS A_IMPORTO
002600170105
002700170105  		FROM PJBARSOC/NDREG00F VREG
002800170105  		JOIN PJBARSOC/NDMOV00F VMOV ON VREG.REGSYS = VMOV.MOVSYS AND VREG.REGNRASREG = VMOV.MOVNRASREG A
002900170105ND 1 = VMOV.MOVNRRIGAM
003000170105  		LEFT OUTER JOIN PJBARSOC/NDREG00F AREG ON VMOV.MOVSOCVAL = AREG.REGSOCIETA AND VREG.REGCTB = ARE
003100170105G.REGCTB AND VREG.REGDTDOC = AREG.REGDTDOC AND VREG.REGNRDOC = AREG.REGNRDOC AND VREG.REGSERDOC = AR
003200170105EG.REGSERDOC
003300170105  		LEFT OUTER JOIN PJBARSOC/NDMOV00F AMOV ON AREG.REGSYS = AMOV.MOVSYS AND AREG.REGNRASREG = AMOV.M
003400170105OVNRASREG AND 1 = AMOV.MOVNRRIGAM
003500170105  		WHERE VREG.REGCTB = 'CG' AND VREG.REGDOCIVA = '1' AND LEFT(VREG.REGSERREG, 1) = '2' AND VREG.REG
003600170105UNITA = '500' AND VREG.REGDTREG > '2015-12-31' AND (AREG.REGDOCIVA = VREG.REGDOCIVA OR AREG.REGDOCIV
003700170105A IS NULL) AND (LEFT(AREG.REGSERREG, 1) = '1' OR AREG.REGSERREG IS NULL) AND (AREG.REGUNITA = VREG.R
003800170105EGUNITA OR AREG.REGUNITA IS NULL) AND (VREG.REGSOCIETA = AMOV.MOVSOCVAL OR AMOV.MOVSOCVAL IS NULL)
003900170105
004000170105  RCDFMT NDREG5000V ;
004100170105
004200170105LABEL ON TABLE PJBARSOC/NDREG5000V
004300170105  IS 'Fatture infragruppo (vendita <-> acquisto).' ;
004400170105
004500170105LABEL ON COLUMN PJBARSOC/NDREG5000V
004600170105( V_DATA_INSERT IS 'Data                immiss.' ,
004700170105  V_SOCIETA IS 'Soc' ,
004800170105  V_SERIE IS 'Serie               reg.' ,
004900170105  V_DATA IS 'Data                registraz.' ,
005000170105  V_NUMERO IS 'Numero              registraz.' ,
005100170105  V_CAUSALE IS 'Cau                 ope                 tes' ,
005200170105  A_SOCIETA IS 'Soc                 a                   val' ,
005300170105  A_SERIE IS 'Serie               reg.' ,
005400170105  A_DATA IS 'Data                registraz.' ,
005500170105  A_NUMERO IS 'Numero              registraz.' ,
005600170105  A_CAUSALE IS 'Cau                 ope                 tes' ) ;
005700170105
005800170105LABEL ON COLUMN PJBARSOC/NDREG5000V
005900170105( V_DATA_INSERT TEXT IS 'Data immissione ............' ,
006000170105  V_SOCIETA TEXT IS 'Societ� ....................' ,
006100170105  V_SERIE TEXT IS 'Serie registrazione ........' ,
006200170105  V_DATA TEXT IS 'Data registrazione .........' ,
006300170105  V_NUMERO TEXT IS 'Numero registrazione .......' ,
006400170105  V_CAUSALE TEXT IS 'Causale operativa di testata' ,
006500170105  A_SOCIETA TEXT IS 'Societ� a valere ...........' ,
006600170105  A_SERIE TEXT IS 'Serie registrazione ........' ,
006700170105  A_DATA TEXT IS 'Data registrazione .........' ,
006800170105  A_NUMERO TEXT IS 'Numero registrazione .......' ,
006900170105  A_CAUSALE TEXT IS 'Causale operativa di testata' ) ;
007000170105
007100170105GRANT SELECT
007200170105ON PJBARSOC/NDREG5000V TO PUBLIC ;
007300170105
007400170105GRANT ALTER , REFERENCES , SELECT
007500170105ON PJBARSOC/NDREG5000V TO QPGMR WITH GRANT OPTION ;
007600170105
