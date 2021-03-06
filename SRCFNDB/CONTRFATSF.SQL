000100171114--  Generazione SQL 
000200171114--  Versione:                  	V7R2M0 140418 
000300171114--  Generata su:               	14/11/17 12:57:46 
000400171114--  Database relazionale:      	SETRAS 
000500171114--  Opzioni standard:          	DB2 for i 
000600171114CREATE VIEW UNITRAGRU.CONTRFATSF ( 
000700171114	ANNO , 
000800171114	L_PART , 
000900171114	SERIE , 
001000171114	NUM_SPED , 
001100171114	L_ARRIVO , 
001200171114	COD_BOLLA , 
001300171114	TIPO_BOLLA , 
001400171114	DESTINATARIO FOR COLUMN DESTI00001 , 
001500171114	VAR_IN_SF , 
001600171114	DATA_IN_SF , 
001700171114	UTE_VARIAZ , 
001800171114	"IMP FATTURA" FOR COLUMN IMP_F00001 , 
001900171114	"IMPONIBILE DEL SF" FOR COLUMN IMPON00001 , 
002000171114	"CLI SF" FOR COLUMN CLI_S00001 , 
002100171114	"RAG.SOC SF" FOR COLUMN RAG_S00001 ) 
002200171114	AS 
002300171114	WITH SEL1 AS(  
002400171114	 SELECT TASAAS, TASLNP, TASNRS, TASNSP, TASLNA, TASCBO, TASTBL,  
002500171114	 TASRSD,  
002600171114	 TASKSC, TASDCM, FIARBT0F.ARBPRU, FIARBT0F.ARBDTV, FIARBT0F.ARBCVB,  
002700171114	 FIARBT0F.ARBIFT, TASIMV  
002800171114	 FROM UNITRAGRU.FIARBT0F FIARBT0F, GAITRAGRU.TITAS00F TITAS00F  
002900171114	 WHERE ARBCVB='T6' AND  
003000171114	 ARBAAS=TASAAS AND ARBLNP=TASLNP AND ARBNRS=TASNRS AND ARBNSP=TASNSP 
003100171114	 UNION  
003200171114	 SELECT TASAAS, TASLNP, TASNRS, TASNSP, TASLNA, TASCBO, TASTBL,  
003300171114	 TASRSD,  
003400171114	 TASKSC, TASDCM, FIARBT0F.ARBPRU, FIARBT0F.ARBDTV, FIARBT0F.ARBCVB,  
003500171114	 FIARBT0F.ARBIFT, TASIMV  
003600171114	 FROM UNITRAGRU.FIARBT0F FIARBT0F, GAITRAGRU.TITAS10F TITAS10F  
003700171114	 WHERE ARBCVB='T6'  
003800171114	 AND  
003900171114	 ARBAAS=TASAAS AND ARBLNP=TASLNP AND ARBNRS=TASNRS AND ARBNSP=TASNSP 
004000171114	 )  
004100171127	 UNION  
004200171127	 SELECT TASAAS, TASLNP, TASNRS, TASNSP, TASLNA, TASCBO, TASTBL,  
004300171127	 TASRSD,  
004400171127	 TASKSC, TASDCM, FIARBT0F.ARBPRU, FIARBT0F.ARBDTV, FIARBT0F.ARBCVB,  
004500171127	 FIARBT0F.ARBIFT, TASIMV  
004600171127	 FROM UNITRAGRU.FIARBT0F FIARBT0F, GAITRAGRUS.TITASP0F TITASP0F  
004700171127	 WHERE ARBCVB='T6'  
004800171127	 AND  
004900171127	 ARBAAS=TASAAS AND ARBLNP=TASLNP AND ARBNRS=TASNRS AND ARBNSP=TASNSP 
005000171127	 )  
005100171114	 SELECT TASAAS AS ANNO, TASLNP AS "L_PART", TASNRS AS SERIE, TASNSP  
005200171114	AS "NUM_SPED", TASLNA "L_ARRIVO", TASCBO AS "COD_BOLLA", TASTBL AS  
005300171114	"TIPO_BOLLA", TASRSD AS "DESTINATARIO",  
005400171114	 SEL1.ARBCVB AS "VAR_IN_SF", SEL1.ARBDTV AS "DATA_IN_SF",  
005500171127	SEL1.ARBPRU AS "UTE_VARIAZ", SEL1.ARBIFT AS"IMP_FATTURA", SEL1.TASIMV 
005600171127	 AS "IMPONIBILE_DEL_SF",  
005700171127	 TASKSC AS "CLI_SF",  
005800171127	 CNACO00F.ACORAG AS "RAG_SOC_SF"  
005900171114	  
006000171114	 FROM SEL1 SEL1, UNITRAGRU.CNACO00F CNACO00F WHERE CNACO00F.ACOKSC  
006100171114	 =TASKSC  
006200171114	  
006300171114	 AND CNACO00F.ACOKCC=00151   
006400171114	RCDFMT CONTRFATSF ; 
006500171114  
006600171114LABEL ON TABLE UNITRAGRU.CONTRFATSF 
006700171127	IS 'Controlla fattura imm. trasformate in segue fat' ; 
006800171114  
006900171114LABEL ON COLUMN UNITRAGRU.CONTRFATSF 
007000171114( ANNO IS 'ANNO                SPED' , 
007100171114	L_PART IS 'P.O                 PARTENZA' , 
007200171114	SERIE IS 'NUM                 SERIE' , 
007300171114	NUM_SPED IS 'NUM.                SPED.' , 
007400171114	L_ARRIVO IS 'P.O                 ARR' , 
007500171114	COD_BOLLA IS 'COD                 BOLLA' , 
007600171114	TIPO_BOLLA IS 'TIP.                BOL.' , 
007700171114	DESTINATARIO IS 'RAG.SOC.DESTINATARIO' , 
007800171114	VAR_IN_SF IS 'CAU                 VARIAZ              BOLLE' , 
007900171114	DATA_IN_SF IS 'DATA                VARIAZ.' , 
008000171114	UTE_VARIAZ IS 'PROFILO             UTENTE' , 
008100171127	"IMP_FATTURA" IS 'IMP.FATTURA' , 
008200171127	"IMPONIBILE_DEL_SF" IS 'IMPONIBILE IVA' , 
008300171127	"CLI_SF" IS 'CODICE              CLIENTE' , 
008400171127	"RAG_SOC_SF" IS 'DESCR.CONTO' ) ; 
008500171114  
008600171114LABEL ON COLUMN UNITRAGRU.CONTRFATSF 
008700171114( ANNO TEXT IS 'ANNO SPEDIZIONE' , 
008800171114	L_PART TEXT IS 'P.O. PARTENZA' , 
008900171114	SERIE TEXT IS 'NUMERO SERIE' , 
009000171114	NUM_SPED TEXT IS 'NUMERO SPEDIZIONE' , 
009100171114	L_ARRIVO TEXT IS 'P.O. ARRIVO' , 
009200171114	COD_BOLLA TEXT IS 'CODICE BOLLA' , 
009300171114	TIPO_BOLLA TEXT IS 'TIPO BOLLA' , 
009400171114	DESTINATARIO TEXT IS 'RAGIONE SOCIALE DESTINATARIO' , 
009500171114	VAR_IN_SF TEXT IS 'CAUS.VARIAZ.BOLLE' , 
009600171114	DATA_IN_SF TEXT IS 'DATA VARIAZIONE' , 
009700171114	UTE_VARIAZ TEXT IS 'PROFILO UTENTE CHE HA ESEGUITO LA VARIAZIONE' , 
009800171127	"IMP_FATTURA" TEXT IS 'IMPORTO FATTURA' , 
009900171127	"IMPONIBILE_DEL_SF" TEXT IS 'IMPONIBILE VVA' , 
010000171127	"CLI_SF" TEXT IS 'CODICE CLIENTE FATTURAZIONE' , 
010100171127	"RAG_SOC_SF" TEXT IS 'DESCRIZIONE CONTO' ) ; 
010200171114  
010300171114GRANT ALTER , REFERENCES , SELECT   
010400171114ON UNITRAGRU.CONTRFATSF TO QPGMR WITH GRANT OPTION ;
