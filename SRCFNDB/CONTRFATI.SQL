000100171025--  Generazione SQL 
000110171025--  Versione:                  	V7R2M0 140418 
000120171025--  Generata su:               	25/10/17 13:25:19 
000130171025--  Database relazionale:      	SETRAS 
000140171025--  Opzioni standard:          	DB2 for i 
000150171025CREATE VIEW UNITRAGRU.CONTRFATI ( 
000160171025	ANNO , 
000170171025	L_PART , 
000180171025	SERIE , 
000190171025	NUM_SPED , 
000200171025	L_ARRIVO , 
000210171025	COD_BOLLA , 
000220171025	TIPO_BOLLA , 
000230171025	IMP_FAT , 
000240171025	VAR_IN_SF , 
000250171025	DATA_IN_SF , 
000260171025	UTE_VARIAZ , 
000270171025	CAU_STORNO , 
000280171025	DAT_STORNO , 
000290171025	UTE_STORNO ) 
000300171025	AS 
000310171025	WITH SEL1 AS(  
000320171025	 SELECT TASAAS, TASLNP, TASNRS, TASNSP, TASLNA, TASCBO, TASTBL,  
000330171025	 TASDCM, TASCCA, FIARBT0F.ARBPRU, FIARBT0F.ARBDTV, FIARBT0F.ARBCVB, 
000340171025	 FIARBT0F.ARBIFT  
000350171025	FROM UNITRAGRU.FIARBT0F FIARBT0F, GAITRAGRU.TITAS00F TITAS00F  
000360171025	 WHERE ARBCVB='T6'  
000370171025	 AND TASTBL= 'AS' AND  
000380171025	 ARBAAS=TASAAS AND ARBLNP=TASLNP AND ARBNRS=TASNRS AND  
000390171025	ARBNSP=TASNSP  
000400171025	 AND TASCCA= ' '  
000410171025	 UNION  
000420171025	 SELECT TASAAS, TASLNP, TASNRS, TASNSP, TASLNA, TASCBO, TASTBL,  
000430171025	 TASDCM, TASCCA, FIARBT0F.ARBPRU, FIARBT0F.ARBDTV, FIARBT0F.ARBCVB, 
000440171025	 FIARBT0F.ARBIFT  
000450171025	 FROM UNITRAGRU.FIARBT0F FIARBT0F, GAITRAGRU.TITAS10F TITAS10F  
000460171025	 WHERE ARBCVB='T6'  
000470171025	 AND TASTBL= 'AS' AND TASCCA= ' ' AND  
000480171025	 ARBAAS=TASAAS AND ARBLNP=TASLNP AND ARBNRS=TASNRS AND  
000490171025	ARBNSP=TASNSP  
000500171025	 )  
000510171025	 SELECT TASAAS AS ANNO, TASLNP AS "L_PART", TASNRS AS SERIE, TASNSP 
000520171025	 AS "NUM_SPED", TASLNA "L_ARRIVO", TASCBO AS "COD_BOLLA", TASTBL AS  
000530171025	 "TIPO_BOLLA", SEL1.ARBIFT AS "IMP_FAT",  
000540171025	 SEL1.ARBCVB AS "VAR_IN_SF", SEL1.ARBDTV AS "DATA_IN_SF",  
000550171025	 SUBSTR(SEL1.ARBPRU, 1, 9) AS "UTE_VARIAZ",  
000560171025	 FNARBK0F.ARBCVB AS "CAU_STORNO" , FNARBK0F.ARBDTV AS "DAT_STORNO"  
000570171025	 , SUBSTR(FNARBK0F.ARBPRU, 1, 9) AS "UTE_STORNO"  
000580171025	 FROM SEL1 SEL1, UNITRAGRU.FNARBK0F FNARBK0F WHERE FNARBK0F.ARBAAS  
000590171025	 =TASAAS AND FNARBK0F.ARBLNP=  
000600171025	 TASLNP AND FNARBK0F.ARBNRS=TASNRS AND FNARBK0F.ARBNSP=TASNSP  
000610171025	 AND FNARBK0F.ARBCVB='KT'   
000620171025	RCDFMT CONTRFATI  ; 
000630171025  
000640171025LABEL ON TABLE UNITRAGRU.CONTRFATI 
000650171025	IS 'Controlla fatture immediate stornate' ; 
000660171025  
000670171025LABEL ON COLUMN UNITRAGRU.CONTRFATI 
000680171025( ANNO IS 'ANNO                SPED' , 
000690171025	L_PART IS 'P.O                 PARTENZA' , 
000700171025	SERIE IS 'NUM                 SERIE' , 
000710171025	NUM_SPED IS 'NUM.                SPED.' , 
000720171025	L_ARRIVO IS 'P.O                 ARR' , 
000730171025	COD_BOLLA IS 'COD                 BOLLA' , 
000740171025	TIPO_BOLLA IS 'TIP.                BOL.' , 
000750171025	IMP_FAT IS 'IMP.FATTURA' , 
000760171025	VAR_IN_SF IS 'CAU                 VARIAZ              BOLLE' , 
000770171025	DATA_IN_SF IS 'DATA                VARIAZ.' , 
000780171025	CAU_STORNO IS 'CAU                 VARIAZ              BOLLE' , 
000790171025	DAT_STORNO IS 'DATA                VARIAZ.' ) ; 
000800171025  
000810171025LABEL ON COLUMN UNITRAGRU.CONTRFATI 
000820171025( ANNO TEXT IS 'ANNO SPEDIZIONE' , 
000830171025	L_PART TEXT IS 'P.O. PARTENZA' , 
000840171025	SERIE TEXT IS 'NUMERO SERIE' , 
000850171025	NUM_SPED TEXT IS 'NUMERO SPEDIZIONE' , 
000860171025	L_ARRIVO TEXT IS 'P.O. ARRIVO' , 
000870171025	COD_BOLLA TEXT IS 'CODICE BOLLA' , 
000880171025	TIPO_BOLLA TEXT IS 'TIPO BOLLA' , 
000890171025	IMP_FAT TEXT IS 'IMPORTO FATTURA' , 
000900171025	VAR_IN_SF TEXT IS 'CAUS.VARIAZ.BOLLE' , 
000910171025	DATA_IN_SF TEXT IS 'DATA VARIAZIONE' , 
000920171025	CAU_STORNO TEXT IS 'CAUS.VARIAZ.BOLLE' , 
000930171025	DAT_STORNO TEXT IS 'DATA VARIAZIONE' ) ; 
000940171025  
000950171025GRANT ALTER , REFERENCES , SELECT   
000960171025ON UNITRAGRU.CONTRFATI TO QPGMR WITH GRANT OPTION ;
