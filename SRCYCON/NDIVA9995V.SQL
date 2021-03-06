000100170205--  Generazione SQL
000200170205--  Versione:                  V7R2M0 140418
000300170205--  Generata su:               06/02/17 16:59:31
000400170205--  Database relazionale:      AS888
000500170205--  Opzioni standard:          DB2 for i
000600170205
000700170205CREATE VIEW PJBARSOC/NDIVA9995V (
000800170205  ID_AREA ,
000900170205  ID_FILIALE ,
001000170205  DESCRIZIONE_FILIALE FOR COLUMN DESCR00001 ,
001100170205  DATA_ESTRAZIONE FOR COLUMN DATA_00001 ,
001200170205  LIBRO_IVA ,
001300170205  DATA_IVA ,
001400170205  NUMERO_IVA ,
001500170205  IMPONIBILE ,
001600170205  IVA ,
001700170205  TOTALE_FATTURA FOR COLUMN TOTAL00001 ,
001800170205  DESCRIZIONE_CLIENTE FOR COLUMN DESCR00002 ,
001900170205  ID_CLIENTE )
002000170205  AS
002100170205  SELECT ORG.ORGCAR AS ID_AREA, ORG.ORGFIL AS ID_FILIALE, ORG.ORGDES AS DESCRIZIONE_FILIALE, CURRENT
002200170205 DATE AS DATA_ESTRAZIONE, IVA.IVALIBRO AS LIBRO_IVA, IVA.IVADTREG AS DATA_IVA, IVA.IVANRREG AS NUMER
002300170205O_IVA, DEC(IVA.IVAIMPONIB, 15, 2) AS IMPONIBILE, DEC(IVA.IVAIMPORTO, 15, 2) AS IVA, DEC(IVA.IVAIMPON
002400170205IB + IVA.IVAIMPORTO, 15, 2) AS TOTALE_FATTURA, MOV.MOVDESBREV AS DESCRIZIONE_CLIENTE, IVA.IVAKSCCP A
002500170205S ID_CLIENTE FROM PJBARSOC/NDIVACGD IVA JOIN PJBARSOC/NDMOV00F MOV ON IVA.IVASYS = MOV.MOVSYS AND IV
002600170205A.IVANRASREG = MOV.MOVNRASREG AND 1 = MOV.MOVNRRIGAM
002700170205  	JOIN GAITRAGRU/AZORG00F ORG ON DEC(IVA.IVAUNITA, 3, 0) = ORG.ORGFIL WHERE IVA.IVASOCIETA = '201'
002800170205AND IVA.IVATPREG = '2' AND SUBSTR(IVA.IVAKSCCP, 5, 4) IN ('8888', '9999') AND IVA.IVADTREG > '2015-1
0029001702052-31' AND IVA.IVAAVERE = 1
003000170205  RCDFMT NDIVA9995V ;
003100170205
003200170205LABEL ON TABLE PJBARSOC/NDIVA9995V
003300170205  IS 'Fatture a clienti non codificati.' ;
003400170205
003500170205LABEL ON COLUMN PJBARSOC/NDIVA9995V
003600170205( ID_FILIALE IS 'COD.FIL.' ,
003700170205  LIBRO_IVA IS 'Lbr                 IVA' ,
003800170205  DATA_IVA IS 'Data                registraz.' ,
003900170205  NUMERO_IVA IS 'Numero              registraz.' ,
004000170205  DESCRIZIONE_CLIENTE IS 'Descrizione         riga' ,
004100170205  ID_CLIENTE IS 'S/C                 ctp' ) ;
004200170205
004300170205LABEL ON COLUMN PJBARSOC/NDIVA9995V
004400170205( ID_AREA TEXT IS 'COD.AREA' ,
004500170205  ID_FILIALE TEXT IS 'COD.FILIALE' ,
004600170205  DESCRIZIONE_FILIALE TEXT IS 'DESCRIZIONE FILIALE' ,
004700170205  LIBRO_IVA TEXT IS 'Libro IVA ..................' ,
004800170205  DATA_IVA TEXT IS 'Data registrazione .........' ,
004900170205  NUMERO_IVA TEXT IS 'Numero registrazione .......' ,
005000170205  DESCRIZIONE_CLIENTE TEXT IS 'Descrizione di riga ........' ,
005100170205  ID_CLIENTE TEXT IS 'Sottoconto contropartita ...' ) ;
005200170205
005300170205GRANT SELECT
005400170205ON PJBARSOC/NDIVA9995V TO PUBLIC ;
005500170205
005600170205GRANT ALTER , REFERENCES , SELECT
005700170205ON PJBARSOC/NDIVA9995V TO QPGMR WITH GRANT OPTION ;
005800170205
