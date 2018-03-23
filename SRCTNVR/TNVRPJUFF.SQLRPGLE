000100120517     ***************************************************************************
000200120517     **
000300120517     ** Questo programma esegue la ricodifica dell'attributo ufficio utente:
000400120517     **
000500120517     ** - ANUTN00F.UTNUFF
000600120517     ** - YNFMFT00F.UFFICIORIC
000700120517     **
000800120517     ***************************************************************************
000900120517     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')
001000120517
001100120517     ***************************************************************************
001200120517     **
001300120517     ** Campi.
001400120517     **
001500120517     ***************************************************************************
001600120517     D commit          S               N
001700120517
001800120517      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
001900120517
002000120517     D*--------------------------------------------------
002100120517     D* Procedure name: RunUtnProj
002200120517     D* Purpose:        Cambia ANUTN00F.UTNUFF.
002300120517     D* Returns:        Esito.
002400120517     D*--------------------------------------------------
002500120517     D RunUtnProj      PR            10I 0
002600120517
002700120517     D*--------------------------------------------------
002800120517     D* Procedure name: RunFattureMan
002900120517     D* Purpose:        Cambia l'ufficio richiedente sulle fatture manuali.
003000120517     D* Returns:        Esito.
003100120517     D*--------------------------------------------------
003200120517     D RunFattureMan   PR            10I 0
003300120517
003400120517     ***************************************************************************
003500120517     **
003600120517     ** Parametri.
003700120517     **
003800120517     ***************************************************************************
003900120517     C     *ENTRY        PLIST
004000120517     C                   PARM                    commit
004100120517
004200120517      /FREE
004300120517
004400120517       *INLR = *ON;
004500120517
004600120517       IF RunUtnProj() < *ZERO OR RunFattureMan() < *ZERO OR NOT commit;
004700120517         EXEC SQL ROLLBACK;
004800120517         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
004900120517       ELSE;
005000120517         EXEC SQL COMMIT;
005100120517       ENDIF;
005200120517
005300120517      /END-FREE
005400120517
005500120517     P*--------------------------------------------------
005600120517     P* Procedure name: RunUtnProj
005700120517     P* Purpose:        Cambia ANUTN00F.UTNUFF.
005800120517     P* Returns:        Esito.
005900120517     P*--------------------------------------------------
006000120517     P RunUtnProj      B
006100120517     D RunUtnProj      PI            10I 0
006200120517
006300120517     D retField        S             10I 0
006400120517
006500120517      /FREE
006600120517
006700120517       EXEC SQL
006800120517         UPDATE anutn00f
006900120517           SET utnUff = CASE
007000120517                        WHEN utnPrf = 'ACQ001' THEN 'DB' -- Lolli
007100120517                        WHEN utnPrf = 'CDG001' THEN 'DC' -- Marinoni
007200120517                        WHEN utnPrf = 'DIR003' THEN 'DC' -- Cappelletti Teresa
007300120517                        WHEN utnPrf = 'DLO001' THEN 'DD' -- Tontini
007400120517                        WHEN utnPrf = 'EDPLB'  THEN 'DG' -- Bocchi
007500120517                        WHEN utnPrf = 'EDPRB'  THEN 'DG' -- Rossana B.
007600120517                        WHEN utnPrf = 'FIN002' THEN 'PA' -- Pizziolo
007700120517                        WHEN utnPrf = 'GEN001' THEN 'PC' -- Lombardi Stefano
007800120517                        WHEN utnPrf = 'GEN002' THEN 'PB' -- Previato
007900120517                        WHEN utnPrf = 'GEN004' THEN 'DA' -- Mattei
008000120517                        WHEN utnPrf = 'IMP002' THEN 'PD' -- Lambertini
008100120517                        WHEN utnPrf = 'IMP008' THEN 'DB' -- Calamelli
008200120517                        WHEN utnPrf IN ('QPGMR','QUSER') THEN 'PV'
008300120517                        WHEN utnPrf = 'SEG001' THEN 'PT' -- Onofri
008400120517                        WHEN utnPrf = 'SEG005' THEN 'PN' -- Breveglieri
008500120517                        WHEN utnPrf = 'SEG009' THEN 'PS' -- Brighetti
008600120517                        WHEN utnPrf = 'SEG010' THEN 'PE' -- Tugnoli
008700120517                        WHEN utnPrf = 'SEG018' THEN 'PQ' -- Delogu
008800120517                        WHEN LEFT(utnPrf, 3) = 'ACQ' THEN 'PE'
008900120517                        WHEN LEFT(utnPrf, 3) = 'ASS' THEN 'PH'
009000120517                        WHEN LEFT(utnPrf, 3) = 'AUT' THEN 'PN'
009100120517                        WHEN LEFT(utnPrf, 3) = 'BAN' THEN 'RB'
009200120517                        WHEN LEFT(utnPrf, 3) = 'CAS' THEN 'RE'
009300120517                        WHEN LEFT(utnPrf, 3) = 'CDG' THEN 'PG'
009400120517                        WHEN LEFT(utnPrf, 3) = 'CLI' THEN 'RD'
009500120517                        WHEN LEFT(utnPrf, 3) = 'COC' THEN 'PK'
009600120517                        WHEN LEFT(utnPrf, 3) = 'CON' THEN 'RF'
009700120517                        WHEN LEFT(utnPrf, 3) = 'CRE' THEN 'RC'
009800120517                        WHEN LEFT(utnPrf, 3) = 'EDP' THEN 'PV'
009900120517                        WHEN LEFT(utnPrf, 3) = 'FAT' THEN 'RI'
010000120517                        WHEN LEFT(utnPrf, 3) = 'FIN' THEN 'RA'
010100120517                        WHEN LEFT(utnPrf, 3) = 'FOR' THEN 'RH'
010200120517                        WHEN LEFT(utnPrf, 3) = 'GEN' THEN 'RG'
010300120517                        WHEN LEFT(utnPrf, 3) = 'IMP' THEN 'PF'
010400120517                        WHEN LEFT(utnPrf, 3) = 'LEG' THEN 'P0'
010500120517                        WHEN LEFT(utnPrf, 3) IN ('OFB','OFF') THEN 'PN'
010600120517                        WHEN LEFT(utnPrf, 3) = 'PAG' THEN 'PY'
010700120517                        WHEN LEFT(utnPrf, 3) = 'QUA' THEN 'PM'
010800120517                        WHEN LEFT(utnPrf, 3) = 'RAI' THEN 'PM'
010900120517                        WHEN LEFT(utnPrf, 3) = 'RIC' AND UTNUFF = 'SD' THEN 'PO'
011000120517                        WHEN LEFT(utnPrf, 3) = 'RIC' AND UTNUFF = 'SH' THEN 'PZ'
011100120517                        WHEN LEFT(utnPrf, 3) = 'SEG' AND UTNUFF = 'SI' THEN 'PS'
011200120517                        WHEN LEFT(utnPrf, 3) = 'SEG' AND UTNUFF = 'SK' THEN 'PR'
011300120517                        WHEN LEFT(utnPrf, 3) = 'SIC' THEN 'PL'
011400120517                        WHEN LEFT(utnPrf, 3) = 'TLC' THEN 'PX'
011500120517                        WHEN LEFT(utnPrf, 3) = 'TRA' THEN 'PP'
011600120517                        ELSE utnUff
011700120517                        END
011800120518           WHERE utnUff <> 'FI'
011900120517           WITH UR
012000120517       ;
012100120517
012200120517       IF sqlCode < *ZERO;
012300120517         DUMP(A);
012400120517         RETURN sqlCode;
012500120517       ENDIF;
012600120517
012700120517       RETURN retField;
012800120517
012900120517      /END-FREE
013000120517     P RunUtnProj      E
013100120517
013200120517
013300120517     P*--------------------------------------------------
013400120517     P* Procedure name: RunFattureMan
013500120517     P* Purpose:        Cambia l'ufficio richiedente sulle fatture manuali.
013600120517     P* Returns:        Esito.
013700120517     P*--------------------------------------------------
013800120517     P RunFattureMan   B
013900120517     D RunFattureMan   PI            10I 0
014000120517
014100120517     D retField        S             10I 0
014200120517
014300120517      /FREE
014400120517
014500120517       EXEC SQL
014600120517         UPDATE ynfmft00f
014700120517           SET ufficio_richiedente = IFNULL (
014800120517               ( SELECT utnUff FROM anutn00f
014900120517                 WHERE anutn00f.utnPrf = ynfmft00f.utente_inserimento )
015000120517               , ufficio_richiedente )
015100120518           WHERE ufficio_richiedente <> 'FI'
015200120517           WITH UR
015300120517       ;
015400120517
015500120517       IF sqlCode < *ZERO;
015600120517         DUMP(A);
015700120517         RETURN sqlCode;
015800120517       ENDIF;
015900120517
016000120517       // Cancello i filtri delle fatture manuali così il programma propone
016100120517       // il nuovo ID ufficio.
016200120517
016300120517       EXEC SQL
016400120517         DELETE FROM anmpe00f
016500120517           WHERE mpeSoc = '888' AND mpePgm = 'YCO02410R'
016600120517           WITH UR
016700120517       ;
016800120517
016900120517       IF sqlCode < *ZERO;
017000120517         DUMP(A);
017100120517         RETURN sqlCode;
017200120517       ENDIF;
017300120517
017400120517       RETURN retField;
017500120517
017600120517      /END-FREE
017700120517     P RunFattureMan   E
017800120517
