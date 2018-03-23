000100990205     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990205     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* OFN800R *-OF/38-*--------------------------------------------*
000400000000     F*-----------------*                                            *
000500000000     F*               - LANCIA STAMPA ORDINI FORNITORI -             *
000600000000     F*               ----------------------------------  AZ.:N800   *
000700000000     F*                                                              *
000800990205     F*         FILE : tmfop01L - tmmof01L - OFN800D                 *
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R - RICERCA DESCRIZIONE DITTA           *
001100990317     F*                tii103R - RICERCA ALFABETICA PARTIC.FORNIT.   *
001200000000     F*                                                              *
001300000000     F*         AZN. : N80A    - STAMPA ORDINI USO INTERNO           *
001400850520     F*                N80B    - STAMPA ORDINI SU MODULO -OFMOD01-   *
001500860605     F*                                                              *
001600860605     F*--------------------------------------------------------------*
001700860605     F* SE IL PROGRAMMA E' RICHIAMATO ESEGUE LA STAMPA IN AMBIENTE   *
001800860605     F* INTERATTIVO ALTRIMENTI SOTTOMETTE AZIONI IN AMBIENTE BATCH   *
001900000000     F*--------------------------------------------------------------*
002000990205     Ftmfop01L  IF   E           K DISK
002100990205     Ftmmof01L  IF   E           K DISK
002200000000     FANTAB00F  IF   E           K DISK
002300990317     FTII405D   CF   E             WORKSTN
002400000000     F                                     SFILE(SFL1:REC1)
002500000000     D PARSTA          DS
002600000000     D*--- PARAMETRI PER STAMPA ORDINE :
002700990205     D  CODSTA                 1      8
002800990205     D  NUMSTA                 9     13  0
002900990205     D  TPRSTA                14     14
003000990205     D  FASSTA                15     16
003100990205     D  socpas                17     19
003200000000     D KPJBA         E DS
003300860605     D*------------- PASSAGGIO PARAMETRI :
003400990205     D  CODFOR               247    254
003500990205     D  NOMFOR               255    284
003600990205     D  ORDNUM               285    289
003700990205     D  TIPRIG               290    290
003800990205     D  TIPSTA               291    292
003900990205     D  STFABB               293    294
004000000000     D  MSGERR               453    502
004100990205     D soc001        E DS                  EXTNAME(xsoc001ds)
004200990205     D xsocds          DS          1000
004300990205     d dataiso         s               d   datfmt(*iso)
004400990205     d dataeur         s               d   datfmt(*eur)
004500000000     C*----------------------------------------------------*
004600000000     C     *ENTRY        PLIST                                                  *
004700000000     C                   PARM                    KPJBA                          *
004800000000     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F"              *
004900000000     C     KEYANT        KLIST                                                  *
005000000000     C                   KFLD                    TABGRU                         *
005100000000     C                   KFLD                    TABCOD                         *
005200990205     C*--- FILE ORDINI FORNIT. "tmmof01L" X ORDINE ERRATO  *
005300850118     C     KEYERR        KLIST                                                  *
005400000000     C                   KFLD                    CODFOR                         *
005500850118     C                   KFLD                    NUMORD                         *
005600990205     C*--- FILE ORDINI FORNIT. "tmmof01L" X SUBFILE        *
005700850118     C     KEYORD        KLIST                                                  *
005800850118     C                   KFLD                    CODFOR                         *
005900850118     C                   KFLD                    MOFNOR                         *
006000000000     C*--- RICERCA DITTA ----------------------------------*
006100990205     C                   MOVEL     'SOC001'      TIPXSC
006200990205     C                   MOVEL     *blank        SOCXSC
006300990205     C                   EXSR      REPSOC
006400990205     C     RTNXSC        IFNE      '1'
006500990205     C                   MOVEL     XSOCDS        SOC001
006600990205     C                   MOVEL     xscrgs        desdit
006700990205     C                   MOVEL     xscsoc        wsoc              3
006800990205     C                   MOVEL     xscsoc        socpas            3
006900990205     c                   end
007000860605     C*----------------------------------------------------*
007100000000     C     INIZIO        TAG                                                    *
007200000000     C                   SETOFF                                           10    *
007300000000     C     ORDNUM        IFEQ      *BLANKS                                      *
007400000000     C                   Z-ADD     0             NUMORD                         *
007500000000     C                   ELSE                                                   *
007600000000     C                   MOVE      ORDNUM        NUMORD                         *
007700990205     C                   MOVE      'R'           RICHIA            1            *RICHIAMATO
007800000000     C                   END                                                    *
007900000000     C     TIPRIG        IFEQ      *BLANKS                                      *
008000000000     C                   MOVE      'C'           TIPRIG                         *
008100000000     C                   END                                                    *
008200000000     C     TIPSTA        IFEQ      *BLANKS                                      *
008300871117     C                   MOVE      'SI'          TIPSTA                         *
008400000000     C                   END                                                    *
008500860605     C     STFABB        IFEQ      *BLANKS                                      *
008600860605     C                   MOVE      'NO'          STFABB                         *
008700860605     C                   END                                                    *
008800000000     C                   WRITE     OFN800TS                                     *
008900000000     C                   EXFMT     OFN800PT                                     *
009000990409     C   Kc              GOTO      FINE                                         *
009100850118     C                   MOVE      TIPSTA        TIPSAV            2            *
009200860605     C                   MOVE      STFABB        STFSAV            2            *
009300000000     C                   MOVEA     *ZEROS        *IN(1)                         *
009400860605     C*--- RICH. STAMPA ORDINI DI TUTTI I FORNITORI -------*
009500990205     C     CODFOR        IFEQ      '99999999'                                   *
009600000000     C                   EXSR      SUBMIT                                       *
009700000000     C                   GOTO      FINE                                         *
009800000000     C                   END                                                    *
009900000000     C*----------                                          *
010000000000     C     CODFOR        IFEQ      *BLANKS                                      *
010100000000     C     NOMFOR        CABEQ     *BLANKS       INIZIO                   91    *
010200000000     C                   END                                                    *
010300000000     C*---------- RICERCA FORNITORE :                      *
010400990205     C     CODFOR        IFEQ      '?       '                                   *
010500000000     C                   MOVE      *BLANKS       CODFOR                         *
010600990317     C                   CALL      'TII103R'                                    *
010700000000     C                   PARM                    CODFOR                         *
010800000000     C                   PARM                    NOMFOR                         *
010900990205     C     CODFOR        IFEQ      '99999999'                                   *
011000000000     C                   MOVE      *BLANKS       CODFOR                         *
011100000000     C                   SETON                                        91        *
011200000000     C                   END                                                    *
011300000000     C                   GOTO      INIZIO                                       *
011400000000     C                   END                                                    *
011500000000     C*---------- RICERCA PER CODICE FORNITORE :           *
011600000000     C                   MOVE      *BLANKS       NOMFOR                         *
011700990205     C     CODFOR        CHAIN     tmfop01L                           91        *
011800000000     C   91              GOTO      INIZIO                                       *
011900000000     C                   MOVE      FOPRSI        NOMFOR                         *
012000850118     C*--- CONTROLLO NUMERO ORDINE RICHIESTO               *
012100850118     C     NUMORD        CABEQ     *ZEROS        NONORD                         *
012200850118     C     NUMORD        CABEQ     99999         NONORD                         *
012300990205     C     KEYERR        SETLL     tmmof01L                               11    *
012400850118     C  N11              SETON                                            94    *
012500850118     C   94              GOTO      INIZIO                                       *
012600850118     C     NONORD        TAG                                                    *
012700850118     C*--- CONTROLLO ESISTENZA ORDINI DEL FORNITORE        *
012800990205     C     CODFOR        SETLL     tmmof01L                               11    *
012900000000     C  N11              SETON                                            93    *
013000000000     C   93              GOTO      INIZIO                                       *
013100000000     C*--- RICH. STAMPA DI TUTTI GLI ORDINI DEL FORNITORE :*
013200000000     C*--- RICH. STAMPA DI UN ORDINE DA PROMPT :           *
013300000000     C     NUMORD        IFNE      *ZEROS                                       *
013400000000     C                   EXSR      SUBMIT                                       *
013500000000     C                   GOTO      FINE                                         *
013600000000     C                   END                                                    *
013700000000     C*---------- RICERCA ORDINI ATTIVI -------------------*
013800000000     C*---------- PULIZIA SUBFILE :                        *
013900000000     C                   Z-ADD     0             REC1              3 0          *
014000000000     C                   SETON                                        70        *
014100000000     C                   WRITE     SFL1CT                                       *
014200000000     C                   SETOFF                                       70        *
014300000000     C*--- RICERCA ORDINI ---------------------------------*
014400000000     C     SUORDI        TAG                                                    *
014500000000     C*          -------------                             *
014600990205     C     CODFOR        READE     tmmof01L                               11    *
014700000000     C   11              GOTO      ESPTAG                                       *
014800000000     C     REC1          ADD       1             REC1                           *
014900990205     C                   move      MOFDOR        DATAIso                        *
015000990205     C                   move      dataiso       dataeur                        *
015100990205     C                   move      DATAeur       DATaiso                        *
015200000000     C                   MOVE      '162'         TABGRU                         *
015300000000     C                   MOVE      *ZEROS        TABCOD                         *
015400000000     C                   MOVE      MOFTOR        TABCOD                         *
015500000000     C     KEYANT        CHAIN     ANTAB00F                           11        *
015600000000     C   11              MOVEL     'ERRORE'      DESTOR                         *
015700000000     C  N11              MOVEL     TABDES        DESTOR                         *
015800990205     C                   move      MOFDCN        DATAiso                        *
015900990205     C                   move      dataiso       dataeur                        *
016000990205     C                   move      DATAeur       DATCON                         *
016100000000     C*--- SCRIVO ORDINI ----------------------------------*
016200000000     C                   WRITE     SFL1                                         *
016300000000     C                   SETON                                        7110      *
016400990205     C     KEYORD        SETGT     tmmof01L                           11        *
016500000000     C  N11              GOTO      SUORDI                                       *
016600000000     C     ESPTAG        TAG                                                    *
016700000000     C*          -------------                             *
016800000000     C                   WRITE     OFN800TS                                     *
016900000000     C                   EXFMT     SFL1CT                                       *
017000990205     C   Kl              GOTO      INIZIO                                       *
017100000000     C     SUREAD        TAG                                                    *
017200000000     C*          -------------                             *
017300000000     C                   READC     SFL1                                   12    *
017400000000     C   12              GOTO      FINE                                         *
017500000000     C  N12              DO                                                     *
017600000000     C     SCELTA        CABEQ     ' '           SUREAD                         *
017700000000     C                   Z-ADD     MOFNOR        NUMORD                         *
017800000000     C     SCELTA        IFEQ      'C'                                          *
017900000000     C                   MOVE      'C'           TIPRIG                         *
018000000000     C                   END                                                    *
018100000000     C     SCELTA        IFEQ      'A'                                          *
018200000000     C                   MOVE      'A'           TIPRIG                         *
018300000000     C                   END                                                    *
018400000000     C                   EXSR      SUBMIT                                       *
018500000000     C                   GOTO      SUREAD                                       *
018600000000     C                   END                                                    *
018700000000     C*--- ERRORE X DITTA INESISTENTE ---------------------*
018800000000     C     FINERR        TAG                                                    *
018900000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
019000850114     C*---------- FINE LAVORO :                            *
019100850114     C     FINE          TAG                                                    *
019200000000     C                   SETON                                            LR    *
019300000000     C*--------- LANCIO STAMPA ORDINI ---------------------*
019400000000     C*----------------------------------------------------*
019500000000     CSR   SUBMIT        BEGSR                                                  *
019600000000     C                   MOVE      CODFOR        CODSTA                         *
019700000000     C                   MOVE      NUMORD        NUMSTA                         *
019800000000     C                   MOVE      TIPRIG        TPRSTA                         *
019900860605     C                   MOVE      STFSAV        FASSTA                         *
020000850114     C                   MOVE      *BLANKS       KPJBU                          *
020100000000     C                   MOVEL     PARSTA        KPJBU                          *
020200860605     C*--------------- INTERATTIVO ------------------------*
020300860605     C     RICHIA        IFEQ      'R'                                          *
020400860605     C     TIPSAV        IFEQ      'SI'                                         *
020500990317     C                   CALL      'TII404R'                                    *LETT.FACIL.
020600860605     C                   PARM                    KPJBA                          *
020700860605     C                   ELSE                                                   *
020800990317     C* non utilizzato   CALL      'OFN80BR'                                    *MODULO
020900990317     C*                  PARM                    KPJBA                          *
021000860605     C                   END                                                    *
021100860605     C*--------------- BATCH ------------------------------*
021200860605     C                   ELSE                                                   *
021300860605     C     TIPSAV        IFEQ      'SI'                                         *
021400990317     C                   MOVE      'I404'        KCOAZ                          *LETT.FACIL.
021500860605     C                   ELSE                                                   *
021600990317     C*                  MOVE      'N80B'        KCOAZ                          *MODULO
021700860605     C                   END                                                    *
021800860605     C*----------                                          *
021900990205     C     CODFOR        IFEQ      '99999999'                                   *
022000900109     C                   MOVEL     'KSNOTTE'     KQEBI                          *
022100860605     C                   ELSE                                                   *
022200900109     C                   MOVEL     'KSDIFFE'     KQEBI                          *
022300860605     C                   END                                                    *
022400860605     C*----------                                          *
022500860605     C  N13              DO                                                     *
022600860605     C                   CALL      'BCH09'                                      *
022700860605     C                   PARM                    KPJBA                          *
022800860605     C                   SETON                                        13        *
022900860605     C                   END                                                    *
023000860605     C*----------                                          *
023100860605     C                   CALL      'BCH10'                                      *BATCH
023200000000     C                   PARM                    KPJBA                          *
023300860605     C                   END                                                    *
023400000000     CSR                 ENDSR                                                  *
023500990205     C*----------------------------------------------------*
023600990205     C* Reperimento dati società
023700990205     C*----------------------------------------------------*
023800990205     C     REPSOC        BEGSR
023900990205     C*
024000990205     C                   CALLB     'XSOC'
024100990205     C                   PARM                    TIPXSC            6
024200990205     C                   PARM                    SOCXSC            3
024300990205     C                   PARM                    CDSXSC            9 0
024400990205     C                   PARM                    MODXSC            3
024500990205     C                   PARM      *blanks       RTNXSC            1
024600990205     C                   PARM                    XSOCDS
024700990205     C                   PARM                    KPJBA
024800990205     C*
024900990205     C                   ENDSR
