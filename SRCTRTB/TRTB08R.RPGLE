000100991119     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200991119     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300891030     F*--------------------------------------------------------------*
000400891108     F* GESTIONE TABELLA CONTROPARTITE PER FATTURAZIONE MAGAZZINO    *
000500891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000600891030     F*  01        INSERIMENTO                                       *
000700891030     F*  02        VARIAZIONE                                        *
000800891107     F*  29        CODICE OBBLIGATORIO                               *
000900891030     F*  30        DESCRIZIONE OBBLIGATORIA                          *
001000891108     F*  31        CAPO CONTO IVA ERRATO                             *
001100891108     F*  32        CONTO IVA ERRATO                                  *
001200891108     F*  33        CONTO CONTROPART. ERRATO                          *
001300891108     F*  34        CENTRO DI COSTO ERRATO                            *
001400891108     F*  79        COMANDO DI ANNULLAMENTO CONSENTITO SOLO DA EDP    *
001500891030     F*--------------------------------------------------------------*
001600891108     FTRTB08D   CF   E             WORKSTN
001700991119     FANPDC01L  IF   E           K DISK
001800991119     FANENT01L  IF   E           K DISK
001900891030     FTABEL00F  UF A E           K DISK
002000891108     D DSCP          E DS
002100891107     D TCUDS           DS
002200891107     D  F1                     1      1
002300891107     D  F3                     3      3
002400891107     D  F2                     2      2
002500891107     D  F4                     4      4
002600891107     D  F56                    5      6
002700891030     D KPJBA         E DS
002800971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
002900991119     D SOC001        E DS                  EXTNAME(XSOC001DS)
003000991122     D XSOCDS          DS          1000
003100891030     I*--------------------------------------------------------------*
003200891030     I*                        I N P U T                             *
003300891030     I*--------------------------------------------------------------*
003400891030     C******************************************************
003500891030     C     *ENTRY        PLIST
003600891030     C                   PARM                    KPJBA
003700991119     C                   Z-ADD     1             CODUT             1 0
003800991119     C*---------- RICERCA DITTA :
003900991119     C                   MOVEL     'SOC001'      TIPXSC
004000991119     C                   MOVEL     *BLANK        SOCXSC
004100991119     C                   EXSR      REPSOC
004200991119     C     RTNXSC        IFNE      '1'
004300991119     C                   MOVEL     XSOCDS        SOC001
004400991119     C                   ENDIF
004500991119     C                   MOVEL     XSCRGS        RSUT
004600991119     C                   MOVEL     XSCSOC        SOC
004700991119     C*                                                    *
004800891030     C******************************************************
004900891030     C* POSIZIONAMENTO TABELLE                             *
005000891107     C     KTAB          KLIST
005100891030     C                   KFLD                    §KUT
005200891030     C                   KFLD                    §COD
005300891030     C                   KFLD                    §KEY
005400991119     C     KPDC          KLIST
005500991119     C                   KFLD                    SOC
005600991119     C                   KFLD                    §CPKCC
005700991119     C                   KFLD                    §CPKSC
005800991119     C     KCDC          KLIST
005900991119     C                   KFLD                    SOC               3
006000991119     C                   KFLD                    RIF               3
006100991119     C                   KFLD                    §CPCDC
006200991119     C                   MOVE      'CDC'         RIF
006300891030     C******************************************************
006400891030     C     *LIKE         DEFINE    TBLKUT        §KUT
006500891030     C     *LIKE         DEFINE    TBLCOD        §COD
006600891030     C     *LIKE         DEFINE    TBLKEY        §KEY
006700971210     C******************************************************
006800971210     C* Richiamo pgm per controllo modificabilità tabella
006900971210     C                   CLEAR                   TRUL12
007000971210     C                   MOVEL     'L'           I12TLA
007100971210     C                   MOVEL     KNSIF         I12NSI
007200971210     C                   MOVEL     'CP'          I12COD
007300971210     C                   CALL      'TRUL12R'
007400971210     C                   PARM                    TRUL12
007500971210     C     O12ERR        IFEQ      '1'
007600971210     C                   GOTO      FINE
007700971210     C                   END
007800971210     C******************************************************
007900891102     C     STAR          TAG
008000891102     C                   SETOFF                                       79
008100991119     C                   MOVEL     XSCCUT        PROF              3
008200891102     C     PROF          IFNE      'EDP'
008300891102     C                   SETON                                        79
008400891102     C                   END
008500891102     C                   MOVEL     *BLANKS       §DES             25
008600891107     C                   MOVEL     *BLANKS       VCOD
008700891107     C                   MOVEL     '?'           VCOD
008800891030     C     INIZIO        TAG
008900891108     C                   EXFMT     TB08D1
009000891030     C   KL              GOTO      FINE
009100891107     C     VCOD          IFEQ      *BLANKS
009200891030     C                   SETON                                        29
009300891030     C                   GOTO      INIZIO
009400891030     C                   END
009500891030     C*§  RICERCA IN TABEL00F
009600891107     C     VCOD          IFEQ      '?'
009700891030     C                   MOVEL     CODUT         §KUT
009800891108     C                   MOVEL     'CP'          §COD
009900891107     C                   MOVE      *ZEROS        VCOD
010000891030     C                   CALL      'X§TABER'
010100891030     C                   PARM                    §KUT
010200891030     C                   PARM                    §COD
010300891030     C                   PARM                    §KEY
010400891030     C                   PARM                    §DES
010500891107     C                   MOVEL     §KEY          VCOD
010600891030     C                   GOTO      INIZIO
010700891102     C                   ELSE
010800891102     C                   MOVEL     CODUT         §KUT
010900891108     C                   MOVEL     'CP'          §COD
011000891107     C                   MOVEL     VCOD          §KEY
011100891030     C                   END
011200891030     C                   SETOFF                                       0102
011300891107     C                   EXSR      PULVID
011400891107     C     KTAB          CHAIN     TABEL                              80
011500891030     C     *IN80         IFEQ      '1'
011600891030     C                   SETON                                        01
011700891030     C                   ELSE
011800891108     C                   MOVEL     TBLUNI        DSCP
011900891110     C                   EXSR      RICDES
012000900604     C     TBLFLG        IFEQ      '*'
012100900604     C                   SETON                                        03
012200900604     C                   ELSE
012300891030     C                   SETON                                        02
012400900604     C                   END
012500891030     C                   END
012600891030     C     SU            TAG
012700891030     C                   SETOFF                                       99
012800891108     C                   EXFMT     TB08D2
012900891030     C   KL              GOTO      FINE
013000891030     C   KD              GOTO      INIZIO
013100891030     C**--- ANNULLAMENTO
013200900604     C   KQ              DO
013300900605     C                   MOVEL     '*'           TBLFLG
013400900605     C                   UPDATE    TABEL
013500891030     C                   GOTO      INIZIO
013600891030     C                   END
013700900605     C** RIPRISTINO
013800900605     C   KE              DO
013900900605     C                   MOVEL     *BLANKS       TBLFLG
014000900605     C                   UPDATE    TABEL
014100900605     C                   GOTO      INIZIO
014200900605     C                   END
014300900605     C*
014400891102     C                   EXSR      CTRFMT
014500891102     C   99              GOTO      SU
014600891030     C                   EXSR      SCRIVI
014700891102     C                   GOTO      STAR
014800891030     C     FINE          TAG
014900891030     C                   SETON                                        LR
015000891030     C**------------------------
015100891030     C** PULISCI CAMPI VIDEO
015200891030     C**------------------------
015300891030     C     PULVID        BEGSR
015400891108     C                   MOVEL     *BLANKS       §CPDES
015500991119     C                   MOVE      *ZEROS        §CPKSC
015600991119     C                   MOVE      *ZEROS        §CPKCC
015700991119     C                   Z-ADD     0             §CPALI
015800991119     C                   MOVE      *BLANK        §CPESE
015900991119     C                   MOVE      *BLANK        §CPVOC
016000891110     C                   MOVEL     *BLANKS       DESCCP
016100891110     C                   MOVEL     *BLANKS       DESCDC
016200891030     C                   ENDSR
016300891030     C**------------------------
016400891030     C** CONTROLLI 2 VIDEATA
016500891030     C**------------------------
016600891030     C     CTRFMT        BEGSR
016700891107     C**--- DESCRIZIONE
016800891108     C     §CPDES        IFEQ      *BLANKS
016900891107     C                   SETON                                        3099
017000891030     C                   GOTO      END1
017100891030     C                   END
017200891108     C**--- CONTROPART.GENERALE
017300991119     C     §CPKSC        IFEQ      *zeros
017400991119     C     §CPKSC        oreq      *blank
017500891110     C                   SETON                                        3399
017600891110     C                   GOTO      END1
017700891110     C                   ELSE
017800991119     C     KPDC          CHAIN     ANPDC000                           33
017900891108     C   33              DO
018000891107     C                   SETON                                        99
018100891107     C                   GOTO      END1
018200891107     C                   END
018300991119     C                   MOVEL     PDCDEScr      DESCCP
018400891110     C                   END
018500891108     C**--- CENTRO DI COSTO
018600991119     C     §CPCDC        IFEQ      *blank
018700891110     C                   SETON                                        3499
018800891110     C                   GOTO      END1
018900891110     C                   ELSE
019000991119     C     KCDC          CHAIN     ANENT01L                           34
019100891110     C   34              DO
019200891110     C                   SETON                                        99
019300891110     C                   GOTO      END1
019400891110     C                   END
019500991119     C                   MOVEL     ENTDEScr      DESCDC
019600891110     C                   END
019700891030     C     END1          ENDSR
019800891030     C**----------------------------
019900891030     C** AGGIORNAMENTO / INSERIMENTO
020000891030     C**----------------------------
020100891030     C     SCRIVI        BEGSR
020200891107     C     KTAB          CHAIN     TABEL                              80
020300891030     C                   MOVEL     §KUT          TBLKUT
020400891030     C                   MOVEL     §COD          TBLCOD
020500891030     C                   MOVEL     §KEY          TBLKEY
020600891108     C                   MOVEL     DSCP          TBLUNI
020700900420     C*§
020800900420     C                   MOVE      *BLANK        TBLFTT
020900900420     C                   MOVE      *ZERO         TBLFLT
021000900420     C                   MOVE      *BLANK        TBLFTR
021100900420     C                   MOVE      *ZERO         TBLDTR
021200900420     C*§
021300891102     C   80              WRITE     TABEL                                98
021400891102     C  N80              UPDATE    TABEL                                98
021500891110     C                   ENDSR
021600891110     C**----------------------------
021700891110     C** RICERCA DESCRIZIONI CONTI SUL
021800891110     C** PIANO DEI CONTI
021900891110     C**----------------------------
022000891110     C     RICDES        BEGSR
022100991119     C     KPDC          CHAIN     ANPDC000                           85
022200991119     C  N85              MOVEL     PDCDEScr      DESCCP
022300891110     C**--- DESCRIZIONE CONTO CENTRO DI COSTO
022400991119     C     KCDC          CHAIN     ANENT01L                           85
022500991119     C  N85              MOVEL     ENTDEScr      DESCDC
022600891110     C                   ENDSR
022700991119     C*----------------------------------------------------*
022800991119     C* Reperimento dati società
022900991119     C*----------------------------------------------------*
023000991119     C     REPSOC        BEGSR
023100991119     C*
023200991119     C                   CALLB     'XSOC'
023300991119     C                   PARM                    TIPXSC            6
023400991119     C                   PARM                    SOCXSC            3
023500991119     C                   PARM                    CDSXSC            9 0
023600991119     C                   PARM                    MODXSC            3
023700991119     C                   PARM      *BLANKS       RTNXSC            1
023800991119     C                   PARM                    XSOCDS
023900991119     C                   PARM                    KPJBA
024000991119     C*
024100991119     C                   ENDSR
