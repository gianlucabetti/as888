000100990202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990202     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870615     H* OCO304R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500870615     H*        - REVISIONE ASSEGNAZIONE SIMULATA -                   *
000600870615     H*          ---------------------------------                   *
000700870615     H*                                                      AZ.O304 *
000800990202     H*         FILE : tmmoc01L - tmmoc00F - tmsmp01L -              *
000900870615     H*                OCO304D  -                                    *
001000000000     H*                                                              *
001100000000     H*--------------------------------------------------------------*
001200990202     Ftmmoc00F  UF   E             DISK
001300990202     F                                     RENAME(tmmoc000:ORDFIS)
001400870626     F                                     INFDS(INF2)
001500990202     Ftmmoc01L  IF   E           K DISK
001600870626     F                                     INFDS(INF1)
001700990202     Ftmsmp01L  UF   E           K DISK
001800990317     FTII307D   CF   E             WORKSTN
001900870615     F                                     SFILE(SFL1:NRR)
002000871221     D MPHDT1          DS
002100871221     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
002200871221     D  NUMMOV                 1      5  0
002300871221     D  FILL1                  6      8
002400871221     D  DATDTA                 9     14  0
002500871221     D  FILL2                 15     17
002600871221     D  ANNDTA                18     19  0
002700871221     D  FIL03                 20     50
002800871221     D  DGIO                  51     56  0
002900871221     D  FIL04                 57    100
003000871221     D  FILL5                101    300
003100871221     D  GIOTOL               301    302  0
003200871221     D  FILL6                303    512
003300870625     D*-------------
003400870625     D INF1            DS           512
003500870625     D*--- RELREC LOGICO
003600870625     D  NREL1                397    400B 0
003700870626     D INF2            DS           512
003800870626     D*--- RELREC FISICO
003900870626     D  NREL2                397    400B 0
004000870615     D KPJBA         E DS
004100990202     D soc001        E DS                  EXTNAME(xsoc001ds)
004200990202     D xsocds          DS          1000
004300990202     d dataiso         s               d   datfmt(*iso)
004400990202     d dataeur         s               d   datfmt(*eur)
004500870615     C*----------------------------------------------------*
004600870615     C     *ENTRY        PLIST                                                  *
004700870615     C                   PARM                    KPJBA                          *
004800990202     C*---------- RICERCA DITTA :
004900990202     C                   MOVEL     'SOC001'      TIPXSC
005000990202     C                   MOVEL     *blank        SOCXSC
005100990202     C                   EXSR      REPSOC
005200990202     C     RTNXSC        IFNE      '1'
005300990202     C                   MOVEL     XSOCDS        SOC001
005400990202     C                   MOVEL     xscrgs        desdit
005500990202     c                   end
005600990202     C*--- FILE SIMULAZIONE ASSEGNAZIONE 'tmmoc01L'        *
005700870615     C     KEYMOC        KLIST                                                  *
005800870623     C                   KFLD                    VIDCLI                         *
005900870615     C                   KFLD                    VIDNOR                         *
006000990202     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
006100870615     C     KEYSAL        KLIST                                                  *
006200990202     C                   KFLD                    ANNDTA4           4 0          *
006300870615     C                   KFLD                    MOCMAG                         *
006400870615     C                   KFLD                    MOCMAT                         *
006500870615     C*----------------------------------------------------*
006600871221     C     *DTAARA       DEFINE                  MPHDT1                         *
006700871221     C                   IN        MPHDT1
006800870615     C     INIZIO        TAG
006900870616     C*AZZERAMENTO DEI CAMPI A VIDEO
007000990202     c                   z-add     anndta        anndta4
007100990202     c                   if        anndta4 > 40
007200990202     c                   add       1900          anndta4
007300990202     c                   else
007400990202     c                   add       2000          anndta4
007500990202     c                   end
007600870623     C                   MOVEL     *BLANKS       VIDCLI
007700870616     C                   Z-ADD     0             VIDNOR
007800870624     C                   Z-ADD     0             VIDDOR
007900870616     C                   Z-ADD     0             VIDASS
008000870623     C                   MOVEL     *BLANKS       VIDFLS
008100870929     C                   MOVEL     *BLANKS       VIDDES
008200870626     C                   Z-ADD     0             SC1REC
008300870918     C     STAR          TAG
008400870615     C                   EXFMT     OCO304D1
008500990409     C   Kc              GOTO      FINE
008600870616     C     VIDNOR        CABEQ     0             INIZIO
008700990202     C     KEYMOC        SETLL     tmmoc000                               10
008800870630     C  N10              DO
008900870630     C                   SETON                                            50
009000870918     C                   GOTO      STAR
009100870630     C                   END
009200870615     C* PULIZIA SUBFILE
009300870615     C                   EXSR      PULIZ
009400870615     C* CARICAMENTO SUBFILE
009500870615     C                   EXSR      CARIC
009600870918     C     NRR           IFEQ      0
009700870918     C                   SETON                                            51
009800870918     C                   GOTO      STAR
009900870918     C                   END
010000870615     C     EMIS          TAG
010100870615     C                   WRITE     OCO304D2
010200870615     C                   EXFMT     SFL1CT
010300990202     C   Kl              GOTO      STAR
010400871221     C                   EXSR      CONTR
010500990409     C  NKf              GOTO      EMIS
010600990409     C   Kf              EXSR      AGGFIL
010700870616     C*LETTURA DEI RECORD MODIFICATI/AGGIUNTI O ANNULLATI NEL SUBFILE
010800870617     C     FINE          TAG
010900870617     C                   SETON                                        LR
011000870615     C***********************************************************
011100870615     C*ROUTINE DI PULIZIA DEL SUBFILE
011200870615     C     PULIZ         BEGSR
011300870615     C                   SETON                                        70
011400870615     C                   WRITE     SFL1CT
011500870615     C                   SETOFF                                       70
011600870615     C                   ENDSR
011700870615     C***********************************************************
011800870616     C*CARICAMENTO SUBFILE
011900870616     C     CARIC         BEGSR
012000870616     C                   Z-ADD     0             NRR               3 0
012100870616     C                   DO        100
012200870616     C     SU            TAG
012300990202     C     KEYMOC        READE     tmmoc000                               02
012400870616     C   02              GOTO      END1
012500870616     C*SCELTA DI TUTTE LE RIGHE 'OR' SIA ASSEGNATE CHE NON ASSEGNATE
012600870630     C* DI TIPO RIGA 1
012700870616     C     MOCFAS        CABNE     'OR'          SU
012800870630     C     MOCTRG        CABNE     '1'           SU
012900990202     C                   move      MOCDOR        DATAiso
013000990202     C                   move      dataiso       dataeur
013100990202     C                   move      DATAeur       VIDDOR
013200870625     C                   Z-ADD     NREL1         SC1REC
013300870616     C                   MOVEL     MOCDMT        VIDDES
013400870616     C                   Z-ADD     0             VIDASS
013500870623     C                   MOVEL     *BLANKS       VIDFLS
013600870616     C                   ADD       1             NRR
013700870616     C                   WRITE     SFL1
013800870616     C                   END
013900870616     C                   GOTO      SU
014000870616     C     END1          ENDSR
014100870617     C***********************************************************
014200870617     C*ROUTINE DI LETTURA
014300871221     C     CONTR         BEGSR
014400871221     C                   DO        *HIVAL
014500870617     C                   READC     SFL1                                   05
014600871221     C   05              GOTO      END5
014700871221     C     VIDASS        CABEQ     0             SCEL
014800871222     C     VIDASS        CABEQ     999999999     SCEL
014900870617     C     VIDASS        IFGT      MOCQTA
015000870617     C                   SETON                                            72
015100870617     C   72              EXSR      AGGIOR
015200870617     C                   END
015300871221     C** Q,.TA ASSEGN. MAGG. Q,TA ORDINATA
015400871221     C     VIDASS        IFGT      MOCASS
015500870626     C                   Z-ADD     0             RIGASS
015600870623     C     VIDASS        SUB       MOCASS        RIGASS           11 2
015700870617     C                   EXSR      DSPMAG
015800871221     C     MATDSP        IFLT      RIGASS
015900870623     C                   SETON                                          73
016000870623     C   73              EXSR      AGGIOR
016100870626     C                   END
016200871221     C                   END
016300871221     C     SCEL          TAG
016400871221     C                   END
016500871221     C     END5          ENDSR
016600870617     C***********************************************************
016700870622     C*CALCOLO LA DISPONIBILITA DI MAGAZZINO
016800870617     C     DSPMAG        BEGSR
016900990202     C     KEYSAL        CHAIN     tmsmp000                           03
017000870622     C     SMPQGF        SUB       SMPQIM        TOT              11 2
017100870622     C     TOT           SUB       SMPQIP        MATDSP           11 2
017200870617     C                   ENDSR
017300870617     C******************************************************
017400870617     C*AGGIORNAMENTO SUBFILE
017500870617     C     AGGIOR        BEGSR
017600870617     C                   SETON                                        30
017700870617     C                   UPDATE    SFL1
017800870617     C                   SETOFF                                       30
017900870617     C                   GOTO      EMIS
018000870617     C                   ENDSR
018100870617     C******************************************************
018200870617     C*SE IL CAMPO DELLA NUOVA QUANTITA' ASSEGNATA E PIENO DI 999
018300870617     C*ANNULLO TUTTA LA RIGA
018400870623     C     ANNUL         BEGSR
018500990202     C     KEYSAL        CHAIN     tmsmp000                           04
018600870617     C     SMPQIP        SUB       MOCASS        SMPQIP
018700990202     C                   UPDATE    tmsmp000
018800870617     C                   ENDSR
018900870626     C******************************************************
019000870626     C     AGGORD        BEGSR
019100990202     C     KEYSAL        CHAIN     tmsmp000                           11
019200870626     C                   ADD       RIGASS        SMPQIP
019300990202     C                   UPDATE    tmsmp000
019400871221     C** AGGIORNA SALDI
019500871221     C     SC1REC        CHAIN     ORDFIS                             80
019600871221     C                   MOVEL     VIDFLS        MOCFLS
019700871221     C     MOCFLS        IFNE      'S'
019800871221     C     VIDASS        IFEQ      MOCQTA
019900871221     C                   MOVEL     'S'           MOCFLS
020000871221     C                   ELSE
020100871221     C                   MOVEL     *BLANKS       MOCFLS
020200871221     C                   END
020300871221     C                   END
020400871221     C                   Z-ADD     VIDASS        MOCASS
020500871221     C                   UPDATE    ORDFIS
020600871221     C** AGGIORNA ORDINI
020700870626     C                   ENDSR
020800871221     C******************************************************
020900871221     C     AGGFIL        BEGSR
021000871221     C                   DO        *HIVAL        GIRO              5 0
021100871221     C     GIRO          CHAIN     SFL1                               05
021200871221     C   05              GOTO      STAR
021300871221     C     VIDASS        CABEQ     0             END4
021400871221     C     VIDASS        CABEQ     MOCASS        END4
021500871221     C     VIDASS        IFEQ      999999999
021600871221     C                   EXSR      ANNUL
021700871221     C     SC1REC        CHAIN     ORDFIS                             75
021800871221     C                   Z-ADD     0             MOCASS
021900871221     C                   Z-ADD     0             VIDASS
022000871221     C                   MOVEL     *BLANKS       MOCFLS
022100871221     C                   MOVEL     *BLANKS       VIDFLS
022200871221     C                   UPDATE    ORDFIS
022300871221     C                   UPDATE    SFL1
022400871221     C                   GOTO      END4
022500871221     C                   END
022600871221     C** ANNULLAMENTO ASSEGNAZIONE
022700871221     C                   Z-ADD     0             RIGASS
022800871221     C     VIDASS        SUB       MOCASS        RIGASS           11 2
022900871221     C                   EXSR      AGGORD
023000871221     C** AGGIORNAMENTO ASSEGNATO PROVA
023100871221     C     END4          TAG
023200871221     C                   END
023300871221     C                   ENDSR
023400990202     C*----------------------------------------------------*
023500990202     C* Reperimento dati società
023600990202     C*----------------------------------------------------*
023700990202     C     REPSOC        BEGSR
023800990202     C*
023900990202     C                   CALLB     'XSOC'
024000990202     C                   PARM                    TIPXSC            6
024100990202     C                   PARM                    SOCXSC            3
024200990202     C                   PARM                    CDSXSC            9 0
024300990202     C                   PARM                    MODXSC            3
024400990202     C                   PARM      *blanks       RTNXSC            1
024500990202     C                   PARM                    XSOCDS
024600990202     C                   PARM                    KPJBA
024700990202     C*
024800990202     C                   ENDSR
