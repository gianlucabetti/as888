000100990203     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* OCO700R  *-OC/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*     - GESTIONE SCONTI X CLIENTE / CLASSE MERCEOLOGICA -      *
000600000000     F*       ------------------------------------------------       *
000700000000     F*                                                              *
000800990203     F*         FILE : tmclp01L - tmscm01L - ANTAB00F                *
000900000000     F*                OCO700D -                                     *
001000000000     F*--------------------------------------------------------------*
001100990203     Ftmclp01L  IF   E           K DISK
001200990203     Ftmscm01L  UF A E           K DISK
001300990203     Fancln01l  IF   E           K DISK
001400990203     Fansog01l  IF   E           K DISK
001500000000     FANTAB00F  IF   E           K DISK
001600990317     FTII315D   CF   E             WORKSTN
001700000000     F                                     SFILE(SFL1:NRR)
001800000000     D KPJBA         E DS
001900990203     D soc001        E DS                  EXTNAME(xsoc001ds)
002000990203     D xsocds          DS          1000
002100000000     I*---- PASSAGGIO PARAMETRI
002200000000     C*----------------------------------------------------*
002300000000     C     *ENTRY        PLIST                                                  *
002400000000     C                   PARM                    KPJBA                          *
002500990203     C*---------- RICERCA DITTA :
002600990203     C                   MOVEL     'SOC001'      TIPXSC
002700990203     C                   MOVEL     *blank        SOCXSC
002800990203     C                   EXSR      REPSOC
002900990203     C     RTNXSC        IFNE      '1'
003000990203     C                   MOVEL     XSOCDS        SOC001
003100990203     C                   MOVEL     xscsoc        wsoc              3
003200990203     c                   end
003300870924     C******************************************************
003400990203     C*--- RICERCA CLIENTE SUL FILES 'tmscm01L'            *
003500870924     C     KEYCLI        KLIST                                                  *
003600990203     C                   KFLD                    CODCLI                         *
003700870924     C                   KFLD                    VIDCLM                         *
003800870924     C*--- RICERCA RAS.SOCIALE CLIENTE     'CNACO00F       *
003900990203     C     KEYcln        KLIST                                                  *
004000990203     C                   KFLD                    wsoc                           *
004100990203     C                   KFLD                    codcli                         *
004200870924     C*--- RICERCA CLASSE SUL FIL TABELLE 'ANTAB00F'       *
004300870924     C     KEYCL         KLIST                                                  *
004400870924     C                   KFLD                    GRU               3            *
004500870924     C                   KFLD                    COD               7            *
004600870924     C                   MOVEL     '155'         GRU                            *
004700870924     C*----------------------------------------------------*
004800990203     C                   MOVE      '?       '    CODCLI
004900000000     C                   MOVE      *BLANKS       DESCLI
005000000000     C*AZZERAMENTO DEI CAMPI A VIDEO
005100000000     C                   MOVEL     *BLANKS       VIDCLM
005200000000     C                   Z-ADD     0             VIDSC1
005300000000     C                   Z-ADD     0             VIDSC2
005400000000     C                   MOVEL     *BLANKS       SCELTA
005500000000     C*
005600000000     C     INIZIO        TAG
005700000000     C                   WRITE     ANG8201
005800000000     C   91
005900000000     COR 92              SETON                                            90
006000000000     C                   EXFMT     ANG8202
006100000000     C                   MOVEA     *ZEROS        *IN(11)
006200990409     C   Kc              GOTO      FINE
006300000000     C*---------- CONTROLLO DATI INPUT :
006400000000     C     CODCLI        CABEQ     *BLANKS       INIZIO                   91
006500000000     C*---------- RICERCA ALFABETICA :
006600990203     C     CODCLI        IFEQ      '?       '
006700000000     C                   MOVE      *BLANKS       CODCLI
006800990317     C                   CALL      'TII119R'
006900000000     C                   PARM                    CODCLI
007000000000     C                   PARM                    DESCLI
007100000000     C                   GOTO      INIZIO
007200000000     C                   END
007300000000     C* CONTROLLO PRESENZA CLIENTE SUI FILES
007400990203     C     CODCLI        CHAIN     tmclp01L                           10
007500000000     C     *IN(10)       CABEQ     '1'           INIZIO                   92
007600870924     C* AGGANCIO DELLA RAGIONE SOCIALE SUL PIANO DEI CONTI
007700990203     C     KEYcln        CHAIN     ancln01l                           20
007800990203     C     clnsogg       CHAIN     ansog01l                           20
007900990203     C  N20              MOVEL     sogdes        VIDDCL
008000870924     C   20              MOVEL     *BLANKS       VIDDCL
008100870924     C* POSIZIONAMENTO SUL FILE SCONTI
008200990203     C     CODCLI        SETLL     tmscm000
008300000000     C* PULIZIA SUBFILE
008400000000     C                   EXSR      PULIZ
008500000000     C*
008600000000     C* CARICAMENTO SUBFILE
008700000000     C                   EXSR      CARIC
008800000000     C     EMIS          TAG
008900000000     C                   WRITE     OCO700D1
009000000000     C                   EXFMT     SFL1CT
009100990203     C   Kl              GOTO      INIZIO
009200000000     C*LETTURA DEI RECORD MODIFICATI/AGGIUNTI O ANNULLATI NEL SUBFILE
009300000000     C                   EXSR      LET2
009400000000     C                   GOTO      INIZIO
009500000000     C     FINE          TAG
009600000000     C                   SETON                                        LR
009700870924     C******************************************************
009800000000     C     PULIZ         BEGSR
009900870924     C**----------------------
010000870924     C** PULIZIA DEL SUBFILE
010100870924     C**----------------------
010200000000     C                   SETON                                        70
010300000000     C                   WRITE     SFL1CT
010400000000     C                   SETOFF                                       70
010500000000     C                   ENDSR
010600870924     C******************************************************
010700000000     C     CARIC         BEGSR
010800870924     C**----------------------
010900870924     C** CARICAMENTO SUBFILE
011000870924     C** CARICO RECORDS VUOTI
011100870924     C**----------------------
011200871218     C                   Z-ADD     0             NRR               4 0
011300000000     C     LET           TAG
011400990203     C     CODCLI        READE     tmscm000                               12
011500000000     C     *IN(12)       IFEQ      '1'
011600000000     C                   DO        60
011700000000     C                   MOVEL     *BLANKS       VIDCLM
011800870924     C                   MOVEL     *BLANKS       DESCLM
011900000000     C                   Z-ADD     0             VIDSC1
012000000000     C                   Z-ADD     0             VIDSC2
012100000000     C                   ADD       1             NRR
012200000000     C                   WRITE     SFL1
012300000000     C                   END
012400000000     C                   GOTO      EMIS
012500000000     C                   END
012600870924     C**------------------------
012700870924     C** CARICO RECORDS DA FILE
012800870924     C**------------------------
012900000000     C                   MOVEL     SCCCLM        VIDCLM
013000870924     C                   MOVEL     *ZEROS        COD
013100870924     C                   MOVE      VIDCLM        COD
013200870924     C     KEYCL         CHAIN     ANTAB000                           65
013300870924     C  N65              MOVEL     TABDES        DESCLM
013400000000     C                   Z-ADD     SCCSC1        VIDSC1
013500000000     C                   Z-ADD     SCCSC2        VIDSC2
013600000000     C                   ADD       1             NRR
013700000000     C                   WRITE     SFL1
013800000000     C                   GOTO      LET
013900000000     C                   ENDSR
014000870924     C******************************************************
014100000000     C     LET2          BEGSR
014200870924     C**------------------------
014300870924     C** LETTURA E AGGIORNAMENTO
014400870924     C**------------------------
014500000000     C                   Z-ADD     0             GIRO              3 0
014600000000     C                   DO        *HIVAL        GIRO
014700000000     C     SCEL          TAG
014800000000     C                   READC     SFL1                                   05
014900000000     C   05              GOTO      EMIS
015000000000     C                   MOVEL     *ZEROS        COD
015100000000     C                   MOVE      VIDCLM        COD
015200000000     C     KEYCL         CHAIN     ANTAB000                           68
015300870924     C   68              EXSR      AGGIOR
015400871120     C  N68              MOVEL     TABDES        DESCLM
015500000000     C     VIDSC1        IFEQ      0
015600000000     C                   SETON                                        67
015700000000     C   67              EXSR      AGGIOR
015800000000     C                   END
015900000000     C     SCELTA        IFEQ      'A'
016000000000     C                   EXSR      ANNUL
016100000000     C                   MOVE      *BLANKS       SCELTA
016200000000     C                   MOVE      *BLANKS       VIDCLM
016300870924     C                   MOVEL     *BLANKS       DESCLM
016400000000     C                   Z-ADD     0             VIDSC1
016500000000     C                   Z-ADD     0             VIDSC2
016600000000     C                   UPDATE    SFL1
016700000000     C                   GOTO      SCEL
016800000000     C                   END
016900000000     C*
017000990203     C     KEYCLI        CHAIN     tmscm000                           22
017100870410     C                   DO
017200000000     C                   MOVE      CODCLI        SCCCLI
017300000000     C                   MOVE      VIDCLM        SCCCLM
017400000000     C                   Z-ADD     VIDSC1        SCCSC1
017500000000     C                   Z-ADD     VIDSC2        SCCSC2
017600990203     C   22              WRITE     tmscm000
017700990203     C  N22              UPDATE    tmscm000
017800870410     C                   END
017900000000     C                   END
018000000000     C                   GOTO      SCEL
018100000000     C                   ENDSR
018200870924     C******************************************************
018300870924     C     AGGIOR        BEGSR
018400870924     C**------------------------
018500870924     C** AGGIORNAMENTO SUBFILE
018600870924     C**------------------------
018700000000     C                   SETON                                        30
018800000000     C                   UPDATE    SFL1
018900000000     C                   SETOFF                                       30
019000000000     C                   GOTO      EMIS
019100000000     C                   ENDSR
019200870924     C******************************************************
019300000000     C     ANNUL         BEGSR
019400870924     C**------------------------
019500870924     C** ANNULLAMENTO RIGA
019600870924     C**------------------------
019700990203     C     KEYCLI        CHAIN     tmscm000                           07
019800000000     C   07              GOTO      INIZIO
019900990203     C                   DELETE    tmscm000
020000000000     C                   ENDSR
020100990203     C*----------------------------------------------------*
020200990203     C* Reperimento dati società
020300990203     C*----------------------------------------------------*
020400990203     C     REPSOC        BEGSR
020500990203     C*
020600990203     C                   CALLB     'XSOC'
020700990203     C                   PARM                    TIPXSC            6
020800990203     C                   PARM                    SOCXSC            3
020900990203     C                   PARM                    CDSXSC            9 0
021000990203     C                   PARM                    MODXSC            3
021100990203     C                   PARM      *blanks       RTNXSC            1
021200990203     C                   PARM                    XSOCDS
021300990203     C                   PARM                    KPJBA
021400990203     C*
021500990203     C                   ENDSR
