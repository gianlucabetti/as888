000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941209     H* FNLG32R *----------------------------------------------------*
000300900418     H*                                                              *
000400911120     H*             - SUBFILE X INTERROGAZIONI STORICO               *
000500900418     H*  03 FINE LAVORO                                              *
000600900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000700900418     H*  21 IND. COMODO                                              *
000800900418     H*                                                              *
000900900418     H*--------------------------------------------------------------*
001000941209     FFNLG30D   CF   E             WORKSTN
001100941209     F                                     SFILE(LG30S02:NRR)
001200050307     FTIGCP01L  IF   E           K DISK
001201050411     Ftigcp51L  IF   E           K DISK    USROPN
001202050411     F                                     RENAME(tigcp000:tigcpa)
001300941209     FFNBLP01L  IF   E           K DISK    USROPN
001400941209     FFNARB01L  IF   E           K DISK    USROPN
001500990831     FTITAS30C  IF   E           K DISK    USROPN
001600900509     D KSC§            S              1    DIM(7)
001700900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001800900418     D  TCU                  398    697
001900900509     D                                     DIM(50)
002000900418     D  KCU                  698    847P 0
002100900509     D                                     DIM(50)
002200900509     D                                     PACKEVEN
002300900509     D TCUDS           DS
002400900509     D  F1                     1      1
002500900509     D  F3                     3      3
002600900509     D  F2                     2      2
002700900509     D  F4                     4      4
002800900509     D  F56                    5      6
002900900418     D CNCR80        E DS
003000900418     D KPJBA         E DS
003100941209     D FNLG30        E DS                  EXTNAME(FNLG30DS)
003200960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
003300941209     D WLBDA8          DS
003400941209     D  G08DAT                 1      8  0
003500941209     D  G08INV                 9     16  0
003600941209     D  G08ERR                17     17
003700941209     D  G08TGI                18     22  0
003800941209     D                SDS
003900941209     D  §PGM                   1     10
004000900418     C*
004100900418     C     INIZIO        TAG
004200900418     C*                    ------------
004201050411     C     SIMFEL        IFEQ      0
004300050411     C     KGIA          SETLL     TIGCP51L
004301050411     c                   else
004302050411     C     KGIA          SETLL     TIGCP01L
004303050411     c                   end
004400900509     C                   EXSR      GESSFL
004500941209     C*
004600941209     C     NRR           IFEQ      *ZEROS
004700941209     C                   EXSR      VUOSFL
004800941209     C     $FINE         CABEQ     'S'           FINE
004900900509     C*
005000941209     C                   ELSE
005100941212     C     SFL           TAG
005200941212     C                   CLEAR                   D30F03
005300941212     C                   CLEAR                   D30F12
005400941213     C                   CLEAR                   D30ERR
005500941209     C                   WRITE     LG30Z02
005600941209     C                   EXFMT     LG30C02
005700900418     C*                    ------------
005800941209     C     *INKL         CABEQ     '1'           FINE
005900900509     C* LETTURA SFL
006000941209     C                   EXSR      LETSFL
006100941212     C     D30F03        CABEQ     '1'           FINE
006200941212     C     D30F12        CABEQ     '1'           SFL
006300941209     C                   ENDIF
006400900618     C*
006500900510     C                   GOTO      INIZIO
006600900418     C*                    ___________
006700900418     C*
006800900418     C     FINE          TAG
006900941212     C                   MOVEL     FNLG30        KPJBU
007000900418     C                   SETON                                        LR
007100900418     C/EJECT
007200900418     C*---------------------------------------------------------------*
007300941209     C*   I N Z S R                                                   *
007400900418     C*---------------------------------------------------------------*
007500941209     C     *INZSR        BEGSR
007600900418     C     *ENTRY        PLIST
007700900418     C                   PARM                    KPJBA
007800941209     C                   MOVEL     KPJBU         FNLG30
007900941209      *
008000900418     C                   Z-ADD     1             CODUT
008100900418     C                   CALL      'X§PARUT'
008200900418     C                   PARM                    UTEDSE
008300900418     C     CODUT         CABEQ     -1            FINE
008400900418     C                   MOVEL     RAGUT         RSUT             20
008500900418     C                   MOVEL     RAGUT         RSUT2            25
008600900502     C                   MOVEL     RAGUT         PARDUT           30
008700900418     C*§
008800900509     C*--- RICERCA CAPOCONTI------------------------------------------*
008900900509     C                   DO        50            X                 2 0
009000900509     C                   MOVE      TCU(X)        TCUDS
009100900509     C     F56           CABNE     'CG'          END1
009200900509     C     F4            COMP      '1'                                    21
009300900509     C** 1 CLIENTI   21
009400900509     C     F3            COMP      '0'                                242425
009500900509     C** 0 ITALIA   25
009600900509     C** 1 ESTERO   24
009700900509     C   21
009800900509     CAN 24              Z-ADD     KCU(X)        KCE               4 0
009900900509     C   21
010000900509     CAN 25              Z-ADD     KCU(X)        KCI               4 0
010100900509     C     END1          TAG
010200900509     C                   END
010300900509     C*---------------------------------------------------------------*
010400941209     C                   MOVEL     'S'           D30RIC
010500900418     C*-                                                   *
010600900418     C                   MOVEL     REC80         CNCR80
010700990831     C*
010800990831     C     SIMFEL        IFEQ      0
010900990831     C                   SETON                                        93
011000990831     C                   OPEN      TITAS30C
011001050411     C                   OPEN      TIgcp51l
011100990831     C                   ELSE
011200990831     C* VERIFICO SE SIAMO IN PARTENZA O ARRIVO
011300990831     C                   SETOFF                                       93
011400990831     C     D30POA        IFEQ      'A'
011500990831     C                   SETON                                        80
011600990831     C                   OPEN      FNARB01L
011700990831     C                   ELSE
011800990831     C                   OPEN      FNBLP01L
011900990831     C                   ENDIF
012000990831     C                   ENDIF
012100900418     C*
012200900419     C*                                                    *
012300900418     C*  KLIST
012400900509     C     KSPE          KLIST
012500900509     C                   KFLD                    §AAS
012600900509     C                   KFLD                    §LNP
012700900509     C                   KFLD                    §NRS
012800900509     C                   KFLD                    §NSP
012900900509     C     KGIA          KLIST
013000000403     C                   KFLD                    D30AAS
013100000403     C                   KFLD                    D30LNP
013200000403     C                   KFLD                    D30NRS
013300000403     C                   KFLD                    D30NSP
013400900418     C*
013500900509     C     *LIKE         DEFINE    GCPAAS        §AAS
013600900509     C     *LIKE         DEFINE    GCPLNP        §LNP
013700900509     C     *LIKE         DEFINE    GCPNRS        §NRS
013800900509     C     *LIKE         DEFINE    GCPNSP        §NSP
013900900418     C                   ENDSR
014000900509     C*---------------------------------------------------------------*
014100900509     C*   W T R S F L                                                 *
014200900509     C*---------------------------------------------------------------*
014300900509     C     GESSFL        BEGSR
014400900509     C                   EXSR      PULIZ
014500900509     C* CARICAMENTO SUBFILE
014600900509     C                   EXSR      CARIC
014700900509     C*                    ----------
014800900509     C                   ENDSR
014900900509     C**----------------------
015000900509     C** PULIZIA DEL SUBFILE
015100900509     C**----------------------
015200900509     C     PULIZ         BEGSR
015300900509     C                   SETON                                        70
015400941209     C                   WRITE     LG30C02
015500941209     C                   SETOFF                                       7071
015600900509     C                   ENDSR
015700900509     C**----------------------
015800900509     C** CARICAMENTO SUBFILE
015900900509     C**----------------------
016000900509     C     CARIC         BEGSR
016100900509     C                   Z-ADD     0             NRR               4 0
016200900510     C                   SETOFF                                       2122
016300900509     C     *IN21         DOUEQ     '1'
016400900509     C* X NR. GIACENZA
016401050411     C     SIMFEL        IFEQ      0
016402050411     C     KGIA          reade     TIGCP51L                               21
016403050411     c                   else
016500050307     C     KGIA          READE     TIGCP01L                               21
016501050411     c                   end
016600900510     C     *IN21         IFEQ      '0'
016700911119     C     GCPFRG        ANDNE     0
016800900509     C**------------------------
016900900509     C** CARICO RECORDS DA FILE
017000900509     C**------------------------
017100901005     C                   Z-ADD     GCPAAS        §AAS
017200901005     C                   Z-ADD     GCPLNP        §LNP
017300901005     C                   Z-ADD     GCPNRS        §NRS
017400901005     C                   Z-ADD     GCPNSP        §NSP
017500990831     C     *IN93         IFEQ      '1'
017600990831     C     KSPE          CHAIN     TITAS30C                           31
017700990831     C  N31              MOVEL     TASRSD        SDES
017800990831     C                   ELSE
017900950117     C     *IN80         IFEQ      '1'
018000950117     C     KSPE          CHAIN     FNARB01L                           31
018100950117     C  N31              MOVEL     ARBRSD        SDES
018200950117     C                   ELSE
018300960124     C                   Z-ADD     GCPAAS        DN0AAS
018400960124     C                   Z-ADD     GCPLNP        DN0LNP
018500960124     C                   Z-ADD     GCPNRS        DN0NRS
018600960124     C                   Z-ADD     GCPNSP        DN0NSP
018700960124     C                   MOVEL     'R'           DN0TRC
018800960125     C                   Z-ADD     GCPDUR        DN0DAG
018900960124     C                   MOVEL     FNLSN0        KPJBU
019000960124     C                   CALL      'FNLSN0R'
019100960124     C                   PARM                    KPJBA
019200960124     C                   MOVEL     KPJBU         FNLSN0
019300960124     C     DN0TRV        IFEQ      'S'
019400960124     C                   MOVEL     DN0RSD        SDES
019500960124     C                   SETOFF                                       31
019600960124     C                   ELSE
019700950117     C     KSPE          CHAIN     FNBLP01L                           31
019800950117     C     *IN31         IFEQ      '1'
019900950117     C                   ELSE
020000950117     C                   MOVEL     BLPRSD        SDES
020100950117     C                   END
020200950117     C                   END
020300960124     C                   END
020400990831     C                   END
020500950117     C*
020600950117     C   31              MOVEL     *BLANKS       SDES
020700900509     C                   MOVEL     *BLANKS       SCELTA
020800900509     C                   Z-ADD     GCPAGC        SAGC
020900900509     C                   Z-ADD     GCPFGC        SFGC
021000900509     C                   Z-ADD     GCPNGC        SNGC
021100050307     C     D30POA        IFEQ      'A'
021200050307     C                   Z-ADD     GCPAASa       SAAS
021300050307     C                   Z-ADD     GCPLNPa       SLNP
021400050307     C                   Z-ADD     GCPNRSa       SNRS
021500050307     C                   Z-ADD     GCPNSPa       SNSP
021600050307     C                   ELSE
021700050307     C                   Z-ADD     GCPAAS        SAAS
021800050307     C                   Z-ADD     GCPLNP        SLNP
021900050307     C                   Z-ADD     GCPNRS        SNRS
022000050307     C                   Z-ADD     GCPNSP        SNSP
022100050307     C                   END
022200900509     C                   Z-ADD     GCPSCM        SSCM
022300900509     C                   MOVEL     GCPCFG        SCFG
022400911129     C                   MOVE      GCPFRG        SFRG
022500900509     C*INVERSIONE DATA CHIUSURA
022600941209     C                   MOVEL     *ZEROS        G08DAT
022700941209     C                   MOVEL     *ZEROS        G08TGI
022800941209     C                   MOVE      GCPDCG        G08INV
022900941209     C                   MOVEL     '3'           G08ERR
023000941209     C                   CALL      'XSRDA8'
023100941209     C                   PARM                    WLBDA8
023200941209     C                   Z-ADD     G08DAT        SDCG
023300900509     C*
023400900509     C                   ADD       1             NRR
023500941209     C                   WRITE     LG30S02
023600900509     C                   END
023700900509     C                   END
023800950316     C                   Z-ADD     1             REC
023900900509     C     FINCAR        ENDSR
024000900509     C**------------------------
024100900509     C** LETTURA SFL
024200900509     C**------------------------
024300900509     C     LETSFL        BEGSR
024400900509     C     SCEL          TAG
024500941209     C                   READC     LG30S02                                05
024600900509     C   05              GOTO      FINLET
024700941209     C     SCELTA        IFEQ      '1'
024800900509     C* RICH. PGM INTERROGAZIONE SINGOLA GIACENZA SCELTA
024900900509     C*
025000900509     C                   MOVEL     KPJBU         §KPJBU          256
025100941212     C                   CLEAR                   D30F03
025200941212     C                   CLEAR                   D30F12
025300941213     C                   CLEAR                   D30ERR
025400941209     C                   MOVE      SAGC          D30AGC
025500941209     C                   MOVE      SFGC          D30FGC
025600941209     C                   MOVE      SNGC          D30NGC
025700941209     C                   MOVE      SFRG          D30FRG
025800941209     C                   MOVEL     FNLG30        KPJBU
025900941209     C                   CALL      'FNLG31R'
026000900510     C                   PARM                    KPJBA
026100900509     C*
026200941209     C                   MOVEL     KPJBU         FNLG30
026300900509     C                   MOVE      *BLANKS       SCELTA
026400900509     C                   EXSR      AGGIOR
026500900509     C                   GOTO      SCEL
026600900509     C                   END
026700900509     C     FINLET        ENDSR
026800900509     C**------------------------
026900900509     C** AGGIORNAMENTO SUBFILE
027000900509     C**------------------------
027100900509     C     AGGIOR        BEGSR
027200900509     C                   SETON                                        30
027300941209     C                   UPDATE    LG30S02
027400900509     C                   SETOFF                                       30
027500900509     C                   ENDSR
027600941209     C**------------------------
027700941209     C**  SUBFILE VUOTO
027800941209     C**------------------------
027900941209     C     VUOSFL        BEGSR
028000941209     C*-                                                   *
028100941213     C                   MOVEL     ' '           D30F03
028200941213     C                   MOVEL     ' '           D30F12
028300941213     C                   MOVEL     ' '           D30ERR
028400941209     C                   SETON                                        70
028500941209     C                   WRITE     LG30Z02
028600941209     C                   WRITE     LG30D02
028700941209     C                   EXFMT     LG30C02
028800941209     C*                                                    *
028900941209     C* Fine Programma                                     *
029000941209     C     *INKL         IFEQ      '1'
029100941209     C                   MOVEL     'S'           $FINE             1
029200941209     C                   END
029300941209     C*-                                                   *
029400941209     C                   ENDSR
