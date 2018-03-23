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
001300050411     Ftigcp51L  IF   E           K DISK    USROPN
001400050411     F                                     RENAME(tigcp000:tigcpa)
001500941209     FFNBLP01L  IF   E           K DISK    USROPN
001600941209     FFNARB01L  IF   E           K DISK    USROPN
001700990831     FTITAS30C  IF   E           K DISK    USROPN
001800900509     D KSC§            S              1    DIM(7)
001900900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002000900418     D  TCU                  398    697
002100900509     D                                     DIM(50)
002200900418     D  KCU                  698    847P 0
002300900509     D                                     DIM(50)
002400900509     D                                     PACKEVEN
002500900509     D TCUDS           DS
002600900509     D  F1                     1      1
002700900509     D  F3                     3      3
002800900509     D  F2                     2      2
002900900509     D  F4                     4      4
003000900509     D  F56                    5      6
003100900418     D CNCR80        E DS
003200900418     D KPJBA         E DS
003300941209     D FNLG30        E DS                  EXTNAME(FNLG30DS)
003400960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
003500941209     D WLBDA8          DS
003600941209     D  G08DAT                 1      8  0
003700941209     D  G08INV                 9     16  0
003800941209     D  G08ERR                17     17
003900941209     D  G08TGI                18     22  0
004000941209     D                SDS
004100941209     D  §PGM                   1     10
004200900418     C*
004300900418     C     INIZIO        TAG
004400900418     C*                    ------------
004500050411     C     SIMFEL        IFEQ      0
004600050411     C     KGIA          SETLL     TIGCP51L
004700050411     c                   else
004800050411     C     KGIA          SETLL     TIGCP01L
004900050411     c                   end
005000900509     C                   EXSR      GESSFL
005100941209     C*
005200941209     C     NRR           IFEQ      *ZEROS
005300941209     C                   EXSR      VUOSFL
005400941209     C     $FINE         CABEQ     'S'           FINE
005500900509     C*
005600941209     C                   ELSE
005700941212     C     SFL           TAG
005800941212     C                   CLEAR                   D30F03
005900941212     C                   CLEAR                   D30F12
006000941213     C                   CLEAR                   D30ERR
006100941209     C                   WRITE     LG30Z02
006200941209     C                   EXFMT     LG30C02
006300900418     C*                    ------------
006400941209     C     *INKL         CABEQ     '1'           FINE
006500900509     C* LETTURA SFL
006600941209     C                   EXSR      LETSFL
006700941212     C     D30F03        CABEQ     '1'           FINE
006800941212     C     D30F12        CABEQ     '1'           SFL
006900941209     C                   ENDIF
007000900618     C*
007100900510     C                   GOTO      INIZIO
007200900418     C*                    ___________
007300900418     C*
007400900418     C     FINE          TAG
007500941212     C                   MOVEL     FNLG30        KPJBU
007600900418     C                   SETON                                        LR
007700900418     C/EJECT
007800900418     C*---------------------------------------------------------------*
007900941209     C*   I N Z S R                                                   *
008000900418     C*---------------------------------------------------------------*
008100941209     C     *INZSR        BEGSR
008200900418     C     *ENTRY        PLIST
008300900418     C                   PARM                    KPJBA
008400941209     C                   MOVEL     KPJBU         FNLG30
008500941209      *
008600900418     C                   Z-ADD     1             CODUT
008700900418     C                   CALL      'X§PARUT'
008800900418     C                   PARM                    UTEDSE
008900900418     C     CODUT         CABEQ     -1            FINE
009000900418     C                   MOVEL     RAGUT         RSUT             20
009100900418     C                   MOVEL     RAGUT         RSUT2            25
009200900502     C                   MOVEL     RAGUT         PARDUT           30
009300900418     C*§
009400900509     C*--- RICERCA CAPOCONTI------------------------------------------*
009500900509     C                   DO        50            X                 2 0
009600900509     C                   MOVE      TCU(X)        TCUDS
009700900509     C     F56           CABNE     'CG'          END1
009800900509     C     F4            COMP      '1'                                    21
009900900509     C** 1 CLIENTI   21
010000900509     C     F3            COMP      '0'                                242425
010100900509     C** 0 ITALIA   25
010200900509     C** 1 ESTERO   24
010300900509     C   21
010400900509     CAN 24              Z-ADD     KCU(X)        KCE               4 0
010500900509     C   21
010600900509     CAN 25              Z-ADD     KCU(X)        KCI               4 0
010700900509     C     END1          TAG
010800900509     C                   END
010900900509     C*---------------------------------------------------------------*
011000941209     C                   MOVEL     'S'           D30RIC
011100900418     C*-                                                   *
011200900418     C                   MOVEL     REC80         CNCR80
011300990831     C*
011400990831     C     SIMFEL        IFEQ      0
011500990831     C                   SETON                                        93
011600990831     C                   OPEN      TITAS30C
011700050411     C                   OPEN      TIgcp51l
011800990831     C                   ELSE
011900990831     C* VERIFICO SE SIAMO IN PARTENZA O ARRIVO
012000990831     C                   SETOFF                                       93
012100990831     C     D30POA        IFEQ      'A'
012200990831     C                   SETON                                        80
012300990831     C                   OPEN      FNARB01L
012400990831     C                   ELSE
012500990831     C                   OPEN      FNBLP01L
012600990831     C                   ENDIF
012700990831     C                   ENDIF
012800900418     C*
012900900419     C*                                                    *
013000900418     C*  KLIST
013100900509     C     KSPE          KLIST
013200900509     C                   KFLD                    §AAS
013300900509     C                   KFLD                    §LNP
013400900509     C                   KFLD                    §NRS
013500900509     C                   KFLD                    §NSP
013600900509     C     KGIA          KLIST
013700000403     C                   KFLD                    D30AAS
013800000403     C                   KFLD                    D30LNP
013900000403     C                   KFLD                    D30NRS
014000000403     C                   KFLD                    D30NSP
014100900418     C*
014200900509     C     *LIKE         DEFINE    GCPAAS        §AAS
014300900509     C     *LIKE         DEFINE    GCPLNP        §LNP
014400900509     C     *LIKE         DEFINE    GCPNRS        §NRS
014500900509     C     *LIKE         DEFINE    GCPNSP        §NSP
014600900418     C                   ENDSR
014700900509     C*---------------------------------------------------------------*
014800900509     C*   W T R S F L                                                 *
014900900509     C*---------------------------------------------------------------*
015000900509     C     GESSFL        BEGSR
015100900509     C                   EXSR      PULIZ
015200900509     C* CARICAMENTO SUBFILE
015300900509     C                   EXSR      CARIC
015400900509     C*                    ----------
015500900509     C                   ENDSR
015600900509     C**----------------------
015700900509     C** PULIZIA DEL SUBFILE
015800900509     C**----------------------
015900900509     C     PULIZ         BEGSR
016000900509     C                   SETON                                        70
016100941209     C                   WRITE     LG30C02
016200941209     C                   SETOFF                                       7071
016300900509     C                   ENDSR
016400900509     C**----------------------
016500900509     C** CARICAMENTO SUBFILE
016600900509     C**----------------------
016700900509     C     CARIC         BEGSR
016800900509     C                   Z-ADD     0             NRR               4 0
016900900510     C                   SETOFF                                       2122
017000900509     C     *IN21         DOUEQ     '1'
017100900509     C* X NR. GIACENZA
017200050411     C     SIMFEL        IFEQ      0
017300050411     C     KGIA          reade     TIGCP51L                               21
017400050411     c                   else
017500050307     C     KGIA          READE     TIGCP01L                               21
017600050411     c                   end
017700900510     C     *IN21         IFEQ      '0'
017800050413     C     GCPFRG        ifeq      0
017900050413     c                   if        gcpagc = d30agc and gcpngc = d30ngc
018000050413     c                             and gcpfgc = d30fgc
018100050413     c                   iter
018200050413     c                   endif
018300050413     c                   endif
018400900509     C**------------------------
018500900509     C** CARICO RECORDS DA FILE
018600900509     C**------------------------
018700901005     C                   Z-ADD     GCPAAS        §AAS
018800901005     C                   Z-ADD     GCPLNP        §LNP
018900901005     C                   Z-ADD     GCPNRS        §NRS
019000901005     C                   Z-ADD     GCPNSP        §NSP
019100990831     C     *IN93         IFEQ      '1'
019200990831     C     KSPE          CHAIN     TITAS30C                           31
019300990831     C  N31              MOVEL     TASRSD        SDES
019400990831     C                   ELSE
019500950117     C     *IN80         IFEQ      '1'
019600950117     C     KSPE          CHAIN     FNARB01L                           31
019700950117     C  N31              MOVEL     ARBRSD        SDES
019800950117     C                   ELSE
019900960124     C                   Z-ADD     GCPAAS        DN0AAS
020000960124     C                   Z-ADD     GCPLNP        DN0LNP
020100960124     C                   Z-ADD     GCPNRS        DN0NRS
020200960124     C                   Z-ADD     GCPNSP        DN0NSP
020300960124     C                   MOVEL     'R'           DN0TRC
020400960125     C                   Z-ADD     GCPDUR        DN0DAG
020500960124     C                   MOVEL     FNLSN0        KPJBU
020600960124     C                   CALL      'FNLSN0R'
020700960124     C                   PARM                    KPJBA
020800960124     C                   MOVEL     KPJBU         FNLSN0
020900960124     C     DN0TRV        IFEQ      'S'
021000960124     C                   MOVEL     DN0RSD        SDES
021100960124     C                   SETOFF                                       31
021200960124     C                   ELSE
021300950117     C     KSPE          CHAIN     FNBLP01L                           31
021400950117     C     *IN31         IFEQ      '1'
021500950117     C                   ELSE
021600950117     C                   MOVEL     BLPRSD        SDES
021700950117     C                   END
021800950117     C                   END
021900960124     C                   END
022000990831     C                   END
022100950117     C*
022200950117     C   31              MOVEL     *BLANKS       SDES
022300900509     C                   MOVEL     *BLANKS       SCELTA
022400900509     C                   Z-ADD     GCPAGC        SAGC
022500900509     C                   Z-ADD     GCPFGC        SFGC
022600900509     C                   Z-ADD     GCPNGC        SNGC
022700050307     C     D30POA        IFEQ      'A'
022800050307     C                   Z-ADD     GCPAASa       SAAS
022900050307     C                   Z-ADD     GCPLNPa       SLNP
023000050307     C                   Z-ADD     GCPNRSa       SNRS
023100050307     C                   Z-ADD     GCPNSPa       SNSP
023200050307     C                   ELSE
023300050307     C                   Z-ADD     GCPAAS        SAAS
023400050307     C                   Z-ADD     GCPLNP        SLNP
023500050307     C                   Z-ADD     GCPNRS        SNRS
023600050307     C                   Z-ADD     GCPNSP        SNSP
023700050307     C                   END
023800900509     C                   Z-ADD     GCPSCM        SSCM
023900900509     C                   MOVEL     GCPCFG        SCFG
024000911129     C                   MOVE      GCPFRG        SFRG
024100900509     C*INVERSIONE DATA CHIUSURA
024200941209     C                   MOVEL     *ZEROS        G08DAT
024300941209     C                   MOVEL     *ZEROS        G08TGI
024400941209     C                   MOVE      GCPDCG        G08INV
024500941209     C                   MOVEL     '3'           G08ERR
024600941209     C                   CALL      'XSRDA8'
024700941209     C                   PARM                    WLBDA8
024800941209     C                   Z-ADD     G08DAT        SDCG
024900900509     C*
025000900509     C                   ADD       1             NRR
025100941209     C                   WRITE     LG30S02
025200900509     C                   END
025300900509     C                   END
025400950316     C                   Z-ADD     1             REC
025500900509     C     FINCAR        ENDSR
025600900509     C**------------------------
025700900509     C** LETTURA SFL
025800900509     C**------------------------
025900900509     C     LETSFL        BEGSR
026000900509     C     SCEL          TAG
026100941209     C                   READC     LG30S02                                05
026200900509     C   05              GOTO      FINLET
026300941209     C     SCELTA        IFEQ      '1'
026400900509     C* RICH. PGM INTERROGAZIONE SINGOLA GIACENZA SCELTA
026500900509     C*
026600900509     C                   MOVEL     KPJBU         §KPJBU          256
026700941212     C                   CLEAR                   D30F03
026800941212     C                   CLEAR                   D30F12
026900941213     C                   CLEAR                   D30ERR
027000941209     C                   MOVE      SAGC          D30AGC
027100941209     C                   MOVE      SFGC          D30FGC
027200941209     C                   MOVE      SNGC          D30NGC
027300941209     C                   MOVE      SFRG          D30FRG
027400941209     C                   MOVEL     FNLG30        KPJBU
027500941209     C                   CALL      'FNLG31R'
027600900510     C                   PARM                    KPJBA
027700900509     C*
027800941209     C                   MOVEL     KPJBU         FNLG30
027900900509     C                   MOVE      *BLANKS       SCELTA
028000900509     C                   EXSR      AGGIOR
028100900509     C                   GOTO      SCEL
028200900509     C                   END
028300900509     C     FINLET        ENDSR
028400900509     C**------------------------
028500900509     C** AGGIORNAMENTO SUBFILE
028600900509     C**------------------------
028700900509     C     AGGIOR        BEGSR
028800900509     C                   SETON                                        30
028900941209     C                   UPDATE    LG30S02
029000900509     C                   SETOFF                                       30
029100900509     C                   ENDSR
029200941209     C**------------------------
029300941209     C**  SUBFILE VUOTO
029400941209     C**------------------------
029500941209     C     VUOSFL        BEGSR
029600941209     C*-                                                   *
029700941213     C                   MOVEL     ' '           D30F03
029800941213     C                   MOVEL     ' '           D30F12
029900941213     C                   MOVEL     ' '           D30ERR
030000941209     C                   SETON                                        70
030100941209     C                   WRITE     LG30Z02
030200941209     C                   WRITE     LG30D02
030300941209     C                   EXFMT     LG30C02
030400941209     C*                                                    *
030500941209     C* Fine Programma                                     *
030600941209     C     *INKL         IFEQ      '1'
030700941209     C                   MOVEL     'S'           $FINE             1
030800941209     C                   END
030900941209     C*-                                                   *
031000941209     C                   ENDSR
