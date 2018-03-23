000100940304     H DECEDIT('0,') DATEDIT(*DMY.)
000200940304?     *--------------------------------------------------------------*
000300950125      * TNSC60R                                                      *
000400940304      *       *-----------------------------------------*            *
000500940304      *           STAMPA/PULIZIA CONTRASSEGNI DI SEDE                *
000600940304      *       *-----------------------------------------*            *
000700940304      *                                                              *
000800940304?     *--------------------------------------------------------------*
000900940304      * KC -Fine Lavoro                                              *
001000940304      * KF -Conferma                                                 *
001100940304      * KV -Pe.Batch                                                 *
001200940304?     *--------------------------------------------------------------*
001300950125     FTNSC60D   CF   E             WORKSTN
001400950127?     *--------------------------------------------------------------*
001500140909     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
001600130715     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001700130715     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001800950127?     *--------------------------------------------------------------*
001900940304?     *  DS                                                          *
002000940304?     *--------------------------------------------------------------*
002100940304     D KPJBA         E DS
002200940304      *
002300950125     D TNSC60        E DS                  EXTNAME(TNSC60DS)
002400940304      *
002500940304     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002600940304     D CNCR80        E DS
002700940304      *
002800940304     D §§§EDP        E DS
002900940304      *
003000950125     D WLBDA8          DS
003100950125     D  G02DAT                 1      8  0
003200950125     D  G02INV                 9     16  0
003300950125     D  G02ERR                17     17
003400950125     D  G02TGI                18     22  0
003500940304     D                SDS
003600940304     D  NOMPGM                 1     10
003700130715     d coman           s             99
003800130715     d luncmd          s             15  5
003900940304      *---------------------------------------------------------------*
004000940304      *   C I C L O      P R I N C I P A L E                          *
004100940304      *---------------------------------------------------------------*
004200940304     C*  Inizializzazione videata
004300940304     C                   EXSR      INZVID
004400940304     C*
004500940304     C*  Loop
004600940304     C     £FINE         DOUEQ     'S'
004700940304     C*
004800940304     C     TIPVID        CASEQ     '1'           GESVD1
004900940304     C                   END
005000940304     C*
005100940304     C                   END
005200940304     C*
005300940304     C                   SETON                                        LR
005400940304      *---------------------------------------------------------------*
005500940304      *  INZVID: Inizializzazione parametri video                     *
005600940304      *---------------------------------------------------------------*
005700940304     C     INZVID        BEGSR
005800940304     C*
005900940304     C                   Z-ADD     0             SPDDA
006000940304     C                   Z-ADD     0             SPADA
006100950203     C                   Z-ADD     0             V1CDT1
006200950203     C                   Z-ADD     0             V1CDT2
006300950125     C                   MOVEL     *BLANKS       V1CSTP
006400950125     C                   MOVEL     *BLANKS       V1CPUL
006500950125     C                   MOVEL     *BLANKS       V1CPSW
006600940304     C                   MOVEL     'NO'          CLRWRK
006700130715     C                   MOVE      'SAVCON'      V1CCLIb
006800130715     C                   Z-ADD     0             V1Cnlib
006900940304     C*
007000940304     C                   ENDSR
007100940304      *---------------------------------------------------------------*
007200940304      *  GESVD1: Gestione prima videata richiesta parametri           *
007300940304      *---------------------------------------------------------------*
007400940304     C     GESVD1        BEGSR
007500940304     C*
007600950125     C                   EXFMT     SC60D1
007700950125     C                   SETOFF                                       284041
007800950125     C                   SETOFF                                       424344
007900940304     C*  Fine elaborazione
008000940304     C     *INKC         IFEQ      '1'
008100940304     C                   MOVEL     'S'           £FINE
008200940304     C                   GOTO      FINVD1
008300940304     C                   END
008400940304     C*  Pe.Batch
008500940304     C     *INKV         IFEQ      '1'
008600940304     C                   CALL      'BCH09'
008700940304     C                   PARM                    KPJBA
008800940304     C                   END
008900940304     C*  Controlli
009000940304     C                   EXSR      CNTVD1
009100950203     C   28              GOTO      FINVD1
009200940304     C*  conferma
009300940304     C     *INKF         IFEQ      '1'
009400940304     C                   EXSR      SBMP61
009500940304     C                   EXSR      INZVID
009600940304     C                   END
009700940304     C*
009800940304     C     FINVD1        ENDSR
009900940304      *---------------------------------------------------------------*
010000940304      *  CNTVD1: Controlli prima videata                              *
010100940304      *---------------------------------------------------------------*
010200940304     C     CNTVD1        BEGSR
010300940304     C*----------------------------------------------------*
010400940304     C*  Stampa/Pulizia
010500950125     C     V1CPUL        IFEQ      ' '
010600950125     C     V1CSTP        ANDEQ     ' '
010700950203     C                   SETON                                        434228
010800950127     C                   MOVEL     ERR(4)        $MSG
010900940304     C                   GOTO      FINCT1
011000940304     C                   END
011100140909     C     V1CPUL        IFne      ' '
011200140909     C     V1CSTP        ANDEQ     ' '
011300140909     C                   SETON                                        434228
011400140909     C                   MOVEL     ERR(10)       $MSG
011500140909     C                   GOTO      FINCT1
011600140909     C                   END
011700130715      * controllo per nome liberia
011800130715     c                   if        v1cnlib = 0
011900130715     C                   SETON                                          4528
012000130715     C                   MOVEL     ERR(8)        $MSG
012100130715     C                   GOTO      FINCT1
012200130715     c                   else
012300130715     c                   move      v1cnlib       wlib             10
012400130715     c                   movel     v1cclib       wlib
012500130715     c  n46              exsr      chkcrtlib
012600130715     c                   if        *in45
012700130715     C                   SETON                                            28
012800130715     C                   MOVEL     ERR(9)        $MSG
012900130715     C                   GOTO      FINCT1
013000130715     c                   endif
013100130715     c                   endif
013200940304     C*----------------------------------------------------*
013300940304     C*  Password x pulizia
013400950125     C     V1CPUL        IFNE      ' '
013500940304     C*----------------------------------------------------*
013600940304     C*  Se archivi di Work sono pieni richiedo un ulteriore conferma
013700940304     C     NRR           IFGT      0
013800940304     C     CLRWRK        ANDNE     'SI'
013900950125     C                   EXFMT     SC60D2
014000940304     C*  Se non si voglio ricreare archivi pieni non si può
014100940304     C*  effettuare la pulizia
014200940304     C     CLRWRK        IFEQ      'NO'
014300950203     C                   MOVEL     ' '           V1CPUL
014400950125     C                   WRITE     SC60D1
014500940304     C                   GOTO      FINCT1
014600940304     C                   END
014700940304     C                   END
014800940304     C*----------------------------------------------------*
014900950125     C     V1CPSW        IFEQ      *BLANKS
015000950125     C                   SETON                                        2844
015100950127     C                   MOVEL     ERR(6)        $MSG
015200940304     C                   GOTO      FINCT1
015300940304     C                   ELSE
015400940304     C*
015500950125     C     V1CPSW        IFNE      PASPUL
015600950125     C                   SETON                                        2844
015700950127     C                   MOVEL     ERR(7)        $MSG
015800940304     C                   GOTO      FINCT1
015900940304     C                   END
016000940304     C                   END
016100940304     C*----------------------------------------------------*
016200940311     C*  Controllo che le date spedizioni siano diverse
016300940311     C*  da zero
016400950127     C     V1CDT1        IFEQ      0
016500950127     C                   SETON                                        2840
016600950127     C                   MOVEL     ERR(1)        $MSG
016700940304     C                   GOTO      FINCT1
016800940304     C                   END
016900940304     C*
017000950127     C     V1CDT2        IFEQ      0
017100950127     C                   SETON                                        4128
017200950127     C                   MOVEL     ERR(1)        $MSG
017300940311     C                   GOTO      FINCT1
017400940311     C                   END
017500940311     C*----------------------------------------------------*
017600940311     C                   ELSE
017700940311     C*  Se richiesta la sola stampa devono esistere gli
017800940311     C*  archivi
017900940311     C     NRR           IFEQ      0
018000950127     C                   SETON                                        2842
018100950127     C                   MOVEL     ERR(5)        $MSG
018200940311     C                   GOTO      FINCT1
018300940311     C                   END
018400940311     C*----------------------------------------------------*
018500940311     C                   END
018600940311     C*----------------------------------------------------*
018700940311     C*  Data di spedizione da ...
018800950127     C     V1CDT1        IFNE      0
018900950127     C                   MOVE      V1CDT1        G02DAT
019000940304     C                   MOVEL     *BLANK        G02ERR
019100950203     C                   CALL      'XSRDA8'
019200950203     C                   PARM                    WLBDA8
019300940304     C     G02ERR        IFEQ      '1'
019400950127     C                   SETON                                        2840
019500950127     C                   MOVEL     ERR(2)        $MSG
019600940304     C                   GOTO      FINCT1
019700940304     C                   ELSE
019800940304     C                   Z-ADD     G02INV        SPDDA
019900950203     C                   Z-ADD     G02DAT        V1CDT1
020000940304     C                   END
020100940311     C                   END
020200940304     C*----------------------------------------------------*
020300940304     C*  Controllo data di spedizione a ....
020400950127     C     V1CDT2        IFNE      0
020500950127     C                   MOVE      V1CDT2        G02DAT
020600940304     C                   MOVEL     *BLANK        G02ERR
020700950203     C                   CALL      'XSRDA8'
020800950203     C                   PARM                    WLBDA8
020900940304     C     G02ERR        IFEQ      '1'
021000950127     C                   SETON                                        2841
021100950127     C                   MOVEL     ERR(2)        $MSG
021200940304     C                   GOTO      FINCT1
021300940304     C                   ELSE
021400940304     C                   Z-ADD     G02INV        SPADA
021500950203     C                   Z-ADD     G02DAT        V1CDT2
021600940304     C                   END
021700940311     C                   END
021800940304     C*----------------------------------------------------*
021900940304     C*  Controllo limite delle due date
022000940304     C     SPDDA         IFGT      SPADA
022100950127     C                   SETON                                        404128
022200950127     C                   MOVEL     ERR(3)        $MSG
022300940304     C                   END
022400940304     C*----------------------------------------------------*
022500940304     C     FINCT1        ENDSR
022600130715      *---------------------------------------------------------------*
022700130715     c     Chkcrtlib     begsr
022800130715      *---------------------------------------------------------------*
022900130715      *verifica se esiste libreria di salvataggio dati
023000130715     c                   setoff                                       45
023100130715     c                   movea     cmd1          coman
023200130715     c                   eval      %subst(coman:12:10) = wlib
023300130715     c                   exsr      comandi2
023400130715     c                   if        *in34
023500130715     c                   movea     cmd2          coman
023600130715     c                   eval      %subst(coman:12:10) = wlib
023700130715     c                   exsr      comandi2
023800130715     c                   seton                                        46
023900130715     c                   else
024000130715     c                   seton                                        45
024100130715     c                   endif
024200130715     c                   endsr
024300130715     c*-------------------------------------------------
024400130715     c     comandi2      begsr
024500130715     c*-------------------------------------------------
024600130715      *
024700130715     C                   z-add     99            luncmd
024800130715     C                   CALL      'QCMDEXC'                            34
024900130715     C                   parm                    coman
025000130715     C                   parm                    luncmd
025100130715      *
025200130715     c                   endsr
025300940304      *----------------------------------------------------*
025400940304      *  SBMP61: Sottometto elaborazione                   *
025500940304      *----------------------------------------------------*
025600940304     C     SBMP61        BEGSR
025700940304     C*----------------------------------------------------*
025800950127     C     V1CDT1        IFEQ      0
025900950127     C     V1CDT2        ANDEQ     0
026000940311     C                   Z-ADD     999999        SPADA
026100940311     C                   END
026200950127     C                   Z-ADD     SPDDA         D60SDD
026300950127     C                   Z-ADD     SPADA         D60SDA
026400950127     C                   MOVEL     V1CSTP        D60STA
026500950127     C                   MOVEL     V1CPUL        D60PUL
026600130715     C                   MOVEL     wlib          D60lib
026700940304     C*---
026800950127     C                   MOVE      'SC61'        KCOAZ
026900950127     C                   MOVEL     TNSC60        KPJBU
027000940304     C                   CALL      'BCH10'
027100940304     C                   PARM                    KPJBA
027200940304     C*---
027300940304     C                   ENDSR
027400940304      *---------------------------------------------------------------*
027500940304      *  *INZSR: OPERAZIONI INIZIALI                                  *
027600940304      *---------------------------------------------------------------*
027700940304     C     *INZSR        BEGSR
027800940304      *---------------------------------------------------------------*
027900940304     C     *ENTRY        PLIST
028000940304     C                   PARM                    KPJBA
028100940304      *
028200940304     C                   Z-ADD     1             CODUT
028300940304     C                   CALL      'X§PARUT'
028400940304     C                   PARM                    UT§DSE
028500940304     C                   MOVEL     RAGUT         RSUT             20
028600940304     C                   MOVEL     REC80         CNCR80
028700940304     C*----------------------------------------------------*
028800940304     C*  Definizione variabili
028900950125     C     *LIKE         DEFINE    D60SDD        SPDDA
029000950125     C     *LIKE         DEFINE    D60SDA        SPADA
029100940304     C     *LIKE         DEFINE    §§§CSP        PASPUL
029200940304     C*----------------------------------------------------*
029300940304     C                   MOVEL     'N'           £FINE             1
029400940304     C                   MOVEL     '1'           TIPVID            1
029500940304     C*----------------------------------------------------*
029600940304     C     *DTAARA       DEFINE                  §§§EDP
029700940304     C                   IN        §§§EDP
029800940304     C                   MOVEL     §§§CSP        PASPUL
029900940304     C*----------------------------------------------------*
030000940304     C*  Richiamo pgm per sapere se archivio di Work vuoti
030100940304     C                   MOVEL     KNSIF         LIBR             10
030200940304     C                   MOVEL     *BLANK        FILE             10
030300940304     C                   MOVEL     *BLANK        MBR              10
030400940304     C                   Z-ADD     0             NRR              10 0
030500950125     C                   MOVEL     'TNCPB00F'    FILE
030600940304     C                   MOVEL     '*FIRST'      MBR
030700940304     C*
030800940304     C                   CALL      'TRUL51C'
030900940304     C                   PARM                    LIBR
031000940304     C                   PARM                    FILE
031100940304     C                   PARM                    MBR
031200940304     C                   PARM                    NRR
031300940304     C*----------------------------------------------------*
031400940304     C                   ENDSR
031500950125**
031600950125Immettere la data di spedizione                                       01
031700950125Data di spedizione errata                                             02
031800950125Limiti date incompatibili                                             03
031900950125Effettuare almeno una scelta                                          04
032000950125Archivi di Work vuoti impossibile eseguire la stampa                  05
032100950125Immettere parola d'ordine per richiesta pulizia                       06
032200950125Parola d'ordine errata                                                07
032300130715Inserire la parte finale del nome libreria in forma numerica          08
032400130715Libreria già esistente proporre altro nome                            09
032500140909Per pulizia obbligatoria la stampa                                    10
032600130715**  CMD1
032700130715CHKOBJ OBJ(SAVCONxxxx) OBJTYPE(*LIB)
032800130715**  CMD2
032900130715CRTLIB LIB(SAVCONXXXX)
