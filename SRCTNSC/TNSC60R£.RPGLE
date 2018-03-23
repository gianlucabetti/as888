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
001500130715     D ERR             S             70    DIM(9) CTDATA PERRCD(1)
001501130715     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
001502130715     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
001600950127?     *--------------------------------------------------------------*
001700940304?     *  DS                                                          *
001800940304?     *--------------------------------------------------------------*
001900940304     D KPJBA         E DS
002000940304      *
002100950125     D TNSC60        E DS                  EXTNAME(TNSC60DS)
002200940304      *
002300940304     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400940304     D CNCR80        E DS
002500940304      *
002600940304     D §§§EDP        E DS
002700940304      *
002800950125     D WLBDA8          DS
002900950125     D  G02DAT                 1      8  0
003000950125     D  G02INV                 9     16  0
003100950125     D  G02ERR                17     17
003200950125     D  G02TGI                18     22  0
003300940304     D                SDS
003400940304     D  NOMPGM                 1     10
003401130715     d coman           s             99
003402130715     d luncmd          s             15  5
003500940304      *---------------------------------------------------------------*
003600940304      *   C I C L O      P R I N C I P A L E                          *
003700940304      *---------------------------------------------------------------*
003800940304     C*  Inizializzazione videata
003900940304     C                   EXSR      INZVID
004000940304     C*
004100940304     C*  Loop
004200940304     C     £FINE         DOUEQ     'S'
004300940304     C*
004400940304     C     TIPVID        CASEQ     '1'           GESVD1
004500940304     C                   END
004600940304     C*
004700940304     C                   END
004800940304     C*
004900940304     C                   SETON                                        LR
005000940304      *---------------------------------------------------------------*
005100940304      *  INZVID: Inizializzazione parametri video                     *
005200940304      *---------------------------------------------------------------*
005300940304     C     INZVID        BEGSR
005400940304     C*
005500940304     C                   Z-ADD     0             SPDDA
005600940304     C                   Z-ADD     0             SPADA
005700950203     C                   Z-ADD     0             V1CDT1
005800950203     C                   Z-ADD     0             V1CDT2
005900950125     C                   MOVEL     *BLANKS       V1CSTP
006000950125     C                   MOVEL     *BLANKS       V1CPUL
006100950125     C                   MOVEL     *BLANKS       V1CPSW
006200940304     C                   MOVEL     'NO'          CLRWRK
006300130715     C                   MOVE      'SAVCON'      V1CCLIb
006301130715     C                   Z-ADD     0             V1Cnlib
006400940304     C*
006500940304     C                   ENDSR
006600940304      *---------------------------------------------------------------*
006700940304      *  GESVD1: Gestione prima videata richiesta parametri           *
006800940304      *---------------------------------------------------------------*
006900940304     C     GESVD1        BEGSR
007000940304     C*
007100950125     C                   EXFMT     SC60D1
007200950125     C                   SETOFF                                       284041
007300950125     C                   SETOFF                                       424344
007400940304     C*  Fine elaborazione
007500940304     C     *INKC         IFEQ      '1'
007600940304     C                   MOVEL     'S'           £FINE
007700940304     C                   GOTO      FINVD1
007800940304     C                   END
007900940304     C*  Pe.Batch
008000940304     C     *INKV         IFEQ      '1'
008100940304     C                   CALL      'BCH09'
008200940304     C                   PARM                    KPJBA
008300940304     C                   END
008400940304     C*  Controlli
008500940304     C                   EXSR      CNTVD1
008600950203     C   28              GOTO      FINVD1
008700940304     C*  conferma
008800940304     C     *INKF         IFEQ      '1'
008900940304     C                   EXSR      SBMP61
009000940304     C                   EXSR      INZVID
009100940304     C                   END
009200940304     C*
009300940304     C     FINVD1        ENDSR
009400940304      *---------------------------------------------------------------*
009500940304      *  CNTVD1: Controlli prima videata                              *
009600940304      *---------------------------------------------------------------*
009700940304     C     CNTVD1        BEGSR
009800940304     C*----------------------------------------------------*
009900940304     C*  Stampa/Pulizia
010000950125     C     V1CPUL        IFEQ      ' '
010100950125     C     V1CSTP        ANDEQ     ' '
010200950203     C                   SETON                                        434228
010300950127     C                   MOVEL     ERR(4)        $MSG
010400940304     C                   GOTO      FINCT1
010500940304     C                   END
010501130715      * controllo per nome liberia
010502130715     c                   if        v1cnlib = 0
010503130715     C                   SETON                                          4528
010504130715     C                   MOVEL     ERR(8)        $MSG
010505130715     C                   GOTO      FINCT1
010506130715     c                   else
010507130715     c                   move      v1cnlib       wlib             10
010508130715     c                   movel     v1cclib       wlib
010509130715     c  n46              exsr      chkcrtlib
010510130715     c                   if        *in45
010511130715     C                   SETON                                            28
010512130715     C                   MOVEL     ERR(9)        $MSG
010513130715     C                   GOTO      FINCT1
010514130715     c                   endif
010515130715     c                   endif
010600940304     C*----------------------------------------------------*
010700940304     C*  Password x pulizia
010800950125     C     V1CPUL        IFNE      ' '
010900940304     C*----------------------------------------------------*
011000940304     C*  Se archivi di Work sono pieni richiedo un ulteriore conferma
011100940304     C     NRR           IFGT      0
011200940304     C     CLRWRK        ANDNE     'SI'
011300950125     C                   EXFMT     SC60D2
011400940304     C*  Se non si voglio ricreare archivi pieni non si può
011500940304     C*  effettuare la pulizia
011600940304     C     CLRWRK        IFEQ      'NO'
011700950203     C                   MOVEL     ' '           V1CPUL
011800950125     C                   WRITE     SC60D1
011900940304     C                   GOTO      FINCT1
012000940304     C                   END
012100940304     C                   END
012200940304     C*----------------------------------------------------*
012300950125     C     V1CPSW        IFEQ      *BLANKS
012400950125     C                   SETON                                        2844
012500950127     C                   MOVEL     ERR(6)        $MSG
012600940304     C                   GOTO      FINCT1
012700940304     C                   ELSE
012800940304     C*
012900950125     C     V1CPSW        IFNE      PASPUL
013000950125     C                   SETON                                        2844
013100950127     C                   MOVEL     ERR(7)        $MSG
013200940304     C                   GOTO      FINCT1
013300940304     C                   END
013400940304     C                   END
013500940304     C*----------------------------------------------------*
013600940311     C*  Controllo che le date spedizioni siano diverse
013700940311     C*  da zero
013800950127     C     V1CDT1        IFEQ      0
013900950127     C                   SETON                                        2840
014000950127     C                   MOVEL     ERR(1)        $MSG
014100940304     C                   GOTO      FINCT1
014200940304     C                   END
014300940304     C*
014400950127     C     V1CDT2        IFEQ      0
014500950127     C                   SETON                                        4128
014600950127     C                   MOVEL     ERR(1)        $MSG
014700940311     C                   GOTO      FINCT1
014800940311     C                   END
014900940311     C*----------------------------------------------------*
015000940311     C                   ELSE
015100940311     C*  Se richiesta la sola stampa devono esistere gli
015200940311     C*  archivi
015300940311     C     NRR           IFEQ      0
015400950127     C                   SETON                                        2842
015500950127     C                   MOVEL     ERR(5)        $MSG
015600940311     C                   GOTO      FINCT1
015700940311     C                   END
015800940311     C*----------------------------------------------------*
015900940311     C                   END
016000940311     C*----------------------------------------------------*
016100940311     C*  Data di spedizione da ...
016200950127     C     V1CDT1        IFNE      0
016300950127     C                   MOVE      V1CDT1        G02DAT
016400940304     C                   MOVEL     *BLANK        G02ERR
016500950203     C                   CALL      'XSRDA8'
016600950203     C                   PARM                    WLBDA8
016700940304     C     G02ERR        IFEQ      '1'
016800950127     C                   SETON                                        2840
016900950127     C                   MOVEL     ERR(2)        $MSG
017000940304     C                   GOTO      FINCT1
017100940304     C                   ELSE
017200940304     C                   Z-ADD     G02INV        SPDDA
017300950203     C                   Z-ADD     G02DAT        V1CDT1
017400940304     C                   END
017500940311     C                   END
017600940304     C*----------------------------------------------------*
017700940304     C*  Controllo data di spedizione a ....
017800950127     C     V1CDT2        IFNE      0
017900950127     C                   MOVE      V1CDT2        G02DAT
018000940304     C                   MOVEL     *BLANK        G02ERR
018100950203     C                   CALL      'XSRDA8'
018200950203     C                   PARM                    WLBDA8
018300940304     C     G02ERR        IFEQ      '1'
018400950127     C                   SETON                                        2841
018500950127     C                   MOVEL     ERR(2)        $MSG
018600940304     C                   GOTO      FINCT1
018700940304     C                   ELSE
018800940304     C                   Z-ADD     G02INV        SPADA
018900950203     C                   Z-ADD     G02DAT        V1CDT2
019000940304     C                   END
019100940311     C                   END
019200940304     C*----------------------------------------------------*
019300940304     C*  Controllo limite delle due date
019400940304     C     SPDDA         IFGT      SPADA
019500950127     C                   SETON                                        404128
019600950127     C                   MOVEL     ERR(3)        $MSG
019700940304     C                   END
019800940304     C*----------------------------------------------------*
019900940304     C     FINCT1        ENDSR
019901130715      *---------------------------------------------------------------*
019902130715     c     Chkcrtlib     begsr
019903130715      *---------------------------------------------------------------*
019904130715      *verifica se esiste libreria di salvataggio dati
019905130715     c                   setoff                                       45
019906130715     c                   movea     cmd1          coman
019907130715     c                   eval      %subst(coman:12:10) = wlib
019908130715     c                   exsr      comandi2
019909130715     c                   if        *in34
019910130715     c                   movea     cmd2          coman
019911130715     c                   eval      %subst(coman:12:10) = wlib
019912130715     c                   exsr      comandi2
019913130715     c                   seton                                        46
019914130715     c                   else
019915130715     c                   seton                                        45
019916130715     c                   endif
019917130715     c                   endsr
019918130715     c*-------------------------------------------------
019919130715     c     comandi2      begsr
019920130715     c*-------------------------------------------------
019921130715      *
019922130715     C                   z-add     99            luncmd
019923130715     C                   CALL      'QCMDEXC'                            34
019924130715     C                   parm                    coman
019925130715     C                   parm                    luncmd
019926130715      *
019927130715     c                   endsr
020000940304      *----------------------------------------------------*
020100940304      *  SBMP61: Sottometto elaborazione                   *
020200940304      *----------------------------------------------------*
020300940304     C     SBMP61        BEGSR
020400940304     C*----------------------------------------------------*
020500950127     C     V1CDT1        IFEQ      0
020600950127     C     V1CDT2        ANDEQ     0
020700940311     C                   Z-ADD     999999        SPADA
020800940311     C                   END
020900950127     C                   Z-ADD     SPDDA         D60SDD
021000950127     C                   Z-ADD     SPADA         D60SDA
021100950127     C                   MOVEL     V1CSTP        D60STA
021200950127     C                   MOVEL     V1CPUL        D60PUL
021201130715     C                   MOVEL     wlib          D60lib
021300940304     C*---
021400950127     C                   MOVE      'SC61'        KCOAZ
021500950127     C                   MOVEL     TNSC60        KPJBU
021600940304     C                   CALL      'BCH10'
021700940304     C                   PARM                    KPJBA
021800940304     C*---
021900940304     C                   ENDSR
022000940304      *---------------------------------------------------------------*
022100940304      *  *INZSR: OPERAZIONI INIZIALI                                  *
022200940304      *---------------------------------------------------------------*
022300940304     C     *INZSR        BEGSR
022400940304      *---------------------------------------------------------------*
022500940304     C     *ENTRY        PLIST
022600940304     C                   PARM                    KPJBA
022700940304      *
022800940304     C                   Z-ADD     1             CODUT
022900940304     C                   CALL      'X§PARUT'
023000940304     C                   PARM                    UT§DSE
023100940304     C                   MOVEL     RAGUT         RSUT             20
023200940304     C                   MOVEL     REC80         CNCR80
023300940304     C*----------------------------------------------------*
023400940304     C*  Definizione variabili
023500950125     C     *LIKE         DEFINE    D60SDD        SPDDA
023600950125     C     *LIKE         DEFINE    D60SDA        SPADA
023700940304     C     *LIKE         DEFINE    §§§CSP        PASPUL
023800940304     C*----------------------------------------------------*
023900940304     C                   MOVEL     'N'           £FINE             1
024000940304     C                   MOVEL     '1'           TIPVID            1
024100940304     C*----------------------------------------------------*
024200940304     C     *DTAARA       DEFINE                  §§§EDP
024300940304     C                   IN        §§§EDP
024400940304     C                   MOVEL     §§§CSP        PASPUL
024500940304     C*----------------------------------------------------*
024600940304     C*  Richiamo pgm per sapere se archivio di Work vuoti
024700940304     C                   MOVEL     KNSIF         LIBR             10
024800940304     C                   MOVEL     *BLANK        FILE             10
024900940304     C                   MOVEL     *BLANK        MBR              10
025000940304     C                   Z-ADD     0             NRR              10 0
025100950125     C                   MOVEL     'TNCPB00F'    FILE
025200940304     C                   MOVEL     '*FIRST'      MBR
025300940304     C*
025400940304     C                   CALL      'TRUL51C'
025500940304     C                   PARM                    LIBR
025600940304     C                   PARM                    FILE
025700940304     C                   PARM                    MBR
025800940304     C                   PARM                    NRR
025900940304     C*----------------------------------------------------*
026000940304     C                   ENDSR
026100950125**
026200950125Immettere la data di spedizione                                       01
026300950125Data di spedizione errata                                             02
026400950125Limiti date incompatibili                                             03
026500950125Effettuare almeno una scelta                                          04
026600950125Archivi di Work vuoti impossibile eseguire la stampa                  05
026700950125Immettere parola d'ordine per richiesta pulizia                       06
026800950125Parola d'ordine errata                                                07
026900130715Inserire la parte finale del nome libreria in forma numerica          08
026901130715Libreria già esistente proporre altro nome                            09
027000130715**  CMD1
027100130715CHKOBJ OBJ(SAVCONxxxx) OBJTYPE(*LIB)
027200130715**  CMD2
027300130715CRTLIB LIB(SAVCONXXXX)
