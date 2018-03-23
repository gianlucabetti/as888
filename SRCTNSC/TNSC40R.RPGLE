000100931008     H DECEDIT('0,') DATEDIT(*DMY.)
000200931008?     *--------------------------------------------------------------*
000300950626      * TNSC40R                                                      *
000400931008      *       *-----------------------------------------*            *
000500950626      *           INTERROGAZIONE CONTRASSEGNI X SEDE                 *
000600931008      *       *-----------------------------------------*            *
000700931008      *                                                              *
000800931008?     *--------------------------------------------------------------*
000900931008      * KC -FINE LAVORO                                              *
001000931008      * KG -INTERROGAZIONE BOLLE                                     *
001100931008      * KJ -STAMPA                                                   *
001200931008?     *--------------------------------------------------------------*
001300950626     FTNSC40D   CF   E             WORKSTN
001400950712     FTNCSB03L  IF   E           K DISK
001500000107     FTABEL00F  IF   E           K DISK
001600950626     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001700950704?     *--------------------------------------------------------------*
001800950704?     *  SCHIERE                                                     *
001900950704?     *--------------------------------------------------------------*
002000160802     D ERR             S             70    DIM(24) CTDATA PERRCD(1)
002100000107     D CCV             S              3    DIM(200)
002200931008?     *--------------------------------------------------------------*
002300931008?     *  DS                                                          *
002400931008?     *--------------------------------------------------------------*
002500931008     D KPJBA         E DS
002600931008      *
002700971223     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002800971223     D CNACO         E DS                  EXTNAME(CNACO00F)
002900971223     D CNIND         E DS                  EXTNAME(CNIND00F)
003000971223     D CNCLP         E DS                  EXTNAME(CNCLP00F)
003100971223     D FNCLS         E DS                  EXTNAME(FNCLS00F)
003200120612     D TNSC40ds      E DS                  EXTNAME(TNSC40DS) prefix(£)
003300120612     D TNSC40        E DS                  EXTNAME(TNSC40DS)
003400990728     D TNSB50        E DS                  EXTNAME(TNSB50DS)
003500950704     D TNSC84        E DS                  EXTNAME(TNSC84DS)
003600931008      *
003700931008     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800950704     D  TCU                  398    697
003900950704     D                                     DIM(50)
004000950704     D  KCU                  698    847P 0
004100950704     D                                     DIM(50)
004200950704     D                                     PACKEVEN
004300950704     D TCUDS           DS
004400950704     D  F34                    3      4
004500950704     D  F56                    5      6
004600931008     D CNCR80        E DS
004700931008      *
004800941102     D WLBDA8          DS
004900941102     D  G02DAT                 1      8  0
005000941102     D  G02INV                 9     16  0
005100941102     D  G02ERR                17     17
005200941102     D  G02TGI                18     22  0
005300931008     D                SDS
005400931008     D  NOMPG                  1     10
005500950626?     *--------------------------------------------------------------*
005600950626?     *  FILE                                                        *
005700950626?     *--------------------------------------------------------------*
005800950626     IAZORG01L  AA
005900950626     I                             P    4    5 0ORGFIL
006000950626     I                                 14   33  ORGDES
006100931008      *---------------------------------------------------------------*
006200931008      *   C I C L O      P R I N C I P A L E                          *
006300931008      *---------------------------------------------------------------*
006400931008     C*  INIZIALIZZAZIONE VIDEATA
006500931008     C                   EXSR      INZVID
006600931008     C*
006700931008     C*  LOOP
006800950704     C     WFINE         DOUEQ     'S'
006900931008     C*
007000950704     C     WTPVID        CASEQ     '1'           GESVD1
007100931008     C                   END
007200931008     C*
007300931008     C                   END
007400971223     C*
007500971223     C                   CLEAR                   TIBS69
007600971223     C                   MOVEL     'C'           I69TLA
007700971223     C                   CALL      'TIBS69R'
007800971223     C                   PARM                    TIBS69
007900971223     C                   PARM                    CNACO
008000971223     C                   PARM                    CNIND
008100971223     C                   PARM                    CNCLP
008200971223     C                   PARM                    FNCLS
008300931008     C*
008400931008     C                   SETON                                        LR
008500931008      *---------------------------------------------------------------*
008600931008      *  INZVID: INIZIALIZZAZIONE PARAMETRI VIDEO                     *
008700931008      *---------------------------------------------------------------*
008800931008     C     INZVID        BEGSR
008900931008     C*
009000950626     C                   SETOFF                                       404142
009100950626     C                   SETOFF                                       434445
009200950626     C                   SETOFF                                       464748
009300950626     C                   SETOFF                                       495051
009400950626     C                   SETOFF                                       525354
009500950627     C                   SETOFF                                       555657
009600950704     C                   SETOFF                                       585928
009700000107     C                   SETOFF                                       906061
009800151026     C                   setoff                                       62
009900950626     C                   MOVEL     *BLANKS       $MSG
010000950626     C                   MOVEL     *BLANK        V1CNAS
010100950718     C                   MOVEL     *BLANK        V1CINT
010200160414     C                   MOVEL     *BLANK        V1CINT3
010300160414     C                   MOVEL     *BLANK        V1Crag
010400950704     C                   Z-ADD     1             V1CSCE
010500950626     C                   MOVEL     *BLANK        V1CSTA
010600931008     C                   MOVEL     NOMPG         NOMPGM
010700160420     C                   MOVE      WHHDAT        V1CAAS
010800950718     C                   Z-ADD     0             V1CRGP
010900950718     C                   Z-ADD     0             V1CLNP
011000950626     C                   Z-ADD     0             V1CNSP
011100950626     C                   Z-ADD     0             V1CNRS
011200950626     C                   Z-ADD     0             V1CRMN
011300950626     C                   Z-ADD     0             V1CSD1
011400950626     C                   Z-ADD     0             V1CSA1
011500950626     C                   Z-ADD     0             V1CLP1
011600950626     C                   Z-ADD     0             V1CSPD
011700950626     C                   Z-ADD     0             V1CSPA
011800950626     C                   Z-ADD     0             V1CKCS
011900950626     C                   Z-ADD     0             V1CPDD
012000950626     C                   Z-ADD     0             V1CPDA
012100950626     C                   Z-ADD     0             V1CNDP
012200950626     C                   Z-ADD     0             V1CKCP
012300950626     C                   Z-ADD     0             V1CDDT
012400950626     C                   Z-ADD     0             V1CNDT
012500950626     C                   Z-ADD     0             V1CABI
012600950626     C                   Z-ADD     0             V1CCAB
012700140325     C                   Z-ADD     0             V1Cimp
012800971223     C                   MOVEL     *BLANKS       V1CPIG
012900000107     C                   MOVEL     *BLANKS       V1CPDV
013000151026     C                   CLEAR                   V1Cnbn
013100931011     C*
013200931011     C                   ENDSR
013300931011      *---------------------------------------------------------------*
013400931011      *  GESVD1: GESTIONE PRIMA VIDEATA RICHIESTA PARAMETRI           *
013500931011      *---------------------------------------------------------------*
013600931011     C     GESVD1        BEGSR
013700931011     C*
013800950626     C                   EXFMT     SC40D01
013900950626     C                   MOVEL     *BLANKS       $MSG
014000950626     C                   SETOFF                                       404142
014100950626     C                   SETOFF                                       434445
014200950626     C                   SETOFF                                       464748
014300950626     C                   SETOFF                                       495051
014400950626     C                   SETOFF                                       525354
014500950627     C                   SETOFF                                       555657
014600950704     C                   SETOFF                                       585928
014700000107     C                   SETOFF                                       906061
014800151026     C                   setoff                                       62
014900931011     C*  Fine elaborazione
015000931011     C     *INKC         IFEQ      '1'
015100950704     C                   MOVEL     'S'           WFINE
015200931011     C                   GOTO      FINVD1
015300931011     C                   END
015400931011     C*  Interrogazione bolle
015500931011     C     *INKG         IFEQ      '1'
015600950626     C                   EXSR      INTBOL
015700950626     C                   GOTO      FINVD1
015800931011     C                   END
015900931011     C*  Controlli
016000950627     C                   EXSR      CNTVD1
016100950627     C   90
016200950627     COR 28              GOTO      FINVD1
016300931012     C*  Stampa
016400931012     C     *INKJ         IFEQ      '1'
016500931012     C                   EXSR      STAMPA
016600931013     C                   ELSE
016700931011     C*  Se non ci sono errori procedo a richiamare i pgm. di interrogazione
016800931012     C                   EXSR      CALLIN
016900931104     C                   EXSR      INZVID
017000931013     C                   END
017100931011     C*
017200950704     C                   Z-ADD     1             V1CSCE
017300950626     C*
017400931011     C     FINVD1        ENDSR
017500950626      *---------------------------------------------------------------*
017600950626      *  INTBOL: INTERROGAZIONE BOLLE                                 *
017700950626      *---------------------------------------------------------------*
017800950626     C     INTBOL        BEGSR
017900950626     C*----------------------------------------------------*
018000950626     C                   SETON                                        30
018100990728     C                   CLEAR                   TNSB50
018200990729     C                   MOVEL     'C05'         I50OP0
018300990729     C                   MOVEL     '0'           O50F03
018400990729     C                   MOVEL     '0'           O50F12
018500990729     C                   MOVEL     '0'           O50ERR
018600990729     C                   MOVEL     *BLANKS       O50MSG
018700950627     C*
018800990729     C     V1CSCE        IFEQ      1
018900950714     C     V1CNSP        IFNE      0
019000990729     C                   Z-ADD     V1CAAS        D50AAS
019100990729     C                   Z-ADD     V1CLNP        D50LNP
019200990729     C                   Z-ADD     V1CNRS        D50NRS
019300990729     C                   Z-ADD     V1CNSP        D50NSP
019400990729     C                   Z-ADD     V1CAAS        KAAS
019500990729     C                   Z-ADD     V1CLNP        KLNP
019600990729     C                   Z-ADD     V1CNRS        KNRS
019700990729     C                   Z-ADD     V1CNSP        KNSP
019800950712     C                   MOVEL     *BLANKS       CSBTBL
019900950712     C     KCSB          CHAIN     TNCSB03L                           30
020000950712     C*  Se non trovo bolla con quel codice emetto subfile
020100950714     C     *IN30         IFEQ      '0'
020200990729     C                   MOVEL     CSBTBL        D50TBL
020300990729     C                   MOVEL     TNSB50        KPJBU
020400990729     C                   CALL      'TNSB50R'
020500990910     C                   PARM                    KPJBA
020600990910     C                   MOVEL     KPJBU         TNSB50
020700990729     C     O50ERR        IFEQ      *BLANKS
020800990729     C                   GOTO      FININT
020900990729     C                   END
021000990729     C                   END
021100990729     C* Se ho richiamato il pgm con il numero spedizione/
021200990729     C* codice bolla prova a richiamare senza codice bolla
021300990729     C* come del resto faccio se non ho recuperato il C/Ass.
021400990729     C                   MOVEL     *BLANKS       D50TBL
021500990729     C                   MOVEL     TNSB50        KPJBU
021600990729     C                   CALL      'TNSB50R'
021700950714     C                   PARM                    KPJBA
021800990729     C                   MOVEL     KPJBU         TNSB50
021900950626     C*
022000950714     C                   ELSE
022100990729     C*  Se non non avevo impostato il nr.spedizione ma ho selezionato
022200990729     C*  l'interrogazione x tipo spedizione permetto la selezione
022300990729     C                   MOVEL     'C01'         I50OP0
022400990729     C                   MOVEL     TNSB50        KPJBU
022500990729     C                   CALL      'TNSB50R'
022600950627     C                   PARM                    KPJBA
022700990729     C                   MOVEL     KPJBU         TNSB50
022800990729     C* Controllo se ho reimpostato il numero della spedizione
022900990729     C     D50NSP        IFNE      0
023000990729     C                   Z-ADD     D50AAS        V1CAAS
023100990729     C                   Z-ADD     D50LNP        V1CLNP
023200990729     C                   Z-ADD     D50NRS        V1CNRS
023300990729     C                   Z-ADD     D50NSP        V1CNSP
023400990729     C                   END
023500990729     C                   END
023600990729     C* se non ho impostato l'interrogazione x numero spedizione
023700990729     C* non propongo la selezione
023800990729     C                   ELSE
023900990729     C                   MOVEL     'C00'         I50OP0
024000990729     C                   MOVEL     TNSB50        KPJBU
024100990729     C                   CALL      'TNSB50R'
024200990729     C                   PARM                    KPJBA
024300990729     C                   MOVEL     KPJBU         TNSB50
024400990729     C                   END
024500950627     C*
024600990729     C     FININT        ENDSR
024700931011      *---------------------------------------------------------------*
024800931011      *  CNTVD1: CONTROLLI PRIMA VIDEATA                              *
024900931011      *---------------------------------------------------------------*
025000931011     C     CNTVD1        BEGSR
025100950627     C*
025200950627     C*  Eseguo controlli in base al tipo di interrogazione richiesta
025300950704     C     V1CSCE        CASEQ     1             CNTSC1
025400950704     C     V1CSCE        CASEQ     2             CNTSC2
025500950704     C     V1CSCE        CASEQ     3             CNTSC3
025600950704     C     V1CSCE        CASEQ     4             CNTSC4
025700950704     C     V1CSCE        CASEQ     5             CNTSC5
025800151026     C     V1CSCE        CASEQ     6             CNTSC6
025900950627     C                   END
026000950627     C* Controlli su paerzializzazioni
026100950627     C  N28              EXSR      CNTPAR
026200950627     C*
026300950627     C                   ENDSR
026400950627      *---------------------------------------------------------------*
026500950627      *  CNTSC1: Controlli  su selezione x numero spedizione          *
026600950627      *---------------------------------------------------------------*
026700950627     C     CNTSC1        BEGSR
026800950627     C*
026900931012     C*  Stampa non permessa se seleziono x numero spedizione
027000931012     C     *INKJ         IFEQ      '1'
027100950627     C                   SETON                                        4028
027200950627     C                   MOVEL     ERR(1)        $MSG
027300950627     C                   GOTO      FINSC1
027400950627     C                   END
027500950627     C*  Anno di spedizione
027600950627     C                   MOVEL     V1CAAS        WAA2              2 0
027700950627     C     WAA2          IFEQ      0
027800950627     C     V1CAAS        IFGT      60
027900950627     C                   ADD       1900          V1CAAS
028000950627     C                   ELSE
028100950627     C                   ADD       2000          V1CAAS
028200950627     C                   END
028300950704     C                   END
028400950627     C*  Linea di partenza obbligatoria
028500950627     C     V1CLNP        IFEQ      0
028600950627     C                   SETON                                        4128
028700950627     C                   MOVEL     ERR(2)        $MSG
028800950627     C                   GOTO      FINSC1
028900950627     C                   ELSE
029000950627     C*  Linea di partenza inesistente
029100950627     C                   MOVE      V1CLNP        ORGFIL
029200950627     C     ORGFIL        CHAIN     AZORG01L                           30
029300950627     C     *IN30         IFEQ      '1'
029400950627     C                   SETON                                        4128
029500950627     C                   MOVEL     ERR(3)        $MSG
029600950627     C                   GOTO      FINSC1
029700950627     C                   END
029800950627     C                   END
029900950627     C*  Numero di spedizione obbligatorio
030000950627     C     V1CNSP        IFEQ      0
030100950627     C                   SETON                                        4228
030200950627     C                   MOVEL     ERR(4)        $MSG
030300950627     C                   GOTO      FINSC1
030400931012     C                   END
030500950627     C*
030600950627     C     FINSC1        ENDSR
030700950627      *---------------------------------------------------------------*
030800950627      *  CNTSC2: Cntrolli selezione x dati numero rif. mittente       *
030900950627      *---------------------------------------------------------------*
031000950627     C     CNTSC2        BEGSR
031100950627     C*
031200950627     C*  Controllo numero riferimento mittente
031300950627     C     V1CRMN        IFEQ      0
031400950627     C                   SETON                                        4328
031500950627     C                   MOVEL     ERR(5)        $MSG
031600950627     C                   GOTO      FINSC2
031700950627     C                   END
031800950627     C*  Controllo limiti date spedizione
031900950627     C     V1CSD1        IFEQ      0
032000950627     C                   SETON                                        4428
032100950627     C                   MOVEL     ERR(8)        $MSG
032200950627     C                   GOTO      FINSC2
032300950627     C                   END
032400950627     C*
032500950627     C                   MOVE      V1CSD1        G02DAT
032600950627     C                   MOVEL     *BLANK        G02ERR
032700950627     C                   CALL      'XSRDA8'
032800950627     C                   PARM                    WLBDA8
032900950627     C     G02ERR        IFEQ      '1'
033000950627     C                   SETON                                        4428
033100950627     C                   MOVEL     ERR(6)        $MSG
033200950627     C                   GOTO      FINSC2
033300950627     C                   ELSE
033400950627     C                   Z-ADD     G02INV        WSDT1
033500950627     C                   Z-ADD     G02TGI        WTGIDA
033600950627     C                   Z-ADD     G02DAT        V1CSD1
033700950627     C                   END
033800950627     C*
033900950627     C     V1CSA1        IFEQ      0
034000950627     C                   MOVE      V1CSD1        V1CSA1
034100950627     C                   SETON                                        90
034200950627     C                   END
034300950627     C*
034400950627     C                   MOVE      V1CSA1        G02DAT
034500950627     C                   MOVEL     *BLANK        G02ERR
034600950627     C                   CALL      'XSRDA8'
034700950627     C                   PARM                    WLBDA8
034800950627     C     G02ERR        IFEQ      '1'
034900950627     C                   SETON                                        4528
035000950627     C                   MOVEL     ERR(6)        $MSG
035100950627     C                   GOTO      FINSC2
035200950627     C                   ELSE
035300950627     C                   Z-ADD     G02INV        WSAT1
035400950627     C                   Z-ADD     G02TGI        WTGIA
035500950627     C                   Z-ADD     G02DAT        V1CSA1
035600950627     C                   END
035700950627     C*  Controllo congruenza date
035800950627     C     WSDT1         IFGT      WSAT1
035900950627     C                   SETON                                        444528
036000950627     C                   MOVEL     ERR(7)        $MSG
036100950627     C                   GOTO      FINSC2
036200950627     C                   END
036300950627     C*  Controllo differenza
036400950717     C     V1CLP1        IFEQ      0
036500950627     C     WTGIA         SUB       WTGIDA        WDIFF
036600160802     c                   if        *inkj
036700160802     C     WDIFF         IFGT      90
036800160802     c     simpou        andne     046
036900160802     C                   SETON                                        444528
037000160802     C                   MOVEL     ERR(24)       $MSG
037100160802     C                   GOTO      FINSC2
037200160802     C                   END
037300160802     c                   else
037400160802     C     WDIFF         IFGT      7
037500950627     C                   SETON                                        444528
037600950627     C                   MOVEL     ERR(9)        $MSG
037700950627     C                   GOTO      FINSC2
037800950627     C                   END
037900160802     c                   endif
038000950627     C                   ELSE
038100950627     C*  Controllo linea di partenza
038200950627     C                   MOVE      V1CLP1        ORGFIL
038300950627     C     ORGFIL        CHAIN     AZORG01L                           30
038400950627     C     *IN30         IFEQ      '1'
038500950627     C                   SETON                                        4628
038600950627     C                   MOVEL     ERR(3)        $MSG
038700950627     C                   GOTO      FINSC2
038800950627     C                   END
038900950627     C                   END
039000950627     C*
039100950627     C     FINSC2        ENDSR
039200950627      *---------------------------------------------------------------*
039300950627      *  CNTSC3: Cntrolli selezione x dati data spedizione/cliente    *
039400950627      *---------------------------------------------------------------*
039500950627     C     CNTSC3        BEGSR
039600950627     C*
039700950627     C*  Controllo data di spedizione da....
039800950627     C     V1CSPD        IFEQ      0
039900950627     C                   SETON                                        4728
040000950627     C                   MOVEL     ERR(8)        $MSG
040100950627     C                   GOTO      FINSC3
040200950627     C                   END
040300950627     C*
040400950627     C                   MOVE      V1CSPD        G02DAT
040500950627     C                   MOVEL     *BLANK        G02ERR
040600950627     C                   CALL      'XSRDA8'
040700950627     C                   PARM                    WLBDA8
040800950627     C     G02ERR        IFEQ      '1'
040900950627     C                   SETON                                        4728
041000950627     C                   MOVEL     ERR(6)        $MSG
041100950627     C                   GOTO      FINSC3
041200950627     C                   ELSE
041300950627     C                   Z-ADD     G02INV        WSDDAT
041400950627     C                   Z-ADD     G02TGI        WTGIDA
041500950627     C                   Z-ADD     G02DAT        V1CSPD
041600950627     C                   END
041700950627     C*
041800950627     C*  Controllo data di spedizione a ....
041900950627     C     V1CSPA        IFEQ      0
042000950627     C                   MOVE      V1CSPD        V1CSPA
042100950627     C                   SETON                                        90
042200950627     C                   END
042300950627     C*
042400950627     C                   MOVE      V1CSPA        G02DAT
042500950627     C                   MOVEL     *BLANK        G02ERR
042600950627     C                   CALL      'XSRDA8'
042700950627     C                   PARM                    WLBDA8
042800950627     C     G02ERR        IFEQ      '1'
042900950627     C                   SETON                                        4828
043000950627     C                   MOVEL     ERR(6)        $MSG
043100950627     C                   GOTO      FINSC3
043200950627     C                   ELSE
043300950627     C                   Z-ADD     G02INV        WSADAT
043400950627     C                   Z-ADD     G02TGI        WTGIA
043500950627     C                   Z-ADD     G02DAT        V1CSPA
043600950627     C                   END
043700950627     C*
043800950627     C*  Controllo limite delle due date
043900950627     C     WSDDAT        IFGT      WSADAT
044000950627     C                   SETON                                        284748
044100950627     C                   MOVEL     ERR(7)        $MSG
044200950627     C                   GOTO      FINSC3
044300950627     C                   END
044400950627     C*  Controllo differenza
044500950627     C     V1CKCS        IFEQ      0
044600950627     C     WTGIA         SUB       WTGIDA        WDIFF
044700160802     c                   if        *inkj
044800160802     C     WDIFF         IFGT      90
044900160802     c     simpou        andne     046
045000160802     C                   SETON                                        474828
045100160802     C                   MOVEL     ERR(24)       $MSG
045200160802     C                   GOTO      FINSC3
045300160802     C                   END
045400160802     c                   else
045500160802     C     WDIFF         IFGT      7
045600950627     C                   SETON                                        474828
045700950627     C                   MOVEL     ERR(9)        $MSG
045800950627     C                   GOTO      FINSC3
045900950627     C                   END
046000160802     c                   endif
046100950627     C                   ELSE
046200950627     C*  Controllo cliente
046300971223      * CHIAMO TIBS69 PER RICERCA CODICE
046400971223     C                   CLEAR                   TIBS69
046500971223     C                   CLEAR                   CNACO
046600971223     C                   CLEAR                   CNIND
046700971223     C                   CLEAR                   CNCLP
046800971223     C                   CLEAR                   FNCLS
046900971223     C                   MOVEL     ' '           I69TLA
047000971223     C                   MOVEL     KNSIF         I69SIF
047100971223     C                   Z-ADD     V1CKCS        I69KAC
047200971223     C                   CALL      'TIBS69R'
047300971223     C                   PARM                    TIBS69
047400971223     C                   PARM                    CNACO
047500971223     C                   PARM                    CNIND
047600971223     C                   PARM                    CNCLP
047700971223     C                   PARM                    FNCLS
047800971223     C     O69ERR        IFNE      *BLANKS
047900950627     C                   SETON                                        4928
048000950627     C                   MOVEL     ERR(10)       $MSG
048100950627     C                   GOTO      FINSC3
048200950627     C                   END
048300950627     C                   END
048400950627     C*
048500950627     C     FINSC3        ENDSR
048600950627      *---------------------------------------------------------------*
048700950627      *  CNTSC4: Cntrolli selezione x dati doc.pagamnto/cliente       *
048800950627      *---------------------------------------------------------------*
048900950627     C     CNTSC4        BEGSR
049000950627     C*
049100931011     C*  Controllo data di pagamento da ....
049200950627     C     V1CPDD        IFEQ      0
049300950627     C                   SETON                                        5028
049400950627     C                   MOVEL     ERR(11)       $MSG
049500950627     C                   GOTO      FINSC4
049600950627     C                   END
049700931011     C*
049800950627     C                   MOVE      V1CPDD        G02DAT
049900931011     C                   MOVEL     *BLANK        G02ERR
050000941102     C                   CALL      'XSRDA8'
050100941102     C                   PARM                    WLBDA8
050200931011     C     G02ERR        IFEQ      '1'
050300950627     C                   SETON                                        5028
050400950627     C                   MOVEL     ERR(6)        $MSG
050500950627     C                   GOTO      FINSC4
050600931011     C                   ELSE
050700950627     C                   Z-ADD     G02INV        WPDDAT
050800950627     C                   Z-ADD     G02TGI        WTGIDA
050900950627     C                   Z-ADD     G02DAT        V1CPDD
051000931011     C                   END
051100931011     C*
051200931011     C*  Controllo data di pagamento a ....
051300950627     C     V1CPDA        IFEQ      0
051400950627     C                   MOVE      V1CPDD        V1CPDA
051500950627     C                   SETON                                        90
051600931011     C                   END
051700931011     C*
051800950627     C                   MOVE      V1CPDA        G02DAT
051900931011     C                   MOVEL     *BLANK        G02ERR
052000941102     C                   CALL      'XSRDA8'
052100941102     C                   PARM                    WLBDA8
052200931011     C     G02ERR        IFEQ      '1'
052300950627     C                   SETON                                        5128
052400950627     C                   MOVEL     ERR(6)        $MSG
052500950627     C                   GOTO      FINSC4
052600931011     C                   ELSE
052700950627     C                   Z-ADD     G02INV        WPADAT
052800950627     C                   Z-ADD     G02TGI        WTGIA
052900950627     C                   Z-ADD     G02DAT        V1CPDA
053000950627     C                   END
053100931011     C*
053200931011     C*  Controllo limite delle due date
053300950627     C     WPDDAT        IFGT      WPADAT
053400950627     C                   SETON                                        505128
053500950627     C                   MOVEL     ERR(7)        $MSG
053600950627     C                   GOTO      FINSC4
053700931011     C                   END
053800931011     C*
053900950704     C     WTGIA         SUB       WTGIDA        WDIFF
054000160802     c                   if        *inkj
054100160802     C     WDIFF         IFGT      90
054200160802     c     simpou        andne     046
054300160802     C                   SETON                                        505128
054400160802     C                   MOVEL     ERR(24)       $MSG
054500160802     C                   GOTO      FINSC4
054600160802     C                   END
054700160802     c                   else
054800160802     C     WDIFF         IFGT      7
054900950627     C                   SETON                                        505128
055000950627     C                   MOVEL     ERR(9)        $MSG
055100950627     C                   GOTO      FINSC4
055200931011     C                   END
055300160802     c                   endif
055400000107     C* Controllo la divisa se immessa
055500000107     C     V1CPDV        IFNE      *BLANKS
055600000107     C                   Z-ADD     1             X
055700000107     C     V1CPDV        LOOKUP    CCV(X)                                 32
055800000107     C     *IN32         IFEQ      '0'
055900000107     C                   SETON                                        6128
056000000107     C                   MOVEL     ERR(21)       $MSG
056100000107     C                   GOTO      FINSC4
056200000107     C                   END
056300000107     C                   END
056400950627     C*  Controllo cliente
056500971223      * CHIAMO TIBS69 PER RICERCA CODICE
056600971223     C                   CLEAR                   TIBS69
056700971223     C                   CLEAR                   CNACO
056800971223     C                   CLEAR                   CNIND
056900971223     C                   CLEAR                   CNCLP
057000971223     C                   CLEAR                   FNCLS
057100971223     C                   MOVEL     ' '           I69TLA
057200971223     C                   MOVEL     KNSIF         I69SIF
057300971223     C                   Z-ADD     V1CKCP        I69KAC
057400971223     C                   CALL      'TIBS69R'
057500971223     C                   PARM                    TIBS69
057600971223     C                   PARM                    CNACO
057700971223     C                   PARM                    CNIND
057800971223     C                   PARM                    CNCLP
057900971223     C                   PARM                    FNCLS
058000971223     C     O69ERR        IFNE      *BLANKS
058100950627     C                   SETON                                        5228
058200950627     C                   MOVEL     ERR(10)       $MSG
058300950627     C                   GOTO      FINSC4
058400950627     C                   END
058500160505     c                   if        v1ckcp > 0 and v1crag <> *blank
058600160505     C                   SETON                                        5228
058700160505     C                   MOVEL     ERR(23)       $MSG
058800160505     C                   GOTO      FINSC4
058900160505     C                   END
059000931011     C*
059100950627     C     FINSC4        ENDSR
059200950627      *---------------------------------------------------------------*
059300950627      *  CNTSC5: Cntrolli selezione x dati nr.e data distinta         *
059400950627      *---------------------------------------------------------------*
059500950627     C     CNTSC5        BEGSR
059600950627     C*
059700950627     C*  Controllo data distinta
059800950627     C     V1CDDT        IFEQ      0
059900950627     C                   SETON                                        5328
060000950627     C                   MOVEL     ERR(12)       $MSG
060100950627     C                   GOTO      FINSC5
060200950627     C                   END
060300950627     C*
060400950627     C                   MOVE      V1CDDT        G02DAT
060500950627     C                   MOVEL     *BLANK        G02ERR
060600950627     C                   CALL      'XSRDA8'
060700950627     C                   PARM                    WLBDA8
060800950627     C     G02ERR        IFEQ      '1'
060900950627     C                   SETON                                        5328
061000950627     C                   MOVEL     ERR(6)        $MSG
061100950627     C                   GOTO      FINSC5
061200950627     C                   ELSE
061300950704     C                   Z-ADD     G02INV        WDDT              8 0
061400950627     C                   Z-ADD     G02DAT        V1CDDT
061500950627     C                   END
061600950627     C*  Controllo numero distinta
061700950627     C     V1CNDT        IFEQ      0
061800950627     C                   SETON                                        5428
061900950627     C                   MOVEL     ERR(13)       $MSG
062000950627     C                   GOTO      FINSC5
062100950627     C                   END
062200950627     C*  RAggruppamento
062300950627     C     V1CRGP        IFEQ      0
062400950627     C                   SETON                                        5828
062500950627     C                   MOVEL     ERR(16)       $MSG
062600950627     C                   GOTO      FINSC5
062700950627     C                   END
062800950627     C*
062900950627     C     FINSC5        ENDSR
063000151026      *---------------------------------------------------------------*
063100151026      *  CNTSC6: Controlli selezione x dati ID bonifico               *
063200151026      *---------------------------------------------------------------*
063300151026     c     CNTSC6        BEGSR
063400151026      *
063500151026      *?Controllo ID bonifico?
063600151026     c                   Select
063700151026      * -?Se *zero: si emette errore a video?
063800151026     c                   When      V1Cnbn = *zero
063900151026     c                   eval      *in62  = *on
064000151026     c                   eval      *in28  = *on
064100151026     c                   eval      $Msg   = Err(22)
064200151026     c                   leavesr
064300151026      * -?Se negativo: si calcola il valore assoluto?
064400151026     c                   When      V1Cnbn < *zero
064500151026     c                   eval      V1Cnbn = %abs(V1Cnbn)
064600151026     c                   EndSl
064700151026      *
064800151026     c                   ENDSR
064900950627      *---------------------------------------------------------------*
065000950627      *  CNTPAR: Controlli  su parzializzazioni                       *
065100950627      *---------------------------------------------------------------*
065200950627     C     CNTPAR        BEGSR
065300950704     C*
065400960911     C*  Se ho selezionato interrogazione x dati pagamento stato obblig.
065500960911     C     V1CSCE        IFEQ      4
065600960911     C     V1CSTA        ANDEQ     ' '
065700960911     C                   SETON                                        405928
065800960911     C                   MOVEL     ERR(19)       $MSG
065900960911     C                   GOTO      FINPAR
066000960911     C                   END
066100950704     C*  Se ho selezionato interrogazione x dati pagamento non posso selez.
066200950704     C*  C/assegni non pagati
066300950704     C     V1CSCE        IFEQ      4
066400950704     C     V1CSTA        ANDNE     'P'
066500960911     C     V1CSTA        ANDNE     'T'
066600960911     C     V1CSTA        ANDNE     'R'
066700950704     C                   SETON                                        405928
066800950704     C                   MOVEL     ERR(17)       $MSG
066900950706     C                   GOTO      FINPAR
067000950704     C                   END
067100960911     C*  Se ho selezionato interrogazione x dati dstinta non posso selez.
067200960911     C*  C/assegni non incassati
067300960911     C     V1CSCE        IFEQ      5
067400960911     C     V1CSTA        ANDNE     ' '
067500960911     C     V1CSTA        ANDNE     'I'
067600960911     C     V1CSTA        ANDNE     'P'
067700960911     C     V1CSTA        ANDNE     'T'
067800960911     C     V1CSTA        ANDNE     'R'
067900960911     C                   SETON                                        405928
068000960911     C                   MOVEL     ERR(17)       $MSG
068100960911     C                   GOTO      FINPAR
068200960911     C                   END
068300950706     C*  Se ho impostato o CAB o Nr.Assegno o ABI non posso interrogare per
068400950706     C*  spedizione/rif.mittente
068500950706     C     V1CSCE        IFEQ      2
068600950719     C     V1CSCE        OREQ      1
068700950706     C     V1CABI        IFNE      *ZEROS
068800950706     C                   SETON                                        405528
068900950706     C                   MOVEL     ERR(18)       $MSG
069000950706     C                   GOTO      FINPAR
069100950706     C                   END
069200950706     C     V1CCAB        IFNE      *ZEROS
069300950706     C                   SETON                                        405628
069400950706     C                   MOVEL     ERR(18)       $MSG
069500950706     C                   GOTO      FINPAR
069600950706     C                   END
069700950706     C     V1CNAS        IFNE      *ZEROS
069800950706     C     V1CNAS        ANDNE     *BLANKS
069900950706     C                   SETON                                        405728
070000950706     C                   MOVEL     ERR(18)       $MSG
070100950706     C                   GOTO      FINPAR
070200950706     C                   END
070300950706     C                   END
070400950627     C*  Se ho impostato o CAB o Nr.Assegno devo impostare ABI e viceversa
070500950627     C     V1CABI        IFEQ      0
070600950627     C     V1CCAB        IFNE      0
070700950627     C     V1CNAS        ORNE      *BLANKS
070800950627     C                   SETON                                        5528
070900950627     C                   MOVEL     ERR(14)       $MSG
071000950627     C                   END
071100950627     C*
071200950627     C                   END
071300950627     C*
071400950706     C     FINPAR        ENDSR
071500931012      *---------------------------------------------------------------*
071600931012      *  STAMPA: RICHIAMO I VARI PROGRAMMI DI STAMPA                  *
071700931012      *---------------------------------------------------------------*
071800931012     C     STAMPA        BEGSR
071900950627     C*----------------------------------------------------*
072000950720     C                   CLEAR                   TNSC40
072100950704     C     V1CSCE        IFEQ      5
072200950627     C                   CLEAR                   TNSC84
072300950627     C                   Z-ADD     V1CRGP        D84RGF
072400950627     C                   Z-ADD     V1CNDT        D84NDT
072500950627     C                   Z-ADD     WDDT          D84DDC
072600950627     C                   MOVEL     TNSC84        KPJBU
072700950627     C                   MOVEL     'SC84'        KCOAZ
072800950627     C                   CALL      'BCH10'
072900950627     C                   PARM                    KPJBA
073000950717     C                   ELSE
073100950717     C                   MOVEL     V1CSCE        D40INT
073200950717     C                   MOVEL     V1CSTA        D40TCR
073300950717     C                   MOVE      V1CABI        D40ABI
073400950717     C                   MOVE      V1CCAB        D40CAB
073500140325     C                   MOVE      V1Cimp        D40imp
073600950717     C                   MOVEL     V1CNAS        D40NAS
073700950717     C                   MOVEL     *BLANK        KPJBU
073800950717     C                   MOVE      V1CRMN        D40NRM
073900950717     C                   MOVE      WSDT1         D40SD1
074000950717     C                   MOVE      WSAT1         D40SA1
074100950717     C                   MOVE      V1CLP1        D40LP1
074200950717     C                   MOVE      WSDDAT        D40SDD
074300950717     C                   MOVE      WSADAT        D40SDA
074400950717     C                   MOVE      V1CKCS        D40SCD
074500950717     C                   MOVE      WPDDAT        D40PDD
074600950717     C                   MOVE      WPADAT        D40PDA
074700950717     C                   MOVE      V1CINT        D40PTN
074800950717     C                   MOVE      V1CNDP        D40PND
074900950717     C                   MOVE      V1CKCP        D40PCD
075000971223     C                   MOVE      V1CPIG        D40PIG
075100000110     C                   MOVE      V1CPDV        D40PDV
075200151026     c                   eval      D40nbn = V1Cnbn
075300950717     C                   MOVEL     TNSC40        KPJBU
075400950717     C                   MOVEL     'SC42'        KCOAZ
075500950717     C                   CALL      'BCH10'
075600950717     C                   PARM                    KPJBA
075700950627     C                   END
075800931012     C*----------------------------------------------------*
075900950627     C     D40CMD        IFEQ      3
076000950704     C                   MOVE      'S'           WFINE
076100931012     C                   END
076200931012     C*----------------------------------------------------*
076300931012     C                   ENDSR
076400931011      *---------------------------------------------------------------*
076500931011      *  CALLIN: RICHIAMO I VARI PROGRAMMI DI INTERROGAZIONE          *
076600931011      *---------------------------------------------------------------*
076700931011     C     CALLIN        BEGSR
076800931011     C*
076900950627     C                   CLEAR                   TNSC40
077000950627     C                   MOVEL     V1CINT        D40INT
077100950627     C                   MOVEL     V1CSTA        D40TCR
077200950627     C                   MOVEL     *BLANK        KPJBU
077300120612     C                   MOVE      V1CABI        D40ABI
077400120612     C                   MOVE      V1CCAB        D40CAB
077500120612     C                   MOVEL     V1CNAS        D40NAS
077600140325     C                   MOVE      V1Cimp        D40imp
077700931011     C*----------------------------------------------------*
077800950704     C     V1CSCE        IFEQ      1
077900120612     c                   eval      tnsc40ds  = tnsc40
078000120612     C                   MOVE      V1CAAS        £D40AAS
078100120612     C                   MOVE      V1CLNP        £D40LNP
078200120612     C                   MOVE      V1CNRS        £D40NRS
078300120612     C                   MOVE      V1CNSP        £D40NSP
078400120612     C                   clear                   £D40ABI
078500120612     C                   clear                   £D40CAB
078600120612     C                   clear                   £D40NAS
078700140325     C                   clear                   £D40imp
078800120612     C                   MOVEL     TNSC40ds      KPJBU
078900950627     C                   CALL      'TNSC43R'
079000931011     C                   PARM                    KPJBA
079100931011     C                   END
079200931012     C*----------------------------------------------------*
079300950704     C     V1CSCE        IFEQ      2
079400950627     C                   MOVE      V1CRMN        D40NRM
079500950627     C                   MOVE      WSDT1         D40SD1
079600950627     C                   MOVE      WSAT1         D40SA1
079700950627     C                   MOVE      V1CLP1        D40LP1
079800950627     C                   MOVEL     TNSC40        KPJBU
079900950627     C                   CALL      'TNSC41R2'
080000931012     C                   PARM                    KPJBA
080100931012     C                   END
080200931011     C*----------------------------------------------------*
080300950704     C     V1CSCE        IFEQ      3
080400950627     C                   MOVE      WSDDAT        D40SDD
080500950627     C                   MOVE      WSADAT        D40SDA
080600950627     C                   MOVE      V1CKCS        D40SCD
080700160414     C                   MOVE      V1CINT3       D40PTN
080800950627     C                   MOVEL     TNSC40        KPJBU
080900950627     C     V1CKCS        IFEQ      0
081000950627     C                   CALL      'TNSC41R7'
081100950627     C                   PARM                    KPJBA
081200950627     C                   ELSE
081300950627     C                   CALL      'TNSC41R3'
081400931011     C                   PARM                    KPJBA
081500931011     C                   END
081600950627     C                   END
081700931012     C*----------------------------------------------------*
081800950704     C     V1CSCE        IFEQ      4
081900950627     C                   MOVE      WPDDAT        D40PDD
082000950627     C                   MOVE      WPADAT        D40PDA
082100950627     C                   MOVE      V1CINT        D40PTN
082200950627     C                   MOVE      V1CNDP        D40PND
082300950627     C                   MOVE      V1CKCP        D40PCD
082400971223     C                   MOVE      V1CPIG        D40PIG
082500000107     C                   MOVE      V1CPDV        D40PDV
082600160414     C                   MOVE      V1Crag        D40rag
082700950627     C                   MOVEL     TNSC40        KPJBU
082800950629     C     D40PND        IFEQ      0
082900950629     C                   CALL      'TNSC41R6'
083000931012     C                   PARM                    KPJBA
083100931012     C                   ELSE
083200950629     C                   CALL      'TNSC41R4'
083300931012     C                   PARM                    KPJBA
083400931012     C                   END
083500931012     C                   END
083600950629     C*----------------------------------------------------*
083700950704     C     V1CSCE        IFEQ      5
083800950629     C                   MOVE      WDDT          D40DDT
083900950629     C                   MOVE      V1CNDT        D40NDT
084000950629     C                   MOVE      V1CRGP        D40RGP
084100950629     C                   MOVEL     TNSC40        KPJBU
084200950629     C                   CALL      'TNSC41R5'
084300950629     C                   PARM                    KPJBA
084400950629     C                   END
084500151026      *----------------------------------------------------*
084600151026     c                   IF        V1Csce = 6
084700151026     c                   eval      D40nbn = V1Cnbn
084800151026     c                   eval      KPJBU  = TNSC40
084900151026     c                   call      'TNSC41R8'
085000151026     c                   parm                    KPJBA
085100151026     C                   ENDIF
085200931011     C*----------------------------------------------------*
085300950629     C                   MOVEL     KPJBU         TNSC40
085400950629     C     D40CMD        IFEQ      3
085500950704     C                   MOVE      'S'           WFINE
085600931011     C                   END
085700931011     C*----------------------------------------------------*
085800931011     C                   ENDSR
085900931011      *---------------------------------------------------------------*
086000941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
086100931011      *---------------------------------------------------------------*
086200941102     C     *INZSR        BEGSR
086300931011      *---------------------------------------------------------------*
086400941102     C     *ENTRY        PLIST
086500931011     C                   PARM                    KPJBA
086600931011      *
086700931011     C                   Z-ADD     1             CODUT
086800931011     C                   CALL      'X§PARUT'
086900931011     C                   PARM                    UT§DSE
087000931011     C*          CODUT     CABEQ-1        FINE
087100931011     C                   MOVEL     RAGUT         RSUT             20
087200931011     C                   MOVEL     REC80         CNCR80
087300931011     C                   MOVEL     KNMUS         §SEI              6
087400950704      *
087500950704     C                   Z-ADD     1             X                 3 0
087600950704     C                   Z-ADD     0             KCI
087700950704      *  Ricerco capoconto clienti
087800950704     C     X             DOWLE     50
087900950704     C     KCI           ANDEQ     0
088000950704     C                   MOVE      TCU(X)        TCUDS
088100950704     C     F56           IFEQ      'CG'
088200950704     C     F34           ANDEQ     '01'
088300950704     C                   Z-ADD     KCU(X)        KCI               4 0
088400950704     C                   END
088500950704     C                   ADD       1             X
088600950704     C                   END
088700950626     C*----------------------------------------------------*
088800950626     C*  Definizione Chiavi di accesso
088900950712     C     KCSB          KLIST
089000950712     C                   KFLD                    KAAS
089100950712     C                   KFLD                    KLNP
089200950712     C                   KFLD                    KNRS
089300950712     C                   KFLD                    KNSP
089400000107     C*  Definizione chiave di accesso  TABEL
089500000107     C     KTAB          KLIST
089600000107     C                   KFLD                    KKUT
089700000107     C                   KFLD                    KCOD
089800931011     C*----------------------------------------------------*
089900931011     C*  Definizione variabili
090000950626     C     *LIKE         DEFINE    D40PDD        WPDDAT
090100950626     C     *LIKE         DEFINE    D40PDD        WPADAT
090200950626     C     *LIKE         DEFINE    D40PDD        WSDDAT
090300950626     C     *LIKE         DEFINE    D40PDD        WSADAT
090400950627     C     *LIKE         DEFINE    D40PDD        WSDT1
090500950627     C     *LIKE         DEFINE    D40PDD        WSAT1
090600950626     C     *LIKE         DEFINE    G02TGI        WDIFF
090700950626     C     *LIKE         DEFINE    G02TGI        WTGIA
090800950626     C     *LIKE         DEFINE    G02TGI        WTGIDA
090900990729     C     *LIKE         DEFINE    CSBAAS        KAAS
091000990729     C     *LIKE         DEFINE    CSBLNP        KLNP
091100990729     C     *LIKE         DEFINE    CSBNRS        KNRS
091200990729     C     *LIKE         DEFINE    CSBNSP        KNSP
091300000107     C     *LIKE         DEFINE    TBLKUT        KKUT
091400000107     C     *LIKE         DEFINE    TBLCOD        KCOD
091500931011     C*----------------------------------------------------*
091600950626     C                   MOVEL     'N'           WFINE             1
091700950626     C                   MOVEL     '1'           WTPVID            1
091800941102     C                   TIME                    WHHDAT           14 0
091900000107     C*----------------------------------------------------*
092000000107     C*  Carico tabella delle divise
092100000107     C                   Z-ADD     1             KKUT
092200000107     C                   MOVEL     'CV'          KCOD
092300000107     C                   MOVEL     *BLANKS       CCV
092400000107     C                   Z-ADD     0             X                 3 0
092500000107     C     KTAB          CHAIN     TABEL00F                           31
092600000107     C     *IN31         DOWEQ     '0'
092700000107     C     TBLFLG        IFEQ      ' '
092800000107     C     TBLKEY        ANDNE     *BLANKS
092900000107     C                   ADD       1             X
093000000107     C                   MOVEL     TBLKEY        CCV(X)
093100000107     C                   END
093200000107     C     KTAB          READE     TABEL00F                               31
093300000107     C                   END
093400931011     C*----------------------------------------------------*
093500931011     C                   ENDSR
093600000107**
093700950627Stampa non prevista per interrogazione per numero spedizione           01
093800950627Linea di partenza obbligatoria                                         02
093900950627Linea di partenza inesistente                                          03
094000950627Numero di spedizione obbligatorio                                      04
094100950627Immettere numero riferimento mittente                                  05
094200950627Data non valida                                                        06
094300950627Prima data maggiore della seconda                                      07
094400950627Immettere limite inferiore data di spedizione                          08
094500950627RANGE di date superiore a 7 gg.                                        09
094600950627Codice cliente inesistente                                             10
094700950627Immettere data limite iniziale di pagamento                            11
094800950627Immettere data distinta                                                12
094900950627Immettere numero distinta                                              13
095000950627Immettere Codice ABI                                                   14
095100950627Immettere Codice CAB o numero Assegno                                  15
095200950627Raggruppamento obbligatorio                                            16
095300950704Selezione stato C/assegno incongruente con tipo interrogazione         17
095400950706Selezione incongruente con il tipo di interrogazione richiesto         18
095500960911Con l'interrogazione per dati pagam. è obbligatorio selezionare stato  19
095600971223Se non specificato numero documento devo selezionare o no infragruppo  20
095700000107Divisa inesistente                                                     21
095800151026Immettere ID bonifico                                                  22
095900160505Inserire o codice cliente o parzializzazione per ragione sociale       23
096000160802RANGE di date superiore a 90 gg.                                       24
