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
002000160505     D ERR             S             70    DIM(23) CTDATA PERRCD(1)
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
036600950717     C  NKJWDIFF         IFGT      7
036700950627     C                   SETON                                        444528
036800950627     C                   MOVEL     ERR(9)        $MSG
036900950627     C                   GOTO      FINSC2
037000950627     C                   END
037100950627     C                   ELSE
037200950627     C*  Controllo linea di partenza
037300950627     C                   MOVE      V1CLP1        ORGFIL
037400950627     C     ORGFIL        CHAIN     AZORG01L                           30
037500950627     C     *IN30         IFEQ      '1'
037600950627     C                   SETON                                        4628
037700950627     C                   MOVEL     ERR(3)        $MSG
037800950627     C                   GOTO      FINSC2
037900950627     C                   END
038000950627     C                   END
038100950627     C*
038200950627     C     FINSC2        ENDSR
038300950627      *---------------------------------------------------------------*
038400950627      *  CNTSC3: Cntrolli selezione x dati data spedizione/cliente    *
038500950627      *---------------------------------------------------------------*
038600950627     C     CNTSC3        BEGSR
038700950627     C*
038800950627     C*  Controllo data di spedizione da....
038900950627     C     V1CSPD        IFEQ      0
039000950627     C                   SETON                                        4728
039100950627     C                   MOVEL     ERR(8)        $MSG
039200950627     C                   GOTO      FINSC3
039300950627     C                   END
039400950627     C*
039500950627     C                   MOVE      V1CSPD        G02DAT
039600950627     C                   MOVEL     *BLANK        G02ERR
039700950627     C                   CALL      'XSRDA8'
039800950627     C                   PARM                    WLBDA8
039900950627     C     G02ERR        IFEQ      '1'
040000950627     C                   SETON                                        4728
040100950627     C                   MOVEL     ERR(6)        $MSG
040200950627     C                   GOTO      FINSC3
040300950627     C                   ELSE
040400950627     C                   Z-ADD     G02INV        WSDDAT
040500950627     C                   Z-ADD     G02TGI        WTGIDA
040600950627     C                   Z-ADD     G02DAT        V1CSPD
040700950627     C                   END
040800950627     C*
040900950627     C*  Controllo data di spedizione a ....
041000950627     C     V1CSPA        IFEQ      0
041100950627     C                   MOVE      V1CSPD        V1CSPA
041200950627     C                   SETON                                        90
041300950627     C                   END
041400950627     C*
041500950627     C                   MOVE      V1CSPA        G02DAT
041600950627     C                   MOVEL     *BLANK        G02ERR
041700950627     C                   CALL      'XSRDA8'
041800950627     C                   PARM                    WLBDA8
041900950627     C     G02ERR        IFEQ      '1'
042000950627     C                   SETON                                        4828
042100950627     C                   MOVEL     ERR(6)        $MSG
042200950627     C                   GOTO      FINSC3
042300950627     C                   ELSE
042400950627     C                   Z-ADD     G02INV        WSADAT
042500950627     C                   Z-ADD     G02TGI        WTGIA
042600950627     C                   Z-ADD     G02DAT        V1CSPA
042700950627     C                   END
042800950627     C*
042900950627     C*  Controllo limite delle due date
043000950627     C     WSDDAT        IFGT      WSADAT
043100950627     C                   SETON                                        284748
043200950627     C                   MOVEL     ERR(7)        $MSG
043300950627     C                   GOTO      FINSC3
043400950627     C                   END
043500950627     C*  Controllo differenza
043600950627     C     V1CKCS        IFEQ      0
043700950627     C     WTGIA         SUB       WTGIDA        WDIFF
043800950717     C  NKJWDIFF         IFGT      7
043900950627     C                   SETON                                        474828
044000950627     C                   MOVEL     ERR(9)        $MSG
044100950627     C                   GOTO      FINSC3
044200950627     C                   END
044300950627     C                   ELSE
044400950627     C*  Controllo cliente
044500971223      * CHIAMO TIBS69 PER RICERCA CODICE
044600971223     C                   CLEAR                   TIBS69
044700971223     C                   CLEAR                   CNACO
044800971223     C                   CLEAR                   CNIND
044900971223     C                   CLEAR                   CNCLP
045000971223     C                   CLEAR                   FNCLS
045100971223     C                   MOVEL     ' '           I69TLA
045200971223     C                   MOVEL     KNSIF         I69SIF
045300971223     C                   Z-ADD     V1CKCS        I69KAC
045400971223     C                   CALL      'TIBS69R'
045500971223     C                   PARM                    TIBS69
045600971223     C                   PARM                    CNACO
045700971223     C                   PARM                    CNIND
045800971223     C                   PARM                    CNCLP
045900971223     C                   PARM                    FNCLS
046000971223     C     O69ERR        IFNE      *BLANKS
046100950627     C                   SETON                                        4928
046200950627     C                   MOVEL     ERR(10)       $MSG
046300950627     C                   GOTO      FINSC3
046400950627     C                   END
046500950627     C                   END
046600950627     C*
046700950627     C     FINSC3        ENDSR
046800950627      *---------------------------------------------------------------*
046900950627      *  CNTSC4: Cntrolli selezione x dati doc.pagamnto/cliente       *
047000950627      *---------------------------------------------------------------*
047100950627     C     CNTSC4        BEGSR
047200950627     C*
047300931011     C*  Controllo data di pagamento da ....
047400950627     C     V1CPDD        IFEQ      0
047500950627     C                   SETON                                        5028
047600950627     C                   MOVEL     ERR(11)       $MSG
047700950627     C                   GOTO      FINSC4
047800950627     C                   END
047900931011     C*
048000950627     C                   MOVE      V1CPDD        G02DAT
048100931011     C                   MOVEL     *BLANK        G02ERR
048200941102     C                   CALL      'XSRDA8'
048300941102     C                   PARM                    WLBDA8
048400931011     C     G02ERR        IFEQ      '1'
048500950627     C                   SETON                                        5028
048600950627     C                   MOVEL     ERR(6)        $MSG
048700950627     C                   GOTO      FINSC4
048800931011     C                   ELSE
048900950627     C                   Z-ADD     G02INV        WPDDAT
049000950627     C                   Z-ADD     G02TGI        WTGIDA
049100950627     C                   Z-ADD     G02DAT        V1CPDD
049200931011     C                   END
049300931011     C*
049400931011     C*  Controllo data di pagamento a ....
049500950627     C     V1CPDA        IFEQ      0
049600950627     C                   MOVE      V1CPDD        V1CPDA
049700950627     C                   SETON                                        90
049800931011     C                   END
049900931011     C*
050000950627     C                   MOVE      V1CPDA        G02DAT
050100931011     C                   MOVEL     *BLANK        G02ERR
050200941102     C                   CALL      'XSRDA8'
050300941102     C                   PARM                    WLBDA8
050400931011     C     G02ERR        IFEQ      '1'
050500950627     C                   SETON                                        5128
050600950627     C                   MOVEL     ERR(6)        $MSG
050700950627     C                   GOTO      FINSC4
050800931011     C                   ELSE
050900950627     C                   Z-ADD     G02INV        WPADAT
051000950627     C                   Z-ADD     G02TGI        WTGIA
051100950627     C                   Z-ADD     G02DAT        V1CPDA
051200950627     C                   END
051300931011     C*
051400931011     C*  Controllo limite delle due date
051500950627     C     WPDDAT        IFGT      WPADAT
051600950627     C                   SETON                                        505128
051700950627     C                   MOVEL     ERR(7)        $MSG
051800950627     C                   GOTO      FINSC4
051900931011     C                   END
052000931011     C*
052100950704     C     WTGIA         SUB       WTGIDA        WDIFF
052200950717     C  NKJWDIFF         IFGT      7
052300950627     C                   SETON                                        505128
052400950627     C                   MOVEL     ERR(9)        $MSG
052500950627     C                   GOTO      FINSC4
052600931011     C                   END
052700000107     C* Controllo la divisa se immessa
052800000107     C     V1CPDV        IFNE      *BLANKS
052900000107     C                   Z-ADD     1             X
053000000107     C     V1CPDV        LOOKUP    CCV(X)                                 32
053100000107     C     *IN32         IFEQ      '0'
053200000107     C                   SETON                                        6128
053300000107     C                   MOVEL     ERR(21)       $MSG
053400000107     C                   GOTO      FINSC4
053500000107     C                   END
053600000107     C                   END
053700950627     C*  Controllo cliente
053800971223      * CHIAMO TIBS69 PER RICERCA CODICE
053900971223     C                   CLEAR                   TIBS69
054000971223     C                   CLEAR                   CNACO
054100971223     C                   CLEAR                   CNIND
054200971223     C                   CLEAR                   CNCLP
054300971223     C                   CLEAR                   FNCLS
054400971223     C                   MOVEL     ' '           I69TLA
054500971223     C                   MOVEL     KNSIF         I69SIF
054600971223     C                   Z-ADD     V1CKCP        I69KAC
054700971223     C                   CALL      'TIBS69R'
054800971223     C                   PARM                    TIBS69
054900971223     C                   PARM                    CNACO
055000971223     C                   PARM                    CNIND
055100971223     C                   PARM                    CNCLP
055200971223     C                   PARM                    FNCLS
055300971223     C     O69ERR        IFNE      *BLANKS
055400950627     C                   SETON                                        5228
055500950627     C                   MOVEL     ERR(10)       $MSG
055600950627     C                   GOTO      FINSC4
055700950627     C                   END
055800160505     c                   if        v1ckcp > 0 and v1crag <> *blank
055900160505     C                   SETON                                        5228
056000160505     C                   MOVEL     ERR(23)       $MSG
056100160505     C                   GOTO      FINSC4
056200160505     C                   END
056300931011     C*
056400950627     C     FINSC4        ENDSR
056500950627      *---------------------------------------------------------------*
056600950627      *  CNTSC5: Cntrolli selezione x dati nr.e data distinta         *
056700950627      *---------------------------------------------------------------*
056800950627     C     CNTSC5        BEGSR
056900950627     C*
057000950627     C*  Controllo data distinta
057100950627     C     V1CDDT        IFEQ      0
057200950627     C                   SETON                                        5328
057300950627     C                   MOVEL     ERR(12)       $MSG
057400950627     C                   GOTO      FINSC5
057500950627     C                   END
057600950627     C*
057700950627     C                   MOVE      V1CDDT        G02DAT
057800950627     C                   MOVEL     *BLANK        G02ERR
057900950627     C                   CALL      'XSRDA8'
058000950627     C                   PARM                    WLBDA8
058100950627     C     G02ERR        IFEQ      '1'
058200950627     C                   SETON                                        5328
058300950627     C                   MOVEL     ERR(6)        $MSG
058400950627     C                   GOTO      FINSC5
058500950627     C                   ELSE
058600950704     C                   Z-ADD     G02INV        WDDT              8 0
058700950627     C                   Z-ADD     G02DAT        V1CDDT
058800950627     C                   END
058900950627     C*  Controllo numero distinta
059000950627     C     V1CNDT        IFEQ      0
059100950627     C                   SETON                                        5428
059200950627     C                   MOVEL     ERR(13)       $MSG
059300950627     C                   GOTO      FINSC5
059400950627     C                   END
059500950627     C*  RAggruppamento
059600950627     C     V1CRGP        IFEQ      0
059700950627     C                   SETON                                        5828
059800950627     C                   MOVEL     ERR(16)       $MSG
059900950627     C                   GOTO      FINSC5
060000950627     C                   END
060100950627     C*
060200950627     C     FINSC5        ENDSR
060300151026      *---------------------------------------------------------------*
060400151026      *  CNTSC6: Controlli selezione x dati ID bonifico               *
060500151026      *---------------------------------------------------------------*
060600151026     c     CNTSC6        BEGSR
060700151026      *
060800151026      *?Controllo ID bonifico?
060900151026     c                   Select
061000151026      * -?Se *zero: si emette errore a video?
061100151026     c                   When      V1Cnbn = *zero
061200151026     c                   eval      *in62  = *on
061300151026     c                   eval      *in28  = *on
061400151026     c                   eval      $Msg   = Err(22)
061500151026     c                   leavesr
061600151026      * -?Se negativo: si calcola il valore assoluto?
061700151026     c                   When      V1Cnbn < *zero
061800151026     c                   eval      V1Cnbn = %abs(V1Cnbn)
061900151026     c                   EndSl
062000151026      *
062100151026     c                   ENDSR
062200950627      *---------------------------------------------------------------*
062300950627      *  CNTPAR: Controlli  su parzializzazioni                       *
062400950627      *---------------------------------------------------------------*
062500950627     C     CNTPAR        BEGSR
062600950704     C*
062700960911     C*  Se ho selezionato interrogazione x dati pagamento stato obblig.
062800960911     C     V1CSCE        IFEQ      4
062900960911     C     V1CSTA        ANDEQ     ' '
063000960911     C                   SETON                                        405928
063100960911     C                   MOVEL     ERR(19)       $MSG
063200960911     C                   GOTO      FINPAR
063300960911     C                   END
063400950704     C*  Se ho selezionato interrogazione x dati pagamento non posso selez.
063500950704     C*  C/assegni non pagati
063600950704     C     V1CSCE        IFEQ      4
063700950704     C     V1CSTA        ANDNE     'P'
063800960911     C     V1CSTA        ANDNE     'T'
063900960911     C     V1CSTA        ANDNE     'R'
064000950704     C                   SETON                                        405928
064100950704     C                   MOVEL     ERR(17)       $MSG
064200950706     C                   GOTO      FINPAR
064300950704     C                   END
064400960911     C*  Se ho selezionato interrogazione x dati dstinta non posso selez.
064500960911     C*  C/assegni non incassati
064600960911     C     V1CSCE        IFEQ      5
064700960911     C     V1CSTA        ANDNE     ' '
064800960911     C     V1CSTA        ANDNE     'I'
064900960911     C     V1CSTA        ANDNE     'P'
065000960911     C     V1CSTA        ANDNE     'T'
065100960911     C     V1CSTA        ANDNE     'R'
065200960911     C                   SETON                                        405928
065300960911     C                   MOVEL     ERR(17)       $MSG
065400960911     C                   GOTO      FINPAR
065500960911     C                   END
065600950706     C*  Se ho impostato o CAB o Nr.Assegno o ABI non posso interrogare per
065700950706     C*  spedizione/rif.mittente
065800950706     C     V1CSCE        IFEQ      2
065900950719     C     V1CSCE        OREQ      1
066000950706     C     V1CABI        IFNE      *ZEROS
066100950706     C                   SETON                                        405528
066200950706     C                   MOVEL     ERR(18)       $MSG
066300950706     C                   GOTO      FINPAR
066400950706     C                   END
066500950706     C     V1CCAB        IFNE      *ZEROS
066600950706     C                   SETON                                        405628
066700950706     C                   MOVEL     ERR(18)       $MSG
066800950706     C                   GOTO      FINPAR
066900950706     C                   END
067000950706     C     V1CNAS        IFNE      *ZEROS
067100950706     C     V1CNAS        ANDNE     *BLANKS
067200950706     C                   SETON                                        405728
067300950706     C                   MOVEL     ERR(18)       $MSG
067400950706     C                   GOTO      FINPAR
067500950706     C                   END
067600950706     C                   END
067700950627     C*  Se ho impostato o CAB o Nr.Assegno devo impostare ABI e viceversa
067800950627     C     V1CABI        IFEQ      0
067900950627     C     V1CCAB        IFNE      0
068000950627     C     V1CNAS        ORNE      *BLANKS
068100950627     C                   SETON                                        5528
068200950627     C                   MOVEL     ERR(14)       $MSG
068300950627     C                   END
068400950627     C*
068500950627     C                   END
068600950627     C*
068700950706     C     FINPAR        ENDSR
068800931012      *---------------------------------------------------------------*
068900931012      *  STAMPA: RICHIAMO I VARI PROGRAMMI DI STAMPA                  *
069000931012      *---------------------------------------------------------------*
069100931012     C     STAMPA        BEGSR
069200950627     C*----------------------------------------------------*
069300950720     C                   CLEAR                   TNSC40
069400950704     C     V1CSCE        IFEQ      5
069500950627     C                   CLEAR                   TNSC84
069600950627     C                   Z-ADD     V1CRGP        D84RGF
069700950627     C                   Z-ADD     V1CNDT        D84NDT
069800950627     C                   Z-ADD     WDDT          D84DDC
069900950627     C                   MOVEL     TNSC84        KPJBU
070000950627     C                   MOVEL     'SC84'        KCOAZ
070100950627     C                   CALL      'BCH10'
070200950627     C                   PARM                    KPJBA
070300950717     C                   ELSE
070400950717     C                   MOVEL     V1CSCE        D40INT
070500950717     C                   MOVEL     V1CSTA        D40TCR
070600950717     C                   MOVE      V1CABI        D40ABI
070700950717     C                   MOVE      V1CCAB        D40CAB
070800140325     C                   MOVE      V1Cimp        D40imp
070900950717     C                   MOVEL     V1CNAS        D40NAS
071000950717     C                   MOVEL     *BLANK        KPJBU
071100950717     C                   MOVE      V1CRMN        D40NRM
071200950717     C                   MOVE      WSDT1         D40SD1
071300950717     C                   MOVE      WSAT1         D40SA1
071400950717     C                   MOVE      V1CLP1        D40LP1
071500950717     C                   MOVE      WSDDAT        D40SDD
071600950717     C                   MOVE      WSADAT        D40SDA
071700950717     C                   MOVE      V1CKCS        D40SCD
071800950717     C                   MOVE      WPDDAT        D40PDD
071900950717     C                   MOVE      WPADAT        D40PDA
072000950717     C                   MOVE      V1CINT        D40PTN
072100950717     C                   MOVE      V1CNDP        D40PND
072200950717     C                   MOVE      V1CKCP        D40PCD
072300971223     C                   MOVE      V1CPIG        D40PIG
072400000110     C                   MOVE      V1CPDV        D40PDV
072500151026     c                   eval      D40nbn = V1Cnbn
072600950717     C                   MOVEL     TNSC40        KPJBU
072700950717     C                   MOVEL     'SC42'        KCOAZ
072800950717     C                   CALL      'BCH10'
072900950717     C                   PARM                    KPJBA
073000950627     C                   END
073100931012     C*----------------------------------------------------*
073200950627     C     D40CMD        IFEQ      3
073300950704     C                   MOVE      'S'           WFINE
073400931012     C                   END
073500931012     C*----------------------------------------------------*
073600931012     C                   ENDSR
073700931011      *---------------------------------------------------------------*
073800931011      *  CALLIN: RICHIAMO I VARI PROGRAMMI DI INTERROGAZIONE          *
073900931011      *---------------------------------------------------------------*
074000931011     C     CALLIN        BEGSR
074100931011     C*
074200950627     C                   CLEAR                   TNSC40
074300950627     C                   MOVEL     V1CINT        D40INT
074400950627     C                   MOVEL     V1CSTA        D40TCR
074500950627     C                   MOVEL     *BLANK        KPJBU
074600120612     C                   MOVE      V1CABI        D40ABI
074700120612     C                   MOVE      V1CCAB        D40CAB
074800120612     C                   MOVEL     V1CNAS        D40NAS
074900140325     C                   MOVE      V1Cimp        D40imp
075000931011     C*----------------------------------------------------*
075100950704     C     V1CSCE        IFEQ      1
075200120612     c                   eval      tnsc40ds  = tnsc40
075300120612     C                   MOVE      V1CAAS        £D40AAS
075400120612     C                   MOVE      V1CLNP        £D40LNP
075500120612     C                   MOVE      V1CNRS        £D40NRS
075600120612     C                   MOVE      V1CNSP        £D40NSP
075700120612     C                   clear                   £D40ABI
075800120612     C                   clear                   £D40CAB
075900120612     C                   clear                   £D40NAS
076000140325     C                   clear                   £D40imp
076100120612     C                   MOVEL     TNSC40ds      KPJBU
076200950627     C                   CALL      'TNSC43R'
076300931011     C                   PARM                    KPJBA
076400931011     C                   END
076500931012     C*----------------------------------------------------*
076600950704     C     V1CSCE        IFEQ      2
076700950627     C                   MOVE      V1CRMN        D40NRM
076800950627     C                   MOVE      WSDT1         D40SD1
076900950627     C                   MOVE      WSAT1         D40SA1
077000950627     C                   MOVE      V1CLP1        D40LP1
077100950627     C                   MOVEL     TNSC40        KPJBU
077200950627     C                   CALL      'TNSC41R2'
077300931012     C                   PARM                    KPJBA
077400931012     C                   END
077500931011     C*----------------------------------------------------*
077600950704     C     V1CSCE        IFEQ      3
077700950627     C                   MOVE      WSDDAT        D40SDD
077800950627     C                   MOVE      WSADAT        D40SDA
077900950627     C                   MOVE      V1CKCS        D40SCD
078000160414     C                   MOVE      V1CINT3       D40PTN
078100950627     C                   MOVEL     TNSC40        KPJBU
078200950627     C     V1CKCS        IFEQ      0
078300950627     C                   CALL      'TNSC41R7'
078400950627     C                   PARM                    KPJBA
078500950627     C                   ELSE
078600950627     C                   CALL      'TNSC41R3'
078700931011     C                   PARM                    KPJBA
078800931011     C                   END
078900950627     C                   END
079000931012     C*----------------------------------------------------*
079100950704     C     V1CSCE        IFEQ      4
079200950627     C                   MOVE      WPDDAT        D40PDD
079300950627     C                   MOVE      WPADAT        D40PDA
079400950627     C                   MOVE      V1CINT        D40PTN
079500950627     C                   MOVE      V1CNDP        D40PND
079600950627     C                   MOVE      V1CKCP        D40PCD
079700971223     C                   MOVE      V1CPIG        D40PIG
079800000107     C                   MOVE      V1CPDV        D40PDV
079900160414     C                   MOVE      V1Crag        D40rag
080000950627     C                   MOVEL     TNSC40        KPJBU
080100950629     C     D40PND        IFEQ      0
080200950629     C                   CALL      'TNSC41R6'
080300931012     C                   PARM                    KPJBA
080400931012     C                   ELSE
080500950629     C                   CALL      'TNSC41R4'
080600931012     C                   PARM                    KPJBA
080700931012     C                   END
080800931012     C                   END
080900950629     C*----------------------------------------------------*
081000950704     C     V1CSCE        IFEQ      5
081100950629     C                   MOVE      WDDT          D40DDT
081200950629     C                   MOVE      V1CNDT        D40NDT
081300950629     C                   MOVE      V1CRGP        D40RGP
081400950629     C                   MOVEL     TNSC40        KPJBU
081500950629     C                   CALL      'TNSC41R5'
081600950629     C                   PARM                    KPJBA
081700950629     C                   END
081800151026      *----------------------------------------------------*
081900151026     c                   IF        V1Csce = 6
082000151026     c                   eval      D40nbn = V1Cnbn
082100151026     c                   eval      KPJBU  = TNSC40
082200151026     c                   call      'TNSC41R8'
082300151026     c                   parm                    KPJBA
082400151026     C                   ENDIF
082500931011     C*----------------------------------------------------*
082600950629     C                   MOVEL     KPJBU         TNSC40
082700950629     C     D40CMD        IFEQ      3
082800950704     C                   MOVE      'S'           WFINE
082900931011     C                   END
083000931011     C*----------------------------------------------------*
083100931011     C                   ENDSR
083200931011      *---------------------------------------------------------------*
083300941102      *  *INZSR: OPERAZIONI INIZIALI                                  *
083400931011      *---------------------------------------------------------------*
083500941102     C     *INZSR        BEGSR
083600931011      *---------------------------------------------------------------*
083700941102     C     *ENTRY        PLIST
083800931011     C                   PARM                    KPJBA
083900931011      *
084000931011     C                   Z-ADD     1             CODUT
084100931011     C                   CALL      'X§PARUT'
084200931011     C                   PARM                    UT§DSE
084300931011     C*          CODUT     CABEQ-1        FINE
084400931011     C                   MOVEL     RAGUT         RSUT             20
084500931011     C                   MOVEL     REC80         CNCR80
084600931011     C                   MOVEL     KNMUS         §SEI              6
084700950704      *
084800950704     C                   Z-ADD     1             X                 3 0
084900950704     C                   Z-ADD     0             KCI
085000950704      *  Ricerco capoconto clienti
085100950704     C     X             DOWLE     50
085200950704     C     KCI           ANDEQ     0
085300950704     C                   MOVE      TCU(X)        TCUDS
085400950704     C     F56           IFEQ      'CG'
085500950704     C     F34           ANDEQ     '01'
085600950704     C                   Z-ADD     KCU(X)        KCI               4 0
085700950704     C                   END
085800950704     C                   ADD       1             X
085900950704     C                   END
086000950626     C*----------------------------------------------------*
086100950626     C*  Definizione Chiavi di accesso
086200950712     C     KCSB          KLIST
086300950712     C                   KFLD                    KAAS
086400950712     C                   KFLD                    KLNP
086500950712     C                   KFLD                    KNRS
086600950712     C                   KFLD                    KNSP
086700000107     C*  Definizione chiave di accesso  TABEL
086800000107     C     KTAB          KLIST
086900000107     C                   KFLD                    KKUT
087000000107     C                   KFLD                    KCOD
087100931011     C*----------------------------------------------------*
087200931011     C*  Definizione variabili
087300950626     C     *LIKE         DEFINE    D40PDD        WPDDAT
087400950626     C     *LIKE         DEFINE    D40PDD        WPADAT
087500950626     C     *LIKE         DEFINE    D40PDD        WSDDAT
087600950626     C     *LIKE         DEFINE    D40PDD        WSADAT
087700950627     C     *LIKE         DEFINE    D40PDD        WSDT1
087800950627     C     *LIKE         DEFINE    D40PDD        WSAT1
087900950626     C     *LIKE         DEFINE    G02TGI        WDIFF
088000950626     C     *LIKE         DEFINE    G02TGI        WTGIA
088100950626     C     *LIKE         DEFINE    G02TGI        WTGIDA
088200990729     C     *LIKE         DEFINE    CSBAAS        KAAS
088300990729     C     *LIKE         DEFINE    CSBLNP        KLNP
088400990729     C     *LIKE         DEFINE    CSBNRS        KNRS
088500990729     C     *LIKE         DEFINE    CSBNSP        KNSP
088600000107     C     *LIKE         DEFINE    TBLKUT        KKUT
088700000107     C     *LIKE         DEFINE    TBLCOD        KCOD
088800931011     C*----------------------------------------------------*
088900950626     C                   MOVEL     'N'           WFINE             1
089000950626     C                   MOVEL     '1'           WTPVID            1
089100941102     C                   TIME                    WHHDAT           14 0
089200000107     C*----------------------------------------------------*
089300000107     C*  Carico tabella delle divise
089400000107     C                   Z-ADD     1             KKUT
089500000107     C                   MOVEL     'CV'          KCOD
089600000107     C                   MOVEL     *BLANKS       CCV
089700000107     C                   Z-ADD     0             X                 3 0
089800000107     C     KTAB          CHAIN     TABEL00F                           31
089900000107     C     *IN31         DOWEQ     '0'
090000000107     C     TBLFLG        IFEQ      ' '
090100000107     C     TBLKEY        ANDNE     *BLANKS
090200000107     C                   ADD       1             X
090300000107     C                   MOVEL     TBLKEY        CCV(X)
090400000107     C                   END
090500000107     C     KTAB          READE     TABEL00F                               31
090600000107     C                   END
090700931011     C*----------------------------------------------------*
090800931011     C                   ENDSR
090900000107**
091000950627Stampa non prevista per interrogazione per numero spedizione           01
091100950627Linea di partenza obbligatoria                                         02
091200950627Linea di partenza inesistente                                          03
091300950627Numero di spedizione obbligatorio                                      04
091400950627Immettere numero riferimento mittente                                  05
091500950627Data non valida                                                        06
091600950627Prima data maggiore della seconda                                      07
091700950627Immettere limite inferiore data di spedizione                          08
091800950627RANGE di date superiore a 7 gg.                                        09
091900950627Codice cliente inesistente                                             10
092000950627Immettere data limite iniziale di pagamento                            11
092100950627Immettere data distinta                                                12
092200950627Immettere numero distinta                                              13
092300950627Immettere Codice ABI                                                   14
092400950627Immettere Codice CAB o numero Assegno                                  15
092500950627Raggruppamento obbligatorio                                            16
092600950704Selezione stato C/assegno incongruente con tipo interrogazione         17
092700950706Selezione incongruente con il tipo di interrogazione richiesto         18
092800960911Con l'interrogazione per dati pagam. è obbligatorio selezionare stato  19
092900971223Se non specificato numero documento devo selezionare o no infragruppo  20
093000000107Divisa inesistente                                                     21
093100151026Immettere ID bonifico                                                  22
093200160505Inserire o codice cliente o parzializzazione per ragione sociale       23
