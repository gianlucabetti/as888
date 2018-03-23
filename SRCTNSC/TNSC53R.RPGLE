000100960910     H DECEDIT('0,') DATEDIT(*DMY.)
000200960910?     *--------------------------------------------------------------*
000300960910      * TNSC53R                                                      *
000400960910      *       *-----------------------------------------*            *
000500960910      *        STAMPA CONTRASSEGNI X STATO ELABORAZIONE              *
000600960910      *       *-----------------------------------------*            *
000700960910      *                                                              *
000800960910?     *--------------------------------------------------------------*
000900960910     FTNCSB10L  IF   E           K DISK
001000960910     F*---------
001100960910     FTNCSA01L  IF   E           K DISK
001200960910     F*---------
001300960910     FTNCSBSWK  O    E             DISK
001400960910?     *--------------------------------------------------------------*
001500960910?     *  SCHIERE                                                     *
001600960910?     *--------------------------------------------------------------*
001700960910?     *--------------------------------------------------------------*
001800960910?     *  DS                                                          *
001900960910?     *--------------------------------------------------------------*
002000981007     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002100981007     D CNACO         E DS                  EXTNAME(CNACO00F)
002200981007     D CNIND         E DS                  EXTNAME(CNIND00F)
002300981007     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002400981007     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002500960910     D KPJBA         E DS
002600960910      *
002700960910     D TNSC52        E DS                  EXTNAME(TNSC52DS)
002800010302     D*FTNSC96    E DSTNSC96DS
002900980105     D*  Ds per ricezione dati di incasso
003000010302     D*FCSBINC      DS
003100010302     D*F                                      1  10 CSBNRA
003200010302     D*F                                     11  180CSBDTE
003300010302     D*F                                     19  230CSBABI
003400010302     D*F                                     24  280CSBCAB
003500010302     D*F                                     29  29 CSBTPA
003600010302     D*F                                     30  30 CSBTPI
003700010302     D*F                                     31  380CSBUDA
003800010302     D*F                                     39  460CSBDTA
003900010302     D*F                                     47  540CSBDDC
004000010302     D*F                                     55  610CSBNDT
004100010302     D*F                                     62  670CSBNRI
004200010302     D*F                                     68  750CSBDRI
004300980105     D*  Ds per ricezione dati di incasso
004400010302     D*FCSBPAG      DS
004500010302     D*F                                      1   1 CSBFPC
004600010302     D*F                                      2   80CSBBNA
004700010302     D*F                                      9  150CSBNDP
004800010302     D*F                                     16  230CSBDDP
004900010302     D*F                                     24  310CSBDRG
005000010302     D*F                                     32  370CSBNRG
005100010302     D*F                                     38  464CSBCMP
005200960910      *
005300960910     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005400960910     D  TCU                  398    697
005500960910     D                                     DIM(50)
005600960910     D  KCU                  698    847P 0
005700960910     D                                     DIM(50)
005800960910     D                                     PACKEVEN
005900960910     D CNCR80        E DS
006000960910     D                 DS
006100960910     D  CSBAAS                 1      4  0
006200960910     D  CSBMGS                 5      8  0
006300960910     D  CSBDSP                 1      8  0
006400960910     D TCUDS           DS
006500960910     D  F56                    5      6
006600960910     D  F34                    3      4
006700960910     D                SDS
006800960910     D  NOMPGM                 1     10
006900960910      *---------------------------------------------------------------*
007000960910      *   C I C L O      P R I N C I P A L E                          *
007100960910      *---------------------------------------------------------------*
007200960910      *  Posizionamento iniziale
007300960910     C                   EXSR      POSINI
007400960910      *  Loop letture
007500960911     C     *IN30         DOWEQ     '0'
007600960910     C                   EXSR      REDCSB
007700960910     C                   END
007800980105     C* RICHIAMO PGM X CHIUSURA IN LR
007900010302     C*F                   CLEARTNSC96
008000010302     C*F                   MOVEL'C'       D96TLA
008100010302     C*F                   CALL 'TNSC96R'
008200010302     C*F                   PARM           KPJBA
008300010302     C*F                   PARM           TNSC96
008400010302     C*F                   PARM           CSBINC
008500010302     C*F                   PARM           CSBPAG
008600980105      *  Fine
008700990617     C                   MOVEL     TNSC52        KPJBU
008800960910     C                   SETON                                        LR
008900960910      *---------------------------------------------------------------*
009000960910      *  POSINI: Posizionamento iniziale
009100960910      *---------------------------------------------------------------*
009200960910     C     POSINI        BEGSR
009300960910      *
009400960910      *  Eseguo lettura per stato Contrassegno
009500960910     C                   SELECT
009600960910     C     D52SCE        WHENEQ    '1'
009700960910     C                   MOVEL     '2'           KSTA
009800010109     C     D52SCE        WHENEQ    '7'
009900010109     C                   MOVEL     '3'           KSTA
010000960910     C     D52SCE        WHENEQ    '2'
010100960910     C                   MOVEL     '5'           KSTA
010200960910     C     D52SCE        WHENEQ    '3'
010300960910     C                   MOVEL     '6'           KSTA
010400960910     C     D52SCE        WHENEQ    '4'
010500960910     C                   MOVEL     '7'           KSTA
010600960910     C     D52SCE        WHENEQ    '5'
010700960910     C                   MOVEL     '8'           KSTA
010800960910     C     D52SCE        WHENEQ    '6'
010900960910     C                   MOVEL     '9'           KSTA
011000960910     C                   SETON                                        09
011100960910     C                   ENDSL
011200960910     C*  Primo posizionamento
011300960910     C     KSTA          CHAIN     TNCSB10L                           30
011400960910     C*
011500960910     C                   ENDSR
011600960910      *---------------------------------------------------------------*
011700960910      *  REDCSB: Lettura file
011800960910      *---------------------------------------------------------------*
011900960910     C     REDCSB        BEGSR
012000010305      *
012100960910      *  Verifico se record letto rientra nei limiti data spedizione
012200960910     C     CSBDSP        IFGE      D52DDS
012300960910     C     CSBDSP        ANDLE     D52ADS
012400980105     C* Controllo se C/Assegno infragruppo
012500980105     C* devo richiamare il pgm per riconoscere i dati infragruppo
012600010302     C*F                   CLEARTNSC96
012700010302     C*F                   MOVEL*BLANKS   D96TLA
012800010302     C*F                   MOVEL'E'       D96TPD
012900010302     C*F                   MOVELKNSIF     D96NSI
013000010302     C*F                   Z-ADDCSBAAS    D96AAS
013100010302     C*F                   Z-ADDCSBLNP    D96LNP
013200010302     C*F                   Z-ADDCSBLNA    D96LNA
013300010302     C*F                   Z-ADDCSBNRS    D96NRS
013400010302     C*F                   Z-ADDCSBNSP    D96NSP
013500990611     C* Controllo il codice cliente da passare
013600010302     C*F         CSBCDI    IFNE 0
013700010302     C*F                   MOVELCSBCDI    D96CDI
013800010302     C*F                   ELSE
013900010302     C*F                   MOVELCSBKSC    D96CDI
014000010302     C*F                   END
014100980105     C* Richiamo pgm
014200010302     C*F                   CALL 'TNSC96R'
014300010302     C*F                   PARM           KPJBA
014400010302     C*F                   PARM           TNSC96
014500010302     C*F                   PARM           CSBINC
014600010302     C*F                   PARM           CSBPAG
014700000223     C*----------------------------------------------------*
014800000223     C* Per gli stati 6, 7 e 8 ...
014900000223     C*----------------------------------------------------*
015000000223     C* QUESTO TEST E' VALIDO SOLO SE CI SONO SOLO 2 RAMI AZIENDALI
015100000223     C*----------------------------------------------------*
015200000223     C                   MOVEL     'N'           WOK               1
015300000223     C     CSBSTA        IFEQ      6
015400000223     C     CSBSTA        OREQ      7
015500000223     C     CSBSTA        OREQ      8
015600000224     C* Se richiesta stampa per lna esamino C/Assegno solo se
015700000224     C* quest'ultima è del mio ramo aziendale ....
015800010305     C*F         D52ORD    IFEQ 'A'
015900010305     C*F         D96RSI    ANDEQD96RLA
016000000224     C* Se richiesta stampa per lnp esamino C/Assegno solo se
016100000224     C* il ramo di pagamento è il mio ............
016200000224     C* spedizione valida se non c'è TNCSG e la lna è del mio ramo
016300000224     C* (no infragruppo)  ....
016400010305     C*F         D96TRV    ORNE 'S'
016500010305     C*F         D96RSI    ANDEQD96RLA
016600010305     C*F         D52ORD    ANDEQ'P'
016700000224     C* ....... oppure se c'è TNCSG (e la lna non è del mio ramo)
016800010305     C*F         D96TRV    OREQ 'S'
016900010305     C*F         D96RSI    ANDNED96RLA
017000010305     C*F         D52ORD    ANDEQ'P'
017100000223     C                   MOVEL     'S'           WOK
017200010305     C*F                   END
017300010109      * . per gli stati 2, 5, 9 ragiono in base ai rami di lna ANCHE 3
017400010305     C                   ELSE
017500000223      *  e di LNP
017600010305     C*F         D96RSI    IFEQ D96RLP
017700010305     C*F         D52ORD    ANDEQ'P'
017800010305     C*F         D96RSI    OREQ D96RLA
017900010305     C*F         D52ORD    ANDEQ'A'
018000000223     C                   MOVEL     'S'           WOK
018100010305     C*F                   END
018200010305     C                   END
018300000223     C* C/Assegno valido in base ragionamenti infragruppo
018400010305     C*          WOK       IFEQ 'S'
018500000223     C*
018600010305     C     D52ORD        IFEQ      'P'
018700010305     C     CSBLNP        ANDGE     D52LN1
018800010305     C     CSBLNP        ANDLE     D52LN2
018900010305     C                   Z-ADD     CSBLNP        CSBKE1
019000960910     C                   Z-ADD     0             CSBKE3
019100960910     C                   EXSR      WRTREC
019200960910     C                   END
019300960910      *  verifico se record letto rientra nei limiti lna se richiesto
019400960910      *  ordinamento x lna
019500960910     C     D52ORD        IFEQ      'A'
019600960911     C     CSBLNA        ANDGE     D52LN1
019700960911     C     CSBLNA        ANDLE     D52LN2
019800960910     C                   Z-ADD     CSBLNA        CSBKE1
019900960910     C                   Z-ADD     CSBLNP        CSBKE3
020000960910     C                   EXSR      WRTREC
020100960910     C                   END
020200960910     C*
020300010305     C                   END
020400010305     C*F                   END
020500960910     C*  Lettura successiva
020600960910     C     KSTA          READE     TNCSB10L                               30
020700010305     C*
020800960910     C                   ENDSR
020900960910      *---------------------------------------------------------------*
021000960910      *  WRTREC: Scrittura file di work
021100960910      *---------------------------------------------------------------*
021200960910     C     WRTREC        BEGSR
021300960910      *
021400960910      *  Campi chiave
021500960910     C                   Z-ADD     CSBDSP        CSBKE2
021600960910     C                   MOVEL     CSBNRS        CSBKE4
021700960910     C                   MOVE      CSBNSP        CSBKE4
021800960910      *  provincia destinatario
021900960910     C                   MOVEL     CSBPRO        CSBPRD
022000960910      *  decodifica mittente
022100960910     C     CSBCDI        IFNE      0
022200960910     C                   Z-ADD     CSBCDI        KKSC
022300960910     C                   ELSE
022400960910     C                   Z-ADD     CSBKSC        KKSC
022500960910     C                   END
022600960910     C*
022700980105     C* Se lnp no del mio ramo imposto dati cliente infragr. (mitt.)
022800010305     C*F         D96RSI    IFNE D96RLP
022900010305     C*F                   MOVELD96RAG    CSBRSM
023000010305     C*F                   MOVELD96PRV    CSBPRM
023100010305     C*F                   ELSE
023200960910     C*  Controllo se devo cercare decodifica su CNACO O TNCSA
023300960910     C                   MOVE      KKSC          W8899             4 0
023400960910     C     W8899         IFEQ      8888
023500960910     C     W8899         OREQ      9999
023600960910     C*  Se cliente vario decodfico sempre
023700960910     C                   Z-ADD     CSBAAS        KAAS
023800960910     C                   Z-ADD     CSBLNP        KLNP
023900960910     C                   Z-ADD     CSBNRS        KNRS
024000960910     C                   Z-ADD     CSBNSP        KNSP
024100960910     C     KCSA          CHAIN     TNCSA01L                           31
024200960910     C     *IN31         IFEQ      '0'
024300960910     C                   MOVEL     CSARSD        CSBRSM
024400960910     C                   MOVEL     CSAPRM        CSBPRM
024500960910     C                   ELSE
024600960910     C                   MOVEL     *ALL'?'       CSBRSM
024700960910     C                   MOVEL     *BLANKS       CSBPRM
024800960910     C                   END
024900960910     C*
025000960910     C                   ELSE
025100960910     C*  Se cliente codificato decodico solo se cambia
025200960910     C     KKSC          IFNE      SAVCDI
025300981007     C                   CLEAR                   TIBS69
025400981007     C                   CLEAR                   CNACO
025500981007     C                   CLEAR                   CNIND
025600981007     C                   CLEAR                   CNCLP
025700981007     C                   CLEAR                   FNCLS
025800981007     C                   MOVEL     ' '           I69TLA
025900981007     C                   MOVEL     KNSIF         I69SIF
026000981007     C                   Z-ADD     0             I69KCC
026100981007     C                   Z-ADD     KKSC          I69KAC
026200981007     C                   Z-ADD     KKSC          I69KIN
026300981007     C                   CALL      'TIBS69R'
026400981007     C                   PARM                    TIBS69
026500981007     C                   PARM                    CNACO
026600981007     C                   PARM                    CNIND
026700981007     C                   PARM                    CNCLP
026800981007     C                   PARM                    FNCLS
026900981007     C                   Z-ADD     KKSC          SAVCDI
027000020806     C     O69ERR        IFEQ      *BLANK
027100960910     C                   MOVEL     ACORAG        CSBRSM
027200960910     C                   MOVEL     INDPRV        CSBPRM
027300960910     C                   ELSE
027400960910     C                   MOVEL     *ALL'?'       CSBRSM
027500960910     C                   MOVEL     *BLANKS       CSBPRM
027600960910     C                   END
027700960910     C                   END
027800960910     C*
027900010305     C*F                   END
028000010305     C                   END
028100960910     C*  Scrittura  work file
028200960910     C                   WRITE     TNCSBS00
028300960910     C*
028400960910     C                   ENDSR
028500960910      *---------------------------------------------------------------*
028600960910      *  *INZSR: Operazioni iniziali
028700960910      *---------------------------------------------------------------*
028800960910     C     *INZSR        BEGSR
028900960910     C*
029000960910     C     *ENTRY        PLIST
029100960910     C                   PARM                    KPJBA
029200960910     C                   MOVEL     KPJBU         TNSC52
029300960910     C* Richiamo XPARUT
029400960910     C                   Z-ADD     1             CODUT
029500960910     C                   CALL      'X§PARUT'
029600960910     C                   PARM                    UT§DSE
029700960910     C* Ricerca capoconti
029800960910     C                   Z-ADD     1             X                 3 0
029900960910     C                   DO        50            X
030000960910     C                   MOVE      TCU(X)        TCUDS
030100960910     C*  Capoconto Clienti
030200960910     C     F56           IFEQ      'CG'
030300960910     C     F34           ANDEQ     '01'
030400960910     C                   Z-ADD     KCU(X)        KCI               4 0
030500960910     C                   END
030600960910     C                   END
030700960910     C*
030800960910     C* Definisco chiavi di accesso ai file
030900960910     C     KACO          KLIST
031000960910     C                   KFLD                    KKUT
031100960910     C                   KFLD                    KKCC
031200960910     C                   KFLD                    KKSC
031300960910     C     KIND          KLIST
031400960910     C                   KFLD                    KKUT
031500960910     C                   KFLD                    KKCC
031600960910     C                   KFLD                    KKSC
031700960910     C     KCSA          KLIST
031800960910     C                   KFLD                    KAAS
031900960910     C                   KFLD                    KLNP
032000960910     C                   KFLD                    KNRS
032100960910     C                   KFLD                    KNSP
032200960910     C* Definisco variabili
032300960910     C     *LIKE         DEFINE    ACOKUT        KKUT
032400960910     C     *LIKE         DEFINE    ACOKCC        KKCC
032500960910     C     *LIKE         DEFINE    ACOKSC        KKSC
032600960910     C     *LIKE         DEFINE    CSBAAS        KAAS
032700960910     C     *LIKE         DEFINE    CSBLNP        KLNP
032800960910     C     *LIKE         DEFINE    CSBNRS        KNRS
032900960910     C     *LIKE         DEFINE    CSBNSP        KNSP
033000960910     C     *LIKE         DEFINE    CSBSTA        KSTA
033100960910     C     *LIKE         DEFINE    CSBCDI        SAVCDI
033200980105     C*
033300960910     C                   ENDSR
