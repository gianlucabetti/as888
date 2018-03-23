000100111005     /*PRM TGTRLS(V5R3M0)
000200111005     /*END
000300111005      *-----------*
000400111010      * FNLV22Z1  *---------------------------------------------------*
000500111005      *-----------*                                                   *
000600111005      * STAMPA SEGNACOLLO CON BARCODE A 18 CON ZEBRA 12 DOT/MM        *
000700111005      *        CON BARCODE 3X70                                       *
000800111005      *---------------------------------------------------------------*
000900111005      *
001000111005     H DECEDIT('0,') DATEDIT(*DMY.)
001100111005      *
001200111005      *---------------------------------------------------------------*
001300111005      *
001400941216     FFNLV22P   O    F  132        PRINTER
001500111005      *
001600111005      *---------------------------------------------------------------*
001700111005      *
001800910404     D §RI             S              1    DIM(37)
001900951020     D §RX             S              1    DIM(11)
002000910403     D §O7             S              1    DIM(7)
002100980505     D §O9             S              1    DIM(9)
002200980507     D §EU             S              1    DIM(35)
002300111005      *
002400111005      *---------------------------------------------------------------*
002500111005      *
002600910403     D                 DS
002700910403     D  §CB                    1     18  0
002800910403     D                                     DIM(18)
002900910403     D  §CA                    1     18
003000910403     D                                     DIM(18)
003100910403     D                 DS
003200910403     D  §N7                    1      7  0
003300910403     D  §S7                    1      7
003400910403     D                                     DIM(7)
003500910403     D                 DS
003600910403     D  §2                     1      2  0
003700910403     D                                     DIM(2)
003800910403     D  §2A                    1      2
003900910403     D                                     DIM(2)
004000910403     D DSBLD         E DS
004100971209     D DSBLDE        E DS
004200910403     D DSBLD7        E DS
004300910403     D  §C                     1     17  0
004400910403     D                                     DIM(17)
004500011119     D §bceurex        DS
004600011119     D  alfa5                         5
004700011119     D  nume12                       12  0
004800011119     D  alfa2                         2
004900000118     D §dpd            DS
005000000118     D  §dpd3                         3
005100000118     D  §dpd8                         8
005200000118     D  §dpd12                        1
005300000118     D §deawbds        DS
005400000118     D  §dpdc1w                       2
005500000118     D  §dpdc3w                       5
005600000118     D  §dpdc5w                       2
005700000118     D  §dpdc7w                       1
005800000124     D §dexx1ds        DS
005900000124     D  §dex1                         5
006000000124     D  §dex2                         5
006100000124     D  §dex3                         3
006200000124     D  §dex4                         2
006300000118     D §dpdcap         DS
006400000118     D  §dpdc1                        2
006500000118     D  §dpdc2                        1    inz('-')
006600000118     D  §dpdc3                        5
006700000118     D  §dpdc4                        1    inz('-')
006800000118     D  §dpdc5                        2
006900000118     D  §dpdc6                        1    inz('-')
007000000118     D  §dpdc7                        1
007100000118     d §dpdn           ds
007200000118     d §dpdn1                        38
007300000118     d §dpdn2                        38
007400000118     d §dpdn3                        38
007500000118     d §dpdn4                        39
007600111005      *
007700000118     d dpd             c                   const('DPD')
007800000118     d dpd1            c                   const('Deutscher Paket Dienst')
007900000118     d dpd3            c                   const('Sender:')
008000000118     d dpd4            c                   const('Receiver:')
008100000118     d dpd2            ds
008200000118     d  dpdtel                        3    inz('Tel')
008300000118     d  dpdteln                      15
008400000118     d dpd2f           ds
008500000118     d  dpdfax                        3    inz('Fax')
008600000118     d  dpdfaxn                      15
008700000118     d dpd8            c                   const('30519 Hannover----------x')
008800000118     d dpd9            c                   const('01')
008900111005      *
009000111005      *---------------------------------------------------------------*
009100111005      *
009200910403     C     *ENTRY        PLIST
009300910403     C                   PARM                    DSBLD
009400971209     C                   PARM                    DSBLDE
009500941216     C*
009600971222     C                   MOVEL     §LDNRS        §LDNRT            3
009700971222     C                   MOVE      '-'           §LDNRT
009800971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
009900971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
010000971222     C     §DETST        IFEQ      'EST'
010100971222     C                   EXSR      COLZER
010200971222     C                   ENDIF
010300980505     C*
010400980505     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
010500980505     C*
010600980505     C     §DETST        IFEQ      'EST'
010700980505     C     §DETST        OREQ      'ALL'
010800000118     c                   select
010900000118     c     §detse        wheneq    'EUR'
011000000118     c                   exsr      eurexp
011100000118     c     §detse        wheneq    'DP1'
011200000118     c                   exsr      dpddove
011300000118     c     §detse        wheneq    'DP2'
011400000118     c                   exsr      dpddove
011500000118     c                   exsr      dpdchi
011600000118     c                   endsl
011700980505     C                   ENDIF
011800000202     C     §DETST        IFEQ      'ITA'
011900000202     C     §DETST        OREQ      'ALL'
012000000202     C                   EXSR      PRINT
012100000202     C                   ENDIF
012200910403     C*
012300910403     C                   SETON                                        LR
012400111005     C*
012500951021     C*****************************************************
012600971209     C* IMPOSTAZIONE E STAMPA ETICHETTA ITALIA          ***
012700951021     C*****************************************************
012800951023     C     PRINT         BEGSR
012900951023     C                   MOVE      §LDLNP        §LCLNP
013000951023     C                   MOVE      §LDLNA        §LCLNA
013100951023     C                   MOVE      §LDNRS        §LCNRS
013200951023     C                   MOVE      §LDNCD        §LCNCD
013300951023     C                   MOVE      §LDZSC        §LCZSC
013400040430     c     §detse        comp      'EEX'                                  55
013500951023     C*
013600951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
013700951023     C                   EXSR      M10
013800951023     C                   MOVEA     §CA           §C18A            18
013900951023     C                   MOVE      §C18A         §CDBAR           18
014000951023     C*
014100970221     C     §LDTSP        IFEQ      'D'
014200970303     C     §LDLNA        ANDLT     300
014300970221     C                   MOVE      *BLANK        §LDTSP
014400970221     C                   END
014500951023     C*
014600951023     C     §LDTSP        COMP      *BLANK                             1616
014700951023     C     §LDTSP        COMP      'E'                                    17
014800090806     C  N17§LDTSP        COMP      'H'                                    17
014900951023     C     §LDCDP        COMP      *BLANK                             18
015000951023     C     §LDNRS        COMP      *ZERO                              19
015100951023     C*
015200951023     C** INIZIALIZZA STAMPANTE
015300951023     C                   EXCEPT    LV22
015400951023     C*
015500951023     C                   MOVEA     *BLANK        §RX
015600951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
015700951023     C                   SETOFF                                       77
015800951023     C                   MOVE      *BLANK        §A7               7
015900951023     C                   Z-ADD     §LDNC1        §N7
016000951023     C                   DO        7             §1
016100951023     C     §S7(§1)       IFEQ      *ZERO
016200951023     C                   MOVE      *BLANK        §S7(§1)
016300951023     C                   ELSE
016400951023     C                   SETON                                        77
016500951023     C                   END
016600951023     C  N77              END
016700951023     C                   MOVEA     §S7           §A7
016800951023     C                   MOVE      §A7           §A3               3
016900951023     C                   MOVEA     §A3           §RX(1)
017000951023     C                   MOVEA     '-'           §RX(4)
017100951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
017200951023     C                   SETOFF                                       77
017300951023     C                   MOVE      *BLANK        §A7
017400951023     C                   Z-ADD     §LDNC2        §N7
017500951023     C                   DO        7             §1
017600951023     C     §S7(§1)       IFEQ      *ZERO
017700951023     C                   MOVE      *BLANK        §S7(§1)
017800951023     C                   ELSE
017900951023     C                   SETON                                        77
018000951023     C                   END
018100951023     C  N77              END
018200951023     C                   MOVEA     §S7           §A7
018300951023     C                   MOVE      §A7           §A3
018400951023     C                   MOVEA     §A3           §RX(5)
018500951023     C                   MOVE      §LDCPD        §A3
018600951023     C                   MOVEA     §A3           §RX(9)
018700951023     C* STAMPA LINEA PARTENZA
018800951023     C                   EXCEPT    LV22H
018900951023     C*
019000951023     C                   MOVEA     *BLANK        §RI
019100951023     C                   MOVEA     §LDRSM        §RI(8)
019200951023     C                   MOVEA     ' '           §RI(30)
019300951023     C                   MOVEA     §LDPRM        §RI(31)
019400951023     C                   MOVEA     *BLANK        §RI(33)
019500951023     C                   EXCEPT    LV22D1
019600951023     C* STAMPA DATI MITTENTE
019700951023     C*
019800951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
019900971222     C                   EXSR      COLZER
020000951023     C*
020100971222     C*
020200951023     C* STAMPA NUMERO SEGNACOLLO
020300951023     C                   EXCEPT    LV22D2
020400951023     C*
020500951023     C                   MOVE      §LDTNA        §A3               3
020600951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
020700951023     C                   MOVE      §LDLNA        §A3
020800951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
020900951023     C                   MOVE      §LDZSC        §A2
021000951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
021100951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
021200951023     C                   MOVEL     §LDDLA        §ODDLA            8
021300951023     C* STAMPA LINEA ARRIVO
021400951023     C                   EXCEPT    LV22D3
021500951023     C*
021600951023     C                   MOVEA     *BLANK        §RI
021700951023     C                   MOVEA     §LDRSD        §RI(8)
021800951023     C                   MOVEA     ' '           §RI(30)
021900951023     C                   MOVEA     §LDPRD        §RI(31)
022000951023     C                   MOVEA     *BLANK        §RI(33)
022100951023     C* STAMPA 1° DATI DESTINATARIO
022200951023     C                   EXCEPT    LV22D4
022300951023     C*
022400951023     C                   MOVEA     *BLANK        §RI
022500951023     C                   MOVEA     §LDIND        §RI(8)
022600951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
022700951023     C                   Z-ADD     37            §Y                2 0    88
022800951023     C                   DO        36
022900951023     C                   SUB       1             §Y
023000951023     C     §RI(§Y)       IFNE      ' '
023100951023     C                   SETON                                        88
023200951023     C                   END
023300951023     C  N88              END
023400951023     C     §Y            IFLE      25
023500951023     C                   ADD       3             §Y
023600951023     C                   ELSE
023700951023     C                   MOVEA     '  '          §RI(26)
023800951023     C                   Z-ADD     28            §Y
023900951023     C                   END
024000951023     C                   MOVEA     §LDLOD        §RI(§Y)
024100951023     C* STAMPA 2° DATI DESTINATARIO
024200951023     C                   EXCEPT    LV22D5
024300951023     C* STAMPA SERVIZIO
024400951023     C   16              EXCEPT    LV22X5
024500951023     C*
024600951023     C* STAMPA BARCODE
024700951023     C*
024800951023     C* (CAMPO UNICO BARCODE E COSTANTI)
024900951023     C                   MOVE      *BLANK        §§CDB1           19
025000951023     C                   MOVE      §CDBAR        §§CDB1
025100951023     C                   MOVEL     '"'           §§CDB1
025200951023     C                   MOVE      *BLANK        §§CDB2           21
025300951023     C                   MOVEL     §§CDB1        §§CDB2
025400951023     C                   MOVE      '";'          §§CDB2
025500951023     C                   EXCEPT    LV22D6
025600951023     C*
025700951023     C                   MOVE      *BLANK        §RI21            21
025800951023     C   18              DO
025900951023     C                   MOVEL     'Rif.:'       §RI21
026000951023     C                   MOVE      §LDCDP        §RI21
026100951023     C                   END
026200951023     C* STAMPA RIFERIMENTO/RISTAMPA
026300951023     C                   EXCEPT    LV22D7
026400951023     C*
026500951023     C* STAMPA CHIUSURA ETICHETTA
026600951023     C                   EXCEPT    LV22D8
026700951023     C                   ENDSR
026800111005      *
026900971222     C*****************************************************
027000971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027100971222     C*****************************************************
027200971222     C     COLZER        BEGSR
027300971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027400971222     C                   SETOFF                                       77
027500971222     C                   MOVE      §LDNCD        §N7
027600971222     C                   DO        7             §1
027700971222     C     §S7(§1)       IFEQ      *ZERO
027800971222     C                   MOVE      *BLANK        §S7(§1)
027900971222     C                   ELSE
028000971222     C                   SETON                                        77
028100971222     C                   END
028200971222     C  N77              END
028300971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
028400971222     C                   MOVE      *ZERO         §X                1 0
028500971222     C                   MOVEA     *BLANK        §O7
028600971222     C                   DO        7             §1
028700971222     C     §S7(§1)       IFNE      *BLANK
028800971222     C                   ADD       1             §X
028900971222     C     §S7(§1)       IFEQ      '0'
029000971222     C                   MOVE      'O'           §O7(§X)
029100971222     C                   ELSE
029200971222     C                   MOVE      §S7(§1)       §O7(§X)
029300971222     C                   END
029400971222     C                   END
029500971222     C                   END
029600971222     C                   MOVEA     §O7           §S7
029700971222     C                   ENDSR
029800111005      *
029900980507     C*****************************************************
030000980507     C* STAMPA EUROEXPRESS                              ***
030100980507     C*****************************************************
030200980507     C     EUREXP        BEGSR
030300980507     C*
030400980507     C* IMPOSTO COSTANTI
030500980507     C                   MOVEL     'JUT'         §DEJEU            3
030600980507     C                   MOVEL     '55L'         §DE55L            3
030700980507     C                   MOVEL     'IT'          §DEISP            2
030800011116      * controllo se contenuto 55L nel codice passato
030900011116     c     '55L'         scan      §deeur                                 23
031000011119      * se contenuto 55l scompongo a modo vecchio
031100011119     c                   if        *in23
031200980507     C* ESTRAGGO IL CAP:
031300980507     C                   MOVE      *ZEROS        §Y
031400980507     C                   MOVEA     §DEEUR        §EU
031500980507     C* TROVO QUANTO è LUNGO IL BARCODE
031600980507     C     ' '           CHECKR    §DEEUR        §Y
031700980507     C* ISOLO IL CAP
031800980507     C                   MOVE      *BLANK        §O9
031900980507     C                   MOVE      *ZEROS        §W                2 0
032000980507     C     §Y            SUB       2             §Y
032100980507     C     22            DO        §Y            §X1               2 0
032200980507     C     §EU(§X1)      IFNE      *BLANKS
032300980507     C                   ADD       1             §W
032400980507     C                   MOVE      §EU(§X1)      §O9(§W)
032500980507     C                   END
032600980507     C                   END
032700980507     C*
032800980507     C                   MOVEA     §O9           §LDCAD
032900111005      *
033000011119      *scompongo a modo nuovo senza 55l
033100011119     c                   else
033200011119     c                   movel     §deeur        §bceurex
033300011119     c                   end
033400980507     C*
033500980507     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
033600980507     C* DEL BARCODE
033700980507     C                   MOVEL     '^FS'         §END              7
033800980507     C     §LDCAD        CAT       §DEISA:0      §BCD             11
033900980507     C     §BCD          CAT       §END:0        §BCEND           20
034000980507     C*
034100980507     C* VERIFICO SE CONTRASSEGNO
034200980507     C*
034300980507     C     §DEFCA        COMP      'S'                                    14
034400980507     C  N14§DEFCA        COMP      'X'                                    14
034500980507     C*
034600980507     C                   EXCEPT    LV22Y
034700980507     C*
034800980507     C                   ENDSR
034900111005      *
035000910403     C*****************************************************
035100910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
035200910403     C*****************************************************
035300910403     C     M10           BEGSR
035400910403     C                   MOVE      *ZERO         §TT               3 0
035500910403     C                   MOVE      *ZERO         §TT2              3 0
035600910403     C                   MOVE      *ZERO         §1                1 0
035700910403     C                   Z-ADD     18            §S                2 0
035800910403     C                   DO        17            §N                2 0
035900910403     C                   SUB       1             §S
036000910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
036100910403     C                   MOVE      §C(§N)        §CB(§N)
036200910403     C     §N            DIV       2             §CTR              3 0
036300910403     C                   MVR                     §RST              3 010
036400111010     C     *IN10         IFEQ      '1'
036500910403     C     §C(§S)        MULT      2             §N2               2 0
036600910403     C                   MOVE      §N2           §A2               2
036700910403     C                   MOVEA     §A2           §2A
036800910403     C                   XFOOT     §2            §CTR
036900910403     C                   ELSE
037000910403     C                   Z-ADD     §C(§S)        §CTR
037100910403     C                   END
037200910403     C                   ADD       §CTR          §TT
037300910403     C                   END
037400910403     C*
037500910403     C     §TT           ADD       9             §TT2
037600910403     C                   MOVE      0             §TT2
037700910403     C     §TT2          SUB       §TT           §1
037800910403     C*
037900910403     C                   MOVE      §1            §CB(18)
038000910403     C                   ENDSR
038100000118      *--------------------------------------------------
038200000118     c     dpddove       begsr
038300000118      *--------------------------------------------------
038400000118     c                   eval      §dpd = §deeur
038500000124     c                   eval      §dexx1ds = §dexx1
038600000118     c                   eval      §deawbds = §deawb
038700000118     c                   eval      §dpdc1 = §dpdc1w
038800000118     c                   eval      §dpdc3 = §dpdc3w
038900000118     c                   eval      §dpdc5 = §dpdc5w
039000000118     c                   eval      §dpdc7 = §dpdc7w
039100000118     c                   eval      §dpdn = 'Notifications on damages which are -
039200000118     c                             not recognizable from the outside have to -
039300000118     c                             have to be submitted to the transport -
039400000118     c                             organisation withing 7 days in written.'
039500111010     c                   eval      §DPDn2 = %triml(§DPDn2)
039600111010     c                   eval      §DPDn3 = %triml(§DPDn3)
039700111010     c                   eval      §DPDn4 = %triml(§DPDn4)
039800000118     C                   except    LV22dpd1
039900000118     C                   endsr
040000000118      *--------------------------------------------------
040100000118     c     dpdchi        begsr
040200000118      *--------------------------------------------------
040300000118     c                   move      §ldmgs        giorno            2 0
040400000118     c                   movel     §ldmgs        mese              2 0
040500000118     c                   move      mese          giomes            4 0
040600000118     c                   movel     giorno        giomes
040700000118     c                   movel     giomes        datasp            8 0
040800000118     c                   move      §ldaas        datasp
040900000118     c                   movel     §dexx2        dpdteln
041000000118     c                   move      §dexx2        dpdfaxn
041100000118     C                   except    LV22dpd2
041200000118     C                   endsr
041300111005      *
041400111005      *---------------------------------------------------------------*
041500111005      *
041600971114     O********* INIZIALIZZAZIONE ETICHETTA
041700971209     O********* ETICHETTA ITALIA
041800971209     O*
041900971120     OFNLV22P   E            LV22             01
042000971114     O                                              '^XA'
042100971114     O          E            LV22        1
042200111007     O                                              '^LH080,40'
042300971114     O          E            LV22        1
042400971114     O                                              '^PRC^MSY^FS'
042500971114     O********* Mittente
042600971114     O          E            LV22D1      1
042700111007     O                                              '^FO005,000'
042800111007     O                                              '^ACN,54,15'
042900971114     O                                              '^FD'
043000971114     O                       §RI
043100971114     O                                              '^FS'
043200111007     O********* Linea separatrice Mittente/Destinatario
043300971114     O          E            LV22D4      1
043400111007     O                                              '^FO160,060'
043500111010     O                                              '^GB610,0,5'
043600971114     O                                              '^FS'
043700111007     O********* Destinatario (Rag.Soc.)
043800971114     O          E            LV22D4      1
043900111007     O                                              '^FO005,080'
044000111005     O                                              '^ACN,54,15'
044100971114     O                                              '^FD'
044200971114     O                       §RI
044300971114     O                                              '^FS'
044400111010     O********* Destinatario (Indirizzo/Località)
044500971114     O          E            LV22D5      1
044600111007     O                                              '^FO005,140'
044700111007     O                                              '^ACN,54,15'
044800971114     O                                              '^FD'
044900971114     O                       §RI
045000971114     O                                              '^FS'
045100971114     O********* SERVIZIO
045200971114     O          E   N17      LV22X5      1
045300111007     O                                              '^FO780,000'
045400111010     O                                              '^GB180,135,5'
045500971114     O                                              '^FS'
045600971114     O          E   N17      LV22X5      1
045700111007     O                                              '^FO825,015'
045800111010     O                                              '^A0N,140,180'
045900971114     O                                              '^FD'
046000971114     O                       §LDTSP
046100971114     O                                              '^FS'
046200971114     O          E    17      LV22X5      1
046300111007     O                                              '^FO780,000'
046400111010     O                                              '^GB180,140,90'
046500971114     O                                              '^FS'
046600971114     O          E    17      LV22X5      1
046700111007     O                                              '^FO825,015'
046800111010     O                                              '^A0N,150,180'
046900971114     O                                              '^FR'
047000971114     O                                              '^FD'
047100971114     O                       §LDTSP
047200971114     O                                              '^FS'
047300971114     O********* Linea Partenza
047400971114     O          E            LV22H       1
047500111007     O                                              '^FO170,200'
047600111005     O                                              '^ACN,81,30'
047700971114     O                                              '^FD'
047800971209     O                       §LDLNP
047900971114     O                                              ' '
048000971114     O                       §LDDLP
048100971114     O                                              '^FS'
048200111007     O********* MC. SPEDIZIONE
048300111007     O          E            LV22D5      1
048400111007     O                                              '^FO680,200'
048500111007     O                                              '^ACN,40,15'
048600111007     O                                              '^FD'
048700111007     O                                              'MC.'
048800111007     O                                              ' '
048900111007     O                       §LDVOL                 '0 ,   '
049000111007     O                                              ' '
049100111007     O                       §LDFVR
049200111007     O                                              '^FS'
049300971114     O********* Sequenza colli
049400971114     O          E            LV22H       1
049500111007     O                                              '^FO700,250'
049600111007     O                                              '^ACN,40,15'
049700971114     O                                              '^FD'
049800971114     O                       §RX
049900971114     O                                              '^FS'
050000971114     O********* Segnacollo
050100971114     O          E    19      LV22D2      1
050200111007     O                                              '^FO000,350'
050300111005     O                                              '^A0N,180,90'
050400971114     O                                              '^FD'
050500971114     O                       §LDNRT
050600971114     O                                              '^FS'
050700971114     O          E            LV22D2      1
050800111007     O                                              '^FO170,290'
050900111007     O                                              '^A0N,250,180'
051000971114     O                                              '^FD'
051100971114     O                       §S7
051200971114     O                                              '^FS'
051300971114     O********* TERMINAL/LINEA/ZONA ARRIVO
051400971114     O********* TERMINAL
051500971114     O          E            LV22D3      1
051600111007     O                                              '^FO000,510'
051700111005     O                                              '^GB180,165,90'
051800971114     O                                              '^FS'
051900971114     O          E            LV22D3      1
052000111007     O                                              '^FO022,525'
052100111005     O                                              '^A0N,180,90'
052200971114     O                                              '^FR'
052300971114     O                                              '^FD'
052400971114     O                       §ODTNA
052500971114     O                                              '^FS'
052600971114     O********* CODICE LINEA ARRIVO
052700971114     O          E            LV22D3      1
052800111007     O                                              '^FO200,525'
052900111005     O                                              '^A0N,180,90'
053000971114     O                                              '^FD'
053100971114     O                       §ODLNA
053200971114     O                                              '^FS'
053300111007     O********* DESCRIZIONE LINEA DI ARRIVO
053400111007     O          E   N55      LV22D3      1
053500111007     O                                              '^FO360,525'
053600111007     O                                              '^A0N,180,90'
053700111007     O                                              '^FD'
053800111007     O                       §ODDLA
053900111007     O                                              '^FS'
054000971114     O********* ZONA ARRIVO
054100040430     O          E   N55      LV22D3      1
054200111007     O                                              '^FO825,510'
054300111005     O                                              '^GB135,165,90'
054400971114     O                                              '^FS'
054500040430     O          E   N55      LV22D3      1
054600111007     O                                              '^FO848,525'
054700111005     O                                              '^A0N,180,90'
054800971114     O                                              '^FR'
054900971114     O                                              '^FD'
055000971114     O                       §ODZSC
055100971114     O                                              '^FS'
055200040430     O********* Box per nazione e cap EEX
055300040430     O          E    55      LV22D3      1
055400111007     O                                              '^FO425,510'
055500111005     O                                              '^GB450,165,5'
055600040430     O                                              '^FS'
055700040430     O********* Nazione per EEX
055800040430     O          E    55      LV22D3      1
055900111010     O                                              '^FO440,525'
056000111005     O                                              '^A0N,180,90'
056100111007     O*** NO Reverse Image:                         '^FR'
056200040430     O                                              '^FD'
056300040430     O                       §DEISA
056400040430     O                                              '^FS'
056500040430     O********* CAP estero per EEX
056600040430     O          E    55      LV22D3      1
056700111010     O                                              '^FO590,525'
056800111005     O                                              '^A0N,180,90'
056900040430     O                                              '^FD'
057000040430     O                       §LDCAD
057100040430     O                                              '^FS'
057200041115     O********* chiusura superiore
057300041115     O          E            LV22D6      1
057400111007     O                                              '^FO100,680'
057500111007     O                                              '^GBO670,015,015'
057600041115     O                                              '^FS'
057700041115     O********* BARCODE  code 128
057800041115     O          E            LV22D6      1
057900111005     O                                              '^BY5,5,0'
058000041115     O          E            LV22D6      1
058100111007     O                                              '^FO100,680'
058200111014     O*//                                           '^BCN,350,N,N,N'
058300111014     O                                              '^BCN,300,N,N,N'
058400041115     O                                              '^FD>;'
058500041115     O                       §CDBAR
058600041115     O                                              '^FS'
058700041115     O********* chiusura inferiore
058800041115     O          E            LV22D6      1
058900111014     O*//                                           '^FO100,1030'
059000111014     O                                              '^FO100,980'
059100111007     O                                              '^GBO670,008,008'
059200041115     O                                              '^FS'
059300971114     O********* Codice Prodotto/KG/RISTAMPA
059400971114     O          E            LV22D7      1
059500111014     O*//                                           '^FO020,1045'
059600111014     O                                              '^FO020,995'
059700111005     O                                              '^ACN,54,15'
059800971114     O                                              '^FD'
059900971114     O                       §RI21
060000971114     O                                              '^FS'
060100971114     O          E            LV22D7      1
060200111014     O*//                                           '^FO610,1045'
060300111014     O                                              '^FO610,995'
060400111005     O                                              '^ACN,54,15'
060500971114     O                                              '^FD'
060600971114     O                                              'KG.'
060700971114     O                                              ' '
060800971114     O                       §LDPKG                 '    0 , '
060900971114     O                                              '^FS'
061000971114     O          E            LV22D7      1
061100111014     O*//                                           '^FO920,1045'
061200111014     O                                              '^FO920,995'
061300111005     O                                              '^ACN,54,15'
061400971114     O                                              '^FD'
061500971114     O                       §LDRST
061600971114     O                                              '^FS'
061700971114     O********* Chiusura etchetta
061800971114     O          E            LV22D8      1
061900971114     O                                              '^XZ'
062000111007      *
062100111007      *===============================================================*
062200111007      *
062300980505     O*-------------------------------------------------------
062400980505     O* ETICHETTA ESTERO EUROEXPRESS
062500980505     O*-------------------------------------------------------
062600980506      *---*   INIZIALIZZAZIONI
062700980505     O          E            LV22Y            01
062800980505     O                                              '^XA'
062900980505     O          E            LV22Y       1
063000111005     O                                              '^LH60,75'
063100980505     O          E            LV22Y       1
063200980505     O                                              '^PRC^MSY^FS'
063300980505      *---*   NAZIONE PARTENZA
063400980505     O          E            LV22Y       1
063500111005     O                                              '^FO240,000'
063600111005     O                                              '^A0N,090,075'
063700980505     O                                              '^FD'
063800980505     O                       §DEISP
063900980505     O                                              '^FS'
064000980505      *---*   CONTRASSEGNO
064100980505     O          E    14      LV22Y       1
064200111005     O                                              '^FO638,000'
064300111005     O                                              '^GB300,158,5'
064400980505     O                                              '^FS'
064500980505     O          E    14      LV22Y       1
064600111005     O                                              '^FO660,023'
064700111005     O                                              '^A0N,150,150'
064800980505     O                                              '^FD'
064900980505     O                                              'COD'
065000980505     O                                              '^FS'
065100980505      *---*   EUROEXPRESS
065200980505     O          E            LV22Y       1
065300111005     O                                              '^FO240,210'
065400111005     O                                              '^A0N,180,150'
065500980505     O                                              '^FD'
065600980505     O                                              'EuroExpress'
065700980505     O                                              '^FS'
065800980505      *---*   NAZIONE ARRIVO
065900980506     O          E            LV22Y       1
066000111005     O                                              '^FO053,398'
066100111005     O                                              '^GB300,225,5'
066200980506     O                                              '^FS'
066300980506     O          E            LV22Y       1
066400111005     O                                              '^FO068,413'
066500111005     O                                              '^A0N,045,045'
066600980507     O                                              '^FD'
066700980506     O                                              'Country'
066800980506     O                                              '^FS'
066900980505     O          E            LV22Y       1
067000111005     O                                              '^FO068,465'
067100111005     O                                              '^A0N,180,150'
067200980505     O                                              '^FD'
067300980505     O                       §DEISA
067400980505     O                                              '^FS'
067500980505      *---*   CAP DESTINO
067600980506     O          E            LV22Y       1
067700111005     O                                              '^FO390,398'
067800111005     O                                              '^GB600,225,5'
067900980506     O                                              '^FS'
068000980506     O          E            LV22Y       1
068100111005     O                                              '^FO405,413'
068200111005     O                                              '^A0N,045,045'
068300980506     O                                              '^FD'
068400980506     O                                              'Post Code'
068500980506     O                                              '^FS'
068600980505     O          E            LV22Y       1
068700111005     O                                              '^FO405,465'
068800111005     O                                              '^A0N,180,150'
068900980506     O                                              '^FD'
069000980506     O                       §LDCAD
069100980506     O                                              '^FS'
069200980505      *---*   BARCODE EUROEXPRESS
069300111010      * - senza 55L
069400011119     O          E   n23      LV22Y       1
069500111010     O                                              '^BY5,5,0^FS'
069600111010     O                                              '^FO000,645'
069700111005     O                                              '^BCN,300,Y,N,N'
069800980505     O                                              '^FD'
069900011119     O                       alfa5
070000980505     O                                              '>5'
070100011119     O                       nume12
070200011119     O                                              '>6'
070300011119     o                       alfa2
070400011119     O                       §end
070500111010      * - con  55L
070600011119     O          E    23      LV22Y       1
070700111010     O                                              '^BY3,5,0^FS'
070800111010     O                                              '^FO060,645'
070900111005     O                                              '^BCN,300,Y,N,N'
071000011119     O                                              '^FD'
071100011119     O                       §DEJEU
071200011119     O                       §DEISP
071300011119     O                                              '>5'
071400011119     O                       §LDLNP
071500011119     O                       §LDNRS
071600011119     O                       §LDNCD
071700011119     O                                              '>6'
071800011119     O                                              ' '
071900011119     O                       §DE55L
072000011119     O                       §BCEND
072100980505      *---*   RIFERIMENTI BARTOLINI
072200980505     O          E            LV22Y       1
072300111005     O                                              '^FO075,1020'
072400111005     O                                              '^ADN,054,030'
072500980507     O                                              '^FD'
072600980506     O                       §LDLNP
072700980506     O                                              ' '
072800980506     O                       §LDNRS
072900980506     O                                              ' '
073000980506     O                       §LDNCD
073100980506     O                                              '^FS'
073200980505     O********* Chiusura etchetta
073300980505     O          E            LV22Y       1
073400980505     O                                              '^XZ'
073500111007      *
073600111007      *===============================================================*
073700111007      *
073800000118     O*-------------------------------------------------------
073900000118     O* ETICHETTA ESTERO DPD
074000000118     O*-------------------------------------------------------
074100000118     O* etichetta 1 " DOVE "
074200000118     O          E            LV22dpd1         01
074300000118     o                                              '^XA'
074400000118     O          E            LV22dpd1    1
074500111005     o                                              '^LH120,60'
074600000118     O          E            LV22dpd1    1
074700000118     o                                              '^PRC^MSY^FS'
074800000118      *---*   numero  spedizione
074900000118     O          E            LV22dpd1    1
075000111005     O                                              '^FO390,030'
075100111005     O                                              '^ABN,33,21'
075200000118     O                                              '^FD'
075300000118     O                       §dpd3
075400000118     O                                              '^FS'
075500000118      * 8 caratteri numero spedizione
075600000118     O          E            LV22dpd1    1
075700111005     O                                              '^FO480,030'
075800111005     O                                              '^ABN,33,21'
075900000118     O                                              '^FD'
076000000118     O                       §dpd8
076100000118     O                                              '^FS'
076200000118      *ultimo carattere numero spedizione
076300000118     O          E            LV22dpd1    1
076400111005     O                                              '^FO705,030'
076500111005     O                                              '^ABN,33,21'
076600000118     O                                              '^FD'
076700000118     O                       §dpd12
076800000118     O                                              '^FS'
076900000118      *---*   costante scritta
077000000118     O          E            LV22dpd1    1
077100111005     O                                              '^FO255,120'
077200111005     O                                              '^ABN,17,11'
077300000118     O                                              '^FD'
077400000118     O                       §dpdn1
077500000118     O                                              '^FS'
077600000118     O          E            LV22dpd1    1
077700111005     O                                              '^FO255,150'
077800111005     O                                              '^ABN,17,11'
077900000118     O                                              '^FD'
078000000118     O                       §dpdn2
078100000118     O                                              '^FS'
078200000118     O          E            LV22dpd1    1
078300111005     O                                              '^FO255,180'
078400111005     O                                              '^ABN,17,11'
078500000118     O                                              '^FD'
078600000118     O                       §dpdn3
078700000118     O                                              '^FS'
078800000118     O          E            LV22dpd1    1
078900111005     O                                              '^FO255,210'
079000111005     O                                              '^ABN,17,11'
079100000118     O                                              '^FD'
079200000118     O                       §dpdn4
079300000118     O                                              '^FS'
079400000118     o*chiusura superiore
079500000118     O          e            lv22dpd1    1
079600111005     o                                              '^FO090,375'
079700111005     O                                              '^GB465,0,006'
079800000118     O                                              '^FS'
079900000118     o*chiusura INFERIORE
080000000118     O          e            lv22dpd1    1
080100111005     o                                              '^FO090,735'
080200111005     O                                              '^GB465,0,006'
080300000118     O                                              '^FS'
080400000118     o*  codice a barre
080500000118     O          e            lv22dpd1    1
080600111005     o                                              '^FO090,375^BY6,3'
080700111005     O                                              '^B2N,360,N,N,N'
080800000118     O                                              '^FD'
080900000118     O                       §deawb
081000000118     O                                              '^FS'
081100000118      *versione
081200000118     O          E            LV22dpd1    1
081300111005     O                                              '^FO300,300'
081400111005     O                                              '^ABN,50,32'
081500000118     O                                              '^FD'
081600000124     O                       §dex1
081700000118     O                                              '^FS'
081800000118      *decodifica 2° bcc
081900000118     O          E            LV22dpd1    1
082000111005     O                                              '^FO195,750'
082100111005     O                                              '^ABN,33,21'
082200000118     O                                              '^FD'
082300000118     O                       §dpdcap
082400000118     O                                              '^FS'
082500000118      *costante x
082600000118     O          E            LV22dpd1    1
082700111005     O                                              '^FO045,825'
082800111005     O                                              '^ACN,108,45'
082900000118     O                                              '^FD'
083000000118     O                       §desse
083100000118     O                                              '^FS'
083200000118      *costante 09
083300000118     O          E            LV22dpd1    1
083400111005     O                                              '^FO045,945'
083500111005     O                                              '^ACN,108,45'
083600000118     O                                              '^FD'
083700000118     O                       dpd9
083800000118     O                                              '^FS'
083900000118      *linea arrivo
084000000118     O          E            LV22dpd1    1
084100111005     O                                              '^FO270,885'
084200111005     O                                              '^ACN,189,60'
084300000118     O                                              '^FD'
084400000124     O                       §dex3
084500000118     O                                              '^FS'
084600000118      *zona arrivo
084700000118     O          E            LV22dpd1    1
084800111005     O                                              '^FO540,945'
084900111005     O                                              '^ACN,108,45'
085000000118     O                                              '^FD'
085100000124     O                       §dex4
085200000118     O                                              '^FS'
085300000118     O* costante dpd
085400000118     O          E            LV22dpd1    1
085500111005     O                                              '^FO135,000'
085600111005     O                                              '^ACB,81,60'
085700000118     O                                              '^FD'
085800000118     O                       dpd
085900000118     O                                              '^FS'
086000000118     o          E            LV22dpd1    1
086100000118     O                                              '^XZ'
086200111007      *
086300111007      *===============================================================*
086400111007      *
086500000118     O* etichetta 2 " CHI "
086600000118     O*--------------------------------------------------------------------
086700000118     O          E            LV22dpd2         01
086800000118     o                                              '^XA'
086900000118     O          E            LV22dpd2    1
087000111005     o                                              '^LH120,60'
087100000118     O          E            LV22dpd2    1
087200000118     o                                              '^PRC^MSY^FS'
087300000118      *---*   numero  spedizione
087400000118     O          E            LV22dpd2    1
087500111005     O                                              '^FO255,015'
087600111005     O                                              '^ACN,108,45'
087700000118     O                                              '^FD'
087800000118     O                       §dpd3
087900000118     O                                              '^FS'
088000000118      * 8 caratteri numero spedizione
088100000118     O          E            LV22dpd2    1
088200111005     O                                              '^FO435,030'
088300111005     O                                              '^ACN,81,30'
088400000118     O                                              '^FD'
088500000118     O                       §dpd8
088600000118     O                                              '^FS'
088700000118      *ultimo carattere numero spedizione
088800000118     O          E            LV22dpd2    1
088900111005     O                                              '^FO720,030'
089000111005     O                                              '^ACN,54,30'
089100000118     O                                              '^FD'
089200000118     O                       §dpd12
089300000118     O                                              '^FS'
089400000118      *---*  telefono
089500000118     O          E            LV22dpd2    1
089600111005     O                                              '^FO255,120'
089700111005     O                                              '^ACN,27,15'
089800000118     O                                              '^FD'
089900000118     O                       dpd2
090000000118     O                                              '^FS'
090100000118      *---*  Fax
090200000118     O          E            LV22dpd2    1
090300111005     O                                              '^FO255,150'
090400111005     O                                              '^ACN,27,15'
090500000118     O                                              '^FD'
090600000118     O                       dpd2f
090700000118     O                                              '^FS'
090800000118      *---*   costante MITTENTE 1
090900000118     O          E            LV22dpd2    1
091000111005     O                                              '^FO240,188'
091100111005     O                                              '^ACN,27,15'
091200000118     O                                              '^FD'
091300000118     O                       dpd3
091400000118     O                                              '^FS'
091500000118      *---*   costante mittente 2
091600000118     O          E            LV22dpd2    1
091700111005     O                                              '^FO150,225'
091800111005     O                                              '^A0N,45,30'
091900000118     O                                              '^FD'
092000000118     O                       §ldrsm
092100000118     O                                              '^FS'
092200000118      *---*   costante mittente 3
092300000118     O          E            LV22dpd2    1
092400111005     O                                              '^FO150,278'
092500111005     O                                              '^A0N,45,30'
092600000118     O                                              '^FD'
092700000118     O                       §ldprm
092800000118     O                                              '^FS'
092900000118     o*LINEA DIVISIONE
093000000118     O          e            lv22dpd2    1
093100111005     o                                              '^FO135,330'
093200111005     O                                              '^GB600,0,006'
093300000118     O                                              '^FS'
093400000118     o*chiusura superiore
093500000118     O          e            lv22dpd2    1
093600111005     o                                              '^FO090,705'
093700111005     O                                              '^GB555,0,006'
093800000118     O                                              '^FS'
093900000118     o*chiusura INFERIORE
094000000118     O          e            lv22dpd2    1
094100111005     o                                              '^FO090,1035'
094200111005     O                                              '^GB555,0,006'
094300000118     O                                              '^FS'
094400000118      *costante primo B-C
094500000118     O          e            lv22dpd2    1
094600111005     o                                              '^FO090,705^BY6,3'
094700111005     O                                              '^B2N,330,N,N,N'
094800000118     O                                              '^FD'
094900000118     O                       §dpd
095000000118     O                                              '^FS'
095100000118      *costante data
095200000118     O          E            LV22dpd2    1
095300111005     O                                              '^FO000,1065'
095400111005     O                                              '^ABN,33,11'
095500000118     O                                              '^FD'
095600000118     O                       datasp                 '  /  /    '
095700000118     O                                              '^FS'
095800000118      *costante 1 collo
095900000118     O          E            LV22dpd2    1
096000111005     O                                              '^FO195,1065'
096100111005     O                                              '^ABN,33,11'
096200000118     O                                              '^FD'
096300000118     O                                              '1 Collo'
096400000118     O                                              '^FS'
096500000118      *costante peso
096600000118     O          E            LV22dpd2    1
096700111005     O                                              '^FO345,1065'
096800111005     O                                              '^ABN,33,11'
096900000118     O                                              '^FD'
097000000118     O                                              'Peso Kg'
097100000118     O                                              '^FS'
097200000118      *peso
097300000118     O          E            LV22dpd2    1
097400111005     O                                              '^FO480,1065'
097500111005     O                                              '^ABN,33,21'
097600000118     O                                              '^FD'
097700000118     O                       §ldpkg        2
097800000118     O                                              '^FS'
097900000118      *costante destinatario
098000000118     O          E            LV22dpd2    1
098100111005     O                                              '^FO000,345'
098200111005     O                                              '^ACN,27,15'
098300000118     O                                              '^FD'
098400000118     O                       dpd4
098500000118     O                                              '^FS'
098600000118      * destinatario
098700000118     O          E            LV22dpd2    1
098800111005     O                                              '^FO000,375'
098900111005     O                                              '^ACN,54,15'
099000000118     O                                              '^FD'
099100000118     O                       §ldrsd
099200000118     O                                              '^FS'
099300000118      *indirizzo destinatario
099400000118     O          E            LV22dpd2    1
099500111005     O                                              '^FO000,435'
099600111005     O                                              '^ACN,54,15'
099700000118     O                                              '^FD'
099800000118     O                       §ldind
099900000118     O                                              '^FS'
100000000118      *località destinatario
100100000118     O          E            LV22dpd2    1
100200111005     O                                              '^FO000,495'
100300111005     O                                              '^A0N,75,45'
100400000118     O                                              '^FD'
100500000118     O                       §ldlod
100600000118     O                                              '^FS'
100700000118      *cap destinatario
100800000118     O          E            LV22dpd2    1
100900111005     O                                              '^FO000,585'
101000111005     O                                              '^A0N,105,60'
101100000118     O                                              '^FD'
101200000118     O                       §ldcad
101300000118     O                                              '^FS'
101400000118      *nazione  destinatario
101500000118     O          E            LV22dpd2    1
101600111005     O                                              '^FO300,585'
101700111005     O                                              '^A0N,105,60'
101800000118     O                                              '^FD'
101900000118     O                       §desia
102000000118     O                                              '^FS'
102100000118     O* costante dpd
102200000118     O          E            LV22dpd2    1
102300111005     O                                              '^FO135,000'
102400111005     O                                              '^ACB,81,60'
102500000118     O                                              '^FD'
102600000118     O                       dpd
102700000118     O                                              '^FS'
102800000118     o          E            LV22dpd2    1
102900000118     O                                              '^XZ'
