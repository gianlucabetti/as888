000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200971209     F* FNLV22RZ *---------------------------------------------------*
000300910403     F*-----------------*                                            *
000400971118     F* STAMPA SEGNACOLLO CON BARCODE A 18 CON ZEBRA 8 DOT/MM        *
000500921120     F*        CON BARCODE 3X70                                      *
000600910403     F*--------------------------------------------------------------*
000700941216     FFNLV22P   O    F  132        PRINTER
000800910404     D §RI             S              1    DIM(37)
000900951020     D §RX             S              1    DIM(11)
001000910403     D §O7             S              1    DIM(7)
001100980505     D §S9             S              1    DIM(9)
001200980505     D §O9             S              1    DIM(9)
001300980507     D §EU             S              1    DIM(35)
001400910403     D                 DS
001500910403     D  §CB                    1     18  0
001600910403     D                                     DIM(18)
001700910403     D  §CA                    1     18
001800910403     D                                     DIM(18)
001900910403     D                 DS
002000910403     D  §N7                    1      7  0
002100910403     D  §S7                    1      7
002200910403     D                                     DIM(7)
002300910403     D                 DS
002400910403     D  §2                     1      2  0
002500910403     D                                     DIM(2)
002600910403     D  §2A                    1      2
002700910403     D                                     DIM(2)
002800910403     D DSBLD         E DS
002900971209     D DSBLDE        E DS
003000910403     D DSBLD7        E DS
003100910403     D  §C                     1     17  0
003200910403     D                                     DIM(17)
003300011119     D §bceurex        DS
003400011119     D  alfa5                         5
003500011119     D  nume12                       12  0
003600011119     D  alfa2                         2
003700000118     D §dpd            DS
003800000118     D  §dpd3                         3
003900000118     D  §dpd8                         8
004000000118     D  §dpd12                        1
004100000118     D §deawbds        DS
004200000118     D  §dpdc1w                       2
004300000118     D  §dpdc3w                       5
004400000118     D  §dpdc5w                       2
004500000118     D  §dpdc7w                       1
004600000124     D §dexx1ds        DS
004700000124     D  §dex1                         5
004800000124     D  §dex2                         5
004900000124     D  §dex3                         3
005000000124     D  §dex4                         2
005100000118     D §dpdcap         DS
005200000118     D  §dpdc1                        2
005300000118     D  §dpdc2                        1    inz('-')
005400000118     D  §dpdc3                        5
005500000118     D  §dpdc4                        1    inz('-')
005600000118     D  §dpdc5                        2
005700000118     D  §dpdc6                        1    inz('-')
005800000118     D  §dpdc7                        1
005900000118     d §dpdn           ds
006000000118     d §dpdn1                        38
006100000118     d §dpdn2                        38
006200000118     d §dpdn3                        38
006300000118     d §dpdn4                        39
006400000118     d dpd             c                   const('DPD')
006500000118     d dpd1            c                   const('Deutscher Paket Dienst')
006600000118     d dpd3            c                   const('Sender:')
006700000118     d dpd4            c                   const('Receiver:')
006800000118     d dpd2            ds
006900000118     d  dpdtel                        3    inz('Tel')
007000000118     d  dpdteln                      15
007100000118     d dpd2f           ds
007200000118     d  dpdfax                        3    inz('Fax')
007300000118     d  dpdfaxn                      15
007400000118     d dpd8            c                   const('30519 Hannover----------x')
007500000118     d dpd9            c                   const('01')
007600910403     I/SPACE 3
007700910403     C*---------------------------------------------------------------*
007800910403     C     *ENTRY        PLIST
007900910403     C                   PARM                    DSBLD
008000971209     C                   PARM                    DSBLDE
008100941216     C*
008200951021     C*** PROVE CON VALORI FISSI + ETICHETTE
008300970303     C**                   DO   2
008400970303     C**                   TIME           §HMS    60
008500970303     C**                   MOVEL§HMS      §LDCDP
008600970303     C**                   ADD  1         §LDNCD
008700941216     C*
008800971222     C                   MOVEL     §LDNRS        §LDNRT            3
008900971222     C                   MOVE      '-'           §LDNRT
009000971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
009100971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
009200971222     C     §DETST        IFEQ      'EST'
009300971222     C                   EXSR      COLZER
009400971222     C                   ENDIF
009500971222     C*
009600971209     C*
009700971222     C**                   ENDDO
009800980505     C*
009900980505     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
010000980505     C*
010100980505     C     §DETST        IFEQ      'EST'
010200980505     C     §DETST        OREQ      'ALL'
010300000118     c                   select
010400000118     c     §detse        wheneq    'REX'
010500000118     c                   except    lv22e
010600000118     c     §detse        wheneq    'EUR'
010700000118     c                   exsr      eurexp
010800000118     c     §detse        wheneq    'DP1'
010900000118     c                   exsr      dpddove
011000000118     c     §detse        wheneq    'DP2'
011100000118     c                   exsr      dpddove
011200000118     c                   exsr      dpdchi
011300000118     c                   endsl
011400980505     C                   ENDIF
011500000202     C     §DETST        IFEQ      'ITA'
011600000202     C     §DETST        OREQ      'ALL'
011700000202     C                   EXSR      PRINT
011800000202     C                   ENDIF
011900910403     C*
012000910403     C                   SETON                                        LR
012100951021     C/SPACE 005
012200951021     C*****************************************************
012300971209     C* IMPOSTAZIONE E STAMPA ETICHETTA ITALIA          ***
012400951021     C*****************************************************
012500951023     C     PRINT         BEGSR
012600951023     C                   MOVE      §LDLNP        §LCLNP
012700951023     C                   MOVE      §LDLNA        §LCLNA
012800951023     C                   MOVE      §LDNRS        §LCNRS
012900951023     C                   MOVE      §LDNCD        §LCNCD
013000951023     C                   MOVE      §LDZSC        §LCZSC
013100040430     c     §detse        comp      'EEX'                                  55
013200951023     C*
013300951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
013400951023     C                   EXSR      M10
013500951023     C                   MOVEA     §CA           §C18A            18
013600951023     C                   MOVE      §C18A         §CDBAR           18
013700951023     C*
013800970221     C     §LDTSP        IFEQ      'D'
013900970303     C     §LDLNA        ANDLT     300
014000970221     C                   MOVE      *BLANK        §LDTSP
014100970221     C                   END
014200951023     C*
014300040430     ***C     §LDLNA        COMP      300                                15
014400951023     C     §LDTSP        COMP      *BLANK                             1616
014500951023     C     §LDTSP        COMP      'E'                                    17
014600090806     C  N17§LDTSP        COMP      'H'                                    17
014700951023     C     §LDCDP        COMP      *BLANK                             18
014800951023     C     §LDNRS        COMP      *ZERO                              19
014900951023     C*
015000951023     C** INIZIALIZZA STAMPANTE
015100951023     C                   EXCEPT    LV22
015200951023     C*
015300951023     C                   MOVEA     *BLANK        §RX
015400951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
015500951023     C                   SETOFF                                       77
015600951023     C                   MOVE      *BLANK        §A7               7
015700951023     C                   Z-ADD     §LDNC1        §N7
015800951023     C                   DO        7             §1
015900951023     C     §S7(§1)       IFEQ      *ZERO
016000951023     C                   MOVE      *BLANK        §S7(§1)
016100951023     C                   ELSE
016200951023     C                   SETON                                        77
016300951023     C                   END
016400951023     C  N77              END
016500951023     C                   MOVEA     §S7           §A7
016600951023     C                   MOVE      §A7           §A3               3
016700951023     C                   MOVEA     §A3           §RX(1)
016800951023     C                   MOVEA     '-'           §RX(4)
016900951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
017000951023     C                   SETOFF                                       77
017100951023     C                   MOVE      *BLANK        §A7
017200951023     C                   Z-ADD     §LDNC2        §N7
017300951023     C                   DO        7             §1
017400951023     C     §S7(§1)       IFEQ      *ZERO
017500951023     C                   MOVE      *BLANK        §S7(§1)
017600951023     C                   ELSE
017700951023     C                   SETON                                        77
017800951023     C                   END
017900951023     C  N77              END
018000951023     C                   MOVEA     §S7           §A7
018100951023     C                   MOVE      §A7           §A3
018200951023     C                   MOVEA     §A3           §RX(5)
018300951023     C                   MOVE      §LDCPD        §A3
018400951023     C                   MOVEA     §A3           §RX(9)
018500951023     C* STAMPA LINEA PARTENZA
018600951023     C                   EXCEPT    LV22H
018700951023     C*
018800951023     C                   MOVEA     *BLANK        §RI
018900951023     C                   MOVEA     §LDRSM        §RI(8)
019000951023     C                   MOVEA     ' '           §RI(30)
019100951023     C                   MOVEA     §LDPRM        §RI(31)
019200951023     C                   MOVEA     *BLANK        §RI(33)
019300951023     C                   EXCEPT    LV22D1
019400951023     C* STAMPA DATI MITTENTE
019500951023     C*
019600951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
019700971222     C                   EXSR      COLZER
019800951023     C*
019900971222     C*
020000951023     C* STAMPA NUMERO SEGNACOLLO
020100951023     C                   EXCEPT    LV22D2
020200951023     C*
020300951023     C                   MOVE      §LDTNA        §A3               3
020400951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
020500951023     C                   MOVE      §LDLNA        §A3
020600951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
020700951023     C                   MOVE      §LDZSC        §A2
020800951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
020900951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
021000951023     C                   MOVEL     §LDDLA        §ODDLA            8
021100951023     C* STAMPA LINEA ARRIVO
021200040430     ***C* X 15 STAMPA A "MODOD ESTERO"
021300040430     ***C   15              DO
021400040430     ***C                   MOVEL     §ODDLA        §EEN              3
021500040430     ***C                   MOVEL     §LDCAD        §CA2              2
021600040430     ***C                   MOVE      §LDCAD        §CA7              7
021700040430     ***C                   END
021800951023     C                   EXCEPT    LV22D3
021900951023     C*
022000951023     C                   MOVEA     *BLANK        §RI
022100951023     C                   MOVEA     §LDRSD        §RI(8)
022200951023     C                   MOVEA     ' '           §RI(30)
022300951023     C                   MOVEA     §LDPRD        §RI(31)
022400951023     C                   MOVEA     *BLANK        §RI(33)
022500951023     C* STAMPA 1° DATI DESTINATARIO
022600951023     C                   EXCEPT    LV22D4
022700951023     C*
022800951023     C                   MOVEA     *BLANK        §RI
022900951023     C                   MOVEA     §LDIND        §RI(8)
023000951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
023100951023     C                   Z-ADD     37            §Y                2 0    88
023200951023     C                   DO        36
023300951023     C                   SUB       1             §Y
023400951023     C     §RI(§Y)       IFNE      ' '
023500951023     C                   SETON                                        88
023600951023     C                   END
023700951023     C  N88              END
023800951023     C     §Y            IFLE      25
023900951023     C                   ADD       3             §Y
024000951023     C                   ELSE
024100951023     C                   MOVEA     '  '          §RI(26)
024200951023     C                   Z-ADD     28            §Y
024300951023     C                   END
024400951023     C                   MOVEA     §LDLOD        §RI(§Y)
024500951023     C* STAMPA 2° DATI DESTINATARIO
024600951023     C                   EXCEPT    LV22D5
024700951023     C* STAMPA SERVIZIO
024800951023     C   16              EXCEPT    LV22X5
024900951023     C*
025000951023     C* STAMPA BARCODE
025100951023     C*
025200951023     C* (CAMPO UNICO BARCODE E COSTANTI)
025300951023     C                   MOVE      *BLANK        §§CDB1           19
025400951023     C                   MOVE      §CDBAR        §§CDB1
025500951023     C                   MOVEL     '"'           §§CDB1
025600951023     C                   MOVE      *BLANK        §§CDB2           21
025700951023     C                   MOVEL     §§CDB1        §§CDB2
025800951023     C                   MOVE      '";'          §§CDB2
025900951023     C                   EXCEPT    LV22D6
026000951023     C*
026100951023     C                   MOVE      *BLANK        §RI21            21
026200951023     C   18              DO
026300951023     C                   MOVEL     'Rif.:'       §RI21
026400951023     C                   MOVE      §LDCDP        §RI21
026500951023     C                   END
026600951023     C* STAMPA RIFERIMENTO/RISTAMPA
026700951023     C                   EXCEPT    LV22D7
026800951023     C*
026900951023     C* STAMPA CHIUSURA ETICHETTA
027000951023     C                   EXCEPT    LV22D8
027100951023     C                   ENDSR
027200971222     C/SPACE 005
027300971222     C*****************************************************
027400971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027500971222     C*****************************************************
027600971222     C     COLZER        BEGSR
027700971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027800971222     C                   SETOFF                                       77
027900971222     C                   MOVE      §LDNCD        §N7
028000971222     C                   DO        7             §1
028100971222     C     §S7(§1)       IFEQ      *ZERO
028200971222     C                   MOVE      *BLANK        §S7(§1)
028300971222     C                   ELSE
028400971222     C                   SETON                                        77
028500971222     C                   END
028600971222     C  N77              END
028700971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
028800971222     C                   MOVE      *ZERO         §X                1 0
028900971222     C                   MOVEA     *BLANK        §O7
029000971222     C                   DO        7             §1
029100971222     C     §S7(§1)       IFNE      *BLANK
029200971222     C                   ADD       1             §X
029300971222     C     §S7(§1)       IFEQ      '0'
029400971222     C                   MOVE      'O'           §O7(§X)
029500971222     C                   ELSE
029600971222     C                   MOVE      §S7(§1)       §O7(§X)
029700971222     C                   END
029800971222     C                   END
029900971222     C                   END
030000971222     C                   MOVEA     §O7           §S7
030100971222     C                   ENDSR
030200971209     C/SPACE 005
030300980507     C*****************************************************
030400980507     C* STAMPA EUROEXPRESS                              ***
030500980507     C*****************************************************
030600980507     C     EUREXP        BEGSR
030700980507     C*
030800980507     C* IMPOSTO COSTANTI
030900980507     C                   MOVEL     'JUT'         §DEJEU            3
031000980507     C                   MOVEL     '55L'         §DE55L            3
031100980507     C                   MOVEL     'IT'          §DEISP            2
031200011116      * controllo se contenuto 55L nel codice passato
031300011116     c     '55L'         scan      §deeur                                 23
031400011119      * se contenuto 55l scompongo a modo vecchio
031500011119     c                   if        *in23
031600980507     C* ESTRAGGO IL CAP:
031700980507     C                   MOVE      *ZEROS        §Y
031800980507     C                   MOVEA     §DEEUR        §EU
031900980507     C* TROVO QUANTO è LUNGO IL BARCODE
032000980507     C     ' '           CHECKR    §DEEUR        §Y
032100980507     C* ISOLO IL CAP
032200980507     C                   MOVE      *BLANK        §O9
032300980507     C                   MOVE      *ZEROS        §W                2 0
032400980507     C     §Y            SUB       2             §Y
032500980507     C     22            DO        §Y            §X1               2 0
032600980507     C     §EU(§X1)      IFNE      *BLANKS
032700980507     C                   ADD       1             §W
032800980507     C                   MOVE      §EU(§X1)      §O9(§W)
032900980507     C                   END
033000980507     C                   END
033100980507     C*
033200980507     C                   MOVEA     §O9           §LDCAD
033300011119
033400011119      *scompongo a modo nuovo senza 55l
033500011119     c                   else
033600011119     c                   movel     §deeur        §bceurex
033700011119     c                   end
033800980507     C*
033900980507     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
034000980507     C* DEL BARCODE
034100980507     C                   MOVEL     '^FS'         §END              7
034200980507     C     §LDCAD        CAT       §DEISA:0      §BCD             11
034300980507     C     §BCD          CAT       §END:0        §BCEND           20
034400980507     C*
034500980507     C* VERIFICO SE CONTRASSEGNO
034600980507     C*
034700980507     C     §DEFCA        COMP      'S'                                    14
034800980507     C  N14§DEFCA        COMP      'X'                                    14
034900980507     C*
035000980507     C                   EXCEPT    LV22Y
035100980507     C*
035200980507     C                   ENDSR
035300910403     C/SPACE 005
035400910403     C*****************************************************
035500910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
035600910403     C*****************************************************
035700910403     C     M10           BEGSR
035800910403     C                   MOVE      *ZERO         §TT               3 0
035900910403     C                   MOVE      *ZERO         §TT2              3 0
036000910403     C                   MOVE      *ZERO         §1                1 0
036100910403     C                   Z-ADD     18            §S                2 0
036200910403     C                   DO        17            §N                2 0
036300910403     C                   SUB       1             §S
036400910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
036500910403     C                   MOVE      §C(§N)        §CB(§N)
036600910403     C     §N            DIV       2             §CTR              3 0
036700910403     C                   MVR                     §RST              3 010
036800910403     C     *IN(10)       IFEQ      '1'
036900910403     C     §C(§S)        MULT      2             §N2               2 0
037000910403     C                   MOVE      §N2           §A2               2
037100910403     C                   MOVEA     §A2           §2A
037200910403     C                   XFOOT     §2            §CTR
037300910403     C                   ELSE
037400910403     C                   Z-ADD     §C(§S)        §CTR
037500910403     C                   END
037600910403     C                   ADD       §CTR          §TT
037700910403     C                   END
037800910403     C*
037900910403     C     §TT           ADD       9             §TT2
038000910403     C                   MOVE      0             §TT2
038100910403     C     §TT2          SUB       §TT           §1
038200910403     C*
038300910403     C                   MOVE      §1            §CB(18)
038400910403     C                   ENDSR
038500000118      *--------------------------------------------------
038600000118     c     dpddove       begsr
038700000118      *--------------------------------------------------
038800000118     c                   eval      §dpd = §deeur
038900000124     c                   eval      §dexx1ds = §dexx1
039000000118     c                   eval      §deawbds = §deawb
039100000118     c                   eval      §dpdc1 = §dpdc1w
039200000118     c                   eval      §dpdc3 = §dpdc3w
039300000118     c                   eval      §dpdc5 = §dpdc5w
039400000118     c                   eval      §dpdc7 = §dpdc7w
039500000118     c                   eval      §dpdn = 'Notifications on damages which are -
039600000118     c                             not recognizable from the outside have to -
039700000118     c                             have to be submitted to the transport -
039800000118     c                             organisation withing 7 days in written.'
039900000118     C                   except    LV22dpd1
040000000118     C                   endsr
040100000118      *--------------------------------------------------
040200000118     c     dpdchi        begsr
040300000118      *--------------------------------------------------
040400000118     c                   move      §ldmgs        giorno            2 0
040500000118     c                   movel     §ldmgs        mese              2 0
040600000118     c                   move      mese          giomes            4 0
040700000118     c                   movel     giorno        giomes
040800000118     c                   movel     giomes        datasp            8 0
040900000118     c                   move      §ldaas        datasp
041000000118     c                   movel     §dexx2        dpdteln
041100000118     c                   move      §dexx2        dpdfaxn
041200000118     C                   except    LV22dpd2
041300000118     C                   endsr
041400000118      *--------------------------------------------------
041500000118     O*
041600971114     O********* INIZIALIZZAZIONE ETICHETTA
041700971209     O********* ETICHETTA ITALIA
041800971209     O*
041900971120     OFNLV22P   E            LV22             01
042000971114     O                                              '^XA'
042100971114     O          E            LV22        1
042200971117     O                                              '^LH80,40'
042300971114     O          E            LV22        1
042400971114     O                                              '^PRC^MSY^FS'
042500971114     O********* Mittente
042600971114     O          E            LV22D1      1
042700971114     O                                              '^FO040,000'
042800971114     O                                              '^ACN,36,10'
042900971114     O                                              '^FD'
043000971114     O                       §RI
043100971114     O                                              '^FS'
043200971114     O********* Destinatario (Rag.Soc.)
043300971114     O          E            LV22D4      1
043400971114     O                                              '^FO120,040'
043500971114     O                                              '^GB390,0,3'
043600971114     O                                              '^FS'
043700971114     O          E            LV22D4      1
043800971114     O                                              '^FO040,055'
043900971114     O                                              '^ACN,36,10'
044000971114     O                                              '^FD'
044100971114     O                       §RI
044200971114     O                                              '^FS'
044300971114     O********* Destinatario (Località)
044400971114     O          E            LV22D5      1
044500971114     O                                              '^FO041,095'
044600971114     O                                              '^ACN,36,10'
044700971114     O                                              '^FD'
044800971114     O                       §RI
044900971114     O                                              '^FS'
045000971114     O********* MC. SPEDIZIONE
045100971114     O          E            LV22D5      1
045200971114     O                                              '^FO495,120'
045300971114     O                                              '^ACN,36,10'
045400971114     O                                              '^FD'
045500971114     O                                              'MC.'
045600971114     O                                              ' '
045700971114     O                       §LDVOL                 '0 ,   '
045800971114     O                                              ' '
045900971114     O                       §LDFVR
046000971114     O                                              '^FS'
046100971114     O********* SERVIZIO
046200971114     O          E   N17      LV22X5      1
046300971114     O                                              '^FO520,000'
046400971114     O                                              '^GB120,110,3'
046500971114     O                                              '^FS'
046600971114     O          E   N17      LV22X5      1
046700971114     O                                              '^FO550,010'
046800971114     O                                              '^A0N,120,120'
046900971114     O                                              '^FD'
047000971114     O                       §LDTSP
047100971114     O                                              '^FS'
047200971114     O          E    17      LV22X5      1
047300971114     O                                              '^FO520,000'
047400971114     O                                              '^GB120,110,60'
047500971114     O                                              '^FS'
047600971114     O          E    17      LV22X5      1
047700971114     O                                              '^FO550,010'
047800971114     O                                              '^A0N,120,120'
047900971114     O                                              '^FR'
048000971114     O                                              '^FD'
048100971114     O                       §LDTSP
048200971114     O                                              '^FS'
048300971114     O********* Linea Partenza
048400971114     O          E            LV22H       1
048500971114     O                                              '^FO120,140'
048600971114     O                                              '^ACN,54,20'
048700971114     O                                              '^FD'
048800971209     O                       §LDLNP
048900971114     O                                              ' '
049000971114     O                       §LDDLP
049100971114     O                                              '^FS'
049200971114     O********* Sequenza colli
049300971114     O          E            LV22H       1
049400971114     O                                              '^FO495,160'
049500971114     O                                              '^ACN,36,10'
049600971114     O                                              '^FD'
049700971114     O                       §RX
049800971114     O                                              '^FS'
049900971114     O********* Segnacollo
050000971114     O          E    19      LV22D2      1
050100971114     O                                              '^FO000,240'
050200971114     O                                              '^A0N,120,60'
050300971114     O                                              '^FD'
050400971114     O                       §LDNRT
050500971114     O                                              '^FS'
050600971114     O          E            LV22D2      1
050700971114     O                                              '^FO115,200'
050800971114     O                                              '^A0N,180,120'
050900971114     O                                              '^FD'
051000971114     O                       §S7
051100971114     O                                              '^FS'
051200971114     O********* TERMINAL/LINEA/ZONA ARRIVO
051300971114     O********* TERMINAL
051400971114     O          E            LV22D3      1
051500971114     O                                              '^FO000,350'
051600971114     O                                              '^GB120,110,60'
051700971114     O                                              '^FS'
051800971114     O          E            LV22D3      1
051900971114     O                                              '^FO015,360'
052000971114     O                                              '^A0N,120,60'
052100971114     O                                              '^FR'
052200971114     O                                              '^FD'
052300971114     O                       §ODTNA
052400971114     O                                              '^FS'
052500971114     O********* CODICE LINEA ARRIVO
052600971114     O          E            LV22D3      1
052700971114     O                                              '^FO135,360'
052800971114     O                                              '^A0N,120,60'
052900971114     O                                              '^FD'
053000971114     O                       §ODLNA
053100971114     O                                              '^FS'
053200971114     O********* ZONA ARRIVO
053300040430     O          E   N55      LV22D3      1
053400971114     O                                              '^FO550,350'
053500971114     O                                              '^GB090,110,60'
053600971114     O                                              '^FS'
053700040430     O          E   N55      LV22D3      1
053800971114     O                                              '^FO565,360'
053900971114     O                                              '^A0N,120,60'
054000971114     O                                              '^FR'
054100971114     O                                              '^FD'
054200971114     O                       §ODZSC
054300971114     O                                              '^FS'
054400971114     O********* DESCRIZIONE LINEA DI ARRIVO
054500040430     O          E   N55      LV22D3      1
054600971114     O                                              '^FO240,360'
054700971114     O                                              '^A0N,120,60'
054800971114     O                                              '^FD'
054900971114     O                       §ODDLA
055000971114     O                                              '^FS'
055100040430     O********* Box per nazione e cap EEX
055200040430     O          E    55      LV22D3      1
055300040430     O                                              '^FO270,350'
055400040430     O                                              '^GB300,110,3'
055500040430     O                                              '^FS'
055600040430     O********* Nazione per EEX
055700040430     O          E    55      LV22D3      1
055800040430     O                                              '^FO280,360'
055900040430     O                                              '^A0N,120,60'
056000040430     O*** NO Reverse Image:                         '^FR'
056100040430     O                                              '^FD'
056200040430     O                       §DEISA
056300040430     O                                              '^FS'
056400040430     O********* CAP estero per EEX
056500040430     O          E    55      LV22D3      1
056600040430     O                                              '^FO380,360'
056700040430     O                                              '^A0N,120,60'
056800040430     O                                              '^FD'
056900040430     O                       §LDCAD
057000040430     O                                              '^FS'
057100040430     ***O          E    15      LV22D3      1
057200040430     ***O                                              '^FO240,350'
057300040430     ***O                                              '^GB120,110,60'
057400040430     ***O                                              '^FS'
057500040430     ***O          E    15      LV22D3      1
057600040430     ***O                                              '^FO255,360'
057700040430     ***O                                              '^A0N,120,60'
057800040430     ***O                                              '^FR'
057900040430     ***O                                              '^FD'
058000040430     ***O                       §EEN
058100040430     ***O                                              '^FS'
058200040430     ***O          E    15      LV22D3      1
058300040430     ***O                                              '^FO375,360'
058400040430     ***O                                              '^A0N,120,60'
058500040430     ***O                                              '^FD'
058600040430     ***O                       §CA2
058700040430     ***O                                              ' '
058800040430     ***O                       §CA7
058900040430     ***O                                              '^FS'
059000041115     O********* chiusura superiore
059100041115     O          E            LV22D6      1
059200041115     O                                              '^FO065,463'
059300041115     O                                              '^GBO402,010,010'
059400041115     O                                              '^FS'
059500041115     O********* BARCODE  code 128
059600041115     O          E            LV22D6      1
059700041115     O                                              '^BY3,3,0'
059800041115     O          E            LV22D6      1
059900041115     O                                              '^FO065,470'
060000041115     O                                              '^BCN,240,N,N,N'
060100041115     O                                              '^FD>;'
060200041115     O                       §CDBAR
060300041115     O                                              '^FS'
060400041115     O********* chiusura inferiore
060500041115     O          E            LV22D6      1
060600041115     O                                              '^FO065,710'
060700041115     O                                              '^GBO402,005,005'
060800041115     O                                              '^FS'
060900041115     O********* BARCODE  INTERL. 2/5
061000041115     O*         E            LV22D6      1
061100041115     O*                                             '^BY3,3,0^FS'
061200041115     O*         E            LV22D6      1
061300041115     O*                                             '^FO065,470'
061400041115     O*                                             '^B2N,240,N,N,N'
061500041115     O*                                             '^FD'
061600041115     O*                      §CDBAR
061700041115     O*                                             '^FS'
061800971114     O********* Codice Prodotto/KG/RISTAMPA
061900971114     O          E            LV22D7      1
062000971114     O                                              '^FO000,720'
062100971114     O                                              '^ACN,36,10'
062200971114     O                                              '^FD'
062300971114     O                       §RI21
062400971114     O                                              '^FS'
062500971114     O          E            LV22D7      1
062600971114     O                                              '^FO400,720'
062700971114     O                                              '^ACN,36,10'
062800971114     O                                              '^FD'
062900971114     O                                              'KG.'
063000971114     O                                              ' '
063100971114     O                       §LDPKG                 '    0 , '
063200971114     O                                              '^FS'
063300971114     O          E            LV22D7      1
063400971114     O                                              '^FO600,720'
063500971114     O                                              '^ACN,36,10'
063600971114     O                                              '^FD'
063700971114     O                       §LDRST
063800971114     O                                              '^FS'
063900971114     O********* Chiusura etchetta
064000971114     O          E            LV22D8      1
064100971114     O                                              '^XZ'
064200971209     O*
064300971209     O********* INIZIALIZZAZIONE ETICHETTA
064400971209     O********* ETICHETTA ESTERO
064500971209     O*
064600971209     O          E            LV22E            01
064700971209     O                                              '^XA'
064800971209     O          E            LV22E       1
064900971209     O                                              '^LH80,30'
065000971209     O          E            LV22E       1
065100971209     O                                              '^PRC^MSY^FS'
065200971209      *---*   TIPO PRODOTTO DHL -REX-
065300971209     O          E            LV22E       1
065400971209     O                                              '^FO160,000'
065500971209     O                                              '^A0N,30,25'
065600971209     O                                              '^FD'
065700971209     O                                              'PRODUCT:'
065800971209     O                                              '^FS'
065900971209     O          E            LV22E       1
066000971209     O                                              '^FO280,000'
066100971209     O                                              '^GB344,120,2'
066200971209     O                                              '^FS'
066300971209     O          E            LV22E       1
066400971209     O                                              '^FO285,016'
066500971209     O                                              '^A0N,120,200'
066600971209     O                                              '^FD'
066700971209     O                                              'REX'
066800971209     O                                              '^FS'
066900971209      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
067000971209     O          E            LV22E       1
067100971209     O                                              '^FO000,220'
067200971209     O                                              '^A0N,30,25'
067300971209     O                                              '^FD'
067400971209     O                                              'DESTINATION:'
067500971209     O                                              '^FS'
067600971209     O          E            LV22E       1
067700971209     O                                              '^FO000,264'
067800971209     O                                              '^A0N,130,80'
067900971209     O                                              '^FD'
068000971209     O                       §DESIA
068100971209     O                                              '^FS'
068200971209     O          E            LV22E       1
068300971209     O                                              '^BY3,3,0^FS'
068400971209     O          E            LV22E       1
068500971209     O                                              '^FO198,156'
068600971209     O                                              '^B3N,N,208,N,N'
068700971209     O                                              '^FD'
068800971209     O                       §DESIP
068900971209     O                       §DESIA
069000971209     O                       §DESSE
069100971209     O                                              '^FS'
069200971209      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
069300971209     O          E            LV22E       1
069400971209     O                                              '^FO000,392'
069500971209     O                                              '^A0N,30,25'
069600971209     O                                              '^FD'
069700971209     O                                              'AWB:'
069800971209     O                                              '^FS'
069900971209     O          E            LV22E       1
070000971209     O                                              '^FO164,392'
070100971209     O                                              '^A0N,55,75'
070200971209     O                                              '^FD'
070300971209     O                       §DEAWB
070400971209     O                                              '^FS'
070500971209     O          E            LV22E       1
070600971209     O                                              '^BY3,3,0^FS'
070700971209     O          E            LV22E       1
070800971209     O                                              '^FO054,456'
070900971209     O                                              '^B3N,N,208,N,N'
071000971209     O                                              '^FD'
071100971209     O                       §DEAWB
071200971209     O                                              '^FS'
071300971209      *---*   NAZIONE DESTINO -CODICE ISO-
071400971209     O          E            LV22E       1
071500971209     O                                              '^FO000,690'
071600971209     O                                              '^A0N,100,85'
071700971209     O                                              '^FD'
071800971209     O                       §DEISA
071900971209     O                                              '^FS'
072000971209      *---*
072100971209     O          E            LV22E       1
072200971209     O                                              '^FO150,680'
072300971209     O                                              '^A0N,44,33'
072400971209     O                                              '^FD'
072500971209     O                                              'P: '
072600971209     O                       §LDLNP
072700971209     O                                              ' A: '
072800971209     O                       §LDLNA
072900971209     O                                              '^FS'
073000971209     O          E            LV22E       1
073100971209     O                                              '^FO150,728'
073200971209     O                                              '^A0N,44,33'
073300971209     O                                              '^FD'
073400971209     O                                              'N. '
073500971209     O                       §LDNRT
073600971209     O                       §S7
073700971209     O                                              '^FS'
073800971209      *---*   SEQUENZA COLLO   (XX OF XX)
073900971209     O          E            LV22E       1
074000971209     O                                              '^FO400,680'
074100971209     O                                              '^A0N,44,44'
074200971209     O                                              '^FD'
074300971209     O                       §LDNC1        Z
074400971209     O                                              ' OF '
074500971209     O                       §LDNC2        Z
074600971209     O                                              '^FS'
074700971209      *---*   I.A.T.A. ORIGINE
074800971209     O          E            LV22E       1
074900971209     O                                              '^FO400,728'
075000971209     O                                              '^A0N,44,44'
075100971209     O                                              '^FD'
075200971209     O                                              'ORIGIN:  '
075300971209     O                       §DESIP
075400971209     O                                              '^FS'
075500971209      *---*   ULTIMO CARATTERE PER CHIUDERE LA GRAFICA:
075600971209     O          E            LV22E       1
075700971209     O                                              '^XZ'
075800980505     O*-------------------------------------------------------
075900980505     O* ETICHETTA ESTERO EUROEXPRESS
076000980505     O*-------------------------------------------------------
076100980506      *---*   INIZIALIZZAZIONI
076200980505     O          E            LV22Y            01
076300980505     O                                              '^XA'
076400980505     O          E            LV22Y       1
076500980507     O                                              '^LH40,50'
076600980505     O          E            LV22Y       1
076700980505     O                                              '^PRC^MSY^FS'
076800980505      *---*   NAZIONE PARTENZA
076900980505     O          E            LV22Y       1
077000980505     O                                              '^FO160,000'
077100980505     O                                              '^A0N,060,050'
077200980505     O                                              '^FD'
077300980505     O                       §DEISP
077400980505     O                                              '^FS'
077500980505      *---*   CONTRASSEGNO
077600980505     O          E    14      LV22Y       1
077700980505     O                                              '^FO425,000'
077800980505     O                                              '^GB200,105,3'
077900980505     O                                              '^FS'
078000980505     O          E    14      LV22Y       1
078100980505     O                                              '^FO440,015'
078200980505     O                                              '^A0N,100,100'
078300980505     O                                              '^FD'
078400980505     O                                              'COD'
078500980505     O                                              '^FS'
078600980505      *---*   EUROEXPRESS
078700980505     O          E            LV22Y       1
078800980505     O                                              '^FO160,140'
078900980505     O                                              '^A0N,120,100'
079000980505     O                                              '^FD'
079100980505     O                                              'EuroExpress'
079200980505     O                                              '^FS'
079300980505      *---*   NAZIONE ARRIVO
079400980506     O          E            LV22Y       1
079500980506     O                                              '^FO035,265'
079600980506     O                                              '^GB200,150,3'
079700980506     O                                              '^FS'
079800980506     O          E            LV22Y       1
079900980506     O                                              '^FO045,275'
080000980506     O                                              '^A0N,030,030'
080100980507     O                                              '^FD'
080200980506     O                                              'Country'
080300980506     O                                              '^FS'
080400980505     O          E            LV22Y       1
080500980505     O                                              '^FO045,310'
080600980505     O                                              '^A0N,120,100'
080700980505     O                                              '^FD'
080800980505     O                       §DEISA
080900980505     O                                              '^FS'
081000980505      *---*   CAP DESTINO
081100980506     O          E            LV22Y       1
081200980506     O                                              '^FO260,265'
081300980506     O                                              '^GB400,150,3'
081400980506     O                                              '^FS'
081500980506     O          E            LV22Y       1
081600980506     O                                              '^FO270,275'
081700980506     O                                              '^A0N,030,030'
081800980506     O                                              '^FD'
081900980506     O                                              'Post Code'
082000980506     O                                              '^FS'
082100980505     O          E            LV22Y       1
082200980506     O                                              '^FO270,310'
082300980506     O                                              '^A0N,120,100'
082400980506     O                                              '^FD'
082500980506     O                       §LDCAD
082600980506     O                                              '^FS'
082700980505      *---*   BARCODE EUROEXPRESS
082800980507     O          E            LV22Y       1
082900011116     O              n23                             '^BY3,3,0^FS'
083000011116     O               23                             '^BY2,3,0^FS'
083100011119      *senza 55l
083200011119     O          E   n23      LV22Y       1
083300980506     O                                              '^FO040,430'
083400980505     O                                              '^BCN,200,Y,N,N'
083500980505     O                                              '^FD'
083600011119     O                       alfa5
083700980505     O                                              '>5'
083800011119     O                       nume12
083900011119     O                                              '>6'
084000011119     o                       alfa2
084100011119     O                       §end
084200011119      *con  55l
084300011119     O          E    23      LV22Y       1
084400011119     O                                              '^FO040,430'
084500011119     O                                              '^BCN,200,Y,N,N'
084600011119     O                                              '^FD'
084700011119     O                       §DEJEU
084800011119     O                       §DEISP
084900011119     O                                              '>5'
085000011119     O                       §LDLNP
085100011119     O                       §LDNRS
085200011119     O                       §LDNCD
085300011119     O                                              '>6'
085400011119     O                                              ' '
085500011119     O                       §DE55L
085600011119     O                       §BCEND
085700980505      *---*   RIFERIMENTI BARTOLINI
085800980505     O          E            LV22Y       1
085900980506     O                                              '^FO050,680'
086000980506     O                                              '^ADN,036,020'
086100980507     O                                              '^FD'
086200980506     O                       §LDLNP
086300980506     O                                              ' '
086400980506     O                       §LDNRS
086500980506     O                                              ' '
086600980506     O                       §LDNCD
086700980506     O                                              '^FS'
086800980505     O********* Chiusura etchetta
086900980505     O          E            LV22Y       1
087000980505     O                                              '^XZ'
087100000118     O*-------------------------------------------------------
087200000118     O* ETICHETTA ESTERO DPD
087300000118     O*-------------------------------------------------------
087400000118     O* etichetta 1 " DOVE "
087500000118     O          E            LV22dpd1         01
087600000118     o                                              '^XA'
087700000118     O          E            LV22dpd1    1
087800000118     o                                              '^LH80,40'
087900000118     O          E            LV22dpd1    1
088000000118     o                                              '^PRC^MSY^FS'
088100000118      *---*   numero  spedizione
088200000118     O          E            LV22dpd1    1
088300000128     O                                              '^FO260,020'
088400000128     O                                              '^ABN,22,14'
088500000118     O                                              '^FD'
088600000118     O                       §dpd3
088700000118     O                                              '^FS'
088800000118      * 8 caratteri numero spedizione
088900000118     O          E            LV22dpd1    1
089000000128     O                                              '^FO320,020'
089100000128     O                                              '^ABN,22,14'
089200000118     O                                              '^FD'
089300000118     O                       §dpd8
089400000118     O                                              '^FS'
089500000118      *ultimo carattere numero spedizione
089600000118     O          E            LV22dpd1    1
089700000128     O                                              '^FO470,020'
089800000128     O                                              '^ABN,22,14'
089900000118     O                                              '^FD'
090000000118     O                       §dpd12
090100000118     O                                              '^FS'
090200000118      *---*   costante scritta
090300000118     O          E            LV22dpd1    1
090400000118     O                                              '^FO170,080'
090500000118     O                                              '^ABN,11,07'
090600000118     O                                              '^FD'
090700000118     O                       §dpdn1
090800000118     O                                              '^FS'
090900000118     O          E            LV22dpd1    1
091000000118     O                                              '^FO170,100'
091100000118     O                                              '^ABN,11,07'
091200000118     O                                              '^FD'
091300000118     O                       §dpdn2
091400000118     O                                              '^FS'
091500000118     O          E            LV22dpd1    1
091600000118     O                                              '^FO170,120'
091700000118     O                                              '^ABN,11,07'
091800000118     O                                              '^FD'
091900000118     O                       §dpdn3
092000000118     O                                              '^FS'
092100000118     O          E            LV22dpd1    1
092200000118     O                                              '^FO170,140'
092300000118     O                                              '^ABN,11,07'
092400000118     O                                              '^FD'
092500000118     O                       §dpdn4
092600000118     O                                              '^FS'
092700000118     o*chiusura superiore
092800000118     O          e            lv22dpd1    1
092900000118     o                                              '^FO060,250'
093000000118     O                                              '^GB310,0,004'
093100000118     O                                              '^FS'
093200000118     o*chiusura INFERIORE
093300000118     O          e            lv22dpd1    1
093400000118     o                                              '^FO060,490'
093500000118     O                                              '^GB310,0,004'
093600000118     O                                              '^FS'
093700000118     o*  codice a barre
093800000118     O          e            lv22dpd1    1
093900000118     o                                              '^FO060,250^BY4,2'
094000000118     O                                              '^B2N,240,N,N,N'
094100000118     O                                              '^FD'
094200000118     O                       §deawb
094300000118     O                                              '^FS'
094400000118      *versione
094500000118     O          E            LV22dpd1    1
094600000118     O                                              '^FO200,200'
094700000118     O                                              '^ABN,33,21'
094800000118     O                                              '^FD'
094900000124     O                       §dex1
095000000118     O                                              '^FS'
095100000118      *decodifica 2° bcc
095200000118     O          E            LV22dpd1    1
095300000118     O                                              '^FO130,500'
095400000118     O                                              '^ABN,22,14'
095500000118     O                                              '^FD'
095600000118     O                       §dpdcap
095700000118     O                                              '^FS'
095800000118      *costante x
095900000118     O          E            LV22dpd1    1
096000000118     O                                              '^FO030,550'
096100000118     O                                              '^ACN,72,30'
096200000118     O                                              '^FD'
096300000118     O                       §desse
096400000118     O                                              '^FS'
096500000118      *costante 09
096600000118     O          E            LV22dpd1    1
096700000118     O                                              '^FO030,630'
096800000118     O                                              '^ACN,72,30'
096900000118     O                                              '^FD'
097000000118     O                       dpd9
097100000118     O                                              '^FS'
097200000118      *linea arrivo
097300000118     O          E            LV22dpd1    1
097400000118     O                                              '^FO180,590'
097500000118     O                                              '^ACN,126,40'
097600000118     O                                              '^FD'
097700000124     O                       §dex3
097800000118     O                                              '^FS'
097900000118      *zona arrivo
098000000118     O          E            LV22dpd1    1
098100000118     O                                              '^FO360,630'
098200000118     O                                              '^ACN,72,30'
098300000118     O                                              '^FD'
098400000124     O                       §dex4
098500000118     O                                              '^FS'
098600000118     O* costante dpd
098700000118     O          E            LV22dpd1    1
098800000118     O                                              '^FO090,000'
098900000118     O                                              '^ACB,54,40'
099000000118     O                                              '^FD'
099100000118     O                       dpd
099200000118     O                                              '^FS'
099300000118     o          E            LV22dpd1    1
099400000118     O                                              '^XZ'
099500000118     O* etichetta 2 " CHI "
099600000118     O*--------------------------------------------------------------------
099700000118     O          E            LV22dpd2         01
099800000118     o                                              '^XA'
099900000118     O          E            LV22dpd2    1
100000000118     o                                              '^LH80,40'
100100000118     O          E            LV22dpd2    1
100200000118     o                                              '^PRC^MSY^FS'
100300000118      *---*   numero  spedizione
100400000118     O          E            LV22dpd2    1
100500000118     O                                              '^FO170,010'
100600000118     O                                              '^ACN,72,30'
100700000118     O                                              '^FD'
100800000118     O                       §dpd3
100900000118     O                                              '^FS'
101000000118      * 8 caratteri numero spedizione
101100000118     O          E            LV22dpd2    1
101200000118     O                                              '^FO290,020'
101300000118     O                                              '^ACN,54,20'
101400000118     O                                              '^FD'
101500000118     O                       §dpd8
101600000118     O                                              '^FS'
101700000118      *ultimo carattere numero spedizione
101800000118     O          E            LV22dpd2    1
101900000118     O                                              '^FO480,020'
102000000118     O                                              '^ACN,36,20'
102100000118     O                                              '^FD'
102200000118     O                       §dpd12
102300000118     O                                              '^FS'
102400000118      *---*  telefono
102500000118     O          E            LV22dpd2    1
102600000118     O                                              '^FO170,080'
102700000118     O                                              '^ACN,18,10'
102800000118     O                                              '^FD'
102900000118     O                       dpd2
103000000118     O                                              '^FS'
103100000118      *---*  Fax
103200000118     O          E            LV22dpd2    1
103300000118     O                                              '^FO170,100'
103400000118     O                                              '^ACN,18,10'
103500000118     O                                              '^FD'
103600000118     O                       dpd2f
103700000118     O                                              '^FS'
103800000118      *---*   costante MITTENTE 1
103900000118     O          E            LV22dpd2    1
104000000118     O                                              '^FO160,125'
104100000118     O                                              '^ACN,18,10'
104200000118     O                                              '^FD'
104300000118     O                       dpd3
104400000118     O                                              '^FS'
104500000118      *---*   costante mittente 2
104600000118     O          E            LV22dpd2    1
104700000118     O                                              '^FO100,150'
104800000118     O                                              '^A0N,30,20'
104900000118     O                                              '^FD'
105000000118     O                       §ldrsm
105100000118     O                                              '^FS'
105200000118      *---*   costante mittente 3
105300000118     O          E            LV22dpd2    1
105400000118     O                                              '^FO100,185'
105500000118     O                                              '^A0N,30,20'
105600000118     O                                              '^FD'
105700000118     O                       §ldprm
105800000118     O                                              '^FS'
105900000118     o*LINEA DIVISIONE
106000000118     O          e            lv22dpd2    1
106100000118     o                                              '^FO090,220'
106200000118     O                                              '^GB400,0,004'
106300000118     O                                              '^FS'
106400000118     o*chiusura superiore
106500000118     O          e            lv22dpd2    1
106600000118     o                                              '^FO060,470'
106700000118     O                                              '^GB370,0,004'
106800000118     O                                              '^FS'
106900000118     o*chiusura INFERIORE
107000000118     O          e            lv22dpd2    1
107100000118     o                                              '^FO060,690'
107200000118     O                                              '^GB370,0,004'
107300000118     O                                              '^FS'
107400000118      *costante primo B-C
107500000118     O          e            lv22dpd2    1
107600000118     o                                              '^FO060,470^BY4,2'
107700000118     O                                              '^B2N,220,N,N,N'
107800000118     O                                              '^FD'
107900000118     O                       §dpd
108000000118     O                                              '^FS'
108100000118      *costante data
108200000118     O          E            LV22dpd2    1
108300000118     O                                              '^FO000,710'
108400000118     O                                              '^ABN,22,07'
108500000118     O                                              '^FD'
108600000118     O                       datasp                 '  /  /    '
108700000118     O                                              '^FS'
108800000118      *costante 1 collo
108900000118     O          E            LV22dpd2    1
109000000118     O                                              '^FO130,710'
109100000118     O                                              '^ABN,22,07'
109200000118     O                                              '^FD'
109300000118     O                                              '1 Collo'
109400000118     O                                              '^FS'
109500000118      *costante peso
109600000118     O          E            LV22dpd2    1
109700000118     O                                              '^FO230,710'
109800000118     O                                              '^ABN,22,07'
109900000118     O                                              '^FD'
110000000118     O                                              'Peso Kg'
110100000118     O                                              '^FS'
110200000118      *peso
110300000118     O          E            LV22dpd2    1
110400000118     O                                              '^FO320,710'
110500000118     O                                              '^ABN,22,14'
110600000118     O                                              '^FD'
110700000118     O                       §ldpkg        2
110800000118     O                                              '^FS'
110900000118      *costante destinatario
111000000118     O          E            LV22dpd2    1
111100000118     O                                              '^FO000,230'
111200000118     O                                              '^ACN,18,10'
111300000118     O                                              '^FD'
111400000118     O                       dpd4
111500000118     O                                              '^FS'
111600000118      * destinatario
111700000118     O          E            LV22dpd2    1
111800000118     O                                              '^FO000,250'
111900000118     O                                              '^ACN,36,10'
112000000118     O                                              '^FD'
112100000118     O                       §ldrsd
112200000118     O                                              '^FS'
112300000118      *indirizzo destinatario
112400000118     O          E            LV22dpd2    1
112500000118     O                                              '^FO000,290'
112600000118     O                                              '^ACN,36,10'
112700000118     O                                              '^FD'
112800000118     O                       §ldind
112900000118     O                                              '^FS'
113000000118      *località destinatario
113100000118     O          E            LV22dpd2    1
113200000118     O                                              '^FO000,330'
113300000118     O                                              '^A0N,50,30'
113400000118     O                                              '^FD'
113500000118     O                       §ldlod
113600000118     O                                              '^FS'
113700000118      *cap destinatario
113800000118     O          E            LV22dpd2    1
113900000118     O                                              '^FO000,390'
114000000118     O                                              '^A0N,70,40'
114100000118     O                                              '^FD'
114200000118     O                       §ldcad
114300000118     O                                              '^FS'
114400000118      *nazione  destinatario
114500000118     O          E            LV22dpd2    1
114600000118     O                                              '^FO200,390'
114700000118     O                                              '^A0N,70,40'
114800000118     O                                              '^FD'
114900000118     O                       §desia
115000000118     O                                              '^FS'
115100000118     O* costante dpd
115200000118     O          E            LV22dpd2    1
115300000118     O                                              '^FO090,000'
115400000118     O                                              '^ACB,54,40'
115500000118     O                                              '^FD'
115600000118     O                       dpd
115700000118     O                                              '^FS'
115800000118     o          E            LV22dpd2    1
115900000118     O                                              '^XZ'
