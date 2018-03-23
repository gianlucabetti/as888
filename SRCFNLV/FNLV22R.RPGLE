000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200941216     F* FNLV22R  *---------------------------------------------------*
000300910403     F*-----------------*                                            *
000400951021     F* STAMPA SEGNACOLLO CON BARCODE A 18 CON RJS                   *
000500921120     F*        CON BARCODE 3X70                                      *
000600910403     F*--------------------------------------------------------------*
000700941216     FFNLV22P   O    F  132        PRINTER
000800910404     D §RI             S              1    DIM(37)
000900951020     D §RX             S              1    DIM(11)
001000910403     D §O7             S              1    DIM(7)
001100980326     D §S9             S              1    DIM(9)
001200980326     D §O9             S              1    DIM(9)
001300980326     D §EU             S              1    DIM(35)
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
002900971215     D DSBLDE        E DS
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
006000000118     d §dpdn1                        51
006100000118     d §dpdn2                        49
006200000118     d §dpdn3                        53
006300000118     d dpd             c                   const('DPD')
006400000118     d dpd1            c                   const('Deutscher Paket Dienst')
006500000118     d dpd3            c                   const('Sender:')
006600000118     d dpd4            c                   const('Receiver:')
006700000118     d dpd2            ds
006800000118     d  dpdtel                        3    inz('Tel')
006900000118     d  dpdteln                      15
007000000118     d dpd2f           ds
007100000118     d  dpdfax                        3    inz('Fax')
007200000118     d  dpdfaxn                      15
007300000118     d dpd9            c                   const('01')
007400910403     I/SPACE 3
007500910403     C*---------------------------------------------------------------*
007600910403     C     *ENTRY        PLIST
007700910403     C                   PARM                    DSBLD
007800971215     C                   PARM                    DSBLDE
007900050606
008000050606      * Elimino carattere "\"
008100050606     c     '\':' '       xlate     §ldrsm        §ldrsm
008200050606     c     '\':' '       xlate     §ldrsd        §ldrsd
008300050606     c     '\':' '       xlate     §ldlod        §ldlod
008400050606     c     '\':' '       xlate     §ldind        §ldind
008500050606     c     '\':' '       xlate     §ldcdp        §ldcdp
008600941216     C*
008700951021     C*** PROVE CON VALORI FISSI + ETICHETTE
008800951023     C***                  DO   10
008900951023     C***                  TIME           §HMS    60
009000951023     C***                  MOVEL§HMS      §LDCDP
009100951023     C***                  ADD  1         §LDNCD
009200941216     C*
009300971222     C                   MOVEL     §LDNRS        §LDNRT            3
009400971222     C                   MOVE      '-'           §LDNRT
009500971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
009600971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
009700971222     C     §DETST        IFEQ      'EST'
009800971222     C                   EXSR      COLZER
009900971222     C                   ENDIF
010000971222     C*
010100910403     C*
010200951023     C***                  ENDDO
010300971215     C*
010400980402     C*
010500980326     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
010600971215     C*
010700971215     C     §DETST        IFEQ      'EST'
010800971215     C     §DETST        OREQ      'ALL'
010900000118     c                   select
011000000118     c     §detse        wheneq    'REX'
011100000118     c                   except    lv22e
011200000118     c     §detse        wheneq    'EUR'
011300000118     c                   exsr      eurexp
011400000118     c     §detse        wheneq    'DP1'
011500000118     c                   exsr      dpddove
011600000118     c     §detse        wheneq    'DP2'
011700000118     c                   exsr      dpddove
011800000118     c                   exsr      dpdchi
011900000118     c                   endsl
012000971215     C                   ENDIF
012100000202     C     §DETST        IFEQ      'ITA'
012200000202     C     §DETST        OREQ      'ALL'
012300000202     C                   EXSR      PRINT
012400000202     C                   ENDIF
012500971215     C*
012600910403     C                   SETON                                        LR
012700951021     C/SPACE 005
012800951021     C*****************************************************
012900951023     C* IMPOSTAZIONE E STAMPA                           ***
013000951021     C*****************************************************
013100951023     C     PRINT         BEGSR
013200951023     C                   MOVE      §LDLNP        §LCLNP
013300951023     C                   MOVE      §LDLNA        §LCLNA
013400951023     C                   MOVE      §LDNRS        §LCNRS
013500951023     C                   MOVE      §LDNCD        §LCNCD
013600951023     C                   MOVE      §LDZSC        §LCZSC
013700040429     c     §detse        comp      'EEX'                                  55
013800951023     C*
013900951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
014000951023     C                   EXSR      M10
014100951023     C                   MOVEA     §CA           §C18A            18
014200951023     C                   MOVE      §C18A         §CDBAR           18
014300970210     C*
014400970210     C     §LDTSP        IFEQ      'D'
014500970303     C     §LDLNA        ANDLT     300
014600970210     C                   MOVE      *BLANK        §LDTSP
014700970210     C                   END
014800970210     C*
014900951023     C     §LDTSP        COMP      *BLANK                             1616
015000951023     C     §LDTSP        COMP      'E'                                    17
015100090806     C  N17§LDTSP        COMP      'H'                                    17
015200951023     C     §LDCDP        COMP      *BLANK                             18
015300951023     C     §LDNRS        COMP      *ZERO                              19
015400951023     C*
015500951023     C** INIZIALIZZA STAMPANTE
015600951023     C                   EXCEPT    LV22
015700951023     C*
015800951023     C                   MOVEA     *BLANK        §RX
015900951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
016000951023     C                   SETOFF                                       77
016100951023     C                   MOVE      *BLANK        §A7               7
016200951023     C                   Z-ADD     §LDNC1        §N7
016300951023     C                   DO        7             §1
016400951023     C     §S7(§1)       IFEQ      *ZERO
016500951023     C                   MOVE      *BLANK        §S7(§1)
016600951023     C                   ELSE
016700951023     C                   SETON                                        77
016800951023     C                   END
016900951023     C  N77              END
017000951023     C                   MOVEA     §S7           §A7
017100951023     C                   MOVE      §A7           §A3               3
017200951023     C                   MOVEA     §A3           §RX(1)
017300951023     C                   MOVEA     '-'           §RX(4)
017400951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
017500951023     C                   SETOFF                                       77
017600951023     C                   MOVE      *BLANK        §A7
017700951023     C                   Z-ADD     §LDNC2        §N7
017800951023     C                   DO        7             §1
017900951023     C     §S7(§1)       IFEQ      *ZERO
018000951023     C                   MOVE      *BLANK        §S7(§1)
018100951023     C                   ELSE
018200951023     C                   SETON                                        77
018300951023     C                   END
018400951023     C  N77              END
018500951023     C                   MOVEA     §S7           §A7
018600951023     C                   MOVE      §A7           §A3
018700951023     C                   MOVEA     §A3           §RX(5)
018800951023     C                   MOVE      §LDCPD        §A3
018900951023     C                   MOVEA     §A3           §RX(9)
019000951023     C* STAMPA LINEA PARTENZA
019100951023     C                   EXCEPT    LV22H
019200951023     C*
019300951023     C                   MOVEA     *BLANK        §RI
019400951023     C                   MOVEA     §LDRSM        §RI(8)
019500951023     C                   MOVEA     ' '           §RI(30)
019600951023     C                   MOVEA     §LDPRM        §RI(31)
019700951023     C                   MOVEA     *BLANK        §RI(33)
019800951023     C                   EXCEPT    LV22D1
019900951023     C* STAMPA DATI MITTENTE
020000951023     C*
020100951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
020200971222     C                   EXSR      COLZER
020300951023     C*
020400971222     C*
020500951023     C* STAMPA NUMERO SEGNACOLLO
020600951023     C                   EXCEPT    LV22D2
020700951023     C*
020800951023     C                   MOVE      §LDTNA        §A3               3
020900951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
021000951023     C                   MOVE      §LDLNA        §A3
021100951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
021200951023     C                   MOVE      §LDZSC        §A2
021300951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
021400951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
021500951023     C                   MOVEL     §LDDLA        §ODDLA            8
021600951023     C* STAMPA LINEA ARRIVO
021700951023     C                   EXCEPT    LV22D3
021800951023     C*
021900951023     C                   MOVEA     *BLANK        §RI
022000951023     C                   MOVEA     §LDRSD        §RI(8)
022100951023     C                   MOVEA     ' '           §RI(30)
022200951023     C                   MOVEA     §LDPRD        §RI(31)
022300951023     C                   MOVEA     *BLANK        §RI(33)
022400951023     C* STAMPA 1° DATI DESTINATARIO
022500951023     C                   EXCEPT    LV22D4
022600951023     C*
022700951023     C                   MOVEA     *BLANK        §RI
022800951023     C                   MOVEA     §LDIND        §RI(8)
022900951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
023000951023     C                   Z-ADD     37            §Y                2 0    88
023100951023     C                   DO        36
023200951023     C                   SUB       1             §Y
023300951023     C     §RI(§Y)       IFNE      ' '
023400951023     C                   SETON                                        88
023500951023     C                   END
023600951023     C  N88              END
023700951023     C     §Y            IFLE      25
023800951023     C                   ADD       3             §Y
023900951023     C                   ELSE
024000951023     C                   MOVEA     '  '          §RI(26)
024100951023     C                   Z-ADD     28            §Y
024200951023     C                   END
024300951023     C                   MOVEA     §LDLOD        §RI(§Y)
024400951023     C* STAMPA 2° DATI DESTINATARIO
024500951023     C                   EXCEPT    LV22D5
024600951023     C* STAMPA SERVIZIO
024700951023     C   16              EXCEPT    LV22X5
024800951023     C*
024900951023     C* STAMPA BARCODE
025000951023     C*
025100951023     C* (CAMPO UNICO BARCODE E COSTANTI)
025200951023     C                   MOVE      *BLANK        §§CDB1           19
025300951023     C                   MOVE      §CDBAR        §§CDB1
025400951023     C                   MOVEL     '"'           §§CDB1
025500951023     C                   MOVE      *BLANK        §§CDB2           21
025600951023     C                   MOVEL     §§CDB1        §§CDB2
025700951023     C                   MOVE      '";'          §§CDB2
025800951023     C                   EXCEPT    LV22D6
025900951023     C*
026000951023     C                   MOVE      *BLANK        §RI21            21
026100951023     C   18              DO
026200951023     C                   MOVEL     'Rif.:'       §RI21
026300980402     C     §DEEUR        IFNE      *BLANK
026400980402     C     §DETST        ANDEQ     'ITA'
026500980402     C                   MOVEL     'JUT :'       §RI21
026600980402     C                   ENDIF
026700951023     C                   MOVE      §LDCDP        §RI21
026800951023     C                   END
026900951023     C* STAMPA RIFERIMENTO/RISTAMPA
027000951023     C                   EXCEPT    LV22D7
027100951023     C*
027200951023     C* STAMPA CHIUSURA ETICHETTA
027300951023     C                   EXCEPT    LV22D8
027400951023     C                   ENDSR
027500971222     C/SPACE 005
027600971222     C*****************************************************
027700971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027800971222     C*****************************************************
027900971222     C     COLZER        BEGSR
028000971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
028100971222     C                   SETOFF                                       77
028200971222     C                   MOVE      §LDNCD        §N7
028300971222     C                   DO        7             §1
028400971222     C     §S7(§1)       IFEQ      *ZERO
028500971222     C                   MOVE      *BLANK        §S7(§1)
028600971222     C                   ELSE
028700971222     C                   SETON                                        77
028800971222     C                   END
028900971222     C  N77              END
029000971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
029100971222     C                   MOVE      *ZERO         §X                1 0
029200971222     C                   MOVEA     *BLANK        §O7
029300971222     C                   DO        7             §1
029400971222     C     §S7(§1)       IFNE      *BLANK
029500971222     C                   ADD       1             §X
029600971222     C     §S7(§1)       IFEQ      '0'
029700971222     C                   MOVE      'O'           §O7(§X)
029800971222     C                   ELSE
029900971222     C                   MOVE      §S7(§1)       §O7(§X)
030000971222     C                   END
030100971222     C                   END
030200971222     C                   END
030300971222     C                   MOVEA     §O7           §S7
030400971222     C                   ENDSR
030500980326     C/SPACE 005
030600980326     C*****************************************************
030700980326     C* STAMPA EUROEXPRESS                              ***
030800980326     C*****************************************************
030900980326     C     EUREXP        BEGSR
031000980326     C*
031100980326     C* IMPOSTO COSTANTI
031200980326     C                   MOVEL     'JUT'         §DEJEU            3
031300980326     C                   MOVEL     '55L'         §DE55L            3
031400980326     C                   MOVEL     'IT'          §DEISP            2
031500011116      * controllo se contenuto 55L nel codice passato
031600011116     c     '55L'         scan      §deeur                                 23
031700011119      * se contenuto 55l scompongo a modo vecchio
031800011119     c                   if        *in23
031900980326     C* ESTRAGGO IL CAP:
032000980326     C                   MOVE      *ZEROS        §Y
032100980326     C                   MOVEA     §DEEUR        §EU
032200980326     C* TROVO QUANTO è LUNGO IL BARCODE
032300980326     C     ' '           CHECKR    §DEEUR        §Y
032400980326     C* ISOLO IL CAP
032500980326     C                   MOVE      *BLANK        §O9
032600980326     C                   MOVE      *ZEROS        §W                2 0
032700980326     C     §Y            SUB       2             §Y
032800980326     C     22            DO        §Y            §X1               2 0
032900980326     C     §EU(§X1)      IFNE      *BLANKS
033000980326     C                   ADD       1             §W
033100980326     C                   MOVE      §EU(§X1)      §O9(§W)
033200980326     C                   END
033300980326     C                   END
033400980326     C                   MOVEA     §O9           §LDCAD
033500011119
033600011119      *scompongo a modo nuovo senza 55l
033700011119     c                   else
033800011119     c                   movel     §deeur        §bceurex
033900011119     c                   end
034000980326     C*
034100980326     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
034200980326     C* DEL BARCODE
034300980326     C                   MOVEL     '";BSTP;'     §END              7
034400980326     C     §LDCAD        CAT       §DEISA:0      §BCD             11
034500980326     C     §BCD          CAT       §END:0        §BCEND           20
034600980326     C*
034700980326     C* VERIFICO SE CONTRASSEGNO
034800980326     C*
034900980326     C     §DEFCA        COMP      'S'                                    14
035000980326     C  N14§DEFCA        COMP      'X'                                    14
035100990323     C*
035200990323     C* VERIFICO SE INGHILTERRA
035300990323     C*
035400990323     C     §DEISA        COMP      'GB'                                   13
035500980326     C*
035600980326     C                   EXCEPT    LV22Y
035700980326     C*
035800980326     C                   ENDSR
035900910403     C/SPACE 005
036000910403     C*****************************************************
036100910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
036200910403     C*****************************************************
036300910403     C     M10           BEGSR
036400910403     C                   MOVE      *ZERO         §TT               3 0
036500910403     C                   MOVE      *ZERO         §TT2              3 0
036600910403     C                   MOVE      *ZERO         §1                1 0
036700910403     C                   Z-ADD     18            §S                2 0
036800910403     C                   DO        17            §N                2 0
036900910403     C                   SUB       1             §S
037000910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
037100910403     C                   MOVE      §C(§N)        §CB(§N)
037200910403     C     §N            DIV       2             §CTR              3 0
037300910403     C                   MVR                     §RST              3 010
037400910403     C     *IN(10)       IFEQ      '1'
037500910403     C     §C(§S)        MULT      2             §N2               2 0
037600910403     C                   MOVE      §N2           §A2               2
037700910403     C                   MOVEA     §A2           §2A
037800910403     C                   XFOOT     §2            §CTR
037900910403     C                   ELSE
038000910403     C                   Z-ADD     §C(§S)        §CTR
038100910403     C                   END
038200910403     C                   ADD       §CTR          §TT
038300910403     C                   END
038400910403     C*
038500910403     C     §TT           ADD       9             §TT2
038600910403     C                   MOVE      0             §TT2
038700910403     C     §TT2          SUB       §TT           §1
038800910403     C*
038900910403     C                   MOVE      §1            §CB(18)
039000910403     C                   ENDSR
039100000118      *--------------------------------------------------
039200000118     c     dpddove       begsr
039300000118      *--------------------------------------------------
039400000118     c                   eval      §dpd = §deeur
039500000124     c                   eval      §dexx1ds = §dexx1
039600000118     c                   eval      §deawbds = §deawb
039700000118     c                   eval      §dpdc1 = §dpdc1w
039800000118     c                   eval      §dpdc3 = §dpdc3w
039900000118     c                   eval      §dpdc5 = §dpdc5w
040000000118     c                   eval      §dpdc7 = §dpdc7w
040100000118     c                   eval      §dpdn = 'Notifications on damages which are -
040200000118     c                             not recognizable from the outside have to -
040300000118     c                             have to be submitted to the transport -
040400000118     c                             organisation withing 7 days in written.'
040500000118     C                   except    LV22dpd1
040600000118     C                   except    LV22dpd
040700000118     C                   endsr
040800000118      *--------------------------------------------------
040900000118     c     dpdchi        begsr
041000000118      *--------------------------------------------------
041100000118     c                   move      §ldmgs        giorno            2 0
041200000118     c                   movel     §ldmgs        mese              2 0
041300000118     c                   move      mese          giomes            4 0
041400000118     c                   movel     giorno        giomes
041500000118     c                   movel     giomes        datasp            8 0
041600000118     c                   move      §ldaas        datasp
041700000118     c                   movel     §dexx2        dpdteln
041800000118     c                   move      §dexx2        dpdfaxn
041900000118     C                   except    LV22dpd2
042000000118     C                   except    LV22dpd
042100000118     C                   endsr
042200000118     c*-------------------------------------------------------------------
042300971215     OFNLV22P   E            LV22             01
042400931008     O******                                 inizializzazione NUOVA
042500951023     O                                           20 '&&??%%P%%7E%^"LV22";'
042600951020     O******                                 inizializzazione VIDEO
042700040429     O**                                          9 '~^"LV22";'
042800941216     O          E            LV22        1
042900941228     O*LOGICA                                N.ET. V (H) O=-35 X TB340
043000941228     O*                                                  O=000 X TX240
043100941216     O*                                                  O=-17 X TUTTI
043200941228     O                                           16 '00001;0;389;-17;'
043300941216     O          E            LV22        1
043400910403     O*FISICA
043500980327     O** X QB400
043600040429     O*                                          21 'UTOF;4100;TEAR;0;SPB;'
043700980327     O** X TX340/240
043800921104     O                                           14 'UTOF;4100;SPB;'
043900941216     O          E            LV22        1
044000910403     O*GIRO SCRITTA A 180°
044100910403     O                                            9 'FLIP;800;'
044200941216     O          E            LV22        1
044300910403     O*                                       FONT
044400910403     O                                            7 'DHR;$1;'
044500941216     O          E            LV22        1
044600910403     O                                           14 'HLT;01;VLT;01;'
044700910403     O********* Mittente
044800941216     O          E            LV22D1      1
044900910403     O*                                       CARATTERE
045000910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
045100941216     O          E            LV22D1      1
045200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
045300910403     O                                           16 'VBR;028;HBR;060;'
045400941216     O          E            LV22D1      1
045500910403     O                                            1 '"'
045600910404     O                       §RI                 38
045700920910     O                                           40 '";'
045800910403     O********* Destinatario (Rag.Soc.)
045900941216     O          E            LV22D4      1
046000910403     O*                         RIGA DI SEPARAZIONE:
046100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
046200941216     O                                           16 'VBR;032;HBR;058;'
046300941216     O          E            LV22D4      1
046400920910     O                                            6 'HLT;3;'
046500941216     O          E            LV22D4      1
046600040429     O*                         RIGA             O            V   L
046700941216     O                                           16 'DHL;117;000;390;'
046800941216     O          E            LV22D4      1
046900910403     O*                                       CARATTERE
047000910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
047100941216     O          E            LV22D4      1
047200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
047300910403     O                                           16 'VBR;048;HBR;060;'
047400941216     O          E            LV22D4      1
047500910403     O                                            1 '"'
047600910404     O                       §RI                 38
047700910404     O                                           40 '";'
047800910403     O********* Destinatario (Località)
047900941216     O          E            LV22D5      1
048000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048100951021     O                                           16 'VBR;065;HBR;060;'
048200941216     O          E            LV22D5      1
048300910403     O                                            1 '"'
048400910404     O                       §RI                 38
048500910404     O                                           40 '";'
048600920221     O********* MC. SPEDIZIONE
048700941216     O          E            LV22D5      1
048800920221     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048900951021     O                                           16 'VBR;079;HBR;465;'
049000941216     O          E            LV22D5      1
049100921104     O                                            1 '"'
049200951021     O                                            4 'MC.'
049300951021     O                       §LDVOL              11 '0 ,   '
049400951021     O                       §LDFVR              14
049500951021     O                                           16 '";'
049600941216     O********* SERVIZIO
049700941216     O          E            LV22X5      1
049800921104     O*                                       CARATTERE
049900921104     O                                           22 'DCH;45;DCW;085;ICS;03;'
050000941216     O          E            LV22X5      1
050100921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050200951021     O                                           16 'VBR;047;HBR;600;'
050300941216     O          E            LV22X5      1
050400941216     O                                            1 '"'
050500941216     O                       §LDTSP               2
050600941216     O                                            4 '";'
050700941216     O          E            LV22X5      1
050800951021     O                                           21 'HOME;VBR;012;HBR;575;'
050900941216     O          E            LV22X5      1
051000040429     O* FONDO NERO                                             L   H
051100941216     O                     17                    16 'DCBX;0;0;100;40;'
051200040429     O* FONDO BIANCO                                           L   H
051300941216     O                    N17                    16 'DBOX;0;0;100;40;'
051400910403     O********* Linea Partenza
051500941216     O          E            LV22H       1
051600910403     O*                                       CARATTERE
051700910403     O                                           23 'DCH;015;DCW;026;ICS;03;'
051800941216     O          E            LV22H       1
051900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052000951021     O                                           16 'VBR;093;HBR;450;'
052100941216     O          E            LV22H       1
052200910403     O                                            1 '"'
052300951021     O                       §RX                 12
052400951021     O                                           14 '";'
052500941216     O          E            LV22H       1
052600910403     O*                                       CARATTERE
052700910403     O                                           23 'DCH;030;DCW;030;ICS;03;'
052800941216     O          E            LV22H       1
052900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053000910403     O                                           16 'VBR;093;HBR;175;'
053100941216     O          E            LV22H       1
053200910403     O                                            1 '"'
053300910403     O                       §LDLNP               4
053400910403     O                                            6 '";'
053500941216     O          E            LV22H       1
053600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053700910403     O                                           16 'VBR;093;HBR;245;'
053800941216     O          E            LV22H       1
053900910403     O                                            1 '"'
054000910403     O                       §LDDLP              11
054100910403     O                                           13 '";'
054200910403     O********* Segnacollo
054300941216     O          E    19      LV22D2      1
054400910403     O*                                       CARATTERE
054500921104     O                                           23 'DCH;080;DCW;050;ICS;08;'
054600941216     O          E    19      LV22D2      1
054700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054800951021     O                                           16 'VBR;180;HBR;080;'
054900941216     O          E    19      LV22D2      1
055000910403     O                                            1 '"'
055100920910     O                       §LDNRT               4
055200920910     O                                            6 '";'
055300941216     O          E            LV22D2      1
055400910403     O*                                       CARATTERE
055500921105     O                                           23 'DCH;110;DCW;105;ICS;12;'
055600941216     O          E            LV22D2      1
055700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055800921105     O                                           16 'VBR;175;HBR;205;'
055900941216     O          E            LV22D2      1
056000910403     O                                            1 '"'
056100910403     O                       §S7                  8
056200910403     O                                           10 '";'
056300921106     O********* TERMINAL/LINEA/ZONA ARRIVO
056400941216     O          E            LV22D3      1
056500921106     O                                           14 'HLT;05;VLT;08;'
056600941216     O          E            LV22D3      1
056700910403     O*                                       CARATTERE
056800921106     O                                           21 'DCH;75;DCW;70;ICS;08;'
056900921106     O********* TERMINAL
057000941216     O          E            LV22D3      1
057100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057200921105     O                                           16 'VBR;240;HBR;060;'
057300941216     O          E            LV22D3      1
057400910403     O                                            1 '"'
057500921106     O                       §ODTNA               4
057600921106     O                                            6 '";'
057700910403     O* BOX X TERMINAL ARRIVO
057800941216     O          E            LV22D3      1
057900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058000921105     O                                           21 'HOME;VBR;185;HBR;046;'
058100941216     O          E            LV22D3      1
058200040429     O* FONDO NERO                                             L   H
058300921106     O                                           16 'DCBX;0;0;147;60;'
058400040429     O* FONDO BIANCO                                           L   H
058500040429     O*                                          16 'DBOX;0;0;147;60;'
058600921106     O********* CODICE LINEA ARRIVO
058700941216     O          E            LV22D3      1
058800921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058900921106     O                                           16 'VBR;240;HBR;207;'
059000941216     O          E            LV22D3      1
059100921106     O                                            1 '"'
059200921106     O                       §ODLNA               4
059300921106     O                                            6 '";'
059400921106     O********* ZONA ARRIVO
059500040429     O          E   N55      LV22D3      1
059600921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059700921106     O                                           16 'VBR;240;HBR;592;'
059800040429     O          E   N55      LV22D3      1
059900921106     O                                            1 '"'
060000921106     O                       §ODZSC               3
060100921106     O                                            5 '";'
060200910403     O* BOX X ZONA ARRIVO
060300040429     O          E   N55      LV22D3      1
060400910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060500921105     O                                           21 'HOME;VBR;185;HBR;582;'
060600040429     O          E   N55      LV22D3      1
060700040429     O* FONDO NERO                                             L   H
060800921106     O                                           16 'DCBX;0;0;102;60;'
060900040429     O* FONDO BIANCO                                           L   H
061000040429     O*                                          16 'DBOX;0;0;102;60;'
061100921106     O********* DESCRIZIONE LINEA DI ARRIVO
061200040429     O          E   N55      LV22D3      1
061300921106     O                                           14 'HLT;05;VLT;06;'
061400040429     O          E   N55      LV22D3      1
061500040429     O*                         CARATTERE
061600040429     O                                              'DCH;75;DCW;45;ICS;04;'
061700921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
061800040429     O                                              'VBR;240;HBR;355;'
061900040429     O          E   N55      LV22D3      1
062000921106     O                                            1 '"'
062100921106     O                       §ODDLA               9
062200921106     O                                           11 '";'
062300040429     O********* Nazione per EEX
062400040429     O          E    55      LV22D3      1
062500040429     O*                         CARATTERE
062600040429     O                                              'DCH;065;DCW;065;ICS;08;'
062700040429     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
062800040429     O                                              'VBR;237;HBR;360;'
062900040429     O          E    55      LV22D3      1
063000040429     O                                              '"'
063100040429     O                       §DEISA
063200040429     O                                              '";'
063300040429     O********* CAP estero per EEX
063400040429     O          E    55      LV22D3      1
063500040429     O                                              'VBR;237;HBR;480;'
063600040429     O                                              '"'
063700040429     O                       §LDCAD
063800040429     O                                              '";'
063900040429     O********* > Box per nazione e cap EEX
064000040429     O          E    55      LV22D3      1
064100040429     O*                         CARATTERE
064200040429     O                                              'DCH;020;DCW;015;ICS;02;'
064300040429     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
064400040429     O                                              'HOME;HBR;000;VBR;040;'
064500040429     O***       E    55      LV22D3      1
064600040429     O* FONDO NERO                                                 L   H
064700040429     O*                                             'DCBX;340;140;355;068;'
064800040429     O* FONDO BIANCO                                               L   H
064900040429     O                                              'DBOX;340;140;355;068;'
065000041122      *---*   costante barra sopra
065100041122     O          E            LV22d6      1
065200041122     O                                           16 'VBR;248;HBR;045;'
065300041122     O          E            LV22d6      1
065400041122     O                                           22 'HLT;4;DHL;045;000;536;'
065500910403     O********* Bar Code
065600941216     O          E            LV22D6      1
065700940530     O                                           16 'VBR;371;HBR;090;'
065800951121     O* BARCODE INTERL. 2/5
065900041122     O**        E            LV22D6      1
066000041122     O**                                         22 'BDEF;8;BNEW;3;BWEW;10;'
066100041122     O**        E            LV22D6      1
066200041122     O**                                         14 'BCSH;121;BCST;'
066300951121     O* BARCODE CODE 128
066400041122     O          E            LV22D6
066500041122     O                                           15 'BDEF;25;BCPI;3;'
066600041122     O          E            LV22D6
066700041122     O                                           18 'BCSH;121;BCST;"C";'
066800941216     O          E            LV22D6      1
066900921104     O                       §§CDB2              21
067000941216     O          E            LV22D6      1
067100910403     O                                            5 'BSTP;'
067200041122      *---*   costante barra sotto
067300041122     O          E            LV22d6      1
067400041122     O                                           16 'VBR;370;HBR;045;'
067500041122     O          E            LV22d6      1
067600041122     O                                           22 'HLT;2;DHL;045;000;536;'
067700951020     O********* Codice Prodotto/KG/RISTAMPA
067800941216     O          E            LV22D7      1
067900910403     O*                                       CARATTERE
068000910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
068100941216     O          E            LV22D7      1
068200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
068300951021     O                                           16 'VBR;387;HBR;060;'
068400941216     O          E            LV22D7      1
068500921104     O                                            1 '"'
068600951020     O                       §RI21               22
068700941216     O                                           27 'KG.'
068800941216     O                       §LDPKG              35 '    0 , '
068900951020     O                       §LDRST              39
069000941216     O                                           41 '";'
069100941216     O********* Chiusura etchetta
069200941216     O          E            LV22D8      1
069300951020     O********                               CON COVERTITORE
069400951023     O                                            9 'TRM;%%5C%'
069500951020     O********                               CON VIDEO
069600040429     O**                                          5 'TRM;\'
069700980327     O*-------------------------------------------------------
069800971215     O* ETICHETTA ESTERO DHL
069900980327     O*-------------------------------------------------------
070000981117     O          E            LV22E            01
070100971215     O******                                 inizializzazione NUOVA
070200981117     O                                              '&&??%%P%%7E%^"LV22";'
070300971215     O******                                 inizializzazione VIDEO
070400040429     O**                                            '~^"LV22";'
070500971215     O          E            LV22E       1
070600971215     O*LOGICA                                N.ET. V (H) O=-35 X TB340
070700971215     O*                                                  O=000 X TX240
070800980327     O*                                                  O=073 X QB440
070900971215     O*                                                  O=-17 X TUTTI
071000981117     O                                              '00001;0;400;-17;'
071100971215     O          E            LV22E       1
071200971215     O*FISICA
071300981117     O                                              'UTOF;4100;SPB;'
071400981117     O          E            LV22E       1
071500971215     O*GIRO SCRITTA A 180°
071600981117     O                                              'FLIP;800;'
071700971215     O          E            LV22E       1
071800971215     O*                                       FONT
071900981117     O                                              'DHR;$1;'
072000971215     O          E            LV22E       1
072100981117     O                                              'HLT;01;VLT;01;'
072200971215      *---*   TIPO PRODOTTO DHL -REX-
072300971215     O          E            LV22E       1
072400971215     O                                              'VBR;040;HBR;260;'
072500971215     O                                              'DCH;20;DCW;15;ICS;04;'
072600971215     O          E            LV22E       1
072700971215     O                                              '"'
072800971215     O                                              'PRODUCT:'
072900971215     O                                              '";'
073000981117     O          E            LV22E       1
073100971215     O                                              'VBR;000;HBR;000;'
073200971215     O                                              'DBOX;380;20;280;68;'
073300981117     O          E            LV22E       1
073400971215     O                                              'VBR;080;HBR;400;'
073500971215     O                                              'DCH;080;DCW;160;ICS;04;'
073600981117     O          E            LV22E       1
073700971215     O                                              '"'
073800971215     O                                              'REX'
073900971215     O                                              '";'
074000971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
074100981117     O          E            LV22E       1
074200971215     O                                              'VBR;140;HBR;080;'
074300971215     O                                              'DCH;20;DCW;15;ICS;04;'
074400981117     O          E            LV22E       1
074500971215     O                                              '"'
074600971215     O                                              'DESTINATION:'
074700971215     O                                              '";'
074800981117     O          E            LV22E       1
074900971215     O                                              'VBR;200;HBR;080;'
075000971215     O                                              'DCH;80;DCW;80;ICS;04;'
075100981117     O          E            LV22E       1
075200971215     O                                              '"'
075300971215     O                       §DESIA
075400971215     O                                              '";'
075500971215     O* BARCODE 39
075600981117     O          E            LV22E       1
075700971215     O                                              'VBR;200;HBR;260;'
075800971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
075900981117     O          E            LV22E       1
076000971215     O                                              'BCSH;100;BCST;"*'
076100971215     O                       §DESIP
076200971215     O                       §DESIA
076300971215     O                       §DESSE
076400971215     O                                              '*";BSTP;'
076500971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
076600981117     O          E            LV22E       1
076700971215     O                                              'VBR;236;HBR;080;'
076800971215     O                                              'DCH;20;DCW;15;ICS;04;'
076900981117     O          E            LV22E       1
077000971215     O                                              '"'
077100971215     O                                              'AWB:'
077200971215     O                                              '";'
077300981117     O          E            LV22E       1
077400971215     O                                              'VBR;236;HBR;260;'
077500971215     O                                              'DCH;40;DCW;50;ICS;04;'
077600971215     O                                              '"'
077700971215     O                       §DEAWB
077800971215     O                                              '";'
077900971215     O* BARCODE 39
078000981117     O          E            LV22E       1
078100971215     O                                              'VBR;348;HBR;110;'
078200971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
078300981117     O          E            LV22E       1
078400971215     O                                              'BCSH;100;BCST;"*'
078500971215     O                       §DEAWB
078600971215     O                                              '*";BSTP;'
078700971215      *---*   NAZIONE DESTINO -CODICE ISO-
078800981117     O          E            LV22E       1
078900971215     O                                              'VBR;389;HBR;080;'
079000971215     O                                              'DCH;50;DCW;60;ICS;04;'
079100981117     O          E            LV22E       1
079200971215     O                                              '"'
079300971215     O                       §DEISA
079400971215     O                                              '";'
079500971215      *---*
079600981117     O          E            LV22E       1
079700971215     O                                              'VBR;369;HBR;220;'
079800971215     O                                              'DCH;20;DCW;20;ICS;04;'
079900981117     O          E            LV22E       1
080000971215     O                                              '"'
080100971215     O                                              'P: '
080200971215     O                       §LDLNP
080300971215     O                                              ' A: '
080400971215     O                       §LDLNA
080500971215     O                                              '";'
080600981117     O          E            LV22E       1
080700971215     O                                              'VBR;389;HBR;220;'
080800971215     O                                              'DCH;20;DCW;20;ICS;04;'
080900981117     O          E            LV22E       1
081000971215     O                                              '"'
081100971215     O                                              'N. '
081200971215     O                       §LDNRT
081300971215     O                       §S7
081400971215     O                                              '";'
081500971215      *---*   SEQUENZA COLLO   (XX OF XX)
081600981117     O          E            LV22E       1
081700971215     O                                              'VBR;369;HBR;480;'
081800971215     O                                              'DCH;20;DCW;25;ICS;04;'
081900981117     O          E            LV22E       1
082000971215     O                                              '"'
082100971215     O                       §LDNC1        Z
082200971215     O                                              ' OF '
082300971215     O                       §LDNC2        Z
082400971215     O                                              '";'
082500971215      *---*   I.A.T.A. ORIGINE
082600981117     O          E            LV22E       1
082700971215     O                                              'VBR;389;HBR;480;'
082800971215     O                                              'DCH;20;DCW;25;ICS;04;'
082900981117     O          E            LV22E       1
083000971215     O                                              '"'
083100971215     O                                              'ORIGIN:  '
083200971215     O                       §DESIP
083300971215     O                                              '";'
083400971215     O********* Chiusura etchetta
083500971215     O          E            LV22E       1
083600971215     O********                               CON COVERTITORE
083700971215     O                                              'TRM;%%5C%'
083800971215     O********                               CON VIDEO
083900040429     O**                                            'TRM;\'
084000980327     O*-------------------------------------------------------
084100980327     O* ETICHETTA ESTERO EUROEXPRESS
084200980327     O*-------------------------------------------------------
084300980327     O          E            LV22Y            01
084400980327     O******                                 inizializzazione NUOVA
084500980327     O                                              '&&??%%P%%7E%^"LV22";'
084600980327     O******                                 inizializzazione VIDEO
084700040429     O**                                            '~^"LV22";'
084800980327     O          E            LV22Y       1
084900980327     O*LOGICA                                N.ET. V (H) O=-35 X TB340
085000980327     O*                                                  O=000 X TX240
085100980327     O*                                                  O=073 X QB440
085200980327     O*                                                  O=-17 X TUTTI
085300980330     O                                              '00001;0;389;000;'
085400980327     O          E            LV22Y       1
085500980327     O*FISICA
085600980327     O                                              'UTOF;4100;SPB;'
085700980327     O          E            LV22Y       1
085800980327     O*GIRO SCRITTA A 180°
085900980327     O                                              'FLIP;800;'
086000980327     O          E            LV22Y       1
086100980327     O*                                       FONT
086200980327     O                                              'DHR;$1;'
086300980327     O          E            LV22Y       1
086400980327     O                                              'HLT;01;VLT;01;'
086500980327      *---*   NAZIONE PARTENZA
086600980327     O          E            LV22Y       1
086700980330     O                                              'HBR;160;VBR;042;'
086800980327     O                                              'DCH;30;DCW;30;ICS;02;'
086900980327     O                                              '"'
087000980327     O                       §DEISP
087100980327     O                                              '";'
087200980327      *---*   CONTRASSEGNO
087300980327     O          E    14      LV22Y       1
087400980330     O                                              'HBR;000;VBR;012;'
087500980327     O                                              'DBOX;490;010;190;048;'
087600980327     O          E    14      LV22Y       1
087700980330     O                                              'HBR;500;VBR;062;'
087800980327     O                                              'DCH;050;DCW;090;ICS;02;'
087900980327     O                                              '"COD";'
088000980327      *---*   EUROEXPRESS
088100980327     O          E            LV22Y       1
088200980330     O                                              'HBR;160;VBR;142;'
088300980327     O                                              'DCH;070;DCW;090;ICS;02;'
088400980327     O                                              '"EuroExpress";'
088500980327      *---*   NAZIONE ARRIVO
088600980327     O          E            LV22Y       1
088700980330     O                                              'HBR;000;VBR;012;'
088800980327     O                                              'DBOX;040;140;200;068;'
088900980327     O          E            LV22Y       1
089000980330     O                                              'HBR;050;VBR;172;'
089100980327     O                                              'DCH;020;DCW;015;ICS;02;'
089200980327     O                                              '"Country";'
089300980327     O          E            LV22Y       1
089400980330     O                                              'HBR;050;VBR;212;'
089500980327     O                                              'DCH;050;DCW;090;ICS;02;'
089600980327     O                                              '"'
089700980327     O                       §DEISA
089800980327     O                                              '";'
089900980327      *---*   CAP DESTINO
090000980327     O          E            LV22Y       1
090100980330     O                                              'HBR;000;VBR;012;'
090200980327     O                                              'DBOX;260;140;400;068;'
090300980327     O          E            LV22Y       1
090400980330     O                                              'HBR;270;VBR;172;'
090500980327     O                                              'DCH;020;DCW;015;ICS;02;'
090600980327     O                                              '"Post Code";'
090700980327     O          E            LV22Y       1
090800980330     O                                              'HBR;270;VBR;212;'
090900980327     O                                              'DCH;050;DCW;090;ICS;02;'
091000980327     O                                              '"'
091100980327     O                       §LDCAD
091200980327     O                                              '";'
091300980327      *---*   BARCODE EUROEXPRESS
091400980330     O          E            LV22Y       1
091500980327     O                                              'BDEF;25;BCSH;100;'
091600980330     O*                                     'BCPI;1;PRHR;1;'
091700011119     O          E   n23      LV22Y       1
091800011119     O                                              'BCPI;2;'
091900011116     O          E   n23      LV22Y       1
092000011116     O                     13                       'HBR;037;VBR;332;'
092100011116     O                    N13                       'HBR;062;VBR;332;'
092200011116     O                                              'BCST;"A'
092300011119     O*                      §DEJEU
092400011119     O*                      §DEISP
092500011119     O                       alfa5
092600011119     O                                              '%%40%D'
092700011119     O*                      §LDLNP
092800011119     O*                      §LDNRS
092900011119     O*                      §LDNCD
093000011119     O                       nume12
093100011119     O                                              '%%40%F'
093200011119     O*                      §deisa
093300011119     o                       alfa2
093400011116     O                       §end
093500011116      *con 55l
093600011119     O          E    23      LV22Y       1
093700011119     O                                              'BCPI;1;'
093800011116     O          E    23      LV22Y       1
093900990323     O                     13                       'HBR;037;VBR;332;'
094000990323     O                    N13                       'HBR;062;VBR;332;'
094100040429     O**                                            'BCST;"C@F'
094200980401     O                                              'BCST;"%%40%F'
094300980327     O                       §DEJEU
094400980327     O                       §DEISP
094500040429     O**                                            '@D'
094600980401     O                                              '%%40%D'
094700980327     O                       §LDLNP
094800980327     O                       §LDNRS
094900980327     O                       §LDNCD
095000040429     O**                                            '@F'
095100980401     O                                              '%%40%F'
095200980330     O                                              ' '
095300980327     O                       §DE55L
095400980327     O                       §BCEND
095500011119     O          E    23      LV22Y       1
095600980401      *---*   DECODIFICA
095700011119     O                                              'HBR;100;VBR;350;'
095800011119     O                                              'DCH;18;DCW;18;ICS;02;'
095900011119     O                                              '"'
096000011119     O                       §DEJEU
096100011119     O                       §DEISP
096200011119     O                       §LDLNP
096300011119     O                       §LDNRS
096400011119     O                       §LDNCD
096500011119     O                                              ' '
096600011119     O                       §DE55L
096700011119     O                       §BCD
096800011119     O                                              '";'
096900011119     O          E   n23      LV22Y       1
097000011119      *---*   DECODIFICA
097100011119     O                                              'HBR;100;VBR;350;'
097200011119     O                                              'DCH;18;DCW;18;ICS;02;'
097300011119     O                                              '"'
097400011119     O                       alfa5
097500011119     O                       nume12
097600011119     O                       alfa2
097700011119     O                                              '";'
097800980327      *---*   RIFERIMENTI BARTOLINI
097900980327     O          E            LV22Y       1
098000980401     O                                              'HBR;050;VBR;380;'
098100980327     O                                              'DCH;020;DCW;020;ICS;02;'
098200980327     O                                              '"'
098300980327     O                       §LDLNP
098400980327     O                                              ' '
098500980327     O                       §LDNRS
098600980327     O                                              ' '
098700980327     O                       §LDNCD
098800980327     O                                              '";'
098900980327     O********* Chiusura etchetta
099000980327     O          E            LV22Y       1
099100980327     O********                               CON COVERTITORE
099200980327     O                                              'TRM;%%5C%'
099300980327     O********                               CON VIDEO
099400040429     O**                                            'TRM;\'
099500000118     o*-----------------------------------------------------------------
099600000118     o*etichetta dpd "dove"
099700000118      *
099800000118     O          E            LV22dpd1         01
099900000118     O                                           20 '&&??%%P%%7E%^"LV22";'
100000000118     O          E            LV22dpd1    1
100100000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
100200000118     O                                           16 '00001;0;389;-17;'
100300000118     O          E            LV22dpd1    1
100400000118     O*FISICA
100500000118     O                                           14 'UTOF;4100;SPB;'
100600000118     O          E            LV22dpd1    1
100700000118     O*GIRO SCRITTA A 180°
100800000118     O                                            9 'FLIP;800;'
100900000118     O          E            LV22dpd1    1
101000000118     O                                            7 'DHR;$1;'
101100000118     O          E            LV22dpd1    1
101200000118     O                                           14 'HLT;01;VLT;01;'
101300000118      *---*   numero  spedizione
101400000118     O          E            LV22dpd1    1
101500000128     O                                           23 'DCH;020;DCW;020;ICS;03;'
101600000118     O          E            LV22dpd1    1
101700000128     O                                           16 'HBR;370;VBR;060;'
101800000118     O          E            LV22dpd1    1
101900000118     O                                            1 '"'
102000000118     O                       §dpd3                4
102100000118     O                                            6 '";'
102200000118      * 8 caratteri numero spedizione
102300000118     O          E            LV22dpd1    1
102400000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
102500000118     O          E            LV22dpd1    1
102600000128     O                                           16 'HBR;420;VBR;060;'
102700000118     O          E            LV22dpd1    1
102800000118     O                                            1 '"'
102900000118     O                       §dpd8                9
103000000118     O                                           11 '";'
103100000118      *ultimo carattere numero spedizione
103200000118     O          E            LV22dpd1    1
103300000128     O                                           23 'DCH;020;DCW;020;ICS;01;'
103400000118     O          E            LV22dpd1    1
103500000118     O                                           16 'HBR;520;VBR;060;'
103600000118     o          E            LV22dpd1    1
103700000118     O                                            1 '"'
103800000118     O                       §dpd12               2
103900000118     O                                            4 '";'
104000000118      *---*   costante scritta
104100000118     O          E            LV22dpd1    1
104200000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
104300000118     O          E            LV22dpd1    1
104400000118     O                                           16 'HBR;240;VBR;080;'
104500000118     O          E            LV22dpd1    1
104600000118     O                                            1 '"'
104700000118     O                       §dpdn1              52
104800000118     O                                           54 '";'
104900000118      *---*   costante scritta 2
105000000118     O          E            LV22dpd1    1
105100000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
105200000118     O          E            LV22dpd1    1
105300000118     O                                           16 'HBR;240;VBR;090;'
105400000118     O          E            LV22dpd1    1
105500000118     O                                            1 '"'
105600000118     O                       §dpdn2              50
105700000118     O                                           52 '";'
105800000118      *---*   costante scritta 3
105900000118     O          E            LV22dpd1    1
106000000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
106100000118     O          E            LV22dpd1    1
106200000118     O                                           16 'HBR;240;VBR;100;'
106300000118     O          E            LV22dpd1    1
106400000118     O                                            1 '"'
106500000118     O                       §dpdn3              54
106600000118     O                                           56 '";'
106700000118      *---*   costante barra sopra
106800000118     O          E            LV22dpd1    1
106900000118     O                                           16 'HBR;130;VBR;180;'
107000000118     O          E            LV22dpd1    1
107100000118     O                                           18 'HLT;2;DHL;0;0;310;'
107200000118      *---*   costante barra sotto
107300000118     O          E            LV22dpd1    1
107400000118     O                                           16 'HBR;130;VBR;280;'
107500000118     O          E            LV22dpd1    1
107600000118     O                                           18 'HLT;2;DHL;0;0;310;'
107700000118     o* secondo codice a barre
107800000118     O          e            lv22dpd1    1
107900000118     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
108000000118     O          e            lv22dpd1    1
108100000118     O                                            9 'BCSH;100;'
108200000118     O          E            LV22dpd1    1
108300000118     O                                           16 'HBR;130;VBR;280;'
108400000118     o          E            LV22dpd1    1
108500000118     O                                            5 'BCST;'
108600000118     O                                            6 '"'
108700000118     O                       §deawb              16
108800000118     O                                           18 '";'
108900000118     O                                           23 'BSTP;'
109000000118      *costante 3 v1/00
109100000118     O          E            LV22dpd1    1
109200000118     O                                           23 'DCH;040;DCW;040;ICS;02;'
109300000118     O          E            LV22dpd1    1
109400000118     O                                           16 'HBR;300;VBR;160;'
109500000118     o          E            LV22dpd1    1
109600000118     O                                            1 '"'
109700000124     O                       §dex1                6
109800000124     O                                            8 '";'
109900000118      *decodifica 2° bcc
110000000118     O          E            LV22dpd1    1
110100000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
110200000118     O          E            LV22dpd1    1
110300000118     O                                           16 'HBR;200;VBR;300;'
110400000118     o          E            LV22dpd1    1
110500000118     O                                            1 '"'
110600000118     O                       §dpdcap             14
110700000118     O                                           16 '";'
110800000118      *costante x
110900000118     O          E            LV22dpd1    1
111000000118     O                                           23 'DCH;050;DCW;030;ICS;02;'
111100000118     O          E            LV22dpd1    1
111200000118     O                                           16 'HBR;070;VBR;320;'
111300000118     o          E            LV22dpd1    1
111400000118     O                                            1 '"'
111500000118     O                       §desse               2
111600000118     O                                            4 '";'
111700000118      *costante 09
111800000118     O          E            LV22dpd1    1
111900000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
112000000118     O          E            LV22dpd1    1
112100000118     O                                           16 'HBR;070;VBR;380;'
112200000118     o          E            LV22dpd1    1
112300000118     O                                            1 '"'
112400000118     O                       dpd9                 3
112500000118     O                                            5 '";'
112600000118      *linea di arrivo
112700000118     O          E            LV22dpd1    1
112800000118     O                                           23 'DCH;080;DCW;040;ICS;02;'
112900000118     O          E            LV22dpd1    1
113000000118     O                                           16 'HBR;250;VBR;380;'
113100000118     o          E            LV22dpd1    1
113200000118     O                                            1 '"'
113300000124     O                       §dex3                4
113400000118     O                                            6 '";'
113500000118      *zona arrivo
113600000118     O          E            LV22dpd1    1
113700000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
113800000118     O          E            LV22dpd1    1
113900000118     O                                           16 'HBR;350;VBR;380;'
114000000118     o          E            LV22dpd1    1
114100000118     O                                            1 '"'
114200000124     O                       §dex4                3
114300000118     O                                            5 '";'
114400000118     O*------------------------------------------------------------------
114500000118     O          E            LV22dpd2         01
114600000118     O                                           20 '&&??%%P%%7E%^"LV22";'
114700000118     O          E            LV22dpd2    1
114800000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
114900000118     O                                           16 '00001;0;389;-17;'
115000000118     O          E            LV22dpd2    1
115100000118     O*FISICA
115200000118     O                                           14 'UTOF;4100;SPB;'
115300000118     O          E            LV22dpd2    1
115400000118     O*GIRO SCRITTA A 180°
115500000118     O                                            9 'FLIP;800;'
115600000118     O          E            LV22dpd2    1
115700000118     O                                            7 'DHR;$1;'
115800000118     O          E            LV22dpd2    1
115900000118     O                                           14 'HLT;01;VLT;01;'
116000000118      *---*   numero  spedizione
116100000118     O          E            LV22dpd2    1
116200000118     O                                           23 'DCH;050;DCW;030;ICS;03;'
116300000118     O          E            LV22dpd2    1
116400000118     O                                           16 'HBR;330;VBR;060;'
116500000118     O          E            LV22dpd2    1
116600000118     O                                            1 '"'
116700000118     O                       §dpd3                4
116800000118     O                                            6 '";'
116900000118      * 8 caratteri numero spedizione
117000000118     O          E            LV22dpd2    1
117100000118     O                                           23 'DCH;040;DCW;025;ICS;02;'
117200000118     O          E            LV22dpd2    1
117300000118     O                                           16 'HBR;400;VBR;060;'
117400000118     O          E            LV22dpd2    1
117500000118     O                                            1 '"'
117600000118     O                       §dpd8                9
117700000118     O                                           11 '";'
117800000118      *ultimo carattere numero spedizione
117900000118     O          E            LV22dpd2    1
118000000118     O                                           23 'DCH;035;DCW;020;ICS;01;'
118100000118     O          E            LV22dpd2    1
118200000118     O                                           16 'HBR;520;VBR;060;'
118300000118     o          E            LV22dpd2    1
118400000118     O                                            1 '"'
118500000118     O                       §dpd12               2
118600000118     O                                            4 '";'
118700000118      *---*   costante TELEFONO
118800000118     O          E            LV22dpd2    1
118900000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
119000000118     O          E            LV22dpd2    1
119100000118     O                                           16 'HBR;250;VBR;080;'
119200000118     O          E            LV22dpd2    1
119300000118     O                                            1 '"'
119400000118     O                       dpd2                20
119500000118     O                                           22 '";'
119600000118      *---*   costante fax
119700000118     O          E            LV22dpd2    1
119800000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
119900000118     O          E            LV22dpd2    1
120000000118     O                                           16 'HBR;250;VBR;090;'
120100000118     O          E            LV22dpd2    1
120200000118     O                                            1 '"'
120300000118     O                       dpd2f               20
120400000118     O                                           22 '";'
120500000118      *costante mittente
120600000118     O          E            LV22dpd2    1
120700000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
120800000118     O          E            LV22dpd2    1
120900000118     O                                           25 'HBR;160;VBR;100;'
121000000118     o          E            LV22dpd2    1
121100000118     O                                            1 '"'
121200000118     O                       dpd3                 8
121300000118     O                                           10 '";'
121400000118      *costante mittente2
121500000118     O          E            LV22dpd2    1
121600000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
121700000118     O          E            LV22dpd2    1
121800000118     O                                           25 'HBR;160;VBR;120;'
121900000118     o          E            LV22dpd2    1
122000000118     O                                            1 '"'
122100000118     O                       §ldrsm              36
122200000118     O                                           38 '";'
122300000118      *costante mittente 3
122400000118     O          E            LV22dpd2    1
122500000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
122600000118     O          E            LV22dpd2    1
122700000118     O                                           25 'HBR;160;VBR;140;'
122800000118     o          E            LV22dpd2    1
122900000118     O                                            1 '"'
123000000118     O                       §ldprm               3
123100000118     O                                            5 '";'
123200000118      *costante destinatario
123300000118     O          E            LV22dpd2    1
123400000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
123500000118     O          E            LV22dpd2    1
123600000118     O                                           25 'HBR;070;VBR;150;'
123700000118     o          E            LV22dpd2    1
123800000118     O                                            1 '"'
123900000118     O                       dpd4                11
124000000118     O                                           13 '";'
124100000118      *destinatario
124200000118     O          E            LV22dpd2    1
124300000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
124400000118     O          E            LV22dpd2    1
124500000118     O                                           20 'HBR;070;VBR;170;'
124600000118     o          E            LV22dpd2    1
124700000118     O                                            1 '"'
124800000118     O                       §ldrsd              36
124900000118     O                                           38 '";'
125000000118      * destinatario indirizzo
125100000118     O          E            LV22dpd2    1
125200000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
125300000118     O          E            LV22dpd2    1
125400000118     O                                           20 'HBR;070;VBR;190;'
125500000118     o          E            LV22dpd2    1
125600000118     O                                            1 '"'
125700000118     O                       §ldind              36
125800000118     O                                           38 '";'
125900000118      *località destinatario 7
126000000118     O          E            LV22dpd2    1
126100000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
126200000118     O          E            LV22dpd2    1
126300000118     O                                           20 'HBR;070;VBR;210;'
126400000118     o          E            LV22dpd2    1
126500000118     O                                            1 '"'
126600000118     O                       §ldlod              36
126700000118     O                                           38 '";'
126800000118      *cap      destinatario 8
126900000118     O          E            LV22dpd2    1
127000000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
127100000118     O          E            LV22dpd2    1
127200000118     O                                           20 'HBR;070;VBR;240;'
127300000118     o          E            LV22dpd2    1
127400000118     O                                            1 '"'
127500000118     O                       §ldcad              10
127600000118     O                                           12 '";'
127700000118      *cap      destinatario 8
127800000118     O          E            LV22dpd2    1
127900000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
128000000118     O          E            LV22dpd2    1
128100000118     O                                           20 'HBR;250;VBR;240;'
128200000118     o          E            LV22dpd2    1
128300000118     O                                            1 '"'
128400000118     O                       §desia               4
128500000118     O                                            6 '";'
128600000118      *---*   costante barra sopra
128700000118     O          E            LV22dpd2    1
128800000118     O                                           16 'HBR;130;VBR;250;'
128900000118     O          E            LV22dpd2    1
129000000118     O                                           18 'HLT;2;DHL;0;0;370;'
129100000118      *---*   costante barra sotto
129200000118     O          E            LV22dpd2    1
129300000118     O                                           16 'HBR;130;VBR;350;'
129400000118     O          E            LV22dpd2    1
129500000118     O                                           18 'HLT;2;DHL;0;0;370;'
129600000118      *costante primo B-C
129700000118     O          e            lv22dpd2    1
129800000118     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
129900000118     O          e            lv22dpd2    1
130000000118     O                                            9 'BCSH;100;'
130100000118     O          E            LV22dpd2    1
130200000118     O                                           16 'HBR;130;VBR;350;'
130300000118     o          E            LV22dpd2    1
130400000118     O                                            5 'BCST;'
130500000118     O                                            6 '"'
130600000118     O                       §dpd                18
130700000118     O                                           20 '";'
130800000118     O                                           25 'BSTP;'
130900000118      *costante data
131000000118     O          E            LV22dpd2    1
131100000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
131200000118     O          E            LV22dpd2    1
131300000118     O                                           16 'HBR;070;VBR;385;'
131400000118     o          E            LV22dpd2    1
131500000118     O                                            1 '"'
131600000118     O                       datasp              11 '  /  /    '
131700000118     O                                           13 '";'
131800000118      *costante 1 collo
131900000118     O          E            LV22dpd2    1
132000000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
132100000118     O          E            LV22dpd2    1
132200000118     O                                           16 'HBR;200;VBR;385;'
132300000118     o          E            LV22dpd2    1
132400000118     O                                            1 '"'
132500000118     O                                            8 '1 Collo'
132600000118     O                                           10 '";'
132700000118      *costante peso
132800000118     O          E            LV22dpd2    1
132900000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
133000000118     O          E            LV22dpd2    1
133100000118     O                                           16 'HBR;300;VBR;385;'
133200000118     o          E            LV22dpd2    1
133300000118     O                                            1 '"'
133400000118     O                                            8 'Peso Kg'
133500000118     O                                           10 '";'
133600000118      * peso
133700000118     O          E            LV22dpd2    1
133800000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
133900000118     O          E            LV22dpd2    1
134000000118     O                                           16 'HBR;400;VBR;385;'
134100000118     o          E            LV22dpd2    1
134200000118     O                                            1 '"'
134300000118     O                       §ldpkg        2     10
134400000118     O                                           12 '";'
134500000118      *------------------------------------------------------------------
134600000118      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
134700000118     O* costante dpd
134800000118     O          E            LV22dpd     1
134900000118     O                                           23 'DCH;030;DCW;070;ICS;03;'
135000000118     O          E            LV22dpd     1
135100000118     O                                           25 'HBR;170;VBR;110;DHR;$201;'
135200000118     o          E            LV22dpd     1
135300000118     O                                            1 '"'
135400000118     O                       dpd                  4
135500000118     O                                            6 '";'
135600000118     O* costante dpd riga sotto
135700000118     O          E            LV22dpd     1
135800000118     O                                           23 'DCH;006;DCW;005;ICS;01;'
135900000118     O          E            LV22dpd     1
136000000118     O                                           20 'HBR;210;VBR;095;'
136100000118     o          E            LV22dpd     1
136200000118     O                                            1 '"'
136300000118     O                       dpd1                23
136400000118     O                                           25 '";'
136500000118     o          E            LV22dpd     1
136600000118     O                                            9 'TRM;%%5C%'
