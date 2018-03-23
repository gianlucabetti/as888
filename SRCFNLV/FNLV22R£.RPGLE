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
015100951023     C     §LDCDP        COMP      *BLANK                             18
015200951023     C     §LDNRS        COMP      *ZERO                              19
015300951023     C*
015400951023     C** INIZIALIZZA STAMPANTE
015500951023     C                   EXCEPT    LV22
015600951023     C*
015700951023     C                   MOVEA     *BLANK        §RX
015800951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
015900951023     C                   SETOFF                                       77
016000951023     C                   MOVE      *BLANK        §A7               7
016100951023     C                   Z-ADD     §LDNC1        §N7
016200951023     C                   DO        7             §1
016300951023     C     §S7(§1)       IFEQ      *ZERO
016400951023     C                   MOVE      *BLANK        §S7(§1)
016500951023     C                   ELSE
016600951023     C                   SETON                                        77
016700951023     C                   END
016800951023     C  N77              END
016900951023     C                   MOVEA     §S7           §A7
017000951023     C                   MOVE      §A7           §A3               3
017100951023     C                   MOVEA     §A3           §RX(1)
017200951023     C                   MOVEA     '-'           §RX(4)
017300951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
017400951023     C                   SETOFF                                       77
017500951023     C                   MOVE      *BLANK        §A7
017600951023     C                   Z-ADD     §LDNC2        §N7
017700951023     C                   DO        7             §1
017800951023     C     §S7(§1)       IFEQ      *ZERO
017900951023     C                   MOVE      *BLANK        §S7(§1)
018000951023     C                   ELSE
018100951023     C                   SETON                                        77
018200951023     C                   END
018300951023     C  N77              END
018400951023     C                   MOVEA     §S7           §A7
018500951023     C                   MOVE      §A7           §A3
018600951023     C                   MOVEA     §A3           §RX(5)
018700951023     C                   MOVE      §LDCPD        §A3
018800951023     C                   MOVEA     §A3           §RX(9)
018900951023     C* STAMPA LINEA PARTENZA
019000951023     C                   EXCEPT    LV22H
019100951023     C*
019200951023     C                   MOVEA     *BLANK        §RI
019300951023     C                   MOVEA     §LDRSM        §RI(8)
019400951023     C                   MOVEA     ' '           §RI(30)
019500951023     C                   MOVEA     §LDPRM        §RI(31)
019600951023     C                   MOVEA     *BLANK        §RI(33)
019700951023     C                   EXCEPT    LV22D1
019800951023     C* STAMPA DATI MITTENTE
019900951023     C*
020000951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
020100971222     C                   EXSR      COLZER
020200951023     C*
020300971222     C*
020400951023     C* STAMPA NUMERO SEGNACOLLO
020500951023     C                   EXCEPT    LV22D2
020600951023     C*
020700951023     C                   MOVE      §LDTNA        §A3               3
020800951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
020900951023     C                   MOVE      §LDLNA        §A3
021000951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
021100951023     C                   MOVE      §LDZSC        §A2
021200951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
021300951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
021400951023     C                   MOVEL     §LDDLA        §ODDLA            8
021500951023     C* STAMPA LINEA ARRIVO
021600951023     C                   EXCEPT    LV22D3
021700951023     C*
021800951023     C                   MOVEA     *BLANK        §RI
021900951023     C                   MOVEA     §LDRSD        §RI(8)
022000951023     C                   MOVEA     ' '           §RI(30)
022100951023     C                   MOVEA     §LDPRD        §RI(31)
022200951023     C                   MOVEA     *BLANK        §RI(33)
022300951023     C* STAMPA 1° DATI DESTINATARIO
022400951023     C                   EXCEPT    LV22D4
022500951023     C*
022600951023     C                   MOVEA     *BLANK        §RI
022700951023     C                   MOVEA     §LDIND        §RI(8)
022800951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
022900951023     C                   Z-ADD     37            §Y                2 0    88
023000951023     C                   DO        36
023100951023     C                   SUB       1             §Y
023200951023     C     §RI(§Y)       IFNE      ' '
023300951023     C                   SETON                                        88
023400951023     C                   END
023500951023     C  N88              END
023600951023     C     §Y            IFLE      25
023700951023     C                   ADD       3             §Y
023800951023     C                   ELSE
023900951023     C                   MOVEA     '  '          §RI(26)
024000951023     C                   Z-ADD     28            §Y
024100951023     C                   END
024200951023     C                   MOVEA     §LDLOD        §RI(§Y)
024300951023     C* STAMPA 2° DATI DESTINATARIO
024400951023     C                   EXCEPT    LV22D5
024500951023     C* STAMPA SERVIZIO
024600951023     C   16              EXCEPT    LV22X5
024700951023     C*
024800951023     C* STAMPA BARCODE
024900951023     C*
025000951023     C* (CAMPO UNICO BARCODE E COSTANTI)
025100951023     C                   MOVE      *BLANK        §§CDB1           19
025200951023     C                   MOVE      §CDBAR        §§CDB1
025300951023     C                   MOVEL     '"'           §§CDB1
025400951023     C                   MOVE      *BLANK        §§CDB2           21
025500951023     C                   MOVEL     §§CDB1        §§CDB2
025600951023     C                   MOVE      '";'          §§CDB2
025700951023     C                   EXCEPT    LV22D6
025800951023     C*
025900951023     C                   MOVE      *BLANK        §RI21            21
026000951023     C   18              DO
026100951023     C                   MOVEL     'Rif.:'       §RI21
026200980402     C     §DEEUR        IFNE      *BLANK
026300980402     C     §DETST        ANDEQ     'ITA'
026400980402     C                   MOVEL     'JUT :'       §RI21
026500980402     C                   ENDIF
026600951023     C                   MOVE      §LDCDP        §RI21
026700951023     C                   END
026800951023     C* STAMPA RIFERIMENTO/RISTAMPA
026900951023     C                   EXCEPT    LV22D7
027000951023     C*
027100951023     C* STAMPA CHIUSURA ETICHETTA
027200951023     C                   EXCEPT    LV22D8
027300951023     C                   ENDSR
027400971222     C/SPACE 005
027500971222     C*****************************************************
027600971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027700971222     C*****************************************************
027800971222     C     COLZER        BEGSR
027900971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
028000971222     C                   SETOFF                                       77
028100971222     C                   MOVE      §LDNCD        §N7
028200971222     C                   DO        7             §1
028300971222     C     §S7(§1)       IFEQ      *ZERO
028400971222     C                   MOVE      *BLANK        §S7(§1)
028500971222     C                   ELSE
028600971222     C                   SETON                                        77
028700971222     C                   END
028800971222     C  N77              END
028900971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
029000971222     C                   MOVE      *ZERO         §X                1 0
029100971222     C                   MOVEA     *BLANK        §O7
029200971222     C                   DO        7             §1
029300971222     C     §S7(§1)       IFNE      *BLANK
029400971222     C                   ADD       1             §X
029500971222     C     §S7(§1)       IFEQ      '0'
029600971222     C                   MOVE      'O'           §O7(§X)
029700971222     C                   ELSE
029800971222     C                   MOVE      §S7(§1)       §O7(§X)
029900971222     C                   END
030000971222     C                   END
030100971222     C                   END
030200971222     C                   MOVEA     §O7           §S7
030300971222     C                   ENDSR
030400980326     C/SPACE 005
030500980326     C*****************************************************
030600980326     C* STAMPA EUROEXPRESS                              ***
030700980326     C*****************************************************
030800980326     C     EUREXP        BEGSR
030900980326     C*
031000980326     C* IMPOSTO COSTANTI
031100980326     C                   MOVEL     'JUT'         §DEJEU            3
031200980326     C                   MOVEL     '55L'         §DE55L            3
031300980326     C                   MOVEL     'IT'          §DEISP            2
031400011116      * controllo se contenuto 55L nel codice passato
031500011116     c     '55L'         scan      §deeur                                 23
031600011119      * se contenuto 55l scompongo a modo vecchio
031700011119     c                   if        *in23
031800980326     C* ESTRAGGO IL CAP:
031900980326     C                   MOVE      *ZEROS        §Y
032000980326     C                   MOVEA     §DEEUR        §EU
032100980326     C* TROVO QUANTO è LUNGO IL BARCODE
032200980326     C     ' '           CHECKR    §DEEUR        §Y
032300980326     C* ISOLO IL CAP
032400980326     C                   MOVE      *BLANK        §O9
032500980326     C                   MOVE      *ZEROS        §W                2 0
032600980326     C     §Y            SUB       2             §Y
032700980326     C     22            DO        §Y            §X1               2 0
032800980326     C     §EU(§X1)      IFNE      *BLANKS
032900980326     C                   ADD       1             §W
033000980326     C                   MOVE      §EU(§X1)      §O9(§W)
033100980326     C                   END
033200980326     C                   END
033300980326     C                   MOVEA     §O9           §LDCAD
033400011119
033500011119      *scompongo a modo nuovo senza 55l
033600011119     c                   else
033700011119     c                   movel     §deeur        §bceurex
033800011119     c                   end
033900980326     C*
034000980326     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
034100980326     C* DEL BARCODE
034200980326     C                   MOVEL     '";BSTP;'     §END              7
034300980326     C     §LDCAD        CAT       §DEISA:0      §BCD             11
034400980326     C     §BCD          CAT       §END:0        §BCEND           20
034500980326     C*
034600980326     C* VERIFICO SE CONTRASSEGNO
034700980326     C*
034800980326     C     §DEFCA        COMP      'S'                                    14
034900980326     C  N14§DEFCA        COMP      'X'                                    14
035000990323     C*
035100990323     C* VERIFICO SE INGHILTERRA
035200990323     C*
035300990323     C     §DEISA        COMP      'GB'                                   13
035400980326     C*
035500980326     C                   EXCEPT    LV22Y
035600980326     C*
035700980326     C                   ENDSR
035800910403     C/SPACE 005
035900910403     C*****************************************************
036000910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
036100910403     C*****************************************************
036200910403     C     M10           BEGSR
036300910403     C                   MOVE      *ZERO         §TT               3 0
036400910403     C                   MOVE      *ZERO         §TT2              3 0
036500910403     C                   MOVE      *ZERO         §1                1 0
036600910403     C                   Z-ADD     18            §S                2 0
036700910403     C                   DO        17            §N                2 0
036800910403     C                   SUB       1             §S
036900910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
037000910403     C                   MOVE      §C(§N)        §CB(§N)
037100910403     C     §N            DIV       2             §CTR              3 0
037200910403     C                   MVR                     §RST              3 010
037300910403     C     *IN(10)       IFEQ      '1'
037400910403     C     §C(§S)        MULT      2             §N2               2 0
037500910403     C                   MOVE      §N2           §A2               2
037600910403     C                   MOVEA     §A2           §2A
037700910403     C                   XFOOT     §2            §CTR
037800910403     C                   ELSE
037900910403     C                   Z-ADD     §C(§S)        §CTR
038000910403     C                   END
038100910403     C                   ADD       §CTR          §TT
038200910403     C                   END
038300910403     C*
038400910403     C     §TT           ADD       9             §TT2
038500910403     C                   MOVE      0             §TT2
038600910403     C     §TT2          SUB       §TT           §1
038700910403     C*
038800910403     C                   MOVE      §1            §CB(18)
038900910403     C                   ENDSR
039000000118      *--------------------------------------------------
039100000118     c     dpddove       begsr
039200000118      *--------------------------------------------------
039300000118     c                   eval      §dpd = §deeur
039400000124     c                   eval      §dexx1ds = §dexx1
039500000118     c                   eval      §deawbds = §deawb
039600000118     c                   eval      §dpdc1 = §dpdc1w
039700000118     c                   eval      §dpdc3 = §dpdc3w
039800000118     c                   eval      §dpdc5 = §dpdc5w
039900000118     c                   eval      §dpdc7 = §dpdc7w
040000000118     c                   eval      §dpdn = 'Notifications on damages which are -
040100000118     c                             not recognizable from the outside have to -
040200000118     c                             have to be submitted to the transport -
040300000118     c                             organisation withing 7 days in written.'
040400000118     C                   except    LV22dpd1
040500000118     C                   except    LV22dpd
040600000118     C                   endsr
040700000118      *--------------------------------------------------
040800000118     c     dpdchi        begsr
040900000118      *--------------------------------------------------
041000000118     c                   move      §ldmgs        giorno            2 0
041100000118     c                   movel     §ldmgs        mese              2 0
041200000118     c                   move      mese          giomes            4 0
041300000118     c                   movel     giorno        giomes
041400000118     c                   movel     giomes        datasp            8 0
041500000118     c                   move      §ldaas        datasp
041600000118     c                   movel     §dexx2        dpdteln
041700000118     c                   move      §dexx2        dpdfaxn
041800000118     C                   except    LV22dpd2
041900000118     C                   except    LV22dpd
042000000118     C                   endsr
042100000118     c*-------------------------------------------------------------------
042200971215     OFNLV22P   E            LV22             01
042300931008     O******                                 inizializzazione NUOVA
042400951023     O                                           20 '&&??%%P%%7E%^"LV22";'
042500951020     O******                                 inizializzazione VIDEO
042600040429     O**                                          9 '~^"LV22";'
042700941216     O          E            LV22        1
042800941228     O*LOGICA                                N.ET. V (H) O=-35 X TB340
042900941228     O*                                                  O=000 X TX240
043000941216     O*                                                  O=-17 X TUTTI
043100941228     O                                           16 '00001;0;389;-17;'
043200941216     O          E            LV22        1
043300910403     O*FISICA
043400980327     O** X QB400
043500040429     O*                                          21 'UTOF;4100;TEAR;0;SPB;'
043600980327     O** X TX340/240
043700921104     O                                           14 'UTOF;4100;SPB;'
043800941216     O          E            LV22        1
043900910403     O*GIRO SCRITTA A 180°
044000910403     O                                            9 'FLIP;800;'
044100941216     O          E            LV22        1
044200910403     O*                                       FONT
044300910403     O                                            7 'DHR;$1;'
044400941216     O          E            LV22        1
044500910403     O                                           14 'HLT;01;VLT;01;'
044600910403     O********* Mittente
044700941216     O          E            LV22D1      1
044800910403     O*                                       CARATTERE
044900910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
045000941216     O          E            LV22D1      1
045100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
045200910403     O                                           16 'VBR;028;HBR;060;'
045300941216     O          E            LV22D1      1
045400910403     O                                            1 '"'
045500910404     O                       §RI                 38
045600920910     O                                           40 '";'
045700910403     O********* Destinatario (Rag.Soc.)
045800941216     O          E            LV22D4      1
045900910403     O*                         RIGA DI SEPARAZIONE:
046000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
046100941216     O                                           16 'VBR;032;HBR;058;'
046200941216     O          E            LV22D4      1
046300920910     O                                            6 'HLT;3;'
046400941216     O          E            LV22D4      1
046500040429     O*                         RIGA             O            V   L
046600941216     O                                           16 'DHL;117;000;390;'
046700941216     O          E            LV22D4      1
046800910403     O*                                       CARATTERE
046900910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
047000941216     O          E            LV22D4      1
047100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
047200910403     O                                           16 'VBR;048;HBR;060;'
047300941216     O          E            LV22D4      1
047400910403     O                                            1 '"'
047500910404     O                       §RI                 38
047600910404     O                                           40 '";'
047700910403     O********* Destinatario (Località)
047800941216     O          E            LV22D5      1
047900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048000951021     O                                           16 'VBR;065;HBR;060;'
048100941216     O          E            LV22D5      1
048200910403     O                                            1 '"'
048300910404     O                       §RI                 38
048400910404     O                                           40 '";'
048500920221     O********* MC. SPEDIZIONE
048600941216     O          E            LV22D5      1
048700920221     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048800951021     O                                           16 'VBR;079;HBR;465;'
048900941216     O          E            LV22D5      1
049000921104     O                                            1 '"'
049100951021     O                                            4 'MC.'
049200951021     O                       §LDVOL              11 '0 ,   '
049300951021     O                       §LDFVR              14
049400951021     O                                           16 '";'
049500941216     O********* SERVIZIO
049600941216     O          E            LV22X5      1
049700921104     O*                                       CARATTERE
049800921104     O                                           22 'DCH;45;DCW;085;ICS;03;'
049900941216     O          E            LV22X5      1
050000921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050100951021     O                                           16 'VBR;047;HBR;600;'
050200941216     O          E            LV22X5      1
050300941216     O                                            1 '"'
050400941216     O                       §LDTSP               2
050500941216     O                                            4 '";'
050600941216     O          E            LV22X5      1
050700951021     O                                           21 'HOME;VBR;012;HBR;575;'
050800941216     O          E            LV22X5      1
050900040429     O* FONDO NERO                                             L   H
051000941216     O                     17                    16 'DCBX;0;0;100;40;'
051100040429     O* FONDO BIANCO                                           L   H
051200941216     O                    N17                    16 'DBOX;0;0;100;40;'
051300910403     O********* Linea Partenza
051400941216     O          E            LV22H       1
051500910403     O*                                       CARATTERE
051600910403     O                                           23 'DCH;015;DCW;026;ICS;03;'
051700941216     O          E            LV22H       1
051800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
051900951021     O                                           16 'VBR;093;HBR;450;'
052000941216     O          E            LV22H       1
052100910403     O                                            1 '"'
052200951021     O                       §RX                 12
052300951021     O                                           14 '";'
052400941216     O          E            LV22H       1
052500910403     O*                                       CARATTERE
052600910403     O                                           23 'DCH;030;DCW;030;ICS;03;'
052700941216     O          E            LV22H       1
052800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052900910403     O                                           16 'VBR;093;HBR;175;'
053000941216     O          E            LV22H       1
053100910403     O                                            1 '"'
053200910403     O                       §LDLNP               4
053300910403     O                                            6 '";'
053400941216     O          E            LV22H       1
053500910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053600910403     O                                           16 'VBR;093;HBR;245;'
053700941216     O          E            LV22H       1
053800910403     O                                            1 '"'
053900910403     O                       §LDDLP              11
054000910403     O                                           13 '";'
054100910403     O********* Segnacollo
054200941216     O          E    19      LV22D2      1
054300910403     O*                                       CARATTERE
054400921104     O                                           23 'DCH;080;DCW;050;ICS;08;'
054500941216     O          E    19      LV22D2      1
054600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054700951021     O                                           16 'VBR;180;HBR;080;'
054800941216     O          E    19      LV22D2      1
054900910403     O                                            1 '"'
055000920910     O                       §LDNRT               4
055100920910     O                                            6 '";'
055200941216     O          E            LV22D2      1
055300910403     O*                                       CARATTERE
055400921105     O                                           23 'DCH;110;DCW;105;ICS;12;'
055500941216     O          E            LV22D2      1
055600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055700921105     O                                           16 'VBR;175;HBR;205;'
055800941216     O          E            LV22D2      1
055900910403     O                                            1 '"'
056000910403     O                       §S7                  8
056100910403     O                                           10 '";'
056200921106     O********* TERMINAL/LINEA/ZONA ARRIVO
056300941216     O          E            LV22D3      1
056400921106     O                                           14 'HLT;05;VLT;08;'
056500941216     O          E            LV22D3      1
056600910403     O*                                       CARATTERE
056700921106     O                                           21 'DCH;75;DCW;70;ICS;08;'
056800921106     O********* TERMINAL
056900941216     O          E            LV22D3      1
057000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057100921105     O                                           16 'VBR;240;HBR;060;'
057200941216     O          E            LV22D3      1
057300910403     O                                            1 '"'
057400921106     O                       §ODTNA               4
057500921106     O                                            6 '";'
057600910403     O* BOX X TERMINAL ARRIVO
057700941216     O          E            LV22D3      1
057800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057900921105     O                                           21 'HOME;VBR;185;HBR;046;'
058000941216     O          E            LV22D3      1
058100040429     O* FONDO NERO                                             L   H
058200921106     O                                           16 'DCBX;0;0;147;60;'
058300040429     O* FONDO BIANCO                                           L   H
058400040429     O*                                          16 'DBOX;0;0;147;60;'
058500921106     O********* CODICE LINEA ARRIVO
058600941216     O          E            LV22D3      1
058700921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058800921106     O                                           16 'VBR;240;HBR;207;'
058900941216     O          E            LV22D3      1
059000921106     O                                            1 '"'
059100921106     O                       §ODLNA               4
059200921106     O                                            6 '";'
059300921106     O********* ZONA ARRIVO
059400040429     O          E   N55      LV22D3      1
059500921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059600921106     O                                           16 'VBR;240;HBR;592;'
059700040429     O          E   N55      LV22D3      1
059800921106     O                                            1 '"'
059900921106     O                       §ODZSC               3
060000921106     O                                            5 '";'
060100910403     O* BOX X ZONA ARRIVO
060200040429     O          E   N55      LV22D3      1
060300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060400921105     O                                           21 'HOME;VBR;185;HBR;582;'
060500040429     O          E   N55      LV22D3      1
060600040429     O* FONDO NERO                                             L   H
060700921106     O                                           16 'DCBX;0;0;102;60;'
060800040429     O* FONDO BIANCO                                           L   H
060900040429     O*                                          16 'DBOX;0;0;102;60;'
061000921106     O********* DESCRIZIONE LINEA DI ARRIVO
061100040429     O          E   N55      LV22D3      1
061200921106     O                                           14 'HLT;05;VLT;06;'
061300040429     O          E   N55      LV22D3      1
061400040429     O*                         CARATTERE
061500040429     O                                              'DCH;75;DCW;45;ICS;04;'
061600921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
061700040429     O                                              'VBR;240;HBR;355;'
061800040429     O          E   N55      LV22D3      1
061900921106     O                                            1 '"'
062000921106     O                       §ODDLA               9
062100921106     O                                           11 '";'
062200040429     O********* Nazione per EEX
062300040429     O          E    55      LV22D3      1
062400040429     O*                         CARATTERE
062500040429     O                                              'DCH;065;DCW;065;ICS;08;'
062600040429     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
062700040429     O                                              'VBR;237;HBR;360;'
062800040429     O          E    55      LV22D3      1
062900040429     O                                              '"'
063000040429     O                       §DEISA
063100040429     O                                              '";'
063200040429     O********* CAP estero per EEX
063300040429     O          E    55      LV22D3      1
063400040429     O                                              'VBR;237;HBR;480;'
063500040429     O                                              '"'
063600040429     O                       §LDCAD
063700040429     O                                              '";'
063800040429     O********* > Box per nazione e cap EEX
063900040429     O          E    55      LV22D3      1
064000040429     O*                         CARATTERE
064100040429     O                                              'DCH;020;DCW;015;ICS;02;'
064200040429     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
064300040429     O                                              'HOME;HBR;000;VBR;040;'
064400040429     O***       E    55      LV22D3      1
064500040429     O* FONDO NERO                                                 L   H
064600040429     O*                                             'DCBX;340;140;355;068;'
064700040429     O* FONDO BIANCO                                               L   H
064800040429     O                                              'DBOX;340;140;355;068;'
064900041122      *---*   costante barra sopra
065000041122     O          E            LV22d6      1
065100041122     O                                           16 'VBR;248;HBR;045;'
065200041122     O          E            LV22d6      1
065300041122     O                                           22 'HLT;4;DHL;045;000;536;'
065400910403     O********* Bar Code
065500941216     O          E            LV22D6      1
065600940530     O                                           16 'VBR;371;HBR;090;'
065700951121     O* BARCODE INTERL. 2/5
065800041122     O**        E            LV22D6      1
065900041122     O**                                         22 'BDEF;8;BNEW;3;BWEW;10;'
066000041122     O**        E            LV22D6      1
066100041122     O**                                         14 'BCSH;121;BCST;'
066200951121     O* BARCODE CODE 128
066300041122     O          E            LV22D6
066400041122     O                                           15 'BDEF;25;BCPI;3;'
066500041122     O          E            LV22D6
066600041122     O                                           18 'BCSH;121;BCST;"C";'
066700941216     O          E            LV22D6      1
066800921104     O                       §§CDB2              21
066900941216     O          E            LV22D6      1
067000910403     O                                            5 'BSTP;'
067100041122      *---*   costante barra sotto
067200041122     O          E            LV22d6      1
067300041122     O                                           16 'VBR;370;HBR;045;'
067400041122     O          E            LV22d6      1
067500041122     O                                           22 'HLT;2;DHL;045;000;536;'
067600951020     O********* Codice Prodotto/KG/RISTAMPA
067700941216     O          E            LV22D7      1
067800910403     O*                                       CARATTERE
067900910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
068000941216     O          E            LV22D7      1
068100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
068200951021     O                                           16 'VBR;387;HBR;060;'
068300941216     O          E            LV22D7      1
068400921104     O                                            1 '"'
068500951020     O                       §RI21               22
068600941216     O                                           27 'KG.'
068700941216     O                       §LDPKG              35 '    0 , '
068800951020     O                       §LDRST              39
068900941216     O                                           41 '";'
069000941216     O********* Chiusura etchetta
069100941216     O          E            LV22D8      1
069200951020     O********                               CON COVERTITORE
069300951023     O                                            9 'TRM;%%5C%'
069400951020     O********                               CON VIDEO
069500040429     O**                                          5 'TRM;\'
069600980327     O*-------------------------------------------------------
069700971215     O* ETICHETTA ESTERO DHL
069800980327     O*-------------------------------------------------------
069900981117     O          E            LV22E            01
070000971215     O******                                 inizializzazione NUOVA
070100981117     O                                              '&&??%%P%%7E%^"LV22";'
070200971215     O******                                 inizializzazione VIDEO
070300040429     O**                                            '~^"LV22";'
070400971215     O          E            LV22E       1
070500971215     O*LOGICA                                N.ET. V (H) O=-35 X TB340
070600971215     O*                                                  O=000 X TX240
070700980327     O*                                                  O=073 X QB440
070800971215     O*                                                  O=-17 X TUTTI
070900981117     O                                              '00001;0;400;-17;'
071000971215     O          E            LV22E       1
071100971215     O*FISICA
071200981117     O                                              'UTOF;4100;SPB;'
071300981117     O          E            LV22E       1
071400971215     O*GIRO SCRITTA A 180°
071500981117     O                                              'FLIP;800;'
071600971215     O          E            LV22E       1
071700971215     O*                                       FONT
071800981117     O                                              'DHR;$1;'
071900971215     O          E            LV22E       1
072000981117     O                                              'HLT;01;VLT;01;'
072100971215      *---*   TIPO PRODOTTO DHL -REX-
072200971215     O          E            LV22E       1
072300971215     O                                              'VBR;040;HBR;260;'
072400971215     O                                              'DCH;20;DCW;15;ICS;04;'
072500971215     O          E            LV22E       1
072600971215     O                                              '"'
072700971215     O                                              'PRODUCT:'
072800971215     O                                              '";'
072900981117     O          E            LV22E       1
073000971215     O                                              'VBR;000;HBR;000;'
073100971215     O                                              'DBOX;380;20;280;68;'
073200981117     O          E            LV22E       1
073300971215     O                                              'VBR;080;HBR;400;'
073400971215     O                                              'DCH;080;DCW;160;ICS;04;'
073500981117     O          E            LV22E       1
073600971215     O                                              '"'
073700971215     O                                              'REX'
073800971215     O                                              '";'
073900971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
074000981117     O          E            LV22E       1
074100971215     O                                              'VBR;140;HBR;080;'
074200971215     O                                              'DCH;20;DCW;15;ICS;04;'
074300981117     O          E            LV22E       1
074400971215     O                                              '"'
074500971215     O                                              'DESTINATION:'
074600971215     O                                              '";'
074700981117     O          E            LV22E       1
074800971215     O                                              'VBR;200;HBR;080;'
074900971215     O                                              'DCH;80;DCW;80;ICS;04;'
075000981117     O          E            LV22E       1
075100971215     O                                              '"'
075200971215     O                       §DESIA
075300971215     O                                              '";'
075400971215     O* BARCODE 39
075500981117     O          E            LV22E       1
075600971215     O                                              'VBR;200;HBR;260;'
075700971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
075800981117     O          E            LV22E       1
075900971215     O                                              'BCSH;100;BCST;"*'
076000971215     O                       §DESIP
076100971215     O                       §DESIA
076200971215     O                       §DESSE
076300971215     O                                              '*";BSTP;'
076400971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
076500981117     O          E            LV22E       1
076600971215     O                                              'VBR;236;HBR;080;'
076700971215     O                                              'DCH;20;DCW;15;ICS;04;'
076800981117     O          E            LV22E       1
076900971215     O                                              '"'
077000971215     O                                              'AWB:'
077100971215     O                                              '";'
077200981117     O          E            LV22E       1
077300971215     O                                              'VBR;236;HBR;260;'
077400971215     O                                              'DCH;40;DCW;50;ICS;04;'
077500971215     O                                              '"'
077600971215     O                       §DEAWB
077700971215     O                                              '";'
077800971215     O* BARCODE 39
077900981117     O          E            LV22E       1
078000971215     O                                              'VBR;348;HBR;110;'
078100971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
078200981117     O          E            LV22E       1
078300971215     O                                              'BCSH;100;BCST;"*'
078400971215     O                       §DEAWB
078500971215     O                                              '*";BSTP;'
078600971215      *---*   NAZIONE DESTINO -CODICE ISO-
078700981117     O          E            LV22E       1
078800971215     O                                              'VBR;389;HBR;080;'
078900971215     O                                              'DCH;50;DCW;60;ICS;04;'
079000981117     O          E            LV22E       1
079100971215     O                                              '"'
079200971215     O                       §DEISA
079300971215     O                                              '";'
079400971215      *---*
079500981117     O          E            LV22E       1
079600971215     O                                              'VBR;369;HBR;220;'
079700971215     O                                              'DCH;20;DCW;20;ICS;04;'
079800981117     O          E            LV22E       1
079900971215     O                                              '"'
080000971215     O                                              'P: '
080100971215     O                       §LDLNP
080200971215     O                                              ' A: '
080300971215     O                       §LDLNA
080400971215     O                                              '";'
080500981117     O          E            LV22E       1
080600971215     O                                              'VBR;389;HBR;220;'
080700971215     O                                              'DCH;20;DCW;20;ICS;04;'
080800981117     O          E            LV22E       1
080900971215     O                                              '"'
081000971215     O                                              'N. '
081100971215     O                       §LDNRT
081200971215     O                       §S7
081300971215     O                                              '";'
081400971215      *---*   SEQUENZA COLLO   (XX OF XX)
081500981117     O          E            LV22E       1
081600971215     O                                              'VBR;369;HBR;480;'
081700971215     O                                              'DCH;20;DCW;25;ICS;04;'
081800981117     O          E            LV22E       1
081900971215     O                                              '"'
082000971215     O                       §LDNC1        Z
082100971215     O                                              ' OF '
082200971215     O                       §LDNC2        Z
082300971215     O                                              '";'
082400971215      *---*   I.A.T.A. ORIGINE
082500981117     O          E            LV22E       1
082600971215     O                                              'VBR;389;HBR;480;'
082700971215     O                                              'DCH;20;DCW;25;ICS;04;'
082800981117     O          E            LV22E       1
082900971215     O                                              '"'
083000971215     O                                              'ORIGIN:  '
083100971215     O                       §DESIP
083200971215     O                                              '";'
083300971215     O********* Chiusura etchetta
083400971215     O          E            LV22E       1
083500971215     O********                               CON COVERTITORE
083600971215     O                                              'TRM;%%5C%'
083700971215     O********                               CON VIDEO
083800040429     O**                                            'TRM;\'
083900980327     O*-------------------------------------------------------
084000980327     O* ETICHETTA ESTERO EUROEXPRESS
084100980327     O*-------------------------------------------------------
084200980327     O          E            LV22Y            01
084300980327     O******                                 inizializzazione NUOVA
084400980327     O                                              '&&??%%P%%7E%^"LV22";'
084500980327     O******                                 inizializzazione VIDEO
084600040429     O**                                            '~^"LV22";'
084700980327     O          E            LV22Y       1
084800980327     O*LOGICA                                N.ET. V (H) O=-35 X TB340
084900980327     O*                                                  O=000 X TX240
085000980327     O*                                                  O=073 X QB440
085100980327     O*                                                  O=-17 X TUTTI
085200980330     O                                              '00001;0;389;000;'
085300980327     O          E            LV22Y       1
085400980327     O*FISICA
085500980327     O                                              'UTOF;4100;SPB;'
085600980327     O          E            LV22Y       1
085700980327     O*GIRO SCRITTA A 180°
085800980327     O                                              'FLIP;800;'
085900980327     O          E            LV22Y       1
086000980327     O*                                       FONT
086100980327     O                                              'DHR;$1;'
086200980327     O          E            LV22Y       1
086300980327     O                                              'HLT;01;VLT;01;'
086400980327      *---*   NAZIONE PARTENZA
086500980327     O          E            LV22Y       1
086600980330     O                                              'HBR;160;VBR;042;'
086700980327     O                                              'DCH;30;DCW;30;ICS;02;'
086800980327     O                                              '"'
086900980327     O                       §DEISP
087000980327     O                                              '";'
087100980327      *---*   CONTRASSEGNO
087200980327     O          E    14      LV22Y       1
087300980330     O                                              'HBR;000;VBR;012;'
087400980327     O                                              'DBOX;490;010;190;048;'
087500980327     O          E    14      LV22Y       1
087600980330     O                                              'HBR;500;VBR;062;'
087700980327     O                                              'DCH;050;DCW;090;ICS;02;'
087800980327     O                                              '"COD";'
087900980327      *---*   EUROEXPRESS
088000980327     O          E            LV22Y       1
088100980330     O                                              'HBR;160;VBR;142;'
088200980327     O                                              'DCH;070;DCW;090;ICS;02;'
088300980327     O                                              '"EuroExpress";'
088400980327      *---*   NAZIONE ARRIVO
088500980327     O          E            LV22Y       1
088600980330     O                                              'HBR;000;VBR;012;'
088700980327     O                                              'DBOX;040;140;200;068;'
088800980327     O          E            LV22Y       1
088900980330     O                                              'HBR;050;VBR;172;'
089000980327     O                                              'DCH;020;DCW;015;ICS;02;'
089100980327     O                                              '"Country";'
089200980327     O          E            LV22Y       1
089300980330     O                                              'HBR;050;VBR;212;'
089400980327     O                                              'DCH;050;DCW;090;ICS;02;'
089500980327     O                                              '"'
089600980327     O                       §DEISA
089700980327     O                                              '";'
089800980327      *---*   CAP DESTINO
089900980327     O          E            LV22Y       1
090000980330     O                                              'HBR;000;VBR;012;'
090100980327     O                                              'DBOX;260;140;400;068;'
090200980327     O          E            LV22Y       1
090300980330     O                                              'HBR;270;VBR;172;'
090400980327     O                                              'DCH;020;DCW;015;ICS;02;'
090500980327     O                                              '"Post Code";'
090600980327     O          E            LV22Y       1
090700980330     O                                              'HBR;270;VBR;212;'
090800980327     O                                              'DCH;050;DCW;090;ICS;02;'
090900980327     O                                              '"'
091000980327     O                       §LDCAD
091100980327     O                                              '";'
091200980327      *---*   BARCODE EUROEXPRESS
091300980330     O          E            LV22Y       1
091400980327     O                                              'BDEF;25;BCSH;100;'
091500980330     O*                                     'BCPI;1;PRHR;1;'
091600011119     O          E   n23      LV22Y       1
091700011119     O                                              'BCPI;2;'
091800011116     O          E   n23      LV22Y       1
091900011116     O                     13                       'HBR;037;VBR;332;'
092000011116     O                    N13                       'HBR;062;VBR;332;'
092100011116     O                                              'BCST;"A'
092200011119     O*                      §DEJEU
092300011119     O*                      §DEISP
092400011119     O                       alfa5
092500011119     O                                              '%%40%D'
092600011119     O*                      §LDLNP
092700011119     O*                      §LDNRS
092800011119     O*                      §LDNCD
092900011119     O                       nume12
093000011119     O                                              '%%40%F'
093100011119     O*                      §deisa
093200011119     o                       alfa2
093300011116     O                       §end
093400011116      *con 55l
093500011119     O          E    23      LV22Y       1
093600011119     O                                              'BCPI;1;'
093700011116     O          E    23      LV22Y       1
093800990323     O                     13                       'HBR;037;VBR;332;'
093900990323     O                    N13                       'HBR;062;VBR;332;'
094000040429     O**                                            'BCST;"C@F'
094100980401     O                                              'BCST;"%%40%F'
094200980327     O                       §DEJEU
094300980327     O                       §DEISP
094400040429     O**                                            '@D'
094500980401     O                                              '%%40%D'
094600980327     O                       §LDLNP
094700980327     O                       §LDNRS
094800980327     O                       §LDNCD
094900040429     O**                                            '@F'
095000980401     O                                              '%%40%F'
095100980330     O                                              ' '
095200980327     O                       §DE55L
095300980327     O                       §BCEND
095400011119     O          E    23      LV22Y       1
095500980401      *---*   DECODIFICA
095600011119     O                                              'HBR;100;VBR;350;'
095700011119     O                                              'DCH;18;DCW;18;ICS;02;'
095800011119     O                                              '"'
095900011119     O                       §DEJEU
096000011119     O                       §DEISP
096100011119     O                       §LDLNP
096200011119     O                       §LDNRS
096300011119     O                       §LDNCD
096400011119     O                                              ' '
096500011119     O                       §DE55L
096600011119     O                       §BCD
096700011119     O                                              '";'
096800011119     O          E   n23      LV22Y       1
096900011119      *---*   DECODIFICA
097000011119     O                                              'HBR;100;VBR;350;'
097100011119     O                                              'DCH;18;DCW;18;ICS;02;'
097200011119     O                                              '"'
097300011119     O                       alfa5
097400011119     O                       nume12
097500011119     O                       alfa2
097600011119     O                                              '";'
097700980327      *---*   RIFERIMENTI BARTOLINI
097800980327     O          E            LV22Y       1
097900980401     O                                              'HBR;050;VBR;380;'
098000980327     O                                              'DCH;020;DCW;020;ICS;02;'
098100980327     O                                              '"'
098200980327     O                       §LDLNP
098300980327     O                                              ' '
098400980327     O                       §LDNRS
098500980327     O                                              ' '
098600980327     O                       §LDNCD
098700980327     O                                              '";'
098800980327     O********* Chiusura etchetta
098900980327     O          E            LV22Y       1
099000980327     O********                               CON COVERTITORE
099100980327     O                                              'TRM;%%5C%'
099200980327     O********                               CON VIDEO
099300040429     O**                                            'TRM;\'
099400000118     o*-----------------------------------------------------------------
099500000118     o*etichetta dpd "dove"
099600000118      *
099700000118     O          E            LV22dpd1         01
099800000118     O                                           20 '&&??%%P%%7E%^"LV22";'
099900000118     O          E            LV22dpd1    1
100000000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
100100000118     O                                           16 '00001;0;389;-17;'
100200000118     O          E            LV22dpd1    1
100300000118     O*FISICA
100400000118     O                                           14 'UTOF;4100;SPB;'
100500000118     O          E            LV22dpd1    1
100600000118     O*GIRO SCRITTA A 180°
100700000118     O                                            9 'FLIP;800;'
100800000118     O          E            LV22dpd1    1
100900000118     O                                            7 'DHR;$1;'
101000000118     O          E            LV22dpd1    1
101100000118     O                                           14 'HLT;01;VLT;01;'
101200000118      *---*   numero  spedizione
101300000118     O          E            LV22dpd1    1
101400000128     O                                           23 'DCH;020;DCW;020;ICS;03;'
101500000118     O          E            LV22dpd1    1
101600000128     O                                           16 'HBR;370;VBR;060;'
101700000118     O          E            LV22dpd1    1
101800000118     O                                            1 '"'
101900000118     O                       §dpd3                4
102000000118     O                                            6 '";'
102100000118      * 8 caratteri numero spedizione
102200000118     O          E            LV22dpd1    1
102300000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
102400000118     O          E            LV22dpd1    1
102500000128     O                                           16 'HBR;420;VBR;060;'
102600000118     O          E            LV22dpd1    1
102700000118     O                                            1 '"'
102800000118     O                       §dpd8                9
102900000118     O                                           11 '";'
103000000118      *ultimo carattere numero spedizione
103100000118     O          E            LV22dpd1    1
103200000128     O                                           23 'DCH;020;DCW;020;ICS;01;'
103300000118     O          E            LV22dpd1    1
103400000118     O                                           16 'HBR;520;VBR;060;'
103500000118     o          E            LV22dpd1    1
103600000118     O                                            1 '"'
103700000118     O                       §dpd12               2
103800000118     O                                            4 '";'
103900000118      *---*   costante scritta
104000000118     O          E            LV22dpd1    1
104100000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
104200000118     O          E            LV22dpd1    1
104300000118     O                                           16 'HBR;240;VBR;080;'
104400000118     O          E            LV22dpd1    1
104500000118     O                                            1 '"'
104600000118     O                       §dpdn1              52
104700000118     O                                           54 '";'
104800000118      *---*   costante scritta 2
104900000118     O          E            LV22dpd1    1
105000000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
105100000118     O          E            LV22dpd1    1
105200000118     O                                           16 'HBR;240;VBR;090;'
105300000118     O          E            LV22dpd1    1
105400000118     O                                            1 '"'
105500000118     O                       §dpdn2              50
105600000118     O                                           52 '";'
105700000118      *---*   costante scritta 3
105800000118     O          E            LV22dpd1    1
105900000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
106000000118     O          E            LV22dpd1    1
106100000118     O                                           16 'HBR;240;VBR;100;'
106200000118     O          E            LV22dpd1    1
106300000118     O                                            1 '"'
106400000118     O                       §dpdn3              54
106500000118     O                                           56 '";'
106600000118      *---*   costante barra sopra
106700000118     O          E            LV22dpd1    1
106800000118     O                                           16 'HBR;130;VBR;180;'
106900000118     O          E            LV22dpd1    1
107000000118     O                                           18 'HLT;2;DHL;0;0;310;'
107100000118      *---*   costante barra sotto
107200000118     O          E            LV22dpd1    1
107300000118     O                                           16 'HBR;130;VBR;280;'
107400000118     O          E            LV22dpd1    1
107500000118     O                                           18 'HLT;2;DHL;0;0;310;'
107600000118     o* secondo codice a barre
107700000118     O          e            lv22dpd1    1
107800000118     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
107900000118     O          e            lv22dpd1    1
108000000118     O                                            9 'BCSH;100;'
108100000118     O          E            LV22dpd1    1
108200000118     O                                           16 'HBR;130;VBR;280;'
108300000118     o          E            LV22dpd1    1
108400000118     O                                            5 'BCST;'
108500000118     O                                            6 '"'
108600000118     O                       §deawb              16
108700000118     O                                           18 '";'
108800000118     O                                           23 'BSTP;'
108900000118      *costante 3 v1/00
109000000118     O          E            LV22dpd1    1
109100000118     O                                           23 'DCH;040;DCW;040;ICS;02;'
109200000118     O          E            LV22dpd1    1
109300000118     O                                           16 'HBR;300;VBR;160;'
109400000118     o          E            LV22dpd1    1
109500000118     O                                            1 '"'
109600000124     O                       §dex1                6
109700000124     O                                            8 '";'
109800000118      *decodifica 2° bcc
109900000118     O          E            LV22dpd1    1
110000000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
110100000118     O          E            LV22dpd1    1
110200000118     O                                           16 'HBR;200;VBR;300;'
110300000118     o          E            LV22dpd1    1
110400000118     O                                            1 '"'
110500000118     O                       §dpdcap             14
110600000118     O                                           16 '";'
110700000118      *costante x
110800000118     O          E            LV22dpd1    1
110900000118     O                                           23 'DCH;050;DCW;030;ICS;02;'
111000000118     O          E            LV22dpd1    1
111100000118     O                                           16 'HBR;070;VBR;320;'
111200000118     o          E            LV22dpd1    1
111300000118     O                                            1 '"'
111400000118     O                       §desse               2
111500000118     O                                            4 '";'
111600000118      *costante 09
111700000118     O          E            LV22dpd1    1
111800000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
111900000118     O          E            LV22dpd1    1
112000000118     O                                           16 'HBR;070;VBR;380;'
112100000118     o          E            LV22dpd1    1
112200000118     O                                            1 '"'
112300000118     O                       dpd9                 3
112400000118     O                                            5 '";'
112500000118      *linea di arrivo
112600000118     O          E            LV22dpd1    1
112700000118     O                                           23 'DCH;080;DCW;040;ICS;02;'
112800000118     O          E            LV22dpd1    1
112900000118     O                                           16 'HBR;250;VBR;380;'
113000000118     o          E            LV22dpd1    1
113100000118     O                                            1 '"'
113200000124     O                       §dex3                4
113300000118     O                                            6 '";'
113400000118      *zona arrivo
113500000118     O          E            LV22dpd1    1
113600000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
113700000118     O          E            LV22dpd1    1
113800000118     O                                           16 'HBR;350;VBR;380;'
113900000118     o          E            LV22dpd1    1
114000000118     O                                            1 '"'
114100000124     O                       §dex4                3
114200000118     O                                            5 '";'
114300000118     O*------------------------------------------------------------------
114400000118     O          E            LV22dpd2         01
114500000118     O                                           20 '&&??%%P%%7E%^"LV22";'
114600000118     O          E            LV22dpd2    1
114700000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
114800000118     O                                           16 '00001;0;389;-17;'
114900000118     O          E            LV22dpd2    1
115000000118     O*FISICA
115100000118     O                                           14 'UTOF;4100;SPB;'
115200000118     O          E            LV22dpd2    1
115300000118     O*GIRO SCRITTA A 180°
115400000118     O                                            9 'FLIP;800;'
115500000118     O          E            LV22dpd2    1
115600000118     O                                            7 'DHR;$1;'
115700000118     O          E            LV22dpd2    1
115800000118     O                                           14 'HLT;01;VLT;01;'
115900000118      *---*   numero  spedizione
116000000118     O          E            LV22dpd2    1
116100000118     O                                           23 'DCH;050;DCW;030;ICS;03;'
116200000118     O          E            LV22dpd2    1
116300000118     O                                           16 'HBR;330;VBR;060;'
116400000118     O          E            LV22dpd2    1
116500000118     O                                            1 '"'
116600000118     O                       §dpd3                4
116700000118     O                                            6 '";'
116800000118      * 8 caratteri numero spedizione
116900000118     O          E            LV22dpd2    1
117000000118     O                                           23 'DCH;040;DCW;025;ICS;02;'
117100000118     O          E            LV22dpd2    1
117200000118     O                                           16 'HBR;400;VBR;060;'
117300000118     O          E            LV22dpd2    1
117400000118     O                                            1 '"'
117500000118     O                       §dpd8                9
117600000118     O                                           11 '";'
117700000118      *ultimo carattere numero spedizione
117800000118     O          E            LV22dpd2    1
117900000118     O                                           23 'DCH;035;DCW;020;ICS;01;'
118000000118     O          E            LV22dpd2    1
118100000118     O                                           16 'HBR;520;VBR;060;'
118200000118     o          E            LV22dpd2    1
118300000118     O                                            1 '"'
118400000118     O                       §dpd12               2
118500000118     O                                            4 '";'
118600000118      *---*   costante TELEFONO
118700000118     O          E            LV22dpd2    1
118800000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
118900000118     O          E            LV22dpd2    1
119000000118     O                                           16 'HBR;250;VBR;080;'
119100000118     O          E            LV22dpd2    1
119200000118     O                                            1 '"'
119300000118     O                       dpd2                20
119400000118     O                                           22 '";'
119500000118      *---*   costante fax
119600000118     O          E            LV22dpd2    1
119700000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
119800000118     O          E            LV22dpd2    1
119900000118     O                                           16 'HBR;250;VBR;090;'
120000000118     O          E            LV22dpd2    1
120100000118     O                                            1 '"'
120200000118     O                       dpd2f               20
120300000118     O                                           22 '";'
120400000118      *costante mittente
120500000118     O          E            LV22dpd2    1
120600000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
120700000118     O          E            LV22dpd2    1
120800000118     O                                           25 'HBR;160;VBR;100;'
120900000118     o          E            LV22dpd2    1
121000000118     O                                            1 '"'
121100000118     O                       dpd3                 8
121200000118     O                                           10 '";'
121300000118      *costante mittente2
121400000118     O          E            LV22dpd2    1
121500000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
121600000118     O          E            LV22dpd2    1
121700000118     O                                           25 'HBR;160;VBR;120;'
121800000118     o          E            LV22dpd2    1
121900000118     O                                            1 '"'
122000000118     O                       §ldrsm              36
122100000118     O                                           38 '";'
122200000118      *costante mittente 3
122300000118     O          E            LV22dpd2    1
122400000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
122500000118     O          E            LV22dpd2    1
122600000118     O                                           25 'HBR;160;VBR;140;'
122700000118     o          E            LV22dpd2    1
122800000118     O                                            1 '"'
122900000118     O                       §ldprm               3
123000000118     O                                            5 '";'
123100000118      *costante destinatario
123200000118     O          E            LV22dpd2    1
123300000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
123400000118     O          E            LV22dpd2    1
123500000118     O                                           25 'HBR;070;VBR;150;'
123600000118     o          E            LV22dpd2    1
123700000118     O                                            1 '"'
123800000118     O                       dpd4                11
123900000118     O                                           13 '";'
124000000118      *destinatario
124100000118     O          E            LV22dpd2    1
124200000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
124300000118     O          E            LV22dpd2    1
124400000118     O                                           20 'HBR;070;VBR;170;'
124500000118     o          E            LV22dpd2    1
124600000118     O                                            1 '"'
124700000118     O                       §ldrsd              36
124800000118     O                                           38 '";'
124900000118      * destinatario indirizzo
125000000118     O          E            LV22dpd2    1
125100000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
125200000118     O          E            LV22dpd2    1
125300000118     O                                           20 'HBR;070;VBR;190;'
125400000118     o          E            LV22dpd2    1
125500000118     O                                            1 '"'
125600000118     O                       §ldind              36
125700000118     O                                           38 '";'
125800000118      *località destinatario 7
125900000118     O          E            LV22dpd2    1
126000000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
126100000118     O          E            LV22dpd2    1
126200000118     O                                           20 'HBR;070;VBR;210;'
126300000118     o          E            LV22dpd2    1
126400000118     O                                            1 '"'
126500000118     O                       §ldlod              36
126600000118     O                                           38 '";'
126700000118      *cap      destinatario 8
126800000118     O          E            LV22dpd2    1
126900000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
127000000118     O          E            LV22dpd2    1
127100000118     O                                           20 'HBR;070;VBR;240;'
127200000118     o          E            LV22dpd2    1
127300000118     O                                            1 '"'
127400000118     O                       §ldcad              10
127500000118     O                                           12 '";'
127600000118      *cap      destinatario 8
127700000118     O          E            LV22dpd2    1
127800000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
127900000118     O          E            LV22dpd2    1
128000000118     O                                           20 'HBR;250;VBR;240;'
128100000118     o          E            LV22dpd2    1
128200000118     O                                            1 '"'
128300000118     O                       §desia               4
128400000118     O                                            6 '";'
128500000118      *---*   costante barra sopra
128600000118     O          E            LV22dpd2    1
128700000118     O                                           16 'HBR;130;VBR;250;'
128800000118     O          E            LV22dpd2    1
128900000118     O                                           18 'HLT;2;DHL;0;0;370;'
129000000118      *---*   costante barra sotto
129100000118     O          E            LV22dpd2    1
129200000118     O                                           16 'HBR;130;VBR;350;'
129300000118     O          E            LV22dpd2    1
129400000118     O                                           18 'HLT;2;DHL;0;0;370;'
129500000118      *costante primo B-C
129600000118     O          e            lv22dpd2    1
129700000118     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
129800000118     O          e            lv22dpd2    1
129900000118     O                                            9 'BCSH;100;'
130000000118     O          E            LV22dpd2    1
130100000118     O                                           16 'HBR;130;VBR;350;'
130200000118     o          E            LV22dpd2    1
130300000118     O                                            5 'BCST;'
130400000118     O                                            6 '"'
130500000118     O                       §dpd                18
130600000118     O                                           20 '";'
130700000118     O                                           25 'BSTP;'
130800000118      *costante data
130900000118     O          E            LV22dpd2    1
131000000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
131100000118     O          E            LV22dpd2    1
131200000118     O                                           16 'HBR;070;VBR;385;'
131300000118     o          E            LV22dpd2    1
131400000118     O                                            1 '"'
131500000118     O                       datasp              11 '  /  /    '
131600000118     O                                           13 '";'
131700000118      *costante 1 collo
131800000118     O          E            LV22dpd2    1
131900000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
132000000118     O          E            LV22dpd2    1
132100000118     O                                           16 'HBR;200;VBR;385;'
132200000118     o          E            LV22dpd2    1
132300000118     O                                            1 '"'
132400000118     O                                            8 '1 Collo'
132500000118     O                                           10 '";'
132600000118      *costante peso
132700000118     O          E            LV22dpd2    1
132800000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
132900000118     O          E            LV22dpd2    1
133000000118     O                                           16 'HBR;300;VBR;385;'
133100000118     o          E            LV22dpd2    1
133200000118     O                                            1 '"'
133300000118     O                                            8 'Peso Kg'
133400000118     O                                           10 '";'
133500000118      * peso
133600000118     O          E            LV22dpd2    1
133700000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
133800000118     O          E            LV22dpd2    1
133900000118     O                                           16 'HBR;400;VBR;385;'
134000000118     o          E            LV22dpd2    1
134100000118     O                                            1 '"'
134200000118     O                       §ldpkg        2     10
134300000118     O                                           12 '";'
134400000118      *------------------------------------------------------------------
134500000118      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
134600000118     O* costante dpd
134700000118     O          E            LV22dpd     1
134800000118     O                                           23 'DCH;030;DCW;070;ICS;03;'
134900000118     O          E            LV22dpd     1
135000000118     O                                           25 'HBR;170;VBR;110;DHR;$201;'
135100000118     o          E            LV22dpd     1
135200000118     O                                            1 '"'
135300000118     O                       dpd                  4
135400000118     O                                            6 '";'
135500000118     O* costante dpd riga sotto
135600000118     O          E            LV22dpd     1
135700000118     O                                           23 'DCH;006;DCW;005;ICS;01;'
135800000118     O          E            LV22dpd     1
135900000118     O                                           20 'HBR;210;VBR;095;'
136000000118     o          E            LV22dpd     1
136100000118     O                                            1 '"'
136200000118     O                       dpd1                23
136300000118     O                                           25 '";'
136400000118     o          E            LV22dpd     1
136500000118     O                                            9 'TRM;%%5C%'
