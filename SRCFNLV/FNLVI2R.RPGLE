000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200950725     F* FNLVI2R  *---------------------------------------------------*
000300910403     F*-----------------*                                            *
000400950711     F* STAMPA SEGNACOLLO CON BARCODE A 18 CON ITALORA               *
000500921120     F*        CON BARCODE 3X70                                      *
000600910403     F*--------------------------------------------------------------*
000700941216     FFNLV22P   O    F  132        PRINTER
000800910404     D §RI             S              1    DIM(37)
000900910403     D §O7             S              1    DIM(7)
001000980318     D §S9             S              1    DIM(9)
001100980318     D §O9             S              1    DIM(9)
001200980327     D §EU             S              1    DIM(35)
001300910403     D                 DS
001400910403     D  §CB                    1     18  0
001500910403     D                                     DIM(18)
001600910403     D  §CA                    1     18
001700910403     D                                     DIM(18)
001800910403     D                 DS
001900910403     D  §N7                    1      7  0
002000910403     D  §S7                    1      7
002100910403     D                                     DIM(7)
002200910403     D                 DS
002300910403     D  §2                     1      2  0
002400910403     D                                     DIM(2)
002500910403     D  §2A                    1      2
002600910403     D                                     DIM(2)
002700910403     D DSBLD         E DS
002800971215     D DSBLDE        E DS
002900910403     D DSBLD7        E DS
003000910403     D  §C                     1     17  0
003100910403     D                                     DIM(17)
003200011119     D §bceurex        DS
003300011119     D  alfa5                         5
003400011119     D  nume12                       12  0
003500011119     D  alfa2                         2
003600910403     I/SPACE 3
003700910403     C*---------------------------------------------------------------*
003800910403     C     *ENTRY        PLIST
003900910403     C                   PARM                    DSBLD
004000971215     C                   PARM                    DSBLDE
004100941216     C*
004200971222     C*** PROVE CON VALORI FISSI + ETICHETTE
004300971222     C**                   DO   2
004400971222     C**                   TIME           §HMS    60
004500971222     C**                   MOVEL§HMS      §LDCDP
004600971222     C**                   ADD  1         §LDNCD
004700941228     C*
004800921202     C                   MOVEL     §LDNRS        §LDNRT            3
004900921202     C                   MOVE      '-'           §LDNRT
005000971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
005100971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
005200971222     C     §DETST        IFEQ      'EST'
005300971222     C                   EXSR      COLZER
005400971222     C                   ENDIF
005500921202     C*
005600971222     C     §DETST        IFEQ      'ITA'
005700971222     C     §DETST        OREQ      'ALL'
005800971222     C                   EXSR      PRINT
005900971222     C                   ENDIF
006000971222     C*
006100971222     C**                   ENDDO
006200980318     C*
006300980318     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
006400980318     C*
006500971222     C     §DETST        IFEQ      'EST'
006600971222     C     §DETST        OREQ      'ALL'
006700000128     c                   select
006800000128     c     §detse        wheneq    'REX'
006900000128     c                   except    lv22e
007000000128     c     §detse        wheneq    'EUR'
007100000128     c                   exsr      eurexp
007200000128     c                   endsl
007300000128     C*    §DEAWB        IFEQ      *BLANKS
007400000128     C*                  EXSR      EUREXP
007500000128     C*                  ELSE
007600000128     C*                  EXCEPT    LV22E
007700000128     C*                  ENDIF
007800971222     C                   ENDIF
007900971222     C*
008000971222     C                   SETON                                        LR
008100971222     C/SPACE 005
008200971222     C*****************************************************
008300971222     C* IMPOSTAZIONE E STAMPA                           ***
008400971222     C*****************************************************
008500971222     C     PRINT         BEGSR
008600910403     C                   MOVE      §LDLNP        §LCLNP
008700910403     C                   MOVE      §LDLNA        §LCLNA
008800910403     C                   MOVE      §LDNRS        §LCNRS
008900910403     C                   MOVE      §LDNCD        §LCNCD
009000910403     C                   MOVE      §LDZSC        §LCZSC
009100040503     c     §detse        comp      'EEX'                                  55
009200960601     C*
009300960601     C* TOLGO IL CARATTERE ° IN QUANTO BLOCCA LA STAMPA
009400960601     C     '°':' '       XLATE     §LDRSM        §LDRSM
009500960601     C     '°':' '       XLATE     §LDRSD        §LDRSD
009600960601     C     '°':' '       XLATE     §LDLOD        §LDLOD
009700960601     C     '°':' '       XLATE     §LDIND        §LDIND
009800960601     C     '°':' '       XLATE     §LDCDP        §LDCDP
009900050606     C*
010000050606     C* TOLGO IL CARATTERE \ IN QUANTO BLOCCA LA STAMPA
010100050606     C     '\':' '       XLATE     §LDRSM        §LDRSM
010200050606     C     '\':' '       XLATE     §LDRSD        §LDRSD
010300050606     C     '\':' '       XLATE     §LDLOD        §LDLOD
010400050606     C     '\':' '       XLATE     §LDIND        §LDIND
010500050606     C     '\':' '       XLATE     §LDCDP        §LDCDP
010600910403     C*
010700910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
010800910403     C                   EXSR      M10
010900910403     C                   MOVEA     §CA           §C18A            18
011000910403     C                   MOVE      §C18A         §CDBAR           18
011100970210     C*
011200941216     C     §LDTSP        IFEQ      'D'
011300970303     C     §LDLNA        ANDLT     300
011400941216     C                   MOVE      *BLANK        §LDTSP
011500941216     C                   END
011600941216     C*
011700941216     C     §LDTSP        COMP      *BLANK                             1616
011800910403     C     §LDTSP        COMP      'E'                                    17
011900090806     C  N17§LDTSP        COMP      'H'                                    17
012000910403     C     §LDCDP        COMP      *BLANK                             18
012100910403     C     §LDNRS        COMP      *ZERO                              19
012200910403     C*
012300921106     C** INIZIALIZZA STAMPANTE
012400941216     C                   EXCEPT    LV22
012500910403     C*
012600910404     C                   MOVEA     *BLANK        §RI
012700910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
012800910403     C                   SETOFF                                       77
012900910403     C                   MOVE      *BLANK        §A7               7
013000910403     C                   Z-ADD     §LDNC1        §N7
013100910403     C                   DO        7             §1
013200910403     C     §S7(§1)       IFEQ      *ZERO
013300910403     C                   MOVE      *BLANK        §S7(§1)
013400910403     C                   ELSE
013500910403     C                   SETON                                        77
013600910403     C                   END
013700910403     C  N77              END
013800910403     C                   MOVEA     §S7           §A7
013900910403     C                   MOVE      §A7           §A3               3
014000910403     C                   MOVEA     §A3           §RI(26)
014100910403     C                   MOVEA     '-'           §RI(29)
014200910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
014300910403     C                   SETOFF                                       77
014400910403     C                   MOVE      *BLANK        §A7
014500910403     C                   Z-ADD     §LDNC2        §N7
014600910403     C                   DO        7             §1
014700910403     C     §S7(§1)       IFEQ      *ZERO
014800910403     C                   MOVE      *BLANK        §S7(§1)
014900910403     C                   ELSE
015000910403     C                   SETON                                        77
015100910403     C                   END
015200910403     C  N77              END
015300910403     C                   MOVEA     §S7           §A7
015400910403     C                   MOVE      §A7           §A3
015500910403     C                   MOVEA     §A3           §RI(30)
015600910403     C                   MOVE      §LDCPD        §A3
015700910404     C                   MOVEA     §A3           §RI(34)
015800910403     C* STAMPA LINEA PARTENZA
015900941216     C                   EXCEPT    LV22H
016000910403     C*
016100910404     C                   MOVEA     *BLANK        §RI
016200910403     C                   MOVEA     §LDRSM        §RI(8)
016300941216     C                   MOVEA     ' '           §RI(30)
016400941216     C                   MOVEA     §LDPRM        §RI(31)
016500950127     C                   MOVEA     *BLANK        §RI(33)
016600941216     C                   EXCEPT    LV22D1
016700910403     C* STAMPA DATI MITTENTE
016800910403     C*
016900910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
017000971222     C                   EXSR      COLZER
017100910403     C*
017200971223     C*
017300910403     C* STAMPA NUMERO SEGNACOLLO
017400941216     C                   EXCEPT    LV22D2
017500910403     C*
017600921106     C                   MOVE      §LDTNA        §A3               3
017700921106     C     '0':'O'       XLATE     §A3           §ODTNA            3
017800921106     C                   MOVE      §LDLNA        §A3
017900921106     C     '0':'O'       XLATE     §A3           §ODLNA            3
018000921106     C                   MOVE      §LDZSC        §A2
018100921106     C     '0':'O'       XLATE     §A2           §ODZSC            2
018200921106     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
018300921106     C                   MOVEL     §LDDLA        §ODDLA            8
018400910403     C* STAMPA LINEA ARRIVO
018500950712     C                   EXCEPT    LV22D3
018600910403     C*
018700910404     C                   MOVEA     *BLANK        §RI
018800910403     C                   MOVEA     §LDRSD        §RI(8)
018900941216     C                   MOVEA     ' '           §RI(30)
019000941216     C                   MOVEA     §LDPRD        §RI(31)
019100950127     C                   MOVEA     *BLANK        §RI(33)
019200910403     C* STAMPA 1° DATI DESTINATARIO
019300941216     C                   EXCEPT    LV22D4
019400910403     C*
019500910404     C                   MOVEA     *BLANK        §RI
019600910404     C                   MOVEA     §LDIND        §RI(8)
019700910404     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
019800910404     C                   Z-ADD     37            §Y                2 0    88
019900910404     C                   DO        36
020000910404     C                   SUB       1             §Y
020100910404     C     §RI(§Y)       IFNE      ' '
020200910404     C                   SETON                                        88
020300910404     C                   END
020400910404     C  N88              END
020500910404     C     §Y            IFLE      25
020600910404     C                   ADD       3             §Y
020700910404     C                   ELSE
020800910404     C                   MOVEA     '  '          §RI(26)
020900910404     C                   Z-ADD     28            §Y
021000910404     C                   END
021100910404     C                   MOVEA     §LDLOD        §RI(§Y)
021200950725     C                   MOVEA     *BLANK        §RI(33)
021300910403     C* STAMPA 2° DATI DESTINATARIO
021400941216     C                   EXCEPT    LV22D5
021500941216     C* STAMPA SERVIZIO
021600941216     C   16              EXCEPT    LV22X5
021700041126      *
021800910403     C*
021900910403     C* STAMPA BARCODE
022000920720     C*
022100950712     C                   EXCEPT    LV22D6
022200910403     C*
022300910410     C                   MOVEA     *BLANK        §RI
022400910403     C   18              DO
022500941216     C                   MOVEA     'Rif.:'       §RI(1)
022600980402     C     §DEEUR        IFNE      *BLANK
022700980402     C     §DETST        ANDEQ     'ITA'
022800980402     C                   MOVEA     'JUT :'       §RI(1)
022900980402     C                   ENDIF
023000941216     C                   MOVEA     §LDCDP        §RI(7)
023100910410     C                   END
023200041126      * STAMPA barra barcode
023300041126     C                   EXCEPT    LV22DX
023400041126     C                   EXCEPT    LV22DY
023500941216     C* STAMPA RIFERIMENTO/RISTAMPA
023600950712     C                   EXCEPT    LV22D7
023700910403     C*
023800910403     C* STAMPA CHIUSURA ETICHETTA
023900941216     C                   EXCEPT    LV22D8
024000971224     C                   ENDSR
024100971224     C/SPACE 005
024200971224     C*****************************************************
024300971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024400971224     C*****************************************************
024500971224     C     COLZER        BEGSR
024600971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024700971224     C                   SETOFF                                       77
024800971224     C                   MOVE      §LDNCD        §N7
024900971224     C                   DO        7             §1
025000971224     C     §S7(§1)       IFEQ      *ZERO
025100971224     C                   MOVE      *BLANK        §S7(§1)
025200971224     C                   ELSE
025300971224     C                   SETON                                        77
025400971224     C                   END
025500971224     C  N77              END
025600971224     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
025700971224     C                   MOVE      *ZERO         §X                1 0
025800971224     C                   MOVEA     *BLANK        §O7
025900971224     C                   DO        7             §1
026000971224     C     §S7(§1)       IFNE      *BLANK
026100971224     C                   ADD       1             §X
026200971224     C     §S7(§1)       IFEQ      '0'
026300971224     C                   MOVE      'O'           §O7(§X)
026400971224     C                   ELSE
026500971224     C                   MOVE      §S7(§1)       §O7(§X)
026600971224     C                   END
026700971224     C                   END
026800971224     C                   END
026900971224     C                   MOVEA     §O7           §S7
027000971224     C                   ENDSR
027100971222     C/SPACE 005
027200980318     C*****************************************************
027300980318     C* STAMPA EUROEXPRESS                              ***
027400980318     C*****************************************************
027500980318     C     EUREXP        BEGSR
027600980327     C*
027700980327     C* IMPOSTO COSTANTI
027800980327     C                   MOVEL     'JUT'         §DEJEU            3
027900980327     C                   MOVEL     '55L'         §DE55L            3
028000980327     C                   MOVEL     'IT'          §DEISP            2
028100011116      * controllo se contenuto 55L nel codice passato
028200011116     c     '55L'         scan      §deeur                                 23
028300011119      * se contenuto 55l scompongo a modo vecchio
028400011119     c                   if        *in23
028500980327     C* ESTRAGGO IL CAP:
028600980327     C                   MOVE      *ZEROS        §Y
028700980327     C                   MOVEA     §DEEUR        §EU
028800980327     C* TROVO QUANTO è LUNGO IL BARCODE
028900980327     C     ' '           CHECKR    §DEEUR        §Y
029000980327     C* ISOLO IL CAP
029100980327     C                   MOVE      *BLANK        §O9
029200980327     C                   MOVE      *ZEROS        §W                2 0
029300980327     C     §Y            SUB       2             §Y
029400980327     C     22            DO        §Y            §X1               2 0
029500980327     C     §EU(§X1)      IFNE      *BLANKS
029600980327     C                   ADD       1             §W
029700980327     C                   MOVE      §EU(§X1)      §O9(§W)
029800980327     C                   END
029900980327     C                   END
030000980318     C*
030100980318     C                   MOVEA     §O9           §LDCAD
030200011119
030300011119      *scompongo a modo nuovo senza 55l
030400011119     c                   else
030500011119     c                   movel     §deeur        §bceurex
030600011119     c                   end
030700980318     C*
030800980318     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
030900980318     C* DEL BARCODE
031000980319     C     §LDCAD        CAT       §DEISA:0      §BCEND           11
031100980318     C*
031200980318     C* VERIFICO SE CONTRASSEGNO
031300980318     C*
031400980318     C     §DEFCA        COMP      'S'                                    14
031500980318     C  N14§DEFCA        COMP      'X'                                    14
031600980318     C*
031700980318     C                   EXCEPT    LV22Y
031800980318     C*
031900980318     C                   ENDSR
032000910403     C/SPACE 005
032100910403     C*****************************************************
032200910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
032300910403     C*****************************************************
032400910403     C     M10           BEGSR
032500910403     C                   MOVE      *ZERO         §TT               3 0
032600910403     C                   MOVE      *ZERO         §TT2              3 0
032700910403     C                   MOVE      *ZERO         §1                1 0
032800910403     C                   Z-ADD     18            §S                2 0
032900910403     C                   DO        17            §N                2 0
033000910403     C                   SUB       1             §S
033100910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
033200910403     C                   MOVE      §C(§N)        §CB(§N)
033300910403     C     §N            DIV       2             §CTR              3 0
033400910403     C                   MVR                     §RST              3 010
033500910403     C     *IN(10)       IFEQ      '1'
033600910403     C     §C(§S)        MULT      2             §N2               2 0
033700910403     C                   MOVE      §N2           §A2               2
033800910403     C                   MOVEA     §A2           §2A
033900910403     C                   XFOOT     §2            §CTR
034000910403     C                   ELSE
034100910403     C                   Z-ADD     §C(§S)        §CTR
034200910403     C                   END
034300910403     C                   ADD       §CTR          §TT
034400910403     C                   END
034500910403     C*
034600910403     C     §TT           ADD       9             §TT2
034700910403     C                   MOVE      0             §TT2
034800910403     C     §TT2          SUB       §TT           §1
034900910403     C*
035000910403     C                   MOVE      §1            §CB(18)
035100910403     C                   ENDSR
035200040503      *---------------------------------------------------------------*
035300971215     OFNLV22P   E            LV22             01
035400950712     O******                                 Inizializzazioni
035500951010     O                                            7 '?07&120'
035600950712     O          E            LV22        1
035700950712     O                                            5 '?13&4'
035800950712     O          E            LV22        1
035900950713     O                                            7 '?06&-90'
036000950725     O**      E 1              LV22
036100950725     O**                                  4 '?39&'
036200910403     O********* Mittente
036300941216     O          E            LV22D1      1
036400041213     O                                           21 '?02&12,100,20,3,1,22,'
036500041213     O                                           25 '8,1;'
036600041213     O                       §RI                 62
036700910403     O********* Destinatario (Rag.Soc.)
036800941216     O          E            LV22D4      1
036900041213     O                                           18 '?15&205,48,405,2,6'
037000941216     O          E            LV22D4      1
037100041213     O                                           21 '?02&12,100,60,3,1,22,'
037200950725     O                                           25 '8,1;'
037300950725     O                       §RI                 62
037400910403     O********* Destinatario (Località)
037500941216     O          E            LV22D5      1
037600041213     O                                           21 '?02&12,100,92,3,1,22,'
037700950725     O                                           25 '8,1;'
037800950725     O                       §RI                 62
037900920221     O********* MC. SPEDIZIONE
038000941216     O          E            LV22D5      1
038100041213     O                                           22 '?02&12,485,128,2,1,11,'
038200950712     O                                           27 '12,1;'
038300950712     O                                           30 'MC.'
038400950712     O                       §LDVOL              37 '0 ,   '
038500950712     O                       §LDFVR              40
038600941216     O********* SERVIZIO
038700941216     O          E            LV22X5      1
038800041213     O                                           23 '?02&10,645,21,2,1,23,5,'
038900041213     O                                           25 '1;'
039000041213     O                       §LDTSP              26
039100950712     O          E            LV22X5      1
039200950714     O* FONDO NERO
039300041213     O                     17                    18 '?22&620,20,90,90,2'
039400950714     O* FONDO BIANCO
039500041213     O                    N17                    18 '?16&620,20,90,90,5'
039600910403     O********* Linea Partenza
039700950712     O**colli/padroncino
039800941216     O          E            LV22H       1
039900041213     O                                           22 '?02&12,262,156,2,1,11,'
040000950712     O                                           27 '12,1;'
040100950712     O                       §RI                 65
040200941216     O          E            LV22H       1
040300950712     O**Linea Partenza
040400041213     O                                           22 '?02&12,205,113,2,1,12,'
040500950712     O                                           27 '10,1;'
040600950714     O                       §LDLNP              30
040700950712     O                       §LDDLP              42
040800910403     O********* Segnacollo
040900941216     O          E    19      LV22D2      1
041000041213     O                                           22 '?02&10,110,205,2,1,25,'
041100950713     O                                           26 '5,1;'
041200950712     O                       §LDNRT              31
041300941216     O          E            LV22D2      1
041400041213     O                                           22 '?02&10,255,175,2,1,46,'
041500950712     O                                           26 '5,1;'
041600950712     O                       §S7                 33
041700921106     O********* TERMINAL
041800941216     O          E            LV22D3      1
041900950725     O                                           22 '?02&12,095,348,4,1,12,'
042000950712     O                                           26 '3,1;'
042100950714     O                       §ODTNA              29
042200910403     O* BOX X TERMINAL ARRIVO
042300941216     O          E            LV22D3      1
042400041126     O                                           21 '?22&089,343,111,105,2'
042500921106     O********* CODICE LINEA ARRIVO
042600941216     O          E            LV22D3      1
042700950725     O                                           22 '?02&12,216,348,4,1,12,'
042800950712     O                                           26 '3,1;'
042900950714     O                       §ODLNA              29
043000950712     O********* DESCRIZIONE LINEA DI ARRIVO
043100950712     O** ITALIA
043200040503     O          E   N55      LV22D3      1
043300950725     O                                           22 '?02&12,345,348,4,1,12,'
043400950712     O                                           26 '7,1;'
043500950712     O                       §ODDLA              34
043600921106     O********* ZONA ARRIVO
043700040503     O          E   N55      LV22D3      1
043800950725     O                                           22 '?02&12,626,348,4,1,12,'
043900950712     O                                           26 '2,1;'
044000950712     O                       §ODZSC              28
044100910403     O* BOX X ZONA ARRIVO
044200040503     O          E   N55      LV22D3      1
044300950713     O                                           20 '?22&615,343,89,107,2'
044400040503     O********* Nazione & CAP estero per EEX
044500040503     O          E    55      LV22D3      1
044600040503     O                                              '?02&12,345,348,4,1,12,'
044700040503     O                                          + 0 '7,1;'
044800040503     O                       §DEISA             + 0
044900040503     O                       §LDCAD             + 2
045000040503     O********* Box per nazione e cap estero EEX
045100040503     O          E    55      LV22D3      1
045200040503     O                                              '?16&335,345,91,300,4'
045300041126     O********* Barra chiusura superiore
045400041126     O          E            LV22DX      1
045500041213     O                                           19 '?15&160,455,382,2,8'
045600041126     O********* Barra chiusura inferiore
045700041126     O          E            LV22DY      1
045800041213     O                                           19 '?15&160,655,382,2,6'
045900910403     O********* Bar Code
046000941216     O          E            LV22D6      1
046100950712     O* RAPPORTO BARRA
046200950714     O                                            5 '?09&3'
046300950712     O          E            LV22D6      1
046400950712     O                                            5 '?10&1'
046500950712     O* LARGHEZZA ELEMENTO UNITARIO CODICE
046600950712     O          E            LV22D6      1
046700950714     O                                            5 '?11&3'
046800950712     O          E            LV22D6      1
046900950712     O* SOPPRESSIONE CARATTERI LEGGIBILI
047000950712     O                                            5 '?13&3'
047100041202      * 2/5
047200041202     O*         E            LV22D6      1
047300041202     O*                                          21 '?02&13,163,457,1,225,'
047400041202     O*                                          26 '18,1;'
047500041202     O*                      §CDBAR              44
047600041202     o*code 128
047700041202     O          E            LV22D6      1
047800041202     O                                           22 '?02&13,163,457,14,200,'
047900041202     O                                           27 '18,1;'
048000041202     O                       §CDBAR              45
048100910403     O********* Codice Prodotto
048200941216     O          E            LV22D7      1
048300041210     O                                           22 '?02&12,120,665,2,1,11,'
048400950712     O                                           27 '10,1;'
048500950712     O                       §RI                 64
048600920221     O********* KG. SPEDIZIONE
048700941216     O          E            LV22D7      1
048800041210     O                                           22 '?02&12,468,665,2,1,11,'
048900950712     O                                           27 '10,1;'
049000950712     O                                           30 'KG.'
049100950712     O                       §LDPKG              38 '    0 , '
049200971224     O                       §LDRST              43
049300941216     O********* Chiusura etchetta
049400941216     O          E            LV22D8      1
049500950712     O                                            5 '?14&1'
049600951010     O          E            LV22D8      1
049700951010     O                                            4 '?00&'
049800971215     O*----------------------------------------------------------------
049900971215     O* ETICHETTA ESTERO DHL
050000971215     O*----------------------------------------------------------------
050100971215     O          E            LV22E            01
050200971215     O******                                 Inizializzazioni
050300971215     O                                            7 '?07&120'
050400971215     O          E            LV22E       1
050500971215     O                                            5 '?13&4'
050600971215     O          E            LV22E       1
050700971215     O                                            7 '?06&-90'
050800971215     O**      E 1              LV22E
050900971215     O**                                  4 '?39&'
051000971215      *---*   TIPO PRODOTTO DHL -REX-
051100971215     O          E            LV22E       1
051200971215     O                                              '?02&10,300,0,0,1,23,'
051300971215     O                                              '1,1;'
051400971215     O                                              'PRODUCT:'
051500971215     O          E            LV22E       1
051600971215     O                                              '?16&415,0,110,280,3'
051700971215     O          E            LV22E       1
051800971215     O                                              '?02&10,440,10,6,1,11,'
051900971215     O                                              '1,1;'
052000971215     O                                              'REX'
052100971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
052200971215     O          E            LV22E       1
052300971215     O                                              '?02&10,120,210,0,1,23,'
052400971215     O                                              '1,1;'
052500971215     O                                              'DESTINATION:'
052600971215     O          E            LV22E       1
052700971215     O                                              '?02&10,120,225,4,1,13,'
052800971215     O                                              '1,1;'
052900971215     O                       §DESIA
053000971215     O* RAPPORTO BARRA
053100971215     O          E            LV22E       1
053200971215     O                                              '?09&3'
053300971215     O          E            LV22E       1
053400971215     O                                              '?10&1'
053500971215     O* LARGHEZZA ELEMENTO UNITARIO CODICE
053600971215     O          E            LV22E       1
053700971215     O                                              '?11&3'
053800971215     O          E            LV22E       1
053900971215     O* SOPPRESSIONE CARATTERI LEGGIBILI
054000971215     O                                              '?13&3'
054100971215     O          E            LV22E       1
054200971215     O                                              '?02&13,290,130,6,200,'
054300971215     O                                              '1,1;'
054400971215     O                       §DESIP
054500971215     O                       §DESIA
054600971215     O                       §DESSE
054700971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
054800971215     O          E            LV22E       1
054900971215     O                                              '?02&10,120,360,0,1,23,'
055000971215     O                                              '1,1;'
055100971215     O                                              'AWB:'
055200971215     O          E            LV22E       1
055300971215     O                                              '?02&10,300,335,2,1,22,'
055400971215     O                                              '1,1;'
055500971215     O                       §DEAWB
055600971215     O          E            LV22E       1
055700971215     O                                              '?02&13,145,405,6,200,'
055800971215     O                                              '1,1;'
055900971215     O                       §DEAWB
056000971215      *---*   NAZIONE DESTINO -CODICE ISO-
056100971215     O          E            LV22E       1
056200971215     O                                              '?02&10,120,605,4,1,13,'
056300971215     O                                              '1,1;'
056400971215     O                       §DEISA
056500971215      *---*
056600971215     O          E            LV22E       1
056700971215     O                                              '?02&10,260,605,2,1,12,'
056800971215     O                                              '1,1;'
056900971215     O                                              'P: '
057000971215     O                       §LDLNP
057100971215     O                                              ' A: '
057200971215     O                       §LDLNA
057300971215     O          E            LV22E       1
057400971215     O                                              '?02&10,260,655,2,1,12,'
057500971215     O                                              '1,1;'
057600971215     O                                              'N. '
057700971215     O                       §LDNRT
057800971215     O                       §S7
057900971215      *---*   SEQUENZA COLLO   (XX OF XX)
058000971215     O          E            LV22E       1
058100971215     O                                              '?02&10,500,605,2,1,12,'
058200971215     O                                              '1,1;'
058300971215     O                       §LDNC1        Z
058400971215     O                                              ' OF '
058500971215     O                       §LDNC2        Z
058600971215      *---*   I.A.T.A. ORIGINE
058700971215     O          E            LV22E       1
058800971215     O                                              '?02&10,500,655,2,1,12,'
058900971215     O                                              '1,1;'
059000971215     O                                              'ORIGIN:  '
059100971215     O                       §DESIP
059200971215     O********* Chiusura etichetta
059300971215     O          E            LV22E       1
059400971215     O                                              '?14&1'
059500971215     O          E            LV22E       1
059600971215     O                                              '?00&'
059700980318     O*-------------------------------------------------------
059800980318     O* ETICHETTA ESTERO EUROEXPRESS
059900980318     O*-------------------------------------------------------
060000980318     O          E            LV22Y            01
060100980318     O******                                 Inizializzazioni
060200980318     O                                            7 '?07&120'
060300980318     O          E            LV22Y       1
060400980318     O                                            5 '?13&2'
060500980318     O          E            LV22Y       1
060600980318     O                                            7 '?06&-90'
060700980318     O**      E 1              LV22Y
060800980318     O**                                  4 '?39&'
060900980318      *---*   NAZIONE PARTENZA
061000980318     O          E            LV22Y       1
061100980402     O                                              '?02&10,230,005,2,1,22,'
061200980318     O                                              '1,1;'
061300980318     O                       §DEISP
061400980318      *---*   CONTRASSEGNO
061500980318     O          E    14      LV22Y       1
061600980402     O                                              '?02&10,510,000,2,1,34,'
061700980318     O                                              '1,1;'
061800980318     O                                              'COD'
061900980318     O          E    14      LV22Y       1
062000980402     O                                              '?16&505,000,110,200,5'
062100980318      *---*   EUROEXPRESS
062200980318     O          E            LV22Y       1
062300980402     O                                              '?02&10,230,115,5,1,23,'
062400980318     O                                              '1,1;'
062500980318     O                                              'EuroExpress'
062600980318      *---*   NAZIONE ARRIVO
062700980318     O          E            LV22Y       1
062800980402     O                                              '?02&10,120,295,2,1,34,'
062900980318     O                                              '1,1;'
063000980318     O                       §DEISA
063100980318     O          E            LV22Y       1
063200980402     O                                              '?02&10,120,270,7,1,22,'
063300980318     O                                              '1,1;'
063400980318     O                                              'Country'
063500980318     O          E            LV22Y       1
063600980402     O                                              '?16&110,265,150,200,5'
063700980318      *---*   CAP DESTINO
063800980318     O          E            LV22Y       1
063900980402     O                                              '?02&10,345,295,2,1,34,'
064000980318     O                                              '1,1;'
064100980318     O                       §LDCAD
064200980318     O          E            LV22Y       1
064300980402     O                                              '?02&10,345,270,7,1,22,'
064400980318     O                                              '1,1;'
064500980318     O                                              'Post Code'
064600980318     O          E            LV22Y       1
064700980402     O                                              '?16&335,265,150,400,5'
064800011116      *---*   BARCODE EUROEXPRESS    *in23 *off
064900011116     O          E   n23      LV22Y       1
065000011116     O                                              '?11&3'
065100011116     O          E   n23      LV22Y       1
065200011116     O                                              '?02&13,120,440,14,200,'
065300011116     O                                              '1,1;'
065400011119     O                       alfa5
065500011119     O                       nume12
065600011119     o                       alfa2
065700011116      *---*   BARCODE EUROEXPRESS     *in23 *on
065800011116     O          E    23      LV22Y       1
065900980318     O                                              '?11&2'
066000011116     O          E    23      LV22Y       1
066100980402     O                                              '?02&13,120,440,14,200,'
066200980318     O                                              '1,1;'
066300980318     O                       §DEJEU
066400980318     O                       §DEISP
066500980318     O                       §LDLNP
066600980318     O                       §LDNRS
066700980318     O                       §LDNCD
066800980318     O                                              ' '
066900980318     O                       §DE55L
067000980319     O                       §BCEND
067100980318      *---*   RIFERIMENTI BARTOLINI
067200980318     O          E            LV22Y       1
067300980402     O                                              '?02&10,120,660,2,1,11,'
067400980318     O                                              '1,1;'
067500980318     O                       §LDLNP
067600980318     O                                              ' '
067700980318     O                       §LDNRS
067800980318     O                                              ' '
067900980318     O                       §LDNCD
068000980318     O********* Chiusura etchetta
068100980318     O          E            LV22Y       1
068200980318     O                                              '?14&1'
068300980318     O          E            LV22Y       1
068400980318     O                                              '?00&'
