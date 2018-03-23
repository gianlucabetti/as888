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
011900910403     C     §LDCDP        COMP      *BLANK                             18
012000910403     C     §LDNRS        COMP      *ZERO                              19
012100910403     C*
012200921106     C** INIZIALIZZA STAMPANTE
012300941216     C                   EXCEPT    LV22
012400910403     C*
012500910404     C                   MOVEA     *BLANK        §RI
012600910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
012700910403     C                   SETOFF                                       77
012800910403     C                   MOVE      *BLANK        §A7               7
012900910403     C                   Z-ADD     §LDNC1        §N7
013000910403     C                   DO        7             §1
013100910403     C     §S7(§1)       IFEQ      *ZERO
013200910403     C                   MOVE      *BLANK        §S7(§1)
013300910403     C                   ELSE
013400910403     C                   SETON                                        77
013500910403     C                   END
013600910403     C  N77              END
013700910403     C                   MOVEA     §S7           §A7
013800910403     C                   MOVE      §A7           §A3               3
013900910403     C                   MOVEA     §A3           §RI(26)
014000910403     C                   MOVEA     '-'           §RI(29)
014100910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
014200910403     C                   SETOFF                                       77
014300910403     C                   MOVE      *BLANK        §A7
014400910403     C                   Z-ADD     §LDNC2        §N7
014500910403     C                   DO        7             §1
014600910403     C     §S7(§1)       IFEQ      *ZERO
014700910403     C                   MOVE      *BLANK        §S7(§1)
014800910403     C                   ELSE
014900910403     C                   SETON                                        77
015000910403     C                   END
015100910403     C  N77              END
015200910403     C                   MOVEA     §S7           §A7
015300910403     C                   MOVE      §A7           §A3
015400910403     C                   MOVEA     §A3           §RI(30)
015500910403     C                   MOVE      §LDCPD        §A3
015600910404     C                   MOVEA     §A3           §RI(34)
015700910403     C* STAMPA LINEA PARTENZA
015800941216     C                   EXCEPT    LV22H
015900910403     C*
016000910404     C                   MOVEA     *BLANK        §RI
016100910403     C                   MOVEA     §LDRSM        §RI(8)
016200941216     C                   MOVEA     ' '           §RI(30)
016300941216     C                   MOVEA     §LDPRM        §RI(31)
016400950127     C                   MOVEA     *BLANK        §RI(33)
016500941216     C                   EXCEPT    LV22D1
016600910403     C* STAMPA DATI MITTENTE
016700910403     C*
016800910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
016900971222     C                   EXSR      COLZER
017000910403     C*
017100971223     C*
017200910403     C* STAMPA NUMERO SEGNACOLLO
017300941216     C                   EXCEPT    LV22D2
017400910403     C*
017500921106     C                   MOVE      §LDTNA        §A3               3
017600921106     C     '0':'O'       XLATE     §A3           §ODTNA            3
017700921106     C                   MOVE      §LDLNA        §A3
017800921106     C     '0':'O'       XLATE     §A3           §ODLNA            3
017900921106     C                   MOVE      §LDZSC        §A2
018000921106     C     '0':'O'       XLATE     §A2           §ODZSC            2
018100921106     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
018200921106     C                   MOVEL     §LDDLA        §ODDLA            8
018300910403     C* STAMPA LINEA ARRIVO
018400950712     C                   EXCEPT    LV22D3
018500910403     C*
018600910404     C                   MOVEA     *BLANK        §RI
018700910403     C                   MOVEA     §LDRSD        §RI(8)
018800941216     C                   MOVEA     ' '           §RI(30)
018900941216     C                   MOVEA     §LDPRD        §RI(31)
019000950127     C                   MOVEA     *BLANK        §RI(33)
019100910403     C* STAMPA 1° DATI DESTINATARIO
019200941216     C                   EXCEPT    LV22D4
019300910403     C*
019400910404     C                   MOVEA     *BLANK        §RI
019500910404     C                   MOVEA     §LDIND        §RI(8)
019600910404     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
019700910404     C                   Z-ADD     37            §Y                2 0    88
019800910404     C                   DO        36
019900910404     C                   SUB       1             §Y
020000910404     C     §RI(§Y)       IFNE      ' '
020100910404     C                   SETON                                        88
020200910404     C                   END
020300910404     C  N88              END
020400910404     C     §Y            IFLE      25
020500910404     C                   ADD       3             §Y
020600910404     C                   ELSE
020700910404     C                   MOVEA     '  '          §RI(26)
020800910404     C                   Z-ADD     28            §Y
020900910404     C                   END
021000910404     C                   MOVEA     §LDLOD        §RI(§Y)
021100950725     C                   MOVEA     *BLANK        §RI(33)
021200910403     C* STAMPA 2° DATI DESTINATARIO
021300941216     C                   EXCEPT    LV22D5
021400941216     C* STAMPA SERVIZIO
021500941216     C   16              EXCEPT    LV22X5
021600041126      *
021700910403     C*
021800910403     C* STAMPA BARCODE
021900920720     C*
022000950712     C                   EXCEPT    LV22D6
022100910403     C*
022200910410     C                   MOVEA     *BLANK        §RI
022300910403     C   18              DO
022400941216     C                   MOVEA     'Rif.:'       §RI(1)
022500980402     C     §DEEUR        IFNE      *BLANK
022600980402     C     §DETST        ANDEQ     'ITA'
022700980402     C                   MOVEA     'JUT :'       §RI(1)
022800980402     C                   ENDIF
022900941216     C                   MOVEA     §LDCDP        §RI(7)
023000910410     C                   END
023100041126      * STAMPA barra barcode
023200041126     C                   EXCEPT    LV22DX
023300041126     C                   EXCEPT    LV22DY
023400941216     C* STAMPA RIFERIMENTO/RISTAMPA
023500950712     C                   EXCEPT    LV22D7
023600910403     C*
023700910403     C* STAMPA CHIUSURA ETICHETTA
023800941216     C                   EXCEPT    LV22D8
023900971224     C                   ENDSR
024000971224     C/SPACE 005
024100971224     C*****************************************************
024200971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024300971224     C*****************************************************
024400971224     C     COLZER        BEGSR
024500971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024600971224     C                   SETOFF                                       77
024700971224     C                   MOVE      §LDNCD        §N7
024800971224     C                   DO        7             §1
024900971224     C     §S7(§1)       IFEQ      *ZERO
025000971224     C                   MOVE      *BLANK        §S7(§1)
025100971224     C                   ELSE
025200971224     C                   SETON                                        77
025300971224     C                   END
025400971224     C  N77              END
025500971224     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
025600971224     C                   MOVE      *ZERO         §X                1 0
025700971224     C                   MOVEA     *BLANK        §O7
025800971224     C                   DO        7             §1
025900971224     C     §S7(§1)       IFNE      *BLANK
026000971224     C                   ADD       1             §X
026100971224     C     §S7(§1)       IFEQ      '0'
026200971224     C                   MOVE      'O'           §O7(§X)
026300971224     C                   ELSE
026400971224     C                   MOVE      §S7(§1)       §O7(§X)
026500971224     C                   END
026600971224     C                   END
026700971224     C                   END
026800971224     C                   MOVEA     §O7           §S7
026900971224     C                   ENDSR
027000971222     C/SPACE 005
027100980318     C*****************************************************
027200980318     C* STAMPA EUROEXPRESS                              ***
027300980318     C*****************************************************
027400980318     C     EUREXP        BEGSR
027500980327     C*
027600980327     C* IMPOSTO COSTANTI
027700980327     C                   MOVEL     'JUT'         §DEJEU            3
027800980327     C                   MOVEL     '55L'         §DE55L            3
027900980327     C                   MOVEL     'IT'          §DEISP            2
028000011116      * controllo se contenuto 55L nel codice passato
028100011116     c     '55L'         scan      §deeur                                 23
028200011119      * se contenuto 55l scompongo a modo vecchio
028300011119     c                   if        *in23
028400980327     C* ESTRAGGO IL CAP:
028500980327     C                   MOVE      *ZEROS        §Y
028600980327     C                   MOVEA     §DEEUR        §EU
028700980327     C* TROVO QUANTO è LUNGO IL BARCODE
028800980327     C     ' '           CHECKR    §DEEUR        §Y
028900980327     C* ISOLO IL CAP
029000980327     C                   MOVE      *BLANK        §O9
029100980327     C                   MOVE      *ZEROS        §W                2 0
029200980327     C     §Y            SUB       2             §Y
029300980327     C     22            DO        §Y            §X1               2 0
029400980327     C     §EU(§X1)      IFNE      *BLANKS
029500980327     C                   ADD       1             §W
029600980327     C                   MOVE      §EU(§X1)      §O9(§W)
029700980327     C                   END
029800980327     C                   END
029900980318     C*
030000980318     C                   MOVEA     §O9           §LDCAD
030100011119
030200011119      *scompongo a modo nuovo senza 55l
030300011119     c                   else
030400011119     c                   movel     §deeur        §bceurex
030500011119     c                   end
030600980318     C*
030700980318     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
030800980318     C* DEL BARCODE
030900980319     C     §LDCAD        CAT       §DEISA:0      §BCEND           11
031000980318     C*
031100980318     C* VERIFICO SE CONTRASSEGNO
031200980318     C*
031300980318     C     §DEFCA        COMP      'S'                                    14
031400980318     C  N14§DEFCA        COMP      'X'                                    14
031500980318     C*
031600980318     C                   EXCEPT    LV22Y
031700980318     C*
031800980318     C                   ENDSR
031900910403     C/SPACE 005
032000910403     C*****************************************************
032100910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
032200910403     C*****************************************************
032300910403     C     M10           BEGSR
032400910403     C                   MOVE      *ZERO         §TT               3 0
032500910403     C                   MOVE      *ZERO         §TT2              3 0
032600910403     C                   MOVE      *ZERO         §1                1 0
032700910403     C                   Z-ADD     18            §S                2 0
032800910403     C                   DO        17            §N                2 0
032900910403     C                   SUB       1             §S
033000910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
033100910403     C                   MOVE      §C(§N)        §CB(§N)
033200910403     C     §N            DIV       2             §CTR              3 0
033300910403     C                   MVR                     §RST              3 010
033400910403     C     *IN(10)       IFEQ      '1'
033500910403     C     §C(§S)        MULT      2             §N2               2 0
033600910403     C                   MOVE      §N2           §A2               2
033700910403     C                   MOVEA     §A2           §2A
033800910403     C                   XFOOT     §2            §CTR
033900910403     C                   ELSE
034000910403     C                   Z-ADD     §C(§S)        §CTR
034100910403     C                   END
034200910403     C                   ADD       §CTR          §TT
034300910403     C                   END
034400910403     C*
034500910403     C     §TT           ADD       9             §TT2
034600910403     C                   MOVE      0             §TT2
034700910403     C     §TT2          SUB       §TT           §1
034800910403     C*
034900910403     C                   MOVE      §1            §CB(18)
035000910403     C                   ENDSR
035100040503      *---------------------------------------------------------------*
035200971215     OFNLV22P   E            LV22             01
035300950712     O******                                 Inizializzazioni
035400951010     O                                            7 '?07&120'
035500950712     O          E            LV22        1
035600950712     O                                            5 '?13&4'
035700950712     O          E            LV22        1
035800950713     O                                            7 '?06&-90'
035900950725     O**      E 1              LV22
036000950725     O**                                  4 '?39&'
036100910403     O********* Mittente
036200941216     O          E            LV22D1      1
036300041213     O                                           21 '?02&12,100,20,3,1,22,'
036400041213     O                                           25 '8,1;'
036500041213     O                       §RI                 62
036600910403     O********* Destinatario (Rag.Soc.)
036700941216     O          E            LV22D4      1
036800041213     O                                           18 '?15&205,48,405,2,6'
036900941216     O          E            LV22D4      1
037000041213     O                                           21 '?02&12,100,60,3,1,22,'
037100950725     O                                           25 '8,1;'
037200950725     O                       §RI                 62
037300910403     O********* Destinatario (Località)
037400941216     O          E            LV22D5      1
037500041213     O                                           21 '?02&12,100,92,3,1,22,'
037600950725     O                                           25 '8,1;'
037700950725     O                       §RI                 62
037800920221     O********* MC. SPEDIZIONE
037900941216     O          E            LV22D5      1
038000041213     O                                           22 '?02&12,485,128,2,1,11,'
038100950712     O                                           27 '12,1;'
038200950712     O                                           30 'MC.'
038300950712     O                       §LDVOL              37 '0 ,   '
038400950712     O                       §LDFVR              40
038500941216     O********* SERVIZIO
038600941216     O          E            LV22X5      1
038700041213     O                                           23 '?02&10,645,21,2,1,23,5,'
038800041213     O                                           25 '1;'
038900041213     O                       §LDTSP              26
039000950712     O          E            LV22X5      1
039100950714     O* FONDO NERO
039200041213     O                     17                    18 '?22&620,20,90,90,2'
039300950714     O* FONDO BIANCO
039400041213     O                    N17                    18 '?16&620,20,90,90,5'
039500910403     O********* Linea Partenza
039600950712     O**colli/padroncino
039700941216     O          E            LV22H       1
039800041213     O                                           22 '?02&12,262,156,2,1,11,'
039900950712     O                                           27 '12,1;'
040000950712     O                       §RI                 65
040100941216     O          E            LV22H       1
040200950712     O**Linea Partenza
040300041213     O                                           22 '?02&12,205,113,2,1,12,'
040400950712     O                                           27 '10,1;'
040500950714     O                       §LDLNP              30
040600950712     O                       §LDDLP              42
040700910403     O********* Segnacollo
040800941216     O          E    19      LV22D2      1
040900041213     O                                           22 '?02&10,110,205,2,1,25,'
041000950713     O                                           26 '5,1;'
041100950712     O                       §LDNRT              31
041200941216     O          E            LV22D2      1
041300041213     O                                           22 '?02&10,255,175,2,1,46,'
041400950712     O                                           26 '5,1;'
041500950712     O                       §S7                 33
041600921106     O********* TERMINAL
041700941216     O          E            LV22D3      1
041800950725     O                                           22 '?02&12,095,348,4,1,12,'
041900950712     O                                           26 '3,1;'
042000950714     O                       §ODTNA              29
042100910403     O* BOX X TERMINAL ARRIVO
042200941216     O          E            LV22D3      1
042300041126     O                                           21 '?22&089,343,111,105,2'
042400921106     O********* CODICE LINEA ARRIVO
042500941216     O          E            LV22D3      1
042600950725     O                                           22 '?02&12,216,348,4,1,12,'
042700950712     O                                           26 '3,1;'
042800950714     O                       §ODLNA              29
042900950712     O********* DESCRIZIONE LINEA DI ARRIVO
043000950712     O** ITALIA
043100040503     O          E   N55      LV22D3      1
043200950725     O                                           22 '?02&12,345,348,4,1,12,'
043300950712     O                                           26 '7,1;'
043400950712     O                       §ODDLA              34
043500921106     O********* ZONA ARRIVO
043600040503     O          E   N55      LV22D3      1
043700950725     O                                           22 '?02&12,626,348,4,1,12,'
043800950712     O                                           26 '2,1;'
043900950712     O                       §ODZSC              28
044000910403     O* BOX X ZONA ARRIVO
044100040503     O          E   N55      LV22D3      1
044200950713     O                                           20 '?22&615,343,89,107,2'
044300040503     O********* Nazione & CAP estero per EEX
044400040503     O          E    55      LV22D3      1
044500040503     O                                              '?02&12,345,348,4,1,12,'
044600040503     O                                          + 0 '7,1;'
044700040503     O                       §DEISA             + 0
044800040503     O                       §LDCAD             + 2
044900040503     O********* Box per nazione e cap estero EEX
045000040503     O          E    55      LV22D3      1
045100040503     O                                              '?16&335,345,91,300,4'
045200041126     O********* Barra chiusura superiore
045300041126     O          E            LV22DX      1
045400041213     O                                           19 '?15&160,455,382,2,8'
045500041126     O********* Barra chiusura inferiore
045600041126     O          E            LV22DY      1
045700041213     O                                           19 '?15&160,655,382,2,6'
045800910403     O********* Bar Code
045900941216     O          E            LV22D6      1
046000950712     O* RAPPORTO BARRA
046100950714     O                                            5 '?09&3'
046200950712     O          E            LV22D6      1
046300950712     O                                            5 '?10&1'
046400950712     O* LARGHEZZA ELEMENTO UNITARIO CODICE
046500950712     O          E            LV22D6      1
046600950714     O                                            5 '?11&3'
046700950712     O          E            LV22D6      1
046800950712     O* SOPPRESSIONE CARATTERI LEGGIBILI
046900950712     O                                            5 '?13&3'
047000041202      * 2/5
047100041202     O*         E            LV22D6      1
047200041202     O*                                          21 '?02&13,163,457,1,225,'
047300041202     O*                                          26 '18,1;'
047400041202     O*                      §CDBAR              44
047500041202     o*code 128
047600041202     O          E            LV22D6      1
047700041202     O                                           22 '?02&13,163,457,14,200,'
047800041202     O                                           27 '18,1;'
047900041202     O                       §CDBAR              45
048000910403     O********* Codice Prodotto
048100941216     O          E            LV22D7      1
048200041210     O                                           22 '?02&12,120,665,2,1,11,'
048300950712     O                                           27 '10,1;'
048400950712     O                       §RI                 64
048500920221     O********* KG. SPEDIZIONE
048600941216     O          E            LV22D7      1
048700041210     O                                           22 '?02&12,468,665,2,1,11,'
048800950712     O                                           27 '10,1;'
048900950712     O                                           30 'KG.'
049000950712     O                       §LDPKG              38 '    0 , '
049100971224     O                       §LDRST              43
049200941216     O********* Chiusura etchetta
049300941216     O          E            LV22D8      1
049400950712     O                                            5 '?14&1'
049500951010     O          E            LV22D8      1
049600951010     O                                            4 '?00&'
049700971215     O*----------------------------------------------------------------
049800971215     O* ETICHETTA ESTERO DHL
049900971215     O*----------------------------------------------------------------
050000971215     O          E            LV22E            01
050100971215     O******                                 Inizializzazioni
050200971215     O                                            7 '?07&120'
050300971215     O          E            LV22E       1
050400971215     O                                            5 '?13&4'
050500971215     O          E            LV22E       1
050600971215     O                                            7 '?06&-90'
050700971215     O**      E 1              LV22E
050800971215     O**                                  4 '?39&'
050900971215      *---*   TIPO PRODOTTO DHL -REX-
051000971215     O          E            LV22E       1
051100971215     O                                              '?02&10,300,0,0,1,23,'
051200971215     O                                              '1,1;'
051300971215     O                                              'PRODUCT:'
051400971215     O          E            LV22E       1
051500971215     O                                              '?16&415,0,110,280,3'
051600971215     O          E            LV22E       1
051700971215     O                                              '?02&10,440,10,6,1,11,'
051800971215     O                                              '1,1;'
051900971215     O                                              'REX'
052000971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
052100971215     O          E            LV22E       1
052200971215     O                                              '?02&10,120,210,0,1,23,'
052300971215     O                                              '1,1;'
052400971215     O                                              'DESTINATION:'
052500971215     O          E            LV22E       1
052600971215     O                                              '?02&10,120,225,4,1,13,'
052700971215     O                                              '1,1;'
052800971215     O                       §DESIA
052900971215     O* RAPPORTO BARRA
053000971215     O          E            LV22E       1
053100971215     O                                              '?09&3'
053200971215     O          E            LV22E       1
053300971215     O                                              '?10&1'
053400971215     O* LARGHEZZA ELEMENTO UNITARIO CODICE
053500971215     O          E            LV22E       1
053600971215     O                                              '?11&3'
053700971215     O          E            LV22E       1
053800971215     O* SOPPRESSIONE CARATTERI LEGGIBILI
053900971215     O                                              '?13&3'
054000971215     O          E            LV22E       1
054100971215     O                                              '?02&13,290,130,6,200,'
054200971215     O                                              '1,1;'
054300971215     O                       §DESIP
054400971215     O                       §DESIA
054500971215     O                       §DESSE
054600971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
054700971215     O          E            LV22E       1
054800971215     O                                              '?02&10,120,360,0,1,23,'
054900971215     O                                              '1,1;'
055000971215     O                                              'AWB:'
055100971215     O          E            LV22E       1
055200971215     O                                              '?02&10,300,335,2,1,22,'
055300971215     O                                              '1,1;'
055400971215     O                       §DEAWB
055500971215     O          E            LV22E       1
055600971215     O                                              '?02&13,145,405,6,200,'
055700971215     O                                              '1,1;'
055800971215     O                       §DEAWB
055900971215      *---*   NAZIONE DESTINO -CODICE ISO-
056000971215     O          E            LV22E       1
056100971215     O                                              '?02&10,120,605,4,1,13,'
056200971215     O                                              '1,1;'
056300971215     O                       §DEISA
056400971215      *---*
056500971215     O          E            LV22E       1
056600971215     O                                              '?02&10,260,605,2,1,12,'
056700971215     O                                              '1,1;'
056800971215     O                                              'P: '
056900971215     O                       §LDLNP
057000971215     O                                              ' A: '
057100971215     O                       §LDLNA
057200971215     O          E            LV22E       1
057300971215     O                                              '?02&10,260,655,2,1,12,'
057400971215     O                                              '1,1;'
057500971215     O                                              'N. '
057600971215     O                       §LDNRT
057700971215     O                       §S7
057800971215      *---*   SEQUENZA COLLO   (XX OF XX)
057900971215     O          E            LV22E       1
058000971215     O                                              '?02&10,500,605,2,1,12,'
058100971215     O                                              '1,1;'
058200971215     O                       §LDNC1        Z
058300971215     O                                              ' OF '
058400971215     O                       §LDNC2        Z
058500971215      *---*   I.A.T.A. ORIGINE
058600971215     O          E            LV22E       1
058700971215     O                                              '?02&10,500,655,2,1,12,'
058800971215     O                                              '1,1;'
058900971215     O                                              'ORIGIN:  '
059000971215     O                       §DESIP
059100971215     O********* Chiusura etichetta
059200971215     O          E            LV22E       1
059300971215     O                                              '?14&1'
059400971215     O          E            LV22E       1
059500971215     O                                              '?00&'
059600980318     O*-------------------------------------------------------
059700980318     O* ETICHETTA ESTERO EUROEXPRESS
059800980318     O*-------------------------------------------------------
059900980318     O          E            LV22Y            01
060000980318     O******                                 Inizializzazioni
060100980318     O                                            7 '?07&120'
060200980318     O          E            LV22Y       1
060300980318     O                                            5 '?13&2'
060400980318     O          E            LV22Y       1
060500980318     O                                            7 '?06&-90'
060600980318     O**      E 1              LV22Y
060700980318     O**                                  4 '?39&'
060800980318      *---*   NAZIONE PARTENZA
060900980318     O          E            LV22Y       1
061000980402     O                                              '?02&10,230,005,2,1,22,'
061100980318     O                                              '1,1;'
061200980318     O                       §DEISP
061300980318      *---*   CONTRASSEGNO
061400980318     O          E    14      LV22Y       1
061500980402     O                                              '?02&10,510,000,2,1,34,'
061600980318     O                                              '1,1;'
061700980318     O                                              'COD'
061800980318     O          E    14      LV22Y       1
061900980402     O                                              '?16&505,000,110,200,5'
062000980318      *---*   EUROEXPRESS
062100980318     O          E            LV22Y       1
062200980402     O                                              '?02&10,230,115,5,1,23,'
062300980318     O                                              '1,1;'
062400980318     O                                              'EuroExpress'
062500980318      *---*   NAZIONE ARRIVO
062600980318     O          E            LV22Y       1
062700980402     O                                              '?02&10,120,295,2,1,34,'
062800980318     O                                              '1,1;'
062900980318     O                       §DEISA
063000980318     O          E            LV22Y       1
063100980402     O                                              '?02&10,120,270,7,1,22,'
063200980318     O                                              '1,1;'
063300980318     O                                              'Country'
063400980318     O          E            LV22Y       1
063500980402     O                                              '?16&110,265,150,200,5'
063600980318      *---*   CAP DESTINO
063700980318     O          E            LV22Y       1
063800980402     O                                              '?02&10,345,295,2,1,34,'
063900980318     O                                              '1,1;'
064000980318     O                       §LDCAD
064100980318     O          E            LV22Y       1
064200980402     O                                              '?02&10,345,270,7,1,22,'
064300980318     O                                              '1,1;'
064400980318     O                                              'Post Code'
064500980318     O          E            LV22Y       1
064600980402     O                                              '?16&335,265,150,400,5'
064700011116      *---*   BARCODE EUROEXPRESS    *in23 *off
064800011116     O          E   n23      LV22Y       1
064900011116     O                                              '?11&3'
065000011116     O          E   n23      LV22Y       1
065100011116     O                                              '?02&13,120,440,14,200,'
065200011116     O                                              '1,1;'
065300011119     O                       alfa5
065400011119     O                       nume12
065500011119     o                       alfa2
065600011116      *---*   BARCODE EUROEXPRESS     *in23 *on
065700011116     O          E    23      LV22Y       1
065800980318     O                                              '?11&2'
065900011116     O          E    23      LV22Y       1
066000980402     O                                              '?02&13,120,440,14,200,'
066100980318     O                                              '1,1;'
066200980318     O                       §DEJEU
066300980318     O                       §DEISP
066400980318     O                       §LDLNP
066500980318     O                       §LDNRS
066600980318     O                       §LDNCD
066700980318     O                                              ' '
066800980318     O                       §DE55L
066900980319     O                       §BCEND
067000980318      *---*   RIFERIMENTI BARTOLINI
067100980318     O          E            LV22Y       1
067200980402     O                                              '?02&10,120,660,2,1,11,'
067300980318     O                                              '1,1;'
067400980318     O                       §LDLNP
067500980318     O                                              ' '
067600980318     O                       §LDNRS
067700980318     O                                              ' '
067800980318     O                       §LDNCD
067900980318     O********* Chiusura etchetta
068000980318     O          E            LV22Y       1
068100980318     O                                              '?14&1'
068200980318     O          E            LV22Y       1
068300980318     O                                              '?00&'
