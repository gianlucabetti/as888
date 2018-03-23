000100050603     H DECEDIT('0,') DATEDIT(*DMY.)
000200970221     F* FNLV22RQ *---------------------------------------------------*
000300910403     F*-----------------*                                            *
000400970221     F* STAMPA SEGNACOLLO CON BARCODE A 18 CON RJS  QB440            *
000500921120     F*        CON BARCODE 3X70                                      *
000600970221     F*   VBR DIMINUITO DI 12 RISPETTO ALLA 240/340                  *
000700910403     F*--------------------------------------------------------------*
000800941216     FFNLV22P   O    F  132        PRINTER
000900910404     D §RI             S              1    DIM(37)
001000951020     D §RX             S              1    DIM(11)
001100910403     D §O7             S              1    DIM(7)
001200980129     D §S9             S              1    DIM(9)
001300980129     D §O9             S              1    DIM(9)
001400980321     D §EU             S              1    DIM(35)
001500910403     D                 DS
001600910403     D  §CB                    1     18  0
001700910403     D                                     DIM(18)
001800910403     D  §CA                    1     18
001900910403     D                                     DIM(18)
002000910403     D                 DS
002100910403     D  §N7                    1      7  0
002200910403     D  §S7                    1      7
002300910403     D                                     DIM(7)
002400910403     D                 DS
002500910403     D  §2                     1      2  0
002600910403     D                                     DIM(2)
002700910403     D  §2A                    1      2
002800910403     D                                     DIM(2)
002900910403     D DSBLD         E DS
003000971215     D DSBLDE        E DS
003100910403     D DSBLD7        E DS
003200910403     D  §C                     1     17  0
003300910403     D                                     DIM(17)
003400011119     D §bceurex        DS
003500011119     D  alfa5                         5
003600011119     D  nume12                       12  0
003700011119     D  alfa2                         2
003800000121     D §dpd            DS
003900000121     D  §dpd3                         3
004000000121     D  §dpd8                         8
004100000121     D  §dpd12                        1
004200000121     D §deawbds        DS
004300000121     D  §dpdc1w                       2
004400000121     D  §dpdc3w                       5
004500000121     D  §dpdc5w                       2
004600000121     D  §dpdc7w                       1
004700000124     D §dexx1ds        DS
004800000124     D  §dex1                         5
004900000124     D  §dex2                         5
005000000124     D  §dex3                         3
005100000124     D  §dex4                         2
005200000121     D §dpdcap         DS
005300000121     D  §dpdc1                        2
005400000121     D  §dpdc2                        1    inz('-')
005500000121     D  §dpdc3                        5
005600000121     D  §dpdc4                        1    inz('-')
005700000121     D  §dpdc5                        2
005800000121     D  §dpdc6                        1    inz('-')
005900000121     D  §dpdc7                        1
006000000121     d §dpdn           ds
006100000124     d §dpdn1                        38
006200000124     d §dpdn2                        38
006300000124     d §dpdn3                        38
006400000124     d §dpdn4                        39
006500000121     d dpd             c                   const('DPD')
006600000121     d dpd1            c                   const('Deutscher Paket Dienst')
006700000121     d dpd3            c                   const('Sender:')
006800000121     d dpd4            c                   const('Receiver:')
006900000121     d dpd2            ds
007000000121     d  dpdtel                        3    inz('Tel')
007100000121     d  dpdteln                      15
007200000121     d dpd2f           ds
007300000121     d  dpdfax                        3    inz('Fax')
007400000121     d  dpdfaxn                      15
007500000121     d dpd9            c                   const('01')
007600910403     I/SPACE 3
007700910403     C*---------------------------------------------------------------*
007800910403     C     *ENTRY        PLIST
007900910403     C                   PARM                    DSBLD
008000971215     C                   PARM                    DSBLDE
008100050603      *
008200050603      * Elimino carattere "\"
008300050603     c     '\':' '       xlate     §ldrsm        §ldrsm
008400050603     c     '\':' '       xlate     §ldrsd        §ldrsd
008500050603     c     '\':' '       xlate     §ldlod        §ldlod
008600050603     c     '\':' '       xlate     §ldind        §ldind
008700050603     c     '\':' '       xlate     §ldcdp        §ldcdp
008800050603      *
008900971217     C                   MOVEL     §LDNRS        §LDNRT            3
009000971217     C                   MOVE      '-'           §LDNRT
009100971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
009200971217     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
009300971217     C     §DETST        IFEQ      'EST'
009400971217     C                   EXSR      COLZER
009500971217     C                   ENDIF
009600971215     C*
009700980129     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
009800971215     C*
009900971215     C     §DETST        IFEQ      'EST'
010000971215     C     §DETST        OREQ      'ALL'
010100000121     c                   select
010200000121     c     §detse        wheneq    'REX'
010300000121     c                   except    lv22e
010400000121     c     §detse        wheneq    'EUR'
010500000121     c                   exsr      eurexp
010600000121     c     §detse        wheneq    'DP1'
010700000121     c                   exsr      dpddove
010800000121     c     §detse        wheneq    'DP2'
010900000121     c                   exsr      dpddove
011000000121     c                   exsr      dpdchi
011100000121     c                   endsl
011200971215     C                   ENDIF
011300000202     C     §DETST        IFEQ      'ITA'
011400000202     C     §DETST        OREQ      'ALL'
011500000202     C                   EXSR      PRINT
011600000202     C                   ENDIF
011700971215     C*
011800910403     C                   SETON                                        LR
011900951021     C/SPACE 005
012000951021     C*****************************************************
012100951023     C* IMPOSTAZIONE E STAMPA                           ***
012200951021     C*****************************************************
012300951023     C     PRINT         BEGSR
012400951023     C                   MOVE      §LDLNP        §LCLNP
012500951023     C                   MOVE      §LDLNA        §LCLNA
012600951023     C                   MOVE      §LDNRS        §LCNRS
012700951023     C                   MOVE      §LDNCD        §LCNCD
012800951023     C                   MOVE      §LDZSC        §LCZSC
012900040430     c     §detse        comp      'EEX'                                  55
013000951023     C*
013100951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
013200951023     C                   EXSR      M10
013300951023     C                   MOVEA     §CA           §C18A            18
013400951023     C                   MOVE      §C18A         §CDBAR           18
013500951023     C*
013600970221     C     §LDTSP        IFEQ      'D'
013700970303     C     §LDLNA        ANDLT     300
013800970221     C                   MOVE      *BLANK        §LDTSP
013900970221     C                   END
014000951023     C*
014100040430     ***C     §LDLNA        COMP      300                                15
014200951023     C     §LDTSP        COMP      *BLANK                             1616
014300951023     C     §LDTSP        COMP      'E'                                    17
014400951023     C     §LDCDP        COMP      *BLANK                             18
014500951023     C     §LDNRS        COMP      *ZERO                              19
014600951023     C*
014700951023     C** INIZIALIZZA STAMPANTE
014800951023     C                   EXCEPT    LV22
014900951023     C*
015000951023     C                   MOVEA     *BLANK        §RX
015100951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
015200951023     C                   SETOFF                                       77
015300951023     C                   MOVE      *BLANK        §A7               7
015400951023     C                   Z-ADD     §LDNC1        §N7
015500951023     C                   DO        7             §1
015600951023     C     §S7(§1)       IFEQ      *ZERO
015700951023     C                   MOVE      *BLANK        §S7(§1)
015800951023     C                   ELSE
015900951023     C                   SETON                                        77
016000951023     C                   END
016100951023     C  N77              END
016200951023     C                   MOVEA     §S7           §A7
016300951023     C                   MOVE      §A7           §A3               3
016400951023     C                   MOVEA     §A3           §RX(1)
016500951023     C                   MOVEA     '-'           §RX(4)
016600951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
016700951023     C                   SETOFF                                       77
016800951023     C                   MOVE      *BLANK        §A7
016900951023     C                   Z-ADD     §LDNC2        §N7
017000951023     C                   DO        7             §1
017100951023     C     §S7(§1)       IFEQ      *ZERO
017200951023     C                   MOVE      *BLANK        §S7(§1)
017300951023     C                   ELSE
017400951023     C                   SETON                                        77
017500951023     C                   END
017600951023     C  N77              END
017700951023     C                   MOVEA     §S7           §A7
017800951023     C                   MOVE      §A7           §A3
017900951023     C                   MOVEA     §A3           §RX(5)
018000951023     C                   MOVE      §LDCPD        §A3
018100951023     C                   MOVEA     §A3           §RX(9)
018200951023     C* STAMPA LINEA PARTENZA
018300951023     C                   EXCEPT    LV22H
018400951023     C*
018500951023     C                   MOVEA     *BLANK        §RI
018600951023     C                   MOVEA     §LDRSM        §RI(8)
018700951023     C                   MOVEA     ' '           §RI(30)
018800951023     C                   MOVEA     §LDPRM        §RI(31)
018900951023     C                   MOVEA     *BLANK        §RI(33)
019000951023     C                   EXCEPT    LV22D1
019100951023     C* STAMPA DATI MITTENTE
019200951023     C*
019300971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
019400971217     C                   EXSR      COLZER
019500971217     C*
019600951023     C*
019700951023     C* STAMPA NUMERO SEGNACOLLO
019800951023     C                   EXCEPT    LV22D2
019900951023     C*
020000951023     C                   MOVE      §LDTNA        §A3               3
020100951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
020200951023     C                   MOVE      §LDLNA        §A3
020300951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
020400951023     C                   MOVE      §LDZSC        §A2
020500951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
020600951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
020700951023     C                   MOVEL     §LDDLA        §ODDLA            8
020800951023     C* STAMPA LINEA ARRIVO
020900040430     ***C* X 15 STAMPA A "MODOD ESTERO"
021000040430     ***C   15              DO
021100040430     ***C                   MOVEL     §ODDLA        §EEN              3
021200040430     ***C                   MOVEL     §LDCAD        §CA2              2
021300040430     ***C                   MOVE      §LDCAD        §CA7              7
021400040430     ***C                   END
021500951023     C                   EXCEPT    LV22D3
021600951023     C*
021700951023     C                   MOVEA     *BLANK        §RI
021800951023     C                   MOVEA     §LDRSD        §RI(8)
021900951023     C                   MOVEA     ' '           §RI(30)
022000951023     C                   MOVEA     §LDPRD        §RI(31)
022100951023     C                   MOVEA     *BLANK        §RI(33)
022200951023     C* STAMPA 1° DATI DESTINATARIO
022300951023     C                   EXCEPT    LV22D4
022400951023     C*
022500951023     C                   MOVEA     *BLANK        §RI
022600951023     C                   MOVEA     §LDIND        §RI(8)
022700951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
022800951023     C                   Z-ADD     37            §Y                2 0    88
022900951023     C                   DO        36
023000951023     C                   SUB       1             §Y
023100951023     C     §RI(§Y)       IFNE      ' '
023200951023     C                   SETON                                        88
023300951023     C                   END
023400951023     C  N88              END
023500951023     C     §Y            IFLE      25
023600951023     C                   ADD       3             §Y
023700951023     C                   ELSE
023800951023     C                   MOVEA     '  '          §RI(26)
023900951023     C                   Z-ADD     28            §Y
024000951023     C                   END
024100951023     C                   MOVEA     §LDLOD        §RI(§Y)
024200951023     C* STAMPA 2° DATI DESTINATARIO
024300951023     C                   EXCEPT    LV22D5
024400951023     C* STAMPA SERVIZIO
024500951023     C   16              EXCEPT    LV22X5
024600951023     C*
024700951023     C* STAMPA BARCODE
024800951023     C*
024900951023     C* (CAMPO UNICO BARCODE E COSTANTI)
025000951023     C                   MOVE      *BLANK        §§CDB1           19
025100951023     C                   MOVE      §CDBAR        §§CDB1
025200951023     C                   MOVEL     '"'           §§CDB1
025300951023     C                   MOVE      *BLANK        §§CDB2           21
025400951023     C                   MOVEL     §§CDB1        §§CDB2
025500951023     C                   MOVE      '";'          §§CDB2
025600951023     C                   EXCEPT    LV22D6
025700951023     C*
025800951023     C                   MOVE      *BLANK        §RI21            21
025900951023     C   18              DO
026000951023     C                   MOVEL     'Rif.:'       §RI21
026100980402     C     §DEEUR        IFNE      *BLANK
026200980402     C     §DETST        ANDEQ     'ITA'
026300980402     C                   MOVEL     'JUT :'       §RI21
026400980402     C                   ENDIF
026500951023     C                   MOVE      §LDCDP        §RI21
026600951023     C                   END
026700951023     C* STAMPA RIFERIMENTO/RISTAMPA
026800951023     C                   EXCEPT    LV22D7
026900951023     C*
027000951023     C* STAMPA CHIUSURA ETICHETTA
027100951023     C                   EXCEPT    LV22D8
027200951023     C                   ENDSR
027300971217     C/SPACE 005
027400971217     C*****************************************************
027500971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027600971217     C*****************************************************
027700971217     C     COLZER        BEGSR
027800971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027900971217     C                   SETOFF                                       77
028000971217     C                   MOVE      §LDNCD        §N7
028100971217     C                   DO        7             §1
028200971217     C     §S7(§1)       IFEQ      *ZERO
028300971217     C                   MOVE      *BLANK        §S7(§1)
028400971217     C                   ELSE
028500971217     C                   SETON                                        77
028600971217     C                   END
028700971217     C  N77              END
028800971217     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
028900971217     C                   MOVE      *ZERO         §X                1 0
029000971217     C                   MOVEA     *BLANK        §O7
029100971217     C                   DO        7             §1
029200971217     C     §S7(§1)       IFNE      *BLANK
029300971217     C                   ADD       1             §X
029400971217     C     §S7(§1)       IFEQ      '0'
029500971217     C                   MOVE      'O'           §O7(§X)
029600971217     C                   ELSE
029700971217     C                   MOVE      §S7(§1)       §O7(§X)
029800971217     C                   END
029900971217     C                   END
030000971217     C                   END
030100971217     C                   MOVEA     §O7           §S7
030200971217     C                   ENDSR
030300951023     C/SPACE 005
030400951023     C*****************************************************
030500980129     C* STAMPA EUROEXPRESS                              ***
030600951023     C*****************************************************
030700980129     C     EUREXP        BEGSR
030800980321     C*
030900980321     C* IMPOSTO COSTANTI
031000980321     C                   MOVEL     'JUT'         §DEJEU            3
031100980321     C                   MOVEL     '55L'         §DE55L            3
031200980321     C                   MOVEL     'IT'          §DEISP            2
031300011116      * controllo se contenuto 55L nel codice passato
031400011116     c     '55L'         scan      §deeur                                 23
031500011119      * se contenuto 55l scompongo a modo vecchio
031600011119     c                   if        *in23
031700980321     C* ESTRAGGO IL CAP:
031800980321     C                   MOVE      *ZEROS        §Y
031900980321     C                   MOVEA     §DEEUR        §EU
032000980321     C* TROVO QUANTO è LUNGO IL BARCODE
032100980321     C     ' '           CHECKR    §DEEUR        §Y
032200980321     C* ISOLO IL CAP
032300980321     C                   MOVE      *BLANK        §O9
032400980321     C                   MOVE      *ZEROS        §W                2 0
032500980321     C     §Y            SUB       2             §Y
032600980321     C     22            DO        §Y            §X1               2 0
032700980321     C     §EU(§X1)      IFNE      *BLANKS
032800980321     C                   ADD       1             §W
032900980321     C                   MOVE      §EU(§X1)      §O9(§W)
033000980321     C                   END
033100980321     C                   END
033200980129     C*
033300980129     C                   MOVEA     §O9           §LDCAD
033400011119
033500011119      *scompongo a modo nuovo senza 55l
033600011119     c                   else
033700011119     c                   movel     §deeur        §bceurex
033800011119     c                   end
033900980129     C*
034000980129     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
034100980129     C* DEL BARCODE
034200980129     C                   MOVEL     '";BSTP;'     §END              7
034300980129     C     §LDCAD        CAT       §DEISA:0      §BCD             11
034400980129     C     §BCD          CAT       §END:0        §BCEND           20
034500980318     C*
034600980318     C* VERIFICO SE CONTRASSEGNO
034700980318     C*
034800980318     C     §DEFCA        COMP      'S'                                    14
034900980318     C  N14§DEFCA        COMP      'X'                                    14
035000990323     C*
035100990323     C* VERIFICO SE INGHILTERRA
035200990323     C*
035300990323     C     §DEISA        COMP      'GB'                                   13
035400980129     C*
035500980129     C                   EXCEPT    LV22Y
035600980129     C*
035700951021     C                   ENDSR
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
039000000121      *--------------------------------------------------
039100000121     c     dpddove       begsr
039200000121      *--------------------------------------------------
039300000121     c                   eval      §dpd = §deeur
039400000124     c                   eval      §dexx1ds = §dexx1
039500000121     c                   eval      §deawbds = §deawb
039600000121     c                   eval      §dpdc1 = §dpdc1w
039700000121     c                   eval      §dpdc3 = §dpdc3w
039800000121     c                   eval      §dpdc5 = §dpdc5w
039900000121     c                   eval      §dpdc7 = §dpdc7w
040000000121     c                   eval      §dpdn = 'Notifications on damages which are -
040100000121     c                             not recognizable from the outside have to -
040200000121     c                             have to be submitted to the transport -
040300000121     c                             organisation withing 7 days in written.'
040400000121     C                   except    LV22dpd1
040500000121     C                   except    LV22dpd
040600000121     C                   endsr
040700000121      *--------------------------------------------------
040800000121     c     dpdchi        begsr
040900000121      *--------------------------------------------------
041000000121     c                   move      §ldmgs        giorno            2 0
041100000121     c                   movel     §ldmgs        mese              2 0
041200000121     c                   move      mese          giomes            4 0
041300000121     c                   movel     giorno        giomes
041400000121     c                   movel     giomes        datasp            8 0
041500000121     c                   move      §ldaas        datasp
041600000121     c                   movel     §dexx2        dpdteln
041700000121     c                   move      §dexx2        dpdfaxn
041800000121     C                   except    LV22dpd2
041900000121     C                   except    LV22dpd
042000000121     C                   endsr
042100040430      *--------------------------------------------------
042200941216     OFNLV22P   E            LV22        1
042300931008     O******                                 inizializzazione NUOVA
042400040430     O**                                         20 '&&??%%P%%7E%^"LV22";'
042500951020     O******                                 inizializzazione VIDEO
042600961223     O                                            9 '~^"LV22";'
042700941216     O          E            LV22        1
042800941228     O*LOGICA                                N.ET. V (H) O=-35 X TB340
042900941228     O*                                                  O=000 X TX240
043000961223     O*                                                  O=073 X QB440
043100941216     O*                                                  O=-17 X TUTTI
043200961223     O                                           16 '00001;0;389;073;'
043300941216     O          E            LV22        1
043400910403     O*FISICA
043500961223     O** X QB400
043600961223     O                                           21 'UTOF;4100;TEAR;0;SPB;'
043700961223     O** X TX340/240
043800040430     O*                                          14 'UTOF;4100;SPB;'
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
045300970221     O                                           16 'VBR;016;HBR;060;'
045400941216     O          E            LV22D1      1
045500910403     O                                            1 '"'
045600910404     O                       §RI                 38
045700920910     O                                           40 '";'
045800910403     O********* Destinatario (Rag.Soc.)
045900941216     O          E            LV22D4      1
046000910403     O*                         RIGA DI SEPARAZIONE:
046100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
046200970221     O                                           16 'VBR;020;HBR;058;'
046300941216     O          E            LV22D4      1
046400040511     O*****                                       6 'HLT;3;'
046500040511     O                                            6 'HLT;2;'
046600941216     O          E            LV22D4      1
046700040430     O*                                       RIGA        O   V   L
046800941216     O                                           16 'DHL;117;000;390;'
046900941216     O          E            LV22D4      1
047000910403     O*                                       CARATTERE
047100910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
047200941216     O          E            LV22D4      1
047300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
047400970221     O                                           16 'VBR;036;HBR;060;'
047500941216     O          E            LV22D4      1
047600910403     O                                            1 '"'
047700910404     O                       §RI                 38
047800910404     O                                           40 '";'
047900910403     O********* Destinatario (Località)
048000941216     O          E            LV22D5      1
048100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048200970221     O                                           16 'VBR;053;HBR;060;'
048300941216     O          E            LV22D5      1
048400910403     O                                            1 '"'
048500910404     O                       §RI                 38
048600910404     O                                           40 '";'
048700920221     O********* MC. SPEDIZIONE
048800941216     O          E            LV22D5      1
048900920221     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049000970221     O                                           16 'VBR;067;HBR;465;'
049100941216     O          E            LV22D5      1
049200921104     O                                            1 '"'
049300951021     O                                            4 'MC.'
049400951021     O                       §LDVOL              11 '0 ,   '
049500951021     O                       §LDFVR              14
049600951021     O                                           16 '";'
049700941216     O********* SERVIZIO
049800941216     O          E            LV22X5      1
049900921104     O*                                       CARATTERE
050000921104     O                                           22 'DCH;45;DCW;085;ICS;03;'
050100941216     O          E            LV22X5      1
050200921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050300970221     O                                           16 'VBR;035;HBR;600;'
050400941216     O          E            LV22X5      1
050500941216     O                                            1 '"'
050600941216     O                       §LDTSP               2
050700941216     O                                            4 '";'
050800941216     O          E            LV22X5      1
050900970221     O                                           21 'HOME;VBR;000;HBR;575;'
051000941216     O          E            LV22X5      1
051100040430     O* FONDO NERO                                             L   H
051200941216     O                     17                    16 'DCBX;0;0;100;40;'
051300040430     O* FONDO BIANCO                                           L   H
051400941216     O                    N17                    16 'DBOX;0;0;100;40;'
051500910403     O********* Linea Partenza
051600941216     O          E            LV22H       1
051700910403     O*                                       CARATTERE
051800910403     O                                           23 'DCH;015;DCW;026;ICS;03;'
051900941216     O          E            LV22H       1
052000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052100970221     O                                           16 'VBR;081;HBR;450;'
052200941216     O          E            LV22H       1
052300910403     O                                            1 '"'
052400951021     O                       §RX                 12
052500951021     O                                           14 '";'
052600941216     O          E            LV22H       1
052700910403     O*                                       CARATTERE
052800910403     O                                           23 'DCH;030;DCW;030;ICS;03;'
052900941216     O          E            LV22H       1
053000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053100970221     O                                           16 'VBR;081;HBR;170;'
053200941216     O          E            LV22H       1
053300910403     O                                            1 '"'
053400910403     O                       §LDLNP               4
053500910403     O                                            6 '";'
053600941216     O          E            LV22H       1
053700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053800970221     O                                           16 'VBR;081;HBR;245;'
053900941216     O          E            LV22H       1
054000910403     O                                            1 '"'
054100910403     O                       §LDDLP              11
054200910403     O                                           13 '";'
054300910403     O********* Segnacollo
054400941216     O          E    19      LV22D2      1
054500910403     O*                                       CARATTERE
054600921104     O                                           23 'DCH;080;DCW;050;ICS;08;'
054700941216     O          E    19      LV22D2      1
054800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054900040511     O*****                                      16 'VBR;168;HBR;080;'
055000040511     O                                           16 'VBR;163;HBR;080;'
055100941216     O          E    19      LV22D2      1
055200910403     O                                            1 '"'
055300920910     O                       §LDNRT               4
055400920910     O                                            6 '";'
055500941216     O          E            LV22D2      1
055600910403     O*                                       CARATTERE
055700921105     O                                           23 'DCH;110;DCW;105;ICS;12;'
055800941216     O          E            LV22D2      1
055900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
056000970221     O                                           16 'VBR;163;HBR;205;'
056100941216     O          E            LV22D2      1
056200910403     O                                            1 '"'
056300910403     O                       §S7                  8
056400910403     O                                           10 '";'
056500921106     O********* TERMINAL/LINEA/ZONA ARRIVO
056600941216     O          E            LV22D3      1
056700921106     O                                           14 'HLT;05;VLT;08;'
056800941216     O          E            LV22D3      1
056900910403     O*                                       CARATTERE
057000921106     O                                           21 'DCH;75;DCW;70;ICS;08;'
057100921106     O********* TERMINAL
057200941216     O          E            LV22D3      1
057300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057400970221     O                                           16 'VBR;228;HBR;060;'
057500941216     O          E            LV22D3      1
057600910403     O                                            1 '"'
057700921106     O                       §ODTNA               4
057800921106     O                                            6 '";'
057900910403     O* BOX X TERMINAL ARRIVO
058000941216     O          E            LV22D3      1
058100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058200970221     O                                           21 'HOME;VBR;173;HBR;046;'
058300941216     O          E            LV22D3      1
058400921106     O* FONDO NERO                                     L   H
058500921106     O                                           16 'DCBX;0;0;147;60;'
058600921106     O* FONDO BIANCO                                   L   H
058700040511     O*                                          16 'DBOX;0;0;147;60;'
058800921106     O********* CODICE LINEA ARRIVO
058900941216     O          E            LV22D3      1
059000921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059100970221     O                                           16 'VBR;228;HBR;207;'
059200941216     O          E            LV22D3      1
059300921106     O                                            1 '"'
059400921106     O                       §ODLNA               4
059500921106     O                                            6 '";'
059600921106     O********* ZONA ARRIVO
059700040430     O          E   N55      LV22D3      1
059800921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059900970221     O                                           16 'VBR;228;HBR;592;'
060000040430     O          E   N55      LV22D3      1
060100921106     O                                            1 '"'
060200921106     O                       §ODZSC               3
060300921106     O                                            5 '";'
060400910403     O* BOX X ZONA ARRIVO
060500040430     O          E   N55      LV22D3      1
060600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060700970221     O                                           21 'HOME;VBR;173;HBR;582;'
060800040430     O          E   N55      LV22D3      1
060900921106     O* FONDO NERO                                     L   H
061000921106     O                                           16 'DCBX;0;0;102;60;'
061100921106     O* FONDO BIANCO                                   L   H
061200040430     O*                                          16 'DBOX;0;0;102;60;'
061300921106     O********* DESCRIZIONE LINEA DI ARRIVO
061400040430     O          E   N55      LV22D3      1
061500921106     O                                           14 'HLT;05;VLT;06;'
061600040430     O          E   N55      LV22D3      1
061700921106     O*                                       CARATTERE
061800921106     O                                           21 'DCH;75;DCW;45;ICS;04;'
061900040430     O          E   N55      LV22D3      1
062000921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
062100970221     O                                           16 'VBR;228;HBR;355;'
062200040430     O          E   N55      LV22D3      1
062300921106     O                                            1 '"'
062400921106     O                       §ODDLA               9
062500921106     O                                           11 '";'
062600040430     O********* Nazione per EEX
062700040430     O          E    55      LV22D3      1
062800040430     O*                         CARATTERE
062900040430     O                                              'DCH;065;DCW;065;ICS;08;'
063000040430     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
063100040511     O                                              'VBR;225;HBR;350;'
063200040430     O          E    55      LV22D3      1
063300040430     O                                              '"'
063400040430     O                       §DEISA
063500040430     O                                              '";'
063600040430     O********* CAP estero per EEX
063700040430     O          E    55      LV22D3      1
063800040511     O                                              'VBR;225;HBR;470;'
063900040430     O                                              '"'
064000040430     O                       §LDCAD
064100040430     O                                              '";'
064200040430     O********* > Box per nazione e cap EEX
064300040511     O          E    55      LV22D3      1
064400040511     O                                           14 'HLT;02;VLT;03;'
064500040430     O          E    55      LV22D3      1
064600040430     O*                         CARATTERE
064700040430     O                                              'DCH;020;DCW;015;ICS;02;'
064800040430     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
064900040430     O                                              'HOME;HBR;000;VBR;040;'
065000040430     O***       E    55      LV22D3      1
065100040430     O* FONDO NERO                                                 L   H
065200040430     O*                                             'DCBX;340;140;355;068;'
065300040430     O* FONDO BIANCO                                               L   H
065400040511     O                                              'DBOX;330;132;355;060;'
065500040430     ***O          E    15      LV22D3      1
065600040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065700040430     ***O                                           16 'VBR;228;HBR;468;'
065800040430     ***O          E    15      LV22D3      1
065900040430     ***O                                            1 '"'
066000040430     ***O                       §CA2                 5
066100040430     ***O                       §CA7                13
066200040430     ***O                                           15 '";'
066300040430     ***O* CARATTERE LINEA ESTERA
066400040430     ***O          E    15      LV22D3      1
066500040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066600040430     ***O                                           16 'VBR;228;HBR;355;'
066700040430     ***O          E    15      LV22D3      1
066800040430     ***O                                            8 'DCW;090;'
066900040430     ***O          E    15      LV22D3      1
067000040430     ***O                                            1 '"'
067100040430     ***O                       §EEN                 4
067200040430     ***O                                            6 '";'
067300040430     ***O* BOX DESCRIZIONE LINEA ESTERA
067400040430     ***O          E    15      LV22D3      1
067500040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067600040430     ***O                                           21 'HOME;VBR;173;HBR;348;'
067700040430     ***O          E    15      LV22D3      1
067800040430     ***O* BOX DESCRIZIONE LINEA ESTERA                   L   H
067900040430     ***O                                           16 'DCBX;0;0;155;60;'
068000041115      *---*   Chiusura superiore barcode
068100041115     O          E            LV22D2      1
068200041115     O                                           16 'VBR;236;HBR;090;'
068300041115     O          E            LV22D2      1
068400041115     O                                           18 'VLT;529;DVL;0;0;4;'
068500041115     O********* Bar Code
068600941216     O          E            LV22D6      1
068700040511     O                                           16 'VBR;359;HBR;090;'
068800041115     O* BARCODE CODE 128
068900041115     O          E            LV22D6      1
069000041115     O                                           15 'BDEF;25;BCPI;3;'
069100041115     O          E            LV22D6      1
069200041115     O                                           18 'BCSH;121;BCST;"C";'
069300951121     O* BARCODE INTERL. 2/5
069400041115     O**        E            LV22D6      1
069500041115     O**                                         22 'BDEF;8;BNEW;3;BWEW;10;'
069600041115     O**        E            LV22D6      1
069700041115     O**                                         14 'BCSH;121;BCST;'
069800941216     O          E            LV22D6      1
069900921104     O                       §§CDB2              21
070000941216     O          E            LV22D6      1
070100910403     O                                            5 'BSTP;'
070200041115      *---*   Chiusura inferiore barcode
070300041115     O          E            LV22D2      1
070400041115     O                                           16 'VBR;359;HBR;090;'
070500041115     O          E            LV22D2      1
070600041115     O                                           18 'VLT;529;DVL;0;0;2;'
070700951020     O********* Codice Prodotto/KG/RISTAMPA
070800941216     O          E            LV22D7      1
070900910403     O*                                       CARATTERE
071000910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
071100941216     O          E            LV22D7      1
071200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071300040511     O                                           16 'VBR;375;HBR;060;'
071400941216     O          E            LV22D7      1
071500921104     O                                            1 '"'
071600951020     O                       §RI21               22
071700941216     O                                           27 'KG.'
071800941216     O                       §LDPKG              35 '    0 , '
071900951020     O                       §LDRST              39
072000941216     O                                           41 '";'
072100941216     O********* Chiusura etchetta
072200941216     O          E            LV22D8      1
072300951020     O********                               CON COVERTITORE
072400040430     O**                                          9 'TRM;%%5C%'
072500951020     O********                               CON VIDEO
072600961223     O                                            5 'TRM;\'
072700980129     O*-------------------------------------------------------
072800971215     O* ETICHETTA ESTERO DHL
072900980129     O*-------------------------------------------------------
073000971215     O          E            LV22E            01
073100971215     O******                                 inizializzazione NUOVA
073200040430     O**                                            '&&??%%P%%7E%^"LV22";'
073300971215     O******                                 inizializzazione VIDEO
073400980129     O                                              '~^"LV22";'
073500971215     O          E            LV22E       1
073600971215     O*LOGICA                                N.ET. V (H) O=-35 X TB340
073700971215     O*                                                  O=000 X TX240
073800971215     O*                                                  O=073 X QB440
073900971215     O*                                                  O=-17 X TUTTI
074000980129     O                                              '00001;0;389;73;'
074100971215     O          E            LV22E       1
074200971215     O*FISICA
074300980129     O                                              'UTOF;4100;SPB;'
074400971215     O          E            LV22E       1
074500971215     O*GIRO SCRITTA A 180°
074600980129     O                                              'FLIP;800;'
074700971215     O          E            LV22E       1
074800980129     O                                              'HBR;00;VBR;00;'
074900971215     O          E            LV22E       1
075000971215     O*                                       FONT
075100980129     O                                              'DHR;$1;'
075200971215     O          E            LV22E       1
075300980129     O                                              'HLT;01;VLT;01;'
075400971215      *---*   TIPO PRODOTTO DHL -REX-
075500971215     O          E            LV22E       1
075600971215     O                                              'VBR;028;HBR;260;'
075700971215     O                                              'DCH;20;DCW;15;ICS;02;'
075800971215     O          E            LV22E       1
075900971215     O                                              '"'
076000971215     O                                              'PRODUCT:'
076100971215     O                                              '";'
076200971215     O          E            LV22E       1
076300971215     O                                              'VBR;000;HBR;000;'
076400971215     O                                              'DBOX;380;08;300;68;'
076500971215     O          E            LV22E       1
076600971215     O                                              'VBR;068;HBR;400;'
076700971215     O                                              'DCH;080;DCW;160;ICS;04;'
076800971215     O          E            LV22E       1
076900971215     O                                              '"'
077000971215     O                                              'REX'
077100971215     O                                              '";'
077200971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
077300971215     O          E            LV22E       1
077400971215     O                                              'VBR;128;HBR;080;'
077500971215     O                                              'DCH;20;DCW;15;ICS;02;'
077600971215     O          E            LV22E       1
077700971215     O                                              '"'
077800971215     O                                              'DESTINATION:'
077900971215     O                                              '";'
078000971215     O          E            LV22E       1
078100971215     O                                              'VBR;188;HBR;080;'
078200971215     O                                              'DCH;80;DCW;80;ICS;04;'
078300971215     O          E            LV22E       1
078400971215     O                                              '"'
078500971215     O                       §DESIA
078600971215     O                                              '";'
078700971215     O* BARCODE 39
078800971215     O          E            LV22E       1
078900971215     O                                              'VBR;188;HBR;260;'
079000971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
079100971215     O          E            LV22E       1
079200971215     O                                              'BCSH;100;BCST;"*'
079300971215     O                       §DESIP
079400971215     O                       §DESIA
079500971215     O                       §DESSE
079600971215     O                                              '*";BSTP;'
079700971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
079800971215     O          E            LV22E       1
079900971215     O                                              'VBR;224;HBR;080;'
080000971215     O                                              'DCH;20;DCW;15;ICS;02;'
080100971215     O          E            LV22E       1
080200971215     O                                              '"'
080300971215     O                                              'AWB:'
080400971215     O                                              '";'
080500971215     O          E            LV22E       1
080600971215     O                                              'VBR;224;HBR;260;'
080700971215     O                                              'DCH;40;DCW;50;ICS;04;'
080800971215     O                                              '"'
080900971215     O                       §DEAWB
081000971215     O                                              '";'
081100971215     O* BARCODE 39
081200971215     O          E            LV22E       1
081300971215     O                                              'VBR;336;HBR;110;'
081400971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
081500971215     O          E            LV22E       1
081600971215     O                                              'BCSH;100;BCST;"*'
081700971215     O                       §DEAWB
081800971215     O                                              '*";BSTP;'
081900971215      *---*   NAZIONE DESTINO -CODICE ISO-
082000971215     O          E            LV22E       1
082100971215     O                                              'VBR;377;HBR;080;'
082200971215     O                                              'DCH;50;DCW;60;ICS;04;'
082300971215     O          E            LV22E       1
082400971215     O                                              '"'
082500971215     O                       §DEISA
082600971215     O                                              '";'
082700971215      *---*
082800971215     O          E            LV22E       1
082900971215     O                                              'VBR;357;HBR;220;'
083000971215     O                                              'DCH;20;DCW;20;ICS;04;'
083100971215     O          E            LV22E       1
083200971215     O                                              '"'
083300971215     O                                              'P: '
083400971215     O                       §LDLNP
083500971215     O                                              ' A: '
083600971215     O                       §LDLNA
083700971215     O                                              '";'
083800971215     O          E            LV22E       1
083900971215     O                                              'VBR;377;HBR;220;'
084000971215     O                                              'DCH;20;DCW;20;ICS;04;'
084100971215     O          E            LV22E       1
084200971215     O                                              '"'
084300971215     O                                              'N. '
084400971215     O                       §LDNRT
084500971215     O                       §S7
084600971215     O                                              '";'
084700971215      *---*   SEQUENZA COLLO   (XX OF XX)
084800971215     O          E            LV22E       1
084900971215     O                                              'VBR;357;HBR;480;'
085000971215     O                                              'DCH;20;DCW;25;ICS;04;'
085100971215     O          E            LV22E       1
085200971215     O                                              '"'
085300971215     O                       §LDNC1        Z
085400971215     O                                              ' OF '
085500971215     O                       §LDNC2        Z
085600971215     O                                              '";'
085700971215      *---*   I.A.T.A. ORIGINE
085800971215     O          E            LV22E       1
085900971215     O                                              'VBR;377;HBR;480;'
086000971215     O                                              'DCH;20;DCW;25;ICS;04;'
086100971215     O          E            LV22E       1
086200971215     O                                              '"'
086300971215     O                                              'ORIGIN:  '
086400971215     O                       §DESIP
086500971215     O                                              '";'
086600971215     O********* Chiusura etchetta
086700971215     O          E            LV22E       1
086800971215     O********                               CON COVERTITORE
086900040430     O**                                            'TRM;%%5C%'
087000971215     O********                               CON VIDEO
087100971215     O                                              'TRM;\'
087200980129     O*-------------------------------------------------------
087300980129     O* ETICHETTA ESTERO EUROEXPRESS
087400980129     O*-------------------------------------------------------
087500980129     O          E            LV22Y            01
087600980129     O******                                 inizializzazione NUOVA
087700040430     O**                                            '&&??%%P%%7E%^"LV22";'
087800980129     O******                                 inizializzazione VIDEO
087900980129     O                                              '~^"LV22";'
088000980129     O          E            LV22Y       1
088100980129     O*LOGICA                                N.ET. V (H) O=-35 X TB340
088200980129     O*                                                  O=000 X TX240
088300980129     O*                                                  O=073 X QB440
088400980129     O*                                                  O=-17 X TUTTI
088500980129     O                                              '00001;0;389;73;'
088600980129     O          E            LV22Y       1
088700980129     O*FISICA
088800980129     O                                              'UTOF;4100;SPB;'
088900980129     O          E            LV22Y       1
089000980129     O*GIRO SCRITTA A 180°
089100980129     O                                              'FLIP;800;'
089200980129     O          E            LV22Y       1
089300980129     O                                              'HBR;00;VBR;00;'
089400980129     O          E            LV22Y       1
089500980129     O*                                       FONT
089600980129     O                                              'DHR;$1;'
089700980129     O          E            LV22Y       1
089800980129     O                                              'HLT;01;VLT;01;'
089900980129      *---*   NAZIONE PARTENZA
090000980129     O          E            LV22Y       1
090100980129     O                                              'HBR;160;VBR;030;'
090200980129     O                                              'DCH;30;DCW;30;ICS;02;'
090300980129     O                                              '"'
090400980129     O                       §DEISP
090500980129     O                                              '";'
090600980129      *---*   CONTRASSEGNO
090700980318     O          E    14      LV22Y       1
090800980129     O                                              'HBR;000;VBR;000;'
090900980129     O                                              'DBOX;490;010;190;048;'
091000980318     O          E    14      LV22Y       1
091100980129     O                                              'HBR;500;VBR;050;'
091200980129     O                                              'DCH;050;DCW;090;ICS;02;'
091300980129     O                                              '"COD";'
091400980129      *---*   EUROEXPRESS
091500980129     O          E            LV22Y       1
091600980129     O                                              'HBR;160;VBR;130;'
091700980129     O                                              'DCH;070;DCW;090;ICS;02;'
091800980129     O                                              '"EuroExpress";'
091900980129      *---*   NAZIONE ARRIVO
092000980129     O          E            LV22Y       1
092100980129     O                                              'HBR;000;VBR;000;'
092200011119     O                                              'DBOX;070;140;200;068;'
092300980129     O          E            LV22Y       1
092400011119     O                                              'HBR;080;VBR;160;'
092500980129     O                                              'DCH;020;DCW;015;ICS;02;'
092600980129     O                                              '"Country";'
092700980129     O          E            LV22Y       1
092800011119     O                                              'HBR;080;VBR;200;'
092900980129     O                                              'DCH;050;DCW;090;ICS;02;'
093000980129     O                                              '"'
093100980129     O                       §DEISA
093200980129     O                                              '";'
093300980129      *---*   CAP DESTINO
093400980129     O          E            LV22Y       1
093500980129     O                                              'HBR;000;VBR;000;'
093600011119     O                                              'DBOX;290;140;400;068;'
093700980129     O          E            LV22Y       1
093800011119     O                                              'HBR;300;VBR;160;'
093900980129     O                                              'DCH;020;DCW;015;ICS;02;'
094000980129     O                                              '"Post Code";'
094100980129     O          E            LV22Y       1
094200011119     O                                              'HBR;300;VBR;200;'
094300980129     O                                              'DCH;050;DCW;090;ICS;02;'
094400980129     O                                              '"'
094500980129     O                       §LDCAD
094600980129     O                                              '";'
094700980129      *---*   BARCODE EUROEXPRESS
094800980129     O          E            LV22Y       1
094900980129     O                                              'BDEF;25;BCSH;100;'
095000011119     O          E   n23      LV22Y       1
095100011119     O                                              'BCPI;2;PRHR;1;'
095200011116     O          E   n23      LV22Y       1
095300011119     O                                              'HBR;080;VBR;320;'
095400011116     O                                              'BCST;"A'
095500011119     O*                      §DEJEU
095600011119     O*                      §DEISP
095700011119     O                       alfa5
095800011119     O                                              '@D'
095900011119     O*                      §LDLNP
096000011119     O*                      §LDNRS
096100011119     O*                      §LDNCD
096200011119     O                       nume12
096300011119     O                                              '@F'
096400011119     O*                      §deisa
096500011119     o                       alfa2
096600011116     O                       §end
096700011119     O          E    23      LV22Y       1
096800011119     O                                              'BCPI;1;PRHR;1;'
096900011116     O          E    23      LV22Y       1
097000011119     O                     13                       'HBR;057;VBR;320;'
097100011119     O                    N13                       'HBR;082;VBR;320;'
097200980129     O                                              'BCST;"C@F'
097300980129     O                       §DEJEU
097400980129     O                       §DEISP
097500980129     O                                              '@D'
097600980129     O                       §LDLNP
097700980129     O                       §LDNRS
097800980129     O                       §LDNCD
097900011119     O                                              '@F '
098000980129     O                       §DE55L
098100980129     O                       §BCEND
098200980129      *---*   RIFERIMENTI BARTOLINI
098300980129     O          E            LV22Y       1
098400011119     O                                              'HBR;080;VBR;360;'
098500980129     O                                              'DCH;020;DCW;020;ICS;02;'
098600980129     O                                              '"'
098700980129     O                       §LDLNP
098800980129     O                                              ' '
098900980129     O                       §LDNRS
099000980129     O                                              ' '
099100980129     O                       §LDNCD
099200980129     O                                              '";'
099300980129     O********* Chiusura etchetta
099400980129     O          E            LV22Y       1
099500980129     O********                               CON COVERTITORE
099600040430     O**                                            'TRM;%%5C%'
099700980129     O********                               CON VIDEO
099800980129     O                                              'TRM;\'
099900000121     o*-----------------------------------------------------------------
100000000121     o*-etichetta dove ' DPD ' -----------------------------------------
100100000121     O          E            LV22dpd1    1
100200000121     O                                            9 '~^"LV22";'
100300000121     O          E            LV22dpd1    1
100400000124     O                                           16 '00001;0;389;020;'
100500000121     O          E            LV22dpd1    1
100600000121     O*FISICA
100700000121     O** X QB400
100800000121     O                                           21 'UTOF;4100;TEAR;0;SPB;'
100900000121     O          E            LV22dpd1    1
101000000121     O*GIRO SCRITTA A 180°
101100000121     O                                            9 'FLIP;800;'
101200000121     O          E            LV22dpd1    1
101300000121     O                                            7 'DHR;$1;'
101400000121     O          E            LV22dpd1    1
101500000121     O                                           14 'HLT;01;VLT;01;'
101600000121      *---*   numero  spedizione
101700000121     O          E            LV22dpd1    1
101800000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
101900000121     O          E            LV22dpd1    1
102000000128     O                                           16 'HBR;350;VBR;060;'
102100000121     O          E            LV22dpd1    1
102200000121     O                                            1 '"'
102300000121     O                       §dpd3                4
102400000121     O                                            6 '";'
102500000121      * 8 caratteri numero spedizione
102600000121     O          E            LV22dpd1    1
102700000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
102800000121     O          E            LV22dpd1    1
102900000121     O                                           16 'HBR;400;VBR;060;'
103000000121     O          E            LV22dpd1    1
103100000121     O                                            1 '"'
103200000121     O                       §dpd8                9
103300000121     O                                           11 '";'
103400000121      *ultimo carattere numero spedizione
103500000121     O          E            LV22dpd1    1
103600000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
103700000121     O          E            LV22dpd1    1
103800000128     O                                           16 'HBR;510;VBR;060;'
103900000121     o          E            LV22dpd1    1
104000000121     O                                            1 '"'
104100000121     O                       §dpd12               2
104200000121     O                                            4 '";'
104300000121      *---*   costante scritta
104400000121     O          E            LV22dpd1    1
104500000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
104600000121     O          E            LV22dpd1    1
104700000121     O                                           16 'HBR;240;VBR;080;'
104800000121     O          E            LV22dpd1    1
104900000121     O                                            1 '"'
105000000124     O                       §dpdn1              39
105100000124     O                                           41 '";'
105200000121      *---*   costante scritta 2
105300000121     O          E            LV22dpd1    1
105400000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
105500000121     O          E            LV22dpd1    1
105600000121     O                                           16 'HBR;240;VBR;090;'
105700000121     O          E            LV22dpd1    1
105800000121     O                                            1 '"'
105900000124     O                       §dpdn2              39
106000000124     O                                           41 '";'
106100000121      *---*   costante scritta 3
106200000121     O          E            LV22dpd1    1
106300000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
106400000121     O          E            LV22dpd1    1
106500000121     O                                           16 'HBR;240;VBR;100;'
106600000121     O          E            LV22dpd1    1
106700000121     O                                            1 '"'
106800000124     O                       §dpdn3              39
106900000124     O                                           41 '";'
107000000124      *---*   costante scritta 4
107100000124     O          E            LV22dpd1    1
107200000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
107300000124     O          E            LV22dpd1    1
107400000124     O                                           16 'HBR;240;VBR;110;'
107500000124     O          E            LV22dpd1    1
107600000124     O                                            1 '"'
107700000124     O                       §dpdn4              40
107800000124     O                                           42 '";'
107900000121      *---*   costante barra sopra
108000000121     O          E            LV22dpd1    1
108100000124     O                                           16 'HBR;130;VBR;178;'
108200000121     O          E            LV22dpd1    1
108300000124     O                                           18 'HLT;2;DHL;0;0;307;'
108400000121      *---*   costante barra sotto
108500000121     O          E            LV22dpd1    1
108600000121     O                                           16 'HBR;130;VBR;280;'
108700000121     O          E            LV22dpd1    1
108800000124     O                                           18 'HLT;2;DHL;0;0;307;'
108900000121     o* secondo codice a barre
109000000121     O          e            lv22dpd1    1
109100000121     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
109200000121     O          e            lv22dpd1    1
109300000121     O                                            9 'BCSH;100;'
109400000121     O          E            LV22dpd1    1
109500000121     O                                           16 'HBR;130;VBR;280;'
109600000121     o          E            LV22dpd1    1
109700000121     O                                            5 'BCST;'
109800000121     O                                            6 '"'
109900000121     O                       §deawb              16
110000000121     O                                           18 '";'
110100000121     O                                           23 'BSTP;'
110200000121      *costante 3 v1/00
110300000121     O          E            LV22dpd1    1
110400000121     O                                           23 'DCH;040;DCW;040;ICS;02;'
110500000121     O          E            LV22dpd1    1
110600000124     O                                           16 'HBR;250;VBR;160;'
110700000121     o          E            LV22dpd1    1
110800000121     O                                            1 '"'
110900000124     O                       §dex1                6
111000000124     O                                            8 '";'
111100000121      *decodifica 2° bcc
111200000121     O          E            LV22dpd1    1
111300000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
111400000121     O          E            LV22dpd1    1
111500000121     O                                           16 'HBR;200;VBR;300;'
111600000121     o          E            LV22dpd1    1
111700000121     O                                            1 '"'
111800000121     O                       §dpdcap             14
111900000121     O                                           16 '";'
112000000121      *costante x
112100000121     O          E            LV22dpd1    1
112200000124     O                                           23 'DCH;040;DCW;030;ICS;02;'
112300000121     O          E            LV22dpd1    1
112400000124     O                                           16 'HBR;100;VBR;320;'
112500000121     o          E            LV22dpd1    1
112600000121     O                                            1 '"'
112700000121     O                       §desse               2
112800000121     O                                            4 '";'
112900000121      *costante 09
113000000121     O          E            LV22dpd1    1
113100000121     O                                           23 'DCH;060;DCW;030;ICS;02;'
113200000121     O          E            LV22dpd1    1
113300000124     O                                           16 'HBR;090;VBR;380;'
113400000121     o          E            LV22dpd1    1
113500000121     O                                            1 '"'
113600000121     O                       dpd9                 3
113700000121     O                                            5 '";'
113800000121      *linea di arrivo
113900000121     O          E            LV22dpd1    1
114000000121     O                                           23 'DCH;080;DCW;040;ICS;02;'
114100000121     O          E            LV22dpd1    1
114200000124     O                                           16 'HBR;270;VBR;380;'
114300000121     o          E            LV22dpd1    1
114400000121     O                                            1 '"'
114500000124     O                       §dex3                4
114600000121     O                                            6 '";'
114700000121      *zona arrivo
114800000121     O          E            LV22dpd1    1
114900000121     O                                           23 'DCH;060;DCW;030;ICS;02;'
115000000121     O          E            LV22dpd1    1
115100000124     O                                           16 'HBR;370;VBR;380;'
115200000121     o          E            LV22dpd1    1
115300000121     O                                            1 '"'
115400000124     O                       §dex4                3
115500000121     O                                            5 '";'
115600000121     O*------------------------------------------------------------------
115700000121     O          E            LV22dpd2    1
115800000121     O                                            9 '~^"LV22";'
115900000121     O          E            LV22dpd2    1
116000000124     O                                           16 '00001;0;389;020;'
116100000121     O          E            LV22dpd2    1
116200000121     O*FISICA
116300000121     O** X QB400
116400000121     O                                           21 'UTOF;4100;TEAR;0;SPB;'
116500000121     O*GIRO SCRITTA A 180°
116600000121     O          E            LV22dpd2    1
116700000121     O                                            9 'FLIP;800;'
116800000121     O          E            LV22dpd2    1
116900000121     O                                            7 'DHR;$1;'
117000000121     O          E            LV22dpd2    1
117100000121     O                                           14 'HLT;01;VLT;01;'
117200000121      *---*   numero  spedizione
117300000121     O          E            LV22dpd2    1
117400000121     O                                           23 'DCH;045;DCW;030;ICS;03;'
117500000121     O          E            LV22dpd2    1
117600000121     O                                           16 'HBR;330;VBR;060;'
117700000121     O          E            LV22dpd2    1
117800000121     O                                            1 '"'
117900000121     O                       §dpd3                4
118000000121     O                                            6 '";'
118100000121      * 8 caratteri numero spedizione
118200000121     O          E            LV22dpd2    1
118300000121     O                                           23 'DCH;040;DCW;025;ICS;02;'
118400000121     O          E            LV22dpd2    1
118500000121     O                                           16 'HBR;400;VBR;060;'
118600000121     O          E            LV22dpd2    1
118700000121     O                                            1 '"'
118800000121     O                       §dpd8                9
118900000121     O                                           11 '";'
119000000121      *ultimo carattere numero spedizione
119100000121     O          E            LV22dpd2    1
119200000121     O                                           23 'DCH;035;DCW;020;ICS;01;'
119300000121     O          E            LV22dpd2    1
119400000121     O                                           16 'HBR;520;VBR;060;'
119500000121     o          E            LV22dpd2    1
119600000121     O                                            1 '"'
119700000121     O                       §dpd12               2
119800000121     O                                            4 '";'
119900000121      *---*   costante TELEFONO
120000000121     O          E            LV22dpd2    1
120100000121     O                                           23 'DCH;012;DCW;012;ICS;01;'
120200000121     O          E            LV22dpd2    1
120300000121     O                                           16 'HBR;250;VBR;080;'
120400000121     O          E            LV22dpd2    1
120500000121     O                                            1 '"'
120600000121     O                       dpd2                20
120700000121     O                                           22 '";'
120800000121      *---*   costante fax
120900000121     O          E            LV22dpd2    1
121000000121     O                                           23 'DCH;012;DCW;012;ICS;01;'
121100000121     O          E            LV22dpd2    1
121200000121     O                                           16 'HBR;250;VBR;090;'
121300000121     O          E            LV22dpd2    1
121400000121     O                                            1 '"'
121500000121     O                       dpd2f               20
121600000121     O                                           22 '";'
121700000121      *costante mittente
121800000121     O          E            LV22dpd2    1
121900000121     O                                           23 'DCH;015;DCW;015;ICS;01;'
122000000121     O          E            LV22dpd2    1
122100000124     O                                           25 'HBR;190;VBR;100;'
122200000121     o          E            LV22dpd2    1
122300000121     O                                            1 '"'
122400000121     O                       dpd3                 8
122500000121     O                                           10 '";'
122600000121      *costante mittente2
122700000121     O          E            LV22dpd2    1
122800000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
122900000121     O          E            LV22dpd2    1
123000000124     O                                           25 'HBR;190;VBR;120;'
123100000121     o          E            LV22dpd2    1
123200000121     O                                            1 '"'
123300000121     O                       §ldrsm              36
123400000121     O                                           38 '";'
123500000121      *costante mittente 3
123600000121     O          E            LV22dpd2    1
123700000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
123800000121     O          E            LV22dpd2    1
123900000124     O                                           25 'HBR;190;VBR;140;'
124000000121     o          E            LV22dpd2    1
124100000121     O                                            1 '"'
124200000121     O                       §ldprm               3
124300000121     O                                            5 '";'
124400000121      *costante destinatario
124500000121     O          E            LV22dpd2    1
124600000121     O                                           23 'DCH;015;DCW;015;ICS;01;'
124700000121     O          E            LV22dpd2    1
124800000124     O                                           25 'HBR;090;VBR;150;'
124900000121     o          E            LV22dpd2    1
125000000121     O                                            1 '"'
125100000121     O                       dpd4                11
125200000121     O                                           13 '";'
125300000121      *destinatario
125400000121     O          E            LV22dpd2    1
125500000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
125600000121     O          E            LV22dpd2    1
125700000124     O                                           20 'HBR;090;VBR;170;'
125800000121     o          E            LV22dpd2    1
125900000121     O                                            1 '"'
126000000121     O                       §ldrsd              36
126100000121     O                                           38 '";'
126200000121      * destinatario indirizzo
126300000121     O          E            LV22dpd2    1
126400000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
126500000121     O          E            LV22dpd2    1
126600000124     O                                           20 'HBR;090;VBR;190;'
126700000121     o          E            LV22dpd2    1
126800000121     O                                            1 '"'
126900000121     O                       §ldind              36
127000000121     O                                           38 '";'
127100000121      *località destinatario 7
127200000121     O          E            LV22dpd2    1
127300000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
127400000121     O          E            LV22dpd2    1
127500000124     O                                           20 'HBR;090;VBR;210;'
127600000121     o          E            LV22dpd2    1
127700000121     O                                            1 '"'
127800000121     O                       §ldlod              36
127900000121     O                                           38 '";'
128000000121      *cap      destinatario 8
128100000121     O          E            LV22dpd2    1
128200000121     O                                           23 'DCH;030;DCW;030;ICS;03;'
128300000121     O          E            LV22dpd2    1
128400000124     O                                           20 'HBR;090;VBR;240;'
128500000121     o          E            LV22dpd2    1
128600000121     O                                            1 '"'
128700000121     O                       §ldcad              10
128800000121     O                                           12 '";'
128900000121      *cap      destinatario 8
129000000121     O          E            LV22dpd2    1
129100000121     O                                           23 'DCH;030;DCW;030;ICS;03;'
129200000121     O          E            LV22dpd2    1
129300000121     O                                           20 'HBR;250;VBR;240;'
129400000121     o          E            LV22dpd2    1
129500000121     O                                            1 '"'
129600000121     O                       §desia               4
129700000121     O                                            6 '";'
129800000121      *---*   costante barra sopra
129900000121     O          E            LV22dpd2    1
130000000124     O                                           16 'HBR;130;VBR;248;'
130100000121     O          E            LV22dpd2    1
130200000124     O                                           18 'HLT;2;DHL;0;0;360;'
130300000121      *---*   costante barra sotto
130400000121     O          E            LV22dpd2    1
130500000121     O                                           16 'HBR;130;VBR;350;'
130600000121     O          E            LV22dpd2    1
130700000124     O                                           18 'HLT;2;DHL;0;0;360;'
130800000121      *costante primo B-C
130900000121     O          e            lv22dpd2    1
131000000121     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
131100000121     O          e            lv22dpd2    1
131200000121     O                                            9 'BCSH;100;'
131300000121     O          E            LV22dpd2    1
131400000121     O                                           16 'HBR;130;VBR;350;'
131500000121     o          E            LV22dpd2    1
131600000121     O                                            5 'BCST;'
131700000121     O                                            6 '"'
131800000121     O                       §dpd                18
131900000121     O                                           20 '";'
132000000121     O                                           25 'BSTP;'
132100000121      *costante data
132200000121     O          E            LV22dpd2    1
132300000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
132400000121     O          E            LV22dpd2    1
132500000124     O                                           16 'HBR;090;VBR;380;'
132600000121     o          E            LV22dpd2    1
132700000121     O                                            1 '"'
132800000121     O                       datasp              11 '  /  /    '
132900000121     O                                           13 '";'
133000000121      *costante 1 collo
133100000121     O          E            LV22dpd2    1
133200000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
133300000121     O          E            LV22dpd2    1
133400000124     O                                           16 'HBR;220;VBR;380;'
133500000121     o          E            LV22dpd2    1
133600000121     O                                            1 '"'
133700000121     O                                            8 '1 Collo'
133800000121     O                                           10 '";'
133900000121      *costante peso
134000000121     O          E            LV22dpd2    1
134100000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
134200000121     O          E            LV22dpd2    1
134300000124     O                                           16 'HBR;320;VBR;380;'
134400000121     o          E            LV22dpd2    1
134500000121     O                                            1 '"'
134600000121     O                                            8 'Peso Kg'
134700000121     O                                           10 '";'
134800000121      * peso
134900000121     O          E            LV22dpd2    1
135000000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
135100000121     O          E            LV22dpd2    1
135200000124     O                                           16 'HBR;420;VBR;380;'
135300000121     o          E            LV22dpd2    1
135400000121     O                                            1 '"'
135500000121     O                       §ldpkg        2     10
135600000121     O                                           12 '";'
135700000121      *------------------------------------------------------------------
135800000121      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
135900000121     O* costante dpd
136000000121     O          E            LV22dpd     1
136100000121     O                                           23 'DCH;030;DCW;070;ICS;03;'
136200000121     O          E            LV22dpd     1
136300000121     O                                           25 'HBR;170;VBR;110;DHR;$201;'
136400000121     o          E            LV22dpd     1
136500000121     O                                            1 '"'
136600000121     O                       dpd                  4
136700000121     O                                            6 '";'
136800000121     O* costante dpd riga sotto
136900000121     O*         E            LV22dpd     1
137000000121     O*                                          23 'DCH;006;DCW;005;ICS;01;'
137100000121     O*         E            LV22dpd     1
137200000121     O*                                          20 'HBR;210;VBR;083;'
137300000121     o*         E            LV22dpd     1
137400000121     O*                                           1 '"'
137500000121     O*                      dpd1                23
137600000121     O*                                          25 '";'
137700000121     o          E            LV22dpd     1
137800000121     O                                            5 'TRM;\'
