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
014400090806     C  N17§LDTSP        COMP      'H'                                    17
014500951023     C     §LDCDP        COMP      *BLANK                             18
014600951023     C     §LDNRS        COMP      *ZERO                              19
014700951023     C*
014800951023     C** INIZIALIZZA STAMPANTE
014900951023     C                   EXCEPT    LV22
015000951023     C*
015100951023     C                   MOVEA     *BLANK        §RX
015200951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
015300951023     C                   SETOFF                                       77
015400951023     C                   MOVE      *BLANK        §A7               7
015500951023     C                   Z-ADD     §LDNC1        §N7
015600951023     C                   DO        7             §1
015700951023     C     §S7(§1)       IFEQ      *ZERO
015800951023     C                   MOVE      *BLANK        §S7(§1)
015900951023     C                   ELSE
016000951023     C                   SETON                                        77
016100951023     C                   END
016200951023     C  N77              END
016300951023     C                   MOVEA     §S7           §A7
016400951023     C                   MOVE      §A7           §A3               3
016500951023     C                   MOVEA     §A3           §RX(1)
016600951023     C                   MOVEA     '-'           §RX(4)
016700951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
016800951023     C                   SETOFF                                       77
016900951023     C                   MOVE      *BLANK        §A7
017000951023     C                   Z-ADD     §LDNC2        §N7
017100951023     C                   DO        7             §1
017200951023     C     §S7(§1)       IFEQ      *ZERO
017300951023     C                   MOVE      *BLANK        §S7(§1)
017400951023     C                   ELSE
017500951023     C                   SETON                                        77
017600951023     C                   END
017700951023     C  N77              END
017800951023     C                   MOVEA     §S7           §A7
017900951023     C                   MOVE      §A7           §A3
018000951023     C                   MOVEA     §A3           §RX(5)
018100951023     C                   MOVE      §LDCPD        §A3
018200951023     C                   MOVEA     §A3           §RX(9)
018300951023     C* STAMPA LINEA PARTENZA
018400951023     C                   EXCEPT    LV22H
018500951023     C*
018600951023     C                   MOVEA     *BLANK        §RI
018700951023     C                   MOVEA     §LDRSM        §RI(8)
018800951023     C                   MOVEA     ' '           §RI(30)
018900951023     C                   MOVEA     §LDPRM        §RI(31)
019000951023     C                   MOVEA     *BLANK        §RI(33)
019100951023     C                   EXCEPT    LV22D1
019200951023     C* STAMPA DATI MITTENTE
019300951023     C*
019400971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
019500971217     C                   EXSR      COLZER
019600971217     C*
019700951023     C*
019800951023     C* STAMPA NUMERO SEGNACOLLO
019900951023     C                   EXCEPT    LV22D2
020000951023     C*
020100951023     C                   MOVE      §LDTNA        §A3               3
020200951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
020300951023     C                   MOVE      §LDLNA        §A3
020400951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
020500951023     C                   MOVE      §LDZSC        §A2
020600951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
020700951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
020800951023     C                   MOVEL     §LDDLA        §ODDLA            8
020900951023     C* STAMPA LINEA ARRIVO
021000040430     ***C* X 15 STAMPA A "MODOD ESTERO"
021100040430     ***C   15              DO
021200040430     ***C                   MOVEL     §ODDLA        §EEN              3
021300040430     ***C                   MOVEL     §LDCAD        §CA2              2
021400040430     ***C                   MOVE      §LDCAD        §CA7              7
021500040430     ***C                   END
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
027400971217     C/SPACE 005
027500971217     C*****************************************************
027600971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
027700971217     C*****************************************************
027800971217     C     COLZER        BEGSR
027900971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
028000971217     C                   SETOFF                                       77
028100971217     C                   MOVE      §LDNCD        §N7
028200971217     C                   DO        7             §1
028300971217     C     §S7(§1)       IFEQ      *ZERO
028400971217     C                   MOVE      *BLANK        §S7(§1)
028500971217     C                   ELSE
028600971217     C                   SETON                                        77
028700971217     C                   END
028800971217     C  N77              END
028900971217     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
029000971217     C                   MOVE      *ZERO         §X                1 0
029100971217     C                   MOVEA     *BLANK        §O7
029200971217     C                   DO        7             §1
029300971217     C     §S7(§1)       IFNE      *BLANK
029400971217     C                   ADD       1             §X
029500971217     C     §S7(§1)       IFEQ      '0'
029600971217     C                   MOVE      'O'           §O7(§X)
029700971217     C                   ELSE
029800971217     C                   MOVE      §S7(§1)       §O7(§X)
029900971217     C                   END
030000971217     C                   END
030100971217     C                   END
030200971217     C                   MOVEA     §O7           §S7
030300971217     C                   ENDSR
030400951023     C/SPACE 005
030500951023     C*****************************************************
030600980129     C* STAMPA EUROEXPRESS                              ***
030700951023     C*****************************************************
030800980129     C     EUREXP        BEGSR
030900980321     C*
031000980321     C* IMPOSTO COSTANTI
031100980321     C                   MOVEL     'JUT'         §DEJEU            3
031200980321     C                   MOVEL     '55L'         §DE55L            3
031300980321     C                   MOVEL     'IT'          §DEISP            2
031400011116      * controllo se contenuto 55L nel codice passato
031500011116     c     '55L'         scan      §deeur                                 23
031600011119      * se contenuto 55l scompongo a modo vecchio
031700011119     c                   if        *in23
031800980321     C* ESTRAGGO IL CAP:
031900980321     C                   MOVE      *ZEROS        §Y
032000980321     C                   MOVEA     §DEEUR        §EU
032100980321     C* TROVO QUANTO è LUNGO IL BARCODE
032200980321     C     ' '           CHECKR    §DEEUR        §Y
032300980321     C* ISOLO IL CAP
032400980321     C                   MOVE      *BLANK        §O9
032500980321     C                   MOVE      *ZEROS        §W                2 0
032600980321     C     §Y            SUB       2             §Y
032700980321     C     22            DO        §Y            §X1               2 0
032800980321     C     §EU(§X1)      IFNE      *BLANKS
032900980321     C                   ADD       1             §W
033000980321     C                   MOVE      §EU(§X1)      §O9(§W)
033100980321     C                   END
033200980321     C                   END
033300980129     C*
033400980129     C                   MOVEA     §O9           §LDCAD
033500011119
033600011119      *scompongo a modo nuovo senza 55l
033700011119     c                   else
033800011119     c                   movel     §deeur        §bceurex
033900011119     c                   end
034000980129     C*
034100980129     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
034200980129     C* DEL BARCODE
034300980129     C                   MOVEL     '";BSTP;'     §END              7
034400980129     C     §LDCAD        CAT       §DEISA:0      §BCD             11
034500980129     C     §BCD          CAT       §END:0        §BCEND           20
034600980318     C*
034700980318     C* VERIFICO SE CONTRASSEGNO
034800980318     C*
034900980318     C     §DEFCA        COMP      'S'                                    14
035000980318     C  N14§DEFCA        COMP      'X'                                    14
035100990323     C*
035200990323     C* VERIFICO SE INGHILTERRA
035300990323     C*
035400990323     C     §DEISA        COMP      'GB'                                   13
035500980129     C*
035600980129     C                   EXCEPT    LV22Y
035700980129     C*
035800951021     C                   ENDSR
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
039100000121      *--------------------------------------------------
039200000121     c     dpddove       begsr
039300000121      *--------------------------------------------------
039400000121     c                   eval      §dpd = §deeur
039500000124     c                   eval      §dexx1ds = §dexx1
039600000121     c                   eval      §deawbds = §deawb
039700000121     c                   eval      §dpdc1 = §dpdc1w
039800000121     c                   eval      §dpdc3 = §dpdc3w
039900000121     c                   eval      §dpdc5 = §dpdc5w
040000000121     c                   eval      §dpdc7 = §dpdc7w
040100000121     c                   eval      §dpdn = 'Notifications on damages which are -
040200000121     c                             not recognizable from the outside have to -
040300000121     c                             have to be submitted to the transport -
040400000121     c                             organisation withing 7 days in written.'
040500000121     C                   except    LV22dpd1
040600000121     C                   except    LV22dpd
040700000121     C                   endsr
040800000121      *--------------------------------------------------
040900000121     c     dpdchi        begsr
041000000121      *--------------------------------------------------
041100000121     c                   move      §ldmgs        giorno            2 0
041200000121     c                   movel     §ldmgs        mese              2 0
041300000121     c                   move      mese          giomes            4 0
041400000121     c                   movel     giorno        giomes
041500000121     c                   movel     giomes        datasp            8 0
041600000121     c                   move      §ldaas        datasp
041700000121     c                   movel     §dexx2        dpdteln
041800000121     c                   move      §dexx2        dpdfaxn
041900000121     C                   except    LV22dpd2
042000000121     C                   except    LV22dpd
042100000121     C                   endsr
042200040430      *--------------------------------------------------
042300941216     OFNLV22P   E            LV22        1
042400931008     O******                                 inizializzazione NUOVA
042500040430     O**                                         20 '&&??%%P%%7E%^"LV22";'
042600951020     O******                                 inizializzazione VIDEO
042700961223     O                                            9 '~^"LV22";'
042800941216     O          E            LV22        1
042900941228     O*LOGICA                                N.ET. V (H) O=-35 X TB340
043000941228     O*                                                  O=000 X TX240
043100961223     O*                                                  O=073 X QB440
043200941216     O*                                                  O=-17 X TUTTI
043300961223     O                                           16 '00001;0;389;073;'
043400941216     O          E            LV22        1
043500910403     O*FISICA
043600961223     O** X QB400
043700961223     O                                           21 'UTOF;4100;TEAR;0;SPB;'
043800961223     O** X TX340/240
043900040430     O*                                          14 'UTOF;4100;SPB;'
044000941216     O          E            LV22        1
044100910403     O*GIRO SCRITTA A 180°
044200910403     O                                            9 'FLIP;800;'
044300941216     O          E            LV22        1
044400910403     O*                                       FONT
044500910403     O                                            7 'DHR;$1;'
044600941216     O          E            LV22        1
044700910403     O                                           14 'HLT;01;VLT;01;'
044800910403     O********* Mittente
044900941216     O          E            LV22D1      1
045000910403     O*                                       CARATTERE
045100910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
045200941216     O          E            LV22D1      1
045300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
045400970221     O                                           16 'VBR;016;HBR;060;'
045500941216     O          E            LV22D1      1
045600910403     O                                            1 '"'
045700910404     O                       §RI                 38
045800920910     O                                           40 '";'
045900910403     O********* Destinatario (Rag.Soc.)
046000941216     O          E            LV22D4      1
046100910403     O*                         RIGA DI SEPARAZIONE:
046200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
046300970221     O                                           16 'VBR;020;HBR;058;'
046400941216     O          E            LV22D4      1
046500040511     O*****                                       6 'HLT;3;'
046600040511     O                                            6 'HLT;2;'
046700941216     O          E            LV22D4      1
046800040430     O*                                       RIGA        O   V   L
046900941216     O                                           16 'DHL;117;000;390;'
047000941216     O          E            LV22D4      1
047100910403     O*                                       CARATTERE
047200910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
047300941216     O          E            LV22D4      1
047400910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
047500970221     O                                           16 'VBR;036;HBR;060;'
047600941216     O          E            LV22D4      1
047700910403     O                                            1 '"'
047800910404     O                       §RI                 38
047900910404     O                                           40 '";'
048000910403     O********* Destinatario (Località)
048100941216     O          E            LV22D5      1
048200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048300970221     O                                           16 'VBR;053;HBR;060;'
048400941216     O          E            LV22D5      1
048500910403     O                                            1 '"'
048600910404     O                       §RI                 38
048700910404     O                                           40 '";'
048800920221     O********* MC. SPEDIZIONE
048900941216     O          E            LV22D5      1
049000920221     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049100970221     O                                           16 'VBR;067;HBR;465;'
049200941216     O          E            LV22D5      1
049300921104     O                                            1 '"'
049400951021     O                                            4 'MC.'
049500951021     O                       §LDVOL              11 '0 ,   '
049600951021     O                       §LDFVR              14
049700951021     O                                           16 '";'
049800941216     O********* SERVIZIO
049900941216     O          E            LV22X5      1
050000921104     O*                                       CARATTERE
050100921104     O                                           22 'DCH;45;DCW;085;ICS;03;'
050200941216     O          E            LV22X5      1
050300921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050400970221     O                                           16 'VBR;035;HBR;600;'
050500941216     O          E            LV22X5      1
050600941216     O                                            1 '"'
050700941216     O                       §LDTSP               2
050800941216     O                                            4 '";'
050900941216     O          E            LV22X5      1
051000970221     O                                           21 'HOME;VBR;000;HBR;575;'
051100941216     O          E            LV22X5      1
051200040430     O* FONDO NERO                                             L   H
051300941216     O                     17                    16 'DCBX;0;0;100;40;'
051400040430     O* FONDO BIANCO                                           L   H
051500941216     O                    N17                    16 'DBOX;0;0;100;40;'
051600910403     O********* Linea Partenza
051700941216     O          E            LV22H       1
051800910403     O*                                       CARATTERE
051900910403     O                                           23 'DCH;015;DCW;026;ICS;03;'
052000941216     O          E            LV22H       1
052100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052200970221     O                                           16 'VBR;081;HBR;450;'
052300941216     O          E            LV22H       1
052400910403     O                                            1 '"'
052500951021     O                       §RX                 12
052600951021     O                                           14 '";'
052700941216     O          E            LV22H       1
052800910403     O*                                       CARATTERE
052900910403     O                                           23 'DCH;030;DCW;030;ICS;03;'
053000941216     O          E            LV22H       1
053100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053200970221     O                                           16 'VBR;081;HBR;170;'
053300941216     O          E            LV22H       1
053400910403     O                                            1 '"'
053500910403     O                       §LDLNP               4
053600910403     O                                            6 '";'
053700941216     O          E            LV22H       1
053800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053900970221     O                                           16 'VBR;081;HBR;245;'
054000941216     O          E            LV22H       1
054100910403     O                                            1 '"'
054200910403     O                       §LDDLP              11
054300910403     O                                           13 '";'
054400910403     O********* Segnacollo
054500941216     O          E    19      LV22D2      1
054600910403     O*                                       CARATTERE
054700921104     O                                           23 'DCH;080;DCW;050;ICS;08;'
054800941216     O          E    19      LV22D2      1
054900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055000040511     O*****                                      16 'VBR;168;HBR;080;'
055100040511     O                                           16 'VBR;163;HBR;080;'
055200941216     O          E    19      LV22D2      1
055300910403     O                                            1 '"'
055400920910     O                       §LDNRT               4
055500920910     O                                            6 '";'
055600941216     O          E            LV22D2      1
055700910403     O*                                       CARATTERE
055800921105     O                                           23 'DCH;110;DCW;105;ICS;12;'
055900941216     O          E            LV22D2      1
056000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
056100970221     O                                           16 'VBR;163;HBR;205;'
056200941216     O          E            LV22D2      1
056300910403     O                                            1 '"'
056400910403     O                       §S7                  8
056500910403     O                                           10 '";'
056600921106     O********* TERMINAL/LINEA/ZONA ARRIVO
056700941216     O          E            LV22D3      1
056800921106     O                                           14 'HLT;05;VLT;08;'
056900941216     O          E            LV22D3      1
057000910403     O*                                       CARATTERE
057100921106     O                                           21 'DCH;75;DCW;70;ICS;08;'
057200921106     O********* TERMINAL
057300941216     O          E            LV22D3      1
057400910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057500970221     O                                           16 'VBR;228;HBR;060;'
057600941216     O          E            LV22D3      1
057700910403     O                                            1 '"'
057800921106     O                       §ODTNA               4
057900921106     O                                            6 '";'
058000910403     O* BOX X TERMINAL ARRIVO
058100941216     O          E            LV22D3      1
058200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058300970221     O                                           21 'HOME;VBR;173;HBR;046;'
058400941216     O          E            LV22D3      1
058500921106     O* FONDO NERO                                     L   H
058600921106     O                                           16 'DCBX;0;0;147;60;'
058700921106     O* FONDO BIANCO                                   L   H
058800040511     O*                                          16 'DBOX;0;0;147;60;'
058900921106     O********* CODICE LINEA ARRIVO
059000941216     O          E            LV22D3      1
059100921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059200970221     O                                           16 'VBR;228;HBR;207;'
059300941216     O          E            LV22D3      1
059400921106     O                                            1 '"'
059500921106     O                       §ODLNA               4
059600921106     O                                            6 '";'
059700921106     O********* ZONA ARRIVO
059800040430     O          E   N55      LV22D3      1
059900921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060000970221     O                                           16 'VBR;228;HBR;592;'
060100040430     O          E   N55      LV22D3      1
060200921106     O                                            1 '"'
060300921106     O                       §ODZSC               3
060400921106     O                                            5 '";'
060500910403     O* BOX X ZONA ARRIVO
060600040430     O          E   N55      LV22D3      1
060700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060800970221     O                                           21 'HOME;VBR;173;HBR;582;'
060900040430     O          E   N55      LV22D3      1
061000921106     O* FONDO NERO                                     L   H
061100921106     O                                           16 'DCBX;0;0;102;60;'
061200921106     O* FONDO BIANCO                                   L   H
061300040430     O*                                          16 'DBOX;0;0;102;60;'
061400921106     O********* DESCRIZIONE LINEA DI ARRIVO
061500040430     O          E   N55      LV22D3      1
061600921106     O                                           14 'HLT;05;VLT;06;'
061700040430     O          E   N55      LV22D3      1
061800921106     O*                                       CARATTERE
061900921106     O                                           21 'DCH;75;DCW;45;ICS;04;'
062000040430     O          E   N55      LV22D3      1
062100921106     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
062200970221     O                                           16 'VBR;228;HBR;355;'
062300040430     O          E   N55      LV22D3      1
062400921106     O                                            1 '"'
062500921106     O                       §ODDLA               9
062600921106     O                                           11 '";'
062700040430     O********* Nazione per EEX
062800040430     O          E    55      LV22D3      1
062900040430     O*                         CARATTERE
063000040430     O                                              'DCH;065;DCW;065;ICS;08;'
063100040430     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
063200040511     O                                              'VBR;225;HBR;350;'
063300040430     O          E    55      LV22D3      1
063400040430     O                                              '"'
063500040430     O                       §DEISA
063600040430     O                                              '";'
063700040430     O********* CAP estero per EEX
063800040430     O          E    55      LV22D3      1
063900040511     O                                              'VBR;225;HBR;470;'
064000040430     O                                              '"'
064100040430     O                       §LDCAD
064200040430     O                                              '";'
064300040430     O********* > Box per nazione e cap EEX
064400040511     O          E    55      LV22D3      1
064500040511     O                                           14 'HLT;02;VLT;03;'
064600040430     O          E    55      LV22D3      1
064700040430     O*                         CARATTERE
064800040430     O                                              'DCH;020;DCW;015;ICS;02;'
064900040430     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065000040430     O                                              'HOME;HBR;000;VBR;040;'
065100040430     O***       E    55      LV22D3      1
065200040430     O* FONDO NERO                                                 L   H
065300040430     O*                                             'DCBX;340;140;355;068;'
065400040430     O* FONDO BIANCO                                               L   H
065500040511     O                                              'DBOX;330;132;355;060;'
065600040430     ***O          E    15      LV22D3      1
065700040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065800040430     ***O                                           16 'VBR;228;HBR;468;'
065900040430     ***O          E    15      LV22D3      1
066000040430     ***O                                            1 '"'
066100040430     ***O                       §CA2                 5
066200040430     ***O                       §CA7                13
066300040430     ***O                                           15 '";'
066400040430     ***O* CARATTERE LINEA ESTERA
066500040430     ***O          E    15      LV22D3      1
066600040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066700040430     ***O                                           16 'VBR;228;HBR;355;'
066800040430     ***O          E    15      LV22D3      1
066900040430     ***O                                            8 'DCW;090;'
067000040430     ***O          E    15      LV22D3      1
067100040430     ***O                                            1 '"'
067200040430     ***O                       §EEN                 4
067300040430     ***O                                            6 '";'
067400040430     ***O* BOX DESCRIZIONE LINEA ESTERA
067500040430     ***O          E    15      LV22D3      1
067600040430     ***O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067700040430     ***O                                           21 'HOME;VBR;173;HBR;348;'
067800040430     ***O          E    15      LV22D3      1
067900040430     ***O* BOX DESCRIZIONE LINEA ESTERA                   L   H
068000040430     ***O                                           16 'DCBX;0;0;155;60;'
068100041115      *---*   Chiusura superiore barcode
068200041115     O          E            LV22D2      1
068300041115     O                                           16 'VBR;236;HBR;090;'
068400041115     O          E            LV22D2      1
068500041115     O                                           18 'VLT;529;DVL;0;0;4;'
068600041115     O********* Bar Code
068700941216     O          E            LV22D6      1
068800040511     O                                           16 'VBR;359;HBR;090;'
068900041115     O* BARCODE CODE 128
069000041115     O          E            LV22D6      1
069100041115     O                                           15 'BDEF;25;BCPI;3;'
069200041115     O          E            LV22D6      1
069300041115     O                                           18 'BCSH;121;BCST;"C";'
069400951121     O* BARCODE INTERL. 2/5
069500041115     O**        E            LV22D6      1
069600041115     O**                                         22 'BDEF;8;BNEW;3;BWEW;10;'
069700041115     O**        E            LV22D6      1
069800041115     O**                                         14 'BCSH;121;BCST;'
069900941216     O          E            LV22D6      1
070000921104     O                       §§CDB2              21
070100941216     O          E            LV22D6      1
070200910403     O                                            5 'BSTP;'
070300041115      *---*   Chiusura inferiore barcode
070400041115     O          E            LV22D2      1
070500041115     O                                           16 'VBR;359;HBR;090;'
070600041115     O          E            LV22D2      1
070700041115     O                                           18 'VLT;529;DVL;0;0;2;'
070800951020     O********* Codice Prodotto/KG/RISTAMPA
070900941216     O          E            LV22D7      1
071000910403     O*                                       CARATTERE
071100910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
071200941216     O          E            LV22D7      1
071300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071400040511     O                                           16 'VBR;375;HBR;060;'
071500941216     O          E            LV22D7      1
071600921104     O                                            1 '"'
071700951020     O                       §RI21               22
071800941216     O                                           27 'KG.'
071900941216     O                       §LDPKG              35 '    0 , '
072000951020     O                       §LDRST              39
072100941216     O                                           41 '";'
072200941216     O********* Chiusura etchetta
072300941216     O          E            LV22D8      1
072400951020     O********                               CON COVERTITORE
072500040430     O**                                          9 'TRM;%%5C%'
072600951020     O********                               CON VIDEO
072700961223     O                                            5 'TRM;\'
072800980129     O*-------------------------------------------------------
072900971215     O* ETICHETTA ESTERO DHL
073000980129     O*-------------------------------------------------------
073100971215     O          E            LV22E            01
073200971215     O******                                 inizializzazione NUOVA
073300040430     O**                                            '&&??%%P%%7E%^"LV22";'
073400971215     O******                                 inizializzazione VIDEO
073500980129     O                                              '~^"LV22";'
073600971215     O          E            LV22E       1
073700971215     O*LOGICA                                N.ET. V (H) O=-35 X TB340
073800971215     O*                                                  O=000 X TX240
073900971215     O*                                                  O=073 X QB440
074000971215     O*                                                  O=-17 X TUTTI
074100980129     O                                              '00001;0;389;73;'
074200971215     O          E            LV22E       1
074300971215     O*FISICA
074400980129     O                                              'UTOF;4100;SPB;'
074500971215     O          E            LV22E       1
074600971215     O*GIRO SCRITTA A 180°
074700980129     O                                              'FLIP;800;'
074800971215     O          E            LV22E       1
074900980129     O                                              'HBR;00;VBR;00;'
075000971215     O          E            LV22E       1
075100971215     O*                                       FONT
075200980129     O                                              'DHR;$1;'
075300971215     O          E            LV22E       1
075400980129     O                                              'HLT;01;VLT;01;'
075500971215      *---*   TIPO PRODOTTO DHL -REX-
075600971215     O          E            LV22E       1
075700971215     O                                              'VBR;028;HBR;260;'
075800971215     O                                              'DCH;20;DCW;15;ICS;02;'
075900971215     O          E            LV22E       1
076000971215     O                                              '"'
076100971215     O                                              'PRODUCT:'
076200971215     O                                              '";'
076300971215     O          E            LV22E       1
076400971215     O                                              'VBR;000;HBR;000;'
076500971215     O                                              'DBOX;380;08;300;68;'
076600971215     O          E            LV22E       1
076700971215     O                                              'VBR;068;HBR;400;'
076800971215     O                                              'DCH;080;DCW;160;ICS;04;'
076900971215     O          E            LV22E       1
077000971215     O                                              '"'
077100971215     O                                              'REX'
077200971215     O                                              '";'
077300971215      *---*   DESTINAZIONE (CODICE I.A.T.A. + BARCODE TIPO CODE 39)
077400971215     O          E            LV22E       1
077500971215     O                                              'VBR;128;HBR;080;'
077600971215     O                                              'DCH;20;DCW;15;ICS;02;'
077700971215     O          E            LV22E       1
077800971215     O                                              '"'
077900971215     O                                              'DESTINATION:'
078000971215     O                                              '";'
078100971215     O          E            LV22E       1
078200971215     O                                              'VBR;188;HBR;080;'
078300971215     O                                              'DCH;80;DCW;80;ICS;04;'
078400971215     O          E            LV22E       1
078500971215     O                                              '"'
078600971215     O                       §DESIA
078700971215     O                                              '";'
078800971215     O* BARCODE 39
078900971215     O          E            LV22E       1
079000971215     O                                              'VBR;188;HBR;260;'
079100971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
079200971215     O          E            LV22E       1
079300971215     O                                              'BCSH;100;BCST;"*'
079400971215     O                       §DESIP
079500971215     O                       §DESIA
079600971215     O                       §DESSE
079700971215     O                                              '*";BSTP;'
079800971215      *---*   N° AWB (NUMERO + BARCODE TIPO CODE 39)
079900971215     O          E            LV22E       1
080000971215     O                                              'VBR;224;HBR;080;'
080100971215     O                                              'DCH;20;DCW;15;ICS;02;'
080200971215     O          E            LV22E       1
080300971215     O                                              '"'
080400971215     O                                              'AWB:'
080500971215     O                                              '";'
080600971215     O          E            LV22E       1
080700971215     O                                              'VBR;224;HBR;260;'
080800971215     O                                              'DCH;40;DCW;50;ICS;04;'
080900971215     O                                              '"'
081000971215     O                       §DEAWB
081100971215     O                                              '";'
081200971215     O* BARCODE 39
081300971215     O          E            LV22E       1
081400971215     O                                              'VBR;336;HBR;110;'
081500971215     O                                              'BDEF;1;BNEW;3;BWEW;9;'
081600971215     O          E            LV22E       1
081700971215     O                                              'BCSH;100;BCST;"*'
081800971215     O                       §DEAWB
081900971215     O                                              '*";BSTP;'
082000971215      *---*   NAZIONE DESTINO -CODICE ISO-
082100971215     O          E            LV22E       1
082200971215     O                                              'VBR;377;HBR;080;'
082300971215     O                                              'DCH;50;DCW;60;ICS;04;'
082400971215     O          E            LV22E       1
082500971215     O                                              '"'
082600971215     O                       §DEISA
082700971215     O                                              '";'
082800971215      *---*
082900971215     O          E            LV22E       1
083000971215     O                                              'VBR;357;HBR;220;'
083100971215     O                                              'DCH;20;DCW;20;ICS;04;'
083200971215     O          E            LV22E       1
083300971215     O                                              '"'
083400971215     O                                              'P: '
083500971215     O                       §LDLNP
083600971215     O                                              ' A: '
083700971215     O                       §LDLNA
083800971215     O                                              '";'
083900971215     O          E            LV22E       1
084000971215     O                                              'VBR;377;HBR;220;'
084100971215     O                                              'DCH;20;DCW;20;ICS;04;'
084200971215     O          E            LV22E       1
084300971215     O                                              '"'
084400971215     O                                              'N. '
084500971215     O                       §LDNRT
084600971215     O                       §S7
084700971215     O                                              '";'
084800971215      *---*   SEQUENZA COLLO   (XX OF XX)
084900971215     O          E            LV22E       1
085000971215     O                                              'VBR;357;HBR;480;'
085100971215     O                                              'DCH;20;DCW;25;ICS;04;'
085200971215     O          E            LV22E       1
085300971215     O                                              '"'
085400971215     O                       §LDNC1        Z
085500971215     O                                              ' OF '
085600971215     O                       §LDNC2        Z
085700971215     O                                              '";'
085800971215      *---*   I.A.T.A. ORIGINE
085900971215     O          E            LV22E       1
086000971215     O                                              'VBR;377;HBR;480;'
086100971215     O                                              'DCH;20;DCW;25;ICS;04;'
086200971215     O          E            LV22E       1
086300971215     O                                              '"'
086400971215     O                                              'ORIGIN:  '
086500971215     O                       §DESIP
086600971215     O                                              '";'
086700971215     O********* Chiusura etchetta
086800971215     O          E            LV22E       1
086900971215     O********                               CON COVERTITORE
087000040430     O**                                            'TRM;%%5C%'
087100971215     O********                               CON VIDEO
087200971215     O                                              'TRM;\'
087300980129     O*-------------------------------------------------------
087400980129     O* ETICHETTA ESTERO EUROEXPRESS
087500980129     O*-------------------------------------------------------
087600980129     O          E            LV22Y            01
087700980129     O******                                 inizializzazione NUOVA
087800040430     O**                                            '&&??%%P%%7E%^"LV22";'
087900980129     O******                                 inizializzazione VIDEO
088000980129     O                                              '~^"LV22";'
088100980129     O          E            LV22Y       1
088200980129     O*LOGICA                                N.ET. V (H) O=-35 X TB340
088300980129     O*                                                  O=000 X TX240
088400980129     O*                                                  O=073 X QB440
088500980129     O*                                                  O=-17 X TUTTI
088600980129     O                                              '00001;0;389;73;'
088700980129     O          E            LV22Y       1
088800980129     O*FISICA
088900980129     O                                              'UTOF;4100;SPB;'
089000980129     O          E            LV22Y       1
089100980129     O*GIRO SCRITTA A 180°
089200980129     O                                              'FLIP;800;'
089300980129     O          E            LV22Y       1
089400980129     O                                              'HBR;00;VBR;00;'
089500980129     O          E            LV22Y       1
089600980129     O*                                       FONT
089700980129     O                                              'DHR;$1;'
089800980129     O          E            LV22Y       1
089900980129     O                                              'HLT;01;VLT;01;'
090000980129      *---*   NAZIONE PARTENZA
090100980129     O          E            LV22Y       1
090200980129     O                                              'HBR;160;VBR;030;'
090300980129     O                                              'DCH;30;DCW;30;ICS;02;'
090400980129     O                                              '"'
090500980129     O                       §DEISP
090600980129     O                                              '";'
090700980129      *---*   CONTRASSEGNO
090800980318     O          E    14      LV22Y       1
090900980129     O                                              'HBR;000;VBR;000;'
091000980129     O                                              'DBOX;490;010;190;048;'
091100980318     O          E    14      LV22Y       1
091200980129     O                                              'HBR;500;VBR;050;'
091300980129     O                                              'DCH;050;DCW;090;ICS;02;'
091400980129     O                                              '"COD";'
091500980129      *---*   EUROEXPRESS
091600980129     O          E            LV22Y       1
091700980129     O                                              'HBR;160;VBR;130;'
091800980129     O                                              'DCH;070;DCW;090;ICS;02;'
091900980129     O                                              '"EuroExpress";'
092000980129      *---*   NAZIONE ARRIVO
092100980129     O          E            LV22Y       1
092200980129     O                                              'HBR;000;VBR;000;'
092300011119     O                                              'DBOX;070;140;200;068;'
092400980129     O          E            LV22Y       1
092500011119     O                                              'HBR;080;VBR;160;'
092600980129     O                                              'DCH;020;DCW;015;ICS;02;'
092700980129     O                                              '"Country";'
092800980129     O          E            LV22Y       1
092900011119     O                                              'HBR;080;VBR;200;'
093000980129     O                                              'DCH;050;DCW;090;ICS;02;'
093100980129     O                                              '"'
093200980129     O                       §DEISA
093300980129     O                                              '";'
093400980129      *---*   CAP DESTINO
093500980129     O          E            LV22Y       1
093600980129     O                                              'HBR;000;VBR;000;'
093700011119     O                                              'DBOX;290;140;400;068;'
093800980129     O          E            LV22Y       1
093900011119     O                                              'HBR;300;VBR;160;'
094000980129     O                                              'DCH;020;DCW;015;ICS;02;'
094100980129     O                                              '"Post Code";'
094200980129     O          E            LV22Y       1
094300011119     O                                              'HBR;300;VBR;200;'
094400980129     O                                              'DCH;050;DCW;090;ICS;02;'
094500980129     O                                              '"'
094600980129     O                       §LDCAD
094700980129     O                                              '";'
094800980129      *---*   BARCODE EUROEXPRESS
094900980129     O          E            LV22Y       1
095000980129     O                                              'BDEF;25;BCSH;100;'
095100011119     O          E   n23      LV22Y       1
095200011119     O                                              'BCPI;2;PRHR;1;'
095300011116     O          E   n23      LV22Y       1
095400011119     O                                              'HBR;080;VBR;320;'
095500011116     O                                              'BCST;"A'
095600011119     O*                      §DEJEU
095700011119     O*                      §DEISP
095800011119     O                       alfa5
095900011119     O                                              '@D'
096000011119     O*                      §LDLNP
096100011119     O*                      §LDNRS
096200011119     O*                      §LDNCD
096300011119     O                       nume12
096400011119     O                                              '@F'
096500011119     O*                      §deisa
096600011119     o                       alfa2
096700011116     O                       §end
096800011119     O          E    23      LV22Y       1
096900011119     O                                              'BCPI;1;PRHR;1;'
097000011116     O          E    23      LV22Y       1
097100011119     O                     13                       'HBR;057;VBR;320;'
097200011119     O                    N13                       'HBR;082;VBR;320;'
097300980129     O                                              'BCST;"C@F'
097400980129     O                       §DEJEU
097500980129     O                       §DEISP
097600980129     O                                              '@D'
097700980129     O                       §LDLNP
097800980129     O                       §LDNRS
097900980129     O                       §LDNCD
098000011119     O                                              '@F '
098100980129     O                       §DE55L
098200980129     O                       §BCEND
098300980129      *---*   RIFERIMENTI BARTOLINI
098400980129     O          E            LV22Y       1
098500011119     O                                              'HBR;080;VBR;360;'
098600980129     O                                              'DCH;020;DCW;020;ICS;02;'
098700980129     O                                              '"'
098800980129     O                       §LDLNP
098900980129     O                                              ' '
099000980129     O                       §LDNRS
099100980129     O                                              ' '
099200980129     O                       §LDNCD
099300980129     O                                              '";'
099400980129     O********* Chiusura etchetta
099500980129     O          E            LV22Y       1
099600980129     O********                               CON COVERTITORE
099700040430     O**                                            'TRM;%%5C%'
099800980129     O********                               CON VIDEO
099900980129     O                                              'TRM;\'
100000000121     o*-----------------------------------------------------------------
100100000121     o*-etichetta dove ' DPD ' -----------------------------------------
100200000121     O          E            LV22dpd1    1
100300000121     O                                            9 '~^"LV22";'
100400000121     O          E            LV22dpd1    1
100500000124     O                                           16 '00001;0;389;020;'
100600000121     O          E            LV22dpd1    1
100700000121     O*FISICA
100800000121     O** X QB400
100900000121     O                                           21 'UTOF;4100;TEAR;0;SPB;'
101000000121     O          E            LV22dpd1    1
101100000121     O*GIRO SCRITTA A 180°
101200000121     O                                            9 'FLIP;800;'
101300000121     O          E            LV22dpd1    1
101400000121     O                                            7 'DHR;$1;'
101500000121     O          E            LV22dpd1    1
101600000121     O                                           14 'HLT;01;VLT;01;'
101700000121      *---*   numero  spedizione
101800000121     O          E            LV22dpd1    1
101900000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
102000000121     O          E            LV22dpd1    1
102100000128     O                                           16 'HBR;350;VBR;060;'
102200000121     O          E            LV22dpd1    1
102300000121     O                                            1 '"'
102400000121     O                       §dpd3                4
102500000121     O                                            6 '";'
102600000121      * 8 caratteri numero spedizione
102700000121     O          E            LV22dpd1    1
102800000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
102900000121     O          E            LV22dpd1    1
103000000121     O                                           16 'HBR;400;VBR;060;'
103100000121     O          E            LV22dpd1    1
103200000121     O                                            1 '"'
103300000121     O                       §dpd8                9
103400000121     O                                           11 '";'
103500000121      *ultimo carattere numero spedizione
103600000121     O          E            LV22dpd1    1
103700000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
103800000121     O          E            LV22dpd1    1
103900000128     O                                           16 'HBR;510;VBR;060;'
104000000121     o          E            LV22dpd1    1
104100000121     O                                            1 '"'
104200000121     O                       §dpd12               2
104300000121     O                                            4 '";'
104400000121      *---*   costante scritta
104500000121     O          E            LV22dpd1    1
104600000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
104700000121     O          E            LV22dpd1    1
104800000121     O                                           16 'HBR;240;VBR;080;'
104900000121     O          E            LV22dpd1    1
105000000121     O                                            1 '"'
105100000124     O                       §dpdn1              39
105200000124     O                                           41 '";'
105300000121      *---*   costante scritta 2
105400000121     O          E            LV22dpd1    1
105500000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
105600000121     O          E            LV22dpd1    1
105700000121     O                                           16 'HBR;240;VBR;090;'
105800000121     O          E            LV22dpd1    1
105900000121     O                                            1 '"'
106000000124     O                       §dpdn2              39
106100000124     O                                           41 '";'
106200000121      *---*   costante scritta 3
106300000121     O          E            LV22dpd1    1
106400000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
106500000121     O          E            LV22dpd1    1
106600000121     O                                           16 'HBR;240;VBR;100;'
106700000121     O          E            LV22dpd1    1
106800000121     O                                            1 '"'
106900000124     O                       §dpdn3              39
107000000124     O                                           41 '";'
107100000124      *---*   costante scritta 4
107200000124     O          E            LV22dpd1    1
107300000124     O                                           23 'DCH;008;DCW;005;ICS;01;'
107400000124     O          E            LV22dpd1    1
107500000124     O                                           16 'HBR;240;VBR;110;'
107600000124     O          E            LV22dpd1    1
107700000124     O                                            1 '"'
107800000124     O                       §dpdn4              40
107900000124     O                                           42 '";'
108000000121      *---*   costante barra sopra
108100000121     O          E            LV22dpd1    1
108200000124     O                                           16 'HBR;130;VBR;178;'
108300000121     O          E            LV22dpd1    1
108400000124     O                                           18 'HLT;2;DHL;0;0;307;'
108500000121      *---*   costante barra sotto
108600000121     O          E            LV22dpd1    1
108700000121     O                                           16 'HBR;130;VBR;280;'
108800000121     O          E            LV22dpd1    1
108900000124     O                                           18 'HLT;2;DHL;0;0;307;'
109000000121     o* secondo codice a barre
109100000121     O          e            lv22dpd1    1
109200000121     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
109300000121     O          e            lv22dpd1    1
109400000121     O                                            9 'BCSH;100;'
109500000121     O          E            LV22dpd1    1
109600000121     O                                           16 'HBR;130;VBR;280;'
109700000121     o          E            LV22dpd1    1
109800000121     O                                            5 'BCST;'
109900000121     O                                            6 '"'
110000000121     O                       §deawb              16
110100000121     O                                           18 '";'
110200000121     O                                           23 'BSTP;'
110300000121      *costante 3 v1/00
110400000121     O          E            LV22dpd1    1
110500000121     O                                           23 'DCH;040;DCW;040;ICS;02;'
110600000121     O          E            LV22dpd1    1
110700000124     O                                           16 'HBR;250;VBR;160;'
110800000121     o          E            LV22dpd1    1
110900000121     O                                            1 '"'
111000000124     O                       §dex1                6
111100000124     O                                            8 '";'
111200000121      *decodifica 2° bcc
111300000121     O          E            LV22dpd1    1
111400000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
111500000121     O          E            LV22dpd1    1
111600000121     O                                           16 'HBR;200;VBR;300;'
111700000121     o          E            LV22dpd1    1
111800000121     O                                            1 '"'
111900000121     O                       §dpdcap             14
112000000121     O                                           16 '";'
112100000121      *costante x
112200000121     O          E            LV22dpd1    1
112300000124     O                                           23 'DCH;040;DCW;030;ICS;02;'
112400000121     O          E            LV22dpd1    1
112500000124     O                                           16 'HBR;100;VBR;320;'
112600000121     o          E            LV22dpd1    1
112700000121     O                                            1 '"'
112800000121     O                       §desse               2
112900000121     O                                            4 '";'
113000000121      *costante 09
113100000121     O          E            LV22dpd1    1
113200000121     O                                           23 'DCH;060;DCW;030;ICS;02;'
113300000121     O          E            LV22dpd1    1
113400000124     O                                           16 'HBR;090;VBR;380;'
113500000121     o          E            LV22dpd1    1
113600000121     O                                            1 '"'
113700000121     O                       dpd9                 3
113800000121     O                                            5 '";'
113900000121      *linea di arrivo
114000000121     O          E            LV22dpd1    1
114100000121     O                                           23 'DCH;080;DCW;040;ICS;02;'
114200000121     O          E            LV22dpd1    1
114300000124     O                                           16 'HBR;270;VBR;380;'
114400000121     o          E            LV22dpd1    1
114500000121     O                                            1 '"'
114600000124     O                       §dex3                4
114700000121     O                                            6 '";'
114800000121      *zona arrivo
114900000121     O          E            LV22dpd1    1
115000000121     O                                           23 'DCH;060;DCW;030;ICS;02;'
115100000121     O          E            LV22dpd1    1
115200000124     O                                           16 'HBR;370;VBR;380;'
115300000121     o          E            LV22dpd1    1
115400000121     O                                            1 '"'
115500000124     O                       §dex4                3
115600000121     O                                            5 '";'
115700000121     O*------------------------------------------------------------------
115800000121     O          E            LV22dpd2    1
115900000121     O                                            9 '~^"LV22";'
116000000121     O          E            LV22dpd2    1
116100000124     O                                           16 '00001;0;389;020;'
116200000121     O          E            LV22dpd2    1
116300000121     O*FISICA
116400000121     O** X QB400
116500000121     O                                           21 'UTOF;4100;TEAR;0;SPB;'
116600000121     O*GIRO SCRITTA A 180°
116700000121     O          E            LV22dpd2    1
116800000121     O                                            9 'FLIP;800;'
116900000121     O          E            LV22dpd2    1
117000000121     O                                            7 'DHR;$1;'
117100000121     O          E            LV22dpd2    1
117200000121     O                                           14 'HLT;01;VLT;01;'
117300000121      *---*   numero  spedizione
117400000121     O          E            LV22dpd2    1
117500000121     O                                           23 'DCH;045;DCW;030;ICS;03;'
117600000121     O          E            LV22dpd2    1
117700000121     O                                           16 'HBR;330;VBR;060;'
117800000121     O          E            LV22dpd2    1
117900000121     O                                            1 '"'
118000000121     O                       §dpd3                4
118100000121     O                                            6 '";'
118200000121      * 8 caratteri numero spedizione
118300000121     O          E            LV22dpd2    1
118400000121     O                                           23 'DCH;040;DCW;025;ICS;02;'
118500000121     O          E            LV22dpd2    1
118600000121     O                                           16 'HBR;400;VBR;060;'
118700000121     O          E            LV22dpd2    1
118800000121     O                                            1 '"'
118900000121     O                       §dpd8                9
119000000121     O                                           11 '";'
119100000121      *ultimo carattere numero spedizione
119200000121     O          E            LV22dpd2    1
119300000121     O                                           23 'DCH;035;DCW;020;ICS;01;'
119400000121     O          E            LV22dpd2    1
119500000121     O                                           16 'HBR;520;VBR;060;'
119600000121     o          E            LV22dpd2    1
119700000121     O                                            1 '"'
119800000121     O                       §dpd12               2
119900000121     O                                            4 '";'
120000000121      *---*   costante TELEFONO
120100000121     O          E            LV22dpd2    1
120200000121     O                                           23 'DCH;012;DCW;012;ICS;01;'
120300000121     O          E            LV22dpd2    1
120400000121     O                                           16 'HBR;250;VBR;080;'
120500000121     O          E            LV22dpd2    1
120600000121     O                                            1 '"'
120700000121     O                       dpd2                20
120800000121     O                                           22 '";'
120900000121      *---*   costante fax
121000000121     O          E            LV22dpd2    1
121100000121     O                                           23 'DCH;012;DCW;012;ICS;01;'
121200000121     O          E            LV22dpd2    1
121300000121     O                                           16 'HBR;250;VBR;090;'
121400000121     O          E            LV22dpd2    1
121500000121     O                                            1 '"'
121600000121     O                       dpd2f               20
121700000121     O                                           22 '";'
121800000121      *costante mittente
121900000121     O          E            LV22dpd2    1
122000000121     O                                           23 'DCH;015;DCW;015;ICS;01;'
122100000121     O          E            LV22dpd2    1
122200000124     O                                           25 'HBR;190;VBR;100;'
122300000121     o          E            LV22dpd2    1
122400000121     O                                            1 '"'
122500000121     O                       dpd3                 8
122600000121     O                                           10 '";'
122700000121      *costante mittente2
122800000121     O          E            LV22dpd2    1
122900000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
123000000121     O          E            LV22dpd2    1
123100000124     O                                           25 'HBR;190;VBR;120;'
123200000121     o          E            LV22dpd2    1
123300000121     O                                            1 '"'
123400000121     O                       §ldrsm              36
123500000121     O                                           38 '";'
123600000121      *costante mittente 3
123700000121     O          E            LV22dpd2    1
123800000121     O                                           23 'DCH;020;DCW;020;ICS;01;'
123900000121     O          E            LV22dpd2    1
124000000124     O                                           25 'HBR;190;VBR;140;'
124100000121     o          E            LV22dpd2    1
124200000121     O                                            1 '"'
124300000121     O                       §ldprm               3
124400000121     O                                            5 '";'
124500000121      *costante destinatario
124600000121     O          E            LV22dpd2    1
124700000121     O                                           23 'DCH;015;DCW;015;ICS;01;'
124800000121     O          E            LV22dpd2    1
124900000124     O                                           25 'HBR;090;VBR;150;'
125000000121     o          E            LV22dpd2    1
125100000121     O                                            1 '"'
125200000121     O                       dpd4                11
125300000121     O                                           13 '";'
125400000121      *destinatario
125500000121     O          E            LV22dpd2    1
125600000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
125700000121     O          E            LV22dpd2    1
125800000124     O                                           20 'HBR;090;VBR;170;'
125900000121     o          E            LV22dpd2    1
126000000121     O                                            1 '"'
126100000121     O                       §ldrsd              36
126200000121     O                                           38 '";'
126300000121      * destinatario indirizzo
126400000121     O          E            LV22dpd2    1
126500000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
126600000121     O          E            LV22dpd2    1
126700000124     O                                           20 'HBR;090;VBR;190;'
126800000121     o          E            LV22dpd2    1
126900000121     O                                            1 '"'
127000000121     O                       §ldind              36
127100000121     O                                           38 '";'
127200000121      *località destinatario 7
127300000121     O          E            LV22dpd2    1
127400000121     O                                           23 'DCH;020;DCW;020;ICS;02;'
127500000121     O          E            LV22dpd2    1
127600000124     O                                           20 'HBR;090;VBR;210;'
127700000121     o          E            LV22dpd2    1
127800000121     O                                            1 '"'
127900000121     O                       §ldlod              36
128000000121     O                                           38 '";'
128100000121      *cap      destinatario 8
128200000121     O          E            LV22dpd2    1
128300000121     O                                           23 'DCH;030;DCW;030;ICS;03;'
128400000121     O          E            LV22dpd2    1
128500000124     O                                           20 'HBR;090;VBR;240;'
128600000121     o          E            LV22dpd2    1
128700000121     O                                            1 '"'
128800000121     O                       §ldcad              10
128900000121     O                                           12 '";'
129000000121      *cap      destinatario 8
129100000121     O          E            LV22dpd2    1
129200000121     O                                           23 'DCH;030;DCW;030;ICS;03;'
129300000121     O          E            LV22dpd2    1
129400000121     O                                           20 'HBR;250;VBR;240;'
129500000121     o          E            LV22dpd2    1
129600000121     O                                            1 '"'
129700000121     O                       §desia               4
129800000121     O                                            6 '";'
129900000121      *---*   costante barra sopra
130000000121     O          E            LV22dpd2    1
130100000124     O                                           16 'HBR;130;VBR;248;'
130200000121     O          E            LV22dpd2    1
130300000124     O                                           18 'HLT;2;DHL;0;0;360;'
130400000121      *---*   costante barra sotto
130500000121     O          E            LV22dpd2    1
130600000121     O                                           16 'HBR;130;VBR;350;'
130700000121     O          E            LV22dpd2    1
130800000124     O                                           18 'HLT;2;DHL;0;0;360;'
130900000121      *costante primo B-C
131000000121     O          e            lv22dpd2    1
131100000121     O                                           22 'BDEF;8;BNEW;4;BWEW;08;'
131200000121     O          e            lv22dpd2    1
131300000121     O                                            9 'BCSH;100;'
131400000121     O          E            LV22dpd2    1
131500000121     O                                           16 'HBR;130;VBR;350;'
131600000121     o          E            LV22dpd2    1
131700000121     O                                            5 'BCST;'
131800000121     O                                            6 '"'
131900000121     O                       §dpd                18
132000000121     O                                           20 '";'
132100000121     O                                           25 'BSTP;'
132200000121      *costante data
132300000121     O          E            LV22dpd2    1
132400000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
132500000121     O          E            LV22dpd2    1
132600000124     O                                           16 'HBR;090;VBR;380;'
132700000121     o          E            LV22dpd2    1
132800000121     O                                            1 '"'
132900000121     O                       datasp              11 '  /  /    '
133000000121     O                                           13 '";'
133100000121      *costante 1 collo
133200000121     O          E            LV22dpd2    1
133300000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
133400000121     O          E            LV22dpd2    1
133500000124     O                                           16 'HBR;220;VBR;380;'
133600000121     o          E            LV22dpd2    1
133700000121     O                                            1 '"'
133800000121     O                                            8 '1 Collo'
133900000121     O                                           10 '";'
134000000121      *costante peso
134100000121     O          E            LV22dpd2    1
134200000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
134300000121     O          E            LV22dpd2    1
134400000124     O                                           16 'HBR;320;VBR;380;'
134500000121     o          E            LV22dpd2    1
134600000121     O                                            1 '"'
134700000121     O                                            8 'Peso Kg'
134800000121     O                                           10 '";'
134900000121      * peso
135000000121     O          E            LV22dpd2    1
135100000124     O                                           23 'DCH;015;DCW;015;ICS;01;'
135200000121     O          E            LV22dpd2    1
135300000124     O                                           16 'HBR;420;VBR;380;'
135400000121     o          E            LV22dpd2    1
135500000121     O                                            1 '"'
135600000121     O                       §ldpkg        2     10
135700000121     O                                           12 '";'
135800000121      *------------------------------------------------------------------
135900000121      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
136000000121     O* costante dpd
136100000121     O          E            LV22dpd     1
136200000121     O                                           23 'DCH;030;DCW;070;ICS;03;'
136300000121     O          E            LV22dpd     1
136400000121     O                                           25 'HBR;170;VBR;110;DHR;$201;'
136500000121     o          E            LV22dpd     1
136600000121     O                                            1 '"'
136700000121     O                       dpd                  4
136800000121     O                                            6 '";'
136900000121     O* costante dpd riga sotto
137000000121     O*         E            LV22dpd     1
137100000121     O*                                          23 'DCH;006;DCW;005;ICS;01;'
137200000121     O*         E            LV22dpd     1
137300000121     O*                                          20 'HBR;210;VBR;083;'
137400000121     o*         E            LV22dpd     1
137500000121     O*                                           1 '"'
137600000121     O*                      dpd1                23
137700000121     O*                                          25 '";'
137800000121     o          E            LV22dpd     1
137900000121     O                                            5 'TRM;\'
