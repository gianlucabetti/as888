000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
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
002400010730     d  §s4                    1      4
002500010730     d  §s3                    5      7
002600910403     D                 DS
002700910403     D  §2                     1      2  0
002800910403     D                                     DIM(2)
002900910403     D  §2A                    1      2
003000910403     D                                     DIM(2)
003100910403     D DSBLD         E DS
003200971215     D DSBLDE        E DS
003300910403     D DSBLD7        E DS
003400910403     D  §C                     1     17  0
003500910403     D                                     DIM(17)
003600011120     D §bceurex        DS
003700011120     D  alfa5                         5
003800011120     D  nume12                       12  0
003900011120     D  alfa2                         2
004000000121     D §dpd            DS
004100000121     D  §dpd3                         3
004200000121     D  §dpd8                         8
004300000121     D  §dpd12                        1
004400000121     D §deawbds        DS
004500000121     D  §dpdc1w                       2
004600000121     D  §dpdc3w                       5
004700000121     D  §dpdc5w                       2
004800000121     D  §dpdc7w                       1
004900000124     D §dexx1ds        DS
005000000124     D  §dex1                         5
005100000124     D  §dex2                         5
005200000124     D  §dex3                         3
005300000124     D  §dex4                         2
005400000121     D §dpdcap         DS
005500000121     D  §dpdc1                        2
005600000121     D  §dpdc2                        1    inz('-')
005700000121     D  §dpdc3                        5
005800000121     D  §dpdc4                        1    inz('-')
005900000121     D  §dpdc5                        2
006000000121     D  §dpdc6                        1    inz('-')
006100000121     D  §dpdc7                        1
006200000121     d §dpdn           ds
006300000124     d §dpdn1                        38
006400000124     d §dpdn2                        38
006500000124     d §dpdn3                        38
006600000124     d §dpdn4                        39
006700000121     d dpd             c                   const('DPD')
006800000121     d dpd1            c                   const('Deutscher Paket Dienst')
006900000121     d dpd3            c                   const('Sender:')
007000000121     d dpd4            c                   const('Receiver:')
007100000121     d dpd2            ds
007200000121     d  dpdtel                        3    inz('Tel')
007300000121     d  dpdteln                      15
007400000121     d dpd2f           ds
007500000121     d  dpdfax                        3    inz('Fax')
007600000121     d  dpdfaxn                      15
007700000121     d dpd9            c                   const('01')
007800010730     d dspioppa        ds
007900010730     d  operat                        1    inz
008000010730     d  pista                         1    inz
008100010730     d  pallet                        2    inz
008200910403     I/SPACE 3
008300910403     C*---------------------------------------------------------------*
008400910403     C     *ENTRY        PLIST
008500910403     C                   PARM                    DSBLD
008600971215     C                   PARM                    DSBLDE
008700050603      *
008800050603      * Elimino carattere "\"
008900050603     c     '\':' '       xlate     §ldrsm        §ldrsm
009000050603     c     '\':' '       xlate     §ldrsd        §ldrsd
009100050603     c     '\':' '       xlate     §ldlod        §ldlod
009200050603     c     '\':' '       xlate     §ldind        §ldind
009300050603     c     '\':' '       xlate     §ldcdp        §ldcdp
009400050603      *
009500010926      *data
009600010926     c                   move      §ldmgs        giorno            2 0
009700010926     c                   movel     §ldmgs        mese              2 0
009800010926     c                   move      mese          giomes            4 0
009900010926     c                   movel     giorno        giomes
010000010926     c                   movel     giomes        datasp            8 0
010100010926     c                   move      §ldaas        datasp
010200050415      *
010300050415      * flag ristampa: " ", "R", "M", "V", "X", "W"
010400050415     c                   clear                   wLDrst            1
010500050415     c                   clear                   wLDrst2           1
010600050415     c                   select
010700050415      *
010800050415     c                   when      §LDrst  = *blanks
010900050415      *
011000050415     c                   when      §LDrst  = 'V'
011100050415     c                   eval      wLDrst2 = §LDrst
011200050415      *
011300050415     c                   when      §LDrst  = 'X'
011400050415     c                   eval      wLDrst  = 'R'
011500050415     c                   eval      wLDrst2 = 'V'
011600050415      *
011700050415     c                   when      §LDrst  = 'W'
011800050415     c                   eval      wLDrst  = 'M'
011900050415     c                   eval      wLDrst2 = 'V'
012000050415      *
012100050415     c                   other
012200050415     c                   eval      wLDrst  = §LDrst
012300050415      *
012400050415     c                   endsl
012500050415      *
012600941216     C*
012700951021     C*** PROVE CON VALORI FISSI + ETICHETTE
012800971217     C                   MOVEL     §LDNRS        §LDNRT            3
012900971217     C                   MOVE      '-'           §LDNRT
013000971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
013100971217     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
013200971217     C     §DETST        IFEQ      'EST'
013300971217     C                   EXSR      COLZER
013400971217     C                   ENDIF
013500980129     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
013600971215     C*
013700971215     C     §DETST        IFEQ      'EST'
013800971215     C     §DETST        OREQ      'ALL'
013900000121     c                   select
014000000121     c     §detse        wheneq    'REX'
014100010730     c*                  except    lv22e
014200000121     c     §detse        wheneq    'EUR'
014300000121     c                   exsr      eurexp
014400000121     c     §detse        wheneq    'DP1'
014500000121     c                   exsr      dpddove
014600000121     c     §detse        wheneq    'DP2'
014700000121     c                   exsr      dpddove
014800000121     c                   exsr      dpdchi
014900000121     c                   endsl
015000971215     C                   ENDIF
015100000202     C     §DETST        IFEQ      'ITA'
015200000202     C     §DETST        OREQ      'ALL'
015300000202     C                   EXSR      PRINT
015400000202     C                   ENDIF
015500971215     C*
015600910403     C                   SETON                                        LR
015700951021     C/SPACE 005
015800951021     C*****************************************************
015900951023     C* IMPOSTAZIONE E STAMPA                           ***
016000951021     C*****************************************************
016100951023     C     PRINT         BEGSR
016200951023     C                   MOVE      §LDLNP        §LCLNP
016300951023     C                   MOVE      §LDLNA        §LCLNA
016400951023     C                   MOVE      §LDNRS        §LCNRS
016500951023     C                   MOVE      §LDNCD        §LCNCD
016600951023     C                   MOVE      §LDZSC        §LCZSC
016700040428     C** non cancellare  MOVEl     §LDdla        dspioppa
016800040422     c     §detse        comp      'EEX'                                  55
016900951023     C*
017000951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
017100951023     C                   EXSR      M10
017200951023     C                   MOVEA     §CA           §C18A            18
017300951023     C                   MOVE      §C18A         §CDBAR           18
017400951023     C*
017500970221     C     §LDTSP        IFEQ      'D'
017600970303     C     §LDLNA        ANDLT     300
017700970221     C                   MOVE      *BLANK        §LDTSP
017800970221     C                   END
017900951023     C*
018000951023     C     §LDLNA        COMP      300                                15
018100951023     C     §LDTSP        COMP      *BLANK                             1616
018200951023     C     §LDTSP        COMP      'E'                                    17
018300951023     C     §LDCDP        COMP      *BLANK                             18
018400951023     C     §LDNRS        COMP      *ZERO                              19
018500951023     C*
018600951023     C** INIZIALIZZA STAMPANTE
018700951023     C                   EXCEPT    LV22
018800951023     C*
018900951023     C                   MOVEA     *BLANK        §RX
019000951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
019100951023     C                   SETOFF                                       77
019200951023     C                   MOVE      *BLANK        §A7               7
019300951023     C                   Z-ADD     §LDNC1        §N7
019400951023     C                   DO        7             §1
019500951023     C     §S7(§1)       IFEQ      *ZERO
019600951023     C                   MOVE      *BLANK        §S7(§1)
019700951023     C                   ELSE
019800951023     C                   SETON                                        77
019900951023     C                   END
020000951023     C  N77              END
020100951023     C                   MOVEA     §S7           §A7
020200951023     C                   MOVE      §A7           §A3               3
020300951023     C                   MOVEA     §A3           §RX(1)
020400951023     C                   MOVEA     '-'           §RX(4)
020500951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
020600951023     C                   SETOFF                                       77
020700951023     C                   MOVE      *BLANK        §A7
020800951023     C                   Z-ADD     §LDNC2        §N7
020900951023     C                   DO        7             §1
021000951023     C     §S7(§1)       IFEQ      *ZERO
021100951023     C                   MOVE      *BLANK        §S7(§1)
021200951023     C                   ELSE
021300951023     C                   SETON                                        77
021400951023     C                   END
021500951023     C  N77              END
021600951023     C                   MOVEA     §S7           §A7
021700951023     C                   MOVE      §A7           §A3
021800951023     C                   MOVEA     §A3           §RX(5)
021900951023     C                   MOVE      §LDCPD        §A3
022000951023     C                   MOVEA     §A3           §RX(9)
022100951023     C* STAMPA LINEA PARTENZA
022200951023     C                   EXCEPT    LV22H
022300951023     C*
022400951023     C                   MOVEA     *BLANK        §RI
022500951023     C                   MOVEA     §LDRSM        §RI(8)
022600951023     C                   MOVEA     ' '           §RI(30)
022700951023     C                   MOVEA     §LDPRM        §RI(31)
022800951023     C                   MOVEA     *BLANK        §RI(33)
022900951023     C                   EXCEPT    LV22D1
023000951023     C* STAMPA DATI MITTENTE
023100971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
023200971217     C                   EXSR      COLZER
023300951023     C*
023400951023     C* STAMPA NUMERO SEGNACOLLO
023500951023     C                   EXCEPT    LV22D2
023600951023     C*
023700951023     C                   MOVE      §LDTNA        §A3               3
023800951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
023900951023     C                   MOVE      §LDLNA        §A3
024000951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
024100951023     C                   MOVE      §LDZSC        §A2
024200951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
024300951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
024400951023     C                   MOVEL     §LDDLA        §ODDLA            8
024500951023     C* STAMPA LINEA ARRIVO
024600951023     C* X 15 STAMPA A "MODOD ESTERO"
024700951023     C   15              DO
024800951023     C                   MOVEL     §ODDLA        §EEN              3
024900951023     C                   MOVEL     §LDCAD        §CA2              2
025000951023     C                   MOVE      §LDCAD        §CA7              7
025100951023     C                   END
025200040422     C                   MOVEL     §LDCAD        §CA5              5
025300951023     C                   EXCEPT    LV22D3
025400951023     C*
025500951023     C                   MOVEA     *BLANK        §RI
025600951023     C                   MOVEA     §LDRSD        §RI(8)
025700951023     C                   MOVEA     ' '           §RI(30)
025800951023     C                   MOVEA     §LDPRD        §RI(31)
025900951023     C                   MOVEA     *BLANK        §RI(33)
026000951023     C* STAMPA 1° DATI DESTINATARIO
026100951023     C                   EXCEPT    LV22D4
026200951023     C*
026300951023     C                   MOVEA     *BLANK        §RI
026400951023     C                   MOVEA     §LDIND        §RI(8)
026500951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
026600951023     C                   Z-ADD     37            §Y                2 0    88
026700951023     C                   DO        36
026800951023     C                   SUB       1             §Y
026900951023     C     §RI(§Y)       IFNE      ' '
027000951023     C                   SETON                                        88
027100951023     C                   END
027200951023     C  N88              END
027300951023     C     §Y            IFLE      25
027400951023     C                   ADD       3             §Y
027500951023     C                   ELSE
027600951023     C                   MOVEA     '  '          §RI(26)
027700951023     C                   Z-ADD     28            §Y
027800951023     C                   END
027900951023     C                   MOVEA     §LDLOD        §RI(§Y)
028000951023     C* STAMPA 2° DATI DESTINATARIO
028100951023     C                   EXCEPT    LV22D5
028200951023     C* STAMPA SERVIZIO
028300951023     C   16              EXCEPT    LV22X5
028400951023     C*
028500951023     C* STAMPA BARCODE
028600951023     C*
028700951023     C* (CAMPO UNICO BARCODE E COSTANTI)
028800951023     C                   MOVE      *BLANK        §§CDB1           19
028900951023     C                   MOVE      §CDBAR        §§CDB1
029000951023     C                   MOVEL     '"'           §§CDB1
029100951023     C                   MOVE      *BLANK        §§CDB2           21
029200951023     C                   MOVEL     §§CDB1        §§CDB2
029300951023     C                   MOVE      '";'          §§CDB2
029400951023     C                   EXCEPT    LV22D6
029500951023     C*
029600951023     C                   MOVE      *BLANK        §RI21            21
029700951023     C   18              DO
029800951023     C                   MOVEL     'Rif.:'       §RI21
029900980402     C     §DEEUR        IFNE      *BLANK
030000980402     C     §DETST        ANDEQ     'ITA'
030100980402     C                   MOVEL     'JUT :'       §RI21
030200980402     C                   ENDIF
030300951023     C                   MOVE      §LDCDP        §RI21
030400951023     C                   END
030500951023     C* STAMPA RIFERIMENTO/RISTAMPA
030600951023     C                   EXCEPT    LV22D7
030700951023     C*
030800951023     C* STAMPA CHIUSURA ETICHETTA
030900951023     C                   EXCEPT    LV22D8
031000951023     C                   ENDSR
031100971217     C/SPACE 005
031200971217     C*****************************************************
031300971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
031400971217     C*****************************************************
031500971217     C     COLZER        BEGSR
031600971217     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
031700971217     C                   SETOFF                                       77
031800971217     C                   MOVE      §LDNCD        §N7
031900971217     C                   DO        7             §1
032000971217     C     §S7(§1)       IFEQ      *ZERO
032100971217     C                   MOVE      *BLANK        §S7(§1)
032200971217     C                   ELSE
032300971217     C                   SETON                                        77
032400971217     C                   END
032500971217     C  N77              END
032600971217     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
032700971217     C                   MOVE      *ZERO         §X                1 0
032800971217     C                   MOVEA     *BLANK        §O7
032900971217     C                   DO        7             §1
033000010925     C*    §S7(§1)       IFNE      *BLANK
033100971217     C                   ADD       1             §X
033200971217     C     §S7(§1)       IFEQ      '0'
033300011114     C     §S7(§1)       oreq      *blank
033400971217     C                   MOVE      'O'           §O7(§X)
033500971217     C                   ELSE
033600971217     C                   MOVE      §S7(§1)       §O7(§X)
033700971217     C                   END
033800010925     C*                  END
033900971217     C                   END
034000971217     C                   MOVEA     §O7           §S7
034100971217     C                   ENDSR
034200951023     C/SPACE 005
034300951023     C*****************************************************
034400980129     C* STAMPA EUROEXPRESS                              ***
034500951023     C*****************************************************
034600980129     C     EUREXP        BEGSR
034700980321     C*
034800980321     C* IMPOSTO COSTANTI
034900980321     C                   MOVEL     'JUT'         §DEJEU            3
035000980321     C                   MOVEL     '55L'         §DE55L            3
035100980321     C                   MOVEL     'IT'          §DEISP            2
035200010925      * controllo se contenuto 55L nel codice passato
035300010925     c     '55L'         scan      §deeur                                 23
035400011120
035500011120      * se contenuto 55l scompongo a modo vecchio
035600011120     c                   if        *in23
035700980321     C* ESTRAGGO IL CAP:
035800980321     C                   MOVE      *ZEROS        §Y
035900980321     C                   MOVEA     §DEEUR        §EU
036000980321     C* TROVO QUANTO è LUNGO IL BARCODE
036100980321     C     ' '           CHECKR    §DEEUR        §Y
036200980321     C* ISOLO IL CAP
036300980321     C                   MOVE      *BLANK        §O9
036400980321     C                   MOVE      *ZEROS        §W                2 0
036500980321     C     §Y            SUB       2             §Y
036600980321     C     22            DO        §Y            §X1               2 0
036700980321     C     §EU(§X1)      IFNE      *BLANKS
036800980321     C                   ADD       1             §W
036900980321     C                   MOVE      §EU(§X1)      §O9(§W)
037000980321     C                   END
037100980321     C                   END
037200980129     C*
037300980129     C                   MOVEA     §O9           §LDCAD
037400011120
037500011120      *scompongo a modo nuovo senza 55l
037600011120     c                   else
037700011120     c                   movel     §deeur        §bceurex
037800011120     c                   end
037900980129     C*
038000980129     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
038100980129     C* DEL BARCODE
038200980129     C                   MOVEL     '";BSTP;'     §END              7
038300980129     C     §LDCAD        CAT       §DEISA:0      §BCD             11
038400980129     C     §BCD          CAT       §END:0        §BCEND           20
038500980318     C*
038600980318     C* VERIFICO SE CONTRASSEGNO
038700980318     C*
038800980318     C     §DEFCA        COMP      'S'                                    14
038900980318     C  N14§DEFCA        COMP      'X'                                    14
039000990323     C*
039100990323     C* VERIFICO SE INGHILTERRA
039200990323     C*
039300980129     C                   EXCEPT    LV22Y
039400980129     C*
039500951021     C                   ENDSR
039600910403     C/SPACE 005
039700910403     C*****************************************************
039800910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
039900910403     C*****************************************************
040000910403     C     M10           BEGSR
040100910403     C                   MOVE      *ZERO         §TT               3 0
040200910403     C                   MOVE      *ZERO         §TT2              3 0
040300910403     C                   MOVE      *ZERO         §1                1 0
040400910403     C                   Z-ADD     18            §S                2 0
040500910403     C                   DO        17            §N                2 0
040600910403     C                   SUB       1             §S
040700910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
040800910403     C                   MOVE      §C(§N)        §CB(§N)
040900910403     C     §N            DIV       2             §CTR              3 0
041000910403     C                   MVR                     §RST              3 010
041100910403     C     *IN(10)       IFEQ      '1'
041200910403     C     §C(§S)        MULT      2             §N2               2 0
041300910403     C                   MOVE      §N2           §A2               2
041400910403     C                   MOVEA     §A2           §2A
041500910403     C                   XFOOT     §2            §CTR
041600910403     C                   ELSE
041700910403     C                   Z-ADD     §C(§S)        §CTR
041800910403     C                   END
041900910403     C                   ADD       §CTR          §TT
042000910403     C                   END
042100910403     C*
042200910403     C     §TT           ADD       9             §TT2
042300910403     C                   MOVE      0             §TT2
042400910403     C     §TT2          SUB       §TT           §1
042500910403     C*
042600910403     C                   MOVE      §1            §CB(18)
042700910403     C                   ENDSR
042800000121      *--------------------------------------------------
042900000121     c     dpddove       begsr
043000000121      *--------------------------------------------------
043100000121     c                   eval      §dpd = §deeur
043200000124     c                   eval      §dexx1ds = §dexx1
043300000121     c                   eval      §deawbds = §deawb
043400000121     c                   eval      §dpdc1 = §dpdc1w
043500000121     c                   eval      §dpdc3 = §dpdc3w
043600000121     c                   eval      §dpdc5 = §dpdc5w
043700000121     c                   eval      §dpdc7 = §dpdc7w
043800000121     c                   eval      §dpdn = 'Notifications on damages which are -
043900000121     c                             not recognizable from the outside have to -
044000000121     c                             have to be submitted to the transport -
044100000121     c                             organisation withing 7 days in written.'
044200000121     C                   except    LV22dpd1
044300000121     C                   except    LV22dpd
044400000121     C                   endsr
044500000121      *--------------------------------------------------
044600000121     c     dpdchi        begsr
044700000121      *--------------------------------------------------
044800000121     c                   move      §ldmgs        giorno            2 0
044900000121     c                   movel     §ldmgs        mese              2 0
045000000121     c                   move      mese          giomes            4 0
045100000121     c                   movel     giorno        giomes
045200000121     c                   movel     giomes        datasp            8 0
045300000121     c                   move      §ldaas        datasp
045400000121     c                   movel     §dexx2        dpdteln
045500000121     c                   move      §dexx2        dpdfaxn
045600000121     C                   except    LV22dpd2
045700000121     C                   except    LV22dpd
045800000121     C                   endsr
045900010730      *--------------------------------------------------------------
046000941216     OFNLV22P   E            LV22        1
046100961223     O                                            9 '~^"LV22";'
046200941216     O          E            LV22        1
046300941228     O*LOGICA                                N.ET. V (H) O=-35 X TB340
046400010926     O                                           16 '00001;0;265;073;'
046500941216     O          E            LV22        1
046600910403     O*FISICA
046700961223     O** X QB400
046800010730     O                                           21 'UTOF;2700;TEAR;0;SPB;'
046900941216     O          E            LV22        1
047000910403     O*GIRO SCRITTA A 180°
047100010730     O                                            9 'FLIP;300;'
047200010730     O          E            LV22        1
047300010730     O*                                       FONT
047400010730     O                                            7 'DHR;$1;'
047500010730     O          E            LV22        1
047600010730     O                                           14 'HLT;01;VLT;01;'
047700010926     O********* Mittente
047800010926     O          E            LV22D1      1
047900010926     O*                                       CARATTERE
048000010926     O                                           21 'DCH;15;DCW;26;ICS;03;'
048100010926     O          E            LV22D1      1
048200010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
048300011123     O                                           25 'VBR;290;HBR;610;DHR;$201;'
048400010926     O          E            LV22D1      1
048500010926     O                                            1 '"'
048600010926     O                       §RI                 38
048700010926     O                                           40 '";'
048800010926      *---*   costante barra separazione
048900010926     O          E            LV22d1      1
049000011123     O                                           16 'HBR;640;VBR;040;'
049100010926     O          E            LV22d1      1
049200010926     O                                           18 'VLT;2;DVL;0;0;200;'
049300010926     O********* Destinatario (Rag.Soc.)
049400010926     O          E            LV22D4      1
049500010926     O*                                       CARATTERE
049600010926     O                                           21 'DCH;15;DCW;26;ICS;03;'
049700010926     O          E            LV22D4      1
049800010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049900011123     O                                           25 'VBR;290;HBR;650;DHR;$201;'
050000010926     O          E            LV22D4      1
050100010926     O                                            1 '"'
050200010926     O                       §RI                 38
050300010926     O                                           40 '";'
050400010926     O********* Destinatario (Località)
050500010926     O          E            LV22D5      1
050600010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050700011123     O                                           25 'VBR;290;HBR;675;DHR;$201;'
050800010926     O          E            LV22D5      1
050900010926     O                                            1 '"'
051000010926     O                       §RI                 38
051100010926     O                                           40 '";'
051200010926     O********* MC. SPEDIZIONE
051300010926     O          E            LV22d5      1
051400010926     O*                                       CARATTERE
051500010926     O                                           23 'DCH;015;DCW;020;ICS;01;'
051600010926     O          E            LV22D5      1
051700010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
051800010926     O                                           25 'VBR;240;HBR;705;DHR;$201;'
051900010926     O          E            LV22D5      1
052000010926     O                                            1 '"'
052100010926     O                                            4 'MC.'
052200010926     O                       §LDVOL              11 '0 ,   '
052300010926     O                       §LDFVR              14
052400010926     O                                           16 '";'
052500010730     O********* SERVIZIO
052600010730     O          E            LV22X5      1
052700010730     O*                                       CARATTERE
052800010730     O                                           22 'DCH;30;DCW;050;ICS;03;'
052900010730     O          E            LV22X5      1
053000010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053100011120     O                                           23 'VBR;150;HBR;060;DHR;$1;'
053200010730     O          E            LV22X5      1
053300010730     O                                            1 '"'
053400010730     O                       §LDTSP               2
053500010730     O                                            4 '";'
053600010730     O          E            LV22X5      1
053700011120     O                                           21 'HOME;VBR;120;HBR;050;'
053800010730     O          E            LV22X5      1
053900010730     O* FONDO NERO                                     L   H
054000010730     O                     17                    17 'DCBX;0;0;060;040;'
054100010730     O* FONDO BIANCO                                   L   H
054200010730     O                    N17                    17 'DBOX;0;0;060;040;'
054300010730     O********* progr. colli e peso
054400010926     O          E            LV22H       1
054500010926     O*                                       CARATTERE
054600010926     O                                           23 'DCH;015;DCW;020;ICS;01;'
054700010926     O          E            LV22H       1
054800010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054900010926     O                                           25 'VBR;160;HBR;705;DHR;$201;'
055000010926     O          E            LV22H       1
055100010926     O                                            1 '"'
055200010926     O                       §RX                 12
055300010926     O                                           16 'KG.'
055400010926     O                       §LDPKG              26 '    0 , '
055500010926     O                                           28 '";'
055600010730     O          E            LV22H       1
055700010730     O********* Linea Partenza
055800010730     O*                                       CARATTERE
055900010730     O                                           23 'DCH;050;DCW;035;ICS;01;'
056000010730     O          E            LV22H       1
056100010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
056200011120     O                                           23 'VBR;200;HBR;050;DHR;$1;'
056300010730     O          E            LV22H       1
056400010730     O                                            1 '"'
056500010730     O                       §LDLNP               4
056600010730     O                                            6 '";'
056700010730     O********* Segnacollo
056800010730     O          E    19      LV22D2      1
056900010730     O*                                       CARATTERE
057000011114     O                                           23 'DCH;050;DCW;050;ICS;08;'
057100010730     O          E    19      LV22D2      1
057200010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057300011114     O                                           23 'VBR;230;HBR;130;DHR;$1;'
057400010730     O          E    19      LV22D2      1
057500010730     O                                            1 '"'
057600010730     O                       §LDNRT               4
057700010730     O                                            6 '";'
057800010730     O          E            LV22D2      1
057900010730     O*                                       CARATTERE
058000021112     O                                           23 'DCH;050;DCW;050;ICS;12;'
058100010730     O          E            LV22D2      1
058200010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058300021112     O                                           23 'VBR;230;HBR;240;DHR;$1;'
058400010730     O          E            LV22D2      1
058500010730     O                                            1 '"'
058600010730     O                       §S4                  5
058700010730     O*                      §S7                  8
058800010730     O                                            7 '";'
058900010730     O          E            LV22D2      1
059000010730     O*                                       CARATTERE
059100021112     O                                           23 'DCH;075;DCW;065;ICS;12;'
059200010730     O          E            LV22D2      1
059300010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059400021112     O                                           23 'VBR;230;HBR;410;DHR;$1;'
059500010730     O          E            LV22D2      1
059600010730     O                                            1 '"'
059700010730     O                       §S3                  4
059800010730     O*                      §S7                  8
059900010730     O                                            6 '";'
060000010730     O********* DESCRIZIONE LINEA DI ARRIVO
060100010730     O          E            LV22D3      1
060200010730     O                                           14 'HLT;05;VLT;06;'
060300010730     O          E            LV22D3      1
060400010730     O*                                       CARATTERE
060500010730     O                                           21 'DCH;75;DCW;45;ICS;04;'
060600041115      *---*   Chiusura superiore barcode
060700041115     O          E            LV22D2      1
060800041115     O                                           16 'VBR;070;HBR;128;'
060900041115     O          E            LV22D2      1
061000041115     O                                           18 'VLT;398;DVL;0;0;4;'
061100041115      *---*   Chiusura inferiore barcode
061200041115     O          E            LV22D2      1
061300041115     O                                           16 'VBR;170;HBR;130;'
061400041115     O          E            LV22D2      1
061500041115     O                                           18 'VLT;398;DVL;0;0;2;'
061600010730     O********* Bar Code
061700010730     O          E            LV22D6      1
061800021112     O                                           16 'VBR;170;HBR;130;'
061900010730     O* BARCODE CODE 128
062000010730     O          E            LV22D6      1
062100010730     O                                           15 'BDEF;25;BCPI;2;'
062200010730     O          E            LV22D6      1
062300021112     O                                           18 'BCSH;095;BCST;"C";'
062400010730     O          E            LV22D6      1
062500010730     O                       §§CDB2              21
062600010730     O          E            LV22D6      1
062700010730     O                                            5 'BSTP;'
062800010730     O********* Codice Prodotto
062900050415      *       e 2° flag ristampa "V"
063000010730     O          E            LV22D7      1
063100010730     O*                                       CARATTERE
063200010730     O                                           21 'DCH;15;DCW;26;ICS;03;'
063300010730     O          E            LV22D7      1
063400010730     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
063500010926     O                                           23 'VBR;250;HBR;040;DHR;$1;'
063600010730     O          E            LV22D7      1
063700010730     O                                            1 '"'
063800010730     O                       §RI21               22
063900050415     O                       datasp              34 '  /  /    '
064000050415     O                       wLDRST              37
064100050415     o***                    wLDrst2             40
064200050415     O                                           42 '";'
064300050415      ********* 2° flag ristampa "V"
064400050415     o          E            LV22D7      1
064500050415     o                                           21 'DCH;25;DCW;35;ICS;03;'
064600050415     o          E            LV22D7      1
064700050415     o                                           23 'VBR;250;HBR;580;DHR;$1;'
064800050415     o          E            LV22D7      1
064900050415     o                                            1 '"'
065000050415     o                       wLDrst2              2
065100050415     o                                            4 '";'
065200010730     O********* TERMINAL/LINEA/ZONA ARRIVO
065300010926     O          E            LV22D3      1
065400010926     O                                           14 'HLT;01;VLT;02;'
065500010926     O          E            LV22D3      1
065600010926     O*                                       CARATTERE
065700021112     O                                           21 'DCH;65;DCW;65;ICS;08;'
065800010926     O********* TERMINAL
065900010926     O          E            LV22D3      1
066000010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066100030618     O                                           23 'VBR;060;HBR;105;DHR;$1;'
066200010926     O          E            LV22D3      1
066300010926     O                                            1 '"'
066400010926     O                       §ODTNA               4
066500010926     O                                            6 '";'
066600010926     O* BOX X TERMINAL ARRIVO
066700010926     O          E            LV22D3      1
066800010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066900030618     O                                           21 'HOME;VBR;015;HBR;100;'
067000010926     O          E            LV22D3      1
067100010926     O* FONDO NERO                                     L   H
067200021112     O                                           16 'DBOX;0;0;140;50;'
067300010926     O********* CODICE LINEA ARRIVO
067400010926     O          E            LV22D3      1
067500040422     O                                           21 'DCH;45;DCW;40;ICS;08;'
067600010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067700010926     O          E            LV22D3      1
067800040422     O                                           23 'VBR;065;HBR;260;DHR;$1;'
067900010926     O          E            LV22D3      1
068000010926     O                                            1 '"'
068100010926     O                       §ODLNA               4
068200010926     O                                            6 '";'
068300040422      *nazione per EEX
068400040422     O          E            LV22D3      1
068500040422     O                                           21 'DCH;40;DCW;40;ICS;08;'
068600040422     O          E    55      LV22d3      1
068700040422     O                                              'HBR;000;VBR;012;'
068800040422     O                                              'DBOX;365;020;240;040;'
068900040422     O          E    55      LV22d3      1
069000040422     O                                              'HBR;375;VBR;065;'
069100040422     O          E    55      LV22d3      1
069200040422     O                                              '"'
069300040422     O                       §DEISA
069400040422     O                                              '";'
069500040422      *CAP     per EEX
069600040422     O          E    55      LV22d3      1
069700040422     O                                           16 'HBR;460;VBR;065;'
069800040422     O          E    55      LV22d3      1
069900040422     O                                              '"'
070000040422     O                       §ca5
070100040422     O                                              '";'
070200010926     O********* CODICE pista
070300040422     O          E            LV22D3      1
070400040422     O                                           21 'DCH;30;DCW;30;ICS;08;'
070500040422     O          E            LV22D3      1
070600010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
070700040422     O                                           23 'VBR;030;HBR;250;DHR;$1;'
070800040422     O          E            Lv22D3      1
070900010926     O                                            1 '"'
071000010926     O                       pista                2
071100010926     O                                            4 '";'
071200010926     O********* CODICE pallet
071300010926     O          E            LV22D3      1
071400010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071500040422     O                                           23 'VBR;030;HBR;340;DHR;$1;'
071600010926     O          E            LV22D3      1
071700010926     O                                            1 '"'
071800010926     O                       pallet               3
071900010926     O                                            5 '";'
072000010926     O********* CODICE operatore
072100010926     O          E            LV22D3      1
072200010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
072300040422     O                                           25 'VBR;030;HBR;460;DHR;$1;'
072400010926     O          E            LV22D3      1
072500010926     O                                            1 '"'
072600010926     O                       operat               2
072700010926     O                                            4 '";'
072800010926     O********* ZONA ARRIVO
072900040422     O          E   n55      LV22D3      1
073000010926     O*                                       CARATTERE
073100021112     O                                           21 'DCH;65;DCW;65;ICS;08;'
073200040422     O          E   n55      LV22D3      1
073300010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
073400021112     O                                           23 'HBR;500;VBR;060;DHR;$1;'
073500040422     O          E   n55      LV22D3      1
073600010926     O                                            1 '"'
073700010926     O                       §ODZSC               3
073800010926     O                                            5 '";'
073900010926     O* BOX X ZONA ARRIVO
074000040422     O          E   n55      LV22D3      1
074100010926     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
074200011120     O                                           21 'HOME;VBR;015;HBR;490;'
074300040422     O          E   n55      LV22D3      1
074400010926     O* FONDO bianco                                   L   H
074500021112     O                                           16 'DBOX;0;0;100;50;'
074600010730     O********* Chiusura etchetta
074700010730     O          E            LV22D8      1
074800961223     O                                            5 'TRM;\'
074900980129     O*-------------------------------------------------------
075000980129     O* ETICHETTA ESTERO EUROEXPRESS
075100980129     O*-------------------------------------------------------
075200980129     O          E            LV22Y            01
075300980129     O                                              '~^"LV22";'
075400980129     O          E            LV22Y       1
075500980129     O*LOGICA                                N.ET. V (H) O=-35 X TB340
075600010730     O                                              '00001;0;265;73;'
075700980129     O          E            LV22Y       1
075800980129     O*FISICA
075900010730     O                                              'UTOF;2700;SPB;'
076000980129     O          E            LV22Y       1
076100980129     O*GIRO SCRITTA A 180°
076200010730     O                                              'FLIP;300;'
076300980129     O          E            LV22Y       1
076400980129     O                                              'HBR;00;VBR;00;'
076500980129     O          E            LV22Y       1
076600980129     O*                                       FONT
076700980129     O                                              'DHR;$1;'
076800980129     O          E            LV22Y       1
076900980129     O                                              'HLT;01;VLT;01;'
077000010730      *---*   NAZIONE PARTENZA
077100010730     O          E            LV22Y       1
077200010926     O                                              'HBR;110;VBR;030;'
077300010730     O                                              'DCH;30;DCW;30;ICS;02;'
077400010730     O                                              '"'
077500010730     O                       §DEISP
077600010730     O                                              '";'
077700010730      *---*   CONTRASSEGNO
077800010730     O          E    14      LV22Y       1
077900010926     O                                              'HBR;000;VBR;007;'
078000010926     O                                              'DBOX;520;000;170;040;'
078100010730     O          E    14      LV22Y       1
078200010926     O                                              'HBR;530;VBR;040;'
078300010730     O                                              'DCH;040;DCW;070;ICS;02;'
078400010730     O                                              '"COD";'
078500010730      *---*   EUROEXPRESS
078600010730     O          E            LV22Y       1
078700010926     O                                              'HBR;180;VBR;040;'
078800010730     O                                              'DCH;030;DCW;050;ICS;02;'
078900010730     O                                              '"EuroExpress";'
079000010730      *---*   NAZIONE ARRIVO
079100010730     O          E            LV22Y       1
079200010730     O                                              'HBR;000;VBR;012;'
079300010730     O                                              'DBOX;120;040;140;050;'
079400010730     O          E            LV22Y       1
079500010730     O                                              'HBR;130;VBR;065;'
079600010730     O                                              'DCH;015;DCW;015;ICS;02;'
079700010730     O                                              '"Country";'
079800010730     O          E            LV22Y       1
079900010730     O                                              'HBR;130;VBR;095;'
080000010730     O                                              'DCH;040;DCW;050;ICS;02;'
080100010730     O                                              '"'
080200010730     O                       §DEISA
080300010730     O                                              '";'
080400010730      *---*   CAP DESTINO
080500010730     O          E            LV22Y       1
080600010730     O                                              'HBR;000;VBR;012;'
080700010730     O                                              'DBOX;310;040;280;050;'
080800010730     O          E            LV22Y       1
080900010730     O                                              'HBR;320;VBR;065;'
081000010730     O                                              'DCH;015;DCW;015;ICS;02;'
081100010730     O                                              '"Post Code";'
081200010730     O          E            LV22Y       1
081300010730     O                                              'HBR;320;VBR;095;'
081400010730     O                                              'DCH;040;DCW;050;ICS;02;'
081500010730     O                                              '"'
081600010730     O                       §LDCAD
081700010730     O                                              '";'
081800010730      *---*   BARCODE EUROEXPRESS
081900010730     O          E            LV22Y       1
082000010730     O                                              'BDEF;25;BCSH;100;'
082100011120     O          E   n23      LV22Y       1
082200011120     O                                              'BCPI;2;PRHR;1;'
082300010925     O          E   n23      LV22Y       1
082400011120     O                                              'HBR;070;VBR;210;'
082500010731     O                                              'BCST;"A'
082600011120     O                       alfa5
082700011120     O                                              '@D'
082800011120     O                       nume12
082900011120     O                                              '@F'
083000011120     o                       alfa2
083100010731     O                       §end
083200010925      * barcode con 55l
083300011120     O          E    23      LV22Y       1
083400011120     O                                              'BCPI;1;PRHR;1;'
083500010925     O          E    23      LV22Y       1
083600011120     O                                              'HBR;070;VBR;210;'
083700010926     O                                              'BCST;"C@F'
083800010925     O                       §DEJEU
083900010925     O                       §DEISP
084000010926     O                                              '@D'
084100010925     O                       §LDLNP
084200010925     O                       §LDNRS
084300010925     O                       §LDNCD
084400010926     O                                              '@F '
084500010925     O                       §DE55L
084600010925     O                       §BCEND
084700011120     O*         E            LV22Y       1
084800010730      *---*   DECODIFICA
084900011120     O*                                             'HBR;220;VBR;230;'
085000011120     O*                                             'DCH;15;DCW;18;ICS;02;'
085100011120     O*                                             '"'
085200011120     O*                      §DEJEU
085300011120     O*                      §DEISP
085400011120     O*                      §LDLNP
085500011120     O*                      §LDNRS
085600011120     O*                      §LDNCD
085700011120     O*             n23      §deisa
085800011120     O*              23                             ' '
085900011120     O*              23      §DE55L
086000011120     O*              23      §BCD
086100011120     O*                                             '";'
086200010730      *---*   RIFERIMENTI BARTOLINI
086300010730     O          E            LV22Y       1
086400010926     O                                              'HBR;035;VBR;245;'
086500010730     O                                              'DCH;020;DCW;020;ICS;02;'
086600010730     O                                              '"'
086700010730     O                       §LDLNP
086800010730     O                                              ' '
086900010730     O                       §LDNRS
087000010730     O                                              ' '
087100010730     O                       §LDNCD
087200010730     O                                              '";'
087300010730     O********* Chiusura etchetta
087400010730     O          E            LV22Y       1
087500980129     O                                              'TRM;\'
087600000121     o*-----------------------------------------------------------------
087700000121     o*-etichetta dove ' DPD ' -----------------------------------------
087800000121     O          E            LV22dpd1    1
087900000121     O                                            9 '~^"LV22";'
088000000121     O          E            LV22dpd1    1
088100010730     O                                           16 '00001;0;265;020;'
088200000121     O          E            LV22dpd1    1
088300000121     O*FISICA
088400000121     O** X QB400
088500010730     O                                           21 'UTOF;2700;TEAR;0;SPB;'
088600000121     O          E            LV22dpd1    1
088700000121     O*GIRO SCRITTA A 180°
088800010730     O                                            9 'FLIP;300;'
088900000121     O          E            LV22dpd1    1
089000010730     O                                            9 'DHR;$201;'
089100010730     O          E            LV22dpd1    1
089200010730     O                                           14 'HLT;01;VLT;01;'
089300010730      *---*   numero  spedizione
089400010730     O          E            LV22dpd1    1
089500010730     O                                           23 'DCH;020;DCW;020;ICS;03;'
089600010730     O          E            LV22dpd1    1
089700010730     O                                           16 'HBR;080;VBR;240;'
089800010730     O          E            LV22dpd1    1
089900010730     O                                            1 '"'
090000010730     O                       §dpd3                4
090100010730     O                                            6 '";'
090200010730      * 8 caratteri numero spedizione
090300010730     O          E            LV22dpd1    1
090400010730     O                                           23 'DCH;020;DCW;020;ICS;02;'
090500010730     O          E            LV22dpd1    1
090600010730     O                                           16 'HBR;080;VBR;215;'
090700010730     O          E            LV22dpd1    1
090800010730     O                                            1 '"'
090900010730     O                       §dpd8                9
091000010730     O                                           11 '";'
091100010730      *ultimo carattere numero spedizione
091200010730     O          E            LV22dpd1    1
091300010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
091400010730     O          E            LV22dpd1    1
091500010730     O                                           16 'HBR;080;VBR;160;'
091600010730     o          E            LV22dpd1    1
091700010730     O                                            1 '"'
091800010730     O                       §dpd12               2
091900010730     O                                            4 '";'
092000010730      *---*   costante scritta
092100010730     O          E            LV22dpd1    1
092200010730     O                                           23 'DCH;010;DCW;010;ICS;01;'
092300010730     O          E            LV22dpd1    1
092400010730     O                                           16 'HBR;150;VBR;230;'
092500010730     O          E            LV22dpd1    1
092600010730     O                                            1 '"'
092700010730     O                       §dpdn1              40
092800010730     O                                           42 '";'
092900010730      *---*   costante scritta 2
093000010730     O          E            LV22dpd1    1
093100010730     O                                           23 'DCH;010;DCW;010;ICS;01;'
093200010730     O          E            LV22dpd1    1
093300010730     O                                           16 'HBR;170;VBR;230;'
093400010730     O          E            LV22dpd1    1
093500010730     O                                            1 '"'
093600010730     O                       §dpdn2              40
093700010730     O                                           42 '";'
093800010730      *---*   costante scritta 3
093900010730     O          E            LV22dpd1    1
094000010730     O                                           23 'DCH;010;DCW;010;ICS;01;'
094100010730     O          E            LV22dpd1    1
094200010730     O                                           16 'HBR;190;VBR;230;'
094300010730     O          E            LV22dpd1    1
094400010730     O                                            1 '"'
094500010730     O                       §dpdn3              40
094600010730     O                                           42 '";'
094700010730      *---*   costante scritta 4
094800010730     O          E            LV22dpd1    1
094900010730     O                                           23 'DCH;010;DCW;010;ICS;01;'
095000010730     O          E            LV22dpd1    1
095100010730     O                                           16 'HBR;210;VBR;230;'
095200010730     O          E            LV22dpd1    1
095300010730     O                                            1 '"'
095400010730     O                       §dpdn4              40
095500010730     O                                           42 '";'
095600010730      *---*   costante barra sopra
095700010730     O          E            LV22dpd1    1
095800010730     O                                           16 'HBR;318;VBR;050;'
095900010730     O          E            LV22dpd1    1
096000011123     O                                           18 'VLT;2;DVL;0;0;164;'
096100010730      *---*   costante barra sotto
096200010730     O          E            LV22dpd1    1
096300010730     O                                           16 'HBR;520;VBR;050;'
096400010730     O          E            LV22dpd1    1
096500011123     O                                           18 'VLT;2;DVL;0;0;164;'
096600010730     o* secondo codice a barre
096700010730     O          e            lv22dpd1    1
096800011123     O*                                          23 'BSYM;8;4;BNEW;2;BWEW;5;'
096900011123     O                                           14 'BDEF;9;BCPI;4;'
097000010730     O          e            lv22dpd1    1
097100010730     O                                            9 'BCSH;200;'
097200010730     O          E            LV22dpd1    1
097300010730     O                                           16 'HBR;320;VBR;050;'
097400010730     o          E            LV22dpd1    1
097500011123     O                                            5 'BCST;'
097600010730     O                                            6 '"'
097700010730     O                       §deawb              16
097800010730     O                                           18 '";'
097900010730     O                                           23 'BSTP;'
098000010730      *costante 3 v1/00
098100010730     O          E            LV22dpd1    1
098200010730     O                                           23 'DCH;040;DCW;040;ICS;02;'
098300010730     O          E            LV22dpd1    1
098400010730     O                                           16 'HBR;235;VBR;180;'
098500010730     o          E            LV22dpd1    1
098600010730     O                                            1 '"'
098700010730     O                       §dex1                6
098800010730     O                                            8 '";'
098900010730      *decodifica 2° bcc
099000010730     O          E            LV22dpd1    1
099100010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
099200010730     O          E            LV22dpd1    1
099300010730     O                                           16 'HBR;530;VBR;180;'
099400010730     o          E            LV22dpd1    1
099500010730     O                                            1 '"'
099600010730     O                       §dpdcap             14
099700010730     O                                           16 '";'
099800010730      *costante x
099900010730     O          E            LV22dpd1    1
100000010730     O                                           23 'DCH;050;DCW;030;ICS;02;'
100100010730     O          E            LV22dpd1    1
100200010730     O                                           16 'HBR;520;VBR;260;'
100300010730     o          E            LV22dpd1    1
100400010730     O                                            1 '"'
100500010730     O                       §desse               2
100600010730     O                                            4 '";'
100700010730      *costante 09
100800010730     O          E            LV22dpd1    1
100900010730     O                                           23 'DCH;060;DCW;040;ICS;02;'
101000010730     O          E            LV22dpd1    1
101100010730     O                                           16 'HBR;620;VBR;270;'
101200010730     o          E            LV22dpd1    1
101300010730     O                                            1 '"'
101400010730     O                       dpd9                 3
101500010730     O                                            5 '";'
101600010730      *linea di arrivo
101700010730     O          E            LV22dpd1    1
101800010730     O                                           23 'DCH;080;DCW;060;ICS;02;'
101900010730     O          E            LV22dpd1    1
102000010730     O                                           16 'HBR;600;VBR;200;'
102100010730     o          E            LV22dpd1    1
102200010730     O                                            1 '"'
102300010730     O                       §dex3                4
102400010730     O                                            6 '";'
102500010730      *zona arrivo
102600010730     O          E            LV22dpd1    1
102700010731     O                                           23 'DCH;060;DCW;040;ICS;02;'
102800010730     O          E            LV22dpd1    1
102900010730     O                                           16 'HBR;620;VBR;100;'
103000010730     o          E            LV22dpd1    1
103100010730     O                                            1 '"'
103200010730     O                       §dex4                3
103300010730     O                                            5 '";'
103400000121     O*------------------------------------------------------------------
103500000121     O          E            LV22dpd2    1
103600000121     O                                            9 '~^"LV22";'
103700000121     O          E            LV22dpd2    1
103800010730     O                                           16 '00001;0;265;020;'
103900000121     O          E            LV22dpd2    1
104000000121     O*FISICA
104100000121     O** X QB400
104200010730     O                                           21 'UTOF;2700;TEAR;0;SPB;'
104300000121     O*GIRO SCRITTA A 180°
104400000121     O          E            LV22dpd2    1
104500010730     O                                            9 'FLIP;300;'
104600000121     O          E            LV22dpd2    1
104700010730     O                                            9 'DHR;$201;'
104800010730     O          E            LV22dpd2    1
104900011123     O*                                          14 'HLT;01;VLT;01;'
105000010730      *---*   numero  spedizione
105100010730     O          E            LV22dpd2    1
105200010730     O                                           23 'DCH;050;DCW;030;ICS;03;'
105300010730     O          E            LV22dpd2    1
105400010730     O                                           16 'HBR;130;VBR;240;'
105500010730     O          E            LV22dpd2    1
105600010730     O                                            1 '"'
105700010730     O                       §dpd3                4
105800010730     O                                            6 '";'
105900010730      * 8 caratteri numero spedizione
106000010730     O          E            LV22dpd2    1
106100010730     O                                           23 'DCH;040;DCW;030;ICS;02;'
106200010730     O          E            LV22dpd2    1
106300010730     O                                           16 'HBR;135;VBR;200;'
106400010730     O          E            LV22dpd2    1
106500010730     O                                            1 '"'
106600010730     O                       §dpd8                9
106700010730     O                                           11 '";'
106800010730      *ultimo carattere numero spedizione
106900010730     O          E            LV22dpd2    1
107000010730     O                                           23 'DCH;035;DCW;020;ICS;01;'
107100010730     O          E            LV22dpd2    1
107200010730     O                                           16 'HBR;140;VBR;120;'
107300010730     o          E            LV22dpd2    1
107400010730     O                                            1 '"'
107500010730     O                       §dpd12               2
107600010730     O                                            4 '";'
107700010730      *---*   costante TELEFONO
107800010730     O          E            LV22dpd2    1
107900010730     O                                           23 'DCH;010;DCW;007;ICS;01;'
108000010730     O          E            LV22dpd2    1
108100010730     O                                           16 'HBR;080;VBR;230;'
108200010730     O          E            LV22dpd2    1
108300010730     O                                            1 '"'
108400010730     O                       dpd2                20
108500010730     O                                           22 '";'
108600010730      *---*   costante fax
108700010730     O          E            LV22dpd2    1
108800010730     O                                           23 'DCH;010;DCW;007;ICS;01;'
108900010730     O          E            LV22dpd2    1
109000010730     O                                           16 'HBR;100;VBR;230;'
109100010730     O          E            LV22dpd2    1
109200010730     O                                            1 '"'
109300010730     O                       dpd2f               20
109400010730     O                                           22 '";'
109500010730      *costante mittente
109600010730     O          E            LV22dpd2    1
109700010730     O                                           23 'DCH;015;DCW;015;ICS;01;'
109800010730     O          E            LV22dpd2    1
109900010731     O                                           25 'HBR;205;VBR;260;'
110000010730     o          E            LV22dpd2    1
110100010730     O                                            1 '"'
110200010730     O                       dpd3                 8
110300010730     O                                           10 '";'
110400010730      *costante mittente2
110500010730     O          E            LV22dpd2    1
110600010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
110700010730     O          E            LV22dpd2    1
110800010731     O                                           25 'HBR;230;VBR;265;'
110900010730     o          E            LV22dpd2    1
111000010730     O                                            1 '"'
111100010730     O                       §ldrsm              36
111200010730     O                                           38 '";'
111300010730      *costante mittente 3
111400010730     O          E            LV22dpd2    1
111500010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
111600010730     O          E            LV22dpd2    1
111700010731     O                                           25 'HBR;260;VBR;265;'
111800010730     o          E            LV22dpd2    1
111900010730     O                                            1 '"'
112000010730     O                       §ldprm               3
112100010730     O                                            5 '";'
112200010730      *costante destinatario
112300010730     O          E            LV22dpd2    1
112400010730     O                                           23 'DCH;015;DCW;015;ICS;01;'
112500010730     O          E            LV22dpd2    1
112600010731     O                                           25 'HBR;300;VBR;260;'
112700010730     o          E            LV22dpd2    1
112800010730     O                                            1 '"'
112900010730     O                       dpd4                11
113000010730     O                                           13 '";'
113100010730      *destinatario
113200010730     O          E            LV22dpd2    1
113300010730     O                                           23 'DCH;020;DCW;020;ICS;02;'
113400010730     O          E            LV22dpd2    1
113500010731     O                                           20 'HBR;320;VBR;265;'
113600010730     o          E            LV22dpd2    1
113700010730     O                                            1 '"'
113800010730     O                       §ldrsd              36
113900010730     O                                           38 '";'
114000010730      * destinatario indirizzo
114100010730     O          E            LV22dpd2    1
114200010730     O                                           23 'DCH;020;DCW;020;ICS;02;'
114300010730     O          E            LV22dpd2    1
114400010731     O                                           20 'HBR;350;VBR;265;'
114500010730     o          E            LV22dpd2    1
114600010730     O                                            1 '"'
114700010730     O                       §ldind              36
114800010730     O                                           38 '";'
114900010730      *località destinatario 7
115000011123     O          E            LV22dpd2    1
115100011123     O                                           23 'DCH;020;DCW;020;ICS;02;'
115200011123     O          E            LV22dpd2    1
115300011123     O                                           20 'HBR;380;VBR;265;'
115400011123     o          E            LV22dpd2    1
115500011123     O                                            1 '"'
115600011123     O                       §ldlod              36
115700011123     o                                           38 '";'
115800010730      *cap      destinatario 8
115900010730     O          E            LV22dpd2    1
116000010730     O                                           23 'DCH;030;DCW;030;ICS;03;'
116100010730     O          E            LV22dpd2    1
116200010731     O                                           20 'HBR;420;VBR;265;'
116300010730     o          E            LV22dpd2    1
116400010730     O                                            1 '"'
116500010730     O                       §ldcad              10
116600010730     O                                           12 '";'
116700010730      *cap      destinatario 8
116800010730     O          E            LV22dpd2    1
116900010730     O                                           23 'DCH;030;DCW;030;ICS;03;'
117000010730     O          E            LV22dpd2    1
117100010730     O                                           20 'HBR;420;VBR;180;'
117200010730     o          E            LV22dpd2    1
117300010730     O                                            1 '"'
117400010730     O                       §desia               4
117500010730     O                                            6 '";'
117600010730      *---*   costante barra sopra
117700010730     O          E            LV22dpd2    1
117800011123     O                                           16 'HBR;478;VBR;030;'
117900010730     O          E            LV22dpd2    1
118000011123     O                                           18 'VLT;2;DVL;0;0;194;'
118100010730      *---*   costante barra sotto
118200010730     O          E            LV22dpd2    1
118300011123     O                                           16 'HBR;680;VBR;030;'
118400010730     O          E            LV22dpd2    1
118500011123     O                                           18 'VLT;2;DVL;0;0;194;'
118600010730      *costante primo B-C
118700010730     O          e            lv22dpd2    1
118800011123     O*                                          23 'BDEF;37;BNEW;3;BWEW;09;'
118900011123     O                                           14 'BDEF;9;BCPI;4;'
119000010730     O          e            lv22dpd2    1
119100010730     O                                            9 'BCSH;200;'
119200010730     O          E            LV22dpd2    1
119300011123     O                                           16 'HBR;480;VBR;030;'
119400010730     o          E            LV22dpd2    1
119500010730     O                                            5 'BCST;'
119600010730     O                                            6 '"'
119700010730     O                       §dpd                18
119800010730     O                                           20 '";'
119900010730     O                                           25 'BSTP;'
120000010730      *costante data
120100010730     O          E            LV22dpd2    1
120200010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
120300010730     O          E            LV22dpd2    1
120400010730     O                                           16 'HBR;690;VBR;270;'
120500010730     o          E            LV22dpd2    1
120600010730     O                                            1 '"'
120700010730     O                       datasp              11 '  /  /    '
120800010730     O                                           13 '";'
120900010730      *costante 1 collo
121000010730     O          E            LV22dpd2    1
121100010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
121200010730     O          E            LV22dpd2    1
121300010730     O                                           16 'HBR;690;VBR;200;'
121400010730     o          E            LV22dpd2    1
121500010730     O                                            1 '"'
121600010730     O                                            8 '1 Collo'
121700010730     O                                           10 '";'
121800010730      *costante peso
121900010730     O          E            LV22dpd2    1
122000010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
122100010730     O          E            LV22dpd2    1
122200010730     O                                           16 'HBR;690;VBR;150;'
122300010730     o          E            LV22dpd2    1
122400010730     O                                            1 '"'
122500010730     O                                            8 'Peso Kg'
122600010730     O                                           10 '";'
122700010730      * peso
122800010730     O          E            LV22dpd2    1
122900010730     O                                           23 'DCH;020;DCW;020;ICS;01;'
123000010730     O          E            LV22dpd2    1
123100010730     O                                           16 'HBR;690;VBR;100;'
123200010730     o          E            LV22dpd2    1
123300010730     O                                            1 '"'
123400010730     O                       §ldpkg        2     10
123500010730     O                                           12 '";'
123600000121      *------------------------------------------------------------------
123700000121      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
123800000121     O* costante dpd
123900000121     O          E            LV22dpd     1
124000010730     O                                           23 'DCH;030;DCW;070;ICS;03;'
124100010730     O          E            LV22dpd     1
124200010730     O                                           25 'HBR;200;VBR;280;DHR;$401;'
124300010730     o          E            LV22dpd     1
124400010730     O                                            1 '"'
124500010730     O                       dpd                  4
124600010730     O                                            6 '";'
124700010730     O* costante dpd riga sotto
124800010730     O*         E            LV22dpd     1
124900010730     O*                                          23 'DCH;006;DCW;005;ICS;01;'
125000010730     O*         E            LV22dpd     1
125100010730     O*                                          20 'HBR;200;VBR;220;'
125200010730     o*         E            LV22dpd     1
125300010730     O*                                           1 '"'
125400010730     O*                      dpd1                23
125500010730     O*                                          25 '";'
125600010730     o          E            LV22dpd     1
125700000121     O                                            5 'TRM;\'
