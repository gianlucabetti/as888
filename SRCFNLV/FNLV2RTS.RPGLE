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
002300010717     d  §s4                    1      4
002400010717     d  §s3                    5      7
002500910403     D                 DS
002600910403     D  §2                     1      2  0
002700910403     D                                     DIM(2)
002800910403     D  §2A                    1      2
002900910403     D                                     DIM(2)
003000910403     D DSBLD         E DS
003100971215     D DSBLDE        E DS
003200910403     D DSBLD7        E DS
003300910403     D  §C                     1     17  0
003400910403     D                                     DIM(17)
003500011120     D §bceurex        DS
003600011120     D  alfa5                         5
003700011120     D  nume12                       12  0
003800011120     D  alfa2                         2
003900000118     D §dpd            DS
004000000118     D  §dpd3                         3
004100000118     D  §dpd8                         8
004200000118     D  §dpd12                        1
004300000118     D §deawbds        DS
004400000118     D  §dpdc1w                       2
004500000118     D  §dpdc3w                       5
004600000118     D  §dpdc5w                       2
004700000118     D  §dpdc7w                       1
004800000124     D §dexx1ds        DS
004900000124     D  §dex1                         5
005000000124     D  §dex2                         5
005100000124     D  §dex3                         3
005200000124     D  §dex4                         2
005300000118     D §dpdcap         DS
005400000118     D  §dpdc1                        2
005500000118     D  §dpdc2                        1    inz('-')
005600000118     D  §dpdc3                        5
005700000118     D  §dpdc4                        1    inz('-')
005800000118     D  §dpdc5                        2
005900000118     D  §dpdc6                        1    inz('-')
006000000118     D  §dpdc7                        1
006100000118     d §dpdn           ds
006200000118     d §dpdn1                        51
006300000118     d §dpdn2                        49
006400000118     d §dpdn3                        53
006500000118     d dpd             c                   const('DPD')
006600000118     d dpd1            c                   const('Deutscher Paket Dienst')
006700000118     d dpd3            c                   const('Sender:')
006800000118     d dpd4            c                   const('Receiver:')
006900000118     d dpd2            ds
007000000118     d  dpdtel                        3    inz('Tel')
007100000118     d  dpdteln                      15
007200000118     d dpd2f           ds
007300000118     d  dpdfax                        3    inz('Fax')
007400000118     d  dpdfaxn                      15
007500000118     d dpd9            c                   const('01')
007600010719     d dspioppa        ds
007700010719     d  operat                        1    inz
007800010719     d  pista                         1    inz
007900010719     d  pallet                        2    inz
008000910403     I/SPACE 3
008100910403     C*---------------------------------------------------------------*
008200910403     C     *ENTRY        PLIST
008300910403     C                   PARM                    DSBLD
008400971215     C                   PARM                    DSBLDE
008500050606
008600050606      * Elimino carattere "\"
008700050606     c     '\':' '       xlate     §ldrsm        §ldrsm
008800050606     c     '\':' '       xlate     §ldrsd        §ldrsd
008900050606     c     '\':' '       xlate     §ldlod        §ldlod
009000050606     c     '\':' '       xlate     §ldind        §ldind
009100050606     c     '\':' '       xlate     §ldcdp        §ldcdp
009200050606
009300010718      *data
009400010718     c                   move      §ldmgs        giorno            2 0
009500010718     c                   movel     §ldmgs        mese              2 0
009600010718     c                   move      mese          giomes            4 0
009700010718     c                   movel     giorno        giomes
009800010718     c                   movel     giomes        datasp            8 0
009900010718     c                   move      §ldaas        datasp
010000050415      *
010100050415      * flag ristampa: " ", "R", "M", "V", "X", "W"
010200050415     c                   clear                   wLDrst            1
010300050415     c                   clear                   wLDrst2           1
010400050415     c                   select
010500050415      *
010600050415     c                   when      §LDrst  = *blanks
010700050415      *
010800050415     c                   when      §LDrst  = 'V'
010900050415     c                   eval      wLDrst2 = §LDrst
011000050415      *
011100050415     c                   when      §LDrst  = 'X'
011200050415     c                   eval      wLDrst  = 'R'
011300050415     c                   eval      wLDrst2 = 'V'
011400050415      *
011500050415     c                   when      §LDrst  = 'W'
011600050415     c                   eval      wLDrst  = 'M'
011700050415     c                   eval      wLDrst2 = 'V'
011800050415      *
011900050415     c                   other
012000050415     c                   eval      wLDrst  = §LDrst
012100050415      *
012200050415     c                   endsl
012300050415      *
012400941216     C*
012500951021     C*** PROVE CON VALORI FISSI + ETICHETTE
012600951023     C***                  DO   10
012700951023     C***                  TIME           §HMS    60
012800951023     C***                  MOVEL§HMS      §LDCDP
012900951023     C***                  ADD  1         §LDNCD
013000941216     C*
013100971222     C                   MOVEL     §LDNRS        §LDNRT            3
013200971222     C                   MOVE      '-'           §LDNRT
013300971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
013400971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
013500971222     C     §DETST        IFEQ      'EST'
013600971222     C                   EXSR      COLZER
013700971222     C                   ENDIF
013800971222     C*
013900910403     C*
014000951023     C***                  ENDDO
014100971215     C*
014200980402     C*
014300980326     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
014400971215     C*
014500971215     C     §DETST        IFEQ      'EST'
014600971215     C     §DETST        OREQ      'ALL'
014700000118     c                   select
014800000118     c     §detse        wheneq    'REX'
014900010718     c*                  except    lv22e
015000000118     c     §detse        wheneq    'EUR'
015100000118     c                   exsr      eurexp
015200000118     c     §detse        wheneq    'DP1'
015300000118     c                   exsr      dpddove
015400000118     c     §detse        wheneq    'DP2'
015500000118     c                   exsr      dpddove
015600000118     c                   exsr      dpdchi
015700000118     c                   endsl
015800971215     C                   ENDIF
015900000202     C     §DETST        IFEQ      'ITA'
016000000202     C     §DETST        OREQ      'ALL'
016100000202     C                   EXSR      PRINT
016200000202     C                   ENDIF
016300971215     C*
016400910403     C                   SETON                                        LR
016500951021     C/SPACE 005
016600951021     C*****************************************************
016700951023     C* IMPOSTAZIONE E STAMPA                           ***
016800951021     C*****************************************************
016900951023     C     PRINT         BEGSR
017000951023     C                   MOVE      §LDLNP        §LCLNP
017100951023     C                   MOVE      §LDLNA        §LCLNA
017200951023     C                   MOVE      §LDNRS        §LCNRS
017300951023     C                   MOVE      §LDNCD        §LCNCD
017400951023     C                   MOVE      §LDZSC        §LCZSC
017500040422     C** non cancellare  MOVEl     §LDdla        dspioppa
017600040422     c     §detse        comp      'EEX'                                  55
017700951023     C*
017800951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
017900951023     C                   EXSR      M10
018000951023     C                   MOVEA     §CA           §C18A            18
018100951023     C                   MOVE      §C18A         §CDBAR           18
018200970210     C*
018300970210     C     §LDTSP        IFEQ      'D'
018400970303     C     §LDLNA        ANDLT     300
018500970210     C                   MOVE      *BLANK        §LDTSP
018600970210     C                   END
018700970210     C*
018800951023     C     §LDLNA        COMP      300                                15
018900951023     C     §LDTSP        COMP      *BLANK                             1616
019000951023     C     §LDTSP        COMP      'E'                                    17
019100090806     C  N17§LDTSP        COMP      'H'                                    17
019200951023     C     §LDCDP        COMP      *BLANK                             18
019300951023     C     §LDNRS        COMP      *ZERO                              19
019400951023     C*
019500951023     C** INIZIALIZZA STAMPANTE
019600951023     C                   EXCEPT    LV22
019700951023     C*
019800951023     C                   MOVEA     *BLANK        §RX
019900951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
020000951023     C                   SETOFF                                       77
020100951023     C                   MOVE      *BLANK        §A7               7
020200951023     C                   Z-ADD     §LDNC1        §N7
020300951023     C                   DO        7             §1
020400951023     C     §S7(§1)       IFEQ      *ZERO
020500951023     C                   MOVE      *BLANK        §S7(§1)
020600951023     C                   ELSE
020700951023     C                   SETON                                        77
020800951023     C                   END
020900951023     C  N77              END
021000951023     C                   MOVEA     §S7           §A7
021100951023     C                   MOVE      §A7           §A3               3
021200951023     C                   MOVEA     §A3           §RX(1)
021300951023     C                   MOVEA     '-'           §RX(4)
021400951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
021500951023     C                   SETOFF                                       77
021600951023     C                   MOVE      *BLANK        §A7
021700951023     C                   Z-ADD     §LDNC2        §N7
021800951023     C                   DO        7             §1
021900951023     C     §S7(§1)       IFEQ      *ZERO
022000951023     C                   MOVE      *BLANK        §S7(§1)
022100951023     C                   ELSE
022200951023     C                   SETON                                        77
022300951023     C                   END
022400951023     C  N77              END
022500951023     C                   MOVEA     §S7           §A7
022600951023     C                   MOVE      §A7           §A3
022700951023     C                   MOVEA     §A3           §RX(5)
022800951023     C                   MOVE      §LDCPD        §A3
022900951023     C                   MOVEA     §A3           §RX(9)
023000951023     C* STAMPA LINEA PARTENZA
023100951023     C                   EXCEPT    LV22H
023200951023     C*
023300951023     C                   MOVEA     *BLANK        §RI
023400010717     C                   MOVEA     §LDRSM        §RI(6)
023500951023     C                   MOVEA     ' '           §RI(30)
023600951023     C                   MOVEA     §LDPRM        §RI(31)
023700951023     C                   MOVEA     *BLANK        §RI(33)
023800951023     C                   EXCEPT    LV22D1
023900951023     C* STAMPA DATI MITTENTE
024000951023     C*
024100951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024200971222     C                   EXSR      COLZER
024300951023     C*
024400971222     C*
024500951023     C* STAMPA NUMERO SEGNACOLLO
024600951023     C                   EXCEPT    LV22D2
024700951023     C*
024800951023     C                   MOVE      §LDTNA        §A3               3
024900951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
025000951023     C                   MOVE      §LDLNA        §A3
025100951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
025200951023     C                   MOVE      §LDZSC        §A2
025300951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
025400951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
025500951023     C                   MOVEL     §LDDLA        §ODDLA            8
025600951023     C* STAMPA LINEA ARRIVO
025700951023     C* X 15 STAMPA A "MODOD ESTERO"
025800951023     C   15              DO
025900951023     C                   MOVEL     §ODDLA        §EEN              3
026000951023     C                   MOVEL     §LDCAD        §CA2              2
026100951023     C                   MOVE      §LDCAD        §CA7              7
026200951023     C                   END
026300951023     C                   EXCEPT    LV22D3
026400951023     C*
026500951023     C                   MOVEA     *BLANK        §RI
026600010717     C                   MOVEA     §LDRSD        §RI(6)
026700951023     C                   MOVEA     ' '           §RI(30)
026800951023     C                   MOVEA     §LDPRD        §RI(31)
026900951023     C                   MOVEA     *BLANK        §RI(33)
027000951023     C* STAMPA 1° DATI DESTINATARIO
027100951023     C                   EXCEPT    LV22D4
027200951023     C*
027300951023     C                   MOVEA     *BLANK        §RI
027400010717     C                   MOVEA     §LDIND        §RI(6)
027500951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
027600951023     C                   Z-ADD     37            §Y                2 0    88
027700951023     C                   DO        36
027800951023     C                   SUB       1             §Y
027900951023     C     §RI(§Y)       IFNE      ' '
028000951023     C                   SETON                                        88
028100951023     C                   END
028200951023     C  N88              END
028300951023     C     §Y            IFLE      25
028400951023     C                   ADD       3             §Y
028500951023     C                   ELSE
028600951023     C                   MOVEA     '  '          §RI(26)
028700951023     C                   Z-ADD     28            §Y
028800951023     C                   END
028900951023     C                   MOVEA     §LDLOD        §RI(§Y)
029000951023     C* STAMPA 2° DATI DESTINATARIO
029100951023     C                   EXCEPT    LV22D5
029200951023     C* STAMPA SERVIZIO
029300951023     C   16              EXCEPT    LV22X5
029400951023     C*
029500951023     C* STAMPA BARCODE
029600951023     C*
029700951023     C* (CAMPO UNICO BARCODE E COSTANTI)
029800951023     C                   MOVE      *BLANK        §§CDB1           19
029900951023     C                   MOVE      §CDBAR        §§CDB1
030000951023     C                   MOVEL     '"'           §§CDB1
030100951023     C                   MOVE      *BLANK        §§CDB2           21
030200951023     C                   MOVEL     §§CDB1        §§CDB2
030300951023     C                   MOVE      '";'          §§CDB2
030400951023     C                   EXCEPT    LV22D6
030500951023     C*
030600951023     C                   MOVE      *BLANK        §RI21            21
030700951023     C   18              DO
030800951023     C                   MOVEL     'Rif.:'       §RI21
030900980402     C     §DEEUR        IFNE      *BLANK
031000980402     C     §DETST        ANDEQ     'ITA'
031100980402     C                   MOVEL     'JUT :'       §RI21
031200980402     C                   ENDIF
031300951023     C                   MOVE      §LDCDP        §RI21
031400951023     C                   END
031500951023     C* STAMPA RIFERIMENTO/RISTAMPA
031600951023     C                   EXCEPT    LV22D7
031700951023     C*
031800951023     C* STAMPA CHIUSURA ETICHETTA
031900951023     C                   EXCEPT    LV22D8
032000951023     C                   ENDSR
032100971222     C/SPACE 005
032200971222     C*****************************************************
032300971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032400971222     C*****************************************************
032500971222     C     COLZER        BEGSR
032600971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032700971222     C                   SETOFF                                       77
032800971222     C                   MOVE      §LDNCD        §N7
032900971222     C                   DO        7             §1
033000971222     C     §S7(§1)       IFEQ      *ZERO
033100971222     C                   MOVE      *BLANK        §S7(§1)
033200971222     C                   ELSE
033300971222     C                   SETON                                        77
033400971222     C                   END
033500971222     C  N77              END
033600971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
033700971222     C                   MOVE      *ZERO         §X                1 0
033800971222     C                   MOVEA     *BLANK        §O7
033900971222     C                   DO        7             §1
034000010925     C*    §S7(§1)       IFNE      *BLANK
034100971222     C                   ADD       1             §X
034200971222     C     §S7(§1)       IFEQ      '0'
034300011114     C     §S7(§1)       oreq      *blank
034400971222     C                   MOVE      'O'           §O7(§X)
034500971222     C                   ELSE
034600971222     C                   MOVE      §S7(§1)       §O7(§X)
034700971222     C                   END
034800010925     C*                  END
034900971222     C                   END
035000971222     C                   MOVEA     §O7           §S7
035100971222     C                   ENDSR
035200980326     C/SPACE 005
035300980326     C*****************************************************
035400980326     C* STAMPA EUROEXPRESS                              ***
035500980326     C*****************************************************
035600980326     C     EUREXP        BEGSR
035700980326     C*
035800980326     C* IMPOSTO COSTANTI
035900980326     C                   MOVEL     'JUT'         §DEJEU            3
036000980326     C                   MOVEL     '55L'         §DE55L            3
036100980326     C                   MOVEL     'IT'          §DEISP            2
036200010925      * controllo se contenuto 55L nel codice passato
036300010925     c     '55L'         scan      §deeur                                 23
036400011120
036500011120      * se contenuto 55l scompongo a modo vecchio
036600011120     c                   if        *in23
036700980326     C* ESTRAGGO IL CAP:
036800980326     C                   MOVE      *ZEROS        §Y
036900980326     C                   MOVEA     §DEEUR        §EU
037000980326     C* TROVO QUANTO è LUNGO IL BARCODE
037100980326     C     ' '           CHECKR    §DEEUR        §Y
037200980326     C* ISOLO IL CAP
037300980326     C                   MOVE      *BLANK        §O9
037400980326     C                   MOVE      *ZEROS        §W                2 0
037500980326     C     §Y            SUB       2             §Y
037600980326     C     22            DO        §Y            §X1               2 0
037700980326     C     §EU(§X1)      IFNE      *BLANKS
037800980326     C                   ADD       1             §W
037900980326     C                   MOVE      §EU(§X1)      §O9(§W)
038000980326     C                   END
038100980326     C                   END
038200980326     C                   MOVEA     §O9           §LDCAD
038300011120
038400011120      *scompongo a modo nuovo senza 55l
038500011120     c                   else
038600011120     c                   movel     §deeur        §bceurex
038700011120     c                   end
038800980326     C*
038900980326     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
039000980326     C* DEL BARCODE
039100980326     C                   MOVEL     '";BSTP;'     §END              7
039200980326     C     §LDCAD        CAT       §DEISA:0      §BCD             11
039300980326     C     §BCD          CAT       §END:0        §BCEND           20
039400980326     C*
039500980326     C* VERIFICO SE CONTRASSEGNO
039600980326     C*
039700980326     C     §DEFCA        COMP      'S'                                    14
039800980326     C  N14§DEFCA        COMP      'X'                                    14
039900990323     C*
040000990323     C* VERIFICO SE INGHILTERRA
040100990323     C*
040200990323     C     §DEISA        COMP      'GB'                                   13
040300980326     C*
040400980326     C                   EXCEPT    LV22Y
040500980326     C*
040600980326     C                   ENDSR
040700910403     C/SPACE 005
040800910403     C*****************************************************
040900910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
041000910403     C*****************************************************
041100910403     C     M10           BEGSR
041200910403     C                   MOVE      *ZERO         §TT               3 0
041300910403     C                   MOVE      *ZERO         §TT2              3 0
041400910403     C                   MOVE      *ZERO         §1                1 0
041500910403     C                   Z-ADD     18            §S                2 0
041600910403     C                   DO        17            §N                2 0
041700910403     C                   SUB       1             §S
041800910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
041900910403     C                   MOVE      §C(§N)        §CB(§N)
042000910403     C     §N            DIV       2             §CTR              3 0
042100910403     C                   MVR                     §RST              3 010
042200910403     C     *IN(10)       IFEQ      '1'
042300910403     C     §C(§S)        MULT      2             §N2               2 0
042400910403     C                   MOVE      §N2           §A2               2
042500910403     C                   MOVEA     §A2           §2A
042600910403     C                   XFOOT     §2            §CTR
042700910403     C                   ELSE
042800910403     C                   Z-ADD     §C(§S)        §CTR
042900910403     C                   END
043000910403     C                   ADD       §CTR          §TT
043100910403     C                   END
043200910403     C*
043300910403     C     §TT           ADD       9             §TT2
043400910403     C                   MOVE      0             §TT2
043500910403     C     §TT2          SUB       §TT           §1
043600910403     C*
043700910403     C                   MOVE      §1            §CB(18)
043800910403     C                   ENDSR
043900000118      *--------------------------------------------------
044000000118     c     dpddove       begsr
044100000118      *--------------------------------------------------
044200000118     c                   eval      §dpd = §deeur
044300000124     c                   eval      §dexx1ds = §dexx1
044400000118     c                   eval      §deawbds = §deawb
044500000118     c                   eval      §dpdc1 = §dpdc1w
044600000118     c                   eval      §dpdc3 = §dpdc3w
044700000118     c                   eval      §dpdc5 = §dpdc5w
044800000118     c                   eval      §dpdc7 = §dpdc7w
044900000118     c                   eval      §dpdn = 'Notifications on damages which are -
045000000118     c                             not recognizable from the outside have to -
045100000118     c                             have to be submitted to the transport -
045200000118     c                             organisation withing 7 days in written.'
045300000118     C                   except    LV22dpd1
045400000118     C                   except    LV22dpd
045500000118     C                   endsr
045600000118      *--------------------------------------------------
045700000118     c     dpdchi        begsr
045800000118      *--------------------------------------------------
045900000118     c                   move      §ldmgs        giorno            2 0
046000000118     c                   movel     §ldmgs        mese              2 0
046100000118     c                   move      mese          giomes            4 0
046200000118     c                   movel     giorno        giomes
046300000118     c                   movel     giomes        datasp            8 0
046400000118     c                   move      §ldaas        datasp
046500000118     c                   movel     §dexx2        dpdteln
046600000118     c                   move      §dexx2        dpdfaxn
046700000118     C                   except    LV22dpd2
046800000118     C                   except    LV22dpd
046900000118     C                   endsr
047000000118     c*-------------------------------------------------------------------
047100971215     OFNLV22P   E            LV22             01
047200951023     O                                           20 '&&??%%P%%7E%^"LV22";'
047300010718     O          E            LV22        1
047400010719     O                                           16 '00001;0;265;-17;'
047500010718     O          E            LV22        1
047600910403     O*FISICA
047700980327     O** X TX340/240
047800010718     O                                           14 'UTOF;2700;SPB;'
047900941216     O          E            LV22        1
048000910403     O*GIRO SCRITTA A 180°
048100010719     O                                            9 'FLIP;300;'
048200941216     O          E            LV22        1
048300910403     O*                                       FONT
048400010717     O                                            7 'DHR;$1;'
048500941216     O          E            LV22        1
048600010717     O                                           14 'HLT;01;VLT;01;'
048700910403     O********* Mittente
048800941216     O          E            LV22D1      1
048900910403     O*                                       CARATTERE
049000910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
049100941216     O          E            LV22D1      1
049200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049300050415     O                                           25 'VBR;297;HBR;610;DHR;$201;'
049400941216     O          E            LV22D1      1
049500910403     O                                            1 '"'
049600910404     O                       §RI                 38
049700920910     O                                           40 '";'
049800010925      *---*   costante barra separazione
049900010925     O          E            LV22d1      1
050000050415     O                                           16 'HBR;640;VBR;015;'
050100010925     O          E            LV22d1      1
050200050415     O                                           18 'VLT;2;DVL;0;0;230;'
050300010925     O********* Destinatario (Rag.Soc.)
050400010925     O          E            LV22D4      1
050500910403     O*                                       CARATTERE
050600020110     O                                           21 'DCH;15;DCW;20;ICS;03;'
050700941216     O          E            LV22D4      1
050800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050900050415     O                                           25 'VBR;290;HBR;650;DHR;$201;'
051000941216     O          E            LV22D4      1
051100910403     O                                            1 '"'
051200910404     O                       §RI                 38
051300910404     O                                           40 '";'
051400910403     O********* Destinatario (Località)
051500941216     O          E            LV22D5      1
051600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
051700050415     O                                           25 'VBR;290;HBR;675;DHR;$201;'
051800941216     O          E            LV22D5      1
051900910403     O                                            1 '"'
052000910404     O                       §RI                 38
052100910404     O                                           40 '";'
052200010717     O********* MC. SPEDIZIONE
052300010717     O          E            LV22d5      1
052400010717     O*                                       CARATTERE
052500010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
052600010717     O          E            LV22D5      1
052700010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052800011121     O                                           25 'VBR;260;HBR;710;DHR;$201;'
052900010717     O          E            LV22D5      1
053000010717     O                                            1 '"'
053100010717     O                                            4 'MC.'
053200010717     O                       §LDVOL              11 '0 ,   '
053300010717     O                       §LDFVR              14
053400010717     O                                           16 '";'
053500941216     O********* SERVIZIO
053600941216     O          E            LV22X5      1
053700921104     O*                                       CARATTERE
053800010717     O                                           22 'DCH;30;DCW;050;ICS;03;'
053900941216     O          E            LV22X5      1
054000921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054100010719     O                                           23 'VBR;170;HBR;050;DHR;$1;'
054200941216     O          E            LV22X5      1
054300941216     O                                            1 '"'
054400941216     O                       §LDTSP               2
054500941216     O                                            4 '";'
054600941216     O          E            LV22X5      1
054700010719     O                                           21 'HOME;VBR;140;HBR;040;'
054800941216     O          E            LV22X5      1
054900941216     O* FONDO NERO                                     L   H
055000010717     O                     17                    17 'DCBX;0;0;060;040;'
055100941216     O* FONDO BIANCO                                   L   H
055200010717     O                    N17                    17 'DBOX;0;0;060;040;'
055300010717     O********* progr. colli e peso
055400941216     O          E            LV22H       1
055500910403     O*                                       CARATTERE
055600010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
055700941216     O          E            LV22H       1
055800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055900011121     O                                           25 'VBR;180;HBR;710;DHR;$201;'
056000941216     O          E            LV22H       1
056100910403     O                                            1 '"'
056200951021     O                       §RX                 12
056300010717     O                                           16 'KG.'
056400010717     O                       §LDPKG              26 '    0 , '
056500010717     O                                           28 '";'
056600941216     O          E            LV22H       1
056700010717     O********* Linea Partenza
056800910403     O*                                       CARATTERE
056900010717     O                                           23 'DCH;050;DCW;035;ICS;01;'
057000941216     O          E            LV22H       1
057100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057200010719     O                                           23 'VBR;220;HBR;040;DHR;$1;'
057300941216     O          E            LV22H       1
057400910403     O                                            1 '"'
057500910403     O                       §LDLNP               4
057600910403     O                                            6 '";'
057700910403     O********* Segnacollo
057800941216     O          E    19      LV22D2      1
057900910403     O*                                       CARATTERE
058000010717     O                                           23 'DCH;050;DCW;040;ICS;08;'
058100941216     O          E    19      LV22D2      1
058200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058300010717     O                                           23 'VBR;240;HBR;140;DHR;$1;'
058400941216     O          E    19      LV22D2      1
058500910403     O                                            1 '"'
058600920910     O                       §LDNRT               4
058700920910     O                                            6 '";'
058800010717     O          E            LV22D2      1
058900010717     O*                                       CARATTERE
059000010717     O                                           23 'DCH;050;DCW;040;ICS;12;'
059100010717     O          E            LV22D2      1
059200010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059300010717     O                                           23 'VBR;240;HBR;230;DHR;$1;'
059400010717     O          E            LV22D2      1
059500010717     O                                            1 '"'
059600010717     O                       §S4                  5
059700010717     O*                      §S7                  8
059800010717     O                                            7 '";'
059900941216     O          E            LV22D2      1
060000910403     O*                                       CARATTERE
060100021114     O                                           23 'DCH;090;DCW;075;ICS;12;'
060200941216     O          E            LV22D2      1
060300910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060400021114     O                                           23 'VBR;245;HBR;360;DHR;$1;'
060500941216     O          E            LV22D2      1
060600910403     O                                            1 '"'
060700010717     O                       §S3                  4
060800010717     O*                      §S7                  8
060900010717     O                                            6 '";'
061000921106     O********* DESCRIZIONE LINEA DI ARRIVO
061100010717     O          E            LV22D3      1
061200010717     O                                           14 'HLT;05;VLT;06;'
061300010717     O          E            LV22D3      1
061400921106     O*                                       CARATTERE
061500010717     O                                           21 'DCH;75;DCW;45;ICS;04;'
061600041125      *---*   costante barra sopra
061700041125     O          E            LV22d6      1
061800041125     O                                           22 'HLT;4;DHL;090;-60;400;'
061900041125      *---*   costante barra sotto
062000041125     O          E            LV22d6      1
062100041125     O                                           22 'HLT;2;DHL;090;040;402;'
062200910403     O********* Bar Code
062300941216     O          E            LV22D6      1
062400021114     O                                           16 'VBR;180;HBR;130;'
062500951121     O* BARCODE CODE 128
062600001218     O          E            LV22D6      1
062700010717     O                                           15 'BDEF;25;BCPI;2;'
062800001218     O          E            LV22D6      1
062900010717     O                                           18 'BCSH;100;BCST;"C";'
063000941216     O          E            LV22D6      1
063100921104     O                       §§CDB2              21
063200941216     O          E            LV22D6      1
063300910403     O                                            5 'BSTP;'
063400010717     O********* Codice Prodotto
063500941216     O          E            LV22D7      1
063600910403     O*                                       CARATTERE
063700050415     o                                           21 'DCH;15;DCW;20;ICS;03;'
063800941216     O          E            LV22D7      1
063900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
064000010717     O                                           23 'VBR;260;HBR;050;DHR;$1;'
064100941216     O          E            LV22D7      1
064200921104     O                                            1 '"'
064300951020     O                       §RI21               22
064400050415     O                       datasp              34 '  /  /    '
064500050415     O                       wLDRST              37
064600050415     O                                           39 '";'
064700050415      ********* 2° flag ristampa "V"
064800050415     o          E            LV22D7      1
064900050415     o                                           21 'DCH;25;DCW;40;ICS;03;'
065000050415     o          E            LV22D7      1
065100050415      *                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065200050415     o                                           23 'VBR;260;HBR;560;DHR;$1;'
065300050415     o          E            LV22D7      1
065400050415     o                                            1 '"'
065500050415     o                       wLDrst2              2
065600050415     o                                            4 '";'
065700010717     O********* TERMINAL/LINEA/ZONA ARRIVO
065800010717     O          E            LV22D3      1
065900010925     O                                           14 'HLT;01;VLT;02;'
066000010717     O          E            LV22D3      1
066100010717     O*                                       CARATTERE
066200010925     O                                           21 'DCH;75;DCW;75;ICS;08;'
066300010717     O********* TERMINAL
066400010717     O          E            LV22D3      1
066500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066600030626     O                                           23 'VBR;070;HBR;105;DHR;$1;'
066700010717     O          E            LV22D3      1
066800010717     O                                            1 '"'
066900010717     O                       §ODTNA               4
067000010717     O                                            6 '";'
067100010717     O* BOX X TERMINAL ARRIVO
067200010717     O          E            LV22D3      1
067300010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067400030626     O                                           21 'HOME;VBR;015;HBR;100;'
067500010717     O          E            LV22D3      1
067600010717     O* FONDO NERO                                     L   H
067700021114     O                                           16 'DBOX;0;0;140;60;'
067800010717     O********* CODICE LINEA ARRIVO
067900010717     O          E            LV22D3      1
068000021114     O                                           21 'DCH;45;DCW;40;ICS;08;'
068100010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
068200010925     O          E            LV22D3      1
068300040422     O                                           23 'VBR;075;HBR;260;DHR;$1;'
068400010717     O          E            LV22D3      1
068500010717     O                                            1 '"'
068600010717     O                       §ODLNA               4
068700010717     O                                            6 '";'
068800040422      *nazione x EEX
068900040422     O          E    55      LV22d3      1
069000040422     O                                              'HBR;360;VBR;075;'
069100040422     O                                              '"'
069200040422     O                       §DEISA
069300040422     O                                              '";'
069400040422      * cap estero per EEX
069500040422     O          E    55      LV22d3      1
069600040422     O                                              'HBR;450;VBR;075;'
069700040422     O                                              '"'
069800040422     O                       §LDCAD
069900040422     O                                              '";'
070000040422      * box per EEX
070100040422     O          E    55      LV22d3      1
070200040422     O                                              'HBR;000;VBR;012;'
070300040422     O                                              'DBOX;355;030;240;037;'
070400010719     O********* CODICE pista
070500040422     O          E            LV22D3      1
070600040422     O                                           21 'DCH;30;DCW;30;ICS;08;'
070700010717     O          E            LV22D3      1
070800010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
070900040422     O                                           23 'VBR;035;HBR;250;DHR;$1;'
071000010717     O          E            LV22D3      1
071100010717     O                                            1 '"'
071200010719     O                       pista                2
071300010717     O                                            4 '";'
071400010719     O********* CODICE pallet
071500010717     O          E            LV22D3      1
071600010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071700040422     O                                           23 'VBR;035;HBR;340;DHR;$1;'
071800010717     O          E            LV22D3      1
071900010717     O                                            1 '"'
072000010719     O                       pallet               3
072100010717     O                                            5 '";'
072200010719     O********* CODICE operatore
072300010717     O          E            LV22D3      1
072400010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
072500040422     O                                           25 'VBR;035;HBR;420;DHR;$1;'
072600010717     O          E            LV22D3      1
072700010717     O                                            1 '"'
072800010719     O                       operat               2
072900010717     O                                            4 '";'
073000010717     O********* ZONA ARRIVO
073100040422     O          E   n55      LV22D3      1
073200010925     O*                                       CARATTERE
073300010925     O                                           21 'DCH;75;DCW;75;ICS;08;'
073400040422     O          E   n55      LV22D3      1
073500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
073600021114     O                                           23 'HBR;500;VBR;070;DHR;$1;'
073700040422     O          E   n55      LV22D3      1
073800010717     O                                            1 '"'
073900010717     O                       §ODZSC               3
074000010717     O                                            5 '";'
074100010717     O* BOX X ZONA ARRIVO
074200040422     O          E   n55      LV22D3      1
074300010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
074400010925     O                                           21 'HOME;VBR;015;HBR;490;'
074500040422     O          E   n55      LV22D3      1
074600010925     O* FONDO bianco                                   L   H
074700021114     O                                           16 'DBOX;0;0;105;60;'
074800941216     O********* Chiusura etchetta
074900941216     O          E            LV22D8      1
075000951020     O********                               CON COVERTITORE
075100951023     O                                            9 'TRM;%%5C%'
075200980327     O*-------------------------------------------------------
075300980327     O* ETICHETTA ESTERO EUROEXPRESS
075400980327     O*-------------------------------------------------------
075500980327     O          E            LV22Y            01
075600980327     O******                                 inizializzazione NUOVA
075700980327     O                                              '&&??%%P%%7E%^"LV22";'
075800980327     O          E            LV22Y       1
075900980327     O*LOGICA                                N.ET. V (H) O=-35 X TB340
076000010719     O                                              '00001;0;265;000;'
076100980327     O          E            LV22Y       1
076200980327     O*FISICA
076300010719     O                                              'UTOF;2700;SPB;'
076400980327     O          E            LV22Y       1
076500980327     O*GIRO SCRITTA A 180°
076600010719     O                                              'FLIP;300;'
076700980327     O          E            LV22Y       1
076800980327     O*                                       FONT
076900010718     O                                              'DHR;$1;'
077000980327     O          E            LV22Y       1
077100980327     O                                              'HLT;01;VLT;01;'
077200980327      *---*   NAZIONE PARTENZA
077300980327     O          E            LV22Y       1
077400010718     O                                              'HBR;110;VBR;042;'
077500980327     O                                              'DCH;30;DCW;30;ICS;02;'
077600980327     O                                              '"'
077700980327     O                       §DEISP
077800980327     O                                              '";'
077900980327      *---*   CONTRASSEGNO
078000980327     O          E    14      LV22Y       1
078100980330     O                                              'HBR;000;VBR;012;'
078200010718     O                                              'DBOX;520;010;160;048;'
078300980327     O          E    14      LV22Y       1
078400010718     O                                              'HBR;530;VBR;062;'
078500980327     O                                              'DCH;050;DCW;090;ICS;02;'
078600980327     O                                              '"COD";'
078700980327      *---*   EUROEXPRESS
078800980327     O          E            LV22Y       1
078900010718     O                                              'HBR;180;VBR;050;'
079000010718     O                                              'DCH;030;DCW;050;ICS;02;'
079100980327     O                                              '"EuroExpress";'
079200980327      *---*   NAZIONE ARRIVO
079300980327     O          E            LV22Y       1
079400980330     O                                              'HBR;000;VBR;012;'
079500010718     O                                              'DBOX;080;060;150;050;'
079600980327     O          E            LV22Y       1
079700010718     O                                              'HBR;090;VBR;085;'
079800010718     O                                              'DCH;015;DCW;015;ICS;02;'
079900980327     O                                              '"Country";'
080000980327     O          E            LV22Y       1
080100010718     O                                              'HBR;090;VBR;115;'
080200010718     O                                              'DCH;040;DCW;050;ICS;02;'
080300980327     O                                              '"'
080400980327     O                       §DEISA
080500980327     O                                              '";'
080600980327      *---*   CAP DESTINO
080700980327     O          E            LV22Y       1
080800980330     O                                              'HBR;000;VBR;012;'
080900010718     O                                              'DBOX;290;060;280;050;'
081000980327     O          E            LV22Y       1
081100010718     O                                              'HBR;300;VBR;085;'
081200010718     O                                              'DCH;015;DCW;015;ICS;02;'
081300980327     O                                              '"Post Code";'
081400980327     O          E            LV22Y       1
081500010718     O                                              'HBR;300;VBR;115;'
081600010718     O                                              'DCH;040;DCW;050;ICS;02;'
081700980327     O                                              '"'
081800980327     O                       §LDCAD
081900980327     O                                              '";'
082000980327      *---*   BARCODE EUROEXPRESS
082100011120     O          E   n23      LV22Y       1
082200010718     O                                              'BDEF;25;BCSH;100;'
082300011120     O                                              'BCPI;2;'
082400010925     O          E   n23      LV22Y       1
082500010718     O                     13                       'HBR;037;VBR;230;'
082600010718     O                    N13                       'HBR;062;VBR;230;'
082700010731     O                                              'BCST;"A'
082800011120     O                       alfa5
082900011120     O                                              '%%40%D'
083000011120     O                       nume12
083100011120     O                                              '%%40%F'
083200011120     o                       alfa2
083300011120     O                       §end
083400011120      *---*   BARCODE EUROEXPRESS
083500011120     O          E    23      LV22Y       1
083600011120     O                                              'BDEF;25;BCSH;100;'
083700011120     O                                              'BCPI;1;'
083800010925      *con 55l
083900010925     O          E    23      LV22Y       1
084000010925     O                     13                       'HBR;037;VBR;230;'
084100010925     O                    N13                       'HBR;062;VBR;230;'
084200010925     O*                                             'BCST;"%%40%F'
084300010925     O                                              'BCST;"A'
084400010925     O                       §DEJEU
084500010925     O                       §DEISP
084600010925     O                                              '%%40%D'
084700010925     O                       §LDLNP
084800010925     O                       §LDNRS
084900010925     O                       §LDNCD
085000010925     O                                              '%%40%F'
085100010925     O                                              ' '
085200010925     O                       §DE55L
085300010925     O                       §BCEND
085400011120     O          E    23      LV22Y       1
085500980401      *---*   DECODIFICA
085600010719     O                                              'HBR;220;VBR;250;'
085700010719     O                                              'DCH;15;DCW;18;ICS;02;'
085800980401     O                                              '"'
085900980401     O                       §DEJEU
086000980401     O                       §DEISP
086100980401     O                       §LDLNP
086200980401     O                       §LDNRS
086300980401     O                       §LDNCD
086400011120     O                                              ' '
086500011120     O                       §DE55L
086600011120     O                       §BCD
086700980401     O                                              '";'
086800011120     O          E   n23      LV22Y       1
086900011120      *---*   DECODIFICA
087000011120     O                                              'HBR;220;VBR;250;'
087100011120     O                                              'DCH;15;DCW;18;ICS;02;'
087200011120     O                                              '"'
087300011120     O                       alfa5
087400011120     O                       nume12
087500011120     O                       alfa2
087600011120     O                                              '";'
087700980327      *---*   RIFERIMENTI BARTOLINI
087800980327     O          E            LV22Y       1
087900020111     O                                              'HBR;030;VBR;260;'
088000010719     O                                              'DCH;020;DCW;020;ICS;02;'
088100980327     O                                              '"'
088200980327     O                       §LDLNP
088300980327     O                                              ' '
088400980327     O                       §LDNRS
088500980327     O                                              ' '
088600980327     O                       §LDNCD
088700980327     O                                              '";'
088800980327     O********* Chiusura etchetta
088900980327     O          E            LV22Y       1
089000980327     O********                               CON COVERTITORE
089100980327     O                                              'TRM;%%5C%'
089200980327     O********                               CON VIDEO
089300980327     O**                                    'TRM;\'
089400000118     o*-----------------------------------------------------------------
089500000118     o*etichetta dpd "dove"
089600000118      *
089700000118     O          E            LV22dpd1         01
089800000118     O                                           20 '&&??%%P%%7E%^"LV22";'
089900000118     O          E            LV22dpd1    1
090000000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
090100010719     O                                           16 '00001;0;265;-17;'
090200010718     O          E            LV22dpd1    1
090300000118     O*FISICA
090400010718     O                                           14 'UTOF;2700;SPB;'
090500000118     O          E            LV22dpd1    1
090600000118     O*GIRO SCRITTA A 180°
090700010719     O                                            9 'FLIP;300;'
090800000118     O          E            LV22dpd1    1
090900010718     O                                            9 'DHR;$201;'
091000000118     O          E            LV22dpd1    1
091100000118     O                                           14 'HLT;01;VLT;01;'
091200000118      *---*   numero  spedizione
091300000118     O          E            LV22dpd1    1
091400000128     O                                           23 'DCH;020;DCW;020;ICS;03;'
091500000118     O          E            LV22dpd1    1
091600020114     O                                           16 'HBR;030;VBR;220;'
091700000118     O          E            LV22dpd1    1
091800000118     O                                            1 '"'
091900000118     O                       §dpd3                4
092000000118     O                                            6 '";'
092100000118      * 8 caratteri numero spedizione
092200000118     O          E            LV22dpd1    1
092300000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
092400000118     O          E            LV22dpd1    1
092500020114     O                                           16 'HBR;030;VBR;200;'
092600000118     O          E            LV22dpd1    1
092700000118     O                                            1 '"'
092800000118     O                       §dpd8                9
092900000118     O                                           11 '";'
093000000118      *ultimo carattere numero spedizione
093100000118     O          E            LV22dpd1    1
093200000128     O                                           23 'DCH;020;DCW;020;ICS;01;'
093300000118     O          E            LV22dpd1    1
093400020114     O                                           16 'HBR;030;VBR;145;'
093500000118     o          E            LV22dpd1    1
093600000118     O                                            1 '"'
093700000118     O                       §dpd12               2
093800000118     O                                            4 '";'
093900000118      *---*   costante scritta
094000000118     O          E            LV22dpd1    1
094100000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
094200000118     O          E            LV22dpd1    1
094300010718     O                                           16 'HBR;110;VBR;230;'
094400000118     O          E            LV22dpd1    1
094500000118     O                                            1 '"'
094600000118     O                       §dpdn1              52
094700000118     O                                           54 '";'
094800000118      *---*   costante scritta 2
094900000118     O          E            LV22dpd1    1
095000000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
095100000118     O          E            LV22dpd1    1
095200010718     O                                           16 'HBR;130;VBR;230;'
095300000118     O          E            LV22dpd1    1
095400000118     O                                            1 '"'
095500000118     O                       §dpdn2              50
095600000118     O                                           52 '";'
095700000118      *---*   costante scritta 3
095800000118     O          E            LV22dpd1    1
095900000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
096000000118     O          E            LV22dpd1    1
096100010718     O                                           16 'HBR;150;VBR;230;'
096200000118     O          E            LV22dpd1    1
096300000118     O                                            1 '"'
096400000118     O                       §dpdn3              54
096500000118     O                                           56 '";'
096600000118      *---*   costante barra sopra
096700000118     O          E            LV22dpd1    1
096800010718     O                                           16 'HBR;270;VBR;050;'
096900000118     O          E            LV22dpd1    1
097000010718     O                                           18 'VLT;2;DVL;0;0;155;'
097100000118      *---*   costante barra sotto
097200000118     O          E            LV22dpd1    1
097300010718     O                                           16 'HBR;470;VBR;050;'
097400000118     O          E            LV22dpd1    1
097500010718     O                                           18 'VLT;2;DVL;0;0;155;'
097600000118     o* secondo codice a barre
097700000118     O          e            lv22dpd1    1
097800020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
097900020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
098000020114     O          e            lv22dpd1    1
098100020114     O                                            9 'BCSH;200;'
098200000118     O          E            LV22dpd1    1
098300010718     O                                           16 'HBR;270;VBR;050;'
098400000118     o          E            LV22dpd1    1
098500000118     O                                            5 'BCST;'
098600000118     O                                            6 '"'
098700000118     O                       §deawb              16
098800000118     O                                           18 '";'
098900000118     O                                           23 'BSTP;'
099000000118      *costante 3 v1/00
099100000118     O          E            LV22dpd1    1
099200000118     O                                           23 'DCH;040;DCW;040;ICS;02;'
099300000118     O          E            LV22dpd1    1
099400010718     O                                           16 'HBR;190;VBR;180;'
099500000118     o          E            LV22dpd1    1
099600000118     O                                            1 '"'
099700000124     O                       §dex1                6
099800000124     O                                            8 '";'
099900000118      *decodifica 2° bcc
100000000118     O          E            LV22dpd1    1
100100000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
100200000118     O          E            LV22dpd1    1
100300010718     O                                           16 'HBR;480;VBR;200;'
100400000118     o          E            LV22dpd1    1
100500000118     O                                            1 '"'
100600000118     O                       §dpdcap             14
100700000118     O                                           16 '";'
100800000118      *costante x
100900000118     O          E            LV22dpd1    1
101000000118     O                                           23 'DCH;050;DCW;030;ICS;02;'
101100000118     O          E            LV22dpd1    1
101200010718     O                                           16 'HBR;470;VBR;260;'
101300000118     o          E            LV22dpd1    1
101400000118     O                                            1 '"'
101500000118     O                       §desse               2
101600000118     O                                            4 '";'
101700000118      *costante 09
101800000118     O          E            LV22dpd1    1
101900000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
102000000118     O          E            LV22dpd1    1
102100020111     O                                           16 'HBR;570;VBR;260;'
102200000118     o          E            LV22dpd1    1
102300000118     O                                            1 '"'
102400000118     O                       dpd9                 3
102500000118     O                                            5 '";'
102600000118      *linea di arrivo
102700000118     O          E            LV22dpd1    1
102800000118     O                                           23 'DCH;080;DCW;040;ICS;02;'
102900000118     O          E            LV22dpd1    1
103000010718     O                                           16 'HBR;550;VBR;200;'
103100000118     o          E            LV22dpd1    1
103200000118     O                                            1 '"'
103300000124     O                       §dex3                4
103400000118     O                                            6 '";'
103500000118      *zona arrivo
103600000118     O          E            LV22dpd1    1
103700000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
103800000118     O          E            LV22dpd1    1
103900010718     O                                           16 'HBR;570;VBR;130;'
104000000118     o          E            LV22dpd1    1
104100000118     O                                            1 '"'
104200000124     O                       §dex4                3
104300000118     O                                            5 '";'
104400000118     O*------------------------------------------------------------------
104500000118     O          E            LV22dpd2         01
104600000118     O                                           20 '&&??%%P%%7E%^"LV22";'
104700000118     O          E            LV22dpd2    1
104800000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
104900010719     O                                           16 '00001;0;265;-17;'
105000000118     O          E            LV22dpd2    1
105100000118     O*FISICA
105200010718     O                                           14 'UTOF;2700;SPB;'
105300000118     O          E            LV22dpd2    1
105400000118     O*GIRO SCRITTA A 180°
105500010719     O                                            9 'FLIP;300;'
105600000118     O          E            LV22dpd2    1
105700010718     O                                            9 'DHR;$201;'
105800000118     O          E            LV22dpd2    1
105900000118     O                                           14 'HLT;01;VLT;01;'
106000000118      *---*   numero  spedizione
106100000118     O          E            LV22dpd2    1
106200000118     O                                           23 'DCH;050;DCW;030;ICS;03;'
106300000118     O          E            LV22dpd2    1
106400010718     O                                           16 'HBR;110;VBR;240;'
106500000118     O          E            LV22dpd2    1
106600000118     O                                            1 '"'
106700000118     O                       §dpd3                4
106800000118     O                                            6 '";'
106900000118      * 8 caratteri numero spedizione
107000000118     O          E            LV22dpd2    1
107100010718     O                                           23 'DCH;040;DCW;030;ICS;02;'
107200000118     O          E            LV22dpd2    1
107300010718     O                                           16 'HBR;120;VBR;200;'
107400000118     O          E            LV22dpd2    1
107500000118     O                                            1 '"'
107600000118     O                       §dpd8                9
107700000118     O                                           11 '";'
107800000118      *ultimo carattere numero spedizione
107900000118     O          E            LV22dpd2    1
108000000118     O                                           23 'DCH;035;DCW;020;ICS;01;'
108100000118     O          E            LV22dpd2    1
108200010718     O                                           16 'HBR;130;VBR;120;'
108300000118     o          E            LV22dpd2    1
108400000118     O                                            1 '"'
108500000118     O                       §dpd12               2
108600000118     O                                            4 '";'
108700000118      *---*   costante TELEFONO
108800000118     O          E            LV22dpd2    1
108900000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
109000000118     O          E            LV22dpd2    1
109100020114     O                                           16 'HBR;020;VBR;230;'
109200000118     O          E            LV22dpd2    1
109300000118     O                                            1 '"'
109400000118     O                       dpd2                20
109500000118     O                                           22 '";'
109600000118      *---*   costante fax
109700000118     O          E            LV22dpd2    1
109800000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
109900000118     O          E            LV22dpd2    1
110000020114     O                                           16 'HBR;040;VBR;230;'
110100000118     O          E            LV22dpd2    1
110200000118     O                                            1 '"'
110300000118     O                       dpd2f               20
110400000118     O                                           22 '";'
110500000118      *costante mittente
110600000118     O          E            LV22dpd2    1
110700000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
110800000118     O          E            LV22dpd2    1
110900020111     O                                           25 'HBR;180;VBR;260;'
111000000118     o          E            LV22dpd2    1
111100000118     O                                            1 '"'
111200000118     O                       dpd3                 8
111300000118     O                                           10 '";'
111400000118      *costante mittente2
111500000118     O          E            LV22dpd2    1
111600000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
111700000118     O          E            LV22dpd2    1
111800020111     O                                           25 'HBR;210;VBR;260;'
111900000118     o          E            LV22dpd2    1
112000000118     O                                            1 '"'
112100000118     O                       §ldrsm              36
112200000118     O                                           38 '";'
112300000118      *costante mittente 3
112400000118     O          E            LV22dpd2    1
112500000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
112600000118     O          E            LV22dpd2    1
112700020111     O                                           25 'HBR;240;VBR;260;'
112800000118     o          E            LV22dpd2    1
112900000118     O                                            1 '"'
113000000118     O                       §ldprm               3
113100000118     O                                            5 '";'
113200000118      *costante destinatario
113300000118     O          E            LV22dpd2    1
113400000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
113500000118     O          E            LV22dpd2    1
113600020111     O                                           25 'HBR;275;VBR;260;'
113700000118     o          E            LV22dpd2    1
113800000118     O                                            1 '"'
113900000118     O                       dpd4                11
114000000118     O                                           13 '";'
114100000118      *destinatario
114200000118     O          E            LV22dpd2    1
114300000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
114400000118     O          E            LV22dpd2    1
114500020111     O                                           20 'HBR;300;VBR;260;'
114600000118     o          E            LV22dpd2    1
114700000118     O                                            1 '"'
114800000118     O                       §ldrsd              36
114900000118     O                                           38 '";'
115000000118      * destinatario indirizzo
115100000118     O          E            LV22dpd2    1
115200000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
115300000118     O          E            LV22dpd2    1
115400020111     O                                           20 'HBR;330;VBR;260;'
115500000118     o          E            LV22dpd2    1
115600000118     O                                            1 '"'
115700000118     O                       §ldind              36
115800000118     O                                           38 '";'
115900000118      *località destinatario 7
116000000118     O          E            LV22dpd2    1
116100000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
116200000118     O          E            LV22dpd2    1
116300020111     O                                           20 'HBR;360;VBR;260;'
116400000118     o          E            LV22dpd2    1
116500000118     O                                            1 '"'
116600000118     O                       §ldlod              36
116700000118     O                                           38 '";'
116800000118      *cap      destinatario 8
116900000118     O          E            LV22dpd2    1
117000000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
117100000118     O          E            LV22dpd2    1
117200020111     O                                           20 'HBR;400;VBR;260;'
117300000118     o          E            LV22dpd2    1
117400000118     O                                            1 '"'
117500000118     O                       §ldcad              10
117600000118     O                                           12 '";'
117700000118      *cap      destinatario 8
117800000118     O          E            LV22dpd2    1
117900000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
118000000118     O          E            LV22dpd2    1
118100010718     O                                           20 'HBR;400;VBR;180;'
118200000118     o          E            LV22dpd2    1
118300000118     O                                            1 '"'
118400000118     O                       §desia               4
118500000118     O                                            6 '";'
118600000118      *---*   costante barra sopra
118700000118     O          E            LV22dpd2    1
118800010718     O                                           16 'HBR;450;VBR;050;'
118900000118     O          E            LV22dpd2    1
119000010718     O                                           18 'VLT;2;DVL;0;0;185;'
119100000118      *---*   costante barra sotto
119200000118     O          E            LV22dpd2    1
119300010718     O                                           16 'HBR;650;VBR;050;'
119400000118     O          E            LV22dpd2    1
119500010718     O                                           18 'VLT;2;DVL;0;0;185;'
119600000118      *costante primo B-C
119700000118     O          e            lv22dpd2    1
119800020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
119900020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
120000020114     O          e            lv22dpd2    1
120100020114     O                                            9 'BCSH;200;'
120200000118     O          E            LV22dpd2    1
120300010718     O                                           16 'HBR;450;VBR;050;'
120400000118     o          E            LV22dpd2    1
120500000118     O                                            5 'BCST;'
120600000118     O                                            6 '"'
120700000118     O                       §dpd                18
120800000118     O                                           20 '";'
120900000118     O                                           25 'BSTP;'
121000000118      *costante data
121100000118     O          E            LV22dpd2    1
121200000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
121300000118     O          E            LV22dpd2    1
121400020111     O                                           16 'HBR;670;VBR;260;'
121500000118     o          E            LV22dpd2    1
121600000118     O                                            1 '"'
121700000118     O                       datasp              11 '  /  /    '
121800000118     O                                           13 '";'
121900000118      *costante 1 collo
122000000118     O          E            LV22dpd2    1
122100000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
122200000118     O          E            LV22dpd2    1
122300010718     O                                           16 'HBR;670;VBR;200;'
122400000118     o          E            LV22dpd2    1
122500000118     O                                            1 '"'
122600000118     O                                            8 '1 Collo'
122700000118     O                                           10 '";'
122800000118      *costante peso
122900000118     O          E            LV22dpd2    1
123000000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
123100000118     O          E            LV22dpd2    1
123200010718     O                                           16 'HBR;670;VBR;150;'
123300000118     o          E            LV22dpd2    1
123400000118     O                                            1 '"'
123500000118     O                                            8 'Peso Kg'
123600000118     O                                           10 '";'
123700000118      * peso
123800000118     O          E            LV22dpd2    1
123900000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
124000000118     O          E            LV22dpd2    1
124100010718     O                                           16 'HBR;670;VBR;100;'
124200000118     o          E            LV22dpd2    1
124300000118     O                                            1 '"'
124400000118     O                       §ldpkg        2     10
124500000118     O                                           12 '";'
124600000118      *------------------------------------------------------------------
124700000118      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
124800000118     O* costante dpd
124900000118     O          E            LV22dpd     1
125000000118     O                                           23 'DCH;030;DCW;070;ICS;03;'
125100000118     O          E            LV22dpd     1
125200020114     O                                           25 'HBR;160;VBR;280;DHR;$401;'
125300000118     o          E            LV22dpd     1
125400000118     O                                            1 '"'
125500000118     O                       dpd                  4
125600000118     O                                            6 '";'
125700000118     O* costante dpd riga sotto
125800000118     O          E            LV22dpd     1
125900000118     O                                           23 'DCH;006;DCW;005;ICS;01;'
126000000118     O          E            LV22dpd     1
126100010718     O                                           20 'HBR;160;VBR;240;'
126200000118     o          E            LV22dpd     1
126300000118     O                                            1 '"'
126400000118     O                       dpd1                23
126500000118     O                                           25 '";'
126600000118     o          E            LV22dpd     1
126700000118     O                                            9 'TRM;%%5C%'
