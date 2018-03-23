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
008500050606      *
008600050606      * Elimino carattere "\"
008700050606     c     '\':' '       xlate     §ldrsm        §ldrsm
008800050606     c     '\':' '       xlate     §ldrsd        §ldrsd
008900050606     c     '\':' '       xlate     §ldlod        §ldlod
009000050606     c     '\':' '       xlate     §ldind        §ldind
009100050606     c     '\':' '       xlate     §ldcdp        §ldcdp
009200010718      *data
009300010718     c                   move      §ldmgs        giorno            2 0
009400010718     c                   movel     §ldmgs        mese              2 0
009500010718     c                   move      mese          giomes            4 0
009600010718     c                   movel     giorno        giomes
009700010718     c                   movel     giomes        datasp            8 0
009800010718     c                   move      §ldaas        datasp
009900050415      *
010000050415      * flag ristampa: " ", "R", "M", "V", "X", "W"
010100050415     c                   clear                   wLDrst            1
010200050415     c                   clear                   wLDrst2           1
010300050415     c                   select
010400050415      *
010500050415     c                   when      §LDrst  = *blanks
010600050415      *
010700050415     c                   when      §LDrst  = 'V'
010800050415     c                   eval      wLDrst2 = §LDrst
010900050415      *
011000050415     c                   when      §LDrst  = 'X'
011100050415     c                   eval      wLDrst  = 'R'
011200050415     c                   eval      wLDrst2 = 'V'
011300050415      *
011400050415     c                   when      §LDrst  = 'W'
011500050415     c                   eval      wLDrst  = 'M'
011600050415     c                   eval      wLDrst2 = 'V'
011700050415      *
011800050415     c                   other
011900050415     c                   eval      wLDrst  = §LDrst
012000050415      *
012100050415     c                   endsl
012200050415      *
012300941216     C*
012400951021     C*** PROVE CON VALORI FISSI + ETICHETTE
012500951023     C***                  DO   10
012600951023     C***                  TIME           §HMS    60
012700951023     C***                  MOVEL§HMS      §LDCDP
012800951023     C***                  ADD  1         §LDNCD
012900941216     C*
013000971222     C                   MOVEL     §LDNRS        §LDNRT            3
013100971222     C                   MOVE      '-'           §LDNRT
013200971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
013300971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
013400971222     C     §DETST        IFEQ      'EST'
013500971222     C                   EXSR      COLZER
013600971222     C                   ENDIF
013700971222     C*
013800910403     C*
013900951023     C***                  ENDDO
014000971215     C*
014100980402     C*
014200980326     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
014300971215     C*
014400971215     C     §DETST        IFEQ      'EST'
014500971215     C     §DETST        OREQ      'ALL'
014600000118     c                   select
014700000118     c     §detse        wheneq    'REX'
014800010718     c*                  except    lv22e
014900000118     c     §detse        wheneq    'EUR'
015000000118     c                   exsr      eurexp
015100000118     c     §detse        wheneq    'DP1'
015200000118     c                   exsr      dpddove
015300000118     c     §detse        wheneq    'DP2'
015400000118     c                   exsr      dpddove
015500000118     c                   exsr      dpdchi
015600000118     c                   endsl
015700971215     C                   ENDIF
015800000202     C     §DETST        IFEQ      'ITA'
015900000202     C     §DETST        OREQ      'ALL'
016000000202     C                   EXSR      PRINT
016100000202     C                   ENDIF
016200971215     C*
016300910403     C                   SETON                                        LR
016400951021     C/SPACE 005
016500951021     C*****************************************************
016600951023     C* IMPOSTAZIONE E STAMPA                           ***
016700951021     C*****************************************************
016800951023     C     PRINT         BEGSR
016900951023     C                   MOVE      §LDLNP        §LCLNP
017000951023     C                   MOVE      §LDLNA        §LCLNA
017100951023     C                   MOVE      §LDNRS        §LCNRS
017200951023     C                   MOVE      §LDNCD        §LCNCD
017300951023     C                   MOVE      §LDZSC        §LCZSC
017400040422     C** non cancellare  MOVEl     §LDdla        dspioppa
017500040422     c     §detse        comp      'EEX'                                  55
017600951023     C*
017700951023     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
017800951023     C                   EXSR      M10
017900951023     C                   MOVEA     §CA           §C18A            18
018000951023     C                   MOVE      §C18A         §CDBAR           18
018100970210     C*
018200970210     C     §LDTSP        IFEQ      'D'
018300970303     C     §LDLNA        ANDLT     300
018400970210     C                   MOVE      *BLANK        §LDTSP
018500970210     C                   END
018600970210     C*
018700951023     C     §LDLNA        COMP      300                                15
018800951023     C     §LDTSP        COMP      *BLANK                             1616
018900951023     C     §LDTSP        COMP      'E'                                    17
019000090806     C  N17§LDTSP        COMP      'H'                                    17
019100951023     C     §LDCDP        COMP      *BLANK                             18
019200951023     C     §LDNRS        COMP      *ZERO                              19
019300951023     C*
019400951023     C** INIZIALIZZA STAMPANTE
019500951023     C                   EXCEPT    LV22
019600951023     C*
019700951023     C                   MOVEA     *BLANK        §RX
019800951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
019900951023     C                   SETOFF                                       77
020000951023     C                   MOVE      *BLANK        §A7               7
020100951023     C                   Z-ADD     §LDNC1        §N7
020200951023     C                   DO        7             §1
020300951023     C     §S7(§1)       IFEQ      *ZERO
020400951023     C                   MOVE      *BLANK        §S7(§1)
020500951023     C                   ELSE
020600951023     C                   SETON                                        77
020700951023     C                   END
020800951023     C  N77              END
020900951023     C                   MOVEA     §S7           §A7
021000951023     C                   MOVE      §A7           §A3               3
021100951023     C                   MOVEA     §A3           §RX(1)
021200951023     C                   MOVEA     '-'           §RX(4)
021300951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
021400951023     C                   SETOFF                                       77
021500951023     C                   MOVE      *BLANK        §A7
021600951023     C                   Z-ADD     §LDNC2        §N7
021700951023     C                   DO        7             §1
021800951023     C     §S7(§1)       IFEQ      *ZERO
021900951023     C                   MOVE      *BLANK        §S7(§1)
022000951023     C                   ELSE
022100951023     C                   SETON                                        77
022200951023     C                   END
022300951023     C  N77              END
022400951023     C                   MOVEA     §S7           §A7
022500951023     C                   MOVE      §A7           §A3
022600951023     C                   MOVEA     §A3           §RX(5)
022700951023     C                   MOVE      §LDCPD        §A3
022800951023     C                   MOVEA     §A3           §RX(9)
022900951023     C* STAMPA LINEA PARTENZA
023000951023     C                   EXCEPT    LV22H
023100951023     C*
023200951023     C                   MOVEA     *BLANK        §RI
023300010717     C                   MOVEA     §LDRSM        §RI(6)
023400951023     C                   MOVEA     ' '           §RI(30)
023500951023     C                   MOVEA     §LDPRM        §RI(31)
023600951023     C                   MOVEA     *BLANK        §RI(33)
023700951023     C                   EXCEPT    LV22D1
023800951023     C* STAMPA DATI MITTENTE
023900951023     C*
024000951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024100971222     C                   EXSR      COLZER
024200951023     C*
024300971222     C*
024400951023     C* STAMPA NUMERO SEGNACOLLO
024500951023     C                   EXCEPT    LV22D2
024600951023     C*
024700951023     C                   MOVE      §LDTNA        §A3               3
024800951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
024900951023     C                   MOVE      §LDLNA        §A3
025000951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
025100951023     C                   MOVE      §LDZSC        §A2
025200951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
025300951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
025400951023     C                   MOVEL     §LDDLA        §ODDLA            8
025500951023     C* STAMPA LINEA ARRIVO
025600951023     C* X 15 STAMPA A "MODOD ESTERO"
025700951023     C   15              DO
025800951023     C                   MOVEL     §ODDLA        §EEN              3
025900951023     C                   MOVEL     §LDCAD        §CA2              2
026000951023     C                   MOVE      §LDCAD        §CA7              7
026100951023     C                   END
026200951023     C                   EXCEPT    LV22D3
026300951023     C*
026400951023     C                   MOVEA     *BLANK        §RI
026500010717     C                   MOVEA     §LDRSD        §RI(6)
026600951023     C                   MOVEA     ' '           §RI(30)
026700951023     C                   MOVEA     §LDPRD        §RI(31)
026800951023     C                   MOVEA     *BLANK        §RI(33)
026900951023     C* STAMPA 1° DATI DESTINATARIO
027000951023     C                   EXCEPT    LV22D4
027100951023     C*
027200951023     C                   MOVEA     *BLANK        §RI
027300010717     C                   MOVEA     §LDIND        §RI(6)
027400951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
027500951023     C                   Z-ADD     37            §Y                2 0    88
027600951023     C                   DO        36
027700951023     C                   SUB       1             §Y
027800951023     C     §RI(§Y)       IFNE      ' '
027900951023     C                   SETON                                        88
028000951023     C                   END
028100951023     C  N88              END
028200951023     C     §Y            IFLE      25
028300951023     C                   ADD       3             §Y
028400951023     C                   ELSE
028500951023     C                   MOVEA     '  '          §RI(26)
028600951023     C                   Z-ADD     28            §Y
028700951023     C                   END
028800951023     C                   MOVEA     §LDLOD        §RI(§Y)
028900951023     C* STAMPA 2° DATI DESTINATARIO
029000951023     C                   EXCEPT    LV22D5
029100951023     C* STAMPA SERVIZIO
029200951023     C   16              EXCEPT    LV22X5
029300951023     C*
029400951023     C* STAMPA BARCODE
029500951023     C*
029600951023     C* (CAMPO UNICO BARCODE E COSTANTI)
029700951023     C                   MOVE      *BLANK        §§CDB1           19
029800951023     C                   MOVE      §CDBAR        §§CDB1
029900951023     C                   MOVEL     '"'           §§CDB1
030000951023     C                   MOVE      *BLANK        §§CDB2           21
030100951023     C                   MOVEL     §§CDB1        §§CDB2
030200951023     C                   MOVE      '";'          §§CDB2
030300951023     C                   EXCEPT    LV22D6
030400951023     C*
030500951023     C                   MOVE      *BLANK        §RI21            21
030600951023     C   18              DO
030700951023     C                   MOVEL     'Rif.:'       §RI21
030800980402     C     §DEEUR        IFNE      *BLANK
030900980402     C     §DETST        ANDEQ     'ITA'
031000980402     C                   MOVEL     'JUT :'       §RI21
031100980402     C                   ENDIF
031200951023     C                   MOVE      §LDCDP        §RI21
031300951023     C                   END
031400951023     C* STAMPA RIFERIMENTO/RISTAMPA
031500951023     C                   EXCEPT    LV22D7
031600951023     C*
031700951023     C* STAMPA CHIUSURA ETICHETTA
031800951023     C                   EXCEPT    LV22D8
031900951023     C                   ENDSR
032000971222     C/SPACE 005
032100971222     C*****************************************************
032200971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032300971222     C*****************************************************
032400971222     C     COLZER        BEGSR
032500971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032600971222     C                   SETOFF                                       77
032700971222     C                   MOVE      §LDNCD        §N7
032800971222     C                   DO        7             §1
032900971222     C     §S7(§1)       IFEQ      *ZERO
033000971222     C                   MOVE      *BLANK        §S7(§1)
033100971222     C                   ELSE
033200971222     C                   SETON                                        77
033300971222     C                   END
033400971222     C  N77              END
033500971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
033600971222     C                   MOVE      *ZERO         §X                1 0
033700971222     C                   MOVEA     *BLANK        §O7
033800971222     C                   DO        7             §1
033900010925     C*    §S7(§1)       IFNE      *BLANK
034000971222     C                   ADD       1             §X
034100971222     C     §S7(§1)       IFEQ      '0'
034200011114     C     §S7(§1)       oreq      *blank
034300971222     C                   MOVE      'O'           §O7(§X)
034400971222     C                   ELSE
034500971222     C                   MOVE      §S7(§1)       §O7(§X)
034600971222     C                   END
034700010925     C*                  END
034800971222     C                   END
034900971222     C                   MOVEA     §O7           §S7
035000971222     C                   ENDSR
035100980326     C/SPACE 005
035200980326     C*****************************************************
035300980326     C* STAMPA EUROEXPRESS                              ***
035400980326     C*****************************************************
035500980326     C     EUREXP        BEGSR
035600980326     C*
035700980326     C* IMPOSTO COSTANTI
035800980326     C                   MOVEL     'JUT'         §DEJEU            3
035900980326     C                   MOVEL     '55L'         §DE55L            3
036000980326     C                   MOVEL     'IT'          §DEISP            2
036100010925      * controllo se contenuto 55L nel codice passato
036200010925     c     '55L'         scan      §deeur                                 23
036300011120
036400011120      * se contenuto 55l scompongo a modo vecchio
036500011120     c                   if        *in23
036600980326     C* ESTRAGGO IL CAP:
036700980326     C                   MOVE      *ZEROS        §Y
036800980326     C                   MOVEA     §DEEUR        §EU
036900980326     C* TROVO QUANTO è LUNGO IL BARCODE
037000980326     C     ' '           CHECKR    §DEEUR        §Y
037100980326     C* ISOLO IL CAP
037200980326     C                   MOVE      *BLANK        §O9
037300980326     C                   MOVE      *ZEROS        §W                2 0
037400980326     C     §Y            SUB       2             §Y
037500980326     C     22            DO        §Y            §X1               2 0
037600980326     C     §EU(§X1)      IFNE      *BLANKS
037700980326     C                   ADD       1             §W
037800980326     C                   MOVE      §EU(§X1)      §O9(§W)
037900980326     C                   END
038000980326     C                   END
038100980326     C                   MOVEA     §O9           §LDCAD
038200011120
038300011120      *scompongo a modo nuovo senza 55l
038400011120     c                   else
038500011120     c                   movel     §deeur        §bceurex
038600011120     c                   end
038700980326     C*
038800980326     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
038900980326     C* DEL BARCODE
039000980326     C                   MOVEL     '";BSTP;'     §END              7
039100980326     C     §LDCAD        CAT       §DEISA:0      §BCD             11
039200980326     C     §BCD          CAT       §END:0        §BCEND           20
039300980326     C*
039400980326     C* VERIFICO SE CONTRASSEGNO
039500980326     C*
039600980326     C     §DEFCA        COMP      'S'                                    14
039700980326     C  N14§DEFCA        COMP      'X'                                    14
039800990323     C*
039900990323     C* VERIFICO SE INGHILTERRA
040000990323     C*
040100990323     C     §DEISA        COMP      'GB'                                   13
040200980326     C*
040300980326     C                   EXCEPT    LV22Y
040400980326     C*
040500980326     C                   ENDSR
040600910403     C/SPACE 005
040700910403     C*****************************************************
040800910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
040900910403     C*****************************************************
041000910403     C     M10           BEGSR
041100910403     C                   MOVE      *ZERO         §TT               3 0
041200910403     C                   MOVE      *ZERO         §TT2              3 0
041300910403     C                   MOVE      *ZERO         §1                1 0
041400910403     C                   Z-ADD     18            §S                2 0
041500910403     C                   DO        17            §N                2 0
041600910403     C                   SUB       1             §S
041700910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
041800910403     C                   MOVE      §C(§N)        §CB(§N)
041900910403     C     §N            DIV       2             §CTR              3 0
042000910403     C                   MVR                     §RST              3 010
042100910403     C     *IN(10)       IFEQ      '1'
042200910403     C     §C(§S)        MULT      2             §N2               2 0
042300910403     C                   MOVE      §N2           §A2               2
042400910403     C                   MOVEA     §A2           §2A
042500910403     C                   XFOOT     §2            §CTR
042600910403     C                   ELSE
042700910403     C                   Z-ADD     §C(§S)        §CTR
042800910403     C                   END
042900910403     C                   ADD       §CTR          §TT
043000910403     C                   END
043100910403     C*
043200910403     C     §TT           ADD       9             §TT2
043300910403     C                   MOVE      0             §TT2
043400910403     C     §TT2          SUB       §TT           §1
043500910403     C*
043600910403     C                   MOVE      §1            §CB(18)
043700910403     C                   ENDSR
043800000118      *--------------------------------------------------
043900000118     c     dpddove       begsr
044000000118      *--------------------------------------------------
044100000118     c                   eval      §dpd = §deeur
044200000124     c                   eval      §dexx1ds = §dexx1
044300000118     c                   eval      §deawbds = §deawb
044400000118     c                   eval      §dpdc1 = §dpdc1w
044500000118     c                   eval      §dpdc3 = §dpdc3w
044600000118     c                   eval      §dpdc5 = §dpdc5w
044700000118     c                   eval      §dpdc7 = §dpdc7w
044800000118     c                   eval      §dpdn = 'Notifications on damages which are -
044900000118     c                             not recognizable from the outside have to -
045000000118     c                             have to be submitted to the transport -
045100000118     c                             organisation withing 7 days in written.'
045200000118     C                   except    LV22dpd1
045300000118     C                   except    LV22dpd
045400000118     C                   endsr
045500000118      *--------------------------------------------------
045600000118     c     dpdchi        begsr
045700000118      *--------------------------------------------------
045800000118     c                   move      §ldmgs        giorno            2 0
045900000118     c                   movel     §ldmgs        mese              2 0
046000000118     c                   move      mese          giomes            4 0
046100000118     c                   movel     giorno        giomes
046200000118     c                   movel     giomes        datasp            8 0
046300000118     c                   move      §ldaas        datasp
046400000118     c                   movel     §dexx2        dpdteln
046500000118     c                   move      §dexx2        dpdfaxn
046600000118     C                   except    LV22dpd2
046700000118     C                   except    LV22dpd
046800000118     C                   endsr
046900000118     c*-------------------------------------------------------------------
047000971215     OFNLV22P   E            LV22             01
047100020131     O                                           21 '&&??%%P%%7E%^"TX340";'
047200010718     O          E            LV22        1
047300020131     O                                           16 '00001;0;265;-70;'
047400010718     O          E            LV22        1
047500910403     O*FISICA
047600980327     O** X TX340/240
047700010718     O                                           14 'UTOF;2700;SPB;'
047800941216     O          E            LV22        1
047900910403     O*GIRO SCRITTA A 180°
048000010719     O                                            9 'FLIP;300;'
048100941216     O          E            LV22        1
048200910403     O*                                       FONT
048300010717     O                                            7 'DHR;$1;'
048400941216     O          E            LV22        1
048500010717     O                                           14 'HLT;01;VLT;01;'
048600910403     O********* Mittente
048700941216     O          E            LV22D1      1
048800910403     O*                                       CARATTERE
048900910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
049000941216     O          E            LV22D1      1
049100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049200050415     O                                           25 'VBR;297;HBR;610;DHR;$201;'
049300941216     O          E            LV22D1      1
049400910403     O                                            1 '"'
049500910404     O                       §RI                 38
049600920910     O                                           40 '";'
049700010925      *---*   costante barra separazione
049800010925     O          E            LV22d1      1
049900050415     O                                           16 'HBR;640;VBR;015;'
050000010925     O          E            LV22d1      1
050100010925     O                                           18 'VLT;2;DVL;0;0;230;'
050200010925     O********* Destinatario (Rag.Soc.)
050300010925     O          E            LV22D4      1
050400910403     O*                                       CARATTERE
050500020110     O                                           21 'DCH;15;DCW;20;ICS;03;'
050600941216     O          E            LV22D4      1
050700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050800050415     O                                           25 'VBR;290;HBR;650;DHR;$201;'
050900941216     O          E            LV22D4      1
051000910403     O                                            1 '"'
051100910404     O                       §RI                 38
051200910404     O                                           40 '";'
051300910403     O********* Destinatario (Località)
051400941216     O          E            LV22D5      1
051500910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
051600050415     O                                           25 'VBR;290;HBR;675;DHR;$201;'
051700941216     O          E            LV22D5      1
051800910403     O                                            1 '"'
051900910404     O                       §RI                 38
052000910404     O                                           40 '";'
052100010717     O********* MC. SPEDIZIONE
052200010717     O          E            LV22d5      1
052300010717     O*                                       CARATTERE
052400010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
052500010717     O          E            LV22D5      1
052600010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052700011121     O                                           25 'VBR;260;HBR;710;DHR;$201;'
052800010717     O          E            LV22D5      1
052900010717     O                                            1 '"'
053000010717     O                                            4 'MC.'
053100010717     O                       §LDVOL              11 '0 ,   '
053200010717     O                       §LDFVR              14
053300010717     O                                           16 '";'
053400941216     O********* SERVIZIO
053500941216     O          E            LV22X5      1
053600921104     O*                                       CARATTERE
053700010717     O                                           22 'DCH;30;DCW;050;ICS;03;'
053800941216     O          E            LV22X5      1
053900921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
054000010719     O                                           23 'VBR;170;HBR;050;DHR;$1;'
054100941216     O          E            LV22X5      1
054200941216     O                                            1 '"'
054300941216     O                       §LDTSP               2
054400941216     O                                            4 '";'
054500941216     O          E            LV22X5      1
054600010719     O                                           21 'HOME;VBR;140;HBR;040;'
054700941216     O          E            LV22X5      1
054800941216     O* FONDO NERO                                     L   H
054900010717     O                     17                    17 'DCBX;0;0;060;040;'
055000941216     O* FONDO BIANCO                                   L   H
055100010717     O                    N17                    17 'DBOX;0;0;060;040;'
055200010717     O********* progr. colli e peso
055300941216     O          E            LV22H       1
055400910403     O*                                       CARATTERE
055500010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
055600941216     O          E            LV22H       1
055700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055800011121     O                                           25 'VBR;180;HBR;710;DHR;$201;'
055900941216     O          E            LV22H       1
056000910403     O                                            1 '"'
056100951021     O                       §RX                 12
056200010717     O                                           16 'KG.'
056300010717     O                       §LDPKG              26 '    0 , '
056400010717     O                                           28 '";'
056500941216     O          E            LV22H       1
056600010717     O********* Linea Partenza
056700910403     O*                                       CARATTERE
056800010717     O                                           23 'DCH;050;DCW;035;ICS;01;'
056900941216     O          E            LV22H       1
057000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057100010719     O                                           23 'VBR;220;HBR;040;DHR;$1;'
057200941216     O          E            LV22H       1
057300910403     O                                            1 '"'
057400910403     O                       §LDLNP               4
057500910403     O                                            6 '";'
057600910403     O********* Segnacollo
057700941216     O          E    19      LV22D2      1
057800910403     O*                                       CARATTERE
057900010717     O                                           23 'DCH;050;DCW;040;ICS;08;'
058000941216     O          E    19      LV22D2      1
058100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058200010717     O                                           23 'VBR;240;HBR;140;DHR;$1;'
058300941216     O          E    19      LV22D2      1
058400910403     O                                            1 '"'
058500920910     O                       §LDNRT               4
058600920910     O                                            6 '";'
058700010717     O          E            LV22D2      1
058800010717     O*                                       CARATTERE
058900010717     O                                           23 'DCH;050;DCW;040;ICS;12;'
059000010717     O          E            LV22D2      1
059100010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059200010717     O                                           23 'VBR;240;HBR;230;DHR;$1;'
059300010717     O          E            LV22D2      1
059400010717     O                                            1 '"'
059500010717     O                       §S4                  5
059600010717     O*                      §S7                  8
059700010717     O                                            7 '";'
059800941216     O          E            LV22D2      1
059900910403     O*                                       CARATTERE
060000021115     O                                           23 'DCH;085;DCW;080;ICS;12;'
060100941216     O          E            LV22D2      1
060200910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060300021115     O                                           23 'VBR;245;HBR;360;DHR;$1;'
060400941216     O          E            LV22D2      1
060500910403     O                                            1 '"'
060600010717     O                       §S3                  4
060700010717     O*                      §S7                  8
060800010717     O                                            6 '";'
060900921106     O********* DESCRIZIONE LINEA DI ARRIVO
061000010717     O          E            LV22D3      1
061100010717     O                                           14 'HLT;05;VLT;06;'
061200010717     O          E            LV22D3      1
061300921106     O*                                       CARATTERE
061400010717     O                                           21 'DCH;75;DCW;45;ICS;04;'
061500041122      *---*   costante barra sopra
061600041122     O          E            LV22d6      1
061700041122     O                                           22 'HLT;6;DHL;090;-60;400;'
061800041122      *---*   costante barra sotto
061900041122     O          E            LV22d6      1
062000041122     O                                           22 'HLT;4;DHL;090;040;402;'
062100910403     O********* Bar Code
062200041119     O          E            LV22D6      1
062300041119     O                                           16 'VBR;180;HBR;130;'
062400951121     O* BARCODE CODE 128
062500041119     O          E            LV22D6      1
062600041119     O                                           15 'BDEF;25;BCPI;2;'
062700041119     O          E            LV22D6      1
062800041119     O                                           18 'BCSH;100;BCST;"C";'
062900041119     O          E            LV22D6      1
063000921104     O                       §§CDB2              21
063100041119     O          E            LV22D6      1
063200041119     O                                            5 'BSTP;'
063300010717     O********* Codice Prodotto
063400941216     O          E            LV22D7      1
063500910403     O*                                       CARATTERE
063600910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
063700941216     O          E            LV22D7      1
063800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
063900010717     O                                           23 'VBR;260;HBR;050;DHR;$1;'
064000941216     O          E            LV22D7      1
064100921104     O                                            1 '"'
064200951020     O                       §RI21               22
064300050415     o                       datasp              34 '  /  /    '
064400050415     o                       wLDRST              37
064500050415     o                       wLDrst2             40
064600050415     O                                           42 '";'
064700010717     O********* TERMINAL/LINEA/ZONA ARRIVO
064800010717     O          E            LV22D3      1
064900010925     O                                           14 'HLT;01;VLT;02;'
065000010717     O          E            LV22D3      1
065100010717     O*                                       CARATTERE
065200021115     O                                           21 'DCH;70;DCW;75;ICS;08;'
065300010717     O********* TERMINAL
065400010717     O          E            LV22D3      1
065500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065600030626     O                                           23 'VBR;065;HBR;105;DHR;$1;'
065700010717     O          E            LV22D3      1
065800010717     O                                            1 '"'
065900010717     O                       §ODTNA               4
066000010717     O                                            6 '";'
066100010717     O* BOX X TERMINAL ARRIVO
066200010717     O          E            LV22D3      1
066300010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066400030626     O                                           21 'HOME;VBR;015;HBR;100;'
066500010717     O          E            LV22D3      1
066600010717     O* FONDO NERO                                     L   H
066700021115     O                                           16 'DBOX;0;0;140;55;'
066800010717     O********* CODICE LINEA ARRIVO
066900010717     O          E            LV22D3      1
067000040422     O                                           21 'DCH;45;DCW;40;ICS;08;'
067100010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067200010925     O          E            LV22D3      1
067300040422     O                                           23 'VBR;075;HBR;260;DHR;$1;'
067400010717     O          E            LV22D3      1
067500010717     O                                            1 '"'
067600010717     O                       §ODLNA               4
067700010717     O                                            6 '";'
067800040422      *nazione x EEX
067900040422     O          E    55      LV22d3      1
068000040422     O                                              'HBR;360;VBR;075;'
068100040422     O                                              '"'
068200040422     O                       §DEISA
068300040422     O                                              '";'
068400040422      * cap estero per EEX
068500040422     O          E    55      LV22d3      1
068600040422     O                                              'HBR;450;VBR;075;'
068700040422     O                                              '"'
068800040422     O                       §LDCAD
068900040422     O                                              '";'
069000040422      * box per EEX
069100040422     O          E    55      LV22d3      1
069200040422     O                                              'HBR;000;VBR;012;'
069300040422     O                                              'DBOX;355;030;240;037;'
069400010719     O********* CODICE pista
069500040422     O          E            LV22D3      1
069600040422     O                                           21 'DCH;30;DCW;30;ICS;08;'
069700010717     O          E            LV22D3      1
069800010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
069900040422     O                                           23 'VBR;035;HBR;250;DHR;$1;'
070000010717     O          E            LV22D3      1
070100010717     O                                            1 '"'
070200010719     O                       pista                2
070300010717     O                                            4 '";'
070400010719     O********* CODICE pallet
070500010717     O          E            LV22D3      1
070600010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
070700040422     O                                           23 'VBR;035;HBR;340;DHR;$1;'
070800010717     O          E            LV22D3      1
070900010717     O                                            1 '"'
071000010719     O                       pallet               3
071100010717     O                                            5 '";'
071200010719     O********* CODICE operatore
071300010717     O          E            LV22D3      1
071400010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071500050415     O                                           23 'VBR;035;HBR;420;DHR;$1;'
071600010717     O          E            LV22D3      1
071700010717     O                                            1 '"'
071800010719     O                       operat               2
071900010717     O                                            4 '";'
072000010717     O********* ZONA ARRIVO
072100040422     O          E   n55      LV22D3      1
072200010925     O*                                       CARATTERE
072300021115     O                                           21 'DCH;70;DCW;75;ICS;08;'
072400040422     O          E   n55      LV22D3      1
072500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
072600021115     O                                           23 'HBR;500;VBR;065;DHR;$1;'
072700040422     O          E   n55      LV22D3      1
072800010717     O                                            1 '"'
072900010717     O                       §ODZSC               3
073000010717     O                                            5 '";'
073100010717     O* BOX X ZONA ARRIVO
073200040422     O          E   n55      LV22D3      1
073300010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
073400021115     O                                           21 'HOME;VBR;015;HBR;490;'
073500040422     O          E   n55      LV22D3      1
073600010925     O* FONDO bianco                                   L   H
073700021115     O                                           16 'DBOX;0;0;105;55;'
073800941216     O********* Chiusura etchetta
073900941216     O          E            LV22D8      1
074000951020     O********                               CON COVERTITORE
074100951023     O                                            9 'TRM;%%5C%'
074200980327     O*-------------------------------------------------------
074300980327     O* ETICHETTA ESTERO EUROEXPRESS
074400980327     O*-------------------------------------------------------
074500980327     O          E            LV22Y            01
074600980327     O******                                 inizializzazione NUOVA
074700980327     O                                              '&&??%%P%%7E%^"LV22";'
074800980327     O          E            LV22Y       1
074900980327     O*LOGICA                                N.ET. V (H) O=-35 X TB340
075000020131     O                                           16 '00001;0;265;-70;'
075100020131     O*                                             '00001;0;265;000;'
075200980327     O          E            LV22Y       1
075300980327     O*FISICA
075400010719     O                                              'UTOF;2700;SPB;'
075500980327     O          E            LV22Y       1
075600980327     O*GIRO SCRITTA A 180°
075700010719     O                                              'FLIP;300;'
075800980327     O          E            LV22Y       1
075900980327     O*                                       FONT
076000010718     O                                              'DHR;$1;'
076100980327     O          E            LV22Y       1
076200980327     O                                              'HLT;01;VLT;01;'
076300980327      *---*   NAZIONE PARTENZA
076400980327     O          E            LV22Y       1
076500010718     O                                              'HBR;110;VBR;042;'
076600980327     O                                              'DCH;30;DCW;30;ICS;02;'
076700980327     O                                              '"'
076800980327     O                       §DEISP
076900980327     O                                              '";'
077000980327      *---*   CONTRASSEGNO
077100980327     O          E    14      LV22Y       1
077200980330     O                                              'HBR;000;VBR;012;'
077300010718     O                                              'DBOX;520;010;160;048;'
077400980327     O          E    14      LV22Y       1
077500010718     O                                              'HBR;530;VBR;062;'
077600980327     O                                              'DCH;050;DCW;090;ICS;02;'
077700980327     O                                              '"COD";'
077800980327      *---*   EUROEXPRESS
077900980327     O          E            LV22Y       1
078000010718     O                                              'HBR;180;VBR;050;'
078100010718     O                                              'DCH;030;DCW;050;ICS;02;'
078200980327     O                                              '"EuroExpress";'
078300980327      *---*   NAZIONE ARRIVO
078400980327     O          E            LV22Y       1
078500980330     O                                              'HBR;000;VBR;012;'
078600010718     O                                              'DBOX;080;060;150;050;'
078700980327     O          E            LV22Y       1
078800010718     O                                              'HBR;090;VBR;085;'
078900010718     O                                              'DCH;015;DCW;015;ICS;02;'
079000980327     O                                              '"Country";'
079100980327     O          E            LV22Y       1
079200010718     O                                              'HBR;090;VBR;115;'
079300010718     O                                              'DCH;040;DCW;050;ICS;02;'
079400980327     O                                              '"'
079500980327     O                       §DEISA
079600980327     O                                              '";'
079700980327      *---*   CAP DESTINO
079800980327     O          E            LV22Y       1
079900980330     O                                              'HBR;000;VBR;012;'
080000010718     O                                              'DBOX;290;060;280;050;'
080100980327     O          E            LV22Y       1
080200010718     O                                              'HBR;300;VBR;085;'
080300010718     O                                              'DCH;015;DCW;015;ICS;02;'
080400980327     O                                              '"Post Code";'
080500980327     O          E            LV22Y       1
080600010718     O                                              'HBR;300;VBR;115;'
080700010718     O                                              'DCH;040;DCW;050;ICS;02;'
080800980327     O                                              '"'
080900980327     O                       §LDCAD
081000980327     O                                              '";'
081100980327      *---*   BARCODE EUROEXPRESS
081200011120     O          E   n23      LV22Y       1
081300010718     O                                              'BDEF;25;BCSH;100;'
081400011120     O                                              'BCPI;2;'
081500010925     O          E   n23      LV22Y       1
081600010718     O                     13                       'HBR;037;VBR;230;'
081700010718     O                    N13                       'HBR;062;VBR;230;'
081800010731     O                                              'BCST;"A'
081900011120     O                       alfa5
082000011120     O                                              '%%40%D'
082100011120     O                       nume12
082200011120     O                                              '%%40%F'
082300011120     o                       alfa2
082400011120     O                       §end
082500011120      *---*   BARCODE EUROEXPRESS
082600011120     O          E    23      LV22Y       1
082700011120     O                                              'BDEF;25;BCSH;100;'
082800011120     O                                              'BCPI;1;'
082900010925      *con 55l
083000010925     O          E    23      LV22Y       1
083100010925     O                     13                       'HBR;037;VBR;230;'
083200010925     O                    N13                       'HBR;062;VBR;230;'
083300010925     O*                                             'BCST;"%%40%F'
083400010925     O                                              'BCST;"A'
083500010925     O                       §DEJEU
083600010925     O                       §DEISP
083700010925     O                                              '%%40%D'
083800010925     O                       §LDLNP
083900010925     O                       §LDNRS
084000010925     O                       §LDNCD
084100010925     O                                              '%%40%F'
084200010925     O                                              ' '
084300010925     O                       §DE55L
084400010925     O                       §BCEND
084500011120     O          E    23      LV22Y       1
084600980401      *---*   DECODIFICA
084700010719     O                                              'HBR;220;VBR;250;'
084800010719     O                                              'DCH;15;DCW;18;ICS;02;'
084900980401     O                                              '"'
085000980401     O                       §DEJEU
085100980401     O                       §DEISP
085200980401     O                       §LDLNP
085300980401     O                       §LDNRS
085400980401     O                       §LDNCD
085500011120     O                                              ' '
085600011120     O                       §DE55L
085700011120     O                       §BCD
085800980401     O                                              '";'
085900011120     O          E   n23      LV22Y       1
086000011120      *---*   DECODIFICA
086100011120     O                                              'HBR;220;VBR;250;'
086200011120     O                                              'DCH;15;DCW;18;ICS;02;'
086300011120     O                                              '"'
086400011120     O                       alfa5
086500011120     O                       nume12
086600011120     O                       alfa2
086700011120     O                                              '";'
086800980327      *---*   RIFERIMENTI BARTOLINI
086900980327     O          E            LV22Y       1
087000020111     O                                              'HBR;030;VBR;260;'
087100010719     O                                              'DCH;020;DCW;020;ICS;02;'
087200980327     O                                              '"'
087300980327     O                       §LDLNP
087400980327     O                                              ' '
087500980327     O                       §LDNRS
087600980327     O                                              ' '
087700980327     O                       §LDNCD
087800980327     O                                              '";'
087900980327     O********* Chiusura etchetta
088000980327     O          E            LV22Y       1
088100980327     O********                               CON COVERTITORE
088200980327     O                                              'TRM;%%5C%'
088300980327     O********                               CON VIDEO
088400980327     O**                                    'TRM;\'
088500000118     o*-----------------------------------------------------------------
088600000118     o*etichetta dpd "dove"
088700000118      *
088800000118     O          E            LV22dpd1         01
088900000118     O                                           20 '&&??%%P%%7E%^"LV22";'
089000000118     O          E            LV22dpd1    1
089100000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
089200020131     O*                                          16 '00001;0;265;-17;'
089300020131     O                                           16 '00001;0;265;-70;'
089400010718     O          E            LV22dpd1    1
089500000118     O*FISICA
089600010718     O                                           14 'UTOF;2700;SPB;'
089700000118     O          E            LV22dpd1    1
089800000118     O*GIRO SCRITTA A 180°
089900010719     O                                            9 'FLIP;300;'
090000000118     O          E            LV22dpd1    1
090100010718     O                                            9 'DHR;$201;'
090200000118     O          E            LV22dpd1    1
090300000118     O                                           14 'HLT;01;VLT;01;'
090400000118      *---*   numero  spedizione
090500000118     O          E            LV22dpd1    1
090600000128     O                                           23 'DCH;020;DCW;020;ICS;03;'
090700000118     O          E            LV22dpd1    1
090800020114     O                                           16 'HBR;030;VBR;220;'
090900000118     O          E            LV22dpd1    1
091000000118     O                                            1 '"'
091100000118     O                       §dpd3                4
091200000118     O                                            6 '";'
091300000118      * 8 caratteri numero spedizione
091400000118     O          E            LV22dpd1    1
091500000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
091600000118     O          E            LV22dpd1    1
091700020114     O                                           16 'HBR;030;VBR;200;'
091800000118     O          E            LV22dpd1    1
091900000118     O                                            1 '"'
092000000118     O                       §dpd8                9
092100000118     O                                           11 '";'
092200000118      *ultimo carattere numero spedizione
092300000118     O          E            LV22dpd1    1
092400000128     O                                           23 'DCH;020;DCW;020;ICS;01;'
092500000118     O          E            LV22dpd1    1
092600020114     O                                           16 'HBR;030;VBR;145;'
092700000118     o          E            LV22dpd1    1
092800000118     O                                            1 '"'
092900000118     O                       §dpd12               2
093000000118     O                                            4 '";'
093100000118      *---*   costante scritta
093200000118     O          E            LV22dpd1    1
093300000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
093400000118     O          E            LV22dpd1    1
093500010718     O                                           16 'HBR;110;VBR;230;'
093600000118     O          E            LV22dpd1    1
093700000118     O                                            1 '"'
093800000118     O                       §dpdn1              52
093900000118     O                                           54 '";'
094000000118      *---*   costante scritta 2
094100000118     O          E            LV22dpd1    1
094200000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
094300000118     O          E            LV22dpd1    1
094400010718     O                                           16 'HBR;130;VBR;230;'
094500000118     O          E            LV22dpd1    1
094600000118     O                                            1 '"'
094700000118     O                       §dpdn2              50
094800000118     O                                           52 '";'
094900000118      *---*   costante scritta 3
095000000118     O          E            LV22dpd1    1
095100000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
095200000118     O          E            LV22dpd1    1
095300010718     O                                           16 'HBR;150;VBR;230;'
095400000118     O          E            LV22dpd1    1
095500000118     O                                            1 '"'
095600000118     O                       §dpdn3              54
095700000118     O                                           56 '";'
095800000118      *---*   costante barra sopra
095900000118     O          E            LV22dpd1    1
096000010718     O                                           16 'HBR;270;VBR;050;'
096100000118     O          E            LV22dpd1    1
096200010718     O                                           18 'VLT;2;DVL;0;0;155;'
096300000118      *---*   costante barra sotto
096400000118     O          E            LV22dpd1    1
096500010718     O                                           16 'HBR;470;VBR;050;'
096600000118     O          E            LV22dpd1    1
096700010718     O                                           18 'VLT;2;DVL;0;0;155;'
096800000118     o* secondo codice a barre
096900000118     O          e            lv22dpd1    1
097000020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
097100020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
097200020114     O          e            lv22dpd1    1
097300020114     O                                            9 'BCSH;200;'
097400000118     O          E            LV22dpd1    1
097500010718     O                                           16 'HBR;270;VBR;050;'
097600000118     o          E            LV22dpd1    1
097700000118     O                                            5 'BCST;'
097800000118     O                                            6 '"'
097900000118     O                       §deawb              16
098000000118     O                                           18 '";'
098100000118     O                                           23 'BSTP;'
098200000118      *costante 3 v1/00
098300000118     O          E            LV22dpd1    1
098400000118     O                                           23 'DCH;040;DCW;040;ICS;02;'
098500000118     O          E            LV22dpd1    1
098600010718     O                                           16 'HBR;190;VBR;180;'
098700000118     o          E            LV22dpd1    1
098800000118     O                                            1 '"'
098900000124     O                       §dex1                6
099000000124     O                                            8 '";'
099100000118      *decodifica 2° bcc
099200000118     O          E            LV22dpd1    1
099300000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
099400000118     O          E            LV22dpd1    1
099500010718     O                                           16 'HBR;480;VBR;200;'
099600000118     o          E            LV22dpd1    1
099700000118     O                                            1 '"'
099800000118     O                       §dpdcap             14
099900000118     O                                           16 '";'
100000000118      *costante x
100100000118     O          E            LV22dpd1    1
100200000118     O                                           23 'DCH;050;DCW;030;ICS;02;'
100300000118     O          E            LV22dpd1    1
100400010718     O                                           16 'HBR;470;VBR;260;'
100500000118     o          E            LV22dpd1    1
100600000118     O                                            1 '"'
100700000118     O                       §desse               2
100800000118     O                                            4 '";'
100900000118      *costante 09
101000000118     O          E            LV22dpd1    1
101100000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
101200000118     O          E            LV22dpd1    1
101300020111     O                                           16 'HBR;570;VBR;260;'
101400000118     o          E            LV22dpd1    1
101500000118     O                                            1 '"'
101600000118     O                       dpd9                 3
101700000118     O                                            5 '";'
101800000118      *linea di arrivo
101900000118     O          E            LV22dpd1    1
102000000118     O                                           23 'DCH;080;DCW;040;ICS;02;'
102100000118     O          E            LV22dpd1    1
102200010718     O                                           16 'HBR;550;VBR;200;'
102300000118     o          E            LV22dpd1    1
102400000118     O                                            1 '"'
102500000124     O                       §dex3                4
102600000118     O                                            6 '";'
102700000118      *zona arrivo
102800000118     O          E            LV22dpd1    1
102900000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
103000000118     O          E            LV22dpd1    1
103100010718     O                                           16 'HBR;570;VBR;130;'
103200000118     o          E            LV22dpd1    1
103300000118     O                                            1 '"'
103400000124     O                       §dex4                3
103500000118     O                                            5 '";'
103600000118     O*------------------------------------------------------------------
103700000118     O          E            LV22dpd2         01
103800000118     O                                           20 '&&??%%P%%7E%^"LV22";'
103900000118     O          E            LV22dpd2    1
104000000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
104100020131     O*                                          16 '00001;0;265;-17;'
104200020131     O                                           16 '00001;0;265;-70;'
104300000118     O          E            LV22dpd2    1
104400000118     O*FISICA
104500010718     O                                           14 'UTOF;2700;SPB;'
104600000118     O          E            LV22dpd2    1
104700000118     O*GIRO SCRITTA A 180°
104800010719     O                                            9 'FLIP;300;'
104900000118     O          E            LV22dpd2    1
105000010718     O                                            9 'DHR;$201;'
105100000118     O          E            LV22dpd2    1
105200000118     O                                           14 'HLT;01;VLT;01;'
105300000118      *---*   numero  spedizione
105400000118     O          E            LV22dpd2    1
105500000118     O                                           23 'DCH;050;DCW;030;ICS;03;'
105600000118     O          E            LV22dpd2    1
105700010718     O                                           16 'HBR;110;VBR;240;'
105800000118     O          E            LV22dpd2    1
105900000118     O                                            1 '"'
106000000118     O                       §dpd3                4
106100000118     O                                            6 '";'
106200000118      * 8 caratteri numero spedizione
106300000118     O          E            LV22dpd2    1
106400010718     O                                           23 'DCH;040;DCW;030;ICS;02;'
106500000118     O          E            LV22dpd2    1
106600010718     O                                           16 'HBR;120;VBR;200;'
106700000118     O          E            LV22dpd2    1
106800000118     O                                            1 '"'
106900000118     O                       §dpd8                9
107000000118     O                                           11 '";'
107100000118      *ultimo carattere numero spedizione
107200000118     O          E            LV22dpd2    1
107300000118     O                                           23 'DCH;035;DCW;020;ICS;01;'
107400000118     O          E            LV22dpd2    1
107500010718     O                                           16 'HBR;130;VBR;120;'
107600000118     o          E            LV22dpd2    1
107700000118     O                                            1 '"'
107800000118     O                       §dpd12               2
107900000118     O                                            4 '";'
108000000118      *---*   costante TELEFONO
108100000118     O          E            LV22dpd2    1
108200000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
108300000118     O          E            LV22dpd2    1
108400020114     O                                           16 'HBR;020;VBR;230;'
108500000118     O          E            LV22dpd2    1
108600000118     O                                            1 '"'
108700000118     O                       dpd2                20
108800000118     O                                           22 '";'
108900000118      *---*   costante fax
109000000118     O          E            LV22dpd2    1
109100000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
109200000118     O          E            LV22dpd2    1
109300020114     O                                           16 'HBR;040;VBR;230;'
109400000118     O          E            LV22dpd2    1
109500000118     O                                            1 '"'
109600000118     O                       dpd2f               20
109700000118     O                                           22 '";'
109800000118      *costante mittente
109900000118     O          E            LV22dpd2    1
110000000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
110100000118     O          E            LV22dpd2    1
110200020111     O                                           25 'HBR;180;VBR;260;'
110300000118     o          E            LV22dpd2    1
110400000118     O                                            1 '"'
110500000118     O                       dpd3                 8
110600000118     O                                           10 '";'
110700000118      *costante mittente2
110800000118     O          E            LV22dpd2    1
110900000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
111000000118     O          E            LV22dpd2    1
111100020111     O                                           25 'HBR;210;VBR;260;'
111200000118     o          E            LV22dpd2    1
111300000118     O                                            1 '"'
111400000118     O                       §ldrsm              36
111500000118     O                                           38 '";'
111600000118      *costante mittente 3
111700000118     O          E            LV22dpd2    1
111800000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
111900000118     O          E            LV22dpd2    1
112000020111     O                                           25 'HBR;240;VBR;260;'
112100000118     o          E            LV22dpd2    1
112200000118     O                                            1 '"'
112300000118     O                       §ldprm               3
112400000118     O                                            5 '";'
112500000118      *costante destinatario
112600000118     O          E            LV22dpd2    1
112700000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
112800000118     O          E            LV22dpd2    1
112900020111     O                                           25 'HBR;275;VBR;260;'
113000000118     o          E            LV22dpd2    1
113100000118     O                                            1 '"'
113200000118     O                       dpd4                11
113300000118     O                                           13 '";'
113400000118      *destinatario
113500000118     O          E            LV22dpd2    1
113600000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
113700000118     O          E            LV22dpd2    1
113800020111     O                                           20 'HBR;300;VBR;260;'
113900000118     o          E            LV22dpd2    1
114000000118     O                                            1 '"'
114100000118     O                       §ldrsd              36
114200000118     O                                           38 '";'
114300000118      * destinatario indirizzo
114400000118     O          E            LV22dpd2    1
114500000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
114600000118     O          E            LV22dpd2    1
114700020111     O                                           20 'HBR;330;VBR;260;'
114800000118     o          E            LV22dpd2    1
114900000118     O                                            1 '"'
115000000118     O                       §ldind              36
115100000118     O                                           38 '";'
115200000118      *località destinatario 7
115300000118     O          E            LV22dpd2    1
115400000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
115500000118     O          E            LV22dpd2    1
115600020111     O                                           20 'HBR;360;VBR;260;'
115700000118     o          E            LV22dpd2    1
115800000118     O                                            1 '"'
115900000118     O                       §ldlod              36
116000000118     O                                           38 '";'
116100000118      *cap      destinatario 8
116200000118     O          E            LV22dpd2    1
116300000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
116400000118     O          E            LV22dpd2    1
116500020111     O                                           20 'HBR;400;VBR;260;'
116600000118     o          E            LV22dpd2    1
116700000118     O                                            1 '"'
116800000118     O                       §ldcad              10
116900000118     O                                           12 '";'
117000000118      *cap      destinatario 8
117100000118     O          E            LV22dpd2    1
117200000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
117300000118     O          E            LV22dpd2    1
117400010718     O                                           20 'HBR;400;VBR;180;'
117500000118     o          E            LV22dpd2    1
117600000118     O                                            1 '"'
117700000118     O                       §desia               4
117800000118     O                                            6 '";'
117900000118      *---*   costante barra sopra
118000000118     O          E            LV22dpd2    1
118100010718     O                                           16 'HBR;450;VBR;050;'
118200000118     O          E            LV22dpd2    1
118300010718     O                                           18 'VLT;2;DVL;0;0;185;'
118400000118      *---*   costante barra sotto
118500000118     O          E            LV22dpd2    1
118600010718     O                                           16 'HBR;650;VBR;050;'
118700000118     O          E            LV22dpd2    1
118800010718     O                                           18 'VLT;2;DVL;0;0;185;'
118900000118      *costante primo B-C
119000000118     O          e            lv22dpd2    1
119100020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
119200020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
119300020114     O          e            lv22dpd2    1
119400020114     O                                            9 'BCSH;200;'
119500000118     O          E            LV22dpd2    1
119600010718     O                                           16 'HBR;450;VBR;050;'
119700000118     o          E            LV22dpd2    1
119800000118     O                                            5 'BCST;'
119900000118     O                                            6 '"'
120000000118     O                       §dpd                18
120100000118     O                                           20 '";'
120200000118     O                                           25 'BSTP;'
120300000118      *costante data
120400000118     O          E            LV22dpd2    1
120500000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
120600000118     O          E            LV22dpd2    1
120700020111     O                                           16 'HBR;670;VBR;260;'
120800000118     o          E            LV22dpd2    1
120900000118     O                                            1 '"'
121000000118     O                       datasp              11 '  /  /    '
121100000118     O                                           13 '";'
121200000118      *costante 1 collo
121300000118     O          E            LV22dpd2    1
121400000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
121500000118     O          E            LV22dpd2    1
121600010718     O                                           16 'HBR;670;VBR;200;'
121700000118     o          E            LV22dpd2    1
121800000118     O                                            1 '"'
121900000118     O                                            8 '1 Collo'
122000000118     O                                           10 '";'
122100000118      *costante peso
122200000118     O          E            LV22dpd2    1
122300000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
122400000118     O          E            LV22dpd2    1
122500010718     O                                           16 'HBR;670;VBR;150;'
122600000118     o          E            LV22dpd2    1
122700000118     O                                            1 '"'
122800000118     O                                            8 'Peso Kg'
122900000118     O                                           10 '";'
123000000118      * peso
123100000118     O          E            LV22dpd2    1
123200000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
123300000118     O          E            LV22dpd2    1
123400010718     O                                           16 'HBR;670;VBR;100;'
123500000118     o          E            LV22dpd2    1
123600000118     O                                            1 '"'
123700000118     O                       §ldpkg        2     10
123800000118     O                                           12 '";'
123900000118      *------------------------------------------------------------------
124000000118      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
124100000118     O* costante dpd
124200000118     O          E            LV22dpd     1
124300000118     O                                           23 'DCH;030;DCW;070;ICS;03;'
124400000118     O          E            LV22dpd     1
124500020114     O                                           25 'HBR;160;VBR;280;DHR;$401;'
124600000118     o          E            LV22dpd     1
124700000118     O                                            1 '"'
124800000118     O                       dpd                  4
124900000118     O                                            6 '";'
125000000118     O* costante dpd riga sotto
125100000118     O          E            LV22dpd     1
125200000118     O                                           23 'DCH;006;DCW;005;ICS;01;'
125300000118     O          E            LV22dpd     1
125400010718     O                                           20 'HBR;160;VBR;240;'
125500000118     o          E            LV22dpd     1
125600000118     O                                            1 '"'
125700000118     O                       dpd1                23
125800000118     O                                           25 '";'
125900000118     o          E            LV22dpd     1
126000000118     O                                            9 'TRM;%%5C%'
