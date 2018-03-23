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
019000951023     C     §LDCDP        COMP      *BLANK                             18
019100951023     C     §LDNRS        COMP      *ZERO                              19
019200951023     C*
019300951023     C** INIZIALIZZA STAMPANTE
019400951023     C                   EXCEPT    LV22
019500951023     C*
019600951023     C                   MOVEA     *BLANK        §RX
019700951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
019800951023     C                   SETOFF                                       77
019900951023     C                   MOVE      *BLANK        §A7               7
020000951023     C                   Z-ADD     §LDNC1        §N7
020100951023     C                   DO        7             §1
020200951023     C     §S7(§1)       IFEQ      *ZERO
020300951023     C                   MOVE      *BLANK        §S7(§1)
020400951023     C                   ELSE
020500951023     C                   SETON                                        77
020600951023     C                   END
020700951023     C  N77              END
020800951023     C                   MOVEA     §S7           §A7
020900951023     C                   MOVE      §A7           §A3               3
021000951023     C                   MOVEA     §A3           §RX(1)
021100951023     C                   MOVEA     '-'           §RX(4)
021200951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
021300951023     C                   SETOFF                                       77
021400951023     C                   MOVE      *BLANK        §A7
021500951023     C                   Z-ADD     §LDNC2        §N7
021600951023     C                   DO        7             §1
021700951023     C     §S7(§1)       IFEQ      *ZERO
021800951023     C                   MOVE      *BLANK        §S7(§1)
021900951023     C                   ELSE
022000951023     C                   SETON                                        77
022100951023     C                   END
022200951023     C  N77              END
022300951023     C                   MOVEA     §S7           §A7
022400951023     C                   MOVE      §A7           §A3
022500951023     C                   MOVEA     §A3           §RX(5)
022600951023     C                   MOVE      §LDCPD        §A3
022700951023     C                   MOVEA     §A3           §RX(9)
022800951023     C* STAMPA LINEA PARTENZA
022900951023     C                   EXCEPT    LV22H
023000951023     C*
023100951023     C                   MOVEA     *BLANK        §RI
023200010717     C                   MOVEA     §LDRSM        §RI(6)
023300951023     C                   MOVEA     ' '           §RI(30)
023400951023     C                   MOVEA     §LDPRM        §RI(31)
023500951023     C                   MOVEA     *BLANK        §RI(33)
023600951023     C                   EXCEPT    LV22D1
023700951023     C* STAMPA DATI MITTENTE
023800951023     C*
023900951023     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024000971222     C                   EXSR      COLZER
024100951023     C*
024200971222     C*
024300951023     C* STAMPA NUMERO SEGNACOLLO
024400951023     C                   EXCEPT    LV22D2
024500951023     C*
024600951023     C                   MOVE      §LDTNA        §A3               3
024700951023     C     '0':'O'       XLATE     §A3           §ODTNA            3
024800951023     C                   MOVE      §LDLNA        §A3
024900951023     C     '0':'O'       XLATE     §A3           §ODLNA            3
025000951023     C                   MOVE      §LDZSC        §A2
025100951023     C     '0':'O'       XLATE     §A2           §ODZSC            2
025200951023     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
025300951023     C                   MOVEL     §LDDLA        §ODDLA            8
025400951023     C* STAMPA LINEA ARRIVO
025500951023     C* X 15 STAMPA A "MODOD ESTERO"
025600951023     C   15              DO
025700951023     C                   MOVEL     §ODDLA        §EEN              3
025800951023     C                   MOVEL     §LDCAD        §CA2              2
025900951023     C                   MOVE      §LDCAD        §CA7              7
026000951023     C                   END
026100951023     C                   EXCEPT    LV22D3
026200951023     C*
026300951023     C                   MOVEA     *BLANK        §RI
026400010717     C                   MOVEA     §LDRSD        §RI(6)
026500951023     C                   MOVEA     ' '           §RI(30)
026600951023     C                   MOVEA     §LDPRD        §RI(31)
026700951023     C                   MOVEA     *BLANK        §RI(33)
026800951023     C* STAMPA 1° DATI DESTINATARIO
026900951023     C                   EXCEPT    LV22D4
027000951023     C*
027100951023     C                   MOVEA     *BLANK        §RI
027200010717     C                   MOVEA     §LDIND        §RI(6)
027300951023     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
027400951023     C                   Z-ADD     37            §Y                2 0    88
027500951023     C                   DO        36
027600951023     C                   SUB       1             §Y
027700951023     C     §RI(§Y)       IFNE      ' '
027800951023     C                   SETON                                        88
027900951023     C                   END
028000951023     C  N88              END
028100951023     C     §Y            IFLE      25
028200951023     C                   ADD       3             §Y
028300951023     C                   ELSE
028400951023     C                   MOVEA     '  '          §RI(26)
028500951023     C                   Z-ADD     28            §Y
028600951023     C                   END
028700951023     C                   MOVEA     §LDLOD        §RI(§Y)
028800951023     C* STAMPA 2° DATI DESTINATARIO
028900951023     C                   EXCEPT    LV22D5
029000951023     C* STAMPA SERVIZIO
029100951023     C   16              EXCEPT    LV22X5
029200951023     C*
029300951023     C* STAMPA BARCODE
029400951023     C*
029500951023     C* (CAMPO UNICO BARCODE E COSTANTI)
029600951023     C                   MOVE      *BLANK        §§CDB1           19
029700951023     C                   MOVE      §CDBAR        §§CDB1
029800951023     C                   MOVEL     '"'           §§CDB1
029900951023     C                   MOVE      *BLANK        §§CDB2           21
030000951023     C                   MOVEL     §§CDB1        §§CDB2
030100951023     C                   MOVE      '";'          §§CDB2
030200951023     C                   EXCEPT    LV22D6
030300951023     C*
030400951023     C                   MOVE      *BLANK        §RI21            21
030500951023     C   18              DO
030600951023     C                   MOVEL     'Rif.:'       §RI21
030700980402     C     §DEEUR        IFNE      *BLANK
030800980402     C     §DETST        ANDEQ     'ITA'
030900980402     C                   MOVEL     'JUT :'       §RI21
031000980402     C                   ENDIF
031100951023     C                   MOVE      §LDCDP        §RI21
031200951023     C                   END
031300951023     C* STAMPA RIFERIMENTO/RISTAMPA
031400951023     C                   EXCEPT    LV22D7
031500951023     C*
031600951023     C* STAMPA CHIUSURA ETICHETTA
031700951023     C                   EXCEPT    LV22D8
031800951023     C                   ENDSR
031900971222     C/SPACE 005
032000971222     C*****************************************************
032100971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032200971222     C*****************************************************
032300971222     C     COLZER        BEGSR
032400971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032500971222     C                   SETOFF                                       77
032600971222     C                   MOVE      §LDNCD        §N7
032700971222     C                   DO        7             §1
032800971222     C     §S7(§1)       IFEQ      *ZERO
032900971222     C                   MOVE      *BLANK        §S7(§1)
033000971222     C                   ELSE
033100971222     C                   SETON                                        77
033200971222     C                   END
033300971222     C  N77              END
033400971222     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
033500971222     C                   MOVE      *ZERO         §X                1 0
033600971222     C                   MOVEA     *BLANK        §O7
033700971222     C                   DO        7             §1
033800010925     C*    §S7(§1)       IFNE      *BLANK
033900971222     C                   ADD       1             §X
034000971222     C     §S7(§1)       IFEQ      '0'
034100011114     C     §S7(§1)       oreq      *blank
034200971222     C                   MOVE      'O'           §O7(§X)
034300971222     C                   ELSE
034400971222     C                   MOVE      §S7(§1)       §O7(§X)
034500971222     C                   END
034600010925     C*                  END
034700971222     C                   END
034800971222     C                   MOVEA     §O7           §S7
034900971222     C                   ENDSR
035000980326     C/SPACE 005
035100980326     C*****************************************************
035200980326     C* STAMPA EUROEXPRESS                              ***
035300980326     C*****************************************************
035400980326     C     EUREXP        BEGSR
035500980326     C*
035600980326     C* IMPOSTO COSTANTI
035700980326     C                   MOVEL     'JUT'         §DEJEU            3
035800980326     C                   MOVEL     '55L'         §DE55L            3
035900980326     C                   MOVEL     'IT'          §DEISP            2
036000010925      * controllo se contenuto 55L nel codice passato
036100010925     c     '55L'         scan      §deeur                                 23
036200011120
036300011120      * se contenuto 55l scompongo a modo vecchio
036400011120     c                   if        *in23
036500980326     C* ESTRAGGO IL CAP:
036600980326     C                   MOVE      *ZEROS        §Y
036700980326     C                   MOVEA     §DEEUR        §EU
036800980326     C* TROVO QUANTO è LUNGO IL BARCODE
036900980326     C     ' '           CHECKR    §DEEUR        §Y
037000980326     C* ISOLO IL CAP
037100980326     C                   MOVE      *BLANK        §O9
037200980326     C                   MOVE      *ZEROS        §W                2 0
037300980326     C     §Y            SUB       2             §Y
037400980326     C     22            DO        §Y            §X1               2 0
037500980326     C     §EU(§X1)      IFNE      *BLANKS
037600980326     C                   ADD       1             §W
037700980326     C                   MOVE      §EU(§X1)      §O9(§W)
037800980326     C                   END
037900980326     C                   END
038000980326     C                   MOVEA     §O9           §LDCAD
038100011120
038200011120      *scompongo a modo nuovo senza 55l
038300011120     c                   else
038400011120     c                   movel     §deeur        §bceurex
038500011120     c                   end
038600980326     C*
038700980326     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
038800980326     C* DEL BARCODE
038900980326     C                   MOVEL     '";BSTP;'     §END              7
039000980326     C     §LDCAD        CAT       §DEISA:0      §BCD             11
039100980326     C     §BCD          CAT       §END:0        §BCEND           20
039200980326     C*
039300980326     C* VERIFICO SE CONTRASSEGNO
039400980326     C*
039500980326     C     §DEFCA        COMP      'S'                                    14
039600980326     C  N14§DEFCA        COMP      'X'                                    14
039700990323     C*
039800990323     C* VERIFICO SE INGHILTERRA
039900990323     C*
040000990323     C     §DEISA        COMP      'GB'                                   13
040100980326     C*
040200980326     C                   EXCEPT    LV22Y
040300980326     C*
040400980326     C                   ENDSR
040500910403     C/SPACE 005
040600910403     C*****************************************************
040700910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
040800910403     C*****************************************************
040900910403     C     M10           BEGSR
041000910403     C                   MOVE      *ZERO         §TT               3 0
041100910403     C                   MOVE      *ZERO         §TT2              3 0
041200910403     C                   MOVE      *ZERO         §1                1 0
041300910403     C                   Z-ADD     18            §S                2 0
041400910403     C                   DO        17            §N                2 0
041500910403     C                   SUB       1             §S
041600910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
041700910403     C                   MOVE      §C(§N)        §CB(§N)
041800910403     C     §N            DIV       2             §CTR              3 0
041900910403     C                   MVR                     §RST              3 010
042000910403     C     *IN(10)       IFEQ      '1'
042100910403     C     §C(§S)        MULT      2             §N2               2 0
042200910403     C                   MOVE      §N2           §A2               2
042300910403     C                   MOVEA     §A2           §2A
042400910403     C                   XFOOT     §2            §CTR
042500910403     C                   ELSE
042600910403     C                   Z-ADD     §C(§S)        §CTR
042700910403     C                   END
042800910403     C                   ADD       §CTR          §TT
042900910403     C                   END
043000910403     C*
043100910403     C     §TT           ADD       9             §TT2
043200910403     C                   MOVE      0             §TT2
043300910403     C     §TT2          SUB       §TT           §1
043400910403     C*
043500910403     C                   MOVE      §1            §CB(18)
043600910403     C                   ENDSR
043700000118      *--------------------------------------------------
043800000118     c     dpddove       begsr
043900000118      *--------------------------------------------------
044000000118     c                   eval      §dpd = §deeur
044100000124     c                   eval      §dexx1ds = §dexx1
044200000118     c                   eval      §deawbds = §deawb
044300000118     c                   eval      §dpdc1 = §dpdc1w
044400000118     c                   eval      §dpdc3 = §dpdc3w
044500000118     c                   eval      §dpdc5 = §dpdc5w
044600000118     c                   eval      §dpdc7 = §dpdc7w
044700000118     c                   eval      §dpdn = 'Notifications on damages which are -
044800000118     c                             not recognizable from the outside have to -
044900000118     c                             have to be submitted to the transport -
045000000118     c                             organisation withing 7 days in written.'
045100000118     C                   except    LV22dpd1
045200000118     C                   except    LV22dpd
045300000118     C                   endsr
045400000118      *--------------------------------------------------
045500000118     c     dpdchi        begsr
045600000118      *--------------------------------------------------
045700000118     c                   move      §ldmgs        giorno            2 0
045800000118     c                   movel     §ldmgs        mese              2 0
045900000118     c                   move      mese          giomes            4 0
046000000118     c                   movel     giorno        giomes
046100000118     c                   movel     giomes        datasp            8 0
046200000118     c                   move      §ldaas        datasp
046300000118     c                   movel     §dexx2        dpdteln
046400000118     c                   move      §dexx2        dpdfaxn
046500000118     C                   except    LV22dpd2
046600000118     C                   except    LV22dpd
046700000118     C                   endsr
046800000118     c*-------------------------------------------------------------------
046900971215     OFNLV22P   E            LV22             01
047000020131     O                                           21 '&&??%%P%%7E%^"TX340";'
047100010718     O          E            LV22        1
047200020131     O                                           16 '00001;0;265;-70;'
047300010718     O          E            LV22        1
047400910403     O*FISICA
047500980327     O** X TX340/240
047600010718     O                                           14 'UTOF;2700;SPB;'
047700941216     O          E            LV22        1
047800910403     O*GIRO SCRITTA A 180°
047900010719     O                                            9 'FLIP;300;'
048000941216     O          E            LV22        1
048100910403     O*                                       FONT
048200010717     O                                            7 'DHR;$1;'
048300941216     O          E            LV22        1
048400010717     O                                           14 'HLT;01;VLT;01;'
048500910403     O********* Mittente
048600941216     O          E            LV22D1      1
048700910403     O*                                       CARATTERE
048800910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
048900941216     O          E            LV22D1      1
049000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
049100050415     O                                           25 'VBR;297;HBR;610;DHR;$201;'
049200941216     O          E            LV22D1      1
049300910403     O                                            1 '"'
049400910404     O                       §RI                 38
049500920910     O                                           40 '";'
049600010925      *---*   costante barra separazione
049700010925     O          E            LV22d1      1
049800050415     O                                           16 'HBR;640;VBR;015;'
049900010925     O          E            LV22d1      1
050000010925     O                                           18 'VLT;2;DVL;0;0;230;'
050100010925     O********* Destinatario (Rag.Soc.)
050200010925     O          E            LV22D4      1
050300910403     O*                                       CARATTERE
050400020110     O                                           21 'DCH;15;DCW;20;ICS;03;'
050500941216     O          E            LV22D4      1
050600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
050700050415     O                                           25 'VBR;290;HBR;650;DHR;$201;'
050800941216     O          E            LV22D4      1
050900910403     O                                            1 '"'
051000910404     O                       §RI                 38
051100910404     O                                           40 '";'
051200910403     O********* Destinatario (Località)
051300941216     O          E            LV22D5      1
051400910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
051500050415     O                                           25 'VBR;290;HBR;675;DHR;$201;'
051600941216     O          E            LV22D5      1
051700910403     O                                            1 '"'
051800910404     O                       §RI                 38
051900910404     O                                           40 '";'
052000010717     O********* MC. SPEDIZIONE
052100010717     O          E            LV22d5      1
052200010717     O*                                       CARATTERE
052300010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
052400010717     O          E            LV22D5      1
052500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
052600011121     O                                           25 'VBR;260;HBR;710;DHR;$201;'
052700010717     O          E            LV22D5      1
052800010717     O                                            1 '"'
052900010717     O                                            4 'MC.'
053000010717     O                       §LDVOL              11 '0 ,   '
053100010717     O                       §LDFVR              14
053200010717     O                                           16 '";'
053300941216     O********* SERVIZIO
053400941216     O          E            LV22X5      1
053500921104     O*                                       CARATTERE
053600010717     O                                           22 'DCH;30;DCW;050;ICS;03;'
053700941216     O          E            LV22X5      1
053800921104     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
053900010719     O                                           23 'VBR;170;HBR;050;DHR;$1;'
054000941216     O          E            LV22X5      1
054100941216     O                                            1 '"'
054200941216     O                       §LDTSP               2
054300941216     O                                            4 '";'
054400941216     O          E            LV22X5      1
054500010719     O                                           21 'HOME;VBR;140;HBR;040;'
054600941216     O          E            LV22X5      1
054700941216     O* FONDO NERO                                     L   H
054800010717     O                     17                    17 'DCBX;0;0;060;040;'
054900941216     O* FONDO BIANCO                                   L   H
055000010717     O                    N17                    17 'DBOX;0;0;060;040;'
055100010717     O********* progr. colli e peso
055200941216     O          E            LV22H       1
055300910403     O*                                       CARATTERE
055400010717     O                                           23 'DCH;015;DCW;020;ICS;01;'
055500941216     O          E            LV22H       1
055600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
055700011121     O                                           25 'VBR;180;HBR;710;DHR;$201;'
055800941216     O          E            LV22H       1
055900910403     O                                            1 '"'
056000951021     O                       §RX                 12
056100010717     O                                           16 'KG.'
056200010717     O                       §LDPKG              26 '    0 , '
056300010717     O                                           28 '";'
056400941216     O          E            LV22H       1
056500010717     O********* Linea Partenza
056600910403     O*                                       CARATTERE
056700010717     O                                           23 'DCH;050;DCW;035;ICS;01;'
056800941216     O          E            LV22H       1
056900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
057000010719     O                                           23 'VBR;220;HBR;040;DHR;$1;'
057100941216     O          E            LV22H       1
057200910403     O                                            1 '"'
057300910403     O                       §LDLNP               4
057400910403     O                                            6 '";'
057500910403     O********* Segnacollo
057600941216     O          E    19      LV22D2      1
057700910403     O*                                       CARATTERE
057800010717     O                                           23 'DCH;050;DCW;040;ICS;08;'
057900941216     O          E    19      LV22D2      1
058000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
058100010717     O                                           23 'VBR;240;HBR;140;DHR;$1;'
058200941216     O          E    19      LV22D2      1
058300910403     O                                            1 '"'
058400920910     O                       §LDNRT               4
058500920910     O                                            6 '";'
058600010717     O          E            LV22D2      1
058700010717     O*                                       CARATTERE
058800010717     O                                           23 'DCH;050;DCW;040;ICS;12;'
058900010717     O          E            LV22D2      1
059000010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
059100010717     O                                           23 'VBR;240;HBR;230;DHR;$1;'
059200010717     O          E            LV22D2      1
059300010717     O                                            1 '"'
059400010717     O                       §S4                  5
059500010717     O*                      §S7                  8
059600010717     O                                            7 '";'
059700941216     O          E            LV22D2      1
059800910403     O*                                       CARATTERE
059900021115     O                                           23 'DCH;085;DCW;080;ICS;12;'
060000941216     O          E            LV22D2      1
060100910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
060200021115     O                                           23 'VBR;245;HBR;360;DHR;$1;'
060300941216     O          E            LV22D2      1
060400910403     O                                            1 '"'
060500010717     O                       §S3                  4
060600010717     O*                      §S7                  8
060700010717     O                                            6 '";'
060800921106     O********* DESCRIZIONE LINEA DI ARRIVO
060900010717     O          E            LV22D3      1
061000010717     O                                           14 'HLT;05;VLT;06;'
061100010717     O          E            LV22D3      1
061200921106     O*                                       CARATTERE
061300010717     O                                           21 'DCH;75;DCW;45;ICS;04;'
061400041122      *---*   costante barra sopra
061500041122     O          E            LV22d6      1
061600041122     O                                           22 'HLT;6;DHL;090;-60;400;'
061700041122      *---*   costante barra sotto
061800041122     O          E            LV22d6      1
061900041122     O                                           22 'HLT;4;DHL;090;040;402;'
062000910403     O********* Bar Code
062100041119     O          E            LV22D6      1
062200041119     O                                           16 'VBR;180;HBR;130;'
062300951121     O* BARCODE CODE 128
062400041119     O          E            LV22D6      1
062500041119     O                                           15 'BDEF;25;BCPI;2;'
062600041119     O          E            LV22D6      1
062700041119     O                                           18 'BCSH;100;BCST;"C";'
062800041119     O          E            LV22D6      1
062900921104     O                       §§CDB2              21
063000041119     O          E            LV22D6      1
063100041119     O                                            5 'BSTP;'
063200010717     O********* Codice Prodotto
063300941216     O          E            LV22D7      1
063400910403     O*                                       CARATTERE
063500910403     O                                           21 'DCH;15;DCW;26;ICS;03;'
063600941216     O          E            LV22D7      1
063700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
063800010717     O                                           23 'VBR;260;HBR;050;DHR;$1;'
063900941216     O          E            LV22D7      1
064000921104     O                                            1 '"'
064100951020     O                       §RI21               22
064200050415     o                       datasp              34 '  /  /    '
064300050415     o                       wLDRST              37
064400050415     o                       wLDrst2             40
064500050415     O                                           42 '";'
064600010717     O********* TERMINAL/LINEA/ZONA ARRIVO
064700010717     O          E            LV22D3      1
064800010925     O                                           14 'HLT;01;VLT;02;'
064900010717     O          E            LV22D3      1
065000010717     O*                                       CARATTERE
065100021115     O                                           21 'DCH;70;DCW;75;ICS;08;'
065200010717     O********* TERMINAL
065300010717     O          E            LV22D3      1
065400010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
065500030626     O                                           23 'VBR;065;HBR;105;DHR;$1;'
065600010717     O          E            LV22D3      1
065700010717     O                                            1 '"'
065800010717     O                       §ODTNA               4
065900010717     O                                            6 '";'
066000010717     O* BOX X TERMINAL ARRIVO
066100010717     O          E            LV22D3      1
066200010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
066300030626     O                                           21 'HOME;VBR;015;HBR;100;'
066400010717     O          E            LV22D3      1
066500010717     O* FONDO NERO                                     L   H
066600021115     O                                           16 'DBOX;0;0;140;55;'
066700010717     O********* CODICE LINEA ARRIVO
066800010717     O          E            LV22D3      1
066900040422     O                                           21 'DCH;45;DCW;40;ICS;08;'
067000010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
067100010925     O          E            LV22D3      1
067200040422     O                                           23 'VBR;075;HBR;260;DHR;$1;'
067300010717     O          E            LV22D3      1
067400010717     O                                            1 '"'
067500010717     O                       §ODLNA               4
067600010717     O                                            6 '";'
067700040422      *nazione x EEX
067800040422     O          E    55      LV22d3      1
067900040422     O                                              'HBR;360;VBR;075;'
068000040422     O                                              '"'
068100040422     O                       §DEISA
068200040422     O                                              '";'
068300040422      * cap estero per EEX
068400040422     O          E    55      LV22d3      1
068500040422     O                                              'HBR;450;VBR;075;'
068600040422     O                                              '"'
068700040422     O                       §LDCAD
068800040422     O                                              '";'
068900040422      * box per EEX
069000040422     O          E    55      LV22d3      1
069100040422     O                                              'HBR;000;VBR;012;'
069200040422     O                                              'DBOX;355;030;240;037;'
069300010719     O********* CODICE pista
069400040422     O          E            LV22D3      1
069500040422     O                                           21 'DCH;30;DCW;30;ICS;08;'
069600010717     O          E            LV22D3      1
069700010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
069800040422     O                                           23 'VBR;035;HBR;250;DHR;$1;'
069900010717     O          E            LV22D3      1
070000010717     O                                            1 '"'
070100010719     O                       pista                2
070200010717     O                                            4 '";'
070300010719     O********* CODICE pallet
070400010717     O          E            LV22D3      1
070500010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
070600040422     O                                           23 'VBR;035;HBR;340;DHR;$1;'
070700010717     O          E            LV22D3      1
070800010717     O                                            1 '"'
070900010719     O                       pallet               3
071000010717     O                                            5 '";'
071100010719     O********* CODICE operatore
071200010717     O          E            LV22D3      1
071300010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
071400050415     O                                           23 'VBR;035;HBR;420;DHR;$1;'
071500010717     O          E            LV22D3      1
071600010717     O                                            1 '"'
071700010719     O                       operat               2
071800010717     O                                            4 '";'
071900010717     O********* ZONA ARRIVO
072000040422     O          E   n55      LV22D3      1
072100010925     O*                                       CARATTERE
072200021115     O                                           21 'DCH;70;DCW;75;ICS;08;'
072300040422     O          E   n55      LV22D3      1
072400010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
072500021115     O                                           23 'HBR;500;VBR;065;DHR;$1;'
072600040422     O          E   n55      LV22D3      1
072700010717     O                                            1 '"'
072800010717     O                       §ODZSC               3
072900010717     O                                            5 '";'
073000010717     O* BOX X ZONA ARRIVO
073100040422     O          E   n55      LV22D3      1
073200010717     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
073300021115     O                                           21 'HOME;VBR;015;HBR;490;'
073400040422     O          E   n55      LV22D3      1
073500010925     O* FONDO bianco                                   L   H
073600021115     O                                           16 'DBOX;0;0;105;55;'
073700941216     O********* Chiusura etchetta
073800941216     O          E            LV22D8      1
073900951020     O********                               CON COVERTITORE
074000951023     O                                            9 'TRM;%%5C%'
074100980327     O*-------------------------------------------------------
074200980327     O* ETICHETTA ESTERO EUROEXPRESS
074300980327     O*-------------------------------------------------------
074400980327     O          E            LV22Y            01
074500980327     O******                                 inizializzazione NUOVA
074600980327     O                                              '&&??%%P%%7E%^"LV22";'
074700980327     O          E            LV22Y       1
074800980327     O*LOGICA                                N.ET. V (H) O=-35 X TB340
074900020131     O                                           16 '00001;0;265;-70;'
075000020131     O*                                             '00001;0;265;000;'
075100980327     O          E            LV22Y       1
075200980327     O*FISICA
075300010719     O                                              'UTOF;2700;SPB;'
075400980327     O          E            LV22Y       1
075500980327     O*GIRO SCRITTA A 180°
075600010719     O                                              'FLIP;300;'
075700980327     O          E            LV22Y       1
075800980327     O*                                       FONT
075900010718     O                                              'DHR;$1;'
076000980327     O          E            LV22Y       1
076100980327     O                                              'HLT;01;VLT;01;'
076200980327      *---*   NAZIONE PARTENZA
076300980327     O          E            LV22Y       1
076400010718     O                                              'HBR;110;VBR;042;'
076500980327     O                                              'DCH;30;DCW;30;ICS;02;'
076600980327     O                                              '"'
076700980327     O                       §DEISP
076800980327     O                                              '";'
076900980327      *---*   CONTRASSEGNO
077000980327     O          E    14      LV22Y       1
077100980330     O                                              'HBR;000;VBR;012;'
077200010718     O                                              'DBOX;520;010;160;048;'
077300980327     O          E    14      LV22Y       1
077400010718     O                                              'HBR;530;VBR;062;'
077500980327     O                                              'DCH;050;DCW;090;ICS;02;'
077600980327     O                                              '"COD";'
077700980327      *---*   EUROEXPRESS
077800980327     O          E            LV22Y       1
077900010718     O                                              'HBR;180;VBR;050;'
078000010718     O                                              'DCH;030;DCW;050;ICS;02;'
078100980327     O                                              '"EuroExpress";'
078200980327      *---*   NAZIONE ARRIVO
078300980327     O          E            LV22Y       1
078400980330     O                                              'HBR;000;VBR;012;'
078500010718     O                                              'DBOX;080;060;150;050;'
078600980327     O          E            LV22Y       1
078700010718     O                                              'HBR;090;VBR;085;'
078800010718     O                                              'DCH;015;DCW;015;ICS;02;'
078900980327     O                                              '"Country";'
079000980327     O          E            LV22Y       1
079100010718     O                                              'HBR;090;VBR;115;'
079200010718     O                                              'DCH;040;DCW;050;ICS;02;'
079300980327     O                                              '"'
079400980327     O                       §DEISA
079500980327     O                                              '";'
079600980327      *---*   CAP DESTINO
079700980327     O          E            LV22Y       1
079800980330     O                                              'HBR;000;VBR;012;'
079900010718     O                                              'DBOX;290;060;280;050;'
080000980327     O          E            LV22Y       1
080100010718     O                                              'HBR;300;VBR;085;'
080200010718     O                                              'DCH;015;DCW;015;ICS;02;'
080300980327     O                                              '"Post Code";'
080400980327     O          E            LV22Y       1
080500010718     O                                              'HBR;300;VBR;115;'
080600010718     O                                              'DCH;040;DCW;050;ICS;02;'
080700980327     O                                              '"'
080800980327     O                       §LDCAD
080900980327     O                                              '";'
081000980327      *---*   BARCODE EUROEXPRESS
081100011120     O          E   n23      LV22Y       1
081200010718     O                                              'BDEF;25;BCSH;100;'
081300011120     O                                              'BCPI;2;'
081400010925     O          E   n23      LV22Y       1
081500010718     O                     13                       'HBR;037;VBR;230;'
081600010718     O                    N13                       'HBR;062;VBR;230;'
081700010731     O                                              'BCST;"A'
081800011120     O                       alfa5
081900011120     O                                              '%%40%D'
082000011120     O                       nume12
082100011120     O                                              '%%40%F'
082200011120     o                       alfa2
082300011120     O                       §end
082400011120      *---*   BARCODE EUROEXPRESS
082500011120     O          E    23      LV22Y       1
082600011120     O                                              'BDEF;25;BCSH;100;'
082700011120     O                                              'BCPI;1;'
082800010925      *con 55l
082900010925     O          E    23      LV22Y       1
083000010925     O                     13                       'HBR;037;VBR;230;'
083100010925     O                    N13                       'HBR;062;VBR;230;'
083200010925     O*                                             'BCST;"%%40%F'
083300010925     O                                              'BCST;"A'
083400010925     O                       §DEJEU
083500010925     O                       §DEISP
083600010925     O                                              '%%40%D'
083700010925     O                       §LDLNP
083800010925     O                       §LDNRS
083900010925     O                       §LDNCD
084000010925     O                                              '%%40%F'
084100010925     O                                              ' '
084200010925     O                       §DE55L
084300010925     O                       §BCEND
084400011120     O          E    23      LV22Y       1
084500980401      *---*   DECODIFICA
084600010719     O                                              'HBR;220;VBR;250;'
084700010719     O                                              'DCH;15;DCW;18;ICS;02;'
084800980401     O                                              '"'
084900980401     O                       §DEJEU
085000980401     O                       §DEISP
085100980401     O                       §LDLNP
085200980401     O                       §LDNRS
085300980401     O                       §LDNCD
085400011120     O                                              ' '
085500011120     O                       §DE55L
085600011120     O                       §BCD
085700980401     O                                              '";'
085800011120     O          E   n23      LV22Y       1
085900011120      *---*   DECODIFICA
086000011120     O                                              'HBR;220;VBR;250;'
086100011120     O                                              'DCH;15;DCW;18;ICS;02;'
086200011120     O                                              '"'
086300011120     O                       alfa5
086400011120     O                       nume12
086500011120     O                       alfa2
086600011120     O                                              '";'
086700980327      *---*   RIFERIMENTI BARTOLINI
086800980327     O          E            LV22Y       1
086900020111     O                                              'HBR;030;VBR;260;'
087000010719     O                                              'DCH;020;DCW;020;ICS;02;'
087100980327     O                                              '"'
087200980327     O                       §LDLNP
087300980327     O                                              ' '
087400980327     O                       §LDNRS
087500980327     O                                              ' '
087600980327     O                       §LDNCD
087700980327     O                                              '";'
087800980327     O********* Chiusura etchetta
087900980327     O          E            LV22Y       1
088000980327     O********                               CON COVERTITORE
088100980327     O                                              'TRM;%%5C%'
088200980327     O********                               CON VIDEO
088300980327     O**                                    'TRM;\'
088400000118     o*-----------------------------------------------------------------
088500000118     o*etichetta dpd "dove"
088600000118      *
088700000118     O          E            LV22dpd1         01
088800000118     O                                           20 '&&??%%P%%7E%^"LV22";'
088900000118     O          E            LV22dpd1    1
089000000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
089100020131     O*                                          16 '00001;0;265;-17;'
089200020131     O                                           16 '00001;0;265;-70;'
089300010718     O          E            LV22dpd1    1
089400000118     O*FISICA
089500010718     O                                           14 'UTOF;2700;SPB;'
089600000118     O          E            LV22dpd1    1
089700000118     O*GIRO SCRITTA A 180°
089800010719     O                                            9 'FLIP;300;'
089900000118     O          E            LV22dpd1    1
090000010718     O                                            9 'DHR;$201;'
090100000118     O          E            LV22dpd1    1
090200000118     O                                           14 'HLT;01;VLT;01;'
090300000118      *---*   numero  spedizione
090400000118     O          E            LV22dpd1    1
090500000128     O                                           23 'DCH;020;DCW;020;ICS;03;'
090600000118     O          E            LV22dpd1    1
090700020114     O                                           16 'HBR;030;VBR;220;'
090800000118     O          E            LV22dpd1    1
090900000118     O                                            1 '"'
091000000118     O                       §dpd3                4
091100000118     O                                            6 '";'
091200000118      * 8 caratteri numero spedizione
091300000118     O          E            LV22dpd1    1
091400000128     O                                           23 'DCH;020;DCW;020;ICS;02;'
091500000118     O          E            LV22dpd1    1
091600020114     O                                           16 'HBR;030;VBR;200;'
091700000118     O          E            LV22dpd1    1
091800000118     O                                            1 '"'
091900000118     O                       §dpd8                9
092000000118     O                                           11 '";'
092100000118      *ultimo carattere numero spedizione
092200000118     O          E            LV22dpd1    1
092300000128     O                                           23 'DCH;020;DCW;020;ICS;01;'
092400000118     O          E            LV22dpd1    1
092500020114     O                                           16 'HBR;030;VBR;145;'
092600000118     o          E            LV22dpd1    1
092700000118     O                                            1 '"'
092800000118     O                       §dpd12               2
092900000118     O                                            4 '";'
093000000118      *---*   costante scritta
093100000118     O          E            LV22dpd1    1
093200000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
093300000118     O          E            LV22dpd1    1
093400010718     O                                           16 'HBR;110;VBR;230;'
093500000118     O          E            LV22dpd1    1
093600000118     O                                            1 '"'
093700000118     O                       §dpdn1              52
093800000118     O                                           54 '";'
093900000118      *---*   costante scritta 2
094000000118     O          E            LV22dpd1    1
094100000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
094200000118     O          E            LV22dpd1    1
094300010718     O                                           16 'HBR;130;VBR;230;'
094400000118     O          E            LV22dpd1    1
094500000118     O                                            1 '"'
094600000118     O                       §dpdn2              50
094700000118     O                                           52 '";'
094800000118      *---*   costante scritta 3
094900000118     O          E            LV22dpd1    1
095000000118     O                                           23 'DCH;010;DCW;010;ICS;01;'
095100000118     O          E            LV22dpd1    1
095200010718     O                                           16 'HBR;150;VBR;230;'
095300000118     O          E            LV22dpd1    1
095400000118     O                                            1 '"'
095500000118     O                       §dpdn3              54
095600000118     O                                           56 '";'
095700000118      *---*   costante barra sopra
095800000118     O          E            LV22dpd1    1
095900010718     O                                           16 'HBR;270;VBR;050;'
096000000118     O          E            LV22dpd1    1
096100010718     O                                           18 'VLT;2;DVL;0;0;155;'
096200000118      *---*   costante barra sotto
096300000118     O          E            LV22dpd1    1
096400010718     O                                           16 'HBR;470;VBR;050;'
096500000118     O          E            LV22dpd1    1
096600010718     O                                           18 'VLT;2;DVL;0;0;155;'
096700000118     o* secondo codice a barre
096800000118     O          e            lv22dpd1    1
096900020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
097000020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
097100020114     O          e            lv22dpd1    1
097200020114     O                                            9 'BCSH;200;'
097300000118     O          E            LV22dpd1    1
097400010718     O                                           16 'HBR;270;VBR;050;'
097500000118     o          E            LV22dpd1    1
097600000118     O                                            5 'BCST;'
097700000118     O                                            6 '"'
097800000118     O                       §deawb              16
097900000118     O                                           18 '";'
098000000118     O                                           23 'BSTP;'
098100000118      *costante 3 v1/00
098200000118     O          E            LV22dpd1    1
098300000118     O                                           23 'DCH;040;DCW;040;ICS;02;'
098400000118     O          E            LV22dpd1    1
098500010718     O                                           16 'HBR;190;VBR;180;'
098600000118     o          E            LV22dpd1    1
098700000118     O                                            1 '"'
098800000124     O                       §dex1                6
098900000124     O                                            8 '";'
099000000118      *decodifica 2° bcc
099100000118     O          E            LV22dpd1    1
099200000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
099300000118     O          E            LV22dpd1    1
099400010718     O                                           16 'HBR;480;VBR;200;'
099500000118     o          E            LV22dpd1    1
099600000118     O                                            1 '"'
099700000118     O                       §dpdcap             14
099800000118     O                                           16 '";'
099900000118      *costante x
100000000118     O          E            LV22dpd1    1
100100000118     O                                           23 'DCH;050;DCW;030;ICS;02;'
100200000118     O          E            LV22dpd1    1
100300010718     O                                           16 'HBR;470;VBR;260;'
100400000118     o          E            LV22dpd1    1
100500000118     O                                            1 '"'
100600000118     O                       §desse               2
100700000118     O                                            4 '";'
100800000118      *costante 09
100900000118     O          E            LV22dpd1    1
101000000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
101100000118     O          E            LV22dpd1    1
101200020111     O                                           16 'HBR;570;VBR;260;'
101300000118     o          E            LV22dpd1    1
101400000118     O                                            1 '"'
101500000118     O                       dpd9                 3
101600000118     O                                            5 '";'
101700000118      *linea di arrivo
101800000118     O          E            LV22dpd1    1
101900000118     O                                           23 'DCH;080;DCW;040;ICS;02;'
102000000118     O          E            LV22dpd1    1
102100010718     O                                           16 'HBR;550;VBR;200;'
102200000118     o          E            LV22dpd1    1
102300000118     O                                            1 '"'
102400000124     O                       §dex3                4
102500000118     O                                            6 '";'
102600000118      *zona arrivo
102700000118     O          E            LV22dpd1    1
102800000118     O                                           23 'DCH;060;DCW;030;ICS;02;'
102900000118     O          E            LV22dpd1    1
103000010718     O                                           16 'HBR;570;VBR;130;'
103100000118     o          E            LV22dpd1    1
103200000118     O                                            1 '"'
103300000124     O                       §dex4                3
103400000118     O                                            5 '";'
103500000118     O*------------------------------------------------------------------
103600000118     O          E            LV22dpd2         01
103700000118     O                                           20 '&&??%%P%%7E%^"LV22";'
103800000118     O          E            LV22dpd2    1
103900000118     O*LOGICA                                N.ET. V (H) O=-35 X TB340
104000020131     O*                                          16 '00001;0;265;-17;'
104100020131     O                                           16 '00001;0;265;-70;'
104200000118     O          E            LV22dpd2    1
104300000118     O*FISICA
104400010718     O                                           14 'UTOF;2700;SPB;'
104500000118     O          E            LV22dpd2    1
104600000118     O*GIRO SCRITTA A 180°
104700010719     O                                            9 'FLIP;300;'
104800000118     O          E            LV22dpd2    1
104900010718     O                                            9 'DHR;$201;'
105000000118     O          E            LV22dpd2    1
105100000118     O                                           14 'HLT;01;VLT;01;'
105200000118      *---*   numero  spedizione
105300000118     O          E            LV22dpd2    1
105400000118     O                                           23 'DCH;050;DCW;030;ICS;03;'
105500000118     O          E            LV22dpd2    1
105600010718     O                                           16 'HBR;110;VBR;240;'
105700000118     O          E            LV22dpd2    1
105800000118     O                                            1 '"'
105900000118     O                       §dpd3                4
106000000118     O                                            6 '";'
106100000118      * 8 caratteri numero spedizione
106200000118     O          E            LV22dpd2    1
106300010718     O                                           23 'DCH;040;DCW;030;ICS;02;'
106400000118     O          E            LV22dpd2    1
106500010718     O                                           16 'HBR;120;VBR;200;'
106600000118     O          E            LV22dpd2    1
106700000118     O                                            1 '"'
106800000118     O                       §dpd8                9
106900000118     O                                           11 '";'
107000000118      *ultimo carattere numero spedizione
107100000118     O          E            LV22dpd2    1
107200000118     O                                           23 'DCH;035;DCW;020;ICS;01;'
107300000118     O          E            LV22dpd2    1
107400010718     O                                           16 'HBR;130;VBR;120;'
107500000118     o          E            LV22dpd2    1
107600000118     O                                            1 '"'
107700000118     O                       §dpd12               2
107800000118     O                                            4 '";'
107900000118      *---*   costante TELEFONO
108000000118     O          E            LV22dpd2    1
108100000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
108200000118     O          E            LV22dpd2    1
108300020114     O                                           16 'HBR;020;VBR;230;'
108400000118     O          E            LV22dpd2    1
108500000118     O                                            1 '"'
108600000118     O                       dpd2                20
108700000118     O                                           22 '";'
108800000118      *---*   costante fax
108900000118     O          E            LV22dpd2    1
109000000118     O                                           23 'DCH;012;DCW;015;ICS;01;'
109100000118     O          E            LV22dpd2    1
109200020114     O                                           16 'HBR;040;VBR;230;'
109300000118     O          E            LV22dpd2    1
109400000118     O                                            1 '"'
109500000118     O                       dpd2f               20
109600000118     O                                           22 '";'
109700000118      *costante mittente
109800000118     O          E            LV22dpd2    1
109900000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
110000000118     O          E            LV22dpd2    1
110100020111     O                                           25 'HBR;180;VBR;260;'
110200000118     o          E            LV22dpd2    1
110300000118     O                                            1 '"'
110400000118     O                       dpd3                 8
110500000118     O                                           10 '";'
110600000118      *costante mittente2
110700000118     O          E            LV22dpd2    1
110800000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
110900000118     O          E            LV22dpd2    1
111000020111     O                                           25 'HBR;210;VBR;260;'
111100000118     o          E            LV22dpd2    1
111200000118     O                                            1 '"'
111300000118     O                       §ldrsm              36
111400000118     O                                           38 '";'
111500000118      *costante mittente 3
111600000118     O          E            LV22dpd2    1
111700000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
111800000118     O          E            LV22dpd2    1
111900020111     O                                           25 'HBR;240;VBR;260;'
112000000118     o          E            LV22dpd2    1
112100000118     O                                            1 '"'
112200000118     O                       §ldprm               3
112300000118     O                                            5 '";'
112400000118      *costante destinatario
112500000118     O          E            LV22dpd2    1
112600000118     O                                           23 'DCH;015;DCW;015;ICS;01;'
112700000118     O          E            LV22dpd2    1
112800020111     O                                           25 'HBR;275;VBR;260;'
112900000118     o          E            LV22dpd2    1
113000000118     O                                            1 '"'
113100000118     O                       dpd4                11
113200000118     O                                           13 '";'
113300000118      *destinatario
113400000118     O          E            LV22dpd2    1
113500000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
113600000118     O          E            LV22dpd2    1
113700020111     O                                           20 'HBR;300;VBR;260;'
113800000118     o          E            LV22dpd2    1
113900000118     O                                            1 '"'
114000000118     O                       §ldrsd              36
114100000118     O                                           38 '";'
114200000118      * destinatario indirizzo
114300000118     O          E            LV22dpd2    1
114400000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
114500000118     O          E            LV22dpd2    1
114600020111     O                                           20 'HBR;330;VBR;260;'
114700000118     o          E            LV22dpd2    1
114800000118     O                                            1 '"'
114900000118     O                       §ldind              36
115000000118     O                                           38 '";'
115100000118      *località destinatario 7
115200000118     O          E            LV22dpd2    1
115300000118     O                                           23 'DCH;020;DCW;020;ICS;02;'
115400000118     O          E            LV22dpd2    1
115500020111     O                                           20 'HBR;360;VBR;260;'
115600000118     o          E            LV22dpd2    1
115700000118     O                                            1 '"'
115800000118     O                       §ldlod              36
115900000118     O                                           38 '";'
116000000118      *cap      destinatario 8
116100000118     O          E            LV22dpd2    1
116200000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
116300000118     O          E            LV22dpd2    1
116400020111     O                                           20 'HBR;400;VBR;260;'
116500000118     o          E            LV22dpd2    1
116600000118     O                                            1 '"'
116700000118     O                       §ldcad              10
116800000118     O                                           12 '";'
116900000118      *cap      destinatario 8
117000000118     O          E            LV22dpd2    1
117100000118     O                                           23 'DCH;030;DCW;030;ICS;03;'
117200000118     O          E            LV22dpd2    1
117300010718     O                                           20 'HBR;400;VBR;180;'
117400000118     o          E            LV22dpd2    1
117500000118     O                                            1 '"'
117600000118     O                       §desia               4
117700000118     O                                            6 '";'
117800000118      *---*   costante barra sopra
117900000118     O          E            LV22dpd2    1
118000010718     O                                           16 'HBR;450;VBR;050;'
118100000118     O          E            LV22dpd2    1
118200010718     O                                           18 'VLT;2;DVL;0;0;185;'
118300000118      *---*   costante barra sotto
118400000118     O          E            LV22dpd2    1
118500010718     O                                           16 'HBR;650;VBR;050;'
118600000118     O          E            LV22dpd2    1
118700010718     O                                           18 'VLT;2;DVL;0;0;185;'
118800000118      *costante primo B-C
118900000118     O          e            lv22dpd2    1
119000020114     O*                                          24 'BSYM;8;4;BNEW;4;BWEW;08;'
119100020114     O                                           22 'BDEF;9;BNEW;4;BWEW;08;'
119200020114     O          e            lv22dpd2    1
119300020114     O                                            9 'BCSH;200;'
119400000118     O          E            LV22dpd2    1
119500010718     O                                           16 'HBR;450;VBR;050;'
119600000118     o          E            LV22dpd2    1
119700000118     O                                            5 'BCST;'
119800000118     O                                            6 '"'
119900000118     O                       §dpd                18
120000000118     O                                           20 '";'
120100000118     O                                           25 'BSTP;'
120200000118      *costante data
120300000118     O          E            LV22dpd2    1
120400000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
120500000118     O          E            LV22dpd2    1
120600020111     O                                           16 'HBR;670;VBR;260;'
120700000118     o          E            LV22dpd2    1
120800000118     O                                            1 '"'
120900000118     O                       datasp              11 '  /  /    '
121000000118     O                                           13 '";'
121100000118      *costante 1 collo
121200000118     O          E            LV22dpd2    1
121300000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
121400000118     O          E            LV22dpd2    1
121500010718     O                                           16 'HBR;670;VBR;200;'
121600000118     o          E            LV22dpd2    1
121700000118     O                                            1 '"'
121800000118     O                                            8 '1 Collo'
121900000118     O                                           10 '";'
122000000118      *costante peso
122100000118     O          E            LV22dpd2    1
122200000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
122300000118     O          E            LV22dpd2    1
122400010718     O                                           16 'HBR;670;VBR;150;'
122500000118     o          E            LV22dpd2    1
122600000118     O                                            1 '"'
122700000118     O                                            8 'Peso Kg'
122800000118     O                                           10 '";'
122900000118      * peso
123000000118     O          E            LV22dpd2    1
123100000118     O                                           23 'DCH;020;DCW;020;ICS;01;'
123200000118     O          E            LV22dpd2    1
123300010718     O                                           16 'HBR;670;VBR;100;'
123400000118     o          E            LV22dpd2    1
123500000118     O                                            1 '"'
123600000118     O                       §ldpkg        2     10
123700000118     O                                           12 '";'
123800000118      *------------------------------------------------------------------
123900000118      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
124000000118     O* costante dpd
124100000118     O          E            LV22dpd     1
124200000118     O                                           23 'DCH;030;DCW;070;ICS;03;'
124300000118     O          E            LV22dpd     1
124400020114     O                                           25 'HBR;160;VBR;280;DHR;$401;'
124500000118     o          E            LV22dpd     1
124600000118     O                                            1 '"'
124700000118     O                       dpd                  4
124800000118     O                                            6 '";'
124900000118     O* costante dpd riga sotto
125000000118     O          E            LV22dpd     1
125100000118     O                                           23 'DCH;006;DCW;005;ICS;01;'
125200000118     O          E            LV22dpd     1
125300010718     O                                           20 'HBR;160;VBR;240;'
125400000118     o          E            LV22dpd     1
125500000118     O                                            1 '"'
125600000118     O                       dpd1                23
125700000118     O                                           25 '";'
125800000118     o          E            LV22dpd     1
125900000118     O                                            9 'TRM;%%5C%'
