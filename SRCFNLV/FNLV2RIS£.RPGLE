000100001222     H DECEDIT('0,') DATEDIT(*DMY.)
000200010926     F* FNLV2RIS *---------------------------------------------------*
000300910403     F*-----------------*                                            *
000400950711     F* STAMPA SEGNACOLLO CON BARCODE A 18 CON ITALORA               *
000500921120     F*        CON BARCODE 3X70                                      *
000600910403     F*--------------------------------------------------------------*
000700941216     FFNLV22P   O    F  132        PRINTER
000800910404     D §RI             S              1    DIM(37)
000900010720     D §RIx            S              1    DIM(11)
001000950706     D §EE             S              1    DIM(12)
001100910403     D §O7             S              1    DIM(7)
001200980318     D §S9             S              1    DIM(9)
001300980318     D §O9             S              1    DIM(9)
001400980327     D §EU             S              1    DIM(35)
001500910403     D                 DS
001600910403     D  §CB                    1     18  0
001700910403     D                                     DIM(18)
001800910403     D  §CA                    1     18
001900910403     D                                     DIM(18)
002000910403     D                 DS
002100910403     D  §N7                    1      7  0
002200910403     D  §S7                    1      7
002300910403     D                                     DIM(7)
002400010720     d  §s4                    1      4
002500010720     d  §s3                    5      7
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
003600011119     D §bceurex        DS
003700011119     D  alfa5                         5
003800011119     D  nume12                       12  0
003900011119     D  alfa2                         2
004000010720     D §dpd            DS
004100010720     D  §dpd3                         3
004200010720     D  §dpd8                         8
004300010720     D  §dpd12                        1
004400010720     D §deawbds        DS
004500010720     D  §dpdc1w                       2
004600010720     D  §dpdc3w                       5
004700010720     D  §dpdc5w                       2
004800010720     D  §dpdc7w                       1
004900010720     D §dexx1ds        DS
005000010720     D  §dex1                         5
005100010720     D  §dex2                         5
005200010720     D  §dex3                         3
005300010720     D  §dex4                         2
005400010720     D §dpdcap         DS
005500010720     D  §dpdc1                        2
005600010720     D  §dpdc2                        1    inz('-')
005700010720     D  §dpdc3                        5
005800010720     D  §dpdc4                        1    inz('-')
005900010720     D  §dpdc5                        2
006000010720     D  §dpdc6                        1    inz('-')
006100010720     D  §dpdc7                        1
006200010720     d §dpdn           ds
006300010725     d §dpdn1                        38
006400010725     d §dpdn2                        38
006500010725     d §dpdn3                        38
006600010725     d §dpdn4                        39
006700010720     d dpd             c                   const('DPD')
006800010720     d dpd1            c                   const('Deutscher Paket Dienst')
006900010720     d dpd3            c                   const('Sender:')
007000010720     d dpd4            c                   const('Receiver:')
007100010720     d dpd2            ds
007200010720     d  dpdtel                        3    inz('Tel')
007300010720     d  dpdteln                      15
007400010720     d dpd2f           ds
007500010720     d  dpdfax                        3    inz('Fax')
007600010720     d  dpdfaxn                      15
007700010720     d dpd9            c                   const('01')
007800010720     d dspioppa        ds
007900010720     d  operat                        1    inz
008000010720     d  pista                         1    inz
008100010720     d  pallet                        2    inz
008200910403     I/SPACE 3
008300910403     C*---------------------------------------------------------------*
008400910403     C     *ENTRY        PLIST
008500910403     C                   PARM                    DSBLD
008600971215     C                   PARM                    DSBLDE
008700010720      *data
008800010720     c                   move      §ldmgs        giorno            2 0
008900010720     c                   movel     §ldmgs        mese              2 0
009000010720     c                   move      mese          giomes            4 0
009100010720     c                   movel     giorno        giomes
009200010720     c                   movel     giomes        datasp            8 0
009300010720     c                   move      §ldaas        datasp
009400050415      *
009500050415      * flag ristampa: " ", "R", "M", "V", "X", "W"
009600050415     c                   clear                   wLDrst            1
009700050415     c                   clear                   wLDrst2           1
009800050415     c                   select
009900050415      *
010000050415     c                   when      §LDrst  = *blanks
010100050415      *
010200050415     c                   when      §LDrst  = 'V'
010300050415     c                   eval      wLDrst2 = §LDrst
010400050415      *
010500050415     c                   when      §LDrst  = 'X'
010600050415     c                   eval      wLDrst  = 'R'
010700050415     c                   eval      wLDrst2 = 'V'
010800050415      *
010900050415     c                   when      §LDrst  = 'W'
011000050415     c                   eval      wLDrst  = 'M'
011100050415     c                   eval      wLDrst2 = 'V'
011200050415      *
011300050415     c                   other
011400050415     c                   eval      wLDrst  = §LDrst
011500050415      *
011600050415     c                   endsl
011700050415      *
011800941228     C*
011900921202     C                   MOVEL     §LDNRS        §LDNRT            3
012000921202     C                   MOVE      '-'           §LDNRT
012100971222     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
012200971222     C* SE NON PASSO DA ROUTINE DI STAMPA ITALIA
012300971222     C     §DETST        IFEQ      'EST'
012400971222     C                   EXSR      COLZER
012500971222     C                   ENDIF
012600980318     C*
012700980318     C* STAMPA ETICHETTA ESTERO EUROEXPRESS O EUROPOLITAN SE AWB <> ' '
012800980318     C*
012900971222     C     §DETST        IFEQ      'EST'
013000971222     C     §DETST        OREQ      'ALL'
013100000128     c                   select
013200000128     c     §detse        wheneq    'REX'
013300010719     c*                  except    lv22e
013400000128     c     §detse        wheneq    'EUR'
013500000128     c                   exsr      eurexp
013600010723     c     §detse        wheneq    'DP1'
013700010726     c                   exsr      dpddove
013800010723     c     §detse        wheneq    'DP2'
013900010726     c                   exsr      dpddove
014000010723     c                   exsr      dpdchi
014100000128     c                   endsl
014200971222     C                   ENDIF
014300010723     C     §DETST        IFEQ      'ITA'
014400010723     C     §DETST        OREQ      'ALL'
014500010726     C                   EXSR      PRINT
014600010723     C                   ENDIF
014700971222     C*
014800971222     C                   SETON                                        LR
014900971222     C/SPACE 005
015000971222     C*****************************************************
015100971222     C* IMPOSTAZIONE E STAMPA                           ***
015200971222     C*****************************************************
015300971222     C     PRINT         BEGSR
015400910403     C                   MOVE      §LDLNP        §LCLNP
015500910403     C                   MOVE      §LDLNA        §LCLNA
015600910403     C                   MOVE      §LDNRS        §LCNRS
015700910403     C                   MOVE      §LDNCD        §LCNCD
015800910403     C                   MOVE      §LDZSC        §LCZSC
015900040525     C** non cancellare  MOVEl     §LDdla        dspioppa
016000040428     c     §detse        comp      'EEX'                                  55
016100960601     C*
016200960601     C* TOLGO IL CARATTERE ° IN QUANTO BLOCCA LA STAMPA
016300960601     C     '°':' '       XLATE     §LDRSM        §LDRSM
016400960601     C     '°':' '       XLATE     §LDRSD        §LDRSD
016500960601     C     '°':' '       XLATE     §LDLOD        §LDLOD
016600960601     C     '°':' '       XLATE     §LDIND        §LDIND
016700960601     C     '°':' '       XLATE     §LDCDP        §LDCDP
016800050606     C*
016900050606     C* TOLGO IL CARATTERE \ IN QUANTO BLOCCA LA STAMPA
017000050606     C     '\':' '       XLATE     §LDRSM        §LDRSM
017100050606     C     '\':' '       XLATE     §LDRSD        §LDRSD
017200050606     C     '\':' '       XLATE     §LDLOD        §LDLOD
017300050606     C     '\':' '       XLATE     §LDIND        §LDIND
017400050606     C     '\':' '       XLATE     §LDCDP        §LDCDP
017500910403     C*
017600910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
017700910403     C                   EXSR      M10
017800910403     C                   MOVEA     §CA           §C18A            18
017900910403     C                   MOVE      §C18A         §CDBAR           18
018000970210     C*
018100941216     C     §LDTSP        IFEQ      'D'
018200970303     C     §LDLNA        ANDLT     300
018300941216     C                   MOVE      *BLANK        §LDTSP
018400941216     C                   END
018500941216     C*
018600950705     C     §LDLNA        COMP      300                                15
018700941216     C     §LDTSP        COMP      *BLANK                             1616
018800910403     C     §LDTSP        COMP      'E'                                    17
018900910403     C     §LDCDP        COMP      *BLANK                             18
019000910403     C     §LDNRS        COMP      *ZERO                              19
019100910403     C*
019200921106     C** INIZIALIZZA STAMPANTE
019300941216     C                   EXCEPT    LV22
019400910403     C*
019500910404     C                   MOVEA     *BLANK        §RI
019600910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL PRIMO NUMERO COLLI
019700910403     C                   SETOFF                                       77
019800910403     C                   MOVE      *BLANK        §A7               7
019900910403     C                   Z-ADD     §LDNC1        §N7
020000011114     C                   DO        7             §1
020100011114     C     §S7(§1)       IFEQ      *ZERO
020200011114     C                   MOVE      *BLANK        §S7(§1)
020300011114     C                   ELSE
020400011114     C                   SETON                                        77
020500011114     C                   END
020600011114     C  N77              END
020700910403     C                   MOVEA     §S7           §A7
020800910403     C                   MOVE      §A7           §A3               3
020900910403     C                   MOVEA     §A3           §RI(26)
021000910403     C                   MOVEA     '-'           §RI(29)
021100910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL ULTIMO NUMERO COLLI
021200910403     C                   SETOFF                                       77
021300910403     C                   MOVE      *BLANK        §A7
021400910403     C                   Z-ADD     §LDNC2        §N7
021500011114     C                   DO        7             §1
021600011114     C     §S7(§1)       IFEQ      *ZERO
021700011114     C                   MOVE      *BLANK        §S7(§1)
021800011114     C                   ELSE
021900011114     C                   SETON                                        77
022000011114     C                   END
022100011114     C  N77              END
022200910403     C                   MOVEA     §S7           §A7
022300910403     C                   MOVE      §A7           §A3
022400910403     C                   MOVEA     §A3           §RI(30)
022500910403     C                   MOVE      §LDCPD        §A3
022600910404     C                   MOVEA     §A3           §RI(34)
022700010720     C                   MOVEA     §ri(27)       §RIx
022800910403     C* STAMPA LINEA PARTENZA
022900011113     C                   EXCEPT    LV22H
023000910403     C*
023100910404     C                   MOVEA     *BLANK        §RI
023200011114     C                   MOVEA     §LDRSM        §RI(1)
023300011114     C                   MOVEA     ' '           §RI(23)
023400011114     C                   MOVEA     §LDPRM        §RI(24)
023500011114     C                   MOVEA     *BLANK        §RI(26)
023600011113     C                   EXCEPT    LV22D1
023700910403     C* STAMPA DATI MITTENTE
023800910403     C*
023900910403     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
024000971222     C                   EXSR      COLZER
024100910403     C*
024200971223     C*
024300910403     C* STAMPA NUMERO SEGNACOLLO
024400011113     C                   EXCEPT    LV22D2
024500910403     C*
024600921106     C                   MOVE      §LDTNA        §A3               3
024700921106     C     '0':'O'       XLATE     §A3           §ODTNA            3
024800921106     C                   MOVE      §LDLNA        §A3
024900921106     C     '0':'O'       XLATE     §A3           §ODLNA            3
025000921106     C                   MOVE      §LDZSC        §A2
025100921106     C     '0':'O'       XLATE     §A2           §ODZSC            2
025200921106     C* DECODIFICA LINEA DI ARRIVO DA 10 A 8
025300921106     C                   MOVEL     §LDDLA        §ODDLA            8
025400910403     C* STAMPA LINEA ARRIVO
025500950705     C* X 15 STAMPA A "MODOD ESTERO"
025600950705     C   15              DO
025700950705     C                   MOVEA     *BLANK        §EE
025800950705     C                   MOVEA     §ODDLA        §EE(1)
025900950706     C                   MOVEA     '  '          §EE(5)
026000950706     C                   MOVEL     §LDCAD        §CA2              2
026100950706     C                   MOVE      §LDCAD        §CA7              7
026200950706     C                   MOVEA     §CA2          §EE(7)
026300950706     C                   MOVEA     §CA7          §EE(10)
026400950705     C                   END
026500011113     C                   EXCEPT    LV22D3
026600910403     C*
026700910404     C                   MOVEA     *BLANK        §RI
026800011114     C                   MOVEA     §LDRSD        §RI(1)
026900011114     C                   MOVEA     ' '           §RI(23)
027000011114     C                   MOVEA     §LDPRD        §RI(24)
027100011114     C                   MOVEA     *BLANK        §RI(26)
027200910403     C* STAMPA 1° DATI DESTINATARIO
027300011113     C                   EXCEPT    LV22D4
027400910403     C*
027500910404     C                   MOVEA     *BLANK        §RI
027600011114     C                   MOVEA     §LDIND        §RI(1)
027700910404     C* TROVO L'ULTIMO CARATTERE DI §RI UTILIZZATO DALL'INDIRIZZO
027800011114     C                   Z-ADD     29            §Y                2 0    88
027900011114     C                   DO        28
028000910404     C                   SUB       1             §Y
028100910404     C     §RI(§Y)       IFNE      ' '
028200910404     C                   SETON                                        88
028300910404     C                   END
028400910404     C  N88              END
028500011114     C     §Y            IFLE      20
028600910404     C                   ADD       3             §Y
028700910404     C                   ELSE
028800011114     C                   MOVEA     '  '          §RI(19)
028900011114     C                   Z-ADD     20            §Y
029000910404     C                   END
029100910404     C                   MOVEA     §LDLOD        §RI(§Y)
029200011114     C                   MOVEA     *BLANK        §RI(26)
029300910403     C* STAMPA 2° DATI DESTINATARIO
029400011113     C                   EXCEPT    LV22D5
029500941216     C* STAMPA SERVIZIO
029600011113     C   16              EXCEPT    LV22X5
029700910403     C*
029800910403     C* STAMPA BARCODE
029900920720     C*
030000011113     C                   EXCEPT    LV22D6
030100910403     C*
030200910410     C                   MOVEA     *BLANK        §RI
030300910403     C   18              DO
030400941216     C                   MOVEA     'Rif.:'       §RI(1)
030500980402     C     §DEEUR        IFNE      *BLANK
030600980402     C     §DETST        ANDEQ     'ITA'
030700980402     C                   MOVEA     'JUT :'       §RI(1)
030800980402     C                   ENDIF
030900941216     C                   MOVEA     §LDCDP        §RI(7)
031000910410     C                   END
031100941216     C* STAMPA RIFERIMENTO/RISTAMPA
031200011113     C                   EXCEPT    LV22D7
031300910403     C*
031400910403     C* STAMPA CHIUSURA ETICHETTA
031500011113     C                   EXCEPT    LV22D8
031600971224     C                   ENDSR
031700971224     C/SPACE 005
031800971224     C*****************************************************
031900971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032000971224     C*****************************************************
032100971224     C     COLZER        BEGSR
032200971224     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL SEGNACOLLO
032300971224     C                   SETOFF                                       77
032400971224     C                   MOVE      §LDNCD        §N7
032500011114     C                   DO        7             §1
032600011114     C     §S7(§1)       IFEQ      *ZERO
032700011114     C                   MOVE      *BLANK        §S7(§1)
032800011114     C                   ELSE
032900011114     C                   SETON                                        77
033000011114     C                   END
033100011114     C  N77              END
033200971224     C* SOSTITUISCO GLI ALTRI ZERI CON -O- E SPOSTO A SX LE CIFRE
033300971224     C                   MOVE      *ZERO         §X                1 0
033400971224     C                   MOVEA     *BLANK        §O7
033500971224     C                   DO        7             §1
033600011113     C*    §S7(§1)       IFNE      *BLANK
033700971224     C                   ADD       1             §X
033800971224     C     §S7(§1)       IFEQ      '0'
033900011114     C     §S7(§1)       oreq      *blank
034000971224     C                   MOVE      'O'           §O7(§X)
034100971224     C                   ELSE
034200971224     C                   MOVE      §S7(§1)       §O7(§X)
034300971224     C                   END
034400011113     C*                  END
034500971224     C                   END
034600971224     C                   MOVEA     §O7           §S7
034700971224     C                   ENDSR
034800971222     C/SPACE 005
034900980318     C*****************************************************
035000980318     C* STAMPA EUROEXPRESS                              ***
035100980318     C*****************************************************
035200980318     C     EUREXP        BEGSR
035300980327     C*
035400980327     C* IMPOSTO COSTANTI
035500980327     C                   MOVEL     'JUT'         §DEJEU            3
035600980327     C                   MOVEL     '55L'         §DE55L            3
035700980327     C                   MOVEL     'IT'          §DEISP            2
035800010926      * controllo se contenuto 55L nel codice passato
035900010926     c     '55L'         scan      §deeur                                 23
036000011119     c
036100011119      * se contenuto 55l scompongo a modo vecchio
036200011119     c                   if        *in23
036300980327     C* ESTRAGGO IL CAP:
036400980327     C                   MOVE      *ZEROS        §Y
036500980327     C                   MOVEA     §DEEUR        §EU
036600980327     C* TROVO QUANTO è LUNGO IL BARCODE
036700980327     C     ' '           CHECKR    §DEEUR        §Y
036800980327     C* ISOLO IL CAP
036900980327     C                   MOVE      *BLANK        §O9
037000980327     C                   MOVE      *ZEROS        §W                2 0
037100980327     C     §Y            SUB       2             §Y
037200980327     C     22            DO        §Y            §X1               2 0
037300980327     C     §EU(§X1)      IFNE      *BLANKS
037400980327     C                   ADD       1             §W
037500980327     C                   MOVE      §EU(§X1)      §O9(§W)
037600980327     C                   END
037700980327     C                   END
037800980318     C*
037900980318     C                   MOVEA     §O9           §LDCAD
038000011119
038100011119      *scompongo a modo nuovo senza 55l
038200011119     c                   else
038300011119     c                   movel     §deeur        §bceurex
038400011119     c                   end
038500980318     C*
038600980318     C* COSTRUISCO LA PARTE TERMINALE DEL COMANDO PER LA STAMPA
038700980318     C* DEL BARCODE
038800980319     C     §LDCAD        CAT       §DEISA:0      §BCEND           11
038900980318     C*
039000980318     C* VERIFICO SE CONTRASSEGNO
039100980318     C*
039200980318     C     §DEFCA        COMP      'S'                                    14
039300980318     C  N14§DEFCA        COMP      'X'                                    14
039400980318     C*
039500980318     C                   EXCEPT    LV22Y
039600980318     C*
039700980318     C                   ENDSR
039800910403     C/SPACE 005
039900910403     C*****************************************************
040000910403     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
040100910403     C*****************************************************
040200910403     C     M10           BEGSR
040300910403     C                   MOVE      *ZERO         §TT               3 0
040400910403     C                   MOVE      *ZERO         §TT2              3 0
040500910403     C                   MOVE      *ZERO         §1                1 0
040600910403     C                   Z-ADD     18            §S                2 0
040700910403     C                   DO        17            §N                2 0
040800910403     C                   SUB       1             §S
040900910403     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
041000910403     C                   MOVE      §C(§N)        §CB(§N)
041100910403     C     §N            DIV       2             §CTR              3 0
041200910403     C                   MVR                     §RST              3 010
041300910403     C     *IN(10)       IFEQ      '1'
041400910403     C     §C(§S)        MULT      2             §N2               2 0
041500910403     C                   MOVE      §N2           §A2               2
041600910403     C                   MOVEA     §A2           §2A
041700910403     C                   XFOOT     §2            §CTR
041800910403     C                   ELSE
041900910403     C                   Z-ADD     §C(§S)        §CTR
042000910403     C                   END
042100910403     C                   ADD       §CTR          §TT
042200910403     C                   END
042300910403     C*
042400910403     C     §TT           ADD       9             §TT2
042500910403     C                   MOVE      0             §TT2
042600910403     C     §TT2          SUB       §TT           §1
042700910403     C*
042800910403     C                   MOVE      §1            §CB(18)
042900910403     C                   ENDSR
043000010723      *--------------------------------------------------
043100010723     c     dpddove       begsr
043200010723      *--------------------------------------------------
043300010723     c                   eval      §dpd = §deeur
043400010723     c                   eval      §dexx1ds = §dexx1
043500010723     c                   eval      §deawbds = §deawb
043600010723     c                   eval      §dpdc1 = §dpdc1w
043700010723     c                   eval      §dpdc3 = §dpdc3w
043800010723     c                   eval      §dpdc5 = §dpdc5w
043900010723     c                   eval      §dpdc7 = §dpdc7w
044000010723     c                   eval      §dpdn = 'Notifications on damages which are -
044100010723     c                             not recognizable from the outside have to -
044200010723     c                             have to be submitted to the transport -
044300010723     c                             organisation withing 7 days in written.'
044400010723     C                   except    LV22dpd1
044500010723     C                   except    LV22dpd
044600010723     C                   endsr
044700010723      *--------------------------------------------------
044800010723     c     dpdchi        begsr
044900010723      *--------------------------------------------------
045000010723     c                   move      §ldmgs        giorno            2 0
045100010723     c                   movel     §ldmgs        mese              2 0
045200010723     c                   move      mese          giomes            4 0
045300010723     c                   movel     giorno        giomes
045400010723     c                   movel     giomes        datasp            8 0
045500010723     c                   move      §ldaas        datasp
045600010723     c                   movel     §dexx2        dpdteln
045700010723     c                   move      §dexx2        dpdfaxn
045800010723     C                   except    LV22dpd2
045900010723     C                   except    LV22dpd
046000010723     C                   endsr
046100010719     c*-----------------------------------------------------------
046200971215     OFNLV22P   E            LV22             01
046300950712     O******                                 Inizializzazioni
046400951010     O                                            7 '?07&120'
046500950712     O          E            LV22        1
046600011113     O                                            5 '?13&2'
046700950712     O          E            LV22        1
046800950713     O                                            7 '?06&-90'
046900910403     O********* Mittente
047000941216     O          E            LV22D1      1
047100041215     O                                           26 '?02&02,650,040,3,1,22,8,1;'
047200010926     O                       §RI                 63
047300910403     O********* Destinatario (Rag.Soc.)
047400941216     O          E            LV22D4      1
047500041215     O                                           18 '?15&677,40,400,0,3'
047600011113     O          E            LV22D4      1
047700041215     O                                           22 '?02&02,685,040,3,1,22,'
047800011113     O                                           26 '8,1;'
047900011113     O                       §RI                 63
048000910403     O********* Destinatario (Località)
048100011113     O          E            LV22D5      1
048200041215     O                                           22 '?02&02,705,040,3,1,22,'
048300011113     O                                           26 '8,1;'
048400011113     O                       §RI                 63
048500920221     O********* MC. SPEDIZIONE
048600011113     O          E            LV22D5      1
048700041215     O                                           22 '?02&02,730,260,3,1,22,'
048800011113     O                                           26 '8,1;'
048900011113     O                                           32 'MC.'
049000011113     O                       §LDVOL              39 '0 ,   '
049100941216     O********* SERVIZIO
049200941216     O          E            LV22X5      1
049300041215     O                                           22 '?02&12,085,240,4,1,12,'
049400010720     O                                           26 '3,1;'
049500010720     O                       §LDTSP              28
049600950712     O          E            LV22X5      1
049700950714     O* FONDO NERO
049800041215     O                     17                    20 '?22&090,245,080,75,2'
049900950714     O* FONDO BIANCO
050000041215     O                    N17                    20 '?16&100,240,085,70,3'
050100910403     O********* Linea Partenza
050200950712     O**colli/padroncino
050300011113     O          E            LV22H       1
050400041215     O                                           22 '?02&02,730,050,3,1,22,'
050500011113     O                                           26 '5,1;'
050600011113     O                       §RIx                40
050700941216     O          E            LV22H       1
050800950712     O**Linea Partenza
050900041215     O                                           22 '?02&10,100,330,3,1,33,'
051000010719     O                                           26 '5,1;'
051100010719     O                       §LDLNP              29
051200010926     O********* Serie
051300010720     O          E    19      LV22D2      1
051400041215     O                                           22 '?02&10,190,350,2,1,23,'
051500010720     O                                           26 '5,1;'
051600010720     O                       §LDNRT              29
051700010926     O********* Segnacollo
051800010926     O          E            LV22D2      1
051900041215     O                                           22 '?02&10,280,350,2,1,23,'
052000010926     O                                           26 '5,1;'
052100010926     O                       §S4                 30
052200010720     O          E            LV22D2      1
052300041215     O                                           22 '?02&10,460,325,2,1,34,'
052400010720     O                                           26 '5,1;'
052500010720     O                       §S3                 29
052600010720     O*                      §S7                 33
052700921106     O********* TERMINAL
052800010719     O          E            LV22D3      1
052900041215     O                                           22 '?02&12,160,007,4,1,23,'
053000041215     O                                           26 '1,1;'
053100010719     O                       §ODTNA              29
053200910403     O* BOX X TERMINAL ARRIVO
053300011113     O          E            LV22D3      1
053400041215     O                                           21 '?16&155,015,110,190,2'
053500921106     O********* CODICE LINEA ARRIVO
053600010719     O          E            LV22D3      1
053700041215     O                                           22 '?02&12,360,070,3,1,45,'
053800010719     O                                           26 '3,1;'
053900010719     O                       §ODLNA              29
054000041215     O                       operat              31
054100040428      *nazione x EEX
054200040428     O          E    55      LV22d3      1
054300040428     O                       §DEISA
054400040428      * cap estero per EEX
054500040428     O          E    55      LV22d3      1
054600040428     O                       §LDCAD
054700040428      * box per EEX
054800040428     O          E    55      LV22d3      1
054900010720     O********* CODICE pista e pallet
055000010720     O          E            LV22D3      1
055100041215     O                                           22 '?02&12,360,010,3,1,45,'
055200010720     O                                           26 '3,1;'
055300041215     O                       pista               27
055400041215     O                       pallet              30
055500921106     O********* ZONA ARRIVO
055600040428     O          E   n55      LV22D3      1
055700041215     O                                           22 '?02&12,520,007,4,1,23,'
055800010719     O                                           26 '2,1;'
055900010719     O                       §ODZSC              28
056000910403     O* BOX X ZONA ARRIVO
056100040428     O          E   n55      LV22D3      1
056200041215     O                                           20 '?16&515,15,110,130,2'
056300910403     O********* Bar Code
056400010719     O          E            LV22D6      1
056500950712     O* RAPPORTO BARRA
056600011122     O                                            5 '?09&3'
056700010719     O          E            LV22D6      1
056800010719     O                                            5 '?10&1'
056900950712     O* LARGHEZZA ELEMENTO UNITARIO CODICE
057000010719     O          E            LV22D6      1
057100011122     O                                            5 '?11&3'
057200010719     O          E            LV22D6      1
057300950712     O* SOPPRESSIONE CARATTERI LEGGIBILI
057400950712     O                                            5 '?13&3'
057500001222     o* 2/5
057600001222     O*         E            LV22D6      1
057700010719     O*                                          21 '?02&13,163,457,1,225,'
057800001218     O*                                          26 '18,1;'
057900001222     o*code 128
058000001218     O          E            LV22D6      1
058100010926     O                                           22 '?02&13,200,150,14,170,'
058200001218     O                                           27 '18,1;'
058300001222     O                       §CDBAR              45
058400041214     O*** Barra chiusura superiore
058500041214     O          E            LV22D6      1
058600041215     O                                           19 '?15&196,145,382,2,8'
058700041214     O*** Barra chiusura inferiore
058800041214     O          E            LV22D6      1
058900041215     O                                           19 '?15&196,318,382,2,6'
059000910403     O********* Codice Prodotto
059100941216     O          E            LV22D7      1
059200011122     O                                           22 '?02&12,090,445,3,1,22,'
059300950712     O                                           27 '10,1;'
059400950712     O                       §RI                 64
059500920221     O********* KG. SPEDIZIONE
059600941216     O          E            LV22D7      1
059700050415     O*****.....                                 22 '?02&12,450,445,3,1,12,'
059800050415     O                                           22 '?02&12,410,445,3,1,12,'
059900950712     O                                           27 '10,1;'
060000050415     O                                           32 'KG.'
060100050415     O                       §LDPKG              40 '    0 , '
060200050415     O                       datasp              53 '  /  /    '
060300050415     O                       wLDRST              56
060400050415      ********* 2° flag ristampa "V"
060500050415     O          E            LV22D7      1
060600050415     O                                           22 '?02&12,640,445,3,1,22,'
060700050415     O                                           27 '10,1;'
060800050415     O                       wLDrst2             28
060900941216     O********* Chiusura etchetta
061000941216     O          E            LV22D8      1
061100950712     O                                            5 '?14&1'
061200951010     O          E            LV22D8      1
061300951010     O                                            4 '?00&'
061400980318     O*-------------------------------------------------------
061500980318     O* ETICHETTA ESTERO EUROEXPRESS
061600980318     O*-------------------------------------------------------
061700980318     O          E            LV22Y            01
061800980318     O******                                 Inizializzazioni
061900980318     O                                            7 '?07&120'
062000980318     O          E            LV22Y       1
062100980318     O                                            5 '?13&2'
062200980318     O          E            LV22Y       1
062300980318     O                                            7 '?06&-90'
062400980318     O**      E 1              LV22Y
062500980318     O**                                  4 '?39&'
062600980318      *---*   NAZIONE PARTENZA
062700980318     O          E            LV22Y       1
062800010720     O                                              '?02&10,200,005,2,1,12,'
062900980318     O                                              '1,1;'
063000980318     O                       §DEISP
063100980318      *---*   CONTRASSEGNO
063200980318     O          E    14      LV22Y       1
063300010720     O                                              '?02&10,610,000,2,1,23,'
063400980318     O                                              '1,1;'
063500980318     O                                              'COD'
063600980318     O          E    14      LV22Y       1
063700010720     O                                              '?16&605,000,090,130,5'
063800980318      *---*   EUROEXPRESS
063900980318     O          E            LV22Y       1
064000010720     O                                              '?02&10,310,010,5,1,12,'
064100980318     O                                              '1,1;'
064200980318     O                                              'EuroExpress'
064300980318      *---*   NAZIONE ARRIVO
064400980318     O          E            LV22Y       1
064500010720     O                                              '?02&10,200,110,2,1,23,'
064600980318     O                                              '1,1;'
064700980318     O                       §DEISA
064800980318     O          E            LV22Y       1
064900010720     O                                              '?02&10,200,100,7,1,21,'
065000980318     O                                              '1,1;'
065100980318     O                                              'Country'
065200980318     O          E            LV22Y       1
065300010720     O                                              '?16&190,095,110,150,5'
065400980318      *---*   CAP DESTINO
065500980318     O          E            LV22Y       1
065600010720     O                                              '?02&10,370,110,2,1,23,'
065700980318     O                                              '1,1;'
065800980318     O                       §LDCAD
065900980318     O          E            LV22Y       1
066000010720     O                                              '?02&10,370,100,7,1,21,'
066100980318     O                                              '1,1;'
066200980318     O                                              'Post Code'
066300980318     O          E            LV22Y       1
066400010720     O                                              '?16&360,095,110,300,5'
066500010926      *---*   BARCODE EUROEXPRESS    *in23 *off
066600010926     O          E   n23      LV22Y       1
066700010727     O                                              '?11&3'
066800010926     O          E   n23      LV22Y       1
066900010720     O                                              '?02&13,120,230,14,200,'
067000980318     O                                              '1,1;'
067100011119     O                       alfa5
067200011119     O                       nume12
067300011119     o                       alfa2
067400010926      *---*   BARCODE EUROEXPRESS con 55L  *in23 *on
067500010926     O          E    23      LV22Y       1
067600010926     O                                              '?11&2'
067700010926     O          E    23      LV22Y       1
067800010926     O                                              '?02&13,120,230,14,200,'
067900010926     O                                              '1,1;'
068000010926     O                       §DEJEU
068100010926     O                       §DEISP
068200010926     O                       §LDLNP
068300010926     O                       §LDNRS
068400011120     O                       §LDNCD
068500010926     O                                              ' '
068600010926     O                       §DE55L
068700010926     O                       §BCEND
068800980318      *---*   RIFERIMENTI BARTOLINI
068900980318     O          E            LV22Y       1
069000010720     O                                              '?02&10,120,440,2,1,11,'
069100980318     O                                              '1,1;'
069200980318     O                       §LDLNP
069300980318     O                                              ' '
069400980318     O                       §LDNRS
069500980318     O                                              ' '
069600980318     O                       §LDNCD
069700980318     O********* Chiusura etchetta
069800980318     O          E            LV22Y       1
069900980318     O                                              '?14&1'
070000980318     O          E            LV22Y       1
070100980318     O                                              '?00&'
070200010723     O*-------------------------------------------------------
070300010723     o*etichetta dpd "dove"
070400010723     O*-------------------------------------------------------
070500010723     O          E            LV22dpd1         01
070600010723     O******                                 Inizializzazioni
070700010725     O                                            7 '?07&120'
070800010723     O          E            LV22dpd1    1
070900010723     O                                            5 '?13&2'
071000010723     O          E            LV22dpd1    1
071100010723     O                                            7 '?06&-90'
071200010723      *---*   numero  spedizione
071300010723     O          E            LV22dpd1    1
071400010725     O                                           22 '?02&02,100,370,3,1,22,'
071500010725     O                                           26 '3,1;'
071600010725     O                       §dpd3               29
071700010723      * 8 caratteri numero spedizione
071800010723     O          E            LV22dpd1    1
071900010725     O                                           22 '?02&02,100,240,3,1,22,'
072000010725     O                                           26 '3,1;'
072100010725     O                       §dpd8               34
072200010723      *ultimo carattere numero spedizione
072300010723     O          E            LV22dpd1    1
072400010725     O                                           22 '?02&02,100,225,3,1,22,'
072500010725     O                                           26 '3,1;'
072600010725     O                       §dpd12              27
072700010723      *---*   costante scritta
072800010723     O          E            LV22dpd1    1
072900010726     O                                           22 '?02&02,150,050,3,1,12,'
073000010725     O                                           26 '3,1;'
073100010725     O                       §dpdn1              65
073200010723      *---*   costante scritta 2
073300010723     O          E            LV22dpd1    1
073400010726     O                                           22 '?02&02,175,050,3,1,12,'
073500010725     O                                           26 '3,1;'
073600010725     O                       §dpdn2              65
073700010723      *---*   costante scritta 3
073800010723     O          E            LV22dpd1    1
073900010726     O                                           22 '?02&02,200,050,3,1,12,'
074000010725     O                                           26 '3,1;'
074100010725     O                       §dpdn3              65
074200010725      *---*   costante scritta 4
074300010725     O          E            LV22dpd1    1
074400010726     O                                           22 '?02&02,225,050,3,1,12,'
074500010725     O                                           26 '3,1;'
074600010725     O                       §dpdn4              65
074700010723      *---*   costante barra sopra
074800010726     O          E            LV22dpd1    1
074900011122     O                                           19 '?15&328,070,313,0,4'
075000010723      *---*   costante barra sotto
075100010726     O          E            LV22dpd1    1
075200011122     O                                           19 '?15&520,070,313,0,4'
075300010723     o* secondo codice a barre
075400010723     O          e            lv22dpd1    1
075500010725     O* RAPPORTO BARRA
075600011122     O                                            5 '?09&2'
075700010725     O          E            LV22Dpd1    1
075800010725     O                                            5 '?10&1'
075900010725     O* LARGHEZZA ELEMENTO UNITARIO CODICE
076000010725     O          E            LV22Dpd1    1
076100011122     O                                            5 '?11&4'
076200010725     O          E            LV22Dpd1    1
076300010725     O* SOPPRESSIONE CARATTERI LEGGIBILI
076400010725     O                                            5 '?13&3'
076500010726     o*code 2/5
076600010725     O          E            LV22Dpd1
076700010726     O                                           21 '?02&21,332,070,1,200,'
076800010726     O                                           26 '18,1;'
076900010726     O                       §deawb              36
077000010723      *costante 3 v1/00
077100010723     O          E            LV22dpd1    1
077200010725     O                                           22 '?02&02,260,200,2,1,22,'
077300010725     O                                           26 '3,1;'
077400010725     O                       §dex1               32
077500010723      *decodifica 2° bcc
077600010723     O          E            LV22dpd1    1
077700010725     O                                           22 '?02&02,540,100,3,1,23,'
077800010725     O                                           26 '3,1;'
077900010725     O                       §dpdcap             41
078000010723      *costante x
078100010723     O          E            LV22dpd1    1
078200010726     O                                           22 '?02&02,550,400,2,1,23,'
078300010725     O                                           26 '3,1;'
078400010725     O                       §desse              28
078500010723      *costante 09
078600010723     O          E            LV22dpd1    1
078700010725     O                                           22 '?02&02,650,380,2,1,23,'
078800010725     O                                           26 '3,1;'
078900010725     O                       dpd9                29
079000010723      *linea di arrivo
079100010723     O          E            LV22dpd1    1
079200010725     O                                           22 '?02&02,630,180,2,1,34,'
079300010725     O                                           26 '3,1;'
079400010725     O                       §dex3               29
079500010723      *zona arrivo
079600010723     O          E            LV22dpd1    1
079700010725     O                                           22 '?02&02,650,050,2,1,23,'
079800010725     O                                           26 '3,1;'
079900010725     O                       §dex4               29
080000010725     O********* Chiusura etchetta
080100010726     O*         E            LV22dpd1    1
080200010726     O*                                             '?14&1'
080300010726     O*         E            LV22dpd1    1
080400010726     O*                                             '?00&'
080500010723     O*-------------------------------------------------------
080600010723     o*etichetta dpd "chi"
080700010723     O*-------------------------------------------------------
080800010723     O          E            LV22dpd2         01
080900010723     O******                                 Inizializzazioni
081000010723     O                                            7 '?07&120'
081100010723     O          E            LV22dpd2    1
081200010723     O                                            5 '?13&2'
081300010723     O          E            LV22dpd2    1
081400010723     O                                            7 '?06&-90'
081500010723      *---*   numero  spedizione
081600010723     O          E            LV22dpd2    1
081700010726     O                                           22 '?02&02,150,280,2,1,22,'
081800010726     O                                           26 '3,1;'
081900010726     O                       §dpd3               29
082000010723      * 8 caratteri numero spedizione
082100010723     O          E            LV22dpd2    1
082200010726     O                                           22 '?02&02,160,140,3,1,23,'
082300010726     O                                           26 '3,1;'
082400010726     O                       §dpd8               34
082500010723      *ultimo carattere numero spedizione
082600010723     O          E            LV22dpd2    1
082700010726     O                                           22 '?02&02,160,120,3,1,22,'
082800010726     O                                           26 '3,1;'
082900010726     O                       §dpd12              27
083000010723      *---*   costante TELEFONO
083100010723     O          E            LV22dpd2    1
083200010726     O                                           22 '?02&02,090,290,3,1,12,'
083300010726     O                                           26 '3,1;'
083400010726     O                       dpd2                45
083500010723      *---*   costante fax
083600010723     O          E            LV22dpd2    1
083700010726     O                                           22 '?02&02,115,290,3,1,12,'
083800010726     O                                           26 '3,1;'
083900010726     O                       dpd2f               45
084000010723      *costante mittente
084100010723     O          E            LV22dpd2    1
084200010726     O                                           22 '?02&02,210,300,3,1,12,'
084300010726     O                                           26 '3,1;'
084400010726     O                       dpd3                34
084500010723      *costante mittente2
084600010723     O          E            LV22dpd2    1
084700010726     O                                           22 '?02&02,240,070,7,1,12,'
084800010726     O                                           26 '3,1;'
084900010726     O                       §ldrsm              62
085000010723      *costante mittente 3
085100010723     O          E            LV22dpd2    1
085200010726     O                                           22 '?02&02,270,340,7,1,12,'
085300010726     O                                           26 '3,1;'
085400010726     O                       §ldprm              29
085500010726      *---*   costante barra sopra
085600010726     O          E            LV22dpd2    1
085700010726     O                                           19 '?15&300,050,310,0,2'
085800010723      *costante destinatario
085900010723     O          E            LV22dpd2    1
086000010726     O                                           22 '?02&02,302,290,3,1,12,'
086100010726     O                                           26 '3,1;'
086200010726     O                       dpd4                37
086300010723      *destinatario
086400010723     O          E            LV22dpd2    1
086500010726     O                                           22 '?02&02,330,010,1,1,24,'
086600010726     O                                           26 '3,1;'
086700010726     O                       §ldrsd              62
086800010723      * destinatario indirizzo
086900010723     O          E            LV22dpd2    1
087000010726     O                                           22 '?02&02,360,010,1,1,24,'
087100010726     O                                           26 '3,1;'
087200010726     O                       §ldind              62
087300010723      *località destinatario 7
087400010723     O          E            LV22dpd2    1
087500010726     O                                           22 '?02&02,390,010,1,1,24,'
087600010726     O                                           26 '3,1;'
087700010726     O                       §ldlod              62
087800010723      *cap      destinatario 8
087900010723     O          E            LV22dpd2    1
088000010726     O                                           22 '?02&02,410,280,2,1,22,'
088100010726     O                                           26 '3,1;'
088200010726     O                       §ldcad              36
088300010723      *cap      destinatario 8
088400010723     O          E            LV22dpd2    1
088500010726     O                                           22 '?02&02,410,180,2,1,22,'
088600010726     O                                           26 '3,1;'
088700010726     O                       §desia              30
088800010723      *---*   costante barra sopra
088900010723     O          E            LV22dpd2    1
089000011122     O                                           19 '?15&476,050,368,0,4'
089100010723      *---*   costante barra sotto
089200010723     O          E            LV22dpd2    1
089300011122     O                                           19 '?15&668,050,368,0,4'
089400010726     O          e            lv22dpd2    1
089500010726     O* RAPPORTO BARRA
089600011122     O                                            5 '?09&2'
089700010726     O          E            LV22Dpd2    1
089800010726     O                                            5 '?10&1'
089900010726     O* LARGHEZZA ELEMENTO UNITARIO CODICE
090000010726     O          E            LV22Dpd2    1
090100011122     O                                            5 '?11&4'
090200010726     O          E            LV22Dpd2    1
090300010726     O* SOPPRESSIONE CARATTERI LEGGIBILI
090400010726     O                                            5 '?13&3'
090500010726     o*code 2/5
090600010723      *costante primo B-C
090700010723     O          e            lv22dpd2    1
090800010727     O                                           21 '?02&21,480,050,1,200,'
090900010726     O                                           26 '18,1;'
091000010726     O                       §dpd                38
091100010723      *costante data
091200010723     O          E            LV22dpd2    1
091300010726     O                                           22 '?02&02,690,340,1,1,24,'
091400010726     O                                           26 '3,1;'
091500010726     O                       datasp              36 '  /  /    '
091600010723      *costante 1 collo
091700010723     O          E            LV22dpd2    1
091800010726     O                                           22 '?02&02,690,235,1,1,24,'
091900010726     O                                           26 '3,1;'
092000010726     O                                           33 '1 COLLO'
092100010723      *costante peso
092200010723     O          E            LV22dpd2    1
092300010726     O                                           22 '?02&02,690,130,1,1,24,'
092400010726     O                                           26 '3,1;'
092500010726     O                                           33 'PESO KG'
092600010723      * peso
092700010723     O          E            LV22dpd2    1
092800010726     O                                           22 '?02&02,690,010,1,1,24,'
092900010726     O                                           26 '3,1;'
093000010726     O                       §ldpkg        2     35
093100010723      *------------------------------------------------------------------
093200010723      *EXCPT DI CHIUSURA ETICHETTA PIÙ COSTANTE DPD
093300010723     O* costante dpd
093400010723     O          E            LV22dpd     1
093500010726     O                                           22 '?02&30,150,380,2,1,23,'
093600010726     O                                           26 '3,1;'
093700010726     O                       dpd                 30
093800010723     O* costante dpd riga sotto
093900010726     O*         E            LV22dpd     1
094000010726     O*                      dpd1                23
094100010725     O********* Chiusura etchetta
094200010725     O          E            LV22dpd     1
094300010725     O                                              '?14&1'
094400010725     O          E            LV22dpd     1
094500010725     O                                              '?00&'
