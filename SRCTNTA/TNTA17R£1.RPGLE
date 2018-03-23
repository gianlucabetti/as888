000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020402      * TNTA17R *----------------------------------------------------*
000300020402      *              LISTA SINTETICA TARIFFE          FILTRO         *
000400020402      *--------------------------------------------------------------*
000500940715     FTNTA17D   CF   E             WORKSTN
000600000000     FTABEL00F  IF   E           K DISK
000700900423     FAZORG01L  IF   E           K DISK
000800041007     fAzorg02l  if   e           k disk    rename(azorg:azorg02)
000900020708
001000041007     d codut           s              1  0 Inz(1)
001100090701     d cod             s                   like(tblcod)
001200090701     d key             s                   like(tblkey)
001300041007     d xx              s              3  0
001400041007     d yy              s              3  0
001500090701     d w003a           s              3
001600041007
001700070531     d car             s              3    dim(20)
001800041007
001900950718     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
002000950718     D*
002100020520     D MSG             S             79    DIM(14) CTDATA PERRCD(1)
002200940715     D*
002300000000     D PARAM           DS
002400940715     D* CODICE FILIALE
002500940715     D  V1CFIL                 1      3  0
002600940715     D* DATA  DAL / AL   (GG/MM/AAAA)
002700940715     D  V1CDAL                 4     11  0
002800940715     D  V1CAL                 12     19  0
002900940715     D* CODICE COMMERCIALE
003000020513     D  V1CAGE                20     26
003100940715     D* DATA  DAL / AL   (AAAA/MM/GG)
003200940715     D  COMDAL                27     34  0
003300940715     D  COMAL                 35     42  0
003400940715     D* RICHIESTA STAMPA: S=IN SCADENZA / V=VARIATE / D=CON DECORRENZA
003500940715     D  WSTA                  43     43
003600950406     D* SALTO PAGINA A ROTTURA CODICE COMMERCIALE (S/ )
003700950406     D  V1CRCC                44     44
003800960621     D* COPIA FIRMATA OFFERTA: S=FIRMATE / N=NON FIRMATE / ' '=TUTTE
003900960621     D  V1CCFO               105    105
004000970407     D* SCELTA CLIENTI_: BLOCCATI NON BLOCCATI TUTTI
004100970407     D  V1CTCL               106    106
004200020328      * cod.tassazioni incongruenti con tariffa
004300020402     d  v1ccts               107    107
004400020405      * tar.partic. senza cod.tass. IT o EE
004500020405     d  v1cpar               108    108
004600020513     D* COMMERCIALE UNIFICANTE
004700020513     D  V1CAGU               109    115
004800020513     D* TIPO ORDINAMENTO
004900020513     D  V1CTOR               116    116
005000090701      * dati necessari x il caricamento della sk abilitazioni
005100041007     d  prmabi               117    118
005200041007     d  prmcdi               119    119
005300041007     d  prmcar               120    122  0
005400041007     d  prmcpo               123    125  0
005500041007      * aree
005600041007     d  prmare               126    155    dim(10)
005700041007     d  prmflg               156    156
005800020513     D*
005900921012     D WLBDAT          DS
006000940715     D  G02DAT                 1      8  0
006100940715     D  G02INV                 9     16  0
006200940715     D  G02ERR                17     17
006300940715     D  G02TGI                18     22  0
006400041007
006500041007     d ds_car          ds
006600041007     d  v1car1
006700041007     d  v1car2
006800041007     d  v1car3
006900041007     d  v1car4
007000041007     d  v1car5
007100041007     d  v1car6
007200041007     d  v1car7
007300041007     d  v1car8
007400041007     d  v1car9
007500041007     d  v1car0
007600041007
007700000000     D KPJBA         E DS
007800020513      *-----------------------------------------------------------
007900020513      * Ds di riferimento al file esterno AzUte00F
008000020513      *--------
008100020513     D AzuteDs       E DS                  ExtName(AzUte00F)
008200020513      *-----------------------------------------------------------
008300020513      * Ds per dati organigramma
008400020513      *--------
008500020513     D DDatiUte      E DS
008600020513      *-----------------------------------------------------------
008700020513      * Parametri per richiamo al pgm di controllo profilo utenti
008800020513      *--------
008900020513     D Tibs34Ds      E DS                  Inz
009000041007     d tibs02ds      e ds
009100090701     d tntaa1ds      e ds
009200940715     C*****************************************************************
009300940715     C* RIEPILOGO INDICATORI
009400940715     C*****************************************************************
009500950718     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
009600940715     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
009700940715     C* 30    - DI COMODO
009800950718     C* 35    - USATO PER LOKUP
009900041007     C* 40/56 - ERRORI
010000940715     C*****************************************************************
010100940719     C*
010200041007     c     startx        tag
010300020513     C                   MOVE      *ZEROS        V1CAGE
010400020513     C                   MOVE      *ZEROS        V1CAGU
010500940715     C                   CLEAR                   V1DAGE
010600940715     C                   CLEAR                   V1CAL
010700940719     C                   Z-ADD     01011901      V1CDAL
010800940715     C                   MOVEL     'S'           V1CSTA
010900960621     C                   MOVEL     'N'           V1CCFO
011000950406     C                   MOVEL     'S'           V1CRCC
011100970407     C                   MOVEL     'N'           V1CTCL
011200020513     C                   clear                   v1ccts
011300020513     C                   clear                   v1cpar
011400020513     C                   MOVE      'U'           V1CTOR
011500041007     C                   clear                   ds_car
011600041007     C*
011700041007     C* SE abilitazione distretto faccio vedere le aree gestite
011800090701     C                   IF        otaa1cabi = 'DI'
011900041007     c                   Exsr      Cararee
012000041007     C                   seton                                        46
012100041007     C                   endif
012200921012     C*
012300940715     C     FOR01         TAG
012400940715     C                   EXFMT     TA17D01
012500940715     C*
012600940715     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
012700940715     C                   CLEAR                   V1CMSG
012800940715     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
012900940715     C                   SETOFF                                       404142
013000020513     C                   SETOFF                                       432844
013100020513     C                   SETOFF                                       4547
013200041007     c                   Setoff                                       484950
013300041007     c                   Setoff                                       515253
013400041007     c                   Setoff                                       545556
013500940715     C*
013600090701     C* F3 - FINE LAVORO
013700950718     C   KC
013800950718     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
013900090701     C**          PERCHÈ UTENTE NON ABILITATO
014000950718     COR 08              GOTO      FINE
014100921012     C*
014200921012     C* CONTROLLI FORMATO
014300940715     C                   EXSR      CTRD01
014400940715     C   28
014500940715     CORNKF              GOTO      FOR01
014600041007      * passo la schiera delle aree
014700041007     c                   movea     ds_car        prmare
014800041007     c                   if        ds_car <> *Blanks
014900041007     c                   Eval      prmflg = '1'
015000041007     c                   EndIf
015100041007
015200900406     C*
015300940715     C* CMD6 - CONFERMA ELABORAZIONE
015400921013     C* SE RICHIESTA STAMPA = *BLANKS  LANCIO TUTTE LE STAMPE
015500020612     C                   IF        V1CAGE = *BLANKS
015600020612     C                   MOVE      *ZEROS        V1CAGE
015700020612     C                   ENDIF
015800020612     C                   IF        V1CAGU = *BLANKS
015900020612     C                   MOVE      *ZEROS        V1CAGU
016000020612     C                   ENDIF
016100020612      *
016200940715    1C     V1CSTA        IFEQ      ' '
016300940715     C                   MOVEL     'S'           WSTA
016400921013     C                   MOVEL     PARAM         KPJBU
016500020531     C                   MOVEL     'TA18'        KCOAZ
016600020531     C                   CALL      'BCH10'
016700921013     C                   PARM                    KPJBA
016800921013     C*
016900940715     C                   MOVEL     'V'           WSTA
017000921013     C                   MOVEL     PARAM         KPJBU
017100020531     C                   MOVEL     'TA18'        KCOAZ
017200020531     C                   CALL      'BCH10'
017300921013     C                   PARM                    KPJBA
017400921013     C*
017500940715     C                   MOVEL     'D'           WSTA
017600921013     C                   MOVEL     PARAM         KPJBU
017700020531     C                   MOVEL     'TA18'        KCOAZ
017800020531     C                   CALL      'BCH10'
017900921013     C                   PARM                    KPJBA
018000921013     C*
018100940715   X1C                   ELSE
018200921013     C* LANCIO SOLO LA STAMPA RICHIESTA
018300940715     C                   MOVEL     V1CSTA        WSTA
018400921012     C                   MOVEL     PARAM         KPJBU
018500020531     C                   MOVEL     'TA18'        KCOAZ
018600020531     C                   CALL      'BCH10'
018700000000     C                   PARM                    KPJBA
018800940715    1C                   ENDIF
018900921012     C*
019000000000     C     FINE          TAG
019100090701     C*
019200090701     C                   clear                   tntaa1ds
019300090701     C                   movel     'C'           Itaa1Tla
019400090701     C                   movel(p)  tntaa1ds      kpjbu
019500090703     C                   CALL      'TNTAA1C'
019600090701     C                   PARM                    KPJBA
019700090701     c
019800000000     C                   SETON                                        LR
019900921012     C*
020000921012     C*--- CONTROLLI FORMATO -----------------------------------------*
020100940715     C     CTRD01        BEGSR
020200020513     C   28              GOTO      ENDCTR
020300930301     C                   Z-ADD     0             COMDAL
020400930301     C                   Z-ADD     0             COMAL
020500921012     C*
020600020513     C****  CODICE COMMERCIALE UNIFICANTE
020700020513      * Ricerche
020800020513     C     '?'           SCAN      V1CAGU                                 27
020900020513     C*
021000020513     C     *IN27         IFEQ      *ON
021100020517     C                   MOVEL     *ZEROS        §FIL
021200020513     C*
021300020520     C                   MOVE      *BLANKS       §KEY
021400020513     C                   CALL      'TRTB85R'
021500020513     C                   PARM                    KPJBA
021600020513     C                   PARM                    §FIL              3
021700020517     C                   PARM                    §KEY              8
021800020513     C                   PARM                    §DES1            25
021900020513     C*
022000020517     C                   MOVEL     §KEY          V1CAGU
022100020513     C                   MOVEL     §DES1         V1DAGU
022200020513     C                   ENDIF
022300020513      * Controlli
022400020513     C                   MOVEL     *BLANKS       V1DAGU
022500020513     C     V1CAGU        IFNE      *BLANKS
022600020513     C     V1CAGU        ANDNE     *ZEROS
022700020513     C                   MOVE      '01'          COD
022800020513     C                   MOVEL     *BLANKS       KEY
022900020513     C                   MOVEL     V1CAGU        KEY
023000020513     C     KTAB          CHAIN     TABEL                              30
023100020513     C  N30TBLFLG        IFNE      ' '
023200020513     C                   SETON                                        30
023300020513     C                   ENDIF
023400020513     C*
023500020513     C     *IN30         IFEQ      *ON
023600020513     C                   MOVEL     MSG(9)        V1CMSG
023700020513     C                   SETON                                        4428
023800020513     C                   GOTO      ENDCTR
023900020513     C                   ENDIF
024000020513     C*
024100020513     C* DECODIFICA CODICE COMMERCIALE UNIFICANTE
024200020513     C                   MOVEL     TBLUNI        V1DAGU
024300020513     C                   ENDIF
024400940715     C****  CODICE FILIALE  ****
024500921012     C* SE IMMESSA CONTROLLO VALIDITA' FILIALE
024600940715    1C     V1CFIL        IFGT      0
024700090701     c                   clear                   tntaa1ds
024800090701     c                   eval      itaa1caut='§utegtc'
024900090701     c                   eval      itaa1fil=v1cfil
025000090701     c                   clear                   kpjbu
025100090701     c                   movel     tntaa1ds      kpjbu
025200090703     c                   call      'TNTAA1C'
025300090701     c                   parm                    kpjba
025400090701     c                   movel     kpjbu         tntaa1ds
025500090701     c
025600090701    1c                   if        otaa1fabi='N'
025700090701     c
025800090701     c***                move      v1cfil        w003a
025900950718     C*
026000950718     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DELLA
026100950718     C*   FILIALE RICHIESTA A VIDEO
026200090701     C***  w003a         LOOKUP    FIG                                    35
026300950718     C*
026400090701    3C***  *IN35         IFEQ      *OFF
026500950718     C                   MOVEL     MSG(7)        V1CMSG
026600950718     C                   SETON                                        4028
026700950718     C                   GOTO      ENDCTR
026800950718    3C                   ENDIF
026900950718     C*
027000940715     C     V1CFIL        CHAIN     AZORG                              30
027100940715     C     *IN30         IFEQ      *ON
027200940715     C                   MOVEL     MSG(1)        V1CMSG
027300940715     C                   SETON                                        4028
027400940715     C                   GOTO      ENDCTR
027500940715     C                   ENDIF
027600940715    1C                   ENDIF
027700921012     C*
027800940715     C****  CODICE COMMERCIALE  ****
027900020513      * Ricerche
028000020513     C     '?'           SCAN      V1CAGE                                 27
028100020513     C*
028200020513     C     *IN27         IFEQ      *ON
028300020513     C                   MOVEL     V1CFIL        §FIL
028400020513     C*
028500020520     C                   MOVE      *BLANKS       §KEY
028600020513     C                   CALL      'TRTB85R'
028700020513     C                   PARM                    KPJBA
028800020513     C                   PARM                    §FIL              3
028900020517     C                   PARM                    §KEY              8
029000020513     C                   PARM                    §DES1            25
029100020513     C*
029200020517     C                   MOVEL     §KEY          V1CAGE
029300020513     C                   MOVEL     §DES1         V1DAGE
029400020513     C                   ENDIF
029500940715     C* SE IMMESSO CONTROLLO CODICE COMMERCIALE
029600020513     C                   MOVEL     *BLANKS       V1DAGE
029700040507    1C     V1CAGE        IFNE      *BLANKS
029800020513     C     V1CAGE        ANDNE     *ZEROS
029900950718     C*
030000950718     C* CONTROLLO VALIDITA' CODICE COMMERCIALE
030100921012     C                   MOVE      '01'          COD
030200940715     C                   MOVEL     *BLANKS       KEY
030300940715     C                   MOVEL     V1CAGE        KEY
030400921012     C     KTAB          CHAIN     TABEL                              30
030500940715     C  N30TBLFLG        IFNE      ' '
030600940715     C                   SETON                                        30
030700940715     C                   ENDIF
030800940715     C*
030900940715     C     *IN30         IFEQ      *ON
031000940715     C                   MOVEL     MSG(2)        V1CMSG
031100940715     C                   SETON                                        4128
031200940715     C                   GOTO      ENDCTR
031300940715     C                   ENDIF
031400940715     C*
031500940715     C* DECODIFICA CODICE COMMERCIALE
031600940715     C                   MOVEL     TBLUNI        V1DAGE
031700090701     C                   MOVEL     V1CAGE        FILAGE            3 0
031800090701     c
031900090701     c                   clear                   tntaa1ds
032000090701     c                   eval      itaa1caut='§utegtc'
032100090701     c                   movel     v1cage        itaa1cmm
032200090701     c                   clear                   kpjbu
032300090701     c                   movel     tntaa1ds      kpjbu
032400090703     c                   call      'TNTAA1C'
032500090701     c                   parm                    kpjba
032600090701     c                   movel     kpjbu         tntaa1ds
032700090701     c
032800090701    1c                   if        otaa1fabi='N'
032900090701     c
033000940715     C*
033100040507     C* CONTROLLO SE LA FILIALE COLLEGATA PUO' GESTIRE LE TARIFFE DEL
033200040507     C*   CODICE COMMERCIALE DIGITATO
033300090701     C**   w003a         LOOKUP    FIG                                    35
033400040507     C*
033500090701    3C**   *IN35         IFEQ      *OFF
033600040507     c* se non trovato controllo il suo unificante se è del p.o utente
033700090701     c**                 movel     tbluni        ds01
033800090701     c**                 movel     §01rgf        wporagru          3
033900090701     C**   wporagru      LOOKUP    FIG                                    35
034000040511     C*
034100090701    3C**   *IN35         IFEQ      *OFF
034200040507     C                   MOVEL     MSG(8)        V1CMSG
034300040507     C                   SETON                                        4128
034400040507     C                   GOTO      ENDCTR
034500040507    4C                   ENDIF
034600090701    3C**                 ENDIF
034700020513     C*
034800940715     C* CONTROLLO CONGRUENZA CON FILIALE SE IMMESSA
034900940715    2C     V1CFIL        IFGT      0
035000940715     C     V1CFIL        IFNE      FILAGE
035100940715     C                   MOVEL     MSG(3)        V1CMSG
035200940715     C                   SETON                                        4028
035300940715     C                   GOTO      ENDCTR
035400940715     C                   ENDIF
035500940715    2C                   ENDIF
035600921012     C*
035700940715   X1C                   ELSE
035800950718     C* CODICE COMMERCIALE = 0
035900940715     C                   CLEAR                   V1DAGE
036000940715    1C                   ENDIF
036100020513      *
036200020513     C****  FILIALE /COMMERCIALE UNIFICANTE ***
036300020513     C     V1CFIL        IFNE      *ZEROS
036400020513     C     V1CAGU        ANDNE     *BLANKS
036500020513     C     V1CAGU        ANDNE     *ZEROS
036600020513     C                   MOVEL     MSG(10)       V1CMSG
036700020513     C                   SETON                                        444028
036800020513     C                   GOTO      ENDCTR
036900020513     C                   ENDIF
037000020520     C*
037100020520     C****  Commerciale Unificante / Codice commerciale
037200020520     C     V1CAGE        IFNE      *BLANKS
037300020520     C     V1CAGE        ANDNE     *ZEROS
037400020520     C     V1CAGU        ANDNE     *BLANKS
037500020520     C     V1CAGU        ANDNE     *ZEROS
037600020520     C                   MOVEL     MSG(11)       V1CMSG
037700020520     C                   SETON                                        414428
037800020520     C                   GOTO      ENDCTR
037900020520     C                   ENDIF
038000020520     C*
038100041007      ****  AREE ***
038200041007      *  Punto Operativo/Aree
038300041007     c                   If        V1cFil <> *Zeros
038400041007     c                             and ds_car <> *Blanks
038500041007     c                   Eval      V1cMsg = msg(12)
038600041007     c                   Seton                                        284740
038700041007     c                   goto      ENDCTR
038800041007     c                   EndIf
038900041007      * Codice commerciale/Aree
039000041007     c                   If        V1cAge <> *Zeros and V1cAge <> *Blanks
039100041007     c                             and ds_car <> *Blanks
039200041007     c                   Eval      V1cMsg = msg(13)
039300041007     c                   Seton                                        284741
039400041007     c                   goto      ENDCTR
039500041007     c                   EndIf
039600041007
039700041007      * passo la schiera delle aree
039800041007     c                   movea     ds_car        prmare
039900041007      * Ricerca
040000041007     c                   Do        10            xx
040100041007     c                   Eval      yy = (46 + xx)
040200041007     c                   if        prmare(xx) <> *blanks
040300041007     c     prmare(xx)    lookup    car                                    30
040400041007     c                   If        Not *In30
040500041007     c                   Eval      v1cmsg = msg(14)
040600041007     c                   Eval      *In(yy) = *On
040700041007     c                   Eval      *In28 = *On
040800041007     c                   EndIf
040900041007     c                   endif
041000041007     c                   enddo
041100041007
041200041007     c                   move      prmare(1)     v1car1
041300041007     c                   move      prmare(2)     v1car2
041400041007     c                   move      prmare(3)     v1car3
041500041007     c                   move      prmare(4)     v1car4
041600041007     c                   move      prmare(5)     v1car5
041700041007     c                   move      prmare(6)     v1car6
041800041007     c                   move      prmare(7)     v1car7
041900041007     c                   move      prmare(8)     v1car8
042000041007     c                   move      prmare(9)     v1car9
042100041007     c                   move      prmare(10)    v1car0
042200041007     c   28              goto      ENDCTR
042300921012     C*
042400940715     C****  CONTROLLO DATA  DAL  ****
042500921012     C* DATA DAL OBBLIGATORIA
042600940715     C                   Z-ADD     V1CDAL        G02DAT
042700921012     C                   MOVEL     *BLANKS       G02ERR
042800940715     C                   CALL      'XSRDA8'
042900930301     C                   PARM                    WLBDAT
043000930301     C     G02ERR        IFEQ      '1'
043100940715     C                   MOVEL     MSG(4)        V1CMSG
043200940715     C                   SETON                                        4228
043300940715     C                   GOTO      ENDCTR
043400940715     C                   ENDIF
043500940715     C                   Z-ADD     G02INV        COMDAL            8 0
043600940715     C*
043700940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
043800940715     C                   Z-ADD     G02DAT        V1CDAL
043900930301     C*
044000940715     C     V1CSTA        IFEQ      'S'
044100940719     C     V1CDAL        ANDNE     01011901
044200940715     C                   MOVEL     MSG(5)        V1CMSG
044300940715     C                   SETON                                        4228
044400940715     C                   GOTO      ENDCTR
044500940715     C                   ENDIF
044600921012     C*
044700940715     C****  CONTROLLO DATA  AL   ****
044800921012     C* DATA AL OBBLIGATORIA
044900940715     C                   Z-ADD     V1CAL         G02DAT
045000921012     C                   MOVEL     *BLANKS       G02ERR
045100940715     C                   CALL      'XSRDA8'
045200921012     C                   PARM                    WLBDAT
045300921013     C     G02ERR        IFEQ      '1'
045400940715     C                   MOVEL     MSG(4)        V1CMSG
045500940715     C                   SETON                                        4328
045600940715     C                   GOTO      ENDCTR
045700940715     C                   ENDIF
045800940715     C                   Z-ADD     G02INV        COMAL             8 0
045900940715     C*
046000940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
046100940715     C                   Z-ADD     G02DAT        V1CAL
046200921012     C*
046300020513     C*
046400940715     C     ENDCTR        ENDSR
046500041007
046600041007      *---------------------------------------------------------------*
046700041007      * Cararee - Carico le aree in gestione al distretto
046800041007      *---------------------------------------------------------------*
046900041007     c     Cararee       BegSr
047000041007
047100041007     c                   Clear                   car
047200041007
047300041007     c     DutDis        Setll     Azorg02l
047400041007do  1c                   Do        *Hival
047500041007     c     DutDis        Reade     Azorg02l
047600041007     c                   If        %Eof(Azorg02l)
047700041007     c                   Leave
047800041007     c                   EndIf
047900041007     c                   If        OrgFva <> *Blanks or
048000041007     c                             (OrgFag <> 'A' and OrgFag <> 'F')
048100041007     c                   Iter
048200041007     c                   EndIf
048300041007
048400041007     c                   Move      OrgCar        w003a
048500041007     c     w003a         Lookup    car                                    30
048600041007if  2c                   If        Not *In30
048700041007     c                   Eval      xx = 1
048800041007     c     *Blanks       Lookup    car(xx)                                30
048900041007if  3c                   If        *In30
049000041007     c                   Eval      car(xx) = w003a
049100041007e   3c                   EndIf
049200041007e   2c                   EndIf
049300041007
049400041007e   1c                   EndDo
049500041007
049600041007     c                   eval      v1car1 = car(1)
049700041007     c                   eval      v1car2 = car(2)
049800041007     c                   eval      v1car3 = car(3)
049900041007     c                   eval      v1car4 = car(4)
050000041007     c                   eval      v1car5 = car(5)
050100041007     c                   eval      v1car6 = car(6)
050200041007     c                   eval      v1car7 = car(7)
050300041007     c                   eval      v1car8 = car(8)
050400041007     c                   eval      v1car9 = car(9)
050500041007     c                   eval      v1car0 = car(10)
050600041007
050700041007     c                   EndSr
050800921012     C*
050900950720     C*--- INZSR INIZIALE --------------------------------------------*
051000940719     C     *INZSR        BEGSR
051100950720     C*
051200950720     C     *ENTRY        PLIST
051300950720     C                   PARM                    KPJBA
051400041007      *
051500041007     C     *dtaara       define    §azute        azuteds
051600041007     C     *dtaara       define    §datiute      ddatiute
051700041007      *
051800041007     C                   in(E)     *dtaara
051900041007     C                   IF        %Error or RSUT = *blanks
052000041007     C                   call      'TIBS34R'
052100041007     C                   parm                    Tibs34Ds
052200041007     C                   in        *dtaara
052300041007     C                   ENDIF
052400020517      *
052500020604     C     SIMFEL        IFNE      *zeros
052600020517     C                   MOVEL     DUTPOU        V1CFIL
052700020604     C                   ELSE
052800020604     C                   MOVE      *ZEROS        V1CFIL
052900020604     C                   ENDIF
053000020604     C
053100090701     c* Richiamo solo per avere codice di abilitazione
053200090701     c                   clear                   tntaa1ds
053300090701     c                   eval      itaa1caut='§utegtc'
053400090701     c                   clear                   kpjbu
053500090701     c                   movel     tntaa1ds      kpjbu
053600090703     c                   call      'TNTAA1C'
053700090701     c                   parm                    kpjba
053800090701     c                   movel     kpjbu         tntaa1ds
053900090701     C
054000090701     c* utente non abilitato alla gestione tariffe
054100090701     c                   if        otaa1fabi='N'
054200090701     c                   Seton                                        442808
054300090701     C                   movel     msg(6)        v1cmsg
054400090701     c                   endif
054500090701      *
054600090701     c                   Eval      prmabi = otaa1cabi
054700090701     c                   Eval      prmcdi = dutdis
054800090701     c                   Eval      prmcar = dutare
054900090701     c                   Eval      prmcpo = dutpou
055000940719     C***
055100940719     C* CHIAVI
055200940719     C***
055300940719     C* ACCESSO TABEL00F
055400940719     C     KTAB          KLIST
055500940719     C                   KFLD                    CODUT
055600940719     C                   KFLD                    COD
055700940719     C                   KFLD                    KEY
055800940719     C*
055900090701     C                   ENDSR
056000940719     C*---------------------------------------------------------------*
056100950904**  MSG
056200090701Filiale  Inesistente
056300940715Codice Commerciale non codificato                                              2
056400090701Filiale  Incongruente con Codice Commerciale
056500940715Data Errata                                                                    4
056600940719Per le tariffe in scadenza la data DAL deve essere 01/01/1901                  5
056700090701UTENTE NON ABILITATO ALLA GESTIONE DELLE TARIFFE                               6
056800090701Filiale non in gestione all'utente
056900090701Codice Commerciale non in gestione all'utente                                  8
057000020513Cod.Commerciale unificante Inesistente o Annullato                             1
057100090701Si puo' scegliere o Cod.Commerciale unificante o Filiale
057200020520Si puo' scegliere o Cod.Commerciale unificante o Codice Commerciale
057300090701Filiale  immessa  non congruente  con aree selezionate
057400090701Codice Commerciale immesso non congruente con aree selezionate
057500041007Codice Area non in gestione all'utente                                        14
