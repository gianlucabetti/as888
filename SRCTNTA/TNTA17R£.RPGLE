000100020708     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020402      * TNTA17R *----------------------------------------------------*
000300020402      *              LISTA SINTETICA TARIFFE          FILTRO         *
000400020402      *--------------------------------------------------------------*
000500940715     FTNTA17D   CF   E             WORKSTN
000600900423     FAZORG01L  IF   E           K DISK
000700041007     fAzorg02l  if   e           k disk    rename(azorg:azorg02)
000800130805     fAZCMM01L  if   e           k disk
000900130805
001000130805       // -?Costante per controllo "caratteri solo numerici"?
001100130805     d c_Digits        c                   const('0123456789')
001200020708
001300041007     d xx              s              3  0
001400041007     d yy              s              3  0
001500090701     d w003a           s              3
001600041007
001700070531     d car             s              3    dim(20)
001800041007
001900950718     D* SCHIERA CONTENENTE LE FILIALI GESTITE DALLA FILIALE COLLEGATA
002000950718     D*
002100130805     D MSG             S             79    DIM(15) CTDATA PERRCD(1)
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
006400130805      *
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
007600130805      *
007700000000     D KPJBA         E DS
007800130805      *
007900130805     d TRMK43ds      e ds                  inz
008000130805     d   oMK43err    e                     inz(*off)
008100020513      *-----------------------------------------------------------
008200020513      * Ds di riferimento al file esterno AzUte00F
008300020513      *--------
008400020513     D AzuteDs       E DS                  ExtName(AzUte00F)
008500020513      *-----------------------------------------------------------
008600020513      * Ds per dati organigramma
008700020513      *--------
008800020513     D DDatiUte      E DS
008900020513      *-----------------------------------------------------------
009000020513      * Parametri per richiamo al pgm di controllo profilo utenti
009100020513      *--------
009200020513     D Tibs34Ds      E DS                  Inz
009300041007     d tibs02ds      e ds
009400090701     d tntaa1ds      e ds
009500940715     C*****************************************************************
009600940715     C* RIEPILOGO INDICATORI
009700940715     C*****************************************************************
009800950718     C* 08    - QUALSIASI TASTO VENGA PREMUTO --> ESCO DAL PROGRAMMA
009900940715     C* 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
010000940715     C* 30    - DI COMODO
010100950718     C* 35    - USATO PER LOKUP
010200041007     C* 40/56 - ERRORI
010300940715     C*****************************************************************
010400940719     C*
010500041007     c     startx        tag
010600020513     C                   MOVE      *ZEROS        V1CAGE
010700020513     C                   MOVE      *ZEROS        V1CAGU
010800940715     C                   CLEAR                   V1DAGE
010900940715     C                   CLEAR                   V1CAL
011000940719     C                   Z-ADD     01011901      V1CDAL
011100940715     C                   MOVEL     'S'           V1CSTA
011200960621     C                   MOVEL     'N'           V1CCFO
011300950406     C                   MOVEL     'S'           V1CRCC
011400970407     C                   MOVEL     'N'           V1CTCL
011500020513     C                   clear                   v1ccts
011600020513     C                   clear                   v1cpar
011700020513     C                   MOVE      'U'           V1CTOR
011800041007     C                   clear                   ds_car
011900041007     C*
012000041007     C* SE abilitazione distretto faccio vedere le aree gestite
012100090701     C                   IF        otaa1cabi = 'DI'
012200041007     c                   Exsr      Cararee
012300041007     C                   seton                                        46
012400041007     C                   endif
012500921012     C*
012600940715     C     FOR01         TAG
012700940715     C                   EXFMT     TA17D01
012800940715     C*
012900940715     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
013000940715     C                   CLEAR                   V1CMSG
013100940715     C* AZZERO GLI INDICATORI RELATIVI AI MESSAGGI DI ERRORE
013200940715     C                   SETOFF                                       404142
013300020513     C                   SETOFF                                       432844
013400020513     C                   SETOFF                                       4547
013500041007     c                   Setoff                                       484950
013600041007     c                   Setoff                                       515253
013700041007     c                   Setoff                                       545556
013800940715     C*
013900090701     C* F3 - FINE LAVORO
014000950718     C   KC
014100950718     C** 08 ON  - QUALSIASI TASTO VENGA PREMUTO, ESCO DAL PROGRAMMA
014200090701     C**          PERCHÈ UTENTE NON ABILITATO
014300950718     COR 08              GOTO      FINE
014400921012     C*
014500921012     C* CONTROLLI FORMATO
014600940715     C                   EXSR      CTRD01
014700940715     C   28
014800940715     CORNKF              GOTO      FOR01
014900041007      * passo la schiera delle aree
015000041007     c                   movea     ds_car        prmare
015100041007     c                   if        ds_car <> *Blanks
015200041007     c                   Eval      prmflg = '1'
015300041007     c                   EndIf
015400041007
015500900406     C*
015600940715     C* CMD6 - CONFERMA ELABORAZIONE
015700921013     C* SE RICHIESTA STAMPA = *BLANKS  LANCIO TUTTE LE STAMPE
015800020612     C                   IF        V1CAGE = *BLANKS
015900020612     C                   MOVE      *ZEROS        V1CAGE
016000020612     C                   ENDIF
016100020612     C                   IF        V1CAGU = *BLANKS
016200020612     C                   MOVE      *ZEROS        V1CAGU
016300020612     C                   ENDIF
016400020612      *
016500940715    1C     V1CSTA        IFEQ      ' '
016600940715     C                   MOVEL     'S'           WSTA
016700921013     C                   MOVEL     PARAM         KPJBU
016800020531     C                   MOVEL     'TA18'        KCOAZ
016900020531     C                   CALL      'BCH10'
017000921013     C                   PARM                    KPJBA
017100921013     C*
017200940715     C                   MOVEL     'V'           WSTA
017300921013     C                   MOVEL     PARAM         KPJBU
017400020531     C                   MOVEL     'TA18'        KCOAZ
017500020531     C                   CALL      'BCH10'
017600921013     C                   PARM                    KPJBA
017700921013     C*
017800940715     C                   MOVEL     'D'           WSTA
017900921013     C                   MOVEL     PARAM         KPJBU
018000020531     C                   MOVEL     'TA18'        KCOAZ
018100020531     C                   CALL      'BCH10'
018200921013     C                   PARM                    KPJBA
018300921013     C*
018400940715   X1C                   ELSE
018500921013     C* LANCIO SOLO LA STAMPA RICHIESTA
018600940715     C                   MOVEL     V1CSTA        WSTA
018700921012     C                   MOVEL     PARAM         KPJBU
018800020531     C                   MOVEL     'TA18'        KCOAZ
018900020531     C                   CALL      'BCH10'
019000000000     C                   PARM                    KPJBA
019100940715    1C                   ENDIF
019200921012     C*
019300000000     C     FINE          TAG
019400090701     C*
019500090701     C                   clear                   tntaa1ds
019600090701     C                   movel     'C'           Itaa1Tla
019700090701     C                   movel(p)  tntaa1ds      kpjbu
019800090703     C                   CALL      'TNTAA1C'
019900090701     C                   PARM                    KPJBA
020000090701     c
020100000000     C                   SETON                                        LR
020200921012     C*
020300921012     C*--- CONTROLLI FORMATO -----------------------------------------*
020400940715     C     CTRD01        BEGSR
020500020513     C   28              GOTO      ENDCTR
020600930301     C                   Z-ADD     0             COMDAL
020700930301     C                   Z-ADD     0             COMAL
020800921012     C*
020900020513     C****  CODICE COMMERCIALE UNIFICANTE
021000020513      * Ricerche
021100020513     C     '?'           SCAN      V1CAGU                                 27
021200020513     C*
021300020513     C     *IN27         IFEQ      *ON
021400130805     C                   reset                   TRMK43ds
021500130805     c                   eval      iMK43solU = 'S'
021600130805     C                   CALL      'TRMK43R'
021700020513     C                   PARM                    KPJBA
021800130805     C                   PARM                    TRMK43ds
021900130805     C                   if        oMK43err = *off  and
022000130805     c                             oMK43fxx = *blank
022100130805     C                   MOVEL     oMK43cmm      V1CAGU
022200130805     C                   MOVEL     oMK43des      V1DAGU
022300130805     C                   endif
022400020513     C                   ENDIF
022500020513      * Controlli
022600020513     C                   MOVEL     *BLANKS       V1DAGU
022700020513     C     V1CAGU        IFNE      *BLANKS
022800020513     C     V1CAGU        ANDNE     *ZEROS
022900130805     c                   if        %check(c_Digits : V1Cagu) > *zero
023000130805     c                   eval      V1Cmsg = Msg(9)
023100130805     c                   eval      *in44 = *on
023200130805     c                   eval      *in28 = *on
023300130805     c                   leavesr
023400130805     c                   endif
023500130805     C                   MOVEL     V1CAGU        k_CMMcod
023600130805     C     k_azcmm01     CHAIN     AZCMM000
023700130805     C                   IF        Not %found(AZCMM01L)
023800020513     C                   MOVEL     MSG(9)        V1CMSG
023900020513     C                   SETON                                        4428
024000020513     C                   GOTO      ENDCTR
024100020513     C                   ENDIF
024200130805     C                   IF        CMMuni <> CMMcod
024300130805     C                   MOVEL     MSG(15)       V1CMSG
024400130805     C                   SETON                                        4428
024500130805     C                   GOTO      ENDCTR
024600130805     C                   ENDIF
024700020513     C*
024800020513     C* DECODIFICA CODICE COMMERCIALE UNIFICANTE
024900130805     C                   MOVEL     CMMdes        V1DAGU
025000020513     C                   ENDIF
025100940715     C****  CODICE FILIALE  ****
025200921012     C* SE IMMESSA CONTROLLO VALIDITA' FILIALE
025300940715    1C     V1CFIL        IFGT      0
025400090701     c                   clear                   tntaa1ds
025500090701     c                   eval      itaa1caut='§utegtc'
025600090701     c                   eval      itaa1fil=v1cfil
025700090701     c                   clear                   kpjbu
025800090701     c                   movel     tntaa1ds      kpjbu
025900090703     c                   call      'TNTAA1C'
026000090701     c                   parm                    kpjba
026100090701     c                   movel     kpjbu         tntaa1ds
026200130805     c*
026300090701    1c                   if        otaa1fabi='N'
026400950718     C                   MOVEL     MSG(7)        V1CMSG
026500950718     C                   SETON                                        4028
026600950718     C                   GOTO      ENDCTR
026700950718    3C                   ENDIF
026800950718     C*
026900940715     C     V1CFIL        CHAIN     AZORG                              30
027000940715     C     *IN30         IFEQ      *ON
027100940715     C                   MOVEL     MSG(1)        V1CMSG
027200940715     C                   SETON                                        4028
027300940715     C                   GOTO      ENDCTR
027400940715     C                   ENDIF
027500940715    1C                   ENDIF
027600921012     C*
027700940715     C****  CODICE COMMERCIALE  ****
027800020513      * Ricerche
027900020513     C     '?'           SCAN      V1CAGE                                 27
028000020513     C*
028100020513     C     *IN27         IFEQ      *ON
028200130805     C                   reset                   TRMK43ds
028300130805     C                   MOVEL     V1CFIL        iMK43fil
028400130805     C                   CALL      'TRMK43R'
028500020513     C                   PARM                    KPJBA
028600130805     C                   PARM                    TRMK43ds
028700130805     C                   if        oMK43err = *off  and
028800130805     c                             oMK43fxx = *blank
028900130805     C                   MOVEL     oMK43cmm      V1CAGE
029000130805     C                   MOVEL     oMK43des      V1DAGE
029100130805     C                   endif
029200020513     C                   ENDIF
029300940715     C* SE IMMESSO CONTROLLO CODICE COMMERCIALE
029400020513     C                   MOVEL     *BLANKS       V1DAGE
029500040507    1C     V1CAGE        IFNE      *BLANKS
029600020513     C     V1CAGE        ANDNE     *ZEROS
029700950718     C*
029800950718     C* CONTROLLO VALIDITA' CODICE COMMERCIALE
029900130805     c                   if        %check(c_Digits : V1Cage) > *zero
030000130805     c                   eval      V1Cmsg = Msg(2)
030100130805     c                   eval      *in41 = *on
030200130805     c                   eval      *in28 = *on
030300130805     c                   leavesr
030400130805     c                   endif
030500130805     C                   MOVEL     V1CAGE        k_CMMcod
030600130805     C     k_azcmm01     CHAIN     AZCMM000
030700130805     C                   IF        Not %found(AZCMM01L)
030800940715     C                   MOVEL     MSG(2)        V1CMSG
030900940715     C                   SETON                                        4128
031000940715     C                   GOTO      ENDCTR
031100940715     C                   ENDIF
031200940715     C*
031300940715     C* DECODIFICA CODICE COMMERCIALE
031400130805     C                   MOVEL     CMMdes        V1DAGE
031500090701     C                   MOVEL     V1CAGE        FILAGE            3 0
031600130805     c*
031700090701     c                   clear                   tntaa1ds
031800090701     c                   eval      itaa1caut='§utegtc'
031900090701     c                   movel     v1cage        itaa1cmm
032000090701     c                   clear                   kpjbu
032100090701     c                   movel     tntaa1ds      kpjbu
032200090703     c                   call      'TNTAA1C'
032300090701     c                   parm                    kpjba
032400090701     c                   movel     kpjbu         tntaa1ds
032500130805     c*
032600130805    2c                   if        otaa1fabi='N'
032700040507     C                   MOVEL     MSG(8)        V1CMSG
032800040507     C                   SETON                                        4128
032900040507     C                   GOTO      ENDCTR
033000130805    2C                   ENDIF
033100020513     C*
033200940715     C* CONTROLLO CONGRUENZA CON FILIALE SE IMMESSA
033300940715    2C     V1CFIL        IFGT      0
033400940715     C     V1CFIL        IFNE      FILAGE
033500940715     C                   MOVEL     MSG(3)        V1CMSG
033600940715     C                   SETON                                        4028
033700940715     C                   GOTO      ENDCTR
033800940715     C                   ENDIF
033900940715    2C                   ENDIF
034000921012     C*
034100940715   X1C                   ELSE
034200950718     C* CODICE COMMERCIALE = 0
034300940715     C                   CLEAR                   V1DAGE
034400940715    1C                   ENDIF
034500020513      *
034600020513     C****  FILIALE /COMMERCIALE UNIFICANTE ***
034700020513     C     V1CFIL        IFNE      *ZEROS
034800020513     C     V1CAGU        ANDNE     *BLANKS
034900020513     C     V1CAGU        ANDNE     *ZEROS
035000020513     C                   MOVEL     MSG(10)       V1CMSG
035100020513     C                   SETON                                        444028
035200020513     C                   GOTO      ENDCTR
035300020513     C                   ENDIF
035400020520     C*
035500020520     C****  Commerciale Unificante / Codice commerciale
035600020520     C     V1CAGE        IFNE      *BLANKS
035700020520     C     V1CAGE        ANDNE     *ZEROS
035800020520     C     V1CAGU        ANDNE     *BLANKS
035900020520     C     V1CAGU        ANDNE     *ZEROS
036000020520     C                   MOVEL     MSG(11)       V1CMSG
036100020520     C                   SETON                                        414428
036200020520     C                   GOTO      ENDCTR
036300020520     C                   ENDIF
036400020520     C*
036500041007      ****  AREE ***
036600041007      *  Punto Operativo/Aree
036700041007     c                   If        V1cFil <> *Zeros
036800041007     c                             and ds_car <> *Blanks
036900041007     c                   Eval      V1cMsg = msg(12)
037000041007     c                   Seton                                        284740
037100041007     c                   goto      ENDCTR
037200041007     c                   EndIf
037300041007      * Codice commerciale/Aree
037400041007     c                   If        V1cAge <> *Zeros and V1cAge <> *Blanks
037500041007     c                             and ds_car <> *Blanks
037600041007     c                   Eval      V1cMsg = msg(13)
037700041007     c                   Seton                                        284741
037800041007     c                   goto      ENDCTR
037900041007     c                   EndIf
038000130805      *
038100041007      * passo la schiera delle aree
038200041007     c                   movea     ds_car        prmare
038300041007      * Ricerca
038400041007     c                   Do        10            xx
038500041007     c                   Eval      yy = (46 + xx)
038600041007     c                   if        prmare(xx) <> *blanks
038700041007     c     prmare(xx)    lookup    car                                    30
038800041007     c                   If        Not *In30
038900041007     c                   Eval      v1cmsg = msg(14)
039000041007     c                   Eval      *In(yy) = *On
039100041007     c                   Eval      *In28 = *On
039200041007     c                   EndIf
039300041007     c                   endif
039400041007     c                   enddo
039500130805      *
039600041007     c                   move      prmare(1)     v1car1
039700041007     c                   move      prmare(2)     v1car2
039800041007     c                   move      prmare(3)     v1car3
039900041007     c                   move      prmare(4)     v1car4
040000041007     c                   move      prmare(5)     v1car5
040100041007     c                   move      prmare(6)     v1car6
040200041007     c                   move      prmare(7)     v1car7
040300041007     c                   move      prmare(8)     v1car8
040400041007     c                   move      prmare(9)     v1car9
040500041007     c                   move      prmare(10)    v1car0
040600041007     c   28              goto      ENDCTR
040700921012     C*
040800940715     C****  CONTROLLO DATA  DAL  ****
040900921012     C* DATA DAL OBBLIGATORIA
041000940715     C                   Z-ADD     V1CDAL        G02DAT
041100921012     C                   MOVEL     *BLANKS       G02ERR
041200940715     C                   CALL      'XSRDA8'
041300930301     C                   PARM                    WLBDAT
041400930301     C     G02ERR        IFEQ      '1'
041500940715     C                   MOVEL     MSG(4)        V1CMSG
041600940715     C                   SETON                                        4228
041700940715     C                   GOTO      ENDCTR
041800940715     C                   ENDIF
041900940715     C                   Z-ADD     G02INV        COMDAL            8 0
042000940715     C*
042100940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
042200940715     C                   Z-ADD     G02DAT        V1CDAL
042300930301     C*
042400940715     C     V1CSTA        IFEQ      'S'
042500940719     C     V1CDAL        ANDNE     01011901
042600940715     C                   MOVEL     MSG(5)        V1CMSG
042700940715     C                   SETON                                        4228
042800940715     C                   GOTO      ENDCTR
042900940715     C                   ENDIF
043000921012     C*
043100940715     C****  CONTROLLO DATA  AL   ****
043200921012     C* DATA AL OBBLIGATORIA
043300940715     C                   Z-ADD     V1CAL         G02DAT
043400921012     C                   MOVEL     *BLANKS       G02ERR
043500940715     C                   CALL      'XSRDA8'
043600921012     C                   PARM                    WLBDAT
043700921013     C     G02ERR        IFEQ      '1'
043800940715     C                   MOVEL     MSG(4)        V1CMSG
043900940715     C                   SETON                                        4328
044000940715     C                   GOTO      ENDCTR
044100940715     C                   ENDIF
044200940715     C                   Z-ADD     G02INV        COMAL             8 0
044300940715     C*
044400940715     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
044500940715     C                   Z-ADD     G02DAT        V1CAL
044600921012     C*
044700940715     C     ENDCTR        ENDSR
044800041007
044900041007      *---------------------------------------------------------------*
045000041007      * Cararee - Carico le aree in gestione al distretto
045100041007      *---------------------------------------------------------------*
045200041007     c     Cararee       BegSr
045300041007
045400041007     c                   Clear                   car
045500041007
045600041007     c     DutDis        Setll     Azorg02l
045700041007do  1c                   Do        *Hival
045800041007     c     DutDis        Reade     Azorg02l
045900041007     c                   If        %Eof(Azorg02l)
046000041007     c                   Leave
046100041007     c                   EndIf
046200041007     c                   If        OrgFva <> *Blanks or
046300041007     c                             (OrgFag <> 'A' and OrgFag <> 'F')
046400041007     c                   Iter
046500041007     c                   EndIf
046600041007
046700041007     c                   Move      OrgCar        w003a
046800041007     c     w003a         Lookup    car                                    30
046900041007if  2c                   If        Not *In30
047000041007     c                   Eval      xx = 1
047100041007     c     *Blanks       Lookup    car(xx)                                30
047200041007if  3c                   If        *In30
047300041007     c                   Eval      car(xx) = w003a
047400041007e   3c                   EndIf
047500041007e   2c                   EndIf
047600041007
047700041007e   1c                   EndDo
047800041007
047900041007     c                   eval      v1car1 = car(1)
048000041007     c                   eval      v1car2 = car(2)
048100041007     c                   eval      v1car3 = car(3)
048200041007     c                   eval      v1car4 = car(4)
048300041007     c                   eval      v1car5 = car(5)
048400041007     c                   eval      v1car6 = car(6)
048500041007     c                   eval      v1car7 = car(7)
048600041007     c                   eval      v1car8 = car(8)
048700041007     c                   eval      v1car9 = car(9)
048800041007     c                   eval      v1car0 = car(10)
048900041007
049000041007     c                   EndSr
049100921012     C*
049200950720     C*--- INZSR INIZIALE --------------------------------------------*
049300940719     C     *INZSR        BEGSR
049400950720     C*
049500950720     C     *ENTRY        PLIST
049600950720     C                   PARM                    KPJBA
049700041007      *
049800041007     C     *dtaara       define    §azute        azuteds
049900041007     C     *dtaara       define    §datiute      ddatiute
050000041007      *
050100041007     C                   in(E)     *dtaara
050200041007     C                   IF        %Error or RSUT = *blanks
050300041007     C                   call      'TIBS34R'
050400041007     C                   parm                    Tibs34Ds
050500041007     C                   in        *dtaara
050600041007     C                   ENDIF
050700020517      *
050800020604     C     SIMFEL        IFNE      *zeros
050900020517     C                   MOVEL     DUTPOU        V1CFIL
051000020604     C                   ELSE
051100020604     C                   MOVE      *ZEROS        V1CFIL
051200020604     C                   ENDIF
051300020604     C
051400090701     c* Richiamo solo per avere codice di abilitazione
051500090701     c                   clear                   tntaa1ds
051600090701     c                   eval      itaa1caut='§utegtc'
051700090701     c                   clear                   kpjbu
051800090701     c                   movel     tntaa1ds      kpjbu
051900090703     c                   call      'TNTAA1C'
052000090701     c                   parm                    kpjba
052100090701     c                   movel     kpjbu         tntaa1ds
052200090701     C
052300090701     c* utente non abilitato alla gestione tariffe
052400090701     c                   if        otaa1fabi='N'
052500090701     c                   Seton                                        442808
052600090701     C                   movel     msg(6)        v1cmsg
052700090701     c                   endif
052800090701      *
052900090701     c                   Eval      prmabi = otaa1cabi
053000090701     c                   Eval      prmcdi = dutdis
053100090701     c                   Eval      prmcar = dutare
053200090701     c                   Eval      prmcpo = dutpou
053300940719     C***
053400940719     C* CHIAVI
053500940719     C***
053600130805      * - AZCMM01L
053700130805     c     K_azcmm01     klist
053800130805     c                   kfld                    k_CMMcod
053900130805     c     *like         define    CMMcod        k_CMMcod
054000940719     C*
054100090701     C                   ENDSR
054200940719     C*---------------------------------------------------------------*
054300950904**  MSG
054400130805Filiale  Inesistente                                                           1
054500940715Codice Commerciale non codificato                                              2
054600130805Filiale  Incongruente con Codice Commerciale                                   3
054700940715Data Errata                                                                    4
054800940719Per le tariffe in scadenza la data DAL deve essere 01/01/1901                  5
054900090701UTENTE NON ABILITATO ALLA GESTIONE DELLE TARIFFE                               6
055000130805Filiale non in gestione all'utente                                             7
055100090701Codice Commerciale non in gestione all'utente                                  8
055200130805Cod.Commerciale unificante Inesistente o Annullato                             9
055300130805Si puo' scegliere o Cod.Commerciale unificante o Filiale                      10
055400130805Si puo' scegliere o Cod.Commerciale unificante o Codice Commerciale           11
055500130805Filiale  immessa  non congruente  con aree selezionate                        12
055600130805Codice Commerciale immesso non congruente con aree selezionate                13
055700041007Codice Area non in gestione all'utente                                        14
055800130805Commerciale NON unificante                                                    15
