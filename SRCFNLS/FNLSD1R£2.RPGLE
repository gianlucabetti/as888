000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300030422     FFNBLP06L  IF   E           K DISK
000400030422     FAZORG01L  IF   E           K DISK
000500030414     FTABEL00F  IF   E           K DISK
000600030507     FFNLSD1P   O    E             PRINTER OFLIND(*IN01)
000700040430     FFNLSD1D   CF   E             WORKSTN
000800001218     D*--------------------
000900001218     D* DS ESTERNE
001000001218     D*--------------------
001100900517     D KPJBA         E DS
001200030512     D DS3A          E DS
001300061108     D DS1B          E DS
001400030422     D*--------------------
001500030422     D* SCHIERE DI MEMORIZZAZIONE
001600030422     D*--------------------
001700040430     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001800030512     D sk3A            s              2    DIM(999)
001900061108     D sk1B            s              2    DIM(99)
002000041227     D**!!! skPOPT          s              3  0 DIM(999)
002100030422     D skPO            s              3  0 DIM(999)
002200030422     D skPOD           s             15    DIM(999)
002300030422     D skPOCOL         s              3  0 DIM(999)
002400030423     D skPOCOLok       s              3  0 DIM(999)
002500030422     D skTA            s              3  0 DIM(999)
002600030423     D skMemDTP        s              7  3 DIM(999)
002700030423     D skMemNTP        s              7  3 DIM(999)
002800030423     D skMemDatPO      ds                  OCCURS(999)
002900030423     D   skMemDPO                     7  3 DIM(999)
003000030423     D   skMemNPO                     7  3 DIM(999)
003100030414     D*--------------------
003200030414     D* VARIABILI DI WRK
003300030414     D*--------------------
003400030422     D i               s              4  0
003500030422     D j               s              4  0
003600030422     D currCOL         s              3  0
003700030423     D depCOL          s              3  0
003800030423     D depDTP          s              5  1
003900030423     D depNTP          s              5  1
004000040430     D wdsd            s                   like(v1cdsd)
004100040430     D wdsa            s                   like(v1cdsa)
004200030414     D*---
004300030414     D* DS REPERIMENTO DATI UTENTE
004400030414     D*---
004500030414     D TIBS34DS      E DS
004600030414     D DDATIUTE      E DS
004700030414     D AZUTEDS       E DS                  extname(AZUTE00F)
004800030423     D*---
004900030423     D* DS REPERIMENTO £ire
005000030423     D*---
005100030423     D TRUL06DS      E DS
005200030423     D  L1                     1     90  0 DIM(30)
005300030521     D*------------------
005400030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500030521     D*------------------
005600030521     D WLBDA8          DS                  INZ
005700030521     D  G08DAT                 1      8  0
005800030521     D  G08INV                 9     16  0
005900030521     D  G08ERR                17     17
006000030521     D  G08TGI                18     22  0
006100001218
006200920812     C*---------------------------------------------------------------*
006300001218     C* MAIN
006400001218     C*---------------------------------------------------------------*
006500040430     c* Gestione videata richiesta data dal/al
006600040430     c                   clear                   v1cdsd
006700040430     c                   z-add     wn8           v1cdsa
006800040430     c                   clear                   v1cmsg
006900040430     c                   do        *hival
007000040430     c                   exfmt     LSD1D01
007100040430     c     *inkc         cabeq     *on           fine
007200040430     c                   exsr      chkd01
007300040430     c     *in28         ifeq      *off
007400040430     c     *inkf         andeq     *on
007500040430     c                   leave
007600040430     c                   endif
007700040430     c                   enddo
007800040430     c*
007900040430     c     wdsa          ifeq      *zeros
008000040430     c                   z-add     wdsd          wdsa
008100040430     c                   z-add     v1cdsd        v1cdsa
008200040430     c                   endif
008300040430     c****
008400001218     C                   exsr      procedi
008500030414     C                   exsr      stampa
008600001218     C*
008700040430     c     fine          tag
008800001218     C                   seton                                        LR
008900010509     C*---------------------------------------------------------------*
009000001218
009100040430     C*------------------------------------------------------------------------*
009200040430     C* CHKD01  - Controlli videata
009300040430     C*------------------------------------------------------------------------*
009400040430     C     CHKD01        BEGSR
009500040430     C*
009600040430     c                   setoff                                       284344
009700040430     c                   clear                   v1cmsg
009800040430     c* controllo data dal se immessa
009900040430     c                   clear                   wdsd
010000040430    1C     V1CDSD        IFGT      0
010100040430     C                   MOVE      V1CdSd        G08DAT
010200040430     C                   MOVEL     *ZERO         G08INV
010300040430     C                   MOVEL     *BLANKS       G08ERR
010400040430     C                   CALL      'XSRDA8'
010500040430     C                   PARM                    WLBDA8
010600040430     C     G08ERR        IFEQ      '1'
010700040430     C                   MOVEL     MSG(1)        V1CMSG
010800040430     C                   SETON                                        4328
010900040430     C                   GOTO      ENDCTR
011000040430     C                   ENDIF
011100040430     C                   Z-ADD     G08DAT        V1CDSD
011200040430     C                   Z-ADD     G08INV        WDSD
011300040430     c                   endif
011400040430     C*
011500040430     c* controllo data al se immessa
011600040430     c                   clear                   wdsa
011700040430    2C     V1CDSA        IFNE      0
011800040430     C                   MOVE      V1CDSA        G08DAT
011900040430     C                   MOVEL     *ZERO         G08INV
012000040430     C                   MOVEL     *BLANKS       G08ERR
012100040430     C                   CALL      'XSRDA8'
012200040430     C                   PARM                    WLBDA8
012300040430     C     G08ERR        IFEQ      '1'
012400040430     C                   MOVEL     MSG(1)        V1CMSG
012500040430     C                   SETON                                        4428
012600040430     C                   GOTO      ENDCTR
012700040430     C                   ENDIF
012800040430     C                   Z-ADD     G08DAT        V1CDSA
012900040430     C                   Z-ADD     G08INV        WDSA
013000040430     c                   endif
013100040430     C*
013200040430     c* almeno una delle due date ci deve essere
013300040430     c     v1cdsd        ifeq      *zeros
013400040430     c     v1cdsa        andeq     *zeros
013500040430     C                   MOVEL     MSG(3)        V1CMSG
013600040430     c                   seton                                        4328
013700040430     c                   goto      endctr
013800040430     c                   endif
013900040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
014000040430    3C     WDSD          IFGT      WDSA
014100040430     c     wdsa          andgt     *zeros
014200040430     C                   MOVEL     MSG(2)        V1CMSG
014300040430     C                   SETON                                        4328
014400040430     C                   GOTO      ENDCTR
014500040430    3C                   ENDIF
014600001218
014700040430     c     endctr        ENDSR
014800001218     C*------------------------------------------------------------------------*
014900001218     C* PROCEDI - Routine principale
015000001218     C*------------------------------------------------------------------------*
015100020613     C     PROCEDI       BEGSR
015200030423     C*
015300030423     C* Inizializzo le schiere di memorizzazione
015400030423     C                   clear                   skMemDTP
015500030423     C                   clear                   skMemNTP
015600030423     C                   z-add     1             i
015700030423     C                   dow       i <= %elem(skMemDatPO)
015800030423     C     i             occur     skMemDatPO
015900030423     C                   clear                   skMemDPO
016000030423     C                   clear                   skMemNPO
016100030423     C                   eval      i = i + 1
016200030423     C                   enddo
016300001218     C*
016400030422     C* Mi posiziono sul FLBLP06L per ciascun P.O. in gestione al T.P.
016500030422     C                   z-add     1             i
016600030422     C                   dow       i <= %elem(skPO)
016700030422     C                   if        skPO(i) > *zeros
016800030422     C                   eval      blpLNP = skPO(i)
016900030422     C     KEYblp06P     chain     fnblp06l
017000030422     C                   if        %found(fnblp06l)
017100030512     C*
017200030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
017300030512     C                   movel     'S'           recOK             1
017400030512     C*
017500030512     C* Eseguo routine di "check" bolla ok x elaborazione
017600030512     C                   exsr      chkrec
017700030512     C*
017800030512     C* Se record da elaborare...
017900030512     C                   if        recOK = 'S'
018000030422     C* Eseguo routine di memorizzazione dati
018100030422     C                   exsr      memdat
018200030512     C                   endif
018300030422     C* Continuo la lettura del fle FNBLP06L
018400030422     C     KEYblp06P     reade     fnblp06l
018500030422     C                   dow       not %eof(fnblp06l)
018600030512     C*
018700030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
018800030512     C                   movel     'S'           recOK
018900030512     C*
019000030512     C* Eseguo routine di "check" bolla ok x elaborazione
019100030512     C                   exsr      chkrec
019200030512     C*
019300030512     C* Se record da elaborare...
019400030512     C                   if        recOK = 'S'
019500030512     C* Eseguo routine di memorizzazione dati
019600030512     C                   exsr      memdat
019700030512     C                   endif
019800030422     C     KEYblp06P     reade     fnblp06l
019900030422     C                   enddo
020000030422     C                   endif
020100030422     C                   endif
020200030422     C                   eval      i = i +1
020300030422     C                   enddo
020400020613     C*
020500001218     C                   ENDSR
020600001218     C*------------------------------------------------------------------------*
020700030512
020800030512
020900030512
021000030512
021100030512     C*------------------------------------------------------------------------*
021200030512     C* CHKREC - Routine di verifica record da trattare
021300030512     C*------------------------------------------------------------------------*
021400030512     C     CHKREC        BEGSR
021500030521     C*
021600030521     C* Verifico che la data della bolla nn sia maggiore della data di lancio
021700040430     C**                 if        (blpAAS*10000)+blpMGS <= DATCOR
021800040430     C* Verifico che la data della bolla sia inclusa nel dal al richiesto   o
021900040430     C                   if        (blpAAS*10000)+blpMGS <= wdsa and
022000040430     c                             (blpaas*10000)+blpmgs >= wdsd
022100030521     C                   else
022200030521     C                   movel     'N'           recOK
022300030521     C                   endif
022400030512     C*
022500030512     C* Verifico se il tipo bolla è di quelli da elaborare
022600030512     C     blpCBO        lookup    sk3A                                   55
022700030512     C                   if        *in55 = *off
022800030512     C                   movel     'N'           recOK
022900030512     C                   endif
023000061108     C* Verifico se il cod.trattamento merce non è da elaborare
023100061108     C     blpCTM        lookup    sk1B                                   55
023200061108     C                   if        *in55 = *on
023300061108     C                   movel     'N'           recOK
023400061108     C                   endif
023500030512     C*
023600030512     C                   ENDSR
023700030512     C*---------------------------------------------------------------*
023800010626
023900010626
024000010626
024100010626
024200010509     C*------------------------------------------------------------------------*
024300030422     C* MEMDAT - Routine di memorizzazione dati
024400010509     C*------------------------------------------------------------------------*
024500030422     C     MEMDAT        BEGSR
024600010509     C*
024700030422     C* Memorizzo i dati x T.A. e P.O.
024800030422     C                   eval      skTA(blpTFA) = blpTFA
024900030423     C     skTA(blpTFA)  occur     skMemDatPO
025000030422     C*
025100030423     C* Evidenzio il volume merce "da partire" scindendo i servizi NON distribuz
025200030423     C                   eval      skMemDPO(blpLNP) = skMemDPO(blpLNP) + blpVLB
025300030423     C                   eval      skMemDTP(blpTFA) = skMemDTP(blpTFA) + blpVLB
025400030423     C                   if        blpTSP = 'D'
025500030423     C                   eval      skMemNPO(blpLNP) = skMemNPO(blpLNP) + blpVLB
025600030423     C                   eval      skMemNTP(blpTFA) = skMemNTP(blpTFA) + blpVLB
025700030423     C                   endif
025800010509     C*
025900010509     C                   ENDSR
026000010509     C*---------------------------------------------------------------*
026100030414
026200030414
026300030414
026400030414
026500030414     C*------------------------------------------------------------------------*
026600030422     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
026700030414     C*------------------------------------------------------------------------*
026800030414     C     STAMPA        BEGSR
026900030422     C*
027000030422     C* INNANZITUTTO ORDINO IN MODO CRESCENTE L'ELENCO DEI TERMINAL ARRIVO
027100030422     C                   sorta     skTA
027200030422     C*
027300030422     C* ABBINO LE COLONNE DI STAMPA AI P.O. IN GESTIONE AL TERMINAL PARTENZA
027400030422     C                   exsr      abbPOCOL
027500030423     C*
027600030423     C* STAMPO TESTATA
027700030423     C                   eval      stpSYF = RSUT
027800030507     C                   write     LSD1PT
027900090625     C  n20              write     LSD1PI
028000090625     C   20              write     LSD1PI15
028100090625     c                   setoff                                       01
028200030422     C*
028300030422     C* STAMPO DETTAGLIO X TERMINAL ARRIVO (solo se c'è qualcosa effettivamente
028400030422     C                   z-add     1             i
028500030422     C                   dow       i <= %elem(skTA)
028600030422     C*
028700030422     C* SOLO X I T.A. VALIDI
028800030422     C                   if        skTA(i) > *zeros
028900030423     C     skTA(i)       occur     skMemDatPO
029000030422     C*
029100030423     C* SOLO SE C'E' QUALCOSA DA STAMPARE
029200030423     C                   eval      depDTP = skMemDTP(skTA(i))
029300030423     C                   eval      depNTP = skMemNTP(skTA(i))
029400030423     C                   if        depDTP > *zeros OR
029500030423     C                             depNTP > *zeros
029600030422     C*
029700030422     C* STAMPO I TOTALI T.P. X T.A.
029800030507     C                   clear                   LSD1P2
029900030422     C                   eval      stpTA  = skTA(i)
030000030428     C                   eval(h)   stpDTP = skMemDTP(skTA(i))
030100030428     C                   eval(h)   stpNTP = skMemNTP(skTA(i))
030200041111     c                   eval      stpgDTP = stpgDTP + stpDTP
030300041111     c                   eval      stpgNTP = stpgNTP + stpNTP
030400030422     C                   z-add     1             j
030500030422     C                   dow       j <= %elem(skPO)
030600030422     C*
030700030422     C* SOLO X I P.O. VALIDI
030800030422     C                   if        skPO(j) > *zeros
030900030422     C                   if        skPOCOL(skPO(j))  > *zeros AND
031000030422     C                             skPOCOL(skPO(j)) <> 999
031100030422     C* REPERISCO LA COLONNA A CUI STAMPARE IL DATO CORRENTE
031200030422     C*
031300030422     C** Colonna 1
031400030422     C                   if        skPOCOL(skPO(j)) = 1
031500030428     C                   eval(h)   stpD01  = skMemDPO(skPO(j))
031600030428     C                   eval(h)   stpN01  = skMemNPO(skPO(j))
031700041111     c                   eval      stpgD01 = stpgD01 + stpD01
031800041111     c                   eval      stpgN01 = stpgN01 + stpN01
031900030422     C                   endif
032000030422     C** Colonna 2
032100030422     C                   if        skPOCOL(skPO(j)) = 2
032200030428     C                   eval(h)   stpD02  = skMemDPO(skPO(j))
032300030428     C                   eval(h)   stpN02  = skMemNPO(skPO(j))
032400041111     c                   eval      stpgD02 = stpgD02 + stpD02
032500041111     c                   eval      stpgN02 = stpgN02 + stpN02
032600030422     C                   endif
032700030422     C** Colonna 3
032800030422     C                   if        skPOCOL(skPO(j)) = 3
032900030428     C                   eval(h)   stpD03  = skMemDPO(skPO(j))
033000030428     C                   eval(h)   stpN03  = skMemNPO(skPO(j))
033100041111     c                   eval      stpgD03 = stpgD03 + stpD03
033200041111     c                   eval      stpgN03 = stpgN03 + stpN03
033300030422     C                   endif
033400030422     C** Colonna 4
033500030422     C                   if        skPOCOL(skPO(j)) = 4
033600030428     C                   eval(h)   stpD04  = skMemDPO(skPO(j))
033700030428     C                   eval(h)   stpN04  = skMemNPO(skPO(j))
033800041111     c                   eval      stpgD04 = stpgD04 + stpD04
033900041111     c                   eval      stpgN04 = stpgN04 + stpN04
034000030422     C                   endif
034100030422     C** Colonna 5
034200030422     C                   if        skPOCOL(skPO(j)) = 5
034300030428     C                   eval(h)   stpD05  = skMemDPO(skPO(j))
034400030428     C                   eval(h)   stpN05  = skMemNPO(skPO(j))
034500041111     c                   eval      stpgD05 = stpgD05 + stpD05
034600041111     c                   eval      stpgN05 = stpgN05 + stpN05
034700030422     C                   endif
034800030422     C** Colonna 6
034900030422     C                   if        skPOCOL(skPO(j)) = 6
035000030428     C                   eval(h)   stpD06  = skMemDPO(skPO(j))
035100030428     C                   eval(h)   stpN06  = skMemNPO(skPO(j))
035200041111     c                   eval      stpgD06 = stpgD06 + stpD06
035300041111     c                   eval      stpgN06 = stpgN06 + stpN06
035400030422     C                   endif
035500030422     C** Colonna 7
035600030422     C                   if        skPOCOL(skPO(j)) = 7
035700030428     C                   eval(h)   stpD07  = skMemDPO(skPO(j))
035800030428     C                   eval(h)   stpN07  = skMemNPO(skPO(j))
035900041111     c                   eval      stpgD07 = stpgD07 + stpD07
036000041111     c                   eval      stpgN07 = stpgN07 + stpN07
036100030422     C                   endif
036200030422     C** Colonna 8
036300030422     C                   if        skPOCOL(skPO(j)) = 8
036400030428     C                   eval(h)   stpD08  = skMemDPO(skPO(j))
036500030428     C                   eval(h)   stpN08  = skMemNPO(skPO(j))
036600041111     c                   eval      stpgD08 = stpgD08 + stpD08
036700041111     c                   eval      stpgN08 = stpgN08 + stpN08
036800030422     C                   endif
036900030422     C** Colonna 9
037000030422     C                   if        skPOCOL(skPO(j)) = 9
037100030428     C                   eval(h)   stpD09  = skMemDPO(skPO(j))
037200030428     C                   eval(h)   stpN09  = skMemNPO(skPO(j))
037300041111     c                   eval      stpgD09 = stpgD09 + stpD09
037400041111     c                   eval      stpgN09 = stpgN09 + stpN09
037500030422     C                   endif
037600090625     C** Colonna 10
037700090625     C                   if        skPOCOL(skPO(j)) = 10
037800090625     C                   eval(h)   stpD10  = skMemDPO(skPO(j))
037900090625     C                   eval(h)   stpN10  = skMemNPO(skPO(j))
038000090625     c                   eval      stpgD10 = stpgD10 + stpD10
038100090625     c                   eval      stpgN10 = stpgN10 + stpN10
038200090625     C                   endif
038300090625     C** Colonna 11
038400090625     C                   if        skPOCOL(skPO(j)) = 11
038500090625     C                   eval(h)   stpD11  = skMemDPO(skPO(j))
038600090625     C                   eval(h)   stpN11  = skMemNPO(skPO(j))
038700090625     c                   eval      stpgD11 = stpgD11 + stpD11
038800090625     c                   eval      stpgN11 = stpgN11 + stpN11
038900090625     C                   endif
039000090625     C** Colonna 12
039100090625     C                   if        skPOCOL(skPO(j)) = 12
039200090625     C                   eval(h)   stpD12  = skMemDPO(skPO(j))
039300090625     C                   eval(h)   stpN12  = skMemNPO(skPO(j))
039400090625     c                   eval      stpgD12 = stpgD12 + stpD12
039500090625     c                   eval      stpgN12 = stpgN12 + stpN12
039600090625     C                   endif
039700090625     C** Colonna 13
039800090625     C                   if        skPOCOL(skPO(j)) = 13
039900090625     C                   eval(h)   stpD13  = skMemDPO(skPO(j))
040000090625     C                   eval(h)   stpN13  = skMemNPO(skPO(j))
040100090625     c                   eval      stpgD13 = stpgD13 + stpD13
040200090625     c                   eval      stpgN13 = stpgN13 + stpN13
040300090625     C                   endif
040400090625     C** Colonna 14
040500090625     C                   if        skPOCOL(skPO(j)) = 14
040600090625     C                   eval(h)   stpD14  = skMemDPO(skPO(j))
040700090625     C                   eval(h)   stpN14  = skMemNPO(skPO(j))
040800090625     c                   eval      stpgD14 = stpgD14 + stpD14
040900090625     c                   eval      stpgN14 = stpgN14 + stpN14
041000090625     C                   endif
041100090625     C** Colonna 15
041200090625     C                   if        skPOCOL(skPO(j)) = 15
041300090625     C                   eval(h)   stpD15  = skMemDPO(skPO(j))
041400090625     C                   eval(h)   stpN15  = skMemNPO(skPO(j))
041500090625     c                   eval      stpgD15 = stpgD15 + stpD15
041600090625     c                   eval      stpgN15 = stpgN15 + stpN15
041700090625     C                   endif
041800090625      * se ho più di 15 colonne devo sommare tutti i p.o. rimanenti
041900041110      * sulla nona colonna
042000090625     c                   if        skPOCOL(skPO(j)) > 15
042100090625     C                   eval(h)   stpD15  = stpD15 + skMemDPO(skPO(j))
042200090625     C                   eval(h)   stpN15  = stpN15 + skMemNPO(skPO(j))
042300090625     c                   eval      stpgD15 = stpgD15 + stpD15
042400090625     c                   eval      stpgN15 = stpgN15 + stpN15
042500041110     C                   endif
042600030422     C*
042700030422     C                   endif
042800030422     C                   endif
042900030422     C*
043000030422     C* SCORRO L'ELENCO P.O. X T.A. CORRENTE
043100030422     C                   eval      j = j + 1
043200030422     C                   enddo
043300030422     C*
043400090625     c
043500090625     c* testata
043600090625     c                   if        *in01
043700090625     C                   write     LSD1PT
043800090625     C  n20              write     LSD1PI
043900090625     C   20              write     LSD1PI15
044000090625     c                   setoff                                       01
044100090625     c                   endif
044200090625     c
044300090625     C* STAMPO LA RIGA DI DETTAGLIO
044400090625     C  n20              write     LSD1P2
044500090625     C   20              write     LSD1P215
044600030422     C                   endif
044700030422     C                   endif
044800030422     C*
044900030422     C* SCORRO L'ELENCO DEI T.A.
045000030422     C                   eval      i = i + 1
045100030422     C                   enddo
045200041111     C*
045300041111     C* STAMPO TOTALI
045400090625     c                   write     LSD1RI
045500090625     C  n20              write     LSD1TG
045600090625     C   20              write     LSD1TG15
045700030422     C*
045800030422     C* STAMPO CHIUSURA PROSPETTO
045900030507     C                   write     LSD1RI
046000030507     C                   write     LSD1FI
046100030414     C*
046200030414     C                   ENDSR
046300030414     C*------------------------------------------------------------------------*
046400030422
046500030422
046600030422
046700030422
046800030422     C*------------------------------------------------------------------------*
046900030422     C* ABBPOCOL -  Routine di abbinamento P.O. a colonne in stampa
047000030422     C*------------------------------------------------------------------------*
047100030422     C     ABBPOCOL      BEGSR
047200030423     C*
047300030423     C                   setoff                                       111213
047400030423     C                   setoff                                       141516
047500030423     C                   setoff                                       171819
047600090625     C                   setoff                                       20
047700030422     C*
047800030422     C* Innanzitutto ordino in modo crescente l'elenco dei P.O.
047900030422     C                   sorta     skPO
048000030423     C*
048100030423     C* Di default abbino alla 1° colonna il P.O. terminal di partenza e alle su
048200030423     C* gli altri P.O. in ordine crescente
048300030423     C                   clear                   skPOCOL
048400030423     C                   z-add     1             currCOL
048500030423     C                   eval      skPOCOL(SIMFEL) = currCOL
048600030423     C                   z-add     1             i
048700030423     C                   dow       i <= %elem(skPO)
048800030423     C                   if        skPO(i)  > *zeros AND
048900030423     C                             skPO(i) <> SIMFEL
049000030423     C                   eval      currCOL = currCOL + 1
049100030423     C                   eval      skPOCOL(skPO(i)) = currCOL
049200030423     C                   endif
049300030423     C                   eval      i = i + 1
049400030423     C                   enddo
049500030422     C*
049600030423     C* Eseguo verifica valorizzazioni x T.A.
049700030423     C                   clear                   skPOCOLok
049800030422     C*
049900030422     C                   z-add     1             i
050000030422     C                   dow       i <= %elem(skTA)
050100030422     C                   if        skTA(i) > *zeros
050200030423     C     skTA(i)       occur     skMemDatPO
050300030422     C                   z-add     1             j
050400030422     C                   dow       j <= %elem(skPO)
050500030422     C*
050600030422     C* Solo x i P.O. validi
050700030422     C                   if        skPO(j) > *zeros
050800030422     C*
050900030423     C* Solo se ci sono dati da stampare
051000030423     C                   eval      depDTP = skMemDPO(skPO(j))
051100030423     C                   eval      depNTP = skMemNPO(skPO(j))
051200030423     C                   if        depDTP > *zeros OR
051300030423     C                             depNTP > *zeros
051400030423     C                   if        skPOCOLok(skPO(j)) = *zeros
051500030422     C*
051600030423     C* Flaggo a OK la colonna relativa al P.O. da stampare
051700030423     C                   eval      skPOCOLok(skPO(j)) = 999
051800030423     C                   endif
051900030423     C                   endif
052000030423     C                   endif
052100030423     C                   eval      j = j + 1
052200030423     C                   enddo
052300030423     C                   endif
052400030423     C                   eval      i = i + 1
052500030423     C                   enddo
052600030423     C*
052700030423     C* A questo punto "mergio" le collonne di default con quelle effettivamente
052800030423     C                   z-add     1             i
052900030423     C                   dow       i <= %elem(skPO)
053000030423     C                   if        skPO(i) > *zeros
053100030423     C                   if        skPOCOLok(skPO(i)) = *zeros
053200030423     C                   eval      depCOL = skPOCOL(skPO(i))
053300030423     C* Diminuisco di 1 le posizioni >= alla colonna relativa al P.O. da nn stam
053400030423     C                   z-add     1             j
053500030423     C                   dow       j <= %elem(skPOCOL)
053600030423     C                   if        skPOCOL(j) = depCOL
053700030423     C                   eval      skPOCOL(j) = *zeros
053800030423     C                   endif
053900030423     C                   if        skPOCOL(j) > depCOL
054000030423     C                   eval      skPOCOL(j) = skPOCOL(j) - 1
054100030423     C                   endif
054200030423     C                   eval      j = j + 1
054300030423     C                   enddo
054400030423     C                   endif
054500030423     C                   endif
054600030423     C                   eval      i = i + 1
054700030423     C                   enddo
054800030423     C*
054900030423     C* Effettuo 1 ultimo giro x valorizzare le intestazioni delle colonne da st
055000030423     C                   z-add     1             j
055100030423     C                   dow       j <= %elem(skPO)
055200030423     C                   if        skPO(j) > *zeros
055300030423     C                   eval      currCOL = skPOCOL(skPO(j))
055400030423     C*
055500030423     C                   if        currCOL = 1
055600030423     C                   seton                                        11
055700030423     C                   eval      stpP01  = skPO(j)
055800030423     C                   eval      stpP01D = skPOD(skPO(j))
055900030423     C                   endif
056000030423     C                   if        currCOL = 2
056100030423     C                   seton                                        12
056200030423     C                   eval      stpP02  = skPO(j)
056300030423     C                   eval      stpP02D = skPOD(skPO(j))
056400030423     C                   endif
056500030423     C                   if        currCOL = 3
056600030423     C                   seton                                        13
056700030423     C                   eval      stpP03  = skPO(j)
056800030423     C                   eval      stpP03D = skPOD(skPO(j))
056900030423     C                   endif
057000030423     C                   if        currCOL = 4
057100030423     C                   seton                                        14
057200030423     C                   eval      stpP04  = skPO(j)
057300030423     C                   eval      stpP04D = skPOD(skPO(j))
057400030423     C                   endif
057500030423     C                   if        currCOL = 5
057600030423     C                   seton                                        15
057700030423     C                   eval      stpP05  = skPO(j)
057800030423     C                   eval      stpP05D = skPOD(skPO(j))
057900030423     C                   endif
058000030423     C                   if        currCOL = 6
058100030423     C                   seton                                        16
058200030423     C                   eval      stpP06  = skPO(j)
058300030423     C                   eval      stpP06D = skPOD(skPO(j))
058400030423     C                   endif
058500090625     C                   if        currCOL = 7
058600030423     C                   seton                                        17
058700030423     C                   eval      stpP07  = skPO(j)
058800030423     C                   eval      stpP07D = skPOD(skPO(j))
058900030423     C                   endif
059000030423     C                   if        currCOL = 8
059100030423     C                   seton                                        18
059200030423     C                   eval      stpP08  = skPO(j)
059300030423     C                   eval      stpP08D = skPOD(skPO(j))
059400030423     C                   endif
059500030423     C                   if        currCOL = 9
059600030423     C                   seton                                        19
059700030423     C                   eval      stpP09  = skPO(j)
059800030423     C                   eval      stpP09D = skPOD(skPO(j))
059900030423     C                   endif
060000090625     C                   if        currCOL = 10
060100090625     C                   seton                                        20
060200090625     C                   eval      stpP10  = skPO(j)
060300090625     C                   eval      stpP10D = skPOD(skPO(j))
060400090625     C                   endif
060500090625     C                   if        currCOL = 11
060600090625     C                   seton                                        20
060700090625     C                   eval      stpP11  = skPO(j)
060800090625     C                   eval      stpP11D = skPOD(skPO(j))
060900090625     C                   endif
061000090625     C                   if        currCOL = 12
061100090625     C                   seton                                        20
061200090625     C                   eval      stpP12  = skPO(j)
061300090625     C                   eval      stpP12D = skPOD(skPO(j))
061400090625     C                   endif
061500090625     C                   if        currCOL = 13
061600090625     C                   seton                                        20
061700090625     C                   eval      stpP13  = skPO(j)
061800090625     C                   eval      stpP13D = skPOD(skPO(j))
061900090625     C                   endif
062000090625     C                   if        currCOL = 14
062100090625     C                   seton                                        20
062200090625     C                   eval      stpP14  = skPO(j)
062300090625     C                   eval      stpP14D = skPOD(skPO(j))
062400090625     C                   endif
062500090625     C                   if        currCOL = 15
062600090625     C                   seton                                        20
062700090625     C                   eval      stpP15  = skPO(j)
062800090625     C                   eval      stpP15D = skPOD(skPO(j))
062900090625     C                   endif
063000090625      * se ho più di 15 p.o. devo intestare la colonna con 'ALTRI'
063100090625     c                   If        currCOL > 15
063200090625     C                   clear                   stpP15
063300090625     C                   eval      stpP15D = 'ALTRI'
063400041110     C                   endif
063500030422     C*
063600030423     C                   endif
063700030423     C                   eval      j = j + 1
063800030423     C                   enddo
063900030423     C*
064000030422     C                   ENDSR
064100030422     C*---------------------------------------------------------------*
064200030414
064300030414
064400030414
064500030414     C*---------------------------------------------------------------*
064600030414     C* CARTAB - Caricamento tabelle di wrk
064700030414     C*---------------------------------------------------------------*
064800030414     C     CARTAB        BEGSR
064900030423     C*
065000030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
065100030423     C                   MOVEL     '£1'          D06COD
065200030423     C                   MOVEL     SIMFEL        D06KEY
065300030423     C                   MOVEL     'L'           D06TLA
065400030423     C                   MOVEL     *BLANKS       D06ESC
065500030423     C                   MOVEL(P)  TRUL06DS      KPJBU
065600030423     C                   CALL      'TRUL06R'
065700030423     C                   PARM                    KPJBA
065800030423     C                   MOVEL     KPJBU         TRUL06DS
065900030414     C*
066000030423     C* Reperisco le rispettive denominazioni dei P.O.
066100030422     C                   clear                   skPO
066200030422     C                   clear                   skPOD
066300030423     C     1             do        30            i
066400030423     C                   if        L1(i) > *zeros
066500030423     C                   eval      skPO(L1(i)) = L1(i)
066600030423     C** Reperisco le descrizioni di ogni P.O.
066700030423     C     L1(i)         chain     azorg01l
066800030423     C                   if        %found(azorg01l)
066900030423     C                   eval      skPOD(L1(i)) = orgDES
067000030423     C                   else
067100030423     C                   eval      skPOD(L1(i)) = *all'*'
067200030423     C                   endif
067300030423     C                   endif
067400030512     C**
067500030512     C                   enddo
067600030512     C*
067700030512     C* Carico i tipi bolla dalla tabella 3A
067800030512     C                   z-add     *zeros        i
067900030512     C                   eval      tblCOD = '3A'
068000030512     C     KEYtabP       setll     tabel00f
068100030512     C     KEYtabP       reade     tabel00f
068200030512     C                   dow       not %eof(tabel00f)
068300030512     C                   movel(p)  tblUNI        DS3A
068400030512     C                   if        §3ARBL <> 'R'
068500030512     C                   add       1             i
068600030512     C                   eval      sk3A(i) = tblKEY
068700030512     C                   endif
068800030512     C     KEYtabP       reade     tabel00f
068900030512     C                   enddo
069000061108     C*
069100061108     C* Carico i tipi bolla dalla tabella 1b
069200061108     C                   z-add     *zeros        i
069300061108     C                   eval      tblCOD = '1B'
069400061108     C     KEYtabP       setll     tabel00f
069500061108     C     KEYtabP       reade     tabel00f
069600061108     C                   dow       not %eof(tabel00f)
069700061108     C                   movel(p)  tblUNI        DS1B
069800061108     C                   if        §1bfg8='N'
069900061108     C                   add       1             i
070000061108     C                   eval      sk1b(i) = tblKEY
070100061108     C                   endif
070200061108     C     KEYtabP       reade     tabel00f
070300061108     C                   enddo
070400030414     C*
070500030414     C                   ENDSR
070600030414     C*------------------------------------------------------------------------*
070700030414     C* REPERISCE I DATI UTENTE
070800030414     C*------------------------------------------------------------------------*
070900030414     C     REPDATIUTE    BEGSR
071000030414     C*
071100030414     C* INIZIALIZZA VARIABILI DI WRK
071200030414     C                   CLEAR                   TIBS34DS
071300030414     C                   CLEAR                   AZUTEDS
071400030414     C                   CLEAR                   DDATIUTE
071500030414     C*
071600030414     C     *DTAARA       DEFINE    §azute        azuteds
071700030414     C     *DTAARA       DEFINE    §datiute      ddatiute
071800030414     C                   IN(E)     *DTAARA
071900030422     C                   IF        %Error
072000030422     C                   EVAL      I34Tla = 'L'
072100030414     C                   CALL      'TIBS34R'
072200030414     C                   PARM                    Tibs34Ds
072300030414     C                   IN        *DTAARA
072400030422     C                   ENDIF
072500030414     C*
072600030414     C                   ENDSR
072700030414     C*------------------------------------------------------------------------*
072800010509
072900010509
073000001218
073100001218     C*------------------------------------------------------------------------*
073200001218     C* *INZSR - ROUTINE INIZIALE
073300001218     C*------------------------------------------------------------------------*
073400001218     C     *INZSR        BEGSR
073500001218     C*
073600030422     C* Ricevo in la DS di architettura
073700010726     C     *ENTRY        PLIST
073800020613     C                   PARM                    KPJBA
073900030521     C*
074000030521     C* Reperisco l'UDATE del job
074100030521     C                   TIME                    WN14             14 0
074200030521     C                   MOVE      WN14          WN8               8 0
074300030521     C                   Z-ADD     WN8           G08DAT
074400030521     C                   Z-ADD     *zeros        G08INV
074500030521     C                   MOVEL     '0'           G08ERR
074600030521     C                   CALL      'XSRDA8'
074700030521     C                   PARM                    WLBDA8
074800030521     C                   Z-ADD     G08INV        DATCOR            8 0
074900010726     C*
075000030422     C* Inizializzo campi chiave "fissi"
075100030414     C                   Z-ADD     1             tblKUT
075200030422     C                   Z-ADD     *zeros        blpDUC
075300030414     C****
075400030414     C* Definizione chiavi
075500030414     C****
075600030512     C* Chiave su TABEL00F - parziale
075700030512     C     KEYtabP       KLIST
075800030414     C                   KFLD                    tblKUT
075900030414     C                   KFLD                    tblCOD
076000030422     C* Chiave su FNBLP06L - parziale
076100030422     C     KEYblp06P     KLIST
076200030422     C                   KFLD                    blpLNP
076300030422     C                   KFLD                    blpDUC
076400030414     C*
076500030414     C* REPERIMEMTO DATI UTENTE
076600030414     C                   EXSR      REPDATIUTE
076700030422     C*
076800030422     C* CARICAMENTO TABELLE DI WRK
076900030422     C                   EXSR      CARTAB
077000001218     C*
077100001218     C                   ENDSR
077200040430** MSG
077300040430Data errata                                                                   1
077400040430La data spedizione DAL non puo' essere maggiore della data AL                 2
077500040430Immettere almeno una delle due date                                           3
