000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130205     Fazcae05L  IF   E           K DISK
000400130206     FFNBLP05L  IF   E           K DISK    UsrOpn extfile(wFNBLP)
000500030422     FAZORG01L  IF   E           K DISK
000600030414     FTABEL00F  IF   E           K DISK
000700130205     FFNLSD9P   O    E             PRINTER OFLIND(*IN01)
000800130206     fWFSAT00F  o    e             disk    UsrOpn extfile(wSAT)
000900001218     D*--------------------
001000001218     D* DS ESTERNE
001100001218     D*--------------------
001200900517     D KPJBA         E DS
001300030512     D DS3A          E DS
001400061108     D DS1B          E DS
001500030422     D*--------------------
001600030422     D* SCHIERE DI MEMORIZZAZIONE
001700030422     D*--------------------
001800030512     D sk3A            s              2    DIM(999)
001900061108     D sk1B            s              2    DIM(99)
002000030422     D skPO            s              3  0 DIM(999)
002100030422     D skPOD           s             15    DIM(999)
002200030422     D skPOCOL         s              3  0 DIM(999)
002300030423     D skPOCOLok       s              3  0 DIM(999)
002400030422     D skTA            s              3  0 DIM(999)
002500030423     D skMemDTP        s              7  3 DIM(999)
002600030423     D skMemNTP        s              7  3 DIM(999)
002700030423     D skMemDatPO      ds                  OCCURS(999)
002800030423     D   skMemDPO                     7  3 DIM(999)
002900030423     D   skMemNPO                     7  3 DIM(999)
003000030414     D*--------------------
003100030414     D* VARIABILI DI WRK
003200030414     D*--------------------
003300030422     D i               s              4  0
003400030422     D j               s              4  0
003500030422     D currCOL         s              3  0
003600030423     D depCOL          s              3  0
003700030423     D depDTP          s              5  1
003800030423     D depNTP          s              5  1
003900040430     D wdsd            s                   like(v1cdsd)
004000040430     D wdsa            s                   like(v1cdsa)
004100130205     D kepa            s                   like(caeepa)
004200130205     D ktfp            s                   like(caetfp)
004300130206     d Dataiso         s               d   datfmt(*iso)
004400130206     d Dataeur         s               d   datfmt(*eur)
004500030414     D*---
004600030414     D* DS REPERIMENTO DATI UTENTE
004700030414     D*---
004800030414     D TIBS34DS      E DS
004900030414     D DDATIUTE      E DS
005000030414     D AZUTEDS       E DS                  extname(AZUTE00F)
005100030423     D*---
005200030423     D* DS REPERIMENTO £ire
005300030423     D*---
005400030423     D TRUL06DS      E DS
005500030423     D  L1                     1     90  0 DIM(30)
005600030521     D*------------------
005700030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800030521     D*------------------
005900030521     D WLBDA8          DS                  INZ
006000030521     D  G08DAT                 1      8  0
006100030521     D  G08INV                 9     16  0
006200030521     D  G08ERR                17     17
006300030521     D  G08TGI                18     22  0
006400130205     D*------------------
006500130205     D FNLSD9DS        DS
006600130205     D  ilsd9dsd               1      8  0
006700130205     D  ilsd9dsa               9     16  0
006800130205     d* S=stampa F=file E=entrambi
006900130205     D  ilsd9RIC              17     17
007000130205     D  ilsd9TFP              18     20  0
007100001218
007200130206     d wFNBLP          s             21    inz('FILTRA201 /FNBLP05L')
007300130206     d wSAT            s             21    inz('GAITRAAZM /WFSAT00F')
007400920812     C*---------------------------------------------------------------*
007500001218     C* MAIN
007600001218     C*---------------------------------------------------------------*
007700130205     c
007800130205     c* Elaboro terminal per terminal alla data  AL oppure il terminal
007900130205     c*  richiesto se solo uno
008000130205     C                   exsr      CAR_LNP
008100130205     c
008200001218     C*
008300001218     C                   seton                                        LR
008400001218
008500001218     C*------------------------------------------------------------------------*
008600001218     C* PROCEDI - Routine principale
008700001218     C*------------------------------------------------------------------------*
008800020613     C     PROCEDI       BEGSR
008900130205     c
009000030423     C* Inizializzo le schiere di memorizzazione
009100030423     C                   clear                   skMemDTP
009200030423     C                   clear                   skMemNTP
009300030423     C                   z-add     1             i
009400030423     C                   dow       i <= %elem(skMemDatPO)
009500030423     C     i             occur     skMemDatPO
009600030423     C                   clear                   skMemDPO
009700030423     C                   clear                   skMemNPO
009800030423     C                   eval      i = i + 1
009900030423     C                   enddo
010000001218     C*
010100130205     C* Mi posiziono sul FNBLP05L per ciascun P.O. in gestione al T.P.
010200030422     C                   z-add     1             i
010300030422     C                   dow       i <= %elem(skPO)
010400030422     C                   if        skPO(i) > *zeros
010500030422     C                   eval      blpLNP = skPO(i)
010600130205     c
010700130205     C     KEYblp05P     setll     fnblp05l
010800130205     C     KEYblp05P     reade     fnblp05l
010900130205     c
011000101126     C                   dow       not %eof(fnblp05l)
011100030512     C*
011200030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
011300130205     C                   movel     'S'           recOK             1
011400030512     C*
011500030512     C* Eseguo routine di "check" bolla ok x elaborazione
011600030512     C                   exsr      chkrec
011700030512     C*
011800030512     C* Se record da elaborare...
011900030512     C                   if        recOK = 'S'
012000030512     C* Eseguo routine di memorizzazione dati
012100030512     C                   exsr      memdat
012200030512     C                   endif
012300130205     c
012400101126     C     KEYblp05P     reade     fnblp05l
012500030422     C                   enddo
012600030422     C                   endif
012700130205     c
012800030422     C                   eval      i = i +1
012900030422     C                   enddo
013000020613     C*
013100130206     C                   exsr      stampa
013200130206     C*
013300001218     C                   ENDSR
013400001218     C*------------------------------------------------------------------------*
013500030512
013600030512
013700030512     C*------------------------------------------------------------------------*
013800030512     C* CHKREC - Routine di verifica record da trattare
013900030512     C*------------------------------------------------------------------------*
014000030512     C     CHKREC        BEGSR
014100030521     C*
014200040430     C* Verifico che la data della bolla sia inclusa nel dal al richiesto   o
014300130205     C                   if        (blpAAS*10000)+blpMGS <= ilsd9dsa and
014400130205     c                             (blpaas*10000)+blpmgs >= ilsd9dsd
014500030521     C                   else
014600030521     C                   movel     'N'           recOK
014700030521     C                   endif
014800030512     C*
014900030512     C* Verifico se il tipo bolla è di quelli da elaborare
015000030512     C     blpCBO        lookup    sk3A                                   55
015100030512     C                   if        *in55 = *off
015200030512     C                   movel     'N'           recOK
015300030512     C                   endif
015400061108     C* Verifico se il cod.trattamento merce non è da elaborare
015500061108     C     blpCTM        lookup    sk1B                                   55
015600061108     C                   if        *in55 = *on
015700061108     C                   movel     'N'           recOK
015800061108     C                   endif
015900030512     C*
016000030512     C                   ENDSR
016100030512     C*---------------------------------------------------------------*
016200010626
016300010626
016400010626
016500010626
016600010509     C*------------------------------------------------------------------------*
016700030422     C* MEMDAT - Routine di memorizzazione dati
016800010509     C*------------------------------------------------------------------------*
016900030422     C     MEMDAT        BEGSR
017000010509     C*
017100030422     C* Memorizzo i dati x T.A. e P.O.
017200030422     C                   eval      skTA(blpTFA) = blpTFA
017300030423     C     skTA(blpTFA)  occur     skMemDatPO
017400130205     c
017500030423     C* Evidenzio il volume merce "da partire" scindendo i servizi NON distribuz
017600030423     C                   eval      skMemDPO(blpLNP) = skMemDPO(blpLNP) + blpVLB
017700030423     C                   eval      skMemDTP(blpTFA) = skMemDTP(blpTFA) + blpVLB
017800030423     C                   if        blpTSP = 'D'
017900030423     C                   eval      skMemNPO(blpLNP) = skMemNPO(blpLNP) + blpVLB
018000030423     C                   eval      skMemNTP(blpTFA) = skMemNTP(blpTFA) + blpVLB
018100030423     C                   endif
018200010509     C*
018300010509     C                   ENDSR
018400010509     C*---------------------------------------------------------------*
018500030414
018600030414
018700030414
018800030414
018900030414     C*------------------------------------------------------------------------*
019000030422     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
019100030414     C*------------------------------------------------------------------------*
019200030414     C     STAMPA        BEGSR
019300030422     C*
019400030422     C* INNANZITUTTO ORDINO IN MODO CRESCENTE L'ELENCO DEI TERMINAL ARRIVO
019500030422     C                   sorta     skTA
019600030422     C*
019700030422     C* ABBINO LE COLONNE DI STAMPA AI P.O. IN GESTIONE AL TERMINAL PARTENZA
019800030422     C                   exsr      abbPOCOL
019900130205     c
020000130205     c                   if        ilsd9RIC<>'F'
020100130206     c                   clear                   stpD01
020200130206     c                   clear                   stpN01
020300130206     c                   clear                   stpgD01
020400130206     c                   clear                   stpgN01
020500130206     c                   clear                   stpD02
020600130206     c                   clear                   stpN02
020700130206     c                   clear                   stpgD02
020800130206     c                   clear                   stpgN02
020900130206     c                   clear                   stpD03
021000130206     c                   clear                   stpN03
021100130206     c                   clear                   stpgD03
021200130206     c                   clear                   stpgN03
021300130206     c                   clear                   stpD04
021400130206     c                   clear                   stpN04
021500130206     c                   clear                   stpgD04
021600130206     c                   clear                   stpgN04
021700130206     c                   clear                   stpD05
021800130206     c                   clear                   stpN05
021900130206     c                   clear                   stpgD05
022000130206     c                   clear                   stpgN05
022100130206     c                   clear                   stpD06
022200130206     c                   clear                   stpN06
022300130206     c                   clear                   stpgD06
022400130206     c                   clear                   stpgN06
022500130206     c                   clear                   stpD07
022600130206     c                   clear                   stpN07
022700130206     c                   clear                   stpgD07
022800130206     c                   clear                   stpgN07
022900130206     c                   clear                   stpD08
023000130206     c                   clear                   stpN08
023100130206     c                   clear                   stpgD08
023200130206     c                   clear                   stpgN08
023300130206     c                   clear                   stpD09
023400130206     c                   clear                   stpN09
023500130206     c                   clear                   stpgD09
023600130206     c                   clear                   stpgN09
023700130206     c                   clear                   stpD10
023800130206     c                   clear                   stpN10
023900130206     c                   clear                   stpgD10
024000130206     c                   clear                   stpgN10
024100130206     c                   clear                   stpD11
024200130206     c                   clear                   stpN11
024300130206     c                   clear                   stpgD11
024400130206     c                   clear                   stpgN11
024500130206     c                   clear                   stpD12
024600130206     c                   clear                   stpN12
024700130206     c                   clear                   stpgD12
024800130206     c                   clear                   stpgN12
024900130206     c                   clear                   stpD13
025000130206     c                   clear                   stpN13
025100130206     c                   clear                   stpgD13
025200130206     c                   clear                   stpgN13
025300130206     c                   clear                   stpD14
025400130206     c                   clear                   stpN14
025500130206     c                   clear                   stpgD14
025600130206     c                   clear                   stpgN14
025700130206     c                   clear                   stpD15
025800130206     c                   clear                   stpN15
025900130206     c                   clear                   stpgD15
026000130206     c                   clear                   stpgN15
026100130206     c                   clear                   depdtp
026200130206     c                   clear                   depntp
026300130206     c                   clear                   stpdtp
026400130206     c                   clear                   stpntp
026500130206     c                   clear                   stpgdtp
026600130206     c                   clear                   stpgNtp
026700030423     C*
026800030423     C* STAMPO TESTATA
026900030423     C                   eval      stpSYF = RSUT
027000030507     C                   write     LSD1PT
027100090625     C  n20              write     LSD1PI
027200090625     C   20              write     LSD1PI15
027300090625     c                   setoff                                       01
027400130205     c                   endif
027500030422     C*
027600030422     C* STAMPO DETTAGLIO X TERMINAL ARRIVO (solo se c'è qualcosa effettivamente
027700030422     C                   z-add     1             i
027800030422     C                   dow       i <= %elem(skTA)
027900030422     C*
028000030422     C* SOLO X I T.A. VALIDI
028100030422     C                   if        skTA(i) > *zeros
028200030423     C     skTA(i)       occur     skMemDatPO
028300030422     C*
028400030423     C* SOLO SE C'E' QUALCOSA DA STAMPARE
028500130205     C                   eval      depDTP = skMemDTP(skTA(i))
028600030423     C                   eval      depNTP = skMemNTP(skTA(i))
028700030423     C                   if        depDTP > *zeros OR
028800030423     C                             depNTP > *zeros
028900030422     C*
029000030422     C* STAMPO I TOTALI T.P. X T.A.
029100030507     C                   clear                   LSD1P2
029200030422     C                   eval      stpTA  = skTA(i)
029300030428     C                   eval(h)   stpDTP = skMemDTP(skTA(i))
029400030428     C                   eval(h)   stpNTP = skMemNTP(skTA(i))
029500041111     c                   eval      stpgDTP = stpgDTP + stpDTP
029600041111     c                   eval      stpgNTP = stpgNTP + stpNTP
029700130205     c
029800130205     c                   if        ilsd9ric<>'S'
029900130205     c                   clear                   wfsat000
030000130205     c                   eval      sattfp=ktfp
030100130205     c                   eval      sattfa=skTa(i)
030200130205     c                   eval      satdre=ilsd9dsa
030300130205     c                   eval      sattre=' '
030400130205     c                   eval      satvpt=skMemDTP(skTA(i))
030500130205     c                   eval      satvpd=skMemNTP(skTA(i))
030600130205     c                   write     wfsat000
030700130205     C                   endif
030800130205     c
030900130205     c* Se richiesta anche stampa
031000130205     c                   if        ilsd9RIC<>'F'
031100130205     c
031200030422     C                   z-add     1             j
031300030422     C                   dow       j <= %elem(skPO)
031400030422     C*
031500030422     C* SOLO X I P.O. VALIDI
031600030422     C                   if        skPO(j) > *zeros
031700030422     C                   if        skPOCOL(skPO(j))  > *zeros AND
031800030422     C                             skPOCOL(skPO(j)) <> 999
031900030422     C* REPERISCO LA COLONNA A CUI STAMPARE IL DATO CORRENTE
032000030422     C*
032100030422     C** Colonna 1
032200030422     C                   if        skPOCOL(skPO(j)) = 1
032300030428     C                   eval(h)   stpD01  = skMemDPO(skPO(j))
032400030428     C                   eval(h)   stpN01  = skMemNPO(skPO(j))
032500041111     c                   eval      stpgD01 = stpgD01 + stpD01
032600041111     c                   eval      stpgN01 = stpgN01 + stpN01
032700130205     c* Se richeisto scrivo anche il file
032800130205     C                   endif
032900030422     C** Colonna 2
033000030422     C                   if        skPOCOL(skPO(j)) = 2
033100030428     C                   eval(h)   stpD02  = skMemDPO(skPO(j))
033200030428     C                   eval(h)   stpN02  = skMemNPO(skPO(j))
033300041111     c                   eval      stpgD02 = stpgD02 + stpD02
033400041111     c                   eval      stpgN02 = stpgN02 + stpN02
033500030422     C                   endif
033600030422     C** Colonna 3
033700030422     C                   if        skPOCOL(skPO(j)) = 3
033800030428     C                   eval(h)   stpD03  = skMemDPO(skPO(j))
033900030428     C                   eval(h)   stpN03  = skMemNPO(skPO(j))
034000041111     c                   eval      stpgD03 = stpgD03 + stpD03
034100041111     c                   eval      stpgN03 = stpgN03 + stpN03
034200030422     C                   endif
034300030422     C** Colonna 4
034400030422     C                   if        skPOCOL(skPO(j)) = 4
034500030428     C                   eval(h)   stpD04  = skMemDPO(skPO(j))
034600030428     C                   eval(h)   stpN04  = skMemNPO(skPO(j))
034700041111     c                   eval      stpgD04 = stpgD04 + stpD04
034800041111     c                   eval      stpgN04 = stpgN04 + stpN04
034900030422     C                   endif
035000030422     C** Colonna 5
035100030422     C                   if        skPOCOL(skPO(j)) = 5
035200030428     C                   eval(h)   stpD05  = skMemDPO(skPO(j))
035300030428     C                   eval(h)   stpN05  = skMemNPO(skPO(j))
035400041111     c                   eval      stpgD05 = stpgD05 + stpD05
035500041111     c                   eval      stpgN05 = stpgN05 + stpN05
035600030422     C                   endif
035700030422     C** Colonna 6
035800030422     C                   if        skPOCOL(skPO(j)) = 6
035900030428     C                   eval(h)   stpD06  = skMemDPO(skPO(j))
036000030428     C                   eval(h)   stpN06  = skMemNPO(skPO(j))
036100041111     c                   eval      stpgD06 = stpgD06 + stpD06
036200041111     c                   eval      stpgN06 = stpgN06 + stpN06
036300030422     C                   endif
036400030422     C** Colonna 7
036500030422     C                   if        skPOCOL(skPO(j)) = 7
036600030428     C                   eval(h)   stpD07  = skMemDPO(skPO(j))
036700030428     C                   eval(h)   stpN07  = skMemNPO(skPO(j))
036800041111     c                   eval      stpgD07 = stpgD07 + stpD07
036900041111     c                   eval      stpgN07 = stpgN07 + stpN07
037000030422     C                   endif
037100030422     C** Colonna 8
037200030422     C                   if        skPOCOL(skPO(j)) = 8
037300030428     C                   eval(h)   stpD08  = skMemDPO(skPO(j))
037400030428     C                   eval(h)   stpN08  = skMemNPO(skPO(j))
037500041111     c                   eval      stpgD08 = stpgD08 + stpD08
037600041111     c                   eval      stpgN08 = stpgN08 + stpN08
037700030422     C                   endif
037800030422     C** Colonna 9
037900030422     C                   if        skPOCOL(skPO(j)) = 9
038000030428     C                   eval(h)   stpD09  = skMemDPO(skPO(j))
038100030428     C                   eval(h)   stpN09  = skMemNPO(skPO(j))
038200041111     c                   eval      stpgD09 = stpgD09 + stpD09
038300041111     c                   eval      stpgN09 = stpgN09 + stpN09
038400030422     C                   endif
038500090625     C** Colonna 10
038600090625     C                   if        skPOCOL(skPO(j)) = 10
038700090625     C                   eval(h)   stpD10  = skMemDPO(skPO(j))
038800090625     C                   eval(h)   stpN10  = skMemNPO(skPO(j))
038900090625     c                   eval      stpgD10 = stpgD10 + stpD10
039000090625     c                   eval      stpgN10 = stpgN10 + stpN10
039100090625     C                   endif
039200090625     C** Colonna 11
039300090625     C                   if        skPOCOL(skPO(j)) = 11
039400090625     C                   eval(h)   stpD11  = skMemDPO(skPO(j))
039500090625     C                   eval(h)   stpN11  = skMemNPO(skPO(j))
039600090625     c                   eval      stpgD11 = stpgD11 + stpD11
039700090625     c                   eval      stpgN11 = stpgN11 + stpN11
039800090625     C                   endif
039900090625     C** Colonna 12
040000090625     C                   if        skPOCOL(skPO(j)) = 12
040100090625     C                   eval(h)   stpD12  = skMemDPO(skPO(j))
040200090625     C                   eval(h)   stpN12  = skMemNPO(skPO(j))
040300090625     c                   eval      stpgD12 = stpgD12 + stpD12
040400090625     c                   eval      stpgN12 = stpgN12 + stpN12
040500090625     C                   endif
040600090625     C** Colonna 13
040700090625     C                   if        skPOCOL(skPO(j)) = 13
040800090625     C                   eval(h)   stpD13  = skMemDPO(skPO(j))
040900090625     C                   eval(h)   stpN13  = skMemNPO(skPO(j))
041000090625     c                   eval      stpgD13 = stpgD13 + stpD13
041100090625     c                   eval      stpgN13 = stpgN13 + stpN13
041200090625     C                   endif
041300090625     C** Colonna 14
041400090625     C                   if        skPOCOL(skPO(j)) = 14
041500090625     C                   eval(h)   stpD14  = skMemDPO(skPO(j))
041600090625     C                   eval(h)   stpN14  = skMemNPO(skPO(j))
041700090625     c                   eval      stpgD14 = stpgD14 + stpD14
041800090625     c                   eval      stpgN14 = stpgN14 + stpN14
041900090625     C                   endif
042000090625     C** Colonna 15
042100090625     C                   if        skPOCOL(skPO(j)) = 15
042200090625     C                   eval(h)   stpD15  = skMemDPO(skPO(j))
042300090625     C                   eval(h)   stpN15  = skMemNPO(skPO(j))
042400090625     c                   eval      stpgD15 = stpgD15 + stpD15
042500090625     c                   eval      stpgN15 = stpgN15 + stpN15
042600090625     C                   endif
042700090625      * se ho più di 15 colonne devo sommare tutti i p.o. rimanenti
042800041110      * sulla nona colonna
042900090625     c                   if        skPOCOL(skPO(j)) > 15
043000090625     C                   eval(h)   stpD15  = stpD15 + skMemDPO(skPO(j))
043100090625     C                   eval(h)   stpN15  = stpN15 + skMemNPO(skPO(j))
043200090625     c                   eval      stpgD15 = stpgD15 + stpD15
043300090625     c                   eval      stpgN15 = stpgN15 + stpN15
043400041110     C                   endif
043500030422     C*
043600030422     C                   endif
043700030422     C                   endif
043800030422     C*
043900030422     C* SCORRO L'ELENCO P.O. X T.A. CORRENTE
044000030422     C                   eval      j = j + 1
044100030422     C                   enddo
044200030422     C*
044300090625     c
044400090625     c* testata
044500090625     c                   if        *in01
044600090625     C                   write     LSD1PT
044700090625     C  n20              write     LSD1PI
044800090625     C   20              write     LSD1PI15
044900090625     c                   setoff                                       01
045000090625     c                   endif
045100090625     c
045200090625     C* STAMPO LA RIGA DI DETTAGLIO
045300090625     C  n20              write     LSD1P2
045400090625     C   20              write     LSD1P215
045500030422     C                   endif
045600130205     c
045700130205     C                   endif
045800030422     C                   endif
045900030422     C*
046000030422     C* SCORRO L'ELENCO DEI T.A.
046100030422     C                   eval      i = i + 1
046200030422     C                   enddo
046300041111     C*
046400130205     c                   if        ilsd9RIC<>'F'
046500041111     C* STAMPO TOTALI
046600090625     c                   write     LSD1RI
046700090625     C  n20              write     LSD1TG
046800090625     C   20              write     LSD1TG15
046900030422     C*
047000030422     C* STAMPO CHIUSURA PROSPETTO
047100030507     C                   write     LSD1RI
047200030507     C                   write     LSD1FI
047300130205     c                   endif
047400030414     C*
047500030414     C                   ENDSR
047600030414     C*------------------------------------------------------------------------*
047700030422
047800030422
047900030422
048000030422
048100030422     C*------------------------------------------------------------------------*
048200030422     C* ABBPOCOL -  Routine di abbinamento P.O. a colonne in stampa
048300030422     C*------------------------------------------------------------------------*
048400030422     C     ABBPOCOL      BEGSR
048500030423     C*
048600030423     C                   setoff                                       111213
048700030423     C                   setoff                                       141516
048800030423     C                   setoff                                       171819
048900090625     C                   setoff                                       20
049000030422     C*
049100030422     C* Innanzitutto ordino in modo crescente l'elenco dei P.O.
049200030422     C                   sorta     skPO
049300030423     C*
049400030423     C* Di default abbino alla 1° colonna il P.O. terminal di partenza e alle su
049500030423     C* gli altri P.O. in ordine crescente
049600030423     C                   clear                   skPOCOL
049700030423     C                   z-add     1             currCOL
049800130205     C                   eval      skPOCOL(ktfp) = currCOL
049900030423     C                   z-add     1             i
050000030423     C                   dow       i <= %elem(skPO)
050100030423     C                   if        skPO(i)  > *zeros AND
050200130205     C                             skPO(i) <> ktfp
050300030423     C                   eval      currCOL = currCOL + 1
050400030423     C                   eval      skPOCOL(skPO(i)) = currCOL
050500030423     C                   endif
050600030423     C                   eval      i = i + 1
050700030423     C                   enddo
050800030422     C*
050900030423     C* Eseguo verifica valorizzazioni x T.A.
051000030423     C                   clear                   skPOCOLok
051100030422     C*
051200030422     C                   z-add     1             i
051300030422     C                   dow       i <= %elem(skTA)
051400030422     C                   if        skTA(i) > *zeros
051500030423     C     skTA(i)       occur     skMemDatPO
051600030422     C                   z-add     1             j
051700030422     C                   dow       j <= %elem(skPO)
051800030422     C*
051900030422     C* Solo x i P.O. validi
052000030422     C                   if        skPO(j) > *zeros
052100030422     C*
052200030423     C* Solo se ci sono dati da stampare
052300030423     C                   eval      depDTP = skMemDPO(skPO(j))
052400030423     C                   eval      depNTP = skMemNPO(skPO(j))
052500030423     C                   if        depDTP > *zeros OR
052600030423     C                             depNTP > *zeros
052700030423     C                   if        skPOCOLok(skPO(j)) = *zeros
052800030422     C*
052900030423     C* Flaggo a OK la colonna relativa al P.O. da stampare
053000030423     C                   eval      skPOCOLok(skPO(j)) = 999
053100030423     C                   endif
053200030423     C                   endif
053300030423     C                   endif
053400030423     C                   eval      j = j + 1
053500030423     C                   enddo
053600030423     C                   endif
053700030423     C                   eval      i = i + 1
053800030423     C                   enddo
053900030423     C*
054000030423     C* A questo punto "mergio" le collonne di default con quelle effettivamente
054100030423     C                   z-add     1             i
054200030423     C                   dow       i <= %elem(skPO)
054300030423     C                   if        skPO(i) > *zeros
054400030423     C                   if        skPOCOLok(skPO(i)) = *zeros
054500030423     C                   eval      depCOL = skPOCOL(skPO(i))
054600030423     C* Diminuisco di 1 le posizioni >= alla colonna relativa al P.O. da nn stam
054700030423     C                   z-add     1             j
054800030423     C                   dow       j <= %elem(skPOCOL)
054900030423     C                   if        skPOCOL(j) = depCOL
055000030423     C                   eval      skPOCOL(j) = *zeros
055100030423     C                   endif
055200030423     C                   if        skPOCOL(j) > depCOL
055300030423     C                   eval      skPOCOL(j) = skPOCOL(j) - 1
055400030423     C                   endif
055500030423     C                   eval      j = j + 1
055600030423     C                   enddo
055700030423     C                   endif
055800030423     C                   endif
055900030423     C                   eval      i = i + 1
056000030423     C                   enddo
056100130206     c                   clear                   stpP01
056200130206     c                   clear                   stpP01D
056300130206     c                   clear                   stpP02
056400130206     c                   clear                   stpP02D
056500130206     c                   clear                   stpP03
056600130206     c                   clear                   stpP03D
056700130206     c                   clear                   stpP04
056800130206     c                   clear                   stpP04D
056900130206     c                   clear                   stpP05
057000130206     c                   clear                   stpP05D
057100130206     c                   clear                   stpP06
057200130206     c                   clear                   stpP06D
057300130206     c                   clear                   stpP07
057400130206     c                   clear                   stpP07D
057500130206     c                   clear                   stpP08
057600130206     c                   clear                   stpP08D
057700130206     c                   clear                   stpP09
057800130206     c                   clear                   stpP09D
057900130206     c                   clear                   stpP10
058000130206     c                   clear                   stpP10D
058100130206     c                   clear                   stpP11
058200130206     c                   clear                   stpP11D
058300130206     c                   clear                   stpP12
058400130206     c                   clear                   stpP12D
058500130206     c                   clear                   stpP13
058600130206     c                   clear                   stpP13D
058700130206     c                   clear                   stpP14
058800130206     c                   clear                   stpP14D
058900130206     c                   clear                   stpP15
059000130206     c                   clear                   stpP15D
059100030423     C*
059200030423     C* Effettuo 1 ultimo giro x valorizzare le intestazioni delle colonne da st
059300030423     C                   z-add     1             j
059400030423     C                   dow       j <= %elem(skPO)
059500030423     C                   if        skPO(j) > *zeros
059600030423     C                   eval      currCOL = skPOCOL(skPO(j))
059700030423     C*
059800030423     C                   if        currCOL = 1
059900030423     C                   seton                                        11
060000030423     C                   eval      stpP01  = skPO(j)
060100030423     C                   eval      stpP01D = skPOD(skPO(j))
060200030423     C                   endif
060300030423     C                   if        currCOL = 2
060400030423     C                   seton                                        12
060500030423     C                   eval      stpP02  = skPO(j)
060600030423     C                   eval      stpP02D = skPOD(skPO(j))
060700030423     C                   endif
060800030423     C                   if        currCOL = 3
060900030423     C                   seton                                        13
061000030423     C                   eval      stpP03  = skPO(j)
061100030423     C                   eval      stpP03D = skPOD(skPO(j))
061200030423     C                   endif
061300030423     C                   if        currCOL = 4
061400030423     C                   seton                                        14
061500030423     C                   eval      stpP04  = skPO(j)
061600030423     C                   eval      stpP04D = skPOD(skPO(j))
061700030423     C                   endif
061800030423     C                   if        currCOL = 5
061900030423     C                   seton                                        15
062000030423     C                   eval      stpP05  = skPO(j)
062100030423     C                   eval      stpP05D = skPOD(skPO(j))
062200030423     C                   endif
062300030423     C                   if        currCOL = 6
062400030423     C                   seton                                        16
062500030423     C                   eval      stpP06  = skPO(j)
062600030423     C                   eval      stpP06D = skPOD(skPO(j))
062700030423     C                   endif
062800090625     C                   if        currCOL = 7
062900030423     C                   seton                                        17
063000030423     C                   eval      stpP07  = skPO(j)
063100030423     C                   eval      stpP07D = skPOD(skPO(j))
063200030423     C                   endif
063300030423     C                   if        currCOL = 8
063400030423     C                   seton                                        18
063500030423     C                   eval      stpP08  = skPO(j)
063600030423     C                   eval      stpP08D = skPOD(skPO(j))
063700030423     C                   endif
063800030423     C                   if        currCOL = 9
063900030423     C                   seton                                        19
064000030423     C                   eval      stpP09  = skPO(j)
064100030423     C                   eval      stpP09D = skPOD(skPO(j))
064200030423     C                   endif
064300090625     C                   if        currCOL = 10
064400090625     C                   seton                                        20
064500090625     C                   eval      stpP10  = skPO(j)
064600090625     C                   eval      stpP10D = skPOD(skPO(j))
064700090625     C                   endif
064800090625     C                   if        currCOL = 11
064900090625     C                   seton                                        20
065000090625     C                   eval      stpP11  = skPO(j)
065100090625     C                   eval      stpP11D = skPOD(skPO(j))
065200090625     C                   endif
065300090625     C                   if        currCOL = 12
065400090625     C                   seton                                        20
065500090625     C                   eval      stpP12  = skPO(j)
065600090625     C                   eval      stpP12D = skPOD(skPO(j))
065700090625     C                   endif
065800090625     C                   if        currCOL = 13
065900090625     C                   seton                                        20
066000090625     C                   eval      stpP13  = skPO(j)
066100090625     C                   eval      stpP13D = skPOD(skPO(j))
066200090625     C                   endif
066300090625     C                   if        currCOL = 14
066400090625     C                   seton                                        20
066500090625     C                   eval      stpP14  = skPO(j)
066600090625     C                   eval      stpP14D = skPOD(skPO(j))
066700090625     C                   endif
066800090625     C                   if        currCOL = 15
066900090625     C                   seton                                        20
067000090625     C                   eval      stpP15  = skPO(j)
067100090625     C                   eval      stpP15D = skPOD(skPO(j))
067200090625     C                   endif
067300090625      * se ho più di 15 p.o. devo intestare la colonna con 'ALTRI'
067400090625     c                   If        currCOL > 15
067500090625     C                   clear                   stpP15
067600090625     C                   eval      stpP15D = 'ALTRI'
067700041110     C                   endif
067800030422     C*
067900030423     C                   endif
068000030423     C                   eval      j = j + 1
068100030423     C                   enddo
068200030423     C*
068300030422     C                   ENDSR
068400030422     C*---------------------------------------------------------------*
068500030414
068600030414
068700030414
068800030414     C*---------------------------------------------------------------*
068900130205     C* CAR_LNP - Caricamento tabelle di wrk
069000030414     C*---------------------------------------------------------------*
069100130205     C     CAR_LNP       BEGSR
069200130205    1c                   if        ilsd9TFP>0
069300130205     c                   eval      ktfp=ilsd9tfp
069400130205     c                   exsr      ElaTFP
069500130205     c                   exsr      Procedi
069600130205     c
069700130205   x1c                   else
069800130205     c
069900130205     c                   eval      kepa='P'
070000130205     c     kepa          setll     azcae05l
070100130205     c     kepa          reade     azcae05l
070200130205    2c                   dow       not %eof(azcae05l)
070300130205     c                   if        caedde<=ilsd9dsa and caedsc>=ilsd9dsa
070400130205     c                             and caeatb=' '
070500130205     c
070600130205     c                   eval      ktfp=caetfe
070700130205     c                   exsr      ElaTFP
070800130205     c                   exsr      Procedi
070900130205     c
071000130205     c     kcae          setgt     azcae05l
071100130205     c                   endif
071200130205     c
071300130205     c     kepa          reade     azcae05l
071400130205    2c                   enddo
071500130205     c
071600130205    1c                   endif
071700130205     c
071800130205     c                   ENDSR
071900130205     C*---------------------------------------------------------------*
072000130205     C* ELATFP - Caricamento linee di partenza del terminal partenza
072100130205     C*---------------------------------------------------------------*
072200130205     C     ELATFP        BEGSR
072300030423     C*
072400030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
072500130205     c                   clear                   trul06ds
072600030423     C                   MOVEL     '£1'          D06COD
072700130205     C                   MOVEL     kTFP          D06KEY
072800130205     c                   if        ilsd9TFP>0
072900030423     C                   MOVEL     'L'           D06TLA
073000130205     c                   endif
073100130205     C                   MOVE      'S'           D06KEY
073200130205     c                   eval      d06drf=ilsd9DSA
073300130205     c
073400030423     C                   MOVEL(P)  TRUL06DS      KPJBU
073500030423     C                   CALL      'TRUL06R'
073600030423     C                   PARM                    KPJBA
073700030423     C                   MOVEL     KPJBU         TRUL06DS
073800030414     C*
073900030423     C* Reperisco le rispettive denominazioni dei P.O.
074000030422     C                   clear                   skPO
074100030422     C                   clear                   skPOD
074200130206     C                   clear                   skta
074300130206     C                   clear                   skpocol
074400130206     C                   clear                   skpocolok
074500030423     C     1             do        30            i
074600030423     C                   if        L1(i) > *zeros
074700030423     C                   eval      skPO(L1(i)) = L1(i)
074800030423     C** Reperisco le descrizioni di ogni P.O.
074900030423     C     L1(i)         chain     azorg01l
075000030423     C                   if        %found(azorg01l)
075100030423     C                   eval      skPOD(L1(i)) = orgDES
075200030423     C                   else
075300030423     C                   eval      skPOD(L1(i)) = *all'*'
075400030423     C                   endif
075500030423     C                   endif
075600030512     C**
075700030512     C                   enddo
075800130205     c                   ENDSR
075900130205     C*---------------------------------------------------------------*
076000130205     C* CARTAB - Caricamento tabelle di wrk
076100130205     C*---------------------------------------------------------------*
076200130205     C     CARTAB        BEGSR
076300030512     C*
076400030512     C* Carico i tipi bolla dalla tabella 3A
076500030512     C                   z-add     *zeros        i
076600030512     C                   eval      tblCOD = '3A'
076700030512     C     KEYtabP       setll     tabel00f
076800030512     C     KEYtabP       reade     tabel00f
076900030512     C                   dow       not %eof(tabel00f)
077000030512     C                   movel(p)  tblUNI        DS3A
077100030512     C                   if        §3ARBL <> 'R'
077200030512     C                   add       1             i
077300030512     C                   eval      sk3A(i) = tblKEY
077400030512     C                   endif
077500030512     C     KEYtabP       reade     tabel00f
077600030512     C                   enddo
077700061108     C*
077800130205     C* Carico i tipi bolla dalla tabella 1B
077900061108     C                   z-add     *zeros        i
078000061108     C                   eval      tblCOD = '1B'
078100061108     C     KEYtabP       setll     tabel00f
078200061108     C     KEYtabP       reade     tabel00f
078300061108     C                   dow       not %eof(tabel00f)
078400061108     C                   movel(p)  tblUNI        DS1B
078500061108     C                   if        §1bfg8='N'
078600061108     C                   add       1             i
078700061108     C                   eval      sk1b(i) = tblKEY
078800061108     C                   endif
078900061108     C     KEYtabP       reade     tabel00f
079000061108     C                   enddo
079100030414     C*
079200030414     C                   ENDSR
079300030414     C*------------------------------------------------------------------------*
079400030414     C* REPERISCE I DATI UTENTE
079500030414     C*------------------------------------------------------------------------*
079600030414     C     REPDATIUTE    BEGSR
079700030414     C*
079800030414     C* INIZIALIZZA VARIABILI DI WRK
079900030414     C                   CLEAR                   TIBS34DS
080000030414     C                   CLEAR                   AZUTEDS
080100030414     C                   CLEAR                   DDATIUTE
080200030414     C*
080300030414     C     *DTAARA       DEFINE    §azute        azuteds
080400030414     C     *DTAARA       DEFINE    §datiute      ddatiute
080500030414     C                   IN(E)     *DTAARA
080600030422     C                   IF        %Error
080700030422     C                   EVAL      I34Tla = 'L'
080800030414     C                   CALL      'TIBS34R'
080900030414     C                   PARM                    Tibs34Ds
081000030414     C                   IN        *DTAARA
081100030422     C                   ENDIF
081200030414     C*
081300030414     C                   ENDSR
081400030414     C*------------------------------------------------------------------------*
081500010509
081600010509
081700001218
081800001218     C*------------------------------------------------------------------------*
081900001218     C* *INZSR - ROUTINE INIZIALE
082000001218     C*------------------------------------------------------------------------*
082100001218     C     *INZSR        BEGSR
082200001218     C*
082300030422     C* Ricevo in la DS di architettura
082400010726     C     *ENTRY        PLIST
082500020613     C                   PARM                    KPJBA
082600130205     c                   movel     kpjbu         fnlsd9ds
082700030521     C*
082800030521     C* Reperisco l'UDATE del job
082900030521     C                   TIME                    WN14             14 0
083000030521     C                   MOVE      WN14          WN8               8 0
083100030521     C                   Z-ADD     WN8           G08DAT
083200030521     C                   Z-ADD     *zeros        G08INV
083300030521     C                   MOVEL     '0'           G08ERR
083400030521     C                   CALL      'XSRDA8'
083500030521     C                   PARM                    WLBDA8
083600030521     C                   Z-ADD     G08INV        DATCOR            8 0
083700010726     C*
083800030422     C* Inizializzo campi chiave "fissi"
083900030414     C                   Z-ADD     1             tblKUT
084000030422     C                   Z-ADD     *zeros        blpDUC
084100130206     c* Giro le date per la stampa
084200130206     c                   clear                   v1cdsd
084300130206     c                   if        ilsd9dsd>0
084400130206     c     *iso          movel     ilsd9dsd      dataiso
084500130206     c                   movel     dataiso       dataeur
084600130206     c                   movel     dataeur       v1cdsd
084700130206     c                   endif
084800130206     c
084900130206     c                   clear                   v1cdsa
085000130206     c                   if        ilsd9dsa>0
085100130206     c     *iso          movel     ilsd9dsa      dataiso
085200130206     c                   movel     dataiso       dataeur
085300130206     c                   movel     dataeur       v1cdsa
085400130206     c                   endif
085500130205     c
085600030414     C****
085700030414     C* Definizione chiavi
085800030414     C****
085900030512     C* Chiave su TABEL00F - parziale
086000030512     C     KEYtabP       KLIST
086100030414     C                   KFLD                    tblKUT
086200030414     C                   KFLD                    tblCOD
086300101126     C* Chiave su FNBLP05L - parziale
086400101126     C     KEYblp05P     KLIST
086500030422     C                   KFLD                    blpLNP
086600030422     C                   KFLD                    blpDUC
086700130205     C     Kcae          KLIST
086800130205     C                   KFLD                    kepa
086900130205     C                   KFLD                    caetfe
087000130205     c* Pulizia file di work se richiesto
087100130205      /FREE
087200130205       exec sql delete from WFSAT00F    ;
087300130205      /END-FREE
087400030414     C*
087500030414     C* REPERIMEMTO DATI UTENTE
087600030414     C                   EXSR      REPDATIUTE
087700030422     C*
087800030422     C* CARICAMENTO TABELLE DI WRK
087900030422     C                   EXSR      CARTAB
088000130206     c*
088100130206     c* Apertura file
088200130206     c                   if        %subst( knsif : 7 : 1) = 'P'
088300130206     c                   eval      %subst( wFNBLP : 7 : 3 ) = 'PRD'
088400130206     c                   eval      %subst( wSAT : 7 : 3 ) = 'AZP'
088500130206     c                   endif
088600130206     c
088700130206     c                   open      fnblp05l
088800130206     c                   open      wfsat00f
088900001218     C*
089000001218     C                   ENDSR
