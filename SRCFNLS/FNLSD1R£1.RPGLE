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
001400041227     D**!!! OG143         E DS
001500030422     D*--------------------
001600030422     D* SCHIERE DI MEMORIZZAZIONE
001700030422     D*--------------------
001800040430     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
001900030512     D sk3A            s              2    DIM(999)
002000061108     D sk1B            s              2    DIM(99)
002100041227     D**!!! skPOPT          s              3  0 DIM(999)
002200030422     D skPO            s              3  0 DIM(999)
002300030422     D skPOD           s             15    DIM(999)
002400030422     D skPOCOL         s              3  0 DIM(999)
002500030423     D skPOCOLok       s              3  0 DIM(999)
002600030422     D skTA            s              3  0 DIM(999)
002700030423     D skMemDTP        s              7  3 DIM(999)
002800030423     D skMemNTP        s              7  3 DIM(999)
002900030423     D skMemDatPO      ds                  OCCURS(999)
003000030423     D   skMemDPO                     7  3 DIM(999)
003100030423     D   skMemNPO                     7  3 DIM(999)
003200030414     D*--------------------
003300030414     D* VARIABILI DI WRK
003400030414     D*--------------------
003500030422     D i               s              4  0
003600030422     D j               s              4  0
003700030422     D currCOL         s              3  0
003800030423     D depCOL          s              3  0
003900030423     D depDTP          s              5  1
004000030423     D depNTP          s              5  1
004100040430     D wdsd            s                   like(v1cdsd)
004200040430     D wdsa            s                   like(v1cdsa)
004300030414     D*---
004400030414     D* DS REPERIMENTO DATI UTENTE
004500030414     D*---
004600030414     D TIBS34DS      E DS
004700030414     D DDATIUTE      E DS
004800030414     D AZUTEDS       E DS                  extname(AZUTE00F)
004900030423     D*---
005000030423     D* DS REPERIMENTO £ire
005100030423     D*---
005200030423     D TRUL06DS      E DS
005300030423     D  L1                     1     90  0 DIM(30)
005400030521     D*------------------
005500030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600030521     D*------------------
005700030521     D WLBDA8          DS                  INZ
005800030521     D  G08DAT                 1      8  0
005900030521     D  G08INV                 9     16  0
006000030521     D  G08ERR                17     17
006100030521     D  G08TGI                18     22  0
006200001218
006300001218
006400920812     C*---------------------------------------------------------------*
006500001218     C* MAIN
006600001218     C*---------------------------------------------------------------*
006700040430     c* Gestione videata richiesta data dal/al
006800040430     c                   clear                   v1cdsd
006900040430     c                   z-add     wn8           v1cdsa
007000040430     c                   clear                   v1cmsg
007100040430     c                   do        *hival
007200040430     c                   exfmt     LSD1D01
007300040430     c     *inkc         cabeq     *on           fine
007400040430     c                   exsr      chkd01
007500040430     c     *in28         ifeq      *off
007600040430     c     *inkf         andeq     *on
007700040430     c                   leave
007800040430     c                   endif
007900040430     c                   enddo
008000040430     c*
008100040430     c     wdsa          ifeq      *zeros
008200040430     c                   z-add     wdsd          wdsa
008300040430     c                   z-add     v1cdsd        v1cdsa
008400040430     c                   endif
008500040430     c****
008600001218     C                   exsr      procedi
008700030414     C                   exsr      stampa
008800001218     C*
008900040430     c     fine          tag
009000001218     C                   seton                                        LR
009100010509     C*---------------------------------------------------------------*
009200001218
009300040430     C*------------------------------------------------------------------------*
009400040430     C* CHKD01  - Controlli videata
009500040430     C*------------------------------------------------------------------------*
009600040430     C     CHKD01        BEGSR
009700040430     C*
009800040430     c                   setoff                                       284344
009900040430     c                   clear                   v1cmsg
010000040430     c* controllo data dal se immessa
010100040430     c                   clear                   wdsd
010200040430    1C     V1CDSD        IFGT      0
010300040430     C                   MOVE      V1CdSd        G08DAT
010400040430     C                   MOVEL     *ZERO         G08INV
010500040430     C                   MOVEL     *BLANKS       G08ERR
010600040430     C                   CALL      'XSRDA8'
010700040430     C                   PARM                    WLBDA8
010800040430     C     G08ERR        IFEQ      '1'
010900040430     C                   MOVEL     MSG(1)        V1CMSG
011000040430     C                   SETON                                        4328
011100040430     C                   GOTO      ENDCTR
011200040430     C                   ENDIF
011300040430     C                   Z-ADD     G08DAT        V1CDSD
011400040430     C                   Z-ADD     G08INV        WDSD
011500040430     c                   endif
011600040430     C*
011700040430     c* controllo data al se immessa
011800040430     c                   clear                   wdsa
011900040430    2C     V1CDSA        IFNE      0
012000040430     C                   MOVE      V1CDSA        G08DAT
012100040430     C                   MOVEL     *ZERO         G08INV
012200040430     C                   MOVEL     *BLANKS       G08ERR
012300040430     C                   CALL      'XSRDA8'
012400040430     C                   PARM                    WLBDA8
012500040430     C     G08ERR        IFEQ      '1'
012600040430     C                   MOVEL     MSG(1)        V1CMSG
012700040430     C                   SETON                                        4428
012800040430     C                   GOTO      ENDCTR
012900040430     C                   ENDIF
013000040430     C                   Z-ADD     G08DAT        V1CDSA
013100040430     C                   Z-ADD     G08INV        WDSA
013200040430     c                   endif
013300040430     C*
013400040430     c* almeno una delle due date ci deve essere
013500040430     c     v1cdsd        ifeq      *zeros
013600040430     c     v1cdsa        andeq     *zeros
013700040430     C                   MOVEL     MSG(3)        V1CMSG
013800040430     c                   seton                                        4328
013900040430     c                   goto      endctr
014000040430     c                   endif
014100040430     c* se immesse entrambe la "data dal" non deve essere > di "data dal"
014200040430    3C     WDSD          IFGT      WDSA
014300040430     c     wdsa          andgt     *zeros
014400040430     C                   MOVEL     MSG(2)        V1CMSG
014500040430     C                   SETON                                        4328
014600040430     C                   GOTO      ENDCTR
014700040430    3C                   ENDIF
014800001218
014900040430     c     endctr        ENDSR
015000001218     C*------------------------------------------------------------------------*
015100001218     C* PROCEDI - Routine principale
015200001218     C*------------------------------------------------------------------------*
015300020613     C     PROCEDI       BEGSR
015400030423     C*
015500030423     C* Inizializzo le schiere di memorizzazione
015600030423     C                   clear                   skMemDTP
015700030423     C                   clear                   skMemNTP
015800030423     C                   z-add     1             i
015900030423     C                   dow       i <= %elem(skMemDatPO)
016000030423     C     i             occur     skMemDatPO
016100030423     C                   clear                   skMemDPO
016200030423     C                   clear                   skMemNPO
016300030423     C                   eval      i = i + 1
016400030423     C                   enddo
016500001218     C*
016600030422     C* Mi posiziono sul FLBLP06L per ciascun P.O. in gestione al T.P.
016700030422     C                   z-add     1             i
016800030422     C                   dow       i <= %elem(skPO)
016900030422     C                   if        skPO(i) > *zeros
017000030422     C                   eval      blpLNP = skPO(i)
017100030422     C     KEYblp06P     chain     fnblp06l
017200030422     C                   if        %found(fnblp06l)
017300030512     C*
017400030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
017500030512     C                   movel     'S'           recOK             1
017600030512     C*
017700030512     C* Eseguo routine di "check" bolla ok x elaborazione
017800030512     C                   exsr      chkrec
017900030512     C*
018000030512     C* Se record da elaborare...
018100030512     C                   if        recOK = 'S'
018200030422     C* Eseguo routine di memorizzazione dati
018300030422     C                   exsr      memdat
018400030512     C                   endif
018500030422     C* Continuo la lettura del fle FNBLP06L
018600030422     C     KEYblp06P     reade     fnblp06l
018700030422     C                   dow       not %eof(fnblp06l)
018800030512     C*
018900030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
019000030512     C                   movel     'S'           recOK
019100030512     C*
019200030512     C* Eseguo routine di "check" bolla ok x elaborazione
019300030512     C                   exsr      chkrec
019400030512     C*
019500030512     C* Se record da elaborare...
019600030512     C                   if        recOK = 'S'
019700030512     C* Eseguo routine di memorizzazione dati
019800030512     C                   exsr      memdat
019900030512     C                   endif
020000030422     C     KEYblp06P     reade     fnblp06l
020100030422     C                   enddo
020200030422     C                   endif
020300030422     C                   endif
020400030422     C                   eval      i = i +1
020500030422     C                   enddo
020600020613     C*
020700001218     C                   ENDSR
020800001218     C*------------------------------------------------------------------------*
020900030512
021000030512
021100030512
021200030512
021300030512     C*------------------------------------------------------------------------*
021400030512     C* CHKREC - Routine di verifica record da trattare
021500030512     C*------------------------------------------------------------------------*
021600030512     C     CHKREC        BEGSR
021700030521     C*
021800030521     C* Verifico che la data della bolla nn sia maggiore della data di lancio
021900040430     C**                 if        (blpAAS*10000)+blpMGS <= DATCOR
022000040430     C* Verifico che la data della bolla sia inclusa nel dal al richiesto   o
022100040430     C                   if        (blpAAS*10000)+blpMGS <= wdsa and
022200040430     c                             (blpaas*10000)+blpmgs >= wdsd
022300030521     C                   else
022400030521     C                   movel     'N'           recOK
022500030521     C                   endif
022600030512     C*
022700030512     C* Verifico se il tipo bolla è di quelli da elaborare
022800030512     C     blpCBO        lookup    sk3A                                   55
022900030512     C                   if        *in55 = *off
023000030512     C                   movel     'N'           recOK
023100030512     C                   endif
023200061108     C* Verifico se il cod.trattamento merce non è da elaborare
023300061108     C     blpCTM        lookup    sk1B                                   55
023400061108     C                   if        *in55 = *on
023500061108     C                   movel     'N'           recOK
023600061108     C                   endif
023700030512     C*
023800030512     C* Verifico che la linea di partenza nn sia poste
023900041227     C**!!!              if        recOK = 'S'
024000041227     C**!!!blpLNP        lookup    skPOPT                                 55
024100041227     C**!!!              if        *in55 = *on
024200041227     C**!!!              movel     'N'           recOK
024300041227     C**!!!              endif
024400041227     C**!!!              endif
024500030512     C*
024600030512     C                   ENDSR
024700030512     C*---------------------------------------------------------------*
024800010626
024900010626
025000010626
025100010626
025200010509     C*------------------------------------------------------------------------*
025300030422     C* MEMDAT - Routine di memorizzazione dati
025400010509     C*------------------------------------------------------------------------*
025500030422     C     MEMDAT        BEGSR
025600010509     C*
025700030422     C* Memorizzo i dati x T.A. e P.O.
025800030422     C                   eval      skTA(blpTFA) = blpTFA
025900030423     C     skTA(blpTFA)  occur     skMemDatPO
026000030422     C*
026100030423     C* Evidenzio il volume merce "da partire" scindendo i servizi NON distribuz
026200030423     C                   eval      skMemDPO(blpLNP) = skMemDPO(blpLNP) + blpVLB
026300030423     C                   eval      skMemDTP(blpTFA) = skMemDTP(blpTFA) + blpVLB
026400030423     C                   if        blpTSP = 'D'
026500030423     C                   eval      skMemNPO(blpLNP) = skMemNPO(blpLNP) + blpVLB
026600030423     C                   eval      skMemNTP(blpTFA) = skMemNTP(blpTFA) + blpVLB
026700030423     C                   endif
026800010509     C*
026900010509     C                   ENDSR
027000010509     C*---------------------------------------------------------------*
027100030414
027200030414
027300030414
027400030414
027500030414     C*------------------------------------------------------------------------*
027600030422     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
027700030414     C*------------------------------------------------------------------------*
027800030414     C     STAMPA        BEGSR
027900030422     C*
028000030422     C* INNANZITUTTO ORDINO IN MODO CRESCENTE L'ELENCO DEI TERMINAL ARRIVO
028100030422     C                   sorta     skTA
028200030422     C*
028300030422     C* ABBINO LE COLONNE DI STAMPA AI P.O. IN GESTIONE AL TERMINAL PARTENZA
028400030422     C                   exsr      abbPOCOL
028500030423     C*
028600030423     C* STAMPO TESTATA
028700030423     C                   eval      stpSYF = RSUT
028800030507     C                   write     LSD1PT
028900030507     C                   write     LSD1PI
029000030422     C*
029100030422     C* STAMPO DETTAGLIO X TERMINAL ARRIVO (solo se c'è qualcosa effettivamente
029200030422     C                   z-add     1             i
029300030422     C                   dow       i <= %elem(skTA)
029400030422     C*
029500030422     C* SOLO X I T.A. VALIDI
029600030422     C                   if        skTA(i) > *zeros
029700030423     C     skTA(i)       occur     skMemDatPO
029800030422     C*
029900030423     C* SOLO SE C'E' QUALCOSA DA STAMPARE
030000030423     C                   eval      depDTP = skMemDTP(skTA(i))
030100030423     C                   eval      depNTP = skMemNTP(skTA(i))
030200030423     C                   if        depDTP > *zeros OR
030300030423     C                             depNTP > *zeros
030400030422     C*
030500030422     C* STAMPO I TOTALI T.P. X T.A.
030600030507     C                   clear                   LSD1P2
030700030422     C                   eval      stpTA  = skTA(i)
030800030428     C                   eval(h)   stpDTP = skMemDTP(skTA(i))
030900030428     C                   eval(h)   stpNTP = skMemNTP(skTA(i))
031000041111     c                   eval      stpgDTP = stpgDTP + stpDTP
031100041111     c                   eval      stpgNTP = stpgNTP + stpNTP
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
032700030422     C                   endif
032800030422     C** Colonna 2
032900030422     C                   if        skPOCOL(skPO(j)) = 2
033000030428     C                   eval(h)   stpD02  = skMemDPO(skPO(j))
033100030428     C                   eval(h)   stpN02  = skMemNPO(skPO(j))
033200041111     c                   eval      stpgD02 = stpgD02 + stpD02
033300041111     c                   eval      stpgN02 = stpgN02 + stpN02
033400030422     C                   endif
033500030422     C** Colonna 3
033600030422     C                   if        skPOCOL(skPO(j)) = 3
033700030428     C                   eval(h)   stpD03  = skMemDPO(skPO(j))
033800030428     C                   eval(h)   stpN03  = skMemNPO(skPO(j))
033900041111     c                   eval      stpgD03 = stpgD03 + stpD03
034000041111     c                   eval      stpgN03 = stpgN03 + stpN03
034100030422     C                   endif
034200030422     C** Colonna 4
034300030422     C                   if        skPOCOL(skPO(j)) = 4
034400030428     C                   eval(h)   stpD04  = skMemDPO(skPO(j))
034500030428     C                   eval(h)   stpN04  = skMemNPO(skPO(j))
034600041111     c                   eval      stpgD04 = stpgD04 + stpD04
034700041111     c                   eval      stpgN04 = stpgN04 + stpN04
034800030422     C                   endif
034900030422     C** Colonna 5
035000030422     C                   if        skPOCOL(skPO(j)) = 5
035100030428     C                   eval(h)   stpD05  = skMemDPO(skPO(j))
035200030428     C                   eval(h)   stpN05  = skMemNPO(skPO(j))
035300041111     c                   eval      stpgD05 = stpgD05 + stpD05
035400041111     c                   eval      stpgN05 = stpgN05 + stpN05
035500030422     C                   endif
035600030422     C** Colonna 6
035700030422     C                   if        skPOCOL(skPO(j)) = 6
035800030428     C                   eval(h)   stpD06  = skMemDPO(skPO(j))
035900030428     C                   eval(h)   stpN06  = skMemNPO(skPO(j))
036000041111     c                   eval      stpgD06 = stpgD06 + stpD06
036100041111     c                   eval      stpgN06 = stpgN06 + stpN06
036200030422     C                   endif
036300030422     C** Colonna 7
036400030422     C                   if        skPOCOL(skPO(j)) = 7
036500030428     C                   eval(h)   stpD07  = skMemDPO(skPO(j))
036600030428     C                   eval(h)   stpN07  = skMemNPO(skPO(j))
036700041111     c                   eval      stpgD07 = stpgD07 + stpD07
036800041111     c                   eval      stpgN07 = stpgN07 + stpN07
036900030422     C                   endif
037000030422     C** Colonna 8
037100030422     C                   if        skPOCOL(skPO(j)) = 8
037200030428     C                   eval(h)   stpD08  = skMemDPO(skPO(j))
037300030428     C                   eval(h)   stpN08  = skMemNPO(skPO(j))
037400041111     c                   eval      stpgD08 = stpgD08 + stpD08
037500041111     c                   eval      stpgN08 = stpgN08 + stpN08
037600030422     C                   endif
037700030422     C** Colonna 9
037800030422     C                   if        skPOCOL(skPO(j)) = 9
037900030428     C                   eval(h)   stpD09  = skMemDPO(skPO(j))
038000030428     C                   eval(h)   stpN09  = skMemNPO(skPO(j))
038100041111     c                   eval      stpgD09 = stpgD09 + stpD09
038200041111     c                   eval      stpgN09 = stpgN09 + stpN09
038300030422     C                   endif
038400041110      * se ho più di 9 colonne devo sommare tutti i p.o. rimanenti
038500041110      * sulla nona colonna
038600041110     c                   if        skPOCOL(skPO(j)) > 9
038700041110     C                   eval(h)   stpD09  = stpD09 + skMemDPO(skPO(j))
038800041110     C                   eval(h)   stpN09  = stpN09 + skMemNPO(skPO(j))
038900041111     c                   eval      stpgD09 = stpgD09 + stpD09
039000041111     c                   eval      stpgN09 = stpgN09 + stpN09
039100041110     C                   endif
039200030422     C*
039300030422     C                   endif
039400030422     C                   endif
039500030422     C*
039600030422     C* SCORRO L'ELENCO P.O. X T.A. CORRENTE
039700030422     C                   eval      j = j + 1
039800030422     C                   enddo
039900030422     C*
040000030422     C* STAMPO LA RIGA DI DETTAGLIO
040100030507     C                   write     LSD1P2
040200030422     C                   endif
040300030422     C                   endif
040400030422     C*
040500030422     C* SCORRO L'ELENCO DEI T.A.
040600030422     C                   eval      i = i + 1
040700030422     C                   enddo
040800041111     C*
040900041111     C* STAMPO TOTALI
041000041111     C                   write     LSD1RI
041100041111     C                   write     LSD1TG
041200030422     C*
041300030422     C* STAMPO CHIUSURA PROSPETTO
041400030507     C                   write     LSD1RI
041500030507     C                   write     LSD1FI
041600030414     C*
041700030414     C                   ENDSR
041800030414     C*------------------------------------------------------------------------*
041900030422
042000030422
042100030422
042200030422
042300030422     C*------------------------------------------------------------------------*
042400030422     C* ABBPOCOL -  Routine di abbinamento P.O. a colonne in stampa
042500030422     C*------------------------------------------------------------------------*
042600030422     C     ABBPOCOL      BEGSR
042700030423     C*
042800030423     C                   setoff                                       111213
042900030423     C                   setoff                                       141516
043000030423     C                   setoff                                       171819
043100030422     C*
043200030422     C* Innanzitutto ordino in modo crescente l'elenco dei P.O.
043300030422     C                   sorta     skPO
043400030423     C*
043500030423     C* Di default abbino alla 1° colonna il P.O. terminal di partenza e alle su
043600030423     C* gli altri P.O. in ordine crescente
043700030423     C                   clear                   skPOCOL
043800030423     C                   z-add     1             currCOL
043900030423     C                   eval      skPOCOL(SIMFEL) = currCOL
044000030423     C                   z-add     1             i
044100030423     C                   dow       i <= %elem(skPO)
044200030423     C                   if        skPO(i)  > *zeros AND
044300030423     C                             skPO(i) <> SIMFEL
044400030423     C                   eval      currCOL = currCOL + 1
044500030423     C                   eval      skPOCOL(skPO(i)) = currCOL
044600030423     C                   endif
044700030423     C                   eval      i = i + 1
044800030423     C                   enddo
044900030422     C*
045000030423     C* Eseguo verifica valorizzazioni x T.A.
045100030423     C                   clear                   skPOCOLok
045200030422     C*
045300030422     C                   z-add     1             i
045400030422     C                   dow       i <= %elem(skTA)
045500030422     C                   if        skTA(i) > *zeros
045600030423     C     skTA(i)       occur     skMemDatPO
045700030422     C                   z-add     1             j
045800030422     C                   dow       j <= %elem(skPO)
045900030422     C*
046000030422     C* Solo x i P.O. validi
046100030422     C                   if        skPO(j) > *zeros
046200030422     C*
046300030423     C* Solo se ci sono dati da stampare
046400030423     C                   eval      depDTP = skMemDPO(skPO(j))
046500030423     C                   eval      depNTP = skMemNPO(skPO(j))
046600030423     C                   if        depDTP > *zeros OR
046700030423     C                             depNTP > *zeros
046800030423     C                   if        skPOCOLok(skPO(j)) = *zeros
046900030422     C*
047000030423     C* Flaggo a OK la colonna relativa al P.O. da stampare
047100030423     C                   eval      skPOCOLok(skPO(j)) = 999
047200030423     C                   endif
047300030423     C                   endif
047400030423     C                   endif
047500030423     C                   eval      j = j + 1
047600030423     C                   enddo
047700030423     C                   endif
047800030423     C                   eval      i = i + 1
047900030423     C                   enddo
048000030423     C*
048100030423     C* A questo punto "mergio" le collonne di default con quelle effettivamente
048200030423     C                   z-add     1             i
048300030423     C                   dow       i <= %elem(skPO)
048400030423     C                   if        skPO(i) > *zeros
048500030423     C                   if        skPOCOLok(skPO(i)) = *zeros
048600030423     C                   eval      depCOL = skPOCOL(skPO(i))
048700030423     C* Diminuisco di 1 le posizioni >= alla colonna relativa al P.O. da nn stam
048800030423     C                   z-add     1             j
048900030423     C                   dow       j <= %elem(skPOCOL)
049000030423     C                   if        skPOCOL(j) = depCOL
049100030423     C                   eval      skPOCOL(j) = *zeros
049200030423     C                   endif
049300030423     C                   if        skPOCOL(j) > depCOL
049400030423     C                   eval      skPOCOL(j) = skPOCOL(j) - 1
049500030423     C                   endif
049600030423     C                   eval      j = j + 1
049700030423     C                   enddo
049800030423     C                   endif
049900030423     C                   endif
050000030423     C                   eval      i = i + 1
050100030423     C                   enddo
050200030423     C*
050300030423     C* Effettuo 1 ultimo giro x valorizzare le intestazioni delle colonne da st
050400030423     C                   z-add     1             j
050500030423     C                   dow       j <= %elem(skPO)
050600030423     C                   if        skPO(j) > *zeros
050700030423     C                   eval      currCOL = skPOCOL(skPO(j))
050800030423     C*
050900030423     C                   if        currCOL = 1
051000030423     C                   seton                                        11
051100030423     C                   eval      stpP01  = skPO(j)
051200030423     C                   eval      stpP01D = skPOD(skPO(j))
051300030423     C                   endif
051400030423     C                   if        currCOL = 2
051500030423     C                   seton                                        12
051600030423     C                   eval      stpP02  = skPO(j)
051700030423     C                   eval      stpP02D = skPOD(skPO(j))
051800030423     C                   endif
051900030423     C                   if        currCOL = 3
052000030423     C                   seton                                        13
052100030423     C                   eval      stpP03  = skPO(j)
052200030423     C                   eval      stpP03D = skPOD(skPO(j))
052300030423     C                   endif
052400030423     C                   if        currCOL = 4
052500030423     C                   seton                                        14
052600030423     C                   eval      stpP04  = skPO(j)
052700030423     C                   eval      stpP04D = skPOD(skPO(j))
052800030423     C                   endif
052900030423     C                   if        currCOL = 5
053000030423     C                   seton                                        15
053100030423     C                   eval      stpP05  = skPO(j)
053200030423     C                   eval      stpP05D = skPOD(skPO(j))
053300030423     C                   endif
053400030423     C                   if        currCOL = 6
053500030423     C                   seton                                        16
053600030423     C                   eval      stpP06  = skPO(j)
053700030423     C                   eval      stpP06D = skPOD(skPO(j))
053800030423     C                   endif
053900030423     C                   if        currCOL = 7
054000030423     C                   seton                                        17
054100030423     C                   eval      stpP07  = skPO(j)
054200030423     C                   eval      stpP07D = skPOD(skPO(j))
054300030423     C                   endif
054400030423     C                   if        currCOL = 8
054500030423     C                   seton                                        18
054600030423     C                   eval      stpP08  = skPO(j)
054700030423     C                   eval      stpP08D = skPOD(skPO(j))
054800030423     C                   endif
054900030423     C                   if        currCOL = 9
055000030423     C                   seton                                        19
055100030423     C                   eval      stpP09  = skPO(j)
055200030423     C                   eval      stpP09D = skPOD(skPO(j))
055300030423     C                   endif
055400041110      * se ho più di 9 p.o. devo intestare la colonna con 'ALTRI'
055500041110     c                   If        currCOL = 10
055600041110     C                   clear                   stpP09
055700041110     C                   eval      stpP09D = 'ALTRI'
055800041110     C                   endif
055900030422     C*
056000030423     C                   endif
056100030423     C                   eval      j = j + 1
056200030423     C                   enddo
056300030423     C*
056400030422     C                   ENDSR
056500030422     C*---------------------------------------------------------------*
056600030414
056700030414
056800030414
056900030414     C*---------------------------------------------------------------*
057000030414     C* CARTAB - Caricamento tabelle di wrk
057100030414     C*---------------------------------------------------------------*
057200030414     C     CARTAB        BEGSR
057300030423     C*
057400030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
057500030423     C                   MOVEL     '£1'          D06COD
057600030423     C                   MOVEL     SIMFEL        D06KEY
057700030423     C                   MOVEL     'L'           D06TLA
057800030423     C                   MOVEL     *BLANKS       D06ESC
057900030423     C                   MOVEL(P)  TRUL06DS      KPJBU
058000030423     C                   CALL      'TRUL06R'
058100030423     C                   PARM                    KPJBA
058200030423     C                   MOVEL     KPJBU         TRUL06DS
058300030414     C*
058400030423     C* Reperisco le rispettive denominazioni dei P.O.
058500030422     C                   clear                   skPO
058600030422     C                   clear                   skPOD
058700030423     C     1             do        30            i
058800030423     C                   if        L1(i) > *zeros
058900030423     C                   eval      skPO(L1(i)) = L1(i)
059000030423     C** Reperisco le descrizioni di ogni P.O.
059100030423     C     L1(i)         chain     azorg01l
059200030423     C                   if        %found(azorg01l)
059300030423     C                   eval      skPOD(L1(i)) = orgDES
059400030423     C                   else
059500030423     C                   eval      skPOD(L1(i)) = *all'*'
059600030423     C                   endif
059700030423     C                   endif
059800030512     C**
059900030512     C                   enddo
060000030512     C*
060100030512     C* Carico i tipi bolla dalla tabella 3A
060200030512     C                   z-add     *zeros        i
060300030512     C                   eval      tblCOD = '3A'
060400030512     C     KEYtabP       setll     tabel00f
060500030512     C     KEYtabP       reade     tabel00f
060600030512     C                   dow       not %eof(tabel00f)
060700030512     C                   movel(p)  tblUNI        DS3A
060800030512     C                   if        §3ARBL <> 'R'
060900030512     C                   add       1             i
061000030512     C                   eval      sk3A(i) = tblKEY
061100030512     C                   endif
061200030512     C     KEYtabP       reade     tabel00f
061300030512     C                   enddo
061400061108     C*
061500061108     C* Carico i tipi bolla dalla tabella 1b
061600061108     C                   z-add     *zeros        i
061700061108     C                   eval      tblCOD = '1B'
061800061108     C     KEYtabP       setll     tabel00f
061900061108     C     KEYtabP       reade     tabel00f
062000061108     C                   dow       not %eof(tabel00f)
062100061108     C                   movel(p)  tblUNI        DS1B
062200061108     C                   if        §1bfg8='N'
062300061108     C                   add       1             i
062400061108     C                   eval      sk1b(i) = tblKEY
062500061108     C                   endif
062600061108     C     KEYtabP       reade     tabel00f
062700061108     C                   enddo
062800030512     C*
062900030512     C* Carico i P.O. di tipo PT
063000041227     C**!!!              z-add     *zeros        i
063100041227     C**!!!*loval        setll     azorg01l
063200041227     C**!!!              read      azorg01l
063300041227     C**!!!              dow       not %eof(azorg01l)
063400041227     C**!!!              movel(p)  orgDE3        OG143
063500041227     C**!!!              if        §OGNTW = 'PPT'
063600041227     C**!!!              add       1             i
063700041227     C**!!!              eval      skPOPT(i) = orgFIL
063800041227     C**!!!              endif
063900041227     C**!!!              read      azorg01l
064000041227     C**!!!              enddo
064100030414     C*
064200030414     C                   ENDSR
064300030414     C*------------------------------------------------------------------------*
064400030414
064500030414
064600030414
064700030414
064800030414     C*------------------------------------------------------------------------*
064900030414     C* REPERISCE I DATI UTENTE
065000030414     C*------------------------------------------------------------------------*
065100030414     C     REPDATIUTE    BEGSR
065200030414     C*
065300030414     C* INIZIALIZZA VARIABILI DI WRK
065400030414     C                   CLEAR                   TIBS34DS
065500030414     C                   CLEAR                   AZUTEDS
065600030414     C                   CLEAR                   DDATIUTE
065700030414     C*
065800030414     C     *DTAARA       DEFINE    §azute        azuteds
065900030414     C     *DTAARA       DEFINE    §datiute      ddatiute
066000030414     C                   IN(E)     *DTAARA
066100030422     C                   IF        %Error
066200030422     C                   EVAL      I34Tla = 'L'
066300030414     C                   CALL      'TIBS34R'
066400030414     C                   PARM                    Tibs34Ds
066500030414     C                   IN        *DTAARA
066600030422     C                   ENDIF
066700030414     C*
066800030414     C                   ENDSR
066900030414     C*------------------------------------------------------------------------*
067000010509
067100010509
067200001218
067300001218     C*------------------------------------------------------------------------*
067400001218     C* *INZSR - ROUTINE INIZIALE
067500001218     C*------------------------------------------------------------------------*
067600001218     C     *INZSR        BEGSR
067700001218     C*
067800030422     C* Ricevo in la DS di architettura
067900010726     C     *ENTRY        PLIST
068000020613     C                   PARM                    KPJBA
068100030521     C*
068200030521     C* Reperisco l'UDATE del job
068300030521     C                   TIME                    WN14             14 0
068400030521     C                   MOVE      WN14          WN8               8 0
068500030521     C                   Z-ADD     WN8           G08DAT
068600030521     C                   Z-ADD     *zeros        G08INV
068700030521     C                   MOVEL     '0'           G08ERR
068800030521     C                   CALL      'XSRDA8'
068900030521     C                   PARM                    WLBDA8
069000030521     C                   Z-ADD     G08INV        DATCOR            8 0
069100010726     C*
069200030422     C* Inizializzo campi chiave "fissi"
069300030414     C                   Z-ADD     1             tblKUT
069400030422     C                   Z-ADD     *zeros        blpDUC
069500030414     C****
069600030414     C* Definizione chiavi
069700030414     C****
069800030512     C* Chiave su TABEL00F - parziale
069900030512     C     KEYtabP       KLIST
070000030414     C                   KFLD                    tblKUT
070100030414     C                   KFLD                    tblCOD
070200030422     C* Chiave su FNBLP06L - parziale
070300030422     C     KEYblp06P     KLIST
070400030422     C                   KFLD                    blpLNP
070500030422     C                   KFLD                    blpDUC
070600030414     C*
070700030414     C* REPERIMEMTO DATI UTENTE
070800030414     C                   EXSR      REPDATIUTE
070900030422     C*
071000030422     C* CARICAMENTO TABELLE DI WRK
071100030422     C                   EXSR      CARTAB
071200001218     C*
071300001218     C                   ENDSR
071400040430** MSG
071500040430Data errata                                                                   1
071600040430La data spedizione DAL non puo' essere maggiore della data AL                 2
071700040430Immettere almeno una delle due date                                           3
