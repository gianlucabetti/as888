000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130205     Fazcae05L  IF   E           K DISK
000400130206     FFNBLP05L  IF   E           K DISK    UsrOpn extfile(wFNBLP)
000401170529     FFNBLT01l  IF   E           K DISK    usropn extfile(wFNBLT)
000402170613     FFNBrv07l  IF   E           K DISK    usropn extfile(wFNBrv)
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
001401170613     D og143         E DS
001500030422     D*--------------------
001600030422     D* SCHIERE DI MEMORIZZAZIONE
001700030422     D*--------------------
001800170608     D sk3A            s              2    DIM(99)
001900061108     D sk1B            s              2    DIM(99)
002000170608     D skPO            s              3  0 DIM(31)
002001170612     D skntw           s              3    DIM(31)
002100170608     D skPOD           s             15    DIM(31)
002200170608     D skPOCOL         s              3  0 DIM(31)
002300170608     D skPOCOLok       s              3  0 DIM(31)
002400030422     D skTA            s              3  0 DIM(999)
002500030423     D skMemDTP        s              7  3 DIM(999)
002600030423     D skMemNTP        s              7  3 DIM(999)
002700030423     D skMemDatPO      ds                  OCCURS(999)
002800170608     D   skMemDPO                     7  3 DIM(31)
002900170608     D   skMemNPO                     7  3 DIM(31)
003000030414     D*--------------------
003100030414     D* VARIABILI DI WRK
003200030414     D*--------------------
003301170608     D i               s              4  0
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
004401170529     d Wvolu           s                   like(BLPvlf) inz
004402170529     d Wvol            s                   like(BLPvlf) inz
004403170529     d Wvolmed         s                   like(BLPvlf) inz
004404170529     d wncl            s                   like(BLPncl) inz
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
007201170529     d wFNBLT          s             21    inz('FILTRA201 /FNBLT01L')
007202170613     d wFNBRV          s             21    inz('FILTRA201 /FNBRV07L')
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
017200130702     c* se il terminal di arrivo è in sardegna --> attribuisco a 099 pisa
017300130702     c*  i MC a terra   tranne  se blptfp = 099 o tfp della sardegna
017400130702     c                   if        ilsd9ric<>'S' and blptfp<>099
017500130702     c     blptfp        chain     azorg01l
017600130702     c                   if        %found(azorg01l) and orgcar<>117
017700130702     c     blptfa        chain     azorg01l
017800130702     c                   if        %found(azorg01l) and orgcar=117
017900130702     c                   eval      blptfa=099
018000130702     c                   endif
018100130702     c                   endif
018200130702     c                   endif
018300130702     c
018400030422     C                   eval      skTA(blpTFA) = blpTFA
018500030423     C     skTA(blpTFA)  occur     skMemDatPO
018505170608     C
018506170529     c                   clear                   wvol
018507170529     c                   clear                   Wvolu
018508170529     c* Assumo come volume spedizione:
018509170529     c* il VDL se totale o parziale ma maggiore del vlf
018510170529     c* il vlf negli altri casi
018511170529     c                   if        BLPncr = BLPncl
018512170529     c                   z-add     BLPvlc        Wvolu
018513170529   x1c                   else
018514170529if  2c                   if        BLPvlc >= BLPvlf
018515170529     c                   z-add     BLPvlc        Wvolu
018516170529   x2c                   else
018517170529     c                   z-add     BLPvlf        Wvolu
018518170529e   2c                   endif
018519170529e   1c                   endif
018520170529      *
018521170529     c* spedizione non spareggiata
018600170529     c                   if        blpdpc=0
018601170529     c                   z-add     wvolu         wvol
018612170529     c                   else
018613170529     c* spedizione     spareggiata:
018614170529      *   Calcolo il volume medio
018615170529     c     Wvolu         div(H)    BLPncl        Wvolmed
018616170529     c                   clear                   wncl
018617170529     c*   leggo dettaglio colli per contare il numero dei colli a terra
018618170529     c     kblt          setll     fnblt01l
018619170529     c     kblt          reade     fnblt01l
018620170529     c                   dow       not %eof(fnblt01l)
018621170529     c                   if        bltdfv=0
018622170529     c                   add       1             wncl
018623170613     c* se bolla import dpd verifico se collo realmente spuntato
018624170613     c                   if        skntw(i)='DPD'
018625170613     c     kbrv7         setll     fnbrv07l
018628170613     c                   endif
018629170529     c                   ENDIF
018630170529     c     kblt          reade     fnblt01l
018631170529     c                   ENDDO
018632170529     c                   eval      wvol=wvolmed*wncl
018633170529     c                   ENDIF
018700030423     C* Evidenzio il volume merce "da partire" scindendo i servizi NON distribuz
018701170613     c* Per Dpd il totale sarà il totale spuntato
018702170613     c*       e il "Di cui distribuzione" sarà il totale bollettato
018703170613     c                   if        skntw(i)<>'DPD' or %equal(fnbrv07l)
018800170613     C                   eval      skMemDPO(i) = skMemDPO(i) + wvol
018900170529     C                   eval      skMemDTP(blpTFA) = skMemDTP(blpTFA) + wvol
018901170613     C                   endif
019000170613     C                   if        blpTSP = 'D' or skntw(i)='DPD'
019100170613     C                   eval      skMemNPO(i) = skMemNPO(i) + wvol
019200170529     C                   eval      skMemNTP(blpTFA) = skMemNTP(blpTFA) + wvol
019300030423     C                   endif
019302170608     C                   endif
019400010509     C*
019500010509     C                   ENDSR
019600010509     C*---------------------------------------------------------------*
019700030414
019800030414
019900030414
020000030414
020100030414     C*------------------------------------------------------------------------*
020200030422     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
020300030414     C*------------------------------------------------------------------------*
020400030414     C     STAMPA        BEGSR
020500030422     C*
020600030422     C* INNANZITUTTO ORDINO IN MODO CRESCENTE L'ELENCO DEI TERMINAL ARRIVO
020700030422     C                   sorta     skTA
020800030422     C*
020900030422     C* ABBINO LE COLONNE DI STAMPA AI P.O. IN GESTIONE AL TERMINAL PARTENZA
021000030422     C                   exsr      abbPOCOL
021100130205     c
021200130205     c                   if        ilsd9RIC<>'F'
021300130206     c                   clear                   stpD01
021400130206     c                   clear                   stpN01
021500130206     c                   clear                   stpgD01
021600130206     c                   clear                   stpgN01
021700130206     c                   clear                   stpD02
021800130206     c                   clear                   stpN02
021900130206     c                   clear                   stpgD02
022000130206     c                   clear                   stpgN02
022100130206     c                   clear                   stpD03
022200130206     c                   clear                   stpN03
022300130206     c                   clear                   stpgD03
022400130206     c                   clear                   stpgN03
022500130206     c                   clear                   stpD04
022600130206     c                   clear                   stpN04
022700130206     c                   clear                   stpgD04
022800130206     c                   clear                   stpgN04
022900130206     c                   clear                   stpD05
023000130206     c                   clear                   stpN05
023100130206     c                   clear                   stpgD05
023200130206     c                   clear                   stpgN05
023300130206     c                   clear                   stpD06
023400130206     c                   clear                   stpN06
023500130206     c                   clear                   stpgD06
023600130206     c                   clear                   stpgN06
023700130206     c                   clear                   stpD07
023800130206     c                   clear                   stpN07
023900130206     c                   clear                   stpgD07
024000130206     c                   clear                   stpgN07
024100130206     c                   clear                   stpD08
024200130206     c                   clear                   stpN08
024300130206     c                   clear                   stpgD08
024400130206     c                   clear                   stpgN08
024500130206     c                   clear                   stpD09
024600130206     c                   clear                   stpN09
024700130206     c                   clear                   stpgD09
024800130206     c                   clear                   stpgN09
024900130206     c                   clear                   stpD10
025000130206     c                   clear                   stpN10
025100130206     c                   clear                   stpgD10
025200130206     c                   clear                   stpgN10
025300130206     c                   clear                   stpD11
025400130206     c                   clear                   stpN11
025500130206     c                   clear                   stpgD11
025600130206     c                   clear                   stpgN11
025700130206     c                   clear                   stpD12
025800130206     c                   clear                   stpN12
025900130206     c                   clear                   stpgD12
026000130206     c                   clear                   stpgN12
026100130206     c                   clear                   stpD13
026200130206     c                   clear                   stpN13
026300130206     c                   clear                   stpgD13
026400130206     c                   clear                   stpgN13
026500130206     c                   clear                   stpD14
026600130206     c                   clear                   stpN14
026700130206     c                   clear                   stpgD14
026800130206     c                   clear                   stpgN14
026900130206     c                   clear                   stpD15
027000130206     c                   clear                   stpN15
027100130206     c                   clear                   stpgD15
027200130206     c                   clear                   stpgN15
027300130206     c                   clear                   depdtp
027400130206     c                   clear                   depntp
027500130206     c                   clear                   stpdtp
027600130206     c                   clear                   stpntp
027700130206     c                   clear                   stpgdtp
027800130206     c                   clear                   stpgNtp
027900030423     C*
028000030423     C* STAMPO TESTATA
028100030423     C                   eval      stpSYF = RSUT
028200030507     C                   write     LSD1PT
028300090625     C  n20              write     LSD1PI
028400090625     C   20              write     LSD1PI15
028500090625     c                   setoff                                       01
028600130205     c                   endif
028700030422     C*
028800030422     C* STAMPO DETTAGLIO X TERMINAL ARRIVO (solo se c'è qualcosa effettivamente
028900030422     C                   z-add     1             i
029000030422     C                   dow       i <= %elem(skTA)
029100030422     C*
029200030422     C* SOLO X I T.A. VALIDI
029300030422     C                   if        skTA(i) > *zeros
029400030423     C     skTA(i)       occur     skMemDatPO
029500030422     C*
029600030423     C* SOLO SE C'E' QUALCOSA DA STAMPARE
029700130205     C                   eval      depDTP = skMemDTP(skTA(i))
029800030423     C                   eval      depNTP = skMemNTP(skTA(i))
029900030423     C                   if        depDTP > *zeros OR
030000030423     C                             depNTP > *zeros
030100030422     C*
030200030422     C* STAMPO I TOTALI T.P. X T.A.
030300130320     c                   if        ilsd9RIC<>'F'
030400030507     C                   clear                   LSD1P2
030500030422     C                   eval      stpTA  = skTA(i)
030600030428     C                   eval(h)   stpDTP = skMemDTP(skTA(i))
030700030428     C                   eval(h)   stpNTP = skMemNTP(skTA(i))
030800041111     c                   eval      stpgDTP = stpgDTP + stpDTP
030900041111     c                   eval      stpgNTP = stpgNTP + stpNTP
031000130320     c                   endif
031100130205     c
031200130205     c                   if        ilsd9ric<>'S'
031300130205     c                   clear                   wfsat000
031400130205     c                   eval      sattfp=ktfp
031500130205     c                   eval      sattfa=skTa(i)
031600130205     c                   eval      satdre=ilsd9dsa
031700130205     c                   eval      sattre=' '
031800130205     c                   eval      satvpt=skMemDTP(skTA(i))
031900130320     c
032000130320     c                   if        skMemNTP(skTA(i)) < 99999,9
032100130205     c                   eval      satvpd=skMemNTP(skTA(i))
032200130320     c                   else
032300130320     c                   eval      satvpd=99999,9
032400130320     c                   endif
032500130320     c
032600130205     c                   write     wfsat000
032700130205     C                   endif
032800130205     c
032900130205     c* Se richiesta anche stampa
033000130205     c                   if        ilsd9RIC<>'F'
033100130205     c
033200030422     C                   z-add     1             j
033300030422     C                   dow       j <= %elem(skPO)
033400030422     C*
033500030422     C* SOLO X I P.O. VALIDI
033600030422     C                   if        skPO(j) > *zeros
033700030422     C                   if        skPOCOL(skPO(j))  > *zeros AND
033800030422     C                             skPOCOL(skPO(j)) <> 999
033900030422     C* REPERISCO LA COLONNA A CUI STAMPARE IL DATO CORRENTE
034000030422     C*
034100030422     C** Colonna 1
034200030422     C                   if        skPOCOL(skPO(j)) = 1
034300030428     C                   eval(h)   stpD01  = skMemDPO(skPO(j))
034400030428     C                   eval(h)   stpN01  = skMemNPO(skPO(j))
034500041111     c                   eval      stpgD01 = stpgD01 + stpD01
034600041111     c                   eval      stpgN01 = stpgN01 + stpN01
034700130205     c* Se richeisto scrivo anche il file
034800130205     C                   endif
034900030422     C** Colonna 2
035000030422     C                   if        skPOCOL(skPO(j)) = 2
035100030428     C                   eval(h)   stpD02  = skMemDPO(skPO(j))
035200030428     C                   eval(h)   stpN02  = skMemNPO(skPO(j))
035300041111     c                   eval      stpgD02 = stpgD02 + stpD02
035400041111     c                   eval      stpgN02 = stpgN02 + stpN02
035500030422     C                   endif
035600030422     C** Colonna 3
035700030422     C                   if        skPOCOL(skPO(j)) = 3
035800030428     C                   eval(h)   stpD03  = skMemDPO(skPO(j))
035900030428     C                   eval(h)   stpN03  = skMemNPO(skPO(j))
036000041111     c                   eval      stpgD03 = stpgD03 + stpD03
036100041111     c                   eval      stpgN03 = stpgN03 + stpN03
036200030422     C                   endif
036300030422     C** Colonna 4
036400030422     C                   if        skPOCOL(skPO(j)) = 4
036500030428     C                   eval(h)   stpD04  = skMemDPO(skPO(j))
036600030428     C                   eval(h)   stpN04  = skMemNPO(skPO(j))
036700041111     c                   eval      stpgD04 = stpgD04 + stpD04
036800041111     c                   eval      stpgN04 = stpgN04 + stpN04
036900030422     C                   endif
037000030422     C** Colonna 5
037100030422     C                   if        skPOCOL(skPO(j)) = 5
037200030428     C                   eval(h)   stpD05  = skMemDPO(skPO(j))
037300030428     C                   eval(h)   stpN05  = skMemNPO(skPO(j))
037400041111     c                   eval      stpgD05 = stpgD05 + stpD05
037500041111     c                   eval      stpgN05 = stpgN05 + stpN05
037600030422     C                   endif
037700030422     C** Colonna 6
037800030422     C                   if        skPOCOL(skPO(j)) = 6
037900030428     C                   eval(h)   stpD06  = skMemDPO(skPO(j))
038000030428     C                   eval(h)   stpN06  = skMemNPO(skPO(j))
038100041111     c                   eval      stpgD06 = stpgD06 + stpD06
038200041111     c                   eval      stpgN06 = stpgN06 + stpN06
038300030422     C                   endif
038400030422     C** Colonna 7
038500030422     C                   if        skPOCOL(skPO(j)) = 7
038600030428     C                   eval(h)   stpD07  = skMemDPO(skPO(j))
038700030428     C                   eval(h)   stpN07  = skMemNPO(skPO(j))
038800041111     c                   eval      stpgD07 = stpgD07 + stpD07
038900041111     c                   eval      stpgN07 = stpgN07 + stpN07
039000030422     C                   endif
039100030422     C** Colonna 8
039200030422     C                   if        skPOCOL(skPO(j)) = 8
039300030428     C                   eval(h)   stpD08  = skMemDPO(skPO(j))
039400030428     C                   eval(h)   stpN08  = skMemNPO(skPO(j))
039500041111     c                   eval      stpgD08 = stpgD08 + stpD08
039600041111     c                   eval      stpgN08 = stpgN08 + stpN08
039700030422     C                   endif
039800030422     C** Colonna 9
039900030422     C                   if        skPOCOL(skPO(j)) = 9
040000030428     C                   eval(h)   stpD09  = skMemDPO(skPO(j))
040100030428     C                   eval(h)   stpN09  = skMemNPO(skPO(j))
040200041111     c                   eval      stpgD09 = stpgD09 + stpD09
040300041111     c                   eval      stpgN09 = stpgN09 + stpN09
040400030422     C                   endif
040500090625     C** Colonna 10
040600090625     C                   if        skPOCOL(skPO(j)) = 10
040700090625     C                   eval(h)   stpD10  = skMemDPO(skPO(j))
040800090625     C                   eval(h)   stpN10  = skMemNPO(skPO(j))
040900090625     c                   eval      stpgD10 = stpgD10 + stpD10
041000090625     c                   eval      stpgN10 = stpgN10 + stpN10
041100090625     C                   endif
041200090625     C** Colonna 11
041300090625     C                   if        skPOCOL(skPO(j)) = 11
041400090625     C                   eval(h)   stpD11  = skMemDPO(skPO(j))
041500090625     C                   eval(h)   stpN11  = skMemNPO(skPO(j))
041600090625     c                   eval      stpgD11 = stpgD11 + stpD11
041700090625     c                   eval      stpgN11 = stpgN11 + stpN11
041800090625     C                   endif
041900090625     C** Colonna 12
042000090625     C                   if        skPOCOL(skPO(j)) = 12
042100090625     C                   eval(h)   stpD12  = skMemDPO(skPO(j))
042200090625     C                   eval(h)   stpN12  = skMemNPO(skPO(j))
042300090625     c                   eval      stpgD12 = stpgD12 + stpD12
042400090625     c                   eval      stpgN12 = stpgN12 + stpN12
042500090625     C                   endif
042600090625     C** Colonna 13
042700090625     C                   if        skPOCOL(skPO(j)) = 13
042800090625     C                   eval(h)   stpD13  = skMemDPO(skPO(j))
042900090625     C                   eval(h)   stpN13  = skMemNPO(skPO(j))
043000090625     c                   eval      stpgD13 = stpgD13 + stpD13
043100090625     c                   eval      stpgN13 = stpgN13 + stpN13
043200090625     C                   endif
043300090625     C** Colonna 14
043400090625     C                   if        skPOCOL(skPO(j)) = 14
043500090625     C                   eval(h)   stpD14  = skMemDPO(skPO(j))
043600090625     C                   eval(h)   stpN14  = skMemNPO(skPO(j))
043700090625     c                   eval      stpgD14 = stpgD14 + stpD14
043800090625     c                   eval      stpgN14 = stpgN14 + stpN14
043900090625     C                   endif
044000090625     C** Colonna 15
044100090625     C                   if        skPOCOL(skPO(j)) = 15
044200090625     C                   eval(h)   stpD15  = skMemDPO(skPO(j))
044300090625     C                   eval(h)   stpN15  = skMemNPO(skPO(j))
044400090625     c                   eval      stpgD15 = stpgD15 + stpD15
044500090625     c                   eval      stpgN15 = stpgN15 + stpN15
044600090625     C                   endif
044700090625      * se ho più di 15 colonne devo sommare tutti i p.o. rimanenti
044800041110      * sulla nona colonna
044900090625     c                   if        skPOCOL(skPO(j)) > 15
045000090625     C                   eval(h)   stpD15  = stpD15 + skMemDPO(skPO(j))
045100090625     C                   eval(h)   stpN15  = stpN15 + skMemNPO(skPO(j))
045200090625     c                   eval      stpgD15 = stpgD15 + stpD15
045300090625     c                   eval      stpgN15 = stpgN15 + stpN15
045400041110     C                   endif
045500030422     C*
045600030422     C                   endif
045700030422     C                   endif
045800030422     C*
045900030422     C* SCORRO L'ELENCO P.O. X T.A. CORRENTE
046000030422     C                   eval      j = j + 1
046100030422     C                   enddo
046200030422     C*
046300090625     c
046400090625     c* testata
046500090625     c                   if        *in01
046600090625     C                   write     LSD1PT
046700090625     C  n20              write     LSD1PI
046800090625     C   20              write     LSD1PI15
046900090625     c                   setoff                                       01
047000090625     c                   endif
047100090625     c
047200090625     C* STAMPO LA RIGA DI DETTAGLIO
047300090625     C  n20              write     LSD1P2
047400090625     C   20              write     LSD1P215
047500030422     C                   endif
047600130205     c
047700130205     C                   endif
047800030422     C                   endif
047900030422     C*
048000030422     C* SCORRO L'ELENCO DEI T.A.
048100030422     C                   eval      i = i + 1
048200030422     C                   enddo
048300041111     C*
048400130205     c                   if        ilsd9RIC<>'F'
048500041111     C* STAMPO TOTALI
048600090625     c                   write     LSD1RI
048700090625     C  n20              write     LSD1TG
048800090625     C   20              write     LSD1TG15
048900030422     C*
049000030422     C* STAMPO CHIUSURA PROSPETTO
049100030507     C                   write     LSD1RI
049200030507     C                   write     LSD1FI
049300130205     c                   endif
049400030414     C*
049500030414     C                   ENDSR
049600030414     C*------------------------------------------------------------------------*
049700030422
049800030422
049900030422
050000030422
050100030422     C*------------------------------------------------------------------------*
050200030422     C* ABBPOCOL -  Routine di abbinamento P.O. a colonne in stampa
050300030422     C*------------------------------------------------------------------------*
050400030422     C     ABBPOCOL      BEGSR
050500030423     C*
050600030423     C                   setoff                                       111213
050700030423     C                   setoff                                       141516
050800030423     C                   setoff                                       171819
050900090625     C                   setoff                                       20
051000030422     C*
051100030422     C* Innanzitutto ordino in modo crescente l'elenco dei P.O.
051200030422     C                   sorta     skPO
051300030423     C*
051400030423     C* Di default abbino alla 1° colonna il P.O. terminal di partenza e alle su
051500030423     C* gli altri P.O. in ordine crescente
051600030423     C                   clear                   skPOCOL
051700030423     C                   z-add     1             currCOL
051800130205     C                   eval      skPOCOL(ktfp) = currCOL
051900030423     C                   z-add     1             i
052000030423     C                   dow       i <= %elem(skPO)
052100030423     C                   if        skPO(i)  > *zeros AND
052200130205     C                             skPO(i) <> ktfp
052300030423     C                   eval      currCOL = currCOL + 1
052400030423     C                   eval      skPOCOL(skPO(i)) = currCOL
052500030423     C                   endif
052600030423     C                   eval      i = i + 1
052700030423     C                   enddo
052800030422     C*
052900030423     C* Eseguo verifica valorizzazioni x T.A.
053000030423     C                   clear                   skPOCOLok
053100030422     C*
053200030422     C                   z-add     1             i
053300030422     C                   dow       i <= %elem(skTA)
053400030422     C                   if        skTA(i) > *zeros
053500030423     C     skTA(i)       occur     skMemDatPO
053600030422     C                   z-add     1             j
053700030422     C                   dow       j <= %elem(skPO)
053800030422     C*
053900030422     C* Solo x i P.O. validi
054000030422     C                   if        skPO(j) > *zeros
054100030422     C*
054200030423     C* Solo se ci sono dati da stampare
054300030423     C                   eval      depDTP = skMemDPO(skPO(j))
054400030423     C                   eval      depNTP = skMemNPO(skPO(j))
054500030423     C                   if        depDTP > *zeros OR
054600030423     C                             depNTP > *zeros
054700030423     C                   if        skPOCOLok(skPO(j)) = *zeros
054800030422     C*
054900030423     C* Flaggo a OK la colonna relativa al P.O. da stampare
055000030423     C                   eval      skPOCOLok(skPO(j)) = 999
055100030423     C                   endif
055200030423     C                   endif
055300030423     C                   endif
055400030423     C                   eval      j = j + 1
055500030423     C                   enddo
055600030423     C                   endif
055700030423     C                   eval      i = i + 1
055800030423     C                   enddo
055900030423     C*
056000030423     C* A questo punto "mergio" le collonne di default con quelle effettivamente
056100030423     C                   z-add     1             i
056200030423     C                   dow       i <= %elem(skPO)
056300030423     C                   if        skPO(i) > *zeros
056400030423     C                   if        skPOCOLok(skPO(i)) = *zeros
056500030423     C                   eval      depCOL = skPOCOL(skPO(i))
056600030423     C* Diminuisco di 1 le posizioni >= alla colonna relativa al P.O. da nn stam
056700030423     C                   z-add     1             j
056800030423     C                   dow       j <= %elem(skPOCOL)
056900030423     C                   if        skPOCOL(j) = depCOL
057000030423     C                   eval      skPOCOL(j) = *zeros
057100030423     C                   endif
057200030423     C                   if        skPOCOL(j) > depCOL
057300030423     C                   eval      skPOCOL(j) = skPOCOL(j) - 1
057400030423     C                   endif
057500030423     C                   eval      j = j + 1
057600030423     C                   enddo
057700030423     C                   endif
057800030423     C                   endif
057900030423     C                   eval      i = i + 1
058000030423     C                   enddo
058100130206     c                   clear                   stpP01
058200130206     c                   clear                   stpP01D
058300130206     c                   clear                   stpP02
058400130206     c                   clear                   stpP02D
058500130206     c                   clear                   stpP03
058600130206     c                   clear                   stpP03D
058700130206     c                   clear                   stpP04
058800130206     c                   clear                   stpP04D
058900130206     c                   clear                   stpP05
059000130206     c                   clear                   stpP05D
059100130206     c                   clear                   stpP06
059200130206     c                   clear                   stpP06D
059300130206     c                   clear                   stpP07
059400130206     c                   clear                   stpP07D
059500130206     c                   clear                   stpP08
059600130206     c                   clear                   stpP08D
059700130206     c                   clear                   stpP09
059800130206     c                   clear                   stpP09D
059900130206     c                   clear                   stpP10
060000130206     c                   clear                   stpP10D
060100130206     c                   clear                   stpP11
060200130206     c                   clear                   stpP11D
060300130206     c                   clear                   stpP12
060400130206     c                   clear                   stpP12D
060500130206     c                   clear                   stpP13
060600130206     c                   clear                   stpP13D
060700130206     c                   clear                   stpP14
060800130206     c                   clear                   stpP14D
060900130206     c                   clear                   stpP15
061000130206     c                   clear                   stpP15D
061100030423     C*
061200030423     C* Effettuo 1 ultimo giro x valorizzare le intestazioni delle colonne da st
061300030423     C                   z-add     1             j
061400030423     C                   dow       j <= %elem(skPO)
061500030423     C                   if        skPO(j) > *zeros
061600030423     C                   eval      currCOL = skPOCOL(skPO(j))
061700030423     C*
061800030423     C                   if        currCOL = 1
061900030423     C                   seton                                        11
062000030423     C                   eval      stpP01  = skPO(j)
062100030423     C                   eval      stpP01D = skPOD(skPO(j))
062200030423     C                   endif
062300030423     C                   if        currCOL = 2
062400030423     C                   seton                                        12
062500030423     C                   eval      stpP02  = skPO(j)
062600030423     C                   eval      stpP02D = skPOD(skPO(j))
062700030423     C                   endif
062800030423     C                   if        currCOL = 3
062900030423     C                   seton                                        13
063000030423     C                   eval      stpP03  = skPO(j)
063100030423     C                   eval      stpP03D = skPOD(skPO(j))
063200030423     C                   endif
063300030423     C                   if        currCOL = 4
063400030423     C                   seton                                        14
063500030423     C                   eval      stpP04  = skPO(j)
063600030423     C                   eval      stpP04D = skPOD(skPO(j))
063700030423     C                   endif
063800030423     C                   if        currCOL = 5
063900030423     C                   seton                                        15
064000030423     C                   eval      stpP05  = skPO(j)
064100030423     C                   eval      stpP05D = skPOD(skPO(j))
064200030423     C                   endif
064300030423     C                   if        currCOL = 6
064400030423     C                   seton                                        16
064500030423     C                   eval      stpP06  = skPO(j)
064600030423     C                   eval      stpP06D = skPOD(skPO(j))
064700030423     C                   endif
064800090625     C                   if        currCOL = 7
064900030423     C                   seton                                        17
065000030423     C                   eval      stpP07  = skPO(j)
065100030423     C                   eval      stpP07D = skPOD(skPO(j))
065200030423     C                   endif
065300030423     C                   if        currCOL = 8
065400030423     C                   seton                                        18
065500030423     C                   eval      stpP08  = skPO(j)
065600030423     C                   eval      stpP08D = skPOD(skPO(j))
065700030423     C                   endif
065800030423     C                   if        currCOL = 9
065900030423     C                   seton                                        19
066000030423     C                   eval      stpP09  = skPO(j)
066100030423     C                   eval      stpP09D = skPOD(skPO(j))
066200030423     C                   endif
066300090625     C                   if        currCOL = 10
066400090625     C                   seton                                        20
066500090625     C                   eval      stpP10  = skPO(j)
066600090625     C                   eval      stpP10D = skPOD(skPO(j))
066700090625     C                   endif
066800090625     C                   if        currCOL = 11
066900090625     C                   seton                                        20
067000090625     C                   eval      stpP11  = skPO(j)
067100090625     C                   eval      stpP11D = skPOD(skPO(j))
067200090625     C                   endif
067300090625     C                   if        currCOL = 12
067400090625     C                   seton                                        20
067500090625     C                   eval      stpP12  = skPO(j)
067600090625     C                   eval      stpP12D = skPOD(skPO(j))
067700090625     C                   endif
067800090625     C                   if        currCOL = 13
067900090625     C                   seton                                        20
068000090625     C                   eval      stpP13  = skPO(j)
068100090625     C                   eval      stpP13D = skPOD(skPO(j))
068200090625     C                   endif
068300090625     C                   if        currCOL = 14
068400090625     C                   seton                                        20
068500090625     C                   eval      stpP14  = skPO(j)
068600090625     C                   eval      stpP14D = skPOD(skPO(j))
068700090625     C                   endif
068800090625     C                   if        currCOL = 15
068900090625     C                   seton                                        20
069000090625     C                   eval      stpP15  = skPO(j)
069100090625     C                   eval      stpP15D = skPOD(skPO(j))
069200090625     C                   endif
069300090625      * se ho più di 15 p.o. devo intestare la colonna con 'ALTRI'
069400090625     c                   If        currCOL > 15
069500090625     C                   clear                   stpP15
069600090625     C                   eval      stpP15D = 'ALTRI'
069700041110     C                   endif
069800030422     C*
069900030423     C                   endif
070000030423     C                   eval      j = j + 1
070100030423     C                   enddo
070200030423     C*
070300030422     C                   ENDSR
070400030422     C*---------------------------------------------------------------*
070500030414
070600030414
070700030414
070800030414     C*---------------------------------------------------------------*
070900130205     C* CAR_LNP - Caricamento tabelle di wrk
071000030414     C*---------------------------------------------------------------*
071100130205     C     CAR_LNP       BEGSR
071200130205    1c                   if        ilsd9TFP>0
071300130205     c                   eval      ktfp=ilsd9tfp
071400130205     c                   exsr      ElaTFP
071500130205     c                   exsr      Procedi
071600130205     c
071700130205   x1c                   else
071800130205     c
071900130205     c                   eval      kepa='P'
072000130205     c     kepa          setll     azcae05l
072100130205     c     kepa          reade     azcae05l
072200130205    2c                   dow       not %eof(azcae05l)
072300130205     c                   if        caedde<=ilsd9dsa and caedsc>=ilsd9dsa
072400130205     c                             and caeatb=' '
072500130205     c
072600130205     c                   eval      ktfp=caetfe
072700130205     c                   exsr      ElaTFP
072800130205     c                   exsr      Procedi
072900130205     c
073000130205     c     kcae          setgt     azcae05l
073100130205     c                   endif
073200130205     c
073300130205     c     kepa          reade     azcae05l
073400130205    2c                   enddo
073500130205     c
073600130205    1c                   endif
073700130205     c
073800130205     c                   ENDSR
073900130205     C*---------------------------------------------------------------*
074000130205     C* ELATFP - Caricamento linee di partenza del terminal partenza
074100130205     C*---------------------------------------------------------------*
074200130205     C     ELATFP        BEGSR
074300030423     C*
074400030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
074500130205     c                   clear                   trul06ds
074600030423     C                   MOVEL     '£1'          D06COD
074700130205     C                   MOVEL     kTFP          D06KEY
074800130205     c                   if        ilsd9TFP>0
074900030423     C                   MOVEL     'L'           D06TLA
075000130205     c                   endif
075100130205     C                   MOVE      'S'           D06KEY
075200130205     c                   eval      d06drf=ilsd9DSA
075300130205     c
075400030423     C                   MOVEL(P)  TRUL06DS      KPJBU
075500030423     C                   CALL      'TRUL06R'
075600030423     C                   PARM                    KPJBA
075700030423     C                   MOVEL     KPJBU         TRUL06DS
075800030414     C*
075900030423     C* Reperisco le rispettive denominazioni dei P.O.
076000030422     C                   clear                   skPO
076100030422     C                   clear                   skPOD
076200130206     C                   clear                   skta
076300130206     C                   clear                   skpocol
076400130206     C                   clear                   skpocolok
076500030423     C     1             do        30            i
076600030423     C                   if        L1(i) > *zeros
076700170608     C*                  eval      skPO(L1(i)) = L1(i)
076701170608     C                   eval      skPO(i) = L1(i)
076800030423     C** Reperisco le descrizioni di ogni P.O.
076900030423     C     L1(i)         chain     azorg01l
077000030423     C                   if        %found(azorg01l)
077100170608     C*                  eval      skPOD(L1(i)) = orgDES
077101170608     C                   eval      skPOD(i) = orgDES
077102170613     C                   MOVEL     ORGDE3        OG143
077103170613     c                   eval      skntw(i) = §ogntw
077200030423     C                   else
077300170608     C*                  eval      skPOD(L1(i)) = *all'*'
077301170608     C                   eval      skPOD(i) = *all'*'
077302170613     c                   eval      skntw(i) = *blanks
077400030423     C                   endif
077500030423     C                   endif
077600030512     C**
077700030512     C                   enddo
077800130205     c                   ENDSR
077900130205     C*---------------------------------------------------------------*
078000130205     C* CARTAB - Caricamento tabelle di wrk
078100130205     C*---------------------------------------------------------------*
078200130205     C     CARTAB        BEGSR
078300030512     C*
078400030512     C* Carico i tipi bolla dalla tabella 3A
078500030512     C                   z-add     *zeros        i
078600030512     C                   eval      tblCOD = '3A'
078700030512     C     KEYtabP       setll     tabel00f
078800030512     C     KEYtabP       reade     tabel00f
078900030512     C                   dow       not %eof(tabel00f)
079000030512     C                   movel(p)  tblUNI        DS3A
079100030512     C                   if        §3ARBL <> 'R'
079200030512     C                   add       1             i
079300030512     C                   eval      sk3A(i) = tblKEY
079400030512     C                   endif
079500030512     C     KEYtabP       reade     tabel00f
079600030512     C                   enddo
079700061108     C*
079800130205     C* Carico i tipi bolla dalla tabella 1B
079900061108     C                   z-add     *zeros        i
080000061108     C                   eval      tblCOD = '1B'
080100061108     C     KEYtabP       setll     tabel00f
080200061108     C     KEYtabP       reade     tabel00f
080300061108     C                   dow       not %eof(tabel00f)
080400061108     C                   movel(p)  tblUNI        DS1B
080500061108     C                   if        §1bfg8='N'
080600061108     C                   add       1             i
080700061108     C                   eval      sk1b(i) = tblKEY
080800061108     C                   endif
080900061108     C     KEYtabP       reade     tabel00f
081000061108     C                   enddo
081100030414     C*
081200030414     C                   ENDSR
081300030414     C*------------------------------------------------------------------------*
081400030414     C* REPERISCE I DATI UTENTE
081500030414     C*------------------------------------------------------------------------*
081600030414     C     REPDATIUTE    BEGSR
081700030414     C*
081800030414     C* INIZIALIZZA VARIABILI DI WRK
081900030414     C                   CLEAR                   TIBS34DS
082000030414     C                   CLEAR                   AZUTEDS
082100030414     C                   CLEAR                   DDATIUTE
082200030414     C*
082300030414     C     *DTAARA       DEFINE    §azute        azuteds
082400030414     C     *DTAARA       DEFINE    §datiute      ddatiute
082500030414     C                   IN(E)     *DTAARA
082600030422     C                   IF        %Error
082700030422     C                   EVAL      I34Tla = 'L'
082800030414     C                   CALL      'TIBS34R'
082900030414     C                   PARM                    Tibs34Ds
083000030414     C                   IN        *DTAARA
083100030422     C                   ENDIF
083200030414     C*
083300030414     C                   ENDSR
083400030414     C*------------------------------------------------------------------------*
083500010509
083600010509
083700001218
083800001218     C*------------------------------------------------------------------------*
083900001218     C* *INZSR - ROUTINE INIZIALE
084000001218     C*------------------------------------------------------------------------*
084100001218     C     *INZSR        BEGSR
084200001218     C*
084300030422     C* Ricevo in la DS di architettura
084400010726     C     *ENTRY        PLIST
084500020613     C                   PARM                    KPJBA
084600130205     c                   movel     kpjbu         fnlsd9ds
084700030521     C*
084800030521     C* Reperisco l'UDATE del job
084900030521     C                   TIME                    WN14             14 0
085000030521     C                   MOVE      WN14          WN8               8 0
085100030521     C                   Z-ADD     WN8           G08DAT
085200030521     C                   Z-ADD     *zeros        G08INV
085300030521     C                   MOVEL     '0'           G08ERR
085400030521     C                   CALL      'XSRDA8'
085500030521     C                   PARM                    WLBDA8
085600030521     C                   Z-ADD     G08INV        DATCOR            8 0
085700010726     C*
085800030422     C* Inizializzo campi chiave "fissi"
085900030414     C                   Z-ADD     1             tblKUT
086000030422     C                   Z-ADD     *zeros        blpDUC
086100130206     c* Giro le date per la stampa
086200130206     c                   clear                   v1cdsd
086300130206     c                   if        ilsd9dsd>0
086400130206     c     *iso          movel     ilsd9dsd      dataiso
086500130206     c                   movel     dataiso       dataeur
086600130206     c                   movel     dataeur       v1cdsd
086700130206     c                   endif
086800130206     c
086900130206     c                   clear                   v1cdsa
087000130206     c                   if        ilsd9dsa>0
087100130206     c     *iso          movel     ilsd9dsa      dataiso
087200130206     c                   movel     dataiso       dataeur
087300130206     c                   movel     dataeur       v1cdsa
087400130206     c                   endif
087500130205     c
087600030414     C****
087700030414     C* Definizione chiavi
087800030414     C****
087900030512     C* Chiave su TABEL00F - parziale
088000030512     C     KEYtabP       KLIST
088100030414     C                   KFLD                    tblKUT
088200030414     C                   KFLD                    tblCOD
088300101126     C* Chiave su FNBLP05L - parziale
088400101126     C     KEYblp05P     KLIST
088500030422     C                   KFLD                    blpLNP
088600030422     C                   KFLD                    blpDUC
088700130205     C     Kcae          KLIST
088800130205     C                   KFLD                    kepa
088900130205     C                   KFLD                    caetfe
088901170529     C     Kblt          KLIST
088902170529     C                   KFLD                    blpaas
088903170529     C                   KFLD                    blplnp
088904170529     C                   KFLD                    blpnrs
088905170529     C                   KFLD                    blpnsp
088906170613     C     Kbrv7         KLIST
088907170613     C                   KFLD                    bltfls
088908170613     C                   KFLD                    bltlna
088909170613     C                   KFLD                    bltnrs
088910170613     C                   KFLD                    bltnsc
088911170613     C                   KFLD                    ktfp
089000130205     c* Pulizia file di work se richiesto
089100130205      /FREE
089200130205       exec sql delete from WFSAT00F    ;
089300130205      /END-FREE
089400030414     C*
089500030414     C* REPERIMEMTO DATI UTENTE
089600030414     C                   EXSR      REPDATIUTE
089700030422     C*
089800030422     C* CARICAMENTO TABELLE DI WRK
089900030422     C                   EXSR      CARTAB
090000130206     c*
090100130206     c* Apertura file
090200130206     c                   if        %subst( knsif : 7 : 1) = 'P'
090300130206     c                   eval      %subst( wFNBLP : 7 : 3 ) = 'PRD'
090301170529     c                   eval      %subst( wFNBLT : 7 : 3 ) = 'PRD'
090302170613     c                   eval      %subst( wFNBRV : 7 : 3 ) = 'PRD'
090400130206     c                   eval      %subst( wSAT : 7 : 3 ) = 'AZP'
090500130206     c                   endif
090600130206     c
090700130206     c                   open      fnblp05l
090701170529     c                   open      fnblt01l
090702170613     c                   open      fnbrv07l
090800130206     c                   open      wfsat00f
090900001218     C*
091000001218     C                   ENDSR
