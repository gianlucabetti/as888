000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------
000300130205     Fazcae05L  IF   E           K DISK
000400130206     FFNBLP05L  IF   E           K DISK    UsrOpn extfile(wFNBLP)
000500170529     FFNBLT01l  IF   E           K DISK    usropn extfile(wFNBLT)
000600170613     FFNBrv07l  IF   E           K DISK    usropn extfile(wFNBrv)
000700030422     FAZORG01L  IF   E           K DISK
000800030414     FTABEL00F  IF   E           K DISK
000900130205     FFNLSD9P   O    E             PRINTER OFLIND(*IN01)
001000130206     fWFSAT00F  o    e             disk    UsrOpn extfile(wSAT)
001100001218     D*--------------------
001200001218     D* DS ESTERNE
001300001218     D*--------------------
001400900517     D KPJBA         E DS
001500030512     D DS3A          E DS
001600061108     D DS1B          E DS
001700170613     D og143         E DS
001800030422     D*--------------------
001900030422     D* SCHIERE DI MEMORIZZAZIONE
002000030422     D*--------------------
002100170608     D sk3A            s              2    DIM(99)
002200061108     D sk1B            s              2    DIM(99)
002300170613     D skPO            s              3  0 DIM(999)
002400170613     D skntw           s              3    DIM(999)
002500170613     D skPOD           s             15    DIM(999)
002600170613     D skPOCOL         s              3  0 DIM(999)
002700170613     D skPOCOLok       s              3  0 DIM(999)
002800030422     D skTA            s              3  0 DIM(999)
002900030423     D skMemDTP        s              7  3 DIM(999)
003000030423     D skMemNTP        s              7  3 DIM(999)
003001171121     D skMemSTP        s              7  3 DIM(999)
003100030423     D skMemDatPO      ds                  OCCURS(999)
003200170613     D   skMemDPO                     7  3 DIM(999)
003300170613     D   skMemNPO                     7  3 DIM(999)
003400030414     D*--------------------
003500030414     D* VARIABILI DI WRK
003600030414     D*--------------------
003700170608     D i               s              4  0
003800030422     D j               s              4  0
003900170614     D currCOLD        s              3  0
004000030422     D currCOL         s              3  0
004100030423     D depCOL          s              3  0
004200030423     D depDTP          s              5  1
004300030423     D depNTP          s              5  1
004400040430     D wdsd            s                   like(v1cdsd)
004500040430     D wdsa            s                   like(v1cdsa)
004600130205     D kepa            s                   like(caeepa)
004700130205     D ktfp            s                   like(caetfp)
004800130206     d Dataiso         s               d   datfmt(*iso)
004900130206     d Dataeur         s               d   datfmt(*eur)
005000170529     d Wvolu           s                   like(BLPvlf) inz
005100170529     d Wvol            s                   like(BLPvlf) inz
005200170529     d Wvolmed         s                   like(BLPvlf) inz
005300170529     d wncl            s                   like(BLPncl) inz
005400170620     d wspu            s              1
005500170614     d NDpd            s              2  0
005600030414     D*---
005700030414     D* DS REPERIMENTO DATI UTENTE
005800030414     D*---
005900030414     D TIBS34DS      E DS
006000030414     D DDATIUTE      E DS
006100030414     D AZUTEDS       E DS                  extname(AZUTE00F)
006200030423     D*---
006300030423     D* DS REPERIMENTO £ire
006400030423     D*---
006500030423     D TRUL06DS      E DS
006600030423     D  L1                     1     90  0 DIM(30)
006700030521     D*------------------
006800030521     D* DS "XSRDA8" - CONTROLLA DATA (8)
006900030521     D*------------------
007000030521     D WLBDA8          DS                  INZ
007100030521     D  G08DAT                 1      8  0
007200030521     D  G08INV                 9     16  0
007300030521     D  G08ERR                17     17
007400030521     D  G08TGI                18     22  0
007500130205     D*------------------
007600130205     D FNLSD9DS        DS
007700130205     D  ilsd9dsd               1      8  0
007800130205     D  ilsd9dsa               9     16  0
007900130205     d* S=stampa F=file E=entrambi
008000130205     D  ilsd9RIC              17     17
008100130205     D  ilsd9TFP              18     20  0
008200001218
008300130206     d wFNBLP          s             21    inz('FILTRA201 /FNBLP05L')
008400170529     d wFNBLT          s             21    inz('FILTRA201 /FNBLT01L')
008500170613     d wFNBRV          s             21    inz('FILTRA201 /FNBRV07L')
008600130206     d wSAT            s             21    inz('GAITRAAZM /WFSAT00F')
008700920812     C*---------------------------------------------------------------*
008800001218     C* MAIN
008900001218     C*---------------------------------------------------------------*
009000130205     c
009100130205     c* Elaboro terminal per terminal alla data  AL oppure il terminal
009200130205     c*  richiesto se solo uno
009300130205     C                   exsr      CAR_LNP
009400130205     c
009500001218     C*
009600001218     C                   seton                                        LR
009700001218
009800001218     C*------------------------------------------------------------------------*
009900001218     C* PROCEDI - Routine principale
010000001218     C*------------------------------------------------------------------------*
010100020613     C     PROCEDI       BEGSR
010200130205     c
010300030423     C* Inizializzo le schiere di memorizzazione
010400030423     C                   clear                   skMemDTP
010500030423     C                   clear                   skMemNTP
010501171120     C                   clear                   skMemSTP
010600030423     C                   z-add     1             i
010700030423     C                   dow       i <= %elem(skMemDatPO)
010800030423     C     i             occur     skMemDatPO
010900030423     C                   clear                   skMemDPO
011000030423     C                   clear                   skMemNPO
011100030423     C                   eval      i = i + 1
011200030423     C                   enddo
011300001218     C*
011400130205     C* Mi posiziono sul FNBLP05L per ciascun P.O. in gestione al T.P.
011500030422     C                   z-add     1             i
011600030422     C                   dow       i <= %elem(skPO)
011700030422     C                   if        skPO(i) > *zeros
011800030422     C                   eval      blpLNP = skPO(i)
011900130205     c
012000130205     C     KEYblp05P     setll     fnblp05l
012100130205     C     KEYblp05P     reade     fnblp05l
012200130205     c
012300101126     C                   dow       not %eof(fnblp05l)
012400030512     C*
012500030512     C* Inizializzo il flag di "check" bolla ok x elaborazione
012600130205     C                   movel     'S'           recOK             1
012700030512     C*
012800030512     C* Eseguo routine di "check" bolla ok x elaborazione
012900030512     C                   exsr      chkrec
013000030512     C*
013100030512     C* Se record da elaborare...
013200030512     C                   if        recOK = 'S'
013300030512     C* Eseguo routine di memorizzazione dati
013400030512     C                   exsr      memdat
013500030512     C                   endif
013600130205     c
013700101126     C     KEYblp05P     reade     fnblp05l
013800030422     C                   enddo
013900030422     C                   endif
014000130205     c
014100030422     C                   eval      i = i +1
014200030422     C                   enddo
014300020613     C*
014400130206     C                   exsr      stampa
014500130206     C*
014600001218     C                   ENDSR
014700001218     C*------------------------------------------------------------------------*
014800030512
014900030512
015000030512     C*------------------------------------------------------------------------*
015100030512     C* CHKREC - Routine di verifica record da trattare
015200030512     C*------------------------------------------------------------------------*
015300030512     C     CHKREC        BEGSR
015400030521     C*
015500040430     C* Verifico che la data della bolla sia inclusa nel dal al richiesto   o
015600130205     C                   if        (blpAAS*10000)+blpMGS <= ilsd9dsa and
015700130205     c                             (blpaas*10000)+blpmgs >= ilsd9dsd
015800030521     C                   else
015900030521     C                   movel     'N'           recOK
016000030521     C                   endif
016100030512     C*
016200030512     C* Verifico se il tipo bolla è di quelli da elaborare
016300030512     C     blpCBO        lookup    sk3A                                   55
016400030512     C                   if        *in55 = *off
016500030512     C                   movel     'N'           recOK
016600030512     C                   endif
016700061108     C* Verifico se il cod.trattamento merce non è da elaborare
016800061108     C     blpCTM        lookup    sk1B                                   55
016900061108     C                   if        *in55 = *on
017000061108     C                   movel     'N'           recOK
017100061108     C                   endif
017200030512     C*
017300030512     C                   ENDSR
017400030512     C*---------------------------------------------------------------*
017500010626
017600010626
017700010626
017800010626
017900010509     C*------------------------------------------------------------------------*
018000030422     C* MEMDAT - Routine di memorizzazione dati
018100010509     C*------------------------------------------------------------------------*
018200030422     C     MEMDAT        BEGSR
018300010509     C*
018400030422     C* Memorizzo i dati x T.A. e P.O.
018500130702     c* se il terminal di arrivo è in sardegna --> attribuisco a 099 pisa
018600130702     c*  i MC a terra   tranne  se blptfp = 099 o tfp della sardegna
018700130702     c                   if        ilsd9ric<>'S' and blptfp<>099
018800130702     c     blptfp        chain     azorg01l
018900130702     c                   if        %found(azorg01l) and orgcar<>117
019000130702     c     blptfa        chain     azorg01l
019100130702     c                   if        %found(azorg01l) and orgcar=117
019200130702     c                   eval      blptfa=099
019300130702     c                   endif
019400130702     c                   endif
019500130702     c                   endif
019600130702     c
019700030422     C                   eval      skTA(blpTFA) = blpTFA
019800030423     C     skTA(blpTFA)  occur     skMemDatPO
019900170608     C
020000170529     c                   clear                   wvol
020100170529     c                   clear                   Wvolu
020200170529     c* Assumo come volume spedizione:
020300170529     c* il VDL se totale o parziale ma maggiore del vlf
020400170529     c* il vlf negli altri casi
020500170529     c                   if        BLPncr = BLPncl
020600170529     c                   z-add     BLPvlc        Wvolu
020700170529   x1c                   else
020800170529if  2c                   if        BLPvlc >= BLPvlf
020900170529     c                   z-add     BLPvlc        Wvolu
021000170529   x2c                   else
021100170529     c                   z-add     BLPvlf        Wvolu
021200170529e   2c                   endif
021300170529e   1c                   endif
021400170529      *
021500170529     c* spedizione non spareggiata
021600170529     c                   if        blpdpc=0
021700170529     c                   z-add     wvolu         wvol
021800170620     c                   endif
021900170620     c* Sepdizione spareggiata o Import DPD
022000170620    1c                   if        blpdpc>0 or skntw(i)='DPD'
022100170620     c                   clear                   wspu
022200170529     c* spedizione     spareggiata:
022300170529      *   Calcolo il volume medio
022400170620    2c                   if        blpdpc>0
022500170529     c     Wvolu         div(H)    BLPncl        Wvolmed
022600170620    2c                   endif
022700170529     c                   clear                   wncl
022800170529     c*   leggo dettaglio colli per contare il numero dei colli a terra
022900170620     c*   e per import dpd per verificare se collo spuntato
023000170529     c     kblt          setll     fnblt01l
023100170529     c     kblt          reade     fnblt01l
023200170620    2c                   dow       not %eof(fnblt01l)
023300170620    3c                   if        bltdfv=0
023400170529     c                   add       1             wncl
023500170613     c* se bolla import dpd verifico se collo realmente spuntato
023600170620    4c                   if        skntw(i)='DPD'
023700170620    5c                   if        bltdse>0
023800170620     c                   eval      wspu='S'
023900170620     c                   else
024000170613     c     kbrv7         setll     fnbrv07l
024100170620    6c                   if        %equal(fnbrv07l)
024200170620     c                   eval      wspu='S'
024300170620    6c                   endif
024400170620    5c                   endif
024500170620    4c                   endif
024600170620    3c                   ENDIF
024700170529     c     kblt          reade     fnblt01l
024800170620    2c                   ENDDO
024900170620     c* Se spareggiata calcolo il volume a terra
025000170620     c                   if        blpdpc>0
025100170615     c                   eval(h)   wvol=wvolmed*wncl
025200170620     c                   endif
025300170620    1c                   ENDIF
025400030423     C* Evidenzio il volume merce "da partire" scindendo i servizi NON distribuz
025500170613     c* Per Dpd il totale sarà il totale spuntato
025600170613     c*       e il "Di cui distribuzione" sarà il totale bollettato
025700170620     c                   if        skntw(i)<>'DPD' or wspu='S'
025800170613     C                   eval      skMemDPO(i) = skMemDPO(i) + wvol
025900170529     C                   eval      skMemDTP(blpTFA) = skMemDTP(blpTFA) + wvol
026000170613     C                   endif
026100170613     C                   if        blpTSP = 'D' or skntw(i)='DPD'
026200170613     C                   eval      skMemNPO(i) = skMemNPO(i) + wvol
026300170615     c* Nel totale di cui "D" per TFP non metto il bollettato di DPD
026400170615     c                   if        skntw(i) <>'DPD'
026500170529     C                   eval      skMemNTP(blpTFA) = skMemNTP(blpTFA) + wvol
026600170615     c                   endif
026700030423     C                   endif
026701171121     c                   if        blpft1=' '
026702171121     c                             and (skntw(i)<>'DPD' or wspu='S')
026703171121     c                   eval      skMemStp(blptfa) = skMemStp(blptfa) + wvol
026704171120     c                   endif
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
028500130205     c
028600130205     c                   if        ilsd9RIC<>'F'
028700130206     c                   clear                   stpD01
028800130206     c                   clear                   stpN01
028900130206     c                   clear                   stpgD01
029000130206     c                   clear                   stpgN01
029100130206     c                   clear                   stpD02
029200130206     c                   clear                   stpN02
029300130206     c                   clear                   stpgD02
029400130206     c                   clear                   stpgN02
029500130206     c                   clear                   stpD03
029600130206     c                   clear                   stpN03
029700130206     c                   clear                   stpgD03
029800130206     c                   clear                   stpgN03
029900130206     c                   clear                   stpD04
030000130206     c                   clear                   stpN04
030100130206     c                   clear                   stpgD04
030200130206     c                   clear                   stpgN04
030300130206     c                   clear                   stpD05
030400130206     c                   clear                   stpN05
030500130206     c                   clear                   stpgD05
030600130206     c                   clear                   stpgN05
030700130206     c                   clear                   stpD06
030800130206     c                   clear                   stpN06
030900130206     c                   clear                   stpgD06
031000130206     c                   clear                   stpgN06
031100130206     c                   clear                   stpD07
031200130206     c                   clear                   stpN07
031300130206     c                   clear                   stpgD07
031400130206     c                   clear                   stpgN07
031500130206     c                   clear                   stpD08
031600130206     c                   clear                   stpN08
031700130206     c                   clear                   stpgD08
031800130206     c                   clear                   stpgN08
031900130206     c                   clear                   stpD09
032000130206     c                   clear                   stpN09
032100130206     c                   clear                   stpgD09
032200130206     c                   clear                   stpgN09
032300130206     c                   clear                   stpD10
032400130206     c                   clear                   stpN10
032500130206     c                   clear                   stpgD10
032600130206     c                   clear                   stpgN10
032700130206     c                   clear                   stpD11
032800130206     c                   clear                   stpN11
032900130206     c                   clear                   stpgD11
033000130206     c                   clear                   stpgN11
033100130206     c                   clear                   stpD12
033200130206     c                   clear                   stpN12
033300130206     c                   clear                   stpgD12
033400130206     c                   clear                   stpgN12
033500130206     c                   clear                   stpD13
033600130206     c                   clear                   stpN13
033700130206     c                   clear                   stpgD13
033800130206     c                   clear                   stpgN13
033900130206     c                   clear                   stpD14
034000130206     c                   clear                   stpN14
034100130206     c                   clear                   stpgD14
034200130206     c                   clear                   stpgN14
034300130206     c                   clear                   stpD15
034400130206     c                   clear                   stpN15
034500130206     c                   clear                   stpgD15
034600130206     c                   clear                   stpgN15
034700130206     c                   clear                   depdtp
034800130206     c                   clear                   depntp
034900130206     c                   clear                   stpdtp
035000130206     c                   clear                   stpntp
035100130206     c                   clear                   stpgdtp
035200130206     c                   clear                   stpgNtp
035300030423     C*
035400030423     C* STAMPO TESTATA
035500030423     C                   eval      stpSYF = RSUT
035600170613     c                   write     LSD1PT
035700090625     C  n20              write     LSD1PI
035800090625     C   20              write     LSD1PI15
035900090625     c                   setoff                                       01
036000130205     c                   endif
036100030422     C*
036200030422     C* STAMPO DETTAGLIO X TERMINAL ARRIVO (solo se c'è qualcosa effettivamente
036300030422     C                   z-add     1             i
036400030422     C                   dow       i <= %elem(skTA)
036500030422     C*
036600030422     C* SOLO X I T.A. VALIDI
036700030422     C                   if        skTA(i) > *zeros
036800030423     C     skTA(i)       occur     skMemDatPO
036900030422     C*
037000030423     C* SOLO SE C'E' QUALCOSA DA STAMPARE
037100170718     C                   eval(h)   depDTP = skMemDTP(skTA(i))
037200170718     C                   eval(h)   depNTP = skMemNTP(skTA(i))
037300030423     C                   if        depDTP > *zeros OR
037400030423     C                             depNTP > *zeros
037500030422     C*
037600030422     C* STAMPO I TOTALI T.P. X T.A.
037700130320     c                   if        ilsd9RIC<>'F'
037800030507     C                   clear                   LSD1P2
037900030422     C                   eval      stpTA  = skTA(i)
038000030428     C                   eval(h)   stpDTP = skMemDTP(skTA(i))
038100030428     C                   eval(h)   stpNTP = skMemNTP(skTA(i))
038200041111     c                   eval      stpgDTP = stpgDTP + stpDTP
038300041111     c                   eval      stpgNTP = stpgNTP + stpNTP
038400130320     c                   endif
038500130205     c
038600130205     c                   if        ilsd9ric<>'S'
038700130205     c                   clear                   wfsat000
038800130205     c                   eval      sattfp=ktfp
038900130205     c                   eval      sattfa=skTa(i)
039000130205     c                   eval      satdre=ilsd9dsa
039100130205     c                   eval      sattre=' '
039200130205     c                   eval      satvpt=skMemDTP(skTA(i))
039300130320     c
039400171120     c**                 if        skMemNTP(skTA(i)) < 99999,9
039500171120     c**                 eval      satvpd=skMemNTP(skTA(i))
039600171120     c**                 else
039700171120     c**                 eval      satvpd=99999,9
039800171120     c**                 endif
039801171121     c                   if        skMemSTP(skTA(i)) < 99999,9
039802171120     c                   eval      satvpd=skMemSTP(skTA(i))
039803171120     c                   else
039804171121     c                   eval      satvpd=99999,9
039805171120     c                   endif
039900130320     c
040000130205     c                   write     wfsat000
040100130205     C                   endif
040200130205     c
040300130205     c* Se richiesta anche stampa
040400130205     c                   if        ilsd9RIC<>'F'
040500130205     c
040600030422     C                   z-add     1             j
040700030422     C                   dow       j <= %elem(skPO)
040800030422     C*
040900030422     C* SOLO X I P.O. VALIDI
041000030422     C                   if        skPO(j) > *zeros
041100030422     C                   if        skPOCOL(skPO(j))  > *zeros AND
041200030422     C                             skPOCOL(skPO(j)) <> 999
041300030422     C* REPERISCO LA COLONNA A CUI STAMPARE IL DATO CORRENTE
041400030422     C*
041500030422     C** Colonna 1
041600030422     C                   if        skPOCOL(skPO(j)) = 1
041700030428     C                   eval(h)   stpD01  = skMemDPO(skPO(j))
041800030428     C                   eval(h)   stpN01  = skMemNPO(skPO(j))
041900041111     c                   eval      stpgD01 = stpgD01 + stpD01
042000041111     c                   eval      stpgN01 = stpgN01 + stpN01
042100130205     c* Se richeisto scrivo anche il file
042200130205     C                   endif
042300030422     C** Colonna 2
042400030422     C                   if        skPOCOL(skPO(j)) = 2
042500030428     C                   eval(h)   stpD02  = skMemDPO(skPO(j))
042600030428     C                   eval(h)   stpN02  = skMemNPO(skPO(j))
042700041111     c                   eval      stpgD02 = stpgD02 + stpD02
042800041111     c                   eval      stpgN02 = stpgN02 + stpN02
042900030422     C                   endif
043000030422     C** Colonna 3
043100030422     C                   if        skPOCOL(skPO(j)) = 3
043200030428     C                   eval(h)   stpD03  = skMemDPO(skPO(j))
043300030428     C                   eval(h)   stpN03  = skMemNPO(skPO(j))
043400041111     c                   eval      stpgD03 = stpgD03 + stpD03
043500041111     c                   eval      stpgN03 = stpgN03 + stpN03
043600030422     C                   endif
043700030422     C** Colonna 4
043800030422     C                   if        skPOCOL(skPO(j)) = 4
043900030428     C                   eval(h)   stpD04  = skMemDPO(skPO(j))
044000030428     C                   eval(h)   stpN04  = skMemNPO(skPO(j))
044100041111     c                   eval      stpgD04 = stpgD04 + stpD04
044200041111     c                   eval      stpgN04 = stpgN04 + stpN04
044300030422     C                   endif
044400030422     C** Colonna 5
044500030422     C                   if        skPOCOL(skPO(j)) = 5
044600030428     C                   eval(h)   stpD05  = skMemDPO(skPO(j))
044700030428     C                   eval(h)   stpN05  = skMemNPO(skPO(j))
044800041111     c                   eval      stpgD05 = stpgD05 + stpD05
044900041111     c                   eval      stpgN05 = stpgN05 + stpN05
045000030422     C                   endif
045100030422     C** Colonna 6
045200030422     C                   if        skPOCOL(skPO(j)) = 6
045300030428     C                   eval(h)   stpD06  = skMemDPO(skPO(j))
045400030428     C                   eval(h)   stpN06  = skMemNPO(skPO(j))
045500041111     c                   eval      stpgD06 = stpgD06 + stpD06
045600041111     c                   eval      stpgN06 = stpgN06 + stpN06
045700030422     C                   endif
045800030422     C** Colonna 7
045900030422     C                   if        skPOCOL(skPO(j)) = 7
046000030428     C                   eval(h)   stpD07  = skMemDPO(skPO(j))
046100030428     C                   eval(h)   stpN07  = skMemNPO(skPO(j))
046200041111     c                   eval      stpgD07 = stpgD07 + stpD07
046300041111     c                   eval      stpgN07 = stpgN07 + stpN07
046400030422     C                   endif
046500030422     C** Colonna 8
046600030422     C                   if        skPOCOL(skPO(j)) = 8
046700030428     C                   eval(h)   stpD08  = skMemDPO(skPO(j))
046800030428     C                   eval(h)   stpN08  = skMemNPO(skPO(j))
046900041111     c                   eval      stpgD08 = stpgD08 + stpD08
047000041111     c                   eval      stpgN08 = stpgN08 + stpN08
047100030422     C                   endif
047200030422     C** Colonna 9
047300030422     C                   if        skPOCOL(skPO(j)) = 9
047400030428     C                   eval(h)   stpD09  = skMemDPO(skPO(j))
047500030428     C                   eval(h)   stpN09  = skMemNPO(skPO(j))
047600041111     c                   eval      stpgD09 = stpgD09 + stpD09
047700041111     c                   eval      stpgN09 = stpgN09 + stpN09
047800030422     C                   endif
047900090625     C** Colonna 10
048000090625     C                   if        skPOCOL(skPO(j)) = 10
048100090625     C                   eval(h)   stpD10  = skMemDPO(skPO(j))
048200090625     C                   eval(h)   stpN10  = skMemNPO(skPO(j))
048300090625     c                   eval      stpgD10 = stpgD10 + stpD10
048400090625     c                   eval      stpgN10 = stpgN10 + stpN10
048500090625     C                   endif
048600090625     C** Colonna 11
048700090625     C                   if        skPOCOL(skPO(j)) = 11
048800090625     C                   eval(h)   stpD11  = skMemDPO(skPO(j))
048900090625     C                   eval(h)   stpN11  = skMemNPO(skPO(j))
049000090625     c                   eval      stpgD11 = stpgD11 + stpD11
049100090625     c                   eval      stpgN11 = stpgN11 + stpN11
049200090625     C                   endif
049300090625     C** Colonna 12
049400090625     C                   if        skPOCOL(skPO(j)) = 12
049500090625     C                   eval(h)   stpD12  = skMemDPO(skPO(j))
049600090625     C                   eval(h)   stpN12  = skMemNPO(skPO(j))
049700090625     c                   eval      stpgD12 = stpgD12 + stpD12
049800090625     c                   eval      stpgN12 = stpgN12 + stpN12
049900090625     C                   endif
050000090625     C** Colonna 13
050100090625     C                   if        skPOCOL(skPO(j)) = 13
050200090625     C                   eval(h)   stpD13  = skMemDPO(skPO(j))
050300090625     C                   eval(h)   stpN13  = skMemNPO(skPO(j))
050400090625     c                   eval      stpgD13 = stpgD13 + stpD13
050500090625     c                   eval      stpgN13 = stpgN13 + stpN13
050600090625     C                   endif
050700090625     C** Colonna 14
050800090625     C                   if        skPOCOL(skPO(j)) = 14
050900090625     C                   eval(h)   stpD14  = skMemDPO(skPO(j))
051000090625     C                   eval(h)   stpN14  = skMemNPO(skPO(j))
051100090625     c                   eval      stpgD14 = stpgD14 + stpD14
051200090625     c                   eval      stpgN14 = stpgN14 + stpN14
051300090625     C                   endif
051400090625     C** Colonna 15
051500090625     C                   if        skPOCOL(skPO(j)) = 15
051600090625     C                   eval(h)   stpD15  = skMemDPO(skPO(j))
051700090625     C                   eval(h)   stpN15  = skMemNPO(skPO(j))
051800090625     c                   eval      stpgD15 = stpgD15 + stpD15
051900090625     c                   eval      stpgN15 = stpgN15 + stpN15
052000090625     C                   endif
052100090625      * se ho più di 15 colonne devo sommare tutti i p.o. rimanenti
052200041110      * sulla nona colonna
052300090625     c                   if        skPOCOL(skPO(j)) > 15
052400090625     C                   eval(h)   stpD15  = stpD15 + skMemDPO(skPO(j))
052500090625     C                   eval(h)   stpN15  = stpN15 + skMemNPO(skPO(j))
052600090625     c                   eval      stpgD15 = stpgD15 + stpD15
052700090625     c                   eval      stpgN15 = stpgN15 + stpN15
052800041110     C                   endif
052900030422     C*
053000030422     C                   endif
053100030422     C                   endif
053200030422     C*
053300030422     C* SCORRO L'ELENCO P.O. X T.A. CORRENTE
053400030422     C                   eval      j = j + 1
053500030422     C                   enddo
053600030422     C*
053700090625     c
053800090625     c* testata
053900090625     c                   if        *in01
054000090625     C                   write     LSD1PT
054100090625     C  n20              write     LSD1PI
054200090625     C   20              write     LSD1PI15
054300090625     c                   setoff                                       01
054400090625     c                   endif
054500090625     c
054600090625     C* STAMPO LA RIGA DI DETTAGLIO
054700090625     C  n20              write     LSD1P2
054800090625     C   20              write     LSD1P215
054900030422     C                   endif
055000130205     c
055100130205     C                   endif
055200030422     C                   endif
055300030422     C*
055400030422     C* SCORRO L'ELENCO DEI T.A.
055500030422     C                   eval      i = i + 1
055600030422     C                   enddo
055700041111     C*
055800130205     c                   if        ilsd9RIC<>'F'
055900041111     C* STAMPO TOTALI
056000090625     c                   write     LSD1RI
056100090625     C  n20              write     LSD1TG
056200090625     C   20              write     LSD1TG15
056300030422     C*
056400030422     C* STAMPO CHIUSURA PROSPETTO
056500030507     C                   write     LSD1RI
056600030507     C                   write     LSD1FI
056700130205     c                   endif
056800030414     C*
056900030414     C                   ENDSR
057000030414     C*------------------------------------------------------------------------*
057100030422
057200030422
057300030422
057400030422
057500030422     C*------------------------------------------------------------------------*
057600030422     C* ABBPOCOL -  Routine di abbinamento P.O. a colonne in stampa
057700030422     C*------------------------------------------------------------------------*
057800030422     C     ABBPOCOL      BEGSR
057900030423     C*
058000030423     C                   setoff                                       111213
058100030423     C                   setoff                                       141516
058200030423     C                   setoff                                       171819
058300090625     C                   setoff                                       20
058400170615     C                   setoff                                       212223
058500170615     C                   setoff                                       2425
058600030422     C*
058700030422     C* Innanzitutto ordino in modo crescente l'elenco dei P.O.
058800030422     C                   sorta     skPO
058900030423     C*
059000030423     C* Di default abbino alla 1° colonna il P.O. terminal di partenza e alle su
059100030423     C* gli altri P.O. in ordine crescente
059200170613     c* mettendo prima le filiali dpd
059300030423     C                   clear                   skPOCOL
059400030423     C                   z-add     1             currCOL
059500130205     C                   eval      skPOCOL(ktfp) = currCOL
059600030423     C                   z-add     1             i
059700170614     C                   z-add     1             currColD
059800170614     c                   eval      currCOL=currCOL+nDpd
059900030423     C                   dow       i <= %elem(skPO)
060000030423     C                   if        skPO(i)  > *zeros AND
060100130205     C                             skPO(i) <> ktfp
060200170614     c* se non è dpd il numero è currCOL
060300170614     c                   if        skntw(skpo(i))<>'DPD'
060400030423     C                   eval      currCOL = currCOL + 1
060500030423     C                   eval      skPOCOL(skPO(i)) = currCOL
060600170614     c                   else
060700170614     c* se     è dpd il numero è currCOLD
060800170614     C                   eval      currCOLD = currCOLD + 1
060900170614     C                   eval      skPOCOL(skPO(i)) = currCOLD
061000030423     C                   endif
061100170614     C                   endif
061200030423     C                   eval      i = i + 1
061300030423     C                   enddo
061400030422     C*
061500030423     C* Eseguo verifica valorizzazioni x T.A.
061600030423     C                   clear                   skPOCOLok
061700030422     C*
061800030422     C                   z-add     1             i
061900030422     C                   dow       i <= %elem(skTA)
062000030422     C                   if        skTA(i) > *zeros
062100030423     C     skTA(i)       occur     skMemDatPO
062200030422     C                   z-add     1             j
062300030422     C                   dow       j <= %elem(skPO)
062400030422     C*
062500030422     C* Solo x i P.O. validi
062600030422     C                   if        skPO(j) > *zeros
062700030422     C*
062800030423     C* Solo se ci sono dati da stampare
062900170718     C                   eval(h)   depDTP = skMemDPO(skPO(j))
063000170718     C                   eval(h)   depNTP = skMemNPO(skPO(j))
063100030423     C                   if        depDTP > *zeros OR
063200030423     C                             depNTP > *zeros
063300030423     C                   if        skPOCOLok(skPO(j)) = *zeros
063400030422     C*
063500030423     C* Flaggo a OK la colonna relativa al P.O. da stampare
063600030423     C                   eval      skPOCOLok(skPO(j)) = 999
063700030423     C                   endif
063800030423     C                   endif
063900030423     C                   endif
064000030423     C                   eval      j = j + 1
064100030423     C                   enddo
064200030423     C                   endif
064300030423     C                   eval      i = i + 1
064400030423     C                   enddo
064500030423     C*
064600030423     C* A questo punto "mergio" le collonne di default con quelle effettivamente
064700030423     C                   z-add     1             i
064800030423     C                   dow       i <= %elem(skPO)
064900030423     C                   if        skPO(i) > *zeros
065000030423     C                   if        skPOCOLok(skPO(i)) = *zeros
065100030423     C                   eval      depCOL = skPOCOL(skPO(i))
065200030423     C* Diminuisco di 1 le posizioni >= alla colonna relativa al P.O. da nn stam
065300030423     C                   z-add     1             j
065400030423     C                   dow       j <= %elem(skPOCOL)
065500030423     C                   if        skPOCOL(j) = depCOL
065600030423     C                   eval      skPOCOL(j) = *zeros
065700030423     C                   endif
065800030423     C                   if        skPOCOL(j) > depCOL
065900030423     C                   eval      skPOCOL(j) = skPOCOL(j) - 1
066000030423     C                   endif
066100030423     C                   eval      j = j + 1
066200030423     C                   enddo
066300030423     C                   endif
066400030423     C                   endif
066500030423     C                   eval      i = i + 1
066600030423     C                   enddo
066700130206     c                   clear                   stpP01
066800130206     c                   clear                   stpP01D
066900130206     c                   clear                   stpP02
067000130206     c                   clear                   stpP02D
067100130206     c                   clear                   stpP03
067200130206     c                   clear                   stpP03D
067300130206     c                   clear                   stpP04
067400130206     c                   clear                   stpP04D
067500130206     c                   clear                   stpP05
067600130206     c                   clear                   stpP05D
067700130206     c                   clear                   stpP06
067800130206     c                   clear                   stpP06D
067900130206     c                   clear                   stpP07
068000130206     c                   clear                   stpP07D
068100130206     c                   clear                   stpP08
068200130206     c                   clear                   stpP08D
068300130206     c                   clear                   stpP09
068400130206     c                   clear                   stpP09D
068500130206     c                   clear                   stpP10
068600130206     c                   clear                   stpP10D
068700130206     c                   clear                   stpP11
068800130206     c                   clear                   stpP11D
068900130206     c                   clear                   stpP12
069000130206     c                   clear                   stpP12D
069100130206     c                   clear                   stpP13
069200130206     c                   clear                   stpP13D
069300130206     c                   clear                   stpP14
069400130206     c                   clear                   stpP14D
069500130206     c                   clear                   stpP15
069600130206     c                   clear                   stpP15D
069700030423     C*
069800030423     C* Effettuo 1 ultimo giro x valorizzare le intestazioni delle colonne da st
069900030423     C                   z-add     1             j
070000030423     C                   dow       j <= %elem(skPO)
070100030423     C                   if        skPO(j) > *zeros
070200030423     C                   eval      currCOL = skPOCOL(skPO(j))
070300030423     C*
070400030423     C                   if        currCOL = 1
070500170614     c                   if        skntw(skPO(j))='DPD'
070600030423     C                   seton                                        11
070700170613     c                   endif
070800030423     C                   eval      stpP01  = skPO(j)
070900030423     C                   eval      stpP01D = skPOD(skPO(j))
071000030423     C                   endif
071100030423     C                   if        currCOL = 2
071200170614     c                   if        skntw(skpo(j))='DPD'
071300030423     C                   seton                                        12
071400170613     c                   endif
071500030423     C                   eval      stpP02  = skPO(j)
071600030423     C                   eval      stpP02D = skPOD(skPO(j))
071700030423     C                   endif
071800030423     C                   if        currCOL = 3
071900170614     c                   if        skntw(skpo(j))='DPD'
072000030423     C                   seton                                        13
072100170613     c                   endif
072200030423     C                   eval      stpP03  = skPO(j)
072300030423     C                   eval      stpP03D = skPOD(skPO(j))
072400030423     C                   endif
072500030423     C                   if        currCOL = 4
072600170614     c                   if        skntw(skpo(j))='DPD'
072700030423     C                   seton                                        14
072800170613     c                   endif
072900030423     C                   eval      stpP04  = skPO(j)
073000030423     C                   eval      stpP04D = skPOD(skPO(j))
073100030423     C                   endif
073200030423     C                   if        currCOL = 5
073300170614     c                   if        skntw(skpo(j))='DPD'
073400030423     C                   seton                                        15
073500170613     c                   endif
073600030423     C                   eval      stpP05  = skPO(j)
073700030423     C                   eval      stpP05D = skPOD(skPO(j))
073800030423     C                   endif
073900030423     C                   if        currCOL = 6
074000170614     c                   if        skntw(skpo(j))='DPD'
074100030423     C                   seton                                        16
074200170613     c                   endif
074300030423     C                   eval      stpP06  = skPO(j)
074400030423     C                   eval      stpP06D = skPOD(skPO(j))
074500030423     C                   endif
074600090625     C                   if        currCOL = 7
074700170614     c                   if        skntw(skpo(j))='DPD'
074800030423     C                   seton                                        17
074900170613     c                   endif
075000030423     C                   eval      stpP07  = skPO(j)
075100030423     C                   eval      stpP07D = skPOD(skPO(j))
075200030423     C                   endif
075300030423     C                   if        currCOL = 8
075400170614     c                   if        skntw(skpo(j))='DPD'
075500030423     C                   seton                                        18
075600170613     c                   endif
075700030423     C                   eval      stpP08  = skPO(j)
075800030423     C                   eval      stpP08D = skPOD(skPO(j))
075900030423     C                   endif
076000030423     C                   if        currCOL = 9
076100170614     c                   if        skntw(skpo(j))='DPD'
076200030423     C                   seton                                        19
076300170613     c                   endif
076400030423     C                   eval      stpP09  = skPO(j)
076500030423     C                   eval      stpP09D = skPOD(skPO(j))
076600030423     C                   endif
076700090625     C                   if        currCOL = 10
076800090625     C                   seton                                        20
076900170615     c                   if        skntw(skpo(j))='DPD'
077000170615     C                   seton                                        21
077100170615     c                   endif
077200090625     C                   eval      stpP10  = skPO(j)
077300090625     C                   eval      stpP10D = skPOD(skPO(j))
077400090625     C                   endif
077500090625     C                   if        currCOL = 11
077600090625     C                   seton                                        20
077700170615     c                   if        skntw(skpo(j))='DPD'
077800170615     C                   seton                                        22
077900170615     c                   endif
078000090625     C                   eval      stpP11  = skPO(j)
078100090625     C                   eval      stpP11D = skPOD(skPO(j))
078200090625     C                   endif
078300090625     C                   if        currCOL = 12
078400090625     C                   seton                                        20
078500170615     c                   if        skntw(skpo(j))='DPD'
078600170615     C                   seton                                        23
078700170615     c                   endif
078800090625     C                   eval      stpP12  = skPO(j)
078900090625     C                   eval      stpP12D = skPOD(skPO(j))
079000090625     C                   endif
079100090625     C                   if        currCOL = 13
079200090625     C                   seton                                        20
079300170615     c                   if        skntw(skpo(j))='DPD'
079400170615     C                   seton                                        24
079500170615     c                   endif
079600090625     C                   eval      stpP13  = skPO(j)
079700090625     C                   eval      stpP13D = skPOD(skPO(j))
079800090625     C                   endif
079900090625     C                   if        currCOL = 14
080000090625     C                   seton                                        20
080100170615     c                   if        skntw(skpo(j))='DPD'
080200170615     C                   seton                                        25
080300170615     c                   endif
080400090625     C                   eval      stpP14  = skPO(j)
080500090625     C                   eval      stpP14D = skPOD(skPO(j))
080600090625     C                   endif
080700090625     C                   if        currCOL = 15
080800090625     C                   seton                                        20
080900090625     C                   eval      stpP15  = skPO(j)
081000090625     C                   eval      stpP15D = skPOD(skPO(j))
081100090625     C                   endif
081200090625      * se ho più di 15 p.o. devo intestare la colonna con 'ALTRI'
081300090625     c                   If        currCOL > 15
081400090625     C                   clear                   stpP15
081500090625     C                   eval      stpP15D = 'ALTRI'
081600041110     C                   endif
081700030422     C*
081800030423     C                   endif
081900030423     C                   eval      j = j + 1
082000030423     C                   enddo
082100030423     C*
082200030422     C                   ENDSR
082300030422     C*---------------------------------------------------------------*
082400030414
082500030414
082600030414
082700030414     C*---------------------------------------------------------------*
082800130205     C* CAR_LNP - Caricamento tabelle di wrk
082900030414     C*---------------------------------------------------------------*
083000130205     C     CAR_LNP       BEGSR
083100130205    1c                   if        ilsd9TFP>0
083200130205     c                   eval      ktfp=ilsd9tfp
083300130205     c                   exsr      ElaTFP
083400130205     c                   exsr      Procedi
083500130205     c
083600130205   x1c                   else
083700130205     c
083800130205     c                   eval      kepa='P'
083900130205     c     kepa          setll     azcae05l
084000130205     c     kepa          reade     azcae05l
084100130205    2c                   dow       not %eof(azcae05l)
084200130205     c                   if        caedde<=ilsd9dsa and caedsc>=ilsd9dsa
084300130205     c                             and caeatb=' '
084400130205     c
084500130205     c                   eval      ktfp=caetfe
084600130205     c                   exsr      ElaTFP
084700130205     c                   exsr      Procedi
084800130205     c
084900130205     c     kcae          setgt     azcae05l
085000130205     c                   endif
085100130205     c
085200130205     c     kepa          reade     azcae05l
085300130205    2c                   enddo
085400130205     c
085500130205    1c                   endif
085600130205     c
085700130205     c                   ENDSR
085800130205     C*---------------------------------------------------------------*
085900130205     C* ELATFP - Caricamento linee di partenza del terminal partenza
086000130205     C*---------------------------------------------------------------*
086100130205     C     ELATFP        BEGSR
086200030423     C*
086300030423     C* Carico i P.O. del T.P. dell'utente reperendoli dalla £1
086400130205     c                   clear                   trul06ds
086500030423     C                   MOVEL     '£1'          D06COD
086600130205     C                   MOVEL     kTFP          D06KEY
086700130205     c                   if        ilsd9TFP>0
086800030423     C                   MOVEL     'L'           D06TLA
086900130205     c                   endif
087000130205     C                   MOVE      'S'           D06KEY
087100130205     c                   eval      d06drf=ilsd9DSA
087200130205     c
087300030423     C                   MOVEL(P)  TRUL06DS      KPJBU
087400030423     C                   CALL      'TRUL06R'
087500030423     C                   PARM                    KPJBA
087600030423     C                   MOVEL     KPJBU         TRUL06DS
087700030414     C*
087800030423     C* Reperisco le rispettive denominazioni dei P.O.
087900030422     C                   clear                   skPO
088000030422     C                   clear                   skPOD
088100130206     C                   clear                   skta
088200130206     C                   clear                   skpocol
088300130206     C                   clear                   skpocolok
088400170614     c                   clear                   ndpd
088500030423     C     1             do        30            i
088600030423     C                   if        L1(i) > *zeros
088700170613     C                   eval      skPO(L1(i)) = L1(i)
088800170613     C***                eval      skPO(i) = L1(i)
088900030423     C** Reperisco le descrizioni di ogni P.O.
089000030423     C     L1(i)         chain     azorg01l
089100030423     C                   if        %found(azorg01l)
089200170613     C                   eval      skPOD(L1(i)) = orgDES
089300170613     C****               eval      skPOD(i) = orgDES
089400170613     C                   MOVEL     ORGDE3        OG143
089500170613     c                   eval      skntw(L1(i)) = §ogntw
089600170614     c* memorizzo il numero dei po dpd presenti
089700170614     c                   if        §ogntw='DPD'
089800170614     c                   eval      nDpd+=1
089900170614     c                   endif
090000030423     C                   else
090100170613     C                   eval      skPOD(L1(i)) = *all'*'
090200170613     C***                eval      skPOD(i) = *all'*'
090300170613     c                   eval      skntw(L1(i)) = *blanks
090400030423     C                   endif
090500030423     C                   endif
090600030512     C**
090700030512     C                   enddo
090800130205     c                   ENDSR
090900130205     C*---------------------------------------------------------------*
091000130205     C* CARTAB - Caricamento tabelle di wrk
091100130205     C*---------------------------------------------------------------*
091200130205     C     CARTAB        BEGSR
091300030512     C*
091400030512     C* Carico i tipi bolla dalla tabella 3A
091500030512     C                   z-add     *zeros        i
091600030512     C                   eval      tblCOD = '3A'
091700030512     C     KEYtabP       setll     tabel00f
091800030512     C     KEYtabP       reade     tabel00f
091900030512     C                   dow       not %eof(tabel00f)
092000030512     C                   movel(p)  tblUNI        DS3A
092100030512     C                   if        §3ARBL <> 'R'
092200030512     C                   add       1             i
092300030512     C                   eval      sk3A(i) = tblKEY
092400030512     C                   endif
092500030512     C     KEYtabP       reade     tabel00f
092600030512     C                   enddo
092700061108     C*
092800130205     C* Carico i tipi bolla dalla tabella 1B
092900061108     C                   z-add     *zeros        i
093000061108     C                   eval      tblCOD = '1B'
093100061108     C     KEYtabP       setll     tabel00f
093200061108     C     KEYtabP       reade     tabel00f
093300061108     C                   dow       not %eof(tabel00f)
093400061108     C                   movel(p)  tblUNI        DS1B
093500061108     C                   if        §1bfg8='N'
093600061108     C                   add       1             i
093700061108     C                   eval      sk1b(i) = tblKEY
093800061108     C                   endif
093900061108     C     KEYtabP       reade     tabel00f
094000061108     C                   enddo
094100030414     C*
094200030414     C                   ENDSR
094300030414     C*------------------------------------------------------------------------*
094400030414     C* REPERISCE I DATI UTENTE
094500030414     C*------------------------------------------------------------------------*
094600030414     C     REPDATIUTE    BEGSR
094700030414     C*
094800030414     C* INIZIALIZZA VARIABILI DI WRK
094900030414     C                   CLEAR                   TIBS34DS
095000030414     C                   CLEAR                   AZUTEDS
095100030414     C                   CLEAR                   DDATIUTE
095200030414     C*
095300030414     C     *DTAARA       DEFINE    §azute        azuteds
095400030414     C     *DTAARA       DEFINE    §datiute      ddatiute
095500030414     C                   IN(E)     *DTAARA
095600030422     C                   IF        %Error
095700030422     C                   EVAL      I34Tla = 'L'
095800030414     C                   CALL      'TIBS34R'
095900030414     C                   PARM                    Tibs34Ds
096000030414     C                   IN        *DTAARA
096100030422     C                   ENDIF
096200030414     C*
096300030414     C                   ENDSR
096400030414     C*------------------------------------------------------------------------*
096500010509
096600010509
096700001218
096800001218     C*------------------------------------------------------------------------*
096900001218     C* *INZSR - ROUTINE INIZIALE
097000001218     C*------------------------------------------------------------------------*
097100001218     C     *INZSR        BEGSR
097200001218     C*
097300030422     C* Ricevo in la DS di architettura
097400010726     C     *ENTRY        PLIST
097500020613     C                   PARM                    KPJBA
097600130205     c                   movel     kpjbu         fnlsd9ds
097700030521     C*
097800030521     C* Reperisco l'UDATE del job
097900030521     C                   TIME                    WN14             14 0
098000030521     C                   MOVE      WN14          WN8               8 0
098100030521     C                   Z-ADD     WN8           G08DAT
098200030521     C                   Z-ADD     *zeros        G08INV
098300030521     C                   MOVEL     '0'           G08ERR
098400030521     C                   CALL      'XSRDA8'
098500030521     C                   PARM                    WLBDA8
098600030521     C                   Z-ADD     G08INV        DATCOR            8 0
098700010726     C*
098800030422     C* Inizializzo campi chiave "fissi"
098900030414     C                   Z-ADD     1             tblKUT
099000030422     C                   Z-ADD     *zeros        blpDUC
099100130206     c* Giro le date per la stampa
099200130206     c                   clear                   v1cdsd
099300130206     c                   if        ilsd9dsd>0
099400130206     c     *iso          movel     ilsd9dsd      dataiso
099500130206     c                   movel     dataiso       dataeur
099600130206     c                   movel     dataeur       v1cdsd
099700130206     c                   endif
099800130206     c
099900130206     c                   clear                   v1cdsa
100000130206     c                   if        ilsd9dsa>0
100100130206     c     *iso          movel     ilsd9dsa      dataiso
100200130206     c                   movel     dataiso       dataeur
100300130206     c                   movel     dataeur       v1cdsa
100400130206     c                   endif
100500130205     c
100600030414     C****
100700030414     C* Definizione chiavi
100800030414     C****
100900030512     C* Chiave su TABEL00F - parziale
101000030512     C     KEYtabP       KLIST
101100030414     C                   KFLD                    tblKUT
101200030414     C                   KFLD                    tblCOD
101300101126     C* Chiave su FNBLP05L - parziale
101400101126     C     KEYblp05P     KLIST
101500030422     C                   KFLD                    blpLNP
101600030422     C                   KFLD                    blpDUC
101700130205     C     Kcae          KLIST
101800130205     C                   KFLD                    kepa
101900130205     C                   KFLD                    caetfe
102000170529     C     Kblt          KLIST
102100170529     C                   KFLD                    blpaas
102200170529     C                   KFLD                    blplnp
102300170529     C                   KFLD                    blpnrs
102400170529     C                   KFLD                    blpnsp
102500170613     C     Kbrv7         KLIST
102600170613     C                   KFLD                    bltfls
102700170613     C                   KFLD                    bltlna
102800170613     C                   KFLD                    bltnrs
102900170613     C                   KFLD                    bltnsc
103000170613     C                   KFLD                    ktfp
103100130205     c* Pulizia file di work se richiesto
103200130205      /FREE
103300130205       exec sql delete from WFSAT00F    ;
103400130205      /END-FREE
103500030414     C*
103600030414     C* REPERIMEMTO DATI UTENTE
103700030414     C                   EXSR      REPDATIUTE
103800030422     C*
103900030422     C* CARICAMENTO TABELLE DI WRK
104000030422     C                   EXSR      CARTAB
104100130206     c*
104200130206     c* Apertura file
104300130206     c                   if        %subst( knsif : 7 : 1) = 'P'
104400130206     c                   eval      %subst( wFNBLP : 7 : 3 ) = 'PRD'
104500170529     c                   eval      %subst( wFNBLT : 7 : 3 ) = 'PRD'
104600170613     c                   eval      %subst( wFNBRV : 7 : 3 ) = 'PRD'
104700130206     c                   eval      %subst( wSAT : 7 : 3 ) = 'AZP'
104800130206     c                   endif
104900130206     c
105000130206     c                   open      fnblp05l
105100170529     c                   open      fnblt01l
105200170613     c                   open      fnbrv07l
105300130206     c                   open      wfsat00f
105400001218     C*
105500001218     C                   ENDSR
