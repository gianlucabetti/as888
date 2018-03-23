000100981019     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(pjxbnd pjcbnd)
000200990222     H*PARMS ACTGRP(qile) CVTOPT(*DATETIME)
000300000121     H DECEDIT('0,') DATEDIT(*DMY.)
000400970528?    H*--------------------------------------------------------------*
000500970528?    H*  INDICATORI                                                  *
000600970528?    H*--------------------------------------------------------------*
000700970528      *
000800970528      * 19 = divisa = alla moneta di conto
000900970528      * 20 = dare
001000970528      * 21 = avere
001100970528      * 25 = stampa dati agente
001200970528      *
001300970528      * 99 = OVERFLOW
001400970528      *
001500970528?    F*--------------------------------------------------------------*
001600970528?    F*  DATA BASE                                                   *
001700970528?    F*--------------------------------------------------------------*
001800970528     FNDPIW01L  IF   E           K DISK
001900970528     FNDSCL01L  IF   E           K DISK
002000970528     FNDSCL01J  IF   E           K DISK
002100970528     FNDCST00F  IF   E           K DISK
002200970528     FNDCL55P2  O    E             PRINTER OFLIND(*IN99) UsrOpn
002300010124     F                                     INFDS(STP1)
002400971020     D*Numero di riga
002500010124     D STP1            DS
002600010124     D  STPLI1               367    368B 0
002700971020     D*
002800970528?    D*------------------------------------------------------------
002900970528?    D*  SCHIERE
003000970528?    D*------------------------------------------------------------
003100970528     D DIV             S              4    DIM(10)
003200970528     D VAL             S             19  3 DIM(10)
003300970528     D SCO             S             19  3 DIM(10)
003400970528     D INT             S             19  3 DIM(10)
003500970528     D SED             S              1    DIM(44)
003600970528?    D*------------------------------------------------------------
003700970528?    D*  STRUTTURE DATI
003800970528?    D*------------------------------------------------------------
003900970528?     *-- DS per Passaggio Parametri
004000970528     D KPJBA         E DS
004100970528     D*
004200970528?     *-- DS per Parametri Ricevuti
004300970528     D NDCL52DS      E DS
004400970528?     *-- DS per Parametri Ricevuti
004500970528     D NDCL68DS      E DS
004600970528     D ANGG12DS      E DS                  INZ
004700970528     D XATBDS        E DS                  INZ
004800050707A1125D* Parametri del PGM di gestione traduzioni
004900050707A1125D XtraDs        E DS                  Inz
005000970528?     *-- DS per Causali contabili
005100970528     D anopeds       E DS                  extname(anope00f) inz
005200970528     D*
005300030311     D* Parametri ricevuti
005400030311     DANPAR          E DS                  extname(anpards)
005500030311     D* DS gestione parametri modulo
005600030311     D XPAOUT          DS          2000
005700030311     D*-- DS per Parametri DI INSTALLAZIONE
005800030311     D anp057ds      E DS
005900970528      *    Interna per dati utente
006000990222c1001D*-------------
006100990222c1001D* Output modulo controlla conto
006200990222c1001D ND002DS       E DS
006300990222c1001D*-------------
006400970528     D XSOCDS          S           1000
006500970528     D SOC001        E DS                  EXTNAME(XSOC001DS)
006600970528     D*
006700970528    >D* DS per chiave tabella G75 (ultimi 12 byte della chiave di 15)
006800970528     D KeyGest         DS
006900970528     D TpLettera               1      1
007000970528     D TpOutPut                2      2
007100060712C1950D Prom                    3      6
007200970528     D*
007300970528     D DATLET          DS
007400970528     D  DSGG                   1      2  0
007500970528     D  DSBAR1                 3      3
007600970528     D  DSMM                   4      5  0
007700970528     D  DSBAR2                 6      6
007800970528     D  DSAA                   7      8  0
007900970528     D*
008000060712C1950X*PARAM           DS
008100060712  "  X* PARKSC                 1      8A
008200060712  "  X* PARNUM                 9     17  0
008300060712  "  X* PARAGE                18     25A
008400060712c1950D NDCL55ds      E DS
008500970528     D* Definizioni parametri driver soggetti
008600970528     D DVASOG        E DS
008700970528      *
008800970528     DdvaRic           S              1
008900970528     DdvaSocieta       S              3
009000970528     DdvaUnita         S              8
009100970528     DdvaDtRif         S               D
009200970528     DdvaStrutt        S             10
009300970528     DdvaLenOut        S              9B 0
009400970528     DdvaSnatura       S              1
009500970528     DdvaCodice        S              8
009600970528     DdvaLineav        S              3
009700970528     DdvaFiliale       S              3  0
009800970528     DdvaSogg          S              8
009900970528     DdvaTpInd         S              2
010000970528     DdvaCdInd         S              3
010100970528     DdvaOutput        S           4000
010200970528     DdvaErrore        S              1
010300970528      *
010400970528     Dwdate            S               D
010500970528     DEsito            S              1
010600970528     DGesmsg           S              1
010700970528     DLenout           S              9B 0
010800970528     DStruttura        S             10
010900970528     DData             S             10
011000000121C1319D ZDateISO        S               D
011100970528     D*-------------
011200970528     D* Parametri per XNMR
011300970528     D  XNMRIC         S              1
011400970528     D  XNMSOC         S              3
011500970528     D  XNMUNI         S              8
011600970528     D  XNMCTB         S              2
011700970528     D  XNMKEY         S              8
011800970528     D  XNMSER         S              4
011900970528     D  XNMCMT         S              1
012000970528     D  XNMDAT         S               D   DATFMT(*ISO)
012100970528     D  XNMIVA         S              1
012200970528     D  XNMREG         S              1
012300970528     D  XNMERR         S              1
012400970528     D  XNMNUM         S              9  0
012500970528     D  XNMAUT         S              1
012600970528     D  XNMDAG         S               D   DATFMT(*ISO)
012700000519A0778D*-------------
012800000519  "  D* Variabili per chiamata a X72Txt
012900000519  "  D XEsito          S              1A
013000000519  "  D XOpe            S              1A
013100000519  "  D XSocieta        S              3A
013200000519  "  D XTpTesto        S              2A
013300000519  "  D XUnita          S              8A
013400000519  "  D XLingua         S              1A
013500000523  "  D XClaUte         S              2A
013600000519  "  D XPosRig         S              1A
013700000519  "  D XNrRiga         S              9P 0
013800000519  "  D XRibat          S              1A
013900000519  "  D XTesRig         S             70A
014000000519  "  D XNrChiavi       S              1  0
014100000519  "  D XUsoUnita       S              1A
014200000519  "  D*
014300000519A0778D SavPosRig       S                   like(XPosRig)
014400000626A0778D SavLingua       S                   like(DvsLingua)
014500970528     D* campi vari
014600970528     D$udate           S               D
014700040331D1571D$udate10         S             10
014800970528     DDATMIN           C                   const('0001-01-01')
014900970528     DDATMAX           C                   const('9999-12-31')
015000970528      *
015100970528?    D*------------------------------------------------------------
015200970528?    D*  SCHIERE  ACCORPAMENTO PARTITE PER NUMERO ASSOLUTO
015300970528?    D*------------------------------------------------------------
015400000128     D $Chiave         S             27    DIM(5000)
015500000128     D $Dati           S             86    DIM(5000)
015600970528     D*Indici per le schiere
015700000128     D Z               S              5S 0 Inz
015800000128     D J               S              5S 0 Inz
015900970528     D*
016000970528     D ChiaveDS        DS
016100970528     D  DtScadenza             1     10
016200970528     D  SysEff                11     13S 0
016300970528     D  NrAsEff               14     22S 0
016400970528     D  NrRigaM               23     27S 0
016500970528     D*
016600970528     D DatiDS          DS
016700970528     D  NrPartita              1      9S 0
016800970528     D  DtPartita             10     19
016900970528     D  Causale               20     23
017000970528     D  Importo               24     42S 3
017100970528     D  ImportoD              43     61S 3
017200970528     D  Segno                 62     62
017300970528     D  Interessi             63     81S 3
017400970528     D  FlgL68                82     82
017500970528     D  Divisa                83     86
017600970528     D*
017700040331D1571DXscDivInt        S                   like(Epadiv)
017800040331  "  DDivInt           S                   like(Epadiv)
017900040331  "  D* Prototipazione procedure EURO
018000040331D1571D/COPY *LIBL/SRCCPY,PJX002PR
018100090504
018200090504>>>>>D S01Societa      S              3
018300090504>>>>>D S01Unita        S              8
018400090504>>>>>D S01nrs          S              6  0
018500090504
018600970528?     *------------------------------------------------------------
018700970528?     *  FLUSSO PRINCIPALE
018800970528?     *------------------------------------------------------------
018900970528      *
019000030627D1383C* Se PARNUM=*ALL'9' devo solo chiudere il prtf per poter eseguire
019100060712D1383C* cpysplf
019200060712c1950x***                if        Parnum=*all'9'
019300060712C1950c                   if        NumL55=*all'9'
019400060712D1383c                   close     ndcl55p2
019500060712C1950C                   seton                                        LR
019600060712D1383c                   goto      fine
019700030627D1383c                   endif
019800970729     C*
019900970528      * pulizia campi e indicatori
020000970528     C                   MOVE      '0'           *IN
020100010625C1501c* se ristampa accendo indicatore 17
020200010625  "  c                   if        L52TipLett='1'
020300010625  "  c                   seton                                        17
020400010625  "  c                   else
020500010625  "  c                   setoff                                       17
020600010625C1501c                   endif
020700970626     c*testo quale formato di stampa è stato richiesto (lettera o PT_Postel)
020800970626     c* e relativamente setto l'indicatore 15 che abilita il CPI(15)
020900970626     c                   if        L52Out  = '2'
021000970626     c                   SetOn                                        15
021100970626     c                   else
021200970626     c                   SetOff                                       15
021300970626     c                   endif
021400970528     C                   clear                   VOLTA1            1
021500970528     C                   z-add     1             pagina
021600970528      * pulizia indice schiera
021700970528     C                   z-add     0             J
021800970528     C* TOTALI CLIENTE
021900970528     C                   clear                   S04INTT
022000970528     C                   clear                   S04INDT
022100970528     C*
022200970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
022300970528     c* e pulisco di conseguenza il campo del tracciato relativo
022400970625     c                   if        L52Out = '1'
022500970528     c* allora stampo in formato PT_Postel
022600970528     C                   clear                   S04SCOTpt
022700970528     C                   clear                   S04SCDTpt
022800970528     C                   clear                   S04VALpt
022900970528     C                   clear                   S04VALMCpt
023000970528     c                   else
023100970528     c* altrimenti stampo in formato lettera
023200970528     C                   clear                   S04SCOT
023300970528     C                   clear                   S04SCDT
023400970528     C                   clear                   S04VAL
023500970528     C                   clear                   S04VALMC
023600970528     c                   endif
023700970528     c*
023800970528     C                   clear                   DIV
023900970528     C                   clear                   VAL
024000970528     C                   clear                   SCO
024100970528     C                   clear                   INT
024200970528     C                   clear                   $Chiave
024300970528     C                   clear                   $Dati
024400970528     C*
024500970528 +1  C     l52ord        ifeq      '1'
024600060712C1950X***                MOVEL     PARKSC        PIWKE1
024700060712C1950C                   MOVEL     KSCL55        PIWKE1
024800970528     C     PIWKE1        SETLL     NDPIW01L                           78
024900970528 x1  C                   else
025000060712C1950X***                movel     PARAGE        PIWKE1
025100060712  "  X***                movel     PARKSC        PIWKE2
025200060712  "  C                   movel     KSCAGEL55     PIWKE1
025300060712C1950C                   MOVEL     KSCL55        PIWKE2
025400970528     C     k2piw1        SETLL     NDPIW01L                           78
025500970528 -1  C                   endif
025600970528     C*
025700970528 1   C     *IN78         DOWEQ     '0'
025800970528     C*
025900970528 2   C     l52ord        ifeq      '1'
026000970528     C     PIWKE1        READE     NDPIW01L                               78
026100970528 x2  C                   else
026200970528     C     k2piw1        READE     NDPIW01L                               78
026300970528 -2  C                   endif
026400970528     C*
026500970528 +2  C     *IN78         IFEQ      '0'
026600970528     C                   move      PIWKSC        KKSC
026700970528      *
026800970528 +3  C     VOLTA1        IFEQ      ' '
026900010207     c*
027000970528      * Stampa Indirizzo e Testata
027100010207     C                   EXSR      ANAGRA
027200000519A0778C                   MOVE      'T'           SavPosRig
027300010207     C                   EXSR      CORPO
027400970528     C                   MOVE      '1'           VOLTA1
027500970528 -3  C                   END
027600970528     C*
027700001002D0717 * se chieste le annullate
027800001002D0717c****               if        l52ann = '*' and piwann = 'A' or
027900001002D0717c* o se chieste le valide
028000001002D0717c****                         l52ann = 'N' and piwann <>'A'
028100001002     c*  si può chiedere solo le valide o tutte
028200001002d0717c                   if        l52ann = 'N' and piwann <>'A' or
028300001002d0717c                             l52ann = 'S'
028400970528      * Verifica se la partita è in moneta di conto o in divisa
028500970528     C     PIWDIV        COMP      xscdiv                                 19
028600970528     C*
028700970528     C                   move      PIWDRP        KDRP
028800970528     C                   Z-ADD     PIWNRP        KNRP
028900970528     C                   move      PIWSERPAR     KSERPAR
029000970528      *
029100970528      * Lettura sintesi rate
029200970528      *
029300970528     C     KSCLPA        SETLL     NDSCL01L                           90
029400970528 +4  C     *IN90         DOWEQ     '0'
029500970528     C     KSCLPA        READE     NDSCL01L                               90
029600970528 +5  C     *IN90         IFEQ      '0'
029700970528      *
029800970528      * CONTROLLO LIMITE DATA SCADENZA E VALIDITA' RECORD
029900970528 +6  C     SCLDSC        IFLE      L52DFS
0300000002230302 C***  SCLANN        ANDEQ     *BLANK
030100000223     c* testo il flag di annullato a seconda di quanto richiesto
030200001002     c* solo le valide
030300001002     C     SCLANN        IFEQ      *blank
030400001002     c     l52ann        andeq     'N'
030500001002d0717C***  SCLANN        oreq      'A'
030600001002d0717c***  l52ann        andne     'N'
030700001002     c* tutte
030800001002d0717c     l52ann        oreq      'S'
030900970528      *
031000970528      * Testo se si tratta di un effetto a rischio
031100970528     C                   clear                   l68flg
031200970528     C                   move      sclcsi        l68csi
031300970528     C                   move      scldva        l68dva
031400970528     C                   move      sclkcc        l68kcc
031500970528     C                   move      scldsc        l68dsc
031600970528     C                   move      scltpg        l68tpg
031700970528     C                   move      scltif        l68tif
031800970528     C                   move      scltip        l68tip
031900970528     C                   move      sclseg        l68seg
032000970528     C                   move      sclcau        l68cau
032100970528     C                   CALL      'NDCL68R'
032200970528     C                   PARM                    NDCL68DS
032300040709     c* se non è un effetto a rischio
032400040709     C                   if        L68flg <> 'R'
032500040326D1568c* e non è a scadere
032600040709D1568C                             and L68flg <> 'A'
032700040709D1639C* o si tratta di un effetto a rischio senza fattura, per cui lo
032800040709  "  C* devo considerare nello scaduto per togliere l'importo, in quant
032900040709  "  C* in realtà si tratta di un anticipo.
033000040709  "  c                             or (L68FLG ='R' and %subst(sclflg:8:1)
033100040709D1639C                             = 'E')
033200010207     C*Valorizzo le schiere
033300010207     C                   EXSR      ValSchiere
033400010207     C                   endif
033500970528     C*
033600000223 -6  C                   ENDif
033700970528 -6  C                   ENDif
033800970528     C*
033900970528 -5  C                   ENDif
034000970528 -4  C                   ENDdo
034100970528     C* ROTTURA PARTITA
034200970528     C*
034300970528 -3  C                   ENDif
034400970528     C*
034500970528 -2  C                   ENDif
034600970528 -1  C                   ENDdo
034700970528      *
034800970528      * ROTTURA CLIENTE
034900970528     C* Scrivo quello che avevo caricato sulle schiere
035000010207     C                   EXSR      ReadSchier
035100971020     C*
035200970528      *
035300970528     C* Salto pagina PER OF
035400970528     C     *IN99         IFEQ      '1'
035500971020     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
035600971020     c* e stampo di conseguenza il tracciato relativo
035700971020     c                   if        L52Out = '1'
035800971020     c* allora stampo in formato PT_Postel
035900971020     C                   WRITE     CnSol09Pt
036000971020     c                   else
036100971020     c* altrimenti stampo in formato lettera
036200971020     C                   WRITE     CnSol09
036300971020     c                   endif
036400970528     c*
036500970528     C                   add       1             pagina
036600970528     C                   WRITE     CNSOL01RID
036700970528     C                   SETOFF                                       99
036800970528     C                   END
036900970528     C* DATA SCADENZA
037000970528     C                   CALLb     'XDT4000'
037100970528     C                   PARM      l52dfs        xdtamg           10
037200970528     C     s04dat        PARM                    xdtgma            6 0
037300970528     C                   PARM      2             xdtsta            1 0
037400970528     C* STAMPA COSTANTI
037500970528     C                   MOVE      'U'           KCCO
037600000626     C                   MOVE      Savlingua     KLIN
037700970528     C     KCST          CHAIN     NDCST00F                           55
037800000105     C   55              MOVE      xsclin        KLIN
037900000105     C   55KCST          CHAIN     NDCST00F                           55
038000000105     C   55              MOVEL     *BLANKS       S04REC
038100970528     C  N55              MOVEL     CSTDCO        S04REC
038200970528     C*
038300970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
038400970528     c* e stampo di conseguenza il tracciato relativo
038500970625     c                   if        L52Out = '1'
038600970528     c* allora stampo in formato PT_Postel
038700970528     C                   WRITE     CnSol30Pt
038800970528     c                   else
038900970528     c* altrimenti stampo in formato lettera
039000970528     C                   WRITE     CnSol30
039100970528     c                   endif
039200970528      *
039300970528      * Stampa totale scaduto del cliente nelle varie divise
039400970528      *
039500970528     C                   Z-ADD     1             Y
039600970528     C                   DO        10            Y
039700970528     C     DIV(Y)        IFNE      *BLANK
039800970528     C                   MOVE      DIV(Y)        S04DDD
039900040331D1571X***  DIV(Y)        COMP      XSCDIV                                 19
040000040331D1571C     DIV(Y)        COMP      XscDivInt                              19
040100970528     C     *IN19         IFEQ      *OFF
040200970528     C*
040300970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
040400970528     c* e valorizzo di conseguenza il campo del tracciato relativo
040500970625     c                   if        L52Out = '1'
040600970528     C                   z-add     SCO(Y)        S04SCDTpt
040700970528     C                   z-add     VAL(Y)        S04VALpt
040800970528     c* allora stampo in formato PT_Postel
040900970528     c                   else
041000970528     C                   z-add     SCO(Y)        S04SCDT
041100970528     C                   z-add     VAL(Y)        S04VAL
041200970528     c* altrimenti stampo in formato lettera
041300970528     c                   endif
041400970528     c*
041500970528     C                   z-add     INT(Y)        S04INDT
041600970528     C                   ELSE
041700970528     C*
041800970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
041900970528     c* e valorizzo di conseguenza il campo del tracciato relativo
042000970625     c                   if        L52Out = '1'
042100970528     C                   z-add     SCO(Y)        S04SCOTpt
042200970528     C                   z-add     VAL(Y)        S04VALMCpt
042300970528     c* allora stampo in formato PT_Postel
042400970528     c                   else
042500970528     C                   z-add     SCO(Y)        S04SCOT
042600970528     C                   z-add     VAL(Y)        S04VALMC
042700970528     c* altrimenti stampo in formato lettera
042800970528     c                   endif
042900970528     c*
043000970528     C                   z-add     INT(Y)        S04INTT
043100970528     C                   ENDIF
043200970528     C*
043300970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
043400970528     c* e stampo di conseguenza il tracciato relativo
043500970625     c                   if        L52Out = '1'
043600970528     c* allora stampo in formato PT_Postel
043700970528     C                   WRITE     CnSol31Pt
043800970528     c                   else
043900970528     c* altrimenti stampo in formato lettera
044000970528     C                   WRITE     CnSol31
044100970528     c                   endif
044200970528     c*
044300970528     C                   END
044400970528     C                   END
044500030311     c*
044600030311C1632C* Se richiesta addebito spese per invio
044700030311  "  c                   if        L52info = 'S'
044800030311  "  c* e se presente importo spese
044900030311  "  c                             and SpeInv057<> 0
045000040331C1632C                   z-add     SpeInv057     SpeInvT
045100040331D1571X***                eval      SpeDiv = xscdiv
045200040331D1571c                   eval      SpeDiv = XscDivInt
045300040331C1632C* Reperisco costante in lingua
045400030311  "  c                   clear                   SpeTesto
045500030311  "  C                   MOVE      'U'           KCCO
045600030311  "  C                   MOVE      Savlingua     KLIN
045700030311  "  C     KCST1         CHAIN     NDCST00F
045800030311  "  c                   if        %found(ndcst00f)
045900030311  "  c                   eval      speTesto = CstDco
046000030311  "  c                   endif
046100030311  "  c                   if        L52Out = '1'
046200030311  "  c                   write     CnSol41PT
046300030311  "  c                   else
046400030311  "  c                   write     CnSol41
046500030311  "  c                   endif
046600030311c1632c                   endif
046700970528      * RIGA DI CHIUSURA
046800970528     C                   MOVE      *ALL'-'       S04REC
046900970528     C                   WRITE     CNSOL03
047000970528     C                   MOVE      *BLANK        S04REC
047100970528      *
047200970528      * Verifico se fare salto pagina per OVERFLOW
047300970528      *
047400970528     C     *IN99         IFEQ      '1'
047500970528     C*
047600970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
047700970528     c* e stampo di conseguenza il tracciato relativo
047800970625     c                   if        L52Out = '1'
047900970528     c* allora stampo in formato PT_Postel
048000970528     C                   WRITE     CnSol09Pt
048100970528     c                   else
048200970528     c* altrimenti stampo in formato lettera
048300970528     C                   WRITE     CnSol09
048400970528     c                   endif
048500970528     c*
048600970528     C                   add       1             pagina
048700970528     C                   WRITE     CNSOL01RID
048800970528     C                   SETOFF                                       99
048900970528     C                   END
049000970528      *
049100970528      * AZZERO TOTALE CLIENTE
049200970528      *
049300970528     C                   clear                   S04INTT
049400970528     C                   clear                   S04INDT
049500970528     C*
049600970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
049700970528     c* e pulisco di conseguenza il campo del tracciato relativo
049800970625     c                   if        L52Out = '1'
049900970528     c* allora stampo in formato PT_Postel
050000970528     C                   clear                   S04SCOTpt
050100970528     C                   clear                   S04SCDTpt
050200970528     C                   clear                   S04VALpt
050300970528     C                   clear                   S04VALMCpt
050400970528     c                   else
050500970528     c* altrimenti stampo in formato lettera
050600970528     C                   clear                   S04SCOT
050700970528     C                   clear                   S04SCDT
050800970528     C                   clear                   S04VAL
050900970528     C                   clear                   S04VALMC
051000970528     c                   endif
051100970528     c*
051200970528     C                   clear                   DIV
051300970528     C                   clear                   VAL
051400970528     C                   clear                   SCO
051500970528     C                   clear                   INT
051600970528     C                   clear                   $Chiave
051700970528     C                   clear                   $Dati
051800970528     C                   clear                   J
051900970528     C*
052000970528      *
052100970528      * Stampa testo finale lettera
052200970528      *
052300000519A0778C                   MOVE      'U'           SavPosRig
052400010207     C                   EXSR      CORPO
052500970528     C* Costanti finali
052600970528     C*******************  WRITECNSOL10
052700970528     C*
052800970528     C* STAMPA PROSSIME SCADENZE
052900970528     C     L52SCA        IFEQ      *BLANK
053000010207     C                   EXSR      SCADE
053100970528     C                   END
053200970528     C*
053300970528     C     FINE          TAG
053400010625C1501C* se ristampa chiudo prtf
053500010625  "  C                   if        L52TipLett = '1'
053600010625  "  c                   close     ndcl55p2
053700010625  "  c                   seton                                        LR
053800010625C1501c                   else
053900010625     C                   SETON                                            RT
054000010625C1501c                   endif
054100970528     C*
054200970729      ************************************************************
054300970729      * Apertura prtf con tipo lettera da Xsoc
054400970729      ************************************************************
054500970729     C     OpnPrtf       BegSr
054600970729     C*
054700970729     c* richiamo programma X32 per recuperare sulla tabella 'G75' il comando
054800970729     C* di ovrPrtf
054900970729     C                   Exsr      CallX32
055000991108B0001C**Se trova il comando di OvrPrtF in tabella 'G75' apre il file solo la
055100991108  "  C**prima volta che apre il programma
055200991108  "  C*
055300991108  "   * Anche se non trova il comando OVRPRTF nella tabella G75
055400991108  "   * apre il file (solo la prima volta che apre il pgm)
055500991108B0001c***                if        X32Errore = *Off
055600970729     C                   Open      NdcL55P2
055700991108B0001c***                endif
055800970729     C*
055900970729     C                   ENDSR
056000970729     C/EJECT
056100030311     c*
056200970528     C***********************************************************
056300970528     C* Chiamata ad X32 per eseguire OvrPrtF
056400970528     C***********************************************************
056500970528     C     CallX32       BEGSR
056600970528     c
056700970528     c                   Eval      X32Societa = L52CSI
056800970528     c                   Eval      X32CdStamp = 'SOL'
056900970528     c                   Eval      X32Unita   = L52DVA
057000970528     c                   Clear                   KeyGest
057100970528     c* Stampo tipo lettera selezionato (Estratto conto o Sintesi rate)
057200970528     c                   Move      L52Let        TpLettera
057300970528     c* Tipo supporto di stampa selezionato (cartaceo o magnetico)
057400970625     c                   if        L52Out  = '2'
057500991108     c                   move      'C'           TpOutPut
057600970528     c                   else
057700991108     c                   move      'P'           TpOutPut
057800970528     c                   endif
057900060712C1950C                   if        PromemL55=*on
058000060712  "  c                   eval      Prom='PROM'
058100060712  "  C                   else
058200060712  "  C                   clear                   Prom
058300060712C1950C                   endif
058400970528     c                   eval      X32KeyGest = KeyGest
058500970528     C                   call      'X32OVRPRTF'
058600970528     C                   PARM                    X32Societa        3
058700970528     C                   PARM                    X32CdStamp        3
058800970528     C                   PARM                    X32Unita          8
058900970729     C                   PARM                    X32KeyGest       12
059000970528     C                   PARM                    X32Errore         1
059100991108B0001C                   PARM                    KPJBA
059200970528     c*
059300970528     c                   ENDSR
059400970528     C/EJECT
059500010207      *----------------------------------------------------
059600010207      * A N A G R A  - DATI ANAGRAFICI CLIENTE
059700010207      *----------------------------------------------------
059800970528     C     ANAGRA        BEGSR
059900010207      *
060000010207      * Reperimento dati cliente
060100010207D0842C                   EXSR      Daticli
060200010207     c*
060300010207      * Reperimento dati agente
060400010207D0842C                   EXSR      DatiAge
060500010207      *
060600970723     C*
060700970723     c* se trovata cerco su antxt
060800000105     c* prima provo  con lingua e unità
060900040326A0778 *
061000000519  "  C                   eval      XEsito = *Off
061100000519  "  C                   eval      XOpe = '6'
061200000519  "  C                   eval      XSocieta = XScSoc
061300000519  "  C                   eval      XUnita   = L52DVA
061400000519  "  C                   eval      XTpTesto = PIWTtx
061500000626  "  C                   eval      XLingua  = SavLingua
061600000523  "  C                   eval      XClaUte = *blanks
061700000519  "  C                   eval      XPosRig = 'A'
061800000519  "  C                   eval      XNrRiga = *zeros
061900000519  "  C                   eval      XRibat = *blanks
062000000519  "  C                   eval      XTesRig = *blanks
062100000519  "  C                   eval      XNrChiavi = 4
062200000519  "  C                   eval      XUsoUnita = '1'
062300000519  "   *
062400000519  "  C                   exsr      CallX72
062500000519  "   *
062600000519  "   *
062700000519  "   * Se non trovato nulla provo con la lingua della società
062800000519  "  C                   if        XEsito <> *Off
062900000519  "   *
063000000519  "  C                   eval      XEsito = *Off
063100000519  "  C                   eval      XOpe = '6'
063200000519  "  C                   eval      XSocieta = XScSoc
063300000519  "  C                   eval      XUnita   = L52DVA
063400000519  "  C                   eval      XTpTesto = PIWTtx
063500000519  "  C                   eval      XLingua  = XScLin
063600000523  "  C                   eval      XClaUte = *blanks
063700000519  "  C                   eval      XPosRig = 'A'
063800000519  "  C                   eval      XNrRiga = *zeros
063900000519  "  C                   eval      XRibat = *blanks
064000000519  "  C                   eval      XTesRig = *blanks
064100000519  "  C                   eval      XNrChiavi = 4
064200000519  "  C                   eval      XUsoUnita = '1'
064300000519  "   *
064400000519  "  C                   exsr      CallX72
064500000519A0778 *
064600000519     C                   endif
064700970528     C*
064800970528     C*
064900970528     C* NUMERAZIONE SOLLECITO
065000000224b1   C     L52DEF        IFEQ      'S'
065100970528     C*
065200970528     C*                                                    *
065300981203b2   C     PIWFLA        IFEQ      'S'
065400970528     C*                                                    *
065500970528     C                   Z-ADD     PIWNUM        S04NRS
065600970528     C*                                                    *
065700981203x2   C                   ELSE
065800970528     C*                                                    *
065900970528     C                   MOVEL     'SOLL'        XNMSER
066000970528     C                   CALLb     'XNMR'
066100970528     C                   PARM      '3'           XNMRIC
066200970528     C                   PARM      XSCSOC        XNMSOC
066300970528     C                   PARM      l52dva        XNMUNI
066400970528     C                   PARM      'CG'          XNMCTB
066500970528     C                   PARM      *BLANKS       XNMKEY
066600970528     C                   PARM                    XNMSER
066700970528     C                   PARM      *OFF          XNMCMT
066800970528     C                   PARM      $udate        XNMDAT
066900970528     C                   PARM      *OFF          XNMIVA
067000970528     C                   PARM      *OFF          XNMREG
067100970528     C                   PARM      *BLANKS       XNMERR
067200970528     C                   PARM      0             XNMNUM
067300970528     C                   PARM      *OFF          XNMAUT
067400970528     C                   PARM      $udate        XNMDAG
067500970528     C                   PARM      '1'           XFLG              1
067600981203 b3  C     xnmerr        IFEQ      '0'
067700060712c1950x***                Z-ADD     XNMNUM        PARNUM
067800060712c1950C                   Z-ADD     XNMNUM        NumL55
067900970528     C                   Z-ADD     XNMNUM        S04NRS
068000970528     C                   ELSE
068100970528     C                   Z-ADD     0             S04NRS
068200060712C1950X***                Z-ADD     0             PARNUM
068300060712C1950C                   Z-ADD     0             NumL55
068400981203 e3  C                   END
068500970528     C***********                                          *
068600981203 e2  C                   END
068700970528     C***********                                          *
068800970528     C*
068900981203 x1  C                   ELSE
069000010625     C*
069100010625C1501c                   if        L52TipLett = ' '
069200010625     C                   Z-ADD     0             S04NRS
069300060712C1950X***                Z-ADD     0             PARNUM
069400060712C1950C                   Z-ADD     0             NumL55
069500010625C1501c                   else
069600060712c1501C                   Z-ADD     PIWNUM        S04NRS
069700060712c1950x***                Z-ADD     PIWNUM        PARNUM
069800060712c1950C                   Z-ADD     PIWNUM        NumL55
069900010625C1501c                   ENDIF
070000970528     C*
070100981203 e1  C                   END
070200030901     C*
070300030901D1383c                   z-add     s04nrs        s01nrs
070400040402D1576C                   EVAL      S01Societa = l52csi
070500040402D1576C                   EVAL      S01Unita   = l52dva
070600060704C1950C                   EVAL      S01KSC     = PIWKSC
070700970528     C*
070800970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
070900970528     c* e stampo di conseguenza il tracciato relativo
071000981203 b1  c                   if        L52Out = '1'
071100970528     c* allora stampo in formato PT_Postel
071200970528     C                   WRITE     CnSol01Pt
071300970528     c                   else
071400970528     c* altrimenti stampo in formato lettera
071500970528     C                   WRITE     CnSol01
071600981203 e1  c                   endif
071700970528     c*
071800970528     C                   SETOFF                                       99
071900970528     C*
072000970528     C                   ENDSR
072100010207     C/EJECT
072200010207      *----------------------------------------------------
072300010207      * Reperimento dati cliente
072400010207      *----------------------------------------------------
072500010207     C     DatiCli       BEGSR
072600010207      *
072700010207     c                   clear                   s05rag
072800010207     c                   clear                   s05via
072900010207     c                   clear                   s05cit
073000010207     c                   clear                   s05citpt
073100010207     c                   clear                   s05naz
073200010207      *
073300010207     C                   movel     'DVASOG'      DVASTRUTT
073400010207     C                   move      xscsoc        DVASOCIETA
073500010207     C                   move      piwksc        dvacodice
073600010207     C                   move      'C'           dvasnatura
073700010207     C                   clear                   dvalineav
073800010207     C                   clear                   dvafiliale
073900010207c1001C                   clear                   DVATpInd
074000010207c1001C                   Clear                   DVACdInd
074100010207c1001C                   clear                   DVASogg
074200010207c1001 * reperimento codice soggetto + tipo e codice indirizzo
074300010207c1001C                   EXSR      CALLNDMVC2
074400010207c1001 *
074500010207     C                   EXSR      CALLDVASOG
074600010207      *
074700010207b1   C     dvaerrore     ifeq      *off
074800010207     c* salvo codice lingua cliente sempre, in modo tale che se l'ordinament
074900010207     c* è per agente e l'agente non c'è rimane quella del cliente
075000010207     c                   eval      SavLingua = DvsLingua
075100010207D0035C****               movel     dvsdes        s05rag
075200010207D0035C                   movel     DvsDesFil     s05rag
075300010207     C                   MOVEL     dvsindriz     S05VIA
075400010207     C*
075500010207b2   C     DVSCAP        IFEQ      *BLANKS
075600010207     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
075700010207     c* e valorizzo di conseguenza il campo del tracciato relativo
075800010207b3   c                   if        L52Out = '1'
075900010207     c* allora stampo in formato PT_Postel
076000010207     C                   EVAL      S05CITpt      = DVSLOCALIT
076100010207     c                   else
076200010207     c* altrimenti stampo in formato lettera
076300010207     C                   EVAL      S05CIT        = DVSLOCALIT
076400010207E3   c                   endif
076500010207     c*
076600010207X2   C                   ELSE
076700010207     C*
076800010207     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
076900010207     c* e valorizzo di conseguenza il campo del tracciato relativo
077000010207B3   c                   if        L52Out = '1'
077100010207     c* allora stampo in formato PT_Postel
077200010207     C     DVSCAP        CAT(P)    DVSLOCALIT:1  S05CITpt
077300010207c1001C     S05CITpt      CAT       DVSPROV:1     S05CITpt
077400010207     c                   else
077500010207     c* altrimenti stampo in formato lettera
077600010207     C     DVSCAP        CAT(P)    DVSLOCALIT:1  S05CIT
077700010207     C     S05CIT        CAT       DVSPROV:1     S05CIT
077800010207e3   c                   endif
077900010207     c*
078000010207e2   C                   ENDIF
078100010207     c*
078200010207     C* Stampo la nazione solo se estera
078300010207B2   C     DVSSTATO      IFNE      XSCSTA
078400010207     C                   MOVEL     DESG12        S05NAZ
078500010207E2   C                   ENDIF
078600010207     c*
078700010207E1   C                   endif
078800010207     c*
078900010207     c                   endsr
079000010207     C/EJECT
079100010207      *----------------------------------------------------
079200010207      * Reperimento dati agente
079300010207      *----------------------------------------------------
079400010207     C     DatiAge       BEGSR
079500010207      *
079600010207     C                   clear                   s05Age
079700010207     C                   clear                   s05deg
079800010207     C                   clear                   S05VIg
079900010207     C*                  clear                   S05CAg
080000010207     C                   clear                   S05CIg
080100010207     C                   clear                   S05NAG
080200010207     C                   clear                   S05epc
080300010207      *
080400010207     C                   move      piwsap        dvacodice
080500010207     C                   move      'F'           dvasnatura
080600010207     C                   movel     'DVASOG'      DVASTRUTT
080700010207c1001C                   clear                   DVATpInd
080800010207c1001C                   Clear                   DVACdInd
080900010207c1001C                   clear                   DVASogg
081000010207     C                   EXSR      CALLDVASOG
081100010207      *
081200010207b1   C     dvaerrore     ifeq      *off
081300010207     c* salvo codice lingua agente  se ordinamento per agente
081400010209D0832C* la lingua +è sempre quella del cliente
081500010209D0832C**   l52ord        ifeq      '2'
081600010209  "  c**                 eval      SavLingua = DvsLingua
081700010209D0832C**                 endif
081800010207     C                   move      piwsap        s05age
081900010207D0035C****               movel     dvsdes        s05deg
082000010207D0035C                   movel     DvsDesFil     s05deg
082100010207     C                   movel     dvsindriz     S05VIg
082200010207     C*                  movel     dvscap        S05CAg
082300010207     C*                  movel     dvslocalit    S05CIg
082400010207b2   C     DVSCAP        IFEQ      *BLANKS
082500010207     C                   EVAL      S05CIG        = DVSLOCALIT
082600010207     C                   ELSE
082700010207     C     DVSCAP        CAT(P)    DVSLOCALIT:1  S05CIG
082800010207e2   C                   ENDIF
082900010207     C     S05CIG        CAT       DVSPROV:1     S05CIG
083000010207     C* Stampo la nazione solo se estera
083100010207b2   C     DVSSTATO      IFNE      XSCSTA
083200010207     C                   MOVEL     DESG12        S05NAG
083300010207     c                   else
083400010207     c                   movel     *blanks       s05nag
083500010207e2   C                   ENDIF
083600010207     c*
083700010207     c                   movel     'e p.c.'      s05epc
083800010207     C                   seton                                        25
083900010207E1   C                   endif
084000010207     c*
084100010207     c                   endsr
084200970528     C/EJECT
084300970528      ************************************************************
084400990222c1001 * DATI ANAGRAFICI conto
084500990222  .   ************************************************************
084600990222  .  C     CALLNDMVC2    BEGSR
084700990222  .  C* reperisco il conto per avere tipo+codice indirizzo
084800990222  .  C*
084900990222  .  C                   move      datmin        data
085000990222  .  C                   EVAL      LenOut = %Size(ND002DS)
085100990222  .  C                   CALLB     'NDMVC002'
085200990222  .  C                   PARM                    XscSoc
085300990222  .  C                   PARM                    PIWKcc
085400990222  .  C                   PARM                    PIWKsc
085500990222  .  C                   PARM                    Data
085600990222  .  C                   PARM      *off          GesMsg
085700990222  .  C                   PARM                    Esito
085800990222  .  C                   PARM      'ND002DS'     Struttura
085900990222  .  C                   PARM                    ND002DS
086000990222  .  C                   PARM                    LenOut
086100990222  .  C*
086200990222  .  C                   IF        Esito <> '1'
086300990222  .  C                   Eval      DVATpInd  = TpInd002
086400990222  .  C                   Eval      DVACdInd  = CdInd002
086500990222  .  C                   Eval      DVASogg   = Sogg002
086600990222  .  C                   Clear                   DVACodice
086700990222  .  C                   Clear                   DVASnatura
086800990222  .  C                   ENDIF
086900990222  .  C*
087000990222  .  C                   ENDSR
087100990222c1001C/EJECT
087200990215      ************************************************************
087300990215      * DATI ANAGRAFICI SOGGETTO
087400990215      ************************************************************
087500990215     C     CALLDVASOG    BEGSR
087600970528     C*
087700970528     C                   move      '1'           DVARIC
087800970528     C                   move      xscsoc        DVASOCIETA
087900970528     C                   move      l52dva        DVAUNITA
088000970528     C                   eval      DVALENOUT = %Size(dvasog)
088100990222c1001c****************   move      *blanks       dvasogg
088200970528     C*
088300990222c1001c                   If        DVASogg  <>   *blanks
088400990222c1001C                   CALLB     'NDDVASOG'    PDVASOG1
088500990222c1001c                   Else
088600990222     C                   CALLB     'NDDVASOG'    PDVASOG
088700990222C1001c                   EndIf
088800990222     C                   movel     dvaoutput     dvasog
088900970528     C*
089000970528     C                   CLEAR                   ANGG12DS
089100970528     C                   CLEAR                   XATBDS
089200970528     C                   EVAL      XTBCOD        = 'G12'
089300970528     C                   EVAL      XTBKEY        = *ZEROS
089400970528     C                   MOVE      DVSSTATO      XTBKEY
089500970528     C                   EXSR      CALLXATB
089600970528     C     XTBERR        IFEQ      '0'
089700970528     C                   EVAL      ANGG12DS      = XTBUNI
089800970528     C                   ENDIF
089900970528     C*
090000970528     C                   ENDSR
090100970528      /TITLE CHIAMATA A XATB
090200970528      ************************************************************
090300970528      * TABELLE
090400970528      ************************************************************
090500970528     C     CALLXATB      BEGSR
090600970528     C*
090700030707D1393x****               EVAL      XTBLIN        = XSCLIN
090800030707D1393C                   EVAL      XtbLin        = dvsLingua
090900970528     C                   EVAL      XTBAZI        = XSCSOC
091000970528     C                   EVAL      XTBRIC        = '1'
091100970528     C                   EVAL      XTBALC        = *OFF
091200970528     C                   EVAL      XTBUTE        = XSCCUT
091300970528     C                   CALLB     'XATB'
091400970528     C                   PARM                    XATBDS
091500970528     C*
091600970528     C                   ENDSR
091700970528?     *----------------------------------------------------
091800970528?     * C O R P O  - STAMPA TESTO LETTERA -
091900970528?     *----------------------------------------------------
092000970528     C     CORPO         BEGSR
092100970528     C*
092200040326a0778 *
092300000519  "   * Chain su ANTXT01L con i dati relativi al testo avente
092400000519  "   * tipo riga = 'A' reperito in fase di controllo e con il posrig
092500000519  "   * impostato alla chiamata della subroutine
092600000519  "  C                   eval      XEsito = *Off
092700000519  "  C                   eval      XOpe = '1'
092800000523  "  C                   eval      XClaUte = *blanks
092900000519  "  C                   eval      XPosRig = SavPosRig
093000000519  "  C                   eval      XNrRiga = *zeros
093100000519  "  C                   eval      XRibat = *blanks
093200000519  "  C                   eval      XTesRig = *blanks
093300000519  "  C                   eval      XNrChiavi = 5
093400000519  "  C                   eval      XUsoUnita = '0'
093500000519  "   *
093600000519  "  C                   exsr      CallX72
093700000519  "   *
093800000519A0778C                   dow       XEsito = *Off
093900970528     C*
094000970723  2  C     *IN99         IFEQ      '1'
094100970528     C*
094200970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
094300970528     c* e stampo di conseguenza il tracciato relativo
094400970723  3  c                   if        L52Out = '1'
094500970528     c* allora stampo in formato PT_Postel
094600970528     C                   WRITE     CnSol09Pt
094700970528     c                   else
094800970528     c* altrimenti stampo in formato lettera
094900970528     C                   WRITE     CnSol09
095000970723  3e c                   endif
095100970528     c*
095200970528     C                   add       1             pagina
095300970528     C                   WRITE     CNSOL01RID
095400970528     C                   SETOFF                                       99
095500970723  2e C                   Endif
095600970528     C*
095700040401     C     XRibat        COMP      '1'                                    26
095800000519A0778C                   movel     XTesRig       TxtRig
095900990614d0110C*doppio campo  per la ribattitura
096000000519A0778C   26              movel     XTesRig       TxtRig1
096100970528     C                   WRITE     CNSOL02
096200990614d0110C***26              WRITE     CNSOL02
096300970528     C*
096400040401A0778 *
096500000519  "   * Reade su ANTXT01L per trovare i record successivi
096600000519  "  C                   eval      XEsito = *Off
096700000519  "  C                   eval      XOpe = '4'
096800000523  "  C                   eval      XClaUte = *blanks
096900000519  "  C                   eval      XNrRiga = *zeros
097000000519  "  C                   eval      XRibat = *blanks
097100000519  "  C                   eval      XTesRig = *blanks
097200000519  "  C                   eval      XNrChiavi = 5
097300000519  "  C                   eval      XUsoUnita = '0'
097400000519  "   *
097500000519  "  C                   exsr      CallX72
097600000519A0778 *
097700970723   1eC                   ENDdo
097800970528     C*
097900000519A0778C     SavPosRig     IFEQ      'T'
098000970528     C*
098100970528     C                   MOVE      *ALL'-'       S04REC
098200970528     C                   WRITE     CNSOL03
098300970528     C                   MOVE      *BLANK        S04REC
098400970528     C*
098500970528     C                   MOVE      'T'           KCCO
098600000626     C                   MOVE      Savlingua     KLIN
098700000511     C     KCST          SETLL     NDCST00F                               41
098800000511     c  N41              do
098900000110     C                   MOVE      xsclin        KLIN
099000000511     C     KCST          SETLL     NDCST00F                               41
099100000110     c                   enddo
099200000511     c                   if        *in41=*on
099300000511     c                   setoff                                       40
099400970723  2  C     *IN40         DOWEQ     '0'
099500970528     C     KCST          READE     NDCST00F                               40
099600970723  3  C     *IN40         IFEQ      '0'
099700970528     C                   MOVEL     CSTDCO        S04REC          120
099800970528     C                   WRITE     CNSOL03
099900970528     C                   MOVE      *BLANK        S04REC
100000000511  3e C                   endif
100100000511  2e C                   enddo
100200000511  3e C                   endif
100300970528     C*
100400970528     C                   MOVE      *ALL'-'       S04REC
100500970528     C                   WRITE     CNSOL03
100600970528     C                   MOVE      *BLANK        S04REC
100700970528     C*
100800970723  1e C                   END
100900970528     C*
101000970528     C                   ENDSR
101100970528     C*
101200000519A0778C************************************************************
101300000519  "  C* Chiamata al driver di lettura ANTXT00F
101400000519  "  C************************************************************
101500000519A0778C     CallX72       begsr
101600000519      *
101700000519     C                   call      'X72TXT'
101800000519     C                   parm                    XEsito
101900000519     C                   parm                    XOpe
102000000519     C                   parm                    XSocieta
102100000519     C                   parm                    XTpTesto
102200000519     C                   parm                    XUnita
102300000519     C                   parm                    XLingua
102400000523     C                   parm                    XClaUte
102500000519     C                   parm                    XPosRig
102600000519     C                   parm                    XNrRiga
102700000519     C                   parm                    XRibat
102800000519     C                   parm                    XTesRig
102900000519     C                   parm                    XNrChiavi
103000000519     C                   parm                    XUsoUnita
103100000519      *
103200000519A0778C                   endsr
103300970528?    C**********************************************************************
103400970528?    C* ValSchiere - Valorizzo le schiere Chiave + Dati
103500970528?    C**********************************************************************
103600970528     C     ValSchiere    BEGSR
103700970528     C*
103800970528     C*Compongo la chiave
103900970528     C*
104000970528     C                   Clear                   DtScadenza
104100970528     C*
104200970528     C                   eval      DtScadenza  = SclDsc
104300970528     C                   z-add     SclSysEff     SysEff
104400970528     C                   z-add     SclNrAsEff    NrAsEff
104500970528     C                   z-add     SclNrRigaM    NrRigaM
104600970528     C*
104700970528     C*Compongo i dati
104800970528     C*
104900970528     C                   Clear                   FlgL68
105000970528     C                   Clear                   Divisa
105100970528     C                   Clear                   Segno
105200040331D1571X***                Clear                   DtPartita
105300040331D1571C                   eval      DtPartita = '0001-01-01'
105400970528     C                   Clear                   Causale
105500970528     C*
105600970528     C                   eval      Divisa      = PiwDiv
105700970528     C                   z-add     SclNrP        NrPartita
105800970528     C                   eval      DtPartita   = SclDrP
105900970528     C                   eval      Causale     = SclCau
106000970528     C                   eval      FlgL68      = L68flg
106100970528     C
106200970528     C   19              z-add     SclImD        Importo
106300040505D1594C   19              z-add     0             ImportoD
106400970528     C  n19              z-add     SclImD        ImportoD
106500040505D1594C  n19              z-add     0             Importo
106600970528     C
106700970528     C                   eval      Segno       = SclSeg
106800970528     C                   z-add     SclInt        Interessi
106900000404c1362C* Aggiungo le spese
107000000404c1362C                   add       SclIm1        Interessi
107100970528     C*
107200970528     C*Valorizzo la schiera
107300970528     C*
107400970528     C* NrAsEff = 0
107500970528     C                   If        SclNrAsEff  = 0
107600970528     C                   add       1             J
107700970528     C                   Movea     ChiaveDS      $Chiave(J)
107800970528     C                   Movea     DatiDS        $Dati(J)
107900970528     C*
108000970528     C* NrAsEff <> 0: verifico se i dati sono già nella schiera
108100970528     C*
108200970528     C                   Else
108300970528     C*
108400970528     C                   Z-ADD     1             Z
108500970528     C     Chiaveds      LOOKUP    $Chiave(Z)                             23
108600970528     C                   If        *in23  = *off
108700970528     C* Non trovati
108800970528     C                   add       1             J
108900970528     C                   Movea     ChiaveDS      $Chiave(J)
109000970528     C                   Movea     DatiDS        $Dati(J)
109100970528     C*
109200970528     C                   Else
109300970528     C* Trovati
109400970528     C                   Movea     $Dati(Z)      DatiDs
109500970528     C*Pulisco i riferimenti di partita
109600970528     C                   Clear                   NrPartita
109700040331D1571X***                Clear                   DtPartita
109800040331D1571C                   eval      DtPartita = '0001-01-01'
109900970528     C*
110000970528     C*Lascio la causale precedentemente inserita e lo stesso flgl68
110100970528     C*
110200970528     C*Sommo i nuovi importi
110300970528     C   19              Add       SclImD        Importo
110400970528     C  n19              Add       SclImD        ImportoD
110500970528     C*reimposto il segno
110600970528     C                   If        *in19 = *on and Importo >= 0
110700970528     C                             or *in19 = *off and ImportoD >= 0
110800970528     C                   eval      Segno       = 'D'
110900970528     C                   Else
111000970528     C                   eval      Segno       = 'A'
111100970528     C                   Endif
111200970528     C
111300970528     C                   Add       SclInt        Interessi
111400000404c1362C                   add       SclIm1        Interessi
111500970528     C*
111600970528     C                   Movea     DatiDS        $Dati(Z)
111700970528     C*
111800970528     C                   Endif
111900970528     C                   Endif
112000970528     C*
112100970528     C                   ENDSR
112200970528     C*
112300970528?    C**********************************************************************
112400970528?    C* ReadSchiere - Leggo le schiere e valorizzo CnSol04
112500970528?    C**********************************************************************
112600970528     C     ReadSchier    BEGSR
112700970528     C*
112800970528     C                   Z-ADD     1             J
112900000128     C     1             DO        5000
113000970528     C* Se la schiera è valorizzata
113100970528     C                   If        $Chiave(J) <>*blank
113200970528     C                   Movea     $Dati(J)      DatiDs
113300970528     C                   Movea     $Chiave(J)    ChiaveDs
113400970528     C* valorizzo il CnSol04
113500970528     C                   clear                   s04des
113600970528     C                   clear                   opedesbrev
113700970528     C                   exsr      readanope
113800970528     C                   eval      s04des = opedesbrev
113900970528      *
114000970528     C                   clear                   S04DRP
114100970528     C                   clear                   S04SCA
114200970528     C                   clear                   S04nrp
114300970528     C                   If        DtPartita <> DATMIN and DtPartita <>*blank
114400970528     C                   CALLb     'XDT4000'
114500970528     C                   PARM      DtPartita     xdtamg           10
114600970528     C     s04drp        PARM                    xdtgma            6 0
114700970528     C                   PARM      2             xdtsta            1 0
114800970528     C                   Z-ADD     NrPartita     S04NRP
114900970528     C                   endif
115000970528     C*
115100970528     C     DtScadenza    ifne      DATMIN
115200970528     C                   seton                                          22
115300970528     C                   CALLb     'XDT4000'
115400970528     C                   PARM      DtScadenza    xdtamg           10
115500970528     C     s04sca        PARM                    xdtgma            6 0
115600970528     C                   PARM      2             xdtsta            1 0
115700970528     C                   endif
115800970528     C*
115900970528     C                   Clear                   S04DIV
116000040331D1571x***                Eval      S04DIV      = Divisa
116100040331  "  C                   If        X0201RpDVS(Divisa   : DtPartita:
116200040331  "  C                                       ANDVS00F: %size(ANDVS00F):
116300040331  "  C                                       ANDVX00F: %size(ANDVX00F)) <> 1
116400040331  "  C                   eval      S04Div   =    DvsCdInter
116500040331D1571C                   EndIf
116600970528     C*
116700970528     C* Verifica  la divisa
116800970528     C     Divisa        COMP      xscdiv                                 19
116900970528     C*
117000970528     C                   Clear                   S04Dar
117100970528     C                   Clear                   S04Dad
117200970528     C                   Clear                   S04Ave
117300970528     C                   Clear                   S04Avd
117400970528     C
117500970528     C     Segno         IFEQ      'D'
117600970528     C   19              Z-ADD     Importo       S04DAR
117700970528     C  N19              Z-ADD     ImportoD      S04DAD
117800970528     C                   SETON                                        20
117900970528     C                   ELSE
118000970528     C   19              Z-ADD     Importo       S04AVE
118100970528     C   19              MULT      -1            S04AVE
118200970528     C  N19              Z-ADD     ImportoD      S04AVD
118300970528     C  N19              MULT      -1            S04AVD
118400970528     C                   SETON                                        21
118500970528     C                   END
118600970528      *
118700970528     C                   Clear                   S04InrMc
118800970528     C                   Clear                   S04Inr
118900970528atl  C   19              Z-ADD     Interessi     S04INRMC
119000970528atl  C  N19              Z-ADD     Interessi     S04INR
119100970528      *
119200970528      *
119300970528     C                   Clear                   S04Tot
119400970528     C                   Clear                   S04Tod
119500970528     C     *IN19         ifeq      *on
119600970528     C     Importo       ADD       Interessi     S04TOT
119700970528     C                   ELSE
119800970528     C     ImportoD      ADD       Interessi     S04TOD
119900970528     C                   END
120000970528      *
120100970528?    C   99              EXSR      OVERFL
120200970528      *
120300970528atl  C***  scltip        ifne      'I'
120400970528     C***                MOVEL     opedesbrev    S04DES
120500970528atl  C***                else
120600970528atl  C***                eval      s04des = 'INSOLUTO + SPESE'
120700970528atl  C***                endif
120800970528     C*
120900970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
121000970528     c* e stampo di conseguenza il tracciato relativo
121100970625     c                   if        L52Out = '1'
121200970528     c* allora stampo in formato PT_Postel
121300970528     C                   WRITE     CnSol04Pt
121400970528     c                   else
121500970528     c* altrimenti stampo in formato lettera
121600970528     C                   WRITE     CnSol04
121700970528     c                   endif
121800970528     C*
121900970528      *  CARICAMENTO SCHIERA DIVISE
122000970528      *
122100970528     C                   Z-ADD     1             Y                 3 0
122200040331D1571C                   If        X0201RpDVS(Divisa   : DtPartita:
122300040331  "  C                                       ANDVS00F: %size(ANDVS00F):
122400040331  "  C                                       ANDVX00F: %size(ANDVX00F)) <> 1
122500040331  "  C                   eval      DivInt   =    DvsCdInter
122600040331  "  C                   EndIf
122700040331  "  x***  Divisa        LOOKUP    DIV(Y)                                 80
122800040331D1571c     DivInt        LOOKUP    DIV(Y)                                 80
122900970528     C  N80*blanks       LOOKUP    DIV(Y)                                 80
123000040331D1571X** 80              MOVE      Divisa        DIV(Y)
123100040331D1571C   80              MOVE      DivInt        DIV(Y)
123200970528     C   80              add       Interessi     INT(Y)
123300970528     C                   If        *in80 = *on and *in19 = *on
123400970528     C                   add       Importo       VAL(Y)
123500970528     C                   add       Interessi     VAL(Y)
123600970528     C                   endif
123700970528     C                   If        *in80 = *on and *in19 = *off
123800970528     C                   ADD       ImportoD      VAL(Y)
123900970528     C                   add       Interessi     VAL(Y)
124000970528     C                   endif
124100970528     C                   If        *in80 = *on and *in19 = *on
124200970528     C                   add       Importo       Sco(Y)
124300970528     C                   endif
124400970528     C                   If        *in80 = *on and *in19 = *off
124500970528     C                   ADD       ImportoD      Sco(Y)
124600970528     C                   endif
124700970528      *
124800970528     C*
124900970528     C*Incremento il contatore della schiera
125000970528     C                   Add       1             J
125100970528     C*
125200970528     C                   else
125300970528     C*
125400970528     C                   Leave
125500970528     C*
125600970528     C                   Endif
125700970528     C*
125800970528     C                   EndDo
125900970528     C*
126000970528     C                   ENDSR
126100970528      *
126200970528?     *----------------------------------------------------
126300970528?     * READANOPE  - Reperimento dati da ANOPE00F
126400970528?     *----------------------------------------------------
126500970528     C     readanope     BEGSR
126600970528      *
126700970528     C                   clear                   anopeds
126800970528     C                   move      datmin        data
126900970528     C                   eval      lenout = %size(anopeds)
127000970528     C                   CallB     'NDMVC003'
127100970528     C                   PARM                    xscsoc
127200970528     C                   PARM                    causale
127300970528     C                   PARM                    data
127400970528     C                   PARM      '1'           Uso               1
127500970528     C                   PARM      *OFF          gesmsg
127600970528     C                   PARM      *ON           esito
127700970528     C                   PARM      'ANOPE'       struttura
127800970528     C                   PARM                    anopeds
127900970528     C                   PARM                    lenout
128000970528      * Traduco la causale ( solo se la lingua diversa da quella di società)
128100050707A1125x*                  if        Savlingua<> xsclin
128200050707  "  x*                  CALLB     'XTRA'
128300050707  "  x*                  PARM      '1'           XTRRIC            1
128400050707  "  x*                  PARM      XSCSOC        XTRSOC            3
128500050707  "  x*                  PARM      'ANOPE'       XTRFIL            5
128600050707  "  x*                  PARM      OPECAUSALE    XTRKEY           30
128700050707  "  x*                  PARM      Savlingua     XTRLIN            1
128800050707  "  x*                  PARM      'OPEDESCR'    XTRFLD           10
128900050707  "  x*                  PARM      *BLANKS       XTRTXT           50
129000050707  "  x*                  PARM      *BLANKS       XTRALC            1
129100050707  "  x*                  PARM      '0'           XTRERR            1
129200050707  "  x*                  PARM      ' '           XTRFSC            1
129300050707  "  x*                  PARM      'ANOPE00F'    XTRPAR           10
129400050707  "  C* in xsclin c'è la lingua utente, quindi va usato xsculm,
129500050707  "  c                   if        Savlingua<> xscUlm
129600050707  "  C*
129700050707  "  C                   Reset                   XTrads
129800050707  "  C                   movel     '1'           XTRRIC            1
129900050707  "  C                   movel     XSCSOC        XTRSOC            3
130000050707  "  C                   movel     'ANOPE'       XTRFIL            5
130100050707  "  C                   movel     OPECAUSALE    XTRKEY           30
130200050707  "  C                   movel     Savlingua     XTRLIN            1
130300050719  "  C                   movel     'OPEDESBREV'  XTRFLD           10
130400050707  "  C                   movel     'ANOPE00F'    XTRPAR           10
130500050707  "  c                   CALLB     'XTRA'
130600050707A1125c                   PARM                    XTRADS
130700970528     C     XTRERR        IFEQ      '0'
130800970528     C                   MOVEL     XTRTXT        OPEDESBREV
130900050719A1125c                   else
131000050719  "  C* Se non trovata la breve provo con la lunga
131100050719  "  C                   movel     'OPEDESCR  '  XTRFLD           10
131200050719  "  c                   CALLB     'XTRA'
131300050719  "  c                   PARM                    XtraDs
131400050719  "  C     XTRERR        ifeq      '0'
131500050719  "  C                   MOVEL     XTRTXT        OPEDESBREV
131600050719  "  c                   endif
131700050719A1125C                   ENDIF
131800970528     c                   endif
131900970528      *
132000970528     C                   endsr
132100970528?     *----------------------------------------------------
132200970528?     * O V R F L  - GESTIONE OVERFLOW
132300970528?     *----------------------------------------------------
132400970528     C     OVERFL        BEGSR
132500970528     C*
132600970528     C*
132700970528     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
132800970528     c* e stampo di conseguenza il tracciato relativo
132900970723 1   c                   if        L52Out = '1'
133000970528     c* allora stampo in formato PT_Postel
133100970528     C                   WRITE     CnSol09Pt
133200970528     c                   else
133300970528     c* altrimenti stampo in formato lettera
133400970528     C                   WRITE     CnSol09
133500970723 1e  c                   endif
133600970528     c*
133700970528     C                   add       1             pagina
133800970528     C                   WRITE     CNSOL01RID
133900970528     C                   SETOFF                                       99
134000970528     C*
134100000519A0778C     SavPosRig     IFEQ      'T'
134200970528     C*
134300970528     C                   MOVE      *ALL'-'       S04REC
134400970528     C                   WRITE     CNSOL03
134500970528     C                   MOVE      *BLANK        S04REC
134600970528     C*
134700970528     C                   MOVE      'T'           KCCO
134800000626     c                   move      SavLingua     KLIN
134900970528     C*
135000000511     C     KCST          SETLL     NDCST00F                               41
135100000511     c  N41              do
135200000110     C                   MOVE      xsclin        KLIN
135300000511     C     KCST          SETLL     NDCST00F                               41
135400000110     c                   enddo
135500000511     c                   if        *in41=*on
135600000511     c                   setoff                                       40
135700970723 2   C     *IN40         DOWEQ     '0'
135800970528     C     KCST          READE     NDCST00F                               40
135900970723 3   C     *IN40         IFEQ      '0'
136000970528     C                   MOVEL     CSTDCO        S04REC
136100970528     C                   WRITE     CNSOL03
136200970528     C                   MOVE      *BLANK        S04REC
136300000511 3e  C                   endif
136400000511 2e  C                   enddo
136500000511 3e  C                   endif
136600970723 1e  C                   END
136700970528     C*
136800970528     C                   MOVE      *ALL'-'       S04REC
136900970528     C                   WRITE     CNSOL03
137000970528     C                   MOVE      *BLANK        S04REC
137100970528     C*
137200970528     C                   ENDSR
137300970528?     *----------------------------------------------------
137400970528?     * S C A D E  - STAMPA PROSSIME SCADENZE
137500970528?     *----------------------------------------------------
137600970528     C     SCADE         BEGSR
137700970528      *
137800970528     C                   SETON                                        99
137900970528     C                   MOVE      *OFF          WRKIN01           1
138000970528      *
138100970528     C     KSCL00        SETLL     NDSCL01J
138200991025 b1  C     *IN77         DOWEQ     '0'
138300970528     C     KSCL00        READE     NDSCL01J                               77
138400991025 b2  C                   IF        *in77 = *off
138500991012     C* Se richiesto ordinamento per agente, considero solo le partite
138600991012     c* dell'agente
138700050518D1811x**                 if        (l52ord = '2' and EpaSap = piwke1 and
138800050518  "  x**                            piwke1 <> *blank) or
138900050518D1811c                   if        (l52ord = '2' and EpaSap = piwke1)or
139000991012     c* se chiesto ordinamento per cliente, le considero tutte
139100991012     c                             l52ord = '1'
139200991012     c
139300970528      * Testo se si tratta di un effetto a rischio
139400970528     C                   move      sclcsi        l68csi
139500970528     C                   move      scldva        l68dva
139600970528     C                   move      sclkcc        l68kcc
139700970528     C                   move      scldsc        l68dsc
139800970528     C                   move      scltpg        l68tpg
139900970528     C                   move      scltif        l68tif
140000970528     C                   move      scltip        l68tip
140100970528     C                   move      sclseg        l68seg
140200970528     C                   move      sclcau        l68cau
140300970528     C                   CALL      'NDCL68R'
140400970528     C                   PARM                    ndcl68ds
140500040709     C                   if        L68flg='R'
140600040709D1639c                             and %subst(sclflg:8:1)<>'E'
140700040709     C                             or L68FLG ='A'
140800970528      *
1409009705284    C*    SCLSEG        IFEQ      'D'
141000010124D0830C***  SCLANN        IFEQ      *BLANK
141100010124  "  C***  EPAANN        ANDEQ     *BLANK
141200010124  "  c* testo il flag di annullato a seconda di quanto richiesto
141300010124  "  c* solo le valide
141400010124  "  C     SCLANN        IFEQ      *blank
141500010124  "  c     l52ann        andeq     'N'
141600010124  "  c* tutte
141700010124D0830c     l52ann        oreq      'S'
141800970528      *
141900970528     C*Valorizzo la schiera
142000970528     C                   Exsr      ValSchie2
142100970528      *
142200970528 4-  C                   END
142300970528 3-  C                   endif
142400970528      *
142500970528 x2  C                   ELSE
142600991025 e3  C                   ENDif
142700970528     C     WRKIN01       IFEQ      *ON
142800970528     C                   MOVE      *BLANK        S04REC
142900970528     C                   MOVE      *ALL'-'       REC1
143000970528     C                   MOVEL     REC1          S04REC
143100970528     C                   WRITE     CNSOL07
143200970528     C                   ENDIF
143300991025 e2  C                   ENDif
143400991025 e1  C                   ENDdo
143500970528     C*
143600970528     C*Leggo la schiera e scrivo
143700970528     C                   Exsr      ReadSchie2
143800970528     C*
143900970528     C                   ENDSR
144000970528     C*
144100970528     C************************************************************
144200970528     C* Valorizzo schiere per prossime scadenze
144300970528     C************************************************************
144400970528     C     ValSchie2     Begsr
144500970528     C*
144600970528     C*Compongo la chiave
144700970528     C*
144800970528     C                   Clear                   DtScadenza
144900970528     C                   eval      DtScadenza  = SclDsc
145000970528     C                   z-add     SclSysEff     SysEff
145100970528     C                   z-add     SclNrAsEff    NrAsEff
145200970528     C                   z-add     SclNrRigaM    NrRigaM
145300970528     C*
145400970528     C*Compongo i dati
145500970528     C*
145600970528     C                   Clear                   Divisa
145700970528     C                   Clear                   FlgL68
145800970528     C                   Clear                   Segno
145900040331D1571X***                Clear                   DtPartita
146000040331D1571C                   EVAL      DtPartita  = '0001-01-01'
146100970528     C                   Clear                   Causale
146200970528     C*
146300970528     C                   eval      Divisa      = EpaDiv
146400970528     C                   z-add     SclNrP        NrPartita
146500970528     C                   eval      DtPartita   = SclDrP
146600970528     C                   eval      Causale     = SclCau
146700970528     C                   eval      FlgL68      = L68flg
146800970528     C     Epadiv        Comp      xscdiv                                 19
146900970528     C   19              z-add     SclImD        Importo
147000040505D1594C   19              z-add     0             ImportoD
147100970528     C  n19              z-add     SclImD        ImportoD
147200040505D1594C  n19              z-add     0             Importo
147300970528     C
147400970528     C*
147500970528     C*Valorizzo la schiera
147600970528     C*
147700970528     C* NrAsEff = 0
147800970528     C                   If        SclNrAsEff  = 0
147900970528     C                   add       1             J
148000970528     C                   Movea     ChiaveDS      $Chiave(J)
148100970528     C                   Movea     DatiDS        $Dati(J)
148200970528     C*
148300970528     C* NrAsEff <> 0: verifico se i dati sono già nella schiera
148400970528     C*
148500970528     C                   Else
148600970528     C*
148700970528     C                   Z-ADD     1             Z
148800970528     C     Chiaveds      LOOKUP    $Chiave(Z)                             23
148900970528     C                   If        *in23  = *off
149000970528     C* Non trovati
149100970528     C                   add       1             J
149200970528     C                   Movea     ChiaveDS      $Chiave(J)
149300970528     C                   Movea     DatiDS        $Dati(J)
149400970528     C*
149500970528     C                   Else
149600970528     C* Trovati
149700970528     C                   Movea     $Dati(Z)      DatiDs
149800970528     C*Pulisco i riferimenti di partita
149900970528     C                   Clear                   NrPartita
150000040331D1571X***                Clear                   DtPartita
150100040331D1571C                   eval      DtPartita = '0001-01-01'
150200970528     C*Lascio la causale precedentemente inserita
150300970528     C***********                                Causale
150400970528     C*Sommo i nuovi importi
150500970528     C   19              Add       SclImD        Importo
150600970528     C  n19              Add       SclImD        ImportoD
150700970528     C
150800970528     C*
150900970528     C                   Movea     DatiDS        $Dati(Z)
151000970528     C*
151100970528     C                   Endif
151200970528     C                   Endif
151300970528     C*
151400970528     C                   ENDSR
151500970528     C*
151600970528     C************************************************************
151700970528     C* Leggo schiere per prossime scadenze
151800970528     C************************************************************
151900970528     C     Readschie2    Begsr
152000991105     C                   move      *off          primavolta        1
152100970528     C                   Z-ADD     1             J
152200000128     C     1             DO        5000
152300970528     C* Se la schiera è valorizzata
152400970528     C                   If        $Chiave(J) <>*blank
152500970528     C                   Movea     $Dati(J)      DatiDs
152600970528     C                   Movea     $Chiave(J)    ChiaveDs
152700970528     C
152800970528     C* valorizzo il CnSol08
152900970528     C                   clear                   s07Dpa
153000970528     C                   clear                   s07AttEs
153100970528     C                   clear                   opedesbrev
153200970528     C                   exsr      readanope
153300970528     C                   eval      s07dpa   =    opedesbrev
153400970528     C     FlgL68        IFEQ      'R'
153500970528     C                   Eval      S07AttEs =    xscon
153600970528     C                   ENDIF
153700970528      *
153800970528     C                   clear                   S07DRP
153900970528     C                   clear                   S07dsc
154000970528     C                   clear                   S07nrp
154100970528     C                   If        DtPartita <> DATMIN and DtPartita <>*blank
154200970528     C                   CALLb     'XDT4000'
154300970528     C                   PARM      DtPartita     xdtamg           10
154400970528     C     s07drp        PARM                    xdtgma            6 0
154500970528     C                   PARM      2             xdtsta            1 0
154600970528     C                   Z-ADD     NrPartita     S07NRP
154700970528     C                   endif
154800970528     C*
154900970528     C     DtScadenza    ifne      DATMIN
155000970528     C                   seton                                          22
155100970528     C                   CALLb     'XDT4000'
155200970528     C                   PARM      DtScadenza    xdtamg           10
155300970528     C     s07dsc        PARM                    xdtgma            6 0
155400970528     C                   PARM      2             xdtsta            1 0
155500970528     C                   endif
155600970528     C*
155700040331D1571x****               Eval      StdIvi      = Divisa
155800040331  "  C                   If        X0201RpDVS(Divisa   : DtPartita:
155900040331  "  C                                       ANDVS00F: %size(ANDVS00F):
156000040331  "  C                                       ANDVX00F: %size(ANDVX00F)) <> 1
156100040331  "  C                   eval      StDivi   =    DvsCdInter
156200040331D1571C                   EndIf
156300970528     C*
156400970528     C* Verifica  la divisa
156500970528     C     Divisa        COMP      xscdiv                                 19
156600970528     C*
156700970528     C                   Clear                   S07Cnt
156800970528     C                   Clear                   S07Sal
156900970528     C
157000970528     C   19              Z-ADD     Importo       S07Cnt
157100970528     C  N19              Z-ADD     ImportoD      S07Sal
157200970528     c*
157300970528     C***--------------------------------------------------***
157400970528     C* Salto pagina PER OF
1575009705285    C     *IN99         IFEQ      '1'
157600970528     C*
157700991105     C* STAMPA TESTO solo la prima volta
157800991105     c                   if        Primavolta = *off
157900000519A0778C                   MOVE      'X'           SavPosRig
158000970528     C                   EXSR      CORPO
158100991105     c                   move      *on           Primavolta
158200991105     c                   else
158300991105     c* stampo solo la pagina
158400991105     c* testo se è stato richiesto un formato lettera o un formato PT_Postel
158500991105     c* e stampo di conseguenza il tracciato relativo
158600991105     c                   if        L52Out = '1'
158700991105     c* allora stampo in formato PT_Postel
158800991105     C                   WRITE     CnSol09Pt
158900991105     c                   else
159000991105     c* altrimenti stampo in formato lettera
159100991105     C                   WRITE     CnSol09
159200991105     c                   endif
159300991105     c*
159400991105     C                   add       1             pagina
159500991105     C                   WRITE     CNSOL01RID
159600991105     C                   SETOFF                                       99
159700991105     c                   endif
159800991105      * stampa trattini
159900970528     C                   MOVE      *BLANK        S04REC
160000970528     C                   MOVE      *ALL'-'       REC1             80
160100970528     C                   MOVEL     REC1          S04REC
160200970528     C                   WRITE     CNSOL07
160300970528      * STAMPA COSTANTI
160400970528     C                   MOVE      'S'           KCCO
160500000626     C                   MOVE      Savlingua     KLIN
160600000511     C     KCST          SETLL     NDCST00F                               41
160700000511     c  N41              do
160800000110     C                   MOVE      xsclin        KLIN
160900000511     C     KCST          SETLL     NDCST00F                               41
161000000110     c                   enddo
161100000511     c                   if        *in41=*on
161200000511     c                   setoff                                       40
161300970528 7   C     *IN40         DOWEQ     '0'
161400970528     C     KCST          READE     NDCST00F                               40
161500970528 8   C     *IN40         IFEQ      '0'
161600970528     C                   MOVEL     CSTDCO        S04REC
161700970528     C                   WRITE     CNSOL07
161800000511 8-  C                   endif
161900000511 7-  C                   enddo
162000000511 8-  C                   endif
162100970528     C*
162200970528     C                   MOVE      *BLANK        S04REC
162300970528     C                   MOVE      *ALL'-'       REC1
162400970528     C                   MOVEL     REC1          S04REC
162500970528     C                   WRITE     CNSOL07
162600970528     C                   MOVE      *BLANK        S04REC
162700970528     C*                  WRITE     CNSOL07
162800970528     C*
162900970528     C                   SETOFF                                       99
163000970528     C*
1631009705285-   C                   END
163200970528     C***-----------------------------------------------***
163300970528     C*
163400970528     C                   WRITE     CNSOL08
163500970528     C                   MOVE      *ON           WRKIN01
163600970528     C*
163700970528     C*
163800970528     C*Incremento il contatore della schiera
163900970528     C                   Add       1             J
164000970528     C*
164100970528     C                   else
164200970528     C*
164300970528     C                   Leave
164400970528     C*
164500970528     C                   Endif
164600970528     C*
164700970528     C                   EndDo
164800970528     C*
164900970528     C                   Endsr
165000030311     c************************************************************
165100030311     c* REPERIMENTO DATI da xpar
165200030311     c************************************************************
165300030311     c     Readxpar      BEGSR
165400030311     c*
165500030311     c                   movel     'NDCL1S'      xpapar
165600030311     c*
165700030311     c                   movel     *blank        xparic
165800030311     c                   CALLb     'XPAR'
165900030311     c                   PARM                    xscsoc
166000030311     c                   PARM                    XPAPAR            8
166100030311     c                   PARM                    XPAOUT
166200030311     c                   PARM                    XPAERR            1
166300030311     c                   PARM                    XPARIC            1
166400030311     c* se record allocato va a fine
166500030311     c     xpaerr        ifeq      '2'
166600030311     c                   goto      fine
166700030311     c                   endif
166800030311     c                   movel     xpaout        anpdati
166900030311     c                   movel     anpdati       anp057ds
167000030311     c*
167100030311     c                   ENDSR
167200970528?     *------------------------------------------------------------
167300970528?     *  ROUTINE DI APERTURA PROGRAMMA
167400970528?     *------------------------------------------------------------
167500970528     C     *INZSR        BEGSR
167600000121C1319C     *DMY          Move      Udate         ZDateISO
167700000121  "  C     *JobRun       Move      ZDateISO      ZDate             6 0
167800000121  "  C* reperisco DATFMT del job
167900000121  "  C                   CallB     'XDATFMT'
168000000121C1319C                   Parm                    $DatFmt           4
168100040331     c*
168200970528      *
168300970528     C     *ENTRY        PLIST
168400970528     C                   PARM                    KPJBA
168500060712c1950c                   PARM                    NDCL55DS
168600060712c1950x***                PARM                    PARAM
168700970528     C                   MOVEL     KPJBU         NDCL52DS
168800970528     c*
168900970528     c*
169000970528      * PLIST per NDVASOG
169100970528     C     PDVASOG       PLIST
169200970528     C                   PARM                    DVARIC
169300970528     C                   PARM                    DVASOCIETA
169400970528     C                   PARM                    DVAUNITA
169500970528     C                   PARM                    DVASTRUTT
169600970528     C                   PARM                    DVADTRIF
169700970528     C                   PARM                    DVALENOUT
169800990222     C                   PARM                    DVASNATURA
169900990222     C                   PARM                    DVACODICE
170000970528     C                   PARM                    DVALINEAV
170100970528     C                   PARM                    DVAFILIALE
170200990222     C                   PARM                    *omit
170300990222     C                   PARM                    *omit
170400990222     C                   PARM                    *omit
170500970528     C                   PARM                    DVAERRORE
170600970528     C                   PARM                    DVAOUTPUT
170700990222      * PLIST per NDVASOG con sogg.+tipo/cod.indirizzo
170800990222     C     PDVASOG1      PLIST
170900990222     C                   PARM                    DVARIC
171000990222     C                   PARM                    DVASOCIETA
171100990222     C                   PARM                    DVAUNITA
171200990222     C                   PARM                    DVASTRUTT
171300990222     C                   PARM                    DVADTRIF
171400990222     C                   PARM                    DVALENOUT
171500990222     C                   PARM                    DVASNATURA
171600990222     C                   PARM                    DVACODICE
171700990222     C                   PARM                    DVALINEAV
171800990222     C                   PARM                    DVAFILIALE
171900990222c1001C                   PARM                    DVASogg
172000990222c1001C                   PARM                    DVATpind
172100990222c1001C                   PARM                    DVACdind
172200990222     C                   PARM                    DVAERRORE
172300990222     C                   PARM                    DVAOUTPUT
172400970528     C*
172500970528     C* Controlla validità installazione
172600970528     c* chiamata a xsoc
172700970528    >C                   MOVEL     'SOC001'      TIPXSC
172800970528     C                   MOVEL     L52csi        SOCXSC
172900970528     C                   CALLb     'XSOC'
173000970528     C                   PARM                    TIPXSC            6
173100970528     C                   PARM                    SOCXSC            3
173200970528     C                   PARM                    CDSXSC            9 0
173300970528     C                   PARM                    MODXSC            3
173400970528     C                   PARM      *blanks       RTNXSC            1
173500970528     C                   PARM                    XSOCDS
173600970528     C                   PARM                    KPJBA
173700970528     C     RTNXSC        IFNE      '1'
173800970528    >C                   MOVEL     XSOCDS        SOC001
173900970528     C                   ELSE
174000970528     C                   return
174100970528     C                   seton                                        LR
174200970528     C                   ENDIF
174300040331     c*
174400040331D1571c                   move      ZDateiso      $udate10
174500040331  "  C*  reperisco codice divisa internazionale della m.c.
174600040331  "  C                   If        X0201RpDVS(Xscdiv   : $udate10:
174700040331  "  C                                       ANDVS00F: %size(ANDVS00F):
174800040331  "  C                                       ANDVX00F: %size(ANDVX00F)) <> 1
174900040331  "  C                   eval      XscDivInt = DvsCdInter
175000040331D1571C                   EndIf
175100970528     C*
175200970528     C***************************************
175300970528     C*       elenco KLIST usate            *
175400970528     C***************************************
175500970528     C*
175600970528     C*  NDCST00F
175700970528     C     KCST          KLIST
175800970528     C                   KFLD                    KLIN
175900970528     C                   KFLD                    KCCO
176000970528     C                   KFLD                    KTIP
176100970528     C                   MOVE      '2'           KTIP
176200030311c1632C*  NDCST00F
176300030311 "   C     KCST1         KLIST
176400030311 "   C                   KFLD                    KLIN
176500030311 "   C                   KFLD                    KCCO
176600030311 "   C                   KFLD                    KTIP
176700030311 "   C                   KFLD                    KPRG
176800030311C1632C                   Z-ADD     9             KPRG              1 0
176900970528     C*
177000970528     C*  NDSCL01L
177100970528     C     KSCL00        KLIST
177200970528     C                   KFLD                    KCSI
177300970528     C                   KFLD                    KDVA
177400970528     C                   KFLD                    KKCC
177500970528     C                   KFLD                    KKSC
177600970528     C*
177700970528     C     KSCLPA        KLIST
177800970528     C                   KFLD                    KCSI
177900970528     C                   KFLD                    KDVA
178000970528     C                   KFLD                    KKCC
178100970528     C                   KFLD                    KKSC
178200970528     C                   KFLD                    KDRP
178300970528     C                   KFLD                    KNRP
178400970528     C                   KFLD                    KSERPAR
178500970528     C*
178600970528     C     KSCL01        KLIST
178700970528     C                   KFLD                    KCSI
178800970528     C                   KFLD                    KDVA
178900970528     C                   KFLD                    KKCC
179000970528     C                   KFLD                    KKSC
179100970528     C                   KFLD                    KDSC
179200970528      *
179300970528      *  NDPIW01L
179400970528      *
179500970528     C     k2piw1        KLIST
179600970528     C                   KFLD                    PIWKE1
179700970528     C                   KFLD                    PIWKE2
179800030311     c*
179900970528     C***************************************
180000970528     C* definizione dei campi di lavoro     *
180100970528     C***************************************
180200970528     C     *LIKE         DEFINE    L52CSI        KCSI
180300970528     C     *LIKE         DEFINE    L52DVA        KDVA
180400970528     C     *LIKE         DEFINE    L52KCC        KKCC
180500970528     C     *LIKE         DEFINE    PIWKSC        KKSC
180600970528     C     *LIKE         DEFINE    PIWDRP        KDRP
180700970528     C     *LIKE         DEFINE    PIWNRP        KNRP
180800970528     C     *LIKE         DEFINE    PIWSERPAR     KSERPAR
180900970528     C     *LIKE         DEFINE    L52DFS        KDSC
181000970528     C     *LIKE         DEFINE    CSTLIN        KLIN
181100970528     C     *LIKE         DEFINE    CSTCCO        KCCO
181200970528     C     *LIKE         DEFINE    CSTTIP        KTIP
181300970528     C*
181400970528     C                   MOVEL     L52CSI        KCSI
181500970528     C                   move      L52DVA        KDVA
181600970528     C                   move      L52KCC        KKCC
181700970528     C                   move      L52DFS        KDSC
181800970528      *
181900030311     c* Impostazione dati di base solleciti
182000030311     c                   exsr      readxpar
182100970528     C*
182200970528     C* REPERISCO SEDE AZIENDA
182300970528     C*
182400970528     C                   movea     xsccit        sed
182500970528     C* COMPONGO COSTANTE "LOCALITA, GG/MM/AA" ------------*
182600970528     C                   Z-ADD     34            Y
182700970528     C     Y             DOWGT     0
182800970528     C     SED(Y)        IFNE      *BLANK
182900970528     C                   ADD       1             Y
183000970528     C                   MOVE      ','           SED(Y)
183100000121C1319 * Costruisco la stringa a seconda del formato data del job
183200000121  "  C                   Select
183300000121  "  C* formato mm/gg/aa
183400000121  "  C                   When      $DatFmt = '*MDY'
183500000121  "  c                   move      uday          Dsmm
183600000121  "  c                   move      umonth        Dsgg
183700000121  "  c                   move      uyear         Dsaa
183800000121  "  C* formato aa/mm/gg
183900000121  "  C                   When      $DatFmt = '*YMD'
184000000121  "  c                   move      uday          Dsaa
184100000121  "  c                   move      umonth        Dsmm
184200000121  "  c                   move      uyear         Dsgg
184300000121  "  C* formato gg/mm/aa
184400000121  "  C                   When      $DatFmt = '*DMY'
184500000121  "  c                   move      uday          Dsgg
184600000121  "  c                   move      umonth        Dsmm
184700000121  "  c                   move      uyear         Dsaa
184800000121  "  C                   EndSl
184900000121  "   *
185000000121C1319C*****              Z-ADD     UDAY          DSGG
185100970528     C                   MOVEL     '/'           DSBAR1
185200000121C1319C*****              Z-ADD     UMONTH        DSMM
185300970528     C                   MOVEL     '/'           DSBAR2
185400000121C1319C*****              Z-ADD     UYEAR         DSAA
185500970528     C                   ADD       2             Y
185600970528     C                   MOVEA     DATLET        SED(Y)
185700970528     C                   Z-ADD     0             Y
185800970528     C                   ELSE
185900970528     C                   SUB       1             Y
186000970528     C                   END
186100970528     C                   END
186200970528     C                   MOVEA     SED           S05SED
186300970528     C*
186400970729     C* Apertura prtf
186500970729     C                   Exsr      OpnPrtf
186600030311     c*
186700030311     C                   ENDSR
