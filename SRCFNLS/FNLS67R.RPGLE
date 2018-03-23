000100050805     h decedit('0,') datedit(*dmy/)
000200050805
000300050805      * FNLS67R *-----------------------------------------------------*
000400050808      * CANCELLAZIONE DI FIVAD/FIVAT/FIVAX SENZA IL RELATIVO FIVAB    *
000500050805      *---------------------------------------------------------------*
000600050805
000700050805     fFIVAB04L  if   e           k disk
000800120510     F*---------
000900120510     FTNTBE01L  IF   E           K DISK
001000050805      *
001100050805     fFIVAD01L  Uf   e           k disk
001101171215     fFIVAD12L  Uf   e           k disk
001200050805     fFIVAT01L  Uf   e           k disk
001300050805     fFIVAX01L  Uf   e           k disk
001400081020     fFIrab02l  Uf   e           k disk
001500050805
001600050808      *
001700050805      * STRUTTURE DATI -----------------------------------------------*
001800050808      *
001900050805     d KPJBA         e ds
002000050905     d FNLS67ds        ds                  inz
002100050905     d  LS67ccm                       7s 0 inz
002200050905     d  LS67lnp                       3s 0 inz
002300050905     d  LS67nrs                       2s 0 inz
002400050805      * - Parametri x Controllo profilo utenti
002500050805     d TIBS34ds      e ds
002600050805      * - Ds di riferimento al file esterno AZUTE00F
002700050805     d AZUTEds       e ds                  extname(AZUTE00F)
002800050805      * - Ds per dati organigramma
002900050805     d dDATIUTE      e ds
003000050805      *
003100050808      * - DS per TRUL06R - caricamento £X
003200050805     d TRUL06ds      e ds                  inz
003300050805     d  £1                     1     90  0 inz  dim(30)
003400120424      *
003500120424      * Tabella 3BC = Bypass ctrl x cliente
003600120424     d d3BC          e ds                  inz
003700120510     D CCM             S              7  0 DIM(100)
003800120424      * DS PER TIBS02R - GESTIONE TNTBE00F
003900120424     D DSBS02        E DS                  EXTNAME(TIBS02DS)
004000050808      *
004100050808     d Status         sds
004200050808     d  SDSpgm           *proc
004300050808     d  SDSusr               254    263
004400050805      *
004500050805      * VARIABILI ----------------------------------------------------*
004600050808      *
004700050808     d P_ccm           s                   like(VABccm) inz
004800050805     d Kccm            s                   like(VABccm) inz
004900050805     d Kfgs            s                   like(VABfgs) inz
005000050808      * - Indici di schiera
005100050805     d xx              s              3  0 inz
005200120510     D wY              s              3s 0
005300050808      * - Flags booleani
005400050805     d $Found          s              1    inz(*off)
005500050808      * - Parametri per QCMDEXC
005600050808     d Qcmd            s             80    inz
005700050808     d Qlen            s             15  5 inz(80)
005800050805      *
005900050805      * KEY - LIST ---------------------------------------------------*
006000050808      *
006100050805     c     K02VADTX01    klist
006200050805     c                   kfld                    £1(xx)
006300050905     c                   kfld                    LS67ccm
006400050805      *
006500050805     c     K04VAB04      klist
006600050805     c                   kfld                    VABaas
006700050805     c                   kfld                    VABlnp
006800050805     c                   kfld                    VABnrs
006900050805     c                   kfld                    VABnsp
007000050805
007100050805      *---------------------------------------------------------------*
007200050805      *
007300050805     c                   if        %subst(KPJBU:1:7) <= *zeros
007400050905     C                              and
007500050905     c                             %subst(KPJBU:8:5) <= *zeros
007600050905     c                   reset                   FNLS67ds
007700050805     c                   else
007800050905     c                   movel     KPJBU         FNLS67ds
007900050805     c                   exsr      OpeIni
008000050805     c                   endif
008100050808      *
008200050905if  1c                   IF            LS67ccm  = *zeros
008300050905     c                             and LS67lnp  = *zeros
008400050905     c                             and LS67nrs  = *zeros
008500050808      *
008600050808      * Lettura "globale"
008601171215      * - Elaborazione FIVAD12L
008602171215     c                   exsr      ElabFIVAD1
008700050808      * - Elaborazione FIVAD01L
008800050808     c                   exsr      ElabFIVAD
008900050808      * - Elaborazione FIVAT01L
009000050808     c                   exsr      ElabFIVAT
009100050808      * - Elaborazione FIVAX01L
009200050808     c                   exsr      ElabFIVAX
009300081017      * - Elaborazione FIRAB
009400081017     c                   exsr      ElabFIRAB
009500050808      *
009600050808x   1c                   ELSE
009700050808      *
009800050808      * Ricerca in tutti questi archivi dei record con
009900050808      * · con P.O. Gestione in "£1"   e
010000050905      *   · cod. cliente mittente = parametro CCM   o
010100050905      *   · P.O. partenza & Serie = parametri LNP & NRS
010200050808     c                   eval      xx     = 1
010300050808do  2c                   dow           xx     <= %elem(£1)
010400050808     c                             and £1(xx) >  *zeros
010401180130      * - Elaborazione FIVAD12L
010402180130     c                   exsr      ElabFIVAD1
010500050808      * - Elaborazione FIVAD01L
010600050808     c                   exsr      ElabFIVAD
010700050808      * - Elaborazione FIVAT01L
010800050808     c                   exsr      ElabFIVAT
010900050808      * - Elaborazione FIVAX01L
011000050808     c                   exsr      ElabFIVAX
011100081020      * - Elaborazione FIRAB
011200081020     c                   exsr      ElabFIRAB
011300050808     c                   add       1             xx
011400050808e   2c                   enddo
011500050808      *
011600050808     c                   ENDIF
011700050805      *
011800050808      * Fine
011900050805     c                   eval      *inLR  = *on
012000050805      *
012100050805      *---------------------------------------------------------------*
012200050805      * Operazioni Iniziali                                           *
012300050805      *---------------------------------------------------------------*
012400050805     c     OpeIni        BEGSR
012500050805      *
012600050805      * Reperisco nome azienda per stampa PREPAGATI
012700050805     c     *dtaara       define    §azute        AZUTEds
012800050805     c     *dtaara       define    §datiute      dDATIUTE
012900050805     c                   in(E)     *dtaara
013000050805     c                   if        %ERROR or RSUT = *blanks
013100050805     c                   clear                   Tibs34Ds
013200050805     c                   call      'TIBS34R'
013300050805     c                   parm                    Tibs34Ds
013400050805     c                   in        *dtaara
013500050805     c                   endif
013600050805      *
013700050805      * Carico tab. P.O. gestiti (£1)
013800050805     c                   clear                   TRUL06ds
013900050805     c                   movel     '£1'          D06cod
014000050805     c                   movel     SIMFEL        D06key
014100050805     c                   movel     'L'           D06tla
014200050805     c                   movel     TRUL06ds      KPJBU
014300050805     c                   call      'TRUL06R'
014400050805     c                   parm                    KPJBA
014500050805     c                   movel     KPJBU         TRUL06ds
014600050805      *
014700050805     c                   ENDSR
014701171215      *---------------------------------------------------------------*
014702171215      * Elaborazione file FIVAD12L                                    *
014703171215      *---------------------------------------------------------------*
014704171215     c     ElabFIVAD1    BEGSR
014705171215      *
014706171215sel 1c                   select
014707171215      * Lettura "globale"
014708171215w   1c                   when          LS67ccm  = *zeros
014709171215     c                             and LS67lnp  = *zeros
014710171215     c                             and LS67nrs  = *zeros
014711171215     c     *loval        setll     FIVAD100
014712171215     c                   read      FIVAD100
014713171215      * Lettura per FGS e CCM
014714171215w   1c                   when      LS67ccm  > *zeros
014715171215     c     K02VADTX01    setll     FIVAD100
014716171215     c     K02VADTX01    reade     FIVAD100
014717171215      * Lettura per FGS e LNP / NRS
014718171215w   1c                   when      LS67lnp  > *zeros
014719171215     c     £1(xx)        setll     FIVAD100
014720171215     c     £1(Xx)        reade     FIVAD100
014721171215e   1c                   endsl
014722171215      *
014723171215      * Elaborazione
014724171215do  1c                   DOW       NOT %eof(FIVAD12L)
014725171215      *
014726171215if  2c                   if            LS67lnp  = *zeros
014727171215     c                             or (VADlnp   = LS67lnp
014728171215     c                             and VADnrs   = LS67nrs)
014729171215      *
014730171215     c                   eval      VABaas   = VADaas
014731171215     c                   eval      VABlnp   = VADlnp
014732171215     c                   eval      VABnrs   = VADnrs
014733171215     c                   eval      VABnsp   = VADnsp
014734171215     c                   eval      Kfgs     = VADfgs
014735171215     c                   eval      Kccm     = VADccm
014736171215     C*
014737171215     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
014738171215     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
014739171215     C                   IF        %lookup(KCCM : CCM) = 0
014740171215     c                   exsr      CercaFIVAB
014741171215if  3c                   if        $Found   = *off
014742171215     c                   DELETE    FIVAD100
014743171215e   3c                   endif
014744171215     C                   ENDIF
014745171215      *
014746171215e   2c                   endif
014747171215      *
014748171215sel 2c                   select
014749171215      * Lettura "globale"
014750171215w   2c                   when          LS67ccm  = *zeros
014751171215     c                             and LS67lnp  = *zeros
014752171215     c                             and LS67nrs  = *zeros
014753171215     c                   read      FIVAD100
014754171215      * Lettura per FGS e CCM
014755171215w   2c                   when      LS67ccm  > *zeros
014756171215     c     K02VADTX01    reade     FIVAD100
014757171215      * Lettura per FGS e LNP / NRS
014758171215w   2c                   when      LS67lnp  > *zeros
014759171215     c     £1(xx)        reade     FIVAD100
014760171215e   2c                   endsl
014761171215      *
014762171215e   1c                   ENDDO
014763171215      *
014764171215     c                   ENDSR
014800050805      *
014900050805      *---------------------------------------------------------------*
015000050805      * Elaborazione file FIVAD01L                                    *
015100050805      *---------------------------------------------------------------*
015200050808     c     ElabFIVAD     BEGSR
015300050808      *
015400050905sel 1c                   select
015500050808      * Lettura "globale"
015600050905w   1c                   when          LS67ccm  = *zeros
015700050905     c                             and LS67lnp  = *zeros
015800050905     c                             and LS67nrs  = *zeros
015900050808     c     *loval        setll     FIVAD000
016000050808     c                   read      FIVAD000
016100050808      * Lettura per FGS e CCM
016200050905w   1c                   when      LS67ccm  > *zeros
016300050805     c     K02VADTX01    setll     FIVAD000
016400050808     c     K02VADTX01    reade     FIVAD000
016500050905      * Lettura per FGS e LNP / NRS
016600050905w   1c                   when      LS67lnp  > *zeros
016700050905     c     £1(xx)        setll     FIVAD000
016800050905     c     £1(Xx)        reade     FIVAD000
016900050905e   1c                   endsl
017000050805      *
017100050805      * Elaborazione
017200050805do  1c                   DOW       NOT %eof(FIVAD01L)
017300050805      *
017400050905if  2c                   if            LS67lnp  = *zeros
017500050905     c                             or (VADlnp   = LS67lnp
017600050905     c                             and VADnrs   = LS67nrs)
017700050905      *
017800050905     c                   eval      VABaas   = VADaas
017900050905     c                   eval      VABlnp   = VADlnp
018000050905     c                   eval      VABnrs   = VADnrs
018100050905     c                   eval      VABnsp   = VADnsp
018200050905     c                   eval      Kfgs     = VADfgs
018300050905     c                   eval      Kccm     = VADccm
018400120510     C*
018500120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
018600120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
018700120510     C                   IF        %lookup(KCCM : CCM) = 0
018800050805     c                   exsr      CercaFIVAB
018900050905if  3c                   if        $Found   = *off
019000050805     c                   DELETE    FIVAD000
019100050905e   3c                   endif
019200120510     C                   ENDIF
019300050905      *
019400050905e   2c                   endif
019500050805      *
019600050905sel 2c                   select
019700050905      * Lettura "globale"
019800050905w   2c                   when          LS67ccm  = *zeros
019900050905     c                             and LS67lnp  = *zeros
020000050905     c                             and LS67nrs  = *zeros
020100050905     c                   read      FIVAD000
020200050905      * Lettura per FGS e CCM
020300050905w   2c                   when      LS67ccm  > *zeros
020400050905     c     K02VADTX01    reade     FIVAD000
020500050905      * Lettura per FGS e LNP / NRS
020600050905w   2c                   when      LS67lnp  > *zeros
020700050905     c     £1(xx)        reade     FIVAD000
020800050905e   2c                   endsl
020900050805      *
021000050805e   1c                   ENDDO
021100050805      *
021200050805     c                   ENDSR
021300050805      *
021400050805      *---------------------------------------------------------------*
021500050805      * Elaborazione file FIVAT01L                                    *
021600050805      *---------------------------------------------------------------*
021700050805     c     ElabFIVAT     BEGSR
021800050805      *
021900050905sel 1c                   select
022000050808      * Lettura "globale"
022100050905w   1c                   when          LS67ccm  = *zeros
022200050905     c                             and LS67lnp  = *zeros
022300050905     c                             and LS67nrs  = *zeros
022400050808     c     *loval        setll     FIVAT000
022500050808     c                   read      FIVAT000
022600050808      * Lettura per FGS e CCM
022700050905w   1c                   when      LS67ccm  > *zeros
022800050808     c     K02VADTX01    setll     FIVAT000
022900050808     c     K02VADTX01    reade     FIVAT000
023000050905      * Lettura per FGS e LNP / NRS
023100050905w   1c                   when      LS67lnp  > *zeros
023200050905     c     £1(xx)        setll     FIVAT000
023300050905     c     £1(xx)        reade     FIVAT000
023400050905e   1c                   endsl
023500050805      *
023600050805      * Elaborazione
023700050805do  1c                   DOW       NOT %eof(FIVAT01L)
023800050905      *
023900050905if  2c                   if            LS67lnp  = *zeros
024000050905     c                             or (VATlnp   = LS67lnp
024100050905     c                             and VATnrs   = LS67nrs)
024200050805      *
024300050905     c                   eval      VABaas   = VATaas
024400050905     c                   eval      VABlnp   = VATlnp
024500050905     c                   eval      VABnrs   = VATnrs
024600050905     c                   eval      VABnsp   = VATnsp
024700050905     c                   eval      Kfgs     = VATfgs
024800050905     c                   eval      Kccm     = VATccm
024900120510     C*
025000120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
025100120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
025200120510     C                   IF        %lookup(KCCM : CCM) = 0
025300050805     c                   exsr      CercaFIVAB
025400050905if  3c                   if        $Found   = *off
025500050805     c                   DELETE    FIVAT000
025600050905e   3c                   endif
025700120510     C                   ENDIF
025800050905      *
025900050905e   2c                   endif
026000050805      *
026100050905sel 2c                   select
026200050905      * Lettura "globale"
026300050905w   2c                   when          LS67ccm  = *zeros
026400050905     c                             and LS67lnp  = *zeros
026500050905     c                             and LS67nrs  = *zeros
026600050808     c                   read      FIVAT000
026700050905      * Lettura per FGS e CCM
026800050905w   2c                   when      LS67ccm  > *zeros
026900050808     c     K02VADTX01    reade     FIVAT000
027000050905      * Lettura per FGS e LNP / NRS
027100050905w   2c                   when      LS67lnp  > *zeros
027200050905     c     £1(xx)        reade     FIVAT000
027300050905e   2c                   endsl
027400050805      *
027500050805e   1c                   ENDDO
027600050805      *
027700050805     c                   ENDSR
027800050805      *
027900050805      *---------------------------------------------------------------*
028000050805      * Elaborazione file FIVAX01L                                    *
028100050805      *---------------------------------------------------------------*
028200050805     c     ElabFIVAX     BEGSR
028300050805      *
028400050905sel 1c                   select
028500050808      * Lettura "globale"
028600050905w   1c                   when          LS67ccm  = *zeros
028700050905     c                             and LS67lnp  = *zeros
028800050905     c                             and LS67nrs  = *zeros
028900050808     c     *loval        setll     FIVAX000
029000050808     c                   read      FIVAX000
029100050808      * Lettura per FGS e CCM
029200050905w   1c                   when      LS67ccm  > *zeros
029300050808     c     K02VADTX01    setll     FIVAX000
029400050808     c     K02VADTX01    reade     FIVAX000
029500050905      * Lettura per FGS e LNP / NRS
029600050905w   1c                   when      LS67lnp  > *zeros
029700050905     c     £1(xx)        setll     FIVAX000
029800050905     c     £1(xx)        reade     FIVAX000
029900050905e   1c                   endsl
030000050805      *
030100050805      * Elaborazione
030200050805do  1c                   DOW       NOT %eof(FIVAX01L)
030300050905      *
030400050905if  2c                   if            LS67lnp  = *zeros
030500050905     c                             or (VAXlnp   = LS67lnp
030600050905     c                             and VAXnrs   = LS67nrs)
030700050805      *
030800050905     c                   eval      VABaas   = VAXaas
030900050905     c                   eval      VABlnp   = VAXlnp
031000050905     c                   eval      VABnrs   = VAXnrs
031100050905     c                   eval      VABnsp   = VAXnsp
031200050905     c                   eval      Kfgs     = VAXfgs
031300050905     c                   eval      Kccm     = VAXccm
031400120510     C*
031500120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
031600120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
031700120510     C                   IF        %lookup(KCCM : CCM) = 0
031800050805     c                   exsr      CercaFIVAB
031900050905if  3c                   if        $Found   = *off
032000050805     c                   DELETE    FIVAX000
032100050905e   3c                   endif
032200120510     C                   ENDIF
032300050905      *
032400050905e   2c                   endif
032500050805      *
032600050905sel 2c                   select
032700050905      * Lettura "globale"
032800050905w   2c                   when          LS67ccm  = *zeros
032900050905     c                             and LS67lnp  = *zeros
033000050905     c                             and LS67nrs  = *zeros
033100050808     c                   read      FIVAX000
033200050905      * Lettura per FGS e CCM
033300050905w   2c                   when      LS67ccm  > *zeros
033400050808     c     K02VADTX01    reade     FIVAX000
033500050905      * Lettura per FGS e LNP / NRS
033600050905w   2c                   when      LS67lnp  > *zeros
033700050905     c     £1(xx)        reade     FIVAX000
033800050905e   2c                   endsl
033900050805      *
034000050805e   1c                   ENDDO
034100050805      *
034200050805     c                   ENDSR
034300081020      *---------------------------------------------------------------*
034400081020      * Elaborazione file FIRAB02L                                    *
034500081020      *---------------------------------------------------------------*
034600081020     c     ElabFIRAB     BEGSR
034700081020      *
034800081020sel 1c                   select
034900081020      * Lettura "globale"
035000081020w   1c                   when          LS67ccm  = *zeros
035100081020     c                             and LS67lnp  = *zeros
035200081020     c                             and LS67nrs  = *zeros
035300081020     c     *loval        setll     FIRAB000
035400081020     c                   read      FIRAB000
035500081020      * Lettura per FGS e CCM
035600081020w   1c                   when      LS67ccm  > *zeros
035700081020     c     K02VADTX01    setll     FIRAB000
035800081020     c     K02VADTX01    reade     FIRAB000
035900081020      * Lettura per FGS e LNP / NRS
036000081020w   1c                   when      LS67lnp  > *zeros
036100081020     c     £1(xx)        setll     FIRAB000
036200081020     c     £1(Xx)        reade     FIRAB000
036300081020e   1c                   endsl
036400081020      *
036500081020      * Elaborazione
036600081020do  1c                   DOW       NOT %eof(FIRAB02L)
036700081020      *
036800081020if  2c                   if            LS67lnp  = *zeros
036900081020     c                             or (RABlnp   = LS67lnp
037000081020     c                             and RABnrs   = LS67nrs)
037100081020      *
037200081020     c                   eval      VABaas   = RABaas
037300081020     c                   eval      VABlnp   = RABlnp
037400081020     c                   eval      VABnrs   = RABnrs
037500081020     c                   eval      VABnsp   = RABnsp
037600081020     c                   eval      Kfgs     = RABfgs
037700081020     c                   eval      Kccm     = RABccm
037800120510     C*
037900120510     C* controllo se esiste un bypass su questo cliente (solo NON se c'è cerco i
038000120510     C*  dati di testata, altrimenti non serve perché comunque NON cancellerei)
038100120510     C                   IF        %lookup(KCCM : CCM) = 0
038200081020     c                   exsr      CercaFIVAB
038300081020if  3c                   if        $Found   = *off
038400081020     c                   DELETE    FIRAB000
038500081020e   3c                   endif
038600120510     C                   ENDIF
038700081020      *
038800081020e   2c                   endif
038900081020      *
039000081020sel 2c                   select
039100081020      * Lettura "globale"
039200081020w   2c                   when          LS67ccm  = *zeros
039300081020     c                             and LS67lnp  = *zeros
039400081020     c                             and LS67nrs  = *zeros
039500081020     c                   read      FIRAB000
039600081020      * Lettura per FGS e CCM
039700081020w   2c                   when      LS67ccm  > *zeros
039800081020     c     K02VADTX01    reade     FIRAB000
039900081020      * Lettura per FGS e LNP / NRS
040000081020w   2c                   when      LS67lnp  > *zeros
040100081020     c     £1(xx)        reade     FIRAB000
040200081020e   2c                   endsl
040300081020      *
040400081020e   1c                   ENDDO
040500081020      *
040600081020     c                   ENDSR
040700050808      *
040800050808      *---------------------------------------------------------------*
040900050808      * Ricerca record corrispondenti in FIVAB04L                     *
041000050808      *---------------------------------------------------------------*
041100050808     c     CercaFIVAB    BEGSR
041200050808      *
041300050808     c                   eval      $Found = *off
041400050808      *
041500050808     c     K04VAB04      setll     FIVAB000
041600050808     c     K04VAB04      reade     FIVAB000
041700050808      *
041800050808do  1c                   DOW       NOT %eof(FIVAB04L)
041900050808      *
042000050808if  2c                   if            VABccm = Kccm
042100050808     c                             and VABfgs = Kfgs
042200120510     c                   eval      $Found = *on
042300050808     c                   leave
042400050808e   2c                   endif
042500050808      *
042600050808     c     K04VAB04      reade     FIVAB000
042700050808      *
042800050808e   1c                   ENDDO
042900050808      *
043000050808     c                   ENDSR
043100120510     C*----------------------------------------------------*
043200120510     C*   *INZSR: Operazioni iniziali                      *
043300120510     C*----------------------------------------------------*
043400120510     C     *INZSR        BEGSR
043500120510     C*
043600120510     C     *ENTRY        PLIST
043700120510     C                   PARM                    KPJBA
043800120510      *
043900120510      * Caricamento Tabella 3BC bypass ctrl
044000120510     C                   Z-ADD     0             wY
044100120510     C     '3BC'         CHAIN     TNTBE01L                           31
044200120510      *
044300120510     C     *IN31         DOWEQ     '0'
044400120510      * Se il S.I. è indicato deve essere uguale al mio
044500120510     C     TBEATB        IFEQ      ' '
044600120510     C     TBEKE2        ANDEQ     *blank
044700120510     C     TBESIF        IFEQ      KNSIF
044800120510     C     TBESIF        OREQ      *BLANK
044900120510     C                   MOVEL     TBEUNI        D3BC
045000120510      * salvo il  cliente solo se il bypass relativo alla NON cancellazione
045100120510      * dati in mancanza del VAB è attivo
045200120510     C                   IF        §3BCDNT = 'S'
045300120510     c                   if        %lookup(%dec(%trim(%subst(TBEKe1 : 1 : 7))
045400120510     C                              : 7 : 0) : CCM) = 0
045500120510     C                   ADD       1             wY
045600120510     C                   EVAL      CCM(wY) =
045700120510     C                             %dec(%trim(%subst(TBEKe1 : 1 : 7)) : 7 : 0)
045800120510     c                   end
045900120510     C                   ENDIF
046000120510     C                   END
046100120510     C                   END
046200120510     C     '3BC'         READE     TNTBE01L                               31
046300120510     C                   ENDDO
046400120510     C*
046500120510     C                   ENDSR
