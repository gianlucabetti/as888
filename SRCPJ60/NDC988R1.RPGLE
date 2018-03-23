000100060907     F*----------------------------------------------------*
000200090310     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000300090310     H*PARMS CVTOPT(*NONE)
000400090310     H DECEDIT('0,') DATEDIT(*DMY.)
000500090310      *
000600090310    > *                   Gestione impegni:reperisce partite scadute
000700090310      *           ---------------------------------------------------
000800060907     F*------------
000900060913     FNDPpa03L  IF   E           K DISK
001000060920     F*------------
001100060913     FNDSol08L  IF   E           K DISK
001200060920     F*------------
001300140317     FNDIMP04L  IF   E           K DISK
001400060911     F                                     RENAME(NDIMP000:NDIMP01)
001500060907     F*------------
001600060920     FNDIMP02L  IF   E           K DISK
001700060920     F                                     RENAME(NDIMP000:NDIMP02)
001800060920     F*------------
001900060907     D*----------------------------------------------------*
002000060911     D* DS per richiesta dati a X20
002100060912     D X20ScadDS     E DS                  inz
002200060912     D* DS parametro esteso se richiesto x20parscd='S'
002300060912     D X20SCADXDS    E DS                  inz
002400060912     D*------------
002500060912     D* schiere per parametro esteso
002600060912     D  DTSCAX                         D   DIM(50) overlay(x20dtscax)
002700060912     D  IMPRMC                       19  3 DIM(50) overlay(x20imprmc)
002800060912     D  IMPRD                        19  3 DIM(50) overlay(x20imprd)
002900060912     D  IMPOMC                       19  3 DIM(50) overlay(x20impomc)
003000060912     D  IMPOD                        19  3 DIM(50) overlay(x20impod)
003100060912     D  CAUS1                         4A   DIM(50) overlay(x20caus1)
003200060912     D  CAUS2                         4A   DIM(50) overlay(x20caus2)
003300100423     D*--------------
003400100423c2156D* schiere per impegni della stessa scadenza
003500100423  "  D  progrX                        3  0 DIM(50)
003600100423  "  D  esitoX                        2    DIM(50)
003700140317  "  D  impAnnx                       1    DIM(50)
003800100423  "  D  notax                       400    DIM(50)
003900100423  "  D  imporX                       19  3 DIM(50)
004000100423c2156D  dataX                          D   DIM(50)
004100060911     D*--------------
004200060911     D* DS per passaggio dati
004300090310     D NDC988DS1     E DS                  occurs(1000)
004400090310     D* Parametri in ricezione
004500090310    >D NDC988DS      E DS
004600100517C2156D  dtpar988x    E                     EXTFLD(dtpar988)                     *
004700100517  "  D  nrpar988x    E                     EXTFLD(nrpar988)                     *
004800100517C2156D  serpar988x   E                     EXTFLD(serpar988)                    *
004900020410     D*--------------
005000090310     D* Parametri DI OUTPUT
005100060907     D*  flag di errore (output)
005200060907     D Errore          S              1A
005300060912     D*  totale scaduto (output)
005400090312     D TotScaduto      S             19S 3
005500060912     D*  totale a scadere (output)
005600090312     D TotAScad        S             19S 3
005700090317     D*  totale impegni
005800090317     D TotImpegni      S             19S 3
005900060922     D*  ultimo progressivo righe fittizie (output)
006000090312     D PrgFittizie     S              9S 0
006100020410     D*-------------
006200060913     D* Variabili di lavoro
006300020410     D ZDateISO        S               D
006400060913     D $grav           S                   like(SolTsl)
006500060913     D*-------------
006600060913     DAnope          E DS                  EXTNAME(ANOPE00F) inz
006700020410     D*--------------
006800060913     D* Definizioni parametri standard chiamate moduli
006900060913     D esito           S              1
007000060913     D gesmsg          S              1
007100060913     D lenout          S              9B 0
007200060913     D Data            S             10
007300060913     D strutturaO      S             10
007400060913     D Operazione      S              2
007500060913     D LenIn           S              9B 0
007600060913     D refresh         S              1
007700060913     D uso             S              1
007800060913     DOutput           S           4000
007900020410     D*--------------
008000090512     D* schiere per dtpar/num/serie/dtscad
008100090512     D  DatiPar        S             33A   DIM(9999)
008200090512     D  WDatiPar       DS            33
008300090512     D  dtpar                  1     10
008400090512     D  nrpar                 11     19
008500090512     D  serpar                20     23
008600090512     D  dtsca                 24     33
008700020410     C*----------------------------------------------------------
008800060913     C* Flusso principale
008900020410     C*----------------------------------------------------------
009000020410     C*
009100020410     C                   EXSR      InzVar
009200060907     C*
009300060911     C* reperisco le partite aperte
009400060911     C                   EXSR      RepParApe
009500020410     C*
009600060912     C* reperimento partite fittizie per impegni globali
009700060912     C                   EXSR      RepParFitt
009800060912     C*
009900060907     C* fine pgm in modo corretto
010000060907     C                   EVAL      Errore = '0'
010100060907     C                   EXSR      EndPgm
010200060907     C************************************************************
010300060907     C* Fine pgm
010400060907     C************************************************************
010500060907     C     EndPgm        BEGSR
010600060912     C*
010700060907     C                   SETON                                        RT
010800060907     C                   RETURN
010900060907     C*
011000060907     C                   ENDSR
011100020410     C************************************************************
011200060911     C* Reperisco partite aperte in scadenza
011300020410     C************************************************************
011400060911     C     RepParApe     BEGSR
011500060913     C* lettura file partite aperte
011600090310     c                   eval      PpaSocieta=Societa988
011700060913     c                   eval      PpaCtb    ='CG'
011800090310     c                   eval      PpaKcc    = Kcc988
011900090310     c                   eval      PpaKsc    = ksc988
012000060913     C*
012100060913     c     K04Ppa03      setll     Ndppa03L
012200060913     c                   do        *hival
012300060913     c     K04Ppa03      reade     Ndppa03L
012400060913     c                   if        %eof(ndPpa03L)
012500060913     c                   leave
012600060913     c                   endif
012700090317     C* Se richiesta unità e diversa, salto
012800090323     c                   if        Unita988<> *blanks
012900090317     c                             and ppaunita <> unita988
013000090317     c                   iter
013100090317     c                   endif
013200060913     c*
013300060913     C* per ogni partita richiamo routine calcolo scadenzario
013400060913     c                   exsr      CallX20
013500060913     c* o se non ci sono rate
013600090316     c                   if        X20Index = 0 or
013700060913     c* o per errori
013800060913     c                             X20Errori<> *blanks
013900060913     c                   iter
014000060913     c                   endif
014100060913     c*
014200060913     c* Reperimento gravità cliente ultimo sollecito della partita
014300090310     c                   clear                   $grav
014400060913     c                   exsr      RepGravita
014500060913     c* Lettura scadenze
014600060913     c                   exsr      RepScaden
014700060913     c* Totalizzo scaduto e a scadere
014800060913     c                   add       X20LScadu     TotScaduto
014900060913     c                   add       X20LAScad     TotAScad
015000060913     c*
015100060913     c                   enddo
015200060911     C*
015300020410     C                   ENDSR
015400060913     C************************************************************
015500060913     C* Chiamata routine calcolo scadenzario
015600060913     C************************************************************
015700060913     C     Callx20       BEGSR
015800060913     C*
015900060913     C                   RESET                   x20scadds
016000060913     C                   RESET                   x20scadXds
016100090310     C                   eval      x20societa = Societa988
016200090421     C                   eval      x20Unita   = Unita988
016300060913     C                   eval      x20ctb     = 'CG'
016400060913    >C                   eval      x20def     = *on
016500060913    >C                   eval      x20prov    = *on
016600060913    >C                   eval      x20gest    = *on
016700060913     C                   eval      x20dtregmd = ZDateIso
016800060913     C                   eval      x20dtregmp = *hival
016900060913     C                   eval      x20dtregmg = ZDateIso
017000090310     C                   eval      x20kcc     =  kcc988
017100090310     C                   eval      x20ksc     =  ksc988
017200060913     C                   move      PPADtPar      X20DtPar
017300060913     C                   move      PPANrPar      X20NrPar
017400060913     C                   move      PPASerPar     X20SerPar
017500060913     C                   eval      x20snatura = 'C'
017600060913     C                   eval      x20parsca  = 'S'
017700060913     C                   eval      X20ParScd  = 'S'
017800060913     C                   eval      x20elasca  = 'R'
017900060913     C                   eval      x20DtSca   = ZdateIso
018000060913     C                   eval      x20ppa     = 'N'
018100060913     C                   eval      x20lrt     = 'R'
018200060913     C                   CALL      'X20SCADR'
018300060913     C                   PARM                    X20SCADDS
018400060913     C                   PARM                    X20ScadXDS
018500060913     C*
018600060913     C                   ENDSR
018700060913     C************************************************************
018800060913     C* reperimento scadenze
018900060913     C************************************************************
019000060913     C     RepScaden     BEGSR
019100090512     C*
019200060913     C*
019300060913     C                   DO        X20Index      $X                3 0
019400060913     C*
019500060913     C* se la scadenza è chiusa (in divisa) la salto
019600060913     C                   IF        ImpRD($x) = 0
019700060913     C                   iter
019800060913     C                   ENDIF
019900090317     C* se la rata è a scadere la salto (SE UGUALE È SCADUTO)
020000090317     C                   IF        DtscaX($x) > DtRif988
020100090317     C                   iter
020200090317     C                   ENDIF
020300060913     C*
020400090407     C* se non portafoglio attivo, salto (solo ricb)
020500060913     C                   exsr      Callmvc003
020600090421     c                   if        OpeStp  =  '1'
020700060913     C                   iter
020800060913     C                   endif
020900090512     C*
021000090512     C* controllo di non aver più scadenze per la stessa partita
021100090512     c* (se ne deve caricare una sola altrimenti non funziona
021200090512     c* l'aggancio con il file degli impegni)
021300090512     c                   move      PpaDtPar      DtPar
021400090512     c                   move      Ppanrpar      Nrpar
021500090512     c                   move      PpaserPar     SerPar
021600090512     c                   move      DtScax($x)    DtSca
021700090512     c     Wdatipar      lookup    datipar                                13
021800090512     c* se già trovata, salto
021900090512     c                   if        *in13=*on
022000090512     C                   iter
022100090512     c                   else
022200090512     c* memorizzo
022300090512     c                   add       1             $Y
022400090512     c                   movea     Wdatipar      datipar($Y)
022500090512     c                   endif
022600090512     c* tutto ok, quindi
022700060913     c* riempimento ds output
022800090310     c                   add       1             $i                5 0
022900090310     c     $I            occur     NDC988DS1
023000090323     c                   move      PpaUnita      Unita1988
023100090310     c                   move      PpaDtPar      DtPar988
023200090310     c                   move      Ppanrpar      NrPar988
023300090310     c                   move      PpaserPar     SerPar988
023400090310     c                   move      DtScax($x)    DtSca988
023500090310     C                   z-add     ImpRMC($X)    ImpScad988
023600090310     c                   move      $Grav         GravSol988
023700140319      * di default imposto il progressivo uguale a 1
023800140319     c                   eval      progr988 = 500
023900060913     C*
024000100423     C* per ogni scadenza recuperata vado a vedere se ci sono impegni
024100100423     c                   EXSR      RepImpegno
024200140313     c                   if        $F10mult = *on
024300140313     c                             and $z > 0
024400140311     c                   eval      $bbb = $z
024500140311     c                   else
024600140311     c                   z-add     1             $bbb              3 0
024700140311     c                   endif
024800140311c2156c***  1             do        $z            $H                3 0
024900140311c2156c     $bbb          do        $z            $H                3 0
025000100423  "  c                   move      PpaUnita      Unita1988
025100100423  "  c                   move      PpaDtPar      DtPar988
025200100423  "  c                   move      Ppanrpar      NrPar988
025300100423  "  c                   move      PpaserPar     SerPar988
025400100423  "  c                   move      DtScax($x)    DtSca988
025500100423  "  C                   z-add     ImpRMC($X)    ImpScad988
025600100423  "  c                   move      $Grav         GravSol988
025700100423  "  C                   EVAL      Esito988     = esitoX($H)
025800140317  "  C                   EVAL      ImpAnn988    = impannX($H)
025900100423  "  C                   EVAL      Nota988      = Notax($H)
026000100423  "  C                   EVAL      Dtimp988     = Datax($H)
026100100423  "  C                   EVAL      ImpImp988    = IMPorx($H)
026200100423  "  C                   EVAL      progr988     = progrx($H)
026300100423  "  c                   if        $H < $z
026400100423  "  c                   add       1             $i                5 0
026500100423  "  c     $I            occur     NDC988DS1
026600100423  "  c                   endif
026700100423c2156C                   enddo
026800140311      *
026900060913     c                   enddo
027000060913     C*
027100060913     C                   ENDSR
027200060913     C************************************************************
027300060913     C* Reperisco gravità
027400060913     C************************************************************
027500060913     C     RepGravita    BEGSR
027600060913     C*
027700060913     c* aggancio NDSOL per reperire gravità effettiva partita ultimo
027800060913     c* sollecito (ndsol08 è descend per nr./data sollecito)
027900060913     c*
028000090310     C                   eval      SolCsi  = Societa988
028100090310     C                   eval      SolKcc  = kcc988
028200090310     C                   eval      SolKsc  = ksc988
028300060913     C                   eval      SolDrp  = ppadtpar
028400060913     C                   eval      SolNrp  = Ppanrpar
028500060913     C                   eval      SolSerPar = PpaSerPar
028600060913     C     K06Sol08      setll     NdSol08L
028700060913     C     K06Sol08      reade     NdSol08L
028800090316     c                   if        not %eof(ndSol08L)
028900060913     c                   move      SolTsl        $Grav
029000060913     c                   endif
029100060913     C*
029200060913     C                   ENDSR
029300060907     C************************************************************
029400100423     C* Reperisco gli impegni collegati alla scadenza trovata
029500060907     C************************************************************
029600060911     C     RepImpegno    BEGSR
029700060907     C*
029800100423c2156c                   clear                   progrx
029900100423  "  c                   clear                   esitox
030000100423  "  c                   clear                   notax
030100100423  "  c                   clear                   dataX
030200100423  "  c                   clear                   imporx
030300100423c2156c                   clear                   $z                3 0
030400100423     c*
030500090310     C                   EVAL      IMPSocieta = Societa988
030600090310     C                   EVAL      IMPKcc     = kcc988
030700090310     C                   EVAL      IMPKsc     = ksc988
030800060920     C                   EVAL      IMPDtPar   = PPADtPar
030900060920     C                   EVAL      IMPNrPar   = PPANrPar
031000060920     C                   EVAL      IMPSerPar  = PPASerPar
031100090310     C                   EVAL      IMPDtSca   = DtSca988
031200060920     C*
031300100423c2156x***  K07IMP01      CHAIN     NDIMP01                            21
031400060920     C*
031500090310     C                   EVAL      Esito988     = *blank
031600140317     C                   EVAL      ImpAnn988    = *blank
031700090310     C                   EVAL      Nota988      = *blank
031800090310     C                   EVAL      Dtimp988     = *loval
031900090310     C                   EVAL      Impimp988    = 0
032000100423c2156x*************      IF        *in21 = *off
032100100423     c*
032200100423c2156C     K07IMP01      setll     NDIMP01                            21
032300100423  "  c                   do        *Hival
032400100423  "  C     K07IMP01      reade     NDIMP01                                21
032500100423  "  c   21              leave
032600100423  "  c                   add       1             $Z
032700100423  "  c                   EVAL      Esitox($Z)   = IMPEsito
032800140317  "  c                   EVAL      ImpAnnx($Z)  = IMPAnn
032900100423  "  c                   EVAL      Notax($Z)    = IMPNota
033000100423  "  c                   EVAL      Datax($Z)    = IMPDtImp
033100100423  "  c                   EVAL      Imporx($Z)   = IMPImpImp
033200100423c2156c                   eval      progrX($Z)   =ImpProgr
033300100423     c                   add       ImpImpImp     totimpegni
033400100423     c*
033500100423c2156x**                 EVAL      Esito988     = IMPEsito
033600100423  "  x**                 EVAL      Nota988      = IMPNota
033700100423  "  x**                 EVAL      Dtimp988     = IMPDtImp
033800100423  "  x**                 EVAL      ImpImp988    = IMPImpImp
033900100423  "  x**                 eval      progrX       =ImpProgr
034000100423c2156x**************     ENDIF
034100100423     C*
034200100423c2156c                   enddo
034300060907     C*
034400060907     C                   ENDSR
034500060912     C************************************************************
034600060912     C* Reperisco partite fittizie per impegni globali
034700060912     C************************************************************
034800060912     C     RepParFitt    BEGSR
034900060920     C*
035000060920     C                   EVAL      IMPFitt = *on
035100090310     C                   EVAL      IMPSocieta = Societa988
035200090310     C                   EVAL      IMPKcc     = Kcc988
035300090310     C                   EVAL      IMPKsc     = ksc988
035400060922     C                   EVAL      IMPNrPar   = 0
035500060920     C*
035600090323     C     K04IMP02      CHAIN     NDIMP02                            21
035700060912     C*
035800060920     C                   DOW       *in21 = *off
035900060922     C*
036000060922     C* se l'impegno non è annullato
036100060922     C                   IF        IMPAnn = *blank
036200090323     C* e se l'unità non è diversa da quella richiesta
036300090421     c                   if        (Unita988<> *blanks
036400090421     c                             and (Impunita =  unita988  or
036500090421     c                             ImpUnita=*blanks)) or
036600090421     c* tutte le unita
036700090421     c                             Unita988=*blanks
036800060920     c                   add       1             $i
036900090310     c     $I            occur     NDC988DS1
037000090323     c                   move      IMPUnita      Unita1988
037100090310     c                   move      IMPDtPar      DtPar988
037200090310     c                   move      IMPNrPar      NrPar988
037300090310     c                   move      IMPSerPar     SerPar988
037400090310     c                   move      IMPDtSca      DtSca988
037500090310     C                   z-add     0             ImpScad988
037600090310     c                   move      *blank        GravSol988
037700090310     C                   EVAL      Esito988     = ImpEsito
037800140317     C                   EVAL      ImpAnn988    = ImpAnn
037900090310     C                   EVAL      Nota988      = ImpNota
038000090310     C                   EVAL      DtImp988     = ImpDtImp
038100090310     C                   EVAL      ImpImp988    = IMPImpImp
038200090317     c                   add       ImpImpImp     totimpegni
038300090323     C                   ENDIF
038400060922     C                   ENDIF
038500060922     C*
038600090323     C     K04IMP02      READE     NDIMP02                                21
038700060920     C                   ENDDO
038800060922     C*
038900060922     C* memorizzo l'ultimo numero impegno fittizio per il cliente
039000060922     C                   EVAL      PrgFittizie = IMPNrPar
039100060912     C*
039200060912     C                   ENDSR
039300060907     C************************************************************
039400060907     C* OPERAZIONI INIZIALI
039500060907     C************************************************************
039600060907     C     *INZSR        BEGSR
039700060907     C*
039800060907     C     *ENTRY        PLIST
039900090310     C                   parm                    NDC988DS
040000090310     C                   parm                    NDC988DS1
040100090310     C                   PARM                    TotScaduto
040200090310     C                   PARM                    TotAScad
040300090317     C                   PARM                    TotImpegni
040400090310     C                   PARM                    PrgFittizie
040500090310     C                   PARM                    Errore
040600140313     C                   PARM                    $F10mult          1
040700060907     C*
040800060929     C                   TIME                    ZDateISO
040900060907     C*
041000060907     C                   ENDSR
041100020410     C************************************************************
041200060907     C* INIZIALIZZAZIONE VARIABILI
041300020410     C************************************************************
041400060907     C     INZVAR        BEGSR
041500060913     c*
041600060913     c* pulizia ds output
041700060913     c                   do        1000          $I
041800090310     c     $I            occur     NDC988DS1
041900090310     C                   CLEAR                   NDC988DS1
042000060913     C                   enddo
042100020410     C*
042200060907     C                   EVAL      $i = 0
042300060913     c                   clear                   TotScaduto
042400060913     c                   clear                   TotAScad
042500090317     c                   clear                   TotImpegni
042600060913     c                   clear                   $grav
042700090310     c                   clear                   errore
042800090512     c                   z-add     0             $Y                5 0
042900090512     c                   clear                   DatiPar
043000020410     C*
043100020410     C                   ENDSR
043200060907     C************************************************************
043300060907     C* Definizione KList
043400060907     C************************************************************
043500060907     C     DefKList      BEGSR
043600060907     C*
043700060913    >C* NdPpa03L
043800060913    >C     K04Ppa03      KLIST
043900060913    >C                   KFLD                    PpaSocieta
044000060913    >C                   KFLD                    PpaCTB
044100060913    >C                   KFLD                    PpaKcc
044200060913    >C                   KFLD                    PpaKsc
044300060920     C* NDSOL08L
044400060913     C     K06Sol08      KLIST
044500060913     C                   KFLD                    SolCsi
044600060913     C                   KFLD                    SolKcc
044700060913     C                   KFLD                    SolKsc
044800060913     C                   KFLD                    SolDrp
044900060913     C                   KFLD                    SolNrp
045000060913     C                   KFLD                    SolSerPar
045100140317     C* NDIMP04L
045200090323     C     K07IMP01      KLIST
045300060920     C                   KFLD                    IMPSocieta
045400060920     C                   KFLD                    IMPKcc
045500060920     C                   KFLD                    IMPKsc
045600060920     C                   KFLD                    IMPDtPar
045700060920     C                   KFLD                    IMPNrPar
045800060920     C                   KFLD                    IMPSerPar
045900060920     C                   KFLD                    IMPDtSca
046000060920     C* NDIMP02L
046100090323     C     K04IMP02      KLIST
046200060920     C                   KFLD                    IMPFitt
046300060920     C                   KFLD                    IMPSocieta
046400060920     C                   KFLD                    IMPKcc
046500060920     C                   KFLD                    IMPKsc
046600060907     C*
046700060907     C                   ENDSR
046800060913     C*----------------------------------------------------------
046900060913     C* Chiamata NDMVC003
047000060913     C*----------------------------------------------------------
047100060913     C     CallMvc003    BEGSR
047200060913     C*
047300060913     C* Controllo causale
047400060913     C                   CLEAR                   ANOPE
047500060913     C                   eval      Data = *loval
047600060913     C                   EVAL      LenOut = %Size(ANOPE)
047700060913     C                   CallB     'NDMVC003'
047800090310     C                   PARM                    Societa988
047900060913     C                   PARM                    Caus2($X)
048000060913     C                   PARM                    Data
048100060913     C                   PARM                    Uso
048200060913     C                   PARM      *OFF          GesMsg
048300060913     C                   PARM      *OFF          Esito
048400060913     C                   PARM      'ANOPE'       StrutturaO
048500060913     C                   PARM                    ANOPE
048600060913     C                   PARM                    LenOut
048700060913     C                   PARM      '0'           Refresh
048800060913     C*
048900060913     C                   ENDSR
049000060913     C/EJECT
