000100060907     F*----------------------------------------------------*
000200090310     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000300090310     H*PARMS CVTOPT(*NONE)
000400090310     H DECEDIT('0,') DATEDIT(*DMY.)
000500090310      *
000600130916    > *           Gestione attività manuali:reperisce partite scadute
000700090310      *           ---------------------------------------------------
000800060907     F*------------
000900060913     FNDPpa03L  IF   E           K DISK
001000060920     F*------------
001100060913     FNDSol08L  IF   E           K DISK
001200060920     F*------------
001300130916     FNDATT01L  IF   E           K DISK
001400130916     F                                     RENAME(NDATT000:NDATT01)
001500060907     F*------------
001600130916     FNDATT02L  IF   E           K DISK
001700130916     F                                     RENAME(NDATT000:NDATT02)
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
003400130916     D* schiere per impegni della stessa scadenza
003500130916     D  progrX                        3  0 DIM(50)
003600130916     D  azioneX                       2    DIM(50)
003700130916     D  gruteX                        3    DIM(50)
003800130916     D  notax                       400    DIM(50)
003900130916     D  dataX                          D   DIM(50)
004000060911     D*--------------
004100060911     D* DS per passaggio dati
004200130916     D NDC980DS1     E DS                  occurs(1000)
004300090310     D* Parametri in ricezione
004400130916    >D NDC980DS      E DS
004500130916     D  dtpar980x    E                     EXTFLD(dtpar980)                     *
004600130916     D  nrpar980x    E                     EXTFLD(nrpar980)                     *
004700130916     D  serpar980x   E                     EXTFLD(serpar980)                    *
004800020410     D*--------------
004900090310     D* Parametri DI OUTPUT
005000060907     D*  flag di errore (output)
005100060907     D Errore          S              1A
005200060912     D*  totale scaduto (output)
005300090312     D TotScaduto      S             19S 3
005400060922     D*  ultimo progressivo righe fittizie (output)
005500090312     D PrgFittizie     S              9S 0
005600020410     D*-------------
005700060913     D* Variabili di lavoro
005800020410     D ZDateISO        S               D
005900060913     D $grav           S                   like(SolTsl)
006000060913     D*-------------
006100060913     DAnope          E DS                  EXTNAME(ANOPE00F) inz
006200020410     D*--------------
006300060913     D* Definizioni parametri standard chiamate moduli
006400060913     D esito           S              1
006500060913     D gesmsg          S              1
006600060913     D lenout          S              9B 0
006700060913     D Data            S             10
006800060913     D strutturaO      S             10
006900060913     D Operazione      S              2
007000060913     D LenIn           S              9B 0
007100060913     D refresh         S              1
007200060913     D uso             S              1
007300060913     DOutput           S           4000
007400020410     D*--------------
007500090512     D* schiere per dtpar/num/serie/dtscad
007600090512     D  DatiPar        S             33A   DIM(9999)
007700090512     D  WDatiPar       DS            33
007800090512     D  dtpar                  1     10
007900090512     D  nrpar                 11     19
008000090512     D  serpar                20     23
008100090512     D  dtsca                 24     33
008200020410     C*----------------------------------------------------------
008300060913     C* Flusso principale
008400020410     C*----------------------------------------------------------
008500020410     C*
008600020410     C                   EXSR      InzVar
008700060907     C*
008800060911     C* reperisco le partite aperte
008900060911     C                   EXSR      RepParApe
009000020410     C*
009100060912     C* reperimento partite fittizie per impegni globali
009200060912     C                   EXSR      RepParFitt
009300060912     C*
009400060907     C* fine pgm in modo corretto
009500060907     C                   EVAL      Errore = '0'
009600060907     C                   EXSR      EndPgm
009700060907     C************************************************************
009800060907     C* Fine pgm
009900060907     C************************************************************
010000060907     C     EndPgm        BEGSR
010100060912     C*
010200060907     C                   SETON                                        RT
010300060907     C                   RETURN
010400060907     C*
010500060907     C                   ENDSR
010600020410     C************************************************************
010700060911     C* Reperisco partite aperte in scadenza
010800020410     C************************************************************
010900060911     C     RepParApe     BEGSR
011000060913     C* lettura file partite aperte
011100130916     c                   eval      PpaSocieta=Societa980
011200060913     c                   eval      PpaCtb    ='CG'
011300130916     c                   eval      PpaKcc    = Kcc980
011400130916     c                   eval      PpaKsc    = ksc980
011500060913     C*
011600060913     c     K04Ppa03      setll     Ndppa03L
011700060913     c                   do        *hival
011800060913     c     K04Ppa03      reade     Ndppa03L
011900060913     c                   if        %eof(ndPpa03L)
012000060913     c                   leave
012100060913     c                   endif
012200090317     C* Se richiesta unità e diversa, salto
012300130916     c                   if        Unita980<> *blanks
012400130916     c                             and ppaunita <> unita980
012500090317     c                   iter
012600090317     c                   endif
012700060913     c*
012800060913     C* per ogni partita richiamo routine calcolo scadenzario
012900060913     c                   exsr      CallX20
013000060913     c* o se non ci sono rate
013100090316     c                   if        X20Index = 0 or
013200060913     c* o per errori
013300060913     c                             X20Errori<> *blanks
013400060913     c                   iter
013500060913     c                   endif
013600060913     c*
013700060913     c* Reperimento gravità cliente ultimo sollecito della partita
013800090310     c                   clear                   $grav
013900060913     c                   exsr      RepGravita
014000060913     c* Lettura scadenze
014100060913     c                   exsr      RepScaden
014200060913     c* Totalizzo scaduto e a scadere
014300060913     c                   add       X20LScadu     TotScaduto
014400060913     c*
014500060913     c                   enddo
014600060911     C*
014700020410     C                   ENDSR
014800060913     C************************************************************
014900060913     C* Chiamata routine calcolo scadenzario
015000060913     C************************************************************
015100060913     C     Callx20       BEGSR
015200060913     C*
015300060913     C                   RESET                   x20scadds
015400060913     C                   RESET                   x20scadXds
015500130916     C                   eval      x20societa = Societa980
015600130916     C                   eval      x20Unita   = Unita980
015700060913     C                   eval      x20ctb     = 'CG'
015800060913    >C                   eval      x20def     = *on
015900060913    >C                   eval      x20prov    = *on
016000060913    >C                   eval      x20gest    = *on
016100060913     C                   eval      x20dtregmd = ZDateIso
016200060913     C                   eval      x20dtregmp = *hival
016300060913     C                   eval      x20dtregmg = ZDateIso
016400130916     C                   eval      x20kcc     =  kcc980
016500130916     C                   eval      x20ksc     =  ksc980
016600060913     C                   move      PPADtPar      X20DtPar
016700060913     C                   move      PPANrPar      X20NrPar
016800060913     C                   move      PPASerPar     X20SerPar
016900060913     C                   eval      x20snatura = 'C'
017000060913     C                   eval      x20parsca  = 'S'
017100060913     C                   eval      X20ParScd  = 'S'
017200060913     C                   eval      x20elasca  = 'R'
017300060913     C                   eval      x20DtSca   = ZdateIso
017400060913     C                   eval      x20ppa     = 'N'
017500060913     C                   eval      x20lrt     = 'R'
017600060913     C                   CALL      'X20SCADR'
017700060913     C                   PARM                    X20SCADDS
017800060913     C                   PARM                    X20ScadXDS
017900060913     C*
018000060913     C                   ENDSR
018100060913     C************************************************************
018200060913     C* reperimento scadenze
018300060913     C************************************************************
018400060913     C     RepScaden     BEGSR
018500090512     C*
018600060913     C*
018700060913     C                   DO        X20Index      $X                3 0
018800060913     C*
018900060913     C* se la scadenza è chiusa (in divisa) la salto
019000060913     C                   IF        ImpRD($x) = 0
019100060913     C                   iter
019200060913     C                   ENDIF
019300090317     C* se la rata è a scadere la salto (SE UGUALE È SCADUTO)
019400130916     C                   IF        DtscaX($x) > DtRif980
019500090317     C                   iter
019600090317     C                   ENDIF
019700060913     C*
019800090407     C* se non portafoglio attivo, salto (solo ricb)
019900060913     C                   exsr      Callmvc003
020000090421     c                   if        OpeStp  =  '1'
020100060913     C                   iter
020200060913     C                   endif
020300090512     C*
020400090512     C* controllo di non aver più scadenze per la stessa partita
020500090512     c* (se ne deve caricare una sola altrimenti non funziona
020600090512     c* l'aggancio con il file degli impegni)
020700090512     c                   move      PpaDtPar      DtPar
020800090512     c                   move      Ppanrpar      Nrpar
020900090512     c                   move      PpaserPar     SerPar
021000090512     c                   move      DtScax($x)    DtSca
021100090512     c     Wdatipar      lookup    datipar                                13
021200090512     c* se già trovata, salto
021300090512     c                   if        *in13=*on
021400090512     C                   iter
021500090512     c                   else
021600090512     c* memorizzo
021700090512     c                   add       1             $Y
021800090512     c                   movea     Wdatipar      datipar($Y)
021900090512     c                   endif
022000090512     c* tutto ok, quindi
022100060913     c* riempimento ds output
022200090310     c                   add       1             $i                5 0
022300130916     c     $I            occur     NDC980DS1
022400130916     c                   move      PpaUnita      Unita1980
022500130916     c                   move      PpaDtPar      DtPar980
022600130916     c                   move      Ppanrpar      NrPar980
022700130916     c                   move      PpaserPar     SerPar980
022800130916     c                   move      DtScax($x)    DtSca980
022900130916     C                   z-add     ImpRMC($X)    ImpScad980
023000130916     c                   move      $Grav         GravSol980
023100060913     C*
023200130916     C* per ogni scadenza recuperata vado a vedere se ci sono attività
023300130916     c                   EXSR      RepAtti
023400140313     c                   if        $F10mult = *on
023500140313     c                             and $z > 0
023600140313     c                   eval      $bbb = $z
023700140313     c                   else
023800140313     c                   z-add     1             $bbb              3 0
023900140313     c                   endif
024000140313     c**** 1             do        $z            $H                3 0
024100140313c2156c     $bbb          do        $z            $H                3 0
024200130916     c                   move      PpaUnita      Unita1980
024300130916     c                   move      PpaDtPar      DtPar980
024400130916     c                   move      Ppanrpar      NrPar980
024500130916     c                   move      PpaserPar     SerPar980
024600130916     c                   move      DtScax($x)    DtSca980
024700130916     C                   z-add     ImpRMC($X)    ImpScad980
024800130916     c                   move      $Grav         GravSol980
024900130916     C                   EVAL      Azione980    = AzioneX($H)
025000130916     C                   EVAL      Grute980     = GruteX($H)
025100130916     C                   EVAL      Nota980      = Notax($H)
025200130916     C                   EVAL      DtAtt980     = Datax($H)
025300130916     C                   EVAL      progr980     = progrx($H)
025400130916     c                   if        $H < $z
025500130916     c                   add       1             $i                5 0
025600130916     c     $I            occur     NDC980DS1
025700130916     c                   endif
025800130916     C                   enddo
025900060913     c                   enddo
026000060913     C*
026100060913     C                   ENDSR
026200060913     C************************************************************
026300060913     C* Reperisco gravità
026400060913     C************************************************************
026500060913     C     RepGravita    BEGSR
026600060913     C*
026700060913     c* aggancio NDSOL per reperire gravità effettiva partita ultimo
026800060913     c* sollecito (ndsol08 è descend per nr./data sollecito)
026900060913     c*
027000130916     C                   eval      SolCsi  = Societa980
027100130916     C                   eval      SolKcc  = kcc980
027200130916     C                   eval      SolKsc  = ksc980
027300060913     C                   eval      SolDrp  = ppadtpar
027400060913     C                   eval      SolNrp  = Ppanrpar
027500060913     C                   eval      SolSerPar = PpaSerPar
027600060913     C     K06Sol08      setll     NdSol08L
027700060913     C     K06Sol08      reade     NdSol08L
027800090316     c                   if        not %eof(ndSol08L)
027900060913     c                   move      SolTsl        $Grav
028000060913     c                   endif
028100060913     C*
028200060913     C                   ENDSR
028300060907     C************************************************************
028400100423     C* Reperisco gli impegni collegati alla scadenza trovata
028500060907     C************************************************************
028600130916     C     RepAtti       BEGSR
028700060907     C*
028800130916     C                   clear                   progrx
028900130916     C                   clear                   azionex
029000130916     C                   clear                   grutex
029100130916     C                   clear                   notax
029200130916     C                   clear                   dataX
029300130916     C                   clear                   $z                3 0
029400130916     C*
029500130916     C                   EVAL      ATTSocieta = Societa980
029600130916     C                   EVAL      ATTKcc     = kcc980
029700130916     C                   EVAL      ATTKsc     = ksc980
029800130916     C                   EVAL      ATTDtPar   = PPADtPar
029900130916     C                   EVAL      ATTNrPar   = PPANrPar
030000130916     C                   EVAL      ATTSerPar  = PPASerPar
030100130916     C                   EVAL      ATTDtSca   = DtSca980
030200060920     C*
030300130916     C                   EVAL      Azione980    = *blank
030400130916     C                   EVAL      Grute980     = *blank
030500130916     C                   EVAL      Nota980      = *blank
030600130916     C                   EVAL      DtAtt980     = *loval
030700100423     c*
030800130916     C     K07ATT01      setll     NDATT01                            21
030900130916     c                   do        *Hival
031000130916     C     K07ATT01      reade     NDATT01                                21
031100130916     c   21              leave
031200130916     c                   add       1             $Z
031300130916     c                   EVAL      Azionex($Z)  = ATTAzione
031400130916     c                   EVAL      Grutex($Z)   = ATTgrute
031500130916     c                   EVAL      Notax($Z)    = ATTNota
031600130916     c                   EVAL      Datax($Z)    = ATTDtAtt
031700130916     c                   eval      progrX($Z)   = ATTProgr
031800100423     C*
031900130916     c                   enddo
032000060907     C*
032100060907     C                   ENDSR
032200060912     C************************************************************
032300130916     C* Reperisco partite fittizie globali
032400060912     C************************************************************
032500060912     C     RepParFitt    BEGSR
032600060920     C*
032700130916     C                   EVAL      ATTFitt = *on
032800130916     C                   EVAL      ATTSocieta = Societa980
032900130916     C                   EVAL      ATTKcc     = Kcc980
033000130916     C                   EVAL      ATTKsc     = ksc980
033100130916     C                   EVAL      ATTNrPar   = 0
033200060920     C*
033300130916     C     K04ATT02      CHAIN     NDATT02                            21
033400060912     C*
033500060920     C                   DOW       *in21 = *off
033600060922     C*
033700060922     C* se l'impegno non è annullato
033800130916     C                   IF        AttAnn = *blank
033900090323     C* e se l'unità non è diversa da quella richiesta
034000130916     c                   if        (Unita980<> *blanks
034100130916     c                             and (ATTunita =  unita980  or
034200130916     c                             ATTUnita=*blanks)) or
034300090421     c* tutte le unita
034400130916     c                             Unita980=*blanks
034500060920     c                   add       1             $i
034600130916     c     $I            occur     NDC980DS1
034700130916     c                   move      ATTUnita      Unita1980
034800130916     c                   move      ATTDtPar      DtPar980
034900130916     c                   move      ATTNrPar      NrPar980
035000130916     c                   move      ATTSerPar     SerPar980
035100130916     c                   move      ATTDtSca      DtSca980
035200130916     C                   z-add     0             ImpScad980
035300130916     c                   move      *blank        GravSol980
035400130916     C                   EVAL      Azione980    = AttAzione
035500130916     C                   EVAL      Grute980     = AttGrute
035600130916     C                   EVAL      Nota980      = AttNota
035700130916     C                   EVAL      DtAtt980     = AttDtAtt
035800090323     C                   ENDIF
035900060922     C                   ENDIF
036000060922     C*
036100130916     C     K04ATT02      READE     NDATT02                                21
036200060920     C                   ENDDO
036300060922     C*
036400060922     C* memorizzo l'ultimo numero impegno fittizio per il cliente
036500130916     C                   EVAL      PrgFittizie = ATTNrPar
036600060912     C*
036700060912     C                   ENDSR
036800060907     C************************************************************
036900060907     C* OPERAZIONI INIZIALI
037000060907     C************************************************************
037100060907     C     *INZSR        BEGSR
037200060907     C*
037300060907     C     *ENTRY        PLIST
037400130916     C                   parm                    NDC980DS
037500130916     C                   parm                    NDC980DS1
037600090310     C                   PARM                    TotScaduto
037700090310     C                   PARM                    PrgFittizie
037800090310     C                   PARM                    Errore
037900140313     C                   PARM                    $F10mult          1
038000060907     C*
038100060929     C                   TIME                    ZDateISO
038200060907     C*
038300060907     C                   ENDSR
038400020410     C************************************************************
038500060907     C* INIZIALIZZAZIONE VARIABILI
038600020410     C************************************************************
038700060907     C     INZVAR        BEGSR
038800060913     c*
038900060913     c* pulizia ds output
039000060913     c                   do        1000          $I
039100130916     c     $I            occur     NDC980DS1
039200130916     C                   CLEAR                   NDC980DS1
039300060913     C                   enddo
039400020410     C*
039500060907     C                   EVAL      $i = 0
039600060913     c                   clear                   TotScaduto
039700060913     c                   clear                   $grav
039800090310     c                   clear                   errore
039900090512     c                   z-add     0             $Y                5 0
040000090512     c                   clear                   DatiPar
040100020410     C*
040200020410     C                   ENDSR
040300060907     C************************************************************
040400060907     C* Definizione KList
040500060907     C************************************************************
040600060907     C     DefKList      BEGSR
040700060907     C*
040800060913    >C* NdPpa03L
040900060913    >C     K04Ppa03      KLIST
041000060913    >C                   KFLD                    PpaSocieta
041100060913    >C                   KFLD                    PpaCTB
041200060913    >C                   KFLD                    PpaKcc
041300060913    >C                   KFLD                    PpaKsc
041400060920     C* NDSOL08L
041500060913     C     K06Sol08      KLIST
041600060913     C                   KFLD                    SolCsi
041700060913     C                   KFLD                    SolKcc
041800060913     C                   KFLD                    SolKsc
041900060913     C                   KFLD                    SolDrp
042000060913     C                   KFLD                    SolNrp
042100060913     C                   KFLD                    SolSerPar
042200130916     C* NDATT01L
042300130916     C     K07ATT01      KLIST
042400130916     C                   KFLD                    ATTSocieta
042500130916     C                   KFLD                    ATTKcc
042600130916     C                   KFLD                    ATTKsc
042700130916     C                   KFLD                    ATTDtPar
042800130916     C                   KFLD                    ATTNrPar
042900130916     C                   KFLD                    ATTSerPar
043000130916     C                   KFLD                    ATTDtSca
043100130916     C* NDATT02L
043200130916     C     K04ATT02      KLIST
043300130916     C                   KFLD                    ATTFitt
043400130916     C                   KFLD                    ATTSocieta
043500130916     C                   KFLD                    ATTKcc
043600130916     C                   KFLD                    ATTKsc
043700060907     C*
043800060907     C                   ENDSR
043900060913     C*----------------------------------------------------------
044000060913     C* Chiamata NDMVC003
044100060913     C*----------------------------------------------------------
044200060913     C     CallMvc003    BEGSR
044300060913     C*
044400060913     C* Controllo causale
044500060913     C                   CLEAR                   ANOPE
044600060913     C                   eval      Data = *loval
044700060913     C                   EVAL      LenOut = %Size(ANOPE)
044800060913     C                   CallB     'NDMVC003'
044900130916     C                   PARM                    Societa980
045000060913     C                   PARM                    Caus2($X)
045100060913     C                   PARM                    Data
045200060913     C                   PARM                    Uso
045300060913     C                   PARM      *OFF          GesMsg
045400060913     C                   PARM      *OFF          Esito
045500060913     C                   PARM      'ANOPE'       StrutturaO
045600060913     C                   PARM                    ANOPE
045700060913     C                   PARM                    LenOut
045800060913     C                   PARM      '0'           Refresh
045900060913     C*
046000060913     C                   ENDSR
046100060913     C/EJECT
