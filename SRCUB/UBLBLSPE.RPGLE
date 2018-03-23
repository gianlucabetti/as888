000100110124     H BNDDIR('UBBNDDIR')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500080612     ** Questo modulo verifica e reperisce i lagami di una bolla restituendo il tipo di legame
000600080612     ** corrente, l'eventuale bolla originale e lo stato attuale dell'ultima bolla figlia.
000700071128     **
000800080611     ** Può essere usato solo come sottoprocedura.
000900080612     ** Ogni metodo restituisce un valore 10I:
001000080612     **             - = 0 => tutto ok
001100080612     **             - < 0 => errore
001200080612     **
001300080612     ** --------- --- --------- ------------------------------
001400071128     **
001500080612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBLBLSPE_Init
001600071128     **
001700071128     ** Posizione Uso Tipo      Descrizione
001800071128     ** --------- --- --------- ------------------------------
001900080611     **     /      /      /           /
002000071128     ** --------- --- --------- ------------------------------
002100071129     **
002200080612     ** PARAMETRI DELLA SOTTOPROCEDURA UBLBLSPE_GetLblTyp
002300071129     **
002400071129     ** Posizione Uso Tipo      Descrizione
002500071129     ** --------- --- --------- ------------------------------
002600080612     ** 1         IN  SIGNED    Anno           sped. da verificare
002700080612     ** 2         IN  SIGNED    Linea partenza sped. da verificare
002800080612     ** 3         IN  SIGNED    Serie          sped. da verificare
002900080612     ** 4         IN  SIGNED    Numero         sped. da verificare
003000080612     ** 5         OUT CHARACTER Tipo del legame:
003100080612     **                         - O = trattasi d bolla originale
003200080612     **                         - F = trattasi d bolla figlia
003300080612     ** 6         OUT SIGNED    Anno           sped. originale
003400080612     ** 7         OUT SIGNED    Linea partenza sped. originale
003500080612     ** 8         OUT SIGNED    Serie          sped. originale
003600080612     ** 9         OUT SIGNED    Numero         sped. originale
003700080613     ** 10        OUT SIGNED    Data consegna  sped. originale
003800080613     ** 11        OUT CHARACTER Cod. anomalia  sped. originale
003900071129     ** --------- --- --------- ------------------------------
004000080612     **
004100080612     ** PARAMETRI DELLA SOTTOPROCEDURA UBLBLSPE_GetLastChild
004200080612     **
004300080612     ** Posizione Uso Tipo      Descrizione
004400080612     ** --------- --- --------- ------------------------------
004500080612     ** 1         IN  SIGNED    Anno           sped. da verificare
004600080612     ** 2         IN  SIGNED    Linea partenza sped. da verificare
004700080612     ** 3         IN  SIGNED    Serie          sped. da verificare
004800080612     ** 4         IN  SIGNED    Numero         sped. da verificare
004900080612     ** 5         OUT SIGNED    Anno           sped. ultima figlia
005000080612     ** 6         OUT SIGNED    Linea partenza sped. ultima figlia
005100080612     ** 7         OUT SIGNED    Serie          sped. ultima figlia
005200080612     ** 8         OUT SIGNED    Numero         sped. ultima figlia
005300080612     ** 9         OUT SIGNED    Data consegna  sped. ultima figlia
005400080612     ** 10        OUT CHARACTER Cod. anomalia  sped. ultima figlia
005500080612     ** --------- --- --------- ------------------------------
005600100107     **
005700100107     ** PARAMETRI DELLA SOTTOPROCEDURA UBLBLSPE_GetFirstChild
005800100107     **
005900100107     ** Posizione Uso Tipo      Descrizione
006000100107     ** --------- --- --------- ------------------------------
006100100107     ** 1         IN  SIGNED    Anno           sped. da verificare
006200100107     ** 2         IN  SIGNED    Linea partenza sped. da verificare
006300100107     ** 3         IN  SIGNED    Serie          sped. da verificare
006400100107     ** 4         IN  SIGNED    Numero         sped. da verificare
006500100107     ** 5         OUT SIGNED    Anno           sped. prima  figlia
006600100107     ** 6         OUT SIGNED    Linea partenza sped. prima  figlia
006700100107     ** 7         OUT SIGNED    Serie          sped. prima  figlia
006800100107     ** 8         OUT SIGNED    Numero         sped. prima  figlia
006900100107     ** 9         OUT SIGNED    Data consegna  sped. prima  figlia
007000100107     ** 10        OUT CHARACTER Cod. anomalia  sped. prima  figlia
007100100107     ** --------- --- --------- ------------------------------
007200130919     **
007300130919     ** PARAMETRI DELLA SOTTOPROCEDURA UBLBLSPE_GetMother
007400130919     **
007500130919     ** Posizione Uso Tipo      Descrizione
007600130919     ** --------- --- --------- ------------------------------
007700130919     ** 1         IN  SIGNED    Anno           sped. da verificare
007800130919     ** 2         IN  SIGNED    Linea partenza sped. da verificare
007900130919     ** 3         IN  SIGNED    Serie          sped. da verificare
008000130919     ** 4         IN  SIGNED    Numero         sped. da verificare
008100130919     ** 5         OUT SIGNED    Anno           sped. mamma
008200130919     ** 6         OUT SIGNED    Linea partenza sped. mamma
008300130919     ** 7         OUT SIGNED    Serie          sped. mamma
008400130919     ** 8         OUT SIGNED    Numero         sped. mamma
008500130919     ** 9         OUT SIGNED    Data consegna  sped. mamma
008600130919     ** 10        OUT CHARACTER Cod. anomalia  sped. mamma
008700130919     ** --------- --- --------- ------------------------------
008800080612     **
008900080612     ** PARAMETRI DELLA SOTTOPROCEDURA: UBLBLSPE_Finalize
009000080612     **
009100080612     ** Posizione Uso Tipo      Descrizione
009200080612     ** --------- --- --------- ------------------------------
009300080612     **     /      /      /           /
009400080612     ** --------- --- --------- ------------------------------
009500071128     **
009600071128     **
009700071128     ** ESEMPI DI CHIAMATA
009800071128     **
009900080612     ** if UBLBLSTS_GetLblTyp(pInAnno:pInLineaPar:pInSerie:pInNumSped:pOutLblTyp:
010000080613     **                       pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO:
010100080806     **                       pOutDcmBO:pOutCcaBO:pOutRblBO) = 0
010200130919     **
010300080612     **
010400080612     ** if UBLBLSTS_GetLastChild(pInAnno:pInLineaPar:pInSerie:pInNumSped:
010500080613     **                          pOutAnnoFI:pOutLineaParFI:pOutSerieFI:pOutNumSpedFI:
010600080613     **                          pOutDcmFI:pOutCcaFI) = 0
010700080612     **
010800100107     **
010900100107     ** if UBLBLSTS_GetFisrtChild(pInAnno:pInLineaPar:pInSerie:pInNumSped:
011000100107     **                          pOutAnnoFFI:pOutLineaPaFFI:pOutSerieFFI:pOutNumSpedFFI:
011100100107     **                          pOutDcmFFI:pOutCcaFFI) = 0
011200130919     **
011300130919     **
011400130919     ** if UBLBLSTS_GetMother(pInAnno:pInLineaPar:pInSerie:pInNumSped:
011500130919     **                       pOutAnnoMAM:pOutLineaPaMAM:pOutSerieMAM:pOutNumSpedMAM:
011600130919     **                       pOutDcmMAM:pOutCcaMAM) = 0
011700100107     **
011800080612     ** --------- --- --------- ------------------------------
011900071128     **
012000071128     ** ISTRUZIONI PER LA COMPILAZIONE
012100071128     **
012200080612     ** 1. Creare il modulo UBLBLSPE (Opz 15 PDM)
012300080612     ** 2. Creare/Aggiornare il programma di servizio UBLBLSPE (CRTSRVPGM / UPDSRVPGM)
012400170519     **    CRTSRVPGM SRVPGM(GAITRAOBJ/UBLBLSPE) MODULE(GAITRAOBJ/UBLBLSPE)
012500170519     **    EXPORT(*SRCFILE) SRCFILE(GAITRASRC/SRCSRVPGM)
012600170519     **    TEXT('Check sped rispetto a legami: orig/mamma/figlia')
012700170519     **    BNDDIR(UBBNDDIR) ACTGRP(*CALLER)
012800080610     ** 4. Cancellare il modulo.
012900071128     **
013000071128     ***********************************************************************************************
013100080612
013200080612
013300080612      //********************************************************************************************
013400080612      //
013500080612      // Definizione file (globali)
013600080612      //
013700080612      //********************************************************************************************
013800080612     FFNLBL01L  IF   E           K DISK    rename(fnlbl000:fnlbl001) usropn
013900080612     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002) usropn
014000101230     FTITAS30C  IF   E           K DISK    extfile(LibFileTAS)       usropn
014100101230     FFNARB01L  IF   E           K DISK    extfile(LibFileARB)       usropn
014200141104     FFNBLP01L  IF   E           K DISK    extfile(LibFileBLP)       usropn
014300080806     FTABEL00F  IF   E           K DISK                              usropn
014400071128
014500080612
014600071128      //********************************************************************************************
014700071128      //
014800080612      // Definizione prototipi procedure (globali)
014900071128      //
015000071128      //********************************************************************************************
015100080612     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
015200080616     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
015300080612
015400080612
015500071128      //********************************************************************************************
015600071128      //
015700080612      // Definizione variabili work (globali)
015800071128      //
015900071128      //********************************************************************************************
016000080612     D InitDone        S               N   INZ(*off)
016100101230     D LibFileTAS      S             21A   INZ
016200101230     D LibFileARB      S             21A   INZ
016300141104     D LibFileBLP      S             21A   INZ
016400071129
016500080612
016600080612     P*--------------------------------------------------
016700080612     P* Procedure name: UBLBLSPE_Init
016800080612     P* Purpose:        Inizializzazioni
016900080612     P* Returns:        Integer (Status_Code)
017000080612     P*--------------------------------------------------
017100080612     P*
017200080612     P UBLBLSPE_Init...
017300080612     P                 B                   EXPORT
017400080612     D*
017500080612     D UBLBLSPE_Init...
017600080612     D                 PI            10I 0
017700080612     D*
017800080612      //********************************************************************************************
017900080612      //
018000080612      // Definizione variabili work (locali)
018100080612      //
018200080612      //********************************************************************************************
018300080612     D locMethodCode   S             10I 0
018400080616     D currSysNeta     S              8A
018500080612
018600080612      /FREE
018700080612        // Inizializzo opcode di metodo
018800080612        locMethodCode = -1;
018900080612        InitDone = *off;
019000080616
019100080616        // Verifico il sistema AS/400 corrente
019200080616        if UBRTVNETA_Rtv(currSysNeta) = 0;
019300080617           if %subst(currSysNeta:1:6) = 'SETRAS';
019400101230              LibFileTAS = 'GAITRAGRU/TITAS30C';
019500101230              LibFileARB = 'FILTRA201/FNARB01L';
019600141104              LibFileBLP = 'FILTRA201/FNBLP01L';
019700080616           else;
019800101230              LibFileTAS = 'GAITRAGRPS/TITAS30C';
019900101230              LibFileARB = 'FILTRAPRD/FNARB01L';
020000141104              LibFileBLP = 'FILTRAPRD/FNBLP01L';
020100080616           endif;
020200080616
020300080616           // Se necessario apro i file
020400080616           if not %open(fnlbl01l);
020500080616              open fnlbl01l;
020600080616           endif;
020700080616           if not %open(fnlbl02l);
020800080616              open fnlbl02l;
020900080616           endif;
021000101230           if not %open(fnarb01l);
021100101230              open fnarb01l;
021200101230           endif;
021300141104           if not %open(fnblp01l);
021400141104              open fnblp01l;
021500141104           endif;
021600080616           if not %open(titas30c);
021700080616              open titas30c;
021800080616           endif;
021900080806           if not %open(tabel00f);
022000080806              open tabel00f;
022100080806           endif;
022200080612
022300080612
022400080616           // Valorizzo opcode di metodo
022500080616           locMethodCode = 0;
022600080616           InitDone = *on;
022700080616
022800080616        endif;
022900080612
023000080612        return locMethodCode;
023100080612
023200080612      /END-FREE
023300080612
023400080612     P UBLBLSPE_Init...
023500080612     P                 E
023600080612
023700080612
023800080612
023900080612     P*--------------------------------------------------
024000080612     P* Procedure name: UBLBLSPE_Finalize
024100080612     P* Purpose:        Finalizzazioni
024200080612     P* Returns:        Integer (Status_Code)
024300080612     P*--------------------------------------------------
024400080612     P*
024500080612     P UBLBLSPE_Finalize...
024600080612     P                 B                   EXPORT
024700080612     D*
024800080612     D UBLBLSPE_Finalize...
024900080612     D                 PI            10I 0
025000080612     D*
025100080612      //********************************************************************************************
025200080612      //
025300080612      // Definizione variabili work (locali)
025400080612      //
025500080612      //********************************************************************************************
025600080612     D locMethodCode   S             10I 0
025700080612
025800080612      /FREE
025900080612        // Inizializzo opcode di metodo
026000080612        locMethodCode = -1;
026100080612
026200080612        // Se necessario chiudo i file
026300080612        if %open(fnlbl01l);
026400080612           close fnlbl01l;
026500080612        endif;
026600080612        if %open(fnlbl02l);
026700080612           close fnlbl02l;
026800080612        endif;
026900101230        if %open(fnarb01l);
027000101230           close fnarb01l;
027100101230        endif;
027200141104        if %open(fnblp01l);
027300141104           close fnblp01l;
027400141104        endif;
027500080612        if %open(titas30c);
027600080612           close titas30c;
027700080612        endif;
027800080806        if %open(tabel00f);
027900080806           close tabel00f;
028000080806        endif;
028100080612
028200080612
028300080612        // Valorizzo opcode di metodo
028400080612        locMethodCode = 0;
028500080612
028600080612        return locMethodCode;
028700080612
028800080612      /END-FREE
028900080612
029000080612     P UBLBLSPE_Finalize...
029100080612     P                 E
029200080612
029300080612
029400071129
029500080611     P*--------------------------------------------------
029600080612     P* Procedure name: UBLBLSPE_GetLblTyp
029700080612     P* Purpose:        Verifica il tipo di legame della bolla corrente
029800080612     P*                 e ritorna la chiave della relativa bolla originale
029900080612     P* Returns:        Integer (Status_Code)
030000080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
030100080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
030200080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
030300080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
030400080612     P* Parameter:      pOutLblTyp     => Tipo del legame
030500080612     P* Parameter:      pOutAnnoBO     => Anno           sped. originale
030600080612     P* Parameter:      pOutLineaParBO => Linea partenza sped. originale
030700080612     P* Parameter:      pOutSerieBO    => Serie          sped. originale
030800080612     P* Parameter:      pOutNumSpedBO  => Numero         sped. originale
030900080613     P* Parameter:      pOutDcmBO      => Data consegna  sped. originale
031000080613     P* Parameter:      pOutCcaBO      => Cod. anomalia  sped. originale
031100080806     P* Parameter:      pOutRblBO      => Raggr. TBL     sped. originale
031200080611     P*--------------------------------------------------
031300080611     P*
031400080612     P UBLBLSPE_GetLblTyp...
031500071129     P                 B                   EXPORT
031600071129     D*
031700080612     D UBLBLSPE_GetLblTyp...
031800080612     D                 PI            10I 0
031900080612     D pInAnno                        4S 0 CONST
032000080612     D pInLineaPar                    3S 0 CONST
032100080612     D pInSerie                       2S 0 CONST
032200080612     D pInNumSped                     7S 0 CONST
032300080612     D pOutLblTyp                     1A
032400080612     D pOutAnnoBO                     4S 0
032500080612     D pOutLineaParBO                 3S 0
032600080612     D pOutSerieBO                    2S 0
032700080612     D pOutNumSpedBO                  7S 0
032800080613     D pOutDcmBO                      8S 0
032900080613     D pOutCcaBO                      1A
033000080806     D pOutRblBO                      1A
033100080612     D*
033200071129      //********************************************************************************************
033300071129      //
033400080612      // Definizione variabili work (locali)
033500071129      //
033600071129      //********************************************************************************************
033700080612     D locMethodCode   S             10I 0
033800080806     D skTBL_Key       S              2A   DIM(100)
033900080806     D skTBL_Dta       S             89A   DIM(100)
034000080806     D jTBL            S              3S 0
034100080806     D wjTBL           S              3S 0
034200080806     D DSTB          E DS
034300101230     D DS3A          E DS
034400071129
034500071129
034600071129      /FREE
034700080612        // Inizializzo opcode di metodo ad errore
034800080612        locMethodCode = -1;
034900080612
035000080612
035100080612        // Resetto tutti i parametri d output
035200080612        clear pOutLblTyp;
035300080612        clear pOutAnnoBO;
035400080612        clear pOutLineaParBO;
035500080612        clear pOutSerieBO;
035600080612        clear pOutNumSpedBO;
035700080613        clear pOutDcmBO;
035800080613        clear pOutCcaBO;
035900080806        clear pOutRblBO;
036000080612
036100080612
036200080612        // Verifico se già evocato metodo Init
036300080612        if not InitDone;
036400080612           UBLBLSPE_Init();
036500080612        endif;
036600080612
036700080611
036800080612        // Verifico subito se trattasi di bolla "figlia"
036900101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl01l;
037000080612        if %found(fnlbl01l);
037100080612           pOutLblTyp     = 'F';
037200080612           pOutAnnoBO     = lblAAO;
037300080612           pOutLineaParBO = lblLPO;
037400080612           pOutSerieBO    = lblNRO;
037500080612           pOutNumSpedBO  = lblNSO;
037600080612        else;
037700080612           pOutLblTyp     = 'O';
037800080612           pOutAnnoBO     = pInAnno;
037900080612           pOutLineaParBO = pInLineaPar;
038000080612           pOutSerieBO    = pInSerie;
038100080612           pOutNumSpedBO  = pInNumSped;
038200080612        endif;
038300101230
038400101230
038500101230        // Quindi reperisco lo stato corrente della bolla originale reperita
038600101230        // ...prima provo su ARB (in quanto "aggiornato" prima)
038700101230        chain (pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO) fnarb01l;
038800101230        if %found(fnarb01l);
038900101230           tasDCM = arbDCM;
039000101230           tasCCA = arbCCA;
039100101230           tasFBR = arbFBR;
039200101230           chain (1:'3A':arbCBO) tabel00f;
039300101230           if %found(tabel00f);
039400110124              DS3A   = tblUNI;
039500101230              tasTBL = §3ATB1;
039600101230           endif;
039700101230        else;
039800141104          // ...altrimenti provo su TAS
039900141104          chain (pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO) titas30c;
040000141104          if %found(titas30c);
040100141104          else;
040200141104           // ...infine provo su BLP
040300141104           chain (pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO) fnblp01l;
040400141104           if %found(fnblp01l);
040500141104              tasDCM = blpDCM;
040600141104              tasCCA = blpCCA;
040700141104              tasFBR = blpFBR;
040800141104              chain (1:'3A':blpCBO) tabel00f;
040900141104              if %found(tabel00f);
041000141104                 DS3A   = tblUNI;
041100141104                 tasTBL = §3ATB1;
041200141104              endif;
041300141104           endif;
041400141104          endif;
041500101230        endif;
041600071129
041700080613
041800080613        // Effettuo considerazioni sullo stato della bolla originale reperita
041900141104        if %found(fnarb01l) or %found(titas30c) or %found(fnblp01l);
042000080613           pOutDcmBO = tasDCM;
042100080613           pOutCcaBO = tasCCA;
042200080613           // Forzatura x casi d resi d dirottamenti
042300080613           if tasFBR = 'S';
042400080613              pOutCcaBO = '2';
042500080613           endif;
042600080613        endif;
042700080806
042800080806
042900080806        // Reperisco il raggruppamento tipo bolla
043000080806        clear DSTB;
043100080806        eval wjTBL = %lookup(tasTBL:skTBL_Key);
043200080806        if wjTBL = *zeros;
043300101230           chain (1:'TB':tasTBL) tabel00f;
043400080806           if %found(tabel00f);
043500080806              eval jTBL = jTBL + 1;
043600080806              eval skTBL_Key(jTBL) = tblKEY;
043700080806              eval skTBL_Dta(jTBL) = tblUNI;
043800080806              eval wjTBL = jTBL;
043900080806           endif;
044000080806        endif;
044100080806        if wjTBL > *zeros;
044200080806           eval DSTB = skTBL_Dta(wjTBL);
044300080806           eval pOutRblBO = §TBRBL;
044400080806        endif;
044500080806
044600080613
044700071205
044800080612        // Valorizzo opcode di metodo a ok
044900080612        locMethodCode = 0;
045000080611
045100080611        return locMethodCode;
045200071129
045300071129      /END-FREE
045400071129
045500080612     P UBLBLSPE_GetLblTyp...
045600071129     P                 E
045700080612
045800080612
045900080612
046000080612     P*--------------------------------------------------
046100080612     P* Procedure name: UBLBLSPE_GetLastChild
046200080612     P* Purpose:        Rispetto alla bolla corrente reperisce l'ultima bolla
046300080612     P*                 figlia ed il relativo stato attuale
046400080612     P* Returns:        Integer (Status_Code)
046500080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
046600080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
046700080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
046800080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
046900080612     P* Parameter:      pOutAnnoFI     => Anno           sped. ultima figlia
047000080612     P* Parameter:      pOutLineaParFI => Linea partenza sped. ultima figlia
047100080612     P* Parameter:      pOutSerieFI    => Serie          sped. ultima figlia
047200080612     P* Parameter:      pOutNumSpedFI  => Numero         sped. ultima figlia
047300080612     P* Parameter:      pOutDcmFI      => Data consegna dell'ultima bolla figlia
047400080612     P* Parameter:      pOutCcaFI      => Codice consegna anomala dell'ultima bolla figlia
047500080612     P*--------------------------------------------------
047600080612     P*
047700080612     P UBLBLSPE_GetLastChild...
047800080612     P                 B                   EXPORT
047900080612     D*
048000080612     D UBLBLSPE_GetLastChild...
048100080612     D                 PI            10I 0
048200080612     D pInAnno                        4S 0 CONST
048300080612     D pInLineaPar                    3S 0 CONST
048400080612     D pInSerie                       2S 0 CONST
048500080612     D pInNumSped                     7S 0 CONST
048600080612     D pOutAnnoFI                     4S 0
048700080612     D pOutLineaParFI                 3S 0
048800080612     D pOutSerieFI                    2S 0
048900080612     D pOutNumSpedFI                  7S 0
049000080612     D pOutDcmFI                      8S 0
049100080612     D pOutCcaFI                      1A
049200080612     D*
049300080612      //********************************************************************************************
049400080612      //
049500080612      // Definizione variabili work (locali)
049600080612      //
049700080612      //********************************************************************************************
049800080612     D locMethodCode   S             10I 0
049900080612
050000080612
050100080612      /FREE
050200080612        // Inizializzo opcode di metodo ad errore
050300080612        locMethodCode = -1;
050400080612
050500080612
050600080612        // Resetto tutti i parametri d output
050700080612        clear pOutAnnoFI;
050800080612        clear pOutLineaParFI;
050900080612        clear pOutSerieFI;
051000080612        clear pOutNumSpedFI;
051100080612        clear pOutDcmFI;
051200080612        clear pOutCcaFI;
051300080612
051400080612
051500080612        // Verifico se già evocato metodo Init
051600080612        if not InitDone;
051700080612           UBLBLSPE_Init();
051800080612        endif;
051900080612
052000080612
052100080612        // Reperisco ultima figlia legata alla bolla corrente
052200101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl02l;
052300080612        if %found(fnlbl02l);
052400170519           chain  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
052500170519           dow %found(fnlbl02l);
052600170519               chain  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
052700170519           enddo;
052800080613
052900080613           // Valorizzo i parametri di output
053000080613           pOutAnnoFI     = lblAAN;
053100080613           pOutLineaParFI = lblLPN;
053200080613           pOutSerieFI    = lblNRN;
053300080613           pOutNumSpedFI  = lblNSN;
053400080613
053500101230
053600101230           // Quindi reperisco lo stato corrente dell'ultima bolla figlia appena reperita
053700101230           // ...prima provo su ARB (in quanto "aggiornato" prima)
053800101230           chain (lblAAN:lblLPN:lblNRN:lblNSN) fnarb01l;
053900101230           if %found(fnarb01l);
054000101230              tasDCM = arbDCM;
054100101230              tasCCA = arbCCA;
054200101230              tasFBR = arbFBR;
054300101230           else;
054400101230           // ...altrimenti provo su TAS
054500101230              chain (lblAAN:lblLPN:lblNRN:lblNSN) titas30c;
054600101230           endif;
054700101230
054800101230
054900101230           if %found(fnarb01l) or %found(titas30c);
055000080612              pOutDcmFI = tasDCM;
055100080612              pOutCcaFI = tasCCA;
055200080612              // Forzatura x casi d resi d dirottamenti
055300080612              if tasFBR = 'S';
055400080612                 pOutCcaFI = '2';
055500080612              endif;
055600080612           endif;
055700081112        else;
055800081112           locMethodCode = 9;
055900081112        return locMethodCode;
056000080612        endif;
056100080612
056200080612
056300080612        // Valorizzo opcode di metodo a ok
056400080612        locMethodCode = 0;
056500080612        return locMethodCode;
056600080612
056700080612      /END-FREE
056800080612
056900080612     P UBLBLSPE_GetLastChild...
057000080612     P                 E
057100100107
057200100107
057300100107
057400100107     P*--------------------------------------------------
057500100107     P* Procedure name: UBLBLSPE_GetFirstChild
057600100107     P* Purpose:        Rispetto alla bolla corrente reperisce la prima bolla
057700100107     P*                 figlia ed il relativo stato attuale
057800100107     P* Returns:        Integer (Status_Code)
057900100107     P* Parameter:      pInAnno        => Anno           sped. da verificare
058000100107     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
058100100107     P* Parameter:      pInSerie       => Serie          sped. da verificare
058200100107     P* Parameter:      pInNumSped     => Numero         sped. da verificare
058300100107     P* Parameter:      pOutAnnoFFI    => Anno           sped. prima  figlia
058400100107     P* Parameter:      pOutLineaPaFFI => Linea partenza sped. prima  figlia
058500100107     P* Parameter:      pOutSerieFFI   => Serie          sped. prima  figlia
058600100107     P* Parameter:      pOutNumSpedFFI => Numero         sped. prima  figlia
058700100107     P* Parameter:      pOutDcmFFI     => Data consegna della prima bolla figlia
058800100107     P* Parameter:      pOutCcaFFI     => Codice consegna anomala della prima bolla figlia
058900100107     P*--------------------------------------------------
059000100107     P*
059100100107     P UBLBLSPE_GetFirstChild...
059200100107     P                 B                   EXPORT
059300100107     D*
059400100107     D UBLBLSPE_GetFirstChild...
059500100107     D                 PI            10I 0
059600100107     D pInAnno                        4S 0 CONST
059700100107     D pInLineaPar                    3S 0 CONST
059800100107     D pInSerie                       2S 0 CONST
059900100107     D pInNumSped                     7S 0 CONST
060000100107     D pOutAnnoFFI                    4S 0
060100100107     D pOutLineaPaFFI                 3S 0
060200100107     D pOutSerieFFI                   2S 0
060300100107     D pOutNumSpedFFI                 7S 0
060400100107     D pOutDcmFFI                     8S 0
060500100107     D pOutCcaFFI                     1A
060600100107     D*
060700100107      //********************************************************************************************
060800100107      //
060900100107      // Definizione variabili work (locali)
061000100107      //
061100100107      //********************************************************************************************
061200100107     D locMethodCode   S             10I 0
061300100107
061400100107
061500100107      /FREE
061600100107        // Inizializzo opcode di metodo ad errore
061700100107        locMethodCode = -1;
061800100107
061900100107
062000100107        // Resetto tutti i parametri d output
062100100107        clear pOutAnnoFFI;
062200100107        clear pOutLineaPaFFI;
062300100107        clear pOutSerieFFI;
062400100107        clear pOutNumSpedFFI;
062500100107        clear pOutDcmFFI;
062600100107        clear pOutCcaFFI;
062700100107
062800100107
062900100107        // Verifico se già evocato metodo Init
063000100107        if not InitDone;
063100100107           UBLBLSPE_Init();
063200100107        endif;
063300100107
063400100107
063500100107        // Reperisco prima figlia legata alla bolla corrente
063600101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl02l;
063700100107        if %found(fnlbl02l);
063800100107
063900100107           // Valorizzo i parametri di output
064000100107           pOutAnnoFFI    = lblAAN;
064100100107           pOutLineaPaFFI = lblLPN;
064200100107           pOutSerieFFI   = lblNRN;
064300100107           pOutNumSpedFFI = lblNSN;
064400101230
064500101230
064600101230           // Quindi reperisco lo stato corrente dell'ultima bolla figlia appena reperita
064700101230           // ...prima provo su ARB (in quanto "aggiornato" prima)
064800101230           chain (lblAAN:lblLPN:lblNRN:lblNSN) fnarb01l;
064900101230           if %found(fnarb01l);
065000101230              tasDCM = arbDCM;
065100101230              tasCCA = arbCCA;
065200101230              tasFBR = arbFBR;
065300101230           else;
065400101230           // ...altrimenti provo su TAS
065500101230              chain (lblAAN:lblLPN:lblNRN:lblNSN) titas30c;
065600101230           endif;
065700101230
065800101230
065900101230           if %found(fnarb01l) or %found(titas30c);
066000100107              pOutDcmFFI = tasDCM;
066100100107              pOutCcaFFI = tasCCA;
066200100107              // Forzatura x casi d resi d dirottamenti
066300100107              if tasFBR = 'S';
066400100107                 pOutCcaFFI = '2';
066500100107              endif;
066600100107           endif;
066700100107        else;
066800100107           locMethodCode = 9;
066900100107        return locMethodCode;
067000100107        endif;
067100100107
067200100107
067300100107        // Valorizzo opcode di metodo a ok
067400100107        locMethodCode = 0;
067500100107        return locMethodCode;
067600100107
067700100107      /END-FREE
067800100107
067900100107     P UBLBLSPE_GetFirstChild...
068000100107     P                 E
068100130919
068200130919
068300130919
068400130919     P*--------------------------------------------------
068500130919     P* Procedure name: UBLBLSPE_GetMother
068600130919     P* Purpose:        Rispetto alla bolla corrente reperisce la propria
068700130919     P*                 bolla mamma ed il relativo stato attuale
068800130919     P* Returns:        Integer (Status_Code)
068900130919     P* Parameter:      pInAnno        => Anno           sped. da verificare
069000130919     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
069100130919     P* Parameter:      pInSerie       => Serie          sped. da verificare
069200130919     P* Parameter:      pInNumSped     => Numero         sped. da verificare
069300130919     P* Parameter:      pOutAnnoMAM    => Anno           sped. mamma
069400130919     P* Parameter:      pOutLineaPaMAM => Linea partenza sped. mamma
069500130919     P* Parameter:      pOutSerieMAM   => Serie          sped. mamma
069600130919     P* Parameter:      pOutNumSpedMAM => Numero         sped. mamma
069700130919     P* Parameter:      pOutDcmMAM     => Data consegna della propria bolla mamma
069800130919     P* Parameter:      pOutCcaMAM     => Codice consegna anomala della propria bolla mamma
069900130919     P*--------------------------------------------------
070000130919     P*
070100130919     P UBLBLSPE_GetMother...
070200130919     P                 B                   EXPORT
070300130919     D*
070400130919     D UBLBLSPE_GetMother...
070500130919     D                 PI            10I 0
070600130919     D pInAnno                        4S 0 CONST
070700130919     D pInLineaPar                    3S 0 CONST
070800130919     D pInSerie                       2S 0 CONST
070900130919     D pInNumSped                     7S 0 CONST
071000130919     D pOutAnnoMAM                    4S 0
071100130919     D pOutLineaPaMAM                 3S 0
071200130919     D pOutSerieMAM                   2S 0
071300130919     D pOutNumSpedMAM                 7S 0
071400130919     D pOutDcmMAM                     8S 0
071500130919     D pOutCcaMAM                     1A
071600130919     D*
071700130919      //********************************************************************************************
071800130919      //
071900130919      // Definizione variabili work (locali)
072000130919      //
072100130919      //********************************************************************************************
072200130919     D locMethodCode   S             10I 0
072300130919
072400130919
072500130919      /FREE
072600130919        // Inizializzo opcode di metodo ad errore
072700130919        locMethodCode = -1;
072800130919
072900130919
073000130919        // Resetto tutti i parametri d output
073100130919        clear pOutAnnoMAM;
073200130919        clear pOutLineaPaMAM;
073300130919        clear pOutSerieMAM;
073400130919        clear pOutNumSpedMAM;
073500130919        clear pOutDcmMAM;
073600130919        clear pOutCcaMAM;
073700130919
073800130919
073900130919        // Verifico se già evocato metodo Init
074000130919        if not InitDone;
074100130919           UBLBLSPE_Init();
074200130919        endif;
074300130919
074400130919
074500130919        // Reperisco propria bolla mamma legata alla bolla corrente
074600130919        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl01l;
074700130919        if %found(fnlbl01l);
074800130919
074900130919           // Valorizzo i parametri di output
075000130919           pOutAnnoMAM    = lblAAP;
075100130919           pOutLineaPaMAM = lblLPP;
075200130919           pOutSerieMAM   = lblNRP;
075300130919           pOutNumSpedMAM = lblNSP;
075400130919
075500130919
075600130919           // Quindi reperisco lo stato corrente della bolla mamma appena reperita
075700130919           // ...prima provo su ARB (in quanto "aggiornato" prima)
075800130919           chain (lblAAP:lblLPP:lblNRP:lblNSP) fnarb01l;
075900130919           if %found(fnarb01l);
076000130919              tasDCM = arbDCM;
076100130919              tasCCA = arbCCA;
076200130919              tasFBR = arbFBR;
076300130919           else;
076400130919           // ...altrimenti provo su TAS
076500130919              chain (lblAAP:lblLPP:lblNRP:lblNSP) titas30c;
076600130919           endif;
076700130919
076800130919
076900130919           if %found(fnarb01l) or %found(titas30c);
077000130919              pOutDcmMAM = tasDCM;
077100130919              pOutCcaMAM = tasCCA;
077200130919              // Forzatura x casi d resi d dirottamenti
077300130919              if tasFBR = 'S';
077400130919                 pOutCcaMAM = '2';
077500130919              endif;
077600130919           endif;
077700130919        else;
077800130919           locMethodCode = 9;
077900130919        return locMethodCode;
078000130919        endif;
078100130919
078200130919
078300130919        // Valorizzo opcode di metodo a ok
078400130919        locMethodCode = 0;
078500130919        return locMethodCode;
078600130919
078700130919      /END-FREE
078800130919
078900130919     P UBLBLSPE_GetMother...
079000130919     P                 E
