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
012400080610     ** 4. Cancellare il modulo.
012500071128     **
012600071128     ***********************************************************************************************
012700080612
012800080612
012900080612      //********************************************************************************************
013000080612      //
013100080612      // Definizione file (globali)
013200080612      //
013300080612      //********************************************************************************************
013400080612     FFNLBL01L  IF   E           K DISK    rename(fnlbl000:fnlbl001) usropn
013500080612     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002) usropn
013600101230     FTITAS30C  IF   E           K DISK    extfile(LibFileTAS)       usropn
013700101230     FFNARB01L  IF   E           K DISK    extfile(LibFileARB)       usropn
013800080806     FTABEL00F  IF   E           K DISK                              usropn
013900071128
014000080612
014100071128      //********************************************************************************************
014200071128      //
014300080612      // Definizione prototipi procedure (globali)
014400071128      //
014500071128      //********************************************************************************************
014600080612     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
014700080616     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014800080612
014900080612
015000071128      //********************************************************************************************
015100071128      //
015200080612      // Definizione variabili work (globali)
015300071128      //
015400071128      //********************************************************************************************
015500080612     D InitDone        S               N   INZ(*off)
015600101230     D LibFileTAS      S             21A   INZ
015700101230     D LibFileARB      S             21A   INZ
015800071129
015900080612
016000080612     P*--------------------------------------------------
016100080612     P* Procedure name: UBLBLSPE_Init
016200080612     P* Purpose:        Inizializzazioni
016300080612     P* Returns:        Integer (Status_Code)
016400080612     P*--------------------------------------------------
016500080612     P*
016600080612     P UBLBLSPE_Init...
016700080612     P                 B                   EXPORT
016800080612     D*
016900080612     D UBLBLSPE_Init...
017000080612     D                 PI            10I 0
017100080612     D*
017200080612      //********************************************************************************************
017300080612      //
017400080612      // Definizione variabili work (locali)
017500080612      //
017600080612      //********************************************************************************************
017700080612     D locMethodCode   S             10I 0
017800080616     D currSysNeta     S              8A
017900080612
018000080612      /FREE
018100080612        // Inizializzo opcode di metodo
018200080612        locMethodCode = -1;
018300080612        InitDone = *off;
018400080616
018500080616        // Verifico il sistema AS/400 corrente
018600080616        if UBRTVNETA_Rtv(currSysNeta) = 0;
018700080617           if %subst(currSysNeta:1:6) = 'SETRAS';
018800101230              LibFileTAS = 'GAITRAGRU/TITAS30C';
018900101230              LibFileARB = 'FILTRA201/FNARB01L';
019000080616           else;
019100101230              LibFileTAS = 'GAITRAGRPS/TITAS30C';
019200101230              LibFileARB = 'FILTRAPRD/FNARB01L';
019300080616           endif;
019400080616
019500080616           // Se necessario apro i file
019600080616           if not %open(fnlbl01l);
019700080616              open fnlbl01l;
019800080616           endif;
019900080616           if not %open(fnlbl02l);
020000080616              open fnlbl02l;
020100080616           endif;
020200101230           if not %open(fnarb01l);
020300101230              open fnarb01l;
020400101230           endif;
020500080616           if not %open(titas30c);
020600080616              open titas30c;
020700080616           endif;
020800080806           if not %open(tabel00f);
020900080806              open tabel00f;
021000080806           endif;
021100080612
021200080612
021300080616           // Valorizzo opcode di metodo
021400080616           locMethodCode = 0;
021500080616           InitDone = *on;
021600080616
021700080616        endif;
021800080612
021900080612        return locMethodCode;
022000080612
022100080612      /END-FREE
022200080612
022300080612     P UBLBLSPE_Init...
022400080612     P                 E
022500080612
022600080612
022700080612
022800080612     P*--------------------------------------------------
022900080612     P* Procedure name: UBLBLSPE_Finalize
023000080612     P* Purpose:        Finalizzazioni
023100080612     P* Returns:        Integer (Status_Code)
023200080612     P*--------------------------------------------------
023300080612     P*
023400080612     P UBLBLSPE_Finalize...
023500080612     P                 B                   EXPORT
023600080612     D*
023700080612     D UBLBLSPE_Finalize...
023800080612     D                 PI            10I 0
023900080612     D*
024000080612      //********************************************************************************************
024100080612      //
024200080612      // Definizione variabili work (locali)
024300080612      //
024400080612      //********************************************************************************************
024500080612     D locMethodCode   S             10I 0
024600080612
024700080612      /FREE
024800080612        // Inizializzo opcode di metodo
024900080612        locMethodCode = -1;
025000080612
025100080612        // Se necessario chiudo i file
025200080612        if %open(fnlbl01l);
025300080612           close fnlbl01l;
025400080612        endif;
025500080612        if %open(fnlbl02l);
025600080612           close fnlbl02l;
025700080612        endif;
025800101230        if %open(fnarb01l);
025900101230           close fnarb01l;
026000101230        endif;
026100080612        if %open(titas30c);
026200080612           close titas30c;
026300080612        endif;
026400080806        if %open(tabel00f);
026500080806           close tabel00f;
026600080806        endif;
026700080612
026800080612
026900080612        // Valorizzo opcode di metodo
027000080612        locMethodCode = 0;
027100080612
027200080612        return locMethodCode;
027300080612
027400080612      /END-FREE
027500080612
027600080612     P UBLBLSPE_Finalize...
027700080612     P                 E
027800080612
027900080612
028000071129
028100080611     P*--------------------------------------------------
028200080612     P* Procedure name: UBLBLSPE_GetLblTyp
028300080612     P* Purpose:        Verifica il tipo di legame della bolla corrente
028400080612     P*                 e ritorna la chiave della relativa bolla originale
028500080612     P* Returns:        Integer (Status_Code)
028600080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
028700080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
028800080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
028900080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
029000080612     P* Parameter:      pOutLblTyp     => Tipo del legame
029100080612     P* Parameter:      pOutAnnoBO     => Anno           sped. originale
029200080612     P* Parameter:      pOutLineaParBO => Linea partenza sped. originale
029300080612     P* Parameter:      pOutSerieBO    => Serie          sped. originale
029400080612     P* Parameter:      pOutNumSpedBO  => Numero         sped. originale
029500080613     P* Parameter:      pOutDcmBO      => Data consegna  sped. originale
029600080613     P* Parameter:      pOutCcaBO      => Cod. anomalia  sped. originale
029700080806     P* Parameter:      pOutRblBO      => Raggr. TBL     sped. originale
029800080611     P*--------------------------------------------------
029900080611     P*
030000080612     P UBLBLSPE_GetLblTyp...
030100071129     P                 B                   EXPORT
030200071129     D*
030300080612     D UBLBLSPE_GetLblTyp...
030400080612     D                 PI            10I 0
030500080612     D pInAnno                        4S 0 CONST
030600080612     D pInLineaPar                    3S 0 CONST
030700080612     D pInSerie                       2S 0 CONST
030800080612     D pInNumSped                     7S 0 CONST
030900080612     D pOutLblTyp                     1A
031000080612     D pOutAnnoBO                     4S 0
031100080612     D pOutLineaParBO                 3S 0
031200080612     D pOutSerieBO                    2S 0
031300080612     D pOutNumSpedBO                  7S 0
031400080613     D pOutDcmBO                      8S 0
031500080613     D pOutCcaBO                      1A
031600080806     D pOutRblBO                      1A
031700080612     D*
031800071129      //********************************************************************************************
031900071129      //
032000080612      // Definizione variabili work (locali)
032100071129      //
032200071129      //********************************************************************************************
032300080612     D locMethodCode   S             10I 0
032400080806     D skTBL_Key       S              2A   DIM(100)
032500080806     D skTBL_Dta       S             89A   DIM(100)
032600080806     D jTBL            S              3S 0
032700080806     D wjTBL           S              3S 0
032800080806     D DSTB          E DS
032900101230     D DS3A          E DS
033000071129
033100071129
033200071129      /FREE
033300080612        // Inizializzo opcode di metodo ad errore
033400080612        locMethodCode = -1;
033500080612
033600080612
033700080612        // Resetto tutti i parametri d output
033800080612        clear pOutLblTyp;
033900080612        clear pOutAnnoBO;
034000080612        clear pOutLineaParBO;
034100080612        clear pOutSerieBO;
034200080612        clear pOutNumSpedBO;
034300080613        clear pOutDcmBO;
034400080613        clear pOutCcaBO;
034500080806        clear pOutRblBO;
034600080612
034700080612
034800080612        // Verifico se già evocato metodo Init
034900080612        if not InitDone;
035000080612           UBLBLSPE_Init();
035100080612        endif;
035200080612
035300080611
035400080612        // Verifico subito se trattasi di bolla "figlia"
035500101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl01l;
035600080612        if %found(fnlbl01l);
035700080612           pOutLblTyp     = 'F';
035800080612           pOutAnnoBO     = lblAAO;
035900080612           pOutLineaParBO = lblLPO;
036000080612           pOutSerieBO    = lblNRO;
036100080612           pOutNumSpedBO  = lblNSO;
036200080612        else;
036300080612           pOutLblTyp     = 'O';
036400080612           pOutAnnoBO     = pInAnno;
036500080612           pOutLineaParBO = pInLineaPar;
036600080612           pOutSerieBO    = pInSerie;
036700080612           pOutNumSpedBO  = pInNumSped;
036800080612        endif;
036900101230
037000101230
037100101230        // Quindi reperisco lo stato corrente della bolla originale reperita
037200101230        // ...prima provo su ARB (in quanto "aggiornato" prima)
037300101230        chain (pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO) fnarb01l;
037400101230        if %found(fnarb01l);
037500101230           tasDCM = arbDCM;
037600101230           tasCCA = arbCCA;
037700101230           tasFBR = arbFBR;
037800101230           chain (1:'3A':arbCBO) tabel00f;
037900101230           if %found(tabel00f);
038000110124              DS3A   = tblUNI;
038100101230              tasTBL = §3ATB1;
038200101230           endif;
038300101230        else;
038400101230        // ...altrimenti provo su TAS
038500101230           chain (pOutAnnoBO:pOutLineaParBO:pOutSerieBO:pOutNumSpedBO) titas30c;
038600101230        endif;
038700071129
038800080613
038900080613        // Effettuo considerazioni sullo stato della bolla originale reperita
039000101230        if %found(fnarb01l) or %found(titas30c);
039100080613           pOutDcmBO = tasDCM;
039200080613           pOutCcaBO = tasCCA;
039300080613           // Forzatura x casi d resi d dirottamenti
039400080613           if tasFBR = 'S';
039500080613              pOutCcaBO = '2';
039600080613           endif;
039700080613        endif;
039800080806
039900080806
040000080806        // Reperisco il raggruppamento tipo bolla
040100080806        clear DSTB;
040200080806        eval wjTBL = %lookup(tasTBL:skTBL_Key);
040300080806        if wjTBL = *zeros;
040400101230           chain (1:'TB':tasTBL) tabel00f;
040500080806           if %found(tabel00f);
040600080806              eval jTBL = jTBL + 1;
040700080806              eval skTBL_Key(jTBL) = tblKEY;
040800080806              eval skTBL_Dta(jTBL) = tblUNI;
040900080806              eval wjTBL = jTBL;
041000080806           endif;
041100080806        endif;
041200080806        if wjTBL > *zeros;
041300080806           eval DSTB = skTBL_Dta(wjTBL);
041400080806           eval pOutRblBO = §TBRBL;
041500080806        endif;
041600080806
041700080613
041800071205
041900080612        // Valorizzo opcode di metodo a ok
042000080612        locMethodCode = 0;
042100080611
042200080611        return locMethodCode;
042300071129
042400071129      /END-FREE
042500071129
042600080612     P UBLBLSPE_GetLblTyp...
042700071129     P                 E
042800080612
042900080612
043000080612
043100080612     P*--------------------------------------------------
043200080612     P* Procedure name: UBLBLSPE_GetLastChild
043300080612     P* Purpose:        Rispetto alla bolla corrente reperisce l'ultima bolla
043400080612     P*                 figlia ed il relativo stato attuale
043500080612     P* Returns:        Integer (Status_Code)
043600080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
043700080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
043800080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
043900080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
044000080612     P* Parameter:      pOutAnnoFI     => Anno           sped. ultima figlia
044100080612     P* Parameter:      pOutLineaParFI => Linea partenza sped. ultima figlia
044200080612     P* Parameter:      pOutSerieFI    => Serie          sped. ultima figlia
044300080612     P* Parameter:      pOutNumSpedFI  => Numero         sped. ultima figlia
044400080612     P* Parameter:      pOutDcmFI      => Data consegna dell'ultima bolla figlia
044500080612     P* Parameter:      pOutCcaFI      => Codice consegna anomala dell'ultima bolla figlia
044600080612     P*--------------------------------------------------
044700080612     P*
044800080612     P UBLBLSPE_GetLastChild...
044900080612     P                 B                   EXPORT
045000080612     D*
045100080612     D UBLBLSPE_GetLastChild...
045200080612     D                 PI            10I 0
045300080612     D pInAnno                        4S 0 CONST
045400080612     D pInLineaPar                    3S 0 CONST
045500080612     D pInSerie                       2S 0 CONST
045600080612     D pInNumSped                     7S 0 CONST
045700080612     D pOutAnnoFI                     4S 0
045800080612     D pOutLineaParFI                 3S 0
045900080612     D pOutSerieFI                    2S 0
046000080612     D pOutNumSpedFI                  7S 0
046100080612     D pOutDcmFI                      8S 0
046200080612     D pOutCcaFI                      1A
046300080612     D*
046400080612      //********************************************************************************************
046500080612      //
046600080612      // Definizione variabili work (locali)
046700080612      //
046800080612      //********************************************************************************************
046900080612     D locMethodCode   S             10I 0
047000080612
047100080612
047200080612      /FREE
047300080612        // Inizializzo opcode di metodo ad errore
047400080612        locMethodCode = -1;
047500080612
047600080612
047700080612        // Resetto tutti i parametri d output
047800080612        clear pOutAnnoFI;
047900080612        clear pOutLineaParFI;
048000080612        clear pOutSerieFI;
048100080612        clear pOutNumSpedFI;
048200080612        clear pOutDcmFI;
048300080612        clear pOutCcaFI;
048400080612
048500080612
048600080612        // Verifico se già evocato metodo Init
048700080612        if not InitDone;
048800080612           UBLBLSPE_Init();
048900080612        endif;
049000080612
049100080612
049200080612        // Reperisco ultima figlia legata alla bolla corrente
049300101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl02l;
049400080612        if %found(fnlbl02l);
049500080612           setll  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
049600080612           if %equal(fnlbl02l);
049700080612              reade  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
049800080612              dow not %eof(fnlbl02l);
049900080612                  reade  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
050000080612              enddo;
050100080612           endif;
050200080613
050300080613           // Valorizzo i parametri di output
050400080613           pOutAnnoFI     = lblAAN;
050500080613           pOutLineaParFI = lblLPN;
050600080613           pOutSerieFI    = lblNRN;
050700080613           pOutNumSpedFI  = lblNSN;
050800080613
050900101230
051000101230           // Quindi reperisco lo stato corrente dell'ultima bolla figlia appena reperita
051100101230           // ...prima provo su ARB (in quanto "aggiornato" prima)
051200101230           chain (lblAAN:lblLPN:lblNRN:lblNSN) fnarb01l;
051300101230           if %found(fnarb01l);
051400101230              tasDCM = arbDCM;
051500101230              tasCCA = arbCCA;
051600101230              tasFBR = arbFBR;
051700101230           else;
051800101230           // ...altrimenti provo su TAS
051900101230              chain (lblAAN:lblLPN:lblNRN:lblNSN) titas30c;
052000101230           endif;
052100101230
052200101230
052300101230           if %found(fnarb01l) or %found(titas30c);
052400080612              pOutDcmFI = tasDCM;
052500080612              pOutCcaFI = tasCCA;
052600080612              // Forzatura x casi d resi d dirottamenti
052700080612              if tasFBR = 'S';
052800080612                 pOutCcaFI = '2';
052900080612              endif;
053000080612           endif;
053100081112        else;
053200081112           locMethodCode = 9;
053300081112        return locMethodCode;
053400080612        endif;
053500080612
053600080612
053700080612        // Valorizzo opcode di metodo a ok
053800080612        locMethodCode = 0;
053900080612        return locMethodCode;
054000080612
054100080612      /END-FREE
054200080612
054300080612     P UBLBLSPE_GetLastChild...
054400080612     P                 E
054500100107
054600100107
054700100107
054800100107     P*--------------------------------------------------
054900100107     P* Procedure name: UBLBLSPE_GetFirstChild
055000100107     P* Purpose:        Rispetto alla bolla corrente reperisce la prima bolla
055100100107     P*                 figlia ed il relativo stato attuale
055200100107     P* Returns:        Integer (Status_Code)
055300100107     P* Parameter:      pInAnno        => Anno           sped. da verificare
055400100107     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
055500100107     P* Parameter:      pInSerie       => Serie          sped. da verificare
055600100107     P* Parameter:      pInNumSped     => Numero         sped. da verificare
055700100107     P* Parameter:      pOutAnnoFFI    => Anno           sped. prima  figlia
055800100107     P* Parameter:      pOutLineaPaFFI => Linea partenza sped. prima  figlia
055900100107     P* Parameter:      pOutSerieFFI   => Serie          sped. prima  figlia
056000100107     P* Parameter:      pOutNumSpedFFI => Numero         sped. prima  figlia
056100100107     P* Parameter:      pOutDcmFFI     => Data consegna della prima bolla figlia
056200100107     P* Parameter:      pOutCcaFFI     => Codice consegna anomala della prima bolla figlia
056300100107     P*--------------------------------------------------
056400100107     P*
056500100107     P UBLBLSPE_GetFirstChild...
056600100107     P                 B                   EXPORT
056700100107     D*
056800100107     D UBLBLSPE_GetFirstChild...
056900100107     D                 PI            10I 0
057000100107     D pInAnno                        4S 0 CONST
057100100107     D pInLineaPar                    3S 0 CONST
057200100107     D pInSerie                       2S 0 CONST
057300100107     D pInNumSped                     7S 0 CONST
057400100107     D pOutAnnoFFI                    4S 0
057500100107     D pOutLineaPaFFI                 3S 0
057600100107     D pOutSerieFFI                   2S 0
057700100107     D pOutNumSpedFFI                 7S 0
057800100107     D pOutDcmFFI                     8S 0
057900100107     D pOutCcaFFI                     1A
058000100107     D*
058100100107      //********************************************************************************************
058200100107      //
058300100107      // Definizione variabili work (locali)
058400100107      //
058500100107      //********************************************************************************************
058600100107     D locMethodCode   S             10I 0
058700100107
058800100107
058900100107      /FREE
059000100107        // Inizializzo opcode di metodo ad errore
059100100107        locMethodCode = -1;
059200100107
059300100107
059400100107        // Resetto tutti i parametri d output
059500100107        clear pOutAnnoFFI;
059600100107        clear pOutLineaPaFFI;
059700100107        clear pOutSerieFFI;
059800100107        clear pOutNumSpedFFI;
059900100107        clear pOutDcmFFI;
060000100107        clear pOutCcaFFI;
060100100107
060200100107
060300100107        // Verifico se già evocato metodo Init
060400100107        if not InitDone;
060500100107           UBLBLSPE_Init();
060600100107        endif;
060700100107
060800100107
060900100107        // Reperisco prima figlia legata alla bolla corrente
061000101230        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl02l;
061100100107        if %found(fnlbl02l);
061200100107           setll  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
061300100107           if %equal(fnlbl02l);
061400100107              reade  (lblAAN:lblLPN:lblNRN:lblNSN) fnlbl02l;
061500100107           endif;
061600100107
061700100107           // Valorizzo i parametri di output
061800100107           pOutAnnoFFI    = lblAAN;
061900100107           pOutLineaPaFFI = lblLPN;
062000100107           pOutSerieFFI   = lblNRN;
062100100107           pOutNumSpedFFI = lblNSN;
062200101230
062300101230
062400101230           // Quindi reperisco lo stato corrente dell'ultima bolla figlia appena reperita
062500101230           // ...prima provo su ARB (in quanto "aggiornato" prima)
062600101230           chain (lblAAN:lblLPN:lblNRN:lblNSN) fnarb01l;
062700101230           if %found(fnarb01l);
062800101230              tasDCM = arbDCM;
062900101230              tasCCA = arbCCA;
063000101230              tasFBR = arbFBR;
063100101230           else;
063200101230           // ...altrimenti provo su TAS
063300101230              chain (lblAAN:lblLPN:lblNRN:lblNSN) titas30c;
063400101230           endif;
063500101230
063600101230
063700101230           if %found(fnarb01l) or %found(titas30c);
063800100107              pOutDcmFFI = tasDCM;
063900100107              pOutCcaFFI = tasCCA;
064000100107              // Forzatura x casi d resi d dirottamenti
064100100107              if tasFBR = 'S';
064200100107                 pOutCcaFFI = '2';
064300100107              endif;
064400100107           endif;
064500100107        else;
064600100107           locMethodCode = 9;
064700100107        return locMethodCode;
064800100107        endif;
064900100107
065000100107
065100100107        // Valorizzo opcode di metodo a ok
065200100107        locMethodCode = 0;
065300100107        return locMethodCode;
065400100107
065500100107      /END-FREE
065600100107
065700100107     P UBLBLSPE_GetFirstChild...
065800100107     P                 E
065900130919
066000130919
066100130919
066200130919     P*--------------------------------------------------
066300130919     P* Procedure name: UBLBLSPE_GetMother
066400130919     P* Purpose:        Rispetto alla bolla corrente reperisce la propria
066500130919     P*                 bolla mamma ed il relativo stato attuale
066600130919     P* Returns:        Integer (Status_Code)
066700130919     P* Parameter:      pInAnno        => Anno           sped. da verificare
066800130919     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
066900130919     P* Parameter:      pInSerie       => Serie          sped. da verificare
067000130919     P* Parameter:      pInNumSped     => Numero         sped. da verificare
067100130919     P* Parameter:      pOutAnnoMAM    => Anno           sped. mamma
067200130919     P* Parameter:      pOutLineaPaMAM => Linea partenza sped. mamma
067300130919     P* Parameter:      pOutSerieMAM   => Serie          sped. mamma
067400130919     P* Parameter:      pOutNumSpedMAM => Numero         sped. mamma
067500130919     P* Parameter:      pOutDcmMAM     => Data consegna della propria bolla mamma
067600130919     P* Parameter:      pOutCcaMAM     => Codice consegna anomala della propria bolla mamma
067700130919     P*--------------------------------------------------
067800130919     P*
067900130919     P UBLBLSPE_GetMother...
068000130919     P                 B                   EXPORT
068100130919     D*
068200130919     D UBLBLSPE_GetMother...
068300130919     D                 PI            10I 0
068400130919     D pInAnno                        4S 0 CONST
068500130919     D pInLineaPar                    3S 0 CONST
068600130919     D pInSerie                       2S 0 CONST
068700130919     D pInNumSped                     7S 0 CONST
068800130919     D pOutAnnoMAM                    4S 0
068900130919     D pOutLineaPaMAM                 3S 0
069000130919     D pOutSerieMAM                   2S 0
069100130919     D pOutNumSpedMAM                 7S 0
069200130919     D pOutDcmMAM                     8S 0
069300130919     D pOutCcaMAM                     1A
069400130919     D*
069500130919      //********************************************************************************************
069600130919      //
069700130919      // Definizione variabili work (locali)
069800130919      //
069900130919      //********************************************************************************************
070000130919     D locMethodCode   S             10I 0
070100130919
070200130919
070300130919      /FREE
070400130919        // Inizializzo opcode di metodo ad errore
070500130919        locMethodCode = -1;
070600130919
070700130919
070800130919        // Resetto tutti i parametri d output
070900130919        clear pOutAnnoMAM;
071000130919        clear pOutLineaPaMAM;
071100130919        clear pOutSerieMAM;
071200130919        clear pOutNumSpedMAM;
071300130919        clear pOutDcmMAM;
071400130919        clear pOutCcaMAM;
071500130919
071600130919
071700130919        // Verifico se già evocato metodo Init
071800130919        if not InitDone;
071900130919           UBLBLSPE_Init();
072000130919        endif;
072100130919
072200130919
072300130919        // Reperisco propria bolla mamma legata alla bolla corrente
072400130919        chain (pInAnno:pInLineaPar:pInSerie:pInNumSped) fnlbl01l;
072500130919        if %found(fnlbl01l);
072600130919
072700130919           // Valorizzo i parametri di output
072800130919           pOutAnnoMAM    = lblAAP;
072900130919           pOutLineaPaMAM = lblLPP;
073000130919           pOutSerieMAM   = lblNRP;
073100130919           pOutNumSpedMAM = lblNSP;
073200130919
073300130919
073400130919           // Quindi reperisco lo stato corrente della bolla mamma appena reperita
073500130919           // ...prima provo su ARB (in quanto "aggiornato" prima)
073600130919           chain (lblAAP:lblLPP:lblNRP:lblNSP) fnarb01l;
073700130919           if %found(fnarb01l);
073800130919              tasDCM = arbDCM;
073900130919              tasCCA = arbCCA;
074000130919              tasFBR = arbFBR;
074100130919           else;
074200130919           // ...altrimenti provo su TAS
074300130919              chain (lblAAP:lblLPP:lblNRP:lblNSP) titas30c;
074400130919           endif;
074500130919
074600130919
074700130919           if %found(fnarb01l) or %found(titas30c);
074800130919              pOutDcmMAM = tasDCM;
074900130919              pOutCcaMAM = tasCCA;
075000130919              // Forzatura x casi d resi d dirottamenti
075100130919              if tasFBR = 'S';
075200130919                 pOutCcaMAM = '2';
075300130919              endif;
075400130919           endif;
075500130919        else;
075600130919           locMethodCode = 9;
075700130919        return locMethodCode;
075800130919        endif;
075900130919
076000130919
076100130919        // Valorizzo opcode di metodo a ok
076200130919        locMethodCode = 0;
076300130919        return locMethodCode;
076400130919
076500130919      /END-FREE
076600130919
076700130919     P UBLBLSPE_GetMother...
076800130919     P                 E
