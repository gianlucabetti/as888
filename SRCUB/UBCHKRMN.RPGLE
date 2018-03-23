000100100113     /*CMD ovrdbf file(FIVAB01L) tofile(FILTRAGRPF/FIVAB01L) ovrscope(*calllvl)
000200100113     /*CMD ovrdbf file(FNBLP33L) tofile(FILTRAPRD/FNBLP33L) ovrscope(*calllvl)
000300100113     /*CMD ovrdbf file(TITAS32C) tofile(GAITRAGRPS/TITAS32C) ovrscope(*calllvl)
000400100113     /*MODULE
000500100113     /*PRM dbgview(*source)
000600100113     /*END dltovr file(FIVAB01L FNBLP33L TITAS32C) lvl(*)
000700100113     /*END
000800100113       //==============================================================
000900100113       //
001000100113       // Questo modulo verifica l'esistenza di un riferimento mittente
001100100113       // numerico (pInRMN) nelle varie spediz. di un mittente (pInCCM)
001200100113       // nei vari archivi: · FILTRAGRU/FIVAB00F
001300100113       //                   · FILTRA201/FNBLP00F
001400100113       //                   · GAITRAGRU/TITAS00F+10F+P0F
001500100113       //
001600100113       // Può essere usato solo come sottoprocedura.
001700100113       // Ogni metodo restituisce un valore 10I:
001800100113       //    · = *zero => tutto ok
001900100113       //    · < *zero => errore
002000100113       //
002100100113       // _________________________________________________
002200100113       // PARAMETRI DELLA SOTTOPROCEDURA: UBCHKFKS_Check...
002300100113       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002400100113       // Posizione Uso Tipo       Lunghezza Descrizione
002500100113       // --------- --- ---------- --------- --------------------------
002600100113       // 1         In  *Dec          7 / 0  Codice cliente mittente
002700100113       // 2         In  *Dec         15 / 0  Riferimento mitt. numerico
002800100113       // 3         Out *Lgl          1      Flag trovato in FIVAB: 0/1
002900100113       // 4         Out *Lgl          1      Flag trovato in FNBLP: 0/1
003000100113       // 4         Out *Lgl          1      Flag trovato in TITAS: 0/1
003100100113       // --------- --- ----------  --------- -------------------------
003200100113       //
003300100113       // __________________
003400100113       // ESEMPI DI CHIAMATA
003500100113       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003600100113       // if  UBCHKRMN_Check ( pInCCM    : pInRMN    :
003700100113       //                      pOutFIVAB : pOutFNBLP : pOutTITAS ) = *zero;
003800100113       //
003900100113       // ______________________________
004000100113       // ISTRUZIONI PER LA COMPILAZIONE
004100100113       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004200100113       // 1. Creare il modulo UBCHKRMN (Opz 15 PDM)
004300100113       // 2. Creare/Aggiornare il programma di servizio UBCHKRMN (CRTSRVPGM / UPDSRVPGM)
004400100113       // 4. Cancellare il modulo.
004500100113       //
004600100113       //==============================================================
004700100113
004800100113     h BndDir('UBRTVNETA')
004900100113     h NoMain
005000100113
005100100113       //--------------------------------------------------------------
005200100113       //?Definizione archivi.                                         ?
005300100113       //--------------------------------------------------------------
005400100113
005500100113       // -?Bolle da cliente (testata)?
005600100113     fFIVAB01L  if   e           k disk    extfile(wLibFile1)  usropn
005700100113
005800100113       // -?Bolle in partenza (testata)?
005900100113     fFNBLP33L  if   e           k disk    extfile(wLibFile2)  usropn
006000100113
006100100113       // -?Bolle Sede: da fatturare, fatturate ultimi mesi e storico?
006200100113     fTITAS32C  if   e           k disk    extfile(wLibFile3)  usropn
006300100113
006400100113
006500100113       //--------------------------------------------------------------
006600100113       //?Definizione costanti.                                        ?
006700100113       //--------------------------------------------------------------
006800100113
006900100113       // -?Errori gestiti?
007000100113     d c_ErrRepCurrSysNetA...
007100100113     d                 c                   const(-1)
007200100113     d c_ErrCloseFIVAB...
007300100113     d                 c                   const(-2)
007400100113     d c_ErrCloseFNBLP...
007500100113     d                 c                   const(-3)
007600100113     d c_ErrCloseTITAS...
007700100113     d                 c                   const(-4)
007800100113
007900100113       //--------------------------------------------------------------
008000100113       //?Definizione prototipi procedure.                             ?
008100100113       //--------------------------------------------------------------
008200100113
008300100113       // -?Verifica esistenza riferimento mittente numerico x cliente?
008400100113      /copy gaitrasrc/srcProtoPR,UBCHKRMN
008500100113
008600100113       // -?Reperimento NETA sistema AS/400 corrente?
008700100113     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
008800100113
008900100113       //--------------------------------------------------------------
009000100113       //?Definizione variabili globali.                               ?
009100100113       //--------------------------------------------------------------
009200100113
009300100113       // -?Valori di ritorno?
009400100113     d rtnEsito        s             10i 0 inz
009500100113
009600100113       // -?Flags booleani?
009700100113     d $InitDone       s               n   inz
009800100113
009900100113       // -?Nome della libreria in cui è il file FIVAB00F?
010000100113     d wLibFile1       s             21a   inz
010100100113       // -?Nome della libreria in cui è il file FNBLP00F?
010200100113     d wLibFile2       s             21a   inz
010300100113       // -?Nome della libreria in cui sono i file TITAS00F/10F/P0F?
010400100113     d wLibFile3       s             21a   inz
010500100113
010600100113       //--------------------------------------------------------------
010700100113       //?Definizione key-list globali.                                ?
010800100113       //--------------------------------------------------------------
010900100113
011000100113       // -?FIVAB01L?
011100100113     d k02fivab01    e ds                  extname(FIVAB01L : *key)
011200100113     d                                     prefix(k_)  inz
011300100113
011400100113       // -?FNBLP33L?
011500100113     d k03fnblp33    e ds                  extname(FNBLP33L : *key)
011600100113     d                                     prefix(k_)  inz
011700100113
011800100113       // -?TITAS32C?
011900100113     d k02titas32    e ds                  extname(TITAS32C : *key)
012000100113     d                                     prefix(k_)  inz
012100100113
012200100113       //--------------------------------------------------------------
012300100113       //?Procedure name: UBCHKRMN_Open                                ?
012400100113       // Purpose:        Apertura archivi
012500100113       // Returns:        Integer (Status_Code)
012600100113       //--------------------------------------------------------------
012700100113
012800100113     p UBCHKRMN_Open...
012900100113     p                 B                   export
013000100113
013100100113     d UBCHKRMN_Open...
013200100113     d                 PI            10i 0
013300100113
013400100113       //--------------------------------------------------------------
013500100113       // Definizione variabili locali.
013600100113       //--------------------------------------------------------------
013700100113
013800100113       // -?Nome del sistema?
013900100113     d currSysNeta     s              8a   inz
014000100113
014100100113       //--------------------------------------------------------------
014200100113
014300100113      /free
014400100113
014500100113       // -?Inizializzazione opcode di metodo?
014600100113       reset  rtnEsito;
014700100113       reset  $InitDone;
014800100113
014900100113       // -?Verifica del sistema AS/400 corrente?
015000100113       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
015100100113         rtnEsito = c_ErrRepCurrSysNetA;
015200100113         return  rtnEsito;
015300100113       endif;
015400100113
015500100113       // -?Apertura archivi?
015600100113       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
015700100113         wLibFile1 = 'FILTRAGRU/FIVAB01L';
015800100113         wLibFile2 = 'FILTRA201/FNBLP33L';
015900100113         wLibFile3 = 'GAITRAGRU/TITAS32C';
016000100113       else;
016100100113         wLibFile1 = 'FILTRAGRPF/FIVAB01L';
016200100113         wLibFile2 = 'FILTRAPRD/FNBLP33L';
016300100113         wLibFile3 = 'GAITRAGRPS/TITAS32C';
016400100113       endif;
016500100113
016600100113       if  not %open(FIVAB01L);
016700100113         open  FIVAB01L;
016800100113       endif;
016900100113       if  not %open(FNBLP33L);
017000100113         open  FNBLP33L;
017100100113       endif;
017200100113       if  not %open(TITAS32C);
017300100113         open  TITAS32C;
017400100113       endif;
017500100113
017600100113       // -?Valorizzazione opcode di metodo?
017700100113       $InitDone = *on;
017800100113
017900100113       return  rtnEsito;
018000100113
018100100113      /end-free
018200100113
018300100113     p UBCHKRMN_Open...
018400100113     p                 E
018500100113
018600100113       //--------------------------------------------------------------
018700100113       //?Procedure name: UBCHKRMN_Exist...                             ?
018800100113       // Purpose:        Verifica l'esistenza del riferimento mittente
018900100113       //                 numerico (pInRMN) nelle varie spedizioni di
019000100113       //                 un mittente (pInCCM) nei vari archivi:
019100100113       //                 · FILTRAGRU/FIVAB00F
019200100113       //                 · FILTRA201/FNBLP00F
019300100113       //                 · GAITRAGRU/TITAS00F+10F+P0F
019400100113       // Returns:        Integer (Status_Code)
019500100113       // Parameter:      pInCCM  => Codice cliente mittente
019600100113       // Parameter:      pInRMN  => Riferimento mittente numerico
019700100113       // Parameter:      pOutFIVAB => Flag esiste in FIVAB: *off/*on
019800100113       // Parameter:      pOutFNBLP => Flag esiste in FNBLP: *off/*on
019900100113       // Parameter:      pOutTITAS => Flag esiste in TITAS: *off/*on
020000100113       //--------------------------------------------------------------
020100100113
020200100113     p UBCHKRMN_Exist...
020300100113     p                 B                   export
020400100113
020500100113     d UBCHKRMN_Exist...
020600100113     d                 PI            10i 0
020700100113     d   pInCCM                            like(TASccm)  const
020800100113     d   pInRMN                            like(TASrmn)  const
020900100113     d   pOutFIVAB                     n
021000100113     d   pOutFNBLP                     n
021100100113     d   pOutTITAS                     n
021200100113
021300100113      /free
021400100113
021500100113       // -?Inizializzazione parametri di output?
021600100113       reset  rtnEsito;
021700100113       clear  pOutFIVAB;
021800100113       clear  pOutFNBLP;
021900100113       clear  pOutTITAS;
022000100113
022100100113       // -?Se non già inizializzata la procedure?
022200100113       //  ?=> si forza l'inizializzazione?
022300100113       if  not $InitDone;
022400100113         rtnEsito = UBCHKRMN_Open;
022500100113       endif;
022600100113       // -?Se ricevuto errore => uscita?
022700100113       if  rtnEsito < *zero;
022800100113         return  rtnEsito;
022900100113       endif;
023000100113
023100100113       // -?Verifica esistenza del riferimento mittente numerico?
023200100113       //  ?per il cliente  su FIVAB?
023300100113       k_VABccm = pInCCM;
023400100113       k_VABrmn = pInRMN;
023500100113       setll  %kds( k02fivab01 )  FIVAB000;
023600100113       pOutFIVAB = %equal(FIVAB01L);
023700100113
023800100113       // -?Verifica esistenza del riferimento mittente numerico?
023900100113       //  ?per il cliente  su FNBLP?
024000100113       k_BLPrmn = pInRMN;
024100100113       setll  %kds( k03fnblp33 : 1 )  FNBLP000;
024200100113       reade  %kds( k03fnblp33 : 1 )  FNBLP000;
024300100113       DoW  Not %eof(FNBLP33L)   and   Not pOutFNBLP;
024400100113         select;
024500100113           when  BLPccm <> *zero   and   BLPccm = pInCCM;
024600100113             pOutFNBLP = *on;
024700100113           when  BLPccm =  *zero   and   BLPksc = pInCCM;
024800100113             pOutFNBLP = *on;
024900100113           other;
025000100113             reade  %kds( k03fnblp33 : 1 )  FNBLP000;
025100100113         endsl;
025200100113       EndDo;
025300100113
025400100113       // -?Verifica esistenza del riferimento mittente numerico?
025500100113       //  ?per il cliente  su TITAS?
025600100113       k_TASrmn = pInRMN;
025700100113       k_TASccm = pInCCM;
025800100113       setll  %kds( k02titas32 )  TITAS32C;
025900100113       pOutTITAS = %equal(TITAS32C);
026000100113
026100100113
026200100113       return  rtnEsito;
026300100113
026400100113      /end-free
026500100113
026600100113     p UBCHKRMN_Exist...
026700100113     p                 E
026800100113
026900100113       //--------------------------------------------------------------
027000100113       //?Procedure name: UBCHKRMN_Close                               ?
027100100113       // Purpose:        Chiusura archivi
027200100113       // Returns:        Integer (Status_Code)
027300100113       //--------------------------------------------------------------
027400100113
027500100113     p UBCHKRMN_Close...
027600100113     p                 B                   export
027700100113
027800100113     d UBCHKRMN_Close...
027900100113     d                 PI            10i 0
028000100113
028100100113      /free
028200100113
028300100113
028400100113       // -?Inizializzazione dei parametri di output?
028500100113       reset  rtnEsito;
028600100113
028700100113       // -?Chiusura archivio FIVAB?
028800100113       if  %open(FIVAB01L);
028900100113         close(E)  FIVAB01L;
029000100113         if  %error   and   rtnEsito = *zero;
029100100113           rtnEsito = c_ErrCloseFIVAB;
029200100113         endif;
029300100113       endif;
029400100113
029500100113       // -?Chiusura archivio FNBLP?
029600100113       if  %open(FNBLP33L);
029700100113         close(E)  FNBLP33L;
029800100113         if  %error   and   rtnEsito = *zero;
029900100113           rtnEsito = c_ErrCloseFNBLP;
030000100113         endif;
030100100113       endif;
030200100113
030300100113       // -?Chiusura archivio TITAS?
030400100113       if  %open(TITAS32C);
030500100113         close(E)  TITAS32C;
030600100113         if  %error   and   rtnEsito = *zero;
030700100113           rtnEsito = c_ErrCloseTITAS;
030800100113         endif;
030900100113       endif;
031000100113
031100100113       // -?Valorizzazione opcode di metodo?
031200100113       $InitDone = *off;
031300100113
031400100113       return  rtnEsito;
031500100113
031600100113      /end-free
031700100113
031800100113     p UBCHKRMN_Close...
031900100113     p                 E
