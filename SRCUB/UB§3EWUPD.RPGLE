000100100323       //==============================================================
000200100323       //
000300100323       // Questo modulo gestisce l'aggiornamento del flag §3EWUPD della
000400100323       //   struttura dati D3EW (tabella "3EW" in TNTBE00F).
000500100323       //
000600100323       // Può essere usato solo come sottoprocedura.
000700100323       // Ogni metodo restituisce un valore 10I:
000800100323       //   · = *zero => tutto ok
000900100323       //   · < *zero => errore
001000100323       //
001100100323       // _____________________________________________
001200100323       // Parametri della sottoprocedura ub§3EWupd_Open
001300100323       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001400100323       // Posizione Uso Tipo      Descrizione
001500100323       // --------- --- --------- -------------------------------------
001600100323       //     /      /      /           /
001700100323       // --------- --- --------- -------------------------------------
001800100323       //
001900100323       // ____________________________________________
002000100323       // Parametri della sottoprocedura ub§3EWupd_Upd
002100100323       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002200100323       // Posizione Uso Tipo      Descrizione
002300100323       // --------- --- --------- -------------------------------------
002400100323       //     1     In  Character TBLKE1
002500100323       //     2     In  Character TBLKE2
002600100323       //     3     In  Character §3EWUPD ("0", "1")
002700100323       // --------- --- --------- -------------------------------------
002800100323       //
002900100323       // ______________________________________________
003000100323       // Parametri della sottoprocedura ub§3EWupd_Close
003100100323       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003200100323       // Posizione Uso Tipo      Descrizione
003300100323       // --------- --- --------- -------------------------------------
003400100323       //     /      /      /           /
003500100323       // --------- --- --------- -------------------------------------
003600100323       //
003700100325       // ___________________
003800100325       // Esempio di chiamata
003900100325       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004000100325       // if ub§3EWupd_Upd ( TBEke1 : TBEke2 : '1' ) = *off;
004100100323       //
004200100323       // ______________________________
004300100323       // Istruzioni per la compilazione
004400100323       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004500100323       // 1. Creare il modulo UB§3EWUPD (Opz 15 PDM).
004600100323       // 2. Creare/Aggiornare il programma di servizio UB§3EWUPD
004700100323       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
004800100323       // 4. Cancellare il modulo.
004900100323       //
005000100323       //==============================================================
005100100323
005200100323       //--------------------------------------------------------------
005300100323       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
005400100323       //--------------------------------------------------------------
005500100323
005600100323     /*MODULE
005700100323     /*END
005800100323
005900100323       //--------------------------------------------------------------
006000100323       //?Specifiche di controllo.                                     ?
006100100323       //--------------------------------------------------------------
006200100323
006300110523     h bnddir('UBRTVNETA')
006400100323     h NoMain
006500100323
006600100323       //--------------------------------------------------------------
006700100323       //?Definizione archivi.                                         ?
006800100323       //--------------------------------------------------------------
006900100323
007000100323       // -?Tabella "3EW"?
007100100323     fTNTBE01L  Uf   e           k disk    extfile(wLibFile)
007200100323     f                                     usropn
007300100323
007400100323       //--------------------------------------------------------------
007500100323       //?Definizione costanti.                                        ?
007600100323       //--------------------------------------------------------------
007700100323
007800100323       // -?Errori gestiti?
007900100323     d c_ErrParms...
008000100323     d                 c                   const(-1)
008100100323     d c_ErrRepCurrSysNetA...
008200100323     d                 c                   const(-2)
008300100323     d c_ErrOpenTNTBE...
008400100323     d                 c                   const(-3)
008500100323     d c_ErrNotFoundRecTBE...
008600100323     d                 c                   const(-4)
008700100323     d c_ErrUpdateTNTBE...
008800100323     d                 c                   const(-5)
008900100323     d c_ErrCloseTNTBE...
009000100323     d                 c                   const(-6)
009100110523     d c_ErrAllocRecTBE...
009200110523     d                 c                   const(-7)
009300100323
009400100323       // -?Codice tabella in gestione?
009500100323     d c_Tab           c                   const('3EW')
009600100323
009700100323       // -?Costanti per la definizione delle schiere con i nomi?
009800100323       //  ?degli iSeries da elaborare e delle relative librerie?
009900100323     d c_NrSyst        c                   const(2)
010000100323     d c_NrLibr        c                   const(2)
010100100323
010200100323       //--------------------------------------------------------------
010300100323       //?Definizione schiere globali.                                 ?
010400100323       //--------------------------------------------------------------
010500100323
010600100323       // -?iSeries  &  Librerie con entrambi i file tabelle?
010700100323     d $iSystem        s                   like(currSysNetA)
010800100323     d                                     dim(c_NrSyst)
010900100323     d                                     ctdata   perrcd( 1)
011000100323     d $Libraries      s                   like(ds_Libr)
011100100323     d                                     dim(c_NrSyst)
011200100323     d                                     alt($iSystem)
011300100323
011400100323       //--------------------------------------------------------------
011500100323       //?Definizione strutture dati globali.                          ?
011600100323       //--------------------------------------------------------------
011700121102
011800121102       // -?Status ds?
011900121102     d Status         sds
012000121102     d   SDSusr              254    263
012100100323
012200100323       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
012300100323     d ds_Libr         ds                  inz
012400121102     d   $Libr                       10    dim(c_NrLibr) inz
012500100323
012600100323       // -?Tabella "3EW" = Dati assegnati alla postazione EasyWEB?
012700100323     d d3EW          e ds                  qualified    inz
012800100323
012900100323       //--------------------------------------------------------------
013000100323       //?Definizione variabili globali.                               ?
013100100323       //--------------------------------------------------------------
013200100323
013300100323       // -?Valori di ritorno?
013400100323     d retField        s             10i 0 inz
013500110524     d WretField       s             10i 0 inz
013600100323
013700100323       // -?Flags booleani?
013800100323     d InitDone        s               n   inz
013900100323     d $OkUpd          s               n   inz
014000100323
014100100323       // -?Flag in aggiornamento?
014200100323     d wFlgUPD         s                   like(d3EW.§3EWupd)  inz
014300100323
014400100323       // -?Nome della libreria in cui è il file?
014500100323     d wLibFile        s             21a   inz
014600100323
014700100323       // -?Campi per la gestione della libreria del file?
014800100323     d w_iSystem       s              1  0 inz
014900100323     d w_SisInf        s              3  0 inz
015000121102
015100121102       //--------------------------------------------------------------
015200121102       //?Definizione prototipi procedure (globali).                   ?
015300121102       //--------------------------------------------------------------
015400121102
015500121102       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
015600121102      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
015700121102
015800121102       // -?Nome del sistema?
015900121102     d currSysNeta     s              8a   inz
016000121102       // -?Reperimento NETA sistema AS/400 corrente?
016100121102      /copy gaitrasrc/srcProtoPR,UBRTVNETA
016200100323
016300100323       //--------------------------------------------------------------
016400100323       //?Procedure name: UB§3EWupd_Upd                                ??
016500100323       // Purpose:        Aggiornamento flag §3EWUPD in tab."3EW"
016600100323       // Returns:        Integer (Status_Code)
016700100323       //--------------------------------------------------------------
016800100323
016900100323     p ub§3EWupd_Upd...
017000100323     p                 B                   export
017100100323
017200100323     d ub§3EWupd_Upd...
017300100323     d                 PI            10i 0
017400100323     d pInKey1                             like(TBEke1)        const
017500100323     d pInKey2                             like(TBEke2)        const
017600100323     d                                       options(*omit)
017700100402     d pInTipOpe                      1                        const
017800100323     d pInFlgUPD                           like(d3EW.§3EWupd)  const
017900100323
018000100323       //--------------------------------------------------------------
018100100323       //?Definizione variabili locali.                                ?
018200100323       //--------------------------------------------------------------
018300100402
018400100402       // -?Flags booleani?
018500100402     d $Found          s              1n   inz
018600100323
018700100323       // -?Contatore records aggiornati?
018800100323     d wCountUpdRec    s              3  0 inz
018900100323
019000100323       //--------------------------------------------------------------
019100100323       //?Definizione key-list locali.                                 ?
019200100323       //--------------------------------------------------------------
019300100323
019400100323       // -?TNTBE01L?
019500100323     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
019600100323     d                                     prefix(k_)   inz
019700100323
019800100323       //--------------------------------------------------------------
019900100323
020000100323      /free
020100100323
020200100323       // -?Inizializzazione parametri di output?
020300100323       reset  retField;
020400110524       reset  WretField;
020500100323
020600100323       // -?Verifica Parametri ricevuti?
020700100402       if  %parms < 4          or
020800100323          (pInFlgUPD <> *off   and   pInFlgUPD <> *on);
020900100323         retField = c_ErrParms;
021000100323         return  retField;
021100100323       endif;
021200100323
021300100323       // -?Ciclo di elaborazione per ogni sistema informativo?
021400100323       For  w_SisInf = 1  To  %elem($Libr);
021500100323
021600100323         clear  wCountUpdRec;
021700100323
021800100323         // -?Inizializzazione forzata per gestire il TNTBE di sede?
021900100323         //  ?per primo (e di filiale per secondo)?
022000100323         //  ?=> Se ricevuto errore => uscita?
022100100323         retField = ub§3EWupd_Open();
022200100323         if  retField < *zero;
022300100323           return  retField;
022400100323         endif;
022500100323
022600100402         // -?Reperimento record(s) "3EW" da aggiornare?
022700110524         //  ?SE richiamato dalla manutenzione tab. "3EW": alloca;?
022800110524         //  ?SE richiamato dalla stored-procedure: NON alloca ora?
022900110524         //     ?(ri-chainerà allocando SE dovrà aggiornare il rec.)?
023000100323         k_TBEcod = c_Tab;
023100100323         k_TBEke1 = pInKey1;
023200100323         if  %addr(pInKey2) <> *null;
023300100323           k_TBEke2 = pInKey2;
023400100323         else;
023500100323           clear  k_TBEke2;
023600100323         endif;
023700100323         clear  k_TBElin;
023800100323         clear  k_TBEsif;
023900100323         if  %addr(pInKey2) <> *null   and   pInKey2 <> *blank;
024000110524           if  pInTipOpe = 'I'   and   pInFlgUPD = '1';
024100110524             chain(n)  %kds(k05tntbe01)  TNTBE000;
024200110524             $Found = (%found(TNTBE01L));
024300110524           else;
024400110524             chain(e)  %kds(k05tntbe01)  TNTBE000;
024500110524             $Found = (Not %error  and  %found(TNTBE01L));
024600110524           endif;
024700100323         else;
024800100402           setll  %kds(k05tntbe01)  TNTBE000;
024900110524           if  pInTipOpe = 'I'   and   pInFlgUPD = '1';
025000110524             reade(n)  %kds(k05tntbe01 : 2)  TNTBE000;
025100110524             $Found = (Not %eof(TNTBE01L));
025200110524           else;
025300110524             reade(e)  %kds(k05tntbe01 : 2)  TNTBE000;
025400110524             $Found = (Not %error  and  Not %eof(TNTBE01L));
025500110524           endif;
025600100323         endif;
025700100323
025800100323         // -?Ciclo di aggiornamento flag §3EWUPD in tab. "3EW"?
025900100402         DoW  $Found;
026000100323
026100100323           d3EW = TBEuni;
026200100323
026300100323           // -?Impostazione flag in base a quello esistente?
026400130604           If  w_SisInf = 1;
026500130604
026600130604             reset  $OkUpd;
026700130604             wFlgUPD = d3EW.§3EWupd;
026800130604             select;
026900130604
027000130604               // ·?Richiamato da *pgm di gestione tabella "3C"?
027100130604               when  pInTipOpe = 'A'   and   pInFlgUPD = '0';
027200130604                 select;
027300130604                   // ·?Modificato record da inviare?
027400130604                   when  d3EW.§3EWupd = '0'   or   d3EW.§3EWupd = *blank;
027500130604                     $OkUpd  = *on;
027600130604                     wFlgUPD = 'M';
027700130604                   // ·?Modificato record già inviato?
027800130604                   when  d3EW.§3EWupd = '1';
027900130604                     $OkUpd  = *on;
028000130604                     wFlgUPD = '0';
028100130604                 endsl;
028200130604
028300130604               // ·?Richiamato da stored-procedure invio dati a EasyWeb?
028400130604               when  pInTipOpe = 'I'   and   pInFlgUPD = '1';
028500130604                 select;
028600130604                   // ·?Estratto record da inviare?
028700130604                   when  d3EW.§3EWupd = '0'   or   d3EW.§3EWupd = *blank;
028800130604                     $OkUpd  = *on;
028900130604                     wFlgUPD = '1';
029000130604                   // ·?Estratto record modificato?
029100130604                   when  d3EW.§3EWupd = 'M';
029200130604                     $OkUpd  = *on;
029300130604                     wFlgUPD = '0';
029400130604                 endsl;
029500130604
029600130604             endsl;
029700130604
029800130604           EndIf;
029900100323
030000100323           // -?Aggiornamento flag §3EWUPD in tab. "3EW"?
030100130604           If  $OkUpd = *on;
030200130604
030300110524             if  pInTipOpe = 'I'   and   pInFlgUPD = '1';
030400110524               chain(e)  (TBEcod:TBEke1:TBEke2:TBElin:TBEsif)  TNTBE000;
030500110524               // ·?Record allocato => avviso & uscita?
030600110524               if  %error;
030700110524                 retField = c_ErrAllocRecTBE;
030800110524                 return  retField;
030900110524               endif;
031000110524             endif;
031100130604
031200110524             // ·?Aggiornamento?
031300100323             d3EW.§3EWupd = wFlgUPD;
031400100323             TBEuni = d3EW;
031500121102             if  pInTipOpe = 'A'   and   pInFlgUPD = '0';
031600121102               d3EW.§3EWuum = SDSusr;
031700121102               if  TBEatb = *blank;
031800121102                 TBEftt = 'S';
031900121102                 clear  TBEflt;
032000121102                 if  w_SisInf = 1;
032100121102                   TBEftr = 'T';
032200121102                 else;
032300121102                   TBEftr = 'R';
032400121102                 endif;
032500121102                 TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
032600121102                                        : 1 : 8 ) );
032700121102               else;
032800121102                 clear  TBEftr;
032900121102               endif;
033000121102             endif;
033100100323             //_______________
033200100323             update  TNTBE000;
033300100323             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
033400100323             wCountUpdRec += 1;
033500130604
033600130604           EndIf;
033700100323
033800100323           // -?Lettura rec. "3EW" successivo per il cliente ricevuto?
033900100323           //  ?(se ricevuta solo Key1 - da tab. "3C")?
034000100323           if  %addr(pInKey2) <> *null   and   pInKey2 <> *blank;
034100100323             leave;
034200100323           endif;
034300100323           reade  %kds(k05tntbe01 : 2)  TNTBE000;
034400100402           $Found = Not %eof(TNTBE01L);
034500100323
034600100323         EndDo;
034700100323
034800130604         select;
034900130604           // -?Non aggiornato alcun record => avviso?
035000130604           when  wCountUpdRec = *zero   and   w_SisInf = 1;
035100130604             WretField = c_ErrNotFoundRecTBE;
035200130604             leave;
035300130604           // -?Non aggiornato il record in filiale => avviso?
035400130604           when  wCountUpdRec = *zero   and   w_SisInf > 1;
035500130604             WretField = c_ErrUpdateTNTBE;
035600130604             leave;
035700130604         endsl;
035800100323
035900100323       EndFor;
036000100323
036100100323       // -?Chiusura file tabelle?
036200100323       ub§3EWupd_Close();
036300100323
036400100323       // -?Ritorno parametri di output?
036500110524       if  retField = *zero  and  WretField <> *zero;
036600110524         retField = WretField;
036700110524       endif;
036800100323       return  retField;
036900100323
037000100323      /end-free
037100100323
037200100323     p ub§3EWupd_Upd...
037300100323     p                 E
037400100323
037500100323       //--------------------------------------------------------------
037600100323       //?Procedure name: UB§3EWupd_Open                               ?
037700100323       // Purpose:        Apertura archivio da aggiornare
037800100323       // Returns:        Integer (Status_Code)
037900100323       //--------------------------------------------------------------
038000100323
038100100323     p ub§3EWupd_Open...
038200100323     p                 B                   export
038300100323
038400100323     d ub§3EWupd_Open...
038500100323     d                 PI            10i 0
038600100323
038700100323       //--------------------------------------------------------------
038800100323
038900100323      /free
039000100323
039100100323       // -?Inizializzazione parametri di output?
039200100323       reset  retField;
039300100323
039400100323       // -?Inizializzazione opcode di metodo?
039500100323       reset  InitDone;
039600100323
039700100323       If  w_SisInf <= 1;
039800100323
039900100323         // -?Verifica del sistema AS/400 corrente?
040000100323         if  ubRtvNetA_Rtv(currSysNeta) <> *zero;
040100100323           retField = c_ErrRepCurrSysNetA;
040200100323           return  retField;
040300100323         endif;
040400100323
040500100323         // -?Impostazione elenco librerie in cui gestire le tabelle?
040600100323         //  ?(a seconda del sistema in cui si stà lavorando)?
040700100323         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
040800100323         if  w_iSystem = *zero;
040900100323           retField = c_ErrRepCurrSysNetA;
041000100323           return  retField;
041100100323         endif;
041200100323
041300100323       EndIf;
041400100323
041500100323       // -?Chiusura (eventuale) file tabelle?
041600100323       retField = ub§3EWupd_Close();
041700100323       if  retField < *zero;
041800100323         return  retField;
041900100323       endif;
042000100323
042100100323       // -?Impostazione archivio TABEL di sede al 1° giro o dopo il 2°?
042200100323       if  w_SisInf < 1   or   w_SisInf > 2;
042300100323         w_SisInf = 1;
042400100323       endif;
042500100323
042600100323       // -?Apertura file tabelle?
042700100323       ds_Libr  = $Libraries(w_iSystem);
042800100323       wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
042900100323       open(E)  TNTBE01L;
043000100323       if  %error;
043100100323         retField = c_ErrOpenTNTBE;
043200100323         return  retField;
043300100323       endif;
043400100323
043500100323       // -?Valorizzazione opcode di metodo?
043600100323       InitDone = *on;
043700100323
043800100323       // -?Ritorno parametri di output?
043900100323       return  retField;
044000100323
044100100323      /end-free
044200100323
044300100323     p ub§3EWupd_Open...
044400100323     p                 E
044500100323
044600100323       //--------------------------------------------------------------
044700100323       //?Procedure name: UB§3EWupd_Close                              ?
044800100323       // Purpose:        Chiusura file TNTBE01L
044900100323       // Returns:        Integer (Status_Code)
045000100323       //--------------------------------------------------------------
045100100323
045200100323     p ub§3EWupd_Close...
045300100323     p                 B                   export
045400100323
045500100323     d ub§3EWupd_Close...
045600100323     d                 PI            10i 0
045700100323
045800100323      /free
045900100323
046000100323       // -?Inizializzazione parametri di output?
046100100323       reset  retField;
046200100323
046300100323       // -?Chiusura archivio e cancellazione override?
046400100323       if  %open(TNTBE01L);
046500100323         close(E)  TNTBE01L;
046600100323         if  %error;
046700110523           if  retField = *zero;
046800110523             retField = c_ErrCloseTNTBE;
046900110523           endif;
047000100323           return  retField;
047100100323         endif;
047200100323       endif;
047300100323
047400100323       // -?Valorizzazione opcode di metodo?
047500100323       InitDone = *off;
047600100323
047700100323       // -?Ritorno parametri di output?
047800100323       return  retField;
047900100323
048000100323      /end-free
048100100323
048200100323     p ub§3EWupd_Close...
048300100323     p                 E
048400100323
048500100323       //--------------------------------------------------------------
048600100323       //?Definizione schiere locali a tempo di compilazione           ?
048700100323       //--------------------------------------------------------------
048800100323
048900100323** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
049000100323SETRAS  GAITRAGRU FILTRAGRU
049100100323AS888   GAITRAGRPSFILTRAGRPF
