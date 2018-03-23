000100110422     /*MODULE
000200110422     /*PRM dbgview(*source)
000300110422     /*END
000400110422       //==============================================================
000500110422       //
000600110422       // Questo modulo reperisce il numero serie (dalla tab. "3C") ed
000700110422       //   il range segnacolli (dalla tab. "3CP" o "3EW") per il
000800110422       //   cliente ricevuto come parametro.
000900110422       //
001000110422       // Può essere usato solo come sottoprocedura.
001100110422       // Ogni metodo restituisce un valore 10I:
001200110422       //    · = *zero => tutto ok
001300110422       //    · < *zero => errore
001400110422       //
001500110422       // ____________________________________________
001600110422       // PARAMETRI DELLA SOTTOPROCEDURA UBNRSRSC_Init
001700110422       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001800110422       // Posizione Uso Tipo      Descrizione
001900110422       // --------- --- --------- -------------------------------------
002000110422       //     /      /      /           /
002100110422       // --------- --- --------- -------------------------------------
002200110422       //
002300110422       // ___________________________________________
002400110422       // PARAMETRI DELLA SOTTOPROCEDURA UBNRSRSC_Rtv
002500110422       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002600110422       // Posizione Uso Tipo      Lunghezza Descrizione
002700110422       // --------- --- --------- --------- ---------------------------
002800110422       //     1     In  *Dec          7 / 0  Codice cliente
002900110422       //     2     Out *Dec          2 / 0  Numero Serie
003000110422       //     3     Out *Dec          7 / 0  Numero Segnacollo Iniziale
003100110422       //     4     Out *Dec          7 / 0  Numero Segnacollo Finale
003200110422       // --------- --- --------- --------- ---------------------------
003300110422       //
003400110422       // _____________________________________________
003500110422       // PARAMETRI DELLA SOTTOPROCEDURA UBNRSRSC_Close
003600110422       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003700110422       // Posizione Uso Tipo      Descrizione
003800110422       // --------- --- --------- -------------------------------------
003900110422       //     /      /      /           /
004000110422       // --------- --- --------- -------------------------------------
004100110422       //
004200110422       // __________________
004300110422       // ESEMPI DI CHIAMATA
004400110422       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004500110422       // if UBNRSRSC_Rtv ( pInKsc : pOutNrs : pOutNscI : pOutNscF )
004600110422       //                 = *off;
004700110422       //
004800110422       // ______________________________
004900110422       // ISTRUZIONI PER LA COMPILAZIONE
005000110422       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
005100110422       // 1. Creare il modulo UBNRSRSC (Opz 15 PDM).
005200110422       // 2. Creare/Aggiornare il programma di servizio UBNRSRSC
005300110422       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
005400110422       // 4. Cancellare il modulo.
005500110422       //
005600110422       //==============================================================
005700110422
005800110422     h BndDir('UBBNDDIR')
005900110422     h NoMain
006000110422
006100110422       //--------------------------------------------------------------
006200110422       //?Definizione archivi.                                         ?
006300110422       //--------------------------------------------------------------
006400110422
006500110422     fTABEL00F  if   e           k disk    extfile(wLibFile1)  usropn
006600110422     fTNTBE01L  if   e           k disk    extfile(wLibFile2)  usropn
006700110422
006800110422       //--------------------------------------------------------------
006900110422       //?Definizione costanti.                                        ?
007000110422       //--------------------------------------------------------------
007100110422
007200110422       // -?Errori gestiti?
007300110422     d c_ErrRepCurrSysNetA...
007400110422     d                 c                   const(-1)
007500110422     d c_ErrOpen...
007600110422     d                 c                   const(-2)
007700110422     d c_NotFound3C...
007800110422     d                 c                   const(-3)
007900110422     d c_ErrClose...
008000110422     d                 c                   const(-4)
008100110422
008200110422       //--------------------------------------------------------------
008300110422       //?Definizione prototipi procedure.                             ?
008400110422       //--------------------------------------------------------------
008500110422
008600110422       // -?Recupero Nr. Serie & Range Segnacolli per cliente?
008700110422      /copy gaitrasrc/srcProtoPR,ubNrsRsc
008800110422
008900110422       // -?Reperimento NETA sistema AS/400 corrente?
009000110422      /copy gaitrasrc/srcProtoPR,ubRtvNetA
009100110422
009200110422       //--------------------------------------------------------------
009300110422       //?Definizione strutture dati globali.                          ?
009400110422       //--------------------------------------------------------------
009500110422
009600110422       // -?Tab. "3C" = Invio dati - BOLLETTAZIONE?
009700110422     d ds3C          e ds                  qualified  inz
009800110422
009900110422       // -?Tab. "3CP" = Serie parziali assegnate a clienti?
010000110422     d d3CP          e ds                  qualified  inz
010100110422
010200110422       // -?Tab. "3EW" = Dati assegnati alla postazione EasyWEB?
010300110422     d d3EW          e ds                  qualified  inz
010400110422
010500110422       //--------------------------------------------------------------
010600110422       //?Definizione variabili globali.                               ?
010700110422       //--------------------------------------------------------------
010800110422
010900110422       // -?Valori di ritorno?
011000110422     d rtnEsito        s             10i 0 inz
011100110422
011200110422       // -?Flags booleani?
011300110422     d $InitDone       s               n   inz
011400110422
011500110422       // -?Nome della libreria in cui è il file?
011600110422     d wLibFile1       s             21a   inz
011700110422     d wLibFile2       s             21a   inz
011800110422
011900110422       //--------------------------------------------------------------
012000110422       //?Definizione key-list globali.                                ?
012100110422       //--------------------------------------------------------------
012200110422
012300110422       // -?TABEL00F?
012400110422     d k03tabel00    e ds                  extname(TABEL00F : *key)
012500110422     d                                     prefix(k_)  inz
012600110422
012700110422       // -?TNTBE01L?
012800110422     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
012900110422     d                                     prefix(k_)  inz
013000110422
013100110422       //--------------------------------------------------------------
013200110422       //?Procedure name: UBNRSRSC_Init                                ?
013300110422       // Purpose:        Inizializzazione variabili di lavoro
013400110422       // Returns:        Integer (Status_Code)
013500110422       //--------------------------------------------------------------
013600110422
013700110422     p UBNRSRSC_Init...
013800110422     p                 B                   export
013900110422
014000110422     d UBNRSRSC_Init...
014100110422     d                 PI            10i 0
014200110422
014300110422       //--------------------------------------------------------------
014400110422       //?Definizione variabili locali.                                ?
014500110422       //--------------------------------------------------------------
014600110422
014700110422       // -?Nome del sistema?
014800110422     d currSysNeta     s              8a   inz
014900110422
015000110422       //--------------------------------------------------------------
015100110422
015200110422      /free
015300110422
015400110422       // -?Inizializzazione variabili?
015500110422       reset  rtnEsito;
015600110422       reset  $InitDone;
015700110422
015800110422       // -?Verifica del sistema AS/400 corrente?
015900110422       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
016000110422         rtnEsito = c_ErrRepCurrSysNetA;
016100110422         return  rtnEsito;
016200110422       endif;
016300110422
016400110422       // -?Apertura archivi?
016500120228       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
016600120228         wLibFile1 = 'GAITRAGRU/TABEL00F';
016700120228       else;
016800120228         wLibFile1 = 'GAITRAGRPS/TABEL00F';
016900120228       endif;
017000120228       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
017100120228         wLibFile2 = 'GAITRAGRU/TNTBE01L';
017200120228       else;
017300120228         wLibFile2 = 'GAITRAGRPS/TNTBE01L';
017400120228       endif;
017500110422
017600110422       //if  %parms() > 1  and  %addr(pOutNRS) <> *null
017700110422       //                  and  Not %open(TABEL00F);
017800120228       if  Not %open(TABEL00F);
017900120228         open  TABEL00F;
018000110422       endif;
018100110422
018200110422       //if  ( (%parms() > 2  and  %addr(pOutNSI) <> *null)    OR
018300110422       //      (%parms() > 3  and  %addr(pOutNSF) <> *null) )  and
018400110422       //    Not %open(TNTBE01L);
018500120228       if  Not %open(TNTBE01L);
018600120228         open  TNTBE01L;
018700120228       endif;
018800110422
018900110422       // -?Valorizzazione variabili?
019000110422       $InitDone = *on;
019100110422
019200110422       return  rtnEsito;
019300110422
019400110422      /end-free
019500110422
019600110422     p UBNRSRSC_Init...
019700110422     p                 E
019800110422
019900110422
020000110422       //--------------------------------------------------------------
020100110422       //?Procedure name: UBNRSRSC_Rtv                                  ?
020200110422       // Purpose:        Reperimento Num. Serie & Range SegnaColli
020300110422       //                 del cliente ricevuto come parametro.
020400110422       // Returns:        Integer (Status_Code)
020500110422       // Parameter:      pInKSC        => Codice Cliente
020600110422       //                 pOutNRS       => Numero Serie
020700110422       //                 pOutNSI       => Numero Segnacollo Iniziale
020800110422       //                 pOutNSF       => Numero Segnacollo Finale
020900110422       //--------------------------------------------------------------
021000110422
021100110422     p UBNRSRSC_Rtv...
021200110422     p                 B                   export
021300110422
021400110422     d UBNRSRSC_Rtv...
021500110422     d                 PI            10i 0
021600110422     d  pInKSC                             like(ds3C.§3Ccks)  const
021700110422     d  pOutNRS                            like(ds3C.§3Cnrs)
021800110422     d                                     options(*nopass : *omit)
021900110422     d  pOutNSI                            like(d3EW.§3EWnsi)
022000110422     d                                     options(*nopass : *omit)
022100110422     d  pOutNSF                            like(d3EW.§3EWnsf)
022200110422     d                                     options(*nopass : *omit)
022300110422
022400110422      /free
022500110422
022600110422       // -?Inizializzazione parametri di output?
022700110422       reset  rtnEsito;
022800110422       if  %parms() > 1  and  %addr(pOutNRS) <> *null;
022900110422         clear  pOutNRS;
023000110422       endif;
023100110422       if  %parms() > 2  and  %addr(pOutNSI) <> *null;
023200110422         clear  pOutNSI;
023300110422       endif;
023400110422       if  %parms() > 3  and  %addr(pOutNSF) <> *null;
023500110422         clear  pOutNSF;
023600110422       endif;
023700110422
023800110422       // -?Se non già inizializzata la procedure?
023900110422       //  ?=> si forza l'inizializzazione?
024000110422       if  not $InitDone;
024100110422         rtnEsito = UBNRSRSC_Init;
024200110422       endif;
024300110422       // -?Se ricevuto errore => uscita?
024400110422       if  rtnEsito < *zero;
024500110422         return  rtnEsito;
024600110422       endif;
024700110422
024800110422       // -?Reperimento Numero Serie da tab. "3C"?
024900110422       //  ?per il cliente ricevuto come parametro?
025000110422       //  ?=> da fare COMUNQUE, perchè la serie serve anche DOPO?
025100110422       IF  (%parms() > 1  and  %addr(pOutNRS) <> *null)  OR
025200110422           (%parms() > 2  and  %addr(pOutNSI) <> *null)  OR
025300110422           (%parms() > 3  and  %addr(pOutNSF) <> *null);
025400110422
025500110422         clear  ds3C;
025600110422         k_TBLkut = 1;
025700110422         k_TBLcod = '3C';
025800110422         k_TBLkey = %editc( pInKSC : 'X');
025900110422         chain  %kds( k03tabel00 )  TABEL;
026000110422
026100110422         // -?Cliente NON trovato in tab. "3C" => errore?
026200110422         if  NOT %found(TABEL00F);
026300110422           rtnEsito = c_NotFound3C;
026400110422           return  rtnEsito;
026500110422         endif;
026600110422
026700110422         // -?Impostazione parametri di output per il cliente ricevuto:?
026800110422         ds3C = TBLuni;
026900110422
027000110422         //   -?Numero Serie?
027100110422         if  %parms() > 1  and  %addr(pOutNRS) <> *null;
027200110422           pOutNRS = ds3C.§3Cnrs;
027300110422         endif;
027400110422
027500110422       ENDIF;
027600110422
027700110422       // -?Reperimento Range Segnacolli da tab. "3CP"/"3EW"?
027800110422       //  ?per il cliente ricevuto come parametro?
027900110422       IF  ( (%parms() > 2  and  %addr(pOutNSI) <> *null)    or
028000110422             (%parms() > 3  and  %addr(pOutNSF) <> *null) )  and
028100110422           ds3C.§3Cnrs > *zero;
028200110422
028300110422         clear  d3CP;
028400110422         clear  d3EW;
028500110422
028600110422         If  ds3C.§3Ccba <> 'ESWEB';
028700110422
028800120228           if  %parms() > 2  and  %addr(pOutNSI) <> *null;
028900120228             pOutNSI = 1;
029000120228           endif;
029100120228           if  %parms() > 3  and  %addr(pOutNSF) <> *null;
029200120228             pOutNSF = *all'9';
029300120228           endif;
029400110422           k_TBEcod = '3CP';
029500110422           k_TBEke1 = %editc( pInKSC : 'X');
029600110422           k_TBEke2 = %editc( ds3C.§3Cnrs : 'X' );
029700110422           setll  %kds( k05tntbe01 : 3 )  TNTBE000;
029800110422           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
029900110422           DoW  Not %eof(TNTBE01L);
030000110422             if  %subst( TBEke2 : 1 : 2 ) = %editc( ds3C.§3Cnrs : 'X' )
030100110422                 and  TBEatb = *blank;
030200110422               d3CP = TBEuni;
030300110422               if  %parms() > 2  and  %addr(pOutNSI) <> *null;
030400110422                 pOutNSI = %int( %subst( TBEke2 : 3 : 7 ) );
030500120228               endif;
030600120228               if  %parms() > 3  and  %addr(pOutNSF) <> *null;
030700110422                 pOutNSF = d3CP.§3CPal;
030800110422               endif;
030900120228               leave;
031000110422             endif;
031100110422             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
031200110422           EndDo;
031300110422
031400110422         Else;
031500110422
031600110422           k_TBEcod = '3EW';
031700120228           k_TBEke1 = '0' + %editc( ds3C.§3Ccks : 'X' );
031800110422           clear  k_TBEke2;
031900110422           clear  k_TBElin;
032000110422           clear  k_TBEsif;
032100110422           setll  %kds( k05tntbe01 )  TNTBE000;
032200110422           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
032300110422           DoW  Not %eof(TNTBE01L);
032400120228             d3EW = TBEuni;
032500120228             If  d3EW.§3EWnrs = ds3C.§3Cnrs;
032600120228               if  %parms() > 2  and  %addr(pOutNSI) <> *null;
032700120228                 pOutNSI = d3EW.§3EWnsi;
032800120228               endif;
032900120228               if  %parms() > 3  and  %addr(pOutNSF) <> *null;
033000120228                 pOutNSF = d3EW.§3EWnsf;
033100120228               endif;
033200110422               leave;
033300120228             EndIf;
033400110422             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
033500110422           EndDo;
033600110422
033700110422         EndIf;
033800110422
033900110422       ENDIF;
034000110422
034100110422       return  rtnEsito;
034200110422
034300110422      /end-free
034400110422
034500110422     p UBNRSRSC_Rtv...
034600110422     p                 E
034700110422
034800110422       //--------------------------------------------------------------
034900110422       //?Procedure name: UBNRSRSC_Close                               ?
035000110422       // Purpose:        Chiusura archivi e cancellazione override
035100110422       // Returns:        Integer (Status_Code)
035200110422       //--------------------------------------------------------------
035300110422
035400110422     p UBNRSRSC_Close...
035500110422     p                 B                   export
035600110422
035700110422     d UBNRSRSC_Close...
035800110422     d                 PI            10i 0
035900110422
036000110422      /free
036100110422
036200110422
036300110422       // -?Inizializzazione dei parametri di output?
036400110422       reset  rtnEsito;
036500110422
036600110422       // -?Chiusura archivio TABEL?
036700110422       if  %open(TABEL00F);
036800110422         close(E)  TABEL00F;
036900110422         if  %error   and   rtnEsito = *zero;
037000110422           rtnEsito = c_ErrClose;
037100110422         endif;
037200110422       endif;
037300110422
037400110422       // -?Chiusura archivio TNTBE?
037500110422       if  %open(TNTBE01L);
037600110422         close(E)  TNTBE01L;
037700110422         if  %error   and   rtnEsito = *zero;
037800110422           rtnEsito = c_ErrClose;
037900110422         endif;
038000110422       endif;
038100110422
038200110422       // -?Valorizzazione opcode di metodo?
038300110422       $InitDone = *off;
038400110422
038500110422       return  rtnEsito;
038600110422
038700110422      /end-free
038800110422
038900110422     p UBNRSRSC_Close...
039000110422     p                 E
