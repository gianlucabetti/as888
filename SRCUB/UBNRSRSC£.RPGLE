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
016500110422       //if  %parms() > 1   and   %addr(pOutNRS) <> *null;
016600110422         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
016700110422           wLibFile1 = 'GAITRAGRU/TABEL00F';
016800110422         else;
016900110422           wLibFile1 = 'GAITRAGRPS/TABEL00F';
017000110422         endif;
017100110422       //endif;
017200110422       //if  (%parms() > 2  and  %addr(pOutNSI) <> *null)  OR
017300110422       //    (%parms() > 3  and  %addr(pOutNSF) <> *null);
017400110422         if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
017500110422           wLibFile2 = 'GAITRAGRU/TNTBE01L';
017600110422         else;
017700110422           wLibFile2 = 'GAITRAGRPS/TNTBE01L';
017800110422         endif;
017900110422       //endif;
018000110422
018100110422       //if  %parms() > 1  and  %addr(pOutNRS) <> *null
018200110422       //                  and  Not %open(TABEL00F);
018300110422         if  Not %open(TABEL00F);
018400110422           open  TABEL00F;
018500110422       endif;
018600110422
018700110422       //if  ( (%parms() > 2  and  %addr(pOutNSI) <> *null)    OR
018800110422       //      (%parms() > 3  and  %addr(pOutNSF) <> *null) )  and
018900110422       //    Not %open(TNTBE01L);
019000110422         if  Not %open(TNTBE01L);
019100110422           open  TNTBE01L;
019200110422         endif;
019300110422
019400110422       // -?Valorizzazione variabili?
019500110422       $InitDone = *on;
019600110422
019700110422       return  rtnEsito;
019800110422
019900110422      /end-free
020000110422
020100110422     p UBNRSRSC_Init...
020200110422     p                 E
020300110422
020400110422
020500110422       //--------------------------------------------------------------
020600110422       //?Procedure name: UBNRSRSC_Rtv                                  ?
020700110422       // Purpose:        Reperimento Num. Serie & Range SegnaColli
020800110422       //                 del cliente ricevuto come parametro.
020900110422       // Returns:        Integer (Status_Code)
021000110422       // Parameter:      pInKSC        => Codice Cliente
021100110422       //                 pOutNRS       => Numero Serie
021200110422       //                 pOutNSI       => Numero Segnacollo Iniziale
021300110422       //                 pOutNSF       => Numero Segnacollo Finale
021400110422       //--------------------------------------------------------------
021500110422
021600110422     p UBNRSRSC_Rtv...
021700110422     p                 B                   export
021800110422
021900110422     d UBNRSRSC_Rtv...
022000110422     d                 PI            10i 0
022100110422     d  pInKSC                             like(ds3C.§3Ccks)  const
022200110422     d  pOutNRS                            like(ds3C.§3Cnrs)
022300110422     d                                     options(*nopass : *omit)
022400110422     d  pOutNSI                            like(d3EW.§3EWnsi)
022500110422     d                                     options(*nopass : *omit)
022600110422     d  pOutNSF                            like(d3EW.§3EWnsf)
022700110422     d                                     options(*nopass : *omit)
022800110422
022900110422      /free
023000110422
023100110422       // -?Inizializzazione parametri di output?
023200110422       reset  rtnEsito;
023300110422       if  %parms() > 1  and  %addr(pOutNRS) <> *null;
023400110422         clear  pOutNRS;
023500110422       endif;
023600110422       if  %parms() > 2  and  %addr(pOutNSI) <> *null;
023700110422         clear  pOutNSI;
023800110422       endif;
023900110422       if  %parms() > 3  and  %addr(pOutNSF) <> *null;
024000110422         clear  pOutNSF;
024100110422       endif;
024200110422
024300110422       // -?Se non già inizializzata la procedure?
024400110422       //  ?=> si forza l'inizializzazione?
024500110422       if  not $InitDone;
024600110422         rtnEsito = UBNRSRSC_Init;
024700110422       endif;
024800110422       // -?Se ricevuto errore => uscita?
024900110422       if  rtnEsito < *zero;
025000110422         return  rtnEsito;
025100110422       endif;
025200110422
025300110422       // -?Reperimento Numero Serie da tab. "3C"?
025400110422       //  ?per il cliente ricevuto come parametro?
025500110422       //  ?=> da fare COMUNQUE, perchè la serie serve anche DOPO?
025600110422       IF  (%parms() > 1  and  %addr(pOutNRS) <> *null)  OR
025700110422           (%parms() > 2  and  %addr(pOutNSI) <> *null)  OR
025800110422           (%parms() > 3  and  %addr(pOutNSF) <> *null);
025900110422
026000110422         clear  ds3C;
026100110422         k_TBLkut = 1;
026200110422         k_TBLcod = '3C';
026300110422         k_TBLkey = %editc( pInKSC : 'X');
026400110422         chain  %kds( k03tabel00 )  TABEL;
026500110422
026600110422         // -?Cliente NON trovato in tab. "3C" => errore?
026700110422         if  NOT %found(TABEL00F);
026800110422           rtnEsito = c_NotFound3C;
026900110422           return  rtnEsito;
027000110422         endif;
027100110422
027200110422         // -?Impostazione parametri di output per il cliente ricevuto:?
027300110422         ds3C = TBLuni;
027400110422
027500110422         //   -?Numero Serie?
027600110422         if  %parms() > 1  and  %addr(pOutNRS) <> *null;
027700110422           pOutNRS = ds3C.§3Cnrs;
027800110422         endif;
027900110422
028000110422       ENDIF;
028100110422
028200110422       // -?Reperimento Range Segnacolli da tab. "3CP"/"3EW"?
028300110422       //  ?per il cliente ricevuto come parametro?
028400110422       IF  ( (%parms() > 2  and  %addr(pOutNSI) <> *null)    or
028500110422             (%parms() > 3  and  %addr(pOutNSF) <> *null) )  and
028600110422           ds3C.§3Cnrs > *zero;
028700110422
028800110422         clear  d3CP;
028900110422         clear  d3EW;
029000110422
029100110422         If  ds3C.§3Ccba <> 'ESWEB';
029200110422
029300110422           k_TBEcod = '3CP';
029400110422           k_TBEke1 = %editc( pInKSC : 'X');
029500110422           k_TBEke2 = %editc( ds3C.§3Cnrs : 'X' );
029600110422           setll  %kds( k05tntbe01 : 3 )  TNTBE000;
029700110422           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
029800110422           DoW  Not %eof(TNTBE01L);
029900110422             if  %subst( TBEke2 : 1 : 2 ) = %editc( ds3C.§3Cnrs : 'X' )
030000110422                 and  TBEatb = *blank;
030100110422               d3CP = TBEuni;
030200110422               if  %parms() > 2  and  %addr(pOutNSI) <> *null;
030300110422                 pOutNSI = %int( %subst( TBEke2 : 3 : 7 ) );
030400110422                 pOutNSF = d3CP.§3CPal;
030500110422                 leave;
030600110422               endif;
030700110422             endif;
030800110422             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
030900110422           EndDo;
031000110422
031100110422         Else;
031200110422
031300110422           k_TBEcod = '3EW';
031400110422           k_TBEke1 = %editc( ds3C.§3Ccks : 'X' );
031500110422           clear  k_TBEke2;
031600110422           clear  k_TBElin;
031700110422           clear  k_TBEsif;
031800110422           setll  %kds( k05tntbe01 )  TNTBE000;
031900110422           reade  %kds( k05tntbe01 : 2 )  TNTBE000;
032000110422           DoW  Not %eof(TNTBE01L);
032100110422             d3CP = TBEuni;
032200110422             if  %parms() > 2  and  %addr(pOutNSI) <> *null
032300110422                               and  d3EW.§3EWnrs = ds3C.§3Cnrs;
032400110422               pOutNSI = d3EW.§3EWnsi;
032500110422               pOutNSF = d3EW.§3EWnsf;
032600110422               leave;
032700110422             endif;
032800110422             reade  %kds( k05tntbe01 : 2 )  TNTBE000;
032900110422           EndDo;
033000110422
033100110422         EndIf;
033200110422
033300110422       ENDIF;
033400110422
033500110422       return  rtnEsito;
033600110422
033700110422      /end-free
033800110422
033900110422     p UBNRSRSC_Rtv...
034000110422     p                 E
034100110422
034200110422       //--------------------------------------------------------------
034300110422       //?Procedure name: UBNRSRSC_Close                               ?
034400110422       // Purpose:        Chiusura archivi e cancellazione override
034500110422       // Returns:        Integer (Status_Code)
034600110422       //--------------------------------------------------------------
034700110422
034800110422     p UBNRSRSC_Close...
034900110422     p                 B                   export
035000110422
035100110422     d UBNRSRSC_Close...
035200110422     d                 PI            10i 0
035300110422
035400110422      /free
035500110422
035600110422
035700110422       // -?Inizializzazione dei parametri di output?
035800110422       reset  rtnEsito;
035900110422
036000110422       // -?Chiusura archivio TABEL?
036100110422       if  %open(TABEL00F);
036200110422         close(E)  TABEL00F;
036300110422         if  %error   and   rtnEsito = *zero;
036400110422           rtnEsito = c_ErrClose;
036500110422         endif;
036600110422       endif;
036700110422
036800110422       // -?Chiusura archivio TNTBE?
036900110422       if  %open(TNTBE01L);
037000110422         close(E)  TNTBE01L;
037100110422         if  %error   and   rtnEsito = *zero;
037200110422           rtnEsito = c_ErrClose;
037300110422         endif;
037400110422       endif;
037500110422
037600110422       // -?Valorizzazione opcode di metodo?
037700110422       $InitDone = *off;
037800110422
037900110422       return  rtnEsito;
038000110422
038100110422      /end-free
038200110422
038300110422     p UBNRSRSC_Close...
038400110422     p                 E
