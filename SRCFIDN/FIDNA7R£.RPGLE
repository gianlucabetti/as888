000100140925       //==============================================================
000200140925       //?FIDNA7R - Driver chiusura R.A. disposizioni da internet      ?
000300140925       //==============================================================
000400140925
000500140925       //--------------------------------------------------------------
000600140925       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140925       //--------------------------------------------------------------
000800140925
000900140925     /*PRM  dbgview(*source)
001000140925     /*END
001100140925
001200140925       //--------------------------------------------------------------
001300140925       //?Specifiche di controllo.                                     ?
001400140925       //--------------------------------------------------------------
001500140925
001600140925     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140925     h dftactgrp(*no)
001800140925     h bnddir('UBRTVNETA':'TRUL')
001900140925
002000140925       //--------------------------------------------------------------
002100140925       //?Dichiarazione file.                                          ?
002200140925       //--------------------------------------------------------------
002300140925
002400140925       // -?Gestione Telefonate: Dettaglio?
002500140925     fFITGD01L  Uf   e           k disk
002600140925
002700140925       //--------------------------------------------------------------
002800140925       //?Definizione costanti.                                        ?
002900140925       //--------------------------------------------------------------
003000140925
003100140925
003200140925       //--------------------------------------------------------------
003300140925       //?Definizione schiere.                                         ?
003400140925       //--------------------------------------------------------------
003500140925
003600140925       // -?Messaggi di errore?
003700140930     d sk_Msg          s             78    dim( 2)  ctdata  perrcd(1)
003800140925
003900140925       //--------------------------------------------------------------
004000140925       //?Definizione aree dati.                                       ?
004100140925       //--------------------------------------------------------------
004200140925
004300140925       // -?Dati utente?
004400140925     d §AzUte        e ds                  extname(AZUTE00F)
004500140925     d                                     dtaara
004600140925     d §DatiUte      e ds                  extname(dDatiUte)
004700140925     d                                     dtaara
004800140925
004900140925       //--------------------------------------------------------------
005000140925       //?Definizione strutture dati.                                  ?
005100140925       //--------------------------------------------------------------
005200140925
005300140925       // -?Parametri ricevuti?
005400140925     d FIDNA7ds      e ds                  qualified
005500141003         // -?Note (8) ricevute nel campo FIDNA7ds.iDNA7NOT?
005600140926     d   sk_iDNA7not                       overlay( iDNA7not )
005700140926     d                                     like(FIDNB0ds.DB0no1)
005800141003     d                                     dim( 8)
005900140925
006000140926       // -?Tab. "CHR" = Chiusura Richieste?
006100140926     d dCHR          e ds                  qualified      inz
006200140925       // -?Tab. "MAR" = Motivi Apertura Reclami?
006300140925     d dMAR          e ds                  qualified      inz
006400140926
006500140926       // -?Descrizione esterna del parametro di GES.AMB.APP?
006600140926     d KPJBA         e ds                  qualified      inz
006700140925
006800140925       //--------------------------------------------------------------
006900140925       //?Definizione variabili globali.                               ?
007000140925       //--------------------------------------------------------------
007100140925
007200140925       // -?Campi di comodo?
007300140925     d wTime14a        s             14    inz
007400140925     d wTime           s              6  0 inz
007500140925     d wDate           s              8  0 inz
007600140925
007700140925       //--------------------------------------------------------------
007800140925       //?Definizione prototipi procedure.                             ?
007900140925       //--------------------------------------------------------------
008000140925
008100140925       // -?Reperimento dati utente?
008200140925     d TIBS34ds      e ds
008300140925      /copy gaitrasrc/srcProtoPR,TIBS34R
008400140925
008500140925       // -?Reperimento dati tabelle?
008600140925      /copy gaitrasrc/srcProtoPI,TRULTAB
008700140925      /copy gaitrasrc/srcProtoPR,TRULTAB
008800140926
008900140926       // -?Note R.A.?
009000140926     d FIDNB0ds      e ds                  qualified    inz
009100141003     d*//iB0tla      e                     inz('L')
009200140926     d   iB0flm      e                     inz('W')
009300141003     d   iB0pgn      e                     inz(*hival)
009400140926     d fidnB0r         pr                  extpgm('FIDNB0R')
009500140926     d   kpjba                             likeds(KPJBA)
009600140926     d   fidnB0ds                          likeds(FIDNB0ds)
009700140925
009800140925       //--------------------------------------------------------------
009900140925       //?Definizione key-list.                                        ?
010000140925       //--------------------------------------------------------------
010100140925
010200140925       // -?File FITGD01L?
010300140925     d keyFITGD01    e ds                  extname(FITGD01L : *key)
010400140925     d                                     prefix(k_)   inz
010500140925
010600140925       //--------------------------------------------------------------
010700140925       //?M A I N - L I N E                                            ?
010800140925       //--------------------------------------------------------------
010900140925
011000140925     c     *Entry        plist
011100140925     c                   parm                    FIDNA7ds
011200140925
011300140925      /free
011400140925
011500140925       // -?Operazioni iniziali?
011600140925       exsr  sr_RoutInz;
011700140925
011800140925       // -?Chiusura R.A.?
011900140925       exsr  sr_ChiusuraRA;
012000140925
012100140925       // -?Operazioni finali?
012200140925       exsr  sr_RoutEnd;
012300140925
012400140925       //--------------------------------------------------------------
012500140925       //?Operazioni iniziali.                                         ?
012600140925       //--------------------------------------------------------------
012700140925       BEGSR  sr_RoutInz;
012800140925
012900140926         *inLR  = *on;
013000140929
013100140929         clear  FIDNA7ds.oDNA7err;
013200140926         clear  FIDNA7ds.oDNA7msg;
013300140929
013400140929         // -?Controllo parametri ricevuti?
013500140929         if  FIDNA7ds.iDNA7ant <= *zero  or
013600140929             FIDNA7ds.iDNA7nut <= *zero  or
013700140929             FIDNA7ds.iDNA7pdt <= *zero  or
013800140929             FIDNA7ds.iDNA7pct <= *zero  or
013900140929             FIDNA7ds.iDNA7utc  = *blank;
014000140929           FIDNA7ds.oDNA7err = 'E';
014100140929           FIDNA7ds.oDNA7msg = sk_Msg(01);
014200140929           exsr  sr_RoutEnd;
014300140929         endif;
014400140925
014500140925         // -?Reperimento dati job?
014600140925         exsr  sr_DatiJob;
014700140925
014800140925         // -?Recupero Data & Ora attuali?
014900140925         wTime14a = %subst( %char( %dec( %timestamp() ) ) : 1 : 14 );
015000140925         wDate = %int( %subst( wTime14a : 1 : 8 ) );
015100140925         wTime = %int( %subst( wTime14a : 9 : 6 ) );
015200140925
015300140925         // -?Reperimento dati R.A. e verifica se già chiusa?
015400140925         clear  keyFITGD01;
015500140925         k_TGDant = FIDNA7ds.iDNA7ant;
015600140925         k_TGDnut = FIDNA7ds.iDNA7nut;
015700140925         k_TGDpdt = FIDNA7ds.iDNA7pdt;
015800140925
015900140925         chain  %kds( keyFITGD01 )  FITGD000;
016000140925
016100140925         select;
016200140925           // -?R.A. non presente: uscita CON errore?
016300140925           when  Not  %found(FITGD01L);
016400140929             FIDNA7ds.oDNA7err = 'E';
016500140930             FIDNA7ds.oDNA7msg = sk_Msg(02);
016600140925             exsr  sr_RoutEnd;
016700140925           // -?R.A. presente e già chiusa: uscita SENZA errore?
016800140925           when  TGDcdt <> *blank;
016900140925             exsr  sr_RoutEnd;
017000140925         endsl;
017100140925
017200140925       ENDSR;
017300140925
017400140925       //--------------------------------------------------------------
017500140925       //?Reperimento Dati del job (Utente/Operativi).                 ?
017600140925       //--------------------------------------------------------------
017700140925       BEGSR  sr_DatiJob;
017800140925
017900140925         in(E) §AzUte;
018000140925         if NOT %error;
018100140925           in(E) §DatiUte;
018200140925         endif;
018300140925         if %error or RSut = *blanks;
018400140925           clear TIBS34ds;
018500140925           tibs34r ( tibs34ds );
018600140925           in §AzUte;
018700140925           in §DatiUte;
018800140925         endif;
018900140925
019000140925       ENDSR;
019100140925
019200140925       //--------------------------------------------------------------
019300140925       //?Chiusura R.A.                                                ?
019400140925       //--------------------------------------------------------------
019500140925       BEGSR  sr_ChiusuraRA;
019600140925
019700140925         // -?Reperimento Codice Chiusura dal Motivo Apertura Dettaglio?
019800140925         //  ?(SE non ricevuto)?
019900140925         clear dMAR;
020000140925         IF  FIDNA7ds.iDNA7cdt = *blank;
020100140925
020200140925           ds_TNTBE.TBEke1 = TGDmad;
020300140926           If  getTabella ( c_Tntbe : 'MAR' : ds_TNTBE.TBEke1 :
020400140925                            *omit : *blank : *blank :
020500140925                            *omit : *omit :
020600140925                            *omit : *omit : *omit : *omit :
020700140925                            *omit : *omit : *omit : *omit :
020800140925                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
020900140925                        = *zero      AND
021000140925               ds_TNTBE.TBEatb = *blank;
021100140925
021200140925             dMAR = ds_TNTBE.TBEuni;
021300140925             if  dMAR.§MARmotCh = *blank;
021400140925               // ·?Motivo Chiusura di default?
021500140925               dMAR.§MARmotCh = '  I';
021600140925             endif;
021700140925
021800140925           EndIf;
021900140925
022000140925         ENDIF;
022100150326
022200150326         // -?Stato (ex Priorità)?
022300150326         TGDprt = '7';
022400140925
022500140925         // -?Impostazione Codice Motivo Chiusura?
022600140925         if  FIDNA7ds.iDNA7cdt <> *blank;
022700140925           TGDcdt = FIDNA7ds.iDNA7cdt;
022800140925         else;
022900140925           TGDcdt = dMAR.§MARmotCh;
023000140925         endif;
023100140925
023200140925         // -?Impostazione Data Chiusura Dettaglio?
023300140925         if  FIDNA7ds.iDNA7dcd <> *zero;
023400140925           TGDdcd = FIDNA7ds.iDNA7dcd;
023500140925         else;
023600140925           TGDdcd = wDate;
023700140925         endif;
023800140925
023900140925         // -?Impostazione Ora Chiusura Dettaglio?
024000140925         if  FIDNA7ds.iDNA7dcd <> *zero;
024100140925           TGDocd = FIDNA7ds.iDNA7ocd;
024200140925         else;
024300140925           TGDocd = wTime;
024400140925         endif;
024500140925
024600140925         // -?Impostazione Filiale Chiusura?
024700140930         TGDpct = FIDNA7ds.iDNA7pct;
024800140925
024900140925         // -?Impostazione Profilo Utente Chiusura?
025000140930         TGDutc = FIDNA7ds.iDNA7utc;
025100140926
025200140926         // -?Reperimento flag Tipo Responsabilità dal codice Chiusura?
025300140926         //  ?Reclami (SE non ricevuto)?
025400140926         clear dCHR;
025500140926         IF  FIDNA7ds.iDNA7rsp = *blank;
025600140926
025700140926           ds_TNTBE.TBEke1 = TGDcdt;
025800140926           If  getTabella ( c_Tntbe : 'CHR' : ds_TNTBE.TBEke1 :
025900140926                            *omit : *blank : *blank :
026000140926                            *omit : *omit :
026100140926                            *omit : *omit : *omit : *omit :
026200140926                            *omit : *omit : *omit : *omit :
026300140926                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
026400140926                        = *zero      AND
026500140926               ds_TNTBE.TBEatb = *blank;
026600140926
026700140926             dCHR = ds_TNTBE.TBEuni;
026800140926
026900140926           EndIf;
027000140926
027100140926         ENDIF;
027200140926
027300140926         // -?Impostazione flag Responsabilità?
027400140926         if  FIDNA7ds.iDNA7rsp <> *blank;
027500140926           TGDrsp = FIDNA7ds.iDNA7rsp;
027600140926         else;
027700140926           TGDrsp = dCHR.§CHRresp;
027800140926         endif;
027900140926
028000140926         // -?Impostazione Filiale Responsabile?
028100140929         TGDpor = FIDNA7ds.iDNA7por;
028200140929         // -?SE Fil.Responsabile = *zero & Cod.Chiusura = Fondato?
028300140930         //  ?=> Filiale di Apertura?
028400140929         if  TGDpor = *zero  and  TGDcdt = '  F';
028500140929           TGDpor = TGDpot;
028600140929         endif;
028700140926
028800140926
028900140926         // -?AGGIORNAMENTO REC. FILE FITGD00F?
029000140926         //_______________
029100140926         update  FITGD000;
029200140926         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
029300140926
029400140926
029500140926         // -?Aggiunta delle Note?
029600141006         //  ?(le note vuote verranno escluse dal chiamato)?
029700141006         // ·?Prime 4 note?
029800141006         If  %subst( FIDNA7ds.iDNA7not : 1 : %size( FIDNB0ds.DB0no1 )
029900141006                                             * 4 ) <> *blank;
030000140926
030100140926           clear  KPJBA;
030200140926           reset  FIDNB0ds;
030300140926
030400140926           FIDNB0ds.iB0ant = TGDant;
030500140926           FIDNB0ds.iB0nut = TGDnut;
030600140926           FIDNB0ds.iB0pdt = TGDpdt;
030700140926           FIDNB0ds.iB0tin = 'GEN';
030800140926           FIDNB0ds.iB0pot = TGDpot;
030900140926           FIDNB0ds.dB0utn = TGDutc;
031000140926           FIDNB0ds.dB0pon = TGDpct;
031100141006           FIDNB0ds.DB0no1 = FIDNA7ds.sk_iDNA7not(1);
031200141006           FIDNB0ds.DB0no2 = FIDNA7ds.sk_iDNA7not(2);
031300141006           FIDNB0ds.DB0no3 = FIDNA7ds.sk_iDNA7not(3);
031400141006           FIDNB0ds.DB0no4 = FIDNA7ds.sk_iDNA7not(4);
031500140926
031600140926           fidnB0r ( kpjba : FIDNB0ds );
031700141006
031800141006         EndIf;
031900141003
032000141006         // ·?Ultime 4 note?
032100141006         If  %subst( FIDNA7ds.iDNA7not : ( %size( FIDNB0ds.DB0no1 )
032200141006                                         * 4 ) + 1 ) <> *blank;
032300141006
032400141006           clear  KPJBA;
032500141006           reset  FIDNB0ds;
032600141006
032700141006           FIDNB0ds.iB0ant = TGDant;
032800141006           FIDNB0ds.iB0nut = TGDnut;
032900141006           FIDNB0ds.iB0pdt = TGDpdt;
033000141006           FIDNB0ds.iB0tin = 'GEN';
033100141006           FIDNB0ds.iB0pot = TGDpot;
033200141006           FIDNB0ds.dB0utn = TGDutc;
033300141006           FIDNB0ds.dB0pon = TGDpct;
033400141006           FIDNB0ds.DB0no1 = FIDNA7ds.sk_iDNA7not(5);
033500141006           FIDNB0ds.DB0no2 = FIDNA7ds.sk_iDNA7not(6);
033600141006           FIDNB0ds.DB0no3 = FIDNA7ds.sk_iDNA7not(7);
033700141006           FIDNB0ds.DB0no4 = FIDNA7ds.sk_iDNA7not(8);
033800141006
033900141006           fidnB0r ( kpjba : FIDNB0ds );
034000140926
034100141006         EndIf;
034200140925
034300140925       ENDSR;
034400140925
034500140925       //--------------------------------------------------------------
034600140925       //?Operazioni finali.                                           ?
034700140925       //--------------------------------------------------------------
034800140925       BEGSR  sr_RoutEnd;
034900141003
035000141003         // -?Chiusura applicazioni?
035100141003         clear  KPJBA;
035200141003         clear  FIDNB0ds;
035300141003         FIDNB0ds.iB0tla = 'C';
035400141003         fidnB0r ( kpjba : FIDNB0ds );
035500140925
035600140925         // -?Chiusura pgm?
035700140925         return;
035800140925
035900140925       ENDSR;
036000140925
036100140925      /end-free
036200140925
036300140925       //--------------------------------------------------------------
036400140925       //?Definizione schiere a tempo di compilazione                  ?
036500140925       //--------------------------------------------------------------
036600140925
036700140925** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
036800140929NON ricevuti parametri obbligatori                                              1
036900140930R.A. non presente                                                               2
