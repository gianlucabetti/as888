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
003100150716     d c_Motivo_DisposizConsegna...
003200150716     d                 c                   const(' 86')
003300140925
003400140925       //--------------------------------------------------------------
003500140925       //?Definizione schiere.                                         ?
003600140925       //--------------------------------------------------------------
003700140925
003800140925       // -?Messaggi di errore?
003900140930     d sk_Msg          s             78    dim( 2)  ctdata  perrcd(1)
004000140925
004100140925       //--------------------------------------------------------------
004200140925       //?Definizione aree dati.                                       ?
004300140925       //--------------------------------------------------------------
004400140925
004500140925       // -?Dati utente?
004600140925     d §AzUte        e ds                  extname(AZUTE00F)
004700140925     d                                     dtaara
004800140925     d §DatiUte      e ds                  extname(dDatiUte)
004900140925     d                                     dtaara
005000140925
005100140925       //--------------------------------------------------------------
005200140925       //?Definizione strutture dati.                                  ?
005300140925       //--------------------------------------------------------------
005400140925
005500140925       // -?Parametri ricevuti?
005600140925     d FIDNA7ds      e ds                  qualified
005700141003         // -?Note (8) ricevute nel campo FIDNA7ds.iDNA7NOT?
005800140926     d   sk_iDNA7not                       overlay( iDNA7not )
005900140926     d                                     like(FIDNB0ds.DB0no1)
006000141003     d                                     dim( 8)
006100140925
006200140926       // -?Tab. "CHR" = Chiusura Richieste?
006300140926     d dCHR          e ds                  qualified      inz
006400140925       // -?Tab. "MAR" = Motivi Apertura Reclami?
006500140925     d dMAR          e ds                  qualified      inz
006600140926
006700140926       // -?Descrizione esterna del parametro di GES.AMB.APP?
006800140926     d KPJBA         e ds                  qualified      inz
006900140925
007000140925       //--------------------------------------------------------------
007100140925       //?Definizione variabili globali.                               ?
007200140925       //--------------------------------------------------------------
007300140925
007400140925       // -?Campi di comodo?
007500140925     d wTime14a        s             14    inz
007600140925     d wTime           s              6  0 inz
007700140925     d wDate           s              8  0 inz
007800140925
007900140925       //--------------------------------------------------------------
008000140925       //?Definizione prototipi procedure.                             ?
008100140925       //--------------------------------------------------------------
008200140925
008300140925       // -?Reperimento dati utente?
008400140925     d TIBS34ds      e ds
008500140925      /copy gaitrasrc/srcProtoPR,TIBS34R
008600140925
008700140925       // -?Reperimento dati tabelle?
008800140925      /copy gaitrasrc/srcProtoPI,TRULTAB
008900140925      /copy gaitrasrc/srcProtoPR,TRULTAB
009000140926
009100140926       // -?Note R.A.?
009200140926     d FIDNB0ds      e ds                  qualified    inz
009300141003     d*//iB0tla      e                     inz('L')
009400140926     d   iB0flm      e                     inz('W')
009500141003     d   iB0pgn      e                     inz(*hival)
009600140926     d fidnB0r         pr                  extpgm('FIDNB0R')
009700140926     d   kpjba                             likeds(KPJBA)
009800140926     d   fidnB0ds                          likeds(FIDNB0ds)
009900140925
010000140925       //--------------------------------------------------------------
010100140925       //?Definizione key-list.                                        ?
010200140925       //--------------------------------------------------------------
010300140925
010400140925       // -?File FITGD01L?
010500140925     d keyFITGD01    e ds                  extname(FITGD01L : *key)
010600140925     d                                     prefix(k_)   inz
010700140925
010800140925       //--------------------------------------------------------------
010900140925       //?M A I N - L I N E                                            ?
011000140925       //--------------------------------------------------------------
011100140925
011200140925     c     *Entry        plist
011300140925     c                   parm                    FIDNA7ds
011400140925
011500140925      /free
011600140925
011700140925       // -?Operazioni iniziali?
011800140925       exsr  sr_RoutInz;
011900140925
012000140925       // -?Chiusura R.A.?
012100140925       exsr  sr_ChiusuraRA;
012200140925
012300140925       // -?Operazioni finali?
012400140925       exsr  sr_RoutEnd;
012500140925
012600140925       //--------------------------------------------------------------
012700140925       //?Operazioni iniziali.                                         ?
012800140925       //--------------------------------------------------------------
012900140925       BEGSR  sr_RoutInz;
013000140925
013100140926         *inLR  = *on;
013200140929
013300140929         clear  FIDNA7ds.oDNA7err;
013400140926         clear  FIDNA7ds.oDNA7msg;
013500140929
013600140929         // -?Controllo parametri ricevuti?
013700140929         if  FIDNA7ds.iDNA7ant <= *zero  or
013800140929             FIDNA7ds.iDNA7nut <= *zero  or
013900140929             FIDNA7ds.iDNA7pdt <= *zero  or
014000140929             FIDNA7ds.iDNA7pct <= *zero  or
014100140929             FIDNA7ds.iDNA7utc  = *blank;
014200140929           FIDNA7ds.oDNA7err = 'E';
014300140929           FIDNA7ds.oDNA7msg = sk_Msg(01);
014400140929           exsr  sr_RoutEnd;
014500140929         endif;
014600140925
014700140925         // -?Reperimento dati job?
014800140925         exsr  sr_DatiJob;
014900140925
015000140925         // -?Recupero Data & Ora attuali?
015100140925         wTime14a = %subst( %char( %dec( %timestamp() ) ) : 1 : 14 );
015200140925         wDate = %int( %subst( wTime14a : 1 : 8 ) );
015300140925         wTime = %int( %subst( wTime14a : 9 : 6 ) );
015400140925
015500140925         // -?Reperimento dati R.A. e verifica se già chiusa?
015600140925         clear  keyFITGD01;
015700140925         k_TGDant = FIDNA7ds.iDNA7ant;
015800140925         k_TGDnut = FIDNA7ds.iDNA7nut;
015900140925         k_TGDpdt = FIDNA7ds.iDNA7pdt;
016000140925
016100140925         chain  %kds( keyFITGD01 )  FITGD000;
016200140925
016300140925         select;
016400140925           // -?R.A. non presente: uscita CON errore?
016500140925           when  Not  %found(FITGD01L);
016600140929             FIDNA7ds.oDNA7err = 'E';
016700140930             FIDNA7ds.oDNA7msg = sk_Msg(02);
016800140925             exsr  sr_RoutEnd;
016900140925           // -?R.A. presente e già chiusa: uscita SENZA errore?
017000140925           when  TGDcdt <> *blank;
017100140925             exsr  sr_RoutEnd;
017200140925         endsl;
017300140925
017400140925       ENDSR;
017500140925
017600140925       //--------------------------------------------------------------
017700140925       //?Reperimento Dati del job (Utente/Operativi).                 ?
017800140925       //--------------------------------------------------------------
017900140925       BEGSR  sr_DatiJob;
018000140925
018100140925         in(E) §AzUte;
018200140925         if NOT %error;
018300140925           in(E) §DatiUte;
018400140925         endif;
018500140925         if %error or RSut = *blanks;
018600140925           clear TIBS34ds;
018700140925           tibs34r ( tibs34ds );
018800140925           in §AzUte;
018900140925           in §DatiUte;
019000140925         endif;
019100140925
019200140925       ENDSR;
019300140925
019400140925       //--------------------------------------------------------------
019500140925       //?Chiusura R.A.                                                ?
019600140925       //--------------------------------------------------------------
019700140925       BEGSR  sr_ChiusuraRA;
019800140925
019900140925         // -?Reperimento Codice Chiusura dal Motivo Apertura Dettaglio?
020000140925         //  ?(SE non ricevuto)?
020100140925         clear dMAR;
020200140925         IF  FIDNA7ds.iDNA7cdt = *blank;
020300140925
020400140925           ds_TNTBE.TBEke1 = TGDmad;
020500140926           If  getTabella ( c_Tntbe : 'MAR' : ds_TNTBE.TBEke1 :
020600140925                            *omit : *blank : *blank :
020700140925                            *omit : *omit :
020800140925                            *omit : *omit : *omit : *omit :
020900140925                            *omit : *omit : *omit : *omit :
021000140925                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
021100140925                        = *zero      AND
021200140925               ds_TNTBE.TBEatb = *blank;
021300140925
021400140925             dMAR = ds_TNTBE.TBEuni;
021500140925             if  dMAR.§MARmotCh = *blank;
021600140925               // ·?Motivo Chiusura di default?
021700140925               dMAR.§MARmotCh = '  I';
021800140925             endif;
021900140925
022000140925           EndIf;
022100140925
022200140925         ENDIF;
022300150326
022400150326         // -?Stato (ex Priorità)?
022500150326         TGDprt = '7';
022600140925
022700140925         // -?Impostazione Codice Motivo Chiusura?
022800140925         if  FIDNA7ds.iDNA7cdt <> *blank;
022900140925           TGDcdt = FIDNA7ds.iDNA7cdt;
023000140925         else;
023100140925           TGDcdt = dMAR.§MARmotCh;
023200140925         endif;
023300140925
023400140925         // -?Impostazione Data Chiusura Dettaglio?
023500140925         if  FIDNA7ds.iDNA7dcd <> *zero;
023600140925           TGDdcd = FIDNA7ds.iDNA7dcd;
023700140925         else;
023800140925           TGDdcd = wDate;
023900140925         endif;
024000140925
024100140925         // -?Impostazione Ora Chiusura Dettaglio?
024200140925         if  FIDNA7ds.iDNA7dcd <> *zero;
024300140925           TGDocd = FIDNA7ds.iDNA7ocd;
024400140925         else;
024500140925           TGDocd = wTime;
024600140925         endif;
024700140925
024800140925         // -?Impostazione Filiale Chiusura?
024900140930         TGDpct = FIDNA7ds.iDNA7pct;
025000140925
025100140925         // -?Impostazione Profilo Utente Chiusura?
025200140930         TGDutc = FIDNA7ds.iDNA7utc;
025300150716
025400150716         // -?Impostazione Profilo Utente Gestione?
025500150716         //  ?(SE motivo " 86")?
025600150716         if  TGDmad = c_Motivo_DisposizConsegna;
025700150716           TGDrpg = FIDNA7ds.iDNA7utc;
025800150716         endif;
025900140926
026000140926         // -?Reperimento flag Tipo Responsabilità dal codice Chiusura?
026100140926         //  ?Reclami (SE non ricevuto)?
026200140926         clear dCHR;
026300140926         IF  FIDNA7ds.iDNA7rsp = *blank;
026400140926
026500140926           ds_TNTBE.TBEke1 = TGDcdt;
026600140926           If  getTabella ( c_Tntbe : 'CHR' : ds_TNTBE.TBEke1 :
026700140926                            *omit : *blank : *blank :
026800140926                            *omit : *omit :
026900140926                            *omit : *omit : *omit : *omit :
027000140926                            *omit : *omit : *omit : *omit :
027100140926                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
027200140926                        = *zero      AND
027300140926               ds_TNTBE.TBEatb = *blank;
027400140926
027500140926             dCHR = ds_TNTBE.TBEuni;
027600140926
027700140926           EndIf;
027800140926
027900140926         ENDIF;
028000140926
028100140926         // -?Impostazione flag Responsabilità?
028200140926         if  FIDNA7ds.iDNA7rsp <> *blank;
028300140926           TGDrsp = FIDNA7ds.iDNA7rsp;
028400140926         else;
028500140926           TGDrsp = dCHR.§CHRresp;
028600140926         endif;
028700140926
028800140926         // -?Impostazione Filiale Responsabile?
028900140929         TGDpor = FIDNA7ds.iDNA7por;
029000140929         // -?SE Fil.Responsabile = *zero & Cod.Chiusura = Fondato?
029100140930         //  ?=> Filiale di Apertura?
029200140929         if  TGDpor = *zero  and  TGDcdt = '  F';
029300140929           TGDpor = TGDpot;
029400140929         endif;
029500140926
029600140926
029700140926         // -?AGGIORNAMENTO REC. FILE FITGD00F?
029800140926         //_______________
029900140926         update  FITGD000;
030000140926         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
030100140926
030200140926
030300140926         // -?Aggiunta delle Note?
030400141006         //  ?(le note vuote verranno escluse dal chiamato)?
030500141006         // ·?Prime 4 note?
030600141006         If  %subst( FIDNA7ds.iDNA7not : 1 : %size( FIDNB0ds.DB0no1 )
030700141006                                             * 4 ) <> *blank;
030800140926
030900140926           clear  KPJBA;
031000140926           reset  FIDNB0ds;
031100140926
031200140926           FIDNB0ds.iB0ant = TGDant;
031300140926           FIDNB0ds.iB0nut = TGDnut;
031400140926           FIDNB0ds.iB0pdt = TGDpdt;
031500140926           FIDNB0ds.iB0tin = 'GEN';
031600140926           FIDNB0ds.iB0pot = TGDpot;
031700140926           FIDNB0ds.dB0utn = TGDutc;
031800140926           FIDNB0ds.dB0pon = TGDpct;
031900141006           FIDNB0ds.DB0no1 = FIDNA7ds.sk_iDNA7not(1);
032000141006           FIDNB0ds.DB0no2 = FIDNA7ds.sk_iDNA7not(2);
032100141006           FIDNB0ds.DB0no3 = FIDNA7ds.sk_iDNA7not(3);
032200141006           FIDNB0ds.DB0no4 = FIDNA7ds.sk_iDNA7not(4);
032300140926
032400140926           fidnB0r ( kpjba : FIDNB0ds );
032500141006
032600141006         EndIf;
032700141003
032800141006         // ·?Ultime 4 note?
032900141006         If  %subst( FIDNA7ds.iDNA7not : ( %size( FIDNB0ds.DB0no1 )
033000141006                                         * 4 ) + 1 ) <> *blank;
033100141006
033200141006           clear  KPJBA;
033300141006           reset  FIDNB0ds;
033400141006
033500141006           FIDNB0ds.iB0ant = TGDant;
033600141006           FIDNB0ds.iB0nut = TGDnut;
033700141006           FIDNB0ds.iB0pdt = TGDpdt;
033800141006           FIDNB0ds.iB0tin = 'GEN';
033900141006           FIDNB0ds.iB0pot = TGDpot;
034000141006           FIDNB0ds.dB0utn = TGDutc;
034100141006           FIDNB0ds.dB0pon = TGDpct;
034200141006           FIDNB0ds.DB0no1 = FIDNA7ds.sk_iDNA7not(5);
034300141006           FIDNB0ds.DB0no2 = FIDNA7ds.sk_iDNA7not(6);
034400141006           FIDNB0ds.DB0no3 = FIDNA7ds.sk_iDNA7not(7);
034500141006           FIDNB0ds.DB0no4 = FIDNA7ds.sk_iDNA7not(8);
034600141006
034700141006           fidnB0r ( kpjba : FIDNB0ds );
034800140926
034900141006         EndIf;
035000140925
035100140925       ENDSR;
035200140925
035300140925       //--------------------------------------------------------------
035400140925       //?Operazioni finali.                                           ?
035500140925       //--------------------------------------------------------------
035600140925       BEGSR  sr_RoutEnd;
035700141003
035800141003         // -?Chiusura applicazioni?
035900141003         clear  KPJBA;
036000141003         clear  FIDNB0ds;
036100141003         FIDNB0ds.iB0tla = 'C';
036200141003         fidnB0r ( kpjba : FIDNB0ds );
036300140925
036400140925         // -?Chiusura pgm?
036500140925         return;
036600140925
036700140925       ENDSR;
036800140925
036900140925      /end-free
037000140925
037100140925       //--------------------------------------------------------------
037200140925       //?Definizione schiere a tempo di compilazione                  ?
037300140925       //--------------------------------------------------------------
037400140925
037500140925** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
037600140929NON ricevuti parametri obbligatori                                              1
037700140930R.A. non presente                                                               2
