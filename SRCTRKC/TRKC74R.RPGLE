000100150115       //==============================================================
000200150115       //?TRKC74R * Riattivazione Cliente in Campagna commerciale.     ?
000300150115       //==============================================================
000400150115
000500150115       //--------------------------------------------------------------
000600150115       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150115       //--------------------------------------------------------------
000800150115
000900150115     /*PRM  dbgview(*source)
001000150115     /*END
001100150115
001200150115       //--------------------------------------------------------------
001300150115       //?Specifiche di controllo.                                     ?
001400150115       //--------------------------------------------------------------
001500150115
001600150115     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150115     h dftactgrp(*no)
001800150115     h bnddir('TRUL')
001900150115
002000150115       //--------------------------------------------------------------
002100150115       //?Dichiarazione file.                                          ?
002200150115       //--------------------------------------------------------------
002300150115
002400150115       // -?Fasi Avanzamento Clienti in Campagne commerciali?
002500150122     fTICMF02L  if   e           k disk
002600150115
002700150115       // -?Anagrafica Clienti in Campagna?
002800150115     fTICMC01L  Uf   e           k disk
002900150115
003000150115       //--------------------------------------------------------------
003100150115       //?Definizione costanti.                                        ?
003200150115       //--------------------------------------------------------------
003300150115
003400150116       // -?Fase di Sblocco?
003500150116     d c_FaseSblocco   c                   const(' SB')
003600150115
003700150115       //--------------------------------------------------------------
003800150115       //?Definizione schiere.                                         ?
003900150115       //--------------------------------------------------------------
004000150115
004100150115       // -?Messaggi di errore?
004200150116     d sk_Msg          s             78    dim( 3)  ctdata  perrcd( 1)
004300150115
004400150115       //--------------------------------------------------------------
004500150115       //?Definizione aree dati.                                       ?
004600150115       //--------------------------------------------------------------
004700150115
004800150115       // -?Dati utente?
004900150115     d §AzUte        e ds                  extname(AZUTE00F)
005000150115     d                                     dtaara
005100150115     d §DatiUte      e ds                  extname(dDatiUte)
005200150115     d                                     dtaara
005300150115
005400150115       //--------------------------------------------------------------
005500150115       //?Definizione strutture dati.                                  ?
005600150115       //--------------------------------------------------------------
005700150115
005800150115       // -?Parametri ricevuti?
005900150115     d KPJBA         e ds
006000150115     d TRKC74ds      e ds                  inz
006100150115
006200150115       // -?Fasi Campagne Comm.li?
006300150115     d dFCM          e ds                  inz
006400150115
006500150115       //--------------------------------------------------------------
006600150115       //?Definizione variabili globali.                               ?
006700150115       //--------------------------------------------------------------
006800150115
006900150115       // -?Flags booleani?
007000150115     d $UpdFaseCMC     s               n   inz
007100150115
007200150115       //--------------------------------------------------------------
007300150115       //?Definizione prototipi procedure.                             ?
007400150115       //--------------------------------------------------------------
007500150115
007600150115       // -?Reperimento dati utente?
007700150115     d TIBS34ds      e ds                  inz
007800150115      /copy gaitrasrc/srcProtoPR,TIBS34R
007900150122
008000150122       // -?Scrittura fase (di Sblocco) per cliente in Campagna?
008100150122     d TRKC71ds      e ds                  inz
008200150122      /copy gaitrasrc/srcProtoPR,TRKC71R
008300150115
008400150115       // -?Reperimento dati tabelle?
008500150115      /copy gaitrasrc/srcProtoPI,TRULTAB
008600150115      /copy gaitrasrc/srcProtoPR,TRULTAB
008700150115
008800150115       //--------------------------------------------------------------
008900150115       //?Definizione key-list.                                        ?
009000150115       //--------------------------------------------------------------
009100150115
009200150115       // -?File TICMF02L: Fasi Avanzamento Clienti in Campagne comm.li?
009300150115     d keyTICMF02    e ds                  extname(TICMF02L : *key)
009400150115     d                                     prefix(k_)   inz
009500150115
009600150115       // -?File TICMC01L: Anagrafica Clienti in Campagna comm.le?
009700150115     d keyTICMC01    e ds                  extname(TICMC01L : *key)
009800150115     d                                     prefix(k_)   inz
009900150115
010000150115       //--------------------------------------------------------------
010100150115       //?M A I N - L I N E                                            ?
010200150115       //--------------------------------------------------------------
010300150115
010400150115     c     *Entry        plist
010500150115     c                   parm                    KPJBA
010600150115
010700150115      /free
010800150115
010900150115       // -?Operazioni iniziali?
011000150115       exsr sr_RoutInz;
011100150115
011200150115       // -?Riapertura Campagna commerciale per il Cliente?
011300150115       exsr  sr_RiaperturaCampagna;
011400150115
011500150115       // -?Operazioni finali?
011600150115       exsr sr_RoutEnd;
011700150115
011800150115       //--------------------------------------------------------------
011900150115       //?Operazioni iniziali.                                         ?
012000150115       //--------------------------------------------------------------
012100150115       BEGSR  sr_RoutInz;
012200150115
012300150115         *inLR = *on;
012400150115
012500150115         // -?Reperimento parametri ricevuti?
012600150115         TRKC74ds = KPJBU;
012700150115
012800150115         // -?Reperimento dati job?
012900150115         exsr  sr_DatiJob;
013000150115
013100150115         // -?Controlli iniziali:?
013200150115         oKC74err = *off;
013300150115         clear  oKC74msg;
013400150115         // ·?Cliente NON in campagna?
013500150115         clear  keyTICMC01;
013600150115         k_CMCncm = iKC74ncm;
013700150115         k_CMCksu = iKC74ksu;
013800150115         k_CMCksc = iKC74ksc;
013900150115         k_CMCcpo = iKC74cpo;
014000150115         chain  %kds( keyTICMC01 )  TICMC000;
014100150115         if  Not  %found( TICMC01L );
014200150115           oKC74err = *on;
014300150115           oKC74msg = sk_Msg(01);
014400150115           exsr  sr_RoutEnd;
014500150115         endif;
014600150115         // ·?Campagna NON chiusa per il cliente?
014700150115         if  CMCcch = *blank;
014800150122           Unlock  TICMC01L;
014900150115           oKC74err = *on;
015000150115           oKC74msg = sk_Msg(02);
015100150115           exsr  sr_RoutEnd;
015200150115         endif;
015300150115
015400150115       ENDSR;
015500150115
015600150115       //--------------------------------------------------------------
015700150115       //?Reperimento Dati del job (Utente/Operativi).                 ?
015800150115       //--------------------------------------------------------------
015900150115       BEGSR  sr_DatiJob;
016000150115
016100150115         in(e) §AzUte;
016200150115         if NOT %error;
016300150115           in(e) §DatiUte;
016400150115         endif;
016500150115         if %error or RSut = *blank;
016600150115           tibs34r ( tibs34ds );
016700150115           in §AzUte;
016800150115           in §DatiUte;
016900150115         endif;
017000150115
017100150115       ENDSR;
017200150115
017300150115       //--------------------------------------------------------------
017400150115       //?Riapertura Campagna commerciale per il Cliente unificante.   ?
017500150115       //--------------------------------------------------------------
017600150115       BEGSR  sr_RiaperturaCampagna;
017700150115
017800150115         $UpdFaseCMC = *off;
017900150115
018000150115         // -?Ricerca Ultima Fase precedente il Blocco?
018100150122         //  ?(PRIMA di Sbloccare il cliente)?
018200150115         clear  keyTICMF02;
018300150115         k_CMFncm = CMCncm;
018400150115         k_CMFksu = CMCksu;
018500150115         k_CMFksc = CMCksc;
018600150115         k_CMFcpo = CMCcpo;
018700150122         setll  %kds( keyTICMF02 : 04 )  TICMF000;
018800150122         reade  %kds( keyTICMF02 : 04 )  TICMF000;
018900150115
019000150115         DoW  Not %eof(TICMF02L);
019100150115
019200150115           //If  CMFacm <> ' 90';
019300150115           If  CMFcch = *blank;
019400150115
019500150115             // -?Reperimento tab. "FCM"?
019600150115             If  CMFacm <> ds_TNTBE.TBEke1;
019700150115               clear  dFCM;
019800150115               ds_TNTBE.TBEke1 = CMFacm;
019900150115               if  getTabella ( c_Tntbe : 'FCM' : ds_TNTBE.TBEke1 :
020000150115                                *blanks : *omit : *omit :
020100150115                                *omit : *omit :
020200150115                                *omit : *omit : *omit : *omit :
020300150115                                *omit : *omit : *omit : *omit :
020400150115                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
020500150115                                = *zero      AND
020600150115                     ds_TNTBE.TBEatb = *blank;
020700150115                 dFCM = ds_TNTBE.TBEuni;
020800150115               endif;
020900150115             EndIf;
021000150115
021100150122             // -?Trovata?
021200150122             if  §FCMaggUFE = 'S';
021300150122               $UpdFaseCMC = *on;
021400150122               leave;
021500150122             endif;
021600150115
021700150115           EndIf;
021800150115
021900150115           readE  %kds( keyTICMF02 : 04 )  TICMF000;
022000150115
022100150115         EndDo;
022200150122
022300150122         // -?Controlli:?
022400150122         // ·?NON trovata ultima fase eseguita per il cliente?
022500150122         //  ?precedentemente a quella del Blocco?
022600150122         if  Not $UpdFaseCMC;
022700150122           Unlock  TICMC01L;
022800150122           oKC74err = *on;
022900150122           oKC74msg = sk_Msg(03);
023000150122           exsr  sr_RoutEnd;
023100150122         endif;
023200150122
023300150122         // -?Scrittura Fase di SBLOCCO?
023400150122         clear  TRKC71ds;
023500150122         iKC71ncm = iKC74ncm;
023600150122         iKC71ksu = iKC74ksu;
023700150122         iKC71ksc = iKC74ksc;
023800150122         iKC71cpo = iKC74cpo;
023900150122         iKC71acm = c_FaseSblocco;  //?" SB"?
024000150122         iKC71aut = 'A';            //?"A" = Fase Automatica?
024100150122
024200150122         TRKC71R ( kpjba : TRKC71ds );
024300150122
024400150122         if  oKC71err <> *blank;
024500150122           oKC74err = *on;
024600150122           oKC74msg = oKC71msg;
024700150122           exsr  sr_RoutEnd;
024800150122         endif;
024900150122
025000150122         // -?Aggiornamento ultima fase su TICMC00F?
025100150122         //  ?(con la fase precedente al Blocco)?
025200150122         if  $UpdFaseCMC;
025300150122           CMCufe = CMFacm;
025400150122         else;
025500150122           clear  CMCufe;
025600150122         endif;
025700150122         clear  CMCcch;
025800150122         clear  CMCdch;
025900150122         //______________
026000150122         update TICMC000;
026100150122         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
026200150115
026300150115       ENDSR;
026400150115
026500150115       //--------------------------------------------------------------
026600150115       //?Operazioni finali.                                           ?
026700150115       //--------------------------------------------------------------
026800150115       BEGSR  sr_RoutEnd;
026900150115
027000150115         // -?Chiusura applicazioni precedentemente aperte?
027100150115         cloTabella ( c_Tntbe );
027200150116
027300150116         // -?Restituzione parametri?
027400150116         kpjbu = TRKC74ds;
027500150115
027600150115         // -?Uscita?
027700150115         return;
027800150115
027900150115       ENDSR;
028000150115
028100150115      /end-free
028200150115
028300150115       //--------------------------------------------------------------
028400150115       //?Definizione schiere a tempo di compilazione                  ?
028500150115       //--------------------------------------------------------------
028600150115
028700150115** -?sk_Msg:?Messaggi di Errore?..+....4....+....5....+....6....+....7....+...|
028800150115Cliente non in campagna                                                       |1
028900150115Campagna non chiusa per il cliente                                            |2
029000150116Non reperita l'ultima fase eseguita per il cliente in campagna                |3
