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
002500150115     fTICMF02L  if A e           k disk
002600150115
002700150115       // -?Anagrafica Clienti in Campagna?
002800150115     fTICMC01L  Uf   e           k disk
002900150115
003000150115       //--------------------------------------------------------------
003100150115       //?Definizione costanti.                                        ?
003200150115       //--------------------------------------------------------------
003300150115
003400150115
003500150115       //--------------------------------------------------------------
003600150115       //?Definizione schiere.                                         ?
003700150115       //--------------------------------------------------------------
003800150115
003900150115       // -?Messaggi di errore?
004000150115     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
004100150115
004200150115       //--------------------------------------------------------------
004300150115       //?Definizione aree dati.                                       ?
004400150115       //--------------------------------------------------------------
004500150115
004600150115       // -?Dati utente?
004700150115     d §AzUte        e ds                  extname(AZUTE00F)
004800150115     d                                     dtaara
004900150115     d §DatiUte      e ds                  extname(dDatiUte)
005000150115     d                                     dtaara
005100150115
005200150115       //--------------------------------------------------------------
005300150115       //?Definizione strutture dati.                                  ?
005400150115       //--------------------------------------------------------------
005500150115
005600150115       // -?Parametri ricevuti?
005700150115     d KPJBA         e ds
005800150115     d TRKC74ds      e ds                  inz
005900150115
006000150115       // -?Fasi Campagne Comm.li?
006100150115     d dFCM          e ds                  inz
006200150115
006300150115       // -?Data/Ora attuali?
006400150115     d wTime_ds        ds                  inz
006500150115     d   wDate                        8s 0 inz
006600150115     d   wTime                        6s 0 inz
006700150115
006800150115       //--------------------------------------------------------------
006900150115       //?Definizione variabili globali.                               ?
007000150115       //--------------------------------------------------------------
007100150115
007200150115       // -?Flags booleani?
007300150115     d $WrtFaseCMF     s               n   inz
007400150115     d $UpdFaseCMC     s               n   inz
007500150115
007600150115       //--------------------------------------------------------------
007700150115       //?Definizione prototipi procedure.                             ?
007800150115       //--------------------------------------------------------------
007900150115
008000150115       // -?Reperimento dati utente?
008100150115     d TIBS34ds      e ds                  inz
008200150115      /copy gaitrasrc/srcProtoPR,TIBS34R
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
013100150115         // -?Reperimento data e ora attuali?
013200150115         wTime_ds = %editc( %dec( %timestamp() ) : 'X' );
013300150115
013400150115         // -?Controlli iniziali:?
013500150115         oKC74err = *off;
013600150115         clear  oKC74msg;
013700150115         // ·?Cliente NON in campagna?
013800150115         clear  keyTICMC01;
013900150115         k_CMCncm = iKC74ncm;
014000150115         k_CMCksu = iKC74ksu;
014100150115         k_CMCksc = iKC74ksc;
014200150115         k_CMCcpo = iKC74cpo;
014300150115         chain  %kds( keyTICMC01 )  TICMC000;
014400150115         if  Not  %found( TICMC01L );
014500150115           oKC74err = *on;
014600150115           oKC74msg = sk_Msg(01);
014700150115           exsr  sr_RoutEnd;
014800150115         endif;
014900150115         // ·?Campagna NON chiusa per il cliente?
015000150115         if  CMCcch = *blank;
015100150115           oKC74err = *on;
015200150115           oKC74msg = sk_Msg(02);
015300150115           exsr  sr_RoutEnd;
015400150115         endif;
015500150115
015600150115       ENDSR;
015700150115
015800150115       //--------------------------------------------------------------
015900150115       //?Reperimento Dati del job (Utente/Operativi).                 ?
016000150115       //--------------------------------------------------------------
016100150115       BEGSR  sr_DatiJob;
016200150115
016300150115         in(e) §AzUte;
016400150115         if NOT %error;
016500150115           in(e) §DatiUte;
016600150115         endif;
016700150115         if %error or RSut = *blank;
016800150115           tibs34r ( tibs34ds );
016900150115           in §AzUte;
017000150115           in §DatiUte;
017100150115         endif;
017200150115
017300150115       ENDSR;
017400150115
017500150115       //--------------------------------------------------------------
017600150115       //?Riapertura Campagna commerciale per il Cliente unificante.   ?
017700150115       //--------------------------------------------------------------
017800150115       BEGSR  sr_RiaperturaCampagna;
017900150115
018000150115         $WrtFaseCMF = *off;
018100150115         $UpdFaseCMC = *off;
018200150115
018300150115         // -?Ricerca Ultima Fase precedente il Blocco?
018400150115         clear  keyTICMF02;
018500150115         k_CMFncm = CMCncm;
018600150115         k_CMFksu = CMCksu;
018700150115         k_CMFksc = CMCksc;
018800150115         k_CMFcpo = CMCcpo;
018900150116         setll  %kds( keyTICMF02 : 04 )  TICMF000;
019000150115         readE  %kds( keyTICMF02 : 04 )  TICMF000;
019100150115
019200150115         DoW  Not %eof(TICMF02L);
019300150115
019400150115           //If  CMFacm <> ' 90';
019500150115           If  CMFcch = *blank;
019600150115
019700150115             // -?Reperimento tab. "FCM"?
019800150115             If  CMFacm <> ds_TNTBE.TBEke1;
019900150115               clear  dFCM;
020000150115               ds_TNTBE.TBEke1 = CMFacm;
020100150115               if  getTabella ( c_Tntbe : 'FCM' : ds_TNTBE.TBEke1 :
020200150115                                *blanks : *omit : *omit :
020300150115                                *omit : *omit :
020400150115                                *omit : *omit : *omit : *omit :
020500150115                                *omit : *omit : *omit : *omit :
020600150115                                ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
020700150115                                = *zero      AND
020800150115                     ds_TNTBE.TBEatb = *blank;
020900150115                 dFCM = ds_TNTBE.TBEuni;
021000150115               endif;
021100150115             EndIf;
021200150115
021300150115             // -?(Ri)Scrittura ultima fase precedente l'esclusione?
021400150115             If  Not $WrtFaseCMF;
021500150115               CMFdfa = wDate;
021600150115               CMFhfc = wTime;
021700150115               CMFfef = DUTpou;
021800150115               CMFpru = DUTute;
021900150115               //______________
022000150115               write  TICMF000;
022100150115               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
022200150115               $WrtFaseCMF = *on;
022300150115             EndIf;
022400150115
022500150115             // -?Aggiornamento Ultima Fase Campagna del Cliente?
022600150115             If  §FCMaggUFE = 'S'  and  Not $UpdFaseCMC;
022700150115               CMCufe = CMFacm;
022800150115               clear  CMCcch;
022900150115               clear  CMCdch;
023000150115               //______________
023100150115               update TICMC000;
023200150115               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
023300150115               $UpdFaseCMC = *on;
023400150115             EndIf;
023500150115
023600150115             // -?Se riscritta l'ultima fase eseguita in TICMF?
023700150115             //  ? e aggiornata l'ultima fase in TICMC => USCITA?
023800150115             if  $WrtFaseCMF  and  $UpdFaseCMC;
023900150115               leave;
024000150115             endif;
024100150115
024200150115           EndIf;
024300150115
024400150115           readE  %kds( keyTICMF02 : 04 )  TICMF000;
024500150115
024600150115         EndDo;
024700150115
024800150115         // -?Controlli finali:?
024900150115         // ·?NON trovata fase di avanzamento da riattivare per il cliente?
025000150115         if  Not $WrtFaseCMF;
025100150115           oKC74err = *on;
025200150115           oKC74msg = sk_Msg(03);
025300150115           exsr  sr_RoutEnd;
025400150115         endif;
025500150115         // ·?NON trovata ultima fase eseguita per il cliente?
025600150115         if  Not $UpdFaseCMC;
025700150115           oKC74err = *on;
025800150115           oKC74msg = sk_Msg(04);
025900150115           exsr  sr_RoutEnd;
026000150115         endif;
026100150115
026200150115       ENDSR;
026300150115
026400150115       //--------------------------------------------------------------
026500150115       //?Operazioni finali.                                           ?
026600150115       //--------------------------------------------------------------
026700150115       BEGSR  sr_RoutEnd;
026800150115
026900150115         // -?Chiusura applicazioni precedentemente aperte?
027000150115         cloTabella ( c_Tntbe );
027100150116
027200150116         // -?Restituzione parametri?
027300150116         kpjbu = TRKC74ds;
027400150115
027500150115         // -?Uscita?
027600150115         return;
027700150115
027800150115       ENDSR;
027900150115
028000150115      /end-free
028100150115
028200150115       //--------------------------------------------------------------
028300150115       //?Definizione schiere a tempo di compilazione                  ?
028400150115       //--------------------------------------------------------------
028500150115
028600150115** -?sk_Msg:?Messaggi di Errore?..+....4....+....5....+....6....+....7....+...|
028700150115Cliente non in campagna                                                       |1
028800150115Campagna non chiusa per il cliente                                            |2
028900150115Non reperita fase avanzamento da riattivare per il cliente in campagna        |3
029000150115Non reperita l'ultima fase eseguita per il cliente in campagna                |4
