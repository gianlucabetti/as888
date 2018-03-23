000100150629       //==============================================================
000200150629       //?FIDNU1R - Driver gestione Anagrafiche per Danni (FNDKA00F).  ?
000300150629       //==============================================================
000400150629
000500150629       //--------------------------------------------------------------
000600150629       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150629       //--------------------------------------------------------------
000800150629
000900150629     /*PRM  dbgview(*source)
001000150629     /*END
001100150629
001200150629       //--------------------------------------------------------------
001300150629       //?Specifiche di controllo.                                     ?
001400150629       //--------------------------------------------------------------
001500150629
001600150629     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150629     h dftactgrp(*no)
001800150629
001900150629       //--------------------------------------------------------------
002000150629       //?Dichiarazione file.                                          ?
002100150629       //--------------------------------------------------------------
002200150629
002300150629       // -?DANNI: Anagrafiche per Danni?
002400150629     fFNDKA01L  Uf A e           k disk
002500150629
002600150629       //--------------------------------------------------------------
002700150629       //?Definizione costanti.                                        ?
002800150629       //--------------------------------------------------------------
002900150629
003000150629
003100150629       //--------------------------------------------------------------
003200150629       //?Definizione schiere.                                         ?
003300150629       //--------------------------------------------------------------
003400150629
003500150629       // -?Messaggi di errore?
003600150629     d sk_Msg          s             78    dim( 9)  ctdata  perrcd(1)
003700150629
003800150629       //--------------------------------------------------------------
003900150629       //?Definizione aree dati.                                       ?
004000150629       //--------------------------------------------------------------
004100150629
004200150629       // -?Dati utente?
004300150629     d §AzUte        e ds                  extname(AZUTE00F)
004400150629     d                                     dtaara
004500150629     d §DatiUte      e ds                  extname(dDatiUte)
004600150629     d                                     dtaara
004700150629
004800150629       //--------------------------------------------------------------
004900150629       //?Definizione strutture dati.                                  ?
005000150629       //--------------------------------------------------------------
005100150629
005200150629       // -?Parametri ricevuti?
005300150629     d FIDNU1ds      e ds                  qualified
005400150629
005500150629       // -?Descrizione esterna del parametro di GES.AMB.APP?
005600150629     d KPJBA         e ds                  qualified      inz
005700150629
005800150629       //--------------------------------------------------------------
005900150629       //?Definizione variabili globali.                               ?
006000150629       //--------------------------------------------------------------
006100150629
006200150629
006300150629       //--------------------------------------------------------------
006400150629       //?Definizione prototipi procedure.                             ?
006500150629       //--------------------------------------------------------------
006600150629
006700150629       // -?Reperimento dati utente?
006800150629     d TIBS34ds      e ds
006900150629      /copy gaitrasrc/srcProtoPR,TIBS34R
007000150629
007100150629       //--------------------------------------------------------------
007200150629       //?Definizione key-list.                                        ?
007300150629       //--------------------------------------------------------------
007400150629
007500150629       // -?File FNDKA01L?
007600150629     d keyFNDKA01    e ds                  extname( FNDKA01L : *key )
007700150629     d                                     prefix( k_ )   inz
007800150629
007900150629       //--------------------------------------------------------------
008000150629       //?M A I N - L I N E                                            ?
008100150629       //--------------------------------------------------------------
008200150629
008300150629     c     *Entry        plist
008400150629     c                   parm                    FIDNU1ds
008500150629
008600150629      /free
008700150629
008800150629       // -?Operazioni iniziali?
008900150629       exsr  sr_RoutInz;
009000150629
009100150629       // -?Gestione operazione richiesta?
009200150629       Select;
009300150630
009400150914         // -?"C" = Cancellazione?
009500150629         When  FIDNU1ds.iDNU1mod = 'C';
009600150629           exsr  sr_Cancellazione;
009700150630
009800150629         // -?"R" = Reperimento?
009900150629         When  FIDNU1ds.iDNU1mod = 'R';
010000150629           exsr  sr_Reperimento;
010100150630
010200150629         // -?"S" = Scrittura?
010300150629         When  FIDNU1ds.iDNU1mod = 'S';
010400150629           exsr  sr_Scrittura;
010500150630
010600150629       EndSl;
010700150629
010800150629       // -?Operazioni finali?
010900150629       exsr  sr_RoutEnd;
011000150629
011100150629       //--------------------------------------------------------------
011200150629       //?Operazioni iniziali.                                         ?
011300150629       //--------------------------------------------------------------
011400150629       BEGSR  sr_RoutInz;
011500150629
011600150629         *inLR  = *on;
011700150629
011800150630         // -?Pulizia campi di Output?
011900150630         If  FIDNU1ds.iDNU1mod = 'C'  or
012000150630             FIDNU1ds.iDNU1mod = 'R';
012100150630           clear  FIDNU1ds.ioDNU1kcc;
012200150630           clear  FIDNU1ds.ioDNU1ksc;
012300150630           clear  FIDNU1ds.ioDNU1rag;
012400150630           clear  FIDNU1ds.ioDNU1via;
012500150630           clear  FIDNU1ds.ioDNU1cap;
012600150630           clear  FIDNU1ds.ioDNU1loc;
012700150630           clear  FIDNU1ds.ioDNU1prv;
012800150630           clear  FIDNU1ds.ioDNU1naz;
012900150630           clear  FIDNU1ds.ioDNU1iva;
013000150630           clear  FIDNU1ds.ioDNU1tel;
013100150630           clear  FIDNU1ds.ioDNU1fax;
013200150630         EndIf;
013300150629         FIDNU1ds.oDNU1err = *off;
013400150629         clear  FIDNU1ds.oDNU1msg;
013500150629
013600150629         // -?Controllo parametri ricevuti?
013700150629         Select;
013800150630
013900150630           // -?Modalità ("C"=Cancellazione, "R"=Reperimento, "S"=Scrittura)?
014000150630           When  FIDNU1ds.iDNU1mod <> 'C'  and
014100150630                 FIDNU1ds.iDNU1mod <> 'R'  and
014200150629                 FIDNU1ds.iDNU1mod <> 'S';
014300150629             FIDNU1ds.oDNU1err = *on;
014400150629             FIDNU1ds.oDNU1msg = sk_Msg(01);
014500150629             exsr  sr_RoutEnd;
014600150630
014700150629           // -?Numero C.A.?
014800150629           When  FIDNU1ds.iDNU1aac <= *zero  or
014900150629                 FIDNU1ds.iDNU1fil <= *zero  or
015000150629                 FIDNU1ds.iDNU1nca <= *zero;
015100150629             FIDNU1ds.oDNU1err = *on;
015200150629             FIDNU1ds.oDNU1msg = sk_Msg(02);
015300150629             exsr  sr_RoutEnd;
015400150630
015500150629           // -?Tipo rec. FNDKA00F?
015600150629           When  FIDNU1ds.iDNU1trc <> 'C'  and
015700150629                 FIDNU1ds.iDNU1trc <> 'I'  and
015800150629                 FIDNU1ds.iDNU1trc <> 'P'  and
015900150629                 FIDNU1ds.iDNU1trc <> 'R';
016000150629             FIDNU1ds.oDNU1err = *on;
016100150629             FIDNU1ds.oDNU1msg = sk_Msg(03);
016200150629             exsr  sr_RoutEnd;
016300150630
016400150629         EndSl;
016500150629
016600150629         // -?Reperimento dati job?
016700150629         exsr  sr_DatiJob;
016800150629
016900150629         // -?Preparazione chiave di accesso al file FNDKA01L?
017000150629         clear  keyFNDKA01;
017100150629         k_DKAaac = FIDNU1ds.iDNU1aac;
017200150629         k_DKAfil = FIDNU1ds.iDNU1fil;
017300150629         k_DKAnca = FIDNU1ds.iDNU1nca;
017400150629         k_DKAtrc = FIDNU1ds.iDNU1trc;
017500150629
017600150629       ENDSR;
017700150629
017800150629       //--------------------------------------------------------------
017900150629       //?Reperimento Dati del job (Utente/Operativi).                 ?
018000150629       //--------------------------------------------------------------
018100150629       BEGSR  sr_DatiJob;
018200150629
018300150629         in(E) §AzUte;
018400150629         if NOT %error;
018500150629           in(E) §DatiUte;
018600150629         endif;
018700150629         if %error or RSut = *blanks;
018800150629           clear TIBS34ds;
018900150629           tibs34r ( tibs34ds );
019000150629           in §AzUte;
019100150629           in §DatiUte;
019200150629         endif;
019300150629
019400150629       ENDSR;
019500150629
019600150629       //--------------------------------------------------------------
019700150629       //?Cancellazione dati del record indicato - *file FNDKA00F.     ?
019800150629       //--------------------------------------------------------------
019900150629       BEGSR  sr_Cancellazione;
020000150629
020100150629         // -?Reperimento rec. da cancellare in FNDKA00F?
020200150629         chain  %kds( keyFNDKA01 )  FNDKA000;
020300150629
020400150629         If  %found( FNDKA01L);
020500150630
020600150630           // -?Impostazione dati da restituire?
020700150630           FIDNU1ds.ioDNU1kcc = DKAkcc;
020800150630           FIDNU1ds.ioDNU1ksc = DKAksc;
020900150630           FIDNU1ds.ioDNU1rag = DKArag;
021000150630           FIDNU1ds.ioDNU1via = DKAvia;
021100150630           FIDNU1ds.ioDNU1cap = DKAcap;
021200150630           FIDNU1ds.ioDNU1loc = DKAloc;
021300150630           FIDNU1ds.ioDNU1prv = DKAprv;
021400150630           FIDNU1ds.ioDNU1naz = DKAnaz;
021500150630           FIDNU1ds.ioDNU1iva = DKAiva;
021600150630           FIDNU1ds.ioDNU1tel = DKAtel;
021700150630           FIDNU1ds.ioDNU1fax = DKAfax;
021800150629
021900150630           // -?Cancellazione rec. in FNDKA00F?
022000150629           //__________________
022100150629           delete(E)  FNDKA000;
022200150629           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
022300150629
022400150629           if  %error();
022500150629             FIDNU1ds.oDNU1err = *on;
022600150629             FIDNU1ds.oDNU1msg = sk_Msg(04);
022700150629           endif;
022800150629
022900150629         Else;
023000150629
023100150630           // -?Non trovato rec. da cancellare?
023200150629           FIDNU1ds.oDNU1err = *on;
023300150629           FIDNU1ds.oDNU1msg = sk_Msg(05);
023400150629
023500150629         EndIf;
023600150629
023700150629       ENDSR;
023800150629
023900150629       //--------------------------------------------------------------
024000150629       //?Reperimento dati dal record indicato - *file FNDKA00F.       ?
024100150629       //--------------------------------------------------------------
024200150629       BEGSR  sr_Reperimento;
024300150629
024400150629         // -?Reperimento dati da FNDKA00F?
024500150629         chain(N)  %kds( keyFNDKA01 )  FNDKA000;
024600150629
024700150630         // -?Non trovato rec.?
024800150629         if  NOT %found( FNDKA01L );
024900150629           FIDNU1ds.oDNU1err = *on;
025000150629           FIDNU1ds.oDNU1msg = sk_Msg(06);
025100150629           leavesr;
025200150629         endif;
025300150629
025400150629         // -?Impostazione dati da restituire?
025500150629         FIDNU1ds.ioDNU1kcc = DKAkcc;
025600150629         FIDNU1ds.ioDNU1ksc = DKAksc;
025700150629         FIDNU1ds.ioDNU1rag = DKArag;
025800150629         FIDNU1ds.ioDNU1via = DKAvia;
025900150629         FIDNU1ds.ioDNU1cap = DKAcap;
026000150629         FIDNU1ds.ioDNU1loc = DKAloc;
026100150629         FIDNU1ds.ioDNU1prv = DKAprv;
026200150629         FIDNU1ds.ioDNU1naz = DKAnaz;
026300150629         FIDNU1ds.ioDNU1iva = DKAiva;
026400150629         FIDNU1ds.ioDNU1tel = DKAtel;
026500150629         FIDNU1ds.ioDNU1fax = DKAfax;
026600150629
026700150629       ENDSR;
026800150629
026900150629       //--------------------------------------------------------------
027000150629       //?Scrittura dati nel record indicato - *file FNDKA00F.         ?
027100150629       //--------------------------------------------------------------
027200150629       BEGSR  sr_Scrittura;
027300150629
027400150629         // -?Reperimento dati da FNDKA00F?
027500150629         chain(E)  %kds( keyFNDKA01 )  FNDKA000;
027600150629
027700150629         if  %error();
027800150629           FIDNU1ds.oDNU1err = *on;
027900150629           FIDNU1ds.oDNU1msg = sk_Msg(07);
028000150629           leavesr;
028100150629         endif;
028200150630
028300150630         // -?Impostazione campi chiave SE rec. nuovo?
028400150630         If  NOT %found( FNDKA01L);
028500150630           clear  FNDKA000;
028600150630           DKAaac = FIDNU1ds.iDNU1aac;
028700150630           DKAfil = FIDNU1ds.iDNU1fil;
028800150630           DKAnca = FIDNU1ds.iDNU1nca;
028900150630           DKAtrc = FIDNU1ds.iDNU1trc;
029000150630         EndIf;
029100150629
029200150629         // -?Impostazione campi da registrare in FNDKA00F?
029300150629         DKAkcc = FIDNU1ds.ioDNU1kcc;
029400150629         DKAksc = FIDNU1ds.ioDNU1ksc;
029500150629         DKArag = FIDNU1ds.ioDNU1rag;
029600150629         DKAvia = FIDNU1ds.ioDNU1via;
029700150629         DKAcap = FIDNU1ds.ioDNU1cap;
029800150629         DKAloc = FIDNU1ds.ioDNU1loc;
029900150629         DKAprv = FIDNU1ds.ioDNU1prv;
030000150629         DKAnaz = FIDNU1ds.ioDNU1naz;
030100150629         DKAiva = FIDNU1ds.ioDNU1iva;
030200150629         DKAtel = FIDNU1ds.ioDNU1tel;
030300150629         DKAfax = FIDNU1ds.ioDNU1fax;
030400150629
030500150629         // -?Scrittura rec. in FNDKA00F?
030600150629         If  %found( FNDKA01L);
030700150629
030800150629           //__________________
030900150629           update(E)  FNDKA000;
031000150629           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
031100150629
031200150629           if  %error();
031300150629             FIDNU1ds.oDNU1err = *on;
031400150629             FIDNU1ds.oDNU1msg = sk_Msg(08);
031500150629             leavesr;
031600150629           endif;
031700150629
031800150629         Else;
031900150629
032000150629           //_________________
032100150629           write(E)  FNDKA000;
032200150629           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
032300150629
032400150629           if  %error();
032500150629             FIDNU1ds.oDNU1err = *on;
032600150629             FIDNU1ds.oDNU1msg = sk_Msg(09);
032700150629             leavesr;
032800150629           endif;
032900150629
033000150629         EndIf;
033100150629
033200150629       ENDSR;
033300150629
033400150629       //--------------------------------------------------------------
033500150629       //?Operazioni finali.                                           ?
033600150629       //--------------------------------------------------------------
033700150629       BEGSR  sr_RoutEnd;
033800150629
033900150629         // -?Chiusura pgm?
034000150629         return;
034100150629
034200150629       ENDSR;
034300150629
034400150629      /end-free
034500150629
034600150629       //--------------------------------------------------------------
034700150629       //?Definizione schiere a tempo di compilazione                  ?
034800150629       //--------------------------------------------------------------
034900150629
035000150629** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
035100150629Ricevuto parametro "modalità" errato                                            1
035200150629NON ricevuta C.A.                                                               2
035300150629Ricevuto parametro "Tipo Record" errato                                         3
035400150629Fallita cancellazione                                                           4
035500150629Non trovati dati da cancellare per la C.A.                                      5
035600150629Non trovati dati per la C.A.                                                    6
035700150629Non riuscito il reperimento dei dati da aggiornare                              7
035800150629Fallito aggiornamento                                                           8
035900150629Fallita scrittura                                                               9
