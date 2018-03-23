000100100129       //==============================================================
000200100129       //?TRTB28R2 * Attribuzione numero di serie libero per filiale   ?
000300100129       //==============================================================
000400111215
000500111215       //--------------------------------------------------------------
000600111215       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700111215       //--------------------------------------------------------------
000800111215
000900111215     /*PRM dbgview(*source)
001000111215     /*END
001100111215
001200111215       //--------------------------------------------------------------
001300111215       //?Specifiche di controllo.                                     ?
001400111215       //--------------------------------------------------------------
001500100129
001600100129     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001700100129     h dftactgrp(*no) actgrp(*caller)
001800100129
001900100129       //--------------------------------------------------------------
002000100129       //?Dichiarazione file.                                          ?
002100100129       //--------------------------------------------------------------
002200100129
002300100129       // -?Tabella "3C"?
002400100129     fTABEL00F  if   e           k disk
002500100129
002600120215       // -?Tabelle "3EW" e "3CL"?
002700100129     fTNTBE01L  if   e           k disk
002800100129
002900100129       //--------------------------------------------------------------
003000100129       //?Definizione costanti.                                        ?
003100100129       //--------------------------------------------------------------
003200100129
003300100129       // -?Codice tabella in gestione?
003400100129     d c_Tab           c                   const('3C')
003500100129
003600100129       // -?Codice utente?
003700100129     d c_Kut           c                   const(1)
003800100129
003900100129       //--------------------------------------------------------------
004000100129       //?Definizione schiere.                                         ?
004100100129       //--------------------------------------------------------------
004200100129
004300100129
004400100129       //--------------------------------------------------------------
004500100129       //?Definizione aree dati.                                       ?
004600100129       //--------------------------------------------------------------
004700100129
004800100129       // -?Dati utente?
004900100129     d §AzUte        e ds                  extname(AZUTE00F)
005000100129     d                                     dtaara
005100100129     d §DatiUte      e ds                  extname(dDatiUte)
005200100129     d                                     dtaara
005300100129
005400100129       //--------------------------------------------------------------
005500100129       //?Definizione strutture dati.                                  ?
005600100129       //--------------------------------------------------------------
005700100129
005800100129       // -?Status ds?
005900100129     d Status         sds
006000100129     d  SDSpgm           *proc
006100100129
006200100129       // -?Parametri ricevuti?
006300100129     d ds_Parms        ds
006400100129     d  ParFIL                        3s 0
006500100129     d  ParNRS                        2s 0
006600100129     d  ParERR                        1a
006700100129     d  ParELB                        1a
006800100129     d  ParKSC                        8a
006900100129     d  OutKSC                        7a
007000100129
007100100129       // -?Parametri per Reperimento dati utente?
007200100129     d TIBS34ds      e ds                  inz
007300100129
007400100129       // -?Tabelle usate:?
007500100129       // ·?"3C" = Clienti che inviano bolle partenza?
007600100129     d ds3C          e ds                  inz
007700100129       // ·?"3EW" = LNP e dati segnacollo assegnati alla "postazione"?
007800100129     d d3EW          e ds                  inz
007900120215       // ·?"3CL" = Serie (totale) per gruppi d'unificante?
008000120215       //  ?=> NON serve! ...Basta verificare l'esistenza del rec.?
008100120215     d*// d3CL          e ds                  inz
008200100129
008300100129       //--------------------------------------------------------------
008400100129       //?Definizione variabili globali.                               ?
008500100129       //--------------------------------------------------------------
008600111215
008700111215       // -?Parametri ricevuti?
008800111215     d P_ubLeg3C_SKC   s           3200
008900100129
009000100129       // -?Flags booleani?
009100100129     d $Fine           s               n   inz
009200100209     d $Err3EW         s               n   inz
009300120130     d $Err3CL         s               n   inz
009400120130     d $Tab3CL         s               n   inz
009500100129
009600100129       //--------------------------------------------------------------
009700100129       //?Definizione prototipi procedure.                             ?
009800100129       //--------------------------------------------------------------
009900100129
010000100129       // -?Reperimento dati utente?
010100100129      /copy gaitrasrc/srcProtoPR,TIBS34R
010200111215
010300111215       // -?Parametri per reperimento legami in tab. "3C"?
010400111215      /copy gaitrasrc/srcProtoPI,UBLEG3C
010500111215     ***  // -?Reperimento legami in tab. "3C"?
010600111215     *** /copy gaitrasrc/srcProtoPR,UBLEG3C
010700100129
010800100129       //--------------------------------------------------------------
010900100129       //?Definizione key-list.                                        ?
011000100129       //--------------------------------------------------------------
011100100129
011200100129       // -?File TABEL00F?
011300100129     d k03tabel00    e ds                  extname(TABEL00F : *key)
011400100129     d                                     prefix(k_)   inz
011500100129
011600100129       // -?File TNTBE01L?
011700100129     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
011800100129     d                                     prefix(k_)   inz
011900100129
012000100129       //--------------------------------------------------------------
012100100129       //?M A I N - L I N E                                            ?
012200100129       //--------------------------------------------------------------
012300100129
012400100129     c     *Entry        plist
012500100129     c                   parm                    ds_Parms
012600111215     c                   parm                    p_ubLeg3C_SKC
012700100129
012800100129      /free
012900100129
013000100129       // -?Operazioni iniziali?
013100100129       exsr  sr_RoutInz;
013200100129
013300100209       // -?Esecuzione operazione richiesta??(ricerca o controllo)?
013400100209       DoW  Not  $Fine;
013500100209
013600120130         // -?Il ctrl di Filiale/Serie in tab."3CL" è già stato fatto?
013700120130         //  ?(SE serie inserita in tab. "3CL" => è "duplicabile"...)?
013800120130         //  ?Quindi occorre operare solo SE richiesta ricerca  o  SE?
013900120130         //  ?Filiale/serie NON trovata in tab."3CL"?
014000120130         If  ParELB = 'R'  or  Not $Tab3CL;
014100120130           // -?Ciclo di lettura tabella "3C"?
014200120130           setll  %kds(k03tabel00)  TABEL;
014300120130           reade  %kds(k03tabel00 : 2)  TABEL;
014400120130           DoW  Not %eof(TABEL00F)   and
014500120130                %subst ( TBLkey : 1 : 3 ) = %editc ( ParFIL : 'X' );
014600120130             if  TBLflg = *blank;
014700120130               exsr  sr_Elab3C;
014800120130             endif;
014900120130             reade  %kds(k03tabel00 : 2)  TABEL;
015000120130           EndDo;
015100120130         EndIf;
015200120130
015300120130         // -?Verifica esistenza in tabella "3CL"?
015400120130         //  ?della serie appena trovata libera in tabella "3C"?
015500120130         //  ?(solo per RICERCA, perchè per controllo?
015600120130         //   ?è già stato eseguito l'accesso - in sr_RoutInz)?
015700120130         If  ParELB = 'R';
015800120130           clear  k05tntbe01;
015900120130           k_TBEcod = '3CL';
016000120130           k_TBEke1 = %editc( ParFIL : 'X' );
016100120130           k_TBEke2 = %editc( ParNRS : 'X' );
016200120130           chain  %kds(k05tntbe01)  TNTBE000;
016300120130           exsr  sr_Verifica3CL;
016400120130           if  $Err3CL;
016500120130             iter;
016600120130           endif;
016700120130         // -?Richiesto controllo:?
016800120130         Else;
016900120130           if  $Tab3CL;
017000120130             ParERR = 'A';
017100120130             //$Fine = *on;
017200120130             //leave;
017300120130           endif;
017400120130         EndIf;
017500100209
017600100209         // -?Ciclo di lettura tabella "3EW"?
017700100209         $Fine = *on;
017800120130         clear  k05tntbe01;
017900120130         k_TBEcod = '3EW';
018000120130         k_TBEke1 = '0' + %editc( ParFIL : 'X' );
018100100209         setll  %kds(k05tntbe01)  TNTBE000;
018200100209         reade  %kds(k05tntbe01 : 1)  TNTBE000;
018300100209         DoW  Not %eof(TNTBE01L);
018400120113           if  TBEatb = *blank;
018500120113             exsr  sr_Elab3EW;
018600120113             if  $Err3EW;
018700120113               leave;
018800120113             endif;
018900120113           endif;
019000100209           reade  %kds(k05tntbe01 : 1)  TNTBE000;
019100100209         EndDo;
019200100209
019300100209       EndDo;
019400100129
019500100129       // -?Operazioni finali?
019600100129       exsr  sr_RoutEnd;
019700100129
019800100129       //--------------------------------------------------------------
019900100129       //?Operazioni iniziali.                                         ?
020000100129       //--------------------------------------------------------------
020100100129       BEGSR  sr_RoutInz;
020200100129
020300100129         *inLR = *on;
020400111215
020500111215         if  %parms() > 1;
020600111215           ubLeg3C_skC = P_ubLeg3C_skC;
020700111215         endif;
020800100129
020900100129         // -?Pulizia parametri di output?
021000100129         clear  ParERR;
021100100129         clear  OutKSC;
021200100129         // -?(per ricerca si parte con serie = 1)?
021300100129         if  ParELB = 'R';
021400100129           ParNRS = 1;
021500100129         endif;
021600120113
021700120113         // -?Controllo parametri ricevuti?
021800120113         if  ParNRS = *hival;
021900120113           ParERR = '9';
022000120113           $Fine = *on;
022100120113           leavesr;
022200120113         endif;
022300100129
022400100129         // -?Reperimento dati job?
022500100129         exsr  sr_DatiJob;
022600100129
022700100129         // -?Impostazione campi "fissi"?
022800100129         k_TBLkut = c_Kut;
022900100129         k_TBLcod = c_Tab;
023000120130         k_TBLkey = %editc ( ParFIL : 'X' );
023100120130
023200120130         // -?Verifica esistenza serie in tab. "3CL"?
023300120130         if  ParELB = 'C';
023400120130           clear  k05tntbe01;
023500120130           k_TBEcod = '3CL';
023600120130           k_TBEke1 = %editc( ParFIL : 'X' );
023700120130           k_TBEke2 = %editc( ParNRS : 'X' );
023800120130           chain  %kds( k05tntbe01 )  TNTBE000;
023900120130           $Tab3CL = (%found(TNTBE01L)  and  TBEatb = *blank);
024000120130           //ParERR = 'A';
024100120130           //OutKSC = *all'9';
024200120130           //exsr  sr_RoutEnd;
024300120130         endif;
024400100129
024500100129       ENDSR;
024600100129
024700100129       //--------------------------------------------------------------
024800100129       //?Reperimento Dati del job (Utente/Operativi).                 ?
024900100129       //--------------------------------------------------------------
025000100129       BEGSR  sr_DatiJob;
025100100129
025200100129         in(e) §AzUte;
025300100129         if NOT %error;
025400100129           in(e) §DatiUte;
025500100129         endif;
025600100129         if %error or RSut = *blank;
025700100129           tibs34r ( tibs34ds );
025800100129           in §AzUte;
025900100129           in §DatiUte;
026000100129         endif;
026100100129
026200100129       ENDSR;
026300100129
026400100129       //--------------------------------------------------------------
026500100129       //?Elaborazione tabella "3C" nel file TABEL00F                  ?
026600100129       //--------------------------------------------------------------
026700100129       BEGSR  sr_Elab3C;
026800100209
026900100209         ds3C = TBLuni;
027000100129
027100100129         // -?Trovata stessa filiale segnacollo & stessa serie?
027200100129         If  §3Cfls = ParFIL   and
027300100129             §3Cnrs = ParNRS;
027400100129
027500100129           // -?Richiesto controllo:?
027600100129           if  ParELB = 'C';
027700100129
027800120130             // -?(SE serie inserita in tab. "3CL"?
027900120130             //   ?=> è duplicabile...)?
028000120130             if  Not  $Tab3CL;
028100120130
028200120130               // -?SE cliente diverso da quello ricevuto?
028300120130               //  ?=> uscita con *errore?
028400120130               if  PARksc <> TBLkey  and
028500120130                   PARksc <> %editc( §3Ccks : 'X' )  and
028600120130                   %lookup( %int( %trimr( TBLkey ) ) : sch_SKC )
028700120130                           = *zero;
028800120130                 OutKSC = TBLkey;
028900120130                 exsr  sr_Errore;
029000120130               endif;
029100100129
029200120130             endif;
029300120130
029400100129           // -?Richiesta ricerca:?
029500100129           else;
029600100129
029700120113             // -?SE provate ormai TUTTE le serie (da 1 a 98)?
029800100129             //  ?=> uscita con *errore?
029900120113             if  (ParNRS + 1) = *hival;
030000100209               OutKSC = TBLkey;
030100100129               exsr  sr_Errore;
030200100129             endif;
030300100129             // -?ALTRIMENTI => si RIcontrolla con la serie successiva?
030400100129             ParNRS += 1;
030500100129             setll  %kds(k03tabel00)  TABEL;
030600100129
030700100129           endif;
030800100129
030900100129         EndIf;
031000100129
031100100129       ENDSR;
031200120130
031300120130       //--------------------------------------------------------------
031400120130       //?Verifica in tabella "3CL" nel file TNTBE01L                  ?
031500120130       //--------------------------------------------------------------
031600120130       BEGSR  sr_Verifica3CL;
031700120130
031800120130         $Err3CL = *off;
031900120130
032000120130         //d3CL = TBEuni;
032100120130
032200120130         // -?Trovata stessa filiale segnacollo & stessa serie?
032300120130         If  %found(TNTBE01L)  and  TBEatb = *blank;
032400120130
032500120130           // -?Richiesto controllo:?
032600120130           if  ParELB = 'C';
032700120130
032800120130             ParERR = 'A';
032900120130             $Fine = *on;
033000120130             leavesr;
033100120130
033200120130           // -?Richiesta ricerca:?
033300120130           else;
033400120130
033500120130             // -?SE provate ormai TUTTE le filiali (da 1 a 98)?
033600120130             //  ?=> uscita con *errore?
033700120130             if  (ParNRS + 1) = *hival;
033800120130               OutKSC = %subst(TBEke1 : 2);
033900120130               exsr  sr_Errore;
034000120130             endif;
034100120130             // -?ALTRIMENTI => si RIcontrolla con la serie successiva?
034200120130             ParNRS += 1;
034300120130             setll  %kds(k03tabel00)  TABEL;
034400120130             $Fine   = *off;
034500120130             $Err3CL = *on;
034600120130
034700120130           endif;
034800120130
034900120130         EndIf;
035000120130
035100120130       ENDSR;
035200100129
035300100129       //--------------------------------------------------------------
035400100129       //?Elaborazione tabella "3EW" nel file TNTBE01L                 ?
035500100129       //--------------------------------------------------------------
035600100129       BEGSR  sr_Elab3EW;
035700100129
035800100209         $Err3EW = *off;
035900100209
036000100209         d3EW = TBEuni;
036100100129
036200100209         // -?Trovata stessa filiale segnacollo & stessa serie?
036300100209         If  §3EWfls = ParFIL   and
036400100209             §3EWnrs = ParNRS;
036500100209
036600100209           // -?Richiesto controllo:?
036700100209           if  ParELB = 'C';
036800120130
036900120130             if  Not  $Tab3CL;
037000100209
037100120130               // -?SE cliente diverso da quello ricevuto?
037200120130               //  ?=> uscita con *errore?
037300120130               if  PARksc <> %subst(TBEke1 : 2)  and
037400120130                   %lookup( %int( %trimr( %subst(TBEke1 : 2) ) ) :
037500120130                            sch_SKC ) = *zero;
037600120130                 OutKSC = %subst(TBEke1 : 2);
037700120130                 exsr  sr_Errore;
037800120130               endif;
037900120130
038000120130             endif;
038100100209
038200100209           // -?Richiesta ricerca:?
038300100209           else;
038400100209
038500120113             // -?SE provate ormai TUTTE le filiali (da 1 a 98)?
038600100402             //  ?=> uscita con *errore?
038700120113             if  (ParNRS + 1) = *hival;
038800100209               OutKSC = %subst(TBEke1 : 2);
038900100209               exsr  sr_Errore;
039000100209             endif;
039100100402             // -?ALTRIMENTI => si RIcontrolla con la serie successiva?
039200100209             ParNRS += 1;
039300100209             setll  %kds(k03tabel00)  TABEL;
039400100209             $Fine   = *off;
039500100209             $Err3EW = *on;
039600100209
039700100209           endif;
039800100129
039900120130         EndIf;
040000100129
040100100129       ENDSR;
040200100129
040300100129       //--------------------------------------------------------------
040400100129       //?Restituzione errore                                          ?
040500100129       //--------------------------------------------------------------
040600100129       BEGSR  sr_Errore;
040700100129
040800120113         ParERR = 'E';       //?<== unico errore "generico"?
040900120130                             //    ?(vedi eccezioni x ParNRS = *hival?
041000120130                             //                    ?o ParNRS = "A")?
041100100129         exsr  sr_RoutEnd;
041200100129
041300100129       ENDSR;
041400100129
041500100129       //--------------------------------------------------------------
041600100129       //?Operazioni finali.                                           ?
041700100129       //--------------------------------------------------------------
041800100129       BEGSR  sr_RoutEnd;
041900100129
042000100129         return;
042100100129
042200100129       ENDSR;
