000100150603       //==============================================================
000200150603       //?TRULIPJR * Impostazione del Programma Iniziale da Richiamare ?
000300150603       //?           nel Profilo utente.                               ?
000400150603       //==============================================================
000500150604       //? Compilare con utente QSECOFR !                              ?
000600150604       //==============================================================
000700150603
000800150603       //--------------------------------------------------------------
000900150603       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001000150603       //--------------------------------------------------------------
001100150603
001200150603     /*PRM  dbgview(*source)
001300150604     /*CMD  addlible lib(GAITRAFIL)  position(*after QGPL)
001400150604     /*CMD  addlible lib(GAITRAGRPS) position(*after QGPL)
001500150604     /*END  rmvlible lib(GAITRAFIL)
001600150604     /*END  rmvlible lib(GAITRAGRPS)
001700150603     /*END
001800150603
001900150603       //--------------------------------------------------------------
002000150603       //?Specifiche di controllo.                                     ?
002100150603       //--------------------------------------------------------------
002200150603
002300150603     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002400160225     h dftactgrp(*no) bnddir('UBRTVNETA')
002500160225     h usrprf(*owner)
002600150603
002700150603       //--------------------------------------------------------------
002800150603       //?Dichiarazione file.                                          ?
002900150603       //--------------------------------------------------------------
003000150603
003100150603       // -?Organigramma?
003200150603     fAZORG01L  if   e           k disk
003300150603
003400150603       // -?Profili Utente?
003500150603     fKPPRF01L  if   e           k disk
003600150604     fAZUTE02L  if   e           k disk    usropn
003700150604     f                                     rename( AZUTE000 : AZUTE002 )
003800150604     f                                     prefix( UTE2 : 3 )
003900150604     fAZUTE01L  Uf   e           k disk
004000150603
004100150603       // -?Video?
004200150603     fTRULIPJD  cf   e             workstn
004300150603     f                                     indds(IndDspF)
004400150603     f                                     infds(InfDspF)
004500150603
004600150603       //--------------------------------------------------------------
004700150603       //?Definizione costanti.                                        ?
004800150603       //--------------------------------------------------------------
004900150603
005000150603       // -?Comando di verifica esistenza libreria?
005100150603     d c_CmdCHKOBJ     c                   const('CHKOBJ +
005200150603     d                                            obj(')
005300150603     d c_CmdCHKOBJlib  c                   const(') +
005400150603     d                                            objtype(*lib)')
005500150603     d c_CmdCHKOBJpgm  c                   const(') +
005600150603     d                                            objtype(*pgm)')
005700150604     d c_CmdCHKOBJprf  c                   const(') +
005800150604     d                                            objtype(*usrprf)')
005900150603
006000150603       // -?Nome del *pgm iniziale per gli utenti?
006100150603     d c_Pgm           c                   const('BAK010C')
006200150603
006300150603       // -?Tasti funzionali a video?
006400150603     d c_F01           c                   const(x'31')
006500150603     d c_F02           c                   const(x'32')
006600150603     d c_F03           c                   const(x'33')
006700150603     d c_F04           c                   const(x'34')
006800150603     d c_F05           c                   const(x'35')
006900150603     d c_F06           c                   const(x'36')
007000150603     d c_F07           c                   const(x'37')
007100150603     d c_F08           c                   const(x'38')
007200150603     d c_F09           c                   const(x'39')
007300150603     d c_F10           c                   const(x'3A')
007400150603     d c_F11           c                   const(x'3B')
007500150603     d c_F12           c                   const(x'3C')
007600150603     d c_F13           c                   const(x'B1')
007700150603     d c_F14           c                   const(x'B2')
007800150603     d c_F15           c                   const(x'B3')
007900150603     d c_F16           c                   const(x'B4')
008000150603     d c_F17           c                   const(x'B5')
008100150603     d c_F18           c                   const(x'B6')
008200150603     d c_F19           c                   const(x'B7')
008300150603     d c_F20           c                   const(x'B8')
008400150603     d c_F21           c                   const(x'B9')
008500150603     d c_F22           c                   const(x'BA')
008600150603     d c_F23           c                   const(x'BB')
008700150603     d c_F24           c                   const(x'BC')
008800150603     d c_Enter         c                   const(x'F1')
008900150603     d c_RollDown      c                   const(x'F4')
009000150603     d c_RollUp        c                   const(x'F5')
009100150603
009200150603       //--------------------------------------------------------------
009300150603       //?Definizione schiere.                                         ?
009400150603       //--------------------------------------------------------------
009500150603
009600150603       // -?Messaggi di errore?
009700160223     d sk_Msg          s             78    dim( 9)  ctdata  perrcd(1)
009800150603
009900150603       //--------------------------------------------------------------
010000150603       //?Definizione aree dati.                                       ?
010100150603       //--------------------------------------------------------------
010200150603
010300150603       // -?Dati utente?
010400150603     d §AzUte        e ds                  extname(AZUTE00F)
010500150603     d                                     dtaara
010600150603     d §DatiUte      e ds                  extname(dDatiUte)
010700150603     d                                     dtaara
010800150603
010900150603       //--------------------------------------------------------------
011000150603       //?Definizione strutture dati.                                  ?
011100150603       //--------------------------------------------------------------
011200150603
011300150603       // -?Status ds?
011400150603     d Status         sds
011500150603     d   SDSpgm          *proc
011600150603     d*//SDSprm          *parms
011700150603     d*//SDSdta              191    198
011800150603     d*//SDSjob              244    253
011900150603     d   SDSusr              254    263
012000150603
012100150603       // -?InfDS?
012200150603     d InfDspF         ds
012300150603     d   dsp_aid             369    369a
012400150603     d*//sfl_rrn             376    377i 0
012500150603     d*//min_nrr             378    379i 0
012600150603     d*//num_rcds            380    381i 0
012700150603
012800150603       // -?Indicatori su DspF?
012900150603     d IndDspF         ds                  inz
013000150603         // -?Abilitazione tasti funzionali?
013100150603         // -?Emissione messaggio di errore?
013200150604     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013300150603         // -?Indicatori di gestione del subfile?
013400150603         // -?Indicatori per Attibuti di visualizzazione?
013500150603         //  ?o   Condizionamento visualizzazione campi?
013600150603         // -?Posizionamento cursore & segnalazione errore?
013700150604     d   $PosCurUTE                    n   overlay( IndDspF : 51 )
013800150604     d   $PosCurFIL                    n   overlay( IndDspF : 52 )
013900150604     d   $PosCurIPT                    n   overlay( IndDspF : 53 )
014000150604     d   $PosCurLIB                    n   overlay( IndDspF : 54 )
014100150603         //   -?Riemissione videata?
014200150604     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
014300150603
014400150603       // -?Parametri ricevuti?
014500150603     d KPJBA         e ds
014600150603
014700150603       // -?Tab. "LAT" = Sigla Funzione Aziendale?
014800150603     d dLAT          e ds                  qualified  inz
014900150603
015000150603       // -?Flags Abilitazioni Utenti Aziendali?
015100150603     d dUte01        e ds                  qualified  inz
015200150603
015300150603       //--------------------------------------------------------------
015400150603       //?Definizione variabili globali.                               ?
015500150603       //--------------------------------------------------------------
015600150603
015700150603       // -?Flags booleani?
015800150603     d $Fine           s               n   inz
015900150603     d $InzD01         s               n   inz(*on)
016000150603
016100150603       // -?Campi associati al video?
016200150603     d $Video          s              2    inz('D1')
016300150604
016400150604       // -?Data odierna?
016500150604     d wDate           s              8  0 inz
016600150604
016700150604       // -?Campi per parzializzazioni?
016800150604     d W1Cute0         s                   like(V1Cute)  inz(*loval)
016900150604     d W1Cute9         s                   like(V1Cute)  inz(*hival)
017000150603
017100150603       //--------------------------------------------------------------
017200150603       //?Definizione prototipi procedure usate.                       ?
017300150603       //--------------------------------------------------------------
017400150603
017500150603       // -?Reperimento NETA sistema AS/400 corrente?
017600150603     d currSysNeta     s              8a   inz
017700150603      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017800150603
017900150603       // -?Reperimento dati utente?
018000150603     d TIBS34ds      e ds                  inz
018100150603      /copy gaitrasrc/srcProtoPR,TIBS34R
018200150603
018300150603       // -?Ricerca/Controllo tabelle?
018400150603     d TIBS02ds      e ds                  inz
018500150604     d   T02mod      e                     inz('C')
018600150604     d   T02cod      e                     inz('LAT')
018700150603      /copy gaitrasrc/srcProtoPR,TIBS02R
018800150603
018900150603       // -?API QCAPCMD (Process Commands)?
019000150603     d Qcmd            s           2048    inz  varying
019100150603      /copy qSysInc/qRpgleSrc,QCAPCMD
019200150603      /copy gaitrasrc/srcProtoPR,QCAPCMD
019300150603
019400150603       // -?Parametri gestione errori API.?
019500150603      /copy qsysinc/qrpglesrc,QUSEC
019600150603
019700150603       //--------------------------------------------------------------
019800150603       //?Definizione key-list.                                        ?
019900150603       //--------------------------------------------------------------
020000150603
020100150604       // -?File KPPRF01L?
020200150604     d keyKPPRF01    e ds                  extname( KPPRF01L : *key )
020300150604     d                                     prefix( k_ )  inz
020400150603
020500150603       //--------------------------------------------------------------
020600150603       //?Riepilogo indicatori utilizzati.                             ?
020700150603       //--------------------------------------------------------------
020800150603
020900150603
021000150603       //--------------------------------------------------------------
021100150603       //?M A I N - L I N E                                            ?
021200150603       //--------------------------------------------------------------
021300150603
021400150603     c     *Entry        plist
021500150603     c                   parm                    KPJBA
021600150603
021700150603      /free
021800150603
021900150603       // -?Operazioni iniziali?
022000150603       exsr  sr_RoutInz;
022100150603
022200150603       // -?Ciclo di gestione del file video?
022300150603       DoW  $Fine = *off;
022400150603
022500150603         select;
022600150603
022700150603           // -?Gestione videata D1 (parzializzazioni)?
022800150603           when  $Video = 'D1';
022900150603             exsr  sr_GesD01;
023000150603
023100150603           // -?? ? ??
023200150603           other;
023300150603             $Fine = *on;
023400150603
023500150603         endsl;
023600150603
023700150603       EndDo;
023800150603
023900150603       // -?Operazioni finali?
024000150603       exsr  sr_RoutEnd;
024100150603
024200150603       //--------------------------------------------------------------
024300150603       //?Operazioni iniziali.                                         ?
024400150603       //--------------------------------------------------------------
024500150603       BEGSR  sr_RoutInz;
024600150603
024700150603         // -?Impostazione chiusura?
024800150603         *inLR = *on;
024900150603
025000150603         // -?Verifica del sistema AS/400 corrente?
025100150603         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
025200150603           exsr  sr_RoutEnd;
025300150603         endif;
025400150604
025500150604         // -?Reperimento data odierna?
025600150604         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025700150604                               : 1 : 8 ) );
025800150603
025900150603         // -?Reperimento dati job?
026000150603         exsr  sr_DatiJob;
026100150603
026200150603         // -?Impostazione nome programma a video?
026300150603         V1Tpgm = SDSpgm;
026400150603
026500150603       ENDSR;
026600150603
026700150603       //--------------------------------------------------------------
026800150603       //?Reperimento Dati del job (Utente/Operativi).                 ?
026900150603       //--------------------------------------------------------------
027000150603       BEGSR  sr_DatiJob;
027100150603
027200150603         in(e) §AzUte;
027300150603         if NOT %error;
027400150603           in(e) §DatiUte;
027500150603         endif;
027600150603         if %error or RSut = *blank;
027700150603           tibs34r ( tibs34ds );
027800150603           in §AzUte;
027900150603           in §DatiUte;
028000150603         endif;
028100150603
028200150603       ENDSR;
028300150603
028400150603       //--------------------------------------------------------------
028500150603       //?Gestione videata D01.                                        ?
028600150603       //--------------------------------------------------------------
028700150603       BEGSR  sr_GesD01;
028800150603
028900150603         // -?Inizializzazione videata?
029000150603         if  $InzD01 = *on;
029100150603           exsr  sr_InzD01;
029200150603           $InzD01 = *off;
029300150603         endif;
029400150603
029500150603         // -?Emissione testata e piede?
029600150603         write  ULIPJT01;
029700150603         write  ULIPJP01;
029800150603
029900150603         // -?Emissione videata?
030000150603         exfmt  ULIPJD01;
030100150603
030200150604         clear  $errGenerico;
030300150604         clear  $errMessage;
030400150603         clear  V1Dmsg;
030500150603
030600150603         Select;
030700150603
030800150603           // -?F3=Fine?
030900150603           when  dsp_aid = c_F03;
031000150603             $Fine  = *on;
031100150603
031200150603           // -?Invio / F6=Conferma?
031300150603           Other;
031400150603             exsr  sr_CtrD01;
031500150604             if  $errGenerico;
031600150603               leavesr;
031700150603             endif;
031800150603             if  dsp_aid = c_F06;
031900150608               write  Protect;
032000150608               write  ULIPJD02;
032100150603               exsr  sr_ChgUsrPrf_All;
032200150603             endif;
032300150603
032400150603         EndSl;
032500150603
032600150603       ENDSR;
032700150603
032800150603       //--------------------------------------------------------------
032900150603       //?Inizializzazione videata D01.                                ?
033000150603       //--------------------------------------------------------------
033100150603       BEGSR  sr_InzD01;
033200150603
033300150603         clear  ULIPJD01;
033400150603
033500160223         //V1Dute = 'Tutti i profili ';
033600150603         V1Cute = SDSusr;
033700150603         chain  ( V1Cute )  KPPRF000;
033800150603         if  %found(KPPRF01L);
033900150603           V1Dute = PRFtxt;
034000150603         else;
034100150603           V1Dute = *all'? ';
034200150603         endif;
034300150603
034400150604         //V1Dfil = 'Tutte le filiali';
034500150604         V1Cfil = DUTpou;
034600150604         chain  ( V1Cfil )  AZORG;
034700150604         if  %found(AZORG01L);
034800150604           V1Dfil = ORGdes;
034900150604         else;
035000150604           V1Dfil = *all'? ';
035100150604         endif;
035200150603
035300150603         V1Cipt = 'PO';
035400150603         reset  TIBS02ds;
035500150603         T02ke1 = V1Cipt;
035600150603         TNTBE_RicercaControllo ( kpjba : tibs02ds );
035700150603         dLAT   = T02uni;
035800150603         V1Dipt = dLAT.§LATdes;
035900150603
036000150603         V1Clib = 'PROBAS    ';
036100150603
036200150603       ENDSR;
036300150603
036400150603       //--------------------------------------------------------------
036500150603       //?Controllo dati in videata D01.                               ?
036600150603       //--------------------------------------------------------------
036700150603       BEGSR  sr_CtrD01;
036800150603
036900150603         // -?Spegnimento degli indicatori di errore?
037000150603         %subst(IndDspF : 50) = *off;
037100150603
037200150603         // -?Controllo Profilo dell'utente?
037300150603         clear  V1Dute;
037400150603         V1Cute = %trimL ( V1Cute );
037500150603         select;
037600150603           when  V1Cute = *blank;
037700150603             V1Dute = 'Tutti i profili ';
037800150604           when  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
037900150603             V1Dute = 'Tutti i profili ' +
038000150604                      %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 ) +
038100150603                      '...';
038200150603           other;
038300150603             chain  ( V1Cute )  KPPRF000;
038400150603             if  %found(KPPRF01L);
038500150603               V1Dute = PRFtxt;
038600150603             else;
038700150603               V1Dute = *all'? ';
038800150604               $errGenerico = *on;
038900150604               $errMessage  = *on;
039000150604               $PosCurUTE   = *on;
039100150604               V1Dmsg = sk_Msg(01);
039200150603               leavesr;
039300150603             endif;
039400150603         endsl;
039500150603
039600150603         clear  dUte01;
039700160223         if  V1Cute <> *blank                                         and
039800150604             %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) <> '*'  and
039900150603             %found(KPPRF01L);
040000150604           chain(N)  ( V1Cute )  AZUTE000;
040100150604           if  %found(AZUTE01L);
040200150603             dUte01 = UTEfaf;
040300150603           endif;
040400150603         endif;
040500150603
040600150603         // -?Controllo Filiale dell'utente?
040700150603         clear  V1Dfil;
040800150603         if  V1Cfil = *zero;
040900160223           V1Dfil = 'Tutte le Filiali';
041000160223           //V1Dfil = *all'? ';
041100160223           //$errGenerico = *on;
041200160223           //$errMessage  = *on;
041300160223           //$PosCurFIL   = *on;
041400160223           //V1Dmsg = sk_Msg(02);
041500160223           //leavesr;
041600150603         else;
041700150603           chain  ( V1Cfil )  AZORG;
041800150603           if  %found(AZORG01L);
041900150603             V1Dfil = ORGdes;
042000150603           else;
042100150603             V1Dfil = *all'? ';
042200150604             $errGenerico = *on;
042300150604             $errMessage  = *on;
042400150604             $PosCurFIL   = *on;
042500150604             V1Dmsg = sk_Msg(03);
042600150603             leavesr;
042700150603           endif;
042800150603         endif;
042900150603
043000150603         // -?Controllo IP terminale dell'utente?
043100150603         clear  V1Dipt;
043200150603         if  V1Cipt <> *blank;
043300150603           reset  TIBS02ds;
043400150603           T02ke1 = V1Cipt;
043500150603           TNTBE_RicercaControllo ( kpjba : tibs02ds );
043600150603           if  T02err <> *blank;
043700150603             V1Dipt = *all'? ';
043800150604             $errGenerico = *on;
043900150604             $errMessage  = *on;
044000150604             $PosCurIPT   = *on;
044100150604             V1Dmsg = sk_Msg(04);
044200150603             leavesr;
044300150603           endif;
044400150603           dLAT   = T02uni;
044500150603           V1Dipt = dLAT.§LATdes;
044600160223         else;
044700160223           V1Dipt = '...Da lasciare com''è!';
044800150603         endif;
044900150603
045000150603         // -?Controllo Nome Libreria del *pgm iniziale?
045100160223         If  V1Clib <> *blank;
045200150603
045300160223           V1Clib = %trimL ( V1Clib );
045400160223
045500160223           Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + c_CmdCHKOBJlib;
045600160223           exsr  sr_ExecCmd;
045700160223           if  Qusei <> *blank;
045800160223             $errGenerico = *on;
045900160223             $errMessage  = *on;
046000160223             $PosCurLIB   = *on;
046100160223             V1Dmsg = sk_Msg(06);
046200160223             leavesr;
046300160223           endif;
046400160223
046500160223           Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + '/' + c_Pgm +
046600160223                  c_CmdCHKOBJpgm;
046700160223           exsr  sr_ExecCmd;
046800160223           if  Qusei <> *blank;
046900160223             $errGenerico = *on;
047000160223             $errMessage  = *on;
047100160223             $PosCurLIB   = *on;
047200160223             V1Dmsg = sk_Msg(07);
047300160223             leavesr;
047400160223           endif;
047500160223
047600160223         EndIf;
047700160223
047800160223         if  V1Cipt = *blank  and  V1Clib = *blank;
047900160223           $errGenerico = *on;
048000160223           $errMessage  = *on;
048100160223           $PosCurLIB   = *on;
048200160223           V1Dmsg = sk_Msg(09);
048300160223           leavesr;
048400160223         endif;
048500150603
048600150603       ENDSR;
048700150603
048800150603       //--------------------------------------------------------------
048900150603       //?Modifica dei profili utente selezionati.                     ?
049000150603       //--------------------------------------------------------------
049100150603       BEGSR  sr_ChgUsrPrf_All;
049200150608
049300150608         clear  V3Dnum;
049400150604
049500150604         // -?Elaborazione dei Profili della Filiale indicata?
049600150604         //  ?(inserito nessun utente o utente parziale)?
049700150604         If  V1Cute = *blank  or
049800150604             %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
049900150604
050000150604           select;
050100150604             when  V1Cute = *blank;
050200150604               W1Cute0 = *loval;
050300150604               W1Cute9 = *hival;
050400150604             when  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
050500150604               W1Cute0 = %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 );
050600150604               W1Cute9 = *hival;
050700150604               %subst ( W1Cute9 : 1 : %len( %trimR( V1Cute ) ) - 1 ) =
050800150604                 %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 );
050900150604             other;
051000150604               W1Cute0 = V1Cute;
051100150604               W1Cute9 = V1Cute;
051200150604           endsl;
051300150604
051400160223           open  AZUTE02L;
051500160223           If  V1Cfil = *zero;
051600160223             setll  ( *loval )  AZUTE002;
051700160223             read   AZUTE002;
051800160223           Else;
051900160223             setll  ( V1Cfil )  AZUTE002;
052000160223             readE  ( V1Cfil )  AZUTE002;
052100160223           EndIf;
052200150604           DoW  Not %eof(AZUTE02L);
052300150604             if  UTE2ute >= W1Cute0  and  UTE2ute <= W1Cute9;
052400150604               chain  ( UTE2ute )  KPPRF000;
052500150604               if  %found(KPPRF01L);
052600150604                 exsr  sr_ChgUsrPrf_Sing;
052700150604               endif;
052800150604             endif;
052900150604             if  $ErrGenerico;
053000150604               leave;
053100150604             endif;
053200160223             if  V1Cfil = *zero;
053300160223               read   AZUTE002;
053400160223             else;
053500160223               readE  ( V1Cfil )  AZUTE002;
053600160223             endif;
053700150604           EndDo;
053800150604           close  AZUTE02L;
053900150604
054000150604         // -?Elaborazione del singolo Profilo?
054100150604         Else;
054200150604
054300150604           chain  ( V1Cute )  KPPRF000;
054400150604           if  %found(KPPRF01L);
054500150604             exsr  sr_ChgUsrPrf_Sing;
054600150608             //if  $ErrGenerico;
054700150608             //  leavesr;
054800150608             //endif;
054900150604           endif;
055000150604
055100150604         EndIf;
055200150608
055300150608         // -?Visualizzazione n° profili modificati?
055400150608         DoU  dsp_aid = c_F12;
055500150608           exfmt  ULIPJD03;
055600150608         EndDo;
055700150603
055800150603       ENDSR;
055900150603
056000150603       //--------------------------------------------------------------
056100150603       //?Modifica del singolo Profilo Utente.                         ?
056200150603       //--------------------------------------------------------------
056300150603       BEGSR  sr_ChgUsrPrf_Sing;
056400150604
056500150604         // -?Utente NON di Filiale => uscita?
056600150604         if  ( %subst( currSysNeta : 1 : 6 ) <> 'SETRAS'   and
056700150604               %subst( currSysNeta : 1 : 6 ) <> 'AS888 ' )  or
056800150604             ( %subst( currSysNeta : 1 : 6 )  = 'SETRAS'   and
056900150604               PRFnsi <> 'FILTRA201 ' )                     or
057000150604             ( %subst( currSysNeta : 1 : 6 )  = 'AS888 '   and
057100150604               PRFnsi <> 'FILTRAPFP ' );
057200150604           leavesr;
057300150604         endif;
057400150604
057500150604         // -?Utente NON esistente su AS/400 => uscita?
057600150604         Qcmd = c_CmdCHKOBJ + %trimR( PRFnmu ) + c_CmdCHKOBJprf;
057700150604         exsr  sr_ExecCmd;
057800150604         if  Qusei <> *blank;
057900150604           leavesr;
058000150604         endif;
058100150603
058200150604         // -?Utente NON "valido" in AZUTE00F => uscita?
058300160223         if  V1Cipt <> *blank;
058400160223           chain  ( PRFnmu )  AZUTE000;
058500160223         else;
058600160223           chain(N)  ( PRFnmu )  AZUTE000;
058700160223         endif;
058800160225         if  NOT %found(AZUTE01L)  or  UTEdsc < wDate  or
058900160225             ( V1Cfil <> *zero    and  UTEfil <> V1Cfil );
059000160223           if  V1Cipt <> *blank;
059100160223             unlock  AZUTE01L;
059200160223           endif;
059300150604           leavesr;
059400150604         endif;
059500150604
059600150604         // -?Aggiornamento IP Terminale?
059700160223         If  V1Cipt <> *blank;
059800160223           dUte01 = UTEfaf;
059900160223           if  dUte01.§UTEipt = V1Cipt;
060000160223             unlock  AZUTE01L;
060100160223           else;
060200160223             dUte01.§UTEipt = V1Cipt;
060300160223             UTEfaf = dUte01;
060400160223             update  AZUTE000;
060500160223           endif;
060600160223         EndIf;
060700150604
060800150604         // -?Aggiornamento Programma Iniziale da Richiamare?
060900150616         //  ?(se indicato)?
061000150616         If  V1Clib <> *blank;
061100150616           Qcmd = 'CHGUSRPRF +
061200150616                      usrprf(' + %trimR( PRFnmu ) + ') +
061300150616                      inlpgm(' + %trimR( V1Clib ) + '/' + c_Pgm +')';
061400150616           exsr  sr_ExecCmd;
061500150616           if  Qusei <> *blank;
061600150616             $errGenerico = *on;
061700150616             $errMessage  = *on;
061800150616             $PosCurLIB   = *on;
061900150616             V1Dmsg = %trimR ( sk_Msg(08) ) + ' ' + PRFnmu;
062000150616             leavesr;
062100150616           endif;
062200150616         EndIf;
062300150608
062400150608         // -?Conteggio profili modificati?
062500150608         V3Dnum += 1;
062600150603
062700150603       ENDSR;
062800150603
062900150603       //--------------------------------------------------------------
063000150603       //?Esecuzione del comando (già impostato).                      ?
063100150603       //--------------------------------------------------------------
063200150603       BEGSR  sr_ExecCmd;
063300150603
063400150603         clear Qcap0100;
063500150603         Qcabcsdh = *off;
063600150603         Qcapa    = *off;
063700150603         Qcacmdss = *off;
063800150603         Qcaerved = *allX'00';
063900150603
064000150603         clear Qusec;
064100150603         Qusbprv  = %size(Qusec);
064200150603
064300150603         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
064400150603                           %size(Qcap0100) : 'CPOP0100' : *omit :
064500150603                           0 : 0 : Qusec);
064600150603
064700150603         //if  Qusei <> *blank;
064800150603         //  ...;
064900150603         //endif;
065000150603
065100150603       ENDSR;
065200150604
065300150604       //--------------------------------------------------------------
065400150604       //?Operazioni finali.                                           ?
065500150604       //--------------------------------------------------------------
065600150604       BEGSR  sr_RoutEnd;
065700150604
065800150604         // -?Chiusura applicazioni precedentemente aperte?
065900150604         clear  TIBS02ds;
066000150604         T02tla = 'C';
066100150604         TNTBE_RicercaControllo ( kpjba : tibs02ds );
066200150604
066300150604         // -?Chiusura pgm?
066400150604         return;
066500150604
066600150604       ENDSR;
066700150603
066800150603      /end-free
066900150604
067000150604       //--------------------------------------------------------------
067100150604       //?Definizione schiere a tempo di compilazione                  ?
067200150604       //--------------------------------------------------------------
067300150604
067400150604** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
067500150604Utente errato                                                                   1
067600150604Filiale obbligatoria                                                            2
067700150604Filiale errata                                                                  3
067800150604IP Terminale errato                                                             4
067900150604Nome Libreria obbligatoria                                                      5
068000150604Libreria inesistente                                                            6
068100150604Programma inesistente in questa libreria                                        7
068200150604Aggiornamento NON riuscito per l'utente                                         8
068300160223Impostare almeno un aggiornamento                                               9
