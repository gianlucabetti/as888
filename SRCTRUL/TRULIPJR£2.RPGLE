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
001300150604     /*PRM  usrprf(*owner)
001400150604     /*CMD  addlible lib(GAITRAFIL)  position(*after QGPL)
001500150604     /*CMD  addlible lib(GAITRAGRPS) position(*after QGPL)
001600150604     /*END  rmvlible lib(GAITRAFIL)
001700150604     /*END  rmvlible lib(GAITRAGRPS)
001800150603     /*END
001900150603
002000150603       //--------------------------------------------------------------
002100150603       //?Specifiche di controllo.                                     ?
002200150603       //--------------------------------------------------------------
002300150603
002400150603     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002500150603     h dftactgrp(*no)
002600150603     h bnddir('UBRTVNETA')
002700150603
002800150603       //--------------------------------------------------------------
002900150603       //?Dichiarazione file.                                          ?
003000150603       //--------------------------------------------------------------
003100150603
003200150603       // -?Organigramma?
003300150603     fAZORG01L  if   e           k disk
003400150603
003500150603       // -?Profili Utente?
003600150603     fKPPRF01L  if   e           k disk
003700150604     fAZUTE02L  if   e           k disk    usropn
003800150604     f                                     rename( AZUTE000 : AZUTE002 )
003900150604     f                                     prefix( UTE2 : 3 )
004000150604     fAZUTE01L  Uf   e           k disk
004100150603
004200150603       // -?Video?
004300150603     fTRULIPJD  cf   e             workstn
004400150603     f                                     indds(IndDspF)
004500150603     f                                     infds(InfDspF)
004600150603
004700150603       //--------------------------------------------------------------
004800150603       //?Definizione costanti.                                        ?
004900150603       //--------------------------------------------------------------
005000150603
005100150603       // -?Comando di verifica esistenza libreria?
005200150603     d c_CmdCHKOBJ     c                   const('CHKOBJ +
005300150603     d                                            obj(')
005400150603     d c_CmdCHKOBJlib  c                   const(') +
005500150603     d                                            objtype(*lib)')
005600150603     d c_CmdCHKOBJpgm  c                   const(') +
005700150603     d                                            objtype(*pgm)')
005800150604     d c_CmdCHKOBJprf  c                   const(') +
005900150604     d                                            objtype(*usrprf)')
006000150603
006100150603       // -?Nome del *pgm iniziale per gli utenti?
006200150603     d c_Pgm           c                   const('BAK010C')
006300150603
006400150603       // -?Tasti funzionali a video?
006500150603     d c_F01           c                   const(x'31')
006600150603     d c_F02           c                   const(x'32')
006700150603     d c_F03           c                   const(x'33')
006800150603     d c_F04           c                   const(x'34')
006900150603     d c_F05           c                   const(x'35')
007000150603     d c_F06           c                   const(x'36')
007100150603     d c_F07           c                   const(x'37')
007200150603     d c_F08           c                   const(x'38')
007300150603     d c_F09           c                   const(x'39')
007400150603     d c_F10           c                   const(x'3A')
007500150603     d c_F11           c                   const(x'3B')
007600150603     d c_F12           c                   const(x'3C')
007700150603     d c_F13           c                   const(x'B1')
007800150603     d c_F14           c                   const(x'B2')
007900150603     d c_F15           c                   const(x'B3')
008000150603     d c_F16           c                   const(x'B4')
008100150603     d c_F17           c                   const(x'B5')
008200150603     d c_F18           c                   const(x'B6')
008300150603     d c_F19           c                   const(x'B7')
008400150603     d c_F20           c                   const(x'B8')
008500150603     d c_F21           c                   const(x'B9')
008600150603     d c_F22           c                   const(x'BA')
008700150603     d c_F23           c                   const(x'BB')
008800150603     d c_F24           c                   const(x'BC')
008900150603     d c_Enter         c                   const(x'F1')
009000150603     d c_RollDown      c                   const(x'F4')
009100150603     d c_RollUp        c                   const(x'F5')
009200150603
009300150603       //--------------------------------------------------------------
009400150603       //?Definizione schiere.                                         ?
009500150603       //--------------------------------------------------------------
009600150603
009700150603       // -?Messaggi di errore?
009800150604     d sk_Msg          s             78    dim( 8)  ctdata  perrcd(1)
009900150603
010000150603       //--------------------------------------------------------------
010100150603       //?Definizione aree dati.                                       ?
010200150603       //--------------------------------------------------------------
010300150603
010400150603       // -?Dati utente?
010500150603     d §AzUte        e ds                  extname(AZUTE00F)
010600150603     d                                     dtaara
010700150603     d §DatiUte      e ds                  extname(dDatiUte)
010800150603     d                                     dtaara
010900150603
011000150603       //--------------------------------------------------------------
011100150603       //?Definizione strutture dati.                                  ?
011200150603       //--------------------------------------------------------------
011300150603
011400150603       // -?Status ds?
011500150603     d Status         sds
011600150603     d   SDSpgm          *proc
011700150603     d*//SDSprm          *parms
011800150603     d*//SDSdta              191    198
011900150603     d*//SDSjob              244    253
012000150603     d   SDSusr              254    263
012100150603
012200150603       // -?InfDS?
012300150603     d InfDspF         ds
012400150603     d   dsp_aid             369    369a
012500150603     d*//sfl_rrn             376    377i 0
012600150603     d*//min_nrr             378    379i 0
012700150603     d*//num_rcds            380    381i 0
012800150603
012900150603       // -?Indicatori su DspF?
013000150603     d IndDspF         ds                  inz
013100150603         // -?Abilitazione tasti funzionali?
013200150603         // -?Emissione messaggio di errore?
013300150604     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013400150603         // -?Indicatori di gestione del subfile?
013500150603         // -?Indicatori per Attibuti di visualizzazione?
013600150603         //  ?o   Condizionamento visualizzazione campi?
013700150603         // -?Posizionamento cursore & segnalazione errore?
013800150604     d   $PosCurUTE                    n   overlay( IndDspF : 51 )
013900150604     d   $PosCurFIL                    n   overlay( IndDspF : 52 )
014000150604     d   $PosCurIPT                    n   overlay( IndDspF : 53 )
014100150604     d   $PosCurLIB                    n   overlay( IndDspF : 54 )
014200150603         //   -?Riemissione videata?
014300150604     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
014400150603
014500150603       // -?Parametri ricevuti?
014600150603     d KPJBA         e ds
014700150603
014800150603       // -?Tab. "LAT" = Sigla Funzione Aziendale?
014900150603     d dLAT          e ds                  qualified  inz
015000150603
015100150603       // -?Flags Abilitazioni Utenti Aziendali?
015200150603     d dUte01        e ds                  qualified  inz
015300150603
015400150603       //--------------------------------------------------------------
015500150603       //?Definizione variabili globali.                               ?
015600150603       //--------------------------------------------------------------
015700150603
015800150603       // -?Flags booleani?
015900150603     d $Fine           s               n   inz
016000150603     d $InzD01         s               n   inz(*on)
016100150603
016200150603       // -?Campi associati al video?
016300150603     d $Video          s              2    inz('D1')
016400150604
016500150604       // -?Data odierna?
016600150604     d wDate           s              8  0 inz
016700150604
016800150604       // -?Campi per parzializzazioni?
016900150604     d W1Cute0         s                   like(V1Cute)  inz(*loval)
017000150604     d W1Cute9         s                   like(V1Cute)  inz(*hival)
017100150603
017200150603       //--------------------------------------------------------------
017300150603       //?Definizione prototipi procedure usate.                       ?
017400150603       //--------------------------------------------------------------
017500150603
017600150603       // -?Reperimento NETA sistema AS/400 corrente?
017700150603     d currSysNeta     s              8a   inz
017800150603      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017900150603
018000150603       // -?Reperimento dati utente?
018100150603     d TIBS34ds      e ds                  inz
018200150603      /copy gaitrasrc/srcProtoPR,TIBS34R
018300150603
018400150603       // -?Ricerca/Controllo tabelle?
018500150603     d TIBS02ds      e ds                  inz
018600150604     d   T02mod      e                     inz('C')
018700150604     d   T02cod      e                     inz('LAT')
018800150603      /copy gaitrasrc/srcProtoPR,TIBS02R
018900150603
019000150603       // -?API QCAPCMD (Process Commands)?
019100150603     d Qcmd            s           2048    inz  varying
019200150603      /copy qSysInc/qRpgleSrc,QCAPCMD
019300150603      /copy gaitrasrc/srcProtoPR,QCAPCMD
019400150603
019500150603       // -?Parametri gestione errori API.?
019600150603      /copy qsysinc/qrpglesrc,QUSEC
019700150603
019800150603       //--------------------------------------------------------------
019900150603       //?Definizione key-list.                                        ?
020000150603       //--------------------------------------------------------------
020100150603
020200150604       // -?File KPPRF01L?
020300150604     d keyKPPRF01    e ds                  extname( KPPRF01L : *key )
020400150604     d                                     prefix( k_ )  inz
020500150603
020600150603       //--------------------------------------------------------------
020700150603       //?Riepilogo indicatori utilizzati.                             ?
020800150603       //--------------------------------------------------------------
020900150603
021000150603
021100150603       //--------------------------------------------------------------
021200150603       //?M A I N - L I N E                                            ?
021300150603       //--------------------------------------------------------------
021400150603
021500150603     c     *Entry        plist
021600150603     c                   parm                    KPJBA
021700150603
021800150603      /free
021900150603
022000150603       // -?Operazioni iniziali?
022100150603       exsr  sr_RoutInz;
022200150603
022300150603       // -?Ciclo di gestione del file video?
022400150603       DoW  $Fine = *off;
022500150603
022600150603         select;
022700150603
022800150603           // -?Gestione videata D1 (parzializzazioni)?
022900150603           when  $Video = 'D1';
023000150603             exsr  sr_GesD01;
023100150603
023200150603           // -?? ? ??
023300150603           other;
023400150603             $Fine = *on;
023500150603
023600150603         endsl;
023700150603
023800150603       EndDo;
023900150603
024000150603       // -?Operazioni finali?
024100150603       exsr  sr_RoutEnd;
024200150603
024300150603       //--------------------------------------------------------------
024400150603       //?Operazioni iniziali.                                         ?
024500150603       //--------------------------------------------------------------
024600150603       BEGSR  sr_RoutInz;
024700150603
024800150603         // -?Impostazione chiusura?
024900150603         *inLR = *on;
025000150603
025100150603         // -?Verifica del sistema AS/400 corrente?
025200150603         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
025300150603           exsr  sr_RoutEnd;
025400150603         endif;
025500150604
025600150604         // -?Reperimento data odierna?
025700150604         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025800150604                               : 1 : 8 ) );
025900150603
026000150603         // -?Reperimento dati job?
026100150603         exsr  sr_DatiJob;
026200150603
026300150603         // -?Impostazione nome programma a video?
026400150603         V1Tpgm = SDSpgm;
026500150603
026600150603       ENDSR;
026700150603
026800150603       //--------------------------------------------------------------
026900150603       //?Reperimento Dati del job (Utente/Operativi).                 ?
027000150603       //--------------------------------------------------------------
027100150603       BEGSR  sr_DatiJob;
027200150603
027300150603         in(e) §AzUte;
027400150603         if NOT %error;
027500150603           in(e) §DatiUte;
027600150603         endif;
027700150603         if %error or RSut = *blank;
027800150603           tibs34r ( tibs34ds );
027900150603           in §AzUte;
028000150603           in §DatiUte;
028100150603         endif;
028200150603
028300150603       ENDSR;
028400150603
028500150603       //--------------------------------------------------------------
028600150603       //?Gestione videata D01.                                        ?
028700150603       //--------------------------------------------------------------
028800150603       BEGSR  sr_GesD01;
028900150603
029000150603         // -?Inizializzazione videata?
029100150603         if  $InzD01 = *on;
029200150603           exsr  sr_InzD01;
029300150603           $InzD01 = *off;
029400150603         endif;
029500150603
029600150603         // -?Emissione testata e piede?
029700150603         write  ULIPJT01;
029800150603         write  ULIPJP01;
029900150603
030000150603         // -?Emissione videata?
030100150603         exfmt  ULIPJD01;
030200150603
030300150604         clear  $errGenerico;
030400150604         clear  $errMessage;
030500150603         clear  V1Dmsg;
030600150603
030700150603         Select;
030800150603
030900150603           // -?F3=Fine?
031000150603           when  dsp_aid = c_F03;
031100150603             $Fine  = *on;
031200150603
031300150603           // -?Invio / F6=Conferma?
031400150603           Other;
031500150603             exsr  sr_CtrD01;
031600150604             if  $errGenerico;
031700150603               leavesr;
031800150603             endif;
031900150603             if  dsp_aid = c_F06;
032000150608               write  Protect;
032100150608               write  ULIPJD02;
032200150603               exsr  sr_ChgUsrPrf_All;
032300150603             endif;
032400150603
032500150603         EndSl;
032600150603
032700150603       ENDSR;
032800150603
032900150603       //--------------------------------------------------------------
033000150603       //?Inizializzazione videata D01.                                ?
033100150603       //--------------------------------------------------------------
033200150603       BEGSR  sr_InzD01;
033300150603
033400150603         clear  ULIPJD01;
033500150603
033600150603         V1Cute = SDSusr;
033700150603         //V1Dute = 'Tutti i profili ';
033800150603         chain  ( V1Cute )  KPPRF000;
033900150603         if  %found(KPPRF01L);
034000150603           V1Dute = PRFtxt;
034100150603         else;
034200150603           V1Dute = *all'? ';
034300150603         endif;
034400150603
034500150604         //V1Dfil = 'Tutte le filiali';
034600150604         V1Cfil = DUTpou;
034700150604         chain  ( V1Cfil )  AZORG;
034800150604         if  %found(AZORG01L);
034900150604           V1Dfil = ORGdes;
035000150604         else;
035100150604           V1Dfil = *all'? ';
035200150604         endif;
035300150603
035400150603         V1Cipt = 'PO';
035500150603         reset  TIBS02ds;
035600150603         T02ke1 = V1Cipt;
035700150603         TNTBE_RicercaControllo ( kpjba : tibs02ds );
035800150603         dLAT   = T02uni;
035900150603         V1Dipt = dLAT.§LATdes;
036000150603
036100150603         V1Clib = 'PROBAS    ';
036200150603
036300150603       ENDSR;
036400150603
036500150603       //--------------------------------------------------------------
036600150603       //?Controllo dati in videata D01.                               ?
036700150603       //--------------------------------------------------------------
036800150603       BEGSR  sr_CtrD01;
036900150603
037000150603         // -?Spegnimento degli indicatori di errore?
037100150603         %subst(IndDspF : 50) = *off;
037200150603
037300150603         // -?Controllo Profilo dell'utente?
037400150603         clear  V1Dute;
037500150603         V1Cute = %trimL ( V1Cute );
037600150603         select;
037700150603           when  V1Cute = *blank;
037800150603             V1Dute = 'Tutti i profili ';
037900150604           when  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
038000150603             V1Dute = 'Tutti i profili ' +
038100150604                      %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 ) +
038200150603                      '...';
038300150603           other;
038400150603             chain  ( V1Cute )  KPPRF000;
038500150603             if  %found(KPPRF01L);
038600150603               V1Dute = PRFtxt;
038700150603             else;
038800150603               V1Dute = *all'? ';
038900150604               $errGenerico = *on;
039000150604               $errMessage  = *on;
039100150604               $PosCurUTE   = *on;
039200150604               V1Dmsg = sk_Msg(01);
039300150603               leavesr;
039400150603             endif;
039500150603         endsl;
039600150603
039700150603         clear  dUte01;
039800150603         if  V1Cute <> *blank                                          and
039900150604             %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) <> '*'  and
040000150603             %found(KPPRF01L);
040100150604           chain(N)  ( V1Cute )  AZUTE000;
040200150604           if  %found(AZUTE01L);
040300150603             dUte01 = UTEfaf;
040400150603           endif;
040500150603         endif;
040600150603
040700150603         // -?Controllo Filiale dell'utente?
040800150603         clear  V1Dfil;
040900150603         if  V1Cfil = *zero;
041000150604           //V1Dfil = 'Tutte le Filiali';
041100150604           V1Dfil = *all'? ';
041200150604           $errGenerico = *on;
041300150604           $errMessage  = *on;
041400150604           $PosCurFIL   = *on;
041500150604           V1Dmsg = sk_Msg(02);
041600150604           leavesr;
041700150603         else;
041800150603           chain  ( V1Cfil )  AZORG;
041900150603           if  %found(AZORG01L);
042000150603             V1Dfil = ORGdes;
042100150603           else;
042200150603             V1Dfil = *all'? ';
042300150604             $errGenerico = *on;
042400150604             $errMessage  = *on;
042500150604             $PosCurFIL   = *on;
042600150604             V1Dmsg = sk_Msg(03);
042700150603             leavesr;
042800150603           endif;
042900150603         endif;
043000150603
043100150603         // -?Controllo IP terminale dell'utente?
043200150603         clear  V1Dipt;
043300150603         if  V1Cipt <> *blank;
043400150603           reset  TIBS02ds;
043500150603           T02ke1 = V1Cipt;
043600150603           TNTBE_RicercaControllo ( kpjba : tibs02ds );
043700150603           if  T02err <> *blank;
043800150603             V1Dipt = *all'? ';
043900150604             $errGenerico = *on;
044000150604             $errMessage  = *on;
044100150604             $PosCurIPT   = *on;
044200150604             V1Dmsg = sk_Msg(04);
044300150603             leavesr;
044400150603           endif;
044500150603           dLAT   = T02uni;
044600150603           V1Dipt = dLAT.§LATdes;
044700150603         endif;
044800150603
044900150603         // -?Controllo Nome Libreria del *pgm iniziale?
045000150603         if  V1Clib = *blank;
045100150604           $errGenerico = *on;
045200150604           $errMessage  = *on;
045300150604           $PosCurLIB   = *on;
045400150604           V1Dmsg = sk_Msg(05);
045500150603           leavesr;
045600150603         endif;
045700150603
045800150603         V1Clib = %trimL ( V1Clib );
045900150603
046000150603         Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + c_CmdCHKOBJlib;
046100150603         exsr  sr_ExecCmd;
046200150603         if  Qusei <> *blank;
046300150604           $errGenerico = *on;
046400150604           $errMessage  = *on;
046500150604           $PosCurLIB   = *on;
046600150604           V1Dmsg = sk_Msg(06);
046700150603           leavesr;
046800150603         endif;
046900150603
047000150603         Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + '/' + c_Pgm +
047100150603                c_CmdCHKOBJpgm;
047200150603         exsr  sr_ExecCmd;
047300150603         if  Qusei <> *blank;
047400150604           $errGenerico = *on;
047500150604           $errMessage  = *on;
047600150604           $PosCurLIB   = *on;
047700150604           V1Dmsg = sk_Msg(07);
047800150603           leavesr;
047900150603         endif;
048000150603
048100150603       ENDSR;
048200150603
048300150603       //--------------------------------------------------------------
048400150603       //?Modifica dei profili utente selezionati.                     ?
048500150603       //--------------------------------------------------------------
048600150603       BEGSR  sr_ChgUsrPrf_All;
048700150608
048800150608         clear  V3Dnum;
048900150604
049000150604         // -?Elaborazione dei Profili della Filiale indicata?
049100150604         //  ?(inserito nessun utente o utente parziale)?
049200150604         If  V1Cute = *blank  or
049300150604             %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
049400150604
049500150604           select;
049600150604             when  V1Cute = *blank;
049700150604               W1Cute0 = *loval;
049800150604               W1Cute9 = *hival;
049900150604             when  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
050000150604               W1Cute0 = %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 );
050100150604               W1Cute9 = *hival;
050200150604               %subst ( W1Cute9 : 1 : %len( %trimR( V1Cute ) ) - 1 ) =
050300150604                 %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 );
050400150604             other;
050500150604               W1Cute0 = V1Cute;
050600150604               W1Cute9 = V1Cute;
050700150604           endsl;
050800150604
050900150604           open  AZUTE02L;
051000150604           setll  ( V1Cfil )  AZUTE002;
051100150604           readE  ( V1Cfil )  AZUTE002;
051200150604           DoW  Not %eof(AZUTE02L);
051300150604             if  UTE2ute >= W1Cute0  and  UTE2ute <= W1Cute9;
051400150604               chain  ( UTE2ute )  KPPRF000;
051500150604               if  %found(KPPRF01L);
051600150604                 exsr  sr_ChgUsrPrf_Sing;
051700150604               endif;
051800150604             endif;
051900150604             if  $ErrGenerico;
052000150604               leave;
052100150604             endif;
052200150604             readE  ( V1Cfil )  AZUTE002;
052300150604           EndDo;
052400150604           close  AZUTE02L;
052500150604
052600150604         // -?Elaborazione del singolo Profilo?
052700150604         Else;
052800150604
052900150604           chain  ( V1Cute )  KPPRF000;
053000150604           if  %found(KPPRF01L);
053100150604             exsr  sr_ChgUsrPrf_Sing;
053200150608             //if  $ErrGenerico;
053300150608             //  leavesr;
053400150608             //endif;
053500150604           endif;
053600150604
053700150604         EndIf;
053800150608
053900150608         // -?Visualizzazione n° profili modificati?
054000150608         DoU  dsp_aid = c_F12;
054100150608           exfmt  ULIPJD03;
054200150608         EndDo;
054300150603
054400150603       ENDSR;
054500150603
054600150603       //--------------------------------------------------------------
054700150603       //?Modifica del singolo Profilo Utente.                         ?
054800150603       //--------------------------------------------------------------
054900150603       BEGSR  sr_ChgUsrPrf_Sing;
055000150604
055100150604         // -?Utente NON di Filiale => uscita?
055200150604         if  ( %subst( currSysNeta : 1 : 6 ) <> 'SETRAS'   and
055300150604               %subst( currSysNeta : 1 : 6 ) <> 'AS888 ' )  or
055400150604             ( %subst( currSysNeta : 1 : 6 )  = 'SETRAS'   and
055500150604               PRFnsi <> 'FILTRA201 ' )                     or
055600150604             ( %subst( currSysNeta : 1 : 6 )  = 'AS888 '   and
055700150604               PRFnsi <> 'FILTRAPFP ' );
055800150604           leavesr;
055900150604         endif;
056000150604
056100150604         // -?Utente NON esistente su AS/400 => uscita?
056200150604         Qcmd = c_CmdCHKOBJ + %trimR( PRFnmu ) + c_CmdCHKOBJprf;
056300150604         exsr  sr_ExecCmd;
056400150604         if  Qusei <> *blank;
056500150604           leavesr;
056600150604         endif;
056700150603
056800150604         // -?Utente NON "valido" in AZUTE00F => uscita?
056900150604         chain  ( PRFnmu )  AZUTE000;
057000150604         if  NOT %found(AZUTE01L)  or  UTEdsc < wDate
057100150604                                   or  UTEfil <> V1Cfil;
057200150604           unlock  AZUTE01L;
057300150604           leavesr;
057400150604         endif;
057500150604
057600150604         // -?Aggiornamento IP Terminale?
057700150604         dUte01 = UTEfaf;
057800150604         if  dUte01.§UTEipt = V1Cipt;
057900150604           unlock  AZUTE01L;
058000150604         else;
058100150604           dUte01.§UTEipt = V1Cipt;
058200150604           UTEfaf = dUte01;
058300150604           update  AZUTE000;
058400150604         endif;
058500150604
058600150604         // -?Aggiornamento Programma Iniziale da Richiamare?
058700150604         Qcmd = 'CHGUSRPRF +
058800150604                    usrprf(' + %trimR( PRFnmu ) + ') +
058900150604                    inlpgm(' + %trimR( V1Clib ) + '/' + c_Pgm +')';
059000150604         exsr  sr_ExecCmd;
059100150604         if  Qusei <> *blank;
059200150604           $errGenerico = *on;
059300150604           $errMessage  = *on;
059400150604           $PosCurLIB   = *on;
059500150604           V1Dmsg = %trimR ( sk_Msg(08) ) + ' ' + PRFnmu;
059600150604           leavesr;
059700150604         endif;
059800150608
059900150608         // -?Conteggio profili modificati?
060000150608         V3Dnum += 1;
060100150603
060200150603       ENDSR;
060300150603
060400150603       //--------------------------------------------------------------
060500150603       //?Esecuzione del comando (già impostato).                      ?
060600150603       //--------------------------------------------------------------
060700150603       BEGSR  sr_ExecCmd;
060800150603
060900150603         clear Qcap0100;
061000150603         Qcabcsdh = *off;
061100150603         Qcapa    = *off;
061200150603         Qcacmdss = *off;
061300150603         Qcaerved = *allX'00';
061400150603
061500150603         clear Qusec;
061600150603         Qusbprv  = %size(Qusec);
061700150603
061800150603         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
061900150603                           %size(Qcap0100) : 'CPOP0100' : *omit :
062000150603                           0 : 0 : Qusec);
062100150603
062200150603         //if  Qusei <> *blank;
062300150603         //  ...;
062400150603         //endif;
062500150603
062600150603       ENDSR;
062700150604
062800150604       //--------------------------------------------------------------
062900150604       //?Operazioni finali.                                           ?
063000150604       //--------------------------------------------------------------
063100150604       BEGSR  sr_RoutEnd;
063200150604
063300150604         // -?Chiusura applicazioni precedentemente aperte?
063400150604         clear  TIBS02ds;
063500150604         T02tla = 'C';
063600150604         TNTBE_RicercaControllo ( kpjba : tibs02ds );
063700150604
063800150604         // -?Chiusura pgm?
063900150604         return;
064000150604
064100150604       ENDSR;
064200150603
064300150603      /end-free
064400150604
064500150604       //--------------------------------------------------------------
064600150604       //?Definizione schiere a tempo di compilazione                  ?
064700150604       //--------------------------------------------------------------
064800150604
064900150604** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
065000150604Utente errato                                                                   1
065100150604Filiale obbligatoria                                                            2
065200150604Filiale errata                                                                  3
065300150604IP Terminale errato                                                             4
065400150604Nome Libreria obbligatoria                                                      5
065500150604Libreria inesistente                                                            6
065600150604Programma inesistente in questa libreria                                        7
065700150604Aggiornamento NON riuscito per l'utente                                         8
