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
003700150603     fAZUTE01L  Uf   e           k disk
003800150603
003900150603       // -?Video?
004000150603     fTRULIPJD  cf   e             workstn
004100150603     f                                     indds(IndDspF)
004200150603     f                                     infds(InfDspF)
004300150603
004400150603       //--------------------------------------------------------------
004500150603       //?Definizione costanti.                                        ?
004600150603       //--------------------------------------------------------------
004700150603
004800150603       // -?Comando di verifica esistenza libreria?
004900150603     d c_CmdCHKOBJ     c                   const('CHKOBJ +
005000150603     d                                            obj(')
005100150603     d c_CmdCHKOBJlib  c                   const(') +
005200150603     d                                            objtype(*lib)')
005300150603     d c_CmdCHKOBJpgm  c                   const(') +
005400150603     d                                            objtype(*pgm)')
005500150604     d c_CmdCHKOBJprf  c                   const(') +
005600150604     d                                            objtype(*usrprf)')
005700150603
005800150603       // -?Nome del *pgm iniziale per gli utenti?
005900150603     d c_Pgm           c                   const('BAK010C')
006000150603
006100150603       // -?Tasti funzionali a video?
006200150603     d c_F01           c                   const(x'31')
006300150603     d c_F02           c                   const(x'32')
006400150603     d c_F03           c                   const(x'33')
006500150603     d c_F04           c                   const(x'34')
006600150603     d c_F05           c                   const(x'35')
006700150603     d c_F06           c                   const(x'36')
006800150603     d c_F07           c                   const(x'37')
006900150603     d c_F08           c                   const(x'38')
007000150603     d c_F09           c                   const(x'39')
007100150603     d c_F10           c                   const(x'3A')
007200150603     d c_F11           c                   const(x'3B')
007300150603     d c_F12           c                   const(x'3C')
007400150603     d c_F13           c                   const(x'B1')
007500150603     d c_F14           c                   const(x'B2')
007600150603     d c_F15           c                   const(x'B3')
007700150603     d c_F16           c                   const(x'B4')
007800150603     d c_F17           c                   const(x'B5')
007900150603     d c_F18           c                   const(x'B6')
008000150603     d c_F19           c                   const(x'B7')
008100150603     d c_F20           c                   const(x'B8')
008200150603     d c_F21           c                   const(x'B9')
008300150603     d c_F22           c                   const(x'BA')
008400150603     d c_F23           c                   const(x'BB')
008500150603     d c_F24           c                   const(x'BC')
008600150603     d c_Enter         c                   const(x'F1')
008700150603     d c_RollDown      c                   const(x'F4')
008800150603     d c_RollUp        c                   const(x'F5')
008900150603
009000150603       //--------------------------------------------------------------
009100150603       //?Definizione schiere.                                         ?
009200150603       //--------------------------------------------------------------
009300150603
009400150603       // -?Messaggi di errore?
009500150604     d sk_Msg          s             78    dim( 8)  ctdata  perrcd(1)
009600150603
009700150603       //--------------------------------------------------------------
009800150603       //?Definizione aree dati.                                       ?
009900150603       //--------------------------------------------------------------
010000150603
010100150603       // -?Dati utente?
010200150603     d §AzUte        e ds                  extname(AZUTE00F)
010300150603     d                                     dtaara
010400150603     d §DatiUte      e ds                  extname(dDatiUte)
010500150603     d                                     dtaara
010600150603
010700150603       //--------------------------------------------------------------
010800150603       //?Definizione strutture dati.                                  ?
010900150603       //--------------------------------------------------------------
011000150603
011100150603       // -?Status ds?
011200150603     d Status         sds
011300150603     d   SDSpgm          *proc
011400150603     d*//SDSprm          *parms
011500150603     d*//SDSdta              191    198
011600150603     d*//SDSjob              244    253
011700150603     d   SDSusr              254    263
011800150603
011900150603       // -?InfDS?
012000150603     d InfDspF         ds
012100150603     d   dsp_aid             369    369a
012200150603     d*//sfl_rrn             376    377i 0
012300150603     d*//min_nrr             378    379i 0
012400150603     d*//num_rcds            380    381i 0
012500150603
012600150603       // -?Indicatori su DspF?
012700150603     d IndDspF         ds                  inz
012800150603         // -?Abilitazione tasti funzionali?
012900150603         // -?Emissione messaggio di errore?
013000150604     d   $ErrMessage                   n   overlay( IndDspF : 28 )
013100150603         // -?Indicatori di gestione del subfile?
013200150603         // -?Indicatori per Attibuti di visualizzazione?
013300150603         //  ?o   Condizionamento visualizzazione campi?
013400150603         // -?Posizionamento cursore & segnalazione errore?
013500150604     d   $PosCurUTE                    n   overlay( IndDspF : 51 )
013600150604     d   $PosCurFIL                    n   overlay( IndDspF : 52 )
013700150604     d   $PosCurIPT                    n   overlay( IndDspF : 53 )
013800150604     d   $PosCurLIB                    n   overlay( IndDspF : 54 )
013900150603         //   -?Riemissione videata?
014000150604     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
014100150603
014200150603       // -?Parametri ricevuti?
014300150603     d KPJBA         e ds
014400150603
014500150603       // -?Tab. "LAT" = Sigla Funzione Aziendale?
014600150603     d dLAT          e ds                  qualified  inz
014700150603
014800150603       // -?Flags Abilitazioni Utenti Aziendali?
014900150603     d dUte01        e ds                  qualified  inz
015000150603
015100150603       //--------------------------------------------------------------
015200150603       //?Definizione variabili globali.                               ?
015300150603       //--------------------------------------------------------------
015400150603
015500150603       // -?Flags booleani?
015600150603     d $Fine           s               n   inz
015700150603     d $InzD01         s               n   inz(*on)
015800150603
015900150603       // -?Campi associati al video?
016000150603     d $Video          s              2    inz('D1')
016100150604
016200150604       // -?Data odierna?
016300150604     d wDate           s              8  0 inz
016400150603
016500150603       //--------------------------------------------------------------
016600150603       //?Definizione prototipi procedure usate.                       ?
016700150603       //--------------------------------------------------------------
016800150603
016900150603       // -?Reperimento NETA sistema AS/400 corrente?
017000150603     d currSysNeta     s              8a   inz
017100150603      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017200150603
017300150603       // -?Reperimento dati utente?
017400150603     d TIBS34ds      e ds                  inz
017500150603      /copy gaitrasrc/srcProtoPR,TIBS34R
017600150603
017700150603       // -?Ricerca/Controllo tabelle?
017800150603     d TIBS02ds      e ds                  inz
017900150604     d   T02mod      e                     inz('C')
018000150604     d   T02cod      e                     inz('LAT')
018100150603      /copy gaitrasrc/srcProtoPR,TIBS02R
018200150603
018300150603       // -?API QCAPCMD (Process Commands)?
018400150603     d Qcmd            s           2048    inz  varying
018500150603      /copy qSysInc/qRpgleSrc,QCAPCMD
018600150603      /copy gaitrasrc/srcProtoPR,QCAPCMD
018700150603
018800150603       // -?Parametri gestione errori API.?
018900150603      /copy qsysinc/qrpglesrc,QUSEC
019000150603
019100150603       //--------------------------------------------------------------
019200150603       //?Definizione key-list.                                        ?
019300150603       //--------------------------------------------------------------
019400150603
019500150604       // -?File KPPRF01L?
019600150604     d keyKPPRF01    e ds                  extname( KPPRF01L : *key )
019700150604     d                                     prefix( k_ )  inz
019800150603
019900150603       //--------------------------------------------------------------
020000150603       //?Riepilogo indicatori utilizzati.                             ?
020100150603       //--------------------------------------------------------------
020200150603
020300150603
020400150603       //--------------------------------------------------------------
020500150603       //?M A I N - L I N E                                            ?
020600150603       //--------------------------------------------------------------
020700150603
020800150603     c     *Entry        plist
020900150603     c                   parm                    KPJBA
021000150603
021100150603      /free
021200150603
021300150603       // -?Operazioni iniziali?
021400150603       exsr  sr_RoutInz;
021500150603
021600150603       // -?Ciclo di gestione del file video?
021700150603       DoW  $Fine = *off;
021800150603
021900150603         select;
022000150603
022100150603           // -?Gestione videata D1 (parzializzazioni)?
022200150603           when  $Video = 'D1';
022300150603             exsr  sr_GesD01;
022400150603
022500150603           // -?? ? ??
022600150603           other;
022700150603             $Fine = *on;
022800150603
022900150603         endsl;
023000150603
023100150603       EndDo;
023200150603
023300150603       // -?Operazioni finali?
023400150603       exsr  sr_RoutEnd;
023500150603
023600150603       //--------------------------------------------------------------
023700150603       //?Operazioni iniziali.                                         ?
023800150603       //--------------------------------------------------------------
023900150603       BEGSR  sr_RoutInz;
024000150603
024100150603         // -?Impostazione chiusura?
024200150603         *inLR = *on;
024300150603
024400150603         // -?Verifica del sistema AS/400 corrente?
024500150603         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
024600150603           exsr  sr_RoutEnd;
024700150603         endif;
024800150604
024900150604         // -?Reperimento data odierna?
025000150604         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025100150604                               : 1 : 8 ) );
025200150603
025300150603         // -?Reperimento dati job?
025400150603         exsr  sr_DatiJob;
025500150603
025600150603         // -?Impostazione nome programma a video?
025700150603         V1Tpgm = SDSpgm;
025800150603
025900150603       ENDSR;
026000150603
026100150603       //--------------------------------------------------------------
026200150603       //?Reperimento Dati del job (Utente/Operativi).                 ?
026300150603       //--------------------------------------------------------------
026400150603       BEGSR  sr_DatiJob;
026500150603
026600150603         in(e) §AzUte;
026700150603         if NOT %error;
026800150603           in(e) §DatiUte;
026900150603         endif;
027000150603         if %error or RSut = *blank;
027100150603           tibs34r ( tibs34ds );
027200150603           in §AzUte;
027300150603           in §DatiUte;
027400150603         endif;
027500150603
027600150603       ENDSR;
027700150603
027800150603       //--------------------------------------------------------------
027900150603       //?Gestione videata D01.                                        ?
028000150603       //--------------------------------------------------------------
028100150603       BEGSR  sr_GesD01;
028200150603
028300150603         // -?Inizializzazione videata?
028400150603         if  $InzD01 = *on;
028500150603           exsr  sr_InzD01;
028600150603           $InzD01 = *off;
028700150603         endif;
028800150603
028900150603         // -?Emissione testata e piede?
029000150603         write  ULIPJT01;
029100150603         write  ULIPJP01;
029200150603
029300150603         // -?Emissione videata?
029400150603         exfmt  ULIPJD01;
029500150603
029600150604         clear  $errGenerico;
029700150604         clear  $errMessage;
029800150603         clear  V1Dmsg;
029900150603
030000150603         Select;
030100150603
030200150603           // -?F3=Fine?
030300150603           when  dsp_aid = c_F03;
030400150603             $Fine  = *on;
030500150603
030600150603           // -?Invio / F6=Conferma?
030700150603           Other;
030800150603             exsr  sr_CtrD01;
030900150604             if  $errGenerico;
031000150603               leavesr;
031100150603             endif;
031200150603             if  dsp_aid = c_F06;
031300150603               exsr  sr_ChgUsrPrf_All;
031400150603             endif;
031500150603
031600150603         EndSl;
031700150603
031800150603       ENDSR;
031900150603
032000150603       //--------------------------------------------------------------
032100150603       //?Inizializzazione videata D01.                                ?
032200150603       //--------------------------------------------------------------
032300150603       BEGSR  sr_InzD01;
032400150603
032500150603         clear  ULIPJD01;
032600150603
032700150603         V1Cute = SDSusr;
032800150603         //V1Dute = 'Tutti i profili ';
032900150603         chain  ( V1Cute )  KPPRF000;
033000150603         if  %found(KPPRF01L);
033100150603           V1Dute = PRFtxt;
033200150603         else;
033300150603           V1Dute = *all'? ';
033400150603         endif;
033500150603
033600150604         //V1Dfil = 'Tutte le filiali';
033700150604         V1Cfil = DUTpou;
033800150604         chain  ( V1Cfil )  AZORG;
033900150604         if  %found(AZORG01L);
034000150604           V1Dfil = ORGdes;
034100150604         else;
034200150604           V1Dfil = *all'? ';
034300150604         endif;
034400150603
034500150603         V1Cipt = 'PO';
034600150603         reset  TIBS02ds;
034700150603         T02ke1 = V1Cipt;
034800150603         TNTBE_RicercaControllo ( kpjba : tibs02ds );
034900150603         dLAT   = T02uni;
035000150603         V1Dipt = dLAT.§LATdes;
035100150603
035200150603         V1Clib = 'PROBAS    ';
035300150603
035400150603       ENDSR;
035500150603
035600150603       //--------------------------------------------------------------
035700150603       //?Controllo dati in videata D01.                               ?
035800150603       //--------------------------------------------------------------
035900150603       BEGSR  sr_CtrD01;
036000150603
036100150603         // -?Spegnimento degli indicatori di errore?
036200150603         %subst(IndDspF : 50) = *off;
036300150603
036400150603         // -?Controllo Profilo dell'utente?
036500150603         clear  V1Dute;
036600150603         V1Cute = %trimL ( V1Cute );
036700150603         select;
036800150603           when  V1Cute = *blank;
036900150603             V1Dute = 'Tutti i profili ';
037000150604           when  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
037100150603             V1Dute = 'Tutti i profili ' +
037200150604                      %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 ) +
037300150603                      '...';
037400150603           other;
037500150603             chain  ( V1Cute )  KPPRF000;
037600150603             if  %found(KPPRF01L);
037700150603               V1Dute = PRFtxt;
037800150603             else;
037900150603               V1Dute = *all'? ';
038000150604               $errGenerico = *on;
038100150604               $errMessage  = *on;
038200150604               $PosCurUTE   = *on;
038300150604               V1Dmsg = sk_Msg(01);
038400150603               leavesr;
038500150603             endif;
038600150603         endsl;
038700150603
038800150603         clear  dUte01;
038900150603         if  V1Cute <> *blank                                          and
039000150604             %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) <> '*'  and
039100150603             %found(KPPRF01L);
039200150604           chain(N)  ( V1Cute )  AZUTE000;
039300150604           if  %found(AZUTE01L);
039400150603             dUte01 = UTEfaf;
039500150603           endif;
039600150603         endif;
039700150603
039800150603         // -?Controllo Filiale dell'utente?
039900150603         clear  V1Dfil;
040000150603         if  V1Cfil = *zero;
040100150604           //V1Dfil = 'Tutte le Filiali';
040200150604           V1Dfil = *all'? ';
040300150604           $errGenerico = *on;
040400150604           $errMessage  = *on;
040500150604           $PosCurFIL   = *on;
040600150604           V1Dmsg = sk_Msg(02);
040700150604           leavesr;
040800150603         else;
040900150603           chain  ( V1Cfil )  AZORG;
041000150603           if  %found(AZORG01L);
041100150603             V1Dfil = ORGdes;
041200150603           else;
041300150603             V1Dfil = *all'? ';
041400150604             $errGenerico = *on;
041500150604             $errMessage  = *on;
041600150604             $PosCurFIL   = *on;
041700150604             V1Dmsg = sk_Msg(03);
041800150603             leavesr;
041900150603           endif;
042000150603         endif;
042100150603
042200150603         // -?Controllo IP terminale dell'utente?
042300150603         clear  V1Dipt;
042400150603         if  V1Cipt <> *blank;
042500150603           reset  TIBS02ds;
042600150603           T02ke1 = V1Cipt;
042700150603           TNTBE_RicercaControllo ( kpjba : tibs02ds );
042800150603           if  T02err <> *blank;
042900150603             V1Dipt = *all'? ';
043000150604             $errGenerico = *on;
043100150604             $errMessage  = *on;
043200150604             $PosCurIPT   = *on;
043300150604             V1Dmsg = sk_Msg(04);
043400150603             leavesr;
043500150603           endif;
043600150603           dLAT   = T02uni;
043700150603           V1Dipt = dLAT.§LATdes;
043800150603         endif;
043900150603
044000150603         // -?Controllo Nome Libreria del *pgm iniziale?
044100150603         if  V1Clib = *blank;
044200150604           $errGenerico = *on;
044300150604           $errMessage  = *on;
044400150604           $PosCurLIB   = *on;
044500150604           V1Dmsg = sk_Msg(05);
044600150603           leavesr;
044700150603         endif;
044800150603
044900150603         V1Clib = %trimL ( V1Clib );
045000150603
045100150603         Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + c_CmdCHKOBJlib;
045200150603         exsr  sr_ExecCmd;
045300150603         if  Qusei <> *blank;
045400150604           $errGenerico = *on;
045500150604           $errMessage  = *on;
045600150604           $PosCurLIB   = *on;
045700150604           V1Dmsg = sk_Msg(06);
045800150603           leavesr;
045900150603         endif;
046000150603
046100150603         Qcmd = c_CmdCHKOBJ + %trimR( V1Clib ) + '/' + c_Pgm +
046200150603                c_CmdCHKOBJpgm;
046300150603         exsr  sr_ExecCmd;
046400150603         if  Qusei <> *blank;
046500150604           $errGenerico = *on;
046600150604           $errMessage  = *on;
046700150604           $PosCurLIB   = *on;
046800150604           V1Dmsg = sk_Msg(07);
046900150603           leavesr;
047000150603         endif;
047100150603
047200150603       ENDSR;
047300150603
047400150603       //--------------------------------------------------------------
047500150603       //?Modifica dei profili utente selezionati.                     ?
047600150603       //--------------------------------------------------------------
047700150603       BEGSR  sr_ChgUsrPrf_All;
047800150603
047900150603         Select;
048000150603
048100150603           // -?Elaborazione di Tutti i Profili?
048200150603           When  V1Cute = *blank;
048300150603             setll  ( *loval )  KPPRF000;
048400150603             read  KPPRF000;
048500150603             DoW  Not %eof(KPPRF01L);
048600150604               exsr  sr_ChgUsrPrf_Sing;
048700150603               read  KPPRF000;
048800150603             EndDo;
048900150603
049000150603           // -?Elaborazione dei Profili selezionati?
049100150604           When  %subst ( V1Cute : %len( %trimR( V1Cute ) ) : 1 ) = '*';
049200150603             clear  keyKPPRF01;
049300150604             k_PRFnmu = %subst ( V1Cute : 1 : %len( %trimR( V1Cute ) ) - 1 );
049400150603             setll  %kds( keyKPPRF01 )  KPPRF000;
049500150603             read   KPPRF000;
049600150603             DoW  Not %eof(KPPRF01L)  and
049700150604                  %subst ( PRFnmu : 1 : %len( %trimR( k_PRFnmu ) ) ) = k_PRFnmu;
049800150604               exsr  sr_ChgUsrPrf_Sing;
049900150604               if  $ErrGenerico;
050000150604                 leavesr;
050100150604               endif;
050200150603               read  KPPRF000;
050300150603             EndDo;
050400150603
050500150603           // -?Elaborazione del singolo Profilo?
050600150603           Other;
050700150603             chain  ( V1Cute )  KPPRF000;
050800150603             if  %found(KPPRF01L);
050900150604               exsr  sr_ChgUsrPrf_Sing;
051000150604               if  $ErrGenerico;
051100150604                 leavesr;
051200150604               endif;
051300150603             endif;
051400150604
051500150604         EndSl;
051600150603
051700150603       ENDSR;
051800150603
051900150603       //--------------------------------------------------------------
052000150603       //?Modifica del singolo Profilo Utente.                         ?
052100150603       //--------------------------------------------------------------
052200150603       BEGSR  sr_ChgUsrPrf_Sing;
052300150604
052400150604         // -?Utente NON di Filiale => uscita?
052500150604         if  PRFnsi <> 'FILTRA201 ';
052600150604           leavesr;
052700150604         endif;
052800150604
052900150604         // -?Utente NON esistente su AS/400 => uscita?
053000150604         Qcmd = c_CmdCHKOBJ + %trimR( PRFnmu ) + c_CmdCHKOBJprf;
053100150604         exsr  sr_ExecCmd;
053200150604         if  Qusei <> *blank;
053300150604           leavesr;
053400150604         endif;
053500150603
053600150604         // -?Utente NON "valido" in AZUTE00F => uscita?
053700150604         chain  ( PRFnmu )  AZUTE000;
053800150604         if  NOT %found(AZUTE01L)  or  UTEdsc < wDate
053900150604                                   or  UTEfil <> V1Cfil;
054000150604           unlock  AZUTE01L;
054100150604           leavesr;
054200150604         endif;
054300150604
054400150604         // -?Aggiornamento IP Terminale?
054500150604         dUte01 = UTEfaf;
054600150604         if  dUte01.§UTEipt = V1Cipt;
054700150604           unlock  AZUTE01L;
054800150604         else;
054900150604           dUte01.§UTEipt = V1Cipt;
055000150604           UTEfaf = dUte01;
055100150604           update  AZUTE000;
055200150604         endif;
055300150604
055400150604         // -?Aggiornamento Programma Iniziale da Richiamare?
055500150604         Qcmd = 'CHGUSRPRF +
055600150604                    usrprf(' + %trimR( PRFnmu ) + ') +
055700150604                    inlpgm(' + %trimR( V1Clib ) + '/' + c_Pgm +')';
055800150604         exsr  sr_ExecCmd;
055900150604         if  Qusei <> *blank;
056000150604           $errGenerico = *on;
056100150604           $errMessage  = *on;
056200150604           $PosCurLIB   = *on;
056300150604           V1Dmsg = %trimR ( sk_Msg(08) ) + ' ' + PRFnmu;
056400150604           leavesr;
056500150604         endif;
056600150603
056700150603       ENDSR;
056800150603
056900150603       //--------------------------------------------------------------
057000150603       //?Esecuzione del comando (già impostato).                      ?
057100150603       //--------------------------------------------------------------
057200150603       BEGSR  sr_ExecCmd;
057300150603
057400150603         clear Qcap0100;
057500150603         Qcabcsdh = *off;
057600150603         Qcapa    = *off;
057700150603         Qcacmdss = *off;
057800150603         Qcaerved = *allX'00';
057900150603
058000150603         clear Qusec;
058100150603         Qusbprv  = %size(Qusec);
058200150603
058300150603         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
058400150603                           %size(Qcap0100) : 'CPOP0100' : *omit :
058500150603                           0 : 0 : Qusec);
058600150603
058700150603         //if  Qusei <> *blank;
058800150603         //  ...;
058900150603         //endif;
059000150603
059100150603       ENDSR;
059200150604
059300150604       //--------------------------------------------------------------
059400150604       //?Operazioni finali.                                           ?
059500150604       //--------------------------------------------------------------
059600150604       BEGSR  sr_RoutEnd;
059700150604
059800150604         // -?Chiusura applicazioni precedentemente aperte?
059900150604         clear  TIBS02ds;
060000150604         T02tla = 'C';
060100150604         TNTBE_RicercaControllo ( kpjba : tibs02ds );
060200150604
060300150604         // -?Chiusura pgm?
060400150604         return;
060500150604
060600150604       ENDSR;
060700150603
060800150603      /end-free
060900150604
061000150604       //--------------------------------------------------------------
061100150604       //?Definizione schiere a tempo di compilazione                  ?
061200150604       //--------------------------------------------------------------
061300150604
061400150604** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
061500150604Utente errato                                                                   1
061600150604Filiale obbligatoria                                                            2
061700150604Filiale errata                                                                  3
061800150604IP Terminale errato                                                             4
061900150604Nome Libreria obbligatoria                                                      5
062000150604Libreria inesistente                                                            6
062100150604Programma inesistente in questa libreria                                        7
062200150604Aggiornamento NON riuscito per l'utente                                         8
