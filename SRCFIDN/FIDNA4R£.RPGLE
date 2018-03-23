000100121022       //==============================================================
000200121022       //?FIDNA4R - Gestione Note RA + Note LDV                        ?
000300121022       //==============================================================
000400121022
000500121022       //--------------------------------------------------------------
000600121022       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121022       //--------------------------------------------------------------
000800121022
000900121022     /*PRM  dbgview(*source)
001000121022     /*CMD  ovrdbf file(FIARN11L) tofile(FILTRAPRD/FIARN11L) +
001100121022     /*CMD         ovrscope(*calllvl)
001200121023     /*END  dltovr file(FIARN11L) lvl(*)
001300121022     /*END
001400121022
001500121022       //--------------------------------------------------------------
001600121022       //?Specifiche di controllo.                                     ?
001700121022       //--------------------------------------------------------------
001800121022
001900121022     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002000121022     h dftactgrp(*no)
002100121022     h bnddir('UBRTVNETA')
002200121022
002300121022       //--------------------------------------------------------------
002400121022       //?Dichiarazione file.                                          ?
002500121022       //--------------------------------------------------------------
002600121022
002700121022       // -?Bolle Arrivi: Note?
002800121022     fFIARN11L  Uf A e           k disk    extfile(wLibFile)
002900121022     f                                     usropn
003000121022
003100121022       // -?Video?
003200121022     fFIDNA4D   cf   e             workstn
003300121022     f                                     indds(IndDspF)
003400121022     f                                     infds(InfDspF)
003500121022
003600121022       //--------------------------------------------------------------
003700121022       //?Definizione costanti.                                        ?
003800121022       //--------------------------------------------------------------
003900121022
004000121022       // -?Codice Nota?
004100121022     d c_CDN           c                   const('LDV')
004200121022
004300121022       // -?Tasti funzionali a video?
004400121022     d c_F01           c                   const(x'31')
004500121022     d c_F02           c                   const(x'32')
004600121022     d c_F03           c                   const(x'33')
004700121022     d c_F04           c                   const(x'34')
004800121022     d c_F05           c                   const(x'35')
004900121022     d c_F06           c                   const(x'36')
005000121022     d c_F07           c                   const(x'37')
005100121022     d c_F08           c                   const(x'38')
005200121022     d c_F09           c                   const(x'39')
005300121022     d c_F10           c                   const(x'3A')
005400121022     d c_F11           c                   const(x'3B')
005500121022     d c_F12           c                   const(x'3C')
005600121022     d c_F13           c                   const(x'B1')
005700121022     d c_F14           c                   const(x'B2')
005800121022     d c_F15           c                   const(x'B3')
005900121022     d c_F16           c                   const(x'B4')
006000121022     d c_F17           c                   const(x'B5')
006100121022     d c_F18           c                   const(x'B6')
006200121022     d c_F19           c                   const(x'B7')
006300121022     d c_F20           c                   const(x'B8')
006400121022     d c_F21           c                   const(x'B9')
006500121022     d c_F22           c                   const(x'BA')
006600121022     d c_F23           c                   const(x'BB')
006700121022     d c_F24           c                   const(x'BC')
006800121022     d c_Enter         c                   const(x'F1')
006900121022     d c_RollDown      c                   const(x'F4')
007000121022     d c_RollUp        c                   const(x'F5')
007100121022
007200121022       //--------------------------------------------------------------
007300121022       //?Definizione schiere.                                         ?
007400121022       //--------------------------------------------------------------
007500121022
007600121022       // -?Messaggi di errore?
007700121022     d $Msg            s             78    dim( 4) ctdata perrcd(1)
007800121022
007900121022       //--------------------------------------------------------------
008000121022       //?Definizione aree dati.                                       ?
008100121022       //--------------------------------------------------------------
008200121022
008300121022       // -?Dati utente?
008400121022     d §AzUte        e ds                  extname(AZUTE00F)
008500121022     d                                     dtaara
008600121022     d §DatiUte      e ds                  extname(dDatiUte)
008700121022     d                                     dtaara
008800121022
008900121022       //--------------------------------------------------------------
009000121022       //?Definizione strutture dati.                                  ?
009100121022       //--------------------------------------------------------------
009200121022
009300121022       // -?Status ds?
009400121022     d Status         sds
009500121022     d  SDSpgm           *proc
009600121022     d  SDSusr               254    263
009700121022
009800121022       // -?InfDS?
009900121022     d InfDspF         ds
010000121022     d   dsp_aid             369    369a
010100121022
010200121022       // -?Indicatori su DspF?
010300121022     d IndDspF         ds                  inz
010400121022         // -?Abilitazione tasti funzionali?
010500121022     d   F2Attivo                      n   overlay(IndDspF : 02)
010600121022         // -?Emissione messaggio di errore?
010700121022     d   ErrMessage                    n   overlay(IndDspF : 28)
010800121022         // -?Indicatori per Attibuti di visualizzazione?
010900121022     d   VisNoteTxt                    n   overlay(IndDspF : 40)
011000121022     d   GesNoteLdV                    n   overlay(IndDspF : 42)
011100121022         // -?Posizionamento cursore & segnalazione errore?
011200121022     d   PosCurNOR                     n   overlay(IndDspF : 51)
011300121022     d   PosCurNOB                     n   overlay(IndDspF : 52)
011400121022         // -?Riemissione videata?
011500121022     d   ErrGenerico                   n   overlay(IndDspF : 99)
011600121022
011700121022       // -?Numero Spedizione ricevuto?
011800121022     d ds_iA4ogg       ds                  inz   qualified
011900121022     d   iA4lnp                       3s 0 inz
012000121022     d   iA4nrs                       2s 0 inz
012100121022     d   iA4nsp                       7s 0 inz
012200121022     d   iA4aas                       4s 0 inz
012300121022
012400121022       // -?Note R.A. a video?
012500121022     d ds_VIDnor       ds                  inz
012600121022     d   VIDnor1                           inz
012700121022     d   VIDnor2                           inz
012800121022     d   VIDnor3                           inz
012900121022     d   VIDnor4                           inz
013000121022     d     $NOR                1    280    inz   dim(4)
013100121022
013200121022       // -?Note LDV a video?
013300121022     d ds_VIDnob       ds                  inz
013400121022     d   VIDnob1                           inz
013500121022     d   VIDnob2                           inz
013600121022     d   VIDnob3                           inz
013700121022     d   VIDnob4                           inz
013800121022     d     $NOB                1    280    inz   dim(4)
013900121022
014000121022       // -?Parametri ricevuti?
014100121022     d KPJBA         e ds
014200121022     d FIDNA4ds      e ds                  inz
014300121022
014400121022       //--------------------------------------------------------------
014500121022       //?Definizione variabili globali.                               ?
014600121022       //--------------------------------------------------------------
014700121022
014800121022       // -?Flags booleani?
014900121022     d $Fine           s               n   inz
015000121022     d $InzD01         s               n   inz(*on)
015100121022     d*// $ErrNoNOB       s               n   inz
015200121022     d*// $ErrNoNOR       s               n   inz
015300121022
015400121022       // -?Indici di schiera / Contatori?
015500121022     d xx              s              3  0 inz
015600121022
015700121022       // -?Variabili per la gestione del video?
015800121022     d $Video          s              2    inz('D1')
015900121022
016000121022       // -?Nome esteso Libreria/File di FIARN11L (s.i. di filiale)?
016100121022     d wLibFile        s             21a   inz
016200121022
016300121022       // -?Campi di comodo?
016400121022     d SaveARNpno      s                   like(ARNpno)   inz
016500121022     d wTime14a        s             14    inz
016600121022     d wTime           s              6  0 inz
016700121022     d wDate           s              8  0 inz
016800121022     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016900121022
017000121022       //--------------------------------------------------------------
017100121022       //?Definizione prototipi procedure.                             ?
017200121022       //--------------------------------------------------------------
017300121022
017400121022       // -?Nome del sistema?
017500121022     d currSysNeta     s              8a   inz
017600121022       // -?Reperimento NETA sistema AS/400 corrente?
017700121022      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017800121022
017900121022       // -?Reperimento dati utente?
018000121022     d TIBS34ds      e ds
018100121022      /copy gaitrasrc/srcProtoPR,TIBS34R
018200121022
018300121022       // -?Scrittura Richiesta Assistenza?
018400121022     d FIDNA3ds      e ds                  inz
018500121022     d fidnA3r         pr                  extpgm('FIDNA3R')
018600121022     d   kpjba                             likeds(KPJBA)
018700121022     d   fidnA3ds                          likeds(FIDNA3ds)
018800121022
018900121022       //--------------------------------------------------------------
019000121022       //?Definizione key-list.                                        ?
019100121022       //--------------------------------------------------------------
019200121022
019300121022       // -?File FIARN11L?
019400121022     d k07fiarn11    e ds                  extname(FIARN11L : *key)
019500121022     d                                     prefix(k_)   inz
019600121022
019700121022       //--------------------------------------------------------------
019800121022       //?M A I N - L I N E                                            ?
019900121022       //--------------------------------------------------------------
020000121022
020100121022     c     *Entry        plist
020200121022     c                   parm                    KPJBA
020300121022
020400121022      /free
020500121022
020600121022       // -?Operazioni iniziali?
020700121022       exsr  sr_RoutInz;
020800121022
020900121022       // -?Ciclo di gestione del file video?
021000121022       DoW  $Fine = *off;
021100121022
021200121022         select;
021300121022
021400121022           // -?Gestione videata D01?
021500121022           when $Video = 'D1';
021600121022             exsr  sr_GesD01;
021700121022
021800121022           // -?? ? ??
021900121022           other;
022000121022             $Fine = *on;
022100121022
022200121022         endsl;
022300121022
022400121022       EndDo;
022500121022
022600121022       // -?Operazioni finali?
022700121022       exsr  sr_RoutEnd;
022800121022
022900121022       //--------------------------------------------------------------
023000121022       //?Operazioni iniziali.                                         ?
023100121022       //--------------------------------------------------------------
023200121022       BEGSR  sr_RoutInz;
023300121022
023400121022         *inLR = *on;
023500121022
023600121022         // -?Verifica del sistema AS/400 corrente?
023700121022         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
023800121022           $Fine = *on;
023900121022           leavesr;
024000121022         endif;
024100121022
024200121022         // -?Apertura file FIARN01L?
024300121022         if  %subst( currSysNetA : 1 : 6 ) = 'SETRAS';
024400121022           wLibFile  = 'FILTRA201/FIARN11L';
024500121022         else;
024600121022           wLibFile  = 'FILTRAPRD/FIARN11L';
024700121022         endif;
024800121022         open  FIARN11L;
024900121022
025000121022         // -?Reperimento dati job?
025100121022         exsr  sr_DatiJob;
025200121022
025300121022         // -?Impostazione parametri ricevuti?
025400121022         FIDNA4ds = kpjbu;
025500121022
025600121022         // -?Impostazione nome programma a video?
025700121022         V1Tpgm = SDSpgm;
025800121022
025900121022         // -?Reperimento data odierna?
026000121022         wDate = %int( %subst( %char( %dec( %timestamp() ) )
026100121022                               : 1 : 8 ) );
026200121022
026300121022       ENDSR;
026400121022
026500121022       //--------------------------------------------------------------
026600121022       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700121022       //--------------------------------------------------------------
026800121022       BEGSR  sr_DatiJob;
026900121022
027000121022         in(E) §AzUte;
027100121022         if NOT %error;
027200121022           in(E) §DatiUte;
027300121022         endif;
027400121022         if %error or RSut = *blanks;
027500121022           clear TIBS34ds;
027600121022           tibs34r ( tibs34ds );
027700121022           in §AzUte;
027800121022           in §DatiUte;
027900121022         endif;
028000121022
028100121022       ENDSR;
028200121022
028300121022       //--------------------------------------------------------------
028400121022       //?Gestione videata D01.                                        ?
028500121022       //--------------------------------------------------------------
028600121022       BEGSR  sr_GesD01;
028700121022
028800121022         // -?Inizializzazione videata?
028900121022         if  $InzD01 = *on;
029000121022           exsr  sr_InzD01;
029100121022           $InzD01 = *off;
029200121022         endif;
029300121022
029400121022         // -?Emissione testata?
029500121022         write  DNA4T01;
029600121022
029700121022         // -?Emissione videata?
029800121022         exfmt  DNA4D01;
029900121022
030000121022         clear  VIDmsg;
030100121022         reset  ErrMessage;
030200121022         reset  ErrGenerico;
030300121022
030400121022         SELECT;
030500121022
030600121022           // -?F2=Copia Note R.A. in Note LdV?
030700121022           WHEN  dsp_aid = c_F02;
030800121022             exsr  sr_CopyNote;
030900121022
031000121022           // -?Invio / F6=Conferma?
031100121022           OTHER;
031200121022             exsr  sr_CtrD01;
031300121022             if  ErrGenerico = *off  and  dsp_aid = c_F06;
031400121022               exsr  sr_F06D01;
031500121022             endif;
031600121022
031700121022         ENDSL;
031800121022
031900121022       ENDSR;
032000121022
032100121022       //--------------------------------------------------------------
032200121022       //?Inizializzazione videata D01.                                ?
032300121022       //--------------------------------------------------------------
032400121022       BEGSR  sr_InzD01;
032500121022
032600121022         clear  DNA4D01;
032700121022
032800121022         ds_iA4ogg = iA4ogg;
032900121022         VIDlnp  = ds_iA4ogg.iA4lnp;
033000121022         VIDnrs  = ds_iA4ogg.iA4nrs;
033100121022         VIDnsp  = ds_iA4ogg.iA4nsp;
033200121022         VIDaas  = ds_iA4ogg.iA4aas;
033300121022         VIDno2a = %subst( iA4no2 : 1 : %len(VIDno2a) );
033400121022         VIDno2b = %subst( iA4no2 : %len(VIDno2a)+1 );
033500121022
033600121022         // -?Impostazione indicatori per attributi di visualizzazione?
033700121022         VisNoteTxt = (iA4no2 <> *blank);
033800121022         GesNoteLdV = (iA4mod  > '1');
033900121022
034000121022         // -?Impostazione indicatori per abilitazione tasti funzionali?
034100121022         F2Attivo  = (iA4mod   > '1');
034200121022
034300121022       ENDSR;
034400121022
034500121022       //--------------------------------------------------------------
034600121022       //?F2 = Copia Note R.A. in Note LDV.                            ?
034700121022       //--------------------------------------------------------------
034800121022       BEGSR  sr_CopyNote;
034900121022
035000121022         // -?Premuto F2 con nessuna nota R.A. da copiare?
035100121022         if  ds_VIDnor = *blank;
035200121022           ErrGenerico = *on;
035300121022           ErrMessage  = *on;
035400121022           PosCurNOB   = *on;
035500121022           VIDmsg = $Msg(01);
035600121022           leavesr;
035700121022         endif;
035800121022
035900121022         // -?Copia delle Note R.A. nelle Note LdV?
036000121022         //For  xx = 1  To  %elem($NOR);
036100121022         //  if  $NOR(xx) <> *blank;
036200121022         //    For  yy = 1  To  %elem($NOB);
036300121022         //      if  $NOB(yy) = *blank;
036400121022         //        $NOB(yy) = $NOR(xx);
036500121022         //      endif;
036600121022         //    EndFor;
036700121022         //  endif;
036800121022         //EndFor;
036900121022         // -?Verifica se c'è posto in aggiunta alle note già a video:?
037000121022         Select;
037100121022           // ·?Tutte occupate: errore?
037200121022           When  $NOB(4) <> *blank;
037300121022             ErrGenerico = *on;
037400121022             ErrMessage  = *on;
037500121022             PosCurNOB   = *on;
037600121022             VIDmsg = $Msg(02);
037700121022             leavesr;
037800121022           // ·?Se la prima nota è vuota: si copiano tutte e 4?
037900121022           When  VIDnob1 = *blank;
038000121022             ds_VIDnob = ds_VIDnor;
038100121022           // ·?Se la seconda nota è vuota: si copiano solo sulle ultime 3?
038200121022           When  VIDnob2 = *blank;
038300121022             VIDnob2 = VIDnor1;
038400121022             VIDnob3 = VIDnor2;
038500121022             VIDnob4 = VIDnor3;
038600121022           // ·?Se la terza   nota è vuota: si copiano solo sulle ultime 2?
038700121022           When  VIDnob3 = *blank;
038800121022             VIDnob3 = VIDnor1;
038900121022             VIDnob4 = VIDnor2;
039000121022           // ·?Se la quarta  nota è vuota: si copia solo sull'ultima?
039100121022           When  VIDnob4 = *blank;
039200121022             VIDnob4 = VIDnor1;
039300121022         EndSl;
039400121022
039500121022       ENDSR;
039600121022
039700121022       //--------------------------------------------------------------
039800121022       //?Controllo dati immessi nella videata D01.                    ?
039900121022       //--------------------------------------------------------------
040000121022       BEGSR  sr_CtrD01;
040100121022
040200121022         // -?Spegnimento indicatori di posizionamento cursore?
040300121022         %subst(IndDspF : 50) = *off;
040400121022
040500121022         // -?Premuto F6 con nessuna nota R.A. (meno di 5 char)?
040600121022         select;
040700121022           when  ds_VIDnor = *blank;
040800121022             ErrGenerico = *on;
040900121022             ErrMessage  = *on;
041000121022             PosCurNOR   = *on;
041100121022             VIDmsg = $Msg(03);
041200121022             leavesr;
041300121022           //when  %len( %trim(VIDnor1) ) + %len( %trim(VIDnor2) ) +
041400121022           //      %len( %trim(VIDnor3) ) + %len( %trim(VIDnor4) ) < 5;
041500121022           when  %len( %trim(VIDnor1) ) < 5  and
041600121022                 %len( %trim(VIDnor2) ) < 5  and
041700121022                 %len( %trim(VIDnor3) ) < 5  and
041800121022                 %len( %trim(VIDnor4) ) < 5;
041900121022             ErrGenerico = *on;
042000121022             ErrMessage  = *on;
042100121022             PosCurNOR   = *on;
042200121022             VIDmsg = %trimr( $Msg(03) ) + ' "significativa"';
042300121022             leavesr;
042400121022         endsl;
042500121022
042600121022         // -?FINE dei CONTROLLI per SOLO note R.A.?
042700121022         if  iA4mod <= '1';
042800121022           leavesr;
042900121022         endif;
043000121022
043100121022         // -?Premuto F6 con nessuna nota LDV - NON obbligatorie?
043200121022         //  ?(messaggio di avvertimento)?
043300121022         //if  ds_VIDnob = *blank  and  dsp_aid = c_F06  and
043400121022         //    Not $ErrNoNOB;
043500121022         //  $ErrNoNOB   = *on;
043600121022         //  ErrGenerico = *on;
043700121022         //  ErrMessage  = *on;
043800121022         //  PosCurNOB   = *on;
043900121022         //  VIDmsg = $Msg(04);
044000121022         //  leavesr;
044100121022         //endif;
044200121022
044300121022       ENDSR;
044400121022
044500121022       //--------------------------------------------------------------
044600121022       //?Controllo dati immessi nella videata D01.                    ?
044700121022       //--------------------------------------------------------------
044800121022       BEGSR  sr_F06D01;
044900121022
045000121022         // -?Richiamo *pgm FIDNA3R = Scrittura Richieste Assistenza?
045100121022         //  ?(vedi file FITGD00F e FITGN00F/"GEN")?
045200121022         clear  FIDNA3ds;
045300121022         iA3mad = iA4mad;
045400121022         iA3tor = iA4tor;
045500121022         iA3ogg = iA4ogg;
045600121022         iA3no1 = ds_VIDnor;
045700121022         iA3no2 = iA4no2;
045800121022         iA3ara = *blank;
045900121022
046000121022         fidnA3r ( kpjba : FIDNA3ds );
046100121022
046200121022
046300121022         // -?Cancellazione delle eventuali note LDV stampabili?
046400121022         //  ?inserite in precedenza?(vedi file FIARN00F/"LDV")?
046500121022         clear  SaveARNpno;
046600121022         k_ARNaas = ds_iA4ogg.iA4aas;
046700121022         k_ARNlnp = ds_iA4ogg.iA4lnp;
046800121022         k_ARNnrs = ds_iA4ogg.iA4nrs;
046900121022         k_ARNnsp = ds_iA4ogg.iA4nsp;
047000121022         k_ARNlnp = ds_iA4ogg.iA4lnp;
047100121022         k_ARNcdn = c_CDN;
047200121022         k_ARNgst = 'S';
047300121022         setll  %kds( k07fiarn11 : 6 )  FIARN000;
047400121022         reade  %kds( k07fiarn11 : 6 )  FIARN000;
047500121022         DoW  Not %eof(FIARN11L);
047600121022           SaveARNpno = ARNpno;
047700121022           clear  ARNgst;
047800121022           //_______________
047900121022           update  FIARN000;
048000121022           //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
048100121022           reade  %kds( k07fiarn11 : 6 )  FIARN000;
048200121022         EndDo;
048300121022
048400121022         // -?Recupero Data & Ora attuali?
048500121022         wTime14a = %subst( %char( %dec( %timestamp() ) ) : 1 : 14 );
048600121022         wDate = %int( %subst( wTime14a : 1 : 8 ) );
048700121022         wTime = %int( %subst( wTime14a : 9 : 6 ) );
048800121022
048900121022         SELECT;
049000121022
049100121022           // -?Nessuna nota inserita  &  Nessuna nota già reperita?
049200121022           //  ?NON registra nessuna nota?
049300121022           WHEN  ds_VIDnob = *blank  and  SaveARNpno = *zero;
049400121022
049500121022           // -?Nessuna nota inserita  &  Note già reperite?
049600121022           //  ?registra una nota vuota?
049700121022           WHEN  ds_VIDnob = *blank  and  SaveARNpno > *zero;
049800121022             xx = 1;
049900121022             exsr  sr_WrtFIARN;
050000121022
050100121022           // -?Scrittura delle nuove Note immesse a video?
050200121022           //  ?(vedi file FIARN00F/"LDV")?
050300121022           OTHER;
050400121022             For  xx = 1  To  %elem($NOB);
050500121022               If  $NOB(xx) <> *blank;
050600121022                 exsr  sr_WrtFIARN;
050700121022               EndIf;
050800121022             EndFor;
050900121022
051000121022         ENDSL;
051100121022
051200121022         feod  FIARN11L;
051300121022
051400121022         // -?Uscita dal *pgm?
051500121022         $Fine = *on;
051600121022
051700121022       ENDSR;
051800121022
051900121022       //--------------------------------------------------------------
052000121022       //?Impostazione campi & scrittura record del file FIARN00F.     ?
052100121022       //--------------------------------------------------------------
052200121022       BEGSR  sr_WrtFIARN;
052300121022
052400121022         clear  FIARN000;
052500121022
052600121022         ARNaas = ds_iA4ogg.iA4aas;
052700121022         ARNlnp = ds_iA4ogg.iA4lnp;
052800121022         ARNnrs = ds_iA4ogg.iA4nrs;
052900121022         ARNnsp = ds_iA4ogg.iA4nsp;
053000121022         ARNcdn = c_CDN;
053100121022         SaveARNpno += 1;
053200121022         ARNpno = SaveARNpno;
053300121022         ARNnob = $NOB(xx);
053400121022         ARNgst = 'S';
053500121022         ARNdim = wDate;
053600121022         ARNoim = wTime;
053700121022         ARNpru = DUTute;
053800121022         if  iA4pgm <> *blank;
053900121022           ARNpgm = iA4pgm;
054000121022         else;
054100121022           ARNpgm = SDSpgm;
054200121022         endif;
054300121022
054400121022         //______________
054500121022         write  FIARN000;
054600121022         //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
054700121022
054800121022       ENDSR;
054900121022
055000121022       //--------------------------------------------------------------
055100121022       //?Operazioni finali.                                           ?
055200121022       //--------------------------------------------------------------
055300121022       BEGSR  sr_RoutEnd;
055400121022
055500121022         // -?Restituzione parametri?
055600121022         kpjbu = FIDNA4ds;
055700121022
055800121022         // -?Chiusura pgm?
055900121022         return;
056000121022
056100121022       ENDSR;
056200121022
056300121022      /end-free
056400121022
056500121022       //--------------------------------------------------------------
056600121022       //?Definizione schiere a tempo di compilazione                  ?
056700121022       //--------------------------------------------------------------
056800121022
056900121022** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
057000121026Inserire le note R.A. (da copiare) PRIMA di premere F2                          1
057100121022Già inserite tutte le note LDV: nessuna di quelle R.A. risulta copiabile        2
057200121022Inserire almeno una nota R.A.                                                   3
057300121022AVVISO: nessuna nota LDV risulta inserita                                       4 - LIBERO
