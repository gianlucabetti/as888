000100141209       //==============================================================
000200141209       //?Richiesta e Controllo del "PINcode".                         ?
000300141209       //==============================================================
000400141209
000500141209       //--------------------------------------------------------------
000600141209       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141209       //--------------------------------------------------------------
000800141209
000900141209     /*PRM  dbgview(*source)
001000141209     /*END
001100141209
001200141209       //--------------------------------------------------------------
001300141209       //?Specifiche di controllo.                                     ?
001400141209       //--------------------------------------------------------------
001500141209
001600141209     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141210     h dftactgrp(*no) actgrp(*caller)
001800141209
001900141209       //--------------------------------------------------------------
002000141209       //?Dichiarazione file.                                          ?
002100141209       //--------------------------------------------------------------
002200141209
002300141209       // -?Bolle: Estensione Descrizioni?
002400141209     fFIAR401L  if   e           k disk    usropn
002500141209
002600141209       // -?Video?
002700141209     fTRULPIND  cf   e             workstn
002800141209     f                                     indds(IndDspF)
002900141209     f                                     infds(InfDspF)
003000141209
003100141209       //--------------------------------------------------------------
003200141209       //?Definizione costanti.                                        ?
003300141209       //--------------------------------------------------------------
003400141209
003500141209       // -?Numero Max di tentativi per inserimento PINcode corretto?
003600141209     d c_MaxErr        c                   const(5)
003700141209
003800141209       // -?Tasti funzionali a video?
003900141209     d c_F01           c                   const(x'31')
004000141209     d c_F02           c                   const(x'32')
004100141209     d c_F03           c                   const(x'33')
004200141209     d c_F04           c                   const(x'34')
004300141209     d c_F05           c                   const(x'35')
004400141209     d c_F06           c                   const(x'36')
004500141209     d c_F07           c                   const(x'37')
004600141209     d c_F08           c                   const(x'38')
004700141209     d c_F09           c                   const(x'39')
004800141209     d c_F10           c                   const(x'3A')
004900141209     d c_F11           c                   const(x'3B')
005000141209     d c_F12           c                   const(x'3C')
005100141209     d c_F13           c                   const(x'B1')
005200141209     d c_F14           c                   const(x'B2')
005300141209     d c_F15           c                   const(x'B3')
005400141209     d c_F16           c                   const(x'B4')
005500141209     d c_F17           c                   const(x'B5')
005600141209     d c_F18           c                   const(x'B6')
005700141209     d c_F19           c                   const(x'B7')
005800141209     d c_F20           c                   const(x'B8')
005900141209     d c_F21           c                   const(x'B9')
006000141209     d c_F22           c                   const(x'BA')
006100141209     d c_F23           c                   const(x'BB')
006200141209     d c_F24           c                   const(x'BC')
006300141209     d c_Enter         c                   const(x'F1')
006400141209     d c_RollDown      c                   const(x'F4')
006500141209     d c_RollUp        c                   const(x'F5')
006600141209
006700141209       //--------------------------------------------------------------
006800141209       //?Definizione schiere.                                         ?
006900141209       //--------------------------------------------------------------
007000141209
007100141209
007200141209       //--------------------------------------------------------------
007300141209       //?Definizione aree dati.                                       ?
007400141209       //--------------------------------------------------------------
007500141209
007600141209       // -?Dati utente?
007700141209     d §AzUte        e ds                  extname(AZUTE00F)
007800141209     d                                     dtaara
007900141209     d §DatiUte      e ds                  extname(dDatiUte)
008000141209     d                                     dtaara
008100141209
008200141209       //--------------------------------------------------------------
008300141209       //?Definizione strutture dati.                                  ?
008400141209       //--------------------------------------------------------------
008500141209
008600141209       // -?Status ds?
008700141209     d Status         sds
008800141209     d   SDSpgm          *proc
008900141209
009000141209       // -?InfDS?
009100141209     d InfDspF         ds
009200141209     d   dsp_aid             369    369a
009300141209
009400141209       // -?Indicatori su DspF?
009500141209     d IndDspF         ds                  inz
009600141210         // -?(Dis)Abilitazione tasti funzionali?
009700141210     d   F6Attivo                      n   overlay(IndDspF : 06)
009800141209         // -?Emissione messaggio di errore?
009900141209     d   ErrMessage                    n   overlay(IndDspF : 28)
010000141209         // -?Posizionamento cursore & segnalazione errore?
010100141209     d   PosCurBRT                     n   overlay(IndDspF : 51)
010200141209     d   PosCurPIN                     n   overlay(IndDspF : 52)
010300141209         //   -?Riemissione videata?
010400141209     d   ErrGenerico                   n   overlay(IndDspF : 99)
010500141209
010600141209       // -?Codice a Barre della LDV?
010700141209     d BARcodeDS       ds            17    inz
010800141209     d   BARcdeAAS                    2s 0 inz
010900141209     d   BARcdeLNP                    3s 0 inz
011000141209     d   BARcdeNRS                    2s 0 inz
011100141209     d   BARcdeNSP                    7s 0 inz
011200141209
011300141209       // -?Parametri ricevuti?
011400141210     d kpjba         e ds
011500141209     d trulPINds     e ds
011600141209
011700141209       // -?Dati da FIAR400F?
011800141209     d dsBL4Y        e ds                  inz  qualified
011900141209
012000141209       //--------------------------------------------------------------
012100141209       //?Definizione variabili globali.                               ?
012200141209       //--------------------------------------------------------------
012300141209
012400141209       // -?Flags booleani?
012500141209     d $Fine           s               n   inz
012600141209     d $InzD01         s               n   inz(*on)
012700141209     d $InzD02         s               n   inz(*on)
012800141209
012900141209       // -?Variabili per la gestione del video?
013000141209     d $Video          s              2    inz('D1')
013100141209
013200141209       // -?Contatore PINcode errati?
013300141209     d wCountErr       s              3  0 inz
013400141209
013500141209       //--------------------------------------------------------------
013600141209       //?Definizione prototipi procedure.                             ?
013700141209       //--------------------------------------------------------------
013800141209
013900141209       // -?Reperimento dati utente?
014000141209     d TIBS34ds      e ds
014100141209      /copy gaitrasrc/srcProtoPR,TIBS34R
014200141209
014300141209       // -?Calcolo/Controllo BRTcode?
014400141209      /copy gaitrasrc/srcProtoPI,TRUL28R0
014500141209      /copy gaitrasrc/srcProtoPR,TRUL28R0
014600141209
014700141209       //--------------------------------------------------------------
014800141209       //?Definizione key-list.                                        ?
014900141209       //--------------------------------------------------------------
015000141209
015100141209       // -?File FIAR401L?
015200141209     d keyFIAR401    e ds                  extname(FIAR401L : *key)
015300141209     d                                     prefix(k_)   inz
015400141209
015500141209       //--------------------------------------------------------------
015600141209       //?M A I N - L I N E                                            ?
015700141209       //--------------------------------------------------------------
015800141209
015900141209     c     *Entry        plist
016000141210     c                   parm                    KPJBA
016100141209     c                   parm                    trulPINds
016200141209
016300141209      /free
016400141209
016500141209       // -?Operazioni iniziali?
016600141209       exsr  sr_RoutInz;
016700141209
016800141209
016900141209       // -?Gestione Video?
017000141209       DoW  Not $Fine;
017100141209
017200141209         Select;
017300141209
017400141209           // -?Richiesta BRTcode?
017500141209           When  $Video = 'D1';
017600141209             exsr  sr_GesD01;
017700141209
017800141209           // -?Richiesta PINcode?
017900141209           When  $Video = 'D2';
018000141209             exsr  sr_GesD02;
018100141209
018200141209           // -?? ? ? ? ??
018300141209           Other;
018400141209             $Fine = *on;
018500141209
018600141209         EndSl;
018700141209
018800141209       EndDo;
018900141209
019000141209
019100141209       // -?Operazioni finali?
019200141209       exsr  sr_RoutEnd;
019300141209
019400141209       //--------------------------------------------------------------
019500141209       //?Operazioni iniziali.                                         ?
019600141209       //--------------------------------------------------------------
019700141209       BEGSR  sr_RoutInz;
019800141209
019900141209         *inLR = *on;
020000141209
020100141209         // -?Inizializzazione campi di Output?
020200141209         clear  oPINfxx;
020300141209         clear  oPINerr;
020400141209         clear  oPINmsg;
020500141210
020600141210         T1Cpgm = SDSpgm;
020700141209
020800141209         // -?Reperimento dati job?
020900141209         exsr  sr_DatiJob;
021000141209
021100141209         // -?NON eseguibile da Sede (FIAR4* è solo in filiale)?
021200141209         if  DUTlpo = 'S';
021300141209           oPINerr = *on;
021400141209           oPINmsg = 'Calcolo PINcode eseguibile solo da filiale';
021500141209           exsr  sr_RoutEnd;
021600141209         endif;
021700141209
021800141209         // -?Verifica dei parametri ricevuti?
021900141209         //  ?(Numero spedizione mancante)?
022000141209         if  iPINaas = *zero  or  iPINlnp = *zero  or  iPINnsp = *zero;
022100141209           oPINerr = *on;
022200141209           oPINmsg = 'Numero spedizione non ricevuto';
022300141209           exsr  sr_RoutEnd;
022400141209         endif;
022500141209
022600141209         // -?Calcolo BRTcode?
022700141209         clear  BarCodeDS;
022800141209         BARcdeAAS = %int( %subst( %editc( iPINaas : 'X' ) : 3 : 2 ) );
022900141209         BARcdeLNP = iPINlnp;
023000141209         BARcdeNRS = iPINnrs;
023100141209         BARcdeNSP = iPINnsp;
023200141209         reset  TRUL28ds0;
023300141209         trul28ds0.i280cod = BarCodeDS;
023400141209         trul28r0 ( TRUL28ds0 );
023500141209         if  trul28ds0.o280err = *on;
023600141209           oPINerr = *on;
023700141209           oPINmsg = 'BRTcode non calcolabile per la spedizione';
023800141209           exsr  sr_RoutEnd;
023900141209         endif;
024000141210
024100141210         // -?Reperimento PINcode?
024200141210         open  FIAR401L;
024300141210         clear  keyFIAR401;
024400141210         k_AR4aas = iPINaas;
024500141210         k_AR4lnp = iPINlnp;
024600141210         k_AR4nrs = iPINnrs;
024700141210         k_AR4nsp = iPINnsp;
024800141210         k_AR4trc = 'Y';
024900141210         chain  %kds( keyFIAR401 )  FIAR4000;
025000141210         if  Not %found(FIAR401L);
025100141210           oPINerr = *on;
025200141210           oPINmsg = 'PINcode non reperibile per la spedizione';
025300141210           exsr  sr_RoutEnd;
025400141210         endif;
025500141210
025600141210         dsBL4Y = AR4not;
025700141209
025800141209       ENDSR;
025900141209
026000141209       //--------------------------------------------------------------
026100141209       //?Reperimento Dati del job (Utente/Operativi).                 ?
026200141209       //--------------------------------------------------------------
026300141209       BEGSR  sr_DatiJob;
026400141209
026500141209         in(E) §AzUte;
026600141209         if NOT %error;
026700141209           in(E) §DatiUte;
026800141209         endif;
026900141209         if %error or RSut = *blanks;
027000141209           clear TIBS34ds;
027100141209           tibs34r ( tibs34ds );
027200141209           in §AzUte;
027300141209           in §DatiUte;
027400141209         endif;
027500141209
027600141209       ENDSR;
027700141209
027800141209       //--------------------------------------------------------------
027900141209       //?Richiesta / Controllo "BRTcode".                             ?
028000141209       //--------------------------------------------------------------
028100141209       BEGSR  sr_GesD01;
028200141209
028300141209         // -?Inizializzazione videata?
028400141209         if  $InzD01 = *on;
028500141210           exsr  sr_InzD01;
028600141209           $InzD01 = *off;
028700141209         endif;
028800141209
028900141209         // -?Emissione videata?
029000141210         write  ulPINw1;
029100141209         write  ulPINt1;
029200141209         exfmt  ulPINd1;
029300141209
029400141209         clear  V1Dmsg;
029500141209         reset  ErrMessage;
029600141209         reset  ErrGenerico;
029700141209
029800141209         Select;
029900141209
030000141209           // -?F3=Fine?
030100141209           When  dsp_aid = c_F03;
030200141210             oPINfxx = '03';
030300141209             $Fine = *on;
030400141209
030500141209           // -?Invio?
030600141209           Other;
030700141209             exsr  sr_CtrD01;
030800141210             //if  ErrGenerico = *off  and  dsp_aid = c_F06;
030900141210             if  ErrGenerico = *off;
031000141209               $Video  = 'D2';
031100141209               $InzD02 = *on;
031200141209             endif;
031300141209
031400141209         EndSl;
031500141209
031600141209       ENDSR;
031700141210
031800141210       //--------------------------------------------------------------
031900141210       //?Inizializzazione videata D01.                                ?
032000141210       //--------------------------------------------------------------
032100141210       BEGSR  sr_InzD01;
032200141210
032300141210         clear  ulPINd1;
032400141210
032500141210         F6Attivo = *off;
032600141210
032700141210         if  iPINrow > *zero  and  iPINrow < 16;
032800141210           W1row = iPINrow;
032900141210         else;
033000141210           W1row = 7;
033100141210         endif;
033200141210
033300141210         if  ( ( iPINrow = 1  and  iPINcol > 1 )        or
033400141210               ( iPINrow > 1  and  iPINcol > *zero ) )  and
033500141210             iPINcol < 13;
033600141210           W1col = iPINcol;
033700141210         else;
033800141210           W1col = 5;
033900141210         endif;
034000141210
034100141210       ENDSR;
034200141209
034300141209       //--------------------------------------------------------------
034400141209       //?Controllo "BRTcode".                                         ?
034500141209       //--------------------------------------------------------------
034600141209       BEGSR  sr_CtrD01;
034700141209
034800141209         if  D1Cbrt <> trul28ds0.o280cod;
034900141209           ErrGenerico = *on;
035000141209           ErrMessage  = *on;
035100141209           V1Dmsg = 'BRTcode errato';
035200141209           leavesr;
035300141209         endif;
035400141209
035500141209       ENDSR;
035600141209
035700141209       //--------------------------------------------------------------
035800141209       //?Richiesta / Controllo "PINcode".                             ?
035900141209       //--------------------------------------------------------------
036000141209       BEGSR  sr_GesD02;
036100141209
036200141209         // -?Inizializzazione videata?
036300141209         if  $InzD02 = *on;
036400141209           exsr  sr_InzD02;
036500141209           $InzD02 = *off;
036600141209         endif;
036700141209
036800141209         // -?Emissione videata?
036900141210         if  Not ErrMessage;
037000141210           write  ulPINw1;
037100141210           write  ulPINt1;
037200141210           write  ulPINd1;
037300141210         endif;
037400141209         exfmt  ulPINd2;
037500141209
037600141209         clear  V1Dmsg;
037700141209         reset  ErrMessage;
037800141209         reset  ErrGenerico;
037900141209
038000141209         Select;
038100141209
038200141209           // -?F3=Fine?
038300141209           When  dsp_aid = c_F03;
038400141210             oPINfxx = '03';
038500141209             $Fine = *on;
038600141210
038700141210           //// -?F12=Ritorno?
038800141210           //When  dsp_aid = c_F12;
038900141210           //  oPINfxx = '12';
039000141210           //  $Video  = 'D1';
039100141209
039200141210           // -?F6=Conferma?
039300141210           //Other;
039400141210           When  dsp_aid = c_F06;
039500141210             exsr  sr_CtrD02;
039600141210             if  ErrGenerico = *off;
039700141209               $Fine = *on;
039800141209             endif;
039900141209
040000141209         EndSl;
040100141209
040200141209       ENDSR;
040300141209
040400141209       //--------------------------------------------------------------
040500141209       //?Inizializzazione videata D02.                                ?
040600141209       //--------------------------------------------------------------
040700141209       BEGSR  sr_InzD02;
040800141209
040900141209         clear  ulPINd2;
041000141210         D2Cbrt = D1Cbrt;
041100141210
041200141210         F6Attivo = *on;
041300141209
041400141209         clear  wCountErr;
041500141209
041600141209       ENDSR;
041700141209
041800141209       //--------------------------------------------------------------
041900141209       //?Controllo "PINcode".                                         ?
042000141209       //--------------------------------------------------------------
042100141209       BEGSR  sr_CtrD02;
042200141209
042300141209         // -?PINcode errato?
042400141210         If  D2Cpin <> dsBL4Y.§B4pcd;
042500141209
042600141209           wCountErr += 1;
042700141209
042800141209           // -?Non ancora raggiunto il num. max di tentativi (5)?
042900141210           if  wCountErr < c_MaxErr;
043000141209
043100141209             ErrGenerico = *on;
043200141209             ErrMessage  = *on;
043300141210             V1Dmsg = 'PINcode errato - rimangono ' +
043400141209                      %trim( %editc( c_MaxErr - wCountErr : '3' ) ) +
043500141210                      ' tentativi di inserimento';
043600141209             leavesr;
043700141209
043800141209           // -?Raggiunto il num. max di tentativi (5)?
043900141209           else;
044000141209
044100141209             oPINerr = *on;
044200141209             oPINmsg = 'Esauriti i tentativi di inserimento PINcode';
044300141209             exsr  sr_RoutEnd;
044400141209
044500141209           endif;
044600141209
044700141209         EndIf;
044800141209
044900141209       ENDSR;
045000141209
045100141209       //--------------------------------------------------------------
045200141209       //?Operazioni finali.                                           ?
045300141209       //--------------------------------------------------------------
045400141209       BEGSR  sr_RoutEnd;
045500141209
045600141209         return;
045700141209
045800141209       ENDSR;
045900141210
046000141210      /end-free
