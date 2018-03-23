000100090515       //--------------------------------------------------------------
000200090515       //?TRTB47R * Manutenzione tabella "3R" = Scheda clienti         ?
000300090515       //--------------------------------------------------------------
000400110811
000500110811      *?  ATTENZIONE!!  ?
000600110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110811      *?    'Interrogazione abilitazioni clienti'              ?
000800110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110811      *?    verificare se sono validi per la visualizzazione   ?
001000110811      *?    da TA61                                            ?
001100090515
001200090515     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001300090515
001400090515       //--------------------------------------------------------------
001500090515       //?Dichiarazione file.                                          ?
001600090515       //--------------------------------------------------------------
001700090515
001800090515       // - Organigramma filiale/aziendale
001900090515     fAZORG01L  if   e           k disk
002000090515
002100090515       // - Tabelle
002200090515     fTABEL00F  Uf A e           k disk
002300090515
002400090515       // - Video Gestione
002500090525     fTRTB47D   cf   e             workstn
002600090515     f                                     indds(IndDspF)
002700090515     f                                     infds(InfDspF)
002800090515
002900090515       //--------------------------------------------------------------
003000090515       //?Definizione costanti.                                        ?
003100090515       //--------------------------------------------------------------
003200090515
003300090515       // - Codice tabella in gestione
003400090518     d c_Tab           c                   const('3R')
003500090515
003600090515       // - Codice utente
003700090515     d c_Kut           c                   const(1)
003800090515
003900090515       // - Costante utente "EDP"
004000090515     d c_Edp           c                   const('EDP')
004100090515
004200090515       // - Costante per controllo "caratteri solo numerici"
004300090515     d c_Digit         c                   const('0123456789')
004400090515
004500090515       // - Tasti funzionali a video
004600090515     d c_F01           c                   const(x'31')
004700090515     d c_F02           c                   const(x'32')
004800090515     d c_F03           c                   const(x'33')
004900090515     d c_F05           c                   const(x'35')
005000090515     d c_F06           c                   const(x'36')
005100090515     d c_F07           c                   const(x'37')
005200090515     d c_F08           c                   const(x'38')
005300090515     d c_F09           c                   const(x'39')
005400090515     d c_F10           c                   const(x'3A')
005500090515     d c_F12           c                   const(x'3C')
005600090515     d c_F13           c                   const(x'B1')
005700090515     d c_F14           c                   const(x'B2')
005800090515     d c_F15           c                   const(x'B3')
005900090515     d c_F16           c                   const(x'B4')
006000090515     d c_F17           c                   const(x'B5')
006100090515     d c_F18           c                   const(x'B6')
006200090515     d c_F19           c                   const(x'B7')
006300090515     d c_F20           c                   const(x'B8')
006400090515     d c_F21           c                   const(x'B9')
006500090515     d c_F22           c                   const(x'BA')
006600090515     d c_F23           c                   const(x'BB')
006700090515     d c_F24           c                   const(x'BC')
006800090515     d c_Enter         c                   const(x'F1')
006900090515     d c_RollDown      c                   const(x'F4')
007000090515     d c_RollUp        c                   const(x'F5')
007100090515
007200090515       //--------------------------------------------------------------
007300090515       //?Definizione schiere.                                         ?
007400090515       //--------------------------------------------------------------
007500090515
007600090518       // - Messaggi di errore
007700090518     d $Msg            s             78    dim( 6) ctdata perrcd(1)
007800090515
007900090515       //--------------------------------------------------------------
008000090515       //?Definizione aree dati.                                       ?
008100090515       //--------------------------------------------------------------
008200090515
008300090515       // - Dati utente
008400090515     d §AzUte        e ds                  extname(AZUTE00F)
008500090515     d                                     dtaara
008600090515     d §DatiUte      e ds                  extname(dDatiUte)
008700090515     d                                     dtaara
008800090515
008900090515       //--------------------------------------------------------------
009000090515       //?Definizione strutture dati.                                  ?
009100090515       //--------------------------------------------------------------
009200090515
009300090515       // - Status ds
009400090515     d Status         sds
009500090515     d  SDSpgm           *proc
009600090515
009700090515       // - InfDS
009800090515     d InfDspF         ds
009900090515     d   dsp_aid             369    369a
010000090515
010100090515       // - Indicatori su DspF
010200090515     d IndDspF         ds                  inz
010300090515         // - Abilitazione tasti funzionali
010400090515     d   F5Attivo                      n   overlay(IndDspF : 05)
010500090515     d   F6Attivo                      n   overlay(IndDspF : 06)
010600090515     d   F12Attivo                     n   overlay(IndDspF : 12)
010700090515     d   F14Attivo                     n   overlay(IndDspF : 14)
010800090515     d   F16Attivo                     n   overlay(IndDspF : 16)
010900090515     d   F20Attivo                     n   overlay(IndDspF : 20)
011000090515     d   F21Attivo                     n   overlay(IndDspF : 21)
011100090515     d   F23Attivo                     n   overlay(IndDspF : 23)
011200090518         // - Emissione messaggio di errore
011300090518     d   ErrMessage                    n   overlay(IndDspF : 28)
011400090515         // - Indicatori per Attibuti di visualizzazione
011500090518     d   VisBloAnn                     n   overlay(IndDspF : 40)
011600090515         // - Posizionamento cursore & segnalazione errore
011700090518     d   PosCurCLI                     n   overlay(IndDspF : 50)
011800090518     d   PosCurFIL                     n   overlay(IndDspF : 53)
011900090518     d   PosCurINV                     n   overlay(IndDspF : 54)
012000090515         //   - Riemissione videata
012100090515     d   ErrGenerico                   n   overlay(IndDspF : 99)
012200090515
012300090515       // - Parametri ricevuti
012400090515     d KPJBA         e ds
012500090515
012600090515       // - Parametri per Reperimento dati utente
012700090515     d TIBS34ds      e ds                  inz
012800090515
012900090515       // - Tabelle usate
013000090515     d ds1B          e ds                  inz
013100090515     d ds3R          e ds                  inz
013200090515
013300090515       // - Parametri per pgm. TIBS69R (decodifica cliente)
013400090515     d TIBS69ds      e ds                  inz
013500090515     d ds_CNACO      e ds                  extname(CNACO00F) inz
013600090515     d ds_CNIND      e ds                  extname(CNIND00F) inz
013700090515     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
013800090515     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
013900090515
014000090515       //--------------------------------------------------------------
014100090515       //?Definizione variabili globali.                               ?
014200090515       //--------------------------------------------------------------
014300090515
014400090515       // - Flags booleani
014500090515     d $Fine           s               n   inz
014600090515     d $InzD01         s               n   inz(*on)
014700090515     d $InzD02         s               n   inz(*on)
014800090515     d $Called         s               n   inz
014900090518     d $Protect        s               n   inz
015000090515
015100090515       // - Variabili per la gestione del video
015200090515     d $Video          s              2    inz('D1')
015300090515
015400090515       // - Codice cliemte eventualmente ricevuto
015500090515     d SaveCLI         s                   like(V1Ccli)  inz
015600090515
015700090515       // - Parametri per pgm interrogazione tabelle
015800090515      /copy gaitrasrc/srcProtoPI,X§TABER
015900090518
016000090518       // - Parametri per pgm interrogazione sottoconti
016100090518      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
016200090515
016300090515       //--------------------------------------------------------------
016400090515       //?Definizione prototipi procedure.                             ?
016500090515       //--------------------------------------------------------------
016600090515
016700090515       // - Reperimento dati utente
016800090515      /copy gaitrasrc/srcProtoPR,TIBS34R
016900090515
017000090515       // - Interrogazione tabelle
017100090515      /copy gaitrasrc/srcProtoPR,X§TABER
017200090515
017300090515       // - Controllo/Decodifica cliente
017400090515      /copy gaitrasrc/srcprotopr,TIBS69R
017500090518
017600090518       // - Interrogazione sottoconti
017700090518      /copy gaitrasrc/srcprotopr,XALFA3BR
017800090518
017900090518       // - Gestione scambio dati bolle partenze
018000090518     d trtb28r         pr                  extpgm('TRTB28R')
018100090518     d   kpjba                             likeds(kpjba)
018200090518
018300090518       // - Gestione scambio dati Cons.Giac.Disp. con clienti
018400090518     d trtb40r         pr                  extpgm('TRTB40R')
018500090518     d   kpjba                             likeds(kpjba)
018600090518
018700090518       // - Gestione scambio dati C/A e fattura con clienti
018800090518     d trtb44r         pr                  extpgm('TRTB44R')
018900090518     d   kpjba                             likeds(kpjba)
019000090515
019100090515       //--------------------------------------------------------------
019200090515       //?Definizione key-list.                                        ?
019300090515       //--------------------------------------------------------------
019400090515
019500090515       // - File TABEL00F
019600090515     d k03tabel00    e ds                  extname(TABEL00F : *key)
019700090515     d                                     prefix(k_)   inz
019800090515
019900090515       //--------------------------------------------------------------
020000090515       //?Riepilogo indicatori utilizzati.                             ?
020100090515       //--------------------------------------------------------------
020200090515       //--------------------------------------------------------------
020300090515
020400090515       //--------------------------------------------------------------
020500090515       //?M A I N - L I N E                                            ?
020600090515       //--------------------------------------------------------------
020700090515
020800090515     c     *Entry        plist
020900090515     c                   parm                    KPJBA
021000090515
021100090515      /free
021200090515
021300090515       //?Operazioni iniziali?
021400090515       exsr sr_RoutInz;
021500090515
021600090515       DoW  $Fine = *off;
021700090515         select;
021800090515           // - Gestione videata D1 (richiesta codice cliente)
021900090515           when $Video = 'D1';
022000090515             exsr sr_GesD01;
022100090515           // - Gestione subfile D2 (manutenzione dati cliente)
022200090515           when $Video = 'D2';
022300090515             exsr sr_GesD02;
022400090515           other;
022500090515             $Fine = *on;
022600090515         endsl;
022700090515       enddo;
022800090515
022900090515       //?Operazioni finali?
023000090515       exsr sr_RoutEnd;
023100090515
023200090515       //--------------------------------------------------------------
023300090515       //?Operazioni iniziali.                                         ?
023400090515       //--------------------------------------------------------------
023500090515       BEGSR  sr_RoutInz;
023600090515
023700090515         *inLR = *on;
023800090515
023900090515         //?Reperimento dati job?
024000090515         exsr  sr_DatiJob;
024100090515
024200090515         //?Impostazione nome programma a video?
024300090515         V1Tpgm = SDSpgm;
024400090515
024500090515         //?Impostazione campi "fissi"?
024600090515         k_TBLkut = c_Kut;
024700090515         k_TBLcod = c_Tab;
024800090515
024900090515         // Test delle prime 7 posizioni della KPJBA per sapere se il
025000090515         //  pgm è stato richiamato da un altro pgm o da menù tabelle,
025100090515         //  per sapere se abilitare o meno i tasti funzionali F7, F14
025200090515         //  e F21, che non vanno abilitati se è stato richiamato.
025300090515         $Called = (%subst( KPJBU : 1 : %len(V1Ccli) ) <> *blank);
025400090515         if  $Called = *on   and
025500090515             %check( c_Digit :
025600090519                     %subst( KPJBU : 1 : %len(V1Ccli) ) ) = *zero;
025700090515           SaveCLI = %subst( KPJBU : 1 : %len(V1Ccli) );
025800090519           $InzD01 = *off;
025900090519           exsr  sr_InzD01;
026000090519           exsr  sr_CtrD01;
026100090519           if  Not ErrGenerico;
026200090519             $Video = 'D2';
026300090519           endif;
026400090515         endif;
026500090515
026600090515       ENDSR;
026700090515
026800090515       //--------------------------------------------------------------
026900090515       //?Reperimento Dati del job (Utente/Operativi).                 ?
027000090515       //--------------------------------------------------------------
027100090515       BEGSR  sr_DatiJob;
027200090515
027300090515         in(e) §AzUte;
027400090515         if NOT %error;
027500090515           in(e) §DatiUte;
027600090515         endif;
027700090515         if %error or RSut = *blank;
027800090515           tibs34r ( tibs34ds );
027900090515           in §AzUte;
028000090515           in §DatiUte;
028100090515         endif;
028200090515
028300090515       ENDSR;
028400090518
028500090518       //--------------------------------------------------------------
028600090518       //?Operazioni finali.                                           ?
028700090518       //--------------------------------------------------------------
028800090518       BEGSR  sr_RoutEnd;
028900090518
029000090518         clear  TIBS69ds;
029100090518         I69tla = 'C';
029200090518         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
029300090518
029400090518         return;
029500090518
029600090518       ENDSR;
029700090515
029800090515       //--------------------------------------------------------------
029900090518       //?Gestione videata D01.                                        ?
030000090515       //--------------------------------------------------------------
030100090515       BEGSR  sr_GesD01;
030200090515
030300090515         //?Inizializzazione videata?
030400090515         if  $InzD01 = *on;
030500090515           exsr  sr_InzD01;
030600090515           $InzD01 = *off;
030700090515         endif;
030800090515
030900090515         //?SE ricevuto codice cliente NON si emette la prima videata?
031000090515         if  $Called   and   SaveCLI > *zero;
031100090515           exsr  sr_CtrD01;
031200090515           if  Not ErrGenerico;
031300090515             $Video  = 'D2';
031400090515             $InzD02 = *on;
031500090515             leavesr;
031600090518           else;
031700090518             clear SaveCLI;
031800090515           endif;
031900090515         endif;
032000090515
032100090518         //?Emissione Testata?
032200090515         write  TB47T1;
032300090515
032400090515         //?Emissione videata?
032500090515         exfmt  TB47D1;
032600090518         reset  ErrMessage;
032700090515         reset  ErrGenerico;
032800090518         clear  V1Dmsg;
032900090515         SELECT;
033000090515
033100090515           //?F3=Fine?
033200090515           WHEN  dsp_aid = c_F03;
033300090515             $Fine = *on;
033400090515
033500090515           //?F14=Gestione scambio dati C/A e fattura  con clienti?
033600090515           //?F20=Gestione scambio dati Bolle          con clienti?
033700090515           //?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
033800090515           WHEN  dsp_aid = c_F14   or
033900090515                 dsp_aid = c_F20   or
034000090515                 dsp_aid = c_F21;
034100090518             exsr  sr_F142021;
034200090515
034300090515           //?Invio?
034400090515           OTHER;
034500090515             exsr sr_CtrD01;
034600090515             if  ErrGenerico = *off;
034700090515               $Video    = 'D2';
034800090515               $InzD02   = *on;
034900090515             endif;
035000090515
035100090515         ENDSL;
035200090515
035300090515       ENDSR;
035400090515
035500090515       //--------------------------------------------------------------
035600090518       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
035700090515       //--------------------------------------------------------------
035800090515       BEGSR  sr_InzD01;
035900090515
036000090515         clear  V1Topz;
036100090518         clear  TB47D1;
036200090515
036300090515         // Se è stato richiamato: nei primi 7 caratteri della KPJBU
036400090515         //   c'è il codice cliente => NON devono essere abilitati i
036500090515         //   tasti funzionali F7, F14 e F21.
036600090515         F14Attivo = (Not $Called);
036700090515         F20Attivo = (Not $Called);
036800090515         F21Attivo = (Not $Called);
036900090515
037000090515         // Se è stato richiamato con un codice cliente numerico, si
037100090515         //   imposta anche tale codice a video
037200090519         select;
037300090519           when  $Called   and   SaveCLI > *zero;
037400090519             V1Ccli = SaveCLI;
037500090519           when  Not $Called;
037600090519             V1Ccli = '?';
037700090519         endsl;
037800090515
037900090515       ENDSR;
038000090518
038100090518       //--------------------------------------------------------------
038200090518       //?Gestione tasti funzinali F14, F20 ed F21.                    ?
038300090518       //--------------------------------------------------------------
038400090518       BEGSR  sr_F142021;
038500090518
038600090518         //?Il chiamato RICHIEDE un codice numerico per verificare se?
038700090518         //  ?è stato richiamato?
038800090518         if  V1Ccli = *blank;
038900090518           KPJBU = *zero;
039000090518         else;
039100090518           KPJBU = V1Ccli;
039200090518         endif;
039300090518
039400090518         select;
039500090518
039600090518           //?F14 = Gestione scambio dati C/A e fattura con clienti?
039700090518           when  dsp_aid = c_F14;
039800090518             trtb44r ( kpjba );
039900090518
040000090518           //?F20 = Gestione scambio dati bolle partenze?
040100090518           when  dsp_aid = c_F20;
040200090518             trtb28r ( kpjba );
040300090518
040400090518           //?F21 = Gestione scambio dati Cons.Giac.Disp. con clienti?
040500090518           when  dsp_aid = c_F21;
040600090518             trtb40r ( kpjba );
040700090518
040800090518         endsl;
040900090518
041000090518       ENDSR;
041100090515
041200090515       //--------------------------------------------------------------
041300090518       //?Controllo dati immessi nella videata D01.                    ?
041400090515       //--------------------------------------------------------------
041500090515       BEGSR  sr_CtrD01;
041600090515
041700090515         %subst(IndDspF : 50) = *off;
041800090518         clear  $Protect;
041900090515
042000090518         //?Filiale?
042100090518         if  V1Cfil > *zero;
042200090518           chain  (V1Cfil)  AZORG;
042300090518           if  NOT %found(AZORG01L);
042400090518             V1Dmsg = $Msg(01);
042500090519             PosCurFIL   = *on;
042600090518             ErrMessage  = *on;
042700090518             ErrGenerico = *on;
042800090518             leavesr;
042900090518           endif;
043000090518         endif;
043100090518
043200090515         SELECT;
043300090515
043400090515           //?Ricerca codice cliente?
043500090515           WHEN  %scan('?' : V1Ccli) > *zeros;
043600090515             X§Tkut = c_Kut;
043700090515             X§Tcod = c_Tab;
043800090519             if  V1Cfil > *zero;
043900090519               X§Tkey = %editc( V1Cfil : 'X' );
044000090519             else;
044100090519               clear  X§Tkey;
044200090519             endif;
044300090515             clear  X§Tdes;
044400090515             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
044500090518             V1Ccli = %subst(X§Tkey : 1 : %len(V1Ccli));
044600090515             ErrGenerico = *on;
044700090519             leavesr;
044800090518
044900090518           //?Ricerca alfabetica?
045000090518           WHEN  V1Dcli <> *blank   and
045100090518                (V1Ccli =  *blank   or   V1Ccli = *zero);
045200090518             xParDut = RSut;
045300090519             xParKut = 1;
045400090518             xParRag = V1Dcli;
045500090518             xParKcc = DUTkci;
045600090518             xParSta = 9;
045700090519             xParFlr = %editc( V1Cfil : 'X');
045800090519             clear  xParDit;
045900090518             xParNum = 1;
046000090519             clear  xParKcm;
046100090519             clear  xParKsm;
046200090519             clear  xParKdm;
046300090519             clear  xParEsci;
046400090519             clear  xParErr;
046500090519             clear  xParIva;
046600090519             clear  xParCdf;
046700090518             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
046800090518                        xParSta : xParFlr : xParDit : xParNum :
046900090518                        xParKcm : xParKsm : xParKdm : xParEsci :
047000090518                        xParErr : xParIva : xParCdf );
047100090518             if  xParSta <> -1   and   xParErr = *blank;
047200090518               V1Ccli = xParKsm;
047300090518               V1Dcli = xParRag;
047400090518             endif;
047500090518             ErrGenerico = *on;
047600090518             leavesr;
047700090515
047800090515           //?Controllo immissione codice cliente?
047900090515           WHEN  V1Ccli = *blank  or  V1Ccli  = *zero;
048000090518             V1Dmsg = $Msg(02);
048100090518             PosCurCLI   = *on;
048200090518             ErrMessage  = *on;
048300090515             ErrGenerico = *on;
048400090515             leavesr;
048500090515
048600090515           //?Controllo immisione solo caratteri numerici?
048700090518           WHEN  %check(c_Digit : V1Ccli) > *zero;
048800090518             V1Dmsg = $Msg(03);
048900090518             PosCurCLI   = *on;
049000090518             ErrMessage  = *on;
049100090515             ErrGenerico = *on;
049200090515             leavesr;
049300090515
049400090515         ENDSL;
049500090515
049600090515         //?Controllo/decodifica cliente?
049700090515         clear  TIBS69ds;
049800090515         I69kcc = DUTkci;
049900090518         I69kac = %int(V1Ccli);
050000090515         I69sif = knsif;
050100090515         tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
050200090515         if  O69err = *on;
050300090518           V1Dmsg = $Msg(04);
050400090518           PosCurCLI   = *on;
050500090518           ErrMessage  = *on;
050600090515           ErrGenerico = *on;
050700090515           leavesr;
050800090515         endif;
050900090515         V1Dcli = %subst(ACOrag : 1 : %len(V1Dcli));
051000090518
051100090518         //?Verifica esistenza del record in TABEL?
051200090518         k_TBLkey = V1Ccli;
051300090518
051400090518         chain %kds(k03tabel00)  TABEL;
051500090518
051600090518         select;
051700090518
051800090518           // Codice annullato
051900090518           when  ACOflg <> *blank;
052000090518             if  %found(TABEL00F);
052100090518               $Protect = *on;
052200090518             else;
052300090518               V1Dmsg = $Msg(05);
052400090518               PosCurCLI   = *on;
052500090518               ErrMessage  = *on;
052600090518               ErrGenerico = *on;
052700090518               leavesr;
052800090518             endif;
052900090518
053000090518           // Codice bloccato
053100130315           when  ACOabl <> *blanks;
053200090518             if  %found(TABEL00F);
053300130201       //      //$Protect = *on;
053400090518             else;
053500090518               V1Dmsg = $Msg(05);
053600090518               PosCurCLI   = *on;
053700090518               ErrMessage  = *on;
053800090518               ErrGenerico = *on;
053900090518               leavesr;
054000090518             endif;
054100090518
054200090518         endsl;
054300090515
054400090515       ENDSR;
054500090518
054600090518       //--------------------------------------------------------------
054700090518       //?Gestione videata D02.                                        ?
054800090518       //--------------------------------------------------------------
054900090518       BEGSR  sr_GesD02;
055000090518
055100090518         //?Inizializzazione videata?
055200090518         if  $InzD02 = *on;
055300090518           exsr  sr_InzD02;
055400090518           $InzD02 = *off;
055500090518         endif;
055600090518
055700090518         //?Emissione Testata?
055800090518         write  TB47T1;
055900090518
056000090518         //?Emissione videata?
056100090518         if  Not $Protect;
056200090518           exfmt  TB47D2;
056300090518         else;
056400090518           write  TB47D2;
056500090518           exfmt  Protect;
056600090518         endif;
056700090518         reset  ErrMessage;
056800090518         reset  ErrGenerico;
056900090518         clear  V1Dmsg;
057000090518         SELECT;
057100090518
057200090518           //?F3=Fine?
057300090518           WHEN  dsp_aid = c_F03;
057400090518             unlock  TABEL00F;
057500090518             $Fine = *on;
057600090518
057700090518           //?F12=Ritorno?
057800090518           WHEN  dsp_aid = c_F12;
057900090518             unlock  TABEL00F;
058000090518             $Video = 'D1';
058100090518
058200090518           //?F14=Gestione scambio dati C/A e fattura  con clienti?
058300090518           //?F20=Gestione scambio dati Bolle          con clienti?
058400090518           //?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
058500090518           WHEN  dsp_aid = c_F14   or
058600090518                 dsp_aid = c_F20   or
058700090518                 dsp_aid = c_F21;
058800090518             exsr  sr_F142021;
058900090518
059000090518           //?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
059100090518           OTHER;
059200090518             // - Controlli eseguiti NON se F16=Annullamento
059300090518             if  dsp_aid <> c_F16;
059400090518               exsr sr_CtrD02;
059500090518               if  ErrGenerico = *on;
059600090518                 leavesr;
059700090518               endif;
059800090518             endif;
059900090518             // - Aggiornamento
060000090518             if  dsp_aid = c_F05   or
060100090518                 dsp_aid = c_F06   or
060200090518                 dsp_aid = c_F16;
060300090518               exsr  sr_UpdTABEL;
060400090518               if  ErrGenerico = *on;
060500090518                 leavesr;
060600090518               endif;
060700090518               // Uscita dal programma SE richiamato
060800090518               if  $Called = *on   and   SaveCLI > *zero;
060900090518                 $Fine = *on;
061000090518               else;
061100090518                 $Video  = 'D1';
061200090518                 $InzD01 = *on;
061300090518               endif;
061400090518             endif;
061500090518
061600090518         ENDSL;
061700090518
061800090518         //?Pulizia del sotto-titolo in testata al ritorno in D01?
061900090518         if  $Video <> 'D2';
062000090518           clear  V1Topz;
062100090518         endif;
062200090518
062300090518       ENDSR;
062400090518
062500090518       //--------------------------------------------------------------
062600090518       //?Inizializzazione videata D02 - Gestione dati cliente.        ?
062700090518       //--------------------------------------------------------------
062800090518       BEGSR  sr_InzD02;
062900090518
063000090518         reset  $Protect;
063100090518
063200090518         //?Impostazione testata?
063300090518         clear  V1Topz;
063400090518         select;
063500090518           when  Not %found(TABEL00F);
063600090518             V1Topz = '  INSERIMENTO  ';
063700090518           when  TBLflg = '*';
063800090518             V1Topz = '  RIPRISTINO   ';
063900090518           other;
064000090518             V1Topz = '   MODIFICA    ';
064100090518         endsl;
064200090518
064300090518
064400090518         //?Impostazione dettaglio?
064500090518         clear  TB47D2;
064600090518
064700090518         V2Ccli = %int(V1Ccli);
064800090518         V2Dcli = ACOrag;
064900090518
065000090518         // - Solo se Modifica/Annullamento/Ripristino
065100090518         if  %found(TABEL00F);
065200090518           ds3R = TBLuni;
065300090518           §3RinV = §3Rinn;
065400090518           §3RicV = §3Rinc;
065500090518           §3RhdV = §3Rhdw;
065600090518           §3RnoV = §3Rnot;
065700090518         endif;
065800090518
065900090518         select;
066000090518           // Codice annullato
066100090518           when  ACOflg <> *blank;
066200090518             if  %found(TABEL00F);
066300090518               BloAnn   = 'CODICE ANNULLATO';
066400090518               $Protect = *on;
066500090518             endif;
066600090518           // Codice bloccato
066700130315           when  ACOabl <> *blanks;
066800090518             if  %found(TABEL00F);
066900090518               BloAnn   = 'CODICE BLOCCATO ';
067000130201       //      //$Protect = *on;
067100090518             endif;
067200090518         endsl;
067300090518         VisBloAnn = (BloAnn <> *blank);
067400090518
067500090518         //?Abilitazione F7, F14 e F21 SE NON richiamato?
067600090518         F14Attivo = (Not $Called);
067700090518         F20Attivo = (Not $Called);
067800090518         F21Attivo = (Not $Called);
067900090518
068000090518         //?Abilitazione F5=Ripristino SE record tabella ANNULLATO?
068100090518         F5Attivo = (%found(TABEL00F)   and   TBLflg = '*');
068200090518
068300090518         //?Abilitazione F6=Aggiornam. SE record tabella NON ANNULLATO?
068400090525         F6Attivo = ((%found(TABEL00F)   and   TBLflg = *blank   and
068500090525                      Not $Protect)      OR    Not %found(TABEL00F));
068600090518
068700090518         //?Abilitazione F12=Ritorno SE NON richiamato con cod. cliente?
068800090519         F12Attivo = (Not $Called);
068900090518
069000090518         //?Abilitazione F16=Annullam. SE record tabella NON ANNULLATO?
069100090518         //  ?ed ai soli utenti EDP*?
069200090518         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank   and
069300090518                      %subst( knmus : 1 : 3 ) = 'EDP');
069400090518
069500090518       ENDSR;
069600090518
069700090518       //--------------------------------------------------------------
069800090518       //?Controllo dati immessi nella videata D02.                    ?
069900090518       //--------------------------------------------------------------
070000090518       BEGSR  sr_CtrD02;
070100090518
070200090518         %subst(IndDspF : 50) = *off;
070300090518
070400090518         //?Obbligatorio almeno un campo?
070500090518         if  §3RinV = *blank   and
070600090518             §3RicV = *blank   and
070700090518             §3RhdV = *blank   and
070800090518             §3RnoV = *blank;
070900090518           V1Dmsg = $Msg(06);
071000090518           PosCurINV   = *on;
071100090518           ErrMessage  = *on;
071200090518           ErrGenerico = *on;
071300090518           leavesr;
071400090518         endif;
071500090518
071600090518       ENDSR;
071700090518
071800090518       //--------------------------------------------------------------
071900090518       //?Aggiornamento record nel file TABEL00F.                      ?
072000090518       //--------------------------------------------------------------
072100090518       BEGSR  sr_UpdTABEL;
072200090518
072300090519         //?Impostazioni campi "comuni"?
072400090518         clear  TBLftr;
072500090518         clear  TBLdtr;
072600090518
072700090518         select;
072800090518
072900090518           // -?F5 = Ripristino?
073000090518           when  dsp_aid = c_F05   and   %found(TABEL00F);
073100090518             clear  TBLflg;
073200090518             //____________
073300090518             update  TABEL;
073400090518             //¯¯¯¯¯¯¯¯¯¯¯¯
073500090518
073600090518           // -?F6 = Aggiornamento / Inserimento?
073700090525           when  dsp_aid = c_F06;
073800090518             if  Not %found(TABEL00F);
073900090518               TBLkut = c_Kut;
074000090518               TBLcod = c_Tab;
074100090518               TBLkey = k_TBLkey;
074200090518             endif;
074300090518             clear  ds3R;
074400090518             §3Rinn = §3RinV;
074500090518             §3Rinc = §3RicV;
074600090518             §3Rhdw = §3RhdV;
074700090518             §3Rnot = §3RnoV;
074800090518             TBLuni = ds3R;
074900090518
075000090518             TBLftt = '1';
075100090518             clear  TBLflt;
075200090518
075300090518             if  %found(TABEL00F);
075400090518               //____________
075500090518               update  TABEL;
075600090518               //¯¯¯¯¯¯¯¯¯¯¯¯
075700090518             else;
075800090518               //____________
075900090518               write   TABEL;
076000090518               //¯¯¯¯¯¯¯¯¯¯¯¯
076100090518             endif;
076200090518
076300090518           // -?F16 = Annullamento?
076400090518           when  dsp_aid = c_F16;
076500090518             TBLflg = '*';
076600090518             //____________
076700090518             update  TABEL;
076800090518             //¯¯¯¯¯¯¯¯¯¯¯¯
076900090518
077000090518         endsl;
077100090518
077200090518       ENDSR;
077300090518
077400090518      /end-free
077500090518
077600090518       //--------------------------------------------------------------
077700090518       //?Schiere a tempo di compilazione.                             ?
077800090518       //--------------------------------------------------------------
077900090518
078000090518** - $Msg -------------------------------------------------------------------*
078100090518Filiale inesistente                                                            1
078200090518Campo obbligatorio                                                             2
078300090518Ammessi SOLO caratteri numerici                                                3
078400090518Cliente NON presente in anagrafica                                             4
078500090518Cliente annullato o bloccato                                                   5
078600090518Immettere ALMENO un dato per la scheda cliente                                 6
