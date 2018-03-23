000100101104       //==============================================================
000200101105       //?TRTB84R * GESTIONE TABELLA "3Q" = CLIENTI VAB MULTIMEMBRO    ?
000300101104       //==============================================================
000400110810      *?  ATTENZIONE!!  ?
000500110810      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000600110810      *?    'Interrogazione abilitazioni clienti'              ?
000700110810      *?    In caso di aggiunta/modifica campi alla tabella    ?
000800110810      *?    verificare anche relativo pgm di interrogazione    ?
000900110810      *?    per le abilitazioni clienti --> TRTB841R.          ?
001000101104
001100101104       //--------------------------------------------------------------
001200101104       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001300101104       //--------------------------------------------------------------
001400101104
001500101104     /*PRM dbgview(*source)
001600101104     /*END
001700101104
001800101104       //--------------------------------------------------------------
001900101104       //?Specifiche di controllo.                                     ?
002000101104       //--------------------------------------------------------------
002100101104
002200101104     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002300101104     h dftactgrp(*no)
002400101104     h bnddir('UBBNDDIR':'TRUL')
002500101104
002600101104       //--------------------------------------------------------------
002700101104       //?Dichiarazione file.                                          ?
002800101104       //--------------------------------------------------------------
002900101104
003000101104       // -?Tabella "3Q"?
003100101104     fTABEL00F  Uf A e           k disk
003200101105     f                                     extfile(wLibFile)  usropn
003300101104
003400101104       // -?Video Gestione?
003500101104     fTRTB84D   cf   e             workstn
003600101104     f                                     indds(IndDspF)
003700101104     f                                     infds(InfDspF)
003800101104
003900101104       //--------------------------------------------------------------
004000101104       //?Definizione costanti.                                        ?
004100101104       //--------------------------------------------------------------
004200101104
004300101104       // -?Codice tabella in gestione?
004400101104     d c_Tab           c                   const('3Q')
004500101104
004600101104       // -?Codice utente?
004700101104     d c_Kut           c                   const(1)
004800101104
004900101104       // -?Costanti per la definizione delle schiere con i nomi?
005000101104       //  ?degli iSeries da elaborare e delle relative librerie?
005100101104     d c_NrSyst        c                   const(2)
005200101104     d c_NrLibr        c                   const(2)
005300101104
005400101104       // -?Tasti funzionali a video?
005500101104     d c_F01           c                   const(x'31')
005600101104     d c_F02           c                   const(x'32')
005700101104     d c_F03           c                   const(x'33')
005800101104     d c_F04           c                   const(x'34')
005900101104     d c_F05           c                   const(x'35')
006000101104     d c_F06           c                   const(x'36')
006100101104     d c_F07           c                   const(x'37')
006200101104     d c_F08           c                   const(x'38')
006300101104     d c_F09           c                   const(x'39')
006400101104     d c_F10           c                   const(x'3A')
006500101104     d c_F11           c                   const(x'3B')
006600101104     d c_F12           c                   const(x'3C')
006700101104     d c_F13           c                   const(x'B1')
006800101104     d c_F14           c                   const(x'B2')
006900101104     d c_F15           c                   const(x'B3')
007000101104     d c_F16           c                   const(x'B4')
007100101104     d c_F17           c                   const(x'B5')
007200101104     d c_F18           c                   const(x'B6')
007300101104     d c_F19           c                   const(x'B7')
007400101104     d c_F20           c                   const(x'B8')
007500101104     d c_F21           c                   const(x'B9')
007600101104     d c_F22           c                   const(x'BA')
007700101104     d c_F23           c                   const(x'BB')
007800101104     d c_F24           c                   const(x'BC')
007900101104     d c_Enter         c                   const(x'F1')
008000101104     d c_RollDown      c                   const(x'F4')
008100101104     d c_RollUp        c                   const(x'F5')
008200101104
008300101104       //--------------------------------------------------------------
008400101104       //?Definizione schiere.                                         ?
008500101104       //--------------------------------------------------------------
008600101105
008700101105       // -?iSeries  &  Librerie con entrambi i file tabelle?
008800101105     d $iSystem        s                   like(currSysNetA)
008900101105     d                                     dim(c_NrSyst)
009000101105     d                                     ctdata   perrcd( 1)
009100101105     d $Libraries      s                   like(ds_Libr)
009200101105     d                                     dim(c_NrLibr)
009300101105     d                                     alt($iSystem)
009400101105
009500101105       // -?Opzioni e relative decodifiche?
009600101105     d $Opz            s                   like(D83op1)
009700101105     d                                     dim(10)
009800101105     d                                     ctdata   perrcd(1)
009900101105     d $OpzD           s                   like(VTDfun)
010000101105     d                                     dim(10)
010100101105     d                                     alt($Opz)
010200101104
010300101104       // -?Messaggi di errore?
010400101105     d $Msg            s             78    dim(13) ctdata perrcd(1)
010500101104
010600101104       //--------------------------------------------------------------
010700101104       //?Definizione aree dati.                                       ?
010800101104       //--------------------------------------------------------------
010900101104
011000101104       // -?Dati utente?
011100101104     d §AzUte        e ds                  extname(AZUTE00F)
011200101104     d                                     dtaara
011300101104     d §DatiUte      e ds                  extname(dDatiUte)
011400101104     d                                     dtaara
011500101104
011600101104       //--------------------------------------------------------------
011700101104       //?Definizione strutture dati.                                  ?
011800101104       //--------------------------------------------------------------
011900101104
012000101104       // -?Status ds?
012100101104     d Status         sds
012200101104     d  SDSpgm           *proc
012300101104
012400101104       // -?InfDS?
012500101104     d InfDspF         ds
012600101104     d   dsp_aid             369    369a
012700101104
012800101104       // -?Indicatori su DspF?
012900101104     d IndDspF         ds                  inz
013000101104         // -?Abilitazione tasti funzionali?
013100101104     d   F5Attivo                      n   overlay(IndDspF : 05)
013200101104     d   F6Attivo                      n   overlay(IndDspF : 06)
013300101104     d   F16Attivo                     n   overlay(IndDspF : 16)
013400101104         // -?Emissione messaggio di errore?
013500101104     d   ErrMessage                    n   overlay(IndDspF : 28)
013600101104         // -?Indicatori per Attibuti di visualizzazione?
013700101104     d   ProtectKey                    n   overlay(IndDspF : 40)
013800101104         // -?Posizionamento cursore & Segnalazione errore?
013900101104     d   PosCurCLI                     n   overlay(IndDspF : 50)
014000101104     d   PosCurCSD                     n   overlay(IndDspF : 51)
014100101104     d   PosCurCDM                     n   overlay(IndDspF : 52)
014200101104     d   PosCurCKS                     n   overlay(IndDspF : 53)
014300101104     d   PosCurPRF                     n   overlay(IndDspF : 54)
014400101104         //   -?Riemissione videata?
014500101104     d   ErrGenerico                   n   overlay(IndDspF : 99)
014600101104
014700101104       // -?Parametri ricevuti?
014800101104     d*// KPJBA         e ds
014900101104     d TRTB83ds      e ds
015000101104
015100101104       // -?Parametri per Reperimento dati utente?
015200101104     d TIBS34ds      e ds                  inz
015300101104
015400101104       // -?Tabelle usate:?
015500101104       // ·?"3Q" = Tempi fissi calcolo delivery?
015600101104     d ds3Q          e ds                  inz
015700101104
015800101104       // -?Ridefinizione elenco librerie in elaborare le tabelle?
015900101104     d ds_Libr         ds                  inz
016000101104     d  $Libr                        10    dim(c_NrLibr) inz
016100101104
016200101104       //--------------------------------------------------------------
016300101104       //?Definizione variabili globali.                               ?
016400101104       //--------------------------------------------------------------
016500101104
016600101104       // -?Flags booleani?
016700101104     d $Fine           s               n   inz
016800101104     d $InzD02         s               n   inz(*on)
016900101105
017000101105       // -?Indici di schiera?
017100101105     d xx              s              3  0 inz
017200101104
017300101104       // -?Variabili per la gestione del video?
017400101104     d $Video          s              2    inz('D2')
017500101104
017600101104       // -?Nome del sistema?
017700101104     d currSysNeta     s              8a   inz
017800101104
017900101104       // -?Nome esteso Libreria/File del file tabella?
018000101104     d wLibFile        s             21a   inz
018100101104
018200101104       // -?Campi di comodo?
018300101105     d wDate_EUR       s               d   datfmt(*eur)  inz
018400101105     d wDate           s              8  0 inz
018500101104     d w_iSystem       s              1  0 inz
018600101104     d w_SisInf        s              3  0 inz
018700140916     d SaveADB         s                   like(§3QADB)
018800100406
018900100406       //--------------------------------------------------------------
019000100406       //?Definizione prototipi procedure.                             ?
019100100406       //--------------------------------------------------------------
019200100406
019300100406       // -?Reperimento dati utente?
019400100406      /copy gaitrasrc/srcProtoPR,TIBS34R
019500101104
019600101104       // -?Controllo/Decodifica cliente?
019700101104      /copy gaitrasrc/srcProtoPI,TIBS69R
019800101104      /copy gaitrasrc/srcProtoPR,TIBS69R
019900101104
020000101104       // -?Reperimento NETA sistema AS/400 corrente?
020100101104      /copy gaitrasrc/srcProtoPr,UBRTVNETA
020200101104
020300101104       //--------------------------------------------------------------
020400101104       //?Definizione key-list.                                        ?
020500101104       //--------------------------------------------------------------
020600101104
020700101104       // -?File TABEL00F?
020800101104     d k03tabel00    e ds                  extname(TABEL00F : *key)
020900101104     d                                     prefix(k_)   inz
021000101104
021100101104       //--------------------------------------------------------------
021200101104       //?M A I N - L I N E                                            ?
021300101104       //--------------------------------------------------------------
021400101104
021500101104     c     *Entry        plist
021600101104     c                   parm                    TRTB83ds
021700101104
021800101104      /free
021900101104
022000101104       // -?Operazioni iniziali?
022100101104       exsr sr_RoutInz;
022200101104
022300101104       // -?Gestione videate?
022400101104       DoW  $Fine = *off;
022500101104
022600101104         select;
022700101118           // -?Gestione videata D2 (gestione dati tabella)?
022800101104           when $Video = 'D2';
022900101104             exsr sr_GesD02;
023000101104           // -?? ? ? ? ??
023100101104           other;
023200101104             $Fine = *on;
023300101104         endsl;
023400101104
023500101104       enddo;
023600101104
023700101104       // -?Operazioni finali?
023800101104       exsr sr_RoutEnd;
023900101104
024000101104       //--------------------------------------------------------------
024100101104       //?Operazioni iniziali.                                         ?
024200101104       //--------------------------------------------------------------
024300101104       BEGSR  sr_RoutInz;
024400101104
024500101104         *inLR = *on;
024600101104
024700101104         // -?Verifica del sistema AS/400 corrente?
024800101104         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
024900101104           $Fine = *on;
025000101104           leavesr;
025100101104         endif;
025200101104
025300101104         // -?Impostazione elenco librerie in cui gestire le tabelle?
025400101104         //  ?(a seconda del sistema in cui si stà lavorando)?
025500101104         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
025600101104         if  w_iSystem = *zero;
025700101104           $Fine = *on;
025800101104           leavesr;
025900101104         endif;
026000101104
026100101104         // -?Reperimento dati job?
026200101104         exsr  sr_DatiJob;
026300101105
026400101105         // -?Reperimento data odierna?
026500101105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
026600101105                               : 1 : 8 ) );
026700101104
026800101104         // -?Impostazione nome programma a video?
026900101104         V1Tpgm = SDSpgm;
027000101104
027100101104         // -?Impostazione campi "fissi"?
027200101104         k_TBLkut = c_Kut;
027300101104         k_TBLcod = c_Tab;
027400101104
027500101104         // -?Decodifica opzione richiesta?
027600101105         xx = %lookup( D83op1 : $Opz );
027700101105         if  xx > *zero;
027800101105           VTDfun = $OpzD(xx);
027900101105         else;
028000101105           $Fine  = *on;
028100101105           D83msg = $Msg(01);
028200101105           leavesr;
028300101105         endif;
028400101104
028500101104       ENDSR;
028600101104
028700101104       //--------------------------------------------------------------
028800101104       //?Reperimento Dati del job (Utente/Operativi).                 ?
028900101104       //--------------------------------------------------------------
029000101104       BEGSR  sr_DatiJob;
029100101104
029200101104         in(e) §AzUte;
029300101104         if NOT %error;
029400101104           in(e) §DatiUte;
029500101104         endif;
029600101104         if %error or RSut = *blank;
029700101104           tibs34r ( tibs34ds );
029800101104           in §AzUte;
029900101104           in §DatiUte;
030000101104         endif;
030100101104
030200101104       ENDSR;
030300101104
030400101104       //--------------------------------------------------------------
030500101104       //?Operazioni finali.                                           ?
030600101104       //--------------------------------------------------------------
030700101104       BEGSR  sr_RoutEnd;
030800101104
030900101104         clear  TIBS69ds;
031000101104         I69tla = 'C';
031100101104         tibs69r( tibs69ds :
031200101104                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
031300101104
031400101104         return;
031500101104
031600101104       ENDSR;
031700101104
031800101104       //--------------------------------------------------------------
031900101104       //?Gestione videata D02.                                        ?
032000101104       //--------------------------------------------------------------
032100101104       BEGSR  sr_GesD02;
032200101104
032300101104         // -?Inizializzazione videata?
032400101104         if  $InzD02 = *on;
032500101104           exsr  sr_InzD02;
032600101104           $InzD02 = *off;
032700101104         endif;
032800101104
032900101104         // -?(Ri)Abilitazione tasti funzionali?
033000101104         exsr  sr_AbilFxxD02;
033100101104
033200101104         // -?Emissione Testata e Piede?
033300101104         write  TB84T1;
033400101104         write  TB84P2;
033500101104
033600101104         // -?Emissione videata?
033700101104         if  D83op1 <> 'O05';
033800101104           exfmt  TB84D2;
033900101104         else;
034000101104           write  TB84D2;
034100101104           exfmt  Protect;
034200101104         endif;
034300101104
034400101104         clear  V1Dmsg;
034500101104         reset  ErrMessage;
034600101104         reset  ErrGenerico;
034700101104
034800101104         SELECT;
034900101104
035000101104           // -?F3=Fine?
035100101104           WHEN  dsp_aid = c_F03;
035200101104             unlock  TABEL00F;
035300101104             D83F03 = *on;
035400101104             $Fine  = *on;
035500101104
035600101104           // -?F12=Ritorno?
035700101104           WHEN  dsp_aid = c_F12;
035800101104             unlock  TABEL00F;
035900101104             $Fine = *on;
036000101104
036100101104           // -?Invio in Visualizzazione?
036200101104           WHEN  D83op1  = 'O05'   and
036300101104                 dsp_aid = c_Enter;
036400101104             $Fine = *on;
036500101104
036600101104           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
036700101104           OTHER;
036800101104             // -?Controlli eseguiti NON se F16=Annullamento?
036900101104             if  dsp_aid <> c_F16;
037000101104               exsr sr_CtrD02;
037100101104             endif;
037200101104             if  ErrGenerico = *on;
037300101104               leavesr;
037400101104             endif;
037500101105             // -?Aggiornamento?
037600101105             If  dsp_aid = c_F05   or
037700101104                 dsp_aid = c_F06   or
037800101104                 dsp_aid = c_F16;
037900101105               exsr  sr_UpdateAll;
038000101104               if  ErrGenerico = *on;
038100101104                 leavesr;
038200101104               endif;
038300101104               // ·?Uscita dal programma?
038400101104               $Fine = *on;
038500101105             EndIf;
038600101105
038700101105         ENDSL;
038800101104
038900101104       ENDSR;
039000101104
039100101104       //--------------------------------------------------------------
039200101104       //?Inizializzazione videata D02.                                ?
039300101104       //--------------------------------------------------------------
039400101104       BEGSR  sr_InzD02;
039500101104
039600101104         clear  TB84D2;
039700101104
039800101104         // -?Apertura file TABEL00F del 1° S.I. (sede)?
039900101104         if  w_SisInf <> 1   or   Not %open(TABEL00F);
040000101104           w_SisInf = 1;
040100101104           exsr  sr_OpenFileTab;
040200101104         endif;
040300101105
040400101105         // -?Eventuale protezione del campo chiave (SE NON inserim.)?
040500101105         ProtectKey = ( D83op1 = 'O02'  or  D83op1 = 'O04'  or
040600101105                        D83op1 = 'O05'  or  D83op1 = 'O07' );
040700101104
040800101104         // -?Verifica esistenza del record in TABEL?
040900101105         //  ?SE NON richiamato in INSERIMENTO?
041000101104         k_TBLkey = D83cli;
041100101105         select;
041200101105           when  D83op1 = 'F06';
041300170321             //imposto dft per inserimento
041400170321             V1Ccsd = 'WW';
041500101105             leavesr;
041600101105           when  D83op1 = 'O03'  or  D83op1 = 'O05';
041700101105             chain(n)  %kds(k03tabel00)  TABEL;
041800101105           other;
041900101108             chain(e)  %kds(k03tabel00)  TABEL;
042000101105         endsl;
042100101104
042200101104         // -?SE il record non esiste o è allocato?
042300101104         //  ?=> viene restituito un messaggio di errore al chiamante?
042400101104         select;
042500101104           when  Not %found(TABEL00F);
042600101104             $Fine  = *on;
042700101105             D83msg = $Msg(02);
042800101104             leavesr;
042900101119           when  D83op1 <> 'O03'  and  D83op1 <> 'O05'  and
043000101119                 %error;
043100101104             $Fine  = *on;
043200101105             D83msg = $Msg(03);
043300101104             leavesr;
043400101104         endsl;
043500101104
043600101105         // -?Impostazione campi a video?
043700101104         V1Ccli = %int(D83cli);
043800101104         ds3Q = TBLuni;
043900101104         V1Ccks = §3Qcks;
044000101104         V1Ccdm = §3Qcdm;
044100101105         V1Ccsd = §3Qcsd;
044200101104         V1Cprf = §3Qprf;
044300140915         V1Cadb = §3Qadb;
044400140917         SaveADB = §3Qadb;
044500140915         V1Cads = §3Qads;
044600101104         V1Cvto = §3Qvto;
044700101104         V1Cflg = TBLflg;
044800101104
044900101105         // -?Decodifica clienti?
045000101104         exsr  sr_DecodCli;
045100101104
045200101104       ENDSR;
045300101105
045400101105       //--------------------------------------------------------------
045500101105       //?Apertura dei files tabelle nel sistema informativo impostato.?
045600101105       //--------------------------------------------------------------
045700101105       BEGSR  sr_OpenFileTab;
045800101105
045900101105         // -?Chiusura (eventuale) archivi?
046000101105         if  %open(TABEL00F);
046100101105           close  TABEL00F;
046200101105         endif;
046300101105
046400101105         // -?Apertura archivi?
046500101105         ds_Libr  = $Libraries(w_iSystem);
046600101105         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
046700101105         open  TABEL00F;
046800101105
046900101105       ENDSR;
047000101105
047100101105       //--------------------------------------------------------------
047200101105       //?Controllo / Decodifica clienti                               ?
047300101105       //--------------------------------------------------------------
047400101105       BEGSR  sr_DecodCli;
047500101105
047600101105         // -?Controllo / decodifica cliente codice tabella?
047700101105         IF  V1Ccli > *zero;
047800101105           clear  V1Dcli;
047900101105           clear  TIBS69ds;
048000101105           I69sif = knsif;
048100101105           I69kcc = DUTkci;
048200101105           I69kac = %int( V1Ccli );
048300101105           tibs69r( tibs69ds :
048400101105                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
048500101105           if  O69err = *on;
048600101105             V1Dmsg = $Msg(06);
048700101105             PosCurCLI   = *on;
048800101105             ErrMessage  = *on;
048900101105             ErrGenerico = *on;
049000101105             leavesr;
049100101105           endif;
049200101105           V1Dcli = ACOrag;
049300101105         ELSE;
049400101105           V1Dcli = *all'? ';
049500101105         ENDIF;
049600101105
049700101105         // -?Controllo / decodifica cliente unificante?
049800101105         IF  V1Ccks > *zero;
049900101105           clear  V1Dcks;
050000101105           if  V1Ccks <> I69kac;
050100101105             clear  TIBS69ds;
050200101105             I69sif = knsif;
050300101105             I69kcc = DUTkci;
050400101105             I69kac = V1Ccks;
050500101105             tibs69r( tibs69ds :
050600101105                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
050700101105           endif;
050800101105           if  O69err = *on;
050900101105             V1Dmsg = $Msg(06);
051000101105             PosCurCKS   = *on;
051100101105             ErrMessage  = *on;
051200101105             ErrGenerico = *on;
051300101105             leavesr;
051400101105           endif;
051500101105           V1Dcks = ACOrag;
051600101105         ENDIF;
051700101105
051800101105       ENDSR;
051900101104
052000101104       //--------------------------------------------------------------
052100101104       //?Abilitazione tasti funzionali in P02 (per D02).              ?
052200101104       //--------------------------------------------------------------
052300101104       BEGSR  sr_AbilFxxD02;
052400101104
052500101104         // ->?F5 = Ripristino?
052600101104         F5Attivo = ( %found(TABEL00F)   and   TBLflg <> *blank   and
052700101104                      ( D83op1 = 'O07'   or    D83op1 =  'O02' ) );
052800101104
052900101104         // ->?F6 = Conferma?
053000101104         F6Attivo = ( (( %found(TABEL00F)  and  TBLflg = *blank )  OR
053100101104                      Not %found(TABEL00F) )                      and
053200101104                      ( D83op1 <> 'O04'    and  D83op1 <> 'O05'   and
053300101104                        D83op1 <> 'O07' ) );
053400101104
053500101104         // ->?F16 = Annullamento?
053600101104         F16Attivo = ( %found(TABEL00F)   and   TBLflg = *blank   and
053700101104                       ( D83op1 = 'O04'   or    D83op1 = 'O02' ) );
053800101104
053900101104       ENDSR;
054000101104
054100101104       //--------------------------------------------------------------
054200101104       //?Controllo dati videata D02.                                  ?
054300101104       //--------------------------------------------------------------
054400101104       BEGSR  sr_CtrD02;
054500101104
054600101105         // -?Controllo codice in caso di inserimento o copia?
054700101105         if  (D83Op1 = 'F06'   or   D83op1 = 'O03');
054800101105           // -?Controllo immisione codice?
054900101105           if  V1Ccli <= *zero;
055000101105             V1Dmsg = $Msg(04);
055100101105             PosCurCLI   = *on;
055200101105             ErrMessage  = *on;
055300101105             ErrGenerico = *on;
055400101105             leavesr;
055500101105           endif;
055600101105           // -?Controllo esistenza nuovo codice?
055700101105           k_TBLkey = %editc( V1Ccli : 'X' );
055800101105           setll  %kds(k03tabel00)  TABEL;
055900101105           if  %equal(TABEL00F);
056000101105             V1Dmsg = $Msg(05);
056100101105             PosCurCLI   = *on;
056200101105             ErrMessage  = *on;
056300101105             ErrGenerico = *on;
056400101105             leavesr;
056500101105           endif;
056600101105         endif;
056700101105
056800101105         // -?Controllo / Decodifica clienti?
056900101105         exsr  sr_DecodCli;
057000101105         if  ErrMessage;
057100101105           leavesr;
057200101105         endif;
057300101105
057400101105         SELECT;
057500101105
057600101105           // -?Tipo trasmissione obbligatorio?
057700170321           WHEN  V1Ccsd = *blank;
057800101105             V1Dmsg = $Msg(07);
057900101105             PosCurCSD   = *on;
058000101105             ErrMessage  = *on;
058100101105             ErrGenerico = *on;
058200101105             leavesr;
058300101105
058400101105           // -?Codice cliente unificante obbligatorio?
058500101105           WHEN  V1Ccks = *zero;
058600101105             V1Dmsg = $Msg(07);
058700101105             PosCurCKS   = *on;
058800101105             ErrMessage  = *on;
058900101105             ErrGenerico = *on;
059000101105             leavesr;
059100101105
059200101105           // -?Tipo trasmissione "TR": profilo & nome file obbligatori?
059300101105           WHEN  V1Ccsd = 'TR'  and  (V1Cprf = *blank   or
059400101105                                      V1Ccdm = *blank);
059500101105             V1Dmsg = $Msg(09);
059600101105             if  V1Cprf = *blank;
059700101105               PosCurPRF = *on;
059800101105             else;
059900101105               PosCurCDM = *on;
060000101105             endif;
060100101105             ErrMessage  = *on;
060200101105             ErrGenerico = *on;
060300101105             leavesr;
060400101105
060500101105         endsl;
060600140915
060700140915         // se Invio anticipato dati su bollettazione = 'S', viene
060800140916         // proposto a SI anche quello per spunta
060900140916         if V1CADB = 'S' and SaveADB = *blank;
061000140916           V1CADS = 'S';
061100140916           SaveADB = 'S';
061200140915         endif;
061300140916         if V1CADB = *blank and SaveADB = 'S';
061400140916           SaveADB = *blank;
061500140916         endif;
061600140916
061700140916         // se Invio anticipato dati su spunta = 'S', essendo questo
061800140916         // un "di cui" della bollettazione, viene forzato a SI anche quello
061900140916         if V1CADS = 'S';
062000140916           V1CADB = 'S';
062100140916         endif;
062200101105
062300101104       ENDSR;
062400101105
062500101105       //--------------------------------------------------------------
062600101105       //?Aggiornamento/Inserimento record nel file TABEL00F.          ?
062700101105       //--------------------------------------------------------------
062800101105       BEGSR  sr_UpdateAll;
062900101105
063000101105         // -?Ciclo di elaborazione per ogni sistema informativo?
063100101105         For  w_SisInf = 1  To  %elem($Libr);
063200101105
063300101105           // -?Apertura degli archivi?
063400101105           if  w_SisInf > 1;
063500101105             exsr  sr_OpenFileTab;
063600101105           endif;
063700101105
063800101105           // -?Aggiornamento tab. "3Q"?
063900101105           exsr  sr_UpdTABEL;
064000101105
064100101105         EndFor;
064200101105
064300101105       ENDSR;
064400101105
064500101105       //--------------------------------------------------------------
064600101105       //?Aggiornamento record nel file TABEL00F.                      ?
064700101105       //--------------------------------------------------------------
064800101105       BEGSR  sr_UpdTABEL;
064900101105
065000101105         //? N.B.                                                      ?
065100101105         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
065200101105         //   trasmissione (vedi flag TBEFTR).                        ?
065300101105         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
065400101105         //   subito nello stesso file di entrambi i S.I. - in due    ?
065500101105         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
065600101105
065700101105         // -?Aggiornam. tab. "3Q" = Invio a clienti FNVAB (da fil.)?   ?
065800101105
065900101105         // -?RI-aggancio record da aggiornare?
066000101105         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
066100101105         //  ?(o se in INSERIMENTO o se ricevuta opzione di COPIA)?
066200101105         if  w_SisInf > 1    or
066300101105             D83Op1 = 'F06'  or  D83op1 = 'O03';
066400101105           k_TBLkey = %editc( V1Ccli : 'X' );
066500101105           chain  %kds(k03tabel00)  TABEL;
066600101105         endif;
066700101108
066800101108         if  Not %found(TABEL00F);
066900101108           clear  TABEL;
067000101108           TBLkut = c_Kut;
067100101108           TBLcod = c_Tab;
067200101108           TBLkey = k_TBLkey;
067300101108         endif;
067400101108         exsr  sr_RieDs3Q;
067500101108         TBLuni = ds3Q;
067600101105
067700101105         SELECT;
067800101105
067900101105           // -?F5 = Ripristino?
068000101105           WHEN  dsp_aid = c_F05;
068100101108
068200101105             select;
068300101105               when  %found(TABEL00F)   and   TBLflg = '*';
068400101105                 clear  TBLflg;
068500101105                 clear  TBLftr;
068600101105                 clear  TBLdtr;
068700101105                 //____________
068800101105                 update  TABEL;
068900101105                 //¯¯¯¯¯¯¯¯¯¯¯¯
069000101105               // -?Record appena cancellato fisicamente?
069100101105               when  NOT %found(TABEL00F);
069200101105                 TBLftt = '1';
069300101105                 clear  TBLflt;
069400101105                 if  w_SisInf = 1;
069500101105                   TBLftr = 'T';
069600101105                 else;
069700101105                   TBLftr = 'R';
069800101105                 endif;
069900101105                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
070000101105                                        %len(wDate) - %len(TBLdtr) + 1 :
070100101105                                        %len(TBLdtr) ) );
070200101105                 //____________
070300101105                 write   TABEL;
070400101105                 //¯¯¯¯¯¯¯¯¯¯¯¯
070500101105             endsl;
070600101108
070700101105             // -?Impostazione messaggio per il *pgm chiamante di?
070800101105             //  ?ripristino effettuato?
070900101105             D83msg = $Msg(13);
071000101105
071100101105           // -?F6 = Aggiornamento / Inserimento?
071200101105           WHEN  dsp_aid = c_F06;
071300101105
071400101105             TBLflg = V1Cflg;
071500101105
071600101105             TBLftt = '1';
071700101105             clear  TBLflt;
071800101105             if  w_SisInf = 1;
071900101105               TBLftr = 'T';
072000101105             else;
072100101105               TBLftr = 'R';
072200101105             endif;
072300101105             TBLdtr = %int( %subst( %editc(wDate : 'X') :
072400101105                                    %len(wDate) - %len(TBLdtr) + 1 :
072500101105                                    %len(TBLdtr) ) );
072600101105
072700101105             if  %found(TABEL00F);
072800101105               //____________
072900101105               update  TABEL;
073000101105               //¯¯¯¯¯¯¯¯¯¯¯¯
073100101105             else;
073200101105               //____________
073300101105               write   TABEL;
073400101105               //¯¯¯¯¯¯¯¯¯¯¯¯
073500101105             endif;
073600101105
073700101105             // -?Impostazione messaggio per il *pgm chiamante di?
073800101105             //  ?inserimento/modifica effettuata?
073900101105             if  D83op1 = 'F06'  or   D83op1 = 'O03';
074000101105               D83msg = $Msg(10);
074100101105             else;
074200101105               D83msg = $Msg(11);
074300101105             endif;
074400101105
074500101105           // -?F16 = Annullamento?
074600101105           WHEN  dsp_aid = c_F16;
074700101105
074800101105             if  %found(TABEL00F)   and   TBLflg   = *blank;
074900101105               TBLflg = '*';
075000101105               clear  TBLftr;
075100101105               clear  TBLdtr;
075200101105               //____________
075300101105               update  TABEL;
075400101105               //¯¯¯¯¯¯¯¯¯¯¯¯
075500101105             endif;
075600101105
075700101105             // -?Impostazione messaggio per il *pgm chiamante di?
075800101105             //  ?annullamento effettuato?
075900101105             D83msg = $Msg(12);
076000101105
076100101105         ENDSL;
076200101105
076300101105       ENDSR;
076400101105
076500101105       //--------------------------------------------------------------
076600101105       //?Riempimento struttura dati ds3Q.                             ?
076700101105       //--------------------------------------------------------------
076800101105       BEGSR  sr_RieDs3Q;
076900101105
077000101105         clear  ds3Q;
077100101105
077200101105         §3Qcsd = V1Ccsd;
077300101105         §3Qcdm = V1Ccdm;
077400101105         §3Qcks = V1Ccks;
077500101105         §3Qprf = V1Cprf;
077600140915         §3Qadb = V1Cadb;
077700140915         §3Qads = V1Cads;
077800101105         §3Qvto = V1Cvto;
077900101105
078000101105       ENDSR;
078100101105
078200101105      /end-free
078300101105
078400101105       //--------------------------------------------------------------
078500101105       //?Schiere a tempo di compilazione.                             ?
078600101105       //--------------------------------------------------------------
078700101105
078800101105** -?$iSystem / $Libraries?(Sistemi AS/400 & Librerie con TABEL00F)?
078900101105SETRAS  GAITRAGRU FILTRAGRU
079000101105AS888   GAITRAGRPSFILTRAGRPF
079100101105** -?$Opz / $OpzD?(Opzione richiesta)?
079200101105F06  INSERIMENTO
079300101105O02    MODIFICA
079400101105O03     COPIA
079500101105O04  ANNULLAMENTO
079600101105O05VISUALIZZAZIONE
079700101105O07   RIPRISTINO
079800101105** -?$Msg?(Messaggi di errore)?----------------------------------------------*
079900101105TRTB84R- Opzione errata                                                        1
080000101105TRTB84R- Richiesta funzione per Codice cliente inesistente                     2
080100101105TRTB84R- Codice cliente momentaneamente allocato: riprovare più tardi          3
080200101105TRTB84R- Codice cliente obbligatorio in inserimento                            4
080300101105TRTB84R- Codice cliente già esistente: impossibile l'inserimento               5
080400101105TRTB84R- Codice cliente errato: non reperito in anagrafica                     6
080500101105TRTB84R- CAMPO OBBLIGATORIO !!!!!                                              7
080600101105TRTB84R- Con tipo trasmissione "HL" la postazione halley è obbligatoria        8
080700101105TRTB84R- Con tipo trasmissione "TR" profilo e nome file sono obbligatori       9
080800101105TRTB84R- Inserimento effettuato.                                              10
080900101105TRTB84R- Modifica effettuata.                                                 11
081000101105TRTB84R- Annullamento effettuato.                                             12
081100101105TRTB84R- Ripristino effettuato.                                               13
