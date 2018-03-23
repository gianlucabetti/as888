000100101011       //==============================================================
000200101011       //?TRTB40R * Manutenzione tab. "3K" = Dati per/da clienti       ?
000300101011       //==============================================================
000400110811
000500110811      *?  ATTENZIONE!!  ?
000600110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110811      *?    'Interrogazione abilitazioni clienti'              ?
000800110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110811      *?    verificare anche relativo pgm di interrogazione    ?
001000110811      *?    per le abilitazioni clienti --> TRTB401R VAC       ?
001100110811      *?                                    TRTB402R VAG       ?
001200110811      *?                                    TRTB403R VAI       ?
001300101011
001400101011       //--------------------------------------------------------------
001500101011       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001600101011       //--------------------------------------------------------------
001700101011
001800101011     /*PRM dbgview(*source)
001900101011     /*END
002000101011
002100101011       //--------------------------------------------------------------
002200101011       //?Specifiche di controllo.                                     ?
002300101011       //--------------------------------------------------------------
002400101011
002500101011     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600101011     h dftactgrp(*no)
002700101011     h bnddir('UBBNDDIR':'TRUL')
002800101011
002900101011       //--------------------------------------------------------------
003000101011       //?Dichiarazione file.                                          ?
003100101011       //--------------------------------------------------------------
003200101011
003300101011       // -?Organigramma filiale/aziendale?
003400101011     fAZORG01L  if   e           k disk
003500101011
003600101011       // -?Tabelle per E.D.I. estero?
003700101011     fEDTAB01L  if   e           k disk
003800101011
003900101011       // -?Tabella "3K"?
004000101011     fTABEL00F  Uf A e           k disk
004100101012     f                                     extfile(wLibFile)  usropn
004200101011
004300101011       // -?Video Gestione?
004400101011     fTRTB40D   cf   e             workstn
004500101011     f                                     indds(IndDspF)
004600101011     f                                     infds(InfDspF)
004700101011
004800101011       //--------------------------------------------------------------
004900101011       //?Definizione costanti.                                        ?
005000101011       //--------------------------------------------------------------
005100101011
005200101011       // -?Codice tabella in gestione?
005300101012     d c_Tab           c                   const('3K')
005400101011
005500101011       // -?Codice utente?
005600101011     d c_Kut           c                   const(1)
005700101011
005800101012       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
005900101012     d c_TABEL         c                   const('1')
006000101012       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
006100101012     d c_TNTBE         c                   const('2')
006200101011
006300101011       // -?Costante per controllo "caratteri solo numerici"?
006400101011     d c_Digits        c                   const('0123456789')
006500101011
006600101011       // -?Costanti per la definizione delle schiere con i nomi?
006700101011       //  ?degli iSeries da elaborare e delle relative librerie?
006800101011     d c_NrSyst        c                   const(2)
006900101011     d c_NrLibr        c                   const(2)
007000101011
007100101011       // -?Tasti funzionali a video?
007200101011     d c_F01           c                   const(x'31')
007300101011     d c_F02           c                   const(x'32')
007400101011     d c_F03           c                   const(x'33')
007500101011     d c_F04           c                   const(x'34')
007600101011     d c_F05           c                   const(x'35')
007700101011     d c_F06           c                   const(x'36')
007800101011     d c_F07           c                   const(x'37')
007900101011     d c_F08           c                   const(x'38')
008000101011     d c_F09           c                   const(x'39')
008100101011     d c_F10           c                   const(x'3A')
008200101011     d c_F11           c                   const(x'3B')
008300101011     d c_F12           c                   const(x'3C')
008400101011     d c_F13           c                   const(x'B1')
008500101011     d c_F14           c                   const(x'B2')
008600101011     d c_F15           c                   const(x'B3')
008700101011     d c_F16           c                   const(x'B4')
008800101011     d c_F17           c                   const(x'B5')
008900101011     d c_F18           c                   const(x'B6')
009000101011     d c_F19           c                   const(x'B7')
009100101011     d c_F20           c                   const(x'B8')
009200101011     d c_F21           c                   const(x'B9')
009300101011     d c_F22           c                   const(x'BA')
009400101011     d c_F23           c                   const(x'BB')
009500101011     d c_F24           c                   const(x'BC')
009600101011     d c_Enter         c                   const(x'F1')
009700101011     d c_RollDown      c                   const(x'F4')
009800101011     d c_RollUp        c                   const(x'F5')
009900101011
010000101011       //--------------------------------------------------------------
010100101011       //?Definizione schiere.                                         ?
010200101011       //--------------------------------------------------------------
010300101011
010400101011       // -?iSeries  &  Librerie con entrambi i file tabelle?
010500101011     d $iSystem        s                   like(currSysNetA)
010600101011     d                                     dim(c_NrSyst)
010700101011     d                                     ctdata   perrcd( 1)
010800101011     d $Libraries      s                   like(ds_Libr)
010900101012     d                                     dim(c_NrLibr)
011000101011     d                                     alt($iSystem)
011100101011
011200101011       // -?Messaggi di errore?
011300101012     d $Msg            s             78    dim(17) ctdata perrcd(1)
011400101011
011500101011       //--------------------------------------------------------------
011600101011       //?Definizione aree dati.                                       ?
011700101011       //--------------------------------------------------------------
011800101011
011900101011       // -?Dati utente?
012000101011     d §AzUte        e ds                  extname(AZUTE00F)
012100101011     d                                     dtaara
012200101011     d §DatiUte      e ds                  extname(dDatiUte)
012300101011     d                                     dtaara
012400101011
012500101011       //--------------------------------------------------------------
012600101011       //?Definizione strutture dati.                                  ?
012700101011       //--------------------------------------------------------------
012800101011
012900101011       // -?Status ds?
013000101011     d Status         sds
013100101011     d  SDSpgm           *proc
013200101011
013300101011       // -?InfDS?
013400101011     d InfDspF         ds
013500101011     d   dsp_aid             369    369a
013600101011
013700101011       // -?Indicatori su DspF?
013800101011     d IndDspF         ds                  inz
013900101011         // -?Abilitazione tasti funzionali?
014000101011     d   F5Attivo                      n   overlay(IndDspF : 05)
014100101011     d   F6Attivo                      n   overlay(IndDspF : 06)
014200101011     d   F12Attivo                     n   overlay(IndDspF : 12)
014300101011     d   F16Attivo                     n   overlay(IndDspF : 16)
014500101011     d   F21Attivo                     n   overlay(IndDspF : 21)
014600101011     d   F22Attivo                     n   overlay(IndDspF : 22)
014700101011         // -?Emissione messaggio di errore?
014800101011     d   ErrMessage                    n   overlay(IndDspF : 28)
014900101011         // -?Indicatori per Attibuti di visualizzazione?
015000101011     d   VisBloAnn                     n   overlay(IndDspF : 40)
015100101011     d   ProtectAll                    n   overlay(IndDspF : 41)
015200101011         // -?Posizionamento cursore & Segnalazione errore?
015300101011     d   PosCurLNP                     n   overlay(IndDspF : 51)
015400101011     d   PosCurNUP                     n   overlay(IndDspF : 52)
015500101011     d   PosCurFIL                     n   overlay(IndDspF : 53)
015600101011     d   PosCurCQ1                     n   overlay(IndDspF : 54)
015700101011     d   PosCurVTO                     n   overlay(IndDspF : 55)
015800101011     d   PosCurCGC                     n   overlay(IndDspF : 56)
015900101011     d   PosCurCSD                     n   overlay(IndDspF : 57)
016000101011     d   PosCurCKS                     n   overlay(IndDspF : 59)
016100101011     d   PosCurGQI                     n   overlay(IndDspF : 60)
016200101011     d   PosCurAPP                     n   overlay(IndDspF : 61)
016300101011     d   PosCurGGC                     n   overlay(IndDspF : 62)
016400101011     d   PosCurGSD                     n   overlay(IndDspF : 63)
016500101011     d   PosCurGKS                     n   overlay(IndDspF : 65)
016600101011     d   PosCurGSR                     n   overlay(IndDspF : 66)
016700101011         //   -?Riemissione videata?
016800101011     d   ErrGenerico                   n   overlay(IndDspF : 99)
016900101011
017000101011       // -?Parametri ricevuti?
017100101011     d KPJBA         e ds
017200101011
017300101011       // -?Parametri per Reperimento dati utente?
017400101011     d TIBS34ds      e ds                  inz
017500101011
017600101011       // -?Tabelle usate:?
017700101011       // ·?"3Q" = Dati per/da clienti?
017800101011     d ds3K          e ds                  inz
017900101115     d ds3K_Memo     e ds                  inz  extname(ds3K)
018000101115     d                                          qualified
018100101011       // ·?"3T" = Tipo scarico?
018200101011     d ds3T          e ds                  inz
018300101011       // ·?"LAT" = Funzione aziendale?
018400101011     d dLAT          e ds                  inz
018500101011
018600101011       // -?Dati partner E.D.I. Estero?
018700101011     d EDIdsPT       e ds                  inz
018800101011
018900101011       // -?Ridefinizione flag "Invio dati consegna"?
019000101012     d ds_VI2CQ        ds                  inz
019100101011     d  VI2cq1                             inz
019200101011     d  VI2cq2                             inz
019300101011     d  VI2cq4                             inz
019400101011     d  VI2cq5                             inz
019500101011     d  VI2cq6                             inz
019600101011     d  VI2cq7                             inz
019700101011     d  VI2cmA                             inz
019800101011     d  VI2cbA                             inz
019900101011     d  VI2cq8                             inz
020000101011
020100101011       // -?Ridefinizione elenco librerie in elaborare le tabelle?
020200101011     d ds_Libr         ds                  inz
020300101011     d  $Libr                        10    dim(c_NrLibr) inz
020400101011
020500101011       //--------------------------------------------------------------
020600101011       //?Definizione variabili globali.                               ?
020700101011       //--------------------------------------------------------------
020800101011
020900101011       // -?Flags booleani?
021000101011     d $Fine           s               n   inz
021100101011     d $InzD01         s               n   inz(*on)
021200101011     d $InzD02         s               n   inz(*on)
021300101011     d $Called         s               n   inz
021400101012     d $Found_PT       s               n   inz
021500101011
021600101011       // -?Variabili per la gestione del video?
021700101011     d $Video          s              2    inz('D1')
021800101011
021900101011       // -?Codice cliente eventualmente ricevuto?
022000101011     d W0Ccli          s              7a   inz
022100101012
022200101012       // -?Flag annullamento tabella (per TRULTAB)?
022300101012     d wTBEatb         s                   like(TBLflg)  inz
022400101011
022500101011       // -?Memo...?
022600101011     d SaveNMC         s                   like(VI2NMC)  inz
022700101011
022800101011       // -?Nome del sistema?
022900101011     d currSysNeta     s              8a   inz
023000101011
023100101011       // -?Nome esteso Libreria/File del file tabella?
023200101011     d wLibFile        s             21a   inz
023300101011
023400101011       // -?Campi di comodo?
023500101012     d wDate_EUR       s               d   datfmt(*eur)  inz
023600130527     d wDate_DMY       s               d   datfmt(*dmy)  inz
023700130527     d wDate6          s              6  0 inz
023800101012     d wDate           s              8  0 inz
023900101011     d w_iSystem       s              1  0 inz
024000101011     d w_SisInf        s              3  0 inz
024100101011
024200101011       //--------------------------------------------------------------
024300101011       //?Definizione prototipi procedure.                             ?
024400101011       //--------------------------------------------------------------
024500101011
024600101011       // -?Reperimento dati utente?
024700101011      /copy gaitrasrc/srcProtoPR,TIBS34R
024800101011
024900101011       // -?Parametri per pgm interrogazione tabelle (TABEL00F)?
025000101011      /copy gaitrasrc/srcProtoPI,X§TABER
025100101011       // -?Interrogazione tabelle?
025200101011      /copy gaitrasrc/srcProtoPR,X§TABER
025300101012
025400101012       // -?Reperimento dati tabelle?
025500101012      /copy gaitrasrc/srcProtoPR,TRULTAB
025600101011
025700101011       // -?Parametri per pgm interrogazione sottoconti?
025800101011      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
025900101011       // -?Interrogazione sottoconti?
026000101011      /copy gaitrasrc/srcProtoPR,XALFA3BR
026100101012
026200101012       // -?Controllo/Decodifica cliente?
026300101105      /copy gaitrasrc/srcProtoPI,TIBS69R
026400101012      /copy gaitrasrc/srcProtoPR,TIBS69R
026500130527
026600130527       // -?Parametri per pgm ricerca data blocco cliente?
026700130527     d TRULBLCds     e ds                  inz
026800130527      /copy gaitrasrc/srcProtoPR,TRULBLCR
026900101011
027000101011       // -?Gestione invio dati - BOLLETTAZIONE?
027100101012     d trtb28r         pr                  extpgm('TRTB28R')
027200101012     d   kpjba                             likeds(kpjba)
027300101011
027400101011       // -?Gestione scambio dati C/A e fattura con clienti?
027500101011      /copy gaitrasrc/srcProtoPR,TRTB44R
027600101011
027700101011       // -?Gestione scambio dati principali?
027800101011      /copy gaitrasrc/srcProtoPR,TRTB47R
027900101012
028000101012       // -?Reperimento NETA sistema AS/400 corrente?
028100101012      /copy gaitrasrc/srcProtoPr,UBRTVNETA
028200101012
028300101012       // -?Parametri API QCAPCMD (Process Commands)?
028400101012     d Qcmd            s           2048    inz  varying
028500101012      /copy qSysInc/qRpgleSrc,QCAPCMD
028600101012       // -?API QCAPCMD (Process Commands)?
028700101012      /copy gaitrasrc/srcProtoPR,QCAPCMD
028800101012
028900101012       // -?Parametri gestione errori API.?
029000101012      /copy qsysinc/qrpglesrc,QUSEC
029100101011
029200101011       //--------------------------------------------------------------
029300101011       //?Definizione key-list.                                        ?
029400101011       //--------------------------------------------------------------
029500101011
029600101011       // -?File TABEL00F?
029700101011     d k03tabel00    e ds                  extname(TABEL00F : *key)
029800101011     d                                     prefix(k_)   inz
029900101011
030000101011       //--------------------------------------------------------------
030100101011       //?M A I N - L I N E                                            ?
030200101011       //--------------------------------------------------------------
030300101011
030400101011     c     *Entry        plist
030500101011     c                   parm                    KPJBA
030600101011
030700101011      /free
030800101011
030900101011       // -?Operazioni iniziali?
031000101011       exsr sr_RoutInz;
031100101011
031200101011       // -?Gestione videate?
031300101011       DoW  $Fine = *off;
031400101011
031500101011         select;
031600101011           // -?Gestione videata D1 (richiesta codice cliente)?
031700101011           when $Video = 'D1';
031800101011             exsr sr_GesD01;
031900101011           // -?Gestione videata D2 (manutenzione dati cliente)?
032000101011           when $Video = 'D2';
032100101011             exsr sr_GesD02;
032200101011           // -?? ? ? ? ??
032300101011           other;
032400101011             $Fine = *on;
032500101011         endsl;
032600101011
032700101011       enddo;
032800101011
032900101011       // -?Operazioni finali?
033000101011       exsr sr_RoutEnd;
033100101011
033200101011       //--------------------------------------------------------------
033300101011       //?Operazioni iniziali.                                         ?
033400101011       //--------------------------------------------------------------
033500101011       BEGSR  sr_RoutInz;
033600101011
033700101011         *inLR = *on;
033800101011
033900101011         // -?Verifica del sistema AS/400 corrente?
034000101011         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
034100101011           $Fine = *on;
034200101011           leavesr;
034300101011         endif;
034400101011
034500101011         // -?Impostazione elenco librerie in cui gestire le tabelle?
034600101011         //  ?(a seconda del sistema in cui si stà lavorando)?
034700101011         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
034800101011         if  w_iSystem = *zero;
034900101011           $Fine = *on;
035000101011           leavesr;
035100101011         endif;
035200101011
035300101011         // -?Reperimento dati job?
035400101011         exsr  sr_DatiJob;
035500101105
035600101105         // -?Reperimento data odierna?
035700101105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
035800101105                               : 1 : 8 ) );
035900101011
036000101011         // -?Impostazione nome programma a video?
036100101011         V1Tpgm = SDSpgm;
036200101011
036300101011         // -?Impostazione campi "fissi"?
036400101011         k_TBLkut = c_Kut;
036500101011         k_TBLcod = c_Tab;
036600101011
036700101011         // -?Verifica se *pgm richiamato:?
036800101011         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
036900101011         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
037000101011         clear  W0Ccli;
037100101011         $Called = (%subst( KPJBU : 1 : %len(W0Ccli) ) <> *blank);
037200101011         if  $Called = *on   and
037300101011             %check( c_Digits : %subst( KPJBU : 1 :
037400101011                                       %len(W0Ccli) ) ) = *zero;
037500101011           W0Ccli = %subst( KPJBU : 1 : %len(W0Ccli) );
037600101011         endif;
037700101011
037800101011       ENDSR;
037900101011
038000101011       //--------------------------------------------------------------
038100101011       //?Reperimento Dati del job (Utente/Operativi).                 ?
038200101011       //--------------------------------------------------------------
038300101011       BEGSR  sr_DatiJob;
038400101011
038500101011         in(e) §AzUte;
038600101011         if NOT %error;
038700101011           in(e) §DatiUte;
038800101011         endif;
038900101011         if %error or RSut = *blank;
039000101011           tibs34r ( tibs34ds );
039100101011           in §AzUte;
039200101011           in §DatiUte;
039300101011         endif;
039400101011
039500101011       ENDSR;
039600101011
039700101011       //--------------------------------------------------------------
039800101011       //?Gestione videata D01.                                        ?
039900101011       //--------------------------------------------------------------
040000101011       BEGSR  sr_GesD01;
040100101011
040200101011         // -?Inizializzazione videata?
040300101011         if  $InzD01 = *on;
040400101011           exsr  sr_InzD01;
040500101011           $InzD01 = *off;
040600101011         endif;
040700101011
040800101011         // -?SE ricevuto codice cliente NON si emette la prima videata?
040900101011         if  $Called   and   W0Ccli > *zero;
041000101011           exsr  sr_CtrD01;
041100101011           if  Not ErrGenerico;
041200101011             $Video  = 'D2';
041300101011             $InzD02 = *on;
041400101011             leavesr;
041500101011           else;
041600101011             clear  W0Ccli;
041700101011           endif;
041800101011         endif;
041900101011
042000101011         // -?Emissione Testata?
042100101011         write  TB40T1;
042200101011
042300101011         // -?Emissione videata?
042400101011         exfmt  TB40D1;
042500101011
042600101011         reset  ErrMessage;
042700101011         reset  ErrGenerico;
042800101011         clear  VIDmsg;
042900101011
043000101011         SELECT;
043100101011
043200101011           // -?F3=Fine?
043300101011           WHEN  dsp_aid = c_F03;
043400101011             $Fine = *on;
043500101011
043700101011           // -?F21=Gestione scambio dati bolle?
043800101011           // -?F22=Gestione scambio dati C/A e fattura con clienti?
043900140424           WHEN  dsp_aid = c_F21   or
044000101011                 dsp_aid = c_F22;
044100101011             exsr  sr_F2x;
044200101011
044300101011           // -?Invio?
044400101011           OTHER;
044500101011             exsr sr_CtrD01;
044600101011             if  ErrGenerico = *off;
044700101011               $Video    = 'D2';
044800101011               $InzD02   = *on;
044900101011             endif;
045000101011
045100101011         ENDSL;
045200101011
045300101011       ENDSR;
045400101011
045500101011       //--------------------------------------------------------------
045600101011       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
045700101011       //--------------------------------------------------------------
045800101011       BEGSR  sr_InzD01;
045900101011
046000101011         clear  V1Topz;
046100101011
046200101011         clear  TB40D1;
046300101122
046400101122         // -?Se è stato richiamato con un codice cliente numerico, si?
046500101122         //  ?imposta anche tale codice a video?
046600101122         select;
046700101122           when  $Called   and   W0Ccli > *zero;
046800101122             VIDlnp = %subst( W0Ccli : 1 : %len(VIDlnp) );
046900101122             VIDnup = %int( %subst( W0Ccli : %len(VIDlnp) + 1
047000101122                                           : %len(VIDnup) ) );
047100101122           when  Not $Called;
047200101122             VIDlnp = '?';
047300101122         endsl;
047400101011
047500101011         // -?Abilitazione tasti funzionali?
047600101011         exsr  sr_AbilFxxD01;
047700101011
047800101011       ENDSR;
047900101011
048000101011       //--------------------------------------------------------------
048100101011       //?Abilitazione tasti funzionali in P01 (per D01).              ?
048200101011       //--------------------------------------------------------------
048300101011       BEGSR  sr_AbilFxxD01;
048400101011
048500101011         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
048600101011         //  ?c'è il codice cliente => NON devono essere abilitati i?
048700140424         //  ?tasti funzionali  F21 e F22.?
048800101011
049100101011
049200101011         // ->?F21 = Gestione scambio dati bolle?
049300101011         F21Attivo = (Not $Called);
049400101011
049500101011         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
049600101011         F22Attivo = (Not $Called);
049700101011
049800101011       ENDSR;
049900101011
050000101011       //--------------------------------------------------------------
050100140424       //?Gestione tasti funzionali F21 e F22 in D01 & D02.        ?
050200101011       //--------------------------------------------------------------
050300101011       BEGSR  sr_F2x;
050400101012
050500101012         if  %check( c_Digits : VIDlnp ) = *zero;
050600101012           %subst( KPJBU : 1 : 3 ) = VIDlnp;
050700101012         else;
050800101012           %subst( KPJBU : 1 : 3 ) = *zero;
050900101012         endif;
051000101012         %subst( KPJBU : 4 : 4 ) = %editc( VIDnup : 'X' );
051100101011
051200101011         select;
051700101011
051800101011           // -?F21 = Gestione scambio dati ricezione bolle da clienti?
051900101011           when  dsp_aid = c_F21;
052000101011             trtb28r ( kpjba );
052100101011
052200101011           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
052300101011           when  dsp_aid = c_F22;
052400101011             trtb44r ( kpjba );
052500101011
052600101011         endsl;
052700101011
052800101011       ENDSR;
052900101011
053000101011       //--------------------------------------------------------------
053100101011       //?Controllo dati immessi nella videata D01.                    ?
053200101011       //--------------------------------------------------------------
053300101011       BEGSR  sr_CtrD01;
053400101011
053500101011         %subst(IndDspF : 50) = *off;
053600101011         reset  ProtectAll;
053700101011
053800101011         // -?Filiale per ricerca tabella clienti errata?
053900101011         if  VIDfil > *zero;
054000101011           chain  (VIDfil)  AZORG;
054100101011           if  Not %found(AZORG01L);
054200101011             VIDmsg = $Msg(01);
054300101011             PosCurFIL   = *on;
054400101011             ErrGenerico = *on;
054500101011             leavesr;
054600101011           endif;
054700101011         endif;
054800101011
054900101011         SELECT;
055000101011         // -?Ricerca codice cliente in tabella "3K"?
055100101011           WHEN  %scan('?' : VIDlnp) > *zero;
055200101011             clear  VIDlnp;
055300101011             clear  VIDnup;
055400101011             X§Tkut = c_Kut;
055500101011             X§Tcod = c_Tab;
055600101011             if  VIDfil > *zero;
055700101011               X§Tkey = %editc( VIDfil : 'X' );
055800101011             else;
055900101011               clear  X§Tkey;
056000101011             endif;
056100101011             clear  X§Tdes;
056200101011             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
056300101011             if  X§Tkey <> *blank;
056400101011               VIDlnp = %subst(X§Tkey : 1 : %len(VIDlnp));
056500101012               VIDnup = %int( %subst( X§Tkey : %len(VIDlnp) + 1
056600101012                                             : %len(VIDnup) ) );
056700101011             else;
056800101011               VIDlnp = '?  ';
056900101011             endif;
057000101011             PosCurLNP   = *on;
057100101011             ErrGenerico = *on;
057200101011             leavesr;
057300101011
057400101011           // -?Ricerca alfabetica?
057500101011           WHEN  VIDnup =  *zero   and
057600101012                 VIDdes <> *blank;
057700101011             xParDut = RSut;
057800101011             xParKut = 1;
057900101011             xParRag = VIDdes;
058000101011             xParKcc = DUTkci;
058100101011             xParSta = *zero;
058200101012             xParFlr = VIDlnp;
058300101011             clear  xParDit;
058400101011             xParNum = 1;
058500101011             clear  xParKcm;
058600101011             clear  xParKsm;
058700101011             clear  xParKdm;
058800101011             clear  xParEsci;
058900101011             clear  xParErr;
059000101011             clear  xParIva;
059100101011             clear  xParCdf;
059200101011             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
059300101011                        xParSta : xParFlr : xParDit : xParNum :
059400101011                        xParKcm : xParKsm : xParKdm : xParEsci :
059500101011                        xParErr : xParIva : xParCdf );
059600101011             if  xParSta <> -1   and   xParErr = *blank
059700101011                                 and   xParKsm > *zero;
059800101012               VIDlnp = %subst( xParKsm : 1 : %len( VIDlnp ) );
059900101012               VIDnup = %int( %subst( xParKsm : %len( VIDlnp ) + 1
060000101012                                              : %len( VIDnup ) ) );
060100101011               clear  VIDdes;
060200101011             endif;
060300101011             PosCurLNP   = *on;
060400101011             ErrGenerico = *on;
060500101011             leavesr;
060600101011
060700101011           // -?Controllo immissione filiale del codice cliente?
060800101011           WHEN  VIDlnp = *blank  or  VIDlnp = *zero
060900101011                                  or  VIDnup = *zero;
061000101011             VIDmsg = $Msg(02);
061100101011             PosCurLNP   = *on;
061200101011             ErrMessage  = *on;
061300101011             ErrGenerico = *on;
061400101011             leavesr;
061500101011
061600101011           // -?Controllo immisione solo caratteri numerici?
061700101011           WHEN  %check( c_Digits : VIDlnp ) > *zero;
061800101011             VIDmsg = $Msg(02);
061900101011             PosCurLNP   = *on;
062000101011             ErrMessage  = *on;
062100101011             ErrGenerico = *on;
062200101011             leavesr;
062300101011
062400101011         ENDSL;
062500101011
062600101011         // -?Controllo filiale?
062700101011         ORGfil = %int( VIDlnp);
062800101011         chain  ORGfil  AZORG;
062900101012         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
063000101011           VIDmsg = $Msg(02);
063100101011           PosCurLNP   = *on;
063200101011           ErrMessage  = *on;
063300101011           ErrGenerico = *on;
063400101011           leavesr;
063500101011         endif;
063600101011
063700101011         // -?Controllo / decodifica cliente?
063800101011         clear  TIBS69ds;
063900101011         I69sif = knsif;
064000101011         I69kcc = DUTkci;
064100101011         I69kac = (%int(VIDlnp) * 10000) + VIDnup;
064200101011         tibs69r( tibs69ds :
064300101011                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
064400101011         if  O69err = *on;
064500101011           VIDmsg = $Msg(03);
064600101011           PosCurNUP   = *on;
064700101011           ErrMessage  = *on;
064800101011           ErrGenerico = *on;
064900101011           leavesr;
065000101011         endif;
065100101011
065200101011         if  VIDnup = 8888   or   VIDnup = 9999;
065300101012           VIDmsg = $Msg(04);
065400101011           PosCurNUP   = *on;
065500101011           ErrMessage  = *on;
065600101011           ErrGenerico = *on;
065700101011           leavesr;
065800101011         endif;
065900101115
066000101115         // -?Composizione chiave tabella?
066100101115         k_TBLkey = VIDlnp + %editc( VIDnup : 'X' );
066200101011
066300101115         // -?Reperimento Data Ultima Trasmissione dalla tab. di fil.?
066400101115         clear  ds3K_Memo;
066500101115         if  w_SisInf <> 2   or   Not %open(TABEL00F);
066600101115           w_SisInf = 2;
066700101115           exsr  sr_OpenFileTab;
066800101115         endif;
066900101115         chain(N)  %kds(k03tabel00)  TABEL;
067000101115         if  %found(TABEL00F);
067100101115           ds3K_Memo = TBLuni;
067200101115         endif;
067300101115
067400101115         // -?Apertura file TABEL00F del 1° S.I. (sede)?
067500101011         if  w_SisInf <> 1   or   Not %open(TABEL00F);
067600101011           w_SisInf = 1;
067700101011           exsr  sr_OpenFileTab;
067800101011         endif;
067900101011
068000101011         // -?Verifica esistenza del record in TABEL?
068100101011         chain  %kds(k03tabel00)  TABEL;
068200101011
068300101011       ENDSR;
068400101011
068500101011       //--------------------------------------------------------------
068600101011       //?Apertura dei files tabelle nel sistema informativo impostato.?
068700101011       //--------------------------------------------------------------
068800101011       BEGSR  sr_OpenFileTab;
068900101011
069000101011         // -?Chiusura (eventuale) archivi?
069100101011         if  %open(TABEL00F);
069200101011           close  TABEL00F;
069300101011         endif;
069400101011         cloTabella();
069500101011
069600101011         // -?Apertura archivi?
069700101011         ds_Libr  = $Libraries(w_iSystem);
069800101012         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
069900101011         open  TABEL00F;
070000101011
070100101011       ENDSR;
070200101011
070300101011       //--------------------------------------------------------------
070400101011       //?Gestione videata D02.                                        ?
070500101011       //--------------------------------------------------------------
070600101011       BEGSR  sr_GesD02;
070700101011
070800101011         // -?Inizializzazione videata?
070900101011         if  $InzD02 = *on;
071000101011           exsr  sr_InzD02;
071100101011           $InzD02 = *off;
071200101011         endif;
071300101011
071400101011         // -?(Ri)Abilitazione tasti funzionali?
071500101011         exsr  sr_AbilFxxD02;
071600101011
071700101011         // -?Emissione Testata e Piede?
071800101011         write  TB40T1;
071900101011
072000101011         // -?Emissione videata?
072100101011         if  Not ProtectAll;
072200101011           exfmt  TB40D2;
072300101011         else;
072400101011           write  TB40D2;
072500101011           exfmt  Protect;
072600101011         endif;
072700101011
072800101011         reset  ErrMessage;
072900101011         reset  ErrGenerico;
073000101011         clear  VIDmsg;
073100101011
073200101011         SELECT;
073300101011
073400101011           // -?F3=Fine?
073500101011           WHEN  dsp_aid = c_F03;
073600101011             unlock  TABEL00F;
073700101011             $Fine = *on;
073800101011
073900101011           // -?F12=Ritorno?
074000101011           WHEN  dsp_aid = c_F12;
074100101011             unlock  TABEL00F;
074200101011             $Video = 'D1';
074300101011
074500101011           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
074600101011           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
074700140424           WHEN  dsp_aid = c_F21   or
074800101011                 dsp_aid = c_F22;
074900101011             exsr  sr_F2x;
075000101011
075100101011           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
075200101011           OTHER;
075300101012             // -?Controlli (eseguiti anche se F16=Annullamento)?
075400101012             exsr sr_CtrD02;
075500101011             if  ErrGenerico = *on;
075600101011               leavesr;
075700101011             endif;
075800101011             // -?Aggiornamento?
075900101012             If  dsp_aid = c_F05   or
076000101011                 dsp_aid = c_F06   or
076100101011                 dsp_aid = c_F16;
076200101012               exsr  sr_UpdateAll;
076300101012               if  ErrGenerico = *on;
076400101012                 leavesr;
076500101012               endif;
076600101012               // ·?Uscita dal programma SE richiamato?
076700101012               if  $Called = *on   and   W0Ccli > *zero;
076800101012                 $Fine = *on;
076900101012               // ·?Videata iniziale altrimenti?
077000101012               else;
077100101012                 $Video  = 'D1';
077200101012                 $InzD01 = *on;
077300101012               endif;
077400101012             EndIf;
077500101011
077600101011         ENDSL;
077700101011
077800101011         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
077900101011         if  $Video = 'D1';
078000101011           clear  V1Topz;
078100101011           VisBloAnn = *off;
078200101011         endif;
078300101011
078400101011       ENDSR;
078500101011
078600101011       //--------------------------------------------------------------
078700101011       //?Inizializzazione dati nella videata D02.                     ?
078800101011       //--------------------------------------------------------------
078900101011       BEGSR  sr_InzD02;
079000101011
079100101011         clear  TB40D2;
079200101011         clear  ds3K;
079300101011         VisBloAnn = *off;
079400101011
079500101011         // -?Impostazione testata?
079600101011         reset  ProtectAll;
079700101011         clear  V1Topz;
079800101011         select;
079900101011           when  Not %found(TABEL00F);
080000101011             V1Topz = '  INSERIMENTO  ';
080100101011           when  TBLflg = '*';
080200101011             V1Topz = '  RIPRISTINO   ';
080300101011           other;
080400101011             V1Topz = '   MODIFICA    ';
080500101011         endsl;
080600101011         select;
080700101011           // -?Codice annullato?
080800101011           when  ACOflg <> *blank;
080900130527             //if  %found(TABEL00F);
081000130527               VI2aob   =  'CODICE ANNULLATO';
081100101011               ProtectAll = *on;
081200101011               VisBloAnn  = *on;
081300130527             //endif;
081400101011           // -?Codice bloccato?
081500130315           when  ACOabl <> *blanks;
081600130527             //if  %found(TABEL00F);
081700130527               VI2aob   =  'CODICE BLOCCATO';
081800101011               VisBloAnn  = *on;
081900130527             //endif;
082000130527             clear  TRULBLCds;
082100130527             iBLCkcc = DUTkci;
082200130527             iBLCksc = I69kac;
082300130527             trulBLCr ( kpjba : trulBLCds );
082400130527             if  oBLCdav > 00010101;
082500130527               wDate_Dmy = %date( oBLCdav : *iso );
082600130527               wDate6 = %dec( wDate_Dmy );
082700130527               VI2aob = 'BLOCCATO IL ' + %editw ( wDate6 : '  /  /  ');
082800130527             endif;
082900101011         endsl;
083000101011
083100101011         // -?Impostazione dettaglio?
083200101011         VI2cod = I69kac;
083300101011         VI2rag = ACOrag;
083400101011
083500101011         If  %found(TABEL00F);
083600101011
083700101011           ds3K = TBLuni;
083800101011           // -?Dati consegna:?
083900101115           SaveNMC = §3Knmc;
084000101115           HI2cdu  = ds3K_Memo.§3Kcdu;
084100101115           if  ds3K_Memo.§3Kcdu > *zero;
084200101115             wDate_Eur = %date( ds3K_Memo.§3Kcdu : *ymd );
084300101011             VI2cdu = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
084400101011                        ( %subdt(wDate_Eur : *months) * 10000 ) +
084500101011                        ( %subdt(wDate_Eur : *years) ) );
084600101011           endif;
084700101011           VI2cq1 = §3Kcq1;
084800101011           VI2cq2 = §3Kcq2;
084900101011           VI2cq3 = §3Kcq3;
085000101011           VI2cq4 = §3Kcq4;
085100101011           VI2cq5 = §3Kcq5;
085200101011           VI2cq6 = §3Kcq6;
085300101011           VI2cq7 = §3Kcq7;
085400101011           VI2cq8 = §3Kcq8;
085500101011           VI2nmc = §3Knmc;
085600101011           VI2vto = §3Kvto;
085700101011           VI2cma = §3Kcma;
085800101011           VI2cba = §3Kcba;
085900101011           VI2ctb = §3Kctb;
086000101011           // - ·?Scarico dati:?
086100101011           VI2cgc = §3Kcgc;
086200101011           VI2csd = §3Kcsd;
086300101011           VI2cks = §3Kcks;
086400101011           // -?Dati giacenza:?
086500101011           VI2gqi = §3Kgqi;
086600101011           VI2app = §3Kapp;
086700101011           // - ·?Scarico dati:?
086800101011           VI2ggc = §3Kggc;
086900101011           VI2gsd = §3Kgsd;
087000101011           VI2gks = §3Kgks;
087100130521           if §3Kca <> *blank;
087200130521             VI2gksVGD = §3Kgks;
087300130521           endif;
087400101011           // -?Disposizioni da clienti:?
087500101011           // - ·?Scarico dati:?
087600101011           VI2gsr = §3Kgsr;
087700101011           // -?Dati completi C.A. (VGD)?
087800101011           VI2ca  = §3Kca;
087900101011
088000101011         EndIf;
088100101011
088200101011         // -?Decodifica cliente unificante (dati consegna)?
088300101011         select;
088400101011           when  §3Kcks = *zero;
088500101011           when  §3Kcks = VI2cod;
088600101011             VI2ric = VI2rag;
088700101011           other;
088800101011             clear  TIBS69ds;
088900101011             I69sif = knsif;
089000101011             I69kcc = DUTkci;
089100101011             I69kac = §3Kcks;
089200101011             tibs69r( tibs69ds :
089300101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
089400101115             if  O69err = *on;
089500101115               clear  VI2ric;
089600101115             else;
089700101011               VI2ric = ACOrag;
089800101011             endif;
089900101011         endsl;
090000101011         // -?Decodifica cliente unificante (dati giacenza)?
090100101011         select;
090200101011           when  §3Kgks = *zero;
090300101011           when  §3Kgks = VI2cod;
090400101011             VI2ri2 = VI2rag;
090500101011           other;
090600101011             clear  TIBS69ds;
090700101011             I69sif = knsif;
090800101011             I69kcc = DUTkci;
090900101115             I69kac = §3Kgks;
091000101011             tibs69r( tibs69ds :
091100101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
091200101115             if  O69err = *on;
091300101115               clear  VI2ri2;
091400101115             else;
091500101011               VI2ri2 = ACOrag;
091600101011             endif;
091700101011         endsl;
091800101011
091900101011       ENDSR;
092000101011
092100101011       //--------------------------------------------------------------
092200101011       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
092300101011       //--------------------------------------------------------------
092400101011       BEGSR  sr_AbilFxxD02;
092500101011
092600101011         // ->?F5 = Ripristino?
092700101011         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
092800101011
092900101011         // ->?F6 = Conferma?
093000101011         F6Attivo = ( ( %found(TABEL00F)  and  TBLflg = *blank  and
093100101012                        Not ProtectAll )   OR  Not %found(TABEL00F) );
093200101011
093300101011         // ->?F12 = Ritorno?
093400101011         F12Attivo = (Not $Called);
093500101011
093600101011         // ->?F16 = Annullamento?
093700101011         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank);
093800101011
093900140424         // -?Abilitazione F21 e F22 SE NON richiamato?
094200101011         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
094300101011         F21Attivo = (Not $Called);
094400101011         // ->?F22 = Gestione dati C/A e Fattura per clienti?
094500101011         F22Attivo = (Not $Called);
094600101011
094700101011       ENDSR;
094800101011
094900101011       //--------------------------------------------------------------
095000101011       //?Controllo dati immessi nella videata D02.                    ?
095100101011       //--------------------------------------------------------------
095200101011       BEGSR  sr_CtrD02;
095300101011
095400101011         %subst(IndDspF : 50) = *off;
095500101011
095600130521         // -?se impostato "DAT COMPLETI C.A." allora valorizzo anche il suo cod
095700130521         //  ?cliente unificante
095800130521         if VI2ca <> *blank;
095900130521           VI2gksVGD = VI2gks;
096000130521         else;
096100130521           VI2gksVGD = 0;
096200130521         endif;
096300101012
096400101012         // -?DATI CONSEGNA?
096500101012         IF  ds_VI2cq <> *blank;
096600101012
096700101012           // -?SCARICO DATI: giorni scadenza?
096800101012           if  VI2cgc <= *zero;
096900101012             VIDmsg = $Msg(08);
097000101012             PosCurCGC   = *on;
097100101012             ErrMessage  = *on;
097200101012             ErrGenerico = *on;
097300101012             leavesr;
097400101012           endif;
097500101012
097600101012           // -?SCARICO DATI: tipo invio?
097700101012           Select;
097800101012             // -?Ricerca?
097900101012             When  %scan('?' : VI2csd) > *zero;
098000101012               X§Tkut = c_Kut;
098100101012               X§Tcod = '3T';
098200101012               clear  X§Tkey;
098300101012               clear  X§Tdes;
098400101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
098500101012               VI2csd = %subst(X§Tkey : 1 : %len(VI2csd));
098600101012               ErrGenerico = *on;
098700101012               leavesr;
098800101012             // -?Controllo effettiva immissione?
098900101012             When  VI2csd = *blank;
099000101012               VIDmsg = $Msg(08);
099100101012               PosCurCSD   = *on;
099200101012               ErrMessage  = *on;
099300101012               ErrGenerico = *on;
099400101012               leavesr;
099500101012             // -?Controllo validità codice immesso?
099600101012             Other;
099700101012               ds_TNTBE.TBEke1 = VI2csd;
099800101012               select;
099900101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
100000101012                                    *omit   : *omit : *omit :
100100101012                                    *omit   : *omit :
100200101012                                    *omit   : *omit : *omit : *omit :
100300101012                                    *omit   : *omit : *omit : *omit :
100400101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
100500101012                                  < *zero;
100600101012                   VIDmsg = $Msg(09);
100700101012                   PosCurCSD   = *on;
100800101012                   ErrMessage  = *on;
100900101012                   ErrGenerico = *on;
101000101012                   leavesr;
101100101012                 when  ds_TNTBE.TBEatb <> *blank;
101200101012                   VIDmsg = $Msg(10);
101300101012                   PosCurCSD   = *on;
101400101012                   ErrMessage  = *on;
101500101012                   ErrGenerico = *on;
101600101012                   leavesr;
101700101012               endsl;
101800101012
101900101012           EndSl;
102000101012
102100101012           // -?SCARICO DATI: codice cliente unificante?
102200101012           Select;
102300101012             // -?Ricerca alfabetica?
102400101012             When  VI2cks = *zero   and   VI2ric <> *blank;
102500101012               xParRag = VI2ric;
102600101012               exsr  sr_CallXAlfa3B;
102700101012               if  %subst( xParKsm : 1 : %len(VI2cks) ) > *zero;
102800101012                 VI2cks = ACOksc;
102900101012                 VI2ric = ACOrag;
103000101012               endif;
103100101012               ErrGenerico = *on;
103200101012               leavesr;
103300101012             // -?Controllo effettiva immissione?
103400101012             When  VI2cks = *zero   and   VI2ric = *blank;
103500101012               VIDmsg = $Msg(08);
103600101012               PosCurCKS   = *on;
103700101012               ErrMessage  = *on;
103800101012               ErrGenerico = *on;
103900101012               leavesr;
104000101012             // -?Controllo validità codice immesso?
104100101012             Other;
104200101012               clear  TIBS69ds;
104300101012               I69sif = knsif;
104400101012               I69kcc = DUTkci;
104500101012               I69kac = VI2cks;
104600101012               tibs69r( tibs69ds :
104700101012                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
104800101012               select;
104900101012                 when  O69err = *on;
105000101012                   VIDmsg = $Msg(03);
105100101012                   PosCurCKS   = *on;
105200101012                   ErrMessage  = *on;
105300101012                   ErrGenerico = *on;
105400101012                   leavesr;
105500101012                 when  ACOflg <> *blank;
105600101012                   VIDmsg = $Msg(13);
105700101012                   PosCurCKS   = *on;
105800101012                   ErrMessage  = *on;
105900101012                   ErrGenerico = *on;
106000101012                   leavesr;
106100101012                 other;
106200101012                   VI2ric = %subst( ACOrag : 1 : %len(VI2ric) );
106300101012               endsl;
106400101012           EndSl;
106500101012
106600101012         ENDIF;
106700101012
106800101012         // -?Dati da inviare mancanti?
106900101012         if  ds_VI2cq = *blank   AND   (VI2cgc <> *zero    or
107000101012                                        VI2csd <> *blank   or
107100101012                                        VI2cks <> *zero    or
107200101012                                        VI2ric <> *blank);
107300101012           VIDmsg = $Msg(05);
107400101012           PosCurCQ1   = *on;
107500101012           ErrMessage  = *on;
107600101012           ErrGenerico = *on;
107700101012           leavesr;
107800101012         endif;
107900101012
108000101012
108100101012         // -?DATI GIACENZA?
108200101012         // -?Tipo giacenze da scaricare obbligatorio?
108300101012         if  VI2gqi = *blank   and   VI2nmc = *blank   and
108400101012            (VI2ggc <> *zero    or
108500111020             VI2gsd <> *blank);
108600101012           VIDmsg = $Msg(14);
108700101012           PosCurGQI   = *on;
108800101012           ErrMessage  = *on;
108900101012           ErrGenerico = *on;
109000101012           leavesr;
109100101012         endif;
109200101012
109300101012         // -?Appuntamento?
109400101012         if  VI2app <> *blank   and   (VI2gqi = 'C'   or
109500101012                                       VI2gqi = *blank);
109600101012           VIDmsg = $Msg(15);
109700101012           PosCurAPP   = *on;
109800101012           ErrMessage  = *on;
109900101012           ErrGenerico = *on;
110000101012           leavesr;
110100101012         endif;
110200101012
110300101012         IF  VI2nmc <> *blank   or   VI2gqi <> *blank;
110400101012
110500101012           // -?SCARICO DATI: giorni scadenza?
110600101012           if  VI2ggc <= *zero;
110700101012             VIDmsg = $Msg(08);
110800101012             PosCurGGC   = *on;
110900101012             ErrMessage  = *on;
111000101012             ErrGenerico = *on;
111100101012             leavesr;
111200101012           endif;
111300101012
111400101012           // -?SCARICO DATI: tipo invio?
111500101012           Select;
111600101012             // -?Ricerca?
111700101012             When  %scan('?' : VI2gsd) > *zero;
111800101012               X§Tkut = c_Kut;
111900101012               X§Tcod = '3T';
112000101012               clear  X§Tkey;
112100101012               clear  X§Tdes;
112200101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
112300101012               VI2gsd = %subst(X§Tkey : 1 : %len(VI2gsd));
112400101012               ErrGenerico = *on;
112500101012               leavesr;
112600101012             // -?Controllo effettiva immissione?
112700101012             When  VI2gsd = *blank;
112800101012               VIDmsg = $Msg(08);
112900101012               PosCurGSD   = *on;
113000101012               ErrMessage  = *on;
113100101012               ErrGenerico = *on;
113200101012               leavesr;
113300101012             // -?Controllo validità codice immesso?
113400101012             Other;
113500101012               ds_TNTBE.TBEke1 = VI2gsd;
113600101012               select;
113700101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
113800101012                                    *omit   : *omit : *omit :
113900101012                                    *omit   : *omit :
114000101012                                    *omit   : *omit : *omit : *omit :
114100101012                                    *omit   : *omit : *omit : *omit :
114200101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
114300101012                                  < *zero;
114400101012                   VIDmsg = $Msg(09);
114500101012                   PosCurGSD   = *on;
114600101012                   ErrMessage  = *on;
114700101012                   ErrGenerico = *on;
114800101012                   leavesr;
114900101012                 when  ds_TNTBE.TBEatb <> *blank;
115000101012                   VIDmsg = $Msg(10);
115100101012                   PosCurGSD   = *on;
115200101012                   ErrMessage  = *on;
115300101012                   ErrGenerico = *on;
115400101012                   leavesr;
115500101012               endsl;
115600101012
115700101012           EndSl;
115800101012
115900101012         ENDIF;
116000101012
116100101012         // -?SCARICO DATI: codice cliente unificante?
116200101012         Select;
116300101012           // -?Ricerca alfabetica?
116400101012           When  VI2gks =  *zero   and   VI2ri2 <> *blank;
116500101012             xParRag = VI2ri2;
116600101012             exsr  sr_CallXAlfa3B;
116700101012             if  %subst( xParKsm : 1 : %len(VI2gks) ) > *zero;
116800101012               VI2gks = ACOksc;
116900101012               VI2ri2 = ACOrag;
117000101012             endif;
117100101012             ErrGenerico = *on;
117200101012             leavesr;
117300101012           // -?Controllo effettiva immissione?
117400101012           //  ?(NON È OBBLIGATORIO!!!)?
117500101012           //When  VI2gks =  *zero   and   VI2ri2 = *blank;
117600101012           //  VIDmsg = $Msg(08);
117700101012           //  PosCurGKS   = *on;
117800101012           //  ErrMessage  = *on;
117900101012           //  ErrGenerico = *on;
118000101012           //  leavesr;
118100101012           // -?Controllo validità codice immesso?
118200101012           When  VI2gks <> *zero;
118300101012             clear  TIBS69ds;
118400101012             I69sif = knsif;
118500101012             I69kcc = DUTkci;
118600101012             I69kac = VI2gks;
118700101012             tibs69r( tibs69ds :
118800101012                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
118900101012             select;
119000101012               when  O69err = *on;
119100101012                 VIDmsg = $Msg(03);
119200101012                 PosCurGKS   = *on;
119300101012                 ErrMessage  = *on;
119400101012                 ErrGenerico = *on;
119500101012                 leavesr;
119600101012               when  ACOflg <> *blank;
119700101012                 VIDmsg = $Msg(13);
119800101012                 PosCurGKS   = *on;
119900101012                 ErrMessage  = *on;
120000101012                 ErrGenerico = *on;
120100101012                 leavesr;
120200101012               other;
120300101012                 VI2ri2 = %subst( ACOrag : 1 : %len(VI2ri2) );
120400101012             endsl;
120500101012         EndSl;
120600101012
120700101012
120800101012         // -?DISPOSIZIONI DA CLIENTE?
120900101012         IF  VI2gsr <> *blank;
121000101012
121100101012           // -?SCARICO DATI: tipo ricezione?
121200101012           Select;
121300101012             // -?Ricerca?
121400101012             When  %scan('?' : VI2gsr) > *zero;
121500101012               X§Tkut = c_Kut;
121600101012               X§Tcod = '3T';
121700101012               clear  X§Tkey;
121800101012               clear  X§Tdes;
121900101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
122000101012               VI2gsr = %subst(X§Tkey : 1 : %len(VI2gsr));
122100101012               ErrGenerico = *on;
122200101012               leavesr;
122300101012             // -?Controllo effettiva immissione?
122400101012             //  ?(NON È OBBLIGATORIO!!!)?
122500101012             //When  VI2gsr = *blank;
122600101012             //  VIDmsg = $Msg(08);
122700101012             //  PosCurGSR   = *on;
122800101012             //  ErrMessage  = *on;
122900101012             //  ErrGenerico = *on;
123000101012             //  leavesr;
123100101012             // -?Controllo validità codice immesso?
123200101012             When  VI2gsr <> *blank;
123300101012               ds_TNTBE.TBEke1 = VI2gsr;
123400101012               select;
123500101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
123600101012                                    *omit   : *omit : *omit :
123700101012                                    *omit   : *omit :
123800101012                                    *omit   : *omit : *omit : *omit :
123900101012                                    *omit   : *omit : *omit : *omit :
124000101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
124100101012                                  < *zero;
124200101012                   VIDmsg = $Msg(09);
124300101012                   PosCurGSR   = *on;
124400101012                   ErrMessage  = *on;
124500101012                   ErrGenerico = *on;
124600101012                   leavesr;
124700101012                 when  ds_TNTBE.TBEatb <> *blank;
124800101012                   VIDmsg = $Msg(10);
124900101012                   PosCurGSR   = *on;
125000101012                   ErrMessage  = *on;
125100101012                   ErrGenerico = *on;
125200101012                   leavesr;
125300101012               endsl;
125400101012
125500101012           EndSl;
125600101012
125700101012         ENDIF;
125800101012
125900101012         // -?SCARICO DATI: codice cliente unificante?
126000101012         If  VI2gks =  *zero;
126100101012           if  VI2gsr <> *blank   or
126200101012               VI2gsd <> *blank   or
126300101012               VI2ca  <> *blank;
126400101012             VIDmsg = $Msg(08);
126500101012             PosCurGKS   = *on;
126600101012             ErrMessage  = *on;
126700101012             ErrGenerico = *on;
126800101012             leavesr;
126900101012           endif;
127000101012         Else;
127100101012           if  VI2gsr =  *blank   and
127200101012               VI2gsd =  *blank   and
127300101012               VI2ca  =  *blank;
127400101012             VIDmsg = $Msg(16);
127500101012             PosCurGKS   = *on;
127600101012             ErrMessage  = *on;
127700101012             ErrGenerico = *on;
127800101012             leavesr;
127900101012           endif;
128000101012         EndIf;
128100101012
128200101012         if  ds_VI2cq = *blank   and
128300101012             VI2gsr   = *blank   and
128400101012             VI2gqi   = *blank   and
128500101012             VI2nmc   = *blank   and
128600101012             VI2ca    = *blank;
128700101012           VIDmsg = $Msg(06);
128800101115           PosCurCQ1   = *on;
128900101012           ErrMessage  = *on;
129000101012           ErrGenerico = *on;
129100101012           leavesr;
129200101012         endif;
129300101012
129400101012         // -?SE inserito il flag "Eslcudi VAC x triangolazione ORM"?
129500101012         //  ?almeno uno dei dati del VAC deve essere inserito?
129600101012         if  VI2vto <> *blank   and
129700101012             VI2cq1 =  *blank   and
129800101012             VI2cq4 =  *blank   and
129900101012             VI2cq2 =  *blank   and
130000101012             VI2cq5 =  *blank   and
130100101012             VI2cq8 =  *blank   and
130200101012             VI2cq3 =  *blank;
130300101012           VIDmsg = $Msg(07);
130400101012           PosCurVTO   = *on;
130500101012           ErrMessage  = *on;
130600101012           ErrGenerico = *on;
130700101012           leavesr;
130800101012         endif;
130900101012
131000101012         // -?Ultimo giro di controllo se si tratta di generare?
131100101012         //  ?l'Estero EDI?
131200101012         reset  $Found_PT;
131300101012         IF  VI2csd = 'EW';
131400101012
131500101012           setll  ('PT')  EDTAB000;
131600101012           reade  ('PT')  EDTAB000;
131700101012           DoW  Not %eof(EDTAB01L)  and  Not $Found_PT;
131800101012             EDIdsPT = TABuni;
131900101012             if  §PTksc = VI2cks;
132000101012               $Found_PT = *on;
132100101012             else;
132200101012               reade  ('PT')  EDTAB000;
132300101012             endif;
132400101012           EndDo;
132500101012
132600101012           If  Not $Found_PT;
132700101012             exfmt  TB40W2;
132800101012           EndIf;
132900101012
133000101012         ENDIF;
133100101115
133200101115         // -?Emissione avviso (all'F6) se richiesto l'Invio?
133300101115         //  ?Motivo Mancata Consegna (VAG)?
133400101115         If  dsp_aid = c_F06    and
133500101115             SaveNMC = *blank   and   VI2nmc = 'S';
133600101115           exfmt  TB40W1;
133700101115           dsp_aid = c_F06;
133800101115         EndIf;
133900101011
134000101011       ENDSR;
134100101012
134200101012       //--------------------------------------------------------------
134300101012       //?Richiamo *pgm XALFA3BR per ricerca cliente.                  ?
134400101012       //--------------------------------------------------------------
134500101012       BEGSR  sr_CallXAlfa3B;
134600101012
134700101012         xParDut = RSut;
134800101012         xParKut = 1;
134900101012         //xParRag = VI2...;
135000101012         xParKcc = DUTkci;
135100101012         xParSta = *zero;
135200101012         clear  xParFlr;
135300101012         clear  xParDit;
135400101012         xParNum = 1;
135500101012         clear  xParKcm;
135600101012         clear  xParKsm;
135700101012         clear  xParKdm;
135800101012         clear  xParEsci;
135900101012         clear  xParErr;
136000101012         clear  xParIva;
136100101012         clear  xParCdf;
136200101012
136300101012         xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
136400101012                    xParSta : xParFlr : xParDit : xParNum :
136500101012                    xParKcm : xParKsm : xParKdm : xParEsci :
136600101012                    xParErr : xParIva : xParCdf );
136700101012
136800101012         clear  ds_cnaco;
136900101012
137000101012         If  xParSta <> -1   and   xParErr = *blank   and
137100101012             %subst( xParKsm : 1 : %len( I69kac ) ) > *zero;
137200101012
137300101012           clear  TIBS69ds;
137400101012           I69sif = knsif;
137500101012           I69kcc = DUTkci;
137600101012           I69kac = %int( %subst( xParKsm : 1 : %len( VI2cks ) ) );
137700101012
137800101012           tibs69r( tibs69ds :
137900101012                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
138000101012
138100101012         EndIf;
138200101012
138300101012       ENDSR;
138400101012
138500101012       //--------------------------------------------------------------
138600101012       //?Aggiornam. rec. di TABEL00F in tutti i sistemi informativi   ?
138700101012       //--------------------------------------------------------------
138800101012       BEGSR  sr_UpdateAll;
138900101012
139000101012         // -?Ciclo di elaborazione per ogni sistema informativo?
139100101012         For  w_SisInf = 1  To  %elem($Libr);
139200101012
139300101012           // -?Apertura degli archivi?
139400101012           if  w_SisInf > 1;
139500101012             exsr  sr_OpenFileTab;
139600101012           endif;
139700101012
139800101012           // -?Aggiornamento tab. "3K"?
139900101012           exsr  sr_UpdTABEL;
140000101012
140100101012         EndFor;
140200101012
140300101012       ENDSR;
140400101012
140500101012       //--------------------------------------------------------------
140600101012       //?Aggiornamento record nel file TABEL00F.                      ?
140700101012       //--------------------------------------------------------------
140800101012       BEGSR  sr_UpdTABEL;
140900101012
141000101012         //? N.B.                                                      ?
141100101012         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
141200101012         //   trasmissione (vedi flag TBEFTR).                        ?
141300101012         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
141400101012         //   subito nello stesso file di entrambi i S.I. - in due    ?
141500101012         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
141600101012
141700101012         // -?Aggiornam. tab. "3K" = Invio dati -P.O.D./Giacenze-?   ?
141800101012
141900101012         // -?RI-aggancio record da aggiornare?
142000101012         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
142100101012         if  w_SisInf > 1;
142200101012           k_TBLcod = c_Tab;
142300101012           k_TBLkey = %editc( VI2cod : 'X' );
142400101012           chain  %kds(k03tabel00)  TABEL;
142500101012         endif;
142600101012
142700101012         SELECT;
142800101012
142900101012           // -?F5 = Ripristino?
143000101012           WHEN  dsp_aid = c_F05;
143100101012
143200101012             if  Not %found(TABEL00F);
143300101012               clear  TABEL;
143400101012               TBLkut = c_Kut;
143500101012               TBLcod = c_Tab;
143600101012               TBLkey = k_TBLkey;
143700101012             endif;
143800101012             exsr  sr_RieDS3K;
143900101012             TBLuni = ds3K;
144000101012
144100101012             select;
144200101012               when  %found(TABEL00F)   and   TBLflg = '*';
144300101012                 clear  TBLflg;
144400101012                 clear  TBLftr;
144500101012                 clear  TBLdtr;
144600101012                 //____________
144700101012                 update  TABEL;
144800101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
144900101012               // - Record appena cancellato fisicamente
145000101012               when  NOT %found(TABEL00F);
145100101012                 TBLftt = '1';
145200101012                 clear  TBLflt;
145300101115                 if  w_SisInf = 1;
145400101012                   TBLftr = 'T';
145500101012                 else;
145600101012                   TBLftr = 'R';
145700101012                 endif;
145800101012                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
145900101012                                        %len(wDate) - %len(TBLdtr) + 1 :
146000101012                                        %len(TBLdtr) ) );
146100101012                 //____________
146200101012                 write   TABEL;
146300101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
146400101012             endsl;
146500101012
146600101012           // -?F6 = Aggiornamento / Inserimento?
146700101012           WHEN  dsp_aid = c_F06;
146800101012
146900101012             if  Not %found(TABEL00F);
147000101012               clear  TABEL;
147100101012               TBLkut = c_Kut;
147200101012               TBLcod = c_Tab;
147300101012               TBLkey = k_TBLkey;
147400101012             endif;
147500101012             exsr  sr_RieDS3K;
147600101012             TBLuni = ds3K;
147700101012
147800101012             TBLftt = '1';
147900101012             clear  TBLflt;
148000101115             if  w_SisInf = 1;
148100101012               TBLftr = 'T';
148200101012             else;
148300101012               TBLftr = 'R';
148400101012             endif;
148500101012             TBLdtr = %int( %subst( %editc(wDate : 'X') :
148600101012                                    %len(wDate) - %len(TBLdtr) + 1 :
148700101012                                    %len(TBLdtr) ) );
148800101012
148900101012             if  %found(TABEL00F);
149000101012               //____________
149100101012               update  TABEL;
149200101012               //¯¯¯¯¯¯¯¯¯¯¯¯
149300101012             else;
149400101012               //____________
149500101012               write   TABEL;
149600101012               //¯¯¯¯¯¯¯¯¯¯¯¯
149700101012             endif;
149800101012
149900101012           // -?F16 = Annullamento?
150000101012           WHEN  dsp_aid = c_F16;
150100101012
150200101012             if  %found(TABEL00F)   and   TBLflg   = *blank;
150300101012               TBLflg = '*';
150400101012               clear  TBLftr;
150500101012               clear  TBLdtr;
150600101012               //____________
150700101012               update  TABEL;
150800101012               //¯¯¯¯¯¯¯¯¯¯¯¯
150900101012             endif;
151000101012
151100101012         ENDSL;
151200101012
151300101012       ENDSR;
151400101012
151500101012       //--------------------------------------------------------------
151600101012       //?Riempimento struttura dati ds3K.                             ?
151700101012       //--------------------------------------------------------------
151800101012       BEGSR  sr_RieDS3K;
151900101012
152000101115         if  %found(TABEL00F);
152100101115           ds3K_Memo = TBLuni;
152200101115         else;
152300101115           clear  ds3K_Memo;
152400101115         endif;
152500101115
152600101012         clear  ds3K;
152700101012
152800101012         // -?Impostazione altri campi (per tutti)?
152900101012         §3Kcgc = VI2cgc;
153000101012         §3Kcq1 = VI2cq1;
153100101012         §3Kcq2 = VI2cq2;
153200101012         §3Kcq3 = VI2cq3;
153300101012         §3Kcq4 = VI2cq4;
153400101012         §3Kcq5 = VI2cq5;
153500101012         §3Knmc = VI2nmc;
153600101012         §3Kcsd = VI2csd;
153700101117         if  ds3K_Memo.§3Kcdu > *zero;
153800101115           §3Kcdu = ds3K_Memo.§3Kcdu;
153900101115         endif;
154000101012         §3Kcks = VI2cks;
154100101012         §3Kggc = VI2ggc;
154200101012         §3Kgqi = VI2gqi;
154300101012         §3Kgsd = VI2gsd;
154400101012         §3Kcma = VI2cma;
154500101012         §3Kcba = VI2cba;
154600101012         §3Kapp = VI2app;
154700101012         §3Kcq8 = VI2cq8;
154800101012         §3Kca  = VI2ca ;
154900101012         §3Kvto = VI2vto;
155000101012         §3Kgks = VI2gks;
155100101012         §3Kgsr = VI2gsr;
155200101012         §3Kctb = VI2ctb;
155300101012         §3Kcq6 = VI2cq6;
155400101012         §3Kcq7 = VI2cq7;
155500101012
155600101012       ENDSR;
155700101012
155800101012       //--------------------------------------------------------------
155900101012       //?Esecuzione del comando (già impostato).                      ?
156000101012       //--------------------------------------------------------------
156100101012       BEGSR  sr_ExecCmd;
156200101012
156300101012         clear Qcap0100;
156400101012         Qcabcsdh = *off;
156500101012         Qcapa    = *off;
156600101012         Qcacmdss = *off;
156700101012         Qcaerved = *allX'00';
156800101012
156900101012         clear Qusec;
157000101012         Qusbprv  = %size(Qusec);
157100101012
157200101012         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
157300101012                           %size(Qcap0100) : 'CPOP0100' : *omit :
157400101012                           0 : 0 : Qusec);
157500101012
157600101012         //if  Qusei <> *blank;
157700101012         //  ...;
157800101012         //endif;
157900101012
158000101012       ENDSR;
158100101012
158200101012       //--------------------------------------------------------------
158300101012       //?Operazioni finali.                                           ?
158400101012       //--------------------------------------------------------------
158500101012       BEGSR  sr_RoutEnd;
158600101012
158700101012         cloTabella();
158800101012
158900101012         clear  TIBS69ds;
159000101012         I69tla = 'C';
159100101012         tibs69r( tibs69ds :
159200101012                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
159300101012
159400101012         return;
159500101012
159600101012       ENDSR;
159700101011
159800101011      /end-free
159900101011
160000101011       //--------------------------------------------------------------
160100101011       //?Schiere a tempo di compilazione.                             ?
160200101011       //--------------------------------------------------------------
160300101011
160400101012** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con TABEL00F?
160500101115SETRAS  GAITRAGRU FILTRAGRU
160600101115AS888   GAITRAGRPSFILTRAGRPF
160700101011** - $Msg:?Messaggi di Errore?-----------------------------------------------*
160800101011Filiale inesistente                                                            1
160900101011Linea inesistente                                                              2
161000101011Codice cliente inesistente                                                     3
161100101011Codice cliente NON selezionabile                                               4
161200101011Specificare dati da inviare                                                    5
161300101011Immettere DATI CONSEGNA o DATI GIACENZA o DISPOSIZIONI DA CLIENTE o C.A.       6
161400101011Inserire solo se indicato almeno un ritorno VAC                                7
161500101011Campo obbligatorio                                                             8
161600101011Tipo trasmissione inesistente                                                  9
161700101011Tipo trasmissione annullata                                                   10
161800101011File inesistente                                                              11
161900111020?LIBERO?                                                                      12
162000101011Codice cliente annullato                                                      13
162100101011Specificare tipo giacenze da scaricare                                        14
162200101011Valorizzare abbinato con invio aperture o entrambe                            15
162300101011Dato valido solo per DATI CONSEGNA o DATI GIACENZA o DATI C.A.                16
