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
014400101011     d   F21Attivo                     n   overlay(IndDspF : 21)
014500101011     d   F22Attivo                     n   overlay(IndDspF : 22)
014600101011         // -?Emissione messaggio di errore?
014700101011     d   ErrMessage                    n   overlay(IndDspF : 28)
014800101011         // -?Indicatori per Attibuti di visualizzazione?
014900101011     d   VisBloAnn                     n   overlay(IndDspF : 40)
015000101011     d   ProtectAll                    n   overlay(IndDspF : 41)
015100101011         // -?Posizionamento cursore & Segnalazione errore?
015200101011     d   PosCurLNP                     n   overlay(IndDspF : 51)
015300101011     d   PosCurNUP                     n   overlay(IndDspF : 52)
015400101011     d   PosCurFIL                     n   overlay(IndDspF : 53)
015500101011     d   PosCurCQ1                     n   overlay(IndDspF : 54)
015600101011     d   PosCurVTO                     n   overlay(IndDspF : 55)
015700101011     d   PosCurCGC                     n   overlay(IndDspF : 56)
015800101011     d   PosCurCSD                     n   overlay(IndDspF : 57)
015900101011     d   PosCurCKS                     n   overlay(IndDspF : 59)
016000101011     d   PosCurGQI                     n   overlay(IndDspF : 60)
016100101011     d   PosCurAPP                     n   overlay(IndDspF : 61)
016200101011     d   PosCurGGC                     n   overlay(IndDspF : 62)
016300101011     d   PosCurGSD                     n   overlay(IndDspF : 63)
016400101011     d   PosCurGKS                     n   overlay(IndDspF : 65)
016500101011     d   PosCurGSR                     n   overlay(IndDspF : 66)
016600101011         //   -?Riemissione videata?
016700101011     d   ErrGenerico                   n   overlay(IndDspF : 99)
016800101011
016900101011       // -?Parametri ricevuti?
017000101011     d KPJBA         e ds
017100101011
017200101011       // -?Parametri per Reperimento dati utente?
017300101011     d TIBS34ds      e ds                  inz
017400101011
017500101011       // -?Tabelle usate:?
017600101011       // ·?"3Q" = Dati per/da clienti?
017700101011     d ds3K          e ds                  inz
017800101115     d ds3K_Memo     e ds                  inz  extname(ds3K)
017900101115     d                                          qualified
018000101011       // ·?"3T" = Tipo scarico?
018100101011     d ds3T          e ds                  inz
018200101011       // ·?"LAT" = Funzione aziendale?
018300101011     d dLAT          e ds                  inz
018400101011
018500101011       // -?Dati partner E.D.I. Estero?
018600101011     d EDIdsPT       e ds                  inz
018700101011
018800101011       // -?Ridefinizione flag "Invio dati consegna"?
018900101012     d ds_VI2CQ        ds                  inz
019000101011     d  VI2cq1                             inz
019100101011     d  VI2cq2                             inz
019200101011     d  VI2cq4                             inz
019300101011     d  VI2cq5                             inz
019400101011     d  VI2cq6                             inz
019500101011     d  VI2cq7                             inz
019600101011     d  VI2cmA                             inz
019700101011     d  VI2cbA                             inz
019800101011     d  VI2cq8                             inz
019900101011
020000101011       // -?Ridefinizione elenco librerie in elaborare le tabelle?
020100101011     d ds_Libr         ds                  inz
020200101011     d  $Libr                        10    dim(c_NrLibr) inz
020300101011
020400101011       //--------------------------------------------------------------
020500101011       //?Definizione variabili globali.                               ?
020600101011       //--------------------------------------------------------------
020700101011
020800101011       // -?Flags booleani?
020900101011     d $Fine           s               n   inz
021000101011     d $InzD01         s               n   inz(*on)
021100101011     d $InzD02         s               n   inz(*on)
021200101011     d $Called         s               n   inz
021300101012     d $Found_PT       s               n   inz
021400101011
021500101011       // -?Variabili per la gestione del video?
021600101011     d $Video          s              2    inz('D1')
021700101011
021800101011       // -?Codice cliente eventualmente ricevuto?
021900101011     d W0Ccli          s              7a   inz
022000101012
022100101012       // -?Flag annullamento tabella (per TRULTAB)?
022200101012     d wTBEatb         s                   like(TBLflg)  inz
022300101011
022400101011       // -?Memo...?
022500101011     d SaveNMC         s                   like(VI2NMC)  inz
022600101011
022700101011       // -?Nome del sistema?
022800101011     d currSysNeta     s              8a   inz
022900101011
023000101011       // -?Nome esteso Libreria/File del file tabella?
023100101011     d wLibFile        s             21a   inz
023200101011
023300101011       // -?Campi di comodo?
023400101012     d wDate_EUR       s               d   datfmt(*eur)  inz
023500130527     d wDate_DMY       s               d   datfmt(*dmy)  inz
023600130527     d wDate6          s              6  0 inz
023700101012     d wDate           s              8  0 inz
023800101011     d w_iSystem       s              1  0 inz
023900101011     d w_SisInf        s              3  0 inz
024000101011
024100101011       //--------------------------------------------------------------
024200101011       //?Definizione prototipi procedure.                             ?
024300101011       //--------------------------------------------------------------
024400101011
024500101011       // -?Reperimento dati utente?
024600101011      /copy gaitrasrc/srcProtoPR,TIBS34R
024700101011
024800101011       // -?Parametri per pgm interrogazione tabelle (TABEL00F)?
024900101011      /copy gaitrasrc/srcProtoPI,X§TABER
025000101011       // -?Interrogazione tabelle?
025100101011      /copy gaitrasrc/srcProtoPR,X§TABER
025200101012
025300101012       // -?Reperimento dati tabelle?
025400101012      /copy gaitrasrc/srcProtoPR,TRULTAB
025500101011
025600101011       // -?Parametri per pgm interrogazione sottoconti?
025700101011      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
025800101011       // -?Interrogazione sottoconti?
025900101011      /copy gaitrasrc/srcProtoPR,XALFA3BR
026000101012
026100101012       // -?Controllo/Decodifica cliente?
026200101105      /copy gaitrasrc/srcProtoPI,TIBS69R
026300101012      /copy gaitrasrc/srcProtoPR,TIBS69R
026400130527
026500130527       // -?Parametri per pgm ricerca data blocco cliente?
026600130527     d TRULBLCds     e ds                  inz
026700130527      /copy gaitrasrc/srcProtoPR,TRULBLCR
026800101011
026900101011       // -?Gestione invio dati - BOLLETTAZIONE?
027000101012     d trtb28r         pr                  extpgm('TRTB28R')
027100101012     d   kpjba                             likeds(kpjba)
027200101011
027300101011       // -?Gestione scambio dati C/A e fattura con clienti?
027400101011      /copy gaitrasrc/srcProtoPR,TRTB44R
027500101011
027600101011       // -?Gestione scambio dati principali?
027700101011      /copy gaitrasrc/srcProtoPR,TRTB47R
027800101012
027900101012       // -?Reperimento NETA sistema AS/400 corrente?
028000101012      /copy gaitrasrc/srcProtoPr,UBRTVNETA
028100101012
028200101012       // -?Parametri API QCAPCMD (Process Commands)?
028300101012     d Qcmd            s           2048    inz  varying
028400101012      /copy qSysInc/qRpgleSrc,QCAPCMD
028500101012       // -?API QCAPCMD (Process Commands)?
028600101012      /copy gaitrasrc/srcProtoPR,QCAPCMD
028700101012
028800101012       // -?Parametri gestione errori API.?
028900101012      /copy qsysinc/qrpglesrc,QUSEC
029000101011
029100101011       //--------------------------------------------------------------
029200101011       //?Definizione key-list.                                        ?
029300101011       //--------------------------------------------------------------
029400101011
029500101011       // -?File TABEL00F?
029600101011     d k03tabel00    e ds                  extname(TABEL00F : *key)
029700101011     d                                     prefix(k_)   inz
029800101011
029900101011       //--------------------------------------------------------------
030000101011       //?M A I N - L I N E                                            ?
030100101011       //--------------------------------------------------------------
030200101011
030300101011     c     *Entry        plist
030400101011     c                   parm                    KPJBA
030500101011
030600101011      /free
030700101011
030800101011       // -?Operazioni iniziali?
030900101011       exsr sr_RoutInz;
031000101011
031100101011       // -?Gestione videate?
031200101011       DoW  $Fine = *off;
031300101011
031400101011         select;
031500101011           // -?Gestione videata D1 (richiesta codice cliente)?
031600101011           when $Video = 'D1';
031700101011             exsr sr_GesD01;
031800101011           // -?Gestione videata D2 (manutenzione dati cliente)?
031900101011           when $Video = 'D2';
032000101011             exsr sr_GesD02;
032100101011           // -?? ? ? ? ??
032200101011           other;
032300101011             $Fine = *on;
032400101011         endsl;
032500101011
032600101011       enddo;
032700101011
032800101011       // -?Operazioni finali?
032900101011       exsr sr_RoutEnd;
033000101011
033100101011       //--------------------------------------------------------------
033200101011       //?Operazioni iniziali.                                         ?
033300101011       //--------------------------------------------------------------
033400101011       BEGSR  sr_RoutInz;
033500101011
033600101011         *inLR = *on;
033700101011
033800101011         // -?Verifica del sistema AS/400 corrente?
033900101011         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
034000101011           $Fine = *on;
034100101011           leavesr;
034200101011         endif;
034300101011
034400101011         // -?Impostazione elenco librerie in cui gestire le tabelle?
034500101011         //  ?(a seconda del sistema in cui si stà lavorando)?
034600101011         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
034700101011         if  w_iSystem = *zero;
034800101011           $Fine = *on;
034900101011           leavesr;
035000101011         endif;
035100101011
035200101011         // -?Reperimento dati job?
035300101011         exsr  sr_DatiJob;
035400101105
035500101105         // -?Reperimento data odierna?
035600101105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
035700101105                               : 1 : 8 ) );
035800101011
035900101011         // -?Impostazione nome programma a video?
036000101011         V1Tpgm = SDSpgm;
036100101011
036200101011         // -?Impostazione campi "fissi"?
036300101011         k_TBLkut = c_Kut;
036400101011         k_TBLcod = c_Tab;
036500101011
036600101011         // -?Verifica se *pgm richiamato:?
036700101011         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
036800101011         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
036900101011         clear  W0Ccli;
037000101011         $Called = (%subst( KPJBU : 1 : %len(W0Ccli) ) <> *blank);
037100101011         if  $Called = *on   and
037200101011             %check( c_Digits : %subst( KPJBU : 1 :
037300101011                                       %len(W0Ccli) ) ) = *zero;
037400101011           W0Ccli = %subst( KPJBU : 1 : %len(W0Ccli) );
037500101011         endif;
037600101011
037700101011       ENDSR;
037800101011
037900101011       //--------------------------------------------------------------
038000101011       //?Reperimento Dati del job (Utente/Operativi).                 ?
038100101011       //--------------------------------------------------------------
038200101011       BEGSR  sr_DatiJob;
038300101011
038400101011         in(e) §AzUte;
038500101011         if NOT %error;
038600101011           in(e) §DatiUte;
038700101011         endif;
038800101011         if %error or RSut = *blank;
038900101011           tibs34r ( tibs34ds );
039000101011           in §AzUte;
039100101011           in §DatiUte;
039200101011         endif;
039300101011
039400101011       ENDSR;
039500101011
039600101011       //--------------------------------------------------------------
039700101011       //?Gestione videata D01.                                        ?
039800101011       //--------------------------------------------------------------
039900101011       BEGSR  sr_GesD01;
040000101011
040100101011         // -?Inizializzazione videata?
040200101011         if  $InzD01 = *on;
040300101011           exsr  sr_InzD01;
040400101011           $InzD01 = *off;
040500101011         endif;
040600101011
040700101011         // -?SE ricevuto codice cliente NON si emette la prima videata?
040800101011         if  $Called   and   W0Ccli > *zero;
040900101011           exsr  sr_CtrD01;
041000101011           if  Not ErrGenerico;
041100101011             $Video  = 'D2';
041200101011             $InzD02 = *on;
041300101011             leavesr;
041400101011           else;
041500101011             clear  W0Ccli;
041600101011           endif;
041700101011         endif;
041800101011
041900101011         // -?Emissione Testata?
042000101011         write  TB40T1;
042100101011
042200101011         // -?Emissione videata?
042300101011         exfmt  TB40D1;
042400101011
042500101011         reset  ErrMessage;
042600101011         reset  ErrGenerico;
042700101011         clear  VIDmsg;
042800101011
042900101011         SELECT;
043000101011
043100101011           // -?F3=Fine?
043200101011           WHEN  dsp_aid = c_F03;
043300101011             $Fine = *on;
043400101011
043500101011           // -?F21=Gestione scambio dati bolle?
043600101011           // -?F22=Gestione scambio dati C/A e fattura con clienti?
043700140424           WHEN  dsp_aid = c_F21   or
043800101011                 dsp_aid = c_F22;
043900101011             exsr  sr_F2x;
044000101011
044100101011           // -?Invio?
044200101011           OTHER;
044300101011             exsr sr_CtrD01;
044400101011             if  ErrGenerico = *off;
044500101011               $Video    = 'D2';
044600101011               $InzD02   = *on;
044700101011             endif;
044800101011
044900101011         ENDSL;
045000101011
045100101011       ENDSR;
045200101011
045300101011       //--------------------------------------------------------------
045400101011       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
045500101011       //--------------------------------------------------------------
045600101011       BEGSR  sr_InzD01;
045700101011
045800101011         clear  V1Topz;
045900101011
046000101011         clear  TB40D1;
046100101122
046200101122         // -?Se è stato richiamato con un codice cliente numerico, si?
046300101122         //  ?imposta anche tale codice a video?
046400101122         select;
046500101122           when  $Called   and   W0Ccli > *zero;
046600101122             VIDlnp = %subst( W0Ccli : 1 : %len(VIDlnp) );
046700101122             VIDnup = %int( %subst( W0Ccli : %len(VIDlnp) + 1
046800101122                                           : %len(VIDnup) ) );
046900101122           when  Not $Called;
047000101122             VIDlnp = '?';
047100101122         endsl;
047200101011
047300101011         // -?Abilitazione tasti funzionali?
047400101011         exsr  sr_AbilFxxD01;
047500101011
047600101011       ENDSR;
047700101011
047800101011       //--------------------------------------------------------------
047900101011       //?Abilitazione tasti funzionali in P01 (per D01).              ?
048000101011       //--------------------------------------------------------------
048100101011       BEGSR  sr_AbilFxxD01;
048200101011
048300101011         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
048400101011         //  ?c'è il codice cliente => NON devono essere abilitati i?
048500140424         //  ?tasti funzionali  F21 e F22.?
048600101011
048700101011
048800101011         // ->?F21 = Gestione scambio dati bolle?
048900101011         F21Attivo = (Not $Called);
049000101011
049100101011         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
049200101011         F22Attivo = (Not $Called);
049300101011
049400101011       ENDSR;
049500101011
049600101011       //--------------------------------------------------------------
049700140424       //?Gestione tasti funzionali F21 e F22 in D01 & D02.        ?
049800101011       //--------------------------------------------------------------
049900101011       BEGSR  sr_F2x;
050000101012
050100101012         if  %check( c_Digits : VIDlnp ) = *zero;
050200101012           %subst( KPJBU : 1 : 3 ) = VIDlnp;
050300101012         else;
050400101012           %subst( KPJBU : 1 : 3 ) = *zero;
050500101012         endif;
050600101012         %subst( KPJBU : 4 : 4 ) = %editc( VIDnup : 'X' );
050700101011
050800101011         select;
050900101011
051000101011           // -?F21 = Gestione scambio dati ricezione bolle da clienti?
051100101011           when  dsp_aid = c_F21;
051200101011             trtb28r ( kpjba );
051300101011
051400101011           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
051500101011           when  dsp_aid = c_F22;
051600101011             trtb44r ( kpjba );
051700101011
051800101011         endsl;
051900101011
052000101011       ENDSR;
052100101011
052200101011       //--------------------------------------------------------------
052300101011       //?Controllo dati immessi nella videata D01.                    ?
052400101011       //--------------------------------------------------------------
052500101011       BEGSR  sr_CtrD01;
052600101011
052700101011         %subst(IndDspF : 50) = *off;
052800101011         reset  ProtectAll;
052900101011
053000101011         // -?Filiale per ricerca tabella clienti errata?
053100101011         if  VIDfil > *zero;
053200101011           chain  (VIDfil)  AZORG;
053300101011           if  Not %found(AZORG01L);
053400101011             VIDmsg = $Msg(01);
053500101011             PosCurFIL   = *on;
053600101011             ErrGenerico = *on;
053700101011             leavesr;
053800101011           endif;
053900101011         endif;
054000101011
054100101011         SELECT;
054200101011         // -?Ricerca codice cliente in tabella "3K"?
054300101011           WHEN  %scan('?' : VIDlnp) > *zero;
054400101011             clear  VIDlnp;
054500101011             clear  VIDnup;
054600101011             X§Tkut = c_Kut;
054700101011             X§Tcod = c_Tab;
054800101011             if  VIDfil > *zero;
054900101011               X§Tkey = %editc( VIDfil : 'X' );
055000101011             else;
055100101011               clear  X§Tkey;
055200101011             endif;
055300101011             clear  X§Tdes;
055400101011             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
055500101011             if  X§Tkey <> *blank;
055600101011               VIDlnp = %subst(X§Tkey : 1 : %len(VIDlnp));
055700101012               VIDnup = %int( %subst( X§Tkey : %len(VIDlnp) + 1
055800101012                                             : %len(VIDnup) ) );
055900101011             else;
056000101011               VIDlnp = '?  ';
056100101011             endif;
056200101011             PosCurLNP   = *on;
056300101011             ErrGenerico = *on;
056400101011             leavesr;
056500101011
056600101011           // -?Ricerca alfabetica?
056700101011           WHEN  VIDnup =  *zero   and
056800101012                 VIDdes <> *blank;
056900101011             xParDut = RSut;
057000101011             xParKut = 1;
057100101011             xParRag = VIDdes;
057200101011             xParKcc = DUTkci;
057300101011             xParSta = *zero;
057400101012             xParFlr = VIDlnp;
057500101011             clear  xParDit;
057600101011             xParNum = 1;
057700101011             clear  xParKcm;
057800101011             clear  xParKsm;
057900101011             clear  xParKdm;
058000101011             clear  xParEsci;
058100101011             clear  xParErr;
058200101011             clear  xParIva;
058300101011             clear  xParCdf;
058400101011             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
058500101011                        xParSta : xParFlr : xParDit : xParNum :
058600101011                        xParKcm : xParKsm : xParKdm : xParEsci :
058700101011                        xParErr : xParIva : xParCdf );
058800101011             if  xParSta <> -1   and   xParErr = *blank
058900101011                                 and   xParKsm > *zero;
059000101012               VIDlnp = %subst( xParKsm : 1 : %len( VIDlnp ) );
059100101012               VIDnup = %int( %subst( xParKsm : %len( VIDlnp ) + 1
059200101012                                              : %len( VIDnup ) ) );
059300101011               clear  VIDdes;
059400101011             endif;
059500101011             PosCurLNP   = *on;
059600101011             ErrGenerico = *on;
059700101011             leavesr;
059800101011
059900101011           // -?Controllo immissione filiale del codice cliente?
060000101011           WHEN  VIDlnp = *blank  or  VIDlnp = *zero
060100101011                                  or  VIDnup = *zero;
060200101011             VIDmsg = $Msg(02);
060300101011             PosCurLNP   = *on;
060400101011             ErrMessage  = *on;
060500101011             ErrGenerico = *on;
060600101011             leavesr;
060700101011
060800101011           // -?Controllo immisione solo caratteri numerici?
060900101011           WHEN  %check( c_Digits : VIDlnp ) > *zero;
061000101011             VIDmsg = $Msg(02);
061100101011             PosCurLNP   = *on;
061200101011             ErrMessage  = *on;
061300101011             ErrGenerico = *on;
061400101011             leavesr;
061500101011
061600101011         ENDSL;
061700101011
061800101011         // -?Controllo filiale?
061900101011         ORGfil = %int( VIDlnp);
062000101011         chain  ORGfil  AZORG;
062100101012         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
062200101011           VIDmsg = $Msg(02);
062300101011           PosCurLNP   = *on;
062400101011           ErrMessage  = *on;
062500101011           ErrGenerico = *on;
062600101011           leavesr;
062700101011         endif;
062800101011
062900101011         // -?Controllo / decodifica cliente?
063000101011         clear  TIBS69ds;
063100101011         I69sif = knsif;
063200101011         I69kcc = DUTkci;
063300101011         I69kac = (%int(VIDlnp) * 10000) + VIDnup;
063400101011         tibs69r( tibs69ds :
063500101011                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
063600101011         if  O69err = *on;
063700101011           VIDmsg = $Msg(03);
063800101011           PosCurNUP   = *on;
063900101011           ErrMessage  = *on;
064000101011           ErrGenerico = *on;
064100101011           leavesr;
064200101011         endif;
064300101011
064400101011         if  VIDnup = 8888   or   VIDnup = 9999;
064500101012           VIDmsg = $Msg(04);
064600101011           PosCurNUP   = *on;
064700101011           ErrMessage  = *on;
064800101011           ErrGenerico = *on;
064900101011           leavesr;
065000101011         endif;
065100101115
065200101115         // -?Composizione chiave tabella?
065300101115         k_TBLkey = VIDlnp + %editc( VIDnup : 'X' );
065400101011
065500101115         // -?Reperimento Data Ultima Trasmissione dalla tab. di fil.?
065600101115         clear  ds3K_Memo;
065700101115         if  w_SisInf <> 2   or   Not %open(TABEL00F);
065800101115           w_SisInf = 2;
065900101115           exsr  sr_OpenFileTab;
066000101115         endif;
066100101115         chain(N)  %kds(k03tabel00)  TABEL;
066200101115         if  %found(TABEL00F);
066300101115           ds3K_Memo = TBLuni;
066400101115         endif;
066500101115
066600101115         // -?Apertura file TABEL00F del 1° S.I. (sede)?
066700101011         if  w_SisInf <> 1   or   Not %open(TABEL00F);
066800101011           w_SisInf = 1;
066900101011           exsr  sr_OpenFileTab;
067000101011         endif;
067100101011
067200101011         // -?Verifica esistenza del record in TABEL?
067300101011         chain  %kds(k03tabel00)  TABEL;
067400101011
067500101011       ENDSR;
067600101011
067700101011       //--------------------------------------------------------------
067800101011       //?Apertura dei files tabelle nel sistema informativo impostato.?
067900101011       //--------------------------------------------------------------
068000101011       BEGSR  sr_OpenFileTab;
068100101011
068200101011         // -?Chiusura (eventuale) archivi?
068300101011         if  %open(TABEL00F);
068400101011           close  TABEL00F;
068500101011         endif;
068600101011         cloTabella();
068700101011
068800101011         // -?Apertura archivi?
068900101011         ds_Libr  = $Libraries(w_iSystem);
069000101012         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
069100101011         open  TABEL00F;
069200101011
069300101011       ENDSR;
069400101011
069500101011       //--------------------------------------------------------------
069600101011       //?Gestione videata D02.                                        ?
069700101011       //--------------------------------------------------------------
069800101011       BEGSR  sr_GesD02;
069900101011
070000101011         // -?Inizializzazione videata?
070100101011         if  $InzD02 = *on;
070200101011           exsr  sr_InzD02;
070300101011           $InzD02 = *off;
070400101011         endif;
070500101011
070600101011         // -?(Ri)Abilitazione tasti funzionali?
070700101011         exsr  sr_AbilFxxD02;
070800101011
070900101011         // -?Emissione Testata e Piede?
071000101011         write  TB40T1;
071100101011
071200101011         // -?Emissione videata?
071300101011         if  Not ProtectAll;
071400101011           exfmt  TB40D2;
071500101011         else;
071600101011           write  TB40D2;
071700101011           exfmt  Protect;
071800101011         endif;
071900101011
072000101011         reset  ErrMessage;
072100101011         reset  ErrGenerico;
072200101011         clear  VIDmsg;
072300101011
072400101011         SELECT;
072500101011
072600101011           // -?F3=Fine?
072700101011           WHEN  dsp_aid = c_F03;
072800101011             unlock  TABEL00F;
072900101011             $Fine = *on;
073000101011
073100101011           // -?F12=Ritorno?
073200101011           WHEN  dsp_aid = c_F12;
073300101011             unlock  TABEL00F;
073400101011             $Video = 'D1';
073500101011
073600101011           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
073700101011           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
073800140424           WHEN  dsp_aid = c_F21   or
073900101011                 dsp_aid = c_F22;
074000101011             exsr  sr_F2x;
074100101011
074200101011           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
074300101011           OTHER;
074400101012             // -?Controlli (eseguiti anche se F16=Annullamento)?
074500101012             exsr sr_CtrD02;
074600101011             if  ErrGenerico = *on;
074700101011               leavesr;
074800101011             endif;
074900101011             // -?Aggiornamento?
075000101012             If  dsp_aid = c_F05   or
075100101011                 dsp_aid = c_F06   or
075200101011                 dsp_aid = c_F16;
075300101012               exsr  sr_UpdateAll;
075400101012               if  ErrGenerico = *on;
075500101012                 leavesr;
075600101012               endif;
075700101012               // ·?Uscita dal programma SE richiamato?
075800101012               if  $Called = *on   and   W0Ccli > *zero;
075900101012                 $Fine = *on;
076000101012               // ·?Videata iniziale altrimenti?
076100101012               else;
076200101012                 $Video  = 'D1';
076300101012                 $InzD01 = *on;
076400101012               endif;
076500101012             EndIf;
076600101011
076700101011         ENDSL;
076800101011
076900101011         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
077000101011         if  $Video = 'D1';
077100101011           clear  V1Topz;
077200101011           VisBloAnn = *off;
077300101011         endif;
077400101011
077500101011       ENDSR;
077600101011
077700101011       //--------------------------------------------------------------
077800101011       //?Inizializzazione dati nella videata D02.                     ?
077900101011       //--------------------------------------------------------------
078000101011       BEGSR  sr_InzD02;
078100101011
078200101011         clear  TB40D2;
078300101011         clear  ds3K;
078400101011         VisBloAnn = *off;
078500101011
078600101011         // -?Impostazione testata?
078700101011         reset  ProtectAll;
078800101011         clear  V1Topz;
078900101011         select;
079000101011           when  Not %found(TABEL00F);
079100101011             V1Topz = '  INSERIMENTO  ';
079200101011           when  TBLflg = '*';
079300101011             V1Topz = '  RIPRISTINO   ';
079400101011           other;
079500101011             V1Topz = '   MODIFICA    ';
079600101011         endsl;
079700101011         select;
079800101011           // -?Codice annullato?
079900101011           when  ACOflg <> *blank;
080000130527             //if  %found(TABEL00F);
080100130527               VI2aob   =  'CODICE ANNULLATO';
080200101011               ProtectAll = *on;
080300101011               VisBloAnn  = *on;
080400130527             //endif;
080500101011           // -?Codice bloccato?
080600130315           when  ACOabl <> *blanks;
080700130527             //if  %found(TABEL00F);
080800130527               VI2aob   =  'CODICE BLOCCATO';
080900101011               VisBloAnn  = *on;
081000130527             //endif;
081100130527             clear  TRULBLCds;
081200130527             iBLCkcc = DUTkci;
081300130527             iBLCksc = I69kac;
081400130527             trulBLCr ( kpjba : trulBLCds );
081500130527             if  oBLCdav > 00010101;
081600130527               wDate_Dmy = %date( oBLCdav : *iso );
081700130527               wDate6 = %dec( wDate_Dmy );
081800130527               VI2aob = 'BLOCCATO IL ' + %editw ( wDate6 : '  /  /  ');
081900130527             endif;
082000101011         endsl;
082100101011
082200101011         // -?Impostazione dettaglio?
082300101011         VI2cod = I69kac;
082400101011         VI2rag = ACOrag;
082500101011
082600101011         If  %found(TABEL00F);
082700101011
082800101011           ds3K = TBLuni;
082900101011           // -?Dati consegna:?
083000101115           SaveNMC = §3Knmc;
083100101115           HI2cdu  = ds3K_Memo.§3Kcdu;
083200101115           if  ds3K_Memo.§3Kcdu > *zero;
083300101115             wDate_Eur = %date( ds3K_Memo.§3Kcdu : *ymd );
083400101011             VI2cdu = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
083500101011                        ( %subdt(wDate_Eur : *months) * 10000 ) +
083600101011                        ( %subdt(wDate_Eur : *years) ) );
083700101011           endif;
083800101011           VI2cq1 = §3Kcq1;
083900101011           VI2cq2 = §3Kcq2;
084000101011           VI2cq3 = §3Kcq3;
084100101011           VI2cq4 = §3Kcq4;
084200101011           VI2cq5 = §3Kcq5;
084300101011           VI2cq6 = §3Kcq6;
084400101011           VI2cq7 = §3Kcq7;
084500101011           VI2cq8 = §3Kcq8;
084600101011           VI2nmc = §3Knmc;
084700101011           VI2vto = §3Kvto;
084800101011           VI2cma = §3Kcma;
084900170608           //VI2cba = §3Kcba;
085000101011           VI2ctb = §3Kctb;
085100101011           // - ·?Scarico dati:?
085200101011           VI2cgc = §3Kcgc;
085300101011           VI2csd = §3Kcsd;
085400101011           VI2cks = §3Kcks;
085500101011           // -?Dati giacenza:?
085600101011           VI2gqi = §3Kgqi;
085700101011           VI2app = §3Kapp;
085800101011           // - ·?Scarico dati:?
085900101011           VI2ggc = §3Kggc;
086000101011           VI2gsd = §3Kgsd;
086100101011           VI2gks = §3Kgks;
086200130521           if §3Kca <> *blank;
086300130521             VI2gksVGD = §3Kgks;
086400130521           endif;
086500101011           // -?Disposizioni da clienti:?
086600101011           // - ·?Scarico dati:?
086700101011           VI2gsr = §3Kgsr;
086800101011           // -?Dati completi C.A. (VGD)?
086900101011           VI2ca  = §3Kca;
087000101011
087100101011         EndIf;
087200101011
087300101011         // -?Decodifica cliente unificante (dati consegna)?
087400101011         select;
087500101011           when  §3Kcks = *zero;
087600101011           when  §3Kcks = VI2cod;
087700101011             VI2ric = VI2rag;
087800101011           other;
087900101011             clear  TIBS69ds;
088000101011             I69sif = knsif;
088100101011             I69kcc = DUTkci;
088200101011             I69kac = §3Kcks;
088300101011             tibs69r( tibs69ds :
088400101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
088500101115             if  O69err = *on;
088600101115               clear  VI2ric;
088700101115             else;
088800101011               VI2ric = ACOrag;
088900101011             endif;
089000101011         endsl;
089100101011         // -?Decodifica cliente unificante (dati giacenza)?
089200101011         select;
089300101011           when  §3Kgks = *zero;
089400101011           when  §3Kgks = VI2cod;
089500101011             VI2ri2 = VI2rag;
089600101011           other;
089700101011             clear  TIBS69ds;
089800101011             I69sif = knsif;
089900101011             I69kcc = DUTkci;
090000101115             I69kac = §3Kgks;
090100101011             tibs69r( tibs69ds :
090200101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
090300101115             if  O69err = *on;
090400101115               clear  VI2ri2;
090500101115             else;
090600101011               VI2ri2 = ACOrag;
090700101011             endif;
090800101011         endsl;
090900101011
091000101011       ENDSR;
091100101011
091200101011       //--------------------------------------------------------------
091300101011       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
091400101011       //--------------------------------------------------------------
091500101011       BEGSR  sr_AbilFxxD02;
091600101011
091700101011         // ->?F5 = Ripristino?
091800101011         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
091900101011
092000101011         // ->?F6 = Conferma?
092100101011         F6Attivo = ( ( %found(TABEL00F)  and  TBLflg = *blank  and
092200101012                        Not ProtectAll )   OR  Not %found(TABEL00F) );
092300101011
092400101011         // ->?F12 = Ritorno?
092500101011         F12Attivo = (Not $Called);
092600101011
092700101011         // ->?F16 = Annullamento?
092800101011         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank);
092900101011
093000140424         // -?Abilitazione F21 e F22 SE NON richiamato?
093100101011         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
093200101011         F21Attivo = (Not $Called);
093300101011         // ->?F22 = Gestione dati C/A e Fattura per clienti?
093400101011         F22Attivo = (Not $Called);
093500101011
093600101011       ENDSR;
093700101011
093800101011       //--------------------------------------------------------------
093900101011       //?Controllo dati immessi nella videata D02.                    ?
094000101011       //--------------------------------------------------------------
094100101011       BEGSR  sr_CtrD02;
094200101011
094300101011         %subst(IndDspF : 50) = *off;
094400101011
094500130521         // -?se impostato "DAT COMPLETI C.A." allora valorizzo anche il suo cod
094600130521         //  ?cliente unificante
094700130521         if VI2ca <> *blank;
094800130521           VI2gksVGD = VI2gks;
094900130521         else;
095000130521           VI2gksVGD = 0;
095100130521         endif;
095200101012
095300101012         // -?DATI CONSEGNA?
095400101012         IF  ds_VI2cq <> *blank;
095500101012
095600101012           // -?SCARICO DATI: giorni scadenza?
095700101012           if  VI2cgc <= *zero;
095800101012             VIDmsg = $Msg(08);
095900101012             PosCurCGC   = *on;
096000101012             ErrMessage  = *on;
096100101012             ErrGenerico = *on;
096200101012             leavesr;
096300101012           endif;
096400101012
096500101012           // -?SCARICO DATI: tipo invio?
096600101012           Select;
096700101012             // -?Ricerca?
096800101012             When  %scan('?' : VI2csd) > *zero;
096900101012               X§Tkut = c_Kut;
097000101012               X§Tcod = '3T';
097100101012               clear  X§Tkey;
097200101012               clear  X§Tdes;
097300101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
097400101012               VI2csd = %subst(X§Tkey : 1 : %len(VI2csd));
097500101012               ErrGenerico = *on;
097600101012               leavesr;
097700101012             // -?Controllo effettiva immissione?
097800101012             When  VI2csd = *blank;
097900101012               VIDmsg = $Msg(08);
098000101012               PosCurCSD   = *on;
098100101012               ErrMessage  = *on;
098200101012               ErrGenerico = *on;
098300101012               leavesr;
098400101012             // -?Controllo validità codice immesso?
098500101012             Other;
098600101012               ds_TNTBE.TBEke1 = VI2csd;
098700101012               select;
098800101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
098900101012                                    *omit   : *omit : *omit :
099000101012                                    *omit   : *omit :
099100101012                                    *omit   : *omit : *omit : *omit :
099200101012                                    *omit   : *omit : *omit : *omit :
099300101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
099400101012                                  < *zero;
099500101012                   VIDmsg = $Msg(09);
099600101012                   PosCurCSD   = *on;
099700101012                   ErrMessage  = *on;
099800101012                   ErrGenerico = *on;
099900101012                   leavesr;
100000101012                 when  ds_TNTBE.TBEatb <> *blank;
100100101012                   VIDmsg = $Msg(10);
100200101012                   PosCurCSD   = *on;
100300101012                   ErrMessage  = *on;
100400101012                   ErrGenerico = *on;
100500101012                   leavesr;
100600101012               endsl;
100700101012
100800101012           EndSl;
100900101012
101000101012           // -?SCARICO DATI: codice cliente unificante?
101100101012           Select;
101200101012             // -?Ricerca alfabetica?
101300101012             When  VI2cks = *zero   and   VI2ric <> *blank;
101400101012               xParRag = VI2ric;
101500101012               exsr  sr_CallXAlfa3B;
101600101012               if  %subst( xParKsm : 1 : %len(VI2cks) ) > *zero;
101700101012                 VI2cks = ACOksc;
101800101012                 VI2ric = ACOrag;
101900101012               endif;
102000101012               ErrGenerico = *on;
102100101012               leavesr;
102200101012             // -?Controllo effettiva immissione?
102300101012             When  VI2cks = *zero   and   VI2ric = *blank;
102400101012               VIDmsg = $Msg(08);
102500101012               PosCurCKS   = *on;
102600101012               ErrMessage  = *on;
102700101012               ErrGenerico = *on;
102800101012               leavesr;
102900101012             // -?Controllo validità codice immesso?
103000101012             Other;
103100101012               clear  TIBS69ds;
103200101012               I69sif = knsif;
103300101012               I69kcc = DUTkci;
103400101012               I69kac = VI2cks;
103500101012               tibs69r( tibs69ds :
103600101012                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
103700101012               select;
103800101012                 when  O69err = *on;
103900101012                   VIDmsg = $Msg(03);
104000101012                   PosCurCKS   = *on;
104100101012                   ErrMessage  = *on;
104200101012                   ErrGenerico = *on;
104300101012                   leavesr;
104400101012                 when  ACOflg <> *blank;
104500101012                   VIDmsg = $Msg(13);
104600101012                   PosCurCKS   = *on;
104700101012                   ErrMessage  = *on;
104800101012                   ErrGenerico = *on;
104900101012                   leavesr;
105000101012                 other;
105100101012                   VI2ric = %subst( ACOrag : 1 : %len(VI2ric) );
105200101012               endsl;
105300101012           EndSl;
105400101012
105500101012         ENDIF;
105600101012
105700101012         // -?Dati da inviare mancanti?
105800101012         if  ds_VI2cq = *blank   AND   (VI2cgc <> *zero    or
105900101012                                        VI2csd <> *blank   or
106000101012                                        VI2cks <> *zero    or
106100101012                                        VI2ric <> *blank);
106200101012           VIDmsg = $Msg(05);
106300101012           PosCurCQ1   = *on;
106400101012           ErrMessage  = *on;
106500101012           ErrGenerico = *on;
106600101012           leavesr;
106700101012         endif;
106800101012
106900101012
107000101012         // -?DATI GIACENZA?
107100101012         // -?Tipo giacenze da scaricare obbligatorio?
107200101012         if  VI2gqi = *blank   and   VI2nmc = *blank   and
107300101012            (VI2ggc <> *zero    or
107400111020             VI2gsd <> *blank);
107500101012           VIDmsg = $Msg(14);
107600101012           PosCurGQI   = *on;
107700101012           ErrMessage  = *on;
107800101012           ErrGenerico = *on;
107900101012           leavesr;
108000101012         endif;
108100101012
108200101012         // -?Appuntamento?
108300101012         if  VI2app <> *blank   and   (VI2gqi = 'C'   or
108400101012                                       VI2gqi = *blank);
108500101012           VIDmsg = $Msg(15);
108600101012           PosCurAPP   = *on;
108700101012           ErrMessage  = *on;
108800101012           ErrGenerico = *on;
108900101012           leavesr;
109000101012         endif;
109100101012
109200101012         IF  VI2nmc <> *blank   or   VI2gqi <> *blank;
109300101012
109400101012           // -?SCARICO DATI: giorni scadenza?
109500101012           if  VI2ggc <= *zero;
109600101012             VIDmsg = $Msg(08);
109700101012             PosCurGGC   = *on;
109800101012             ErrMessage  = *on;
109900101012             ErrGenerico = *on;
110000101012             leavesr;
110100101012           endif;
110200101012
110300101012           // -?SCARICO DATI: tipo invio?
110400101012           Select;
110500101012             // -?Ricerca?
110600101012             When  %scan('?' : VI2gsd) > *zero;
110700101012               X§Tkut = c_Kut;
110800101012               X§Tcod = '3T';
110900101012               clear  X§Tkey;
111000101012               clear  X§Tdes;
111100101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
111200101012               VI2gsd = %subst(X§Tkey : 1 : %len(VI2gsd));
111300101012               ErrGenerico = *on;
111400101012               leavesr;
111500101012             // -?Controllo effettiva immissione?
111600101012             When  VI2gsd = *blank;
111700101012               VIDmsg = $Msg(08);
111800101012               PosCurGSD   = *on;
111900101012               ErrMessage  = *on;
112000101012               ErrGenerico = *on;
112100101012               leavesr;
112200101012             // -?Controllo validità codice immesso?
112300101012             Other;
112400101012               ds_TNTBE.TBEke1 = VI2gsd;
112500101012               select;
112600101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
112700101012                                    *omit   : *omit : *omit :
112800101012                                    *omit   : *omit :
112900101012                                    *omit   : *omit : *omit : *omit :
113000101012                                    *omit   : *omit : *omit : *omit :
113100101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
113200101012                                  < *zero;
113300101012                   VIDmsg = $Msg(09);
113400101012                   PosCurGSD   = *on;
113500101012                   ErrMessage  = *on;
113600101012                   ErrGenerico = *on;
113700101012                   leavesr;
113800101012                 when  ds_TNTBE.TBEatb <> *blank;
113900101012                   VIDmsg = $Msg(10);
114000101012                   PosCurGSD   = *on;
114100101012                   ErrMessage  = *on;
114200101012                   ErrGenerico = *on;
114300101012                   leavesr;
114400101012               endsl;
114500101012
114600101012           EndSl;
114700101012
114800101012         ENDIF;
114900101012
115000101012         // -?SCARICO DATI: codice cliente unificante?
115100101012         Select;
115200101012           // -?Ricerca alfabetica?
115300101012           When  VI2gks =  *zero   and   VI2ri2 <> *blank;
115400101012             xParRag = VI2ri2;
115500101012             exsr  sr_CallXAlfa3B;
115600101012             if  %subst( xParKsm : 1 : %len(VI2gks) ) > *zero;
115700101012               VI2gks = ACOksc;
115800101012               VI2ri2 = ACOrag;
115900101012             endif;
116000101012             ErrGenerico = *on;
116100101012             leavesr;
116200101012           // -?Controllo effettiva immissione?
116300101012           //  ?(NON È OBBLIGATORIO!!!)?
116400101012           //When  VI2gks =  *zero   and   VI2ri2 = *blank;
116500101012           //  VIDmsg = $Msg(08);
116600101012           //  PosCurGKS   = *on;
116700101012           //  ErrMessage  = *on;
116800101012           //  ErrGenerico = *on;
116900101012           //  leavesr;
117000101012           // -?Controllo validità codice immesso?
117100101012           When  VI2gks <> *zero;
117200101012             clear  TIBS69ds;
117300101012             I69sif = knsif;
117400101012             I69kcc = DUTkci;
117500101012             I69kac = VI2gks;
117600101012             tibs69r( tibs69ds :
117700101012                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
117800101012             select;
117900101012               when  O69err = *on;
118000101012                 VIDmsg = $Msg(03);
118100101012                 PosCurGKS   = *on;
118200101012                 ErrMessage  = *on;
118300101012                 ErrGenerico = *on;
118400101012                 leavesr;
118500101012               when  ACOflg <> *blank;
118600101012                 VIDmsg = $Msg(13);
118700101012                 PosCurGKS   = *on;
118800101012                 ErrMessage  = *on;
118900101012                 ErrGenerico = *on;
119000101012                 leavesr;
119100101012               other;
119200101012                 VI2ri2 = %subst( ACOrag : 1 : %len(VI2ri2) );
119300101012             endsl;
119400101012         EndSl;
119500101012
119600101012
119700101012         // -?DISPOSIZIONI DA CLIENTE?
119800101012         IF  VI2gsr <> *blank;
119900101012
120000101012           // -?SCARICO DATI: tipo ricezione?
120100101012           Select;
120200101012             // -?Ricerca?
120300101012             When  %scan('?' : VI2gsr) > *zero;
120400101012               X§Tkut = c_Kut;
120500101012               X§Tcod = '3T';
120600101012               clear  X§Tkey;
120700101012               clear  X§Tdes;
120800101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
120900101012               VI2gsr = %subst(X§Tkey : 1 : %len(VI2gsr));
121000101012               ErrGenerico = *on;
121100101012               leavesr;
121200101012             // -?Controllo effettiva immissione?
121300101012             //  ?(NON È OBBLIGATORIO!!!)?
121400101012             //When  VI2gsr = *blank;
121500101012             //  VIDmsg = $Msg(08);
121600101012             //  PosCurGSR   = *on;
121700101012             //  ErrMessage  = *on;
121800101012             //  ErrGenerico = *on;
121900101012             //  leavesr;
122000101012             // -?Controllo validità codice immesso?
122100101012             When  VI2gsr <> *blank;
122200101012               ds_TNTBE.TBEke1 = VI2gsr;
122300101012               select;
122400101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
122500101012                                    *omit   : *omit : *omit :
122600101012                                    *omit   : *omit :
122700101012                                    *omit   : *omit : *omit : *omit :
122800101012                                    *omit   : *omit : *omit : *omit :
122900101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
123000101012                                  < *zero;
123100101012                   VIDmsg = $Msg(09);
123200101012                   PosCurGSR   = *on;
123300101012                   ErrMessage  = *on;
123400101012                   ErrGenerico = *on;
123500101012                   leavesr;
123600101012                 when  ds_TNTBE.TBEatb <> *blank;
123700101012                   VIDmsg = $Msg(10);
123800101012                   PosCurGSR   = *on;
123900101012                   ErrMessage  = *on;
124000101012                   ErrGenerico = *on;
124100101012                   leavesr;
124200101012               endsl;
124300101012
124400101012           EndSl;
124500101012
124600101012         ENDIF;
124700101012
124800101012         // -?SCARICO DATI: codice cliente unificante?
124900101012         If  VI2gks =  *zero;
125000101012           if  VI2gsr <> *blank   or
125100101012               VI2gsd <> *blank   or
125200101012               VI2ca  <> *blank;
125300101012             VIDmsg = $Msg(08);
125400101012             PosCurGKS   = *on;
125500101012             ErrMessage  = *on;
125600101012             ErrGenerico = *on;
125700101012             leavesr;
125800101012           endif;
125900101012         Else;
126000101012           if  VI2gsr =  *blank   and
126100101012               VI2gsd =  *blank   and
126200101012               VI2ca  =  *blank;
126300101012             VIDmsg = $Msg(16);
126400101012             PosCurGKS   = *on;
126500101012             ErrMessage  = *on;
126600101012             ErrGenerico = *on;
126700101012             leavesr;
126800101012           endif;
126900101012         EndIf;
127000101012
127100101012         if  ds_VI2cq = *blank   and
127200101012             VI2gsr   = *blank   and
127300101012             VI2gqi   = *blank   and
127400101012             VI2nmc   = *blank   and
127500101012             VI2ca    = *blank;
127600101012           VIDmsg = $Msg(06);
127700101115           PosCurCQ1   = *on;
127800101012           ErrMessage  = *on;
127900101012           ErrGenerico = *on;
128000101012           leavesr;
128100101012         endif;
128200101012
128300101012         // -?SE inserito il flag "Eslcudi VAC x triangolazione ORM"?
128400101012         //  ?almeno uno dei dati del VAC deve essere inserito?
128500101012         if  VI2vto <> *blank   and
128600101012             VI2cq1 =  *blank   and
128700101012             VI2cq4 =  *blank   and
128800101012             VI2cq2 =  *blank   and
128900101012             VI2cq5 =  *blank   and
129000101012             VI2cq8 =  *blank   and
129100101012             VI2cq3 =  *blank;
129200101012           VIDmsg = $Msg(07);
129300101012           PosCurVTO   = *on;
129400101012           ErrMessage  = *on;
129500101012           ErrGenerico = *on;
129600101012           leavesr;
129700101012         endif;
129800101012
129900101012         // -?Ultimo giro di controllo se si tratta di generare?
130000101012         //  ?l'Estero EDI?
130100101012         reset  $Found_PT;
130200101012         IF  VI2csd = 'EW';
130300101012
130400101012           setll  ('PT')  EDTAB000;
130500101012           reade  ('PT')  EDTAB000;
130600101012           DoW  Not %eof(EDTAB01L)  and  Not $Found_PT;
130700101012             EDIdsPT = TABuni;
130800101012             if  §PTksc = VI2cks;
130900101012               $Found_PT = *on;
131000101012             else;
131100101012               reade  ('PT')  EDTAB000;
131200101012             endif;
131300101012           EndDo;
131400101012
131500101012           If  Not $Found_PT;
131600101012             exfmt  TB40W2;
131700101012           EndIf;
131800101012
131900101012         ENDIF;
132000101115
132100101115         // -?Emissione avviso (all'F6) se richiesto l'Invio?
132200101115         //  ?Motivo Mancata Consegna (VAG)?
132300101115         If  dsp_aid = c_F06    and
132400101115             SaveNMC = *blank   and   VI2nmc = 'S';
132500101115           exfmt  TB40W1;
132600101115           dsp_aid = c_F06;
132700101115         EndIf;
132800101011
132900101011       ENDSR;
133000101012
133100101012       //--------------------------------------------------------------
133200101012       //?Richiamo *pgm XALFA3BR per ricerca cliente.                  ?
133300101012       //--------------------------------------------------------------
133400101012       BEGSR  sr_CallXAlfa3B;
133500101012
133600101012         xParDut = RSut;
133700101012         xParKut = 1;
133800101012         //xParRag = VI2...;
133900101012         xParKcc = DUTkci;
134000101012         xParSta = *zero;
134100101012         clear  xParFlr;
134200101012         clear  xParDit;
134300101012         xParNum = 1;
134400101012         clear  xParKcm;
134500101012         clear  xParKsm;
134600101012         clear  xParKdm;
134700101012         clear  xParEsci;
134800101012         clear  xParErr;
134900101012         clear  xParIva;
135000101012         clear  xParCdf;
135100101012
135200101012         xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
135300101012                    xParSta : xParFlr : xParDit : xParNum :
135400101012                    xParKcm : xParKsm : xParKdm : xParEsci :
135500101012                    xParErr : xParIva : xParCdf );
135600101012
135700101012         clear  ds_cnaco;
135800101012
135900101012         If  xParSta <> -1   and   xParErr = *blank   and
136000101012             %subst( xParKsm : 1 : %len( I69kac ) ) > *zero;
136100101012
136200101012           clear  TIBS69ds;
136300101012           I69sif = knsif;
136400101012           I69kcc = DUTkci;
136500101012           I69kac = %int( %subst( xParKsm : 1 : %len( VI2cks ) ) );
136600101012
136700101012           tibs69r( tibs69ds :
136800101012                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
136900101012
137000101012         EndIf;
137100101012
137200101012       ENDSR;
137300101012
137400101012       //--------------------------------------------------------------
137500101012       //?Aggiornam. rec. di TABEL00F in tutti i sistemi informativi   ?
137600101012       //--------------------------------------------------------------
137700101012       BEGSR  sr_UpdateAll;
137800101012
137900101012         // -?Ciclo di elaborazione per ogni sistema informativo?
138000101012         For  w_SisInf = 1  To  %elem($Libr);
138100101012
138200101012           // -?Apertura degli archivi?
138300101012           if  w_SisInf > 1;
138400101012             exsr  sr_OpenFileTab;
138500101012           endif;
138600101012
138700101012           // -?Aggiornamento tab. "3K"?
138800101012           exsr  sr_UpdTABEL;
138900101012
139000101012         EndFor;
139100101012
139200101012       ENDSR;
139300101012
139400101012       //--------------------------------------------------------------
139500101012       //?Aggiornamento record nel file TABEL00F.                      ?
139600101012       //--------------------------------------------------------------
139700101012       BEGSR  sr_UpdTABEL;
139800101012
139900101012         //? N.B.                                                      ?
140000101012         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
140100101012         //   trasmissione (vedi flag TBEFTR).                        ?
140200101012         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
140300101012         //   subito nello stesso file di entrambi i S.I. - in due    ?
140400101012         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
140500101012
140600101012         // -?Aggiornam. tab. "3K" = Invio dati -P.O.D./Giacenze-?   ?
140700101012
140800101012         // -?RI-aggancio record da aggiornare?
140900101012         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
141000101012         if  w_SisInf > 1;
141100101012           k_TBLcod = c_Tab;
141200101012           k_TBLkey = %editc( VI2cod : 'X' );
141300101012           chain  %kds(k03tabel00)  TABEL;
141400101012         endif;
141500101012
141600101012         SELECT;
141700101012
141800101012           // -?F5 = Ripristino?
141900101012           WHEN  dsp_aid = c_F05;
142000101012
142100101012             if  Not %found(TABEL00F);
142200101012               clear  TABEL;
142300101012               TBLkut = c_Kut;
142400101012               TBLcod = c_Tab;
142500101012               TBLkey = k_TBLkey;
142600101012             endif;
142700101012             exsr  sr_RieDS3K;
142800101012             TBLuni = ds3K;
142900101012
143000101012             select;
143100101012               when  %found(TABEL00F)   and   TBLflg = '*';
143200101012                 clear  TBLflg;
143300101012                 clear  TBLftr;
143400101012                 clear  TBLdtr;
143500101012                 //____________
143600101012                 update  TABEL;
143700101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
143800101012               // - Record appena cancellato fisicamente
143900101012               when  NOT %found(TABEL00F);
144000101012                 TBLftt = '1';
144100101012                 clear  TBLflt;
144200101115                 if  w_SisInf = 1;
144300101012                   TBLftr = 'T';
144400101012                 else;
144500101012                   TBLftr = 'R';
144600101012                 endif;
144700101012                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
144800101012                                        %len(wDate) - %len(TBLdtr) + 1 :
144900101012                                        %len(TBLdtr) ) );
145000101012                 //____________
145100101012                 write   TABEL;
145200101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
145300101012             endsl;
145400101012
145500101012           // -?F6 = Aggiornamento / Inserimento?
145600101012           WHEN  dsp_aid = c_F06;
145700101012
145800101012             if  Not %found(TABEL00F);
145900101012               clear  TABEL;
146000101012               TBLkut = c_Kut;
146100101012               TBLcod = c_Tab;
146200101012               TBLkey = k_TBLkey;
146300101012             endif;
146400101012             exsr  sr_RieDS3K;
146500101012             TBLuni = ds3K;
146600101012
146700101012             TBLftt = '1';
146800101012             clear  TBLflt;
146900101115             if  w_SisInf = 1;
147000101012               TBLftr = 'T';
147100101012             else;
147200101012               TBLftr = 'R';
147300101012             endif;
147400101012             TBLdtr = %int( %subst( %editc(wDate : 'X') :
147500101012                                    %len(wDate) - %len(TBLdtr) + 1 :
147600101012                                    %len(TBLdtr) ) );
147700101012
147800101012             if  %found(TABEL00F);
147900101012               //____________
148000101012               update  TABEL;
148100101012               //¯¯¯¯¯¯¯¯¯¯¯¯
148200101012             else;
148300101012               //____________
148400101012               write   TABEL;
148500101012               //¯¯¯¯¯¯¯¯¯¯¯¯
148600101012             endif;
148700101012
148800101012           // -?F16 = Annullamento?
148900101012           WHEN  dsp_aid = c_F16;
149000101012
149100101012             if  %found(TABEL00F)   and   TBLflg   = *blank;
149200101012               TBLflg = '*';
149300101012               clear  TBLftr;
149400101012               clear  TBLdtr;
149500101012               //____________
149600101012               update  TABEL;
149700101012               //¯¯¯¯¯¯¯¯¯¯¯¯
149800101012             endif;
149900101012
150000101012         ENDSL;
150100101012
150200101012       ENDSR;
150300101012
150400101012       //--------------------------------------------------------------
150500101012       //?Riempimento struttura dati ds3K.                             ?
150600101012       //--------------------------------------------------------------
150700101012       BEGSR  sr_RieDS3K;
150800101012
150900101115         if  %found(TABEL00F);
151000101115           ds3K_Memo = TBLuni;
151100101115         else;
151200101115           clear  ds3K_Memo;
151300101115         endif;
151400101115
151500101012         clear  ds3K;
151600101012
151700101012         // -?Impostazione altri campi (per tutti)?
151800101012         §3Kcgc = VI2cgc;
151900101012         §3Kcq1 = VI2cq1;
152000101012         §3Kcq2 = VI2cq2;
152100101012         §3Kcq3 = VI2cq3;
152200101012         §3Kcq4 = VI2cq4;
152300101012         §3Kcq5 = VI2cq5;
152400101012         §3Knmc = VI2nmc;
152500101012         §3Kcsd = VI2csd;
152600101117         if  ds3K_Memo.§3Kcdu > *zero;
152700101115           §3Kcdu = ds3K_Memo.§3Kcdu;
152800101115         endif;
152900101012         §3Kcks = VI2cks;
153000101012         §3Kggc = VI2ggc;
153100101012         §3Kgqi = VI2gqi;
153200101012         §3Kgsd = VI2gsd;
153300101012         §3Kcma = VI2cma;
153400170608         //§3Kcba = VI2cba;
153500101012         §3Kapp = VI2app;
153600101012         §3Kcq8 = VI2cq8;
153700101012         §3Kca  = VI2ca ;
153800101012         §3Kvto = VI2vto;
153900101012         §3Kgks = VI2gks;
154000101012         §3Kgsr = VI2gsr;
154100101012         §3Kctb = VI2ctb;
154200101012         §3Kcq6 = VI2cq6;
154300101012         §3Kcq7 = VI2cq7;
154400101012
154500101012       ENDSR;
154600101012
154700101012       //--------------------------------------------------------------
154800101012       //?Esecuzione del comando (già impostato).                      ?
154900101012       //--------------------------------------------------------------
155000101012       BEGSR  sr_ExecCmd;
155100101012
155200101012         clear Qcap0100;
155300101012         Qcabcsdh = *off;
155400101012         Qcapa    = *off;
155500101012         Qcacmdss = *off;
155600101012         Qcaerved = *allX'00';
155700101012
155800101012         clear Qusec;
155900101012         Qusbprv  = %size(Qusec);
156000101012
156100101012         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
156200101012                           %size(Qcap0100) : 'CPOP0100' : *omit :
156300101012                           0 : 0 : Qusec);
156400101012
156500101012         //if  Qusei <> *blank;
156600101012         //  ...;
156700101012         //endif;
156800101012
156900101012       ENDSR;
157000101012
157100101012       //--------------------------------------------------------------
157200101012       //?Operazioni finali.                                           ?
157300101012       //--------------------------------------------------------------
157400101012       BEGSR  sr_RoutEnd;
157500101012
157600101012         cloTabella();
157700101012
157800101012         clear  TIBS69ds;
157900101012         I69tla = 'C';
158000101012         tibs69r( tibs69ds :
158100101012                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
158200101012
158300101012         return;
158400101012
158500101012       ENDSR;
158600101011
158700101011      /end-free
158800101011
158900101011       //--------------------------------------------------------------
159000101011       //?Schiere a tempo di compilazione.                             ?
159100101011       //--------------------------------------------------------------
159200101011
159300101012** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con TABEL00F?
159400101115SETRAS  GAITRAGRU FILTRAGRU
159500101115AS888   GAITRAGRPSFILTRAGRPF
159600101011** - $Msg:?Messaggi di Errore?-----------------------------------------------*
159700101011Filiale inesistente                                                            1
159800101011Linea inesistente                                                              2
159900101011Codice cliente inesistente                                                     3
160000101011Codice cliente NON selezionabile                                               4
160100101011Specificare dati da inviare                                                    5
160200101011Immettere DATI CONSEGNA o DATI GIACENZA o DISPOSIZIONI DA CLIENTE o C.A.       6
160300101011Inserire solo se indicato almeno un ritorno VAC                                7
160400101011Campo obbligatorio                                                             8
160500101011Tipo trasmissione inesistente                                                  9
160600101011Tipo trasmissione annullata                                                   10
160700101011File inesistente                                                              11
160800111020?LIBERO?                                                                      12
160900101011Codice cliente annullato                                                      13
161000101011Specificare tipo giacenze da scaricare                                        14
161100101011Valorizzare abbinato con invio aperture o entrambe                            15
161200101011Dato valido solo per DATI CONSEGNA o DATI GIACENZA o DATI C.A.                16
