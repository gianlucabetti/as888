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
014400101011     d   F20Attivo                     n   overlay(IndDspF : 20)
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
023600101012     d wDate           s              8  0 inz
023700101011     d w_iSystem       s              1  0 inz
023800101011     d w_SisInf        s              3  0 inz
023900101011
024000101011       //--------------------------------------------------------------
024100101011       //?Definizione prototipi procedure.                             ?
024200101011       //--------------------------------------------------------------
024300101011
024400101011       // -?Reperimento dati utente?
024500101011      /copy gaitrasrc/srcProtoPR,TIBS34R
024600101011
024700101011       // -?Parametri per pgm interrogazione tabelle (TABEL00F)?
024800101011      /copy gaitrasrc/srcProtoPI,X§TABER
024900101011       // -?Interrogazione tabelle?
025000101011      /copy gaitrasrc/srcProtoPR,X§TABER
025100101012
025200101012       // -?Reperimento dati tabelle?
025300101012      /copy gaitrasrc/srcProtoPR,TRULTAB
025400101011
025500101011       // -?Parametri per pgm interrogazione sottoconti?
025600101011      /copy gaitrasrc/srcProtoPR,XALFA3BRDS
025700101011       // -?Interrogazione sottoconti?
025800101011      /copy gaitrasrc/srcProtoPR,XALFA3BR
025900101012
026000101012       // -?Controllo/Decodifica cliente?
026100101105      /copy gaitrasrc/srcProtoPI,TIBS69R
026200101012      /copy gaitrasrc/srcProtoPR,TIBS69R
026300101011
026400101011       // -?Gestione invio dati - BOLLETTAZIONE?
026500101012     d trtb28r         pr                  extpgm('TRTB28R')
026600101012     d   kpjba                             likeds(kpjba)
026700101011
026800101011       // -?Gestione scambio dati C/A e fattura con clienti?
026900101011      /copy gaitrasrc/srcProtoPR,TRTB44R
027000101011
027100101011       // -?Gestione scambio dati principali?
027200101011      /copy gaitrasrc/srcProtoPR,TRTB47R
027300101012
027400101012       // -?Reperimento NETA sistema AS/400 corrente?
027500101012      /copy gaitrasrc/srcProtoPr,UBRTVNETA
027600101012
027700101012       // -?Parametri API QCAPCMD (Process Commands)?
027800101012     d Qcmd            s           2048    inz  varying
027900101012      /copy qSysInc/qRpgleSrc,QCAPCMD
028000101012       // -?API QCAPCMD (Process Commands)?
028100101012      /copy gaitrasrc/srcProtoPR,QCAPCMD
028200101012
028300101012       // -?Parametri gestione errori API.?
028400101012      /copy qsysinc/qrpglesrc,QUSEC
028500101011
028600101011       //--------------------------------------------------------------
028700101011       //?Definizione key-list.                                        ?
028800101011       //--------------------------------------------------------------
028900101011
029000101011       // -?File TABEL00F?
029100101011     d k03tabel00    e ds                  extname(TABEL00F : *key)
029200101011     d                                     prefix(k_)   inz
029300101011
029400101011       //--------------------------------------------------------------
029500101011       //?M A I N - L I N E                                            ?
029600101011       //--------------------------------------------------------------
029700101011
029800101011     c     *Entry        plist
029900101011     c                   parm                    KPJBA
030000101011
030100101011      /free
030200101011
030300101011       // -?Operazioni iniziali?
030400101011       exsr sr_RoutInz;
030500101011
030600101011       // -?Gestione videate?
030700101011       DoW  $Fine = *off;
030800101011
030900101011         select;
031000101011           // -?Gestione videata D1 (richiesta codice cliente)?
031100101011           when $Video = 'D1';
031200101011             exsr sr_GesD01;
031300101011           // -?Gestione videata D2 (manutenzione dati cliente)?
031400101011           when $Video = 'D2';
031500101011             exsr sr_GesD02;
031600101011           // -?? ? ? ? ??
031700101011           other;
031800101011             $Fine = *on;
031900101011         endsl;
032000101011
032100101011       enddo;
032200101011
032300101011       // -?Operazioni finali?
032400101011       exsr sr_RoutEnd;
032500101011
032600101011       //--------------------------------------------------------------
032700101011       //?Operazioni iniziali.                                         ?
032800101011       //--------------------------------------------------------------
032900101011       BEGSR  sr_RoutInz;
033000101011
033100101011         *inLR = *on;
033200101011
033300101011         // -?Verifica del sistema AS/400 corrente?
033400101011         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
033500101011           $Fine = *on;
033600101011           leavesr;
033700101011         endif;
033800101011
033900101011         // -?Impostazione elenco librerie in cui gestire le tabelle?
034000101011         //  ?(a seconda del sistema in cui si stà lavorando)?
034100101011         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
034200101011         if  w_iSystem = *zero;
034300101011           $Fine = *on;
034400101011           leavesr;
034500101011         endif;
034600101011
034700101011         // -?Reperimento dati job?
034800101011         exsr  sr_DatiJob;
034900101105
035000101105         // -?Reperimento data odierna?
035100101105         wDate = %int( %subst( %char( %dec( %timestamp() ) )
035200101105                               : 1 : 8 ) );
035300101011
035400101011         // -?Impostazione nome programma a video?
035500101011         V1Tpgm = SDSpgm;
035600101011
035700101011         // -?Impostazione campi "fissi"?
035800101011         k_TBLkut = c_Kut;
035900101011         k_TBLcod = c_Tab;
036000101011
036100101011         // -?Verifica se *pgm richiamato:?
036200101011         //  ?test delle prime 7 posizioni della KPJBA per sapere se il?
036300101011         //  ?pgm è stato richiamato da un altro pgm o da menù tabelle.?
036400101011         clear  W0Ccli;
036500101011         $Called = (%subst( KPJBU : 1 : %len(W0Ccli) ) <> *blank);
036600101011         if  $Called = *on   and
036700101011             %check( c_Digits : %subst( KPJBU : 1 :
036800101011                                       %len(W0Ccli) ) ) = *zero;
036900101011           W0Ccli = %subst( KPJBU : 1 : %len(W0Ccli) );
037000101011         endif;
037100101011
037200101011       ENDSR;
037300101011
037400101011       //--------------------------------------------------------------
037500101011       //?Reperimento Dati del job (Utente/Operativi).                 ?
037600101011       //--------------------------------------------------------------
037700101011       BEGSR  sr_DatiJob;
037800101011
037900101011         in(e) §AzUte;
038000101011         if NOT %error;
038100101011           in(e) §DatiUte;
038200101011         endif;
038300101011         if %error or RSut = *blank;
038400101011           tibs34r ( tibs34ds );
038500101011           in §AzUte;
038600101011           in §DatiUte;
038700101011         endif;
038800101011
038900101011       ENDSR;
039000101011
039100101011       //--------------------------------------------------------------
039200101011       //?Gestione videata D01.                                        ?
039300101011       //--------------------------------------------------------------
039400101011       BEGSR  sr_GesD01;
039500101011
039600101011         // -?Inizializzazione videata?
039700101011         if  $InzD01 = *on;
039800101011           exsr  sr_InzD01;
039900101011           $InzD01 = *off;
040000101011         endif;
040100101011
040200101011         // -?SE ricevuto codice cliente NON si emette la prima videata?
040300101011         if  $Called   and   W0Ccli > *zero;
040400101011           exsr  sr_CtrD01;
040500101011           if  Not ErrGenerico;
040600101011             $Video  = 'D2';
040700101011             $InzD02 = *on;
040800101011             leavesr;
040900101011           else;
041000101011             clear  W0Ccli;
041100101011           endif;
041200101011         endif;
041300101011
041400101011         // -?Emissione Testata?
041500101011         write  TB40T1;
041600101011
041700101011         // -?Emissione videata?
041800101011         exfmt  TB40D1;
041900101011
042000101011         reset  ErrMessage;
042100101011         reset  ErrGenerico;
042200101011         clear  VIDmsg;
042300101011
042400101011         SELECT;
042500101011
042600101011           // -?F3=Fine?
042700101011           WHEN  dsp_aid = c_F03;
042800101011             $Fine = *on;
042900101011
043000101011           // -?F20=Gestione scambio dati principali?
043100101011           // -?F21=Gestione scambio dati bolle?
043200101011           // -?F22=Gestione scambio dati C/A e fattura con clienti?
043300101011           WHEN  dsp_aid = c_F20   or   dsp_aid = c_F21   or
043400101011                 dsp_aid = c_F22;
043500101011             exsr  sr_F2x;
043600101011
043700101011           // -?Invio?
043800101011           OTHER;
043900101011             exsr sr_CtrD01;
044000101011             if  ErrGenerico = *off;
044100101011               $Video    = 'D2';
044200101011               $InzD02   = *on;
044300101011             endif;
044400101011
044500101011         ENDSL;
044600101011
044700101011       ENDSR;
044800101011
044900101011       //--------------------------------------------------------------
045000101011       //?Inizializzazione videata D01 - Richiesta codice cliente.     ?
045100101011       //--------------------------------------------------------------
045200101011       BEGSR  sr_InzD01;
045300101011
045400101011         clear  V1Topz;
045500101011
045600101011         clear  TB40D1;
045700101122
045800101122         // -?Se è stato richiamato con un codice cliente numerico, si?
045900101122         //  ?imposta anche tale codice a video?
046000101122         select;
046100101122           when  $Called   and   W0Ccli > *zero;
046200101122             VIDlnp = %subst( W0Ccli : 1 : %len(VIDlnp) );
046300101122             VIDnup = %int( %subst( W0Ccli : %len(VIDlnp) + 1
046400101122                                           : %len(VIDnup) ) );
046500101122           when  Not $Called;
046600101122             VIDlnp = '?';
046700101122         endsl;
046800101011
046900101011         // -?Abilitazione tasti funzionali?
047000101011         exsr  sr_AbilFxxD01;
047100101011
047200101011       ENDSR;
047300101011
047400101011       //--------------------------------------------------------------
047500101011       //?Abilitazione tasti funzionali in P01 (per D01).              ?
047600101011       //--------------------------------------------------------------
047700101011       BEGSR  sr_AbilFxxD01;
047800101011
047900101011         // -?Se è stato richiamato: nei primi 7 caratteri della KPJBU?
048000101011         //  ?c'è il codice cliente => NON devono essere abilitati i?
048100101011         //  ?tasti funzionali F20, F21 e F22.?
048200101011
048300101011         // ->?F20 = Gestione scambio dati principali scheda clienti?
048400101011         F20Attivo = (Not $Called);
048500101011
048600101011         // ->?F21 = Gestione scambio dati bolle?
048700101011         F21Attivo = (Not $Called);
048800101011
048900101011         // ->?F22 = Gestione scambio dati C/A e fattura con clienti?
049000101011         F22Attivo = (Not $Called);
049100101011
049200101011       ENDSR;
049300101011
049400101011       //--------------------------------------------------------------
049500101011       //?Gestione tasti funzionali F20,F21 e F22 in D01 & D02.        ?
049600101011       //--------------------------------------------------------------
049700101011       BEGSR  sr_F2x;
049800101012
049900101012         if  %check( c_Digits : VIDlnp ) = *zero;
050000101012           %subst( KPJBU : 1 : 3 ) = VIDlnp;
050100101012         else;
050200101012           %subst( KPJBU : 1 : 3 ) = *zero;
050300101012         endif;
050400101012         %subst( KPJBU : 4 : 4 ) = %editc( VIDnup : 'X' );
050500101011
050600101011         select;
050700101011
050800101011           // -?F20 = Gestione scambio dati principali?
050900101011           when  dsp_aid = c_F20;
051000101011             trtb47r ( kpjba );
051100101011
051200101011           // -?F21 = Gestione scambio dati ricezione bolle da clienti?
051300101011           when  dsp_aid = c_F21;
051400101011             trtb28r ( kpjba );
051500101011
051600101011           // -?F22 = Gestione scambio dati C/A e fattura con clienti?
051700101011           when  dsp_aid = c_F22;
051800101011             trtb44r ( kpjba );
051900101011
052000101011         endsl;
052100101011
052200101011       ENDSR;
052300101011
052400101011       //--------------------------------------------------------------
052500101011       //?Controllo dati immessi nella videata D01.                    ?
052600101011       //--------------------------------------------------------------
052700101011       BEGSR  sr_CtrD01;
052800101011
052900101011         %subst(IndDspF : 50) = *off;
053000101011         reset  ProtectAll;
053100101011
053200101011         // -?Filiale per ricerca tabella clienti errata?
053300101011         if  VIDfil > *zero;
053400101011           chain  (VIDfil)  AZORG;
053500101011           if  Not %found(AZORG01L);
053600101011             VIDmsg = $Msg(01);
053700101011             PosCurFIL   = *on;
053800101011             ErrGenerico = *on;
053900101011             leavesr;
054000101011           endif;
054100101011         endif;
054200101011
054300101011         SELECT;
054400101011         // -?Ricerca codice cliente in tabella "3K"?
054500101011           WHEN  %scan('?' : VIDlnp) > *zero;
054600101011             clear  VIDlnp;
054700101011             clear  VIDnup;
054800101011             X§Tkut = c_Kut;
054900101011             X§Tcod = c_Tab;
055000101011             if  VIDfil > *zero;
055100101011               X§Tkey = %editc( VIDfil : 'X' );
055200101011             else;
055300101011               clear  X§Tkey;
055400101011             endif;
055500101011             clear  X§Tdes;
055600101011             x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
055700101011             if  X§Tkey <> *blank;
055800101011               VIDlnp = %subst(X§Tkey : 1 : %len(VIDlnp));
055900101012               VIDnup = %int( %subst( X§Tkey : %len(VIDlnp) + 1
056000101012                                             : %len(VIDnup) ) );
056100101011             else;
056200101011               VIDlnp = '?  ';
056300101011             endif;
056400101011             PosCurLNP   = *on;
056500101011             ErrGenerico = *on;
056600101011             leavesr;
056700101011
056800101011           // -?Ricerca alfabetica?
056900101011           WHEN  VIDnup =  *zero   and
057000101012                 VIDdes <> *blank;
057100101011             xParDut = RSut;
057200101011             xParKut = 1;
057300101011             xParRag = VIDdes;
057400101011             xParKcc = DUTkci;
057500101011             xParSta = *zero;
057600101012             xParFlr = VIDlnp;
057700101011             clear  xParDit;
057800101011             xParNum = 1;
057900101011             clear  xParKcm;
058000101011             clear  xParKsm;
058100101011             clear  xParKdm;
058200101011             clear  xParEsci;
058300101011             clear  xParErr;
058400101011             clear  xParIva;
058500101011             clear  xParCdf;
058600101011             xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
058700101011                        xParSta : xParFlr : xParDit : xParNum :
058800101011                        xParKcm : xParKsm : xParKdm : xParEsci :
058900101011                        xParErr : xParIva : xParCdf );
059000101011             if  xParSta <> -1   and   xParErr = *blank
059100101011                                 and   xParKsm > *zero;
059200101012               VIDlnp = %subst( xParKsm : 1 : %len( VIDlnp ) );
059300101012               VIDnup = %int( %subst( xParKsm : %len( VIDlnp ) + 1
059400101012                                              : %len( VIDnup ) ) );
059500101011               clear  VIDdes;
059600101011             endif;
059700101011             PosCurLNP   = *on;
059800101011             ErrGenerico = *on;
059900101011             leavesr;
060000101011
060100101011           // -?Controllo immissione filiale del codice cliente?
060200101011           WHEN  VIDlnp = *blank  or  VIDlnp = *zero
060300101011                                  or  VIDnup = *zero;
060400101011             VIDmsg = $Msg(02);
060500101011             PosCurLNP   = *on;
060600101011             ErrMessage  = *on;
060700101011             ErrGenerico = *on;
060800101011             leavesr;
060900101011
061000101011           // -?Controllo immisione solo caratteri numerici?
061100101011           WHEN  %check( c_Digits : VIDlnp ) > *zero;
061200101011             VIDmsg = $Msg(02);
061300101011             PosCurLNP   = *on;
061400101011             ErrMessage  = *on;
061500101011             ErrGenerico = *on;
061600101011             leavesr;
061700101011
061800101011         ENDSL;
061900101011
062000101011         // -?Controllo filiale?
062100101011         ORGfil = %int( VIDlnp);
062200101011         chain  ORGfil  AZORG;
062300101012         if  NOT %found(AZORG01L)   or   ORGfva <> *blank;
062400101011           VIDmsg = $Msg(02);
062500101011           PosCurLNP   = *on;
062600101011           ErrMessage  = *on;
062700101011           ErrGenerico = *on;
062800101011           leavesr;
062900101011         endif;
063000101011
063100101011         // -?Controllo / decodifica cliente?
063200101011         clear  TIBS69ds;
063300101011         I69sif = knsif;
063400101011         I69kcc = DUTkci;
063500101011         I69kac = (%int(VIDlnp) * 10000) + VIDnup;
063600101011         tibs69r( tibs69ds :
063700101011                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
063800101011         if  O69err = *on;
063900101011           VIDmsg = $Msg(03);
064000101011           PosCurNUP   = *on;
064100101011           ErrMessage  = *on;
064200101011           ErrGenerico = *on;
064300101011           leavesr;
064400101011         endif;
064500101011
064600101011         if  VIDnup = 8888   or   VIDnup = 9999;
064700101012           VIDmsg = $Msg(04);
064800101011           PosCurNUP   = *on;
064900101011           ErrMessage  = *on;
065000101011           ErrGenerico = *on;
065100101011           leavesr;
065200101011         endif;
065300101115
065400101115         // -?Composizione chiave tabella?
065500101115         k_TBLkey = VIDlnp + %editc( VIDnup : 'X' );
065600101011
065700101115         // -?Reperimento Data Ultima Trasmissione dalla tab. di fil.?
065800101115         clear  ds3K_Memo;
065900101115         if  w_SisInf <> 2   or   Not %open(TABEL00F);
066000101115           w_SisInf = 2;
066100101115           exsr  sr_OpenFileTab;
066200101115         endif;
066300101115         chain(N)  %kds(k03tabel00)  TABEL;
066400101115         if  %found(TABEL00F);
066500101115           ds3K_Memo = TBLuni;
066600101115         endif;
066700101115
066800101115         // -?Apertura file TABEL00F del 1° S.I. (sede)?
066900101011         if  w_SisInf <> 1   or   Not %open(TABEL00F);
067000101011           w_SisInf = 1;
067100101011           exsr  sr_OpenFileTab;
067200101011         endif;
067300101011
067400101011         // -?Verifica esistenza del record in TABEL?
067500101011         chain  %kds(k03tabel00)  TABEL;
067600101011
067700101011       ENDSR;
067800101011
067900101011       //--------------------------------------------------------------
068000101011       //?Apertura dei files tabelle nel sistema informativo impostato.?
068100101011       //--------------------------------------------------------------
068200101011       BEGSR  sr_OpenFileTab;
068300101011
068400101011         // -?Chiusura (eventuale) archivi?
068500101011         if  %open(TABEL00F);
068600101011           close  TABEL00F;
068700101011         endif;
068800101011         cloTabella();
068900101011
069000101011         // -?Apertura archivi?
069100101011         ds_Libr  = $Libraries(w_iSystem);
069200101012         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
069300101011         open  TABEL00F;
069400101011
069500101011       ENDSR;
069600101011
069700101011       //--------------------------------------------------------------
069800101011       //?Gestione videata D02.                                        ?
069900101011       //--------------------------------------------------------------
070000101011       BEGSR  sr_GesD02;
070100101011
070200101011         // -?Inizializzazione videata?
070300101011         if  $InzD02 = *on;
070400101011           exsr  sr_InzD02;
070500101011           $InzD02 = *off;
070600101011         endif;
070700101011
070800101011         // -?(Ri)Abilitazione tasti funzionali?
070900101011         exsr  sr_AbilFxxD02;
071000101011
071100101011         // -?Emissione Testata e Piede?
071200101011         write  TB40T1;
071300101011
071400101011         // -?Emissione videata?
071500101011         if  Not ProtectAll;
071600101011           exfmt  TB40D2;
071700101011         else;
071800101011           write  TB40D2;
071900101011           exfmt  Protect;
072000101011         endif;
072100101011
072200101011         reset  ErrMessage;
072300101011         reset  ErrGenerico;
072400101011         clear  VIDmsg;
072500101011
072600101011         SELECT;
072700101011
072800101011           // -?F3=Fine?
072900101011           WHEN  dsp_aid = c_F03;
073000101011             unlock  TABEL00F;
073100101011             $Fine = *on;
073200101011
073300101011           // -?F12=Ritorno?
073400101011           WHEN  dsp_aid = c_F12;
073500101011             unlock  TABEL00F;
073600101011             $Video = 'D1';
073700101011
073800101011           // -?F20=Gestione scambio dati principali     con clienti?
073900101011           // -?F21=Gestione scambio dati Cons/Giac/Disp con clienti?
074000101011           // -?F22=Gestione scambio dati C/A e fattura  con clienti?
074100101012           WHEN  dsp_aid = c_F20   or   dsp_aid = c_F21   or
074200101011                 dsp_aid = c_F22;
074300101011             exsr  sr_F2x;
074400101011
074500101011           // -?Invio, F5=Ripristino, F6=Aggiornamento, F16=Annullamento?
074600101011           OTHER;
074700101012             // -?Controlli (eseguiti anche se F16=Annullamento)?
074800101012             exsr sr_CtrD02;
074900101011             if  ErrGenerico = *on;
075000101011               leavesr;
075100101011             endif;
075200101011             // -?Aggiornamento?
075300101012             If  dsp_aid = c_F05   or
075400101011                 dsp_aid = c_F06   or
075500101011                 dsp_aid = c_F16;
075600101012               exsr  sr_UpdateAll;
075700101012               if  ErrGenerico = *on;
075800101012                 leavesr;
075900101012               endif;
076000101012               // ·?Uscita dal programma SE richiamato?
076100101012               if  $Called = *on   and   W0Ccli > *zero;
076200101012                 $Fine = *on;
076300101012               // ·?Videata iniziale altrimenti?
076400101012               else;
076500101012                 $Video  = 'D1';
076600101012                 $InzD01 = *on;
076700101012               endif;
076800101012             EndIf;
076900101011
077000101011         ENDSL;
077100101011
077200101011         // -?Pulizia del sotto-titolo in testata al ritorno in D01?
077300101011         if  $Video = 'D1';
077400101011           clear  V1Topz;
077500101011           VisBloAnn = *off;
077600101011         endif;
077700101011
077800101011       ENDSR;
077900101011
078000101011       //--------------------------------------------------------------
078100101011       //?Inizializzazione dati nella videata D02.                     ?
078200101011       //--------------------------------------------------------------
078300101011       BEGSR  sr_InzD02;
078400101011
078500101011         clear  TB40D2;
078600101011         clear  ds3K;
078700101011         VisBloAnn = *off;
078800101011
078900101011         // -?Impostazione testata?
079000101011         reset  ProtectAll;
079100101011         clear  V1Topz;
079200101011         select;
079300101011           when  Not %found(TABEL00F);
079400101011             V1Topz = '  INSERIMENTO  ';
079500101011           when  TBLflg = '*';
079600101011             V1Topz = '  RIPRISTINO   ';
079700101011           other;
079800101011             V1Topz = '   MODIFICA    ';
079900101011         endsl;
080000101011         select;
080100101011           // -?Codice annullato?
080200101011           when  ACOflg <> *blank;
080300101011             if  %found(TABEL00F);
080400101115               VI2aob   =  'ANNULLATO';
080500101011               ProtectAll = *on;
080600101011               VisBloAnn  = *on;
080700101011             endif;
080800101011           // -?Codice bloccato?
080900130315           when  ACOabl <> *blanks;
081000101011             if  %found(TABEL00F);
081100101115               VI2aob   =  'BLOCCATO';
081200101011               VisBloAnn  = *on;
081300101011             endif;
081400101011         endsl;
081500101011
081600101011         // -?Impostazione dettaglio?
081700101011         VI2cod = I69kac;
081800101011         VI2rag = ACOrag;
081900101011
082000101011         If  %found(TABEL00F);
082100101011
082200101011           ds3K = TBLuni;
082300101011           // -?Dati consegna:?
082400101115           SaveNMC = §3Knmc;
082500101115           HI2cdu  = ds3K_Memo.§3Kcdu;
082600101115           if  ds3K_Memo.§3Kcdu > *zero;
082700101115             wDate_Eur = %date( ds3K_Memo.§3Kcdu : *ymd );
082800101011             VI2cdu = ( ( %subdt(wDate_Eur : *days) * 1000000 ) +
082900101011                        ( %subdt(wDate_Eur : *months) * 10000 ) +
083000101011                        ( %subdt(wDate_Eur : *years) ) );
083100101011           endif;
083200101011           VI2cq1 = §3Kcq1;
083300101011           VI2cq2 = §3Kcq2;
083400101011           VI2cq3 = §3Kcq3;
083500101011           VI2cq4 = §3Kcq4;
083600101011           VI2cq5 = §3Kcq5;
083700101011           VI2cq6 = §3Kcq6;
083800101011           VI2cq7 = §3Kcq7;
083900101011           VI2cq8 = §3Kcq8;
084000101011           VI2nmc = §3Knmc;
084100101011           VI2vto = §3Kvto;
084200101011           VI2cma = §3Kcma;
084300101011           VI2cba = §3Kcba;
084400101011           VI2ctb = §3Kctb;
084500101011           // - ·?Scarico dati:?
084600101011           VI2cgc = §3Kcgc;
084700101011           VI2csd = §3Kcsd;
084800101011           VI2cks = §3Kcks;
084900101011           // -?Dati giacenza:?
085000101011           VI2gqi = §3Kgqi;
085100101011           VI2app = §3Kapp;
085200101011           // - ·?Scarico dati:?
085300101011           VI2ggc = §3Kggc;
085400101011           VI2gsd = §3Kgsd;
085500101011           VI2gks = §3Kgks;
085600130521           if §3Kca <> *blank;
085700130521             VI2gksVGD = §3Kgks;
085800130521           endif;
085900101011           // -?Disposizioni da clienti:?
086000101011           // - ·?Scarico dati:?
086100101011           VI2gsr = §3Kgsr;
086200101011           // -?Dati completi C.A. (VGD)?
086300101011           VI2ca  = §3Kca;
086400101011
086500101011         EndIf;
086600101011
086700101011         // -?Decodifica cliente unificante (dati consegna)?
086800101011         select;
086900101011           when  §3Kcks = *zero;
087000101011           when  §3Kcks = VI2cod;
087100101011             VI2ric = VI2rag;
087200101011           other;
087300101011             clear  TIBS69ds;
087400101011             I69sif = knsif;
087500101011             I69kcc = DUTkci;
087600101011             I69kac = §3Kcks;
087700101011             tibs69r( tibs69ds :
087800101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
087900101115             if  O69err = *on;
088000101115               clear  VI2ric;
088100101115             else;
088200101011               VI2ric = ACOrag;
088300101011             endif;
088400101011         endsl;
088500101011         // -?Decodifica cliente unificante (dati giacenza)?
088600101011         select;
088700101011           when  §3Kgks = *zero;
088800101011           when  §3Kgks = VI2cod;
088900101011             VI2ri2 = VI2rag;
089000101011           other;
089100101011             clear  TIBS69ds;
089200101011             I69sif = knsif;
089300101011             I69kcc = DUTkci;
089400101115             I69kac = §3Kgks;
089500101011             tibs69r( tibs69ds :
089600101011                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
089700101115             if  O69err = *on;
089800101115               clear  VI2ri2;
089900101115             else;
090000101011               VI2ri2 = ACOrag;
090100101011             endif;
090200101011         endsl;
090300101011
090400101011       ENDSR;
090500101011
090600101011       //--------------------------------------------------------------
090700101011       //?Abilitazione tasti funzionali in P02 (per D02 e D03).        ?
090800101011       //--------------------------------------------------------------
090900101011       BEGSR  sr_AbilFxxD02;
091000101011
091100101011         // ->?F5 = Ripristino?
091200101011         F5Attivo = (%found(TABEL00F)   and   TBLflg <> *blank);
091300101011
091400101011         // ->?F6 = Conferma?
091500101011         F6Attivo = ( ( %found(TABEL00F)  and  TBLflg = *blank  and
091600101012                        Not ProtectAll )   OR  Not %found(TABEL00F) );
091700101011
091800101011         // ->?F12 = Ritorno?
091900101011         F12Attivo = (Not $Called);
092000101011
092100101011         // ->?F16 = Annullamento?
092200101011         F16Attivo = (%found(TABEL00F)   and   TBLflg = *blank);
092300101011
092400101011         // -?Abilitazione F20, F21 e F22 SE NON richiamato?
092500101011         // ->?F20 = Multimembro clienti VAB?
092600101011         F20Attivo = (Not $Called);
092700101011         // ->?F21 = Gestione dati Consegna/Giacenza per clienti?
092800101011         F21Attivo = (Not $Called);
092900101011         // ->?F22 = Gestione dati C/A e Fattura per clienti?
093000101011         F22Attivo = (Not $Called);
093100101011
093200101011       ENDSR;
093300101011
093400101011       //--------------------------------------------------------------
093500101011       //?Controllo dati immessi nella videata D02.                    ?
093600101011       //--------------------------------------------------------------
093700101011       BEGSR  sr_CtrD02;
093800101011
093900101011         %subst(IndDspF : 50) = *off;
094000101011
094100130521         // -?se impostato "DAT COMPLETI C.A." allora valorizzo anche il suo cod
094200130521         //  ?cliente unificante
094300130521         if VI2ca <> *blank;
094400130521           VI2gksVGD = VI2gks;
094500130521         else;
094600130521           VI2gksVGD = 0;
094700130521         endif;
094800101012
094900101012         // -?DATI CONSEGNA?
095000101012         IF  ds_VI2cq <> *blank;
095100101012
095200101012           // -?SCARICO DATI: giorni scadenza?
095300101012           if  VI2cgc <= *zero;
095400101012             VIDmsg = $Msg(08);
095500101012             PosCurCGC   = *on;
095600101012             ErrMessage  = *on;
095700101012             ErrGenerico = *on;
095800101012             leavesr;
095900101012           endif;
096000101012
096100101012           // -?SCARICO DATI: tipo invio?
096200101012           Select;
096300101012             // -?Ricerca?
096400101012             When  %scan('?' : VI2csd) > *zero;
096500101012               X§Tkut = c_Kut;
096600101012               X§Tcod = '3T';
096700101012               clear  X§Tkey;
096800101012               clear  X§Tdes;
096900101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
097000101012               VI2csd = %subst(X§Tkey : 1 : %len(VI2csd));
097100101012               ErrGenerico = *on;
097200101012               leavesr;
097300101012             // -?Controllo effettiva immissione?
097400101012             When  VI2csd = *blank;
097500101012               VIDmsg = $Msg(08);
097600101012               PosCurCSD   = *on;
097700101012               ErrMessage  = *on;
097800101012               ErrGenerico = *on;
097900101012               leavesr;
098000101012             // -?Controllo validità codice immesso?
098100101012             Other;
098200101012               ds_TNTBE.TBEke1 = VI2csd;
098300101012               select;
098400101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
098500101012                                    *omit   : *omit : *omit :
098600101012                                    *omit   : *omit :
098700101012                                    *omit   : *omit : *omit : *omit :
098800101012                                    *omit   : *omit : *omit : *omit :
098900101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
099000101012                                  < *zero;
099100101012                   VIDmsg = $Msg(09);
099200101012                   PosCurCSD   = *on;
099300101012                   ErrMessage  = *on;
099400101012                   ErrGenerico = *on;
099500101012                   leavesr;
099600101012                 when  ds_TNTBE.TBEatb <> *blank;
099700101012                   VIDmsg = $Msg(10);
099800101012                   PosCurCSD   = *on;
099900101012                   ErrMessage  = *on;
100000101012                   ErrGenerico = *on;
100100101012                   leavesr;
100200101012               endsl;
100300101012
100400101012           EndSl;
100500101012
100600101012           // -?SCARICO DATI: codice cliente unificante?
100700101012           Select;
100800101012             // -?Ricerca alfabetica?
100900101012             When  VI2cks = *zero   and   VI2ric <> *blank;
101000101012               xParRag = VI2ric;
101100101012               exsr  sr_CallXAlfa3B;
101200101012               if  %subst( xParKsm : 1 : %len(VI2cks) ) > *zero;
101300101012                 VI2cks = ACOksc;
101400101012                 VI2ric = ACOrag;
101500101012               endif;
101600101012               ErrGenerico = *on;
101700101012               leavesr;
101800101012             // -?Controllo effettiva immissione?
101900101012             When  VI2cks = *zero   and   VI2ric = *blank;
102000101012               VIDmsg = $Msg(08);
102100101012               PosCurCKS   = *on;
102200101012               ErrMessage  = *on;
102300101012               ErrGenerico = *on;
102400101012               leavesr;
102500101012             // -?Controllo validità codice immesso?
102600101012             Other;
102700101012               clear  TIBS69ds;
102800101012               I69sif = knsif;
102900101012               I69kcc = DUTkci;
103000101012               I69kac = VI2cks;
103100101012               tibs69r( tibs69ds :
103200101012                        ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
103300101012               select;
103400101012                 when  O69err = *on;
103500101012                   VIDmsg = $Msg(03);
103600101012                   PosCurCKS   = *on;
103700101012                   ErrMessage  = *on;
103800101012                   ErrGenerico = *on;
103900101012                   leavesr;
104000101012                 when  ACOflg <> *blank;
104100101012                   VIDmsg = $Msg(13);
104200101012                   PosCurCKS   = *on;
104300101012                   ErrMessage  = *on;
104400101012                   ErrGenerico = *on;
104500101012                   leavesr;
104600101012                 other;
104700101012                   VI2ric = %subst( ACOrag : 1 : %len(VI2ric) );
104800101012               endsl;
104900101012           EndSl;
105000101012
105100101012         ENDIF;
105200101012
105300101012         // -?Dati da inviare mancanti?
105400101012         if  ds_VI2cq = *blank   AND   (VI2cgc <> *zero    or
105500101012                                        VI2csd <> *blank   or
105600101012                                        VI2cks <> *zero    or
105700101012                                        VI2ric <> *blank);
105800101012           VIDmsg = $Msg(05);
105900101012           PosCurCQ1   = *on;
106000101012           ErrMessage  = *on;
106100101012           ErrGenerico = *on;
106200101012           leavesr;
106300101012         endif;
106400101012
106500101012
106600101012         // -?DATI GIACENZA?
106700101012         // -?Tipo giacenze da scaricare obbligatorio?
106800101012         if  VI2gqi = *blank   and   VI2nmc = *blank   and
106900101012            (VI2ggc <> *zero    or
107000111020             VI2gsd <> *blank);
107100101012           VIDmsg = $Msg(14);
107200101012           PosCurGQI   = *on;
107300101012           ErrMessage  = *on;
107400101012           ErrGenerico = *on;
107500101012           leavesr;
107600101012         endif;
107700101012
107800101012         // -?Appuntamento?
107900101012         if  VI2app <> *blank   and   (VI2gqi = 'C'   or
108000101012                                       VI2gqi = *blank);
108100101012           VIDmsg = $Msg(15);
108200101012           PosCurAPP   = *on;
108300101012           ErrMessage  = *on;
108400101012           ErrGenerico = *on;
108500101012           leavesr;
108600101012         endif;
108700101012
108800101012         IF  VI2nmc <> *blank   or   VI2gqi <> *blank;
108900101012
109000101012           // -?SCARICO DATI: giorni scadenza?
109100101012           if  VI2ggc <= *zero;
109200101012             VIDmsg = $Msg(08);
109300101012             PosCurGGC   = *on;
109400101012             ErrMessage  = *on;
109500101012             ErrGenerico = *on;
109600101012             leavesr;
109700101012           endif;
109800101012
109900101012           // -?SCARICO DATI: tipo invio?
110000101012           Select;
110100101012             // -?Ricerca?
110200101012             When  %scan('?' : VI2gsd) > *zero;
110300101012               X§Tkut = c_Kut;
110400101012               X§Tcod = '3T';
110500101012               clear  X§Tkey;
110600101012               clear  X§Tdes;
110700101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
110800101012               VI2gsd = %subst(X§Tkey : 1 : %len(VI2gsd));
110900101012               ErrGenerico = *on;
111000101012               leavesr;
111100101012             // -?Controllo effettiva immissione?
111200101012             When  VI2gsd = *blank;
111300101012               VIDmsg = $Msg(08);
111400101012               PosCurGSD   = *on;
111500101012               ErrMessage  = *on;
111600101012               ErrGenerico = *on;
111700101012               leavesr;
111800101012             // -?Controllo validità codice immesso?
111900101012             Other;
112000101012               ds_TNTBE.TBEke1 = VI2gsd;
112100101012               select;
112200101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
112300101012                                    *omit   : *omit : *omit :
112400101012                                    *omit   : *omit :
112500101012                                    *omit   : *omit : *omit : *omit :
112600101012                                    *omit   : *omit : *omit : *omit :
112700101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
112800101012                                  < *zero;
112900101012                   VIDmsg = $Msg(09);
113000101012                   PosCurGSD   = *on;
113100101012                   ErrMessage  = *on;
113200101012                   ErrGenerico = *on;
113300101012                   leavesr;
113400101012                 when  ds_TNTBE.TBEatb <> *blank;
113500101012                   VIDmsg = $Msg(10);
113600101012                   PosCurGSD   = *on;
113700101012                   ErrMessage  = *on;
113800101012                   ErrGenerico = *on;
113900101012                   leavesr;
114000101012               endsl;
114100101012
114200101012           EndSl;
114300101012
114400101012         ENDIF;
114500101012
114600101012         // -?SCARICO DATI: codice cliente unificante?
114700101012         Select;
114800101012           // -?Ricerca alfabetica?
114900101012           When  VI2gks =  *zero   and   VI2ri2 <> *blank;
115000101012             xParRag = VI2ri2;
115100101012             exsr  sr_CallXAlfa3B;
115200101012             if  %subst( xParKsm : 1 : %len(VI2gks) ) > *zero;
115300101012               VI2gks = ACOksc;
115400101012               VI2ri2 = ACOrag;
115500101012             endif;
115600101012             ErrGenerico = *on;
115700101012             leavesr;
115800101012           // -?Controllo effettiva immissione?
115900101012           //  ?(NON È OBBLIGATORIO!!!)?
116000101012           //When  VI2gks =  *zero   and   VI2ri2 = *blank;
116100101012           //  VIDmsg = $Msg(08);
116200101012           //  PosCurGKS   = *on;
116300101012           //  ErrMessage  = *on;
116400101012           //  ErrGenerico = *on;
116500101012           //  leavesr;
116600101012           // -?Controllo validità codice immesso?
116700101012           When  VI2gks <> *zero;
116800101012             clear  TIBS69ds;
116900101012             I69sif = knsif;
117000101012             I69kcc = DUTkci;
117100101012             I69kac = VI2gks;
117200101012             tibs69r( tibs69ds :
117300101012                      ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
117400101012             select;
117500101012               when  O69err = *on;
117600101012                 VIDmsg = $Msg(03);
117700101012                 PosCurGKS   = *on;
117800101012                 ErrMessage  = *on;
117900101012                 ErrGenerico = *on;
118000101012                 leavesr;
118100101012               when  ACOflg <> *blank;
118200101012                 VIDmsg = $Msg(13);
118300101012                 PosCurGKS   = *on;
118400101012                 ErrMessage  = *on;
118500101012                 ErrGenerico = *on;
118600101012                 leavesr;
118700101012               other;
118800101012                 VI2ri2 = %subst( ACOrag : 1 : %len(VI2ri2) );
118900101012             endsl;
119000101012         EndSl;
119100101012
119200101012
119300101012         // -?DISPOSIZIONI DA CLIENTE?
119400101012         IF  VI2gsr <> *blank;
119500101012
119600101012           // -?SCARICO DATI: tipo ricezione?
119700101012           Select;
119800101012             // -?Ricerca?
119900101012             When  %scan('?' : VI2gsr) > *zero;
120000101012               X§Tkut = c_Kut;
120100101012               X§Tcod = '3T';
120200101012               clear  X§Tkey;
120300101012               clear  X§Tdes;
120400101012               x§taber ( X§Tkut : X§Tcod : X§Tkey : X§Tdes );
120500101012               VI2gsr = %subst(X§Tkey : 1 : %len(VI2gsr));
120600101012               ErrGenerico = *on;
120700101012               leavesr;
120800101012             // -?Controllo effettiva immissione?
120900101012             //  ?(NON È OBBLIGATORIO!!!)?
121000101012             //When  VI2gsr = *blank;
121100101012             //  VIDmsg = $Msg(08);
121200101012             //  PosCurGSR   = *on;
121300101012             //  ErrMessage  = *on;
121400101012             //  ErrGenerico = *on;
121500101012             //  leavesr;
121600101012             // -?Controllo validità codice immesso?
121700101012             When  VI2gsr <> *blank;
121800101012               ds_TNTBE.TBEke1 = VI2gsr;
121900101012               select;
122000101012                 when  getTabella ( c_Tabel : '3T'  : ds_TNTBE.TBEke1 :
122100101012                                    *omit   : *omit : *omit :
122200101012                                    *omit   : *omit :
122300101012                                    *omit   : *omit : *omit : *omit :
122400101012                                    *omit   : *omit : *omit : *omit :
122500101012                                    ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
122600101012                                  < *zero;
122700101012                   VIDmsg = $Msg(09);
122800101012                   PosCurGSR   = *on;
122900101012                   ErrMessage  = *on;
123000101012                   ErrGenerico = *on;
123100101012                   leavesr;
123200101012                 when  ds_TNTBE.TBEatb <> *blank;
123300101012                   VIDmsg = $Msg(10);
123400101012                   PosCurGSR   = *on;
123500101012                   ErrMessage  = *on;
123600101012                   ErrGenerico = *on;
123700101012                   leavesr;
123800101012               endsl;
123900101012
124000101012           EndSl;
124100101012
124200101012         ENDIF;
124300101012
124400101012         // -?SCARICO DATI: codice cliente unificante?
124500101012         If  VI2gks =  *zero;
124600101012           if  VI2gsr <> *blank   or
124700101012               VI2gsd <> *blank   or
124800101012               VI2ca  <> *blank;
124900101012             VIDmsg = $Msg(08);
125000101012             PosCurGKS   = *on;
125100101012             ErrMessage  = *on;
125200101012             ErrGenerico = *on;
125300101012             leavesr;
125400101012           endif;
125500101012         Else;
125600101012           if  VI2gsr =  *blank   and
125700101012               VI2gsd =  *blank   and
125800101012               VI2ca  =  *blank;
125900101012             VIDmsg = $Msg(16);
126000101012             PosCurGKS   = *on;
126100101012             ErrMessage  = *on;
126200101012             ErrGenerico = *on;
126300101012             leavesr;
126400101012           endif;
126500101012         EndIf;
126600101012
126700101012         if  ds_VI2cq = *blank   and
126800101012             VI2gsr   = *blank   and
126900101012             VI2gqi   = *blank   and
127000101012             VI2nmc   = *blank   and
127100101012             VI2ca    = *blank;
127200101012           VIDmsg = $Msg(06);
127300101115           PosCurCQ1   = *on;
127400101012           ErrMessage  = *on;
127500101012           ErrGenerico = *on;
127600101012           leavesr;
127700101012         endif;
127800101012
127900101012         // -?SE inserito il flag "Eslcudi VAC x triangolazione ORM"?
128000101012         //  ?almeno uno dei dati del VAC deve essere inserito?
128100101012         if  VI2vto <> *blank   and
128200101012             VI2cq1 =  *blank   and
128300101012             VI2cq4 =  *blank   and
128400101012             VI2cq2 =  *blank   and
128500101012             VI2cq5 =  *blank   and
128600101012             VI2cq8 =  *blank   and
128700101012             VI2cq3 =  *blank;
128800101012           VIDmsg = $Msg(07);
128900101012           PosCurVTO   = *on;
129000101012           ErrMessage  = *on;
129100101012           ErrGenerico = *on;
129200101012           leavesr;
129300101012         endif;
129400101012
129500101012         // -?Ultimo giro di controllo se si tratta di generare?
129600101012         //  ?l'Estero EDI?
129700101012         reset  $Found_PT;
129800101012         IF  VI2csd = 'EW';
129900101012
130000101012           setll  ('PT')  EDTAB000;
130100101012           reade  ('PT')  EDTAB000;
130200101012           DoW  Not %eof(EDTAB01L)  and  Not $Found_PT;
130300101012             EDIdsPT = TABuni;
130400101012             if  §PTksc = VI2cks;
130500101012               $Found_PT = *on;
130600101012             else;
130700101012               reade  ('PT')  EDTAB000;
130800101012             endif;
130900101012           EndDo;
131000101012
131100101012           If  Not $Found_PT;
131200101012             exfmt  TB40W2;
131300101012           EndIf;
131400101012
131500101012         ENDIF;
131600101115
131700101115         // -?Emissione avviso (all'F6) se richiesto l'Invio?
131800101115         //  ?Motivo Mancata Consegna (VAG)?
131900101115         If  dsp_aid = c_F06    and
132000101115             SaveNMC = *blank   and   VI2nmc = 'S';
132100101115           exfmt  TB40W1;
132200101115           dsp_aid = c_F06;
132300101115         EndIf;
132400101011
132500101011       ENDSR;
132600101012
132700101012       //--------------------------------------------------------------
132800101012       //?Richiamo *pgm XALFA3BR per ricerca cliente.                  ?
132900101012       //--------------------------------------------------------------
133000101012       BEGSR  sr_CallXAlfa3B;
133100101012
133200101012         xParDut = RSut;
133300101012         xParKut = 1;
133400101012         //xParRag = VI2...;
133500101012         xParKcc = DUTkci;
133600101012         xParSta = *zero;
133700101012         clear  xParFlr;
133800101012         clear  xParDit;
133900101012         xParNum = 1;
134000101012         clear  xParKcm;
134100101012         clear  xParKsm;
134200101012         clear  xParKdm;
134300101012         clear  xParEsci;
134400101012         clear  xParErr;
134500101012         clear  xParIva;
134600101012         clear  xParCdf;
134700101012
134800101012         xAlfa3Br ( xParDut : xParKut : xParRag : xParKcc :
134900101012                    xParSta : xParFlr : xParDit : xParNum :
135000101012                    xParKcm : xParKsm : xParKdm : xParEsci :
135100101012                    xParErr : xParIva : xParCdf );
135200101012
135300101012         clear  ds_cnaco;
135400101012
135500101012         If  xParSta <> -1   and   xParErr = *blank   and
135600101012             %subst( xParKsm : 1 : %len( I69kac ) ) > *zero;
135700101012
135800101012           clear  TIBS69ds;
135900101012           I69sif = knsif;
136000101012           I69kcc = DUTkci;
136100101012           I69kac = %int( %subst( xParKsm : 1 : %len( VI2cks ) ) );
136200101012
136300101012           tibs69r( tibs69ds :
136400101012                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
136500101012
136600101012         EndIf;
136700101012
136800101012       ENDSR;
136900101012
137000101012       //--------------------------------------------------------------
137100101012       //?Aggiornam. rec. di TABEL00F in tutti i sistemi informativi   ?
137200101012       //--------------------------------------------------------------
137300101012       BEGSR  sr_UpdateAll;
137400101012
137500101012         // -?Ciclo di elaborazione per ogni sistema informativo?
137600101012         For  w_SisInf = 1  To  %elem($Libr);
137700101012
137800101012           // -?Apertura degli archivi?
137900101012           if  w_SisInf > 1;
138000101012             exsr  sr_OpenFileTab;
138100101012           endif;
138200101012
138300101012           // -?Aggiornamento tab. "3K"?
138400101012           exsr  sr_UpdTABEL;
138500101012
138600101012         EndFor;
138700101012
138800101012       ENDSR;
138900101012
139000101012       //--------------------------------------------------------------
139100101012       //?Aggiornamento record nel file TABEL00F.                      ?
139200101012       //--------------------------------------------------------------
139300101012       BEGSR  sr_UpdTABEL;
139400101012
139500101012         //? N.B.                                                      ?
139600101012         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
139700101012         //   trasmissione (vedi flag TBEFTR).                        ?
139800101012         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
139900101012         //   subito nello stesso file di entrambi i S.I. - in due    ?
140000101012         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
140100101012
140200101012         // -?Aggiornam. tab. "3K" = Invio dati -P.O.D./Giacenze-?   ?
140300101012
140400101012         // -?RI-aggancio record da aggiornare?
140500101012         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
140600101012         if  w_SisInf > 1;
140700101012           k_TBLcod = c_Tab;
140800101012           k_TBLkey = %editc( VI2cod : 'X' );
140900101012           chain  %kds(k03tabel00)  TABEL;
141000101012         endif;
141100101012
141200101012         SELECT;
141300101012
141400101012           // -?F5 = Ripristino?
141500101012           WHEN  dsp_aid = c_F05;
141600101012
141700101012             if  Not %found(TABEL00F);
141800101012               clear  TABEL;
141900101012               TBLkut = c_Kut;
142000101012               TBLcod = c_Tab;
142100101012               TBLkey = k_TBLkey;
142200101012             endif;
142300101012             exsr  sr_RieDS3K;
142400101012             TBLuni = ds3K;
142500101012
142600101012             select;
142700101012               when  %found(TABEL00F)   and   TBLflg = '*';
142800101012                 clear  TBLflg;
142900101012                 clear  TBLftr;
143000101012                 clear  TBLdtr;
143100101012                 //____________
143200101012                 update  TABEL;
143300101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
143400101012               // - Record appena cancellato fisicamente
143500101012               when  NOT %found(TABEL00F);
143600101012                 TBLftt = '1';
143700101012                 clear  TBLflt;
143800101115                 if  w_SisInf = 1;
143900101012                   TBLftr = 'T';
144000101012                 else;
144100101012                   TBLftr = 'R';
144200101012                 endif;
144300101012                 TBLdtr = %int( %subst( %editc(wDate : 'X') :
144400101012                                        %len(wDate) - %len(TBLdtr) + 1 :
144500101012                                        %len(TBLdtr) ) );
144600101012                 //____________
144700101012                 write   TABEL;
144800101012                 //¯¯¯¯¯¯¯¯¯¯¯¯
144900101012             endsl;
145000101012
145100101012           // -?F6 = Aggiornamento / Inserimento?
145200101012           WHEN  dsp_aid = c_F06;
145300101012
145400101012             if  Not %found(TABEL00F);
145500101012               clear  TABEL;
145600101012               TBLkut = c_Kut;
145700101012               TBLcod = c_Tab;
145800101012               TBLkey = k_TBLkey;
145900101012             endif;
146000101012             exsr  sr_RieDS3K;
146100101012             TBLuni = ds3K;
146200101012
146300101012             TBLftt = '1';
146400101012             clear  TBLflt;
146500101115             if  w_SisInf = 1;
146600101012               TBLftr = 'T';
146700101012             else;
146800101012               TBLftr = 'R';
146900101012             endif;
147000101012             TBLdtr = %int( %subst( %editc(wDate : 'X') :
147100101012                                    %len(wDate) - %len(TBLdtr) + 1 :
147200101012                                    %len(TBLdtr) ) );
147300101012
147400101012             if  %found(TABEL00F);
147500101012               //____________
147600101012               update  TABEL;
147700101012               //¯¯¯¯¯¯¯¯¯¯¯¯
147800101012             else;
147900101012               //____________
148000101012               write   TABEL;
148100101012               //¯¯¯¯¯¯¯¯¯¯¯¯
148200101012             endif;
148300101012
148400101012           // -?F16 = Annullamento?
148500101012           WHEN  dsp_aid = c_F16;
148600101012
148700101012             if  %found(TABEL00F)   and   TBLflg   = *blank;
148800101012               TBLflg = '*';
148900101012               clear  TBLftr;
149000101012               clear  TBLdtr;
149100101012               //____________
149200101012               update  TABEL;
149300101012               //¯¯¯¯¯¯¯¯¯¯¯¯
149400101012             endif;
149500101012
149600101012         ENDSL;
149700101012
149800101012       ENDSR;
149900101012
150000101012       //--------------------------------------------------------------
150100101012       //?Riempimento struttura dati ds3K.                             ?
150200101012       //--------------------------------------------------------------
150300101012       BEGSR  sr_RieDS3K;
150400101012
150500101115         if  %found(TABEL00F);
150600101115           ds3K_Memo = TBLuni;
150700101115         else;
150800101115           clear  ds3K_Memo;
150900101115         endif;
151000101115
151100101012         clear  ds3K;
151200101012
151300101012         // -?Impostazione altri campi (per tutti)?
151400101012         §3Kcgc = VI2cgc;
151500101012         §3Kcq1 = VI2cq1;
151600101012         §3Kcq2 = VI2cq2;
151700101012         §3Kcq3 = VI2cq3;
151800101012         §3Kcq4 = VI2cq4;
151900101012         §3Kcq5 = VI2cq5;
152000101012         §3Knmc = VI2nmc;
152100101012         §3Kcsd = VI2csd;
152200101117         if  ds3K_Memo.§3Kcdu > *zero;
152300101115           §3Kcdu = ds3K_Memo.§3Kcdu;
152400101115         endif;
152500101012         §3Kcks = VI2cks;
152600101012         §3Kggc = VI2ggc;
152700101012         §3Kgqi = VI2gqi;
152800101012         §3Kgsd = VI2gsd;
152900101012         §3Kcma = VI2cma;
153000101012         §3Kcba = VI2cba;
153100101012         §3Kapp = VI2app;
153200101012         §3Kcq8 = VI2cq8;
153300101012         §3Kca  = VI2ca ;
153400101012         §3Kvto = VI2vto;
153500101012         §3Kgks = VI2gks;
153600101012         §3Kgsr = VI2gsr;
153700101012         §3Kctb = VI2ctb;
153800101012         §3Kcq6 = VI2cq6;
153900101012         §3Kcq7 = VI2cq7;
154000101012
154100101012       ENDSR;
154200101012
154300101012       //--------------------------------------------------------------
154400101012       //?Esecuzione del comando (già impostato).                      ?
154500101012       //--------------------------------------------------------------
154600101012       BEGSR  sr_ExecCmd;
154700101012
154800101012         clear Qcap0100;
154900101012         Qcabcsdh = *off;
155000101012         Qcapa    = *off;
155100101012         Qcacmdss = *off;
155200101012         Qcaerved = *allX'00';
155300101012
155400101012         clear Qusec;
155500101012         Qusbprv  = %size(Qusec);
155600101012
155700101012         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
155800101012                           %size(Qcap0100) : 'CPOP0100' : *omit :
155900101012                           0 : 0 : Qusec);
156000101012
156100101012         //if  Qusei <> *blank;
156200101012         //  ...;
156300101012         //endif;
156400101012
156500101012       ENDSR;
156600101012
156700101012       //--------------------------------------------------------------
156800101012       //?Operazioni finali.                                           ?
156900101012       //--------------------------------------------------------------
157000101012       BEGSR  sr_RoutEnd;
157100101012
157200101012         cloTabella();
157300101012
157400101012         clear  TIBS69ds;
157500101012         I69tla = 'C';
157600101012         tibs69r( tibs69ds :
157700101012                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
157800101012
157900101012         return;
158000101012
158100101012       ENDSR;
158200101011
158300101011      /end-free
158400101011
158500101011       //--------------------------------------------------------------
158600101011       //?Schiere a tempo di compilazione.                             ?
158700101011       //--------------------------------------------------------------
158800101011
158900101012** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con TABEL00F?
159000101115SETRAS  GAITRAGRU FILTRAGRU
159100101115AS888   GAITRAGRPSFILTRAGRPF
159200101011** - $Msg:?Messaggi di Errore?-----------------------------------------------*
159300101011Filiale inesistente                                                            1
159400101011Linea inesistente                                                              2
159500101011Codice cliente inesistente                                                     3
159600101011Codice cliente NON selezionabile                                               4
159700101011Specificare dati da inviare                                                    5
159800101011Immettere DATI CONSEGNA o DATI GIACENZA o DISPOSIZIONI DA CLIENTE o C.A.       6
159900101011Inserire solo se indicato almeno un ritorno VAC                                7
160000101011Campo obbligatorio                                                             8
160100101011Tipo trasmissione inesistente                                                  9
160200101011Tipo trasmissione annullata                                                   10
160300101011File inesistente                                                              11
160400111020?LIBERO?                                                                      12
160500101011Codice cliente annullato                                                      13
160600101011Specificare tipo giacenze da scaricare                                        14
160700101011Valorizzare abbinato con invio aperture o entrambe                            15
160800101011Dato valido solo per DATI CONSEGNA o DATI GIACENZA o DATI C.A.                16
