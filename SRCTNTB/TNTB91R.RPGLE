000100120127       //==============================================================
000200120224       //?Gestione tabella "3CL" (Serie -totali- assegnate a clienti in?
000300120224       //?                        Logistica)                           ?
000400120127       //==============================================================
000500120127
000600120127       //--------------------------------------------------------------
000700120127       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800120127       //--------------------------------------------------------------
000900120127
001000120127     /*PRM  dbgview(*source)
001100120127     /*END
001200120127
001300120127       //--------------------------------------------------------------
001400120127       //?Specifiche di controllo.                                     ?
001500120127       //--------------------------------------------------------------
001600120127
001700120127     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120127     h dftactgrp(*no)
001900120127     h bnddir('UBBNDDIR':'TRUL')
002000120127
002100120127       //--------------------------------------------------------------
002200120127       //?Dichiarazione file.                                          ?
002300120127       //--------------------------------------------------------------
002400120127
002500120127       // -?Organigramma?
002600120127     fAZORG01L  if   e           k disk
002700120127
002800120127       // -?Tabella "3CL"?
002900120127     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
003000120127     f                                     usropn
003100120127
003200120127       // -?Video?
003300120127     fTNTB91D   cf   e             workstn
003400120127     f                                     indds(IndDspF)
003500120127     f                                     infds(InfDspF)
003600120127
003700120127       //--------------------------------------------------------------
003800120127       //?Definizione costanti.                                        ?
003900120127       //--------------------------------------------------------------
004000120127
004100120127       // -?Codice tabella in gestione?
004200120127     d c_Tab           c                   const('3CL')
004300120127
004400120127       // -?Codice utente x TABEL00F?
004500120127     d c_Kut           c                   const(1)
004600120127
004700120127       // -?Tasti funzionali a video?
004800120127     d c_F01           c                   const(x'31')
004900120127     d c_F02           c                   const(x'32')
005000120127     d c_F03           c                   const(x'33')
005100120127     d c_F04           c                   const(x'34')
005200120127     d c_F05           c                   const(x'35')
005300120127     d c_F06           c                   const(x'36')
005400120127     d c_F07           c                   const(x'37')
005500120127     d c_F08           c                   const(x'38')
005600120127     d c_F09           c                   const(x'39')
005700120127     d c_F10           c                   const(x'3A')
005800120127     d c_F11           c                   const(x'3B')
005900120127     d c_F12           c                   const(x'3C')
006000120127     d c_F13           c                   const(x'B1')
006100120127     d c_F14           c                   const(x'B2')
006200120127     d c_F15           c                   const(x'B3')
006300120127     d c_F16           c                   const(x'B4')
006400120127     d c_F17           c                   const(x'B5')
006500120127     d c_F18           c                   const(x'B6')
006600120127     d c_F19           c                   const(x'B7')
006700120127     d c_F20           c                   const(x'B8')
006800120127     d c_F21           c                   const(x'B9')
006900120127     d c_F22           c                   const(x'BA')
007000120127     d c_F23           c                   const(x'BB')
007100120127     d c_F24           c                   const(x'BC')
007200120127     d c_Enter         c                   const(x'F1')
007300120127     d c_RollDown      c                   const(x'F4')
007400120127     d c_RollUp        c                   const(x'F5')
007500120127
007600120127       // -?Costanti per la definizione delle schiere con i nomi?
007700120127       //  ?degli iSeries da elaborare e delle relative librerie?
007800120127     d c_NrSyst        c                   const(2)
007900120127     d c_NrLibr        c                   const(2)
008000120127
008100120127       // -?Costante per controllo "caratteri solo numerici"?
008200120127     d c_Digits        c                   const('0123456789')
008300120130
008400120130       // -?Costante per controllo "caratteri solo numerici"?
008500120130     d c_NTWlogistica  c                   const('LOG')
008600120127
008700120127       //--------------------------------------------------------------
008800120127       //?Definizione schiere.                                         ?
008900120127       //--------------------------------------------------------------
009000120127
009100120127       // -?iSeries  &  Librerie con entrambi i file tabelle?
009200120127     d $iSystem        s                   like(currSysNetA)
009300120127     d                                     dim(c_NrSyst)
009400120127     d                                     ctdata   perrcd( 1)
009500120127     d $Libraries      s                   like(ds_Libr)
009600120127     d                                     dim(c_NrSyst)
009700120127     d                                     alt($iSystem)
009800120127
009900120127       // -?Messaggi di errore?
010000120220     d $Msg            s             78    dim(12)  ctdata  perrcd( 1)
010100120127
010200120127       //--------------------------------------------------------------
010300120127       //?Definizione aree dati.                                       ?
010400120127       //--------------------------------------------------------------
010500120127
010600120127       // -?Dati utente?
010700120127     d §AzUte        e ds                  extname(AZUTE00F)
010800120127     d                                     dtaara
010900120127     d §DatiUte      e ds                  extname(dDatiUte)
011000120127     d                                     dtaara
011100120127
011200120127       //--------------------------------------------------------------
011300120127       //?Definizione strutture dati.                                  ?
011400120127       //--------------------------------------------------------------
011500120127
011600120127       // -?Status ds?
011700120127     d Status         sds
011800120127     d  SDSpgm           *proc
011900120127
012000120127       // -?InfDS?
012100120127     d InfDspF         ds
012200120127     d   dsp_aid             369    369a
012300120127
012400120127       // -?Indicatori su DspF?
012500120127     d IndDspF         ds                  inz
012600120127         // -?Abilitazione tasti funzionali?
012700120127     d   F3Attivo                      n   overlay(IndDspF : 03)
012800120127     d   F5Attivo                      n   overlay(IndDspF : 05)
012900120127     d   F6Attivo                      n   overlay(IndDspF : 06)
013000120217     d   F8Attivo                      n   overlay(IndDspF : 08)
013100120206     d   F10Attivo                     n   overlay(IndDspF : 10)
013200120127     d   F12Attivo                     n   overlay(IndDspF : 12)
013300120127     d   F16Attivo                     n   overlay(IndDspF : 16)
013400120127         // -?Emissione messaggio di errore?
013500120127     d   ErrMessage                    n   overlay(IndDspF : 28)
013600120127         // -?Posizionamento cursore & segnalazione errore?
013700120127     d   PosCurFLS                     n   overlay(IndDspF : 51)
013800120127     d   PosCurNRS                     n   overlay(IndDspF : 52)
013900120127     d   PosCurFLL                     n   overlay(IndDspF : 53)
014000120127     d   PosCurDES                     n   overlay(IndDspF : 54)
014100120127         //   -?Riemissione videata?
014200120127     d   ErrGenerico                   n   overlay(IndDspF : 99)
014300120127
014400120127       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
014500120127     d ds_Libr         ds                  inz
014600120127     d  $Libr                        10    dim(c_NrLibr) inz
014700120127
014800120127       // -?Parametri ricevuti?
014900120127     d KPJBA         e ds
015000120127
015100120127       // -?Parametri per Reperimento dati utente?
015200120127     d TIBS34ds      e ds
015300120127
015400120127       // -?Dati record principale della tabella?
015500120127     d TNTBEds       e ds                  inz  extname(TNTBE00F)
015600120127     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
015700120127     d                                          prefix(TBX:3)
015800120127
015900120127       // -?Tabelle usate:?
016000120127       // ·?"3CL" = Serie (totali) assegnate a gruppi di unificanti?
016100120127     d d3CL          e ds                  inz  qualified
016200120206       // ·?"3C"  = Clienti che inviano bolle partenza?
016300120206     d ds3C          e ds                  inz  qualified
016400120130
016500120130       // -?143ª descrizione dell'organigramma?
016600120131     d Og143_a       e ds                  inz  qualified
016700120131     d                                     extname(Og143)
016800120131     d Og143_b       e ds                  inz  qualified
016900120131     d                                     extname(Og143)
017000120127
017100120127       //--------------------------------------------------------------
017200120127       //?Definizione variabili globali.                               ?
017300120127       //--------------------------------------------------------------
017400120127
017500120127       // -?Flags booleani?
017600120127     d $Fine           s               n   inz
017700120127     d $InzD01         s               n   inz(*on)
017800120127     d $InzD02         s               n   inz(*on)
017900120127     d $InzW01         s               n   inz(*on)
018000120127     d $Protect        s               n   inz
018100120220     d $NrsNotUsed     s               n   inz
018200120206     d $ExistNrsParz   s               n   inz
018300120127     d $Tibs02         s               n   inz
018400120127
018500120127       // -?Variabili per la gestione del video?
018600120127     d $Video          s              2    inz('D1')
018700120127
018800120127       // -?Nome del sistema?
018900120127     d currSysNeta     s              8a   inz
019000120127
019100120127       // -?Nome esteso Libreria/File dei 2 file tabella?
019200120127     d wLibFile        s             21a   inz
019300120127
019400120127       // -?Campi di comodo?
019500120127     d w_iSystem       s              1  0 inz
019600120127     d w_SisInf        s              3  0 inz
019700120127     d wDate           s              8  0 inz
019800120127     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019900120127
020000120127       //--------------------------------------------------------------
020100120127       //?Definizione prototipi procedure.                             ?
020200120127       //--------------------------------------------------------------
020300120127
020400120127       // -?Reperimento NETA sistema AS/400 corrente?
020500120127      /copy gaitrasrc/srcProtoPR,UBRTVNETA
020600120127
020700120127       // -?Reperimento dati utente?
020800120127      /copy gaitrasrc/srcProtoPR,TIBS34R
020900120127
021000120127       // -?Ricerca/Controllo tabelle (TNTBE)?
021100120127     d TIBS02ds      e ds                  inz
021200120127     d   T02mod      e                     inz('R')
021300120127     d   T02cod      e                     inz(c_Tab)
021400120127      /copy gaitrasrc/srcProtoPr,TIBS02R
021500120127
021600120127       // -?Ricerca filiale in organigramma?
021700120127      /copy gaitrasrc/srcProtoPI,TNSD24R
021800120127      /copy gaitrasrc/srcProtoPR,TNSD24R
021900120127
022000120127       // -?Attribuzione/Verifica numero serie libero per filiale?
022100120206     d ubLeg3C_SKC     s           3200    based(nullPtr)
022200120206      /copy gaitrasrc/srcProtoPI,TRTB28R2
022300120206      /copy gaitrasrc/srcProtoPR,TRTB28R2
022400120127
022500120127       // -?Attribuzione/Verifica numero serie parziale libero per fil.?
022600120206     d TNTB54ds      e ds                  inz
022700120206     d   I54tel      e                     inz('C')
022800120206     d   I54dal      e                     inz(1)
022900120206     d   I54al       e                     inz(*all'9')
023000120206      /copy gaitrasrc/srcProtoPR,TNTB54R
023100120206
023200120206       // -?Interrogazione tabella "3C" (oltre 9.999 record)?
023300120206     d trtb28r0        pr                  extpgm('TRTB28R0')
023400120206     d   kpjba                             likeds(KPJBA)
023500120217
023600120217       // -?Reperimento cliente da numero serie segnacollo?
023700120217     d fnlv03r         pr                  extpgm('FNLV03R')
023800120217     d   kpjba                             likeds(KPJBA)
023900120127
024000120127       //--------------------------------------------------------------
024100120127       //?Definizione key-list.                                        ?
024200120127       //--------------------------------------------------------------
024300120127
024400120127       // -?File TNTBE01L?
024500120127     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
024600120127     d                                     prefix(k_)   inz
024700120127
024800120127       //--------------------------------------------------------------
024900120127       //?M A I N - L I N E                                            ?
025000120127       //--------------------------------------------------------------
025100120127
025200120127     c     *Entry        plist
025300120127     c                   parm                    KPJBA
025400120127
025500120127      /free
025600120127
025700120127       // -?Operazioni iniziali?
025800120127       exsr  sr_RoutInz;
025900120127
026000120127       // -?Ciclo di gestione del file video?
026100120127       DOW  $Fine = *off;
026200120127
026300120127         select;
026400120127
026500120127           // -?Richiesta chiave di accesso?
026600120127           when $Video = 'D1';
026700120127             exsr  sr_GesD01;
026800120127
026900120127           // -?Manutenzione dati della singola tabella?
027000120127           when $Video = 'D2';
027100120127             exsr  sr_GesD02;
027200120127
027300120127           // -?Richiesta trasmissione dati?
027400120130           //when $Video = 'W1';
027500120130           //  exsr  sr_GesW01;
027600120127
027700120127           // -?? ? ??
027800120127           other;
027900120127             $Fine = *on;
028000120127
028100120127         endsl;
028200120127
028300120127       ENDDO;
028400120127
028500120127       // -?Operazioni finali?
028600120127       exsr  sr_RoutEnd;
028700120127
028800120127       //--------------------------------------------------------------
028900120127       //?Operazioni iniziali.                                         ?
029000120127       //--------------------------------------------------------------
029100120127       BEGSR  *InzSr;
029200120127
029300120127         // -?Reperimento dati job?
029400120127         exsr  sr_DatiJob;
029500120127
029600120127         // -?Impostazione nome programma a video?
029700120127         V1Tpgm = SDSpgm;
029800120127
029900120127         // -?Reperimento data odierna?
030000120127         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030100120127                               : 1 : 8 ) );
030200120127
030300120127       ENDSR;
030400120127
030500120127       //--------------------------------------------------------------
030600120127       //?Operazioni iniziali.                                         ?
030700120127       //--------------------------------------------------------------
030800120127       BEGSR  sr_RoutInz;
030900120127
031000120127         // -?Impostazione chiusura?
031100120127         *inLR = *on;
031200120127
031300120127         // -?Verifica del sistema AS/400 corrente?
031400120127         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
031500120127           $Fine = *on;
031600120127           leavesr;
031700120127         endif;
031800120127
031900120127         // -?Impostazione elenco librerie in cui gestire le tabelle?
032000120127         //  ?(a seconda del sistema in cui si stà lavorando)?
032100120127         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
032200120127         if  w_iSystem = *zero;
032300120127           $Fine = *on;
032400120127           leavesr;
032500120127         endif;
032600120127
032700120127         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
032800120127         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
032900120127           w_SisInf = 1;
033000120127           exsr  sr_OpenFileTab;
033100120127         endif;
033200120127
033300120127         // -?Aggancio dati generali della tabella in esame?
033400120127         clear  k_TBEcod;
033500120127         k_TBEke1 = *zero;
033600120127         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
033700120127                = c_Tab;
033800120127         clear  k_TBEke2;
033900120127         clear  k_TBElin;
034000120127         k_TBEsif = KNSIF;
034100120127         chain(n)  %kds(k05tntbe01)  TNTBE000;
034200120127         if  not  %found(TNTBE01L);
034300120127           clear  k_TBEsif;
034400120127           chain(n)  %kds(k05tntbe01)  TNTBE000;
034500120127         endif;
034600120127         if  %found(TNTBE01L);
034700120127           xTNTBEds = TNTBEds;
034800120127         else;
034900120127           clear  xTNTBEds;
035000120127         endif;
035100120127
035200120127         // -?Impostazione iniziale / pulizia dei campi chiave?
035300120130         clear  k05tntbe01;
035400120127         k_TBEcod = c_Tab;
035500120127
035600120127       ENDSR;
035700120127
035800120127       //--------------------------------------------------------------
035900120127       //?Reperimento Dati del job (Utente/Operativi).                 ?
036000120127       //--------------------------------------------------------------
036100120127       BEGSR  sr_DatiJob;
036200120127
036300120127         in(E) §AzUte;
036400120127         if NOT %error;
036500120127           in(E) §DatiUte;
036600120127         endif;
036700120127         if %error or RSut = *blanks;
036800120127           clear TIBS34ds;
036900120127           tibs34r ( tibs34ds );
037000120127           in §AzUte;
037100120127           in §DatiUte;
037200120127         endif;
037300120127
037400120127       ENDSR;
037500120127
037600120127       //--------------------------------------------------------------
037700120127       //?Gestione videata D01.                                        ?
037800120127       //--------------------------------------------------------------
037900120127       BEGSR  sr_GesD01;
038000120127
038100120127         // -?Inizializzazione videata?
038200120127         if  $InzD01 = *on;
038300120127           exsr  sr_InzD01;
038400120127           $InzD01 = *off;
038500120127         endif;
038600120127         clear  V1Topz;
038700120127
038800120127         // -?(Dis)Abilitazione tasti funzionali?
038900120127         F3Attivo  = *on;
039000120127         F5Attivo  = *off;
039100120127         F6Attivo  = *off;
039200120220         F8Attivo  = *off;
039300120206         F10Attivo = *on;
039400120127         F12Attivo = *off;
039500120127         F16Attivo = *off;
039600120127
039700120127         // -?Emissione videata?
039800120127         write  TB91T01;
039900120127         write  TB91P01;
040000120127         exfmt  TB91D01;
040100120127
040200120127         clear  V1Dmsg;
040300120127         reset  ErrMessage;
040400120127         reset  ErrGenerico;
040500120127
040600120127         SELECT;
040700120127
040800120127           // -?F3=Fine?
040900120127           WHEN  dsp_aid = c_F03;
041000120127             $Fine = *on;
041100120206
041200120206           // -?F10=Ricerca Serie (globale)?
041300120206           WHEN  dsp_aid = c_F10;
041400120206             exsr  sr_F10D01;
041500120127
041600120127           // -?Invio?
041700120127           OTHER;
041800120127             exsr  sr_CtrD01;
041900120127             if  ErrGenerico = *off;
042000120127               $Video    = 'D2';
042100120127               $InzD02   = *on;
042200120127             endif;
042300120127
042400120127         ENDSL;
042500120127
042600120127       ENDSR;
042700120127
042800120127       //--------------------------------------------------------------
042900120127       //?Inizializzazione videata D01.                                ?
043000120127       //--------------------------------------------------------------
043100120127       BEGSR  sr_InzD01;
043200120127
043300120127         clear  TB91D01;
043400120127
043500120127         V1Cfls = '?  ';
043600120130
043700120130         // -?Impostazione indicatori per abilitazione tasti funzionali?
043800120130         clear  TB91P01;
043900120130         F3Attivo = *on;
044000120127
044100120127       ENDSR;
044200120206
044300120206       //--------------------------------------------------------------
044400120206       //?Gestione tasto funzionale F10=Ricerca Serie (in D01).        ?
044500120206       //--------------------------------------------------------------
044600120206       BEGSR  sr_F10D01;
044700120206
044800120206         // -?Controlli codice filiale segnacolli?
044900120206         exsr  sr_CtrlFLS;
045000120206         if  ErrGenerico;
045100120206           leavesr;
045200120206         endif;
045300120206
045400120206         Select;
045500120206           // -?Controllo immissione?
045600120206           When  V1Cfls = *blank  or  V1Cfls = *zero;
045700120206             ErrMessage  = *on;
045800120206             ErrGenerico = *on;
045900120206             PosCurFLS   = *on;
046000120206             V1Dmsg = $Msg(01);
046100120206             leavesr;
046200120206           // -?Controllo numericità?
046300120206           When  %check( c_Digits : V1Cfls ) > *zero;
046400120206             ErrMessage  = *on;
046500120206             ErrGenerico = *on;
046600120206             PosCurFLS   = *on;
046700120206             V1Dmsg = $Msg(02);
046800120206             leavesr;
046900120206         EndSl;
047000120206
047100120206         // -?Ricerca?
047200120206         clear  TRTB28ds2;
047300120206         ParFIL = %int(V1Cfls);
047400120206         ParELB = 'R';
047500120206
047600120206         trtb28r2 ( TRTB28ds2 );
047700120206
047800120206         Select;
047900120206
048000120206           // ->?Non reperibile una nuova serie?
048100120206           When  ParERR = 'E';
048200120206             V1Dmsg = $Msg(04);
048300120206             PosCurNRS   = *on;
048400120206             ErrMessage  = *on;
048500120206             ErrGenerico = *on;
048600120206             leavesr;
048700120206
048800120206           // ->?Serie NON ammessa (99)?
048900120206           When  ParERR = '9';
049000120206             V1Dmsg = $Msg(07);
049100120206             PosCurNRS   = *on;
049200120206             ErrMessage  = *on;
049300120206             ErrGenerico = *on;
049400120206             leavesr;
049500120206
049600120206           // ->?Serie valida?
049700120206           Other;
049800120206             V1Cnrs = %editc( ParNRS : 'X' );
049900120206
050000120206         EndSl;
050100120206
050200120206       ENDSR;
050300120127
050400120127       //--------------------------------------------------------------
050500120127       //?Controllo dati immessi nella videata D01.                    ?
050600120127       //--------------------------------------------------------------
050700120127       BEGSR  sr_CtrD01;
050800120127
050900120127         // -?Spegnimento indicatori di posizionamento cursore?
051000120127         %subst(IndDspF : 28) = *off;
051100120127
051200120127         reset  $Protect;
051300120127
051400120127         // -?Controlli codice filiale segnacolli?
051500120206         exsr  sr_CtrlFLS;
051600120206         if  ErrGenerico;
051700120206           leavesr;
051800120206         endif;
051900120127
052000120127         // -?Controllo numero serie segnacolli?
052100120127         Select;
052200120206           // -?Ricerca in tab. "3C"?
052300120206           When  %scan( '?' : V1Cnrs ) > *zero;
052400120206             clear  V1Cnrs;
052500120206             kpjbu = V1Cfls + 'FLS';
052600120206             trtb28r0 ( kpjba );
052700120206             if  %subst( kpjbu : 1 : 7 ) > *zero;
052800120206               ds3C   = %subst( kpjbu : 8 );
052900120206               V1Cfls = %editc( ds3C.§3Cfls : 'X' );
053000120206               V1Cnrs = %editc( ds3C.§3Cnrs : 'X' );
053100120206             endif;
053200120206             PosCurNRS   = *on;
053300120206             ErrGenerico = *on;
053400120206             leavesr;
053500120127           // -?Controllo immissione?
053600120206           When  V1Cnrs = *blank  or  V1Cnrs = *zero;
053700120206             V1Dmsg = $Msg(05);
053800120127             PosCurNRS   = *on;
053900120206             ErrMessage  = *on;
054000120206             ErrGenerico = *on;
054100120127             leavesr;
054200120206           // -?Controllo numericità?
054300120206           When  %check( c_Digits : V1Cnrs ) > *zero;
054400120206             V1Dmsg = $Msg(06);
054500120206             PosCurFLS   = *on;
054600120206             ErrMessage  = *on;
054700120206             ErrGenerico = *on;
054800120206             leavesr;
054900120206         EndSl;
055000120127
055100120127         // -?Apertura file TABEL00F del 1° S.I. (sede)?
055200120127         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
055300120127           w_SisInf = 1;
055400120127           exsr  sr_OpenFileTab;
055500120127         endif;
055600120206
055700120220         // -?Controllo serie (se già utilizzata parzialmente, ecc. ecc.)?
055800120220         If  I54fil <> V1Cfls  or
055900120220             I54nrs <> V1Cnrs;
056000120220           reset  $NrsNotUsed;
056100120206           reset  $ExistNrsParz;
056200120206           reset  TNTB54ds;
056300120206           I54fil = V1Cfls;
056400120206           I54nrs = V1Cnrs;
056500120206           exsr  sr_Ctrl_NRS_SCx;
056600120220           // ·?Serie libera?
056700120220           $NrsNotUsed = (O54err = *blank);
056800120220           // ·?Serie già assegnata come parziale?
056900120220           $ExistNrsParz = ( O54err = '1'  and
057000120220                            (O54dal >  1   or   O54al < *hival) );
057100120220           select;
057200120220             // ·?Verifica esistenza tabella "3CL":?
057300120220             //  ?SE inserimento => avvisa ora e avviserà dopo?
057400120220             //  ?SE modifica    => avviserà dopo?
057500120220             when  $ExistNrsParz  or  $NrsNotUsed;
057600120220               k_TBEke1 = V1Cfls;
057700120220               k_TBEke2 = V1Cnrs;
057800120220               setll  %kds(k05tntbe01)  TNTBE000;
057900120220               if  %equal(TNTBE01L);
058000120220                 clear  V1Dmsg;
058100120220                 clear  ErrMessage;
058200120220                 clear  ErrGenerico;
058300120220               endif;
058400120220               leavesr;
058500120220             // ·?Altro errore bloccante?
058600120220             when  O54err <> *blank;
058700120220               reset  TNTB54ds;
058800120220               leavesr;
058900120220           endsl;
059000120220         EndIf;
059100120127
059200120127       ENDSR;
059300120206
059400120206       //--------------------------------------------------------------
059500120206       //?Controllo codice Filiale Segnacolli.                         ?
059600120206       //--------------------------------------------------------------
059700120206       BEGSR  sr_CtrlFLS;
059800120206
059900120206         clear  V1Dfls;
060000120206
060100120206         Select;
060200120206           // -?Ricerca dati già inseriti?
060300120206           When  %scan( '?' : V1Cfls ) > *zero;
060400120206             clear  V1Cfls;
060500120206             $Tibs02 = *on;
060600120206             reset  TIBS02ds;
060700120206             //T02mod = 'R';    ?(già così)?
060800120206             //T02cod = c_Tab;  ?(già così)?
060900120206             T02sif = KNSIF;
061000120206             TNTBE_RicercaControllo ( kpjba : tibs02ds );
061100120206             if  T02err = *blank;
061200120206               V1Cfls = %trimr( T02ke1 );
061300120206               V1Cnrs = %trimr( T02ke2 );
061400120206             else;
061500120206               ErrMessage  = *on;
061600120206               V1Dmsg = T02msg;
061700120206             endif;
061800120206             ErrGenerico = *on;
061900120206             PosCurFLS   = *on;
062000120206             leavesr;
062100120206           // -?Controllo immissione?
062200120206           When  V1Cfls = *blank  or  V1Cfls = *zero;
062300120206             ErrGenerico = *on;
062400120206             ErrMessage  = *on;
062500120206             PosCurFLS   = *on;
062600120206             V1Dmsg = $Msg(01);
062700120206             leavesr;
062800120206           // -?Controllo numericità?
062900120206           When  %check( c_Digits : V1Cfls ) > *zero;
063000120206             ErrGenerico = *on;
063100120206             ErrMessage  = *on;
063200120206             PosCurFLS   = *on;
063300120206             V1Dmsg = $Msg(02);
063400120206             leavesr;
063500120206           // -?Controllo esistenza in anagrafica?
063600120206           //  ?& NO Logistica?
063700120206           Other;
063800120206             chain  ( %int(V1Cfls) )  AZORG;
063900120206             if  Not  %found(AZORG01L);
064000120206               ErrGenerico = *on;
064100120206               ErrMessage  = *on;
064200120206               PosCurFLS   = *on;
064300120206               V1Dmsg = $Msg(02);
064400120206               leavesr;
064500120206             endif;
064600120206             V1Dfls = ORGdes;
064700120206             // -?& NO Logistica?
064800120206             //  ?(se già inserito: meglio farlo annullare)?
064900120206             Og143_a = ORGde3;
065000120206             if  Og143_a.§OGntw = c_NTWlogistica;
065100120206               k_TBEke1 = V1Cfls;
065200120206               k_TBEke2 = V1Cnrs;
065300120206               setll  %kds(k05tntbe01)  TNTBE000;
065400120206               if  Not  %equal(TNTBE01L);
065500120206                 ErrGenerico = *on;
065600120206                 ErrMessage  = *on;
065700120206                 PosCurFLS   = *on;
065800120206                 V1Dmsg = $Msg(03);
065900120206                 leavesr;
066000120206               endif;
066100120206             endif;
066200120206         EndSl;
066300120206
066400120206       ENDSR;
066500120220
066600120220       //--------------------------------------------------------------
066700120220       //?Controllo range segnacolli per filiale e serie.              ?
066800120220       //--------------------------------------------------------------
066900120220       BEGSR  sr_Ctrl_NRS_SCx;
067000120220
067100120220         //I54tel = 'C';              ?(già così)?
067200120220         //I54dal = 1;                ?(già così)?
067300120220         //I54al  = *all'9';          ?(già così)?
067400120220         I54ksc = V1Cfls + '0000';
067500120220
067600120220         kpjbu  = TNTB54ds;
067700120220         tntb54r ( kpjba );
067800120220         TNTB54ds = kpjbu;
067900120220
068000120220         SELECT;
068100120220
068200120220           // ->?Serie già assegnata come completa (in tab. "3CL")?
068300120220           //   ?Qui si elaborano SOLO serie complete!?
068400120220           WHEN  O54err = 'A';
068500120220             leavesr;
068600120220
068700120220           // ->?Errore SQL?
068800120220           WHEN  O54err = 'S';
068900120220             V1Dmsg = $Msg(08);
069000120220             if  $Video  = 'D1';
069100120220               PosCurNRS = *on;
069200120220             else;
069300120220               PosCurFLL = *on;
069400120220             endif;
069500120220             ErrMessage  = *on;
069600120220             ErrGenerico = *on;
069700120220             leavesr;
069800120220
069900120220           // ->?Serie già presente come parziale?
070000120220           //   ?Errore forzabile?
070100120220           WHEN  O54err =  '1'   and
070200120220                 (O54dal >  1    or   O54al < *hival);
070300120220             V1Dmsg = %trimr($Msg(09)) + ' ' + O54ksc +
070400120220                      '. Premere "Invio".';
070500120220             if  $Video  = 'D1';
070600120220               PosCurNRS = *on;
070700120220             else;
070800120220               PosCurFLL = *on;
070900120220             endif;
071000120220             ErrMessage  = *on;
071100120220             ErrGenerico = *on;
071200120220             leavesr;
071300120220
071400120220           // ->?Serie NON ammessa (99)?
071500120220           WHEN  O54err = '9';
071600120220             V1Dmsg = $Msg(07);
071700120220             if  $Video  = 'D1';
071800120220               PosCurNRS = *on;
071900120220             else;
072000120220               PosCurFLL = *on;
072100120220             endif;
072200120220             ErrMessage  = *on;
072300120220             ErrGenerico = *on;
072400120220             leavesr;
072500120220
072600120220           // ->?Serie ancora libera?
072700120220           WHEN  O54err = *blank;
072800120220             V1Dmsg = $Msg(10);
072900120220             if  $Video  = 'D1';
073000120220               PosCurNRS = *on;
073100120220             else;
073200120220               PosCurFLL = *on;
073300120220             endif;
073400120220             ErrMessage  = *on;
073500120220             ErrGenerico = *on;
073600120220             leavesr;
073700120220
073800120220         ENDSL;
073900120220
074000120220       ENDSR;
074100120127
074200120127       //--------------------------------------------------------------
074300120127       //?Apertura dei files tabelle nel sistema informativo impostato.?
074400120127       //--------------------------------------------------------------
074500120127       BEGSR  sr_OpenFileTab;
074600120127
074700120127         // -?Chiusura (eventuale) file tabelle?
074800120130         if  %open(TNTBE01L);
074900120130           close  TNTBE01L;
075000120127         endif;
075100120127
075200120127         // -?Apertura file tabelle?
075300120127         ds_Libr  = $Libraries(w_iSystem);
075400120127         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
075500120127         open  TNTBE01L;
075600120127
075700120127       ENDSR;
075800120127
075900120127       //--------------------------------------------------------------
076000120127       //?Gestione videata D02.                                        ?
076100120127       //--------------------------------------------------------------
076200120127       BEGSR  sr_GesD02;
076300120127
076400120127         // -?Inizializzazione videata?
076500120127         if  $InzD02 = *on;
076600120127           exsr  sr_InzD02;
076700120127           $InzD02 = *off;
076800120127         endif;
076900120127
077000120127         // -?Emissione Testata e Piede con tasti funzionali abilitati?
077100120220         if  Not ErrMessage;
077200120220           write  TB91T01;
077300120220           write  TB91D01;
077400120220           write  TB91P01;
077500120220           write  Protect;
077600120220         endif;
077700120127
077800120127         // -?Emissione videata?
077900120127         if  Not $Protect;
078000120127           exfmt  TB91D02;
078100120127         else;
078200120127           write  TB91D02;
078300120127           exfmt  Protect;
078400120127         endif;
078500120127
078600120127         clear  V1Dmsg;
078700120127         reset  ErrMessage;
078800120127         reset  ErrGenerico;
078900120127
079000120127         SELECT;
079100120127
079200120127           // -?F3=Fine?
079300120127           WHEN  dsp_aid = c_F03;
079400120127             unlock TNTBE01L;
079500120127             $Fine = *on;
079600120217
079700120217           // -?F8=Reperimento cliente da numero serie segnacollo?
079800120217           WHEN  dsp_aid = c_F08;
079900120217             exsr  sr_CallFNLV03;
080000120127
080100120127           // -?F12=Ritorno?
080200120127           WHEN  dsp_aid = c_F12;
080300120127             unlock TNTBE01L;
080400120127             reset  $Video;
080500120220             reset  TNTB54ds;
080600120127
080700120127           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
080800120127           OTHER;
080900120127             // -?Controlli eseguiti NON se F16=Annullamento?
081000120127             if  dsp_aid <> c_F16;
081100120127               exsr  sr_CtrD02;
081200120127             // -?Controlli eseguiti SOLO se F16=Annullamento?
081300120127             //else;
081400120127             //  exsr  sr_CtrANN;
081500120127             endif;
081600120127             if  ErrGenerico;
081700120127               leavesr;
081800120127             endif;
081900120127             // -?Aggiornamento?
082000120127             if  dsp_aid = c_F05   or
082100120127                 dsp_aid = c_F06   or
082200120127                 dsp_aid = c_F16;
082300120127               exsr  sr_UpdateAll;
082400120130               // -?Rilevato errore in fase di aggiornamento?
082500120130               if  ErrGenerico = *on;
082600120130                 leavesr;
082700120130               // -?Videata iniziale altrimenti?
082800120130               else;
082900120130                 $Video  = 'D1';
083000120130                 $InzD01 = *on;
083100120130               endif;
083200120127             endif;
083300120127
083400120127         ENDSL;
083500120127
083600120127       ENDSR;
083700120127
083800120127       //--------------------------------------------------------------
083900120127       //?Inizializzazione videata D02.                                ?
084000120127       //--------------------------------------------------------------
084100120127       BEGSR  sr_InzD02;
084200120127
084300120127         reset  $Protect;
084400120127
084500120127         // -?Spegnimento degli indicatori di errore?
084600120127         %subst(IndDspF : 50) = *off;
084700120127
084800120127         // -?Pulizia videata?
084900120127         clear  TB91D02;
085000120127
085100120130         // -?Reperimento dati tabella "3CL"?
085200120130         k_TBEke1 = V1Cfls;
085300120206         k_TBEke2 = V1Cnrs;
085400120130         chain  %kds(k05tntbe01)  TNTBE000;
085500120127
085600120130         // -?Caricamento dati tabella "3CL"?
085700120127         if  %found(TNTBE01L);
085800120127           exsr  sr_RieD02;
085900120127         endif;
086000120127
086100120127         // -?Impostazione testata?
086200120130         //clear  V1Topz;             ?(già fatto in sr_GesD01)?
086300120127         select;
086400120127           when  Not %found(TNTBE01L);
086500120127             V1Topz = '  INSERIMENTO  ';
086600120131           when  TBEatb <> *blank  and  Og143_a.§OGntw <> c_NTWlogistica;
086700120127             V1Topz = '  RIPRISTINO   ';
086800120131           when  TBEatb =  *blank  and  Og143_a.§OGntw =  c_NTWlogistica;
086900120127             V1Topz = ' ANNULLAMENTO  ';
087000120206             $Protect = *on;
087100120131           when  TBEatb <> *blank  and  Og143_a.§OGntw =  c_NTWlogistica;
087200120130             V1Topz = ' GIÀ ANNULLATO ';
087300120130             $Protect = *on;
087400120127           other;
087500120127             V1Topz = '   MODIFICA    ';
087600120127         endsl;
087700120206
087800120220         // -?Impostazione messaggi a video?
087900120220         select;
088000120220           when  $ExistNrsParz;
088100120220             V1Dtxt1 = %trimr($Msg(09)) + ' ' + O54ksc;
088200120220           when  $NrsNotUsed;
088300120220             V1Dtxt1 = $Msg(10);
088400120220         endsl;
088500120127
088600120127         // -?Impostazione indicatori per abilitazione tasti funzionali?
088700120127         exsr  sr_AbilitFxxD02;
088800120127
088900120127       ENDSR;
089000120127
089100120127       //--------------------------------------------------------------
089200120130       //?Caricamento dati del singolo record della tab. "3CL"         ?
089300120127       //?nella videata D02.                                           ?
089400120127       //--------------------------------------------------------------
089500120127       BEGSR  sr_RieD02;
089600120127
089700120130         d3CL = TBEuni;
089800120127
089900120130         // -?Dati della tab. "3CL"?
090000120130         V1Cfll = %editc( d3CL.§3CLfil : 'X' );
090100120130         V1Cdes = d3CL.§3CLdes;
090200120127
090300120127         // -?Decodifiche?
090400120127         exsr  sr_CtrD02;
090500120127         clear  V1Dmsg;
090600120130         clear  ErrMessage;
090700120130         clear  ErrGenerico;
090800120127         %subst(IndDspF : 50) = *off;
090900120127
091000120127       ENDSR;
091100120127
091200120127       //--------------------------------------------------------------
091300120127       //?Settaggio indicatori nella videata D02 per abilitazione      ?
091400120127       //?  tasti funzionali.                                          ?
091500120127       //--------------------------------------------------------------
091600120127       BEGSR  sr_AbilitFxxD02;
091700120127
091800120130         // -?F3=Fine?
091900120130         //F3Attivo  = (%parms() = 1);
092000120130         F3Attivo = *on;
092100120127
092200120127         // -?F5=Ripristino?
092300120127         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
092400120127                                         and   Not $Protect);
092500120127
092600120127         // -?F6=Conferma?
092700120206         F6Attivo  = (NOT F5Attivo   and   Not $Protect);
092800120217
092900120217         // -?F8=Reperimento clienti da numero serie segnacollo?
093000120217         F8Attivo  = *on;
093100120206
093200120206         // -?F10=Ricerca Serie Totale?
093300120206         F10Attivo = *off;
093400120127
093500120130         // -?F12=Ritorno?
093600120130         //F12Attivo = (%parms() = 1);
093700120130         F12Attivo = *on;
093800120127
093900120130         // -?F16=Annullamento?
094000120130         F16Attivo = ( %found(TNTBE01L)   and   TBEatb = *blank );
094100120127
094200120127       ENDSR;
094300120217
094400120217       //--------------------------------------------------------------
094500120217       //?Gestione tasto funzionale                                    ?
094600120217       //?F8=Reperimento cliente da numero serie segnacollo (in D02).  ?
094700120217       //--------------------------------------------------------------
094800120217       BEGSR  sr_CallFNLV03;
094900120217
095000120217         kpjbu = V1Cfls + V1Cnrs;
095100120217
095200120217         fnlv03r ( kpjba );
095300120217
095400120217       ENDSR;
095500120127
095600120127       //--------------------------------------------------------------
095700120127       //?Controllo dati videata D02.                                  ?
095800120127       //--------------------------------------------------------------
095900120127       BEGSR  sr_CtrD02;
096000120127
096100120127         // -?Spegnimento degli indicatori di errore?
096200120127         %subst(IndDspF : 50) = *off;
096300120130
096400120130         // -?Controlli codice filiale Logistica?
096500120130         clear  V1Dfll;
096600120130         Select;
096700120130           // -?Ricerca filiale?
096800120130           When  %scan( '?' : V1Cfll ) > *zero;
096900120130             clear  V1Cfll;
097000120213             clear  p_FIL;
097100120213             clear  p_FAG;
097200120206             //p_FAG  = 'V';
097300120206             p_DES  = '-LOG ';
097400120213             clear  p_DIT;
097500120130             tnsd24r ( p_FIL : p_FAG : p_DES : p_DIT );
097600120130             V1Cfll = p_FIL;
097700120130             V1Dfll = p_DES;
097800120224             V1Cdes = V1Dfll;
097900120131             leavesr;
098000120130           // -?Controllo immissione?
098100120130           When  V1Cfll = *blank  or  V1Cfll = *zero;
098200120131             ErrGenerico = *on;
098300120131             ErrMessage  = *on;
098400120131             PosCurFLL   = *on;
098500120131             V1Dmsg = $Msg(01);
098600120131             leavesr;
098700120130           // -?Controllo numericità?
098800120130           When  %check( c_Digits : V1Cfll ) > *zero;
098900120130             ErrGenerico = *on;
099000120130             ErrMessage  = *on;
099100120130             PosCurFLL   = *on;
099200120130             V1Dmsg = $Msg(02);
099300120130             leavesr;
099400120130           // -?Controllo esistenza in anagrafica?
099500120130           //  ?& NO Logistica?
099600120130           Other;
099700120130             chain  ( %int(V1Cfll) )  AZORG;
099800120130             if  Not  %found(AZORG01L);
099900120130               ErrGenerico = *on;
100000120130               ErrMessage  = *on;
100100120130               PosCurFLL   = *on;
100200120130               V1Dmsg = $Msg(02);
100300120130               leavesr;
100400120130             endif;
100500120130             V1Dfll = ORGdes;
100600120130             // -?& NO Logistica?
100700120130             //  ?(se già inserito: meglio farlo annullare)?
100800120131             Og143_b = ORGde3;
100900120131             if  Og143_b.§OGntw <> c_NTWlogistica;
101000120130               ErrGenerico = *on;
101100120130               ErrMessage  = *on;
101200120130               PosCurFLL   = *on;
101300120220               V1Dmsg = $Msg(11);
101400120130               leavesr;
101500120130             endif;
101600120130         EndSl;
101700120127
101800120130         // -?Descrizione obblogatoria?
101900120130         if  V1Cdes = *blank;
102000120130           V1Cdes = V1Dfll;
102100120130         endif;
102200120206         if  V1Cdes = *blank;
102300120206           ErrGenerico = *on;
102400120206           ErrMessage  = *on;
102500120206           PosCurDES   = *on;
102600120220           V1Dmsg = $Msg(12);
102700120206           leavesr;
102800120206         endif;
102900120127
103000120127       ENDSR;
103100120127
103200120127       //--------------------------------------------------------------
103300120127       //?Controllo annullabilità del record nella videata D02.        ?
103400120127       //--------------------------------------------------------------
103500120206       //BEGSR  sr_CtrANN;
103600120206       //
103700120206       //  %subst(IndDspF : 50) = *off;
103800120206       //
103900120206       //ENDSR;
104000120127
104100120127       //--------------------------------------------------------------
104200120130       //?Aggiornam. tab. "3CL" nei file di tutti i sistemi informativi?
104300120127       //--------------------------------------------------------------
104400120127       BEGSR  sr_UpdateAll;
104500120127
104600120127         // -?Ciclo di elaborazione per ogni sistema informativo?
104700120127         For  w_SisInf = 1  To  %elem($Libr);
104800120127
104900120127           // -?Apertura degli archivi?
105000120127           if  w_SisInf > 1;
105100120127             exsr  sr_OpenFileTab;
105200120127           endif;
105300120127
105400120130           // -?Aggiornamento tab. "3CL"?
105500120127           exsr  sr_UpdTNTBE;
105600120127
105700120127         EndFor;
105800120127
105900120127       ENDSR;
106000120127
106100120127       //--------------------------------------------------------------
106200120130       //?Aggiornamento records TNTBE00F (tab. "3CL").                 ?
106300120127       //--------------------------------------------------------------
106400120127       BEGSR  sr_UpdTNTBE;
106500120127
106600120127         //? N.B.                                                      ?
106700120127         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
106800120127         //   trasmissione (vedi flag TBEFTR).                        ?
106900120127         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
107000120127         //   subito nello stesso file di entrambi i S.I. - in due    ?
107100120127         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
107200120127
107300120127         // -?RI-aggancio record da aggiornare?
107400120127         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
107500120130         if  w_SisInf > 1;
107600120130           //clear  k05tntbe01;                   ?(già impostata)?
107700120130           //k_TBEcod = c_Tab;                    ?(già così)?
107800120130           //k_TBEke1 = V1Cfls;                   ?(già così)?
107900120206           //k_TBEke2 = V1Cnrs;                   ?(già così)?
108000120130           ////k_TBElin = TBXlin;
108100120130           ////k_TBEsif = TBXsif;
108200120130           chain  %kds(k05tntbe01)  TNTBE000;
108300120130         endif;
108400120127
108500120130         // -?Aggiornamento tab. "3CL"?
108600120127         select;
108700120127
108800120127           // -?F5=Ripristino?
108900120127           when  dsp_aid = c_F05;
109000120127             exsr  sr_RieRecTBE;
109100120127             select;
109200120127               when  %found(TNTBE01L)   and   TBEatb <> *blank;
109300120127                 clear  TBEatb;
109400120127                 //_______________
109500120127                 UPDATE  TNTBE000;
109600120127                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
109700120127               // -?Record appena cancellato fisicamente?
109800120127               when  NOT %found(TNTBE01L);
109900120127                 clear  TBEatb;
110000120127                 //_______________
110100120127                 WRITE   TNTBE000;
110200120127                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
110300120127             endsl;
110400120127
110500120127           // -?F6 = Aggiornamento / Inserimento?
110600120127           when  dsp_aid = c_F06;
110700120127             exsr  sr_RieRecTBE;
110800120127             clear  TBEatb;
110900120127             if  %found(TNTBE01L);
111000120127               //_____________
111100120127               UPDATE TNTBE000;
111200120127               //¯¯¯¯¯¯¯¯¯¯¯¯¯
111300120127             else;
111400120127               //_____________
111500120127               WRITE  TNTBE000;
111600120127               //¯¯¯¯¯¯¯¯¯¯¯¯¯
111700120127             endif;
111800120127
111900120127           // -?F16=Annullamento?
112000120127           when  dsp_aid = c_F16;
112100120127             if  %found(TNTBE01L)   and   TBEatb <> 'A';
112200120127               TBEatb = 'A';
112300120127               //TBEftt = W1ftt;
112400120127               TBEftt = 'S';
112500120127               clear  TBEftr;
112600120127               //_______________
112700120127               UPDATE  TNTBE000;
112800120127               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112900120127             endif;
113000120127
113100120127         endsl;
113200120127
113300120127       ENDSR;
113400120127
113500120127       //--------------------------------------------------------------
113600120130       //?Caricamento dati in tab. "3CL".                              ?
113700120127       //--------------------------------------------------------------
113800120127       BEGSR  sr_RieRecTBE;
113900120127
114000120127         // -?Impostazione campi chiave se inserimento?
114100120127         if  NOT %found(TNTBE01L);
114200120127           clear TNTBE000;
114300120127           TBEcod = k_TBEcod;
114400120127           TBEke1 = k_TBEke1;
114500120127           TBEke2 = k_TBEke2;
114600120127           TBElin = k_TBElin;
114700120127           TBEsif = k_TBEsif;
114800120127         endif;
114900120127
115000120127         // -?Impostazione dati tabella da aggiornare?
115100120130         exsr  sr_Rie3CL;
115200120130         TBEuni = d3CL;
115300120127
115400120127         // -?Impostazione altri campi del record?
115500120127         TBEapl = TBXapl;
115600120127         //clear  TBEatb;       //Sarà impostato caso per caso...?
115700120127         //TBEftt = W1ftt;
115800120127         TBEftt = 'S';
115900120127         clear  TBEflt;
116000120127         if  w_SisInf = 1;
116100120127           TBEftr = 'T';
116200120127         else;
116300120127           TBEftr = 'R';
116400120127         endif;
116500120127         TBEdtr = wDate;
116600120127
116700120127       ENDSR;
116800120127
116900120127       //--------------------------------------------------------------
117000120130       //?Caricamento dati in tab. "3CL".                              ?
117100120127       //--------------------------------------------------------------
117200120130       BEGSR  sr_Rie3CL;
117300120127
117400120130         clear  d3CL;
117500120127
117600120130         d3CL.§3CLfil = %int(V1Cfll);
117700120130         d3CL.§3CLdes = V1Cdes;
117800120127
117900120127       ENDSR;
118000120127
118100120127       //--------------------------------------------------------------
118200120127       //?Operazioni finali.                                           ?
118300120127       //--------------------------------------------------------------
118400120127       BEGSR  sr_RoutEnd;
118500120127
118600120127         // -?Chiusura funzioni precedentemente aperte?
118700120127         if  $TIBS02;
118800120127           clear  TIBS02ds;
118900120206           T02tla = 'C';
119000120127           TNTBE_RicercaControllo ( kpjba : tibs02ds );
119100120127         endif;
119200120127
119300120127         // -?Chiusura pgm?
119400120127         return;
119500120127
119600120127       ENDSR;
119700120127
119800120127      /end-free
119900120127
120000120127       //--------------------------------------------------------------
120100120127       //?Definizione schiere a tempo di compilazione                  ?
120200120127       //--------------------------------------------------------------
120300120127
120400120127** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
120500120127SETRAS  GAITRAGRU FILTRAGRU
120600120127AS888   GAITRAGRPSFILTRAGRPF
120700120127** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
120800120127Codice filiale obbligatorio                                                     1
120900120127Codice filiale errato                                                           2
121000120127Codice filiale Logistica NON ammesso                                            3
121100120206Non ci sono più numeri di SERIE attribuibili tra 1 e 98                         4
121200120206Numero serie segnacolli obbligatorio                                            5
121300120206Numero serie segnacolli errato                                                  6
121400120206Serie non gestibile                                                             7
121500120206Errore istruzione SQL - Contattare il CED                                       8
121600120206AVVISO: serie già assegnata parzialmente al cliente                             9
121700120220AVVISO: serie NON ancora assegnata ad alcun cliente                            10
121800120220Ammesse SOLO filiali Logistica                                                 11
121900120220Descrizione obbligatoria                                                       12
