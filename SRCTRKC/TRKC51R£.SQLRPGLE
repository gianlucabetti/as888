000100150202      //---------------------------------------------------------------
000200150202      //?TRKC51R - CREA WF CLIENTI IN CAMPAGNA  batch
000300150202      //---------------------------------------------------------------
000400150202
000500150202       //--------------------------------------------------------------
000600150202       //?Specifiche di controllo.                                     ?
000700150202       //--------------------------------------------------------------
000800150202
000900150202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000150202     h dftactgrp(*no) actgrp(*caller)
001100150202
001200150202      //---------------------------------------------------------------
001300150202      //?Dichiarazione file.
001400150202      //---------------------------------------------------------------
001500150202
001600150202      // - Organigramma
001700150202     fAZORG01L  if   e           k disk
001800150202
001900150202      // - Anagrafica Commerciali
002000150202     fAZCMM01L  if   e           k disk
002100150202
002200150202      // - Tabelle
002300150202     fTABEL00F  if   e           k disk
002400150202
002500150202      // - File Fasi avanzamento Campagna
002600150202     fTICMF01L  if   e           k disk
002700150202
002800150202      // - Anagrafica Campagna
002900150202     fTICMP01L  if   e           k disk
003000150202
003100150202      // - Trattative/Offerte/Tariffe
003200150202     fTIVIS05L  if   e           k disk
003300150202     fTIVOF11L  if   e           k disk
003400150202
003500150202      // - File di work x Clienti in Campagna
003600150202     fWFCMC00F  o    e             disk    usropn extfile('GAITRAAZM/WFCMC00F')
003700150202
003800150202      //---------------------------------------------------------------
003900150202      //?Definizione costanti.
004000150202      //---------------------------------------------------------------
004100150202
004200150202     d FaseObj         c                   const(' 10')
004300150202     d FaseObjProp     c                   const(' 20')
004400150202     d FaseObjFine     c                   const(' 30')
004500150302     d FaseObjTf       c                   const(' TF')
004600150202     d FaseObjTtr      c                   const(' TR')
004700150202     d FaseObjCf       c                   const(' CF')
004800150202     d FaseChiudi      c                   const(' 90')
004900150202
005000150202      //---------------------------------------------------------------
005100150202      //?Definizione schiere.
005200150202      //---------------------------------------------------------------
005300150202
005400150202      //---------------------------------------------------------------
005500150202      //?Definizione aree dati.
005600150202      //---------------------------------------------------------------
005700150202
005800150202      // - Dati utente
005900150202     d §AzUte        e ds                  extname(AZUTE00F)
006000150202     d                                     dtaara
006100150202     d §DatiUte      e ds                  extname(dDatiUte)
006200150202     d                                     dtaara
006300150202
006400150202      //---------------------------------------------------------------
006500150202      //?Definizione strutture dati.
006600150202      //---------------------------------------------------------------
006700150202
006800150202      // - Status
006900150202     d Psds           sds
007000150202     d   SDSpgm          *proc
007100150202
007200150202      // - Parametri ricevuti
007300150202     d KPJBA         e ds
007400150202     d TRKC51DS      e ds                  inz
007500150202
007600150202      // - Ricerca Unificante Padre?
007700150202     d TIBS10DS      e ds                  inz
007800150202
007900150202      // - Reperimento dati utente
008000150202     d TIBS34DS      e ds
008100150202
008200150202      // - File Attività
008300150202     d TIATC00F      e ds                  extname(TIATC00F)
008400150202
008500150202      // - File Clienti in Campagna Commerciale
008600150202     d TICMC00F      e ds                  extname(TICMC00F)
008700150202
008800150202      // - File INFO Clienti in Campagna Commerciale
008900150202     d TICMI00F      e ds                  extname(TICMI00F)
009000150202
009100150202      // - Reperimento dati Anagrafica Clienti
009200150202      /copy gaitrasrc/srcprotopi,TIBS69R
009300150202
009400150202      // - Parametri per ricerca Commerciale Unificante
009500150202      /copy gaitrasrc/srcprotopi,TRMK43R_1
009600150205
009700150205      // - File WADELPCT
009800150205     d WADELDS       e ds                  extname(WADELPCT)
009900150202
010000150202      // - Tabella 05 - Aree
010100150202     d ds05          e ds
010200150202
010300150202      // - Tabella 17 - Distretti
010400150202     d ds17          e ds
010500150202
010600150202      //---------------------------------------------------------------
010700150202      //?Definizione variabili globali.
010800150202      //---------------------------------------------------------------
010900150202
011000150202      // - Flags booleani
011100150202     d Fine            s               n   inz(*off)
011200150202     d wEnd            s               n   inz(*off)
011300150202
011400150202      // - Campi di comodo data
011500150205     d Anno            s              4s 0 inz
011600150202     d Data_EUR        s               d   datfmt(*eur)
011700150202     d Data_ISO        s               d   datfmt(*iso)
011800150205     d Mese            s              2s 0 inz
011900150202
012000150202      // - Campi di comodo
012100150202     d comando         s            512a   varying
012200150202     d Oggi            s              8s 0 inz
012300150227     d wDel            s              8s 1 inz
012400150209     d wKsu            s                   like(CMCksu) inz
012500150202     d wNrv            s                   like(CMFnrv) inz
012600150209     d wRpres          s             20s 3 inz
012700150209     d wRreale         s             20s 3 inz
012800150203     d wVal            s             20s 3 inz
012900150202
013000150202      //---------------------------------------------------------------
013100150202      //?Definizione procedure usate.
013200150202      //---------------------------------------------------------------
013300150202
013400150202      //---------------------------------------------------------------
013500150202      //?Definizione Prototipi.
013600150202      //---------------------------------------------------------------
013700150202      /copy gaitrasrc/srcprotopr,TIBS10R
013800150202      /copy gaitrasrc/srcprotopr,TIBS34R
013900150202      /copy gaitrasrc/srcprotopr,TIBS69R
014000150202      /copy gaitrasrc/srcprotopr,SYSTEM
014100150202
014200150202      //---------------------------------------------------------------
014300150202      //?Definizione key-list.
014400150202      //---------------------------------------------------------------
014500150202
014600150202       // - File TABEL00F
014700150202     d k03tabel      e ds                  extname(TABEL00F:*key)
014800150202     d                                     prefix(k_)
014900150202
015000150202      //---------------------------------------------------------------
015100150202      //?M A I N - L I N E
015200150202      //---------------------------------------------------------------
015300150202
015400150202     c     *Entry        plist
015500150202     c                   parm                    kpjba
015600150202
015700150202      /free
015800150202
015900150202       //?Operazioni iniziali
016000150202       exsr RoutInz;
016100150202
016200150202       //?Elabora
016300150202       exsr Elabora;
016400150202
016500150202       //?Operazioni finali
016600150202       exsr RoutEnd;
016700150202
016800150202       //--------------------------------------------------------------
016900150202       //?Operazioni iniziali.
017000150202       //--------------------------------------------------------------
017100150202       BEGSR RoutInz;
017200150202
017300150202         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017400150202
017500150202       //?Dati ricevuti
017600150202         TRKC51DS = kpjbu;
017700150202
017800150202       //?Imposto oggi
017900150202         Oggi = %dec(%date());
018000150205
018100150205       //?Imposto Anno e Mese elaborazione
018200150306         Data_ISO = %date(Oggi);
018300150306         Anno     = %subdt(Data_ISO:*years);
018400150306         Mese     = %subdt(Data_ISO:*months);
018500150202
018600150202       //?Reperimento dati job
018700150202         exsr DatiJob;
018800150202
018900150202       //?Pulisco il file di work
019000150202         comando = 'CLRPFM FILE(GAITRAAZM/WFCMC00F)';
019100150202         IF  ExecuteCommand (comando) > 0 ;
019200150202           exsr RoutEnd;
019300150202         ENDIF;
019400150202
019500150202       //?Apro il file di work
019600150202         open WFCMC00F;
019700150203
019800150203       //?Impostazione campi "fissi"
019900150203         k_TBLkut = 1;
020000150202
020100150202       ENDSR;
020200150202
020300150202       //--------------------------------------------------------------
020400150202       //?Reperimento Dati del job (Utente/Operativi).
020500150202       //--------------------------------------------------------------
020600150202       BEGSR DatiJob;
020700150202
020800150202         in(E) §AzUte;
020900150202         IF  NOT %error;
021000150202           in(E) §DatiUte;
021100150202         ENDIF;
021200150202         IF  %error or RSut = *blanks;
021300150202           clear TIBS34ds;
021400150202           tibs34r(tibs34ds);
021500150202           in §AzUte;
021600150202           in §DatiUte;
021700150202         ENDIF;
021800150202
021900150202       ENDSR;
022000150202
022100150202       //--------------------------------------------------------------
022200150202       //?Elabora.
022300150202       //--------------------------------------------------------------
022400150202       BEGSR Elabora;
022500150202
022600150202       //?Leggo i clienti che sono in campagna
022700150202         exec sql
022800150202         declare WRK cursor for
022900150203         select *
023000150202         from TICMC00F join TICMI00F on
023100150202         CMCncm = CMIncm and CMCksu = CMIksu and
023200150202         CMCksc = CMIksc and CMCcpo = CMIcpo
023300150202         where CMCncm = :IKC51ncm;
023400150202
023500150202         //?Apertura del cursore
023600150202         exec sql open WRK;
023700150202
023800150202         IF sqlcode < 0;
023900150202           leavesr;
024000150202         ENDIF;
024100150202
024200150202         DOW  not wEnd;
024300150202           exec sql
024400150202           fetch next from WRK into :TICMC00F, :TICMI00F;
024500150202           IF sqlcod = 100 or sqlcod < 0;
024600150202             wEnd = *on;
024700150202             leave;
024800150202           ENDIF;
024900150202
025000150202         //?Imposto i dati sul file di work
025100150202           exsr ImpostaDati;
025200150202
025300150202         //?Scrivo file di work
025400150202           write WFCMC000;
025500150202
025600150202         ENDDO;
025700150202
025800150202         exec sql close WRK;
025900150202
026000150202       ENDSR;
026100150202
026200150202       //--------------------------------------------------------------
026300150202       //?Imposto dati sul file di work.
026400150202       //--------------------------------------------------------------
026500150202       BEGSR ImpostaDati;
026600150202
026700150202         clear wNrv;
026800150202
026900150202       //?Pulisco record
027000150202         clear  WFCMC000;
027100150202
027200150202       //?Data/Ora/Utente elaborazione
027300150202         WCMCdae = %dec(%date());
027400150202         WCMCore = %dec(%time());
027500150202         WCMCute = knmus;
027600150202
027700150202       //?Campagna
027800150202         WCMCncm = IKC51ncm;
027900150202         chain WCMCncm TICMP01L;
028000150202         IF  %found(TICMP01L);
028100150202           WCMCncmd = CMPdes;
028200150202         ENDIF;
028300150202
028400150202       //?Cliente
028500150202         WCMCksu = CMCksu;
028600150202         clear TIBS69DS;
028700150202         I69kac = CMCksu;
028800150202         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
028900150202         WCMCrag = ACOrag;
029000150209
029100150209       //?Ultima fase
029200150209         SELECT;
029300150209         WHEN  CMCufe = FaseObj;
029400150209           WCMCuob = 'I';
029500150209         WHEN  CMCufe = FaseObjProp;
029600150209           WCMCuob = 'P';
029700150209         WHEN  CMCufe = FaseObjFine;
029800150209           WCMCuob = 'F';
029900150209         WHEN  CMCufe = FaseChiudi;
030000150209           WCMCcch = CMCcch;
030100150209           WCMCdte = CMCdch;
030200150209         ENDSL;
030300150202
030400150202       //?Dati da TICMI
030500150202         WCMCclv = CMIclv;
030600150202         WCMCcmm = CMIcmm;
030700150202         WCMCdis = CMIdcm;
030800150202         WCMCcar = CMIacm;
030900150202         WCMCfil = CMIfcm;
031000150202         WCMCpde = CMIpde;
031100150202         WCMCnsp = CMInsp;
031200150202         WCMCric = CMIric;
031300150202         WCMCpme = CMIpme;
031400150202         WCMCist = CMIist;
031500150202         WCMCcln = CMIcln;
031600150202         WCMCmes = CMImes;
031700150202
031800150202       //?Decodifico Commerciale
031900150202         chain WCMCcmm AZCMM01L;
032000150202         IF  %found(AZCMM01L);
032100150202           WCMCcmmd = CMMdes;
032200150202         ENDIF;
032300150202
032400150202       //?Decodifico Distretto/Area/Filiale
032500150202         clear ds17;
032600150202         k_tblcod = '17';
032700150202         k_tblkey = WCMCdis;
032800150202         chain  %kds(K03tabel) TABEL00F;
032900150202         IF  %found(TABEL00F);
033000150202           ds17 = TBLuni;
033100150202         ENDIF;
033200150202         WCMCdisd = §17des;
033300150202
033400150202         clear ds05;
033500150202         k_tblcod = '05';
033600150202         k_tblkey = %editc(WCMCcar:'X');
033700150202         chain  %kds(K03tabel) TABEL00F;
033800150202         IF  %found(TABEL00F);
033900150202           ds05 = TBLuni;
034000150202         ENDIF;
034100150202         WCMCcard = §05des;
034200150202
034300150202         chain WCMCfil AZORG01L;
034400150202         IF  %found(AZORG01L) and ORGfva = *blanks;
034500150202           WCMCfild = ORGdes;
034600150202         ENDIF;
034700150202
034800150202       //?Imposto i campi da TICMF
034900150202       //?Obiettivo inizio
035000150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
035100150202         IF  %found(TICMF01L);
035200150202           WCMCobi = CMFpea;
035300150202         ENDIF;
035400150202       //?Obiettivo Proposto
035500150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
035600150202         IF  %found(TICMF01L);
035700150202           WCMCobp = CMFpea;
035800150202         ENDIF;
035900150202       //?Obiettivo Finale
036000150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
036100150202         IF  %found(TICMF01L);
036200150202           WCMCobf = CMFpea;
036300150202         ENDIF;
036400150202       //?Aumento da Trattativa  + trattativa SI/NO  + Nr.Trattativa
036500150302       //?Prima provo con fase TR
036600150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTtr) TICMF01L;
036700150202         IF  %found(TICMF01L);
036800150202           WCMCobt = CMFpea;
036900150202           WCMCdtt = CMFdfa;
037000150202           wNrv = CMFnrv;
037100150302         ELSE;
037200150302       //?Se non c'è la fase TR provo con la TF
037300150302           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTf) TICMF01L;
037400150302           IF  %found(TICMF01L);
037500150302             WCMCobt = CMFpea;
037600150302             WCMCdtt = CMFdfa;
037700150302             clear wNrv;
037800150302             WCMCesi = 'P';
037900150302           ENDIF;
038000150302         ENDIF;
038100150202       //?% Confronto Fatturazione
038200150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjCf) TICMF01L;
038300150202         IF  %found(TICMF01L);
038400150202           WCMCobc = CMFpea;
038500150202           WCMCdtc = CMFdfa;
038600150202           WCMCamcf = (CMFaacf * 100) + CMFmmcf;
038700150202           WCMCnocf = CMFnocf;
038800150202           WCMCksucf = CMFnksu;
038900150202         ENDIF;
039000150202
039100150202       //?Calcolo i valori
039200150203         WCMCvobi = (WCMCric * WCMCobi) / 100;
039300150203         WCMCvobp = (WCMCric * WCMCobp) / 100;
039400150203         WCMCvobf = (WCMCric * WCMCobf) / 100;
039500150203         WCMCvobt = (WCMCric * WCMCobt) / 100;
039600150203         WCMCvobc = (WCMCric * WCMCobc) / 100;
039700150202
039800150202       //?Cerco i dati relativi alla trattativa
039900150302         IF  wNrv > 0;
040000150202         chain wNrv TIVIS05L;
040100150202         IF  %found(TIVIS05L);
040200150202           WCMCdat = VISdat;
040300150202           WCMCdch = VISdch;
040400150202           SELECT;
040500150202           WHEN  VISesi = *blanks;
040600150202           WHEN  VISesi <> 'P ';
040700150202             WCMCesi = 'N';
040800150202           OTHER;
040900150202             WCMCesi = 'P';
041000150202           ENDSL;
041100150202         ENDIF;
041200150202
041300150202       //?Imposto se effettuato appuntamento
041400150202         exec sql
041500150202         select TIATC00F.* into :TIATC00F
041600150202         from TIATC00F
041700150202         where ATCksc = :CMCksu and ATCcad = '91' and
041800150202         ATCtat = 'A' and ATCest = 'S' and
041900150202         ATCnrv = :wNrv;
042000150202         IF  sqlcod = 0;
042100150202           WCMCapp = 'S';
042200150202         ENDIF;
042300150202
042400150202       //?Imposto se presente almeno 1 offerta
042500150202         setll (wNrv) TIVOF11L;
042600150202         reade (wNrv) TIVOF11L;
042700150202         DOW not %eof(TIVOF11L);
042800150202       //?se trovo anche solo 1 offerta valida
042900150202       //?è un AVVIATO CON OFFERTA
043000150202           IF  VOFeso <> 'H' and VOFeso <> '*';
043100150202             WCMCoff = 'S';
043200150202             leave;
043300150202           ENDIF;
043400150202           reade (wNrv) TIVOF11L;
043500150202         ENDDO;
043600150302         ENDIF;
043700150202
043800150202       //?Controllo se nel frattempo è diventato figlio
043900150202         exsr NewPadre;
044000150205
044100150205       //?Recupero i dati da WADELPCT
044200150205         exsr Dati_Wadel;
044300150202
044400150202       ENDSR;
044500150202
044600150202       //--------------------------------------------------------------
044700150202       //?Cerco il nuovo padre.
044800150202       //--------------------------------------------------------------
044900150202       BEGSR NewPadre;
045000150202
045100150202       //?Controllo se ora è figlio
045200150202         clear TIBS10DS;
045300150202         D10tle = 'ST';
045400150202         D10paf = 'P';
045500150202         D10cod = CMCksu;
045600150202         TIBS10R (TIBS10DS);
045700150202       //?Se ritorna errore vuol dire che non è un codice figlio
045800150202         IF  D10err <> *blanks;
045900150202           leavesr;
046000150202         ENDIF;
046100150202       //?Se non c'è il codice padre vado via
046200150202         IF  D10cop = *zeros;
046300150202           leavesr;
046400150202         ENDIF;
046500150202
046600150202       //?Se arrivo qua vuol dire che è diventato figlio
046700150202         WCMCksunw = D10cop;
046800150202
046900150202       ENDSR;
047000150205
047100150205       //--------------------------------------------------------------
047200150205       //?Imposto i dati recuperati da WADELPCT.
047300150205       //--------------------------------------------------------------
047400150205       BEGSR Dati_Wadel;
047500150205
047600150209       //?Imposto il codice unificante nuovo, se presente, o da file campagna
047700150209         IF  WCMCksunw <> 0;
047800150209           wKsu = WCMCksunw;
047900150209         ELSE;
048000150209           wKsu = CMCksu;
048100150209         ENDIF;
048200150209
048300150205         exec sql
048400150205         select WADELPCT.* into :WADELDS
048500150205         from GAITRAGRU/WADELPCT
048600150209         where WAPcli = :wKsu;
048700150205         IF  sqlcod <> 0;
048800150205           leavesr;
048900150205         ENDIF;
049000150209
049100150209         clear wVal;
049200150209         clear wDel;
049300150209
049400150209       //?Imposto periodo di elaborazione WADELPCTe
049500150209         WCMCamdel = (WAPaaa * 100) + WAPmma;
049600150205
049700150205       //?Recupero i dati progressivi anno corrente
049800150205         WCMCpdea = WAPdac;
049900150205         WCMCnspa = WAPsac;
050000150205         WCMCrica = WAPfac;
050100150205         WCMCpmea = WAPkgc;
050200150205
050300150205       //?Recupero i dati del mese
050400150205         SELECT;
050500150209         WHEN  WAPmma = 01;
050600150205           WCMCnspm = WAPns_01ac;
050700150205           WCMCricm = WAPrr_01ac;
050800150205           WCMCpmem = WAPkg_01ac;
050900150209           wRpres = WAPrp_01ac;
051000150209         WHEN  WAPmma = 02;
051100150205           WCMCnspm = WAPns_02ac;
051200150205           WCMCricm = WAPrr_02ac;
051300150205           WCMCpmem = WAPkg_02ac;
051400150209           wRpres = WAPrp_02ac;
051500150209         WHEN  WAPmma = 03;
051600150205           WCMCnspm = WAPns_03ac;
051700150205           WCMCricm = WAPrr_03ac;
051800150205           WCMCpmem = WAPkg_03ac;
051900150209           wRpres = WAPrp_03ac;
052000150209         WHEN  WAPmma = 04;
052100150205           WCMCnspm = WAPns_04ac;
052200150205           WCMCricm = WAPrr_04ac;
052300150205           WCMCpmem = WAPkg_04ac;
052400150209           wRpres = WAPrp_04ac;
052500150209         WHEN  WAPmma = 05;
052600150205           WCMCnspm = WAPns_05ac;
052700150205           WCMCricm = WAPrr_05ac;
052800150205           WCMCpmem = WAPkg_05ac;
052900150209           wRpres = WAPrp_05ac;
053000150209         WHEN  WAPmma = 06;
053100150205           WCMCnspm = WAPns_06ac;
053200150205           WCMCricm = WAPrr_06ac;
053300150205           WCMCpmem = WAPkg_06ac;
053400150209           wRpres = WAPrp_06ac;
053500150209         WHEN  WAPmma = 07;
053600150205           WCMCnspm = WAPns_07ac;
053700150205           WCMCricm = WAPrr_07ac;
053800150205           WCMCpmem = WAPkg_07ac;
053900150209           wRpres = WAPrp_07ac;
054000150209         WHEN  WAPmma = 08;
054100150205           WCMCnspm = WAPns_08ac;
054200150205           WCMCricm = WAPrr_08ac;
054300150205           WCMCpmem = WAPkg_08ac;
054400150209           wRpres = WAPrp_08ac;
054500150209         WHEN  WAPmma = 09;
054600150205           WCMCnspm = WAPns_09ac;
054700150205           WCMCricm = WAPrr_09ac;
054800150205           WCMCpmem = WAPkg_09ac;
054900150209           wRpres = WAPrp_09ac;
055000150209         WHEN  WAPmma = 10;
055100150205           WCMCnspm = WAPns_10ac;
055200150205           WCMCricm = WAPrr_10ac;
055300150205           WCMCpmem = WAPkg_10ac;
055400150209           wRpres = WAPrp_10ac;
055500150209         WHEN  WAPmma = 11;
055600150205           WCMCnspm = WAPns_11ac;
055700150205           WCMCricm = WAPrr_11ac;
055800150205           WCMCpmem = WAPkg_11ac;
055900150209           wRpres = WAPrp_11ac;
056000150209         WHEN  WAPmma = 12;
056100150205           WCMCnspm = WAPns_12ac;
056200150205           WCMCricm = WAPrr_12ac;
056300150205           WCMCpmem = WAPkg_12ac;
056400150209           wRpres = WAPrp_12ac;
056500150205         ENDSL;
056600150209
056700150209       //?Calcolo il delta
056800150209       //?Ricavo Reale - Ricavo presunto * 100 / Ricavo Reale
056900150209         wRreale = WCMCricm;
057000150209         wVal = (wRreale - wRpres) * 100;
057100150209
057200150209         IF  wRreale = 0;
057300150209           wRreale = 1;
057400150209         ENDIF;
057500150209
057600150209         wVal = wVal / wRreale;
057700150209         eval(h) wDel = wVal;
057800150209
057900150209         SELECT;
058000150209         WHEN  wDel = 0;
058100150209         WHEN  wDel >= 1000;
058200150209           WCMCpdem = 999,9;
058300150209         WHEN  wDel <= -1000;
058400150209           WCMCpdem = -999,9;
058500150209         OTHER;
058600150209           WCMCpdem = wDel;
058700150209         ENDSL;
058800150209
058900150205       ENDSR;
059000150202
059100150202       //--------------------------------------------------------------
059200150202       //?Operazioni finali.
059300150202       //--------------------------------------------------------------
059400150202       BEGSR RoutEnd;
059500150202
059600150202         *inLR = *on;
059700150202         return;
059800150202
059900150202       ENDSR;
060000150202
060100150202      /end-free
060200150202
