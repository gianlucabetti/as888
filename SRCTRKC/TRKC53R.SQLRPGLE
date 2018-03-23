000100150202      //---------------------------------------------------------------
000200150416      //?TRKC53R - CREA WF CLIENTI IN CAMPAGNA  batch
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
003400150416
003500150416      // - Tariffe
003600150416     fTNTAM01L  if   e           k disk
003700150202
003800150202      // - File di work x Clienti in Campagna
003900150416     fWFCMC10F  o    e             disk    usropn extfile('GAITRAAZM/WFCMC10F')
004000150202
004100150202      //---------------------------------------------------------------
004200150202      //?Definizione costanti.
004300150202      //---------------------------------------------------------------
004400150202
004500150202     d FaseObj         c                   const(' 10')
004600150202     d FaseObjProp     c                   const(' 20')
004700150202     d FaseObjFine     c                   const(' 30')
004800150302     d FaseObjTf       c                   const(' TF')
004900150202     d FaseObjTtr      c                   const(' TR')
005000150202     d FaseObjCf       c                   const(' CF')
005100150202     d FaseChiudi      c                   const(' 90')
005200150202
005300150202      //---------------------------------------------------------------
005400150202      //?Definizione schiere.
005500150202      //---------------------------------------------------------------
005600150202
005700150202      //---------------------------------------------------------------
005800150202      //?Definizione aree dati.
005900150202      //---------------------------------------------------------------
006000150202
006100150202      // - Dati utente
006200150202     d §AzUte        e ds                  extname(AZUTE00F)
006300150202     d                                     dtaara
006400150202     d §DatiUte      e ds                  extname(dDatiUte)
006500150202     d                                     dtaara
006600150202
006700150202      //---------------------------------------------------------------
006800150202      //?Definizione strutture dati.
006900150202      //---------------------------------------------------------------
007000150202
007100150202      // - Status
007200150202     d Psds           sds
007300150202     d   SDSpgm          *proc
007400150202
007500150202      // - Parametri ricevuti
007600150202     d KPJBA         e ds
007700150416     d TRKC53DS      e ds                  inz
007800150202
007900150202      // - Ricerca Unificante Padre?
008000150202     d TIBS10DS      e ds                  inz
008100150202
008200150202      // - Reperimento dati utente
008300150202     d TIBS34DS      e ds
008400150202
008500150202      // - File Attività
008600150202     d TIATC00F      e ds                  extname(TIATC00F)
008700150202
008800150202      // - File Clienti in Campagna Commerciale
008900150202     d TICMC00F      e ds                  extname(TICMC00F)
009000150202
009100150202      // - File INFO Clienti in Campagna Commerciale
009200150202     d TICMI00F      e ds                  extname(TICMI00F)
009300150202
009400150202      // - Reperimento dati Anagrafica Clienti
009500150202      /copy gaitrasrc/srcprotopi,TIBS69R
009600150202
009700150202      // - Parametri per ricerca Commerciale Unificante
009800150202      /copy gaitrasrc/srcprotopi,TRMK43R_1
009900150205
010000150205      // - File WADELPCT
010100150205     d WADELDS       e ds                  extname(WADELPCT)
010200150202
010300150202      // - Tabella 05 - Aree
010400150202     d ds05          e ds
010500150202
010600150202      // - Tabella 17 - Distretti
010700150202     d ds17          e ds
010800150202
010900150202      //---------------------------------------------------------------
011000150202      //?Definizione variabili globali.
011100150202      //---------------------------------------------------------------
011200150202
011300150202      // - Flags booleani
011400150202     d Fine            s               n   inz(*off)
011500150422     d NoTam           s               n   inz(*off)
011600150202     d wEnd            s               n   inz(*off)
011700150202
011800150202      // - Campi di comodo data
011900150205     d Anno            s              4s 0 inz
012000160128     d Annodic         s              4s 0 inz
012100150202     d Data_EUR        s               d   datfmt(*eur)
012200150202     d Data_ISO        s               d   datfmt(*iso)
012300150205     d Mese            s              2s 0 inz
012400150202
012500150202      // - Campi di comodo
012600150202     d comando         s            512a   varying
012700150202     d Oggi            s              8s 0 inz
012800150227     d wDel            s              8s 1 inz
012900150209     d wKsu            s                   like(CMCksu) inz
013000150202     d wNrv            s                   like(CMFnrv) inz
013100150209     d wRpres          s             20s 3 inz
013200150209     d wRreale         s             20s 3 inz
013300150203     d wVal            s             20s 3 inz
013400150202
013500150202      //---------------------------------------------------------------
013600150202      //?Definizione procedure usate.
013700150202      //---------------------------------------------------------------
013800150202
013900150202      //---------------------------------------------------------------
014000150202      //?Definizione Prototipi.
014100150202      //---------------------------------------------------------------
014200150202      /copy gaitrasrc/srcprotopr,TIBS10R
014300150202      /copy gaitrasrc/srcprotopr,TIBS34R
014400150202      /copy gaitrasrc/srcprotopr,TIBS69R
014500150202      /copy gaitrasrc/srcprotopr,SYSTEM
014600150202
014700150202      //---------------------------------------------------------------
014800150202      //?Definizione key-list.
014900150202      //---------------------------------------------------------------
015000150202
015100150202       // - File TABEL00F
015200150202     d k03tabel      e ds                  extname(TABEL00F:*key)
015300150202     d                                     prefix(k_)
015400150202
015500150202      //---------------------------------------------------------------
015600150202      //?M A I N - L I N E
015700150202      //---------------------------------------------------------------
015800150202
015900150202     c     *Entry        plist
016000150202     c                   parm                    kpjba
016100150202
016200150202      /free
016300150202
016400150202       //?Operazioni iniziali
016500150202       exsr RoutInz;
016600150202
016700150202       //?Elabora
016800150202       exsr Elabora;
016900150202
017000150202       //?Operazioni finali
017100150202       exsr RoutEnd;
017200150202
017300150202       //--------------------------------------------------------------
017400150202       //?Operazioni iniziali.
017500150202       //--------------------------------------------------------------
017600150202       BEGSR RoutInz;
017700150202
017800150202         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017900150202
018000150202       //?Dati ricevuti
018100150416         TRKC53DS = kpjbu;
018200150202
018300150202       //?Imposto oggi
018400150202         Oggi = %dec(%date());
018500150205
018600150205       //?Imposto Anno e Mese elaborazione
018700150306         Data_ISO = %date(Oggi);
018800150306         Anno     = %subdt(Data_ISO:*years);
018900150306         Mese     = %subdt(Data_ISO:*months);
019000150202
019100150202       //?Reperimento dati job
019200150202         exsr DatiJob;
019300150202
019400150202       //?Pulisco il file di work
019500150416         comando = 'CLRPFM FILE(GAITRAAZM/WFCMC10F)';
019600150202         IF  ExecuteCommand (comando) > 0 ;
019700150202           exsr RoutEnd;
019800150202         ENDIF;
019900150202
020000150202       //?Apro il file di work
020100150416         open WFCMC10F;
020200150203
020300150203       //?Impostazione campi "fissi"
020400150203         k_TBLkut = 1;
020500150202
020600150202       ENDSR;
020700150202
020800150202       //--------------------------------------------------------------
020900150202       //?Reperimento Dati del job (Utente/Operativi).
021000150202       //--------------------------------------------------------------
021100150202       BEGSR DatiJob;
021200150202
021300150202         in(E) §AzUte;
021400150202         IF  NOT %error;
021500150202           in(E) §DatiUte;
021600150202         ENDIF;
021700150202         IF  %error or RSut = *blanks;
021800150202           clear TIBS34ds;
021900150202           tibs34r(tibs34ds);
022000150202           in §AzUte;
022100150202           in §DatiUte;
022200150202         ENDIF;
022300150202
022400150202       ENDSR;
022500150202
022600150202       //--------------------------------------------------------------
022700150202       //?Elabora.
022800150202       //--------------------------------------------------------------
022900150202       BEGSR Elabora;
023000150202
023100150202       //?Leggo i clienti che sono in campagna
023200150202         exec sql
023300150202         declare WRK cursor for
023400150203         select *
023500150202         from TICMC00F join TICMI00F on
023600150202         CMCncm = CMIncm and CMCksu = CMIksu and
023700150202         CMCksc = CMIksc and CMCcpo = CMIcpo
023800150416         where CMCncm = :IKC53ncm;
023900150202
024000150202         //?Apertura del cursore
024100150202         exec sql open WRK;
024200150202
024300150202         IF sqlcode < 0;
024400150202           leavesr;
024500150202         ENDIF;
024600150202
024700150202         DOW  not wEnd;
024800150202           exec sql
024900150202           fetch next from WRK into :TICMC00F, :TICMI00F;
025000150202           IF sqlcod = 100 or sqlcod < 0;
025100150202             wEnd = *on;
025200150202             leave;
025300150202           ENDIF;
025400150202
025500150202         //?Imposto i dati sul file di work
025600150202           exsr ImpostaDati;
025700150202
025800150202         //?Scrivo file di work
025900150416           write WFCMC100;
026000150202
026100150202         ENDDO;
026200150202
026300150202         exec sql close WRK;
026400150202
026500150202       ENDSR;
026600150202
026700150202       //--------------------------------------------------------------
026800150202       //?Imposto dati sul file di work.
026900150202       //--------------------------------------------------------------
027000150202       BEGSR ImpostaDati;
027100150202
027200150202         clear wNrv;
027300150202
027400150202       //?Pulisco record
027500150416         clear  WFCMC100;
027600150202
027700150202       //?Data/Ora/Utente elaborazione
027800150202         WCMCdae = %dec(%date());
027900150202         WCMCore = %dec(%time());
028000150202         WCMCute = knmus;
028100150202
028200150202       //?Campagna
028300150416         WCMCncm = IKC53ncm;
028400150202         chain WCMCncm TICMP01L;
028500150202         IF  %found(TICMP01L);
028600150202           WCMCncmd = CMPdes;
028700150202         ENDIF;
028800150202
028900150202       //?Cliente
029000150202         WCMCksu = CMCksu;
029100150202         clear TIBS69DS;
029200150202         I69kac = CMCksu;
029300150202         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
029400150202         WCMCrag = ACOrag;
029500150209
029600150209       //?Ultima fase
029700150209         SELECT;
029800150209         WHEN  CMCufe = FaseObj;
029900150209           WCMCuob = 'I';
030000150209         WHEN  CMCufe = FaseObjProp;
030100150209           WCMCuob = 'P';
030200150209         WHEN  CMCufe = FaseObjFine;
030300150209           WCMCuob = 'F';
030400150209         WHEN  CMCufe = FaseChiudi;
030500150209           WCMCcch = CMCcch;
030600150209           WCMCdte = CMCdch;
030700150209         ENDSL;
030800150202
030900150202       //?Dati da TICMI
031000150202         WCMCclv = CMIclv;
031100150202         WCMCcmm = CMIcmm;
031200150202         WCMCdis = CMIdcm;
031300150202         WCMCcar = CMIacm;
031400150202         WCMCfil = CMIfcm;
031500150202         WCMCpde = CMIpde;
031600150202         WCMCnsp = CMInsp;
031700150202         WCMCric = CMIric;
031800150202         WCMCpme = CMIpme;
031900150202         WCMCist = CMIist;
032000150202         WCMCcln = CMIcln;
032100150202         WCMCmes = CMImes;
032200150202
032300150202       //?Decodifico Commerciale
032400150202         chain WCMCcmm AZCMM01L;
032500150202         IF  %found(AZCMM01L);
032600150202           WCMCcmmd = CMMdes;
032700150202         ENDIF;
032800150202
032900150202       //?Decodifico Distretto/Area/Filiale
033000150202         clear ds17;
033100150202         k_tblcod = '17';
033200150202         k_tblkey = WCMCdis;
033300150202         chain  %kds(K03tabel) TABEL00F;
033400150202         IF  %found(TABEL00F);
033500150202           ds17 = TBLuni;
033600150202         ENDIF;
033700150202         WCMCdisd = §17des;
033800150202
033900150202         clear ds05;
034000150202         k_tblcod = '05';
034100150202         k_tblkey = %editc(WCMCcar:'X');
034200150202         chain  %kds(K03tabel) TABEL00F;
034300150202         IF  %found(TABEL00F);
034400150202           ds05 = TBLuni;
034500150202         ENDIF;
034600150202         WCMCcard = §05des;
034700150202
034800150202         chain WCMCfil AZORG01L;
034900150202         IF  %found(AZORG01L) and ORGfva = *blanks;
035000150202           WCMCfild = ORGdes;
035100150202         ENDIF;
035200150202
035300150202       //?Imposto i campi da TICMF
035400150202       //?Obiettivo inizio
035500150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
035600150202         IF  %found(TICMF01L);
035700150202           WCMCobi = CMFpea;
035800150202         ENDIF;
035900150202       //?Obiettivo Proposto
036000150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
036100150202         IF  %found(TICMF01L);
036200150202           WCMCobp = CMFpea;
036300150202         ENDIF;
036400150202       //?Obiettivo Finale
036500150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
036600150202         IF  %found(TICMF01L);
036700150202           WCMCobf = CMFpea;
036800150202         ENDIF;
036900150202       //?Aumento da Trattativa  + trattativa SI/NO  + Nr.Trattativa
037000150302       //?Prima provo con fase TR
037100150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTtr) TICMF01L;
037200150202         IF  %found(TICMF01L);
037300150202           WCMCobt = CMFpea;
037400150202           WCMCdtt = CMFdfa;
037500150202           wNrv = CMFnrv;
037600150302         ELSE;
037700150302       //?Se non c'è la fase TR provo con la TF
037800150302           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTf) TICMF01L;
037900150302           IF  %found(TICMF01L);
038000150302             WCMCobt = CMFpea;
038100150302             WCMCdtt = CMFdfa;
038200150302             clear wNrv;
038300150302             WCMCesi = 'P';
038400150417             WCMCamtr = (CMFaacf * 100) + CMFmmcf;
038500150302           ENDIF;
038600150302         ENDIF;
038700150202       //?% Confronto Fatturazione
038800150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjCf) TICMF01L;
038900150202         IF  %found(TICMF01L);
039000150202           WCMCobc = CMFpea;
039100150202           WCMCdtc = CMFdfa;
039200150202           WCMCamcf = (CMFaacf * 100) + CMFmmcf;
039300150202           WCMCnocf = CMFnocf;
039400150202           WCMCksucf = CMFnksu;
039500150202         ENDIF;
039600150202
039700150202       //?Calcolo i valori
039800150203         WCMCvobi = (WCMCric * WCMCobi) / 100;
039900150203         WCMCvobp = (WCMCric * WCMCobp) / 100;
040000150203         WCMCvobf = (WCMCric * WCMCobf) / 100;
040100150203         WCMCvobt = (WCMCric * WCMCobt) / 100;
040200150203         WCMCvobc = (WCMCric * WCMCobc) / 100;
040300150202
040400150202       //?Cerco i dati relativi alla trattativa
040500150302         IF  wNrv > 0;
040600150202         chain wNrv TIVIS05L;
040700150202         IF  %found(TIVIS05L);
040800150202           WCMCdat = VISdat;
040900150202           WCMCdch = VISdch;
041000150202           SELECT;
041100150202           WHEN  VISesi = *blanks;
041200150202           WHEN  VISesi <> 'P ';
041300150202             WCMCesi = 'N';
041400150202           OTHER;
041500150202             WCMCesi = 'P';
041600150202           ENDSL;
041700150202         ENDIF;
041800150202
041900150202       //?Imposto se effettuato appuntamento
042000150202         exec sql
042100150202         select TIATC00F.* into :TIATC00F
042200150202         from TIATC00F
042300150202         where ATCksc = :CMCksu and ATCcad = '91' and
042400150202         ATCtat = 'A' and ATCest = 'S' and
042500150202         ATCnrv = :wNrv;
042600150202         IF  sqlcod = 0;
042700150202           WCMCapp = 'S';
042800150202         ENDIF;
042900150202
043000150202       //?Imposto se presente almeno 1 offerta
043100150202         setll (wNrv) TIVOF11L;
043200150202         reade (wNrv) TIVOF11L;
043300150202         DOW not %eof(TIVOF11L);
043400150202       //?se trovo anche solo 1 offerta valida
043500150202       //?è un AVVIATO CON OFFERTA
043600150202           IF  VOFeso <> 'H' and VOFeso <> '*';
043700150202             WCMCoff = 'S';
043800150202             leave;
043900150202           ENDIF;
044000150202           reade (wNrv) TIVOF11L;
044100150202         ENDDO;
044200150416
044300150416       //?Cerco i dati dell'offerta convalidata
044400150416           IF  WCMCesi = 'P';
044500150422             NoTam = *off;
044600150416             setll (VISnrv) TIVOF11L;
044700150416             DOW  not wEnd;
044800150416               reade (VISnrv) TIVOF11L;
044900150416               IF  %eof(TIVOF11L);
045000150416                 leave;
045100150416               ENDIF;
045200150416               IF  VOFeso <> 'C';
045300150416                 iter;
045400150416               ENDIF;
045500150416           //?Cerco la tariffa convalidata
045600150416               chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
045700150416               IF  not %found(TNTAM01L);
045800150422                 NoTam = *on;
045900150422                 iter;
046000150416               ENDIF;
046100150422               NoTam = *off;
046200150416               WCMCamtr = (%dec(%subst(%editc(TAMddt:'X'):1:4):4:0) * 100) +
046300150416                          (%dec(%subst(%editc(TAMddt:'X'):5:2):2:0));
046400150416               leave;
046500150416             ENDDO;
046600150422           //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
046700150422           //?ma tariffa cancellata
046800160128           //?in questo caso devo impostare come data tariffa
046900160128           //?Gennaio dell'anno di avvio campagna
047000150422             IF  NoTam;
047100160128               Data_ISO = %date(CMPdic);
047200160128               Annodic  = %subdt(Data_ISO:*years);
047300160128               WCMCamtr = Annodic * 100 + 01;
047400150422             ENDIF;
047500150416           ENDIF;
047600150416
047700150302         ENDIF;
047800150202
047900150202       //?Controllo se nel frattempo è diventato figlio
048000150202         exsr NewPadre;
048100150205
048200150205       //?Recupero i dati da WADELPCT
048300150205         exsr Dati_Wadel;
048400150202
048500150202       ENDSR;
048600150202
048700150202       //--------------------------------------------------------------
048800150202       //?Cerco il nuovo padre.
048900150202       //--------------------------------------------------------------
049000150202       BEGSR NewPadre;
049100150202
049200150202       //?Controllo se ora è figlio
049300150202         clear TIBS10DS;
049400150202         D10tle = 'ST';
049500150202         D10paf = 'P';
049600150202         D10cod = CMCksu;
049700150202         TIBS10R (TIBS10DS);
049800150202       //?Se ritorna errore vuol dire che non è un codice figlio
049900150202         IF  D10err <> *blanks;
050000150202           leavesr;
050100150202         ENDIF;
050200150202       //?Se non c'è il codice padre vado via
050300150202         IF  D10cop = *zeros;
050400150202           leavesr;
050500150202         ENDIF;
050600150202
050700150202       //?Se arrivo qua vuol dire che è diventato figlio
050800150202         WCMCksunw = D10cop;
050900150202
051000150202       ENDSR;
051100150205
051200150205       //--------------------------------------------------------------
051300150205       //?Imposto i dati recuperati da WADELPCT.
051400150205       //--------------------------------------------------------------
051500150205       BEGSR Dati_Wadel;
051600150205
051700150209       //?Imposto il codice unificante nuovo, se presente, o da file campagna
051800150209         IF  WCMCksunw <> 0;
051900150209           wKsu = WCMCksunw;
052000150209         ELSE;
052100150209           wKsu = CMCksu;
052200150209         ENDIF;
052300150209
052400150205         exec sql
052500150205         select WADELPCT.* into :WADELDS
052600150205         from GAITRAGRU/WADELPCT
052700150209         where WAPcli = :wKsu;
052800150205         IF  sqlcod <> 0;
052900150205           leavesr;
053000150205         ENDIF;
053100150209
053200150209         clear wVal;
053300150209         clear wDel;
053400150209
053500150209       //?Imposto periodo di elaborazione WADELPCTe
053600150209         WCMCamdel = (WAPaaa * 100) + WAPmma;
053700150205
053800150205       //?Recupero i dati progressivi anno corrente
053900150205         WCMCpdea = WAPdac;
054000150205         WCMCnspa = WAPsac;
054100150205         WCMCrica = WAPfac;
054200150205         WCMCpmea = WAPkgc;
054300150205
054400150205       //?Recupero i dati del mese
054500150205         SELECT;
054600150209         WHEN  WAPmma = 01;
054700150205           WCMCnspm = WAPns_01ac;
054800150205           WCMCricm = WAPrr_01ac;
054900150205           WCMCpmem = WAPkg_01ac;
055000150209           wRpres = WAPrp_01ac;
055100150209         WHEN  WAPmma = 02;
055200150205           WCMCnspm = WAPns_02ac;
055300150205           WCMCricm = WAPrr_02ac;
055400150205           WCMCpmem = WAPkg_02ac;
055500150209           wRpres = WAPrp_02ac;
055600150209         WHEN  WAPmma = 03;
055700150205           WCMCnspm = WAPns_03ac;
055800150205           WCMCricm = WAPrr_03ac;
055900150205           WCMCpmem = WAPkg_03ac;
056000150209           wRpres = WAPrp_03ac;
056100150209         WHEN  WAPmma = 04;
056200150205           WCMCnspm = WAPns_04ac;
056300150205           WCMCricm = WAPrr_04ac;
056400150205           WCMCpmem = WAPkg_04ac;
056500150209           wRpres = WAPrp_04ac;
056600150209         WHEN  WAPmma = 05;
056700150205           WCMCnspm = WAPns_05ac;
056800150205           WCMCricm = WAPrr_05ac;
056900150205           WCMCpmem = WAPkg_05ac;
057000150209           wRpres = WAPrp_05ac;
057100150209         WHEN  WAPmma = 06;
057200150205           WCMCnspm = WAPns_06ac;
057300150205           WCMCricm = WAPrr_06ac;
057400150205           WCMCpmem = WAPkg_06ac;
057500150209           wRpres = WAPrp_06ac;
057600150209         WHEN  WAPmma = 07;
057700150205           WCMCnspm = WAPns_07ac;
057800150205           WCMCricm = WAPrr_07ac;
057900150205           WCMCpmem = WAPkg_07ac;
058000150209           wRpres = WAPrp_07ac;
058100150209         WHEN  WAPmma = 08;
058200150205           WCMCnspm = WAPns_08ac;
058300150205           WCMCricm = WAPrr_08ac;
058400150205           WCMCpmem = WAPkg_08ac;
058500150209           wRpres = WAPrp_08ac;
058600150209         WHEN  WAPmma = 09;
058700150205           WCMCnspm = WAPns_09ac;
058800150205           WCMCricm = WAPrr_09ac;
058900150205           WCMCpmem = WAPkg_09ac;
059000150209           wRpres = WAPrp_09ac;
059100150209         WHEN  WAPmma = 10;
059200150205           WCMCnspm = WAPns_10ac;
059300150205           WCMCricm = WAPrr_10ac;
059400150205           WCMCpmem = WAPkg_10ac;
059500150209           wRpres = WAPrp_10ac;
059600150209         WHEN  WAPmma = 11;
059700150205           WCMCnspm = WAPns_11ac;
059800150205           WCMCricm = WAPrr_11ac;
059900150205           WCMCpmem = WAPkg_11ac;
060000150209           wRpres = WAPrp_11ac;
060100150209         WHEN  WAPmma = 12;
060200150205           WCMCnspm = WAPns_12ac;
060300150205           WCMCricm = WAPrr_12ac;
060400150205           WCMCpmem = WAPkg_12ac;
060500150209           wRpres = WAPrp_12ac;
060600150205         ENDSL;
060700150209
060800150209       //?Calcolo il delta
060900150209       //?Ricavo Reale - Ricavo presunto * 100 / Ricavo Reale
061000150209         wRreale = WCMCricm;
061100150209         wVal = (wRreale - wRpres) * 100;
061200150209
061300150209         IF  wRreale = 0;
061400150209           wRreale = 1;
061500150209         ENDIF;
061600150209
061700150209         wVal = wVal / wRreale;
061800150209         eval(h) wDel = wVal;
061900150209
062000150209         SELECT;
062100150209         WHEN  wDel = 0;
062200150209         WHEN  wDel >= 1000;
062300150209           WCMCpdem = 999,9;
062400150209         WHEN  wDel <= -1000;
062500150209           WCMCpdem = -999,9;
062600150209         OTHER;
062700150209           WCMCpdem = wDel;
062800150209         ENDSL;
062900150209
063000150205       ENDSR;
063100150202
063200150202       //--------------------------------------------------------------
063300150202       //?Operazioni finali.
063400150202       //--------------------------------------------------------------
063500150202       BEGSR RoutEnd;
063600150202
063700150202         *inLR = *on;
063800150202         return;
063900150202
064000150202       ENDSR;
064100150202
064200150202      /end-free
064300150202
