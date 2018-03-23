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
012000150202     d Data_EUR        s               d   datfmt(*eur)
012100150202     d Data_ISO        s               d   datfmt(*iso)
012200150205     d Mese            s              2s 0 inz
012300150202
012400150202      // - Campi di comodo
012500150202     d comando         s            512a   varying
012600150202     d Oggi            s              8s 0 inz
012700150227     d wDel            s              8s 1 inz
012800150209     d wKsu            s                   like(CMCksu) inz
012900150202     d wNrv            s                   like(CMFnrv) inz
013000150209     d wRpres          s             20s 3 inz
013100150209     d wRreale         s             20s 3 inz
013200150203     d wVal            s             20s 3 inz
013300150202
013400150202      //---------------------------------------------------------------
013500150202      //?Definizione procedure usate.
013600150202      //---------------------------------------------------------------
013700150202
013800150202      //---------------------------------------------------------------
013900150202      //?Definizione Prototipi.
014000150202      //---------------------------------------------------------------
014100150202      /copy gaitrasrc/srcprotopr,TIBS10R
014200150202      /copy gaitrasrc/srcprotopr,TIBS34R
014300150202      /copy gaitrasrc/srcprotopr,TIBS69R
014400150202      /copy gaitrasrc/srcprotopr,SYSTEM
014500150202
014600150202      //---------------------------------------------------------------
014700150202      //?Definizione key-list.
014800150202      //---------------------------------------------------------------
014900150202
015000150202       // - File TABEL00F
015100150202     d k03tabel      e ds                  extname(TABEL00F:*key)
015200150202     d                                     prefix(k_)
015300150202
015400150202      //---------------------------------------------------------------
015500150202      //?M A I N - L I N E
015600150202      //---------------------------------------------------------------
015700150202
015800150202     c     *Entry        plist
015900150202     c                   parm                    kpjba
016000150202
016100150202      /free
016200150202
016300150202       //?Operazioni iniziali
016400150202       exsr RoutInz;
016500150202
016600150202       //?Elabora
016700150202       exsr Elabora;
016800150202
016900150202       //?Operazioni finali
017000150202       exsr RoutEnd;
017100150202
017200150202       //--------------------------------------------------------------
017300150202       //?Operazioni iniziali.
017400150202       //--------------------------------------------------------------
017500150202       BEGSR RoutInz;
017600150202
017700150202         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
017800150202
017900150202       //?Dati ricevuti
018000150416         TRKC53DS = kpjbu;
018100150202
018200150202       //?Imposto oggi
018300150202         Oggi = %dec(%date());
018400150205
018500150205       //?Imposto Anno e Mese elaborazione
018600150306         Data_ISO = %date(Oggi);
018700150306         Anno     = %subdt(Data_ISO:*years);
018800150306         Mese     = %subdt(Data_ISO:*months);
018900150202
019000150202       //?Reperimento dati job
019100150202         exsr DatiJob;
019200150202
019300150202       //?Pulisco il file di work
019400150416         comando = 'CLRPFM FILE(GAITRAAZM/WFCMC10F)';
019500150202         IF  ExecuteCommand (comando) > 0 ;
019600150202           exsr RoutEnd;
019700150202         ENDIF;
019800150202
019900150202       //?Apro il file di work
020000150416         open WFCMC10F;
020100150203
020200150203       //?Impostazione campi "fissi"
020300150203         k_TBLkut = 1;
020400150202
020500150202       ENDSR;
020600150202
020700150202       //--------------------------------------------------------------
020800150202       //?Reperimento Dati del job (Utente/Operativi).
020900150202       //--------------------------------------------------------------
021000150202       BEGSR DatiJob;
021100150202
021200150202         in(E) §AzUte;
021300150202         IF  NOT %error;
021400150202           in(E) §DatiUte;
021500150202         ENDIF;
021600150202         IF  %error or RSut = *blanks;
021700150202           clear TIBS34ds;
021800150202           tibs34r(tibs34ds);
021900150202           in §AzUte;
022000150202           in §DatiUte;
022100150202         ENDIF;
022200150202
022300150202       ENDSR;
022400150202
022500150202       //--------------------------------------------------------------
022600150202       //?Elabora.
022700150202       //--------------------------------------------------------------
022800150202       BEGSR Elabora;
022900150202
023000150202       //?Leggo i clienti che sono in campagna
023100150202         exec sql
023200150202         declare WRK cursor for
023300150203         select *
023400150202         from TICMC00F join TICMI00F on
023500150202         CMCncm = CMIncm and CMCksu = CMIksu and
023600150202         CMCksc = CMIksc and CMCcpo = CMIcpo
023700150416         where CMCncm = :IKC53ncm;
023800150202
023900150202         //?Apertura del cursore
024000150202         exec sql open WRK;
024100150202
024200150202         IF sqlcode < 0;
024300150202           leavesr;
024400150202         ENDIF;
024500150202
024600150202         DOW  not wEnd;
024700150202           exec sql
024800150202           fetch next from WRK into :TICMC00F, :TICMI00F;
024900150202           IF sqlcod = 100 or sqlcod < 0;
025000150202             wEnd = *on;
025100150202             leave;
025200150202           ENDIF;
025300150202
025400150202         //?Imposto i dati sul file di work
025500150202           exsr ImpostaDati;
025600150202
025700150202         //?Scrivo file di work
025800150416           write WFCMC100;
025900150202
026000150202         ENDDO;
026100150202
026200150202         exec sql close WRK;
026300150202
026400150202       ENDSR;
026500150202
026600150202       //--------------------------------------------------------------
026700150202       //?Imposto dati sul file di work.
026800150202       //--------------------------------------------------------------
026900150202       BEGSR ImpostaDati;
027000150202
027100150202         clear wNrv;
027200150202
027300150202       //?Pulisco record
027400150416         clear  WFCMC100;
027500150202
027600150202       //?Data/Ora/Utente elaborazione
027700150202         WCMCdae = %dec(%date());
027800150202         WCMCore = %dec(%time());
027900150202         WCMCute = knmus;
028000150202
028100150202       //?Campagna
028200150416         WCMCncm = IKC53ncm;
028300150202         chain WCMCncm TICMP01L;
028400150202         IF  %found(TICMP01L);
028500150202           WCMCncmd = CMPdes;
028600150202         ENDIF;
028700150202
028800150202       //?Cliente
028900150202         WCMCksu = CMCksu;
029000150202         clear TIBS69DS;
029100150202         I69kac = CMCksu;
029200150202         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
029300150202         WCMCrag = ACOrag;
029400150209
029500150209       //?Ultima fase
029600150209         SELECT;
029700150209         WHEN  CMCufe = FaseObj;
029800150209           WCMCuob = 'I';
029900150209         WHEN  CMCufe = FaseObjProp;
030000150209           WCMCuob = 'P';
030100150209         WHEN  CMCufe = FaseObjFine;
030200150209           WCMCuob = 'F';
030300150209         WHEN  CMCufe = FaseChiudi;
030400150209           WCMCcch = CMCcch;
030500150209           WCMCdte = CMCdch;
030600150209         ENDSL;
030700150202
030800150202       //?Dati da TICMI
030900150202         WCMCclv = CMIclv;
031000150202         WCMCcmm = CMIcmm;
031100150202         WCMCdis = CMIdcm;
031200150202         WCMCcar = CMIacm;
031300150202         WCMCfil = CMIfcm;
031400150202         WCMCpde = CMIpde;
031500150202         WCMCnsp = CMInsp;
031600150202         WCMCric = CMIric;
031700150202         WCMCpme = CMIpme;
031800150202         WCMCist = CMIist;
031900150202         WCMCcln = CMIcln;
032000150202         WCMCmes = CMImes;
032100150202
032200150202       //?Decodifico Commerciale
032300150202         chain WCMCcmm AZCMM01L;
032400150202         IF  %found(AZCMM01L);
032500150202           WCMCcmmd = CMMdes;
032600150202         ENDIF;
032700150202
032800150202       //?Decodifico Distretto/Area/Filiale
032900150202         clear ds17;
033000150202         k_tblcod = '17';
033100150202         k_tblkey = WCMCdis;
033200150202         chain  %kds(K03tabel) TABEL00F;
033300150202         IF  %found(TABEL00F);
033400150202           ds17 = TBLuni;
033500150202         ENDIF;
033600150202         WCMCdisd = §17des;
033700150202
033800150202         clear ds05;
033900150202         k_tblcod = '05';
034000150202         k_tblkey = %editc(WCMCcar:'X');
034100150202         chain  %kds(K03tabel) TABEL00F;
034200150202         IF  %found(TABEL00F);
034300150202           ds05 = TBLuni;
034400150202         ENDIF;
034500150202         WCMCcard = §05des;
034600150202
034700150202         chain WCMCfil AZORG01L;
034800150202         IF  %found(AZORG01L) and ORGfva = *blanks;
034900150202           WCMCfild = ORGdes;
035000150202         ENDIF;
035100150202
035200150202       //?Imposto i campi da TICMF
035300150202       //?Obiettivo inizio
035400150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
035500150202         IF  %found(TICMF01L);
035600150202           WCMCobi = CMFpea;
035700150202         ENDIF;
035800150202       //?Obiettivo Proposto
035900150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
036000150202         IF  %found(TICMF01L);
036100150202           WCMCobp = CMFpea;
036200150202         ENDIF;
036300150202       //?Obiettivo Finale
036400150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
036500150202         IF  %found(TICMF01L);
036600150202           WCMCobf = CMFpea;
036700150202         ENDIF;
036800150202       //?Aumento da Trattativa  + trattativa SI/NO  + Nr.Trattativa
036900150302       //?Prima provo con fase TR
037000150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTtr) TICMF01L;
037100150202         IF  %found(TICMF01L);
037200150202           WCMCobt = CMFpea;
037300150202           WCMCdtt = CMFdfa;
037400150202           wNrv = CMFnrv;
037500150302         ELSE;
037600150302       //?Se non c'è la fase TR provo con la TF
037700150302           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTf) TICMF01L;
037800150302           IF  %found(TICMF01L);
037900150302             WCMCobt = CMFpea;
038000150302             WCMCdtt = CMFdfa;
038100150302             clear wNrv;
038200150302             WCMCesi = 'P';
038300150417             WCMCamtr = (CMFaacf * 100) + CMFmmcf;
038400150302           ENDIF;
038500150302         ENDIF;
038600150202       //?% Confronto Fatturazione
038700150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjCf) TICMF01L;
038800150202         IF  %found(TICMF01L);
038900150202           WCMCobc = CMFpea;
039000150202           WCMCdtc = CMFdfa;
039100150202           WCMCamcf = (CMFaacf * 100) + CMFmmcf;
039200150202           WCMCnocf = CMFnocf;
039300150202           WCMCksucf = CMFnksu;
039400150202         ENDIF;
039500150202
039600150202       //?Calcolo i valori
039700150203         WCMCvobi = (WCMCric * WCMCobi) / 100;
039800150203         WCMCvobp = (WCMCric * WCMCobp) / 100;
039900150203         WCMCvobf = (WCMCric * WCMCobf) / 100;
040000150203         WCMCvobt = (WCMCric * WCMCobt) / 100;
040100150203         WCMCvobc = (WCMCric * WCMCobc) / 100;
040200150202
040300150202       //?Cerco i dati relativi alla trattativa
040400150302         IF  wNrv > 0;
040500150202         chain wNrv TIVIS05L;
040600150202         IF  %found(TIVIS05L);
040700150202           WCMCdat = VISdat;
040800150202           WCMCdch = VISdch;
040900150202           SELECT;
041000150202           WHEN  VISesi = *blanks;
041100150202           WHEN  VISesi <> 'P ';
041200150202             WCMCesi = 'N';
041300150202           OTHER;
041400150202             WCMCesi = 'P';
041500150202           ENDSL;
041600150202         ENDIF;
041700150202
041800150202       //?Imposto se effettuato appuntamento
041900150202         exec sql
042000150202         select TIATC00F.* into :TIATC00F
042100150202         from TIATC00F
042200150202         where ATCksc = :CMCksu and ATCcad = '91' and
042300150202         ATCtat = 'A' and ATCest = 'S' and
042400150202         ATCnrv = :wNrv;
042500150202         IF  sqlcod = 0;
042600150202           WCMCapp = 'S';
042700150202         ENDIF;
042800150202
042900150202       //?Imposto se presente almeno 1 offerta
043000150202         setll (wNrv) TIVOF11L;
043100150202         reade (wNrv) TIVOF11L;
043200150202         DOW not %eof(TIVOF11L);
043300150202       //?se trovo anche solo 1 offerta valida
043400150202       //?è un AVVIATO CON OFFERTA
043500150202           IF  VOFeso <> 'H' and VOFeso <> '*';
043600150202             WCMCoff = 'S';
043700150202             leave;
043800150202           ENDIF;
043900150202           reade (wNrv) TIVOF11L;
044000150202         ENDDO;
044100150416
044200150416       //?Cerco i dati dell'offerta convalidata
044300150416           IF  WCMCesi = 'P';
044400150422             NoTam = *off;
044500150416             setll (VISnrv) TIVOF11L;
044600150416             DOW  not wEnd;
044700150416               reade (VISnrv) TIVOF11L;
044800150416               IF  %eof(TIVOF11L);
044900150416                 leave;
045000150416               ENDIF;
045100150416               IF  VOFeso <> 'C';
045200150416                 iter;
045300150416               ENDIF;
045400150416           //?Cerco la tariffa convalidata
045500150416               chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
045600150416               IF  not %found(TNTAM01L);
045700150422                 NoTam = *on;
045800150422                 iter;
045900150416               ENDIF;
046000150422               NoTam = *off;
046100150416               WCMCamtr = (%dec(%subst(%editc(TAMddt:'X'):1:4):4:0) * 100) +
046200150416                          (%dec(%subst(%editc(TAMddt:'X'):5:2):2:0));
046300150416               leave;
046400150416             ENDDO;
046500150422           //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
046600150422           //?ma tariffa cancellata
046700150422           //?in questo caso devo impostare come data tariffa 01/2015
046800150422             IF  NoTam;
046900150422               WCMCamtr = 201501;
047000150422             ENDIF;
047100150416           ENDIF;
047200150416
047300150302         ENDIF;
047400150202
047500150202       //?Controllo se nel frattempo è diventato figlio
047600150202         exsr NewPadre;
047700150205
047800150205       //?Recupero i dati da WADELPCT
047900150205         exsr Dati_Wadel;
048000150202
048100150202       ENDSR;
048200150202
048300150202       //--------------------------------------------------------------
048400150202       //?Cerco il nuovo padre.
048500150202       //--------------------------------------------------------------
048600150202       BEGSR NewPadre;
048700150202
048800150202       //?Controllo se ora è figlio
048900150202         clear TIBS10DS;
049000150202         D10tle = 'ST';
049100150202         D10paf = 'P';
049200150202         D10cod = CMCksu;
049300150202         TIBS10R (TIBS10DS);
049400150202       //?Se ritorna errore vuol dire che non è un codice figlio
049500150202         IF  D10err <> *blanks;
049600150202           leavesr;
049700150202         ENDIF;
049800150202       //?Se non c'è il codice padre vado via
049900150202         IF  D10cop = *zeros;
050000150202           leavesr;
050100150202         ENDIF;
050200150202
050300150202       //?Se arrivo qua vuol dire che è diventato figlio
050400150202         WCMCksunw = D10cop;
050500150202
050600150202       ENDSR;
050700150205
050800150205       //--------------------------------------------------------------
050900150205       //?Imposto i dati recuperati da WADELPCT.
051000150205       //--------------------------------------------------------------
051100150205       BEGSR Dati_Wadel;
051200150205
051300150209       //?Imposto il codice unificante nuovo, se presente, o da file campagna
051400150209         IF  WCMCksunw <> 0;
051500150209           wKsu = WCMCksunw;
051600150209         ELSE;
051700150209           wKsu = CMCksu;
051800150209         ENDIF;
051900150209
052000150205         exec sql
052100150205         select WADELPCT.* into :WADELDS
052200150205         from GAITRAGRU/WADELPCT
052300150209         where WAPcli = :wKsu;
052400150205         IF  sqlcod <> 0;
052500150205           leavesr;
052600150205         ENDIF;
052700150209
052800150209         clear wVal;
052900150209         clear wDel;
053000150209
053100150209       //?Imposto periodo di elaborazione WADELPCTe
053200150209         WCMCamdel = (WAPaaa * 100) + WAPmma;
053300150205
053400150205       //?Recupero i dati progressivi anno corrente
053500150205         WCMCpdea = WAPdac;
053600150205         WCMCnspa = WAPsac;
053700150205         WCMCrica = WAPfac;
053800150205         WCMCpmea = WAPkgc;
053900150205
054000150205       //?Recupero i dati del mese
054100150205         SELECT;
054200150209         WHEN  WAPmma = 01;
054300150205           WCMCnspm = WAPns_01ac;
054400150205           WCMCricm = WAPrr_01ac;
054500150205           WCMCpmem = WAPkg_01ac;
054600150209           wRpres = WAPrp_01ac;
054700150209         WHEN  WAPmma = 02;
054800150205           WCMCnspm = WAPns_02ac;
054900150205           WCMCricm = WAPrr_02ac;
055000150205           WCMCpmem = WAPkg_02ac;
055100150209           wRpres = WAPrp_02ac;
055200150209         WHEN  WAPmma = 03;
055300150205           WCMCnspm = WAPns_03ac;
055400150205           WCMCricm = WAPrr_03ac;
055500150205           WCMCpmem = WAPkg_03ac;
055600150209           wRpres = WAPrp_03ac;
055700150209         WHEN  WAPmma = 04;
055800150205           WCMCnspm = WAPns_04ac;
055900150205           WCMCricm = WAPrr_04ac;
056000150205           WCMCpmem = WAPkg_04ac;
056100150209           wRpres = WAPrp_04ac;
056200150209         WHEN  WAPmma = 05;
056300150205           WCMCnspm = WAPns_05ac;
056400150205           WCMCricm = WAPrr_05ac;
056500150205           WCMCpmem = WAPkg_05ac;
056600150209           wRpres = WAPrp_05ac;
056700150209         WHEN  WAPmma = 06;
056800150205           WCMCnspm = WAPns_06ac;
056900150205           WCMCricm = WAPrr_06ac;
057000150205           WCMCpmem = WAPkg_06ac;
057100150209           wRpres = WAPrp_06ac;
057200150209         WHEN  WAPmma = 07;
057300150205           WCMCnspm = WAPns_07ac;
057400150205           WCMCricm = WAPrr_07ac;
057500150205           WCMCpmem = WAPkg_07ac;
057600150209           wRpres = WAPrp_07ac;
057700150209         WHEN  WAPmma = 08;
057800150205           WCMCnspm = WAPns_08ac;
057900150205           WCMCricm = WAPrr_08ac;
058000150205           WCMCpmem = WAPkg_08ac;
058100150209           wRpres = WAPrp_08ac;
058200150209         WHEN  WAPmma = 09;
058300150205           WCMCnspm = WAPns_09ac;
058400150205           WCMCricm = WAPrr_09ac;
058500150205           WCMCpmem = WAPkg_09ac;
058600150209           wRpres = WAPrp_09ac;
058700150209         WHEN  WAPmma = 10;
058800150205           WCMCnspm = WAPns_10ac;
058900150205           WCMCricm = WAPrr_10ac;
059000150205           WCMCpmem = WAPkg_10ac;
059100150209           wRpres = WAPrp_10ac;
059200150209         WHEN  WAPmma = 11;
059300150205           WCMCnspm = WAPns_11ac;
059400150205           WCMCricm = WAPrr_11ac;
059500150205           WCMCpmem = WAPkg_11ac;
059600150209           wRpres = WAPrp_11ac;
059700150209         WHEN  WAPmma = 12;
059800150205           WCMCnspm = WAPns_12ac;
059900150205           WCMCricm = WAPrr_12ac;
060000150205           WCMCpmem = WAPkg_12ac;
060100150209           wRpres = WAPrp_12ac;
060200150205         ENDSL;
060300150209
060400150209       //?Calcolo il delta
060500150209       //?Ricavo Reale - Ricavo presunto * 100 / Ricavo Reale
060600150209         wRreale = WCMCricm;
060700150209         wVal = (wRreale - wRpres) * 100;
060800150209
060900150209         IF  wRreale = 0;
061000150209           wRreale = 1;
061100150209         ENDIF;
061200150209
061300150209         wVal = wVal / wRreale;
061400150209         eval(h) wDel = wVal;
061500150209
061600150209         SELECT;
061700150209         WHEN  wDel = 0;
061800150209         WHEN  wDel >= 1000;
061900150209           WCMCpdem = 999,9;
062000150209         WHEN  wDel <= -1000;
062100150209           WCMCpdem = -999,9;
062200150209         OTHER;
062300150209           WCMCpdem = wDel;
062400150209         ENDSL;
062500150209
062600150205       ENDSR;
062700150202
062800150202       //--------------------------------------------------------------
062900150202       //?Operazioni finali.
063000150202       //--------------------------------------------------------------
063100150202       BEGSR RoutEnd;
063200150202
063300150202         *inLR = *on;
063400150202         return;
063500150202
063600150202       ENDSR;
063700150202
063800150202      /end-free
063900150202
