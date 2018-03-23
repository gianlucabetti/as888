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
008100170309
008200170309      // - Calcola Saldi cliente  ?
008300170309     d TISE70DS      e ds                  inz
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
009900150202
010000150202      // - Tabella 05 - Aree
010100150202     d ds05          e ds
010200150202
010300150202      // - Tabella 17 - Distretti
010400150202     d ds17          e ds
010500170308
010600170308      // - Tabella 5S - Statistiche
010700170308     d ds5S          e ds
010800150202
010900150202      //---------------------------------------------------------------
011000150202      //?Definizione variabili globali.
011100150202      //---------------------------------------------------------------
011200150202
011300150202      // - Flags booleani
011400150202     d Fine            s               n   inz(*off)
011500150202     d wEnd            s               n   inz(*off)
011600150202
011700150202      // - Campi di comodo data
011800150205     d Anno            s              4s 0 inz
011900150202     d Data_EUR        s               d   datfmt(*eur)
012000150202     d Data_ISO        s               d   datfmt(*iso)
012100150205     d Mese            s              2s 0 inz
012200170308     d wanno           s              4s 0 inz
012300170308     d wData           s              8s 0 inz
012400170308     d wmese           s              2s 0 inz
012500150202
012600150202      // - Campi di comodo
012700150202     d comando         s            512a   varying
012800150202     d Oggi            s              8s 0 inz
012900150227     d wDel            s              8s 1 inz
013000150209     d wKsu            s                   like(CMCksu) inz
013100150202     d wNrv            s                   like(CMFnrv) inz
013200150209     d wRpres          s             20s 3 inz
013300150209     d wRreale         s             20s 3 inz
013400150203     d wVal            s             20s 3 inz
013500150202
013600150202      //---------------------------------------------------------------
013700150202      //?Definizione procedure usate.
013800150202      //---------------------------------------------------------------
013900170309      // - Calcola saldi cliente
014000170309     d TISE70R         pr                  extpgm('TISE70R')
014100170309     d  tise70ds                           likeds(TISE70DS)
014200150202
014300150202      //---------------------------------------------------------------
014400150202      //?Definizione Prototipi.
014500150202      //---------------------------------------------------------------
014600150202      /copy gaitrasrc/srcprotopr,TIBS10R
014700150202      /copy gaitrasrc/srcprotopr,TIBS34R
014800150202      /copy gaitrasrc/srcprotopr,TIBS69R
014900150202      /copy gaitrasrc/srcprotopr,SYSTEM
015000150202
015100150202      //---------------------------------------------------------------
015200150202      //?Definizione key-list.
015300150202      //---------------------------------------------------------------
015400150202
015500150202       // - File TABEL00F
015600150202     d k03tabel      e ds                  extname(TABEL00F:*key)
015700150202     d                                     prefix(k_)
015800150202
015900150202      //---------------------------------------------------------------
016000150202      //?M A I N - L I N E
016100150202      //---------------------------------------------------------------
016200150202
016300150202     c     *Entry        plist
016400150202     c                   parm                    kpjba
016500150202
016600150202      /free
016700150202
016800150202       //?Operazioni iniziali
016900150202       exsr RoutInz;
017000150202
017100150202       //?Elabora
017200150202       exsr Elabora;
017300150202
017400150202       //?Operazioni finali
017500150202       exsr RoutEnd;
017600150202
017700150202       //--------------------------------------------------------------
017800150202       //?Operazioni iniziali.
017900150202       //--------------------------------------------------------------
018000150202       BEGSR RoutInz;
018100150202
018200150202         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018300150202
018400150202       //?Dati ricevuti
018500150202         TRKC51DS = kpjbu;
018600150202
018700150202       //?Imposto oggi
018800150202         Oggi = %dec(%date());
018900150205
019000150205       //?Imposto Anno e Mese elaborazione
019100150306         Data_ISO = %date(Oggi);
019200150306         Anno     = %subdt(Data_ISO:*years);
019300150306         Mese     = %subdt(Data_ISO:*months);
019400150202
019500150202       //?Reperimento dati job
019600150202         exsr DatiJob;
019700150202
019800150202       //?Pulisco il file di work
019900150202         comando = 'CLRPFM FILE(GAITRAAZM/WFCMC00F)';
020000150202         IF  ExecuteCommand (comando) > 0 ;
020100150202           exsr RoutEnd;
020200150202         ENDIF;
020300150202
020400150202       //?Apro il file di work
020500150202         open WFCMC00F;
020600150203
020700150203       //?Impostazione campi "fissi"
020800150203         k_TBLkut = 1;
020900170308
021000170308       //?Dalla tabella "5S" recupero le date aggiornamento saldi
021100170308         k_TBLkut = 1;
021200170308         k_TBLcod = '5S';
021300170308         clear k_TBLkey;
021400170308         clear ds5S;
021500170308         chain %kds(K03tabel) TABEL00F;
021600170308         IF  %found(TABEL00F) and TBLflg = *blanks;
021700170308           ds5S = TBLuni;
021800170308         ENDIF;
021900170308
022000170308       //?Imposto Anno e Mese per calcolo saldi clienti
022100170308         Data_ISO = %date(§5Sdac);
022200170308       //?Devo elaborare il mese precedente alla data ultimo aggiornamento saldi
022300170308         wdata = %dec(Data_ISO - %months(1));
022400170308         Data_ISO = %date(wdata);
022500170308         wanno = %subdt(Data_ISO:*years);
022600170308         wmese = %subdt(Data_ISO:*months);
022700150202
022800150202       ENDSR;
022900150202
023000150202       //--------------------------------------------------------------
023100150202       //?Reperimento Dati del job (Utente/Operativi).
023200150202       //--------------------------------------------------------------
023300150202       BEGSR DatiJob;
023400150202
023500150202         in(E) §AzUte;
023600150202         IF  NOT %error;
023700150202           in(E) §DatiUte;
023800150202         ENDIF;
023900150202         IF  %error or RSut = *blanks;
024000150202           clear TIBS34ds;
024100150202           tibs34r(tibs34ds);
024200150202           in §AzUte;
024300150202           in §DatiUte;
024400150202         ENDIF;
024500150202
024600150202       ENDSR;
024700150202
024800150202       //--------------------------------------------------------------
024900150202       //?Elabora.
025000150202       //--------------------------------------------------------------
025100150202       BEGSR Elabora;
025200150202
025300150202       //?Leggo i clienti che sono in campagna
025400150202         exec sql
025500150202         declare WRK cursor for
025600150203         select *
025700150202         from TICMC00F join TICMI00F on
025800150202         CMCncm = CMIncm and CMCksu = CMIksu and
025900150202         CMCksc = CMIksc and CMCcpo = CMIcpo
026000150202         where CMCncm = :IKC51ncm;
026100150202
026200150202         //?Apertura del cursore
026300150202         exec sql open WRK;
026400150202
026500150202         IF sqlcode < 0;
026600150202           leavesr;
026700150202         ENDIF;
026800150202
026900150202         DOW  not wEnd;
027000150202           exec sql
027100150202           fetch next from WRK into :TICMC00F, :TICMI00F;
027200150202           IF sqlcod = 100 or sqlcod < 0;
027300150202             wEnd = *on;
027400150202             leave;
027500150202           ENDIF;
027600150202
027700150202         //?Imposto i dati sul file di work
027800150202           exsr ImpostaDati;
027900150202
028000150202         //?Scrivo file di work
028100150202           write WFCMC000;
028200150202
028300150202         ENDDO;
028400150202
028500150202         exec sql close WRK;
028600150202
028700150202       ENDSR;
028800150202
028900150202       //--------------------------------------------------------------
029000150202       //?Imposto dati sul file di work.
029100150202       //--------------------------------------------------------------
029200150202       BEGSR ImpostaDati;
029300150202
029400150202         clear wNrv;
029500150202
029600150202       //?Pulisco record
029700150202         clear  WFCMC000;
029800150202
029900150202       //?Data/Ora/Utente elaborazione
030000150202         WCMCdae = %dec(%date());
030100150202         WCMCore = %dec(%time());
030200150202         WCMCute = knmus;
030300150202
030400150202       //?Campagna
030500150202         WCMCncm = IKC51ncm;
030600150202         chain WCMCncm TICMP01L;
030700150202         IF  %found(TICMP01L);
030800150202           WCMCncmd = CMPdes;
030900150202         ENDIF;
031000150202
031100150202       //?Cliente
031200150202         WCMCksu = CMCksu;
031300150202         clear TIBS69DS;
031400150202         I69kac = CMCksu;
031500150202         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
031600150202         WCMCrag = ACOrag;
031700150209
031800150209       //?Ultima fase
031900150209         SELECT;
032000150209         WHEN  CMCufe = FaseObj;
032100150209           WCMCuob = 'I';
032200150209         WHEN  CMCufe = FaseObjProp;
032300150209           WCMCuob = 'P';
032400150209         WHEN  CMCufe = FaseObjFine;
032500150209           WCMCuob = 'F';
032600150209         WHEN  CMCufe = FaseChiudi;
032700150209           WCMCcch = CMCcch;
032800150209           WCMCdte = CMCdch;
032900150209         ENDSL;
033000150202
033100150202       //?Dati da TICMI
033200150202         WCMCclv = CMIclv;
033300150202         WCMCcmm = CMIcmm;
033400150202         WCMCdis = CMIdcm;
033500150202         WCMCcar = CMIacm;
033600150202         WCMCfil = CMIfcm;
033700150202         WCMCpde = CMIpde;
033800150202         WCMCnsp = CMInsp;
033900150202         WCMCric = CMIric;
034000150202         WCMCpme = CMIpme;
034100150202         WCMCist = CMIist;
034200150202         WCMCcln = CMIcln;
034300150202         WCMCmes = CMImes;
034400150202
034500150202       //?Decodifico Commerciale
034600150202         chain WCMCcmm AZCMM01L;
034700150202         IF  %found(AZCMM01L);
034800150202           WCMCcmmd = CMMdes;
034900150202         ENDIF;
035000150202
035100150202       //?Decodifico Distretto/Area/Filiale
035200150202         clear ds17;
035300150202         k_tblcod = '17';
035400150202         k_tblkey = WCMCdis;
035500150202         chain  %kds(K03tabel) TABEL00F;
035600150202         IF  %found(TABEL00F);
035700150202           ds17 = TBLuni;
035800150202         ENDIF;
035900150202         WCMCdisd = §17des;
036000150202
036100150202         clear ds05;
036200150202         k_tblcod = '05';
036300150202         k_tblkey = %editc(WCMCcar:'X');
036400150202         chain  %kds(K03tabel) TABEL00F;
036500150202         IF  %found(TABEL00F);
036600150202           ds05 = TBLuni;
036700150202         ENDIF;
036800150202         WCMCcard = §05des;
036900150202
037000150202         chain WCMCfil AZORG01L;
037100150202         IF  %found(AZORG01L) and ORGfva = *blanks;
037200150202           WCMCfild = ORGdes;
037300150202         ENDIF;
037400150202
037500150202       //?Imposto i campi da TICMF
037600150202       //?Obiettivo inizio
037700150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObj) TICMF01L;
037800150202         IF  %found(TICMF01L);
037900150202           WCMCobi = CMFpea;
038000150202         ENDIF;
038100150202       //?Obiettivo Proposto
038200150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjProp) TICMF01L;
038300150202         IF  %found(TICMF01L);
038400150202           WCMCobp = CMFpea;
038500150202         ENDIF;
038600150202       //?Obiettivo Finale
038700150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjFine) TICMF01L;
038800150202         IF  %found(TICMF01L);
038900150202           WCMCobf = CMFpea;
039000150202         ENDIF;
039100150202       //?Aumento da Trattativa  + trattativa SI/NO  + Nr.Trattativa
039200150302       //?Prima provo con fase TR
039300150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTtr) TICMF01L;
039400150202         IF  %found(TICMF01L);
039500150202           WCMCobt = CMFpea;
039600150202           WCMCdtt = CMFdfa;
039700150202           wNrv = CMFnrv;
039800150302         ELSE;
039900150302       //?Se non c'è la fase TR provo con la TF
040000150302           chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjTf) TICMF01L;
040100150302           IF  %found(TICMF01L);
040200150302             WCMCobt = CMFpea;
040300150302             WCMCdtt = CMFdfa;
040400150302             clear wNrv;
040500150302             WCMCesi = 'P';
040600150302           ENDIF;
040700150302         ENDIF;
040800150202       //?% Confronto Fatturazione
040900150202         chain (CMIncm:CMIksu:CMIksc:CMIcpo:FaseObjCf) TICMF01L;
041000150202         IF  %found(TICMF01L);
041100150202           WCMCobc = CMFpea;
041200150202           WCMCdtc = CMFdfa;
041300150202           WCMCamcf = (CMFaacf * 100) + CMFmmcf;
041400150202           WCMCnocf = CMFnocf;
041500150202           WCMCksucf = CMFnksu;
041600150202         ENDIF;
041700150202
041800150202       //?Calcolo i valori
041900150203         WCMCvobi = (WCMCric * WCMCobi) / 100;
042000150203         WCMCvobp = (WCMCric * WCMCobp) / 100;
042100150203         WCMCvobf = (WCMCric * WCMCobf) / 100;
042200150203         WCMCvobt = (WCMCric * WCMCobt) / 100;
042300150203         WCMCvobc = (WCMCric * WCMCobc) / 100;
042400150202
042500150202       //?Cerco i dati relativi alla trattativa
042600150302         IF  wNrv > 0;
042700150202         chain wNrv TIVIS05L;
042800150202         IF  %found(TIVIS05L);
042900150202           WCMCdat = VISdat;
043000150202           WCMCdch = VISdch;
043100150202           SELECT;
043200150202           WHEN  VISesi = *blanks;
043300150202           WHEN  VISesi <> 'P ';
043400150202             WCMCesi = 'N';
043500150202           OTHER;
043600150202             WCMCesi = 'P';
043700150202           ENDSL;
043800150202         ENDIF;
043900150202
044000150202       //?Imposto se effettuato appuntamento
044100150202         exec sql
044200150202         select TIATC00F.* into :TIATC00F
044300150202         from TIATC00F
044400150202         where ATCksc = :CMCksu and ATCcad = '91' and
044500150202         ATCtat = 'A' and ATCest = 'S' and
044600150202         ATCnrv = :wNrv;
044700150202         IF  sqlcod = 0;
044800150202           WCMCapp = 'S';
044900150202         ENDIF;
045000150202
045100150202       //?Imposto se presente almeno 1 offerta
045200150202         setll (wNrv) TIVOF11L;
045300150202         reade (wNrv) TIVOF11L;
045400150202         DOW not %eof(TIVOF11L);
045500150202       //?se trovo anche solo 1 offerta valida
045600150202       //?è un AVVIATO CON OFFERTA
045700150202           IF  VOFeso <> 'H' and VOFeso <> '*';
045800150202             WCMCoff = 'S';
045900150202             leave;
046000150202           ENDIF;
046100150202           reade (wNrv) TIVOF11L;
046200150202         ENDDO;
046300150302         ENDIF;
046400150202
046500150202       //?Controllo se nel frattempo è diventato figlio
046600150202         exsr NewPadre;
046700150205
046800170308       //?Dati Delta
046900170308         exsr Dati_Delta;
047000150202
047100150202       ENDSR;
047200150202
047300150202       //--------------------------------------------------------------
047400150202       //?Cerco il nuovo padre.
047500150202       //--------------------------------------------------------------
047600150202       BEGSR NewPadre;
047700150202
047800150202       //?Controllo se ora è figlio
047900150202         clear TIBS10DS;
048000150202         D10tle = 'ST';
048100150202         D10paf = 'P';
048200150202         D10cod = CMCksu;
048300150202         TIBS10R (TIBS10DS);
048400150202       //?Se ritorna errore vuol dire che non è un codice figlio
048500150202         IF  D10err <> *blanks;
048600150202           leavesr;
048700150202         ENDIF;
048800150202       //?Se non c'è il codice padre vado via
048900150202         IF  D10cop = *zeros;
049000150202           leavesr;
049100150202         ENDIF;
049200150202
049300150202       //?Se arrivo qua vuol dire che è diventato figlio
049400150202         WCMCksunw = D10cop;
049500150202
049600150202       ENDSR;
049700150205
049800150205       //--------------------------------------------------------------
049900170308       //?Imposto i dati relativi al Delta.
050000150205       //--------------------------------------------------------------
050100170308       BEGSR Dati_Delta;
050200150205
050300150209       //?Imposto il codice unificante nuovo, se presente, o da file campagna
050400150209         IF  WCMCksunw <> 0;
050500150209           wKsu = WCMCksunw;
050600150209         ELSE;
050700150209           wKsu = CMCksu;
050800150209         ENDIF;
050900150209
051000150209         clear wVal;
051100150209         clear wDel;
051200150209
051300170308       //?Imposto periodo di elaborazione statistica x recupero delta
051400170308         WCMCamdel = (wanno * 100) + wmese;
051500170308
051600170309       //?Cerco i dati relativi al mese e calcolo il delta
051700170309         clear TISE70DS;
051800170309         I70tla = 'U';
051900170309         I70tle = 'ST';
052000170309         I70ksc = CMCksu;
052100170309         I70dai = wanno * 100 + wmese;
052200170309         I70daf = I70dai;
052300170309         tise70r (TISE70DS);
052400170309         WCMCnspm = O70nsp;
052500170309         WCMCricm = O70ric;
052600170309         WCMCpmem = O70pkg;
052700170309
052800170309       //?Calcolo il delta
052900170309       //?Ricavo Reale - Ricavo presunto * 100 / Ricavo Reale
053000170309         wVal = (O70rir - O70rip) * 100;
053100170309         IF  O70rir = 0;
053200170309           O70rir = 1;
053300170309         ENDIF;
053400170309         wVal = wVal / O70rir;
053500170309         eval(h) wDel = wVal;
053600170309         SELECT;
053700170309         WHEN  wDel = 0;
053800170309         WHEN  wDel >= 1000;
053900170309           WCMCpdem = 999,9;
054000170309         WHEN  wDel <= -1000;
054100170309           WCMCpdem = -999,9;
054200170309         OTHER;
054300170309           WCMCpdem = wDel;
054400170309         ENDSL;
054500170309
054600170309       //?Cerco i dati relativi all'anno corrente e calcolo il delta
054700170309         clear TISE70DS;
054800170309         I70tla = 'U';
054900170309         I70tle = 'ST';
055000170309         I70ksc = CMCksu;
055100170309         I70dai = wanno * 100 + 01;
055200170309         I70daf = wanno * 100 + wmese;
055300170309         tise70r (TISE70DS);
055400170309         WCMCnspa = O70nsp;
055500170309         WCMCrica = O70ric;
055600170309         WCMCpmea = O70pkg;
055700150209
055800150209       //?Calcolo il delta
055900150209       //?Ricavo Reale - Ricavo presunto * 100 / Ricavo Reale
056000170309         wVal = (O70rir - O70rip) * 100;
056100170309         IF  O70rir = 0;
056200170309           O70rir = 1;
056300150209         ENDIF;
056400170309         wVal = wVal / O70rir;
056500150209         eval(h) wDel = wVal;
056600150209         SELECT;
056700150209         WHEN  wDel = 0;
056800150209         WHEN  wDel >= 1000;
056900170309           WCMCpdea = 999,9;
057000150209         WHEN  wDel <= -1000;
057100170309           WCMCpdea = -999,9;
057200150209         OTHER;
057300170309           WCMCpdea = wDel;
057400150209         ENDSL;
057500150209
057600150205       ENDSR;
057700150202
057800150202       //--------------------------------------------------------------
057900150202       //?Operazioni finali.
058000150202       //--------------------------------------------------------------
058100150202       BEGSR RoutEnd;
058200150202
058300150202         *inLR = *on;
058400150202         return;
058500150202
058600150202       ENDSR;
058700150202
058800150202      /end-free
058900150202
