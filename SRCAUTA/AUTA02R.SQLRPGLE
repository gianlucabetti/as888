000100141210      //--------------------------------------------------------------
000200141210      //?AUTA02R - Da WF aggiorno le tariffe per Aumento 2015
000300141210      //--------------------------------------------------------------
000400141210
000500141210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141210     h dftactgrp(*no) actgrp(*caller)
000700141210
000800141210      //---------------------------------------------------------------
000900141210      //?Dichiarazione file.
001000141210      //---------------------------------------------------------------
001100141210     fWFAUT01L  uf   e           k disk
001200141210
001300141210     fTNTAM04L  uf   e           k disk    rename(TNTAM000:TNTAM04)
001400141210     fTITAD04L  uf   e           k disk    rename(TITAD000:TITAD04)
001500141210     fTITPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001600141210     fTITPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
001700141210     fTITGC01L  uf   e           k disk    rename(TITGC000:TITGC01)
001800141210
001900141210     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
002000141210     f                                     prefix(old_)
002100141210
002200141210     fTNTAM00F  o    e             disk    prefix(new_)
002300141210     fTITAD00F  o    e             disk    prefix(new_)
002400141210     fTITPT00F  o    e             disk    prefix(new_)
002500141210     fTITPD00F  o    e             disk    prefix(new_)
002600141210     fTITGC00F  o    e             disk    prefix(new_)
002700141210
002800141210     fTITAV01L  if a e           k disk
002900111213
003000141210      //---------------------------------------------------------------
003100141210      //?Definizione costanti.
003200141210      //---------------------------------------------------------------
003300141210
003400141210      //---------------------------------------------------------------
003500141210      //?Definizione schiere.
003600141210      //---------------------------------------------------------------
003700141210
003800141210      //---------------------------------------------------------------
003900141210      //?Definizione aree dati.
004000141210      //---------------------------------------------------------------
004100141210
004200141210      // - Dati utente
004300141210     d §AzUte        e ds                  extname(AZUTE00F)
004400141210     d                                     dtaara
004500141210     d §DatiUte      e ds                  extname(dDatiUte)
004600141210     d                                     dtaara
004700141210
004800141210      //---------------------------------------------------------------
004900141210      //?Definizione strutture dati.
005000141210      //---------------------------------------------------------------
005100141210
005200141210      // - Parametri ricevuti
005300141210     d KPJBA         e ds
005400141210
005500141210      // - Reperimento dati utente
005600141210     d TIBS34DS      e ds
005700141210
005800141210      // - File WFAUT
005900141210     d WFAUTDS       e ds                  extname(WFAUT00F)
006000141210
006100141210      //---------------------------------------------------------------
006200141210      //?Definizione variabili globali.
006300141210      //---------------------------------------------------------------
006400141210
006500141210      // - Flags booleani
006600141210     d Fine            s               n   inz(*off)
006700141210     d wEoF            s               n   inz(*off)
006800141210
006900141210      // - Campi di comodo
007000141210     d Oggi            s              8s 0 inz
007100141210     d scadenza        s                   like(TAMdst) inz
007200141210     d wadd            s                   like(TAMpag) inz
007300141210     d wprog           s                   like(TAMprg) inz
007400141210
007500141210      //---------------------------------------------------------------
007600141210      //?Definizione Procedure usate.
007700141210      //---------------------------------------------------------------
007800141210
007900141210      //---------------------------------------------------------------
008000141210      //?Definizione Prototipi.
008100141210      //---------------------------------------------------------------
008200141210      /copy gaitrasrc/srcprotopr,TIBS34R
008300141210
008400141210      //---------------------------------------------------------------
008500141210      //?Definizione key-list.
008600141210      //---------------------------------------------------------------
008700141210
008800141210      //---------------------------------------------------------------
008900141210      //?M A I N - L I N E
009000141210      //---------------------------------------------------------------
009100141210
009200141210     c     *Entry        plist
009300141210     c                   parm                    kpjba
009400141210
009500141210      /free
009600141210
009700141210       //?Operazioni iniziali
009800141210       exsr RoutInz;
009900141210
010000141210       //?Elaborazione
010100141210       exsr Elabora;
010200141210
010300141210       //?Operazioni finali
010400141210       exsr RoutEnd;
010500141210
010600141210       //--------------------------------------------------------------
010700141210       //?Operazioni iniziali.
010800141210       //--------------------------------------------------------------
010900141210       BEGSR RoutInz;
011000141210
011100141210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011200141210
011300141210       //?Imposto oggi
011400141210         Oggi = %dec(%date());
011500141210
011600141210       //?Reperimento dati job
011700141210         exsr DatiJob;
011800141210
011900141210       ENDSR;
012000141210
012100141210       //--------------------------------------------------------------
012200141210       //?Reperimento Dati del job (Utente/Operativi).
012300141210       //--------------------------------------------------------------
012400141210       BEGSR DatiJob;
012500141210
012600141210         in(E) §AzUte;
012700141210         IF  not %error;
012800141210           in(E) §DatiUte;
012900141210         ENDIF;
013000141210         IF  %error or RSut = *blanks;
013100141210           clear TIBS34ds;
013200141210           tibs34r(tibs34ds);
013300141210           in §AzUte;
013400141210           in §DatiUte;
013500141210         ENDIF;
013600141210
013700141210       ENDSR;
013800141210
013900141210       //--------------------------------------------------------------
014000141210       //?Elaborazione.
014100141210       //--------------------------------------------------------------
014200141210       BEGSR Elabora;
014300141210
014400141210       //?Dichiarazione cursore
014500141210         exec sql
014600141210           declare WRK cursor for
014700141210           select * from WFAUT00F;
014800141210
014900141210         //?Apertura del cursore
015000141210         exec sql
015100141210           open WRK;
015200141210
015300141210         IF sqlcode < 0;
015400141210           exec sql close WRK;
015500141210           leavesr;
015600141210         ENDIF;
015700141210
015800141210         DOW  not wEoF;
015900141210           exec sql
016000141210             fetch next from WRK into :WFAUTDS;
016100141210           IF  sqlcod = 100 or sqlcod < 0;
016200141210             wEoF = *on;
016300141210             leave;
016400141210           ENDIF;
016500141210
016600141210         //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
016700141210           exsr Deleta_Annullati;
016800141210
016900141210         //?Se la tariffa che sto leggendo ha la scadenza nel 2015
017000141210         //?devo creare un nuovo progressivo da 01012015 a data scadenza tariffa
017100141210         //?e il progressivo che sto leggendo devo farlo scadere il 31122014
017200141210           IF  WFAdstv > 20141231;
017300141210             scadenza = WFAdstv;
017400141210           //?Aggiorno il progressivo attuale
017500141210             exsr Agg_Progressivo;
017600141210         //?Altrimenti il nuovo progressivo avrà scadenza 31122015
017700141210           ELSE;
017800141210             scadenza = 20151231;
017900141210           ENDIF;
018000141210
018100141210         //?Scrivo il nuovo progressivo
018200141210           exsr New_Progressivo;
018300141210
018400141210         ENDDO;
018500141210
018600141210         exec sql
018700141210           close WRK;
018800141210
018900141210       ENDSR;
019000141210
019100141210       //--------------------------------------------------------------
019200141210       //?Cancello tariffa annullata.
019300141210       //--------------------------------------------------------------
019400141210       BEGSR Deleta_Annullati;
019500141210
019600141210         wprog = WFAprgv + 1;
019700141210
019800141210         setll (WFAclf:WFActrv:wprog) TITAD04L;
019900141210         reade (WFAclf:WFActrv:wprog) TITAD04L;
020000141210         DOW  not %eof(TITAD04L);
020100141210           IF  TADatb <> *blanks;
020200141210             delete TITAD04;
020300141210           ENDIF;
020400141210           reade (WFAclf:WFActrv:wprog) TITAD04L;
020500141210         ENDDO;
020600141210
020700141210         setll (WFAclf:WFActrv:wprog) TITPT01L;
020800141210         reade (WFAclf:WFActrv:wprog) TITPT01L;
020900141210         DOW  not %eof(TITPT01L);
021000141210           IF  TPTatb <> *blanks;
021100141210             delete TITPT01;
021200141210           ENDIF;
021300141210           reade (WFAclf:WFActrv:wprog) TITPT01L;
021400141210         ENDDO;
021500141210
021600141210         setll (WFAclf:WFActrv:wprog) TITPD01L;
021700141210         reade (WFAclf:WFActrv:wprog) TITPD01L;
021800141210         DOW  not %eof(TITPD01L);
021900141210           IF  TPDatb <> *blanks;
022000141210             delete TITPD01;
022100141210           ENDIF;
022200141210           reade (WFAclf:WFActrv:wprog) TITPD01L;
022300141210         ENDDO;
022400141210
022500141210         setll (WFAclf:WFActrv:wprog) TITGC01L;
022600141210         reade (WFAclf:WFActrv:wprog) TITGC01L;
022700141210         DOW  not %eof(TITGC01L);
022800141210           IF  TGCatb <> *blanks;
022900141210             delete TITGC01;
023000141210           ENDIF;
023100141210           reade (WFAclf:WFActrv:wprog) TITGC01L;
023200141210         ENDDO;
023300141210
023400141210         chain (WFAclf:WFActrv:wprog) TNTAM01L;
023500141210         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
023600141210           delete TNTAM01;
023700141210         ENDIF;
023800141210
023900141210       ENDSR;
024000141210
024100141210       //--------------------------------------------------------------
024200141210       //?Scrivo il nuovo progressivo.
024300141210       //--------------------------------------------------------------
024400141210       BEGSR New_Progressivo;
024500141210
024600141210       //?Dettaglio tariffario
024700141210         setll (WFAclf:WFActrv:WFAprgv) TITAD04L;
024800141210         reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
024900141210         DOW  not %eof(TITAD04L);
025000141210           clear TITAD000;
025100141210           new_TADksc = TADksc;
025200141210           new_TADctr = TADctr;
025300141210           new_TADprg = TADprg + 1;
025400141210           new_TADlnp = TADlnp;
025500141210           new_TADcts = TADcts;
025600141210           new_TADnaz = TADnaz;
025700141210           new_TADcap = TADcap;
025800141210           new_TADsgl = TADsgl;
025900141210           new_TADitr = TADitr;
026000141210           new_TADino = TADino;
026100141210           new_TADrpv = TADrpv;
026200141210           new_TADorp = TADorp;
026300141210           new_TADmin = TADmin;
026400141210           new_TADmax = TADmax;
026500141210           new_TADdtr = oggi;
026600141210           write TITAD000;
026700141210           reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
026800141210         ENDDO;
026900141210
027000141210       //?Testata tariffe particolari
027100141210         setll (WFAclf:WFActrv:WFAprgv) TITPT01L;
027200141210         reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
027300141210         DOW  not %eof(TITPT01L);
027400141210           clear TITPT000;
027500141210           new_TPTksc = TPTksc;
027600141210           new_TPTctr = TPTctr;
027700141210           new_TPTprg = TPTprg + 1;
027800141210           new_TPTduv = oggi;
027900141210           new_TPTftc = TPTftc;
028000141210           new_TPTtpg = TPTtpg;
028100141210           new_TPTarl = TPTarl;
028200141210           new_TPTarf = TPTarf;
028300141210           new_TPTaro = TPTaro;
028400141210           new_TPTrpv = TPTrpv;
028500141210           new_TPTvlm = TPTvlm;
028600141210           new_TPTvvm = TPTvvm;
028700141210           new_TPTfvm = TPTfvm;
028800141210           new_TPTtma = TPTtma;
028900141210           new_TPTapl = TPTapl;
029000141210           new_TPTdtr = oggi;
029100141210           new_TPTdpb = TPTdpb;
029200141210           new_TPTdit = TPTdit;
029300141210           new_TPTflo = TPTflo;
029400141210           write TITPT000;
029500141210           reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
029600141210         ENDDO;
029700141210
029800141210       //?Dettaglio tariffe particolari
029900141210         setll (WFAclf:WFActrv:WFAprgv) TITPD01L;
030000141210         reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
030100141210         DOW  not %eof(TITPD01L);
030200141210           clear TITPD000;
030300141210           new_TPDksc = TPDksc;
030400141210           new_TPDctr = TPDctr;
030500141210           new_TPDprg = TPDprg + 1;
030600141210           new_TPDftc = TPDftc;
030700141210           new_TPDcts = TPDcts;
030800141210           new_TPDorp = TPDorp;
030900141210           new_TPDsgl = TPDsgl;
031000141210           new_TPDitr = TPDitr;
031100141210           new_TPDmin = TPDmin;
031200141210           new_TPDmax = TPDmax;
031300141210           new_TPDain = TPDain;
031400141210           new_TPDdtr = oggi;
031500141210           write TITPD000;
031600141210           reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
031700141210         ENDDO;
031800141210
031900141210       //?Tariffe di giacenza
032000141210         setll (WFAclf:WFActrv:WFAprgv) TITGC01L;
032100141210         reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
032200141210         DOW  not %eof(TITGC01L);
032300141210           clear TITGC000;
032400141210           new_TGCksc = TGCksc;
032500141210           new_TGCctr = TGCctr;
032600141210           new_TGCprg = TGCprg + 1;
032700141210           new_TGCsgv = TGCsgv;
032800141210           new_TGCsgr = TGCsgr;
032900141210           new_TGCsgp = TGCsgp;
033000141210           new_TGCsgd = TGCsgd;
033100141210           new_TGCsg1 = TGCsg1;
033200141210           new_TGCsg2 = TGCsg2;
033300141210           new_TGCsg3 = TGCsg3;
033400141210           new_TGCst1 = TGCst1;
033500141210           new_TGCst2 = TGCst2;
033600141210           new_TGCst3 = TGCst3;
033700141210           new_TGCstm = TGCstm;
033800141210           new_TGCrpv = TGCrpv;
033900141210           new_TGCfaf = TGCfaf;
034000141210           new_TGCsgf = TGCsgf;
034100141210           new_TGCggr = TGCggr;
034200141210           new_TGCtcm = TGCtcm;
034300141210           new_TGCtfg = TGCtfg;
034400141210           new_TGCduv = oggi;
034500141210           new_TGCdtr = oggi;
034600141210           write TITGC000;
034700141210           reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
034800141210         ENDDO;
034900141210
035000141210       //?Testata tariffe
035100141210         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
035200141210         IF  %found(TNTAM04L);
035300141210           clear TNTAM000;
035400141210           new_TAMksc = TAMksc;
035500141210           new_TAMctr = TAMctr;
035600141210           new_TAMprg = TAMprg + 1;
035700141210           new_TAMddt = 20150101;
035800141210           new_TAMdst = scadenza;
035900141210           new_TAMduv = oggi;
036000141210           new_TAMdcv = TAMdcv;
036100141210           new_TAMdif = TAMdif;
036200141210           new_TAMfli = TAMfli;
036300141210           new_TAMpri = TAMpri;
036400141210           new_TAMfmp = TAMfmp;
036500141210           new_TAMlrc = TAMlrc;
036600141210           new_TAMlas = TAMlas;
036700141210         //?Inglobo l'addizionale di gestione Anno Corrente nell'addizionale
036800141210         //?di gestione Anno Precedente
036900141210           wadd = TAMpag + TAMppa +
037000141210                    ((TAMpag * TAMppa) / 100);
037100141210           new_TAMpag = wadd;
037200141210           new_TAMmpa = TAMmpa;
037300141210           new_TAMpam = TAMpam;
037400141210           new_TAMmpm = TAMmpm;
037500141210           new_TAMtin = TAMtin;
037600141210           new_TAMkpm = TAMkpm;
037700141210           new_TAMarl = TAMarl;
037800141210           new_TAMarf = TAMarf;
037900141210           new_TAMaro = TAMaro;
038000141210           new_TAMrat = TAMrat;
038100141210           new_TAMrct = TAMrct;
038200141210           new_TAMpri = TAMpri;
038300141210           new_TAMsc2 = TAMsc2;
038400141210           new_TAMeds = TAMeds;
038500141210           new_TAMars = TAMars;
038600141210           new_TAMata = TAMata;
038700141210           new_TAMftp = TAMftp;
038800141210           new_TAMbap = TAMbap;
038900141210           new_TAMtsp = TAMtsp;
039000141210           new_TAMfie = TAMfie;
039100141210           new_TAMfis = TAMfis;
039200141210           new_TAMsis = TAMsis;
039300141210           new_TAMfvd = TAMfvd;
039400141210           new_TAMfts = TAMfts;
039500141210           new_TAMnas = TAMnas;
039600141210           new_TAMnot = TAMnot;
039700141210           new_TAMftm = TAMftm;
039800141210           new_TAMgca = TAMgca;
039900141210           new_TAMgga = TAMgga;
040000141210           new_TAMgma = TAMgma;
040100141210           new_TAMgva = TAMgva;
040200141210           new_TAMbam = TAMbam;
040300141210           new_TAMtpr = TAMtpr;
040400141210           new_TAMflo = TAMflo;
040500141210           new_TAMdat = TAMdat;
040600141210           new_TAMdtr = oggi;
040700141210           write TNTAM000;
040800141210         ENDIF;
040900141210
041000141210       //?Storico variazioni
041100141210         clear TITAV000;
041200141210         TAVksc = new_TAMksc;
041300141210         TAVctr = new_TAMctr;
041400141210         TAVprg = new_TAMprg;
041500141210         TAVtrd = 'TES';
041600141210         TAVcta = 'D';
041700141210         TAVdav = oggi;
041800141210         TAVorv = %dec(%time());
041900141210         TAVnoj = 'AUMENTO';
042000141210         TAVpru = 'BATCH';
042100141210         TAVdtr = oggi;
042200141210         write TITAV000;
042300141210
042400141210       //?File di WORK
042500141210         chain (' ':WFAclf:WFActrv:WFAprgv) WFAUT01L;
042600141210         IF  %found(WFAUT01L);
042700141210           WFAddtn = new_TAMddt;
042800141210           WFAdstn = new_TAMdst;
042900141210           WFAprgn = new_TAMprg;
043000141210           WFApagn = new_TAMpag;
043100141210           WFAppan = new_TAMppa;
043200141210           WFAela = 'S';
043300141210           update WFAUT000;
043400141210         ENDIF;
043500141210
043600141210       ENDSR;
043700141210
043800141210       //--------------------------------------------------------------
043900141210       //?Aggiorno il progressivo attuale.
044000141210       //--------------------------------------------------------------
044100141210       BEGSR Agg_Progressivo;
044200141210
044300141210       //?Testata tariffe
044400141210         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
044500141210         IF  %found(TNTAM04L);
044600141210           TAMdst = 20141231;
044700141210           TAMduv = oggi;
044800141210           TAMdtr = oggi;
044900141210           clear TAMftr;
045000141212           update TNTAM04;
045100141210         ENDIF;
045200141210
045300141210       //?Storico variazioni
045400141210         clear TITAV000;
045500141210         TAVksc = TAMksc;
045600141210         TAVctr = TAMctr;
045700141210         TAVprg = TAMprg;
045800141210         TAVtrd = 'TES';
045900141210         TAVcta = 'D';
046000141210         TAVdav = oggi;
046100141210         TAVorv = %dec(%time());
046200141210         TAVnoj = 'AUMENTO';
046300141210         TAVpru = 'BATCH';
046400141210         TAVdtr = oggi;
046500141210         write TITAV000;
046600141210
046700141210       ENDSR;
046800141210
046900141210       //--------------------------------------------------------------
047000141210       //?Operazioni finali.
047100141210       //--------------------------------------------------------------
047200141210       BEGSR RoutEnd;
047300141210
047400141210         *inLR = *on;
047500141210         return;
047600141210
047700141210       ENDSR;
047800141210
047900141210      /end-free
