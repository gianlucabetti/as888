000100141210      //--------------------------------------------------------------
000200150130      //?AUTA04R - Da WF aggiorno le tariffe per Aumento da Lettera
000300141210      //--------------------------------------------------------------
000400141210
000500141210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141210     h dftactgrp(*no) actgrp(*caller)
000700141210
000800141210      //---------------------------------------------------------------
000900141210      //?Dichiarazione file.
001000141210      //---------------------------------------------------------------
001100141210     fTNTAM04L  uf   e           k disk    rename(TNTAM000:TNTAM04)
001200141210     fTITAD04L  uf   e           k disk    rename(TITAD000:TITAD04)
001300141210     fTITPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001400141210     fTITPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
001500141210     fTITGC01L  uf   e           k disk    rename(TITGC000:TITGC01)
001600141210
001700141210     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
001800141210     f                                     prefix(old_)
001900141210
002000141210     fTNTAM00F  o    e             disk    prefix(new_)
002100141210     fTITAD00F  o    e             disk    prefix(new_)
002200141210     fTITPT00F  o    e             disk    prefix(new_)
002300141210     fTITPD00F  o    e             disk    prefix(new_)
002400141210     fTITGC00F  o    e             disk    prefix(new_)
002500141210
002600141210     fTITAV01L  if a e           k disk
002700111213
002800141210      //---------------------------------------------------------------
002900141210      //?Definizione costanti.
003000141210      //---------------------------------------------------------------
003100141210
003200141210      //---------------------------------------------------------------
003300141210      //?Definizione schiere.
003400141210      //---------------------------------------------------------------
003500141210
003600141210      //---------------------------------------------------------------
003700141210      //?Definizione aree dati.
003800141210      //---------------------------------------------------------------
003900141210
004000141210      // - Dati utente
004100141210     d §AzUte        e ds                  extname(AZUTE00F)
004200141210     d                                     dtaara
004300141210     d §DatiUte      e ds                  extname(dDatiUte)
004400141210     d                                     dtaara
004500141210
004600141210      //---------------------------------------------------------------
004700141210      //?Definizione strutture dati.
004800141210      //---------------------------------------------------------------
004900141210
005000141210      // - Parametri ricevuti
005100141210     d KPJBA         e ds
005200141210
005300141210      // - Reperimento dati utente
005400141210     d TIBS34DS      e ds
005500141210
005600150130      // - File WFAUC
005700150130     d WFAUC00F      e ds                  extname(WFAUC00F)
005800141210
005900141210      //---------------------------------------------------------------
006000141210      //?Definizione variabili globali.
006100141210      //---------------------------------------------------------------
006200141210
006300141210      // - Flags booleani
006400141210     d Fine            s               n   inz(*off)
006500141210     d wEoF            s               n   inz(*off)
006600141210
006700141210      // - Campi di comodo
006800141210     d Oggi            s              8s 0 inz
006900141210     d scadenza        s                   like(TAMdst) inz
007000141210     d wadd            s                   like(TAMpag) inz
007100141210     d wprog           s                   like(TAMprg) inz
007200141210
007300141210      //---------------------------------------------------------------
007400141210      //?Definizione Procedure usate.
007500141210      //---------------------------------------------------------------
007600141210
007700141210      //---------------------------------------------------------------
007800141210      //?Definizione Prototipi.
007900141210      //---------------------------------------------------------------
008000141210      /copy gaitrasrc/srcprotopr,TIBS34R
008100141210
008200141210      //---------------------------------------------------------------
008300141210      //?Definizione key-list.
008400141210      //---------------------------------------------------------------
008500141210
008600141210      //---------------------------------------------------------------
008700141210      //?M A I N - L I N E
008800141210      //---------------------------------------------------------------
008900141210
009000141210     c     *Entry        plist
009100141210     c                   parm                    kpjba
009200141210
009300141210      /free
009400141210
009500141210       //?Operazioni iniziali
009600141210       exsr RoutInz;
009700141210
009800141210       //?Elaborazione
009900141210       exsr Elabora;
010000141210
010100141210       //?Operazioni finali
010200141210       exsr RoutEnd;
010300141210
010400141210       //--------------------------------------------------------------
010500141210       //?Operazioni iniziali.
010600141210       //--------------------------------------------------------------
010700141210       BEGSR RoutInz;
010800141210
010900141210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011000141210
011100141210       //?Imposto oggi
011200141210         Oggi = %dec(%date());
011300141210
011400141210       //?Reperimento dati job
011500141210         exsr DatiJob;
011600141210
011700141210       ENDSR;
011800141210
011900141210       //--------------------------------------------------------------
012000141210       //?Reperimento Dati del job (Utente/Operativi).
012100141210       //--------------------------------------------------------------
012200141210       BEGSR DatiJob;
012300141210
012400141210         in(E) §AzUte;
012500141210         IF  not %error;
012600141210           in(E) §DatiUte;
012700141210         ENDIF;
012800141210         IF  %error or RSut = *blanks;
012900141210           clear TIBS34ds;
013000141210           tibs34r(tibs34ds);
013100141210           in §AzUte;
013200141210           in §DatiUte;
013300141210         ENDIF;
013400141210
013500141210       ENDSR;
013600141210
013700141210       //--------------------------------------------------------------
013800141210       //?Elaborazione.
013900141210       //--------------------------------------------------------------
014000141210       BEGSR Elabora;
014100141210
014200141210       //?Dichiarazione cursore
014300141210         exec sql
014400141210           declare WRK cursor for
014500150130           select * from WFAUC00F;
014600141210
014700141210         //?Apertura del cursore
014800141210         exec sql
014900141210           open WRK;
015000141210
015100141210         IF sqlcode < 0;
015200141210           exec sql close WRK;
015300141210           leavesr;
015400141210         ENDIF;
015500141210
015600141210         DOW  not wEoF;
015700141210           exec sql
015800150130             fetch next from WRK into :WFAUC00F;
015900141210           IF  sqlcod = 100 or sqlcod < 0;
016000141210             wEoF = *on;
016100141210             leave;
016200141210           ENDIF;
016300141210
016400141210         //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
016500141210           exsr Deleta_Annullati;
016600141210
016700141210         //?Se la tariffa che sto leggendo ha la scadenza nel 2015
016800141210         //?devo creare un nuovo progressivo da 01012015 a data scadenza tariffa
016900141210         //?e il progressivo che sto leggendo devo farlo scadere il 31122014
017000141210           IF  WFAdstv > 20141231;
017100141210             scadenza = WFAdstv;
017200141210           //?Aggiorno il progressivo attuale
017300141210             exsr Agg_Progressivo;
017400141210         //?Altrimenti il nuovo progressivo avrà scadenza 31122015
017500141210           ELSE;
017600141210             scadenza = 20151231;
017700141210           ENDIF;
017800141210
017900141210         //?Scrivo il nuovo progressivo
018000141210           exsr New_Progressivo;
018100141210
018200141210         ENDDO;
018300141210
018400141210         exec sql
018500141210           close WRK;
018600141210
018700141210       ENDSR;
018800141210
018900141210       //--------------------------------------------------------------
019000141210       //?Cancello tariffa annullata.
019100141210       //--------------------------------------------------------------
019200141210       BEGSR Deleta_Annullati;
019300141210
019400141210         wprog = WFAprgv + 1;
019500141210
019600141210         setll (WFAclf:WFActrv:wprog) TITAD04L;
019700141210         reade (WFAclf:WFActrv:wprog) TITAD04L;
019800141210         DOW  not %eof(TITAD04L);
019900141210           IF  TADatb <> *blanks;
020000141210             delete TITAD04;
020100141210           ENDIF;
020200141210           reade (WFAclf:WFActrv:wprog) TITAD04L;
020300141210         ENDDO;
020400141210
020500141210         setll (WFAclf:WFActrv:wprog) TITPT01L;
020600141210         reade (WFAclf:WFActrv:wprog) TITPT01L;
020700141210         DOW  not %eof(TITPT01L);
020800141210           IF  TPTatb <> *blanks;
020900141210             delete TITPT01;
021000141210           ENDIF;
021100141210           reade (WFAclf:WFActrv:wprog) TITPT01L;
021200141210         ENDDO;
021300141210
021400141210         setll (WFAclf:WFActrv:wprog) TITPD01L;
021500141210         reade (WFAclf:WFActrv:wprog) TITPD01L;
021600141210         DOW  not %eof(TITPD01L);
021700141210           IF  TPDatb <> *blanks;
021800141210             delete TITPD01;
021900141210           ENDIF;
022000141210           reade (WFAclf:WFActrv:wprog) TITPD01L;
022100141210         ENDDO;
022200141210
022300141210         setll (WFAclf:WFActrv:wprog) TITGC01L;
022400141210         reade (WFAclf:WFActrv:wprog) TITGC01L;
022500141210         DOW  not %eof(TITGC01L);
022600141210           IF  TGCatb <> *blanks;
022700141210             delete TITGC01;
022800141210           ENDIF;
022900141210           reade (WFAclf:WFActrv:wprog) TITGC01L;
023000141210         ENDDO;
023100141210
023200141210         chain (WFAclf:WFActrv:wprog) TNTAM01L;
023300141210         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
023400141210           delete TNTAM01;
023500141210         ENDIF;
023600141210
023700141210       ENDSR;
023800141210
023900141210       //--------------------------------------------------------------
024000141210       //?Scrivo il nuovo progressivo.
024100141210       //--------------------------------------------------------------
024200141210       BEGSR New_Progressivo;
024300141210
024400141210       //?Dettaglio tariffario
024500141210         setll (WFAclf:WFActrv:WFAprgv) TITAD04L;
024600141210         reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
024700141210         DOW  not %eof(TITAD04L);
024800141210           clear TITAD000;
024900141210           new_TADksc = TADksc;
025000141210           new_TADctr = TADctr;
025100141210           new_TADprg = TADprg + 1;
025200141210           new_TADlnp = TADlnp;
025300141210           new_TADcts = TADcts;
025400141210           new_TADnaz = TADnaz;
025500141210           new_TADcap = TADcap;
025600141210           new_TADsgl = TADsgl;
025700141210           new_TADitr = TADitr;
025800141210           new_TADino = TADino;
025900141210           new_TADrpv = TADrpv;
026000141210           new_TADorp = TADorp;
026100141210           new_TADmin = TADmin;
026200141210           new_TADmax = TADmax;
026300141210           new_TADdtr = oggi;
026400141210           write TITAD000;
026500141210           reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
026600141210         ENDDO;
026700141210
026800141210       //?Testata tariffe particolari
026900141210         setll (WFAclf:WFActrv:WFAprgv) TITPT01L;
027000141210         reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
027100141210         DOW  not %eof(TITPT01L);
027200141210           clear TITPT000;
027300141210           new_TPTksc = TPTksc;
027400141210           new_TPTctr = TPTctr;
027500141210           new_TPTprg = TPTprg + 1;
027600141210           new_TPTduv = oggi;
027700141210           new_TPTftc = TPTftc;
027800141210           new_TPTtpg = TPTtpg;
027900141210           new_TPTarl = TPTarl;
028000141210           new_TPTarf = TPTarf;
028100141210           new_TPTaro = TPTaro;
028200141210           new_TPTrpv = TPTrpv;
028300141210           new_TPTvlm = TPTvlm;
028400141210           new_TPTvvm = TPTvvm;
028500141210           new_TPTfvm = TPTfvm;
028600141210           new_TPTtma = TPTtma;
028700141210           new_TPTapl = TPTapl;
028800141210           new_TPTdtr = oggi;
028900141210           new_TPTdpb = TPTdpb;
029000141210           new_TPTdit = TPTdit;
029100141210           new_TPTflo = TPTflo;
029200141210           write TITPT000;
029300141210           reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
029400141210         ENDDO;
029500141210
029600141210       //?Dettaglio tariffe particolari
029700141210         setll (WFAclf:WFActrv:WFAprgv) TITPD01L;
029800141210         reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
029900141210         DOW  not %eof(TITPD01L);
030000141210           clear TITPD000;
030100141210           new_TPDksc = TPDksc;
030200141210           new_TPDctr = TPDctr;
030300141210           new_TPDprg = TPDprg + 1;
030400141210           new_TPDftc = TPDftc;
030500141210           new_TPDcts = TPDcts;
030600141210           new_TPDorp = TPDorp;
030700141210           new_TPDsgl = TPDsgl;
030800141210           new_TPDitr = TPDitr;
030900141210           new_TPDmin = TPDmin;
031000141210           new_TPDmax = TPDmax;
031100141210           new_TPDain = TPDain;
031200141210           new_TPDdtr = oggi;
031300141210           write TITPD000;
031400141210           reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
031500141210         ENDDO;
031600141210
031700141210       //?Tariffe di giacenza
031800141210         setll (WFAclf:WFActrv:WFAprgv) TITGC01L;
031900141210         reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
032000141210         DOW  not %eof(TITGC01L);
032100141210           clear TITGC000;
032200141210           new_TGCksc = TGCksc;
032300141210           new_TGCctr = TGCctr;
032400141210           new_TGCprg = TGCprg + 1;
032500141210           new_TGCsgv = TGCsgv;
032600141210           new_TGCsgr = TGCsgr;
032700141210           new_TGCsgp = TGCsgp;
032800141210           new_TGCsgd = TGCsgd;
032900141210           new_TGCsg1 = TGCsg1;
033000141210           new_TGCsg2 = TGCsg2;
033100141210           new_TGCsg3 = TGCsg3;
033200141210           new_TGCst1 = TGCst1;
033300141210           new_TGCst2 = TGCst2;
033400141210           new_TGCst3 = TGCst3;
033500141210           new_TGCstm = TGCstm;
033600141210           new_TGCrpv = TGCrpv;
033700141210           new_TGCfaf = TGCfaf;
033800141210           new_TGCsgf = TGCsgf;
033900141210           new_TGCggr = TGCggr;
034000141210           new_TGCtcm = TGCtcm;
034100141210           new_TGCtfg = TGCtfg;
034200141210           new_TGCduv = oggi;
034300141210           new_TGCdtr = oggi;
034400141210           write TITGC000;
034500141210           reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
034600141210         ENDDO;
034700141210
034800141210       //?Testata tariffe
034900141210         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
035000141210         IF  %found(TNTAM04L);
035100141210           clear TNTAM000;
035200141210           new_TAMksc = TAMksc;
035300141210           new_TAMctr = TAMctr;
035400141210           new_TAMprg = TAMprg + 1;
035500141210           new_TAMddt = 20150101;
035600141210           new_TAMdst = scadenza;
035700141210           new_TAMduv = oggi;
035800141210           new_TAMdcv = TAMdcv;
035900141210           new_TAMdif = TAMdif;
036000141210           new_TAMfli = TAMfli;
036100141210           new_TAMpri = TAMpri;
036200141210           new_TAMfmp = TAMfmp;
036300141210           new_TAMlrc = TAMlrc;
036400141210           new_TAMlas = TAMlas;
036500141210         //?Inglobo l'addizionale di gestione Anno Corrente nell'addizionale
036600141210         //?di gestione Anno Precedente
036700141210           wadd = TAMpag + TAMppa +
036800141210                    ((TAMpag * TAMppa) / 100);
036900141210           new_TAMpag = wadd;
037000141210           new_TAMmpa = TAMmpa;
037100141210           new_TAMpam = TAMpam;
037200141210           new_TAMmpm = TAMmpm;
037300141210           new_TAMtin = TAMtin;
037400141210           new_TAMkpm = TAMkpm;
037500141210           new_TAMarl = TAMarl;
037600141210           new_TAMarf = TAMarf;
037700141210           new_TAMaro = TAMaro;
037800141210           new_TAMrat = TAMrat;
037900141210           new_TAMrct = TAMrct;
038000141210           new_TAMpri = TAMpri;
038100141210           new_TAMsc2 = TAMsc2;
038200141210           new_TAMeds = TAMeds;
038300141210           new_TAMars = TAMars;
038400141210           new_TAMata = TAMata;
038500141210           new_TAMftp = TAMftp;
038600141210           new_TAMbap = TAMbap;
038700141210           new_TAMtsp = TAMtsp;
038800141210           new_TAMfie = TAMfie;
038900141210           new_TAMfis = TAMfis;
039000141210           new_TAMsis = TAMsis;
039100141210           new_TAMfvd = TAMfvd;
039200141210           new_TAMfts = TAMfts;
039300141210           new_TAMnas = TAMnas;
039400141210           new_TAMnot = TAMnot;
039500141210           new_TAMftm = TAMftm;
039600141210           new_TAMgca = TAMgca;
039700141210           new_TAMgga = TAMgga;
039800141210           new_TAMgma = TAMgma;
039900141210           new_TAMgva = TAMgva;
040000141210           new_TAMbam = TAMbam;
040100141210           new_TAMtpr = TAMtpr;
040200141210           new_TAMflo = TAMflo;
040300141210           new_TAMdat = TAMdat;
040400141210           new_TAMdtr = oggi;
040500141210           write TNTAM000;
040600141210         ENDIF;
040700141210
040800141210       //?Storico variazioni
040900141210         clear TITAV000;
041000141210         TAVksc = new_TAMksc;
041100141210         TAVctr = new_TAMctr;
041200141210         TAVprg = new_TAMprg;
041300141210         TAVtrd = 'TES';
041400141210         TAVcta = 'D';
041500141210         TAVdav = oggi;
041600141210         TAVorv = %dec(%time());
041700141210         TAVnoj = 'AUMENTO';
041800141210         TAVpru = 'BATCH';
041900141210         TAVdtr = oggi;
042000141210         write TITAV000;
042100141210
042200141210       //?File di WORK
042300141210         chain (' ':WFAclf:WFActrv:WFAprgv) WFAUT01L;
042400141210         IF  %found(WFAUT01L);
042500141210           WFAddtn = new_TAMddt;
042600141210           WFAdstn = new_TAMdst;
042700141210           WFAprgn = new_TAMprg;
042800141210           WFApagn = new_TAMpag;
042900141210           WFAppan = new_TAMppa;
043000141210           WFAela = 'S';
043100141210           update WFAUT000;
043200141210         ENDIF;
043300141210
043400141210       ENDSR;
043500141210
043600141210       //--------------------------------------------------------------
043700141210       //?Aggiorno il progressivo attuale.
043800141210       //--------------------------------------------------------------
043900141210       BEGSR Agg_Progressivo;
044000141210
044100141210       //?Testata tariffe
044200141210         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
044300141210         IF  %found(TNTAM04L);
044400141210           TAMdst = 20141231;
044500141210           TAMduv = oggi;
044600141210           TAMdtr = oggi;
044700141210           clear TAMftr;
044800141212           update TNTAM04;
044900141210         ENDIF;
045000141210
045100141210       //?Storico variazioni
045200141210         clear TITAV000;
045300141210         TAVksc = TAMksc;
045400141210         TAVctr = TAMctr;
045500141210         TAVprg = TAMprg;
045600141210         TAVtrd = 'TES';
045700141210         TAVcta = 'D';
045800141210         TAVdav = oggi;
045900141210         TAVorv = %dec(%time());
046000141210         TAVnoj = 'AUMENTO';
046100141210         TAVpru = 'BATCH';
046200141210         TAVdtr = oggi;
046300141210         write TITAV000;
046400141210
046500141210       ENDSR;
046600141210
046700141210       //--------------------------------------------------------------
046800141210       //?Operazioni finali.
046900141210       //--------------------------------------------------------------
047000141210       BEGSR RoutEnd;
047100141210
047200141210         *inLR = *on;
047300141210         return;
047400141210
047500141210       ENDSR;
047600141210
047700141210      /end-free
