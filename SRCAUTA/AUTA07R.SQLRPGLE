000100151202      //-------------------------------------------------------------------
000200171019      //?AUTA07R - Da WF aggiorno le tariffe per Addizionale Gestione 2017
000300151202      //-------------------------------------------------------------------
000400151202
000500151202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600151202     h dftactgrp(*no) actgrp(*caller)
000700151202
000800151202      //---------------------------------------------------------------
000900151202      //?Dichiarazione file.
001000151202      //---------------------------------------------------------------
001100151202     fWFAUT01L  uf   e           k disk
001200151202
001300151202     fTNTAM04L  uf   e           k disk    rename(TNTAM000:TNTAM04)
001400151202     fTITAD04L  uf   e           k disk    rename(TITAD000:TITAD04)
001500151202     fTITPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001600151202     fTITPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
001700151202     fTITGC01L  uf   e           k disk    rename(TITGC000:TITGC01)
001800151202
001900151202     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
002000151202     f                                     prefix(old_)
002100151202
002200151202     fTNTAM00F  o    e             disk    prefix(new_)
002300151202     fTITAD00F  o    e             disk    prefix(new_)
002400151202     fTITPT00F  o    e             disk    prefix(new_)
002500151202     fTITPD00F  o    e             disk    prefix(new_)
002600151202     fTITGC00F  o    e             disk    prefix(new_)
002700151202
002800171027     fTITAV01L  uf a e           k disk
002900151202
003000151202      //---------------------------------------------------------------
003100151202      //?Definizione costanti.
003200151202      //---------------------------------------------------------------
003300151202
003400151202      //---------------------------------------------------------------
003500151202      //?Definizione schiere.
003600151202      //---------------------------------------------------------------
003700151202
003800151202      //---------------------------------------------------------------
003900151202      //?Definizione aree dati.
004000151202      //---------------------------------------------------------------
004100151202
004200151202      // - Dati utente
004300151202     d �AzUte        e ds                  extname(AZUTE00F)
004400151202     d                                     dtaara
004500151202     d �DatiUte      e ds                  extname(dDatiUte)
004600151202     d                                     dtaara
004700151202
004800151202      //---------------------------------------------------------------
004900151202      //?Definizione strutture dati.
005000151202      //---------------------------------------------------------------
005100151202
005200151202      // - Parametri ricevuti
005300151202     d KPJBA         e ds
005400151202
005500151202      // - Reperimento dati utente
005600151202     d TIBS34DS      e ds
005700151202
005800151202      // - File WFAUT
005900151202     d WFAUTDS       e ds                  extname(WFAUT00F)
006000151202
006100151202      //---------------------------------------------------------------
006200151202      //?Definizione variabili globali.
006300151202      //---------------------------------------------------------------
006400151202
006500151202      // - Flags booleani
006600151202     d Fine            s               n   inz(*off)
006700151202     d wEoF            s               n   inz(*off)
006800151202
006900151202      // - Campi di comodo
007000151202     d Oggi            s              8s 0 inz
007100151202     d scadenza        s                   like(TAMdst) inz
007200151202     d wadd            s                   like(TAMpag) inz
007300151202     d wprog           s                   like(TAMprg) inz
007400151202
007500151202      //---------------------------------------------------------------
007600151202      //?Definizione Procedure usate.
007700151202      //---------------------------------------------------------------
007800151202
007900151202      //---------------------------------------------------------------
008000151202      //?Definizione Prototipi.
008100151202      //---------------------------------------------------------------
008200151202      /copy gaitrasrc/srcprotopr,TIBS34R
008300151202
008400151202      //---------------------------------------------------------------
008500151202      //?Definizione key-list.
008600151202      //---------------------------------------------------------------
008700151202
008800151202      //---------------------------------------------------------------
008900151202      //?M A I N - L I N E
009000151202      //---------------------------------------------------------------
009100151202
009200151202     c     *Entry        plist
009300151202     c                   parm                    kpjba
009400151202
009500151202      /free
009600151202
009700151202       //?Operazioni iniziali
009800151202       exsr RoutInz;
009900151202
010000151202       //?Elaborazione
010100151202       exsr Elabora;
010200151202
010300151202       //?Operazioni finali
010400151202       exsr RoutEnd;
010500151202
010600151202       //--------------------------------------------------------------
010700151202       //?Operazioni iniziali.
010800151202       //--------------------------------------------------------------
010900151202       BEGSR RoutInz;
011000151202
011100151202         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011200151202
011300151202       //?Imposto oggi
011400151202         Oggi = %dec(%date());
011500151202
011600151202       //?Reperimento dati job
011700151202         exsr DatiJob;
011800151202
011900151202       ENDSR;
012000151202
012100151202       //--------------------------------------------------------------
012200151202       //?Reperimento Dati del job (Utente/Operativi).
012300151202       //--------------------------------------------------------------
012400151202       BEGSR DatiJob;
012500151202
012600151202         in(E) �AzUte;
012700151202         IF  not %error;
012800151202           in(E) �DatiUte;
012900151202         ENDIF;
013000151202         IF  %error or RSut = *blanks;
013100151202           clear TIBS34ds;
013200151202           tibs34r(tibs34ds);
013300151202           in �AzUte;
013400151202           in �DatiUte;
013500151202         ENDIF;
013600151202
013700151202       ENDSR;
013800151202
013900151202       //--------------------------------------------------------------
014000151202       //?Elaborazione.
014100151202       //--------------------------------------------------------------
014200151202       BEGSR Elabora;
014300151202
014400151202       //?Dichiarazione cursore
014500151202         exec sql
014600151202           declare WRK cursor for
014700151202           select * from WFAUT00F;
014800151202
014900151202         //?Apertura del cursore
015000151202         exec sql
015100151202           open WRK;
015200151202
015300151202         IF sqlcode < 0;
015400151202           exec sql close WRK;
015500151202           leavesr;
015600151202         ENDIF;
015700151202
015800151202         DOW  not wEoF;
015900151202           exec sql
016000151202             fetch next from WRK into :WFAUTDS;
016100151202           IF  sqlcod = 100 or sqlcod < 0;
016200151202             wEoF = *on;
016300151202             leave;
016400151202           ENDIF;
016500151202
016600151202         //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
016700151202           exsr Deleta_Annullati;
016800151202
016900171019         //?Se la tariffa che sto leggendo ha la scadenza da Novembre 2017
017000171027         //?devo creare un nuovo progressivo da 01112017 a data scadenza tariffa 31122018
017100171019         //?e il progressivo che sto leggendo devo farlo scadere il 31102017
017200171030           IF  WFAdstv > 20171031 ;
017201171030               If WFAdstv > 20180100;
017300171030                  scadenza = WFAdstv ;
017301171030               Else;
017302171030                  scadenza = 20181231;
017303171030               Endif;
017400161116           //?Aggiorno il progressivo attuale
017500161116             exsr Agg_Progressivo;
017600161117           ELSE;
017700171019         //?Altrimenti il nuovo progressivo avr� scadenza 31122018
017800171019               scadenza = 20181231;
017900161116           ENDIF;
018000151202
018100151202         //?Scrivo il nuovo progressivo
018200151202           exsr New_Progressivo;
018300151202
018400151202         ENDDO;
018500151202
018600151202         exec sql
018700151202           close WRK;
018800151202
018900151202       ENDSR;
019000151202
019100151202       //--------------------------------------------------------------
019200151202       //?Cancello tariffa annullata.
019300151202       //--------------------------------------------------------------
019400151202       BEGSR Deleta_Annullati;
019500151202
019600151202         wprog = WFAprgv + 1;
019700151202
019800151202         setll (WFAclf:WFActrv:wprog) TITAD04L;
019900151202         reade (WFAclf:WFActrv:wprog) TITAD04L;
020000151202         DOW  not %eof(TITAD04L);
020100151202           IF  TADatb <> *blanks;
020200151202             delete TITAD04;
020300151202           ENDIF;
020400151202           reade (WFAclf:WFActrv:wprog) TITAD04L;
020500151202         ENDDO;
020600151202
020700151202         setll (WFAclf:WFActrv:wprog) TITPT01L;
020800151202         reade (WFAclf:WFActrv:wprog) TITPT01L;
020900151202         DOW  not %eof(TITPT01L);
021000151202           IF  TPTatb <> *blanks;
021100151202             delete TITPT01;
021200151202           ENDIF;
021300151202           reade (WFAclf:WFActrv:wprog) TITPT01L;
021400151202         ENDDO;
021500151202
021600151202         setll (WFAclf:WFActrv:wprog) TITPD01L;
021700151202         reade (WFAclf:WFActrv:wprog) TITPD01L;
021800151202         DOW  not %eof(TITPD01L);
021900151202           IF  TPDatb <> *blanks;
022000151202             delete TITPD01;
022100151202           ENDIF;
022200151202           reade (WFAclf:WFActrv:wprog) TITPD01L;
022300151202         ENDDO;
022400151202
022500151202         setll (WFAclf:WFActrv:wprog) TITGC01L;
022600151202         reade (WFAclf:WFActrv:wprog) TITGC01L;
022700151202         DOW  not %eof(TITGC01L);
022800151202           IF  TGCatb <> *blanks;
022900151202             delete TITGC01;
023000151202           ENDIF;
023100151202           reade (WFAclf:WFActrv:wprog) TITGC01L;
023200151202         ENDDO;
023300151202
023400151202         chain (WFAclf:WFActrv:wprog) TNTAM01L;
023500151202         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
023600151202           delete TNTAM01;
023700151202         ENDIF;
023703171027
023800171027         setll (WFAclf:WFActrv:wprog) TITAD04L;
023802171027         reade (WFAclf:WFActrv:wprog) TITAD04L;
023803171027         DOW  not %eof(TITAD04L);
023804171027           IF  TADatb <> *blanks;
023805171027             delete TITAD04;
023806171027           ENDIF;
023807171027           reade (WFAclf:WFActrv:wprog) TITAD04L;
023808171027         ENDDO;
023900151202       ENDSR;
024000151202
024100151202       //--------------------------------------------------------------
024200151202       //?Scrivo il nuovo progressivo.
024300151202       //--------------------------------------------------------------
024400151202       BEGSR New_Progressivo;
024500151202
024600151202       //?Dettaglio tariffario
024700151202         setll (WFAclf:WFActrv:WFAprgv) TITAD04L;
024800151202         reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
024900151202         DOW  not %eof(TITAD04L);
025000151202           clear TITAD000;
025100151202           new_TADksc = TADksc;
025200151202           new_TADctr = TADctr;
025300151202           new_TADprg = TADprg + 1;
025400151202           new_TADlnp = TADlnp;
025500151202           new_TADcts = TADcts;
025600151202           new_TADnaz = TADnaz;
025700151202           new_TADcap = TADcap;
025800151202           new_TADsgl = TADsgl;
025900151202           new_TADitr = TADitr;
026000151202           new_TADino = TADino;
026100151202           new_TADrpv = TADrpv;
026200151202           new_TADorp = TADorp;
026300151202           new_TADmin = TADmin;
026400151202           new_TADmax = TADmax;
026500151202           new_TADdtr = oggi;
026600151202           write TITAD000;
026700151202           reade (WFAclf:WFActrv:WFAprgv) TITAD04L;
026800151202         ENDDO;
026900151202
027000151202       //?Testata tariffe particolari
027100151202         setll (WFAclf:WFActrv:WFAprgv) TITPT01L;
027200151202         reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
027300151202         DOW  not %eof(TITPT01L);
027400151202           clear TITPT000;
027500151202           new_TPTksc = TPTksc;
027600151202           new_TPTctr = TPTctr;
027700151202           new_TPTprg = TPTprg + 1;
027800151202           new_TPTduv = oggi;
027900151202           new_TPTftc = TPTftc;
028000151202           new_TPTtpg = TPTtpg;
028100151202           new_TPTarl = TPTarl;
028200151202           new_TPTarf = TPTarf;
028300151202           new_TPTaro = TPTaro;
028400151202           new_TPTrpv = TPTrpv;
028500151202           new_TPTvlm = TPTvlm;
028600151202           new_TPTvvm = TPTvvm;
028700151202           new_TPTfvm = TPTfvm;
028800151202           new_TPTtma = TPTtma;
028900151202           new_TPTapl = TPTapl;
029000151202           new_TPTdtr = oggi;
029100151202           new_TPTdpb = TPTdpb;
029200151202           new_TPTdit = TPTdit;
029300151202           new_TPTflo = TPTflo;
029400151202           write TITPT000;
029500151202           reade (WFAclf:WFActrv:WFAprgv) TITPT01L;
029600151202         ENDDO;
029700151202
029800151202       //?Dettaglio tariffe particolari
029900151202         setll (WFAclf:WFActrv:WFAprgv) TITPD01L;
030000151202         reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
030100151202         DOW  not %eof(TITPD01L);
030200151202           clear TITPD000;
030300151202           new_TPDksc = TPDksc;
030400151202           new_TPDctr = TPDctr;
030500151202           new_TPDprg = TPDprg + 1;
030600151202           new_TPDftc = TPDftc;
030700151202           new_TPDcts = TPDcts;
030800151202           new_TPDorp = TPDorp;
030900151202           new_TPDsgl = TPDsgl;
031000151202           new_TPDitr = TPDitr;
031100151202           new_TPDmin = TPDmin;
031200151202           new_TPDmax = TPDmax;
031300151202           new_TPDain = TPDain;
031400151202           new_TPDdtr = oggi;
031500151202           write TITPD000;
031600151202           reade (WFAclf:WFActrv:WFAprgv) TITPD01L;
031700151202         ENDDO;
031800151202
031900151202       //?Tariffe di giacenza
032000151202         setll (WFAclf:WFActrv:WFAprgv) TITGC01L;
032100151202         reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
032200151202         DOW  not %eof(TITGC01L);
032300151202           clear TITGC000;
032400151202           new_TGCksc = TGCksc;
032500151202           new_TGCctr = TGCctr;
032600151202           new_TGCprg = TGCprg + 1;
032700151202           new_TGCsgv = TGCsgv;
032800151202           new_TGCsgr = TGCsgr;
032900151202           new_TGCsgp = TGCsgp;
033000151202           new_TGCsgd = TGCsgd;
033100151202           new_TGCsg1 = TGCsg1;
033200151202           new_TGCsg2 = TGCsg2;
033300151202           new_TGCsg3 = TGCsg3;
033400151202           new_TGCst1 = TGCst1;
033500151202           new_TGCst2 = TGCst2;
033600151202           new_TGCst3 = TGCst3;
033700151202           new_TGCstm = TGCstm;
033800151202           new_TGCrpv = TGCrpv;
033900151202           new_TGCfaf = TGCfaf;
034000151202           new_TGCsgf = TGCsgf;
034100151202           new_TGCggr = TGCggr;
034200151202           new_TGCtcm = TGCtcm;
034300151202           new_TGCtfg = TGCtfg;
034400151202           new_TGCduv = oggi;
034500151202           new_TGCdtr = oggi;
034600151202           write TITGC000;
034700151202           reade (WFAclf:WFActrv:WFAprgv) TITGC01L;
034800151202         ENDDO;
034900151202
035000151202       //?Testata tariffe
035100151202         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
035200151202         IF  %found(TNTAM04L);
035300151202           clear TNTAM000;
035400151202           new_TAMksc = TAMksc;
035500151202           new_TAMctr = TAMctr;
035600151202           new_TAMprg = TAMprg + 1;
035700171019           new_TAMddt = 20171101;
035800151202           new_TAMdst = scadenza;
035900151202           new_TAMduv = oggi;
036000151202           new_TAMdcv = TAMdcv;
036100151202           new_TAMdif = TAMdif;
036200151202           new_TAMfli = TAMfli;
036300151202           new_TAMpri = TAMpri;
036400151202           new_TAMfmp = TAMfmp;
036500151202           new_TAMlrc = TAMlrc;
036600151202           new_TAMlas = TAMlas;
036700151202         //?Inglobo l'addizionale di gestione Anno Corrente nell'addizionale
036800151202         //?di gestione Anno Precedente
036900151202           wadd = TAMpag + TAMppa +
037000151202                    ((TAMpag * TAMppa) / 100);
037100151202           new_TAMpag = wadd;
037200151202           new_TAMmpa = TAMmpa;
037300151202           new_TAMpam = TAMpam;
037400151202           new_TAMmpm = TAMmpm;
037500151202           new_TAMtin = TAMtin;
037600151202           new_TAMkpm = TAMkpm;
037700151202           new_TAMarl = TAMarl;
037800151202           new_TAMarf = TAMarf;
037900151202           new_TAMaro = TAMaro;
038000151202           new_TAMrat = TAMrat;
038100151202           new_TAMrct = TAMrct;
038200151202           new_TAMpri = TAMpri;
038300151202           new_TAMsc2 = TAMsc2;
038400151202           new_TAMeds = TAMeds;
038500151202           new_TAMars = TAMars;
038600151202           new_TAMata = TAMata;
038700151202           new_TAMftp = TAMftp;
038800151202           new_TAMbap = TAMbap;
038900151202           new_TAMtsp = TAMtsp;
039000151202           new_TAMfie = TAMfie;
039100151202           new_TAMfis = TAMfis;
039200151202           new_TAMsis = TAMsis;
039300151202           new_TAMfvd = TAMfvd;
039400151202           new_TAMfts = TAMfts;
039500151202           new_TAMnas = TAMnas;
039600151202           new_TAMnot = TAMnot;
039700151202           new_TAMftm = TAMftm;
039800151202           new_TAMgca = TAMgca;
039900151202           new_TAMgga = TAMgga;
040000151202           new_TAMgma = TAMgma;
040100151202           new_TAMgva = TAMgva;
040200151202           new_TAMbam = TAMbam;
040300151202           new_TAMtpr = TAMtpr;
040400151202           new_TAMflo = TAMflo;
040500151202           new_TAMdat = TAMdat;
040600151202           new_TAMdtr = oggi;
040700151202           write TNTAM000;
040800151202         ENDIF;
040900151202
041000151202       //?Storico variazioni
041100151202         clear TITAV000;
041200151202         TAVksc = new_TAMksc;
041300151202         TAVctr = new_TAMctr;
041400151202         TAVprg = new_TAMprg;
041500151202         TAVtrd = 'TES';
041600151202         TAVcta = 'D';
041700151202         TAVdav = oggi;
041800151202         TAVorv = %dec(%time());
041900161124         TAVnoj = 'INGLOBAM.';
042000151202         TAVpru = 'BATCH';
042100151202         TAVdtr = oggi;
042200151202         write TITAV000;
042300151202
042400151202       //?File di WORK
042500151202         chain (' ':WFAclf:WFActrv:WFAprgv) WFAUT01L;
042600151202         IF  %found(WFAUT01L);
042700151202           WFAddtn = new_TAMddt;
042800151202           WFAdstn = new_TAMdst;
042900151202           WFAprgn = new_TAMprg;
043000151202           WFApagn = new_TAMpag;
043100151202           WFAppan = new_TAMppa;
043200151202           WFAela = 'S';
043300151202           update WFAUT000;
043400151202         ENDIF;
043500151202
043600151202       ENDSR;
043700151202
043800151202       //--------------------------------------------------------------
043900151202       //?Aggiorno il progressivo attuale.
044000151202       //--------------------------------------------------------------
044100151202       BEGSR Agg_Progressivo;
044200151202
044300151202       //?Testata tariffe
044400151202         chain (WFAclf:WFActrv:WFAprgv) TNTAM04L;
044500151202         IF  %found(TNTAM04L);
044600171019           TAMdst = 20171031;
044700151202           TAMduv = oggi;
044800151202           TAMdtr = oggi;
044900151202           clear TAMftr;
045000151202           update TNTAM04;
045100151202         ENDIF;
045200151202
045300151202       //?Storico variazioni
045400151202         clear TITAV000;
045500151202         TAVksc = TAMksc;
045600151202         TAVctr = TAMctr;
045700151202         TAVprg = TAMprg;
045800151202         TAVtrd = 'TES';
045900161130         TAVcta = 'M';
046000151202         TAVdav = oggi;
046100151202         TAVorv = %dec(%time());
046200161124         TAVnoj = 'INGLOBAM.';
046300151202         TAVpru = 'BATCH';
046400151202         TAVdtr = oggi;
046500151202         write TITAV000;
046600151202
046700151202       ENDSR;
046800151202
046900151202       //--------------------------------------------------------------
047000151202       //?Operazioni finali.
047100151202       //--------------------------------------------------------------
047200151202       BEGSR RoutEnd;
047300151202
047400151202         *inLR = *on;
047500151202         return;
047600151202
047700151202       ENDSR;
047800151202
047900151202      /end-free
