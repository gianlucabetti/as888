000100141210      //--------------------------------------------------------------
000200150130      //?AUTA05R - Da WF aggiorno le tariffe per Aumento da Lettera
000300141210      //--------------------------------------------------------------
000400141210
000500141210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141210     h dftactgrp(*no) actgrp(*caller)
000700141210
000800141210      //---------------------------------------------------------------
000900141210      //?Dichiarazione file.
001000141210      //---------------------------------------------------------------
001100150331     fWFAUC02L  uf   e           k disk    rename(WFAUC000:WFAUC02)
001200150331     f                                     prefix(WF2:3)
001300150331     fWFAUC03L  uf   e           k disk    rename(WFAUC000:WFAUC03)
001400150331     f                                     prefix(WFN:3)
001500150130
001600141210     fTNTAM04L  uf   e           k disk    rename(TNTAM000:TNTAM04)
001700141210     fTITAD04L  uf   e           k disk    rename(TITAD000:TITAD04)
001800141210     fTITPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001900141210     fTITPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
002000141210     fTITGC01L  uf   e           k disk    rename(TITGC000:TITGC01)
002100141210
002200141210     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
002300141210     f                                     prefix(old_)
002400141210
002500141210     fTNTAM00F  o    e             disk    prefix(new_)
002600141210     fTITAD00F  o    e             disk    prefix(new_)
002700141210     fTITPT00F  o    e             disk    prefix(new_)
002800141210     fTITPD00F  o    e             disk    prefix(new_)
002900141210     fTITGC00F  o    e             disk    prefix(new_)
003000141210
003100141210     fTITAV01L  if a e           k disk
003200150330
003300150331     fTICMC01L  if   e           k disk
003400150130
003500150130     fAUTA05P   o    e             printer oflind(*in90)
003600111213
003700141210      //---------------------------------------------------------------
003800141210      //?Definizione costanti.
003900141210      //---------------------------------------------------------------
004000150330
004100150330      // - Fase Lettera Aumento
004200150330     d FaseLettera     c                   const(' LT')
004300141210
004400141210      //---------------------------------------------------------------
004500141210      //?Definizione schiere.
004600141210      //---------------------------------------------------------------
004700141210
004800141210      //---------------------------------------------------------------
004900141210      //?Definizione aree dati.
005000141210      //---------------------------------------------------------------
005100141210
005200141210      // - Dati utente
005300141210     d §AzUte        e ds                  extname(AZUTE00F)
005400141210     d                                     dtaara
005500141210     d §DatiUte      e ds                  extname(dDatiUte)
005600141210     d                                     dtaara
005700141210
005800141210      //---------------------------------------------------------------
005900141210      //?Definizione strutture dati.
006000141210      //---------------------------------------------------------------
006100141210
006200141210      // - Parametri ricevuti
006300141210     d KPJBA         e ds
006400141210
006500141210      // - Reperimento dati utente
006600141210     d TIBS34DS      e ds
006700150330
006800150330      // - Scrive nuova fase in Campagna?
006900150330     d TRKC71DS      e ds                  inz
007000141210
007100150130      // - File WFAUC
007200150331     d WFAUCDS       e ds                  extname(WFAUC00F)
007300141210
007400141210      //---------------------------------------------------------------
007500141210      //?Definizione variabili globali.
007600141210      //---------------------------------------------------------------
007700141210
007800141210      // - Flags booleani
007900150331     d Aumenta         s               n   inz(*off)
008000141210     d Fine            s               n   inz(*off)
008100141210     d wEoF            s               n   inz(*off)
008200150408     d wTesta          s               n   inz(*off)
008300150130
008400150130      // - Campi di comodo data
008500161118     d Decorrenza      s                   like(TAMddt) inz (20170101)
008600150130     d Oggi            s              8s 0 inz
008700150130     d Scadenza        s                   like(TAMdst)
008800161116     d ScadenzaNew     s                   like(TAMdst) inz (20171231)
008900150130     d ScadenzaOld     s                   like(TAMdst) inz
009000150130     d wdata_eur       s               d   Datfmt(*eur)
009100150130     d wdata_iso       s               d   Datfmt(*iso)
009200141210
009300141210      // - Campi di comodo
009400150402     d sav_TAMdst      s                   like(TAMdst) inz
009500150331     d sav_WFAksu      s                   like(WFAksu) inz
009600150331     d sav_WFAncm      s                   like(WFAncm) inz
009700150414     d sav_WFAper      s                   like(WFAper) inz
009800150331     d wadd            s                   like(TAMpag) inz
009900141210     d wprog           s                   like(TAMprg) inz
010000141210
010100141210      //---------------------------------------------------------------
010200141210      //?Definizione Procedure usate.
010300141210      //---------------------------------------------------------------
010400141210
010500141210      //---------------------------------------------------------------
010600141210      //?Definizione Prototipi.
010700141210      //---------------------------------------------------------------
010800141210      /copy gaitrasrc/srcprotopr,TIBS34R
010900150330      /copy gaitrasrc/srcprotopr,TRKC71R
011000141210
011100141210      //---------------------------------------------------------------
011200141210      //?Definizione key-list.
011300141210      //---------------------------------------------------------------
011400141210
011500141210      //---------------------------------------------------------------
011600141210      //?M A I N - L I N E
011700141210      //---------------------------------------------------------------
011800141210
011900141210     c     *Entry        plist
012000141210     c                   parm                    kpjba
012100141210
012200141210      /free
012300141210
012400141210       //?Operazioni iniziali
012500141210       exsr RoutInz;
012600141210
012700141210       //?Elaborazione
012800141210       exsr Elabora;
012900141210
013000141210       //?Operazioni finali
013100141210       exsr RoutEnd;
013200141210
013300141210       //--------------------------------------------------------------
013400141210       //?Operazioni iniziali.
013500141210       //--------------------------------------------------------------
013600141210       BEGSR RoutInz;
013700141210
013800141210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013900150331
014000150331       //?Aumento
014100150331         IF  %subst(kpjbu:1:2) = 'SI';
014200150331           Aumenta = *on;
014300150331         ELSE;
014400150331           Aumenta = *off;
014500150331         ENDIF;
014600141210
014700141210       //?Imposto oggi
014800141210         Oggi = %dec(%date());
014900141210
015000150130       //?Imposto scadenza tariffa
015100150130         wdata_iso = %date(Decorrenza:*ISO);
015200150130         wdata_iso = wdata_iso - %days(1);
015300150130         ScadenzaOld = %dec(wdata_iso);
015400150130
015500141210       //?Reperimento dati job
015600141210         exsr DatiJob;
015700141210
015800141210       ENDSR;
015900141210
016000141210       //--------------------------------------------------------------
016100141210       //?Reperimento Dati del job (Utente/Operativi).
016200141210       //--------------------------------------------------------------
016300141210       BEGSR DatiJob;
016400141210
016500141210         in(E) §AzUte;
016600141210         IF  not %error;
016700141210           in(E) §DatiUte;
016800141210         ENDIF;
016900141210         IF  %error or RSut = *blanks;
017000141210           clear TIBS34ds;
017100141210           tibs34r(tibs34ds);
017200141210           in §AzUte;
017300141210           in §DatiUte;
017400141210         ENDIF;
017500141210
017600141210       ENDSR;
017700141210
017800141210       //--------------------------------------------------------------
017900141210       //?Elaborazione.
018000141210       //--------------------------------------------------------------
018100141210       BEGSR Elabora;
018200141210
018300141210       //?Dichiarazione cursore
018400141210         exec sql
018500150130         declare WRK cursor for
018600150331         select * from WFAUC00F
018700150331         where WFAtcl = 'T' and WFAela = ' '
018800150402         order by WFAksu, WFAksc;
018900141210
019000141210         //?Apertura del cursore
019100150130         exec sql open WRK;
019200141210
019300141210         IF sqlcode < 0;
019400141210           exec sql close WRK;
019500141210           leavesr;
019600141210         ENDIF;
019700141210
019800141210         DOW  not wEoF;
019900141210           exec sql
020000150331           fetch next from WRK into :WFAUCDS;
020100141210           IF  sqlcod = 100 or sqlcod < 0;
020200141210             wEoF = *on;
020300141210             leave;
020400141210           ENDIF;
020500150402
020600150402           clear sav_TAMdst;
020700150330
020800150330         //?A cambio unificante scrivo fase LT sulla campagna
020900150331           IF  WFAksu <> sav_WFAksu;
021000150331             IF  sav_WFAksu > 0;
021100150330               exsr New_fase;
021200150330             ENDIF;
021300150331             sav_WFAksu = WFAksu;
021400150331             sav_WFAncm = WFAncm;
021500150414             sav_WFAper = WFAper;
021600150330           ENDIF;
021700141210
021800141210         //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
021900150331           IF  Aumenta;
022000150331             exsr Deleta_Annullati;
022100150331           ENDIF;
022200150130
022300150130           SELECT;
022400150130
022500150130         //?Se la tariffa che sto leggendo
022600150130         //?ha la scadenza minore della data decorrenza - 1 gg della nuova tariffa
022700150130         //?devo modificare la data scadenza della vecchia tariffa
022800150130         //?in modo da non lasciare buchi
022900150130           WHEN  WFAdstv < ScadenzaOld;
023000150331             IF  Aumenta;
023100150130           //?Aggiorno il progressivo attuale
023200150130             Scadenza = ScadenzaOld;
023300150130             exsr Agg_Progressivo;
023400150130           //?Scrivo il nuovo progressivo
023500150402             Scadenza = ScadenzaNew;
023600150130             exsr New_Progressivo;
023700150331             ENDIF;
023800150331           //?Aggiorno file di work
023900150402             Scadenza = ScadenzaNew;
024000150331             exsr Agg_Wrk;
024100150130
024200150130         //?Se la tariffa che sto leggendo
024300150130         //?ha la scadenza = alla data decorrenza - 1 gg della nuova tariffa
024400150130           WHEN  WFAdstv = ScadenzaOld;
024500150331             IF  Aumenta;
024600150130           //?Scrivo il nuovo progressivo
024700150402             Scadenza = ScadenzaNew;
024800150130             exsr New_Progressivo;
024900150331             ENDIF;
025000150331           //?Aggiorno file di work
025100150402             Scadenza = ScadenzaNew;
025200150331             exsr Agg_Wrk;
025300141210
025400150130         //?Se la tariffa che sto leggendo ha
025500150130         //?ha la scadenza maggiore della data decorrenza della nuova tariffa
025600150130         //?e la decorrenza minore della data decorrenza della nuova tariffa
025700150130         //?devo modifcare la data scadenza della vecchia tariffa
025800150130         //?portandola a data decorrenza - 1 gg
025900150130           WHEN  WFAdstv > Decorrenza and WFAddtv < Decorrenza;
026000150331             IF  Aumenta;
026100141210           //?Aggiorno il progressivo attuale
026200150130             Scadenza = ScadenzaOld;
026300141210             exsr Agg_Progressivo;
026400150130           //?Scrivo il nuovo progressivo
026500150402             Scadenza = sav_TAMdst;
026600150130             exsr New_Progressivo;
026700150331             ENDIF;
026800150331           //?Aggiorno file di work
026900150402             clear Scadenza;
027000150331             exsr Agg_Wrk;
027100150130
027200150130         //?Se la tariffa che sto leggendo ha
027300150130         //?ha la decorrenza maggiore/uguale della data decorrena della nuova tariffa
027400150130         //?stampo
027500150130           WHEN  WFAddtv >= Decorrenza;
027600150130           //?Stampo anomalia
027700150130             exsr Stampa;
027800150130
027900150130           ENDSL;
028000141210
028100141210         ENDDO;
028200150330
028300150330         //?A cambio unificante scrivo fase LT sulla campagna
028400150331         IF  sav_WFAksu > 0;
028500150330           exsr New_fase;
028600150330         ENDIF;
028700141210
028800150130         exec sql close WRK;
028900141210
029000141210       ENDSR;
029100141210
029200141210       //--------------------------------------------------------------
029300141210       //?Cancello tariffa annullata.
029400141210       //--------------------------------------------------------------
029500141210       BEGSR Deleta_Annullati;
029600141210
029700141210         wprog = WFAprgv + 1;
029800141210
029900150331         setll (WFAksc:WFActrv:wprog) TITAD04L;
030000150331         reade (WFAksc:WFActrv:wprog) TITAD04L;
030100141210         DOW  not %eof(TITAD04L);
030200141210           IF  TADatb <> *blanks;
030300141210             delete TITAD04;
030400141210           ENDIF;
030500150331           reade (WFAksc:WFActrv:wprog) TITAD04L;
030600141210         ENDDO;
030700141210
030800150331         setll (WFAksc:WFActrv:wprog) TITPT01L;
030900150331         reade (WFAksc:WFActrv:wprog) TITPT01L;
031000141210         DOW  not %eof(TITPT01L);
031100141210           IF  TPTatb <> *blanks;
031200141210             delete TITPT01;
031300141210           ENDIF;
031400150331           reade (WFAksc:WFActrv:wprog) TITPT01L;
031500141210         ENDDO;
031600141210
031700150331         setll (WFAksc:WFActrv:wprog) TITPD01L;
031800150331         reade (WFAksc:WFActrv:wprog) TITPD01L;
031900141210         DOW  not %eof(TITPD01L);
032000141210           IF  TPDatb <> *blanks;
032100141210             delete TITPD01;
032200141210           ENDIF;
032300150331           reade (WFAksc:WFActrv:wprog) TITPD01L;
032400141210         ENDDO;
032500141210
032600150331         setll (WFAksc:WFActrv:wprog) TITGC01L;
032700150331         reade (WFAksc:WFActrv:wprog) TITGC01L;
032800141210         DOW  not %eof(TITGC01L);
032900141210           IF  TGCatb <> *blanks;
033000141210             delete TITGC01;
033100141210           ENDIF;
033200150331           reade (WFAksc:WFActrv:wprog) TITGC01L;
033300141210         ENDDO;
033400141210
033500150331         chain (WFAksc:WFActrv:wprog) TNTAM01L;
033600141210         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
033700141210           delete TNTAM01;
033800141210         ENDIF;
033900141210
034000141210       ENDSR;
034100150130
034200150130       //--------------------------------------------------------------
034300150130       //?Aggiorno il progressivo attuale.
034400150130       //--------------------------------------------------------------
034500150130       BEGSR Agg_Progressivo;
034600150130
034700150130       //?Testata tariffe
034800150331         chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
034900150130         IF  %found(TNTAM04L);
035000150402           sav_TAMdst = TAMdst;
035100150130           TAMdst = Scadenza;
035200150130           TAMduv = Oggi;
035300150130           TAMdtr = Oggi;
035400150130           clear TAMftr;
035500150130           update TNTAM04;
035600150130         ENDIF;
035700150130
035800150130       //?Storico variazioni
035900150130         clear TITAV000;
036000150130         TAVksc = TAMksc;
036100150130         TAVctr = TAMctr;
036200150130         TAVprg = TAMprg;
036300150130         TAVtrd = 'TES';
036400150130         TAVcta = 'D';
036500150130         TAVdav = Oggi;
036600150130         TAVorv = %dec(%time());
036700161116         TAVnoj = 'LETTERA';
036800150130         TAVpru = 'BATCH';
036900150130         TAVdtr = Oggi;
037000150130         write TITAV000;
037100150130
037200150130       ENDSR;
037300141210
037400141210       //--------------------------------------------------------------
037500141210       //?Scrivo il nuovo progressivo.
037600141210       //--------------------------------------------------------------
037700141210       BEGSR New_Progressivo;
037800141210
037900141210       //?Dettaglio tariffario
038000150331         setll (WFAksc:WFActrv:WFAprgv) TITAD04L;
038100150331         reade (WFAksc:WFActrv:WFAprgv) TITAD04L;
038200141210         DOW  not %eof(TITAD04L);
038300141210           clear TITAD000;
038400141210           new_TADksc = TADksc;
038500141210           new_TADctr = TADctr;
038600141210           new_TADprg = TADprg + 1;
038700141210           new_TADlnp = TADlnp;
038800141210           new_TADcts = TADcts;
038900141210           new_TADnaz = TADnaz;
039000141210           new_TADcap = TADcap;
039100141210           new_TADsgl = TADsgl;
039200141210           new_TADitr = TADitr;
039300141210           new_TADino = TADino;
039400141210           new_TADrpv = TADrpv;
039500141210           new_TADorp = TADorp;
039600141210           new_TADmin = TADmin;
039700141210           new_TADmax = TADmax;
039800150130           new_TADdtr = Oggi;
039900141210           write TITAD000;
040000150331           reade (WFAksc:WFActrv:WFAprgv) TITAD04L;
040100141210         ENDDO;
040200141210
040300141210       //?Testata tariffe particolari
040400150331         setll (WFAksc:WFActrv:WFAprgv) TITPT01L;
040500150331         reade (WFAksc:WFActrv:WFAprgv) TITPT01L;
040600141210         DOW  not %eof(TITPT01L);
040700141210           clear TITPT000;
040800141210           new_TPTksc = TPTksc;
040900141210           new_TPTctr = TPTctr;
041000141210           new_TPTprg = TPTprg + 1;
041100150130           new_TPTduv = Oggi;
041200141210           new_TPTftc = TPTftc;
041300141210           new_TPTtpg = TPTtpg;
041400141210           new_TPTarl = TPTarl;
041500141210           new_TPTarf = TPTarf;
041600141210           new_TPTaro = TPTaro;
041700141210           new_TPTrpv = TPTrpv;
041800141210           new_TPTvlm = TPTvlm;
041900141210           new_TPTvvm = TPTvvm;
042000141210           new_TPTfvm = TPTfvm;
042100141210           new_TPTtma = TPTtma;
042200141210           new_TPTapl = TPTapl;
042300150130           new_TPTdtr = Oggi;
042400141210           new_TPTdpb = TPTdpb;
042500141210           new_TPTdit = TPTdit;
042600141210           new_TPTflo = TPTflo;
042700141210           write TITPT000;
042800150331           reade (WFAksc:WFActrv:WFAprgv) TITPT01L;
042900141210         ENDDO;
043000141210
043100141210       //?Dettaglio tariffe particolari
043200150331         setll (WFAksc:WFActrv:WFAprgv) TITPD01L;
043300150331         reade (WFAksc:WFActrv:WFAprgv) TITPD01L;
043400141210         DOW  not %eof(TITPD01L);
043500141210           clear TITPD000;
043600141210           new_TPDksc = TPDksc;
043700141210           new_TPDctr = TPDctr;
043800141210           new_TPDprg = TPDprg + 1;
043900141210           new_TPDftc = TPDftc;
044000141210           new_TPDcts = TPDcts;
044100141210           new_TPDorp = TPDorp;
044200141210           new_TPDsgl = TPDsgl;
044300141210           new_TPDitr = TPDitr;
044400141210           new_TPDmin = TPDmin;
044500141210           new_TPDmax = TPDmax;
044600141210           new_TPDain = TPDain;
044700150130           new_TPDdtr = Oggi;
044800141210           write TITPD000;
044900150331           reade (WFAksc:WFActrv:WFAprgv) TITPD01L;
045000141210         ENDDO;
045100141210
045200141210       //?Tariffe di giacenza
045300150331         setll (WFAksc:WFActrv:WFAprgv) TITGC01L;
045400150331         reade (WFAksc:WFActrv:WFAprgv) TITGC01L;
045500141210         DOW  not %eof(TITGC01L);
045600141210           clear TITGC000;
045700141210           new_TGCksc = TGCksc;
045800141210           new_TGCctr = TGCctr;
045900141210           new_TGCprg = TGCprg + 1;
046000141210           new_TGCsgv = TGCsgv;
046100141210           new_TGCsgr = TGCsgr;
046200141210           new_TGCsgp = TGCsgp;
046300141210           new_TGCsgd = TGCsgd;
046400141210           new_TGCsg1 = TGCsg1;
046500141210           new_TGCsg2 = TGCsg2;
046600141210           new_TGCsg3 = TGCsg3;
046700141210           new_TGCst1 = TGCst1;
046800141210           new_TGCst2 = TGCst2;
046900141210           new_TGCst3 = TGCst3;
047000141210           new_TGCstm = TGCstm;
047100141210           new_TGCrpv = TGCrpv;
047200141210           new_TGCfaf = TGCfaf;
047300141210           new_TGCsgf = TGCsgf;
047400141210           new_TGCggr = TGCggr;
047500141210           new_TGCtcm = TGCtcm;
047600141210           new_TGCtfg = TGCtfg;
047700150130           new_TGCduv = Oggi;
047800150130           new_TGCdtr = Oggi;
047900141210           write TITGC000;
048000150331           reade (WFAksc:WFActrv:WFAprgv) TITGC01L;
048100141210         ENDDO;
048200141210
048300141210       //?Testata tariffe
048400150331         chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
048500141210         IF  %found(TNTAM04L);
048600141210           clear TNTAM000;
048700141210           new_TAMksc = TAMksc;
048800141210           new_TAMctr = TAMctr;
048900141210           new_TAMprg = TAMprg + 1;
049000150130           new_TAMddt = Decorrenza;
049100150402           new_TAMdst = Scadenza;
049200150130           new_TAMduv = Oggi;
049300141210           new_TAMdcv = TAMdcv;
049400141210           new_TAMdif = TAMdif;
049500141210           new_TAMfli = TAMfli;
049600141210           new_TAMpri = TAMpri;
049700141210           new_TAMfmp = TAMfmp;
049800141210           new_TAMlrc = TAMlrc;
049900141210           new_TAMlas = TAMlas;
050000141210         //?Inglobo l'addizionale di gestione Anno Corrente nell'addizionale
050100141210         //?di gestione Anno Precedente
050200150331           wadd = TAMpag + TAMppa +
050300150331                ((TAMpag * TAMppa) / 100);
050400150331           new_TAMpag = wadd ;
050500150331           new_TAMppa = WFAper;
050600141210           new_TAMmpa = TAMmpa;
050700141210           new_TAMpam = TAMpam;
050800141210           new_TAMmpm = TAMmpm;
050900141210           new_TAMtin = TAMtin;
051000141210           new_TAMkpm = TAMkpm;
051100141210           new_TAMarl = TAMarl;
051200141210           new_TAMarf = TAMarf;
051300141210           new_TAMaro = TAMaro;
051400141210           new_TAMrat = TAMrat;
051500141210           new_TAMrct = TAMrct;
051600141210           new_TAMpri = TAMpri;
051700141210           new_TAMsc2 = TAMsc2;
051800141210           new_TAMeds = TAMeds;
051900141210           new_TAMars = TAMars;
052000141210           new_TAMata = TAMata;
052100141210           new_TAMftp = TAMftp;
052200141210           new_TAMbap = TAMbap;
052300141210           new_TAMtsp = TAMtsp;
052400141210           new_TAMfie = TAMfie;
052500141210           new_TAMfis = TAMfis;
052600141210           new_TAMsis = TAMsis;
052700141210           new_TAMfvd = TAMfvd;
052800141210           new_TAMfts = TAMfts;
052900141210           new_TAMnas = TAMnas;
053000141210           new_TAMnot = TAMnot;
053100141210           new_TAMftm = TAMftm;
053200141210           new_TAMgca = TAMgca;
053300141210           new_TAMgga = TAMgga;
053400141210           new_TAMgma = TAMgma;
053500141210           new_TAMgva = TAMgva;
053600141210           new_TAMbam = TAMbam;
053700141210           new_TAMtpr = TAMtpr;
053800141210           new_TAMflo = TAMflo;
053900141210           new_TAMdat = TAMdat;
054000150130           new_TAMdtr = Oggi;
054100141210           write TNTAM000;
054200141210         ENDIF;
054300141210
054400141210       //?Storico variazioni
054500141210         clear TITAV000;
054600141210         TAVksc = new_TAMksc;
054700141210         TAVctr = new_TAMctr;
054800141210         TAVprg = new_TAMprg;
054900141210         TAVtrd = 'TES';
055000141210         TAVcta = 'D';
055100150130         TAVdav = Oggi;
055200141210         TAVorv = %dec(%time());
055300161116         TAVnoj = 'LETTERA';
055400141210         TAVpru = 'BATCH';
055500150130         TAVdtr = Oggi;
055600141210         write TITAV000;
055700150331
055800150331       ENDSR;
055900150331
056000150331       //--------------------------------------------------------------
056100150331       //?Aggiorno il file di Work.
056200150331       //--------------------------------------------------------------
056300150331       BEGSR Agg_Wrk;
056400150331
056500150331       //?File di WORK
056600150331         chain ('T':WFAksc:WFActrv:WFAprgv) WFAUC03L;
056700150331         IF  %found(WFAUC03L);
056800150331
056900150331           IF  Aumenta;
057000150331             WFNddtn = new_TAMddt;
057100150331             WFNdstn = new_TAMdst;
057200150403             WFNctrn = new_TAMctr;
057300150331             WFNprgn = new_TAMprg;
057400150331             WFNpagn = new_TAMpag;
057500150331             WFNppan = new_TAMppa;
057600150331           ELSE;
057700150331             chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
057800150331             IF  %found(TNTAM04L);
057900150402               IF  Scadenza = 0;
058000150402                 Scadenza = TAMdst;
058100150402               ENDIF;
058200150331               WFNddtn = Decorrenza;
058300150402               WFNdstn = Scadenza;
058400150403               WFNctrn = TAMctr;
058500150331               WFNprgn = TAMprg + 1;
058600150331               wadd = TAMpag + TAMppa +
058700150331                     ((TAMpag * TAMppa) / 100);
058800150331               WFNpagn = wadd;
058900150331               WFNppan = WFAper;
059000150331             ENDIF;
059100150331           ENDIF;
059200150331           WFNela = 'S';
059300150331           update WFAUC03;
059400150331         ENDIF;
059500141210
059600141210       ENDSR;
059700150330
059800150330       //--------------------------------------------------------------
059900150330       //?Scrivo nuova fase LT sulla campagna.
060000150330       //--------------------------------------------------------------
060100150330       BEGSR New_Fase;
060200150331
060300150331         IF  Aumenta;
060400150331           chain (sav_WFAncm:sav_WFAksu) TICMC01L;
060500150331           IF  %found(TICMC01L);
060600150331             clear TRKC71DS;
060700150331             IKC71ncm = sav_WFAncm;
060800150331             IKC71ksu = sav_WFAksu;
060900150331             IKC71acm = FaseLettera;
061000150414             IKC71pea = sav_WFAper;
061100150331             IKC71aut = 'A';
061200150331             IKC71dfa = %dec(%date());
061300150331             IKC71hfc = %dec(%time());
061400150331             TRKC71R (kpjba:TRKC71DS);
061500150331           ENDIF;
061600150331         ENDIF;
061700150331
061800150331         chain ('U':sav_WFAksu) WFAUC02L;
061900150331         IF  %found(WFAUC02L);
062000150331           WF2flt = 'S';
062100150331           update WFAUC02;
062200150331         ENDIF;
062300150330
062400150330       ENDSR;
062500150130
062600150130       //--------------------------------------------------------------
062700150130       //?Stampa.
062800150130       //--------------------------------------------------------------
062900150130       BEGSR Stampa;
063000150130
063100150130       //?Testata
063200150408         IF  not wTesta or *in90;
063300150130           write AUTA05T;
063400150408           wTesta = *on;
063500150130           *in90 = *off;
063600150130         ENDIF;
063700150130
063800150130       //?Dettaglio
063900150130         wdata_iso = %date(WFAddtv:*ISO);
064000150130         wdata_eur = wdata_iso;
064100150130         stpddtv = %dec(wdata_eur);
064200150130         wdata_iso = %date(WFAdstv:*ISO);
064300150130         wdata_eur = wdata_iso;
064400150130         stpdstv = %dec(wdata_eur);
064500150130
064600150331         stpper = WFAper;
064700150130
064800150130         write AUTA05D;
064900150130
065000150130       ENDSR;
065100141210
065200141210       //--------------------------------------------------------------
065300141210       //?Operazioni finali.
065400141210       //--------------------------------------------------------------
065500141210       BEGSR RoutEnd;
065600141210
065700141210         *inLR = *on;
065800141210         return;
065900141210
066000141210       ENDSR;
066100141210
066200141210      /end-free
