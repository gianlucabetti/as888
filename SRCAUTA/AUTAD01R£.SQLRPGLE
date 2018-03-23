000100170210      //-------------------------------------------------------------------
000200170214      //?AUTAD01R - Da WF crea Nuove Tariffe Particolari DPD da TAR
000300170210      //-------------------------------------------------------------------
000400170210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170210
000600170210      //---------------------------------------------------------------
000700170210      //?Dichiarazione file.
000800170210      //---------------------------------------------------------------
000900170210     fWFAUD01L  uf   e           k disk
001000170210
001100170210     fTNTAM04L  uf   e           k disk    rename(TNTAM000:TNTAM04)
001200170210     fTITAD04L  uf   e           k disk    rename(TITAD000:TITAD04)
001300170210     fTITPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001400170210     fTITPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
001500170210     fTITGC01L  uf   e           k disk    rename(TITGC000:TITGC01)
001600170210
001700170210     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
001800170210     f                                     prefix(old_)
001900170210
002000170210     fTNTAM00F  o    e             disk    prefix(new_)
002100170210     fTITAD00F  o    e             disk    prefix(new_)
002200170210     fTITPT00F  o    e             disk    prefix(new_)
002300170210     fTITPD00F  o    e             disk    prefix(new_)
002400170210     fTITGC00F  o    e             disk    prefix(new_)
002500170210
002600170213     fTITAV01L  uf a e           k disk
002700170210
002800170210      //---------------------------------------------------------------
002900170210      //?Definizione costanti.
003000170210      //---------------------------------------------------------------
003100170210
003200170210      //---------------------------------------------------------------
003300170210      //?Definizione schiere.
003400170210      //---------------------------------------------------------------
003500170210
003600170210      //---------------------------------------------------------------
003700170210      //?Definizione aree dati.
003800170210      //---------------------------------------------------------------
003900170210      // - Dati utente
004000170210     d §AzUte        e ds                  extname(AZUTE00F)
004100170210     d                                     dtaara
004200170210     d §DatiUte      e ds                  extname(dDatiUte)
004300170210     d                                     dtaara
004400170210
004500170210      //---------------------------------------------------------------
004600170210      //?Definizione strutture dati.
004700170210      //---------------------------------------------------------------
004800170210      // - Parametri ricevuti
004900170210     d KPJBA         e ds
005000170210
005100170210      // - Reperimento dati utente
005200170210     d TIBS34DS      e ds
005300170210
005400170210      // - File TITPT00F x cartello dirottamento
005500170210     d TITPTDIR      e ds                  extname(TITPT00F)
005600170210     d                                     prefix(dir_)
005700170210
005800170210      // - File TITPT00F x cartello fuori misura
005900170210     d TITPTFUO      e ds                  extname(TITPT00F)
006000170210     d                                     prefix(fuo_)
006100170210
006200170210      // - File WFAUD00F
006300170210     d WFAUDDS       e ds                  extname(WFAUD00F)
006400170210
006500170210      //---------------------------------------------------------------
006600170210      //?Definizione variabili globali.
006700170210      //---------------------------------------------------------------
006800170210      // - Flags booleani
006900170210     d Fine            s               n   inz(*off)
007000170213     d NewPrg          s               n   inz(*off)
007100170210     d wEoF            s               n   inz(*off)
007200170210
007300170210      // - Campi di comodo
007400170210     d decorrenza      s                   like(TAMddt) inz
007500170210     d Dirottamento    s                   like(TPTftc) inz('* ')
007600170210     d FuoriMisura     s                   like(TPTftc) inz('F ')
007700170210     d oggi            s              8s 0 inz
007800170213     d olddst          s                   like(TAMdst) inz
007900170210     d oldprg          s                   like(TAMprg) inz
008000170210     d progressivo     s                   like(TAMprg) inz
008100170210     d scadenza        s                   like(TAMdst) inz
008200170210     d wprog           s                   like(TAMprg) inz
008300170210
008400170210      // - Campi di comodo per tariffa cartello DPD
008500170210     d ksc888          s                   like(TAMksc) inz(8888817)
008600170210     d ctr888          s                   like(TAMctr) inz(300)
008700170210     d prg888          s                   like(TAMprg) inz(17)
008800170210
008900170210      //---------------------------------------------------------------
009000170210      //?Definizione Procedure usate.
009100170210      //---------------------------------------------------------------
009200170210
009300170210      //---------------------------------------------------------------
009400170210      //?Definizione Prototipi.
009500170210      //---------------------------------------------------------------
009600170210      /copy gaitrasrc/srcprotopr,TIBS34R
009700170210
009800170210      //---------------------------------------------------------------
009900170210      //?Definizione key-list.
010000170210      //---------------------------------------------------------------
010100170210
010200170210      //---------------------------------------------------------------
010300170210      //?M A I N - L I N E
010400170210      //---------------------------------------------------------------
010500170210     c     *Entry        plist
010600170210     c                   parm                    kpjba
010700170210
010800170210      /free
010900170210
011000170210       //?Operazioni iniziali
011100170210       exsr RoutInz;
011200170210
011300170210       //?Elaborazione
011400170210       exsr Elabora;
011500170210
011600170210       //?Operazioni finali
011700170210       exsr RoutEnd;
011800170210
011900170210       //--------------------------------------------------------------
012000170210       //?Operazioni iniziali.
012100170210       //--------------------------------------------------------------
012200170210       BEGSR RoutInz;
012300170213
012400170213         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012500170210
012600170210       //?Imposto oggi
012700170210         Oggi = %dec(%date());
012800170210
012900170210       //?Reperimento dati job
013000170210         exsr DatiJob;
013100170210
013200170210       //?Cerco la testata della tariffa particolare dirottamento della cartello DPD
013300170210         exec sql
013400170210         SELECT TITPT00F.* into :TITPTdir
013500170210         FROM   TITPT00F
013600170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
013700170210                TPTftc = :dirottamento;
013800170210         IF  sqlcod <> 0;
013900170210           clear TITPTdir;
014000170210         ENDIF;
014100170210       //?Cerco la testata della tariffa particolare fuori misura della cartello DPD
014200170210         exec sql
014300170210         SELECT TITPT00F.* into :TITPTfuo
014400170210         FROM   TITPT00F
014500170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
014600170210                TPTftc = :fuorimisura;
014700170210         IF  sqlcod <> 0;
014800170210           clear TITPTfuo;
014900170210         ENDIF;
015000170210
015100170210       ENDSR;
015200170210
015300170210       //--------------------------------------------------------------
015400170210       //?Reperimento Dati del job (Utente/Operativi).
015500170210       //--------------------------------------------------------------
015600170210       BEGSR DatiJob;
015700170210
015800170210         in(E) §AzUte;
015900170210         IF  not %error;
016000170210           in(E) §DatiUte;
016100170210         ENDIF;
016200170210         IF  %error or RSut = *blanks;
016300170210           clear TIBS34ds;
016400170210           tibs34r(tibs34ds);
016500170210           in §AzUte;
016600170210           in §DatiUte;
016700170210         ENDIF;
016800170210
016900170210       ENDSR;
017000170210
017100170210       //--------------------------------------------------------------
017200170210       //?Elaborazione.
017300170210       //--------------------------------------------------------------
017400170210       BEGSR Elabora;
017500170210
017600170210       //?Dichiarazione cursore
017700170210         exec sql
017800170210         declare WRK cursor for
017900170210         select * from WFAUD00F
018000170210         where (WFAdir <> ' '  or WFAfuo <> ' ') and
018100170220                WFAtipo = 'T' and WFAela = ' '
018200170213         order by WFAksc, WFActrv, WFAprgv;
018300170210
018400170210         //?Apertura del cursore
018500170210         exec sql
018600170210         open WRK;
018700170210
018800170210         IF  sqlcode < 0;
018900170210           exec sql close WRK;
019000170210           leavesr;
019100170210         ENDIF;
019200170210
019300170210         DOW  not wEoF;
019400170210           exec sql
019500170210           fetch next from WRK into :WFAUDDS;
019600170210           IF  sqlcod = 100 or sqlcod < 0;
019700170210             wEoF = *on;
019800170210             leave;
019900170210           ENDIF;
020000170210
020100170210         //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
020200170210           exsr Deleta_Annullati;
020300170213
020400170213           NewPrg = *off;
020500170210
020600170210         //?Se la tariffa che sto leggendo ha scadenza < 01/03/2017
020700170210         //?devo creare un nuovo progressivo con decorrenza 01/03/2017 e scadenza 31/12/2017
020800170210           IF  WFAdstv < 20170301;
020900170210             decorrenza = 20170301;
021000170210             scadenza = 20171231;
021100170210             progressivo = WFAprgv + 1;
021200170210           //?Scrivo un nuovo progressivo
021300170210             exsr NewProgressivo;
021400170210           ENDIF;
021500170210
021600170210         //?Se la tariffa che sto leggendo ha data decorrenza < 01/03/2017 e
021700170210         //?data scadenza > 01/03/2017
021800170210         //?devo creare un nuovo progressivo con decorrenza 01/03/2017
021900170210         //?e scadenza = data scadenza tariffa
022000170210         //?e il progressivo che sto leggendo deve averea data scadenza = 28/02/2017
022100170210           IF  WFAddtv < 20170301 and WFAdstv > 20170301 ;
022200170210           //?Aggiorno il progressivo attuale
022300170210             exsr AggPrgAttuale;
022400170210           //?Scrivo un nuovo progressivo
022500170210             decorrenza = 20170301;
022600170210             scadenza = WFAdstv ;
022700170210             progressivo = WFAprgv + 1;
022800170210             exsr NewProgressivo;
022900170210           ENDIF;
023000170210
023100170210         //?Se la tariffa che sto leggendo ha data decorrenza = 01/03/2017
023200170210         //?devo aggiornare/scrivere solo le tariffe particolari
023300170210           IF  WFAddtv = 20170301;
023400170210           //?Imposto il progressivo attuale
023500170210             progressivo = WFAprgv;
023600170210             exsr AggTarParticolari;
023700170210           ENDIF;
023800170210
023900170210         //?Se la tariffa che sto leggendo ha data decorrenza > 01/03/2017
024000170220         //?non la elaboro
024100170210           IF  WFAddtv > 20170301;
024200170210           ENDIF;
024300170210
024400170210         ENDDO;
024500170210
024600170210         exec sql
024700170210         close WRK;
024800170210
024900170210       ENDSR;
025000170210
025100170210       //--------------------------------------------------------------
025200170210       //?Cancello tariffa annullata.
025300170210       //--------------------------------------------------------------
025400170210       BEGSR Deleta_Annullati;
025500170210
025600170210         wprog = WFAprgv + 1;
025700170210
025800170210         setll (WFAksc:WFActrv:wprog) TITAD04L;
025900170210         reade (WFAksc:WFActrv:wprog) TITAD04L;
026000170210         DOW  not %eof(TITAD04L);
026100170210           IF  TADatb <> *blanks;
026200170210             delete TITAD04;
026300170210           ENDIF;
026400170210           reade (WFAksc:WFActrv:wprog) TITAD04L;
026500170210         ENDDO;
026600170210
026700170210         setll (WFAksc:WFActrv:wprog) TITPT01L;
026800170210         reade (WFAksc:WFActrv:wprog) TITPT01L;
026900170210         DOW  not %eof(TITPT01L);
027000170210           IF  TPTatb <> *blanks;
027100170210             delete TITPT01;
027200170210           ENDIF;
027300170210           reade (WFAksc:WFActrv:wprog) TITPT01L;
027400170210         ENDDO;
027500170210
027600170210         setll (WFAksc:WFActrv:wprog) TITPD01L;
027700170210         reade (WFAksc:WFActrv:wprog) TITPD01L;
027800170210         DOW  not %eof(TITPD01L);
027900170210           IF  TPDatb <> *blanks;
028000170210             delete TITPD01;
028100170210           ENDIF;
028200170210           reade (WFAksc:WFActrv:wprog) TITPD01L;
028300170210         ENDDO;
028400170210
028500170210         setll (WFAksc:WFActrv:wprog) TITGC01L;
028600170210         reade (WFAksc:WFActrv:wprog) TITGC01L;
028700170210         DOW  not %eof(TITGC01L);
028800170210           IF  TGCatb <> *blanks;
028900170210             delete TITGC01;
029000170210           ENDIF;
029100170210           reade (WFAksc:WFActrv:wprog) TITGC01L;
029200170210         ENDDO;
029300170210
029400170210         chain (WFAksc:WFActrv:wprog) TNTAM01L;
029500170210         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
029600170210           delete TNTAM01;
029700170210         ENDIF;
029800170213
029900170213         setll (WFAksc:WFActrv:wprog) TITAV01L;
030000170213         reade (WFAksc:WFActrv:wprog) TITAV01L;
030100170213         DOW  not %eof(TITAV01L);
030200170213           IF  TAVatb <> *blanks;
030300170213             delete TITAV000;
030400170213           ENDIF;
030500170213           reade (WFAksc:WFActrv:wprog) TITAV01L;
030600170213         ENDDO;
030700170210
030800170210       ENDSR;
030900170210
031000170210       //--------------------------------------------------------------
031100170210       //?Scrivo un nuovo progressivo.
031200170210       //--------------------------------------------------------------
031300170210       BEGSR NewProgressivo;
031400170213
031500170213         NewPrg = *on;
031600170210
031700170210       //?Dettaglio tariffario
031800170210         setll (WFAksc:WFActrv:WFAprgv) TITAD04L;
031900170210         reade (WFAksc:WFActrv:WFAprgv) TITAD04L;
032000170210         DOW  not %eof(TITAD04L);
032100170210           clear TITAD000;
032200170210           new_TADksc = TADksc;
032300170210           new_TADctr = TADctr;
032400170210           new_TADprg = progressivo;
032500170210           new_TADlnp = TADlnp;
032600170210           new_TADcts = TADcts;
032700170210           new_TADnaz = TADnaz;
032800170210           new_TADcap = TADcap;
032900170210           new_TADsgl = TADsgl;
033000170210           new_TADitr = TADitr;
033100170210           new_TADino = TADino;
033200170210           new_TADrpv = TADrpv;
033300170210           new_TADorp = TADorp;
033400170210           new_TADmin = TADmin;
033500170210           new_TADmax = TADmax;
033600170210           new_TADdtr = oggi;
033700170210           write TITAD000;
033800170210           reade (WFAksc:WFActrv:WFAprgv) TITAD04L;
033900170210         ENDDO;
034000170210
034100170210       //?Testata tariffe particolari
034200170210         setll (WFAksc:WFActrv:WFAprgv) TITPT01L;
034300170210         reade (WFAksc:WFActrv:WFAprgv) TITPT01L;
034400170210         DOW  not %eof(TITPT01L);
034500170210           clear TITPT000;
034600170210           new_TPTksc = TPTksc;
034700170210           new_TPTctr = TPTctr;
034800170210           new_TPTprg = progressivo;
034900170210           new_TPTduv = oggi;
035000170210           new_TPTftc = TPTftc;
035100170210           new_TPTtpg = TPTtpg;
035200170210           new_TPTarl = TPTarl;
035300170210           new_TPTarf = TPTarf;
035400170210           new_TPTaro = TPTaro;
035500170210           new_TPTrpv = TPTrpv;
035600170210           new_TPTvlm = TPTvlm;
035700170210           new_TPTvvm = TPTvvm;
035800170210           new_TPTfvm = TPTfvm;
035900170210           new_TPTtma = TPTtma;
036000170210           new_TPTapl = TPTapl;
036100170210           new_TPTdtr = oggi;
036200170210           new_TPTdpb = TPTdpb;
036300170210           new_TPTdit = TPTdit;
036400170210           new_TPTflo = TPTflo;
036500170210           write TITPT000;
036600170210           reade (WFAksc:WFActrv:WFAprgv) TITPT01L;
036700170210         ENDDO;
036800170210
036900170210       //?Se non c'era la testata relativa al dirottamento o al fuori misura
037000170210       //?devo scriverle uguali alla tariffa di cartello
037100170210         IF  WFAdir = 'A';
037200170210           exsr NewTestataDirottamento;
037300170210         ENDIF;
037400170210         IF  WFAfuo = 'A';
037500170210           exsr NewTestataFuoriMisura;
037600170210         ENDIF;
037700170210
037800170210       //?Dettaglio tariffe particolari
037900170210         setll (WFAksc:WFActrv:WFAprgv) TITPD01L;
038000170210         reade (WFAksc:WFActrv:WFAprgv) TITPD01L;
038100170210         DOW  not %eof(TITPD01L);
038200170210           clear TITPD000;
038300170210           new_TPDksc = TPDksc;
038400170210           new_TPDctr = TPDctr;
038500170210           new_TPDprg = progressivo;
038600170210           new_TPDftc = TPDftc;
038700170210           new_TPDcts = TPDcts;
038800170210           new_TPDorp = TPDorp;
038900170210           new_TPDsgl = TPDsgl;
039000170210           new_TPDitr = TPDitr;
039100170210           new_TPDmin = TPDmin;
039200170210           new_TPDmax = TPDmax;
039300170210           new_TPDain = TPDain;
039400170210           new_TPDdtr = oggi;
039500170210         //?Se la tariffa particolare era negata prima di scriverla imposto il nuovo valore
039600170210           IF  WFAdir = 'M' and new_TPDftc = dirottamento;
039700170210             new_TPDsgl = 9999999;
039800170210             new_TPDitr = 6;
039900170210           ENDIF;
040000170210           IF  WFAfuo = 'M' and new_TPDftc = fuorimisura;
040100170210             new_TPDsgl = 9999999;
040200170210             new_TPDitr = 28,922;
040300170210           ENDIF;
040400170210           write TITPD000;
040500170210           reade (WFAksc:WFActrv:WFAprgv) TITPD01L;
040600170210         ENDDO;
040700170210       //?Se non c'era il dettaglio relativo al dirottamento o al fuori misura lo
040800170210       //?devo scrivere adesso
040900170210         IF  WFAdir = 'T' or WFAdir = 'A';
041000170210           exsr NewDettaglioDirottamento;
041100170210         ENDIF;
041200170210         IF  WFAfuo = 'T' or WFAfuo = 'A';
041300170210           exsr NewDettaglioFuoriMisura;
041400170210         ENDIF;
041500170210
041600170210       //?Scrivo lo storico delle variazioni per le tariffe particolari
041700170210         exsr StoricoTarParticolari;
041800170210
041900170210       //?Tariffe di giacenza
042000170210         setll (WFAksc:WFActrv:WFAprgv) TITGC01L;
042100170210         reade (WFAksc:WFActrv:WFAprgv) TITGC01L;
042200170210         DOW  not %eof(TITGC01L);
042300170210           clear TITGC000;
042400170210           new_TGCksc = TGCksc;
042500170210           new_TGCctr = TGCctr;
042600170210           new_TGCprg = progressivo;
042700170210           new_TGCsgv = TGCsgv;
042800170210           new_TGCsgr = TGCsgr;
042900170210           new_TGCsgp = TGCsgp;
043000170210           new_TGCsgd = TGCsgd;
043100170210           new_TGCsg1 = TGCsg1;
043200170210           new_TGCsg2 = TGCsg2;
043300170210           new_TGCsg3 = TGCsg3;
043400170210           new_TGCst1 = TGCst1;
043500170210           new_TGCst2 = TGCst2;
043600170210           new_TGCst3 = TGCst3;
043700170210           new_TGCstm = TGCstm;
043800170210           new_TGCrpv = TGCrpv;
043900170210           new_TGCfaf = TGCfaf;
044000170210           new_TGCsgf = TGCsgf;
044100170210           new_TGCggr = TGCggr;
044200170210           new_TGCtcm = TGCtcm;
044300170210           new_TGCtfg = TGCtfg;
044400170210           new_TGCduv = oggi;
044500170210           new_TGCdtr = oggi;
044600170210           write TITGC000;
044700170210           reade (WFAksc:WFActrv:WFAprgv) TITGC01L;
044800170210         ENDDO;
044900170210
045000170210       //?Testata tariffe
045100170210         chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
045200170210         IF  %found(TNTAM04L);
045300170210           clear TNTAM000;
045400170210           new_TAMksc = TAMksc;
045500170210           new_TAMctr = TAMctr;
045600170210           new_TAMprg = progressivo;
045700170210           new_TAMddt = decorrenza;
045800170210           new_TAMdst = scadenza;
045900170210           new_TAMduv = oggi;
046000170210           new_TAMdcv = TAMdcv;
046100170210           new_TAMdif = TAMdif;
046200170210           new_TAMfli = TAMfli;
046300170210           new_TAMpri = TAMpri;
046400170210           new_TAMfmp = TAMfmp;
046500170210           new_TAMlrc = TAMlrc;
046600170210           new_TAMlas = TAMlas;
046700170210           new_TAMpag = TAMpag;
046800170210           new_TAMmpa = TAMmpa;
046900170210           new_TAMpam = TAMpam;
047000170210           new_TAMmpm = TAMmpm;
047100170210           new_TAMtin = TAMtin;
047200170210           new_TAMkpm = TAMkpm;
047300170210           new_TAMarl = TAMarl;
047400170210           new_TAMarf = TAMarf;
047500170210           new_TAMaro = TAMaro;
047600170210           new_TAMrat = TAMrat;
047700170210           new_TAMrct = TAMrct;
047800170210           new_TAMpri = TAMpri;
047900170210           new_TAMsc2 = TAMsc2;
048000170210           new_TAMeds = TAMeds;
048100170210           new_TAMars = TAMars;
048200170210           new_TAMata = TAMata;
048300170210           new_TAMftp = TAMftp;
048400170210           new_TAMbap = TAMbap;
048500170210           new_TAMtsp = TAMtsp;
048600170210           new_TAMfie = TAMfie;
048700170210           new_TAMfis = TAMfis;
048800170210           new_TAMsis = TAMsis;
048900170210           new_TAMfvd = TAMfvd;
049000170210           new_TAMfts = TAMfts;
049100170210           new_TAMnas = TAMnas;
049200170210           new_TAMnot = TAMnot;
049300170210           new_TAMftm = TAMftm;
049400170210           new_TAMgca = TAMgca;
049500170210           new_TAMgga = TAMgga;
049600170210           new_TAMgma = TAMgma;
049700170210           new_TAMgva = TAMgva;
049800170210           new_TAMbam = TAMbam;
049900170210           new_TAMtpr = TAMtpr;
050000170210           new_TAMflo = TAMflo;
050100170210           new_TAMdat = TAMdat;
050200170210           new_TAMdtr = oggi;
050300170210           write TNTAM000;
050400170210         ENDIF;
050500170210
050600170210       //?Storico variazioni
050700170210         clear TITAV000;
050800170210         TAVksc = new_TAMksc;
050900170210         TAVctr = new_TAMctr;
051000170210         TAVprg = new_TAMprg;
051100170210         TAVtrd = 'TES';
051200170210         TAVcta = 'D';
051300170210         TAVdav = oggi;
051400170210         TAVorv = %dec(%time());
051500170210         TAVnoj = 'DV-DPD';
051600170210         TAVpru = 'BATCH';
051700170210         TAVdtr = oggi;
051800170210         write TITAV000;
051900170210
052000170210       //?File di WORK
052100170210         chain ('T':WFAksc:WFActrv:WFAprgv) WFAUD01L;
052200170210         IF  %found(WFAUD01L);
052300170210           WFAddtn = new_TAMddt;
052400170210           WFAdstn = new_TAMdst;
052500170210           WFAprgn = new_TAMprg;
052600170220           WFAela  = 'S';
052700170210           update WFAUD000;
052800170210         ENDIF;
052900170210
053000170210       ENDSR;
053100170210
053200170210       //--------------------------------------------------------------
053300170210       //?Aggiorno il progressivo attuale.
053400170210       //--------------------------------------------------------------
053500170210       BEGSR AggPrgAttuale;
053600170210
053700170210       //?Testata tariffe
053800170210         chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
053900170210         IF  %found(TNTAM04L);
054000170210           TAMdst = 20170228;
054100170210           TAMduv = oggi;
054200170210           TAMdtr = oggi;
054300170210           clear TAMftr;
054400170210           update TNTAM04;
054500170210         ENDIF;
054600170210
054700170210       //?Storico variazioni
054800170210         clear TITAV000;
054900170210         TAVksc = TAMksc;
055000170210         TAVctr = TAMctr;
055100170210         TAVprg = TAMprg;
055200170210         TAVtrd = 'TES';
055300170210         TAVcta = 'M';
055400170210         TAVdav = oggi;
055500170210         TAVorv = %dec(%time());
055600170213         TAVnoj = 'DV-DPD';
055700170210         TAVpru = 'BATCH';
055800170210         TAVdtr = oggi;
055900170210         write TITAV000;
056000170210
056100170210       ENDSR;
056200170210
056300170210       //--------------------------------------------------------------
056400170210       //?Aggiorno le Tariffe Particolari.
056500170210       //--------------------------------------------------------------
056600170210       BEGSR AggTarParticolari;
056700170210
056800170210       //?Se non c'era la testata relativa al dirottamento o al fuori misura
056900170210       //?devo scriverle uguali alla tariffa di cartello
057000170210       //?e ormai che ci sono scrivo anche il dettaglio della tariffa particolare
057100170210         IF  WFAdir = 'A' or WFAfuo = 'A';
057200170210           IF  WFAdir = 'A';
057300170210             exsr NewTestataDirottamento;
057400170210             exsr NewDettaglioDirottamento;
057500170210           ENDIF;
057600170210           IF  WFAfuo = 'A';
057700170210             exsr NewTestataFuoriMisura;
057800170210             exsr NewDettaglioFuoriMisura;
057900170210           ENDIF;
058000170210         //?Scrivo anche lo storico variazioni
058100170210           exsr StoricoTarParticolari;
058200170210         ENDIF;
058300170210
058400170213       //?Se c'era la testata relativa al dirottamento o al fuori misura
058500170213       //?ma non il relativo dettaglio, lo devo scrivere
058600170210         IF  WFAdir = 'T' or WFAfuo = 'T';
058700170210           IF  WFAdir = 'T';
058800170210             exsr NewDettaglioDirottamento;
058900170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
059000170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPT01L;
059100170213             IF  %found(TITPT01L);
059200170213               TPTduv = oggi;
059300170213               clear TPTftr;
059400170213               TPTdtr = oggi;
059500170213               update TITPT01;
059600170213             ENDIF;
059700170210           ENDIF;
059800170210           IF  WFAfuo = 'T';
059900170210             exsr NewDettaglioFuoriMisura;
060000170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
060100170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPD01L;
060200170213             IF  %found(TITPT01L);
060300170213               TPTduv = oggi;
060400170213               clear TPTftr;
060500170213               TPTdtr = oggi;
060600170213               update TITPT01;
060700170213             ENDIF;
060800170210           ENDIF;
060900170210         //?Scrivo anche lo storico variazioni
061000170210           exsr StoricoTarParticolari;
061100170210         ENDIF;
061200170210
061300170210       //?Se la tariffa particolare è da variare l'aggancio e la modifico
061400170210         IF  WFAdir = 'M' or WFAfuo = 'M';
061500170213         //?Dirottamento
061600170210           IF  WFAdir = 'M';
061700170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPD01L;
061800170210             IF  %found(TITPD01L);
061900170210               TPDsgl = 9999999;
062000170210               TPDitr = 6;
062100170213               clear TPDftr;
062200170213               TPDdtr = oggi;
062300170210               update TITPD01;
062400170210             ENDIF;
062500170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
062600170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPT01L;
062700170213             IF  %found(TITPT01L);
062800170213               TPTduv = oggi;
062900170213               clear TPTftr;
063000170213               TPTdtr = oggi;
063100170213               update TITPT01;
063200170213             ENDIF;
063300170210           ENDIF;
063400170213         //?Fuori Misura
063500170210           IF  WFAfuo = 'M';
063600170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPD01L;
063700170210             IF  %found(TITPD01L);
063800170210               TPDsgl = 9999999;
063900170210               TPDitr = 28,922;
064000170213               clear TPDftr;
064100170213               TPDdtr = oggi;
064200170210               update TITPD01;
064300170210             ENDIF;
064400170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
064500170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPT01L;
064600170213             IF  %found(TITPT01L);
064700170213               TPTduv = oggi;
064800170213               clear TPTftr;
064900170213               TPTdtr = oggi;
065000170213               update TITPT01;
065100170213             ENDIF;
065200170210           ENDIF;
065300170210         //?Scrivo anche lo storico variazioni
065400170210           exsr StoricoTarParticolari;
065500170210         ENDIF;
065600170210
065700170210       //?File di WORK
065800170213         chain ('T':WFAksc:WFActrv:WFAprgv) WFAUD01L;
065900170210         IF  %found(WFAUD01L);
066000170210           WFAddtn = WFAddtv;
066100170210           WFAdstn = WFAdstv;
066200170210           WFAprgn = progressivo;
066300170220           WFAela  = 'S';
066400170210           update WFAUD000;
066500170210         ENDIF;
066600170210
066700170210       ENDSR;
066800170210
066900170210       //--------------------------------------------------------------
067000170210       //?Scrivo la nuova testata del dirottamento da cartello.
067100170210       //--------------------------------------------------------------
067200170210       BEGSR NewTestataDirottamento;
067300170210
067400170210         clear TITPT000;
067500170210         new_TPTksc = WFAksc;
067600170210         new_TPTctr = WFActrv;
067700170210         new_TPTprg = progressivo;
067800170210         new_TPTduv = oggi;
067900170210         new_TPTftc = dir_TPTftc;
068000170210         new_TPTtpg = dir_TPTtpg;
068100170210         new_TPTarl = dir_TPTarl;
068200170210         new_TPTarf = dir_TPTarf;
068300170210         new_TPTaro = dir_TPTaro;
068400170210         new_TPTrpv = dir_TPTrpv;
068500170210         new_TPTvlm = dir_TPTvlm;
068600170210         new_TPTvvm = dir_TPTvvm;
068700170210         new_TPTfvm = dir_TPTfvm;
068800170210         new_TPTtma = dir_TPTtma;
068900170210         new_TPTapl = dir_TPTapl;
069000170210         new_TPTdtr = oggi;
069100170210         new_TPTdpb = dir_TPTdpb;
069200170210         new_TPTdit = oggi;
069300170210         new_TPTflo = dir_TPTflo;
069400170210         write TITPT000;
069500170210
069600170210       ENDSR;
069700170210
069800170210       //--------------------------------------------------------------
069900170210       //?Scrivo la nuova testata del fuori misura da cartello.
070000170210       //--------------------------------------------------------------
070100170210       BEGSR NewTestataFuoriMisura;
070200170210
070300170210         clear TITPT000;
070400170210         new_TPTksc = WFAksc;
070500170210         new_TPTctr = WFActrv;
070600170210         new_TPTprg = progressivo;
070700170215         new_TPTduv = oggi;
070800170213         new_TPTftc = fuo_TPTftc;
070900170213         new_TPTtpg = fuo_TPTtpg;
071000170213         new_TPTarl = fuo_TPTarl;
071100170213         new_TPTarf = fuo_TPTarf;
071200170213         new_TPTaro = fuo_TPTaro;
071300170213         new_TPTrpv = fuo_TPTrpv;
071400170213         new_TPTvlm = fuo_TPTvlm;
071500170213         new_TPTvvm = fuo_TPTvvm;
071600170213         new_TPTfvm = fuo_TPTfvm;
071700170213         new_TPTtma = fuo_TPTtma;
071800170213         new_TPTapl = fuo_TPTapl;
071900170210         new_TPTdtr = oggi;
072000170213         new_TPTdpb = fuo_TPTdpb;
072100170210         new_TPTdit = oggi;
072200170213         new_TPTflo = fuo_TPTflo;
072300170210         write TITPT000;
072400170210
072500170210       ENDSR;
072600170210
072700170210       //--------------------------------------------------------------
072800170210       //?Scrivo il nuovo dettaglio del dirottamento.
072900170210       //--------------------------------------------------------------
073000170210       BEGSR NewDettaglioDirottamento;
073100170210
073200170210         clear TITPD000;
073300170210         new_TPDksc = WFAksc;
073400170210         new_TPDctr = WFActrv;
073500170210         new_TPDprg = progressivo;
073600170210         new_TPDftc = dirottamento;
073700170210         new_TPDcts = 'IT';
073800170214         new_TPDorp = '  A 9';
073900170210         new_TPDsgl = 9999999;
074000170210         new_TPDitr = 6;
074100170210         new_TPDdtr = oggi;
074200170210         write TITPD000;
074300170210
074400170210       ENDSR;
074500170210
074600170210       //--------------------------------------------------------------
074700170210       //?Scrivo il nuovo dettaglio del fuori misura.
074800170210       //--------------------------------------------------------------
074900170210       BEGSR NewDettaglioFuoriMisura;
075000170210
075100170210         clear TITPD000;
075200170210         new_TPDksc = WFAksc;
075300170210         new_TPDctr = WFActrv;
075400170210         new_TPDprg = progressivo;
075500170210         new_TPDftc = fuorimisura;
075600170210         new_TPDcts = 'IT';
075700170214         new_TPDorp = '  A 9';
075800170210         new_TPDsgl = 9999999;
075900170210         new_TPDitr = 28,922;
076000170210         new_TPDdtr = oggi;
076100170210         write TITPD000;
076200170210
076300170210       ENDSR;
076400170210
076500170210       //--------------------------------------------------------------
076600170210       //?Scrivo storico variazione per aggiunta tariffe paricolari.
076700170210       //--------------------------------------------------------------
076800170210       BEGSR StoricoTarParticolari;
076900170210
077000170210         IF  WFAdir <> *blanks;
077100170210           clear TITAV000;
077200170210           TAVksc = WFAksc;
077300170210           TAVctr = WFActrv;
077400170210           TAVprg = progressivo;
077500170210           TAVtrd = 'PAR';
077600170210           TAVftc = dirottamento;
077700170213           SELECT;
077800170213           WHEN  NewPrg or WFAdir = 'A';
077900170213             TAVcta = 'I';
078000170213           WHEN  WFAdir = 'M' or WFAdir = 'T';
078100170213             TAVcta = 'M';
078200170213           ENDSL;
078300170210           TAVdav = oggi;
078400170210           TAVorv = %dec(%time());
078500170213           TAVnoj = 'DV-DPD';
078600170210           TAVpru = 'BATCH';
078700170210           TAVdtr = oggi;
078800170210           write TITAV000;
078900170210         ENDIF;
079000170210
079100170210         IF  WFAfuo <> *blanks;
079200170210           clear TITAV000;
079300170210           TAVksc = WFAksc;
079400170210           TAVctr = WFActrv;
079500170210           TAVprg = progressivo;
079600170210           TAVtrd = 'PAR';
079700170210           TAVftc = fuorimisura;
079800170213           SELECT;
079900170214           WHEN  NewPrg or WFAfuo = 'A';
080000170213             TAVcta = 'I';
080100170214           WHEN  WFAfuo = 'M' or WFAfuo = 'T';
080200170213             TAVcta = 'M';
080300170213           ENDSL;
080400170210           TAVdav = oggi;
080500170210           TAVorv = %dec(%time());
080600170213           TAVnoj = 'DV-DPD';
080700170210           TAVpru = 'BATCH';
080800170210           TAVdtr = oggi;
080900170210           write TITAV000;
081000170210         ENDIF;
081100170210
081200170210       ENDSR;
081300170210
081400170210       //--------------------------------------------------------------
081500170210       //?Operazioni finali.
081600170210       //--------------------------------------------------------------
081700170210       BEGSR RoutEnd;
081800170210
081900170210         *inLR = *on;
082000170210         return;
082100170210
082200170210       ENDSR;
082300170210
082400170210      /end-free
