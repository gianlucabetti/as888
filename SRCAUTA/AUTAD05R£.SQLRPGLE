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
020600170228         //?Se la tariffa che sto leggendo ha scadenza < 01/04/2017
020700170228         //?devo creare un nuovo progressivo con decorrenza 01/04/2017 e scadenza 31/12/2017
020800170228           IF  WFAdstv < 20170401;
020900170228             decorrenza = 20170401;
021000170210             scadenza = 20171231;
021100170210             progressivo = WFAprgv + 1;
021200170210           //?Scrivo un nuovo progressivo
021300170210             exsr NewProgressivo;
021400170210           ENDIF;
021500170210
021600170228         //?Se la tariffa che sto leggendo ha data decorrenza < 01/04/2017 e
021700170228         //?data scadenza > 01/04/2017
021800170228         //?devo creare un nuovo progressivo con decorrenza 01/04/2017
021900170210         //?e scadenza = data scadenza tariffa
022000170228         //?e il progressivo che sto leggendo deve averea data scadenza = 31/03/2017
022100170228           IF  WFAddtv < 20170401 and WFAdstv > 20170401 ;
022200170210           //?Aggiorno il progressivo attuale
022300170210             exsr AggPrgAttuale;
022400170210           //?Scrivo un nuovo progressivo
022500170228             decorrenza = 20170401;
022600170210             scadenza = WFAdstv ;
022700170210             progressivo = WFAprgv + 1;
022800170210             exsr NewProgressivo;
022900170210           ENDIF;
023000170210
023100170228         //?Se la tariffa che sto leggendo ha data decorrenza = 01/04/2017
023200170210         //?devo aggiornare/scrivere solo le tariffe particolari
023300170228           IF  WFAddtv = 20170401;
023400170210           //?Imposto il progressivo attuale
023500170210             progressivo = WFAprgv;
023600170210             exsr AggTarParticolari;
023700170210           ENDIF;
023800170210
023900170228         //?Se la tariffa che sto leggendo ha data decorrenza > 01/04/2017
024000170220         //?non la elaboro
024100170228           IF  WFAddtv > 20170401;
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
046800170313           new_TAMppa = TAMppa;
046900170210           new_TAMmpa = TAMmpa;
047000170210           new_TAMpam = TAMpam;
047100170210           new_TAMmpm = TAMmpm;
047200170210           new_TAMtin = TAMtin;
047300170210           new_TAMkpm = TAMkpm;
047400170210           new_TAMarl = TAMarl;
047500170210           new_TAMarf = TAMarf;
047600170210           new_TAMaro = TAMaro;
047700170210           new_TAMrat = TAMrat;
047800170210           new_TAMrct = TAMrct;
047900170210           new_TAMpri = TAMpri;
048000170210           new_TAMsc2 = TAMsc2;
048100170210           new_TAMeds = TAMeds;
048200170210           new_TAMars = TAMars;
048300170210           new_TAMata = TAMata;
048400170210           new_TAMftp = TAMftp;
048500170210           new_TAMbap = TAMbap;
048600170210           new_TAMtsp = TAMtsp;
048700170210           new_TAMfie = TAMfie;
048800170210           new_TAMfis = TAMfis;
048900170210           new_TAMsis = TAMsis;
049000170210           new_TAMfvd = TAMfvd;
049100170210           new_TAMfts = TAMfts;
049200170210           new_TAMnas = TAMnas;
049300170210           new_TAMnot = TAMnot;
049400170210           new_TAMftm = TAMftm;
049500170210           new_TAMgca = TAMgca;
049600170210           new_TAMgga = TAMgga;
049700170210           new_TAMgma = TAMgma;
049800170210           new_TAMgva = TAMgva;
049900170210           new_TAMbam = TAMbam;
050000170210           new_TAMtpr = TAMtpr;
050100170210           new_TAMflo = TAMflo;
050200170210           new_TAMdat = TAMdat;
050300170210           new_TAMdtr = oggi;
050400170210           write TNTAM000;
050500170210         ENDIF;
050600170210
050700170210       //?Storico variazioni
050800170210         clear TITAV000;
050900170210         TAVksc = new_TAMksc;
051000170210         TAVctr = new_TAMctr;
051100170210         TAVprg = new_TAMprg;
051200170210         TAVtrd = 'TES';
051300170210         TAVcta = 'D';
051400170210         TAVdav = oggi;
051500170210         TAVorv = %dec(%time());
051600170306         TAVnoj = 'DV-2749';
051700170210         TAVpru = 'BATCH';
051800170210         TAVdtr = oggi;
051900170210         write TITAV000;
052000170210
052100170210       //?File di WORK
052200170210         chain ('T':WFAksc:WFActrv:WFAprgv) WFAUD01L;
052300170210         IF  %found(WFAUD01L);
052400170210           WFAddtn = new_TAMddt;
052500170210           WFAdstn = new_TAMdst;
052600170210           WFAprgn = new_TAMprg;
052700170220           WFAela  = 'S';
052800170210           update WFAUD000;
052900170210         ENDIF;
053000170210
053100170210       ENDSR;
053200170210
053300170210       //--------------------------------------------------------------
053400170210       //?Aggiorno il progressivo attuale.
053500170210       //--------------------------------------------------------------
053600170210       BEGSR AggPrgAttuale;
053700170210
053800170210       //?Testata tariffe
053900170210         chain (WFAksc:WFActrv:WFAprgv) TNTAM04L;
054000170210         IF  %found(TNTAM04L);
054100170228           TAMdst = 20170331;
054200170210           TAMduv = oggi;
054300170210           TAMdtr = oggi;
054400170210           clear TAMftr;
054500170210           update TNTAM04;
054600170210         ENDIF;
054700170210
054800170210       //?Storico variazioni
054900170210         clear TITAV000;
055000170210         TAVksc = TAMksc;
055100170210         TAVctr = TAMctr;
055200170210         TAVprg = TAMprg;
055300170210         TAVtrd = 'TES';
055400170210         TAVcta = 'M';
055500170210         TAVdav = oggi;
055600170210         TAVorv = %dec(%time());
055700170306         TAVnoj = 'DV-2749';
055800170210         TAVpru = 'BATCH';
055900170210         TAVdtr = oggi;
056000170210         write TITAV000;
056100170210
056200170210       ENDSR;
056300170210
056400170210       //--------------------------------------------------------------
056500170210       //?Aggiorno le Tariffe Particolari.
056600170210       //--------------------------------------------------------------
056700170210       BEGSR AggTarParticolari;
056800170210
056900170210       //?Se non c'era la testata relativa al dirottamento o al fuori misura
057000170210       //?devo scriverle uguali alla tariffa di cartello
057100170210       //?e ormai che ci sono scrivo anche il dettaglio della tariffa particolare
057200170210         IF  WFAdir = 'A' or WFAfuo = 'A';
057300170210           IF  WFAdir = 'A';
057400170210             exsr NewTestataDirottamento;
057500170210             exsr NewDettaglioDirottamento;
057600170210           ENDIF;
057700170210           IF  WFAfuo = 'A';
057800170210             exsr NewTestataFuoriMisura;
057900170210             exsr NewDettaglioFuoriMisura;
058000170210           ENDIF;
058100170210         //?Scrivo anche lo storico variazioni
058200170210           exsr StoricoTarParticolari;
058300170210         ENDIF;
058400170210
058500170213       //?Se c'era la testata relativa al dirottamento o al fuori misura
058600170213       //?ma non il relativo dettaglio, lo devo scrivere
058700170210         IF  WFAdir = 'T' or WFAfuo = 'T';
058800170210           IF  WFAdir = 'T';
058900170210             exsr NewDettaglioDirottamento;
059000170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
059100170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPT01L;
059200170213             IF  %found(TITPT01L);
059300170213               TPTduv = oggi;
059400170213               clear TPTftr;
059500170213               TPTdtr = oggi;
059600170213               update TITPT01;
059700170213             ENDIF;
059800170210           ENDIF;
059900170210           IF  WFAfuo = 'T';
060000170210             exsr NewDettaglioFuoriMisura;
060100170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
060200170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPD01L;
060300170213             IF  %found(TITPT01L);
060400170213               TPTduv = oggi;
060500170213               clear TPTftr;
060600170213               TPTdtr = oggi;
060700170213               update TITPT01;
060800170213             ENDIF;
060900170210           ENDIF;
061000170210         //?Scrivo anche lo storico variazioni
061100170210           exsr StoricoTarParticolari;
061200170210         ENDIF;
061300170210
061400170210       //?Se la tariffa particolare è da variare l'aggancio e la modifico
061500170210         IF  WFAdir = 'M' or WFAfuo = 'M';
061600170213         //?Dirottamento
061700170210           IF  WFAdir = 'M';
061800170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPD01L;
061900170210             IF  %found(TITPD01L);
062000170210               TPDsgl = 9999999;
062100170210               TPDitr = 6;
062200170213               clear TPDftr;
062300170213               TPDdtr = oggi;
062400170210               update TITPD01;
062500170210             ENDIF;
062600170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
062700170213             chain (WFAksc:WFActrv:progressivo:dirottamento) TITPT01L;
062800170213             IF  %found(TITPT01L);
062900170213               TPTduv = oggi;
063000170213               clear TPTftr;
063100170213               TPTdtr = oggi;
063200170213               update TITPT01;
063300170213             ENDIF;
063400170210           ENDIF;
063500170213         //?Fuori Misura
063600170210           IF  WFAfuo = 'M';
063700170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPD01L;
063800170210             IF  %found(TITPD01L);
063900170210               TPDsgl = 9999999;
064000170210               TPDitr = 28,922;
064100170213               clear TPDftr;
064200170213               TPDdtr = oggi;
064300170210               update TITPD01;
064400170210             ENDIF;
064500170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
064600170213             chain (WFAksc:WFActrv:progressivo:fuorimisura) TITPT01L;
064700170213             IF  %found(TITPT01L);
064800170213               TPTduv = oggi;
064900170213               clear TPTftr;
065000170213               TPTdtr = oggi;
065100170213               update TITPT01;
065200170213             ENDIF;
065300170210           ENDIF;
065400170210         //?Scrivo anche lo storico variazioni
065500170210           exsr StoricoTarParticolari;
065600170210         ENDIF;
065700170210
065800170210       //?File di WORK
065900170213         chain ('T':WFAksc:WFActrv:WFAprgv) WFAUD01L;
066000170210         IF  %found(WFAUD01L);
066100170210           WFAddtn = WFAddtv;
066200170210           WFAdstn = WFAdstv;
066300170210           WFAprgn = progressivo;
066400170220           WFAela  = 'S';
066500170210           update WFAUD000;
066600170210         ENDIF;
066700170210
066800170210       ENDSR;
066900170210
067000170210       //--------------------------------------------------------------
067100170210       //?Scrivo la nuova testata del dirottamento da cartello.
067200170210       //--------------------------------------------------------------
067300170210       BEGSR NewTestataDirottamento;
067400170210
067500170210         clear TITPT000;
067600170210         new_TPTksc = WFAksc;
067700170210         new_TPTctr = WFActrv;
067800170210         new_TPTprg = progressivo;
067900170210         new_TPTduv = oggi;
068000170210         new_TPTftc = dir_TPTftc;
068100170210         new_TPTtpg = dir_TPTtpg;
068200170210         new_TPTarl = dir_TPTarl;
068300170210         new_TPTarf = dir_TPTarf;
068400170210         new_TPTaro = dir_TPTaro;
068500170210         new_TPTrpv = dir_TPTrpv;
068600170210         new_TPTvlm = dir_TPTvlm;
068700170210         new_TPTvvm = dir_TPTvvm;
068800170210         new_TPTfvm = dir_TPTfvm;
068900170210         new_TPTtma = dir_TPTtma;
069000170210         new_TPTapl = dir_TPTapl;
069100170210         new_TPTdtr = oggi;
069200170210         new_TPTdpb = dir_TPTdpb;
069300170210         new_TPTdit = oggi;
069400170210         new_TPTflo = dir_TPTflo;
069500170210         write TITPT000;
069600170210
069700170210       ENDSR;
069800170210
069900170210       //--------------------------------------------------------------
070000170210       //?Scrivo la nuova testata del fuori misura da cartello.
070100170210       //--------------------------------------------------------------
070200170210       BEGSR NewTestataFuoriMisura;
070300170210
070400170210         clear TITPT000;
070500170210         new_TPTksc = WFAksc;
070600170210         new_TPTctr = WFActrv;
070700170210         new_TPTprg = progressivo;
070800170215         new_TPTduv = oggi;
070900170213         new_TPTftc = fuo_TPTftc;
071000170213         new_TPTtpg = fuo_TPTtpg;
071100170213         new_TPTarl = fuo_TPTarl;
071200170213         new_TPTarf = fuo_TPTarf;
071300170213         new_TPTaro = fuo_TPTaro;
071400170213         new_TPTrpv = fuo_TPTrpv;
071500170213         new_TPTvlm = fuo_TPTvlm;
071600170213         new_TPTvvm = fuo_TPTvvm;
071700170213         new_TPTfvm = fuo_TPTfvm;
071800170213         new_TPTtma = fuo_TPTtma;
071900170213         new_TPTapl = fuo_TPTapl;
072000170210         new_TPTdtr = oggi;
072100170213         new_TPTdpb = fuo_TPTdpb;
072200170210         new_TPTdit = oggi;
072300170213         new_TPTflo = fuo_TPTflo;
072400170210         write TITPT000;
072500170210
072600170210       ENDSR;
072700170210
072800170210       //--------------------------------------------------------------
072900170210       //?Scrivo il nuovo dettaglio del dirottamento.
073000170210       //--------------------------------------------------------------
073100170210       BEGSR NewDettaglioDirottamento;
073200170210
073300170210         clear TITPD000;
073400170210         new_TPDksc = WFAksc;
073500170210         new_TPDctr = WFActrv;
073600170210         new_TPDprg = progressivo;
073700170210         new_TPDftc = dirottamento;
073800170210         new_TPDcts = 'IT';
073900170214         new_TPDorp = '  A 9';
074000170210         new_TPDsgl = 9999999;
074100170210         new_TPDitr = 6;
074200170210         new_TPDdtr = oggi;
074300170210         write TITPD000;
074400170210
074500170210       ENDSR;
074600170210
074700170210       //--------------------------------------------------------------
074800170210       //?Scrivo il nuovo dettaglio del fuori misura.
074900170210       //--------------------------------------------------------------
075000170210       BEGSR NewDettaglioFuoriMisura;
075100170210
075200170210         clear TITPD000;
075300170210         new_TPDksc = WFAksc;
075400170210         new_TPDctr = WFActrv;
075500170210         new_TPDprg = progressivo;
075600170210         new_TPDftc = fuorimisura;
075700170210         new_TPDcts = 'IT';
075800170214         new_TPDorp = '  A 9';
075900170210         new_TPDsgl = 9999999;
076000170210         new_TPDitr = 28,922;
076100170210         new_TPDdtr = oggi;
076200170210         write TITPD000;
076300170210
076400170210       ENDSR;
076500170210
076600170210       //--------------------------------------------------------------
076700170210       //?Scrivo storico variazione per aggiunta tariffe paricolari.
076800170210       //--------------------------------------------------------------
076900170210       BEGSR StoricoTarParticolari;
077000170210
077100170210         IF  WFAdir <> *blanks;
077200170210           clear TITAV000;
077300170210           TAVksc = WFAksc;
077400170210           TAVctr = WFActrv;
077500170210           TAVprg = progressivo;
077600170210           TAVtrd = 'PAR';
077700170210           TAVftc = dirottamento;
077800170213           SELECT;
077900170213           WHEN  NewPrg or WFAdir = 'A';
078000170213             TAVcta = 'I';
078100170213           WHEN  WFAdir = 'M' or WFAdir = 'T';
078200170213             TAVcta = 'M';
078300170213           ENDSL;
078400170210           TAVdav = oggi;
078500170210           TAVorv = %dec(%time());
078600170306           TAVnoj = 'DV-2749';
078700170210           TAVpru = 'BATCH';
078800170210           TAVdtr = oggi;
078900170210           write TITAV000;
079000170210         ENDIF;
079100170210
079200170210         IF  WFAfuo <> *blanks;
079300170210           clear TITAV000;
079400170210           TAVksc = WFAksc;
079500170210           TAVctr = WFActrv;
079600170210           TAVprg = progressivo;
079700170210           TAVtrd = 'PAR';
079800170210           TAVftc = fuorimisura;
079900170213           SELECT;
080000170214           WHEN  NewPrg or WFAfuo = 'A';
080100170213             TAVcta = 'I';
080200170214           WHEN  WFAfuo = 'M' or WFAfuo = 'T';
080300170213             TAVcta = 'M';
080400170213           ENDSL;
080500170210           TAVdav = oggi;
080600170210           TAVorv = %dec(%time());
080700170306           TAVnoj = 'DV-2749';
080800170210           TAVpru = 'BATCH';
080900170210           TAVdtr = oggi;
081000170210           write TITAV000;
081100170210         ENDIF;
081200170210
081300170210       ENDSR;
081400170210
081500170210       //--------------------------------------------------------------
081600170210       //?Operazioni finali.
081700170210       //--------------------------------------------------------------
081800170210       BEGSR RoutEnd;
081900170210
082000170210         *inLR = *on;
082100170210         return;
082200170210
082300170210       ENDSR;
082400170210
082500170210      /end-free
