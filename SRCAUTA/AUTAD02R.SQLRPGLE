000100170210      //-------------------------------------------------------------------
000200170220      //?AUTAD02R - Da WF crea Nuove Tariffe Particolari DPD da TAR
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
006900170220     d Dettaglio       s               n   inz
007000170210     d Fine            s               n   inz(*off)
007100170213     d NewPrg          s               n   inz(*off)
007200170221     d TarFebMar       s               n   inz(*off)
007300170210     d wEoF            s               n   inz(*off)
007400170221     d wEoFTam         s               n   inz(*off)
007500170210
007600170210      // - Campi di comodo
007700170220     d Decorrenza      s                   like(TAMddt) inz
007800170210     d Dirottamento    s                   like(TPTftc) inz('* ')
007900170210     d FuoriMisura     s                   like(TPTftc) inz('F ')
008000170221     d Oggi            s              8s 0 inz
008100170220     d OldPrg          s                   like(TAMprg) inz
008200170221     d PrgInCorso      s                   like(TAMprg) inz
008300170221     d Scadenza        s                   like(TAMdst) inz
008400170221     d WFAdirInCorso   s                   like(WFAfuo) inz
008500170221     d WFAfuoInCorso   s                   like(WFAdir) inz
008600170210     d wprog           s                   like(TAMprg) inz
008700170221     d w_TPTksc        s                   like(TPTksc) inz
008800170221     d w_TPTctr        s                   like(TPTctr) inz
008900170220     d w_WFAdir        s                   like(WFAdir) inz
009000170220     d w_WFAfuo        s                   like(WFAfuo) inz
009100170210
009200170210      // - Campi di comodo per tariffa cartello DPD
009300170210     d ksc888          s                   like(TAMksc) inz(8888817)
009400170210     d ctr888          s                   like(TAMctr) inz(300)
009500170210     d prg888          s                   like(TAMprg) inz(17)
009600170210
009700170210      //---------------------------------------------------------------
009800170210      //?Definizione Procedure usate.
009900170210      //---------------------------------------------------------------
010000170210
010100170210      //---------------------------------------------------------------
010200170210      //?Definizione Prototipi.
010300170210      //---------------------------------------------------------------
010400170210      /copy gaitrasrc/srcprotopr,TIBS34R
010500170210
010600170210      //---------------------------------------------------------------
010700170210      //?Definizione key-list.
010800170210      //---------------------------------------------------------------
010900170210
011000170210      //---------------------------------------------------------------
011100170210      //?M A I N - L I N E
011200170210      //---------------------------------------------------------------
011300170210     c     *Entry        plist
011400170210     c                   parm                    kpjba
011500170210
011600170210      /free
011700170210
011800170210       //?Operazioni iniziali
011900170210       exsr RoutInz;
012000170210
012100170210       //?Elaborazione
012200170210       exsr Elabora;
012300170210
012400170210       //?Operazioni finali
012500170210       exsr RoutEnd;
012600170210
012700170210       //--------------------------------------------------------------
012800170210       //?Operazioni iniziali.
012900170210       //--------------------------------------------------------------
013000170210       BEGSR RoutInz;
013100170213
013200170213         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013300170210
013400170210       //?Imposto oggi
013500170210         Oggi = %dec(%date());
013600170210
013700170210       //?Reperimento dati job
013800170210         exsr DatiJob;
013900170210
014000170210       //?Cerco la testata della tariffa particolare dirottamento della cartello DPD
014100170210         exec sql
014200170210         SELECT TITPT00F.* into :TITPTdir
014300170210         FROM   TITPT00F
014400170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
014500170210                TPTftc = :dirottamento;
014600170210         IF  sqlcod <> 0;
014700170210           clear TITPTdir;
014800170210         ENDIF;
014900170210       //?Cerco la testata della tariffa particolare fuori misura della cartello DPD
015000170210         exec sql
015100170210         SELECT TITPT00F.* into :TITPTfuo
015200170210         FROM   TITPT00F
015300170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
015400170210                TPTftc = :fuorimisura;
015500170210         IF  sqlcod <> 0;
015600170210           clear TITPTfuo;
015700170210         ENDIF;
015800170210
015900170210       ENDSR;
016000170210
016100170210       //--------------------------------------------------------------
016200170210       //?Reperimento Dati del job (Utente/Operativi).
016300170210       //--------------------------------------------------------------
016400170210       BEGSR DatiJob;
016500170210
016600170210         in(E) §AzUte;
016700170210         IF  not %error;
016800170210           in(E) §DatiUte;
016900170210         ENDIF;
017000170210         IF  %error or RSut = *blanks;
017100170210           clear TIBS34ds;
017200170210           tibs34r(tibs34ds);
017300170210           in §AzUte;
017400170210           in §DatiUte;
017500170210         ENDIF;
017600170210
017700170210       ENDSR;
017800170210
017900170210       //--------------------------------------------------------------
018000170210       //?Elaborazione.
018100170210       //--------------------------------------------------------------
018200170210       BEGSR Elabora;
018300170217
018400170210       //?Dichiarazione cursore
018500170210         exec sql
018600170210         declare WRK cursor for
018700170210         select * from WFAUD00F
018800170228         where WFAtipo = 'T' and WFAela = ' ' and WFAddtv > 20170401
018900170213         order by WFAksc, WFActrv, WFAprgv;
019000170210
019100170210         //?Apertura del cursore
019200170210         exec sql
019300170210         open WRK;
019400170210
019500170210         IF  sqlcode < 0;
019600170210           exec sql close WRK;
019700170210           leavesr;
019800170210         ENDIF;
019900170210
020000170221         DOW  not Fine;
020100170210           exec sql
020200170210           fetch next from WRK into :WFAUDDS;
020300170210           IF  sqlcod = 100 or sqlcod < 0;
020400170221             Fine = *on;
020500170210             leave;
020600170210           ENDIF;
020700170221
020800170221           NewPrg = *off;
020900170221
021000170221         //?Se la tariffa che ho appena letto ha progressivo 0
021100170221         //?è l'unica tariffa presente quindi se da variare lo faccio subito
021200170221           IF  WFAprgv = 0 and (WFAdir <> *blanks or WFAfuo <> *blanks);
021300170221             wprog = WFAprgv;
021400170221             w_WFAdir = WFAdir;
021500170221             w_WFAfuo = WFAfuo;
021600170221             w_TPTksc = WFAksc;
021700170221             w_TPTctr = WFActrv;
021800170221             exsr AggTarParticolari;
021900170221           //?Aggiorno File di WORK
022000170221             chain ('T':WFAksc:WFActrv:WFAprgv) WFAUD01L;
022100170221             IF  %found(WFAUD01L);
022200170221               WFAddtn = WFAddtv;
022300170221               WFAdstn = WFAdstv;
022400170221               WFAprgn = WFAprgv;
022500170221               WFAela  = 'S';
022600170221               update WFAUD000;
022700170221             ENDIF;
022800170221             iter;
022900170221           ENDIF;
023000170220
023100170221         //?Se la tariffa non ha progressivo 0 devo cercare la tariffa in corso
023200170221           exsr TariffaInCorso;
023300170221         //?Controllo se la tariffa in corso è da aggiornare
023400170221           wprog = PrgInCorso;
023500170221           w_TPTksc = WFAksc;
023600170221           w_TPTctr = WFActrv;
023700170221           clear w_WFAdir;
023800170221           clear w_WFAfuo;
023900170220         //?Controllo la tariffa particolare '*' Dirottamento
024000170220           exsr ControllaDirottamento;
024100170220         //?Controllo la tariffa particolare 'F' Fuori Misura
024200170220           exsr ControllaFuoriMisura;
024300170221           WFAdirInCorso = w_WFAdir;
024400170221           WFAfuoInCorso = w_WFAfuo;
024500170221
024600170221         //?Se la tariffa che sto leggendo è da variare o devo variare la
024700170221         //?tariffa in corso leggo TNTAM
024800170221           IF  WFAdir <> *blanks or WFAfuo <> *blanks or
024900170221               WFAdirInCorso <> *blanks or WFAfuoInCorso <> *blanks;
025000170217
025100170221           //?Leggo il TNTAM
025200170221             wEoFTam = *off;
025300170221             setll (WFAksc:WFActrv) TNTAM04L;
025400170221             DOW  not wEoFTam;
025500170221               reade (WFAksc:WFActrv) TNTAM04L;
025600170221               IF  %eof(TNTAM04L);
025700170221                 wEoFTam = *on;
025800170221                 leave;
025900170221               ENDIF;
026000170221             //?Leggo fino al progressivo in corso
026100170221               IF  TAMprg < PrgInCorso;
026200170221                 wEoFTam = *on;
026300170221                 leave;
026400170221               ENDIF;
026500170221
026600170221             //?Se devo variare il progressivo che sto leggendo da WFAUD lo faccio subito
026700170221               IF  TAMprg = WFAprgv;
026800170221               //?Vario le Tariffe particolari
026900170221                 IF  WFAdir <> *blanks or WFAfuo <> *blanks;
027000170221                   wprog = TAMprg;
027100170221                   w_WFAdir = WFAdir;
027200170221                   w_WFAfuo = WFAfuo;
027300170221                   w_TPTksc = TAMksc;
027400170221                   w_TPTctr = TAMctr;
027500170221                   exsr AggTarParticolari;
027600170221                 ENDIF;
027700170221               //?Se la tariffa in corso è da variare ed è una tariffa a cavallo
027800170221               //?tra febbraio/marzo il progressivo che sto leggendo da WFAUD lo devo
027900170221               //?aumentare di 1
028000170221                 IF  TarFebMar and
028100170221                    (WFAdirInCorso <> *blanks or WFAfuoInCorso <> *blanks);
028200170221                 //?Deleto eventuali rcd annullati
028300170221                   exsr DeletaAnnullati;
028400170221                 //?Aumento il progressivo
028500170221                   exsr AumentaPrg;
028600170221                 ENDIF;
028700170221               //?Aggiorno File di WORK
028800170221                 chain ('T':TAMksc:TAMctr:WFAprgv) WFAUD01L;
028900170221                 IF  %found(WFAUD01L);
029000170221                   WFAddtn = TAMddt;
029100170221                   WFAdstn = TAMdst;
029200170221                   WFAprgn = TAMprg;
029300170221                   WFAela  = 'S';
029400170221                   update WFAUD000;
029500170221                 ENDIF;
029600170221                 iter;
029700170221               ENDIF;
029800170221
029900170221             //?Sto leggendo un progressivo diverso e non è quello in corso
030000170221               IF  TAMprg <> PrgInCorso;
030100170221               //?Controllo se è da aggiornare
030200170221                 wprog = TAMprg;
030300170221                 w_TPTksc = TAMksc;
030400170221                 w_TPTctr = TAMctr;
030500170221                 w_TPTksc = TAMksc;
030600170221                 w_TPTctr = TAMctr;
030700170221                 clear w_WFAdir;
030800170221                 clear w_WFAfuo;
030900170221               //?Controllo la tariffa particolare '*' Dirottamento
031000170221                 exsr ControllaDirottamento;
031100170221               //?Controllo la tariffa particolare 'F' Fuori Misura
031200170221                 exsr ControllaFuoriMisura;
031300170221               //?Se da variare lo faccio ora
031400170221                 IF  w_WFAdir <> *blanks or w_WFAfuo <> *blanks;
031500170221                   exsr AggTarParticolari;
031600170221                 ENDIF;
031700170221               //?Se la tariffa in corso è da variare ed è una tariffa a cavallo
031800170221               //?tra febbraio/marzo il progressivo che sto leggendo da TNTAM lo devo
031900170221               //?aumentare di 1
032000170221                 IF  TarFebMar and
032100170221                    (WFAdirInCorso <> *blanks or WFAfuoInCorso <> *blanks);
032200170221                 //?Deleto eventuali rcd annullati
032300170221                   exsr DeletaAnnullati;
032400170221                 //?Aumento il progressivo
032500170221                   exsr AumentaPrg;
032600170221                 ENDIF;
032700170221                 iter;
032800170221               ENDIF;
032900170221
033000170221             //?Sto leggendo il progressivo in corso
033100170221               IF  TAMprg = PrgInCorso;
033200170221               //?Se la tariffa in corso è da variare ed è una tariffa a cavallo
033300170221               //?tra febbraio/marzo devo far scadere il progressivo in corso a Febbraio
033400170221               //?e farne uno nuovo con decorrenza Marzo
033500170221                 IF  TarFebMar and
033600170221                    (WFAdirInCorso <> *blanks or WFAfuoInCorso <> *blanks);
033700170221                 //?Devo far scadere il progressivo che sto leggendo e crearne uno
033800170228                 //?nuovo con decorrenza 01/04/2017
033900170221                   exsr NewProgressivo;
034000170221                 //?Aggiorno le tariffe particolari
034100170221                   wprog = new_TAMprg;
034200170221                   w_WFAdir = WFAdirInCorso;
034300170221                   w_WFAfuo = WFAfuoInCorso;
034400170221                   w_TPTksc = TAMksc;
034500170221                   w_TPTctr = TAMctr;
034600170221                   exsr AggTarParticolari;
034700170221                 ENDIF;
034800170221               ENDIF;
034900170221             ENDDO;
035000170221
035100170221           ENDIF;
035200170221
035300170221         ENDDO;
035400170221
035500170221         exec sql
035600170221         close WRK;
035700170221
035800170221       ENDSR;
035900170220
036000170220       //--------------------------------------------------------------
036100170220       //?Cerco la tariffa in corso.
036200170220       //--------------------------------------------------------------
036300170221       BEGSR TariffaInCorso;
036400170220
036500170221         TarFebMar = *off;
036600170221         wEoF = *off;
036700170221         OldPrg = WFAprgv - 1;
036800170220
036900170220       //?Cerco il progressivo in corso
037000170221         DOW  not wEoF;
037100170221           chain(n) (WFAksc:WFActrv:OldPrg) TNTAM01L;
037200170221         //?Mi salvo qual è il progressivo in corso
037300170221           IF  %found(TNTAM01L) and
037400170228               old_TAMddt <= 20170401 and old_TAMdst >= 20170401;
037500170221             PrgInCorso = OldPrg;
037600170220           //?Mi memorizzo che tariffa in corso è a cavallo
037700170228             IF  old_TAMddt < 20170401 and old_TAMdst >= 20170401;
037800170221               TarFebMar = *on;
037900170220             ENDIF;
038000170221             wEoF = *on;
038100170220             leave;
038200170220           ENDIF;
038300170220          //?Se progressivo precedente a 0 vado via
038400170220           IF  OldPrg = 0;
038500170221             wEoF = *on;
038600170220             leave;
038700170220           ENDIF;
038800170220          //?leggo altro progressivo OLD
038900170221           OldPrg -= 1;
039000170220         ENDDO;
039100170220
039200170220       ENDSR;
039300170220
039400170220       //--------------------------------------------------------------
039500170220       //?Controllo tariffa particolare Dirottamento.
039600170220       //--------------------------------------------------------------
039700170220       BEGSR ControllaDirottamento;
039800170220
039900170220       //?se non trovo Testata Tariffa Particolare oppure è annullata
040000170220       //?IMPOSTO il campo relativo al dirottamento = 'A'
040100170221         chain(n) (w_TPTksc:w_TPTctr:wprog:Dirottamento) TITPT01L;
040200170220         IF  not %found(TITPT01L) or TPTatb <> *blanks;
040300170220           w_WFAdir = 'A';
040400170220           leavesr;
040500170220         ENDIF;
040600170220
040700170220       //?Vado sul dettaglio della Tariffa Particolare
040800170220         Dettaglio = *off;
040900170221         wEoF = *off;
041000170221         setll (w_TPTksc:w_TPTctr:wprog:Dirottamento) TITPD01L;
041100170221         reade(n) (w_TPTksc:w_TPTctr:wprog:Dirottamento) TITPD01L;
041200170221         DOW  not wEoF;
041300170220           IF  %eof(TITPD01L);
041400170221             wEoF = *on;
041500170220             leave;
041600170220           ENDIF;
041700170220         //?Se almeno 1 valorizato non devo fare niente
041800170220           IF  TPDatb = *blanks and TPDitr > 0;
041900170220             clear w_WFAdir;
042000170220             Dettaglio = *on;
042100170221             wEoF = *on;
042200170220             leave;
042300170220           ENDIF;
042400170220         //?Se dettaglio negato devo variarlo
042500170220       //?IMPOSTO il campo relativo al dirottamento = 'M'
042600170220           IF  TPDatb = *blanks and TPDitr = 0;
042700170220             w_WFAdir = 'M';
042800170220             Dettaglio = *on;
042900170220           ENDIF;
043000170221           reade(n) (w_TPTksc:w_TPTctr:wprog:Dirottamento) TITPD01L;
043100170220         ENDDO;
043200170220       //?Se non ho trovato il dettaglio
043300170220       //?IMPOSTO il campo relativo al dirottamento = 'T'
043400170220         IF  not Dettaglio;
043500170220           w_WFAdir = 'T';
043600170220         ENDIF;
043700170220
043800170220       ENDSR;
043900170220
044000170220       //--------------------------------------------------------------
044100170220       //?Controllo tariffa particolare Fuori Misura.
044200170220       //--------------------------------------------------------------
044300170220       BEGSR ControllaFuoriMisura;
044400170220
044500170220       //?se non trovo Testata Tariffa Particolare oppure è annullata
044600170220       //?IMPOSTO il campo relativo al fuori misura = 'A'
044700170221         chain(n) (w_TPTksc:w_TPTctr:wprog:FuoriMisura) TITPT01L;
044800170220         IF  not %found(TITPT01L) or TPTatb <> *blanks;
044900170221           w_WFAfuo = 'A';
045000170220           leavesr;
045100170220         ENDIF;
045200170220
045300170220       //?Vado sul dettaglio della Tariffa Particolare
045400170220         Dettaglio = *off;
045500170220         wEoF = *off;
045600170221         setll (w_TPTksc:w_TPTctr:wprog:FuoriMisura) TITPD01L;
045700170221         reade(n) (w_TPTksc:w_TPTctr:wprog:FuoriMisura) TITPD01L;
045800170220         DOW  not wEoF;
045900170220           IF  %eof(TITPD01L);
046000170220             wEoF = *on;
046100170220             leave;
046200170220           ENDIF;
046300170220         //?Se almeno 1 valorizato non devo fare niente
046400170220           IF  TPDatb = *blanks and TPDitr > 0;
046500170221             clear w_WFAfuo;
046600170220             Dettaglio = *on;
046700170220             wEoF = *on;
046800170220             leave;
046900170220           ENDIF;
047000170220         //?Se dettaglio negato devo variarlo
047100170220       //?IMPOSTO il campo relativo al fuori misura = 'M'
047200170220           IF  TPDatb = *blanks and TPDitr = 0;
047300170221             w_WFAfuo = 'M';
047400170220             Dettaglio = *on;
047500170220           ENDIF;
047600170221           reade(n) (w_TPTksc:w_TPTctr:wprog:FuoriMisura) TITPD01L;
047700170220         ENDDO;
047800170220       //?Se non ho trovato il dettaglio
047900170220       //?IMPOSTO il campo relativo al fuori misura = 'T'
048000170220         IF  not Dettaglio;
048100170221           w_WFAfuo = 'T';
048200170220         ENDIF;
048300170220
048400170220       ENDSR;
048500170220
048600170220       //--------------------------------------------------------------
048700170220       //?Aggiorno le Tariffe Particolari.
048800170220       //--------------------------------------------------------------
048900170220       BEGSR AggTarParticolari;
049000170220
049100170220       //?Se non c'era la testata relativa al dirottamento o al fuori misura
049200170220       //?devo scriverle uguali alla tariffa di cartello
049300170220       //?e ormai che ci sono scrivo anche il dettaglio della tariffa particolare
049400170220         IF  w_WFAdir = 'A' or w_WFAfuo = 'A';
049500170220           IF  w_WFAdir = 'A';
049600170220             exsr NewTestataDirottamento;
049700170220             exsr NewDettaglioDirottamento;
049800170220           ENDIF;
049900170220           IF  w_WFAfuo = 'A';
050000170220             exsr NewTestataFuoriMisura;
050100170220             exsr NewDettaglioFuoriMisura;
050200170220           ENDIF;
050300170220         //?Scrivo anche lo storico variazioni
050400170220           exsr StoricoTarParticolari;
050500170220         ENDIF;
050600170220
050700170220       //?Se c'era la testata relativa al dirottamento o al fuori misura
050800170220       //?ma non il relativo dettaglio, lo devo scrivere
050900170220         IF  w_WFAdir = 'T' or w_WFAfuo = 'T';
051000170220           IF  w_WFAdir = 'T';
051100170220             exsr NewDettaglioDirottamento;
051200170220         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
051300170221             chain (w_TPTksc:w_TPTctr:wprog:dirottamento) TITPT01L;
051400170220             IF  %found(TITPT01L);
051500170220               TPTduv = oggi;
051600170220               clear TPTftr;
051700170220               TPTdtr = oggi;
051800170220               update TITPT01;
051900170220             ENDIF;
052000170220           ENDIF;
052100170220           IF  w_WFAfuo = 'T';
052200170220             exsr NewDettaglioFuoriMisura;
052300170220         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
052400170221             chain (w_TPTksc:w_TPTctr:wprog:fuorimisura) TITPD01L;
052500170220             IF  %found(TITPT01L);
052600170220               TPTduv = oggi;
052700170220               clear TPTftr;
052800170220               TPTdtr = oggi;
052900170220               update TITPT01;
053000170220             ENDIF;
053100170220           ENDIF;
053200170220         //?Scrivo anche lo storico variazioni
053300170220           exsr StoricoTarParticolari;
053400170220         ENDIF;
053500170220
053600170220       //?Se la tariffa particolare è da variare l'aggancio e la modifico
053700170220         IF  w_WFAdir = 'M' or w_WFAfuo = 'M';
053800170220         //?Dirottamento
053900170220           IF  w_WFAdir = 'M';
054000170221             chain (w_TPTksc:w_TPTctr:wprog:dirottamento) TITPD01L;
054100170220             IF  %found(TITPD01L);
054200170220               TPDsgl = 9999999;
054300170220               TPDitr = 6;
054400170220               clear TPDftr;
054500170220               TPDdtr = oggi;
054600170220               update TITPD01;
054700170220             ENDIF;
054800170220         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
054900170221             chain (w_TPTksc:w_TPTctr:wprog:dirottamento) TITPT01L;
055000170220             IF  %found(TITPT01L);
055100170220               TPTduv = oggi;
055200170220               clear TPTftr;
055300170220               TPTdtr = oggi;
055400170220               update TITPT01;
055500170220             ENDIF;
055600170220           ENDIF;
055700170220         //?Fuori Misura
055800170220           IF  w_WFAfuo = 'M';
055900170221             chain (w_TPTksc:w_TPTctr:wprog:fuorimisura) TITPD01L;
056000170220             IF  %found(TITPD01L);
056100170220               TPDsgl = 9999999;
056200170220               TPDitr = 28,922;
056300170220               clear TPDftr;
056400170220               TPDdtr = oggi;
056500170220               update TITPD01;
056600170220             ENDIF;
056700170220         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
056800170221             chain (w_TPTksc:w_TPTctr:wprog:fuorimisura) TITPT01L;
056900170220             IF  %found(TITPT01L);
057000170220               TPTduv = oggi;
057100170220               clear TPTftr;
057200170220               TPTdtr = oggi;
057300170220               update TITPT01;
057400170220             ENDIF;
057500170220           ENDIF;
057600170220         //?Scrivo anche lo storico variazioni
057700170220           exsr StoricoTarParticolari;
057800170220         ENDIF;
057900170220
058000170220       ENDSR;
058100170221
058200170221       //--------------------------------------------------------------
058300170221       //?Aumento di 1 il progressivo.
058400170221       //--------------------------------------------------------------
058500170221       BEGSR AumentaPrg;
058600170221
058700170221       //?Dettaglio tariffario
058800170221         setll (TAMksc:TAMctr:TAMprg) TITAD04L;
058900170221         reade (TAMksc:TAMctr:TAMprg) TITAD04L;
059000170221         DOW  not %eof(TITAD04L);
059100170221           IF  TADatb = *blanks;
059200170221             TADprg += 1;
059300170221             clear TADftr;
059400170221             TADdtr = oggi;
059500170221             update TITAD04;
059600170221           ENDIF;
059700170221           reade (TAMksc:TAMctr:TAMprg) TITAD04L;
059800170221         ENDDO;
059900170221
060000170221       //?Testata tariffe particolari
060100170221         setll (TAMksc:TAMctr:TAMprg) TITPT01L;
060200170221         reade (TAMksc:TAMctr:TAMprg) TITPT01L;
060300170221         DOW  not %eof(TITPT01L);
060400170221           IF  TPTatb = *blanks;
060500170221             TPTprg += 1;
060600170221             TPTduv = oggi;
060700170221             clear TPTftr;
060800170221             TPTdtr = oggi;
060900170221             update TITPT01;
061000170221           ENDIF;
061100170221           reade (TAMksc:TAMctr:TAMprg) TITPT01L;
061200170221         ENDDO;
061300170221
061400170221       //?Dettaglio tariffe particolari
061500170221         setll (TAMksc:TAMctr:TAMprg) TITPD01L;
061600170221         reade (TAMksc:TAMctr:TAMprg) TITPD01L;
061700170221         DOW  not %eof(TITPD01L);
061800170221           IF  TPDatb = *blanks;
061900170221             TPDprg += 1;
062000170221             clear TPDftr;
062100170221             TPDdtr = oggi;
062200170221             update TITPD01;
062300170221           ENDIF;
062400170221           reade (TAMksc:TAMctr:TAMprg) TITPD01L;
062500170221         ENDDO;
062600170221
062700170221       //?Tariffe di giacenza
062800170221         setll (TAMksc:TAMctr:TAMprg) TITGC01L;
062900170221         reade (TAMksc:TAMctr:TAMprg) TITGC01L;
063000170221         DOW  not %eof(TITGC01L);
063100170221           IF  TGCatb = *blanks;
063200170221             TGCprg += 1;
063300170221             TGCduv = oggi;
063400170221             clear TGCftr;
063500170221             TGCdtr = oggi;
063600170221             update TITGC01;
063700170221           ENDIF;
063800170221           reade (TAMksc:TAMctr:TAMprg) TITGC01L;
063900170221         ENDDO;
064000170221
064100170221       //?Storico Variazioni
064200170221         setll (TAMksc:TAMctr:TAMprg) TITAV01L;
064300170221         reade (TAMksc:TAMctr:TAMprg) TITAV01L;
064400170221         DOW  not %eof(TITAV01L);
064500170221           IF  TAVatb = *blanks;
064600170221             TAVprg += 1;
064700170221             clear TAVftr;
064800170221             TAVdtr = oggi;
064900170221             update TITAV000;
065000170221           ENDIF;
065100170221           reade (TAMksc:TAMctr:TAMprg) TITAV01L;
065200170221         ENDDO;
065300170221
065400170221       //?Testata Tariffe
065500170221         TAMprg += 1;
065600170221         TAMdtr = oggi;
065700170221         clear TAMftr;
065800170221         update TNTAM04;
065900170221
066000170221       //?Storico variazioni
066100170221         clear TITAV000;
066200170221         TAVksc = TAMksc;
066300170221         TAVctr = TAMctr;
066400170221         TAVprg = TAMprg;
066500170221         TAVtrd = 'TES';
066600170221         TAVcta = 'M';
066700170221         TAVdav = oggi;
066800170221         TAVorv = %dec(%time());
066900170306         TAVnoj = 'DV-2749';
067000170221         TAVpru = 'BATCH';
067100170221         TAVdtr = oggi;
067200170221         write TITAV000;
067300170221
067400170221       ENDSR;
067500170221
067600170221       //--------------------------------------------------------------
067700170221       //?Scrivo un nuovo progressivo.
067800170221       //--------------------------------------------------------------
067900170221       BEGSR NewProgressivo;
068000170221
068100170221       //?Faccio scadere il progressivo che sto leggendo
068200170221         Scadenza = TAMdst;
068300170228         TAMdst = 20170331;
068400170221         TAMduv = oggi;
068500170221         TAMdtr = oggi;
068600170221         clear TAMftr;
068700170221         update TNTAM04;
068800170221       //?Storico variazioni
068900170221         clear TITAV000;
069000170221         TAVksc = TAMksc;
069100170221         TAVctr = TAMctr;
069200170221         TAVprg = TAMprg;
069300170221         TAVtrd = 'TES';
069400170221         TAVcta = 'M';
069500170221         TAVdav = oggi;
069600170221         TAVorv = %dec(%time());
069700170306         TAVnoj = 'DV-2749';
069800170221         TAVpru = 'BATCH';
069900170221         TAVdtr = oggi;
070000170221         write TITAV000;
070100170221
070200170221       //?Deleto eventuali rcd annullati
070300170221         exsr DeletaAnnullati;
070400170221
070500170228       //?Creo un nuovo progressivo con decorrenza 01/04/2017
070600170228         Decorrenza = 20170401;
070700170221         NewPrg = *on;
070800170221
070900170221       //?Dettaglio tariffario
071000170221         setll (TAMksc:TAMctr:TAMprg) TITAD04L;
071100170221         reade (TAMksc:TAMctr:TAMprg) TITAD04L;
071200170221         DOW  not %eof(TITAD04L);
071300170221           clear TITAD000;
071400170221           new_TADksc = TADksc;
071500170221           new_TADctr = TADctr;
071600170221           new_TADprg = TADprg + 1;
071700170221           new_TADlnp = TADlnp;
071800170221           new_TADcts = TADcts;
071900170221           new_TADnaz = TADnaz;
072000170221           new_TADcap = TADcap;
072100170221           new_TADsgl = TADsgl;
072200170221           new_TADitr = TADitr;
072300170221           new_TADino = TADino;
072400170221           new_TADrpv = TADrpv;
072500170221           new_TADorp = TADorp;
072600170221           new_TADmin = TADmin;
072700170221           new_TADmax = TADmax;
072800170221           new_TADdtr = oggi;
072900170221           write TITAD000;
073000170221           reade (TAMksc:TAMctr:TAMprg) TITAD04L;
073100170221         ENDDO;
073200170221
073300170221       //?Testata tariffe particolari
073400170221         setll (TAMksc:TAMctr:TAMprg) TITPT01L;
073500170221         reade (TAMksc:TAMctr:TAMprg) TITPT01L;
073600170221         DOW  not %eof(TITPT01L);
073700170221           clear TITPT000;
073800170221           new_TPTksc = TPTksc;
073900170221           new_TPTctr = TPTctr;
074000170221           new_TPTprg = TPTprg +1;
074100170221           new_TPTduv = oggi;
074200170221           new_TPTftc = TPTftc;
074300170221           new_TPTtpg = TPTtpg;
074400170221           new_TPTarl = TPTarl;
074500170221           new_TPTarf = TPTarf;
074600170221           new_TPTaro = TPTaro;
074700170221           new_TPTrpv = TPTrpv;
074800170221           new_TPTvlm = TPTvlm;
074900170221           new_TPTvvm = TPTvvm;
075000170221           new_TPTfvm = TPTfvm;
075100170221           new_TPTtma = TPTtma;
075200170221           new_TPTapl = TPTapl;
075300170221           new_TPTdtr = oggi;
075400170221           new_TPTdpb = TPTdpb;
075500170221           new_TPTdit = TPTdit;
075600170221           new_TPTflo = TPTflo;
075700170221           write TITPT000;
075800170221           reade (TAMksc:TAMctr:TAMprg) TITPT01L;
075900170221         ENDDO;
076000170221
076100170221       //?Dettaglio tariffe particolari
076200170221         setll (TAMksc:TAMctr:TAMprg) TITPD01L;
076300170221         reade (TAMksc:TAMctr:TAMprg) TITPD01L;
076400170221         DOW  not %eof(TITPD01L);
076500170221           clear TITPD000;
076600170221           new_TPDksc = TPDksc;
076700170221           new_TPDctr = TPDctr;
076800170221           new_TPDprg = TPDprg + 1;
076900170221           new_TPDftc = TPDftc;
077000170221           new_TPDcts = TPDcts;
077100170221           new_TPDorp = TPDorp;
077200170221           new_TPDsgl = TPDsgl;
077300170221           new_TPDitr = TPDitr;
077400170221           new_TPDmin = TPDmin;
077500170221           new_TPDmax = TPDmax;
077600170221           new_TPDain = TPDain;
077700170221           new_TPDdtr = oggi;
077800170221           write TITPD000;
077900170221           reade (TAMksc:TAMctr:TAMprg) TITPD01L;
078000170221         ENDDO;
078100170221
078200170221       //?Tariffe di giacenza
078300170221         setll (TAMksc:TAMctr:TAMprg) TITGC01L;
078400170221         reade (TAMksc:TAMctr:TAMprg) TITGC01L;
078500170221         DOW  not %eof(TITGC01L);
078600170221           clear TITGC000;
078700170221           new_TGCksc = TGCksc;
078800170221           new_TGCctr = TGCctr;
078900170221           new_TGCprg = TGCprg + 1;
079000170221           new_TGCsgv = TGCsgv;
079100170221           new_TGCsgr = TGCsgr;
079200170221           new_TGCsgp = TGCsgp;
079300170221           new_TGCsgd = TGCsgd;
079400170221           new_TGCsg1 = TGCsg1;
079500170221           new_TGCsg2 = TGCsg2;
079600170221           new_TGCsg3 = TGCsg3;
079700170221           new_TGCst1 = TGCst1;
079800170221           new_TGCst2 = TGCst2;
079900170221           new_TGCst3 = TGCst3;
080000170221           new_TGCstm = TGCstm;
080100170221           new_TGCrpv = TGCrpv;
080200170221           new_TGCfaf = TGCfaf;
080300170221           new_TGCsgf = TGCsgf;
080400170221           new_TGCggr = TGCggr;
080500170221           new_TGCtcm = TGCtcm;
080600170221           new_TGCtfg = TGCtfg;
080700170221           new_TGCduv = oggi;
080800170221           new_TGCdtr = oggi;
080900170221           write TITGC000;
081000170221           reade (TAMksc:TAMctr:TAMprg) TITGC01L;
081100170221         ENDDO;
081200170221
081300170221       //?Testata tariffe
081400170221         new_TAMksc = TAMksc;
081500170221         new_TAMctr = TAMctr;
081600170221         new_TAMprg = TAMprg + 1;
081700170221         new_TAMddt = Decorrenza;
081800170221         new_TAMdst = Scadenza;
081900170221         new_TAMduv = oggi;
082000170221         new_TAMdcv = TAMdcv;
082100170221         new_TAMdif = TAMdif;
082200170221         new_TAMfli = TAMfli;
082300170221         new_TAMpri = TAMpri;
082400170221         new_TAMfmp = TAMfmp;
082500170221         new_TAMlrc = TAMlrc;
082600170221         new_TAMlas = TAMlas;
082700170221         new_TAMpag = TAMpag;
082800170313         new_TAMppa = TAMppa;
082900170221         new_TAMmpa = TAMmpa;
083000170221         new_TAMpam = TAMpam;
083100170221         new_TAMmpm = TAMmpm;
083200170221         new_TAMtin = TAMtin;
083300170221         new_TAMkpm = TAMkpm;
083400170221         new_TAMarl = TAMarl;
083500170221         new_TAMarf = TAMarf;
083600170221         new_TAMaro = TAMaro;
083700170221         new_TAMrat = TAMrat;
083800170221         new_TAMrct = TAMrct;
083900170221         new_TAMsc2 = TAMsc2;
084000170221         new_TAMeds = TAMeds;
084100170221         new_TAMars = TAMars;
084200170221         new_TAMata = TAMata;
084300170221         new_TAMftp = TAMftp;
084400170221         new_TAMbap = TAMbap;
084500170221         new_TAMtsp = TAMtsp;
084600170221         new_TAMfie = TAMfie;
084700170221         new_TAMfis = TAMfis;
084800170221         new_TAMsis = TAMsis;
084900170221         new_TAMfvd = TAMfvd;
085000170221         new_TAMfts = TAMfts;
085100170221         new_TAMnas = TAMnas;
085200170221         new_TAMnot = TAMnot;
085300170221         new_TAMftm = TAMftm;
085400170221         new_TAMgca = TAMgca;
085500170221         new_TAMgga = TAMgga;
085600170221         new_TAMgma = TAMgma;
085700170221         new_TAMgva = TAMgva;
085800170221         new_TAMbam = TAMbam;
085900170221         new_TAMtpr = TAMtpr;
086000170221         new_TAMflo = TAMflo;
086100170221         new_TAMdat = TAMdat;
086200170221         new_TAMdtr = oggi;
086300170221         write TNTAM000;
086400170221
086500170221       //?Storico variazioni
086600170221         clear TITAV000;
086700170221         TAVksc = new_TAMksc;
086800170221         TAVctr = new_TAMctr;
086900170221         TAVprg = new_TAMprg;
087000170221         TAVtrd = 'TES';
087100170221         TAVcta = 'D';
087200170221         TAVdav = oggi;
087300170221         TAVorv = %dec(%time());
087400170306         TAVnoj = 'DV-2749';
087500170221         TAVpru = 'BATCH';
087600170221         TAVdtr = oggi;
087700170221         write TITAV000;
087800170221
087900170221       ENDSR;
088000170220
088100170220       //--------------------------------------------------------------
088200170220       //?Scrivo la nuova testata del dirottamento da cartello.
088300170220       //--------------------------------------------------------------
088400170220       BEGSR NewTestataDirottamento;
088500170220
088600170220         clear TITPT000;
088700170221         new_TPTksc = w_TPTksc;
088800170221         new_TPTctr = w_TPTctr;
088900170220         new_TPTprg = wprog;
089000170220         new_TPTduv = oggi;
089100170220         new_TPTftc = dir_TPTftc;
089200170220         new_TPTtpg = dir_TPTtpg;
089300170220         new_TPTarl = dir_TPTarl;
089400170220         new_TPTarf = dir_TPTarf;
089500170220         new_TPTaro = dir_TPTaro;
089600170220         new_TPTrpv = dir_TPTrpv;
089700170220         new_TPTvlm = dir_TPTvlm;
089800170220         new_TPTvvm = dir_TPTvvm;
089900170220         new_TPTfvm = dir_TPTfvm;
090000170220         new_TPTtma = dir_TPTtma;
090100170220         new_TPTapl = dir_TPTapl;
090200170220         new_TPTdtr = oggi;
090300170220         new_TPTdpb = dir_TPTdpb;
090400170220         new_TPTdit = oggi;
090500170220         new_TPTflo = dir_TPTflo;
090600170220         write TITPT000;
090700170220
090800170220       ENDSR;
090900170220
091000170220       //--------------------------------------------------------------
091100170220       //?Scrivo la nuova testata del fuori misura da cartello.
091200170220       //--------------------------------------------------------------
091300170220       BEGSR NewTestataFuoriMisura;
091400170220
091500170220         clear TITPT000;
091600170221         new_TPTksc = w_TPTksc;
091700170221         new_TPTctr = w_TPTctr;
091800170220         new_TPTprg = wprog;
091900170220         new_TPTduv = oggi;
092000170220         new_TPTftc = fuo_TPTftc;
092100170220         new_TPTtpg = fuo_TPTtpg;
092200170220         new_TPTarl = fuo_TPTarl;
092300170220         new_TPTarf = fuo_TPTarf;
092400170220         new_TPTaro = fuo_TPTaro;
092500170220         new_TPTrpv = fuo_TPTrpv;
092600170220         new_TPTvlm = fuo_TPTvlm;
092700170220         new_TPTvvm = fuo_TPTvvm;
092800170220         new_TPTfvm = fuo_TPTfvm;
092900170220         new_TPTtma = fuo_TPTtma;
093000170220         new_TPTapl = fuo_TPTapl;
093100170220         new_TPTdtr = oggi;
093200170220         new_TPTdpb = fuo_TPTdpb;
093300170220         new_TPTdit = oggi;
093400170220         new_TPTflo = fuo_TPTflo;
093500170220         write TITPT000;
093600170220
093700170220       ENDSR;
093800170220
093900170220       //--------------------------------------------------------------
094000170220       //?Scrivo il nuovo dettaglio del dirottamento.
094100170220       //--------------------------------------------------------------
094200170220       BEGSR NewDettaglioDirottamento;
094300170220
094400170220         clear TITPD000;
094500170221         new_TPDksc = w_TPTksc;
094600170221         new_TPDctr = w_TPTctr;
094700170220         new_TPDprg = wprog;
094800170220         new_TPDftc = dirottamento;
094900170220         new_TPDcts = 'IT';
095000170220         new_TPDorp = '  A 9';
095100170220         new_TPDsgl = 9999999;
095200170220         new_TPDitr = 6;
095300170220         new_TPDdtr = oggi;
095400170220         write TITPD000;
095500170220
095600170220       ENDSR;
095700170220
095800170220       //--------------------------------------------------------------
095900170220       //?Scrivo il nuovo dettaglio del fuori misura.
096000170220       //--------------------------------------------------------------
096100170220       BEGSR NewDettaglioFuoriMisura;
096200170220
096300170220         clear TITPD000;
096400170221         new_TPDksc = w_TPTksc;
096500170221         new_TPDctr = w_TPTctr;
096600170220         new_TPDprg = wprog;
096700170220         new_TPDftc = fuorimisura;
096800170220         new_TPDcts = 'IT';
096900170220         new_TPDorp = '  A 9';
097000170220         new_TPDsgl = 9999999;
097100170220         new_TPDitr = 28,922;
097200170220         new_TPDdtr = oggi;
097300170220         write TITPD000;
097400170220
097500170220       ENDSR;
097600170220
097700170220       //--------------------------------------------------------------
097800170220       //?Scrivo storico variazione per aggiunta tariffe paricolari.
097900170220       //--------------------------------------------------------------
098000170220       BEGSR StoricoTarParticolari;
098100170220
098200170221         IF  w_WFAdir <> *blanks;
098300170220           clear TITAV000;
098400170221           TAVksc = w_TPTksc;
098500170221           TAVctr = w_TPTctr;
098600170220           TAVprg = wprog;
098700170220           TAVtrd = 'PAR';
098800170220           TAVftc = dirottamento;
098900170220           SELECT;
099000170221           WHEN  NewPrg or w_WFAdir = 'A';
099100170220             TAVcta = 'I';
099200170221           WHEN  w_WFAdir = 'M' or w_WFAdir = 'T';
099300170220             TAVcta = 'M';
099400170220           ENDSL;
099500170220           TAVdav = oggi;
099600170220           TAVorv = %dec(%time());
099700170306           TAVnoj = 'DV-2749';
099800170220           TAVpru = 'BATCH';
099900170220           TAVdtr = oggi;
100000170220           write TITAV000;
100100170220         ENDIF;
100200170220
100300170221         IF  w_WFAfuo <> *blanks;
100400170220           clear TITAV000;
100500170221           TAVksc = w_TPTksc;
100600170221           TAVctr = w_TPTctr;
100700170220           TAVprg = wprog;
100800170220           TAVtrd = 'PAR';
100900170220           TAVftc = fuorimisura;
101000170220           SELECT;
101100170221           WHEN  NewPrg or w_WFAfuo = 'A';
101200170220             TAVcta = 'I';
101300170221           WHEN  w_WFAfuo = 'M' or w_WFAfuo = 'T';
101400170220             TAVcta = 'M';
101500170220           ENDSL;
101600170220           TAVdav = oggi;
101700170220           TAVorv = %dec(%time());
101800170306           TAVnoj = 'DV-2749';
101900170220           TAVpru = 'BATCH';
102000170220           TAVdtr = oggi;
102100170220           write TITAV000;
102200170220         ENDIF;
102300170220
102400170220       ENDSR;
102500170221
102600170221       //--------------------------------------------------------------
102700170221       //?Cancello tariffa annullata.
102800170221       //--------------------------------------------------------------
102900170221       BEGSR DeletaAnnullati;
103000170221
103100170221         wprog = TAMprg + 1;
103200170221
103300170221         setll (TAMksc:TAMctr:wprog) TITAD04L;
103400170221         reade (TAMksc:TAMctr:wprog) TITAD04L;
103500170221         DOW  not %eof(TITAD04L);
103600170221           IF  TADatb <> *blanks;
103700170221             delete TITAD04;
103800170221           ENDIF;
103900170221           reade (TAMksc:TAMctr:wprog) TITAD04L;
104000170221         ENDDO;
104100170221
104200170221         setll (TAMksc:TAMctr:wprog) TITPT01L;
104300170221         reade (TAMksc:TAMctr:wprog) TITPT01L;
104400170221         DOW  not %eof(TITPT01L);
104500170221           IF  TPTatb <> *blanks;
104600170221             delete TITPT01;
104700170221           ENDIF;
104800170221           reade (TAMksc:TAMctr:wprog) TITPT01L;
104900170221         ENDDO;
105000170221
105100170221         setll (TAMksc:TAMctr:wprog) TITPD01L;
105200170221         reade (TAMksc:TAMctr:wprog) TITPD01L;
105300170221         DOW  not %eof(TITPD01L);
105400170221           IF  TPDatb <> *blanks;
105500170221             delete TITPD01;
105600170221           ENDIF;
105700170221           reade (TAMksc:TAMctr:wprog) TITPD01L;
105800170221         ENDDO;
105900170221
106000170221         setll (TAMksc:TAMctr:wprog) TITGC01L;
106100170221         reade (TAMksc:TAMctr:wprog) TITGC01L;
106200170221         DOW  not %eof(TITGC01L);
106300170221           IF  TGCatb <> *blanks;
106400170221             delete TITGC01;
106500170221           ENDIF;
106600170221           reade (TAMksc:TAMctr:wprog) TITGC01L;
106700170221         ENDDO;
106800170221
106900170221         chain (TAMksc:TAMctr:wprog) TNTAM01L;
107000170221         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
107100170221           delete TNTAM01;
107200170221         ENDIF;
107300170221
107400170221         setll (TAMksc:TAMctr:wprog) TITAV01L;
107500170221         reade (TAMksc:TAMctr:wprog) TITAV01L;
107600170221         DOW  not %eof(TITAV01L);
107700170221           IF  TAVatb <> *blanks;
107800170221             delete TITAV000;
107900170221           ENDIF;
108000170221           reade (TAMksc:TAMctr:wprog) TITAV01L;
108100170221         ENDDO;
108200170221
108300170221       ENDSR;
108400170210
108500170210       //--------------------------------------------------------------
108600170210       //?Operazioni finali.
108700170210       //--------------------------------------------------------------
108800170210       BEGSR RoutEnd;
108900170210
109000170210         *inLR = *on;
109100170210         return;
109200170210
109300170210       ENDSR;
109400170210
109500170210      /end-free
