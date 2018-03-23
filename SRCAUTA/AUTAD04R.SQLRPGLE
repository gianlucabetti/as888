000100170210      //-------------------------------------------------------------------
000200170228      //?AUTAD04R - Da WF crea Nuove Tariffe Particolari DPD da OFF
000300170210      //-------------------------------------------------------------------
000400170210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170210
000600170210      //---------------------------------------------------------------
000700170210      //?Dichiarazione file.
000800170210      //---------------------------------------------------------------
000900170210     fWFAUD01L  uf   e           k disk
001000170210
001100170214     fTIOPT01L  uf   e           k disk    rename(TITPT000:TITPT01)
001200170214     fTIOPD01L  uf   e           k disk    rename(TITPD000:TITPD01)
001300170210
001400170214     fTIOPT00F  o    e             disk    prefix(new_)
001500170214     fTIOPD00F  o    e             disk    prefix(new_)
001600170210
001700170210      //---------------------------------------------------------------
001800170210      //?Definizione costanti.
001900170210      //---------------------------------------------------------------
002000170210
002100170210      //---------------------------------------------------------------
002200170210      //?Definizione schiere.
002300170210      //---------------------------------------------------------------
002400170210
002500170210      //---------------------------------------------------------------
002600170210      //?Definizione aree dati.
002700170210      //---------------------------------------------------------------
002800170210      // - Dati utente
002900170210     d §AzUte        e ds                  extname(AZUTE00F)
003000170210     d                                     dtaara
003100170210     d §DatiUte      e ds                  extname(dDatiUte)
003200170210     d                                     dtaara
003300170210
003400170210      //---------------------------------------------------------------
003500170210      //?Definizione strutture dati.
003600170210      //---------------------------------------------------------------
003700170210      // - Parametri ricevuti
003800170210     d KPJBA         e ds
003900170210
004000170210      // - Reperimento dati utente
004100170210     d TIBS34DS      e ds
004200170210
004300170210      // - File TITPT00F x cartello dirottamento
004400170210     d TITPTDIR      e ds                  extname(TITPT00F)
004500170210     d                                     prefix(dir_)
004600170210
004700170210      // - File TITPT00F x cartello fuori misura
004800170210     d TITPTFUO      e ds                  extname(TITPT00F)
004900170210     d                                     prefix(fuo_)
005000170210
005100170210      // - File WFAUD00F
005200170210     d WFAUDDS       e ds                  extname(WFAUD00F)
005300170210
005400170210      //---------------------------------------------------------------
005500170210      //?Definizione variabili globali.
005600170210      //---------------------------------------------------------------
005700170210      // - Flags booleani
005800170210     d Fine            s               n   inz(*off)
005900170214     d wAgg            s               n   inz(*off)
006000170210     d wEoF            s               n   inz(*off)
006100170210
006200170210      // - Campi di comodo
006300170210     d Dirottamento    s                   like(TPTftc) inz('* ')
006400170210     d FuoriMisura     s                   like(TPTftc) inz('F ')
006500170210     d oggi            s              8s 0 inz
006600170210
006700170210      // - Campi di comodo per tariffa cartello DPD
006800170215     d ksc888          s                   like(TPTksc) inz(8888817)
006900170215     d ctr888          s                   like(TPTctr) inz(300)
007000170215     d prg888          s                   like(TPTprg) inz(17)
007100170210
007200170210      //---------------------------------------------------------------
007300170210      //?Definizione Procedure usate.
007400170210      //---------------------------------------------------------------
007500170210
007600170210      //---------------------------------------------------------------
007700170210      //?Definizione Prototipi.
007800170210      //---------------------------------------------------------------
007900170210      /copy gaitrasrc/srcprotopr,TIBS34R
008000170210
008100170210      //---------------------------------------------------------------
008200170210      //?Definizione key-list.
008300170210      //---------------------------------------------------------------
008400170210
008500170210      //---------------------------------------------------------------
008600170210      //?M A I N - L I N E
008700170210      //---------------------------------------------------------------
008800170210     c     *Entry        plist
008900170210     c                   parm                    kpjba
009000170210
009100170210      /free
009200170210
009300170210       //?Operazioni iniziali
009400170210       exsr RoutInz;
009500170210
009600170210       //?Elaborazione
009700170210       exsr Elabora;
009800170210
009900170210       //?Operazioni finali
010000170210       exsr RoutEnd;
010100170210
010200170210       //--------------------------------------------------------------
010300170210       //?Operazioni iniziali.
010400170210       //--------------------------------------------------------------
010500170210       BEGSR RoutInz;
010600170213
010700170213         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010800170210
010900170210       //?Imposto oggi
011000170210         Oggi = %dec(%date());
011100170210
011200170210       //?Reperimento dati job
011300170210         exsr DatiJob;
011400170210
011500170210       //?Cerco la testata della tariffa particolare dirottamento della cartello DPD
011600170210         exec sql
011700170210         SELECT TITPT00F.* into :TITPTdir
011800170210         FROM   TITPT00F
011900170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
012000170210                TPTftc = :dirottamento;
012100170210         IF  sqlcod <> 0;
012200170210           clear TITPTdir;
012300170210         ENDIF;
012400170210       //?Cerco la testata della tariffa particolare fuori misura della cartello DPD
012500170210         exec sql
012600170210         SELECT TITPT00F.* into :TITPTfuo
012700170210         FROM   TITPT00F
012800170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
012900170210                TPTftc = :fuorimisura;
013000170210         IF  sqlcod <> 0;
013100170210           clear TITPTfuo;
013200170210         ENDIF;
013300170210
013400170210       ENDSR;
013500170210
013600170210       //--------------------------------------------------------------
013700170210       //?Reperimento Dati del job (Utente/Operativi).
013800170210       //--------------------------------------------------------------
013900170210       BEGSR DatiJob;
014000170210
014100170210         in(E) §AzUte;
014200170210         IF  not %error;
014300170210           in(E) §DatiUte;
014400170210         ENDIF;
014500170210         IF  %error or RSut = *blanks;
014600170210           clear TIBS34ds;
014700170210           tibs34r(tibs34ds);
014800170210           in §AzUte;
014900170210           in §DatiUte;
015000170210         ENDIF;
015100170210
015200170210       ENDSR;
015300170210
015400170210       //--------------------------------------------------------------
015500170210       //?Elaborazione.
015600170210       //--------------------------------------------------------------
015700170210       BEGSR Elabora;
015800170210
015900170210       //?Dichiarazione cursore
016000170210         exec sql
016100170210         declare WRK cursor for
016200170210         select * from WFAUD00F
016300170210         where (WFAdir <> ' '  or WFAfuo <> ' ') and
016400170220                WFAtipo = 'O' and WFAela = ' '
016500170213         order by WFAksc, WFActrv, WFAprgv;
016600170210
016700170210         //?Apertura del cursore
016800170210         exec sql
016900170210         open WRK;
017000170210
017100170210         IF  sqlcode < 0;
017200170210           exec sql close WRK;
017300170210           leavesr;
017400170210         ENDIF;
017500170210
017600170210         DOW  not wEoF;
017700170210           exec sql
017800170210           fetch next from WRK into :WFAUDDS;
017900170210           IF  sqlcod = 100 or sqlcod < 0;
018000170210             wEoF = *on;
018100170210             leave;
018200170210           ENDIF;
018300170214
018400170214           wAgg = *off;
018500170210
018600170214         //?A = devo aggiungere la tariffa particolare
018700170214         //?T = ho solo la testata devo scrivere il dettaglio
018800170214         //?M = devo modificare la tariffa particolare
018900170215           IF  WFAdir = 'A';
019000170214             exsr NewTestataDirottamento;
019100170214             exsr NewDettaglioDirottamento;
019200170214             wAgg = *on;
019300170215           ENDIF;
019400170215           IF  WFAdir = 'T';
019500170214             exsr NewDettaglioDirottamento;
019600170214             wAgg = *on;
019700170215           ENDIF;
019800170215           IF  WFAdir = 'M';
019900170214             exsr ModificaDirottamento;
020000170214             wAgg = *on;
020100170215           ENDIF;
020200170215           IF  WFAfuo = 'A';
020300170214             exsr NewTestataFuoriMisura;
020400170214             exsr NewDettaglioFuoriMisura;
020500170214             wAgg = *on;
020600170215           ENDIF;
020700170215           IF  WFAfuo = 'T';
020800170214             exsr NewDettaglioFuoriMisura;
020900170214             wAgg = *on;
021000170215           ENDIF;
021100170215           IF  WFAfuo = 'M';
021200170214             exsr ModificaFuoriMisura;
021300170214             wAgg = *on;
021400170215           ENDIF;
021500170214
021600170214       //?File di WORK
021700170214           IF  wAgg;
021800170214             chain ('O':WFAksc:WFActrv:WFAprgv) WFAUD01L;
021900170214             IF  %found(WFAUD01L);
022000170220               WFAela  = 'S';
022100170214               update WFAUD000;
022200170214             ENDIF;
022300170214           ENDIF;
022400170210
022500170210         ENDDO;
022600170210
022700170210         exec sql
022800170210         close WRK;
022900170210
023000170210       ENDSR;
023100170214
023200170214       //--------------------------------------------------------------
023300170214       //?Scrivo la nuova testata del dirottamento da cartello.
023400170214       //--------------------------------------------------------------
023500170214       BEGSR NewTestataDirottamento;
023600170214
023700170214         clear TITPT000;
023800170214         new_TPTksc = WFAksc;
023900170214         new_TPTctr = WFActrv;
024000170215         new_TPTprg = WFAprgv;
024100170214         new_TPTduv = oggi;
024200170214         new_TPTftc = dir_TPTftc;
024300170214         new_TPTtpg = dir_TPTtpg;
024400170214         new_TPTarl = dir_TPTarl;
024500170214         new_TPTarf = dir_TPTarf;
024600170214         new_TPTaro = dir_TPTaro;
024700170214         new_TPTrpv = dir_TPTrpv;
024800170214         new_TPTvlm = dir_TPTvlm;
024900170214         new_TPTvvm = dir_TPTvvm;
025000170214         new_TPTfvm = dir_TPTfvm;
025100170214         new_TPTtma = dir_TPTtma;
025200170214         new_TPTapl = dir_TPTapl;
025300170214         new_TPTdtr = oggi;
025400170214         new_TPTdpb = dir_TPTdpb;
025500170214         new_TPTdit = oggi;
025600170214         new_TPTflo = dir_TPTflo;
025700170214         write TITPT000;
025800170214
025900170214       ENDSR;
026000170214
026100170214       //--------------------------------------------------------------
026200170214       //?Scrivo la nuova testata del fuori misura da cartello.
026300170214       //--------------------------------------------------------------
026400170214       BEGSR NewTestataFuoriMisura;
026500170214
026600170214         clear TITPT000;
026700170214         new_TPTksc = WFAksc;
026800170214         new_TPTctr = WFActrv;
026900170215         new_TPTprg = WFAprgv;
027000170215         new_TPTduv = oggi;
027100170214         new_TPTftc = fuo_TPTftc;
027200170214         new_TPTtpg = fuo_TPTtpg;
027300170214         new_TPTarl = fuo_TPTarl;
027400170214         new_TPTarf = fuo_TPTarf;
027500170214         new_TPTaro = fuo_TPTaro;
027600170214         new_TPTrpv = fuo_TPTrpv;
027700170214         new_TPTvlm = fuo_TPTvlm;
027800170214         new_TPTvvm = fuo_TPTvvm;
027900170214         new_TPTfvm = fuo_TPTfvm;
028000170214         new_TPTtma = fuo_TPTtma;
028100170214         new_TPTapl = fuo_TPTapl;
028200170214         new_TPTdtr = oggi;
028300170214         new_TPTdpb = fuo_TPTdpb;
028400170214         new_TPTdit = oggi;
028500170214         new_TPTflo = fuo_TPTflo;
028600170214         write TITPT000;
028700170214
028800170214       ENDSR;
028900170214
029000170214       //--------------------------------------------------------------
029100170214       //?Scrivo il nuovo dettaglio del dirottamento.
029200170214       //--------------------------------------------------------------
029300170214       BEGSR NewDettaglioDirottamento;
029400170214
029500170214         clear TITPD000;
029600170214         new_TPDksc = WFAksc;
029700170214         new_TPDctr = WFActrv;
029800170215         new_TPDprg = WFAprgv;
029900170214         new_TPDftc = dirottamento;
030000170214         new_TPDcts = 'IT';
030100170214         new_TPDorp = '  A 9';
030200170214         new_TPDsgl = 9999999;
030300170214         new_TPDitr = 6;
030400170214         new_TPDdtr = oggi;
030500170214         write TITPD000;
030600170214
030700170214       ENDSR;
030800170214
030900170214       //--------------------------------------------------------------
031000170214       //?Scrivo il nuovo dettaglio del fuori misura.
031100170214       //--------------------------------------------------------------
031200170214       BEGSR NewDettaglioFuoriMisura;
031300170214
031400170214         clear TITPD000;
031500170214         new_TPDksc = WFAksc;
031600170214         new_TPDctr = WFActrv;
031700170215         new_TPDprg = WFAprgv;
031800170214         new_TPDftc = fuorimisura;
031900170214         new_TPDcts = 'IT';
032000170214         new_TPDorp = '  A 9';
032100170214         new_TPDsgl = 9999999;
032200170214         new_TPDitr = 28,922;
032300170214         new_TPDdtr = oggi;
032400170214         write TITPD000;
032500170214
032600170214       ENDSR;
032700170214
032800170214       //--------------------------------------------------------------
032900170214       //?Aggiorno il dettaglio del dirottamento.
033000170214       //--------------------------------------------------------------
033100170214       BEGSR ModificaDirottamento;
033200170210
033300170215         chain (WFAksc:WFActrv:WFAprgv:dirottamento) TIOPD01L;
033400170215         IF  %found(TIOPD01L);
033500170214           TPDsgl = 9999999;
033600170214           TPDitr = 6;
033700170214           clear TPDftr;
033800170214           TPDdtr = oggi;
033900170214           update TITPD01;
034000170214         ENDIF;
034100170213         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
034200170215         chain (WFAksc:WFActrv:WFAprgv:dirottamento) TIOPT01L;
034300170214         IF  %found(TIOPT01L);
034400170214            TPTduv = oggi;
034500170214            clear TPTftr;
034600170214            TPTdtr = oggi;
034700170214            update TITPT01;
034800170214         ENDIF;
034900170214
035000170214       ENDSR;
035100170214
035200170214       //--------------------------------------------------------------
035300170214       //?Aggiorno il dettaglio del fuori misura.
035400170214       //--------------------------------------------------------------
035500170214       BEGSR ModificaFuoriMisura;
035600170214
035700170215         chain (WFAksc:WFActrv:WFAprgv:fuorimisura) TIOPD01L;
035800170215         IF  %found(TIOPD01L);
035900170214           TPDsgl = 9999999;
036000170214           TPDitr = 28,922;
036100170214           clear TPDftr;
036200170214           TPDdtr = oggi;
036300170214           update TITPD01;
036400170214         ENDIF;
036500170214       //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
036600170215         chain (WFAksc:WFActrv:WFAprgv:fuorimisura) TIOPT01L;
036700170215         IF  %found(TIOPT01L);
036800170214           TPTduv = oggi;
036900170214           clear TPTftr;
037000170214           TPTdtr = oggi;
037100170214           update TITPT01;
037200170214         ENDIF;
037300170210
037400170210       ENDSR;
037500170210
037600170210       //--------------------------------------------------------------
037700170210       //?Operazioni finali.
037800170210       //--------------------------------------------------------------
037900170210       BEGSR RoutEnd;
038000170210
038100170210         *inLR = *on;
038200170210         return;
038300170210
038400170210       ENDSR;
038500170210
038600170210      /end-free
