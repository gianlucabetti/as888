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
007000170217     d FlagA           s               n   inz(*off)
007100170217     d FlagB           s               n   inz(*off)
007200170217     d FlagC           s               n   inz(*off)
007300170217     d FlagD           s               n   inz(*off)
007400170213     d NewPrg          s               n   inz(*off)
007500170220     d OkDir           s               n   inz(*off)
007600170220     d OkFuo           s               n   inz(*off)
007700170210     d wEoF            s               n   inz(*off)
007800170210
007900170210      // - Campi di comodo
008000170210     d decorrenza      s                   like(TAMddt) inz
008100170210     d Dirottamento    s                   like(TPTftc) inz('* ')
008200170210     d FuoriMisura     s                   like(TPTftc) inz('F ')
008300170210     d oggi            s              8s 0 inz
008400170213     d olddst          s                   like(TAMdst) inz
008500170210     d oldprg          s                   like(TAMprg) inz
008600170220     d sav_WFAdir      s                   like(WFAfuo) inz
008700170220     d sav_WFAfuo      s                   like(WFAdir) inz
008800170217     d sav_TAMksc      s                   like(TAMksc) inz
008900170217     d sav_TAMctr      s                   like(TAMctr) inz
009000170217     d sav_TAMprg      s                   like(TAMprg) inz
009100170210     d scadenza        s                   like(TAMdst) inz
009200170210     d wprog           s                   like(TAMprg) inz
009300170210
009400170210      // - Campi di comodo per tariffa cartello DPD
009500170210     d ksc888          s                   like(TAMksc) inz(8888817)
009600170210     d ctr888          s                   like(TAMctr) inz(300)
009700170210     d prg888          s                   like(TAMprg) inz(17)
009800170210
009900170210      //---------------------------------------------------------------
010000170210      //?Definizione Procedure usate.
010100170210      //---------------------------------------------------------------
010200170210
010300170210      //---------------------------------------------------------------
010400170210      //?Definizione Prototipi.
010500170210      //---------------------------------------------------------------
010600170210      /copy gaitrasrc/srcprotopr,TIBS34R
010700170210
010800170210      //---------------------------------------------------------------
010900170210      //?Definizione key-list.
011000170210      //---------------------------------------------------------------
011100170210
011200170210      //---------------------------------------------------------------
011300170210      //?M A I N - L I N E
011400170210      //---------------------------------------------------------------
011500170210     c     *Entry        plist
011600170210     c                   parm                    kpjba
011700170210
011800170210      /free
011900170210
012000170210       //?Operazioni iniziali
012100170210       exsr RoutInz;
012200170210
012300170210       //?Elaborazione
012400170210       exsr Elabora;
012500170210
012600170210       //?Operazioni finali
012700170210       exsr RoutEnd;
012800170210
012900170210       //--------------------------------------------------------------
013000170210       //?Operazioni iniziali.
013100170210       //--------------------------------------------------------------
013200170210       BEGSR RoutInz;
013300170213
013400170213         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013500170210
013600170210       //?Imposto oggi
013700170210         Oggi = %dec(%date());
013800170210
013900170210       //?Reperimento dati job
014000170210         exsr DatiJob;
014100170210
014200170210       //?Cerco la testata della tariffa particolare dirottamento della cartello DPD
014300170210         exec sql
014400170210         SELECT TITPT00F.* into :TITPTdir
014500170210         FROM   TITPT00F
014600170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
014700170210                TPTftc = :dirottamento;
014800170210         IF  sqlcod <> 0;
014900170210           clear TITPTdir;
015000170210         ENDIF;
015100170210       //?Cerco la testata della tariffa particolare fuori misura della cartello DPD
015200170210         exec sql
015300170210         SELECT TITPT00F.* into :TITPTfuo
015400170210         FROM   TITPT00F
015500170210         WHERE  TPTksc = :ksc888 and TPTctr = :ctr888 and TPTprg = :prg888 and
015600170210                TPTftc = :fuorimisura;
015700170210         IF  sqlcod <> 0;
015800170210           clear TITPTfuo;
015900170210         ENDIF;
016000170210
016100170210       ENDSR;
016200170210
016300170210       //--------------------------------------------------------------
016400170210       //?Reperimento Dati del job (Utente/Operativi).
016500170210       //--------------------------------------------------------------
016600170210       BEGSR DatiJob;
016700170210
016800170210         in(E) §AzUte;
016900170210         IF  not %error;
017000170210           in(E) §DatiUte;
017100170210         ENDIF;
017200170210         IF  %error or RSut = *blanks;
017300170210           clear TIBS34ds;
017400170210           tibs34r(tibs34ds);
017500170210           in §AzUte;
017600170210           in §DatiUte;
017700170210         ENDIF;
017800170210
017900170210       ENDSR;
018000170210
018100170210       //--------------------------------------------------------------
018200170210       //?Elaborazione.
018300170210       //--------------------------------------------------------------
018400170210       BEGSR Elabora;
018500170217
018600170210       //?Dichiarazione cursore
018700170210         exec sql
018800170210         declare WRK cursor for
018900170210         select * from WFAUD00F
019000170210         where (WFAdir <> ' '  or WFAfuo <> ' ') and
019100170210                WFAtipo = 'T'
019200170213         order by WFAksc, WFActrv, WFAprgv;
019300170210
019400170210         //?Apertura del cursore
019500170210         exec sql
019600170210         open WRK;
019700170210
019800170210         IF  sqlcode < 0;
019900170210           exec sql close WRK;
020000170210           leavesr;
020100170210         ENDIF;
020200170210
020300170210         DOW  not wEoF;
020400170210           exec sql
020500170210           fetch next from WRK into :WFAUDDS;
020600170210           IF  sqlcod = 100 or sqlcod < 0;
020700170210             wEoF = *on;
020800170210             leave;
020900170210           ENDIF;
021000170220
021100170220           sav_WFAdir = WFAdir;
021200170220           sav_WFAfuo = WFAfuo;
021300170220           OkDir = *off;
021400170220           OkFuo = *off;
021500170217
021600170217         //?Leggo il TNTAM della tariffa da aggiornare
021700170217           Fine = *off;
021800170217           setll (WFAksc:WFActrv) TNTAM04L;
021900170217           DOW  not Fine;
022000170217             reade (WFAksc:WFActrv) TNTAM04L;
022100170217             IF  %eof(TNTAM04L);
022200170217               Fine = *on;
022300170217               leave;
022400170217             ENDIF;
022500170217
022600170217           //?Legenda dei FLAG
022700170217           //?FlagA = decorrenza = 01/03/2017
022800170217           //?FlagB = scadenza   > 01/03/2017
022900170217           //?FlagC = scadenza   < 01/03/2017
023000170217           //?FlagD = decorrenza > 01/03/2017
023100170217           //?        precedenti con decorrenza/scadenza a cavallo
023200170217
023300170217           //?Se sto leggendo la stessa tariffa ed ho già fatto l'aggiornamento
023400170217           //?leggo altra tariffa
023500170217             IF  (TAMksc = sav_TAMksc and TAMctr = sav_TAMctr and FlagA) or
023600170217                 (TAMksc = sav_TAMksc and TAMctr = sav_TAMctr and FlagB) or
023700170217                 (TAMksc = sav_TAMksc and TAMctr = sav_TAMctr and FlagC);
023800170217               iter;
023900170217             ENDIF;
024000170217
024100170217           //?A cambio tariffa devo resettare i flag
024200170217             IF  TAMksc <> sav_TAMksc;
024300170217               FlagA = *off;
024400170217               FlagB = *off;
024500170217               FlagC = *off;
024600170217               sav_TAMksc = TAMksc;
024700170220               clear sav_TAMprg;
024800170217             ENDIF;
024900170217             IF  TAMctr <> sav_TAMctr;
025000170217               FlagA = *off;
025100170217               FlagB = *off;
025200170217               FlagC = *off;
025300170217               sav_TAMctr = TAMctr;
025400170220               clear sav_TAMprg;
025500170217             ENDIF;
025600170213
025700170217             NewPrg = *off;
025800170220             IF  not FlagD;
025900170217             sav_TAMprg = TAMprg;
026000170220             ENDIF;
026100170217
026200170217           //?Cancello eventuale tariffa annullata con progressivo che devo scrivere
026300170217             exsr Deleta_Annullati;
026400170217
026500170217           //?Se la tariffa che sto leggendo ha data decorrenza >= 01/03/2017
026600170217             IF  TAMddt >= 20170301;
026700170217               IF  TAMddt = 20170301;
026800170217                 FlagA = *on;
026900170217               ENDIF;
027000170217               FlagD = *off;
027100170217             //?Se decorrenza e scadenza <> 01/03/2017
027200170217               IF  TAMddt <> 20170301 and TAMdst <> 20170301;
027300170217             //?se la tariffa che sto leggendo non ha progressivo 0
027400170217             //?devo controllare i progressivi precedenti
027500170217                 IF  TAMprg <> 0;
027600170217                   exsr OldProgressivo;
027700170217                 ENDIF;
027800170217               //?devo aumentare il progressivo attuale
027900170217                 IF  FlagD;
028000170217                   exsr AumentaPrgAttuale;
028100170217                 ENDIF;
028200170217               ENDIF;
028300170217             //?Aggiorno le Tariffe particolari
028400170220               IF  not FlagD;
028500170220                 wprog = TAMprg;
028600170220                 exsr AggTarParticolari;
028700170220               ENDIF;
028800170217             //?Aggiorno File di WORK
028900170217               chain ('T':TAMksc:TAMctr:sav_TAMprg) WFAUD01L;
029000170217               IF  %found(WFAUD01L);
029100170217                 WFAddtn = TAMddt;
029200170217                 WFAdstn = TAMdst;
029300170217                 WFAprgn = TAMprg;
029400170220                 WFAela  = 'S';
029500170217                 update WFAUD000;
029600170217               ENDIF;
029700170217             ENDIF;
029800170217
029900170217           //?Se la tariffa che sto leggendo ha data decorrenza < 01/03/2017 e
030000170217           //?data scadenza >= 01/03/2017
030100170217           //?devo creare un nuovo progressivo con decorrenza 01/03/2017
030200170217           //?e scadenza = data scadenza tariffa
030300170217           //?e il progressivo che sto leggendo deve averea data scadenza = 28/02/2017
030400170217             IF  TAMdst >= 20170301 and TAMddt < 20170301;
030500170217               decorrenza = 20170301;
030600170217               scadenza = TAMdst;
030700170217             //?Aggiorno il progressivo attuale
030800170217               FlagB = *on;
030900170217               TAMdst = 20170228;
031000170217               TAMduv = oggi;
031100170217               TAMdtr = oggi;
031200170217               clear TAMftr;
031300170217               update TNTAM04;
031400170217             //?Storico variazioni
031500170217               clear TITAV000;
031600170217               TAVksc = TAMksc;
031700170217               TAVctr = TAMctr;
031800170217               TAVprg = TAMprg;
031900170217               TAVtrd = 'TES';
032000170217               TAVcta = 'M';
032100170217               TAVdav = oggi;
032200170217               TAVorv = %dec(%time());
032300170217               TAVnoj = 'DV-DPD';
032400170217               TAVpru = 'BATCH';
032500170217               TAVdtr = oggi;
032600170217               write TITAV000;
032700170217             //?Scrivo un nuovo progressivo
032800170217               exsr NewProgressivo;
032900170217               iter;
033000170217             ENDIF;
033100170210
033200170217           //?Se la tariffa che sto leggendo ha scadenza < 01/03/2017
033300170217           //?devo creare un nuovo progressivo con decorrenza 01/03/2017 e scadenza 31/12/2017
033400170217             IF  TAMdst < 20170301;
033500170217               FlagC = *on;
033600170217               decorrenza = 20170301;
033700170217               scadenza = 20171231;
033800170217             //?Scrivo un nuovo progressivo
033900170217               exsr NewProgressivo;
034000170217             ENDIF;
034100170217
034200170217           ENDDO;
034300170210
034400170210         ENDDO;
034500170210
034600170210         exec sql
034700170210         close WRK;
034800170210
034900170210       ENDSR;
035000170217
035100170217       //--------------------------------------------------------------
035200170217       //?Cancello tariffa annullata.
035300170217       //--------------------------------------------------------------
035400170217       BEGSR Deleta_Annullati;
035500170217
035600170217         wprog = TAMprg + 1;
035700170217
035800170217         setll (TAMksc:TAMctr:wprog) TITAD04L;
035900170217         reade (TAMksc:TAMctr:wprog) TITAD04L;
036000170217         DOW  not %eof(TITAD04L);
036100170217           IF  TADatb <> *blanks;
036200170217             delete TITAD04;
036300170217           ENDIF;
036400170217           reade (TAMksc:TAMctr:wprog) TITAD04L;
036500170217         ENDDO;
036600170217
036700170217         setll (TAMksc:TAMctr:wprog) TITPT01L;
036800170217         reade (TAMksc:TAMctr:wprog) TITPT01L;
036900170217         DOW  not %eof(TITPT01L);
037000170217           IF  TPTatb <> *blanks;
037100170217             delete TITPT01;
037200170217           ENDIF;
037300170217           reade (TAMksc:TAMctr:wprog) TITPT01L;
037400170217         ENDDO;
037500170217
037600170217         setll (TAMksc:TAMctr:wprog) TITPD01L;
037700170217         reade (TAMksc:TAMctr:wprog) TITPD01L;
037800170217         DOW  not %eof(TITPD01L);
037900170217           IF  TPDatb <> *blanks;
038000170217             delete TITPD01;
038100170217           ENDIF;
038200170217           reade (TAMksc:TAMctr:wprog) TITPD01L;
038300170217         ENDDO;
038400170217
038500170217         setll (TAMksc:TAMctr:wprog) TITGC01L;
038600170217         reade (TAMksc:TAMctr:wprog) TITGC01L;
038700170217         DOW  not %eof(TITGC01L);
038800170217           IF  TGCatb <> *blanks;
038900170217             delete TITGC01;
039000170217           ENDIF;
039100170217           reade (TAMksc:TAMctr:wprog) TITGC01L;
039200170217         ENDDO;
039300170217
039400170217         chain (TAMksc:TAMctr:wprog) TNTAM01L;
039500170217         IF  %found(TNTAM01L) and old_TAMatb <> *blanks;
039600170217           delete TNTAM01;
039700170217         ENDIF;
039800170217
039900170217         setll (TAMksc:TAMctr:wprog) TITAV01L;
040000170217         reade (TAMksc:TAMctr:wprog) TITAV01L;
040100170217         DOW  not %eof(TITAV01L);
040200170217           IF  TAVatb <> *blanks;
040300170217             delete TITAV000;
040400170217           ENDIF;
040500170217           reade (TAMksc:TAMctr:wprog) TITAV01L;
040600170217         ENDDO;
040700170217
040800170217       ENDSR;
040900170217
041000170217       //--------------------------------------------------------------
041100170217       //?Cerco il progressivo precedente.
041200170217       //--------------------------------------------------------------
041300170217       BEGSR OldProgressivo;
041400170217
041500170217         Fine = *off;
041600170217         oldprg = TAMprg - 1;
041700170217
041800170217       //?Cerco il progressivo precedente
041900170217         DOW  not Fine;
042000170217           chain(n) (TAMksc:TAMctr:oldprg) TNTAM01L;
042100170217           IF  not %found(TNTAM01L);
042200170217             Fine = *on;
042300170217             leave;
042400170217           ENDIF;
042500170217         //?Se il progressivo precedente ha data decorrenza < 01/03/2017 e
042600170217         //?data scadenza >= 01/03/2017
042700170217           IF  old_TAMddt < 20170301 and old_TAMdst >= 20170301 ;
042800170217          //?imposto il flag perchè in ogni caso un progressivo nuovo lo devo scrivere
042900170217             FlagD = *on;
043000170217             Fine = *on;
043100170217             leave;
043200170217           ENDIF;
043300170217          //?Se progressivo precedente a 0 vado via
043400170217           IF  oldprg = 0;
043500170217             Fine = *on;
043600170217             leave;
043700170217           ENDIF;
043800170217          //?leggo altro progressivo OLD
043900170217           oldprg = TAMprg - 1;
044000170217         ENDDO;
044100170217
044200170217       ENDSR;
044300170217
044400170217       //--------------------------------------------------------------
044500170217       //?Aumento di 1 il progressivo attuale.
044600170217       //--------------------------------------------------------------
044700170217       BEGSR AumentaPrgAttuale;
044800170217
044900170217       //?Dettaglio tariffario
045000170217         setll (TAMksc:TAMctr:TAMprg) TITAD04L;
045100170217         reade (TAMksc:TAMctr:TAMprg) TITAD04L;
045200170217         DOW  not %eof(TITAD04L);
045300170217           IF  TADatb = *blanks;
045400170217             TADprg += 1;
045500170217             clear TADftr;
045600170217             TADdtr = oggi;
045700170217             update TITAD04;
045800170217           ENDIF;
045900170217           reade (TAMksc:TAMctr:TAMprg) TITAD04L;
046000170217         ENDDO;
046100170217
046200170217       //?Testata tariffe particolari
046300170217         setll (TAMksc:TAMctr:TAMprg) TITPT01L;
046400170217         reade (TAMksc:TAMctr:TAMprg) TITPT01L;
046500170217         DOW  not %eof(TITPT01L);
046600170217           IF  TPTatb = *blanks;
046700170217             TPTprg += 1;
046800170217             TPTduv = oggi;
046900170217             clear TPTftr;
047000170217             TPTdtr = oggi;
047100170217             update TITPT01;
047200170217           ENDIF;
047300170217           reade (TAMksc:TAMctr:TAMprg) TITPT01L;
047400170217         ENDDO;
047500170217
047600170217       //?Dettaglio tariffe particolari
047700170217         setll (TAMksc:TAMctr:TAMprg) TITPD01L;
047800170217         reade (TAMksc:TAMctr:TAMprg) TITPD01L;
047900170217         DOW  not %eof(TITPD01L);
048000170217           IF  TPDatb = *blanks;
048100170217             TPDprg += 1;
048200170217             clear TPDftr;
048300170217             TPDdtr = oggi;
048400170217             update TITPD01;
048500170217           ENDIF;
048600170217           reade (TAMksc:TAMctr:TAMprg) TITPD01L;
048700170217         ENDDO;
048800170217
048900170217      //?devo aggiornare anche le tariffe particolari nuove
049000170217         wprog = TAMprg + 1;
049100170217         exsr AggTarParticolari;
049200170217
049300170217       //?Tariffe di giacenza
049400170217         setll (TAMksc:TAMctr:TAMprg) TITGC01L;
049500170217         reade (TAMksc:TAMctr:TAMprg) TITGC01L;
049600170217         DOW  not %eof(TITGC01L);
049700170217           IF  TGCatb = *blanks;
049800170217             TGCprg += 1;
049900170217             TGCduv = oggi;
050000170217             clear TGCftr;
050100170217             TGCdtr = oggi;
050200170217             update TITGC01;
050300170217           ENDIF;
050400170217           reade (TAMksc:TAMctr:TAMprg) TITGC01L;
050500170217         ENDDO;
050600170217
050700170217       //?Storico Variazioni
050800170217         setll (TAMksc:TAMctr:TAMprg) TITAV01L;
050900170217         reade (TAMksc:TAMctr:TAMprg) TITAV01L;
051000170217         DOW  not %eof(TITAV01L);
051100170217           IF  TAVatb = *blanks;
051200170217             TAVprg += 1;
051300170217             clear TAVftr;
051400170217             TAVdtr = oggi;
051500170217             update TITAV000;
051600170217           ENDIF;
051700170217           reade (TAMksc:TAMctr:TAMprg) TITAV01L;
051800170217         ENDDO;
051900170220
052000170220       //?Testata Tariffe
052100170220         TAMprg += 1;
052200170220         TAMdtr = oggi;
052300170220         clear TAMftr;
052400170220         update TNTAM04;
052500170220
052600170220       //?Storico variazioni
052700170220         clear TITAV000;
052800170220         TAVksc = TAMksc;
052900170220         TAVctr = TAMctr;
053000170220         TAVprg = TAMprg;
053100170220         TAVtrd = 'TES';
053200170220         TAVcta = 'M';
053300170220         TAVdav = oggi;
053400170220         TAVorv = %dec(%time());
053500170220         TAVnoj = 'DV-DPD';
053600170220         TAVpru = 'BATCH';
053700170220         TAVdtr = oggi;
053800170220         write TITAV000;
053900170217
054000170217       ENDSR;
054100170217
054200170217       //--------------------------------------------------------------
054300170217       //?Aggiorno le Tariffe Particolari.
054400170217       //--------------------------------------------------------------
054500170217       BEGSR AggTarParticolari;
054600170217
054700170217       //?Se non c'era la testata relativa al dirottamento o al fuori misura
054800170217       //?devo scriverle uguali alla tariffa di cartello
054900170217       //?e ormai che ci sono scrivo anche il dettaglio della tariffa particolare
055000170220         IF  sav_WFAdir = 'A' or sav_WFAfuo = 'A';
055100170220           IF  sav_WFAdir = 'A';
055200170217             exsr NewTestataDirottamento;
055300170217             exsr NewDettaglioDirottamento;
055400170217           ENDIF;
055500170220           IF  sav_WFAfuo = 'A';
055600170217             exsr NewTestataFuoriMisura;
055700170217             exsr NewDettaglioFuoriMisura;
055800170217           ENDIF;
055900170217         //?Scrivo anche lo storico variazioni
056000170217           exsr StoricoTarParticolari;
056100170217         ENDIF;
056200170217
056300170217       //?Se c'era la testata relativa al dirottamento o al fuori misura
056400170217       //?ma non il relativo dettaglio, lo devo scrivere
056500170220         IF  sav_WFAdir = 'T' or sav_WFAfuo = 'T';
056600170220           IF  sav_WFAdir = 'T';
056700170217             exsr NewDettaglioDirottamento;
056800170217         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
056900170217             chain (TAMksc:TAMctr:wprog:dirottamento) TITPT01L;
057000170217             IF  %found(TITPT01L);
057100170217               TPTduv = oggi;
057200170217               clear TPTftr;
057300170217               TPTdtr = oggi;
057400170217               update TITPT01;
057500170217             ENDIF;
057600170217           ENDIF;
057700170220           IF  sav_WFAfuo = 'T';
057800170217             exsr NewDettaglioFuoriMisura;
057900170217         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
058000170217             chain (TAMksc:TAMctr:wprog:fuorimisura) TITPD01L;
058100170217             IF  %found(TITPT01L);
058200170217               TPTduv = oggi;
058300170217               clear TPTftr;
058400170217               TPTdtr = oggi;
058500170217               update TITPT01;
058600170217             ENDIF;
058700170217           ENDIF;
058800170217         //?Scrivo anche lo storico variazioni
058900170217           exsr StoricoTarParticolari;
059000170217         ENDIF;
059100170217
059200170217       //?Se la tariffa particolare è da variare l'aggancio e la modifico
059300170220         IF  sav_WFAdir = 'M' or sav_WFAfuo = 'M';
059400170217         //?Dirottamento
059500170220           IF  sav_WFAdir = 'M';
059600170217             chain (TAMksc:TAMctr:wprog:dirottamento) TITPD01L;
059700170217             IF  %found(TITPD01L);
059800170217               TPDsgl = 9999999;
059900170217               TPDitr = 6;
060000170217               clear TPDftr;
060100170217               TPDdtr = oggi;
060200170217               update TITPD01;
060300170217             ENDIF;
060400170217         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
060500170217             chain (TAMksc:TAMctr:wprog:dirottamento) TITPT01L;
060600170217             IF  %found(TITPT01L);
060700170217               TPTduv = oggi;
060800170217               clear TPTftr;
060900170217               TPTdtr = oggi;
061000170217               update TITPT01;
061100170217             ENDIF;
061200170217           ENDIF;
061300170217         //?Fuori Misura
061400170220           IF  sav_WFAfuo = 'M';
061500170217             chain (TAMksc:TAMctr:wprog:fuorimisura) TITPD01L;
061600170217             IF  %found(TITPD01L);
061700170217               TPDsgl = 9999999;
061800170217               TPDitr = 28,922;
061900170217               clear TPDftr;
062000170217               TPDdtr = oggi;
062100170217               update TITPD01;
062200170217             ENDIF;
062300170217         //?Imposto oggi come data ultima variazione sulla testata della tariffa particolare
062400170217             chain (TAMksc:TAMctr:wprog:fuorimisura) TITPT01L;
062500170217             IF  %found(TITPT01L);
062600170217               TPTduv = oggi;
062700170217               clear TPTftr;
062800170217               TPTdtr = oggi;
062900170217               update TITPT01;
063000170217             ENDIF;
063100170217           ENDIF;
063200170217         //?Scrivo anche lo storico variazioni
063300170217           exsr StoricoTarParticolari;
063400170217         ENDIF;
063500170217
063600170217       ENDSR;
063700170210
063800170210       //--------------------------------------------------------------
063900170210       //?Scrivo un nuovo progressivo.
064000170210       //--------------------------------------------------------------
064100170210       BEGSR NewProgressivo;
064200170213
064300170213         NewPrg = *on;
064400170217
064500170217         wprog = TAMprg + 1;
064600170210
064700170210       //?Dettaglio tariffario
064800170217         setll (TAMksc:TAMctr:TAMprg) TITAD04L;
064900170217         reade (TAMksc:TAMctr:TAMprg) TITAD04L;
065000170210         DOW  not %eof(TITAD04L);
065100170210           clear TITAD000;
065200170210           new_TADksc = TADksc;
065300170210           new_TADctr = TADctr;
065400170217           new_TADprg = TADprg + 1;
065500170210           new_TADlnp = TADlnp;
065600170210           new_TADcts = TADcts;
065700170210           new_TADnaz = TADnaz;
065800170210           new_TADcap = TADcap;
065900170210           new_TADsgl = TADsgl;
066000170210           new_TADitr = TADitr;
066100170210           new_TADino = TADino;
066200170210           new_TADrpv = TADrpv;
066300170210           new_TADorp = TADorp;
066400170210           new_TADmin = TADmin;
066500170210           new_TADmax = TADmax;
066600170210           new_TADdtr = oggi;
066700170210           write TITAD000;
066800170217           reade (TAMksc:TAMctr:TAMprg) TITAD04L;
066900170210         ENDDO;
067000170220
067100170220         IF  FlagD;
067200170220           clear sav_WFAdir;
067300170220           clear sav_WFAfuo;
067400170220         ENDIF;
067500170210
067600170210       //?Testata tariffe particolari
067700170217         setll (TAMksc:TAMctr:TAMprg) TITPT01L;
067800170217         reade (TAMksc:TAMctr:TAMprg) TITPT01L;
067900170210         DOW  not %eof(TITPT01L);
068000170210           clear TITPT000;
068100170210           new_TPTksc = TPTksc;
068200170210           new_TPTctr = TPTctr;
068300170217           new_TPTprg = TPTprg +1;
068400170210           new_TPTduv = oggi;
068500170210           new_TPTftc = TPTftc;
068600170210           new_TPTtpg = TPTtpg;
068700170210           new_TPTarl = TPTarl;
068800170210           new_TPTarf = TPTarf;
068900170210           new_TPTaro = TPTaro;
069000170210           new_TPTrpv = TPTrpv;
069100170210           new_TPTvlm = TPTvlm;
069200170210           new_TPTvvm = TPTvvm;
069300170210           new_TPTfvm = TPTfvm;
069400170210           new_TPTtma = TPTtma;
069500170210           new_TPTapl = TPTapl;
069600170210           new_TPTdtr = oggi;
069700170210           new_TPTdpb = TPTdpb;
069800170210           new_TPTdit = TPTdit;
069900170210           new_TPTflo = TPTflo;
070000170210           write TITPT000;
070100170220           IF  FlagD and TPTftc = Dirottamento;
070200170220             OkDir = *on;
070300170220           ENDIF;
070400170220           IF  FlagD and TPTftc = FuoriMisura;
070500170220             OkFuo = *on;
070600170220           ENDIF;
070700170217           reade (TAMksc:TAMctr:TAMprg) TITPT01L;
070800170210         ENDDO;
070900170220
071000170220         IF  FlagD;
071100170220         IF  not OkDir;
071200170220           sav_WFAdir = 'A';
071300170220         ENDIF;
071400170220         IF  not OkFuo;
071500170220           sav_WFAfuo = 'A';
071600170220         ENDIF;
071700170220         ENDIF;
071800170210
071900170210       //?Se non c'era la testata relativa al dirottamento o al fuori misura
072000170210       //?devo scriverle uguali alla tariffa di cartello
072100170220         IF  sav_WFAdir = 'A';
072200170210           exsr NewTestataDirottamento;
072300170210         ENDIF;
072400170220         IF  sav_WFAfuo = 'A';
072500170210           exsr NewTestataFuoriMisura;
072600170210         ENDIF;
072700170210
072800170210       //?Dettaglio tariffe particolari
072900170217         setll (TAMksc:TAMctr:TAMprg) TITPD01L;
073000170217         reade (TAMksc:TAMctr:TAMprg) TITPD01L;
073100170210         DOW  not %eof(TITPD01L);
073200170210           clear TITPD000;
073300170210           new_TPDksc = TPDksc;
073400170210           new_TPDctr = TPDctr;
073500170217           new_TPDprg = TPDprg + 1;
073600170210           new_TPDftc = TPDftc;
073700170210           new_TPDcts = TPDcts;
073800170210           new_TPDorp = TPDorp;
073900170210           new_TPDsgl = TPDsgl;
074000170210           new_TPDitr = TPDitr;
074100170210           new_TPDmin = TPDmin;
074200170210           new_TPDmax = TPDmax;
074300170210           new_TPDain = TPDain;
074400170210           new_TPDdtr = oggi;
074500170210         //?Se la tariffa particolare era negata prima di scriverla imposto il nuovo valore
074600170220           IF  sav_WFAdir = 'M' and new_TPDftc = dirottamento;
074700170210             new_TPDsgl = 9999999;
074800170210             new_TPDitr = 6;
074900170210           ENDIF;
075000170220           IF  sav_WFAfuo = 'M' and new_TPDftc = fuorimisura;
075100170210             new_TPDsgl = 9999999;
075200170210             new_TPDitr = 28,922;
075300170210           ENDIF;
075400170210           write TITPD000;
075500170217           reade (TAMksc:TAMctr:TAMprg) TITPD01L;
075600170210         ENDDO;
075700170210       //?Se non c'era il dettaglio relativo al dirottamento o al fuori misura lo
075800170210       //?devo scrivere adesso
075900170220         IF  sav_WFAdir = 'T' or sav_WFAdir = 'A';
076000170210           exsr NewDettaglioDirottamento;
076100170210         ENDIF;
076200170220         IF  sav_WFAfuo = 'T' or sav_WFAfuo = 'A';
076300170210           exsr NewDettaglioFuoriMisura;
076400170210         ENDIF;
076500170210
076600170210       //?Scrivo lo storico delle variazioni per le tariffe particolari
076700170210         exsr StoricoTarParticolari;
076800170210
076900170210       //?Tariffe di giacenza
077000170217         setll (TAMksc:TAMctr:TAMprg) TITGC01L;
077100170217         reade (TAMksc:TAMctr:TAMprg) TITGC01L;
077200170210         DOW  not %eof(TITGC01L);
077300170210           clear TITGC000;
077400170210           new_TGCksc = TGCksc;
077500170210           new_TGCctr = TGCctr;
077600170217           new_TGCprg = TGCprg + 1;
077700170210           new_TGCsgv = TGCsgv;
077800170210           new_TGCsgr = TGCsgr;
077900170210           new_TGCsgp = TGCsgp;
078000170210           new_TGCsgd = TGCsgd;
078100170210           new_TGCsg1 = TGCsg1;
078200170210           new_TGCsg2 = TGCsg2;
078300170210           new_TGCsg3 = TGCsg3;
078400170210           new_TGCst1 = TGCst1;
078500170210           new_TGCst2 = TGCst2;
078600170210           new_TGCst3 = TGCst3;
078700170210           new_TGCstm = TGCstm;
078800170210           new_TGCrpv = TGCrpv;
078900170210           new_TGCfaf = TGCfaf;
079000170210           new_TGCsgf = TGCsgf;
079100170210           new_TGCggr = TGCggr;
079200170210           new_TGCtcm = TGCtcm;
079300170210           new_TGCtfg = TGCtfg;
079400170210           new_TGCduv = oggi;
079500170210           new_TGCdtr = oggi;
079600170210           write TITGC000;
079700170217           reade (TAMksc:TAMctr:TAMprg) TITGC01L;
079800170210         ENDDO;
079900170210
080000170210       //?Testata tariffe
080100170217         new_TAMksc = TAMksc;
080200170217         new_TAMctr = TAMctr;
080300170217         new_TAMprg = TAMprg + 1;
080400170217         new_TAMddt = decorrenza;
080500170217         new_TAMdst = scadenza;
080600170217         new_TAMduv = oggi;
080700170217         new_TAMdcv = TAMdcv;
080800170217         new_TAMdif = TAMdif;
080900170217         new_TAMfli = TAMfli;
081000170217         new_TAMpri = TAMpri;
081100170217         new_TAMfmp = TAMfmp;
081200170217         new_TAMlrc = TAMlrc;
081300170217         new_TAMlas = TAMlas;
081400170217         new_TAMpag = TAMpag;
081500170217         new_TAMmpa = TAMmpa;
081600170217         new_TAMpam = TAMpam;
081700170217         new_TAMmpm = TAMmpm;
081800170217         new_TAMtin = TAMtin;
081900170217         new_TAMkpm = TAMkpm;
082000170217         new_TAMarl = TAMarl;
082100170217         new_TAMarf = TAMarf;
082200170217         new_TAMaro = TAMaro;
082300170217         new_TAMrat = TAMrat;
082400170217         new_TAMrct = TAMrct;
082500170217         new_TAMpri = TAMpri;
082600170217         new_TAMsc2 = TAMsc2;
082700170217         new_TAMeds = TAMeds;
082800170217         new_TAMars = TAMars;
082900170217         new_TAMata = TAMata;
083000170217         new_TAMftp = TAMftp;
083100170217         new_TAMbap = TAMbap;
083200170217         new_TAMtsp = TAMtsp;
083300170217         new_TAMfie = TAMfie;
083400170217         new_TAMfis = TAMfis;
083500170217         new_TAMsis = TAMsis;
083600170217         new_TAMfvd = TAMfvd;
083700170217         new_TAMfts = TAMfts;
083800170217         new_TAMnas = TAMnas;
083900170217         new_TAMnot = TAMnot;
084000170217         new_TAMftm = TAMftm;
084100170217         new_TAMgca = TAMgca;
084200170217         new_TAMgga = TAMgga;
084300170217         new_TAMgma = TAMgma;
084400170217         new_TAMgva = TAMgva;
084500170217         new_TAMbam = TAMbam;
084600170217         new_TAMtpr = TAMtpr;
084700170217         new_TAMflo = TAMflo;
084800170217         new_TAMdat = TAMdat;
084900170217         new_TAMdtr = oggi;
085000170217         write TNTAM000;
085100170210
085200170210       //?Storico variazioni
085300170210         clear TITAV000;
085400170210         TAVksc = new_TAMksc;
085500170210         TAVctr = new_TAMctr;
085600170210         TAVprg = new_TAMprg;
085700170210         TAVtrd = 'TES';
085800170210         TAVcta = 'D';
085900170210         TAVdav = oggi;
086000170210         TAVorv = %dec(%time());
086100170210         TAVnoj = 'DV-DPD';
086200170210         TAVpru = 'BATCH';
086300170210         TAVdtr = oggi;
086400170210         write TITAV000;
086500170210
086600170210       //?File di WORK
086700170217         chain ('T':TAMksc:TAMctr:sav_TAMprg) WFAUD01L;
086800170210         IF  %found(WFAUD01L);
086900170220           IF  not FlagD;
087000170210           WFAddtn = new_TAMddt;
087100170210           WFAdstn = new_TAMdst;
087200170210           WFAprgn = new_TAMprg;
087300170220           EDIF;
087400170220           IF  FlagD;
087500170220           WFAflgD = 'S';
087600170220           ENDIF;
087700170210           update WFAUD000;
087800170210         ENDIF;
087900170210
088000170210       ENDSR;
088100170210
088200170210       //--------------------------------------------------------------
088300170210       //?Scrivo la nuova testata del dirottamento da cartello.
088400170210       //--------------------------------------------------------------
088500170210       BEGSR NewTestataDirottamento;
088600170210
088700170210         clear TITPT000;
088800170217         new_TPTksc = TAMksc;
088900170217         new_TPTctr = TAMctr;
089000170217         new_TPTprg = wprog;
089100170210         new_TPTduv = oggi;
089200170210         new_TPTftc = dir_TPTftc;
089300170210         new_TPTtpg = dir_TPTtpg;
089400170210         new_TPTarl = dir_TPTarl;
089500170210         new_TPTarf = dir_TPTarf;
089600170210         new_TPTaro = dir_TPTaro;
089700170210         new_TPTrpv = dir_TPTrpv;
089800170210         new_TPTvlm = dir_TPTvlm;
089900170210         new_TPTvvm = dir_TPTvvm;
090000170210         new_TPTfvm = dir_TPTfvm;
090100170210         new_TPTtma = dir_TPTtma;
090200170210         new_TPTapl = dir_TPTapl;
090300170210         new_TPTdtr = oggi;
090400170210         new_TPTdpb = dir_TPTdpb;
090500170210         new_TPTdit = oggi;
090600170210         new_TPTflo = dir_TPTflo;
090700170210         write TITPT000;
090800170210
090900170210       ENDSR;
091000170210
091100170210       //--------------------------------------------------------------
091200170210       //?Scrivo la nuova testata del fuori misura da cartello.
091300170210       //--------------------------------------------------------------
091400170210       BEGSR NewTestataFuoriMisura;
091500170210
091600170210         clear TITPT000;
091700170217         new_TPTksc = TAMksc;
091800170217         new_TPTctr = TAMctr;
091900170217         new_TPTprg = wprog;
092000170215         new_TPTduv = oggi;
092100170213         new_TPTftc = fuo_TPTftc;
092200170213         new_TPTtpg = fuo_TPTtpg;
092300170213         new_TPTarl = fuo_TPTarl;
092400170213         new_TPTarf = fuo_TPTarf;
092500170213         new_TPTaro = fuo_TPTaro;
092600170213         new_TPTrpv = fuo_TPTrpv;
092700170213         new_TPTvlm = fuo_TPTvlm;
092800170213         new_TPTvvm = fuo_TPTvvm;
092900170213         new_TPTfvm = fuo_TPTfvm;
093000170213         new_TPTtma = fuo_TPTtma;
093100170213         new_TPTapl = fuo_TPTapl;
093200170210         new_TPTdtr = oggi;
093300170213         new_TPTdpb = fuo_TPTdpb;
093400170210         new_TPTdit = oggi;
093500170213         new_TPTflo = fuo_TPTflo;
093600170210         write TITPT000;
093700170210
093800170210       ENDSR;
093900170210
094000170210       //--------------------------------------------------------------
094100170210       //?Scrivo il nuovo dettaglio del dirottamento.
094200170210       //--------------------------------------------------------------
094300170210       BEGSR NewDettaglioDirottamento;
094400170210
094500170210         clear TITPD000;
094600170217         new_TPDksc = TAMksc;
094700170217         new_TPDctr = TAMctr;
094800170217         new_TPDprg = wprog;
094900170210         new_TPDftc = dirottamento;
095000170210         new_TPDcts = 'IT';
095100170214         new_TPDorp = '  A 9';
095200170210         new_TPDsgl = 9999999;
095300170210         new_TPDitr = 6;
095400170210         new_TPDdtr = oggi;
095500170210         write TITPD000;
095600170210
095700170210       ENDSR;
095800170210
095900170210       //--------------------------------------------------------------
096000170210       //?Scrivo il nuovo dettaglio del fuori misura.
096100170210       //--------------------------------------------------------------
096200170210       BEGSR NewDettaglioFuoriMisura;
096300170210
096400170210         clear TITPD000;
096500170217         new_TPDksc = TAMksc;
096600170217         new_TPDctr = TAMctr;
096700170217         new_TPDprg = wprog;
096800170210         new_TPDftc = fuorimisura;
096900170210         new_TPDcts = 'IT';
097000170214         new_TPDorp = '  A 9';
097100170210         new_TPDsgl = 9999999;
097200170210         new_TPDitr = 28,922;
097300170210         new_TPDdtr = oggi;
097400170210         write TITPD000;
097500170210
097600170210       ENDSR;
097700170210
097800170210       //--------------------------------------------------------------
097900170210       //?Scrivo storico variazione per aggiunta tariffe paricolari.
098000170210       //--------------------------------------------------------------
098100170210       BEGSR StoricoTarParticolari;
098200170210
098300170220         IF  sav_WFAdir <> *blanks;
098400170210           clear TITAV000;
098500170217           TAVksc = TAMksc;
098600170217           TAVctr = TAMctr;
098700170217           TAVprg = wprog;
098800170210           TAVtrd = 'PAR';
098900170210           TAVftc = dirottamento;
099000170213           SELECT;
099100170220           WHEN  NewPrg or sav_WFAdir = 'A';
099200170213             TAVcta = 'I';
099300170220           WHEN  sav_WFAdir = 'M' or sav_WFAdir = 'T';
099400170213             TAVcta = 'M';
099500170213           ENDSL;
099600170210           TAVdav = oggi;
099700170210           TAVorv = %dec(%time());
099800170213           TAVnoj = 'DV-DPD';
099900170210           TAVpru = 'BATCH';
100000170210           TAVdtr = oggi;
100100170210           write TITAV000;
100200170210         ENDIF;
100300170210
100400170220         IF  sav_WFAfuo <> *blanks;
100500170210           clear TITAV000;
100600170217           TAVksc = TAMksc;
100700170217           TAVctr = TAMctr;
100800170217           TAVprg = wprog;
100900170210           TAVtrd = 'PAR';
101000170210           TAVftc = fuorimisura;
101100170213           SELECT;
101200170220           WHEN  NewPrg or sav_WFAfuo = 'A';
101300170213             TAVcta = 'I';
101400170220           WHEN  sav_WFAfuo = 'M' or sav_WFAfuo = 'T';
101500170213             TAVcta = 'M';
101600170213           ENDSL;
101700170210           TAVdav = oggi;
101800170210           TAVorv = %dec(%time());
101900170213           TAVnoj = 'DV-DPD';
102000170210           TAVpru = 'BATCH';
102100170210           TAVdtr = oggi;
102200170210           write TITAV000;
102300170210         ENDIF;
102400170210
102500170210       ENDSR;
102600170210
102700170210       //--------------------------------------------------------------
102800170210       //?Operazioni finali.
102900170210       //--------------------------------------------------------------
103000170210       BEGSR RoutEnd;
103100170210
103200170210         *inLR = *on;
103300170210         return;
103400170210
103500170210       ENDSR;
103600170210
103700170210      /end-free
