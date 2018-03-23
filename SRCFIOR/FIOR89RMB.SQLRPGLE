000100170518      //---------------------------------------------------------------
000200170518      //?FIOR89R - Statistica Ritiri con peso < 5 Kg. - Anticipati
000300170518      //---------------------------------------------------------------
000400170518     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170518     h dftactgrp(*no) actgrp(*caller)
000600170518     h bnddir('UBRTVNETA')
000700170518
000800170518      //---------------------------------------------------------------
000900170518      //?Dichiarazione file.
001000170518      //---------------------------------------------------------------
001100170518      // - Organigramma
001200170518     fAZORG01L  if   e           k disk
001300170518
001400170518      // - File di Work
001500170518     fWFSRA00F  o    e             disk
001600170518
001700170518      //---------------------------------------------------------------
001800170518      //?Definizione costanti.
001900170518      //---------------------------------------------------------------
002000170518
002100170518      //---------------------------------------------------------------
002200170518      //?Definizione schiere.
002300170518      //---------------------------------------------------------------
002400170518
002500170518      //---------------------------------------------------------------
002600170518      //?Definizione aree dati.
002700170518      //---------------------------------------------------------------
002800170518      // - Dati utente
002900170518     d §AzUte        e ds                  extname(AZUTE00F)
003000170518     d                                     dtaara
003100170518     d §DatiUte      e ds                  extname(dDatiUte)
003200170518     d                                     dtaara
003300170518
003400170518      //---------------------------------------------------------------
003500170518      //?Definizione strutture dati.
003600170518      //---------------------------------------------------------------
003700170518      // - Parametri ricevuti
003800170518     d KPJBA         e ds
003900170518     d FIOR89DS      e ds                  inz
004000170518
004100170518      // - Reperimento dati in tabella
004200170518     d TIBS02DS      e ds
004300170518
004400170518      // - Reperimento dati utente
004500170518     d TIBS34DS      e ds
004600170518
004700170518      // - Reperimento filiali gestite dall'utente
004800170518     d TRUL31DS      e ds
004900170518     d  skPOG                 10    759    dim(250)
005000170608
005100170608      // - Tabella GPD - Giorni Posticipo Data Ritiro
005200170608     d dGPD          e ds
005300170608     d  skGiorni               1     14s 0 dim(7)
005400170608     d  skGiorniA             15     28s 0 dim(7)
005500170518
005600170518      // - Tabella LAT - Autorizzazioni
005700170518     d dLAT          e ds
005800170518
005900170518      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
006000170518     d dOREdt        e ds
006100170518
006200170518      // - OG143 - lettura campo ORGde3 file AZORG00F
006300170518     d OG143         e ds
006400170518
006500170518      // - OG148 - lettura campo ORGde8 file AZORG00F
006600170518     d OG148         e ds
006700170518
006800170518      // File ORM
006900170518     d FNORM00F      e ds                  extname(FNORM00F)
007000170608     d FNORE00F      e ds                  extname(FNORE00F)
007100170518
007200170518      // - Mattino/Pomeriggio calendario
007300170518     d CLNmat          DS
007400170518     d  mat                    1     31    dim(31)
007500170518     d CLNpom          DS
007600170518     d  pom                    1     31    dim(31)
007700170518
007800170518      //---------------------------------------------------------------
007900170518      //?Definizione variabili globali.
008000170518      //---------------------------------------------------------------
008100170518      // - Flags booleani
008200170518     d EndSqlORM       s               n   inz(*off)
008300170518     d Fine            s               n   inz(*off)
008400170518
008500170518       // - Stringa SQL da eseguire
008600170518     d wSQL            s           2048    Varying        inz
008700170518
008800170518      // - Indici di schiera
008900170518     d indicex         s              4s 0 inz
009000170518     d xx              s              4s 0 inz
009100170518
009200170518      // - Campi di comodo data
009300170518     d Data_ISO        s               d   datfmt(*iso)
009400170518     d Data_ISO_Fine   s               d   datfmt(*iso)
009500170518     d Data_ISO_Rit    s               d   datfmt(*iso)
009600170518     d wData_ISO       s               d   datfmt(*iso)
009700170518
009800170518      // - Campi di comodo
009900170519     d Filiale         s              3s 0 inz
010000170518     d Oggi            s              8s 0 inz
010100170518     d wAbi            s              2a   inz
010200170518     d wData           s              8s 0 inz
010300170518     d wDataFestivo    s              8s 0 inz
010400170518     d wDay_Of_Week    s              2  0 inz
010500170518     d wFil            s              3s 0 inz
010600170518     d wGiorno         s              2s 0 inz
010700170518     d wMatPom         s              1a   inz
010800170518
010900170518      //---------------------------------------------------------------
011000170518      //?Definizione procedure esterne.
011100170518      //---------------------------------------------------------------
011200170518      // - Caricamento Filiali in gestione
011300170518     d TRUL31R         pr                  extpgm('TRUL31R')
011400170518     d  kpjba                              likeds(kpjba)
011500170518     d  trul31ds                           likeds(trul31ds)
011600170518
011700170518      //---------------------------------------------------------------
011800170518      //?Definizione prototipi.
011900170518      //---------------------------------------------------------------
012000170518      /copy gaitrasrc/srcprotopr,TIBS02R
012100170518      /copy gaitrasrc/srcprotopr,TIBS34R
012200170518
012300170518      //---------------------------------------------------------------
012400170518      //?Definizione key-list.
012500170518      //---------------------------------------------------------------
012600170518      // - File AZCLN01L
012700170518     d kazcln01      e ds                  extname(AZCLN01L:*key)
012800170518     d                                     prefix(k_)
012900170518     d                                     inz
013000170518
013100170518      //---------------------------------------------------------------
013200170518      //?M A I N - L I N E
013300170518      //---------------------------------------------------------------
013400170518     c     *Entry        plist
013500170518     c                   parm                    kpjba
013600170518
013700170518       //?Operazioni iniziali
013800170518       exsr RoutInz;
013900170518
014000170518       //?Elaborazione Statistica per filiale e data
014100170518       //?Leggo per filiale
014200170518       xx = 1;
014300170518       FOR  xx by 1 to %elem(skPOG);
014400170518         IF  skPOG(xx) > *zeros;
014500170518         //?Leggo per data
014600170518           Data_ISO  = (%date(IOR89datai));
014700170518           Data_ISO_Fine = (%date(IOR89dataf));
014800170518           DOW  Data_ISO <= Data_ISO_Fine;
014900170518             wFil = %dec(skPOG(xx):3:0);
015000170518             wData = %dec(Data_ISO);
015100170518             exsr Elabora;
015200170518             Data_ISO += %days(1);
015300170518           ENDDO;
015400170518         ENDIF;
015500170518       ENDFOR;
015600170518
015700170518       //?Operazioni finali
015800170518       exsr RoutEnd;
015900170518
016000170518       //--------------------------------------------------------------
016100170518       //?Operazioni iniziali.
016200170518       //--------------------------------------------------------------
016300170518       BEGSR RoutInz;
016400170518
016500170518         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016600170518
016700170518       //?Imposto oggi
016800170518         Oggi = %dec(%date());
016900170518
017000170518       //?Reperimento dati job
017100170518         exsr DatiJob;
017200170518
017300170518       //?Imposto la DS dalla KPJBU
017400170518         FIOR89DS = kpjbu;
017500170518
017600170518       //?Se richieste tutte le filiali carico in sk le filiali abilitate all'utente
017700170518         IF  IOR89fil = 999;
017800170518           clear TRUL31DS;
017900170518           I31abi = wAbi;
018000170518           I31cdi = DUTdis;
018100170518           I31car = DUTare;
018200170518           I31cpo = DUTpou;
018300170518           TRUL31R (kpjba:trul31ds);
018400170518           IF  O31pog <= *zeros;
018500170518             exsr RoutEnd;
018600170518           ENDIF;
018700170518         ELSE;
018800170518           skPOG(1) = %editc(IOR89fil:'X');
018900170518         ENDIF;
019000170518
019100170518       //?Pulisco il file di work
019200170518         exec sql DELETE from GAITRAAZM/WFSRA00F;
019300170518
019400170518       ENDSR;
019500170518
019600170518       //--------------------------------------------------------------
019700170518       //?Reperimento Dati del job (Utente/Operativi).
019800170518       //--------------------------------------------------------------
019900170518       BEGSR DatiJob;
020000170518
020100170518         in(E) §AzUte;
020200170518         IF  NOT %error;
020300170518           in(E) §DatiUte;
020400170518         ENDIF;
020500170518         IF  %error or RSut = *blanks;
020600170518           clear TIBS34ds;
020700170518           tibs34r(tibs34ds);
020800170518           in §AzUte;
020900170518           in §DatiUte;
021000170518         ENDIF;
021100170518
021200170518         clear wAbi;
021300170518         clear dLAT;
021400170518
021500170518       //?Controllo autorizzazioni profilo
021600170518         SELECT;
021700170518         WHEN  DUTerr = 'E';
021800170518           exsr RoutEnd;
021900170518           leavesr;
022000170518         WHEN  UTEaut <> *blanks;
022100170518           wAbi = UTEaut;
022200170518         OTHER;
022300170518           IF  DUTlpo = 'S';
022400170518             wAbi = 'AZ';
022500170518           ENDIF;
022600170518           IF  DUTlpo = '1';
022700170518             wAbi = 'TP';
022800170518           ENDIF;
022900170518           IF  DUTlpo = '2';
023000170518             wAbi = 'PO';
023100170518           ENDIF;
023200170518         ENDSL;
023300170518
023400170518       //?Controllo se l'autorizzazione è valida
023500170518         clear dLAT;
023600170518         clear TIBS02DS;
023700170518         T02mod = 'C';
023800170518         T02sif = knsif;
023900170518         T02cod = 'LAT';
024000170518         T02ke1 = wAbi;
024100170518         TNTBE_RicercaControllo (kpjba:tibs02ds);
024200170518         dLAT = T02uni;
024300170518         IF  T02err <> *blanks or §LATabi = 'S';
024400170518           exsr RoutEnd;
024500170518         ENDIF;
024600170518
024700170518       ENDSR;
024800170518
024900170518       //--------------------------------------------------------------
025000170518       //?Elabora Statistica.
025100170518       //--------------------------------------------------------------
025200170518       BEGSR Elabora;
025300170519
025400170519         clear OG148;
025500170519         chain (wFil) AZORG01L;
025600170519         IF  %found(AZORG01L) and ORGfva = *blanks and
025700170519             (ORGfag = 'F' or ORGfag = 'A');
025800170519           OG148 = ORGde8;
025900170519         ENDIF;
026000170519       //?Non elaboro se filiale non partita con la procedura ORM
026100170519         IF  §OGorm <> 'S';
026200170519           leavesr;
026300170519         ENDIF;
026400170518
026500170518         clear WFSRA000;
026600170519         SRAdes = ORGdes;
026700170608
026800170608       //?Cerco che giorno della settimana è il giorno di immissione
026900170608         clear wDay_Of_Week;
027000170608         exec sql
027100170608         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
027200170608
027300170608       //?Recupero tabella GPD della filiale emissione
027400170608         Filiale = wFil;
027500170608         exsr Tabella_GPD;
027600170608         SRAgg = skGiorni(wDay_Of_Week);
027700170518
027800170518         EndSqlORM = *off;
027900170518
028000170518       //?Preparo la stringa SQL
028100170518         exsr StringaSql;
028200170518
028300170518       //?Leggo i dati
028400170518         exec sql PREPARE S1 from :wSQL;
028500170518         exec sql DECLARE WORM cursor for S1;
028600170518
028700170518       //?Apertura del cursore
028800170518         exec sql open WORM;
028900170518
029000170518         IF  sqlcode < 0;
029100170518           EndSqlORM = *on;
029200170518         ENDIF;
029300170518
029400170518         DOW  not EndSqlORM;
029500170518           exec sql
029600170608           fetch next from WORM into :FNORM00F, :FNORE00F;
029700170518           IF  sqlcod = 100 or sqlcod < 0;
029800170518             EndSqlORM = *on;
029900170518             leave;
030000170518           ENDIF;
030100170518
030200170518         //?Elaboro a dettaglio per ORM
030300170518           exsr DettaglioORM;
030400170518
030500170518         ENDDO;
030600170518
030700170518         //?Scrivo file di work
030800170518         IF  SRAtot > 0;
030900170518           exsr Scrivi;
031000170518         ENDIF;
031100170518
031200170518         exec sql close WORM;
031300170518
031400170518       ENDSR;
031500170518
031600170518       //--------------------------------------------------------------
031700170518       //?Preparazione della stringa SQL.
031800170518       //--------------------------------------------------------------
031900170518       BEGSR StringaSql;
032000170518
032100170518       //?Seleziono gli ORM
032200170518         wSQL =
032300170608         'SELECT * from FNORM00F join FNORE00F on -
032400170608                   ORMpoe = OREpoe and ORMnor = OREnor and -
032500170608                   ORMnsr = OREnsr and ORMnrv = OREnrv and -
032600170608                   OREtrc = ''DT'' -
032700170518                   WHERE ORMnsr = 0  and ORMtco in(''M'', ''E'')' +
032800170518                 ' and ORMpoe = ' + %editc(wFil:'X') +
032900170518                 ' and ORMdao = ' + %editc(wData:'X') +
033000170518                 ' order by ORMpoe, ORMnsr, ORMnor, ORMnrv';
033100170518       ENDSR;
033200170518
033300170518       //--------------------------------------------------------------
033400170518       //?Elaboro a dettaglio ORM.
033500170518       //--------------------------------------------------------------
033600170518       BEGSR DettaglioORM;
033700170519
033800170519         clear OG143;
033900170519         clear OG148;
034000170519         chain (ORMpor) AZORG01L;
034100170519         IF  %found(AZORG01L) and ORGfva = *blanks and
034200170519             (ORGfag = 'F' or ORGfag = 'A');
034300170519           OG143 = ORGde3;
034400170519           OG148 = ORGde8;
034500170519         ENDIF;
034600170519       //?Non elaboro se filiale non partita con la procedura ORM
034700170519         IF  §OGorm <> 'S';
034800170519           leavesr;
034900170519         ENDIF;
035000170519       //?Non elaboro se network estero
035100170519         IF  §OGntw = 'DPD' or §OGntw = 'EEX' or §OGntw = 'FED';
035200170519           leavesr;
035300170519         ENDIF;
035400170518
035500170608         dOREdt = OREdati;
035600170518
035700170518       //?non considero i ritiri che hanno
035800170608       //?flag di posticipo a blank
035900170608         IF  §OREposd = *blanks;
036000170518           leavesr;
036100170518         ENDIF;
036200170518
036300170518       //?Conto i Ritiri
036400170518         exsr Conta;
036500170518
036600170518       ENDSR;
036700170608
036800170608       //--------------------------------------------------------------
036900170608       //?Cerco tabella GPD.
037000170608       //--------------------------------------------------------------
037100170608       BEGSR Tabella_GPD;
037200170608
037300170608       //?Aggancio la tabella GPD con filiale ritiro ORM
037400170608         clear TIBS02ds;
037500170608         clear dGPD;
037600170608         T02mod = 'C';
037700170608         T02cod = 'GPD';
037800170608         T02sif = KNSIF;
037900170608         T02ke1 = %editc(Filiale:'X');
038000170608         TNTBE_RicercaControllo (kpjba : tibs02ds);
038100170608         IF  T02err = *blanks;
038200170608           dGPD = T02uni;
038300170608           leavesr;
038400170608         ENDIF;
038500170608       //?Non trovo GPD con filiale ritiro
038600170608       //?la cerco con filiale generica 999
038700170608         clear TIBS02ds;
038800170608         T02mod = 'C';
038900170608         T02cod = 'GPD';
039000170608         T02sif = KNSIF;
039100170608         T02ke1 = '999';
039200170608         TNTBE_RicercaControllo (kpjba : tibs02ds);
039300170608         IF  T02err = *blanks;
039400170608           dGPD = T02uni;
039500170608         ENDIF;
039600170608
039700170608       ENDSR;
039800170518
039900170518       //--------------------------------------------------------------
040000170518       //?Conto i ritiri.
040100170518       //--------------------------------------------------------------
040200170518       BEGSR Conta;
040300170518
040400170518       //?Conto totale ORM da posticipare
040500170518         SRAtot += 1;
040600170608
040700170608       //?Conto quanti ne ho fatti nella data calcolata
040800170608         IF  §OREgga = *zeros;
040900170608           SRAdrok += 1;
041000170609           leavesr;
041100170608         ENDIF;
041200170609
041300170609       //?Se data ritiro >= della data calcolata ma ho i gg di anticipo vuol dire
041400170609       //?che hanno variato la data ritiro quindi mi baso sui flag e sul numero
041500170609       //?di giorni anticipati e i gg di posticipo
041600170609         IF  §OREgga > *zeros and ORMdar >= §OREdar;
041700170609         //?Previsti più di 1 gg di posticipo ed ho anticipato di 1 gg
041800170609         //?l'ho pianificato per domani
041900170609           IF  SRAgg > 1 and §REgga = 1;
042000170609             SRAdrdi1 += 1;
042100170609             leavesr;
042200170609           ENDIF;
042300170609         //?Previsti più di 1 gg di posticipo ed ho anticipato di più 1 gg
042400170609         //?l'ho pianificato per oggi
042500170609           IF  SRAgg > 1 and §REgga > 1;
042600170609             SRAdrdi += 1;
042700170609             leavesr;
042800170609           ENDIF;
042900170609         //?Previsti 1 gg di posticipo ed ho anticipato di 1 gg
043000170609         //?l'ho pianificato per oggi
043100170609           IF  SRAgg = 1 and §REgga = 1;
043200170609             SRAdrdi += 1;
043300170609             leavesr;
043400170609           ENDIF;
043500170609         //?Quanti ne ho pianificati oggi  (giorno di immissione ORM)
043600170608           IF  ORMdao = ORMdar;
043700170608             SRAdrdi += 1;
043800170608             leavesr;
043900170608           ENDIF;
044000170609         //?Conto quanti ne ho pianificati domani (giorno dopo immissione ORM)
044100170608           SRAdrdi1 += 1;
044200170518         ENDIF;
044300170518
044400170518       ENDSR;
044500170518
044600170518       //--------------------------------------------------------------
044700170518       //?Scrivo il file di work.
044800170518       //--------------------------------------------------------------
044900170518       BEGSR Scrivi;
045000170518
045100170518         SRAfil = wFil;
045200170518         SRAdata = wData;
045300170518
045400170518         write WFSRA000;
045500170518
045600170518       ENDSR;
045700170518
045800170518       //--------------------------------------------------------------
045900170518       //?Operazioni finali.
046000170518       //--------------------------------------------------------------
046100170518       BEGSR RoutEnd;
046200170518
046300170518         *inLR = *on;
046400170518         return;
046500170518
046600170518       ENDSR;
