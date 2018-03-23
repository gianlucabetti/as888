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
010600170609     d wGiorni         s              2s 0 inz
010700170518
010800170518      //---------------------------------------------------------------
010900170518      //?Definizione procedure esterne.
011000170518      //---------------------------------------------------------------
011100170518      // - Caricamento Filiali in gestione
011200170518     d TRUL31R         pr                  extpgm('TRUL31R')
011300170518     d  kpjba                              likeds(kpjba)
011400170518     d  trul31ds                           likeds(trul31ds)
011500170518
011600170518      //---------------------------------------------------------------
011700170518      //?Definizione prototipi.
011800170518      //---------------------------------------------------------------
011900170518      /copy gaitrasrc/srcprotopr,TIBS02R
012000170518      /copy gaitrasrc/srcprotopr,TIBS34R
012100170518
012200170518      //---------------------------------------------------------------
012300170518      //?Definizione key-list.
012400170518      //---------------------------------------------------------------
012500170518      // - File AZCLN01L
012600170518     d kazcln01      e ds                  extname(AZCLN01L:*key)
012700170518     d                                     prefix(k_)
012800170518     d                                     inz
012900170518
013000170518      //---------------------------------------------------------------
013100170518      //?M A I N - L I N E
013200170518      //---------------------------------------------------------------
013300170518     c     *Entry        plist
013400170518     c                   parm                    kpjba
013500170518
013600170518       //?Operazioni iniziali
013700170518       exsr RoutInz;
013800170518
013900170518       //?Elaborazione Statistica per filiale e data
014000170518       //?Leggo per filiale
014100170518       xx = 1;
014200170518       FOR  xx by 1 to %elem(skPOG);
014300170518         IF  skPOG(xx) > *zeros;
014400170518         //?Leggo per data
014500170518           Data_ISO  = (%date(IOR89datai));
014600170518           Data_ISO_Fine = (%date(IOR89dataf));
014700170518           DOW  Data_ISO <= Data_ISO_Fine;
014800170518             wFil = %dec(skPOG(xx):3:0);
014900170518             wData = %dec(Data_ISO);
015000170518             exsr Elabora;
015100170518             Data_ISO += %days(1);
015200170518           ENDDO;
015300170518         ENDIF;
015400170518       ENDFOR;
015500170518
015600170518       //?Operazioni finali
015700170518       exsr RoutEnd;
015800170518
015900170518       //--------------------------------------------------------------
016000170518       //?Operazioni iniziali.
016100170518       //--------------------------------------------------------------
016200170518       BEGSR RoutInz;
016300170518
016400170518         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016500170518
016600170518       //?Imposto oggi
016700170518         Oggi = %dec(%date());
016800170518
016900170518       //?Reperimento dati job
017000170518         exsr DatiJob;
017100170518
017200170518       //?Imposto la DS dalla KPJBU
017300170518         FIOR89DS = kpjbu;
017400170518
017500170518       //?Se richieste tutte le filiali carico in sk le filiali abilitate all'utente
017600170518         IF  IOR89fil = 999;
017700170518           clear TRUL31DS;
017800170518           I31abi = wAbi;
017900170518           I31cdi = DUTdis;
018000170518           I31car = DUTare;
018100170518           I31cpo = DUTpou;
018200170518           TRUL31R (kpjba:trul31ds);
018300170518           IF  O31pog <= *zeros;
018400170518             exsr RoutEnd;
018500170518           ENDIF;
018600170518         ELSE;
018700170518           skPOG(1) = %editc(IOR89fil:'X');
018800170518         ENDIF;
018900170518
019000170518       //?Pulisco il file di work
019100170518         exec sql DELETE from GAITRAAZM/WFSRA00F;
019200170518
019300170518       ENDSR;
019400170518
019500170518       //--------------------------------------------------------------
019600170518       //?Reperimento Dati del job (Utente/Operativi).
019700170518       //--------------------------------------------------------------
019800170518       BEGSR DatiJob;
019900170518
020000170518         in(E) §AzUte;
020100170518         IF  NOT %error;
020200170518           in(E) §DatiUte;
020300170518         ENDIF;
020400170518         IF  %error or RSut = *blanks;
020500170518           clear TIBS34ds;
020600170518           tibs34r(tibs34ds);
020700170518           in §AzUte;
020800170518           in §DatiUte;
020900170518         ENDIF;
021000170518
021100170518         clear wAbi;
021200170518         clear dLAT;
021300170518
021400170518       //?Controllo autorizzazioni profilo
021500170518         SELECT;
021600170518         WHEN  DUTerr = 'E';
021700170518           exsr RoutEnd;
021800170518           leavesr;
021900170518         WHEN  UTEaut <> *blanks;
022000170518           wAbi = UTEaut;
022100170518         OTHER;
022200170518           IF  DUTlpo = 'S';
022300170518             wAbi = 'AZ';
022400170518           ENDIF;
022500170518           IF  DUTlpo = '1';
022600170518             wAbi = 'TP';
022700170518           ENDIF;
022800170518           IF  DUTlpo = '2';
022900170518             wAbi = 'PO';
023000170518           ENDIF;
023100170518         ENDSL;
023200170518
023300170518       //?Controllo se l'autorizzazione è valida
023400170518         clear dLAT;
023500170518         clear TIBS02DS;
023600170518         T02mod = 'C';
023700170518         T02sif = knsif;
023800170518         T02cod = 'LAT';
023900170518         T02ke1 = wAbi;
024000170518         TNTBE_RicercaControllo (kpjba:tibs02ds);
024100170518         dLAT = T02uni;
024200170518         IF  T02err <> *blanks or §LATabi = 'S';
024300170518           exsr RoutEnd;
024400170518         ENDIF;
024500170518
024600170518       ENDSR;
024700170518
024800170518       //--------------------------------------------------------------
024900170518       //?Elabora Statistica.
025000170518       //--------------------------------------------------------------
025100170518       BEGSR Elabora;
025200170519
025300170519         clear OG148;
025400170519         chain (wFil) AZORG01L;
025500170519         IF  %found(AZORG01L) and ORGfva = *blanks and
025600170519             (ORGfag = 'F' or ORGfag = 'A');
025700170519           OG148 = ORGde8;
025800170519         ENDIF;
025900170519       //?Non elaboro se filiale non partita con la procedura ORM
026000170519         IF  §OGorm <> 'S';
026100170519           leavesr;
026200170519         ENDIF;
026300170518
026400170518         clear WFSRA000;
026500170519         SRAdes = ORGdes;
026600170608
026700170608       //?Cerco che giorno della settimana è il giorno di immissione
026800170608         clear wDay_Of_Week;
026900170608         exec sql
027000170608         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
027100170608
027200170608       //?Recupero tabella GPD della filiale emissione
027300170608         Filiale = wFil;
027400170608         exsr Tabella_GPD;
027500170608         SRAgg = skGiorni(wDay_Of_Week);
027600170518
027700170518         EndSqlORM = *off;
027800170518
027900170518       //?Preparo la stringa SQL
028000170518         exsr StringaSql;
028100170518
028200170518       //?Leggo i dati
028300170518         exec sql PREPARE S1 from :wSQL;
028400170518         exec sql DECLARE WORM cursor for S1;
028500170518
028600170518       //?Apertura del cursore
028700170518         exec sql open WORM;
028800170518
028900170518         IF  sqlcode < 0;
029000170518           EndSqlORM = *on;
029100170518         ENDIF;
029200170518
029300170518         DOW  not EndSqlORM;
029400170518           exec sql
029500170608           fetch next from WORM into :FNORM00F, :FNORE00F;
029600170518           IF  sqlcod = 100 or sqlcod < 0;
029700170518             EndSqlORM = *on;
029800170518             leave;
029900170518           ENDIF;
030000170518
030100170518         //?Elaboro a dettaglio per ORM
030200170518           exsr DettaglioORM;
030300170518
030400170518         ENDDO;
030500170518
030600170518         //?Scrivo file di work
030700170518         IF  SRAtot > 0;
030800170518           exsr Scrivi;
030900170518         ENDIF;
031000170518
031100170518         exec sql close WORM;
031200170518
031300170518       ENDSR;
031400170518
031500170518       //--------------------------------------------------------------
031600170518       //?Preparazione della stringa SQL.
031700170518       //--------------------------------------------------------------
031800170518       BEGSR StringaSql;
031900170518
032000170518       //?Seleziono gli ORM
032100170518         wSQL =
032200170608         'SELECT * from FNORM00F join FNORE00F on -
032300170608                   ORMpoe = OREpoe and ORMnor = OREnor and -
032400170608                   ORMnsr = OREnsr and ORMnrv = OREnrv and -
032500170608                   OREtrc = ''DT'' -
032600170922                   WHERE ORMnsr = 0  and ORMtco in(''M'', ''E'', ''I'')' +
032700170518                 ' and ORMpoe = ' + %editc(wFil:'X') +
032800170518                 ' and ORMdao = ' + %editc(wData:'X') +
032900170518                 ' order by ORMpoe, ORMnsr, ORMnor, ORMnrv';
033000170518       ENDSR;
033100170518
033200170518       //--------------------------------------------------------------
033300170518       //?Elaboro a dettaglio ORM.
033400170518       //--------------------------------------------------------------
033500170518       BEGSR DettaglioORM;
033600170519
033700170519         clear OG143;
033800170519         clear OG148;
033900170519         chain (ORMpor) AZORG01L;
034000170519         IF  %found(AZORG01L) and ORGfva = *blanks and
034100170519             (ORGfag = 'F' or ORGfag = 'A');
034200170519           OG143 = ORGde3;
034300170519           OG148 = ORGde8;
034400170519         ENDIF;
034500170519       //?Non elaboro se filiale non partita con la procedura ORM
034600170519         IF  §OGorm <> 'S';
034700170519           leavesr;
034800170519         ENDIF;
034900170519       //?Non elaboro se network estero
035000170519         IF  §OGntw = 'DPD' or §OGntw = 'EEX' or §OGntw = 'FED';
035100170519           leavesr;
035200170519         ENDIF;
035300170518
035400170608         dOREdt = OREdati;
035500170518
035600170518       //?non considero i ritiri che hanno
035700170608       //?flag di posticipo a blank
035800170608         IF  §OREposd = *blanks;
035900170518           leavesr;
036000170518         ENDIF;
036100170609
036200170609       //?gg di anticipo in campo numerico
036300170609         clear wGiorni;
036400170609         IF  §OREgga <> *blanks and §OREgga <> *zeros;
036500170609           wGiorni = %int(§OREgga);
036600170609         ENDIF;
036700170518
036800170518       //?Conto i Ritiri
036900170518         exsr Conta;
037000170518
037100170518       ENDSR;
037200170608
037300170608       //--------------------------------------------------------------
037400170608       //?Cerco tabella GPD.
037500170608       //--------------------------------------------------------------
037600170608       BEGSR Tabella_GPD;
037700170608
037800170608       //?Aggancio la tabella GPD con filiale ritiro ORM
037900170608         clear TIBS02ds;
038000170608         clear dGPD;
038100170608         T02mod = 'C';
038200170608         T02cod = 'GPD';
038300170608         T02sif = KNSIF;
038400170608         T02ke1 = %editc(Filiale:'X');
038500170608         TNTBE_RicercaControllo (kpjba : tibs02ds);
038600170608         IF  T02err = *blanks;
038700170608           dGPD = T02uni;
038800170608           leavesr;
038900170608         ENDIF;
039000170608       //?Non trovo GPD con filiale ritiro
039100170608       //?la cerco con filiale generica 999
039200170608         clear TIBS02ds;
039300170608         T02mod = 'C';
039400170608         T02cod = 'GPD';
039500170608         T02sif = KNSIF;
039600170608         T02ke1 = '999';
039700170608         TNTBE_RicercaControllo (kpjba : tibs02ds);
039800170608         IF  T02err = *blanks;
039900170608           dGPD = T02uni;
040000170608         ENDIF;
040100170608
040200170608       ENDSR;
040300170518
040400170518       //--------------------------------------------------------------
040500170518       //?Conto i ritiri.
040600170518       //--------------------------------------------------------------
040700170518       BEGSR Conta;
040800170518
040900170518       //?Conto totale ORM da posticipare
041000170518         SRAtot += 1;
041100170608
041200170609       //?Conto quanti ne ho pianificati nella data calcolata
041300170609         IF  wGiorni = *zeros;
041400170608           SRAdrok += 1;
041500170609           leavesr;
041600170608         ENDIF;
041700170609
041800170609       //?Se data ritiro >= della data calcolata ma ho i gg di anticipo vuol dire
041900170609       //?che hanno variato la data ritiro quindi mi baso sui flag e sul numero
042000170609       //?di giorni anticipati e i gg di posticipo
042100170609         IF  wGiorni > *zeros and %editc(ORMdar:'X') >= §OREdar;
042200170609         //?Previsti più di 1 gg di posticipo ed ho anticipato di 1 gg
042300170609         //?l'ho pianificato per domani
042400170609           IF  SRAgg > 1 and wGiorni = 1;
042500170609             SRAdrdi1 += 1;
042600170609           ENDIF;
042700170609         //?Previsti più di 1 gg di posticipo ed ho anticipato di più 1 gg
042800170609         //?l'ho pianificato per oggi
042900170609           IF  SRAgg > 1 and wGiorni > 1;
043000170609             SRAdrdi += 1;
043100170609           ENDIF;
043200170609         //?Previsti 1 gg di posticipo ed ho anticipato di 1 gg
043300170609         //?l'ho pianificato per oggi
043400170609           IF  SRAgg = 1 and wGiorni = 1;
043500170609             SRAdrdi += 1;
043600170609           ENDIF;
043700170609           leavesr;
043800170609         ENDIF;
043900170518
044000170608       //?Se potevo anticipare verifico di quanti gg ho anticipato
044100170608         IF  §OREant <> *blanks and §OREmod <> *blanks;
044200170609         //?Quanti ne ho pianificati oggi  (data immissione = data ritiro)
044300170608           IF  ORMdao = ORMdar;
044400170608             SRAdrdi += 1;
044500170608             leavesr;
044600170608           ENDIF;
044700170609         //?Conto quanti ne ho pianificati domani (ho anticipato ma non l'ho fatto oggi)
044800170608           SRAdrdi1 += 1;
044900170518         ENDIF;
045000170518
045100170518       ENDSR;
045200170518
045300170518       //--------------------------------------------------------------
045400170518       //?Scrivo il file di work.
045500170518       //--------------------------------------------------------------
045600170518       BEGSR Scrivi;
045700170518
045800170518         SRAfil = wFil;
045900170518         SRAdata = wData;
046000170518
046100170518         write WFSRA000;
046200170518
046300170518       ENDSR;
046400170518
046500170518       //--------------------------------------------------------------
046600170518       //?Operazioni finali.
046700170518       //--------------------------------------------------------------
046800170518       BEGSR RoutEnd;
046900170518
047000170518         *inLR = *on;
047100170518         return;
047200170518
047300170518       ENDSR;
