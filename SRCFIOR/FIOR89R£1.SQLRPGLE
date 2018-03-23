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
001400170518      // - Calendario
001500170518     fAZCLN01L  if   e           k disk
001600170518
001700170518      // - ORM
001800170518     fFNORE01L  if   e           k disk
001900170518
002000170518      // - Tabelle
002100170518     fTABEL00F  if   e           k disk
002200170518     fTNTBE01L  if   e           k disk
002300170518
002400170518      // - File di Work
002500170518     fWFSRA00F  o    e             disk
002600170518
002700170518      //---------------------------------------------------------------
002800170518      //?Definizione costanti.
002900170518      //---------------------------------------------------------------
003000170518
003100170518      //---------------------------------------------------------------
003200170518      //?Definizione schiere.
003300170518      //---------------------------------------------------------------
003400170518      // - Sk Date ritiro
003500170518     d skDate_Ritiro   s              8s 0 dim(999) inz
003600170518
003700170518      //---------------------------------------------------------------
003800170518      //?Definizione aree dati.
003900170518      //---------------------------------------------------------------
004000170518      // - Dati utente
004100170518     d §AzUte        e ds                  extname(AZUTE00F)
004200170518     d                                     dtaara
004300170518     d §DatiUte      e ds                  extname(dDatiUte)
004400170518     d                                     dtaara
004500170518
004600170518      //---------------------------------------------------------------
004700170518      //?Definizione strutture dati.
004800170518      //---------------------------------------------------------------
004900170518      // - Parametri ricevuti
005000170518     d KPJBA         e ds
005100170518     d FIOR89DS      e ds                  inz
005200170518
005300170518      // - Reperimento dati in tabella
005400170518     d TIBS02DS      e ds
005500170518
005600170518      // - Reperimento dati utente
005700170518     d TIBS34DS      e ds
005800170518
005900170518      // - Reperimento filiali gestite dall'utente
006000170518     d TRUL31DS      e ds
006100170518     d  skPOG                 10    759    dim(250)
006200170518
006300170518      // - Tabella DFT - Default ORM
006400170518     d dDFT          e ds
006500170518
006600170518      // - Tabella GPD - Giorni Posticipo Data Ritiro
006700170518     d dGPD          e ds
006800170518     d  skGiorni               1     14s 0 dim(7)
006900170518     d  skGiorniA             15     28s 0 dim(7)
007000170518
007100170518      // - Tabella LAT - Autorizzazioni
007200170518     d dLAT          e ds
007300170518
007400170518      // - Tabella 1Y - Simboli calendario BRT
007500170518     d ds1Y          e ds
007600170518
007700170518      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
007800170518     d dOREdt        e ds
007900170518
008000170518      // - OG143 - lettura campo ORGde3 file AZORG00F
008100170518     d OG143         e ds
008200170518
008300170518      // - OG148 - lettura campo ORGde8 file AZORG00F
008400170518     d OG148         e ds
008500170518
008600170518      // File ORM
008700170518     d FNORM00F      e ds                  extname(FNORM00F)
008800170518     d FNORG00F      e ds                  extname(FNORG00F)
008900170518
009000170518      // - Mattino/Pomeriggio calendario
009100170518     d CLNmat          DS
009200170518     d  mat                    1     31    dim(31)
009300170518     d CLNpom          DS
009400170518     d  pom                    1     31    dim(31)
009500170518
009600170518      //---------------------------------------------------------------
009700170518      //?Definizione variabili globali.
009800170518      //---------------------------------------------------------------
009900170518      // - Flags booleani
010000170518     d EndFesta        s               n   inz(*off)
010100170518     d EndSqlORM       s               n   inz(*off)
010200170518     d Fine            s               n   inz(*off)
010300170518     d IsFesta         s               n   inz(*off)
010400170518
010500170518       // - Stringa SQL da eseguire
010600170518     d wSQL            s           2048    Varying        inz
010700170518
010800170518      // - Indici di schiera
010900170518     d indicex         s              4s 0 inz
011000170518     d xx              s              4s 0 inz
011100170518
011200170518      // - Campi di comodo data
011300170518     d Data_ISO        s               d   datfmt(*iso)
011400170518     d Data_ISO_Fine   s               d   datfmt(*iso)
011500170518     d Data_ISO_Rit    s               d   datfmt(*iso)
011600170518     d wData_ISO       s               d   datfmt(*iso)
011700170518
011800170518      // - Campi di comodo
011900170519     d Filiale         s              3s 0 inz
012000170518     d Oggi            s              8s 0 inz
012100170518     d wAbi            s              2a   inz
012200170518     d wData           s              8s 0 inz
012300170518     d wDataFestivo    s              8s 0 inz
012400170518     d wDay_Of_Week    s              2  0 inz
012500170518     d wFestaMat       s              1a   inz
012600170518     d wFestaPom       s              1a   inz
012700170518     d wFil            s              3s 0 inz
012800170518     d wGiorno         s              2s 0 inz
012900170518     d wMatPom         s              1a   inz
013000170518
013100170518      //---------------------------------------------------------------
013200170518      //?Definizione procedure esterne.
013300170518      //---------------------------------------------------------------
013400170518      // - Caricamento Filiali in gestione
013500170518     d TRUL31R         pr                  extpgm('TRUL31R')
013600170518     d  kpjba                              likeds(kpjba)
013700170518     d  trul31ds                           likeds(trul31ds)
013800170518
013900170518      //---------------------------------------------------------------
014000170518      //?Definizione prototipi.
014100170518      //---------------------------------------------------------------
014200170518      /copy gaitrasrc/srcprotopr,TIBS02R
014300170518      /copy gaitrasrc/srcprotopr,TIBS34R
014400170518
014500170518      //---------------------------------------------------------------
014600170518      //?Definizione key-list.
014700170518      //---------------------------------------------------------------
014800170518      // - File AZCLN01L
014900170518     d kazcln01      e ds                  extname(AZCLN01L:*key)
015000170518     d                                     prefix(k_)
015100170518     d                                     inz
015200170518
015300170518      // - File TABEL00F
015400170518     d k03tabel      e ds                  extname(TABEL00F:*key)
015500170518     d                                     prefix(k_)
015600170518
015700170518      //---------------------------------------------------------------
015800170518      //?M A I N - L I N E
015900170518      //---------------------------------------------------------------
016000170518     c     *Entry        plist
016100170518     c                   parm                    kpjba
016200170518
016300170518       //?Operazioni iniziali
016400170518       exsr RoutInz;
016500170518
016600170518       //?Elaborazione Statistica per filiale e data
016700170518       //?Leggo per filiale
016800170518       xx = 1;
016900170518       FOR  xx by 1 to %elem(skPOG);
017000170518         IF  skPOG(xx) > *zeros;
017100170518         //?Leggo per data
017200170518           Data_ISO  = (%date(IOR89datai));
017300170518           Data_ISO_Fine = (%date(IOR89dataf));
017400170518           DOW  Data_ISO <= Data_ISO_Fine;
017500170518             wFil = %dec(skPOG(xx):3:0);
017600170518             wData = %dec(Data_ISO);
017700170518             exsr Elabora;
017800170518             Data_ISO += %days(1);
017900170518           ENDDO;
018000170518         ENDIF;
018100170518       ENDFOR;
018200170518
018300170518       //?Operazioni finali
018400170518       exsr RoutEnd;
018500170518
018600170518       //--------------------------------------------------------------
018700170518       //?Operazioni iniziali.
018800170518       //--------------------------------------------------------------
018900170518       BEGSR RoutInz;
019000170518
019100170518         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
019200170518
019300170518       //?Imposto oggi
019400170518         Oggi = %dec(%date());
019500170518
019600170518       //?Reperimento dati job
019700170518         exsr DatiJob;
019800170518
019900170518       //?Imposto la DS dalla KPJBU
020000170518         FIOR89DS = kpjbu;
020100170518
020200170518       //?Se richieste tutte le filiali carico in sk le filiali abilitate all'utente
020300170518         IF  IOR89fil = 999;
020400170518           clear TRUL31DS;
020500170518           I31abi = wAbi;
020600170518           I31cdi = DUTdis;
020700170518           I31car = DUTare;
020800170518           I31cpo = DUTpou;
020900170518           TRUL31R (kpjba:trul31ds);
021000170518           IF  O31pog <= *zeros;
021100170518             exsr RoutEnd;
021200170518           ENDIF;
021300170518         ELSE;
021400170518           skPOG(1) = %editc(IOR89fil:'X');
021500170518         ENDIF;
021600170518
021700170518       //?Pulisco il file di work
021800170518         exec sql DELETE from GAITRAAZM/WFSRA00F;
021900170518
022000170518       //?Cerco il peso massimo da elaborare recuperandolo dalla tabella DFT
022100170518       //?prima con la filiale utente o filiale da elaborare se <> da 999
022200170518         clear wFil;
022300170518         IF  IOR89fil <> 999;
022400170518           wFil = IOR89fil;
022500170518         ELSE;
022600170518           wFil = DUTpou;
022700170518         ENDIF;
022800170518         clear TIBS02DS;
022900170518         clear dDFT;
023000170518         T02mod = 'C';
023100170518         T02sif = knsif;
023200170518         T02cod = 'DFT';
023300170518         T02ke1 = %editc(wFil:'X');
023400170518         TNTBE_RicercaControllo (kpjba:tibs02ds);
023500170518         IF  T02err = *blanks;
023600170518           dDFT = T02uni;
023700170518           leavesr;
023800170518         ENDIF;
023900170518       //?se non trovo con la filiale specifica provo con 999 generico
024000170518         clear TIBS02DS;
024100170518         clear dDFT;
024200170518         T02mod = 'C';
024300170518         T02sif = knsif;
024400170518         T02cod = 'DFT';
024500170518         T02ke1 = '999';
024600170518         TNTBE_RicercaControllo (kpjba:tibs02ds);
024700170518         IF  T02err = *blanks;
024800170518           dDFT = T02uni;
024900170518         ENDIF;
025000170518
025100170518       ENDSR;
025200170518
025300170518       //--------------------------------------------------------------
025400170518       //?Reperimento Dati del job (Utente/Operativi).
025500170518       //--------------------------------------------------------------
025600170518       BEGSR DatiJob;
025700170518
025800170518         in(E) §AzUte;
025900170518         IF  NOT %error;
026000170518           in(E) §DatiUte;
026100170518         ENDIF;
026200170518         IF  %error or RSut = *blanks;
026300170518           clear TIBS34ds;
026400170518           tibs34r(tibs34ds);
026500170518           in §AzUte;
026600170518           in §DatiUte;
026700170518         ENDIF;
026800170518
026900170518         clear wAbi;
027000170518         clear dLAT;
027100170518
027200170518       //?Controllo autorizzazioni profilo
027300170518         SELECT;
027400170518         WHEN  DUTerr = 'E';
027500170518           exsr RoutEnd;
027600170518           leavesr;
027700170518         WHEN  UTEaut <> *blanks;
027800170518           wAbi = UTEaut;
027900170518         OTHER;
028000170518           IF  DUTlpo = 'S';
028100170518             wAbi = 'AZ';
028200170518           ENDIF;
028300170518           IF  DUTlpo = '1';
028400170518             wAbi = 'TP';
028500170518           ENDIF;
028600170518           IF  DUTlpo = '2';
028700170518             wAbi = 'PO';
028800170518           ENDIF;
028900170518         ENDSL;
029000170518
029100170518       //?Controllo se l'autorizzazione è valida
029200170518         clear dLAT;
029300170518         clear TIBS02DS;
029400170518         T02mod = 'C';
029500170518         T02sif = knsif;
029600170518         T02cod = 'LAT';
029700170518         T02ke1 = wAbi;
029800170518         TNTBE_RicercaControllo (kpjba:tibs02ds);
029900170518         dLAT = T02uni;
030000170518         IF  T02err <> *blanks or §LATabi = 'S';
030100170518           exsr RoutEnd;
030200170518         ENDIF;
030300170518
030400170518       ENDSR;
030500170518
030600170518       //--------------------------------------------------------------
030700170518       //?Elabora Statistica.
030800170518       //--------------------------------------------------------------
030900170518       BEGSR Elabora;
031000170519
031100170519         clear OG148;
031200170519         chain (wFil) AZORG01L;
031300170519         IF  %found(AZORG01L) and ORGfva = *blanks and
031400170519             (ORGfag = 'F' or ORGfag = 'A');
031500170519           OG148 = ORGde8;
031600170519         ENDIF;
031700170519       //?Non elaboro se filiale non partita con la procedura ORM
031800170519         IF  §OGorm <> 'S';
031900170519           leavesr;
032000170519         ENDIF;
032100170518
032200170518         clear WFSRA000;
032300170519         SRAdes = ORGdes;
032400170518
032500170518       //?Cerco che giorno della settimana è il giorno di immissione
032600170518         clear wDay_Of_Week;
032700170518         exec sql
032800170518         set :wDay_Of_Week = dayofweek_iso(:Data_ISO);
032900170519
033000170519       //?Recupero tabella GPD della filiale emissione
033100170519         Filiale = wFil;
033200170519         exsr Tabella_GPD;
033300170519         SRAgg = skGiorni(wDay_Of_Week);
033400170518
033500170518         EndSqlORM = *off;
033600170518
033700170518       //?Preparo la stringa SQL
033800170518         exsr StringaSql;
033900170518
034000170518       //?Leggo i dati
034100170518         exec sql PREPARE S1 from :wSQL;
034200170518         exec sql DECLARE WORM cursor for S1;
034300170518
034400170518       //?Apertura del cursore
034500170518         exec sql open WORM;
034600170518
034700170518         IF  sqlcode < 0;
034800170518           EndSqlORM = *on;
034900170518         ENDIF;
035000170518
035100170518         DOW  not EndSqlORM;
035200170518           exec sql
035300170518           fetch next from WORM into :FNORM00F, :FNORG00F;
035400170518           IF  sqlcod = 100 or sqlcod < 0;
035500170518             EndSqlORM = *on;
035600170518             leave;
035700170518           ENDIF;
035800170518
035900170518         //?Elaboro a dettaglio per ORM
036000170518           exsr DettaglioORM;
036100170518
036200170518         ENDDO;
036300170518
036400170518         //?Scrivo file di work
036500170518         IF  SRAtot > 0;
036600170518           exsr Scrivi;
036700170518         ENDIF;
036800170518
036900170518         exec sql close WORM;
037000170518
037100170518       ENDSR;
037200170518
037300170518       //--------------------------------------------------------------
037400170518       //?Preparazione della stringa SQL.
037500170518       //--------------------------------------------------------------
037600170518       BEGSR StringaSql;
037700170518
037800170518       //?Seleziono gli ORM
037900170518         wSQL =
038000170518         'SELECT * from FNORM00F join FNORG00F on -
038100170518                   ORMpoe = ORGpoe and ORMnsr = ORGnsr and -
038200170518                   ORMnor = ORGnor and ORMnrv = ORGnrv -
038300170518                   WHERE ORMnsr = 0  and ORMtco in(''M'', ''E'')' +
038400170518                 ' and ORMpoe = ' + %editc(wFil:'X') +
038500170518                 ' and ORMdao = ' + %editc(wData:'X') +
038600170518                 ' and ORGpkg < ' + %editc(d§DFTpkgdt:'4') +
038700170529                 ' and (ORMcor = 0 or ORMcor = 0017732000)' +
038800170529                 ' and ORMcra = 0' +
038900170518                 ' order by ORMpoe, ORMnsr, ORMnor, ORMnrv';
039000170518       ENDSR;
039100170518
039200170518       //--------------------------------------------------------------
039300170518       //?Elaboro a dettaglio ORM.
039400170518       //--------------------------------------------------------------
039500170518       BEGSR DettaglioORM;
039600170519
039700170519         clear OG143;
039800170519         clear OG148;
039900170519         chain (ORMpor) AZORG01L;
040000170519         IF  %found(AZORG01L) and ORGfva = *blanks and
040100170519             (ORGfag = 'F' or ORGfag = 'A');
040200170519           OG143 = ORGde3;
040300170519           OG148 = ORGde8;
040400170519         ENDIF;
040500170519       //?Non elaboro se filiale non partita con la procedura ORM
040600170519         IF  §OGorm <> 'S';
040700170519           leavesr;
040800170519         ENDIF;
040900170519       //?Non elaboro se network estero
041000170519         IF  §OGntw = 'DPD' or §OGntw = 'EEX' or §OGntw = 'FED';
041100170519           leavesr;
041200170519         ENDIF;
041300170518
041400170518       //?cerco la data pronta merce
041500170518         clear dOREdt;
041600170518         OREtrc = 'DT';
041700170518         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
041800170518         IF  %found(FNORE01L);
041900170518           dOREdt = OREdati;
042000170518         ELSE;
042100170518           §OREdpm = ORMdar;
042200170518         ENDIF;
042300170518
042400170518       //?non considero i ritiri che hanno
042500170518       //?data pronta merce > data immissione
042600170518       //?in questo caso NON si posticipa
042700170518         IF  §OREdpm > wData;
042800170518           leavesr;
042900170518         ENDIF;
043000170518
043100170518       //?Recupero tabella GPD della filiale ritiro
043200170519         Filiale = ORMpor;
043300170518         exsr Tabella_GPD;
043400170518
043500170519       //?Se devo posticipare
043600170518       //?Cerco le possibili date di ritiro per l'ORM
043700170519         IF  skGiorni(wDay_Of_Week) > 0;
043800170519           exsr Date_Ritiro_Possibili;
043900170519       //?Se NON devo posticipare
044000170519       //?Vado via
044100170519         ELSE;
044200170519           leavesr;
044300170519         ENDIF;
044400170518
044500170518       //?Conto i Ritiri
044600170518         exsr Conta;
044700170518
044800170518       ENDSR;
044900170518
045000170518       //--------------------------------------------------------------
045100170518       //?Cerco tabella GPD.
045200170518       //--------------------------------------------------------------
045300170518       BEGSR Tabella_GPD;
045400170518
045500170518       //?Aggancio la tabella GPD con filiale ritiro ORM
045600170518         clear TIBS02ds;
045700170518         clear dGPD;
045800170518         T02mod = 'C';
045900170518         T02cod = 'GPD';
046000170518         T02sif = KNSIF;
046100170519         T02ke1 = %editc(Filiale:'X');
046200170518         TNTBE_RicercaControllo (kpjba : tibs02ds);
046300170518         IF  T02err = *blanks;
046400170518           dGPD = T02uni;
046500170518           leavesr;
046600170518         ENDIF;
046700170518       //?Non trovo GPD con filiale ritiro
046800170518       //?la cerco con filiale generica 999
046900170518         clear TIBS02ds;
047000170518         T02mod = 'C';
047100170518         T02cod = 'GPD';
047200170518         T02sif = KNSIF;
047300170518         T02ke1 = '999';
047400170518         TNTBE_RicercaControllo (kpjba : tibs02ds);
047500170518         IF  T02err = *blanks;
047600170518           dGPD = T02uni;
047700170518         ENDIF;
047800170518
047900170518       ENDSR;
048000170518
048100170518       //--------------------------------------------------------------
048200170518       //?Cerco le possibili date ritiro.
048300170518       //--------------------------------------------------------------
048400170518       BEGSR Date_Ritiro_Possibili;
048500170518
048600170518         clear skDate_Ritiro;
048700170518         indicex = 1;
048800170518         Data_ISO_Rit = Data_ISO;
048900170519         FOR  indicex by 1 to skGiorni(wDay_Of_Week);
049000170519           Data_ISO_Rit += %days(1);
049100170519         //?Se data di ritiro festivo vado al primo giorno non festivo
049200170519           wData_ISO = Data_ISO_Rit;
049300170519           exsr Festa;
049400170519           IF  IsFesta;
049500170519             Data_ISO_Rit += %days(1);
049600170519             iter;
049700170519           ENDIF;
049800170519           skDate_Ritiro(indicex) = %dec(Data_ISO_Rit);
049900170519         ENDFOR;
050000170518
050100170518       ENDSR;
050200170518
050300170518       //--------------------------------------------------------------
050400170518       //?Conto i ritiri.
050500170518       //--------------------------------------------------------------
050600170518       BEGSR Conta;
050700170518
050800170518       //?Conto totale ORM da posticipare
050900170518         SRAtot += 1;
051000170518
051100170518       //?Conto quanti ne ho fatti oggi  (data immissione = data ritiro)
051200170518         IF  ORMdao = ORMdar;
051300170518           SRAdrdi += 1;
051400170518         ENDIF;
051500170518
051600170518       //?Se da tabella devo posticipae di più di 1 gg
051700170518         IF  skGiorni(wDay_Of_Week) > 1;
051800170518         //?Conto quanti ne ho fatti domani (data immissione + 1 giorno)
051900170518           IF  ORMdar = skDate_Ritiro(01);
052000170518             SRAdrdi1 += 1;
052100170518           ENDIF;
052200170518         //?Conto quanti non ho anticipato
052300170518           IF  ORMdar >= skDate_Ritiro(02);
052400170518             SRAdrok += 1;
052500170518           ENDIF;
052600170518         ENDIF;
052700170518
052800170518       //?Se da tabella devo posticipare di 1 gg
052900170518         IF  skGiorni(wDay_Of_Week) = 1;
053000170518         //?Conto quanti non ho anticipato
053100170518           IF  ORMdar >= skDate_Ritiro(01);
053200170518             SRAdrok += 1;
053300170518           ENDIF;
053400170518         ENDIF;
053500170518
053600170518       ENDSR;
053700170518
053800170518       //--------------------------------------------------------------
053900170518       //?Scrivo il file di work.
054000170518       //--------------------------------------------------------------
054100170518       BEGSR Scrivi;
054200170518
054300170518         SRAfil = wFil;
054400170518         SRAdata = wData;
054500170518
054600170518         write WFSRA000;
054700170518
054800170518       ENDSR;
054900170518
055000170518       //--------------------------------------------------------------
055100170518       //?Controlla se giorno festivo.
055200170518       //--------------------------------------------------------------
055300170518       BEGSR Festa;
055400170518
055500170518         IsFesta = *off;
055600170518         clear k_CLNtfp;
055700170518         k_CLNtfa = ORMpor;
055800170518         clear wFestaMat;
055900170518         clear wFestaPom;
056000170518         k_CLNann = %subdt(Data_ISO:*years);
056100170518         k_CLNmes = %subdt(Data_ISO:*months);
056200170518         wGiorno  = %subdt(Data_ISO:*days);
056300170518         chain %kds(kazcln01) AZCLN01L;
056400170518         IF  not %found(AZCLN01L);
056500170518           leavesr;
056600170518         ENDIF;
056700170518         IF  mat(wGiorno) <> *blanks;
056800170518           wMatPom = mat(wGiorno);
056900170518           exsr Tab1Y;
057000170518           wFestaMat = §1Yfei;
057100170518         ENDIF;
057200170518         IF  pom(wGiorno) <> *blanks;
057300170518           wMatPom = pom(wGiorno);
057400170518           exsr Tab1Y;
057500170518           wFestaPom = §1Yfei;
057600170518         ENDIF;
057700170518         IF  wFestaMat = 'S' or wFestaPom = 'S';
057800170518           IsFesta = *on;
057900170518         ENDIF;
058000170518
058100170518       ENDSR;
058200170518
058300170518       //--------------------------------------------------------------
058400170518       //?Tabella 1Y - Simboli Calendario BRT.
058500170518       //--------------------------------------------------------------
058600170518       BEGSR Tab1Y;
058700170518
058800170518         k_TBLkut = 1;
058900170518         k_TBLcod = '1Y';
059000170518         clear ds1Y;
059100170518         k_TBLkey = wMatPom;
059200170518         chain  %kds(K03tabel) TABEL00F;
059300170518         IF  %found(TABEL00F);
059400170518           ds1Y = TBLuni;
059500170518         ENDIF;
059600170518
059700170518       ENDSR;
059800170518
059900170518       //--------------------------------------------------------------
060000170518       //?Operazioni finali.
060100170518       //--------------------------------------------------------------
060200170518       BEGSR RoutEnd;
060300170518
060400170518         *inLR = *on;
060500170518         return;
060600170518
060700170518       ENDSR;
