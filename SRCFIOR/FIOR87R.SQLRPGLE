000100170512      //---------------------------------------------------------------
000200170512      //?FIOR87R - Statistica Ritiri con peso < 5 Kg.
000300170512      //---------------------------------------------------------------
000400170512     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500170512     h dftactgrp(*no) actgrp(*caller)
000600170512     h bnddir('UBRTVNETA')
000700170512
000800170512      //---------------------------------------------------------------
000900170512      //?Dichiarazione file.
001000170512      //---------------------------------------------------------------
001100170512      // - Organigramma
001200170512     fAZORG01L  if   e           k disk
001300170512
001400170512      // - Calendario
001500170512     fAZCLN01L  if   e           k disk
001600170512
001700170512      // - Distinte
001800170512     fFIDST01L  if   e           k disk    usropn extfile(wFLib)
001900170512
002000170512      // - Ritiri
002100170512     fFNORE01L  if   e           k disk
002200170512     fFNORF01L  if   e           k disk
002300170512     fFNORF07L  if   e           k disk    rename(FNORF000:FNORF07)
002400170512
002500170512      // - Tabelle
002600170512     fTABEL00F  if   e           k disk
002700170512     fTNTBE01L  if   e           k disk
002800170512
002900170512      // - File di Work
003000170512     fWFSRP00F  o    e             disk
003100170512
003200170512      //---------------------------------------------------------------
003300170512      //?Definizione costanti.
003400170512      //---------------------------------------------------------------
003500170512
003600170512      //---------------------------------------------------------------
003700170512      //?Definizione schiere.
003800170512      //---------------------------------------------------------------
003900170512      // - Fasi da NON elaborare in Statistica
004000170512     d skFAR           s              3s 0 dim(999)
004100170512
004200170512      //---------------------------------------------------------------
004300170512      //?Definizione aree dati.
004400170512      //---------------------------------------------------------------
004500170512      // - Dati utente
004600170512     d §AzUte        e ds                  extname(AZUTE00F)
004700170512     d                                     dtaara
004800170512     d §DatiUte      e ds                  extname(dDatiUte)
004900170512     d                                     dtaara
005000170512
005100170512      //---------------------------------------------------------------
005200170512      //?Definizione strutture dati.
005300170512      //---------------------------------------------------------------
005400170512      // - Parametri ricevuti
005500170512     d KPJBA         e ds
005600170512     d FIOR87DS      e ds                  inz
005700170512
005800170512      // - Reperimento dati in tabella
005900170512     d TIBS02DS      e ds
006000170512
006100170512      // - Reperimento dati utente
006200170512     d TIBS34DS      e ds
006300170512
006400170512      // - Reperimento filiali gestite dall'utente
006500170512     d TRUL31DS      e ds
006600170512     d  skPOG                 10    759    dim(250)
006700170512
006800170512      // - Tabella DFT - Default ORM
006900170512     d dDFT          e ds
007000170512
007100170512      // - Tabella FAR - Fasi ORM
007200170512     d dFAR          e ds
007300170512
007400170512      // - Tabella LAT - Autorizzazioni
007500170512     d dLAT          e ds
007600170512
007700170512      // - Tabella 1Y - Simboli calendario BRT
007800170512     d ds1Y          e ds
007900170512
008000170512      // - dOREdt - lettura campo OREdati per rcd 'DT' file FNORE00F
008100170512     d dOREdt        e ds
008200170512
008300170512      // - dORF01 - lettura campo ORFflo file FNORF00F
008400170512     d dORF01        e ds
008500170512
008600170512      // - dDSTflr - lettura campo DSTflr file FIDST00F
008700170512     d dDSTflr       e ds
008800170512
008900170512      // - OG143 - lettura campo ORGde3 file AZORG00F
009000170512     d OG143         e ds
009100170516
009200170516      // - OG148 - lettura campo ORGde8 file AZORG00F
009300170516     d OG148         e ds
009400170512
009500170512      // File ORM
009600170512     d FNORM00F      e ds                  extname(FNORM00F)
009700170512     d FNORG00F      e ds                  extname(FNORG00F)
009800170512     d FNORE00F      e ds                  extname(FNORE00F)
009900170512
010000170512      // File LOG PDA
010100170512     d FIPLG00F      e ds                  extname(FIPLG00F)
010200170512
010300170512      // - FIPRCOKDS - lettura campo PLGdati rcd 'OK ' file FIDST00F
010400170512     d FIPRCOKDS     e ds
010500170512
010600170512      // - FIPRCRESDS - lettura campo PLGdati rcd 'RES' file FIDST00F
010700170512     d FIPRCRESDS    e ds
010800170512
010900170512      // - Mattino/Pomeriggio calendario
011000170512     d CLNmat          DS
011100170512     d  mat                    1     31    dim(31)
011200170512     d CLNpom          DS
011300170512     d  pom                    1     31    dim(31)
011400170512
011500170512      //---------------------------------------------------------------
011600170512      //?Definizione variabili globali.
011700170512      //---------------------------------------------------------------
011800170512      // - Flags booleani
011900170512     d EndFesta        s               n   inz(*off)
012000170512     d EndSqlORM       s               n   inz(*off)
012100170512     d EndSqlPLG       s               n   inz(*off)
012200170512     d Fine            s               n   inz(*off)
012300170515     d IsFesta         s               n   inz(*off)
012400170512     d wOkAssegnabile  s               n   inz(*off)
012500170512     d wOkAssegnato    s               n   inz(*off)
012600170512
012700170512       // - Stringa SQL da eseguire
012800170512     d wSQL            s           2048    Varying        inz
012900170512
013000170512      // - Indici di schiera
013100170512     d xx              s              4s 0 inz
013200170512
013300170512      // - Campi di comodo data
013400170512     d Data_ISO        s               d   datfmt(*iso)
013500170512     d Data_ISOf       s               d   datfmt(*iso)
013600170512     d wData_ISO       s               d   datfmt(*iso)
013700170512
013800170512      // - Campi di comodo
013900170512     d Oggi            s              8s 0 inz
014000170512     d wAbi            s              2a   inz
014100170512     d wData           s              8s 0 inz
014200170515     d wDataFase       s              8s 0 inz
014300170512     d wDataFestivo    s              8s 0 inz
014400170512     d wFestaMat       s              1a   inz
014500170512     d wFestaPom       s              1a   inz
014600170512     d wFil            s              3s 0 inz
014700170512     d wFlib           s             21a   inz
014800170512     d wLib            s             10a   inz
014900170512     d wGiorno         s              2s 0 inz
015000170512     d wMatPom         s              1a   inz
015100170512     d wOLPora         s              4a   inz
015200170512
015300170512      // - Nome del sistema?
015400170512     d currSysNeta     s              8a   inz
015500170512
015600170512      //---------------------------------------------------------------
015700170512      //?Definizione procedure esterne.
015800170512      //---------------------------------------------------------------
015900170512       // - Caricamento Filiali in gestione
016000170512     d TRUL31R         pr                  extpgm('TRUL31R')
016100170512     d  kpjba                              likeds(kpjba)
016200170512     d  trul31ds                           likeds(trul31ds)
016300170512
016400170512      //---------------------------------------------------------------
016500170512      //?Definizione prototipi.
016600170512      //---------------------------------------------------------------
016700170512      /copy gaitrasrc/srcprotopr,TIBS02R
016800170512      /copy gaitrasrc/srcprotopr,TIBS34R
016900170512
017000170512      // - Reperimento NETA sistema AS/400 corrente?
017100170512      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017200170512
017300170512      //---------------------------------------------------------------
017400170512      //?Definizione key-list.
017500170512      //---------------------------------------------------------------
017600170512      // - File AZCLN01L
017700170512     d kazcln01      e ds                  extname(AZCLN01L:*key)
017800170512     d                                     prefix(k_)
017900170512     d                                     inz
018000170512      // - File FIDST01L
018100170512     d k03fidst      e ds                  extname(FIDST01L:*key)
018200170512     d                                     prefix(k_)
018300170512     d                                     inz
018400170512
018500170512      // - File TABEL00F
018600170512     d k03tabel      e ds                  extname(TABEL00F:*key)
018700170512     d                                     prefix(k_)
018800170512
018900170512      //---------------------------------------------------------------
019000170512      //?M A I N - L I N E
019100170512      //---------------------------------------------------------------
019200170512     c     *Entry        plist
019300170512     c                   parm                    kpjba
019400170512
019500170512       //?Operazioni iniziali
019600170512       exsr RoutInz;
019700170512
019800170519       //?Elaborazione Statistica per filiale e data
019900170519       //?Leggo per filiale
020000170519       xx = 1;
020100170519       FOR  xx by 1 to %elem(skPOG);
020200170519         IF  skPOG(xx) > *zeros;
020300170519           wFil = %dec(skPOG(xx):3:0);
020400170519         //?Leggo per data
020500170519           DOW  Data_ISO <= Data_ISOf;
020600170519           //?Se festivo non elaboro
020700170519             exsr Festa;
020800170519             IF  IsFesta;
020900170519               Data_ISO += %days(1);
021000170519               iter;
021100170519             ENDIF;
021200170519             wData = %dec(Data_ISO);
021300170519             exsr Elabora;
021400170519             Data_ISO += %days(1);
021500170519           ENDDO;
021600170519         ENDIF;
021700170519       //?Reimposto la data partenza per la filiale sucessiva
021800170519         Data_ISO = (%date(IOR87datai));
021900170519       ENDFOR;
022000170512
022100170512       //?Operazioni finali
022200170512       exsr RoutEnd;
022300170512
022400170512       //--------------------------------------------------------------
022500170512       //?Operazioni iniziali.
022600170512       //--------------------------------------------------------------
022700170512       BEGSR RoutInz;
022800170512
022900170512         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
023000170512
023100170512       //?Imposto oggi
023200170512         Oggi = %dec(%date());
023300170512
023400170512       //?Reperimento dati job
023500170512         exsr DatiJob;
023600170512
023700170512       //?Imposto la DS dalla KPJBU
023800170512         FIOR87DS = kpjbu;
023900170512
024000170512       //?Cerco il peso massimo da elaborare recuperandolo dalla tabella DFT
024100170512       //?prima con la filiale utente o filiale da elaborare se <> da 999
024200170512         clear wFil;
024300170512         IF  IOR87fil <> 999;
024400170512           wFil = IOR87fil;
024500170512         ELSE;
024600170512           wFil = DUTpou;
024700170512         ENDIF;
024800170512         clear TIBS02DS;
024900170512         clear dDFT;
025000170512         T02mod = 'C';
025100170512         T02sif = knsif;
025200170512         T02cod = 'DFT';
025300170512         T02ke1 = %editc(wFil:'X');
025400170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
025500170512         IF  T02err = *blanks;
025600170512           dDFT = T02uni;
025700170512           leavesr;
025800170512         ENDIF;
025900170512       //?se non trovo con la filiale specifica provo con 999 generico
026000170512         clear TIBS02DS;
026100170512         clear dDFT;
026200170512         T02mod = 'C';
026300170512         T02sif = knsif;
026400170512         T02cod = 'DFT';
026500170512         T02ke1 = '999';
026600170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
026700170512         IF  T02err = *blanks;
026800170512           dDFT = T02uni;
026900170512         ENDIF;
027000170512
027100170512       //?Se richieste tutte le filiali carico in sk le filiali abilitate all'utente
027200170512         IF  IOR87fil = 999;
027300170512           clear TRUL31DS;
027400170512           I31abi = wAbi;
027500170512           I31cdi = DUTdis;
027600170512           I31car = DUTare;
027700170512           I31cpo = DUTpou;
027800170512           TRUL31R (kpjba:trul31ds);
027900170512           IF  O31pog <= *zeros;
028000170512             exsr RoutEnd;
028100170512           ENDIF;
028200170512         ELSE;
028300170512           skPOG(1) = %editc(IOR87fil:'X');
028400170512         ENDIF;
028500170512
028600170512       //?Pulisco il file di work
028700170512         exec sql DELETE from GAITRAAZM/WFSRP00F;
028800170512
028900170512       //?Imposto le date in formato ISO
029000170512         Data_ISO  = (%date(IOR87datai));
029100170512         Data_ISOf = (%date(IOR87dataf));
029200170512
029300170512       //?Carico in schiera le fasi da non elaborare per le statistiche
029400170512         xx = 1;
029500170512         setll ('FAR') TNTBE01L;
029600170512         reade ('FAR') TNTBE01L;
029700170512         DOW  not %eof(TNTBE01L);
029800170512           dFAR = TBEuni;
029900170512           IF  TBEatb = *blanks and d§FARsta <> *blanks;
030000170512             skFAR(xx) = %dec(%subst(TBEke1:1:3):3:0);
030100170512             xx += 1;
030200170512           ENDIF;
030300170512           reade ('FAR') TNTBE01L;
030400170512         ENDDO;
030500170512
030600170512       //?Apro file Distinte
030700170512         IF  %open(FIDST01L);
030800170512           leavesr;
030900170512         ENDIF;
031000170512
031100170512       //?Cerco il nome del sistema
031200170512         UBRTVNETA_Rtv(currSysNeta);
031300170512         IF  %subst(currSysNetA:1:5 ) = 'AS888';
031400170512           wLib = 'FILTRAPRD';
031500170512         ELSE;
031600170512           wLib = 'FILTRA201';
031700170512         ENDIF;
031800170512
031900170512       //?Apro file Distinte
032000170512         wFLib = %trim(wLib) + '/FIDST01L';
032100170512         open FIDST01L;
032200170512
032300170512       ENDSR;
032400170512
032500170512       //--------------------------------------------------------------
032600170512       //?Reperimento Dati del job (Utente/Operativi).
032700170512       //--------------------------------------------------------------
032800170512       BEGSR DatiJob;
032900170512
033000170512         in(E) §AzUte;
033100170512         IF  NOT %error;
033200170512           in(E) §DatiUte;
033300170512         ENDIF;
033400170512         IF  %error or RSut = *blanks;
033500170512           clear TIBS34ds;
033600170512           tibs34r(tibs34ds);
033700170512           in §AzUte;
033800170512           in §DatiUte;
033900170512         ENDIF;
034000170512
034100170512         clear wAbi;
034200170512         clear dLAT;
034300170512
034400170512       //?Controllo autorizzazioni profilo
034500170512         SELECT;
034600170512         WHEN  DUTerr = 'E';
034700170512           exsr RoutEnd;
034800170512           leavesr;
034900170512         WHEN  UTEaut <> *blanks;
035000170512           wAbi = UTEaut;
035100170512         OTHER;
035200170512           IF  DUTlpo = 'S';
035300170512             wAbi = 'AZ';
035400170512           ENDIF;
035500170512           IF  DUTlpo = '1';
035600170512             wAbi = 'TP';
035700170512           ENDIF;
035800170512           IF  DUTlpo = '2';
035900170512             wAbi = 'PO';
036000170512           ENDIF;
036100170512         ENDSL;
036200170512
036300170512       //?Controllo se l'autorizzazione è valida
036400170512         clear dLAT;
036500170512         clear TIBS02DS;
036600170512         T02mod = 'C';
036700170512         T02sif = knsif;
036800170512         T02cod = 'LAT';
036900170512         T02ke1 = wAbi;
037000170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
037100170512         dLAT = T02uni;
037200170512         IF  T02err <> *blanks or §LATabi = 'S';
037300170512           exsr RoutEnd;
037400170512         ENDIF;
037500170512
037600170512       ENDSR;
037700170515
037800170515       //--------------------------------------------------------------
037900170515       //?Controlla se giorno festivo.
038000170515       //--------------------------------------------------------------
038100170515       BEGSR Festa;
038200170515
038300170515         IsFesta = *off;
038400170515         clear wFestaMat;
038500170515         clear wFestaPom;
038600170519         clear k_CLNtfp;
038700170519         k_CLNtfa = wFil;
038800170515         k_CLNann = %subdt(Data_ISO:*years);
038900170515         k_CLNmes = %subdt(Data_ISO:*months);
039000170515         wGiorno  = %subdt(Data_ISO:*days);
039100170515         chain %kds(kazcln01) AZCLN01L;
039200170515         IF  not %found(AZCLN01L);
039300170515           leavesr;
039400170515         ENDIF;
039500170515         IF  mat(wGiorno) <> *blanks;
039600170515           wMatPom = mat(wGiorno);
039700170515           exsr Tab1Y;
039800170515           wFestaMat = §1Yfei;
039900170515         ENDIF;
040000170515         IF  pom(wGiorno) <> *blanks;
040100170515           wMatPom = pom(wGiorno);
040200170515           exsr Tab1Y;
040300170515           wFestaPom = §1Yfei;
040400170515         ENDIF;
040500170515         IF  wFestaMat = 'S' or wFestaPom = 'S';
040600170515           IsFesta = *on;
040700170515         ENDIF;
040800170515
040900170515       ENDSR;
041000170512
041100170512       //--------------------------------------------------------------
041200170512       //?Elabora Statistica.
041300170512       //--------------------------------------------------------------
041400170512       BEGSR Elabora;
041500170512
041600170512         clear WFSRP000;
041700170512
041800170512         clear OG143;
041900170516         clear OG148;
042000170512         chain (wFil) AZORG01L;
042100170512         IF  %found(AZORG01L) and ORGfva = *blanks and
042200170512             (ORGfag = 'F' or ORGfag = 'A');
042300170512           OG143 = ORGde3;
042400170516           OG148 = ORGde8;
042500170512         ENDIF;
042600170516       //?Non elaboro se filiale non partita con la procedura ORM
042700170516         IF  §OGorm <> 'S';
042800170516           leavesr;
042900170516         ENDIF;
043000170512       //?Non elaboro se network estero
043100170512         IF  §OGntw = 'DPD' or §OGntw = 'EEX' or §OGntw = 'FED';
043200170512           leavesr;
043300170512         ENDIF;
043400170512
043500170512       //?Recupero l'ora limite picking
043600170512         wOLPora = GetOraLimitePicking(wFil:wData);
043700170512
043800170512       //?Cerco il primo giorno precedente festivo
043900170512         exsr PrimoGiornoFestivo;
044000170512
044100170512         EndSqlORM = *off;
044200170512
044300170512       //?Preparo la stringa SQL
044400170512         exsr StringaSql;
044500170512
044600170512       //?Leggo i dati
044700170512         exec sql PREPARE S1 from :wSQL;
044800170512         exec sql DECLARE WORM cursor for S1;
044900170512
045000170512       //?Apertura del cursore
045100170512         exec sql open WORM;
045200170512
045300170512         IF  sqlcode < 0;
045400170512           EndSqlORM = *on;
045500170512         ENDIF;
045600170512
045700170512         DOW  not EndSqlORM;
045800170512           exec sql
045900170512           fetch next from WORM into :FNORM00F, :FNORG00F;
046000170512           IF  sqlcod = 100 or sqlcod < 0;
046100170512             EndSqlORM = *on;
046200170512             leave;
046300170512           ENDIF;
046400170512
046500170512         //?Conto i ritiri
046600170512           exsr Conta;
046700170512
046800170512         ENDDO;
046900170512
047000170512         //?Scrivo file di work
047100170512         exsr Scrivi;
047200170512
047300170512         exec sql close WORM;
047400170512
047500170512       ENDSR;
047600170512
047700170512       //--------------------------------------------------------------
047800170512       //?Preparazione della stringa SQL.
047900170512       //--------------------------------------------------------------
048000170512       BEGSR StringaSql;
048100170512
048200170512       //?Seleziono gli ORM
048300170512         wSQL =
048400170512         'SELECT * from FNORM00F join FNORG00F on -
048500170512                   ORMpoe = ORGpoe and ORMnsr = ORGnsr and -
048600170512                   ORMnor = ORGnor and ORMnrv = ORGnrv -
048700170516                   WHERE ORMnsr = 0 and ORMdao <= ORMdar and -
048800170512                   ORGpkg < ' + %editc(d§DFTpkgdt:'4') +
048900170512                 ' and ORMdao <= ' + %editc(wData:'X') +
049000170512                 ' and ORMdar <= ' + %editc(wData:'X') +
049100170515                 ' and ORMpor = ' + %editc(wFil:'X') +
049200170515                 ' order by ORMpoe, ORMnsr, ORMnor, ORMnrv';
049300170512       ENDSR;
049400170512
049500170512       //--------------------------------------------------------------
049600170512       //?Conto i ritiri.
049700170512       //--------------------------------------------------------------
049800170512       BEGSR Conta;
049900170512
050000170516       //?non considero i ritiri che hanno
050100170515       //?data ritiro <= data distinta e
050200170512       //?data ultima fase < data distina e ultima fase >= 600
050300170515         IF  ORMdar <= wData and ORMdfo < wData and ORMfao >= 600;
050400170512           leavesr;
050500170512         ENDIF;
050600170516
050700170516       //?cerco la data pronta merce
050800170512         clear dOREdt;
050900170512         OREtrc = 'DT';
051000170512         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
051100170512         IF  %found(FNORE01L);
051200170512           dOREdt = OREdati;
051300170512         ELSE;
051400170512           §OREdpm = ORMdar;
051500170512         ENDIF;
051600170516
051700170516       //?non considero i ritiri che hanno
051800170516       //?data pronta merce > data distina
051900170517         IF  §OREdpm > wData;
052000170516           leavesr;
052100170516         ENDIF;
052200170516
052300170517       //?Se ORM con data immissione = data distinta non è assegnabile
052400170516       //?prima del picking lo conto e poi vado via
052500170517         IF  ORMdao = wData;
052600170516           SRPnoass += 1;
052700170516           leavesr;
052800170516         ENDIF;
052900170512
053000170512       //?Controllo se il ritiro letto è da considerare come
053100170512       //?ORM assegnabile prima del picking
053200170512       //?se non lo è vado via
053300170512         exsr Assegnabile;
053400170512         IF  not wOkAssegnabile;
053500170512           leavesr;
053600170512         ENDIF;
053700170516
053800170516       //?conto a parte i ritiri assegnabili che hanno
053900170516       //?data pronta merce = data distina e ora pronta merce > 1230
054000170516         IF  §OREdpm = wData and ORMorr > 1230;
054100170516           SRPtotpom += 1;
054200170516           leavesr;
054300170516         ENDIF;
054400170516
054500170516       //?se arrivo qua è un ORM assegnabili
054600170516       //?con data pronta merce = data distina e ora pronta merce <= 1230
054700170516         SRPtotmat += 1;
054800170512
054900170512       //?Controllo se assegnato prima dell'ora limite picking
055000170512         exsr Assegnato;
055100170512       //?se non lo è vado via
055200170512         IF  not wOkAssegnato;
055300170512           leavesr;
055400170512         ENDIF;
055500170512       //?Controllo se Distinta a PDA o no
055600170512       //?se distinta PDA in TEST non considero come PDA
055700170512         k_DSTnpg = 4;
055800170512         k_DSTnfv = ORFndc;
055900170512         k_DSTfgs = ORFfgs;
056000170512         chain %kds(k03fidst) FIDST01L;
056100170512         IF  %found(FIDST01L);
056200170512           dDSTflr = DSTflr;
056300170512           IF  (DSTpda = 'E' or DSTpda <> 'O') and
056400170512               §DSTtstPda = *blanks;
056500170512             SRPass += 1;
056600170512           ELSE;
056700170512             SRPassnpda += 1;
056800170512             leavesr;
056900170512           ENDIF;
057000170512         ENDIF;
057100170512
057200170512       //?Controllo se comunicato prima dell'ora limite picking
057300170512       //?la data e ora comunicazione ce l'ho nella fase 400
057400170512       //?appena letta
057500170512         IF  §ORFesito > *zeros and
057600170515             (%subst(§ORFesito:1:8) < %editc(wData:'X') or
057700170515             (%subst(§ORFesito:1:8) = %editc(wData:'X') and
057800170515              %subst(§ORFesito:9:4) <= wOLPora));
057900170512           SRPcom += 1;
058000170512         ENDIF;
058100170512
058200170512       //?Controllo se confermato prima dell'ora limite picking
058300170512       //?per farlo devo leggere i LOG dei PDA
058400170512         exsr Confermato;
058500170512
058600170516       //?Controllo se esitato entro le 13
058700170512       //?per farlo devo leggere i LOG dei PDA
058800170512         exsr Esito;
058900170512
059000170512       ENDSR;
059100170512
059200170512       //--------------------------------------------------------------
059300170512       //?Scrivo il file di work.
059400170512       //--------------------------------------------------------------
059500170512       BEGSR Scrivi;
059600170512
059700170512         SRPfil = wFil;
059800170512         SRPdes = ORGdes;
059900170512         SRPolp = %int(wOLPora);
060000170512         SRPdata = wData;
060100170512
060200170512         write WFSRP000;
060300170512
060400170512       ENDSR;
060500170512
060600170512       //--------------------------------------------------------------
060700170512       //?Controllo se ORM letto è assegnabile.
060800170512       //--------------------------------------------------------------
060900170512       BEGSR Assegnabile;
061000170512
061100170512         wOkAssegnabile = *off;
061200170512
061300170512       //?Data ritiro = data distinta è assegnabile
061400170512         IF  ORMdar = wData;
061500170512           wOkAssegnabile = *on;
061600170512           leavesr;
061700170512         ENDIF;
061800170512
061900170512       //?Data ritiro < data distinta
062000170515       //?non faccio il controllo tanto sto leggendo SOLO i ritiri con
062100170515       //?data ritiro <= data distinta
062200170515
062300170515       //?Devo leggere le fasi per stabilire se è assegnabile
062400170515         SELECT;
062500170515       //?Se la data ultima fase della testata è >= data distina
062600170515       //?devo leggere l'ultima fase del gg precedente
062700170515         WHEN  ORMdfo >= wData;
062800170515           wData_ISO = (%date(wData));
062900170515           wData_ISO -= %days(1);
063000170515           wDataFase = %dec(wData_ISO);
063100170515           setgt (ORMpoe:ORMnsr:ORMnor:ORMnrv:wDataFase) FNORF01L;
063200170515           readpe (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORF01L;
063300170515       //?Se la data ultima fase della testata è < data distina
063400170515       //?devo leggere l'ultima fase fatta
063500170515         OTHER;
063600170515           setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
063700170515           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
063800170515         ENDSL;
063900170515         //?E' se la fase che sto leggendo non è >= 600
064000170515         DOW  not %eof(FNORF01L);
064100170515           IF  ORFfar >= 600;
064200170515             leave;
064300170515           ENDIF;
064400170515           wOkAssegnabile = *on;
064500170515           leave;
064600170515         ENDDO;
064700170512
064800170512       ENDSR;
064900170512
065000170512       //--------------------------------------------------------------
065100170512       //?ORM Assegnato.
065200170512       //--------------------------------------------------------------
065300170512       BEGSR Assegnato;
065400170512
065500170512         clear dORF01;
065600170515         wOkAssegnato = *off;
065700170512
065800170512       //?Devo leggere le fasi fatte in data distinta e verificare se è stato
065900170512       //?assegnato
066000170512         setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
066100170512         reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
066200170515         DOW  not %eof(FNORF07L);
066300170512           wOkAssegnato = *off;
066400170515           IF  ORFfgs = wFil and
066500170515              (ORFdae < wData or
066600170515              (ORFdae = wData and %subst(%editc(ORFore:'X'):1:4) <= wOLPora));
066700170512             dORF01 = ORFflo;
066800170512             wOkAssegnato = *on;
066900170512           ENDIF;
067000170515           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
067100170512         ENDDO;
067200170512
067300170512       ENDSR;
067400170512
067500170512       //--------------------------------------------------------------
067600170512       //?ORM Confermato.
067700170512       //--------------------------------------------------------------
067800170512       BEGSR Confermato;
067900170512
068000170512         EndSqlPLG = *off;
068100170512
068200170512         exec sql
068300170512         DECLARE CONF cursor for
068400170512         SELECT * from FIPLG00F
068500170512         WHERE PLGtipappl = 'R' and PLGfgs = :ORFfgs and
068600170512               PLGndc = :ORFndc and PLGcodaut = :DSTpdr and
068700170512               PLGtiporcd = 'OK ' and PLGerr = ' ' and
068800170512               PLGiddoc = digits(:ORMpoe) concat
068900170512                         (digits(:ORMnsr)) concat
069000170512                         (digits(:ORMnor)) concat
069100170512                         (digits(:ORMnrv));
069200170512
069300170512         exec sql open CONF;
069400170512
069500170512         IF  sqlcode < 0;
069600170512           EndSqlPLG = *on;
069700170512         ENDIF;
069800170512
069900170512         DOW  not EndSqlPLG;
070000170512           exec sql
070100170512           fetch next from CONF into :FIPLG00F;
070200170512           IF  sqlcod = 100 or sqlcod < 0;
070300170512             EndSqlPLG = *on;
070400170512             leave;
070500170512           ENDIF;
070600170512
070700170512           FIPRCOKDS = PLGdati;
070800170512
070900170512         //?Conto se la conferma è avvenuta prima dell'ora limite picking
071000170512           IF  %subst(§OKdatora:1:8) = %editc(wData:'X') and
071100170512               %subst(§OKdatora:9:4) <= wOLPora;
071200170512             SRPconf += 1;
071300170512             leave;
071400170512           ENDIF;
071500170512         ENDDO;
071600170512
071700170512         exec sql close CONF;
071800170512
071900170512       ENDSR;
072000170512
072100170512       //--------------------------------------------------------------
072200170512       //?ORM Esitato.
072300170512       //--------------------------------------------------------------
072400170512       BEGSR Esito;
072500170512
072600170512         EndSqlPLG = *off;
072700170512
072800170512         exec sql
072900170512         DECLARE PLG cursor for
073000170512         SELECT * from FIPLG00F
073100170512         WHERE PLGtipappl = 'R' and PLGfgs = :ORFfgs and
073200170512               PLGndc = :ORFndc and PLGcodaut = :DSTpdr and
073300170512               PLGtiporcd = 'RES' and PLGerr = ' ' and
073400170512               PLGiddoc = digits(:ORMpoe) concat
073500170512                         (digits(:ORMnsr)) concat
073600170512                         (digits(:ORMnor)) concat
073700170512                         (digits(:ORMnrv));
073800170512
073900170512         exec sql open PLG;
074000170512
074100170512         IF  sqlcode < 0;
074200170512           EndSqlPLG = *on;
074300170512         ENDIF;
074400170512
074500170512         DOW  not EndSqlPLG;
074600170512           exec sql
074700170512           fetch next from PLG into :FIPLG00F;
074800170512           IF  sqlcod = 100 or sqlcod < 0;
074900170512             EndSqlPLG = *on;
075000170512             leave;
075100170512           ENDIF;
075200170512
075300170512           FIPRCRESDS = PLGdati;
075400170512
075500170512         //?Conto se l'esito è stato fatto prima dell'ora limite picking
075600170512           IF  %subst(§RESdatora:1:8) = %editc(wData:'X') and
075700170512               %subst(§RESdatora:9:4) <= '1300';
075800170512             SRPesito += 1;
075900170512             leave;
076000170512           ENDIF;
076100170512         ENDDO;
076200170512
076300170512         exec sql close PLG;
076400170512
076500170512       ENDSR;
076600170512
076700170512       //--------------------------------------------------------------
076800170512       //?Cerco il primo giorno precedente festivo.
076900170512       //--------------------------------------------------------------
077000170512       BEGSR PrimoGiornoFestivo;
077100170512
077200170512         EndFesta = *on;
077300170512         wData_ISO = (%date(wData));
077400170512         clear wDataFestivo;
077500170512
077600170512         DOW  not EndFesta;
077700170512           clear wMatPom;
077800170512           clear wFestaMat;
077900170512           clear wFestaPom;
078000170512           wData_ISO -= %days(1);
078100170519           clear k_CLNtfp;
078200170519           k_CLNtfa = wFil;
078300170512           k_CLNann = %subdt(wData_ISO:*years);
078400170512           k_CLNmes = %subdt(wData_ISO:*months);
078500170512           wGiorno  = %subdt(wData_ISO:*days);
078600170512           chain %kds(kazcln01) AZCLN01L;
078700170512           IF  not %found(AZCLN01L);
078800170512             leavesr;
078900170512           ENDIF;
079000170512           IF  mat(wGiorno) <> *blanks;
079100170512             wMatPom = mat(wGiorno);
079200170512             exsr Tab1Y;
079300170512             wFestaMat = §1Yfei;
079400170512           ENDIF;
079500170512           IF  pom(wGiorno) <> *blanks;
079600170512             wMatPom = pom(wGiorno);
079700170512             exsr Tab1Y;
079800170512             wFestaPom = §1Yfei;
079900170512           ENDIF;
080000170512           IF  wFestaMat = 'S' or wFestaPom = 'S';
080100170512             wDataFestivo = %dec(wData_ISO);
080200170512             leavesr;
080300170512           ENDIF;
080400170512         ENDDO;
080500170512
080600170512       ENDSR;
080700170512
080800170512       //--------------------------------------------------------------
080900170512       //?Tabella 1Y - Simboli Calendario BRT.
081000170512       //--------------------------------------------------------------
081100170512       BEGSR Tab1Y;
081200170512
081300170512         k_TBLkut = 1;
081400170512         k_TBLcod = '1Y';
081500170512         clear ds1Y;
081600170512         k_TBLkey = wMatPom;
081700170512         chain  %kds(K03tabel) TABEL00F;
081800170512         IF  %found(TABEL00F);
081900170512           ds1Y = TBLuni;
082000170512         ENDIF;
082100170512
082200170512       ENDSR;
082300170512
082400170512       //--------------------------------------------------------------
082500170512       //?Operazioni finali.
082600170512       //--------------------------------------------------------------
082700170512       BEGSR RoutEnd;
082800170512
082900170512         *inLR = *on;
083000170512         return;
083100170512
083200170512       ENDSR;
083300170512
083400170512      *--------------------------------------------------
083500170512      * Procedure name: GetOraLimitePicking
083600170512      * Purpose:        Restituisce l'ora limite picking
083700170512      * Returns:        Ora/Minuti impostati in tab OLP
083800170512      * Parameter:      Filiale e Data
083900170512      *--------------------------------------------------
084000170512     p GetOraLimitePicking...
084100170512     p                 B
084200170512     d GetOraLimitePicking...
084300170512     d                 PI            10a
084400170512     d  fil                           3s 0 CONST
084500170512     d  data                          8s 0 CONST
084600170512
084700170512      // campi di comodo
084800170512     d  key1           s                   like(TBEke1)
084900170512     d  key2           s                   like(TBEke2)
085000170512     d  hhmm           s              4a
085100170512
085200170512       //?Imposto ora standard in caso di errore sulle tabelle
085300170512       hhmm = '0930';
085400170512
085500170512       //?Cerco per filiale/data
085600170512       key1 = %editc(fil:'X');
085700170512       key2 = %editc(data:'X');
085800170512       chain ('OLP':key1:key2) TNTBE01L;
085900170512       //?Non trovo per filiale/data cerco per filiale
086000170512       IF  not %found(TNTBE01L) or TBEatb <> *blanks;
086100170512         clear key2;
086200170512         chain ('OLP':key1:key2) TNTBE01L;
086300170512       ENDIF;
086400170512       //?Non trovo neanche per filiale cerco generico
086500170512       IF  not %found(TNTBE01L) or TBEatb <> *blanks;
086600170512         key1 = '000';
086700170512         chain ('OLP':key1:key2) TNTBE01L;
086800170512         ENDIF;
086900170512       //?Imposto ora limite trovata
087000170512       IF  %found(TNTBE01L) and TBEatb = *blanks and
087100170512           %subst(TBEuni:1:4) > *zeros;
087200170512         hhmm = %subst(TBEuni:1:4);
087300170512       ENDIF;
087400170512
087500170512       RETURN hhmm;
087600170512
087700170512     p GetOraLimitePicking...
087800170512     p                 E
