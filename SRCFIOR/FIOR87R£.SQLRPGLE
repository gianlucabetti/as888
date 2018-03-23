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
019800170512       //?Elaborazione Statistica per data e filiale
019900170512       //?Leggo per data
020000170512         DOW  Data_ISO <= Data_ISOf;
020100170515         //?Se fesivo non elaboro
020200170515           exsr Festa;
020300170515           IF  IsFesta;
020400170515             Data_ISO += %days(1);
020500170515             iter;
020600170515           ENDIF;
020700170512           wData = %dec(Data_ISO);
020800170512         //?Leggo per filiale
020900170512           xx = 1;
021000170512           FOR  xx by 1 to %elem(skPOG);
021100170512             IF  skPOG(xx) > *zeros;
021200170512               exsr Elabora;
021300170512             ENDIF;
021400170512           ENDFOR;
021500170512           Data_ISO += %days(1);
021600170512         ENDDO;
021700170512
021800170512       //?Operazioni finali
021900170512       exsr RoutEnd;
022000170512
022100170512       //--------------------------------------------------------------
022200170512       //?Operazioni iniziali.
022300170512       //--------------------------------------------------------------
022400170512       BEGSR RoutInz;
022500170512
022600170512         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022700170512
022800170512       //?Imposto oggi
022900170512         Oggi = %dec(%date());
023000170512
023100170512       //?Reperimento dati job
023200170512         exsr DatiJob;
023300170512
023400170512       //?Imposto la DS dalla KPJBU
023500170512         FIOR87DS = kpjbu;
023600170512
023700170512       //?Cerco il peso massimo da elaborare recuperandolo dalla tabella DFT
023800170512       //?prima con la filiale utente o filiale da elaborare se <> da 999
023900170512         clear wFil;
024000170512         IF  IOR87fil <> 999;
024100170512           wFil = IOR87fil;
024200170512         ELSE;
024300170512           wFil = DUTpou;
024400170512         ENDIF;
024500170512         clear TIBS02DS;
024600170512         clear dDFT;
024700170512         T02mod = 'C';
024800170512         T02sif = knsif;
024900170512         T02cod = 'DFT';
025000170512         T02ke1 = %editc(wFil:'X');
025100170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
025200170512         IF  T02err = *blanks;
025300170512           dDFT = T02uni;
025400170512           leavesr;
025500170512         ENDIF;
025600170512       //?se non trovo con la filiale specifica provo con 999 generico
025700170512         clear TIBS02DS;
025800170512         clear dDFT;
025900170512         T02mod = 'C';
026000170512         T02sif = knsif;
026100170512         T02cod = 'DFT';
026200170512         T02ke1 = '999';
026300170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
026400170512         IF  T02err = *blanks;
026500170512           dDFT = T02uni;
026600170512         ENDIF;
026700170512
026800170512       //?Se richieste tutte le filiali carico in sk le filiali abilitate all'utente
026900170512         IF  IOR87fil = 999;
027000170512           clear TRUL31DS;
027100170512           I31abi = wAbi;
027200170512           I31cdi = DUTdis;
027300170512           I31car = DUTare;
027400170512           I31cpo = DUTpou;
027500170512           TRUL31R (kpjba:trul31ds);
027600170512           IF  O31pog <= *zeros;
027700170512             exsr RoutEnd;
027800170512           ENDIF;
027900170512         ELSE;
028000170512           skPOG(1) = %editc(IOR87fil:'X');
028100170512         ENDIF;
028200170512
028300170512       //?Pulisco il file di work
028400170512         exec sql DELETE from GAITRAAZM/WFSRP00F;
028500170512
028600170512       //?Imposto le date in formato ISO
028700170512         Data_ISO  = (%date(IOR87datai));
028800170512         Data_ISOf = (%date(IOR87dataf));
028900170512
029000170512       //?Carico in schiera le fasi da non elaborare per le statistiche
029100170512         xx = 1;
029200170512         setll ('FAR') TNTBE01L;
029300170512         reade ('FAR') TNTBE01L;
029400170512         DOW  not %eof(TNTBE01L);
029500170512           dFAR = TBEuni;
029600170512           IF  TBEatb = *blanks and d§FARsta <> *blanks;
029700170512             skFAR(xx) = %dec(%subst(TBEke1:1:3):3:0);
029800170512             xx += 1;
029900170512           ENDIF;
030000170512           reade ('FAR') TNTBE01L;
030100170512         ENDDO;
030200170512
030300170512       //?Apro file Distinte
030400170512         IF  %open(FIDST01L);
030500170512           leavesr;
030600170512         ENDIF;
030700170512
030800170512       //?Cerco il nome del sistema
030900170512         UBRTVNETA_Rtv(currSysNeta);
031000170512         IF  %subst(currSysNetA:1:5 ) = 'AS888';
031100170512           wLib = 'FILTRAPRD';
031200170512         ELSE;
031300170512           wLib = 'FILTRA201';
031400170512         ENDIF;
031500170512
031600170512       //?Apro file Distinte
031700170512         wFLib = %trim(wLib) + '/FIDST01L';
031800170512         open FIDST01L;
031900170512
032000170512       ENDSR;
032100170512
032200170512       //--------------------------------------------------------------
032300170512       //?Reperimento Dati del job (Utente/Operativi).
032400170512       //--------------------------------------------------------------
032500170512       BEGSR DatiJob;
032600170512
032700170512         in(E) §AzUte;
032800170512         IF  NOT %error;
032900170512           in(E) §DatiUte;
033000170512         ENDIF;
033100170512         IF  %error or RSut = *blanks;
033200170512           clear TIBS34ds;
033300170512           tibs34r(tibs34ds);
033400170512           in §AzUte;
033500170512           in §DatiUte;
033600170512         ENDIF;
033700170512
033800170512         clear wAbi;
033900170512         clear dLAT;
034000170512
034100170512       //?Controllo autorizzazioni profilo
034200170512         SELECT;
034300170512         WHEN  DUTerr = 'E';
034400170512           exsr RoutEnd;
034500170512           leavesr;
034600170512         WHEN  UTEaut <> *blanks;
034700170512           wAbi = UTEaut;
034800170512         OTHER;
034900170512           IF  DUTlpo = 'S';
035000170512             wAbi = 'AZ';
035100170512           ENDIF;
035200170512           IF  DUTlpo = '1';
035300170512             wAbi = 'TP';
035400170512           ENDIF;
035500170512           IF  DUTlpo = '2';
035600170512             wAbi = 'PO';
035700170512           ENDIF;
035800170512         ENDSL;
035900170512
036000170512       //?Controllo se l'autorizzazione è valida
036100170512         clear dLAT;
036200170512         clear TIBS02DS;
036300170512         T02mod = 'C';
036400170512         T02sif = knsif;
036500170512         T02cod = 'LAT';
036600170512         T02ke1 = wAbi;
036700170512         TNTBE_RicercaControllo (kpjba:tibs02ds);
036800170512         dLAT = T02uni;
036900170512         IF  T02err <> *blanks or §LATabi = 'S';
037000170512           exsr RoutEnd;
037100170512         ENDIF;
037200170512
037300170512       ENDSR;
037400170515
037500170515       //--------------------------------------------------------------
037600170515       //?Controlla se giorno festivo.
037700170515       //--------------------------------------------------------------
037800170515       BEGSR Festa;
037900170515
038000170515         IsFesta = *off;
038100170515         clear wFestaMat;
038200170515         clear wFestaPom;
038300170515         clear k_CLNtfa;
038400170515         k_CLNtfp = wFil;
038500170515         k_CLNann = %subdt(Data_ISO:*years);
038600170515         k_CLNmes = %subdt(Data_ISO:*months);
038700170515         wGiorno  = %subdt(Data_ISO:*days);
038800170515         chain %kds(kazcln01) AZCLN01L;
038900170515         IF  not %found(AZCLN01L);
039000170515           leavesr;
039100170515         ENDIF;
039200170515         IF  mat(wGiorno) <> *blanks;
039300170515           wMatPom = mat(wGiorno);
039400170515           exsr Tab1Y;
039500170515           wFestaMat = §1Yfei;
039600170515         ENDIF;
039700170515         IF  pom(wGiorno) <> *blanks;
039800170515           wMatPom = pom(wGiorno);
039900170515           exsr Tab1Y;
040000170515           wFestaPom = §1Yfei;
040100170515         ENDIF;
040200170515         IF  wFestaMat = 'S' or wFestaPom = 'S';
040300170515           IsFesta = *on;
040400170515         ENDIF;
040500170515
040600170515       ENDSR;
040700170512
040800170512       //--------------------------------------------------------------
040900170512       //?Elabora Statistica.
041000170512       //--------------------------------------------------------------
041100170512       BEGSR Elabora;
041200170512
041300170512         clear WFSRP000;
041400170512         wFil = %dec(skPOG(xx):3:0);
041500170512
041600170512         clear OG143;
041700170516         clear OG148;
041800170512         chain (wFil) AZORG01L;
041900170512         IF  %found(AZORG01L) and ORGfva = *blanks and
042000170512             (ORGfag = 'F' or ORGfag = 'A');
042100170512           OG143 = ORGde3;
042200170516           OG148 = ORGde8;
042300170512         ENDIF;
042400170516       //?Non elaboro se filiale non partita con la procedura ORM
042500170516         IF  §OGorm <> 'S';
042600170516           leavesr;
042700170516         ENDIF;
042800170512       //?Non elaboro se network estero
042900170512         IF  §OGntw = 'DPD' or §OGntw = 'EEX' or §OGntw = 'FED';
043000170512           leavesr;
043100170512         ENDIF;
043200170512
043300170512       //?Recupero l'ora limite picking
043400170512         wOLPora = GetOraLimitePicking(wFil:wData);
043500170512
043600170512       //?Cerco il primo giorno precedente festivo
043700170512         exsr PrimoGiornoFestivo;
043800170512
043900170512         EndSqlORM = *off;
044000170512
044100170512       //?Preparo la stringa SQL
044200170512         exsr StringaSql;
044300170512
044400170512       //?Leggo i dati
044500170512         exec sql PREPARE S1 from :wSQL;
044600170512         exec sql DECLARE WORM cursor for S1;
044700170512
044800170512       //?Apertura del cursore
044900170512         exec sql open WORM;
045000170512
045100170512         IF  sqlcode < 0;
045200170512           EndSqlORM = *on;
045300170512         ENDIF;
045400170512
045500170512         DOW  not EndSqlORM;
045600170512           exec sql
045700170512           fetch next from WORM into :FNORM00F, :FNORG00F;
045800170512           IF  sqlcod = 100 or sqlcod < 0;
045900170512             EndSqlORM = *on;
046000170512             leave;
046100170512           ENDIF;
046200170512
046300170512         //?Conto i ritiri
046400170512           exsr Conta;
046500170512
046600170512         ENDDO;
046700170512
046800170512         //?Scrivo file di work
046900170512         exsr Scrivi;
047000170512
047100170512         exec sql close WORM;
047200170512
047300170512       ENDSR;
047400170512
047500170512       //--------------------------------------------------------------
047600170512       //?Preparazione della stringa SQL.
047700170512       //--------------------------------------------------------------
047800170512       BEGSR StringaSql;
047900170512
048000170512       //?Seleziono gli ORM
048100170512         wSQL =
048200170512         'SELECT * from FNORM00F join FNORG00F on -
048300170512                   ORMpoe = ORGpoe and ORMnsr = ORGnsr and -
048400170512                   ORMnor = ORGnor and ORMnrv = ORGnrv -
048500170516                   WHERE ORMnsr = 0 and ORMdao <= ORMdar and -
048600170512                   ORGpkg < ' + %editc(d§DFTpkgdt:'4') +
048700170512                 ' and ORMdao <= ' + %editc(wData:'X') +
048800170512                 ' and ORMdar <= ' + %editc(wData:'X') +
048900170515                 ' and ORMpor = ' + %editc(wFil:'X') +
049000170515                 ' order by ORMpoe, ORMnsr, ORMnor, ORMnrv';
049100170512       ENDSR;
049200170512
049300170512       //--------------------------------------------------------------
049400170512       //?Conto i ritiri.
049500170512       //--------------------------------------------------------------
049600170512       BEGSR Conta;
049700170512
049800170516       //?non considero i ritiri che hanno
049900170515       //?data ritiro <= data distinta e
050000170512       //?data ultima fase < data distina e ultima fase >= 600
050100170515         IF  ORMdar <= wData and ORMdfo < wData and ORMfao >= 600;
050200170512           leavesr;
050300170512         ENDIF;
050400170516
050500170516       //?cerco la data pronta merce
050600170512         clear dOREdt;
050700170512         OREtrc = 'DT';
050800170512         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
050900170512         IF  %found(FNORE01L);
051000170512           dOREdt = OREdati;
051100170512         ELSE;
051200170512           §OREdpm = ORMdar;
051300170512         ENDIF;
051400170516
051500170516       //?non considero i ritiri che hanno
051600170516       //?data pronta merce > data distina
051700170517         IF  §OREdpm > wData;
051800170516           leavesr;
051900170516         ENDIF;
052000170516
052100170517       //?Se ORM con data immissione = data distinta non è assegnabile
052200170516       //?prima del picking lo conto e poi vado via
052300170517         IF  ORMdao = wData;
052400170516           SRPnoass += 1;
052500170516           leavesr;
052600170516         ENDIF;
052700170512
052800170512       //?Controllo se il ritiro letto è da considerare come
052900170512       //?ORM assegnabile prima del picking
053000170512       //?se non lo è vado via
053100170512         exsr Assegnabile;
053200170512         IF  not wOkAssegnabile;
053300170512           leavesr;
053400170512         ENDIF;
053500170516
053600170516       //?conto a parte i ritiri assegnabili che hanno
053700170516       //?data pronta merce = data distina e ora pronta merce > 1230
053800170516         IF  §OREdpm = wData and ORMorr > 1230;
053900170516           SRPtotpom += 1;
054000170516           leavesr;
054100170516         ENDIF;
054200170516
054300170516       //?se arrivo qua è un ORM assegnabili
054400170516       //?con data pronta merce = data distina e ora pronta merce <= 1230
054500170516         SRPtotmat += 1;
054600170512
054700170512       //?Controllo se assegnato prima dell'ora limite picking
054800170512         exsr Assegnato;
054900170512       //?se non lo è vado via
055000170512         IF  not wOkAssegnato;
055100170512           leavesr;
055200170512         ENDIF;
055300170512       //?Controllo se Distinta a PDA o no
055400170512       //?se distinta PDA in TEST non considero come PDA
055500170512         k_DSTnpg = 4;
055600170512         k_DSTnfv = ORFndc;
055700170512         k_DSTfgs = ORFfgs;
055800170512         chain %kds(k03fidst) FIDST01L;
055900170512         IF  %found(FIDST01L);
056000170512           dDSTflr = DSTflr;
056100170512           IF  (DSTpda = 'E' or DSTpda <> 'O') and
056200170512               §DSTtstPda = *blanks;
056300170512             SRPass += 1;
056400170512           ELSE;
056500170512             SRPassnpda += 1;
056600170512             leavesr;
056700170512           ENDIF;
056800170512         ENDIF;
056900170512
057000170512       //?Controllo se comunicato prima dell'ora limite picking
057100170512       //?la data e ora comunicazione ce l'ho nella fase 400
057200170512       //?appena letta
057300170512         IF  §ORFesito > *zeros and
057400170515             (%subst(§ORFesito:1:8) < %editc(wData:'X') or
057500170515             (%subst(§ORFesito:1:8) = %editc(wData:'X') and
057600170515              %subst(§ORFesito:9:4) <= wOLPora));
057700170512           SRPcom += 1;
057800170512         ENDIF;
057900170512
058000170512       //?Controllo se confermato prima dell'ora limite picking
058100170512       //?per farlo devo leggere i LOG dei PDA
058200170512         exsr Confermato;
058300170512
058400170516       //?Controllo se esitato entro le 13
058500170512       //?per farlo devo leggere i LOG dei PDA
058600170512         exsr Esito;
058700170512
058800170512       ENDSR;
058900170512
059000170512       //--------------------------------------------------------------
059100170512       //?Scrivo il file di work.
059200170512       //--------------------------------------------------------------
059300170512       BEGSR Scrivi;
059400170512
059500170512         SRPfil = wFil;
059600170512         SRPdes = ORGdes;
059700170512         SRPolp = %int(wOLPora);
059800170512         SRPdata = wData;
059900170512
060000170512         write WFSRP000;
060100170512
060200170512       ENDSR;
060300170512
060400170512       //--------------------------------------------------------------
060500170512       //?Controllo se ORM letto è assegnabile.
060600170512       //--------------------------------------------------------------
060700170512       BEGSR Assegnabile;
060800170512
060900170512         wOkAssegnabile = *off;
061000170512
061100170512       //?Data ritiro = data distinta è assegnabile
061200170512         IF  ORMdar = wData;
061300170512           wOkAssegnabile = *on;
061400170512           leavesr;
061500170512         ENDIF;
061600170512
061700170512       //?Data ritiro < data distinta
061800170515       //?non faccio il controllo tanto sto leggendo SOLO i ritiri con
061900170515       //?data ritiro <= data distinta
062000170515
062100170515       //?Devo leggere le fasi per stabilire se è assegnabile
062200170515         SELECT;
062300170515       //?Se la data ultima fase della testata è >= data distina
062400170515       //?devo leggere l'ultima fase del gg precedente
062500170515         WHEN  ORMdfo >= wData;
062600170515           wData_ISO = (%date(wData));
062700170515           wData_ISO -= %days(1);
062800170515           wDataFase = %dec(wData_ISO);
062900170515           setgt (ORMpoe:ORMnsr:ORMnor:ORMnrv:wDataFase) FNORF01L;
063000170515           readpe (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORF01L;
063100170515       //?Se la data ultima fase della testata è < data distina
063200170515       //?devo leggere l'ultima fase fatta
063300170515         OTHER;
063400170515           setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
063500170515           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:ORMdfo:ORMofo:ORMfao) FNORF01L;
063600170515         ENDSL;
063700170515         //?E' se la fase che sto leggendo non è >= 600
063800170515         DOW  not %eof(FNORF01L);
063900170515           IF  ORFfar >= 600;
064000170515             leave;
064100170515           ENDIF;
064200170515           wOkAssegnabile = *on;
064300170515           leave;
064400170515         ENDDO;
064500170512
064600170512       ENDSR;
064700170512
064800170512       //--------------------------------------------------------------
064900170512       //?ORM Assegnato.
065000170512       //--------------------------------------------------------------
065100170512       BEGSR Assegnato;
065200170512
065300170512         clear dORF01;
065400170515         wOkAssegnato = *off;
065500170512
065600170512       //?Devo leggere le fasi fatte in data distinta e verificare se è stato
065700170512       //?assegnato
065800170512         setll (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
065900170512         reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
066000170515         DOW  not %eof(FNORF07L);
066100170512           wOkAssegnato = *off;
066200170515           IF  ORFfgs = wFil and
066300170515              (ORFdae < wData or
066400170515              (ORFdae = wData and %subst(%editc(ORFore:'X'):1:4) <= wOLPora));
066500170512             dORF01 = ORFflo;
066600170512             wOkAssegnato = *on;
066700170512           ENDIF;
066800170515           reade (ORMpoe:ORMnsr:ORMnor:ORMnrv:wData) FNORF07L;
066900170512         ENDDO;
067000170512
067100170512       ENDSR;
067200170512
067300170512       //--------------------------------------------------------------
067400170512       //?ORM Confermato.
067500170512       //--------------------------------------------------------------
067600170512       BEGSR Confermato;
067700170512
067800170512         EndSqlPLG = *off;
067900170512
068000170512         exec sql
068100170512         DECLARE CONF cursor for
068200170512         SELECT * from FIPLG00F
068300170512         WHERE PLGtipappl = 'R' and PLGfgs = :ORFfgs and
068400170512               PLGndc = :ORFndc and PLGcodaut = :DSTpdr and
068500170512               PLGtiporcd = 'OK ' and PLGerr = ' ' and
068600170512               PLGiddoc = digits(:ORMpoe) concat
068700170512                         (digits(:ORMnsr)) concat
068800170512                         (digits(:ORMnor)) concat
068900170512                         (digits(:ORMnrv));
069000170512
069100170512         exec sql open CONF;
069200170512
069300170512         IF  sqlcode < 0;
069400170512           EndSqlPLG = *on;
069500170512         ENDIF;
069600170512
069700170512         DOW  not EndSqlPLG;
069800170512           exec sql
069900170512           fetch next from CONF into :FIPLG00F;
070000170512           IF  sqlcod = 100 or sqlcod < 0;
070100170512             EndSqlPLG = *on;
070200170512             leave;
070300170512           ENDIF;
070400170512
070500170512           FIPRCOKDS = PLGdati;
070600170512
070700170512         //?Conto se la conferma è avvenuta prima dell'ora limite picking
070800170512           IF  %subst(§OKdatora:1:8) = %editc(wData:'X') and
070900170512               %subst(§OKdatora:9:4) <= wOLPora;
071000170512             SRPconf += 1;
071100170512             leave;
071200170512           ENDIF;
071300170512         ENDDO;
071400170512
071500170512         exec sql close CONF;
071600170512
071700170512       ENDSR;
071800170512
071900170512       //--------------------------------------------------------------
072000170512       //?ORM Esitato.
072100170512       //--------------------------------------------------------------
072200170512       BEGSR Esito;
072300170512
072400170512         EndSqlPLG = *off;
072500170512
072600170512         exec sql
072700170512         DECLARE PLG cursor for
072800170512         SELECT * from FIPLG00F
072900170512         WHERE PLGtipappl = 'R' and PLGfgs = :ORFfgs and
073000170512               PLGndc = :ORFndc and PLGcodaut = :DSTpdr and
073100170512               PLGtiporcd = 'RES' and PLGerr = ' ' and
073200170512               PLGiddoc = digits(:ORMpoe) concat
073300170512                         (digits(:ORMnsr)) concat
073400170512                         (digits(:ORMnor)) concat
073500170512                         (digits(:ORMnrv));
073600170512
073700170512         exec sql open PLG;
073800170512
073900170512         IF  sqlcode < 0;
074000170512           EndSqlPLG = *on;
074100170512         ENDIF;
074200170512
074300170512         DOW  not EndSqlPLG;
074400170512           exec sql
074500170512           fetch next from PLG into :FIPLG00F;
074600170512           IF  sqlcod = 100 or sqlcod < 0;
074700170512             EndSqlPLG = *on;
074800170512             leave;
074900170512           ENDIF;
075000170512
075100170512           FIPRCRESDS = PLGdati;
075200170512
075300170512         //?Conto se l'esito è stato fatto prima dell'ora limite picking
075400170512           IF  %subst(§RESdatora:1:8) = %editc(wData:'X') and
075500170512               %subst(§RESdatora:9:4) <= '1300';
075600170512             SRPesito += 1;
075700170512             leave;
075800170512           ENDIF;
075900170512         ENDDO;
076000170512
076100170512         exec sql close PLG;
076200170512
076300170512       ENDSR;
076400170512
076500170512       //--------------------------------------------------------------
076600170512       //?Cerco il primo giorno precedente festivo.
076700170512       //--------------------------------------------------------------
076800170512       BEGSR PrimoGiornoFestivo;
076900170512
077000170512         EndFesta = *on;
077100170512         wData_ISO = (%date(wData));
077200170512         clear wDataFestivo;
077300170512
077400170512         DOW  not EndFesta;
077500170512           clear wMatPom;
077600170512           clear wFestaMat;
077700170512           clear wFestaPom;
077800170512           wData_ISO -= %days(1);
077900170512           clear k_CLNtfa;
078000170512           k_CLNtfp = wFil;
078100170512           k_CLNann = %subdt(wData_ISO:*years);
078200170512           k_CLNmes = %subdt(wData_ISO:*months);
078300170512           wGiorno  = %subdt(wData_ISO:*days);
078400170512           chain %kds(kazcln01) AZCLN01L;
078500170512           IF  not %found(AZCLN01L);
078600170512             leavesr;
078700170512           ENDIF;
078800170512           IF  mat(wGiorno) <> *blanks;
078900170512             wMatPom = mat(wGiorno);
079000170512             exsr Tab1Y;
079100170512             wFestaMat = §1Yfei;
079200170512           ENDIF;
079300170512           IF  pom(wGiorno) <> *blanks;
079400170512             wMatPom = pom(wGiorno);
079500170512             exsr Tab1Y;
079600170512             wFestaPom = §1Yfei;
079700170512           ENDIF;
079800170512           IF  wFestaMat = 'S' or wFestaPom = 'S';
079900170512             wDataFestivo = %dec(wData_ISO);
080000170512             leavesr;
080100170512           ENDIF;
080200170512         ENDDO;
080300170512
080400170512       ENDSR;
080500170512
080600170512       //--------------------------------------------------------------
080700170512       //?Tabella 1Y - Simboli Calendario BRT.
080800170512       //--------------------------------------------------------------
080900170512       BEGSR Tab1Y;
081000170512
081100170512         k_TBLkut = 1;
081200170512         k_TBLcod = '1Y';
081300170512         clear ds1Y;
081400170512         k_TBLkey = wMatPom;
081500170512         chain  %kds(K03tabel) TABEL00F;
081600170512         IF  %found(TABEL00F);
081700170512           ds1Y = TBLuni;
081800170512         ENDIF;
081900170512
082000170512       ENDSR;
082100170512
082200170512       //--------------------------------------------------------------
082300170512       //?Operazioni finali.
082400170512       //--------------------------------------------------------------
082500170512       BEGSR RoutEnd;
082600170512
082700170512         *inLR = *on;
082800170512         return;
082900170512
083000170512       ENDSR;
083100170512
083200170512      *--------------------------------------------------
083300170512      * Procedure name: GetOraLimitePicking
083400170512      * Purpose:        Restituisce l'ora limite picking
083500170512      * Returns:        Ora/Minuti impostati in tab OLP
083600170512      * Parameter:      Filiale e Data
083700170512      *--------------------------------------------------
083800170512     p GetOraLimitePicking...
083900170512     p                 B
084000170512     d GetOraLimitePicking...
084100170512     d                 PI            10a
084200170512     d  fil                           3s 0 CONST
084300170512     d  data                          8s 0 CONST
084400170512
084500170512      // campi di comodo
084600170512     d  key1           s                   like(TBEke1)
084700170512     d  key2           s                   like(TBEke2)
084800170512     d  hhmm           s              4a
084900170512
085000170512       //?Imposto ora standard in caso di errore sulle tabelle
085100170512       hhmm = '0930';
085200170512
085300170512       //?Cerco per filiale/data
085400170512       key1 = %editc(fil:'X');
085500170512       key2 = %editc(data:'X');
085600170512       chain ('OLP':key1:key2) TNTBE01L;
085700170512       //?Non trovo per filiale/data cerco per filiale
085800170512       IF  not %found(TNTBE01L) or TBEatb <> *blanks;
085900170512         clear key2;
086000170512         chain ('OLP':key1:key2) TNTBE01L;
086100170512       ENDIF;
086200170512       //?Non trovo neanche per filiale cerco generico
086300170512       IF  not %found(TNTBE01L) or TBEatb <> *blanks;
086400170512         key1 = '000';
086500170512         chain ('OLP':key1:key2) TNTBE01L;
086600170512         ENDIF;
086700170512       //?Imposto ora limite trovata
086800170512       IF  %found(TNTBE01L) and TBEatb = *blanks and
086900170512           %subst(TBEuni:1:4) > *zeros;
087000170512         hhmm = %subst(TBEuni:1:4);
087100170512       ENDIF;
087200170512
087300170512       RETURN hhmm;
087400170512
087500170512     p GetOraLimitePicking...
087600170512     p                 E
