000100160622       //==============================================================
000200160622       //
000300170530       //?FIORA01R - ORM: Scrittura/Aggiornamento
000400160622       //
000500160622       //==============================================================
000600160622     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS':'UBBNDDIR':'TIO7')
000700160622
000800160622       //--------------------------------------------------------------
000900160622       //?Dichiarazione file.                                          ?
001000160622       //--------------------------------------------------------------
001100160622       // -?File ORM
001200170331     fFNORM00F  o    e             disk
001300170331     fFNORG00F  o    e             disk
001400170331     fFNORF00F  o    e             disk
001500170331     fFNORE00F  o    e             disk
001600170331     fFISAO00F  o    e             disk
001700160622
001800160622       // -?File Tabelle
001900160622     fTABEL00F  if   e           k disk
002000170331     fTNTBE01L  if   e           k disk
002100160622
002200160622       //--------------------------------------------------------------
002300160622       //?Definizione costanti.                                        ?
002400160622       //--------------------------------------------------------------
002500160622      /copy gaitrasrc/srcconst,FIORA00R
002600160622
002700160622       //--------------------------------------------------------------
002800160622       //?Definizione strutture dati.                                  ?
002900160622       //--------------------------------------------------------------
003000160622       // -?Dati INPUT
003100170331     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
003200170420     d FIORA01I      e ds                  QUALIFIED INZ(*EXTDFT)
003300170420
003400170420       // -?Dati OUTPUT
003500170420     d FIORA01O      e ds                  QUALIFIED INZ(*EXTDFT)
003600160622
003700160622       // -?Campo ACRMAI
003800160622     d dACR01        e ds                  QUALIFIED INZ
003900160622
004000160622       // -?Campo VAOFLO
004100160622     d dORM01        e ds                  QUALIFIED INZ
004200170331
004300170331       // -?Campo ORFFLO
004400170331     d dORF01        e ds
004500160622
004600160622       // -?Kpjba
004700160622     d KPJBA         e ds                  INZ
004800160622
004900160622       // -?Tabella FAR
005000160622     d dFAR          e ds                  QUALIFIED INZ
005100170420
005200170420      // -?ds per rcd 'DT' FNORE - Data Pronta Merce
005300170420     d dOREdt        e ds
005400170420
005500170420       // -?ds per rcd 'G' FNORE - generale
005600170420     d dOREgen       e ds
005700170420
005800170420       // -?ds per rcd 'OR' FNORE - orari indicativi ritiro
005900170420     d dOREor        e ds
006000170331
006100170331       // -?ds per rcd 'O' FNORE - orari apertura
006200170331     d dOREorari     e ds
006300170331
006400170331       // -?ds per rcd 'S' FNORE - SMS
006500170331     d dOREsms       e ds
006600170420
006700170420       // -?Gestione note
006800170420     d FIOR06DS      e ds                  QUALIFIED INZ
006900170331
007000170331       // -?ds per Invio mail di preavviso
007100170331     d FIOR52DS      e ds
007200170331
007300170331       // -?ds per Ritorno dati servizio clienti
007400170331     d TRULDSCDS     e ds
007500170526
007600170526       // -?ds per calcolo differenza gg lav. tra due date
007700170526     d wdat8           ds
007800170526     d  data1                  1      8  0
007900170526     d  data2                  9     16  0
008000170526     d  giolav                17     21  0
008100170526     d tfptfa          ds
008200170526     d  p_tfp                  1      3  0
008300170526     d  p_tfa                  4      6  0
008400160622
008500160622       //--------------------------------------------------------------
008600160622       //?Definizione schiere.
008700160622       //--------------------------------------------------------------
008800160622
008900160622       //--------------------------------------------------------------
009000160622       //?Definizione campi.
009100160622       //--------------------------------------------------------------
009200160622       // -?Campi di comodo
009300160622     d dataoggi        s              8s 0 inz
009400170331     d k_TBEcod        s                   like(TBEcod)
009500170331     d k_TBEke1        s                   like(TBEke1)
009600170331     d k_TBEke2        s                   like(TBEke2)
009700160622     d k_TBLcod        s                   like(TBLcod)
009800160622     d k_TBLkey        s                   like(TBLkey)
009900160622     d k_TBLkut        s                   like(TBLkut)
010000170414     d parametroNPR    s             10a   inz
010100160622     d wNrORM          s                   inz like(ORMnor)
010200160622
010300160622       //--------------------------------------------------------------
010400160622       //?Definizione procedure.
010500160622       //--------------------------------------------------------------
010600170420       // -?Pgm gestione note
010700170420     d FIOR06R         pr                  extpgm('FIOR06R')
010800170420     d  kpjba                              likeds(KPJBA)
010900170420     d  fior06ds                           likeds(FIOR06DS)
011000170420
011100170331       // -?Invio mail di preavviso
011200170331     d fior52r         pr                  extpgm('FIOR52R')
011300170331     d  kpjba                              likeds(KPJBA)
011400170331     d  fior52ds                           likeds(FIOR52DS)
011500170331
011600160622       // -?Recupero nuovo Numero Prenotazione Ritiro
011700160622     d GetNPR          pr            10
011800160622     d  ParametroNPR                 10
011900160622
012000160622       // -?Ritorna dati Servizio Clienti
012100160622     d truldscr        pr                  extpgm('TRULDSCR')
012200160622     d  truldscds                          likeds(TRULDSCDS)
012300160622
012400160622       //--------------------------------------------------------------
012500160622       //?Definizione prototipi.
012600160622       //--------------------------------------------------------------
012700160622      /copy gaitrasrc/srcprotopr,FIORA01R
012800170526      /copy gaitrasrc/srcprotopr,XSRLAV8
012900160622
013000160622       //--------------------------------------------------------------
013100160622       //?Definizione parametri programma.
013200160622       //--------------------------------------------------------------
013300160622     d Fiora01_Scrivi...
013400160622     d                 PI
013500160622     d prmRqsOpCode...
013600160622     d                               10I 0 CONST
013700160622     d prmRpyOpCode...
013800160622     d                               10I 0
013900160622     d prmRpyIdMsg...
014000160622     d                               10I 0
014100160622     d prmRqsFormato...
014200160622     d                               10A   CONST
014300160622     d prmRqsData...
014400160622     d                            32767A   OPTIONS(*VARSIZE)
014500160622     d prmRqsDataSize...
014600160622     d                               10I 0 CONST
014700160622     d prmRqsFormato1...
014800160622     d                               10A   CONST
014900160622     d prmRqsData1...
015000160622     d                            32767A   OPTIONS(*VARSIZE)
015100160622     d prmRqsData1Size...
015200160622     d                               10I 0 CONST
015300170420     d prmRpyFormato...
015400170420     d                               10A   CONST
015500170420     d prmRpyData...
015600170420     d                            32767A   OPTIONS(*VARSIZE)
015700170420     d prmRpyDataSize...
015800170420     d                               10I 0 CONST
015900170420     d prmKPJBA...
016000170420     d                              502A   CONST OPTIONS(*NOPASS)
016100160622
016200160622       //--------------------------------------------------------------
016300160622       //?MAIN.
016400160622       //--------------------------------------------------------------
016500160622
016600160622      /free
016700160622
016800160622       //?Operazioni iniziali
016900160622       exsr RoutInz;
017000160622
017100160622       //?Controllo formale dei dati passati
017200160622       exsr Controlla;
017300160622
017400160622       //?Scrivo
017500170414       IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
017600170331         exsr Scrivi;
017700170331       ENDIF;
017800160622
017900160622       //?Operazioni finali
018000160622       exsr RoutEnd;
018100160622
018200160622       //--------------------------------------------------------------
018300160622       //?Operazioni iniziali.                                         ?
018400160622       //--------------------------------------------------------------
018500160622       BEGSR  RoutInz;
018600160622
018700160622         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
018800160622
018900160622         prmRpyOpCode = FIORA00_RPYOPCODE_DONE;
019000160622         prmRpyIdMsg  = FIORA00_ESITO_OK;
019100160622
019200160622       //?Data del giorno
019300160622         dataoggi = %dec(%date());
019400160622
019500160622         *inLR = *on;
019600160622
019700160622       ENDSR;
019800160622
019900160622       //--------------------------------------------------------------
020000160622       //?Controllo formale dei dati passati.
020100160622       //--------------------------------------------------------------
020200160622       BEGSR  Controlla;
020300160622
020400160622       //?OpCode
020500160622         IF  prmRqsOpCode <> FIORA00_RQSOPCODE_WRITE;
020600160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
020700160622           prmRpyIdMsg  = FIORA00_ESITO_RQSOPCODE_SCONOSCIUTO;
020800160622           exsr RoutEnd;
020900160622         ENDIF;
021000160622
021100160622       //?Formato e size RQS
021200160622         IF  prmRqsFormato = fiora0I0.formato;
021300160622           fiora0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
021400160622         ELSE;
021500160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
021600160622           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
021700160622           exsr RoutEnd;
021800160622         ENDIF;
021900160622         IF  prmRqsDataSize > 0;
022000160622         ELSE;
022100160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
022200160622           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
022300160622           exsr RoutEnd;
022400160622         ENDIF;
022500160622
022600160622       //?Formato e size RQS 1
022700170420         IF  prmRqsFormato1 = fiora01I.formato;
022800170420           fiora01I = %SUBST(prmRqsData1:1:prmRqsData1Size);
022900160622         ELSE;
023000160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
023100160622           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
023200160622           exsr RoutEnd;
023300160622         ENDIF;
023400160622         IF  prmRqsData1Size > 0;
023500160622         ELSE;
023600160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
023700160622           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
023800160622           exsr RoutEnd;
023900160622         ENDIF;
024000170420
024100170420       //?Formato e size RPY
024200170420         IF  prmRpyFormato = fiora01O.formato;
024300170420         ELSE;
024400170420           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
024500170420           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
024600170420           exsr RoutEnd;
024700170420         ENDIF;
024800170420         IF  prmRpyDataSize > 0;
024900170420         ELSE;
025000170420           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
025100170420           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
025200170420           exsr RoutEnd;
025300170420         ENDIF;
025400160622
025500160622       ENDSR;
025600160622
025700160622       //--------------------------------------------------------------
025800160622       //?Routine principale per scrittura file ORM.
025900160622       //--------------------------------------------------------------
026000160622       BEGSR  Scrivi;
026100160622
026200160622       //?Come prima cosa recupero il n. ORM
026300160622         WnrORM = GetNumero(fiora0I0.VAOpoe);
026400160622         IF  wNrORM = 0;
026500160622           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
026600160622           prmRpyIdMsg  = FIORA00_ESITO_ERRORE_WRITE;
026700160622           exsr RoutEnd;
026800160622         ENDIF;
026900160622
027000160622       //?Se codice ordinante = codice mittente e imposato che paga ORDINANTE
027100160622       //?metto in automatico che maga MITTENTE
027200170331         IF  fiora0I0.VAOcor = fiora0I0.VAOcra and
027300160622             fiora0I0.VAOpag = 'O';
027400160622           fiora0I0.VAOpag = 'M';
027500160622         ENDIF;
027600160622
027700160622       //?Campo FLO
027800160622         dORM01 = fiora0I0.VAOflo;
027900160622
028000160622       //?Scrivo FNORM00F
028100160622         exsr ScriviORM;
028200170331
028300170331       //?Scrivo FNORG00F
028400170331         exsr ScriviORG;
028500170331
028600170331       //?Scrivo FNORF00F
028700170331         exsr ScriviORF;
028800170331
028900170331       //?Scrivo FNORE00F
029000170331         exsr ScriviORE;
029100170331
029200170331         write FNORM000;
029300170331         feod  FNORM00F;
029400170420
029500170420       //?dopo aver scritto l'ORM scrivo le note
029600170420         clear FIOR06DS;
029700170420         fior06ds.I06dta = ORMdao;
029800170420         fior06ds.I06poe = ORMpoe;
029900170420         fior06ds.I06nor = ORMnor;
030000170420         fior06ds.I06nsr = ORMnsr;
030100170420         fior06ds.I06nrv = ORMnrv;
030200170420         fior06ds.I06far = ORMfao;
030300170420         fior06ds.I06dai = %dec(%date());
030400170420         fior06ds.I06ori = %dec(%time());
030500170420         fior06ds.I06flm = 'C';
030600170420         kpjba = prmkpjba;
030700170420         fior06r (kpjba:FIOR06DS);
030800170331
030900170331       //?dopo aver scritto l'ORM se previsto il preavviso richiamo programma
031000170331       //?per invio mail di preavviso
031100170331        IF  dorm01.§ormpre = 'M';
031200170331         clear fior52ds;
031300170331         I52poe = ORMpoe;
031400170331         I52nsr = ORMnsr;
031500170331         I52nor = ORMnor;
031600170331         I52nrv = ORMnrv;
031700170331         fior52r (kpjba:fior52ds);
031800170331        ENDIF;
031900170331
032000170331       //?dopo aver scritto l'ORM se previsto Alert
032100170331       //?e/o previsto invio mail x conferma accettazione ORM
032200170331       //?scrivo file spia
032300170419        IF  fiora0I0.alertsms = 'S' or fiora0I0.alertmail = 'S';
032400170419          clear FISAO000;
032500170419          SAOdaori = %timestamp();
032600170419          SAOpoe = ORMpoe;
032700170419          SAOnsr = ORMnsr;
032800170419          SAOnor = ORMnor;
032900170419          SAOnrv = ORMnrv;
033000170419          SAOtipa = 'AVV';
033100170419          SAOsts = '0';
033200170419          write FISAO000;
033300170419        ENDIF;
033400170331
033500170427        IF  fiora01I.alertcmail = 'S' or fiora01I.alertcsms = 'S';
033600170419          clear FISAO000;
033700170419          SAOdaori = %timestamp();
033800170419          SAOpoe = ORMpoe;
033900170419          SAOnsr = ORMnsr;
034000170419          SAOnor = ORMnor;
034100170419          SAOnrv = ORMnrv;
034200170419          SAOtipa = 'CON';
034300170419          SAOsts = '0';
034400170419          write FISAO000;
034500170419        ENDIF;
034600170420
034700170420       //?Imposto nella ds di output il numero ORM appena scritto
034800170420         fiora01O.poe = ORMpoe;
034900170420         fiora01O.nsr = ORMnsr;
035000170420         fiora01O.nor = ORMnor;
035100170420         fiora01O.nrv = ORMnrv;
035200160622
035300160622       ENDSR;
035400160622
035500160622       //--------------------------------------------------------------
035600160622       //?Routine per scrittura file FNORM00F.
035700160622       //--------------------------------------------------------------
035800160622       BEGSR  ScriviORM;
035900160622
036000160622         clear FNORM000;
036100160622         ORMpoe = fiora0I0.VAOpoe;
036200170331         ORMnsr = 0;
036300170331         ORMnor = wNRorm;
036400170331         ORMnrv = 0;
036500160622         ORMtor = fiora0I0.tor;
036600160622         ORMtco = fiora0I0.tco;
036700160622         ORMdao = fiora0I0.VAOdao;
036800160622         ORMoao = fiora0I0.VAOoao;
036900160622         ORMcor = fiora0I0.VAOcor;
037000160622         ORMrso = fiora0I0.VAOrso;
037100160622         ORMino = fiora0I0.VAOino;
037200160622         ORMcao = fiora0I0.VAOcao;
037300160622         ORMloo = fiora0I0.VAOloo;
037400160622         ORMpro = fiora0I0.VAOpro;
037500160622         ORMnao = fiora0I0.VAOnao;
037600160622         ORMcra = fiora0I0.VAOcra;
037700160622         ORMrsr = fiora0I0.VAOrsr;
037800160622         ORMinr = fiora0I0.VAOinr;
037900160622         ORMcar = fiora0I0.VAOcar;
038000160622         ORMlor = fiora0I0.VAOlor;
038100160622         ORMprr = fiora0I0.VAOprr;
038200160622         ORMnar = fiora0I0.VAOnar;
038300160622         ORMrer = fiora0I0.VAOrer;
038400160622         ORMter = fiora0I0.VAOter;
038500160622         ORMcrc = fiora0I0.VAOcrc;
038600160622         ORMrsc = fiora0I0.VAOrsc;
038700160622         ORMinc = fiora0I0.VAOinc;
038800170522         ORMloc = fiora0I0.VAOloc;
038900170522         ORMcac = fiora0I0.VAOcac;
039000160622         ORMprc = fiora0I0.VAOprc;
039100160622         ORmnac = fiora0I0.VAOnac;
039200170414         ORMdar = fiora0I0.VAOdar;
039300170414         ORMorr = fiora0I0.VAOorr;
039400170426         ORMrmp = fiora01I.oraestesa;
039500170420         IF  fiora0I0.VAOffd = 'S';
039600170420           ORMffd = fiora0I0.VAOffd;
039700170420         ENDIF;
039800160622         ORMnam = fiora0I0.VAOnam;
039900160622         ORMncl = fiora0I0.VAOncl;
040000160622         ORMpkg = fiora0I0.VAOpkg;
040100160622         ORMvlm = fiora0I0.VAOvlm;
040200160622         ORMbnc = fiora0I0.VAObnc;
040300160622         ORMblc = fiora0I0.VAOblc;
040400160622         ORMatt = fiora0I0.VAOatt;
040500160622         ORMmtc = fiora0I0.VAOmtc;
040600170420         ORMddt = fiora01I.ddt;
040700160622         ORMpag = fiora0I0.VAOpag;
040800160622         ORMksc = fiora0I0.VAOksc;
040900160622         ORMctr = fiora0I0.VAOctr;
041000160622         ORMpor = fiora0I0.VAOpor;
041100160622         ORMpoc = fiora0I0.VAOpoc;
041200160622         ORMsto = fiora0I0.VAOsto;
041300160622         ORMno1 = fiora0I0.VAOno1;
041400160622         ORMno2 = fiora0I0.VAOno2;
041500160622         ORMrfa = fiora0I0.VAOrfa;
041600160622         ORMspi = fiora0I0.VAOspi;
041700160622
041800160622         SELECT;
041900160622       //?Se ORM export DPD scrivo la fase 100
042000160622         WHEN  fiora0I0.SceltaNTW = FIORA00_SERVIZIO_DPD;
042100160622           ORMfao = 100;
042200160622       //?Se ORM commissionato scrivo la fase 050
042300160622         WHEN  fiora0I0.contattot = 'S';
042400160622           ORMfao = 050;
042500160622         OTHER;
042600160622           ORMfao = 100;
042700160622         ENDSL;
042800160622         ORMdfo = dataoggi;
042900160622         ORMofo = %dec(%time());
043000160622
043100160622       //?Imposto flag ORM assegnabile in base alla fase ORM
043200170331       //?Reperimento tabella FAR
043300170331         clear dFAR;
043400170331         k_TBEcod = 'FAR';
043500170331         k_TBEke1 = %editc(ORMfao:'X');
043600170331         chain (k_TBEcod:k_TBEke1) TNTBE01L;
043700170331         IF  %found(TNTBE01L);
043800170331           dFAR = TBEuni;
043900170331         ENDIF;
044000170331         ORMeti = dFAR.d§FARass;
044100160622
044200160622       //?Se devo fare l'Alert per ORM Commissionato
044300160622         IF  fiora0I0.alertsms = 'S' or fiora0I0.alertmail = 'S';
044400160622         //?Se ora ritiro a 0 imposto inizio servizio o l'ora "tappo"
044500170420           IF  fiora0I0.VAOorr = 0 and fiora01I.orainizio > 0;
044600170420             ORMorr = fiora01I.orainizio;
044700160622           ENDIF;
044800160622           IF  fiora0I0.VAOorr = 0;
044900170420             ORMorr = fiora01I.oratappo;
045000160622           ENDIF;
045100160622         //?Se telefono non impostato metto il cell.x sms
045200160622           IF  fiora0I0.VAOter = *blanks;
045300160622             ORMter = fiora0I0.sms;
045400160622           ENDIF;
045500160622         ENDIF;
045600160622
045700170420         ORMstp = fiora01I.oracutoff;
045800160622         ORMdtt = dataoggi;
045900160622
046000160622       //?Se Blocco del destinatario
046100160622         IF  dORM01.§ormfd = 'S';
046200160622         //?pulisco il flag se ORM non Singolo
046300160622           IF  ORMtor <> 'S';
046400160622             clear dORM01.§ormfd;
046500160622           ENDIF;
046600160622         //?pulisco il flag se non c'è il destinatario
046700160622           IF  ORMrsc = *blanks and ORMcrc = 0;
046800160622             clear dORM01.§ormfd;
046900160622           ENDIF;
047000160622         ENDIF;
047100160622
047200160622       //?Se il flag di ORM commissionato è vuoto imposto 'N'
047300160622       //?gli unici ORM con il campo a blank sono gli ORM con serie (RC) scritti
047400160622       //?automaticamente dal traduttore
047500160622       //?e gli ORM commissionati da DPD chiusi in automatico da DPD
047600160622         dORM01.§orcom = fiora0I0.contattot;
047700170331         IF  dORM01.§orcom = *blanks;
047800170331           dORM01.§orcom = 'N';
047900160622         ENDIF;
048000160622
048100160622       //?Imposto il campo §orpfb in base al tipo ORM e a chi paga
048200160622       //?--> 'F' se paga mittente e ORM singolo/prepagato
048300160622       //?--> 'A' se non paga mittente
048400160622         IF  ORMpag <> 'M';
048500160622           dORM01.§orpfb = 'A';
048600160622         ELSE;
048700160622           IF  ORMtor = 'P' or ORMtor = 'S';
048800160622             dORM01.§orpfb = 'F';
048900160622           ENDIF;
049000160622         ENDIF;
049100160622
049200160622       //?Imposto §ormks
049300160622       //?Se paga ordinante e KSC impostato
049400160622         IF  dORM01.§ormks = *blanks and
049500160622             ORMpag = 'O' and ORMksc > 0;
049600170331           dORM01.§ormks = GetDatiVAC(ORMksc);
049700160622         ENDIF;
049800160622
049900160622         ORMflo = dORM01;
050000160622
050100160622       ENDSR;
050200170331
050300170331       //--------------------------------------------------------------
050400170331       //?Routine per scrittura file FNORG00F.
050500170331       //--------------------------------------------------------------
050600170331       BEGSR  ScriviORG;
050700170331
050800170331         clear FNORG000;
050900170331
051000170331         ORGpoe = ORMpoe;
051100170331         ORGnsr = ORMnsr;
051200170331         ORGnor = ORMnor;
051300170331         ORGnrv = ORMnrv;
051400170331         ORGpor = ORMpor;
051500170331
051600170331       //?Memorizzo peso e volume esplosi
051700170414         ORGpkg = fiora0I0.peso;
051800170414         ORGvlm = fiora0I0.volume;
051900170331
052000170331       //?Memorizzo il giro
052100170331         IF  fiora0I0.cgi <> *blanks;
052200170331           ORGcgi = fiora0I0.cgi;
052300170419           ORGtgi = fiora0I0.tgi;
052400170331           ORGpocgi = ORMpor;
052500170331           ORGdtvtel = %dec(%date());
052600170427           ORGhvtel = %dec(%time());
052700170331         ENDIF;
052800170331
052900170331         write FNORG000;
053000170331
053100170331       ENDSR;
053200170331
053300170331       //--------------------------------------------------------------
053400170331       //?Routine per scrittura file FNORF00F.
053500170331       //--------------------------------------------------------------
053600170331       BEGSR  ScriviORF;
053700170331
053800170331         clear dORF01;
053900170331         clear FNORF000;
054000170331
054100170331         ORFpoe = ORMpoe;
054200170331         ORFnsr = ORMnsr;
054300170331         ORFnor = ORMnor;
054400170331         ORFnrv = ORMnrv;
054500170331
054600170331       //?Filiale Esecuzione Fase
054700170331       //?Se richiamo da AS imposto la filiale utente
054800170331         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
054900170420           ORFpog = fiora01I.filute;
055000170331         ENDIF;
055100170331       //?Se richiamo da INTERNET imposto la filiale emissione
055200170331         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
055300170331           ORFpog = fiora0I0.VAOpoe;
055400170331         ENDIF;
055500170331
055600170418         ORFdae = ORMdfo;
055700170418         ORFore = ORMofo;
055800170331         ORFfar = ORMfao;
055900170331
056000170331       //?Utente Esecuzione Fase
056100170331       //?Se richiamo da AS imposto l'utente
056200170331         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
056300170420           ORFpue = fiora01I.utente;
056400170331         ENDIF;
056500170331       //?Se richiamo da INTERNET imposto 'INTERNET'
056600170331         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
056700170331           ORFpue = 'INTERNET';
056800170331         ENDIF;
056900170331
057000170331         ORFflo = dORF01;
057100170331         write FNORF000;
057200170331
057300170331       ENDSR;
057400170331
057500170331       //--------------------------------------------------------------
057600170331       //?Routine per scrittura file FNORE00F.
057700170331       //--------------------------------------------------------------
057800170331       BEGSR  ScriviORE;
057900170331
058000170421       //?Preavviso MAIL - CEVA (solo da ORM via File)
058100170414       //?====>>>>>
058200170331         IF  dorm01.§ormpre = 'M';
058300170529           clear fnore000;
058400170529           OREpoe = ORMpoe;
058500170529           OREnsr = ORMnsr;
058600170529           OREnor = ORMnor;
058700170529           OREnrv = ORMnrv;
058800170529           OREtrc = 'M ';
058900170529           //OREdati = vaoedati;
059000170529           //write fnore000;
059100170331         ENDIF;
059200170331
059300170331       //?Rcd 'O ' - Orari apertura
059400170331         IF  fiora0I0.dalleam > 0 or fiora0I0.alleam > 0 or
059500170331             fiora0I0.dallepm > 0 or fiora0i0.allepm > 0;
059600170331           clear dOREorari;
059700170331           clear fnore000;
059800170331           OREpoe = ORMpoe;
059900170331           OREnsr = ORMnsr;
060000170331           OREnor = ORMnor;
060100170331           OREnrv = ORMnrv;
060200170331           OREtrc = 'O ';
060300170331           §OREoramda = fiora0I0.dalleam;
060400170331           §OREorama  = fiora0I0.alleam;
060500170331           §OREorapda = fiora0I0.dallepm;
060600170331           §OREorapa  = fiora0I0.allepm;
060700170331           OREdati = dOREorari;
060800170331           write fnore000;
060900170331         ENDIF;
061000170331
061100170331       //?Rcd 'MA' - Mail x ALERT
061200170420         IF  fiora0I0.mail <> *blanks;
061300170331           clear fnore000;
061400170418           OREtrc = 'MA';
061500170331           OREpoe = ORMpoe;
061600170331           OREnsr = ORMnsr;
061700170331           OREnor = ORMnor;
061800170331           OREnrv = ORMnrv;
061900170331           OREdati = fiora0I0.mail;
062000170331           write fnore000;
062100170331         ENDIF;
062200170331
062300170331       //?Rcd 'S ' - SMS x ALERT
062400170420         IF  fiora0I0.sms <> *blanks;
062500170331           clear dOREsms;
062600170331           clear fnore000;
062700170418           OREtrc = 'S ';
062800170331           OREpoe = ORMpoe;
062900170331           OREnsr = ORMnsr;
063000170331           OREnor = ORMnor;
063100170331           OREnrv = ORMnrv;
063200170331           §OREsms = fiora0I0.sms;
063300170331           OREdati = dOREsms;
063400170331           write fnore000;
063500170331         ENDIF;
063600170331
063700170331       //?Rcd 'MC' - Mail x Conferma Prenotazione Ritiro
063800170420         IF  fiora0I0.mailconf <> *blanks;
063900170331           clear fnore000;
064000170418           OREtrc = 'MC';
064100170331           OREpoe = ORMpoe;
064200170331           OREnsr = ORMnsr;
064300170331           OREnor = ORMnor;
064400170331           OREnrv = ORMnrv;
064500170331           OREdati = fiora0I0.mailconf;
064600170331           write fnore000;
064700170331         ENDIF;
064800170331
064900170331       //?Rcd 'SC' - SMS x Conferma Prenotazione Ritiro
065000170331         IF  fiora0I0.smsconf <> *blanks;
065100170331           clear fnore000;
065200170418           OREtrc = 'SC';
065300170331           OREpoe = ORMpoe;
065400170331           OREnsr = ORMnsr;
065500170331           OREnor = ORMnor;
065600170331           OREnrv = ORMnrv;
065700170331           OREdati = fiora0I0.smsconf;
065800170331           write fnore000;
065900170331         ENDIF;
066000170331
066100170331       //?Rcd 'OR' - Orari indicativi di ritiro
066200170331       //?Se ORM con Conferma Prenotazione Ritiro
066300170331       //?devo memorizzare su FNORE rcd OR i dati dell'ora indicativa di ritiro
066400170331       //?servono nell'invio dell'alert
066500170331       //?ma solo se devo creare l'alert conferma prenotazione ritiro
066600170427         IF  fiora01I.alertcmail = 'S' or fiora01I.alertcsms = 'S';
066700170331           clear dOREor;
066800170331           clear FNORE000;
066900170418           OREtrc = 'OR';
067000170331           OREpoe = ORMpoe;
067100170331           OREnsr = ORMnsr;
067200170331           OREnor = ORMnor;
067300170331           OREnrv = ORMnrv;
067400170426           §OREori = fiora01I.orainizio;
067500170426           §OREorf = fiora01I.orafine;
067600170331           OREdati = dOREor;
067700170331           write FNORE000;
067800170331         ENDIF;
067900170420
068000170420       //?Rcd 'DT' - Data Pronta Merce
068100170420         clear dOREdt;
068200170420         clear FNORE000;
068300170420         OREtrc = 'DT';
068400170420         OREpoe = ORMpoe;
068500170420         OREnsr = ORMnsr;
068600170420         OREnor = ORMnor;
068700170420         OREnrv = ORMnrv;
068800170420         §OREdpm = fiora0I0.datapm;
068900170526         §OREdar = %editc(fiora01I.dataritiro:'X');
069000170526         §OREposd = fiora01I.posticipo;
069100170526         IF  fiora01I.dataritmin < fiora01I.dataritiro;
069200170526           §OREant = 'S';
069300170526         ENDIF;
069400170526         IF  fiora0I0.vaodar < fiora01I.dataritiro and §OREant = 'S';
069500170526           §OREmod = 'S';
069600170526           clear wdat8;
069700170526           clear tfptfa;
069800170526           p_tfa = fiora0I0.VAOpor;
069900170526           data1 = fiora01I.dataritiro;
070000170526           data2 = fiora0I0.VAOdar;
070100170526           xsrlav8(wdat8);
070200170526         //?dal pgm di calcolo gg lav.tra le 2 date
070300170526         //?torna sempre almeno 1, io lo tolgo
070400170526           §OREgga = %editc((giolav -1):'X');
070500170526         ENDIF;
070600170420         OREdati = dOREdt;
070700170420         write FNORE000;
070800170331
070900170331       //?Rcd 'NP' - Numero Prenotazione Ritiro
071000170414         clear FNORE000;
071100170418         OREtrc = 'NP';
071200170414         OREpoe = ORMpoe;
071300170414         OREnsr = ORMnsr;
071400170414         OREnor = ORMnor;
071500170414         OREnrv = ORMnrv;
071600170414       //?recupero il NUmero Prenotazione Ritiro
071700170414         OREdati = GetNPR(parametroNPR);
071800170414         write FNORE000;
071900170414       //?Se il rif. ORM è vuoto imposto il NPR
072000170414         IF  ORMrfa = *blanks;
072100170414           ORMrfa = OREdati;
072200170414         ENDIF;
072300170331
072400170331       //?Rcd 'G ' - Generale
072500170331       //?Se ho dei flag impostati per gli alert devo memorizzare rcd 'G'
072600170414         IF  fiora0I0.alertmail <> *blanks or
072700170414             fiora0I0.alertsms <> *blanks or
072800170427             fiora01I.alertcmail <> *blanks or
072900170427             fiora01I.alertcsms <> *blanks;
073000170414           clear dOREgen;
073100170414           clear fnore000;
073200170418           OREtrc = 'G ';
073300170414           OREpoe = ORMpoe;
073400170414           OREnsr = ORMnsr;
073500170414           OREnor = ORMnor;
073600170414           OREnrv = ORMnrv;
073700170414           §OREfimo = fiora0I0.alertmail;
073800170414           §OREfiso = fiora0I0.alertsms;
073900170427           §OREfmco = fiora01I.alertcmail;
074000170427           §OREfsco = fiora01I.alertcsms;
074100170414           OREdati = dOREgen;
074200170414           write fnore000;
074300170414         ENDIF;
074400170331
074500170414         feod FNORE00F;
074600170331
074700170331       ENDSR;
074800170331
074900170331       //--------------------------------------------------------------
075000170331       //?Operazioni finali.
075100170331       //--------------------------------------------------------------
075200170331       BEGSR  RoutEnd;
075300170420
075400170420         %subst(prmRpyData:1:prmRpyDataSize) = fiora01O;
075500170331
075600170331         return;
075700170331
075800170331       ENDSR;
075900160622
076000160622      /end-free
076100160622
076200160622      *--------------------------------------------------
076300160622      * Procedure name: GetNumero
076400160622      * Purpose:        Recupera il Numero ORM
076500160622      * Returns:        Numero ORM
076600160622      * Parameter:      Filiale Emissione
076700160622      *--------------------------------------------------
076800160622     p GetNumero       B
076900160622     d GetNumero       PI             7s 0
077000160622     d  filiale                       3s 0 CONST
077100160622
077200160622       // -?Stacca Numero
077300160622     d TRUL33DS      e ds                  QUALIFIED INZ(*EXTDFT)
077400160622
077500160622       // -?Stacca Numero
077600160622     d TRUL33R         pr                  extpgm('TRUL33R')
077700160622     d  kpjba                              likeds(KPJBA)
077800160622
077900160622      /free
078000160622       IF  filiale = *zeros;
078100160622         RETURN *zeros;
078200160622       ENDIF;
078300160622
078400160622       clear TRUL33DS;
078500160622       trul33ds.I33cnu = 300;
078600160622       trul33ds.I33po1 = filiale;
078700160622       trul33ds.I33num = 1;
078800160622       kpjbu = trul33ds;
078900160622       trul33r (kpjba);
079000160622       trul33ds = kpjbu;
079100160622
079200160622       IF  trul33ds.O33err <> *zeros;
079300160622         RETURN *zeros;
079400160622       ENDIF;
079500160622
079600170331       RETURN trul33ds.O33nrf;
079700160622
079800160622      /end-free
079900160622     P GetNumero       E
080000160622
080100160622      *--------------------------------------------------
080200160622      * Procedure name: GetDatiVAC
080300160622      * Purpose:        Recupera il flag VTO
080400160622      * Returns:        flag VTO
080500160622      * Parameter:      Cliente che paga
080600160622      *--------------------------------------------------
080700160622     p GetDatiVAC      B
080800160622     d GetDatiVAC      PI             1a
080900160622     d  cliente                       7s 0 CONST
081000160622
081100160622       // -?Tabella 3K
081200160622     d ds3K          e ds                  QUALIFIED INZ
081300160622       // -?Tabella 3Q
081400160622     d ds3Q          e ds                  QUALIFIED INZ
081500160622
081600160622      /free
081700160622       IF  cliente = *zeros;
081800160622         RETURN *blanks;
081900160622       ENDIF;
082000160622
082100160622       //?Prima provo con la tabella 3K
082200160622       clear ds3K;
082300160622       k_TBLkut = 1;
082400160622       k_TBLcod = '3K';
082500160622       k_TBLkey = %editc(ORMksc:'X');
082600160622       chain (k_TBLkut:k_TBLcod:k_TBLkey) TABEL00F;
082700160622       IF  %found(TABEL00F) and TBLflg = *blanks;
082800160622         ds3K = TBLuni;
082900160622         IF  ds3K.§3Kvto = 'S';
083000160622           RETURN  ds3K.§3Kvto;
083100160622         ENDIF;
083200160622       ENDIF;
083300160622
083400160622       //?Se arrivo qua provo la tabella 3Q
083500160622       clear ds3Q;
083600160622       k_TBLkut = 1;
083700160622       k_TBLcod = '3K';
083800160622       k_TBLkey = %editc(ORMksc:'X');
083900160622       chain (k_TBLkut:k_TBLcod:k_TBLkey) TABEL00F;
084000160622       IF  %found(TABEL00F) and TBLflg = *blanks;
084100160622         ds3Q = TBLuni;
084200160622         IF  ds3Q.§3Qvto = 'S';
084300160622           RETURN  ds3Q.§3Qvto;
084400160622         ENDIF;
084500160622       ENDIF;
084600170505
084700170505       //?Se arrivo qua non ho trovato niente quindi torno blanks
084800170505       RETURN *blanks;
084900160622
085000160622      /end-free
085100160622     P GetDatiVAC      E
