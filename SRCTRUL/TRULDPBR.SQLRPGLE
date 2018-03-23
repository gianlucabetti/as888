000100141119      //--------------------------------------------------------------
000200141120      //?Recupero Data per Prezzo Base + valori VMA.
000300141119      //--------------------------------------------------------------
000400141119
000500141119     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141119     h dftactgrp(*no) actgrp(*caller)
000700141119
000800141119      //---------------------------------------------------------------
000900141119      //?Dichiarazione file.
001000141119      //---------------------------------------------------------------
001100141119
001200141119      // - Data Prezzo Base
001300141120     fTIDPB01L  if   e           k disk
001400141229
001500141229      // - Data Prezzo Medio Gasolio
001600141229     fTIPMG01L  if   e           k disk
001700141119
001800141119      //---------------------------------------------------------------
001900141119      //?Definizione costanti.
002000141119      //---------------------------------------------------------------
002100141119
002200141119      //---------------------------------------------------------------
002300141119      //?Definizione schiere.
002400141119      //---------------------------------------------------------------
002500141119
002600141119      // - Messaggi di errore
002700141119     d Msg             s             78    dim(20) ctdata perrcd(1)
002800141119
002900141119      //---------------------------------------------------------------
003000141119      //?Definizione aree dati.
003100141119      //---------------------------------------------------------------
003200141119
003300141119      // - Dati utente
003400141119     d §AzUte        e ds                  extname(AZUTE00F)
003500141119     d                                     dtaara
003600141119     d §DatiUte      e ds                  extname(dDatiUte)
003700141119     d                                     dtaara
003800141119
003900141119      //---------------------------------------------------------------
004000141119      //?Definizione strutture dati.
004100141119      //---------------------------------------------------------------
004200141119
004300141119      // - Parametri ricevuti
004400141119     d KPJBA         e ds
004500141119     d TRULDPBDS     e ds
004600141120
004700141120      // - Ricerca/Controllo tabelle
004800141120     d TIBS02DS      e ds                  inz
004900141119
005000141119      // - File Prezzo medio Gasolio
005100141119     d TIPMG00F      e ds
005200141119
005300141119      //---------------------------------------------------------------
005400141119      //?Definizione variabili globali.
005500141119      //---------------------------------------------------------------
005600141119
005700141119      // - Flags booleani
005800141119     d wFine           s               n   inz(*off)
005900141119
006000141119      // - Campi di comodo
006100141120     d kDPBdtd         s                   like(DPBdtd) inz
006200141119     d Oggi            s              8  0 inz
006300141119
006400141119      //---------------------------------------------------------------
006500141119      //?Definizione procedure usate.
006600141119      //---------------------------------------------------------------
006700141119
006800141119      //---------------------------------------------------------------
006900141119      //?Definizione prototipi usati.
007000141119      //---------------------------------------------------------------
007100141120      /copy gaitrasrc/srcprotopr,TIBS02R
007200141119
007300141119      //---------------------------------------------------------------
007400141119      //?Definizione key-list.
007500141119      //---------------------------------------------------------------
007600141119
007700141119      //---------------------------------------------------------------
007800141119      //?M A I N - L I N E
007900141119      //---------------------------------------------------------------
008000141119
008100141119     c     *entry        PLIST
008200141119     c                   PARM                    kpjba
008300141119     c                   PARM                    TRULDPBDS
008400141119
008500141119      /free
008600141119
008700141119       //?Operazioni iniziali
008800141119       exsr RoutInz;
008900141119
009000141119       //?Elabora
009100141119       exsr Elabora;
009200141119
009300141119       //?Operazioni finali
009400141119       exsr RoutEnd;
009500141119
009600141119       //--------------------------------------------------------------
009700141119       //?Operazioni iniziali.
009800141119       //--------------------------------------------------------------
009900141119       BEGSR RoutInz;
010000141119
010100141119         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010200141119
010300141120       //?Pulisco i dati di ritorno
010400141120         clear ODPBsca;
010500141120         clear ODPBdpb;
010600141120         clear ODPBpma;
010700141120         clear ODPBvma;
010800141120         clear ODPBsvm;
010900141119         clear ODPBerr;
011000141119         clear ODPBmsg;
011100141119
011200141124       //?Se non ho lo scaglione del prezzo medio del gasolio torno errore
011300141119         IF  IDPBsca = 0;
011400141120           ODPBerr = 'E';
011500141119           ODPBmsg = Msg(01);
011600141119           exsr RoutEnd;
011700141119         ENDIF;
011800141120
011900141120       //?Deve essere uno scaglione valido
012000141120         clear TIBS02DS;
012100141120         T02mod = 'C';
012200141120         T02cod = 'IPG';
012300141120         T02ke1 = %editc(IDPBsca:'X');
012400141120         TNTBE_RicercaControllo  (kpjba : tibs02ds);
012500141120         IF  T02err <> *blanks;
012600141120           ODPBerr = 'E';
012700141120           ODPBmsg = Msg(01);
012800141120           exsr RoutEnd;
012900141120         ENDIF;
013000141120
013100141120       //?Data di oggi
013200141120         Oggi = %dec(%date());
013300141119
013400141119       ENDSR;
013500141119
013600141119       //--------------------------------------------------------------
013700141119       //?Elabora.
013800141119       //--------------------------------------------------------------
013900141119       BEGSR Elabora;
014000141119
014100141120       //?Cerco l'ultimo prezzo base
014200141120         kDPBdtd = Oggi;
014300141120         setll (kDPBdtd) TIDPB01L;
014400141120         read  TIDPB01L;
014500141119         IF  not %eof(TIDPB01L);
014600150114           exsr Calcola_Scaglione;
014700141229         //?Con la data dell'ultimo prezzo cerco lo scaglione del prezzo medio del gasolio
014800141229           chain  (DPBdpb) TIPMG01L;
014900141229         //?se non trovo il prezzo medio del gasolio errore
015000141229           IF  not %found (TIPMG01L);
015100141229             ODPBerr = 'E';
015200141229             ODPBmsg = Msg(03);
015300141229             exsr RoutEnd;
015400141229           ENDIF;
015500141229         //?se lo scaglione trovato è uguale al calcolato vado via
015600141229           IF  ODPBsca = PMGsca;
015700141120             ODPBerr = 'E';
015800141120             ODPBmsg = Msg(02);
015900141120             exsr RoutEnd;
016000141119             leavesr;
016100141119           ENDIF;
016200141120         //?dati del VMA da tornare al chiamante
016300141120           ODPBpma = DPBpma;
016400141120           ODPBvma = DPBvma;
016500141120           ODPBsvm = DPBsvm;
016600141119         ENDIF;
016700141119
016800141120       //?Cerco il prezzo più alto, e la relativa data, dello scaglione calcolato
016900141119         exec sql
017000141119         WITH   A1 as ( select PMGsca, PMGpmg, PMGdtd
017100141119                          from TIPMG00F
017200141120                          where PMGsca = :ODPBsca
017300141119                         order by PMGsca, PMGpmg desc, PMGdtd desc ),
017400141119                A2 as ( select PMGsca, max(PMGpmg) as PMGpmg
017500141119                          from A1
017600141119                         group by PMGsca
017700141119                         order by PMGsca )
017800141120         SELECT distinct A2.*, max(A1.PMGdtd)
017900141120           into :PMGsca, :PMGpmg, :PMGdtd
018000141119           from A2 inner join A1
018100141119             on A2.PMGsca = A1.PMGsca
018200141119            and A2.PMGpmg = A1.PMGpmg
018300141119          group by A2.PMGsca, A2.PMGpmg
018400141119          order by A2.PMGsca;
018500141120
018600141120         IF  sqlcod <> 0;
018700141124           ODPBerr = 'E';
018800141124           ODPBmsg = Msg(03);
018900141120           clear ODPBpma;
019000141120           clear ODPBvma;
019100141120           clear ODPBsvm;
019200141124           exsr RoutEnd;
019300141120           leavesr;
019400141120         ENDIF;
019500141120
019600141120       //?Se arrivo qua vuol dire che tutto OK quindi torno la data che ho trovato
019700141120         ODPBdpb = PMGdtd;
019800141119
019900141119       ENDSR;
020000150114
020100150114       //--------------------------------------------------------------
020200150114       //?Calcolo scaglione del prezzo base.
020300150114       //--------------------------------------------------------------
020400150114       BEGSR Calcola_Scaglione;
020500150114
020600150114       //?Calcolo lo scaglione che deve avere il prezzo base
020700150114       //?deve essere inferiore di 12 scaglioni rispetto allo
020800150114       //?scaglione più alto tra quello ricevuto (prezzo medio) e
020900150114       //?lo scaglione del VMA dell'ultimo prezzo base inserito
021000150114         IF  IDPBsca >= DPBsvm;
021100150114           ODPBsca = IDPBsca - 12;
021200150114         ELSE;
021300150114           ODPBsca = DPBsvm - 12;
021400150114         ENDIF;
021500150114
021600150114       //?Lo scaglione che calcolo NON deve essere negativo
021700150114         IF  ODPBsca < 0;
021800150114           ODPBerr = 'E';
021900150114           ODPBmsg = Msg(01);
022000150114           exsr RoutEnd;
022100150114         ENDIF;
022200150114       //?Lo scaglione che calcolo deve esistere
022300150114         clear TIBS02DS;
022400150114         T02mod = 'C';
022500150114         T02cod = 'IPG';
022600150114         T02ke1 = %editc(ODPBsca:'X');
022700150114         TNTBE_RicercaControllo  (kpjba : tibs02ds);
022800150114         IF  T02err <> *blanks;
022900150114           ODPBerr = 'E';
023000150114           ODPBmsg = Msg(01);
023100150114           exsr RoutEnd;
023200150114         ENDIF;
023300150114
023400150114       ENDSR;
023500141119
023600141119       //--------------------------------------------------------------
023700141119       //?Operazioni finali.
023800141119       //--------------------------------------------------------------
023900141119       BEGSR RoutEnd;
024000141119
024100141119         *inLR = *on;
024200141119         return;
024300141119
024400141119       ENDSR;
024500141119
024600141119      /end-free
024700141119
024800141119       //--------------------------------------------------------------
024900141119       //?Schiere a tempo di compilazione.
025000141119       //--------------------------------------------------------------
025100141119
025200141120** - MSG --------------------------------------------------------------------*
025300141124Scaglione non presente in tabella. AVVISARE CED!!!                             1
025400141124Prezzo base uguale al precedente. Non creato record.                           2
025500141124Non rilevato nessun prezzo medio con lo scaglione calcolato. AVVISARE CED!!!   3
