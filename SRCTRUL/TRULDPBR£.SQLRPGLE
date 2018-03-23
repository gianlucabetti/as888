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
014100141120       //?Calcolo lo scaglione che deve avere il prezzo base
014200141120       //?deve essere inferiore a 6 scaglioni
014300141120         ODPBsca = IDPBsca - 6;
014400141121
014500141121       //?Lo scaglione che calcolo NON deve essere negativo
014600141121         IF  ODPBsca < 0;
014700141121           ODPBerr = 'E';
014800141121           ODPBmsg = Msg(01);
014900141121           exsr RoutEnd;
015000141121         ENDIF;
015100141121       //?Lo scaglione che calcolo deve esistere
015200141121         clear TIBS02DS;
015300141121         T02mod = 'C';
015400141121         T02cod = 'IPG';
015500141121         T02ke1 = %editc(ODPBsca:'X');
015600141121         TNTBE_RicercaControllo  (kpjba : tibs02ds);
015700141121         IF  T02err <> *blanks;
015800141121           ODPBerr = 'E';
015900141121           ODPBmsg = Msg(01);
016000141121           exsr RoutEnd;
016100141121         ENDIF;
016200141119
016300141120       //?Cerco l'ultimo prezzo base
016400141120         kDPBdtd = Oggi;
016500141120         setll (kDPBdtd) TIDPB01L;
016600141120         read  TIDPB01L;
016700141119         IF  not %eof(TIDPB01L);
016800141229         //?Con la data dell'ultimo prezzo cerco lo scaglione del prezzo medio del gasolio
016900141229           chain  (DPBdpb) TIPMG01L;
017000141229         //?se non trovo il prezzo medio del gasolio errore
017100141229           IF  not %found (TIPMG01L);
017200141229             ODPBerr = 'E';
017300141229             ODPBmsg = Msg(03);
017400141229             exsr RoutEnd;
017500141229           ENDIF;
017600141229         //?se lo scaglione trovato è uguale al calcolato vado via
017700141229           IF  ODPBsca = PMGsca;
017800141120             ODPBerr = 'E';
017900141120             ODPBmsg = Msg(02);
018000141120             exsr RoutEnd;
018100141119             leavesr;
018200141119           ENDIF;
018300141120         //?dati del VMA da tornare al chiamante
018400141120           ODPBpma = DPBpma;
018500141120           ODPBvma = DPBvma;
018600141120           ODPBsvm = DPBsvm;
018700141119         ENDIF;
018800141119
018900141120       //?Cerco il prezzo più alto, e la relativa data, dello scaglione calcolato
019000141119         exec sql
019100141119         WITH   A1 as ( select PMGsca, PMGpmg, PMGdtd
019200141119                          from TIPMG00F
019300141120                          where PMGsca = :ODPBsca
019400141119                         order by PMGsca, PMGpmg desc, PMGdtd desc ),
019500141119                A2 as ( select PMGsca, max(PMGpmg) as PMGpmg
019600141119                          from A1
019700141119                         group by PMGsca
019800141119                         order by PMGsca )
019900141120         SELECT distinct A2.*, max(A1.PMGdtd)
020000141120           into :PMGsca, :PMGpmg, :PMGdtd
020100141119           from A2 inner join A1
020200141119             on A2.PMGsca = A1.PMGsca
020300141119            and A2.PMGpmg = A1.PMGpmg
020400141119          group by A2.PMGsca, A2.PMGpmg
020500141119          order by A2.PMGsca;
020600141120
020700141120         IF  sqlcod <> 0;
020800141124           ODPBerr = 'E';
020900141124           ODPBmsg = Msg(03);
021000141120           clear ODPBpma;
021100141120           clear ODPBvma;
021200141120           clear ODPBsvm;
021300141124           exsr RoutEnd;
021400141120           leavesr;
021500141120         ENDIF;
021600141120
021700141120       //?Se arrivo qua vuol dire che tutto OK quindi torno la data che ho trovato
021800141120         ODPBdpb = PMGdtd;
021900141119
022000141119       ENDSR;
022100141119
022200141119       //--------------------------------------------------------------
022300141119       //?Operazioni finali.
022400141119       //--------------------------------------------------------------
022500141119       BEGSR RoutEnd;
022600141119
022700141119         *inLR = *on;
022800141119         return;
022900141119
023000141119       ENDSR;
023100141119
023200141119      /end-free
023300141119
023400141119       //--------------------------------------------------------------
023500141119       //?Schiere a tempo di compilazione.
023600141119       //--------------------------------------------------------------
023700141119
023800141120** - MSG --------------------------------------------------------------------*
023900141124Scaglione non presente in tabella. AVVISARE CED!!!                             1
024000141124Prezzo base uguale al precedente. Non creato record.                           2
024100141124Non rilevato nessun prezzo medio con lo scaglione calcolato. AVVISARE CED!!!   3
