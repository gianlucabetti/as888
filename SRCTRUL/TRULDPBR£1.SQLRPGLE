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
001400141119
001500141119      //---------------------------------------------------------------
001600141119      //?Definizione costanti.
001700141119      //---------------------------------------------------------------
001800141119
001900141119      //---------------------------------------------------------------
002000141119      //?Definizione schiere.
002100141119      //---------------------------------------------------------------
002200141119
002300141119      // - Messaggi di errore
002400141119     d Msg             s             78    dim(20) ctdata perrcd(1)
002500141119
002600141119      //---------------------------------------------------------------
002700141119      //?Definizione aree dati.
002800141119      //---------------------------------------------------------------
002900141119
003000141119      // - Dati utente
003100141119     d §AzUte        e ds                  extname(AZUTE00F)
003200141119     d                                     dtaara
003300141119     d §DatiUte      e ds                  extname(dDatiUte)
003400141119     d                                     dtaara
003500141119
003600141119      //---------------------------------------------------------------
003700141119      //?Definizione strutture dati.
003800141119      //---------------------------------------------------------------
003900141119
004000141119      // - Parametri ricevuti
004100141119     d KPJBA         e ds
004200141119     d TRULDPBDS     e ds
004300141120
004400141120      // - Ricerca/Controllo tabelle
004500141120     d TIBS02DS      e ds                  inz
004600141119
004700141119      // - File Prezzo medio Gasolio
004800141119     d TIPMG00F      e ds
004900141119
005000141119      //---------------------------------------------------------------
005100141119      //?Definizione variabili globali.
005200141119      //---------------------------------------------------------------
005300141119
005400141119      // - Flags booleani
005500141119     d wFine           s               n   inz(*off)
005600141119
005700141119      // - Campi di comodo
005800141120     d kDPBdtd         s                   like(DPBdtd) inz
005900141119     d Oggi            s              8  0 inz
006000141119
006100141119      //---------------------------------------------------------------
006200141119      //?Definizione procedure usate.
006300141119      //---------------------------------------------------------------
006400141119
006500141119      //---------------------------------------------------------------
006600141119      //?Definizione prototipi usati.
006700141119      //---------------------------------------------------------------
006800141120      /copy gaitrasrc/srcprotopr,TIBS02R
006900141119
007000141119      //---------------------------------------------------------------
007100141119      //?Definizione key-list.
007200141119      //---------------------------------------------------------------
007300141119
007400141119      //---------------------------------------------------------------
007500141119      //?M A I N - L I N E
007600141119      //---------------------------------------------------------------
007700141119
007800141119     c     *entry        PLIST
007900141119     c                   PARM                    kpjba
008000141119     c                   PARM                    TRULDPBDS
008100141119
008200141119      /free
008300141119
008400141119       //?Operazioni iniziali
008500141119       exsr RoutInz;
008600141119
008700141119       //?Elabora
008800141119       exsr Elabora;
008900141119
009000141119       //?Operazioni finali
009100141119       exsr RoutEnd;
009200141119
009300141119       //--------------------------------------------------------------
009400141119       //?Operazioni iniziali.
009500141119       //--------------------------------------------------------------
009600141119       BEGSR RoutInz;
009700141119
009800141119         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009900141119
010000141120       //?Pulisco i dati di ritorno
010100141120         clear ODPBsca;
010200141120         clear ODPBdpb;
010300141120         clear ODPBpma;
010400141120         clear ODPBvma;
010500141120         clear ODPBsvm;
010600141119         clear ODPBerr;
010700141119         clear ODPBmsg;
010800141119
010900141124       //?Se non ho lo scaglione del prezzo medio del gasolio torno errore
011000141119         IF  IDPBsca = 0;
011100141120           ODPBerr = 'E';
011200141119           ODPBmsg = Msg(01);
011300141119           exsr RoutEnd;
011400141119         ENDIF;
011500141120
011600141120       //?Deve essere uno scaglione valido
011700141120         clear TIBS02DS;
011800141120         T02mod = 'C';
011900141120         T02cod = 'IPG';
012000141120         T02ke1 = %editc(IDPBsca:'X');
012100141120         TNTBE_RicercaControllo  (kpjba : tibs02ds);
012200141120         IF  T02err <> *blanks;
012300141120           ODPBerr = 'E';
012400141120           ODPBmsg = Msg(01);
012500141120           exsr RoutEnd;
012600141120         ENDIF;
012700141120
012800141120       //?Data di oggi
012900141120         Oggi = %dec(%date());
013000141119
013100141119       ENDSR;
013200141119
013300141119       //--------------------------------------------------------------
013400141119       //?Elabora.
013500141119       //--------------------------------------------------------------
013600141119       BEGSR Elabora;
013700141119
013800141120       //?Calcolo lo scaglione che deve avere il prezzo base
013900141120       //?deve essere inferiore a 6 scaglioni
014000141120         ODPBsca = IDPBsca - 6;
014100141121
014200141121       //?Lo scaglione che calcolo NON deve essere negativo
014300141121         IF  ODPBsca < 0;
014400141121           ODPBerr = 'E';
014500141121           ODPBmsg = Msg(01);
014600141121           exsr RoutEnd;
014700141121         ENDIF;
014800141121       //?Lo scaglione che calcolo deve esistere
014900141121         clear TIBS02DS;
015000141121         T02mod = 'C';
015100141121         T02cod = 'IPG';
015200141121         T02ke1 = %editc(ODPBsca:'X');
015300141121         TNTBE_RicercaControllo  (kpjba : tibs02ds);
015400141121         IF  T02err <> *blanks;
015500141121           ODPBerr = 'E';
015600141121           ODPBmsg = Msg(01);
015700141121           exsr RoutEnd;
015800141121         ENDIF;
015900141119
016000141120       //?Cerco l'ultimo prezzo base
016100141120         kDPBdtd = Oggi;
016200141120         setll (kDPBdtd) TIDPB01L;
016300141120         read  TIDPB01L;
016400141119         IF  not %eof(TIDPB01L);
016500141120         //?se lo scaglione trovato è uguale al calcolato vado via
016600141120           IF  ODPBsca = DPBsvm;
016700141120             ODPBerr = 'E';
016800141120             ODPBmsg = Msg(02);
016900141120             exsr RoutEnd;
017000141119             leavesr;
017100141119           ENDIF;
017200141120         //?dati del VMA da tornare al chiamante
017300141120           ODPBpma = DPBpma;
017400141120           ODPBvma = DPBvma;
017500141120           ODPBsvm = DPBsvm;
017600141119         ENDIF;
017700141119
017800141120       //?Cerco il prezzo più alto, e la relativa data, dello scaglione calcolato
017900141119         exec sql
018000141119         WITH   A1 as ( select PMGsca, PMGpmg, PMGdtd
018100141119                          from TIPMG00F
018200141120                          where PMGsca = :ODPBsca
018300141119                         order by PMGsca, PMGpmg desc, PMGdtd desc ),
018400141119                A2 as ( select PMGsca, max(PMGpmg) as PMGpmg
018500141119                          from A1
018600141119                         group by PMGsca
018700141119                         order by PMGsca )
018800141120         SELECT distinct A2.*, max(A1.PMGdtd)
018900141120           into :PMGsca, :PMGpmg, :PMGdtd
019000141119           from A2 inner join A1
019100141119             on A2.PMGsca = A1.PMGsca
019200141119            and A2.PMGpmg = A1.PMGpmg
019300141119          group by A2.PMGsca, A2.PMGpmg
019400141119          order by A2.PMGsca;
019500141120
019600141120         IF  sqlcod <> 0;
019700141124           ODPBerr = 'E';
019800141124           ODPBmsg = Msg(03);
019900141120           clear ODPBpma;
020000141120           clear ODPBvma;
020100141120           clear ODPBsvm;
020200141124           exsr RoutEnd;
020300141120           leavesr;
020400141120         ENDIF;
020500141120
020600141120       //?Se arrivo qua vuol dire che tutto OK quindi torno la data che ho trovato
020700141120         ODPBdpb = PMGdtd;
020800141119
020900141119       ENDSR;
021000141119
021100141119       //--------------------------------------------------------------
021200141119       //?Operazioni finali.
021300141119       //--------------------------------------------------------------
021400141119       BEGSR RoutEnd;
021500141119
021600141119         *inLR = *on;
021700141119         return;
021800141119
021900141119       ENDSR;
022000141119
022100141119      /end-free
022200141119
022300141119       //--------------------------------------------------------------
022400141119       //?Schiere a tempo di compilazione.
022500141119       //--------------------------------------------------------------
022600141119
022700141120** - MSG --------------------------------------------------------------------*
022800141124Scaglione non presente in tabella. AVVISARE CED!!!                             1
022900141124Prezzo base uguale al precedente. Non creato record.                           2
023000141124Non rilevato nessun prezzo medio con lo scaglione calcolato. AVVISARE CED!!!   3
