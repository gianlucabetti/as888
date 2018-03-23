000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400090316     ** Questo modulo reperisce il tipo di dispositivi (IP / IPX / SNA) ed il relativo
000500090316     ** indirizzo del job corrente
000600081003     **
000700081003     ** Può essere usato solo come sottoprocedura.
000800081003     ** Ogni metodo restituisce un valore 10I:
000900081003     **             - = 0 => tutto ok
001000081003     **             - < 0 => errore
001100071128     **
001200090316     ** PARAMETRI DELLA SOTTOPROCEDURA: UBRTVIPJ_Retrieve...
001300071129     **
001400081003     ** Posizione Uso Tipo      Lunghezza Descrizione
001500081003     ** --------- --- --------- --------- ------------------------------
001600090317     ** 1         IN  CHARACTER 10        Nome Job specifico
001700090317     ** 2         OUT CHARACTER 10        Nome Job corrente
001800090317     ** 3         OUT CHARACTER 03        Device type
001900090317     ** 4         OUT CHARACTER 10        Unità di controllo
002000090317     ** 5         OUT CHARACTER 09        Switch
002100090317     ** 6         OUT CHARACTER 09        Porta
002200090317     ** 7         OUT CHARACTER 15        Network Address (puntato)
002300090317     ** 8         OUT CHARACTER 07        CPF error
002400090317     ** 9         OUT CHARACTER 80        Message error
002500081003     ** --------- --- --------- --------- ------------------------------
002600071128     **
002700071128     **
002800071128     ** ESEMPI DI CHIAMATA
002900071128     **
003000090317     ** if UBRTVIPJ_Retrieve(pInNamJob:pOutCurrJob:pOutDevTyp:pOutCtl:pOutSwitch:pOutPort:
003100090317     **                      pOutNtwAdr:pOutCPF:pOutMessage) = 0
003200071128     **
003300071128     ** ISTRUZIONI PER LA COMPILAZIONE
003400071128     **
003500090316     ** 1. Creare il modulo UBRTVIPJ (Opz 15 PDM)
003600090316     ** 2. Creare/Aggiornare il programma di servizio UBRTVIPJ (CRTSRVPGM / UPDSRVPGM)
003700080610     ** 4. Cancellare il modulo.
003800071128     **
003900071128     ***********************************************************************************************
004000071128
004100071128      //********************************************************************************************
004200071128      //
004300071128      // Definizione prototipi procedure.
004400071128      //
004500071128      //********************************************************************************************
004600090316     D/COPY GAITRASRC/SRCPROTOPR,UBRTVIPJ
004700081003     D/COPY QSYSINC/QRPGLESRC,QUSEC
004800071128
004900071128      //********************************************************************************************
005000071128      //
005100071128      // Definizione variabili globali.
005200071128      //
005300071128      //********************************************************************************************
005400080612     D
005500090317     D                SDS
005600090317     D currJob               244    253
005700071128
005800080611     P*--------------------------------------------------
005900090316     P* Procedure name: UBRTVIPJ_Retrieve...
006000090316     P* Purpose:        Reperimento indirizzo IP del dispositivo di rete sul quale è attivo un job
006100080612     P* Returns:        Integer (Status_Code)
006200090317     P* Parameter:      pInNamJob      => IN  => Nome Job specifico
006300090317     P* Parameter:      pOutCurrJob    => OUT => Nome Job corrente
006400090317     P* Parameter:      pOutDevTyp     => OUT => Device type
006500090317     P* Parameter:      pOutCtl        => OUT => Unità di controllo
006600090317     P* Parameter:      pOutSwitch     => OUT => Switch
006700090317     P* Parameter:      pOutPort       => OUT => Porta
006800090317     P* Parameter:      pOutNtwAdr     => OUT => Network Address (puntato)
006900090317     P* Parameter:      pOutCPF        => OUT => CPF error
007000090317     P* Parameter:      pOutMessage    => OUT => Message error
007100080611     P*--------------------------------------------------
007200090317     P*
007300090316     P UBRTVIPJ_Retrieve...
007400071129     P                 B                   EXPORT
007500071129     D*
007600090316     D UBRTVIPJ_Retrieve...
007700080612     D                 PI            10I 0
007800090317     D   pInNamJob                   10A
007900090317     D   pOutCurrJob                 10A
008000090317     D   pOutDevTyp                   3A
008100090317     D   pOutCtl                     10A
008200090317     D   pOutSwitch                   9A
008300090317     D   pOutPort                     9A
008400090317     D   pOutNtwAdr                  15A
008500090317     D   pOutCPF                      7A
008600090317     D   pOutMessage                 80A
008700090317
008800080612      //********************************************************************************************
008900080612      //
009000080612      // Definizione variabili work.
009100080612      //
009200080612      //********************************************************************************************
009300081003
009400090316     D*
009500090317     D wEsito          S             10I 0
009600090317     D wJob2Rtv        S             10A
009700090317     D*
009800090316     D PROTO_IP        C                   CONST(X'02')
009900090316     D PROTO_IPX       C                   CONST(X'06')
010000090316     D PROTO_SNA       C                   CONST(X'40')
010100090316     D**********************************************************************
010200090316     D stdError        DS                  qualified
010300090316     D  QUSEC                              likeDs(QUSEC)
010400090316     D  outError                   1024
010500090316     D**********************************************************************
010600090316     D*
010700090316     D* QDCD060000 è una sottodefinizione che descrive il formato DEVD0600.
010800090316     D/COPY QSYSINC/QRPGLESRC,QDCRDEVD
010900090316     D*
011000090316     D GetDeviceDescription...
011100090316     D                 PR                  ExtPgm('QDCRDEVD')
011200090316     D  outReceiver                 972A   Const
011300090316     D  inLength                     10I 0 Const
011400090316     D  inFormat                      8A   Const
011500090316     D  inDeviceName                 10A   Const
011600090316     D  ioErrorCode                1040A   Const
011700090316     D*
011800090316      /free
011900090317       // Inizializzo campo e la chiamata a errore
012000090317       wEsito      = -1;
012100090317       pOutCurrJob = currJob;
012200090317       pOutDevTyp  = *blanks;
012300090317       pOutCtl     = *blanks;
012400090317       pOutSwitch  = *blanks;
012500090317       pOutPort    = *blanks;
012600090317       pOutNtwAdr  = *blanks;
012700090317       pOutCPF     = *all'9';
012800090317       pOutMessage = 'Errore in chiamata metodo UBRTVIPJ_Retrieve';
012900090317
013000090317
013100090317       // Se nn passato in input un job specifico considero il job corrente
013200090317       if pInNamJob <> *blanks;
013300090317          wJob2Rtv = pInNamJob;
013400090317       else;
013500090317          wJob2Rtv = pOutCurrJob;
013600090317       endif;
013700090317
013800090317
013900090317       // Impostazione lunghezza di ritorno messaggio d errore
014000090317       stdError.QUSEC.QUSBPRV = 1040;
014100090317
014200090316
014300090316       // Per reperire le informazioni necessarie utilizzo il formato 'DEVD0600'
014400090317           GetDeviceDescription(QDCD060000:972:'DEVD0600':wJob2Rtv:stdError);
014500090316           if stdError.QUSEC.QUSBAVL > 0;
014600090316             if stdError.QUSEC.QUSEI = 'CPF2702';
014700090317               pOutCPF = 'CPF2702';
014800090318               pOutMessage = 'Device sconosciuto:' + wJob2Rtv;
014900090316             else;
015000090317               pOutCPF = stdError.QUSEC.QUSEI;
015100090317               pOutMessage = 'Errore API QDCRDEVD';
015200090316             endif;
015300090316           else;
015400090316             select;
015500090316               when QDCNP = PROTO_IP;
015600090318                 pOutMessage = 'IP Device:' + wJob2Rtv
015700090316                             + ' IP:' + QDCIPADF;
015800090317                 pOutDevTyp  = 'IP';
015900090317                 pOutNtwAdr  = QDCIPADF;
016000090317                 wEsito = 0;
016100090316               when QDCNP = PROTO_IPX;
016200090318                 pOutMessage = 'IPX Device:' + wJob2Rtv
016300090316                             + ' IP:' + QDCIPADF;
016400090317                 pOutDevTyp  = 'IPX';
016500090317                 pOutNtwAdr  = QDCIPADF;
016600090317                 wEsito = 0;
016700090316               other;
016800090318                 pOutMessage = 'SNA Device:' + wJob2Rtv
016900090316                             + ' CTL:' + %trim(QDCANSCN02)
017000090316                             + ' Switch:' + %trim(%editc(QDCSS:'3'))
017100090316                             + ' Port:' + %trim(%editc(QDCDP:'3'));
017200090317                 pOutDevTyp  = 'SNA';
017300090317                 pOutCtl     = %trim(QDCANSCN02);
017400090317                 pOutSwitch  = %trim(%editc(QDCSS:'3'));
017500090317                 pOutPort    = %trim(%editc(QDCDP:'3'));
017600090317                 pOutNtwAdr  = QDCIPADF;
017700090317                 wEsito = 0;
017800090316             endsl;
017900090316           endif;
018000090317
018100090317
018200090317       // Se tutto ok resetto campi errore
018300090317       if wEsito = 0;
018400090317          pOutCPF = *blanks;
018500090317          pOutMessage = *blanks;
018600090317       endif;
018700090317
018800090317
018900090317       // Imposto l'esito chiamata metodo corrente
019000090317       return wEsito;
019100090317
019200090316
019300090317       *inlr = *ON;
019400090316
019500090317      /end-free
019600071129
019700090316     P UBRTVIPJ_Retrieve...
019800071129     P                 E
