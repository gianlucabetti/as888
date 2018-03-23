000100161020     H BNDDIR('QC2LE')
000200080611     H NOMAIN
000300071128     ***********************************************************************************************
000400071128     **
000500161020     ** Questo modulo fornisce la funzionalità di conversione DA/A CCSID
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080612     ** Ogni metodo restituisce un valore 10I:
000900080612     **             - = 0 => tutto ok
001000080612     **             - < 0 => errore
001100080612     **
001200151126     **
001300161017     ** --------- --- --------- ------------------------------
001400161017     **
001500161021     ** PARAMETRI DELLA SOTTOPROCEDURA: UBICONVR_CvtFromToCCSID...
001600161017     **
001700161020     ** Posizione Uso Tipo      Descrizione
001800161017     ** --------- --- --------- ------------------------------
001900161020     ** 1         IN  CHARACTER Dati da convertire
002000161020     ** 2         IN  INTEGER   CCSID origine
002100161020     ** 3         IN  INTEGER   CCSID destinazione
002200161020     ** 4         OUT CHARACTER Dati convertiti
002300161017     ** --------- --- --------- ------------------------------
002400071128     **
002500071128     **
002600161020     ** ESEMPI DI CHIAMATA
002700161020     **
002800161021     **  if UBICONVR_CvtFromToCCSID( iICONVinput   : iICONVfromCCSID
002900161021     **                            : iICONVtoCCSID : oICONVoutput) = 0;
003000161017     **
003100080612     ** --------- --- --------- ------------------------------
003200071128     **
003300071128     ** ISTRUZIONI PER LA COMPILAZIONE
003400071128     **
003500161021     ** 1. Creare il modulo UBICONVR  (Opz 15 PDM) indicando BNDDIR('QC2LE')
003600161021     ** 2. Creare/Aggiornare il programma di servizio UBICONVR  (CRTSRVPGM / UPDSRVPGM)
003700161020     ** 4. Volendo cancellare il modulo
003800071128     **
003900071128     ***********************************************************************************************
004000080612
004100080612
004200080612      //********************************************************************************************
004300080612      //
004400080612      // Definizione file (globali)
004500080612      //
004600080612      //********************************************************************************************
004700071128
004800080612
004900071128      //********************************************************************************************
005000071128      //
005100080612      // Definizione prototipi procedure (globali)
005200071128      //
005300071128      //********************************************************************************************
005400161021     D/COPY GAITRASRC/SRCPROTOPI,UBICONV
005500161021     D/COPY GAITRASRC/SRCPROTOPR,UBICONVR
005600161021     D/COPY GAITRASRC/SRCPROTOPI,UBICONVR
005700080612
005800161020
005900071128      //********************************************************************************************
006000071128      //
006100080612      // Definizione variabili work (globali)
006200071128      //
006300071128      //********************************************************************************************
006400151126     D InitDone        S               N   INZ(*off)
006500161017     D
006600161021
006700161021
006800160404
006900161017     P*--------------------------------------------------
007000161021     P* Procedure name: UBICONVR_CvtFromToCCSID
007100161020     P* Purpose:        Converte dati DA/A CCSID
007200161020     P* Returns:        Integer (Status_Code)
007300161020     P* Parameter:      iICONVinput     => Dati da convertire
007400161020     P* Parameter:      iICONVfromCCSID => CCSID origine
007500161020     P* Parameter:      iICONVtoCCSID   => CCSID destinazione
007600161020     P* Parameter:      oICONVoutput    => Dati convertiti
007700160404     P*--------------------------------------------------
007800160404     P*
007900161021     P UBICONVR_CvtFromToCCSID...
008000161020     P                 B                   EXPORT
008100161021     D*
008200161021     D UBICONVR_CvtFromToCCSID...
008300161020     D                 PI            10I 0
008400161021     DiICONVinput                 65535A   VARYING OPTIONS(*VARSIZE) CONST
008500161020     DiICONVfromCCSID                10I 0 CONST
008600161020     DiICONVtoCCSID                  10I 0 CONST
008700161021     DoICONVoutput                65535A   VARYING OPTIONS(*VARSIZE)
008800161021     D*
008900161021
009000161021     D cd              ds                  likeds(iconv_t)
009100161021     D tocode          ds                  likeds(qtqcode_t)
009200161021     D fromcode        ds                  likeds(qtqcode_t)
009300161021     D rtn             s             10i 0 inz
009400161021     D inbufptr        s               *   inz
009500161021     D inlen           s             10u 0 inz
009600161021     D outbufptr       s               *   inz
009700161021     D outlen          s             10u 0 inz
009800161020
009900161021
010000161021     D locMethodCode   S             10I 0
010100161021     D
010200160404
010300161020
010400160404      /FREE
010500161017        // Inizializzo opcode di metodo a errore
010600160404        locMethodCode = -1;
010700161017
010800161017
010900161017        // Monitorizzo l'intero processo
011000161017        Monitor;
011100161021
011200161021        // Inizializzo i parametri di output
011300161021        clear oICONVoutput;
011400161021
011500161021        // CCSID DA/A
011600161021        tocode = *allx'00';
011700161021        tocode.ccsid = iICONVtoCCSID;
011800161021        fromcode = *allx'00';
011900161021        fromcode.ccsid = iICONVfromCCSID;
012000161021
012100161021        // Apertura
012200161021        cd = QtqIconvOpen(tocode : fromcode);
012300161021
012400161021        // Se OK
012500161021        if cd.rtn = -1;
012600161021        else;
012700161021
012800161021           // Conversione
012900161021           wICONVinput = iICONVinput;
013000161021           inbufptr  = %addr(wICONVinput) + 2;
013100161021           outbufptr = %addr(oICONVoutput) + 2;
013200161021           inlen     = %len(wICONVinput);
013300161021           outlen    = %size(oICONVoutput);
013400161021
013500161021           rtn = iconv( cd
013600161021                      : inbufptr
013700161021                      : inlen
013800161021                      : outbufptr
013900161021                      : outlen );
014000161020
014100161021           // Se OK
014200161021           if rtn = -1;
014300161021           else;
014400161021
014500161021              rtn = iconv_close(cd);
014600161021
014700161021              // Valorizzo opcode di metodo
014800161021              locMethodCode = 0;
014900161021
015000161021           endif;
015100161021        endif;
015200161020
015300161017
015400161020        On-Error;
015500161020
015600161020           // Esito chiamata già inizializzato a errore e parametri output già puliti
015700161020
015800161020           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
015900161020           Monitor;
016000161020              // Chiusura
016100161021              rtn = iconv_close(cd);
016200161020           On-Error;
016300161020           // Fine monitoring
016400161020           EndMon;
016500161020
016600161020        // Fine monitoring
016700161020        EndMon;
016800161017
016900160404
017000160404        return locMethodCode;
017100160404
017200160404      /END-FREE
017300160404
017400161021     P UBICONVR_CvtFromToCCSID...
017500160404     P                 E
017600151126
