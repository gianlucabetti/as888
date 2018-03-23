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
005100161024      // Definizione prototipi procedure e DS (globali)
005200071128      //
005300071128      //********************************************************************************************
005400161021     D/COPY GAITRASRC/SRCPROTOPR,UBICONVR
005500161021     D/COPY GAITRASRC/SRCPROTOPI,UBICONVR
005600161024
005700161024      *  Prototype DS
005800161024     D QtqCode_T       DS                  Qualified
005900161024     D  CCSID                        10I 0 Inz(0)
006000161024     D  CvtAlt                       10I 0 Inz(0)
006100161024     D  SubAlt                       10I 0 Inz(0)
006200161024     D  ShiftState                   10I 0 Inz(1)
006300161024     D  InLenOpt                     10I 0 Inz(0)
006400161024
006500161024      * Some pgm use value 1 for MixDataErrOpt
006600161024     D  MixDataErrOpt                10I 0 Inz(0)
006700161024     D  Reserved                      8A   Inz(*ALLX'00')
006800161024
006900161024      * Template DS
007000161024     D iconv_t         DS                  Qualified INZ
007100161024     D  rtn_value                    10I 0
007200161024     D  cd                           10I 0 Dim(12)
007300161024
007400161024      * To create a conversion environment, we need to call QtqIconvOpen.
007500161024      * The following prototype allows QtqIconvOpen to be called from RPG IV:
007600161024     D QtqIConvOpen    PR                  ExtProc('QtqIconvOpen')
007700161024     D                                     LikeDS(iconv_T)
007800161024     D  toCCSID                            LikeDS(QtqCode_T)
007900161024     D  fromCCSID                          LikeDS(QtqCode_T)
008000161024
008100161024      * The QtqIconvOpen API uses the following data structure to create the conversion environment:
008200161024     D iconv           PR            10U 0 ExtProc('iconv')
008300161024     D  hConv                              LikeDS(iconv_t) VALUE
008400161024     D  pInBuff                        *   VALUE
008500161024     D  nInLen                         *   VALUE
008600161024     D  pOutBuff                       *   VALUE
008700161024     D  nOutLen                        *   VALUE
008800161024
008900161024      *  iconv_close clear memory after used
009000161024     D iconv_close     PR            10I 0 ExtProc('iconv_close')
009100161024     D  hConv                              LikeDS(iconv_t) VALUE
009200161024
009300161024      * Calling QtqIconvOpen to establish/create the conversion environment to convert from
009400161024      * the job's CCSID to ASCII used on the IFS, we would use the following code:
009500161024     D fromCCSID       DS                  LikeDS(QtqCode_T)
009600161024     D                                     Inz(*LIKEDS)
009700161024     D toCCSID         DS                  LikeDS(QtqCode_T)
009800161024     D                                     Inz(*LIKEDS)
009900161024     D hConv           DS                  LikeDS(iconv_T)
010000161024     D                                     Inz(*LIKEDS)
010100080612
010200161024
010300161020
010400071128      //********************************************************************************************
010500071128      //
010600080612      // Definizione variabili work (globali)
010700071128      //
010800071128      //********************************************************************************************
010900151126     D InitDone        S               N   INZ(*off)
011000161017     D
011100161021
011200161021
011300160404
011400161017     P*--------------------------------------------------
011500161021     P* Procedure name: UBICONVR_CvtFromToCCSID
011600161020     P* Purpose:        Converte dati DA/A CCSID
011700161020     P* Returns:        Integer (Status_Code)
011800161020     P* Parameter:      iICONVinput     => Dati da convertire
011900161020     P* Parameter:      iICONVfromCCSID => CCSID origine
012000161020     P* Parameter:      iICONVtoCCSID   => CCSID destinazione
012100161020     P* Parameter:      oICONVoutput    => Dati convertiti
012200160404     P*--------------------------------------------------
012300160404     P*
012400161021     P UBICONVR_CvtFromToCCSID...
012500161020     P                 B                   EXPORT
012600161021     D*
012700161021     D UBICONVR_CvtFromToCCSID...
012800161020     D                 PI            10I 0
012900161021     DiICONVinput                 65535A   VARYING OPTIONS(*VARSIZE) CONST
013000161020     DiICONVfromCCSID                10I 0 CONST
013100161020     DiICONVtoCCSID                  10I 0 CONST
013200161021     DoICONVoutput                65535A   VARYING OPTIONS(*VARSIZE)
013300161021     D*
013400161024
013500161024      * Input Value
013600161024     D pInData         S               *
013700161024     D ppInData        S               *   Inz(%addr(pInData))
013800161024     D nInLen          S             10I 0
013900161024
014000161024      * Output value
014100161024     D pOutData        S               *
014200161024     D ppOutData       S               *   Inz(%addr(pOutData))
014300161024     D nOutLen         S             10I 0
014400161020
014500161021
014600161021     D locMethodCode   S             10I 0
014700161021     D
014800161024     D nReturn         S             10I 0 INZ
014900161024     D wICONVinput     S                   INZ LIKE(iICONVinput)
015000160404
015100161020
015200160404      /FREE
015300161017        // Inizializzo opcode di metodo a errore
015400160404        locMethodCode = -1;
015500161017
015600161017
015700161017        // Monitorizzo l'intero processo
015800161017        Monitor;
015900161021
016000161021        // Inizializzo i parametri di output
016100161021        clear oICONVoutput;
016200161021
016300161024        // CCSID DA/A
016400161024        fromCCSID.CCSID = iICONVfromCCSID;
016500161024        toCCSID.CCSID = iICONVtoCCSID;
016600161024
016700161024        // Apertura
016800161024        hConv = *ALLX'00';
016900161024        hConv = QtqIconvOpen(toCCSID:fromCCSID);
017000161021
017100161021        // Se OK
017200161024        if hConv.rtn_value  = -1;
017300161021        else;
017400161024
017500161024           // Conversione
017600161024           wICONVinput = iICONVinput;
017700161025           nInLen   = %len(wICONVinput);
017800161025           nOutLen  = %size(oICONVoutput) - 2;
017900161025           pInData  = %addr(wICONVinput) + 2;
018000161025           pOutData = %addr(oICONVoutput) + 2;
018100161024
018200161024           // Run Iconv() for xlate
018300161024           nReturn = iconv(hConv
018400161024                          :ppInData
018500161024                          :%addr(nInLen)
018600161024                          :ppOutData
018700161024                          :%addr(nOutLen));
018800161020
018900161021           // Se OK
019000161024           if nReturn = -1;
019100161021           else;
019200161021
019300161024              // Chiusura
019400161024              callp iconv_close(hConv);
019500161021
019600161021              // Valorizzo opcode di metodo
019700161021              locMethodCode = 0;
019800161021
019900161021           endif;
020000161021        endif;
020100161020
020200161017
020300161020        On-Error;
020400161020
020500161020           // Esito chiamata già inizializzato a errore e parametri output già puliti
020600161020
020700161020           // Monitorizzo anche quanto eventualmente eseguito nella gestione errore superiore
020800161020           Monitor;
020900161024
021000161024              // Chiusura
021100161024              callp iconv_close(hConv);
021200161024
021300161020           On-Error;
021400161020           // Fine monitoring
021500161020           EndMon;
021600161020
021700161020        // Fine monitoring
021800161020        EndMon;
021900161017
022000160404
022100160404        return locMethodCode;
022200160404
022300160404      /END-FREE
022400160404
022500161021     P UBICONVR_CvtFromToCCSID...
022600160404     P                 E
022700151126
