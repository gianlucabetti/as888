000100080611     H NOMAIN
000200071128     ***********************************************************************************************
000300071128     **
000400080611     ** Questo modulo verifica, converte ed effettua considerazioni sulla rappresentazione a
000500080611     ** stringa di un valore numerico sia esso con o senza particolari formattazioni/editazioni.
000600071128     **
000700080611     ** Può essere usato solo come sottoprocedura.
000800080611     ** Ogni metodo restituisce un valore booleano true/false
000900071128     **
001000080611     ** PARAMETRI DELLA SOTTOPROCEDURA: UBISNUM_Init
001100071128     **
001200071128     ** Posizione Uso Tipo      Descrizione
001300071128     ** --------- --- --------- ------------------------------
001400080611     **     /      /      /           /
001500071128     ** --------- --- --------- ------------------------------
001600071129     **
001700080611     ** PARAMETRI DELLA SOTTOPROCEDURA UBISNUM_Check...
001800071129     **
001900071129     ** Posizione Uso Tipo      Descrizione
002000071129     ** --------- --- --------- ------------------------------
002100080611     ** 1         IN  CHARACTER Stringa da verificare
002200080611     ** 2         IN  CHARACTER Carattere separatore decimale
002300080611     ** 3         OUT SIGNED    Valore numerico della stringa verificata
002400080611     ** 4         OUT BOOLEAN   Indicatore "ok è numerico"
002500080611     ** 5         OUT BOOLEAN   Indicatore "ok è numerico intero"
002600071129     ** --------- --- --------- ------------------------------
002700071128     **
002800071128     **
002900071128     ** ESEMPI DI CHIAMATA
003000071128     **
003100080611     ** if UBISNUM_Check(pInPiStr:pInDecChr:pOutPiVal:pOutPiNum:pOutPiInt)
003200071128     **
003300071128     ** ISTRUZIONI PER LA COMPILAZIONE
003400170522     **
003500170522     ** 1. Creare il modulo UBISNUM   (Opz 15 PDM)
003600170522     **    indicando TGTRLS(*CURRENT)
003700170522     **
003800170522     ** 2. Creare/Aggiornare il programma di servizio UBISNUM   (CRTSRVPGM / UPDSRVPGM)
003900170522     **    con ACTGRP(*CALLER)
004000170522     **      CRTSRVPGM SRVPGM(GAITRAOBJ/UBISNUM)
004100170522     **         EXPORT(*SRCFILE) SRCFILE(GAITRASRC/SRCSRVPGM)
004200170522     **         TEXT('Str/Num: verifica, conversione e considerazioni')
004300170522     **         TGTRLS(*CURRENT)
004400170522     **
004500170522     ** 4. Eventualmente si può cancellare il modulo.
004600170522     **
004700071128     **
004800071128     ***********************************************************************************************
004900071128
005000071128      //********************************************************************************************
005100071128      //
005200071128      // Definizione prototipi procedure.
005300071128      //
005400071128      //********************************************************************************************
005500080611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005600071128
005700071128      //********************************************************************************************
005800071128      //
005900071128      // Definizione variabili globali.
006000071128      //
006100071128      //********************************************************************************************
006200080611     D InitDone        S               N   INZ(*off)
006300080611     D WrkStr          S            100
006400080611     D FinStr          S            100    INZ(*all'0')
006500080611     D LenWrkStr       S              2  0 INZ(0)
006600080611     D LenFin          S              2  0 INZ(0)
006700080611     D SingleChar      S              1
006800080611     D Indice          S              2  0 INZ(1)
006900080611     D ElencoNum       S             10    INZ('1234567890')
007000080611     D DecSepPos       S              2  0 INZ(0)
007100080611     D MilSepPos       S              2  0 INZ(0)
007200080611     D WrkDec          S             30    VARYING INZ(*all'0')
007300080611     D Segno           S              1  0 INZ(1)
007400080611     D StrMilChr       S              1    INZ
007500080611     D wContaDecChr    S              3  0 INZ(*zeros)
007600071128
007700080611     P*--------------------------------------------------
007800080611     P* Procedure name: UBISNUM_Init
007900080611     P* Purpose:        Inizializzazione variabili di lavoro
008000080611     P* Returns:        Boolean (Init_Code)
008100080611     P*--------------------------------------------------
008200080611     P*
008300080611     P UBISNUM_Init...
008400071129     P                 B                   EXPORT
008500071129     D*
008600080611     D UBISNUM_Init...
008700080611     D                 PI              N
008800071129
008900071129      //********************************************************************************************
009000071129      //
009100071129      // Definizione variabili work.
009200071129      //
009300071129      //********************************************************************************************
009400080611     D locMethodCode   S               N
009500071129
009600071127      /FREE
009700080611        // Inizializzo opcode di metodo
009800080611        locMethodCode = *off;
009900080611        InitDone = *off;
010000071129
010100080611        // Inizializzo variabili di wrk
010200080611        reset WrkStr;
010300080611        reset FinStr;
010400080611        reset LenWrkStr;
010500080611        reset LenFin;
010600080611        reset SingleChar;
010700080611        reset Indice;
010800080611        reset ElencoNum;
010900080611        reset DecSepPos;
011000080611        reset MilSepPos;
011100080611        reset WrkDec;
011200080611        reset Segno;
011300080611        reset StrMilChr;
011400080611        reset wContaDecChr;
011500080611
011600080611        // Valorizzo opcode di metodo
011700080611        locMethodCode = *on;
011800080611        InitDone = *on;
011900080611
012000080611        return locMethodCode;
012100071127
012200071127      /END-FREE
012300071129
012400080611     P UBISNUM_Init...
012500071129     P                 E
012600071129
012700071129
012800080611     P*--------------------------------------------------
012900080611     P* Procedure name: UBISNUM_Check
013000080611     P* Purpose:        Operazioni di verifica e conversione
013100080611     P* Returns:        Boolean (Check_Code)
013200080611     P* Parameter:      pInPiStr      => Stringa da verificare/convertire
013300080611     P* Parameter:      pInPiDecChr   => Carattere separatore decimale
013400080611     P* Parameter:      pOutPiVal     => Valore stringa convertita
013500080611     P* Parameter:      pOutPiNum     => *on = è numerico
013600080611     P* Parameter:      pOutPiInt     => *on = è numerico intero
013700080611     P*--------------------------------------------------
013800080611     P*
013900080611     P UBISNUM_Check...
014000071129     P                 B                   EXPORT
014100071129     D*
014200080611     D UBISNUM_Check...
014300080611     D                 PI              N
014400080611     D  pInPiStr                    100A   CONST
014500080611     D  pInPiDecChr                   1A   VALUE
014600080611     D  pOutPiVal                    63S30
014700080611     D  pOutPiNum                      N
014800080611     D  pOutPiInt                      N
014900071129
015000071129      //********************************************************************************************
015100071129      //
015200071129      // Definizione variabili work.
015300071129      //
015400071129      //********************************************************************************************
015500080611     D locMethodCode   S               N
015600080611     D locInternalErr  S               N
015700071129
015800071129
015900071129      /FREE
016000080611        // Inizializzo opcode di metodo
016100080611        locMethodCode  = *off;
016200080611        locInternalErr = *off;
016300080611
016400080611        // Se nn già inizializzata la procedure => forzo inizializzazione
016500080611        if not InitDone;
016600080611               if not UBISNUM_Init;
016700080611                  locInternalErr = *on;
016800080611               endif;
016900080611        endif;
017000080611
017100071205
017200080611        // Se tutto ok => procedo
017300080611        if not locInternalErr;
017400080611           // Inizializzo subito i parametri d output
017500080611           pOutPiVal = *zeros;
017600080611           pOutPiNum = *off;
017700080611           pOutPiInt = *off;
017800080611
017900080611
018000080611           // Determino in base ai paramteri d input l'eventuale carattere d editazione migliaia
018100080611           if pInPiDecChr = ',';
018200080611              StrMilChr = '.';
018300080611           else;
018400080611              StrMilChr = ',';
018500080611           endif;
018600100827
018700100827
018800100827           // Se il carattere separatore decimale nn passato in input => imposto ','
018900100827           if pInPiDecChr = *blanks;
019000100827              pInPiDecChr = ',';
019100100827           endif;
019200100827
019300080611
019400080611           // Prima di tutto verifico che il parametro da "convertire" non sia vuoto
019500080611           if pInPiStr <> *blanks;
019600080611              pOutPiNum='1';
019700080611              // Tolgo gli spazi a sinistra e a destra
019800080611              WrkStr=%trim(pInPiStr);
019900080611              //Se la stringa è nel formato ',2' => la considero '0,2'
020000080611              if %subst(WrkStr:1:1) = pInPiDecChr;
020100080611                 WrkStr = '0' + %trim(WrkStr);
020200080611              endif;
020300080611
020400080611              // Reperisco lunghezza del contenuto della stringa "spurgato" dagli spazi
020500080611              LenWrkStr=%len(%trim(WrkStr));
020600090921
020700080611              // Verifico se specificato (a sin. o des. della stringa) il segno
020800090921              if %subst(WrkStr:1:1) = '-';
020900090921                 WrkStr = %subst(WrkStr:2);
021000090921                 Segno = -1;
021100090921              endif;
021200090921              if %subst(WrkStr:1:1) = '+';
021300090921                 WrkStr = %subst(WrkStr:2);
021400090921                 Segno = +1;
021500090921              endif;
021600090921              if %subst(WrkStr:LenWrkStr:1) = '-';
021700090921                 WrkStr = %subst(WrkStr:1:LenWrkStr-1);
021800090921                 Segno = -1;
021900090921              endif;
022000090921              if %subst(WrkStr:LenWrkStr:1) = '+';
022100090921                 WrkStr = %subst(WrkStr:1:LenWrkStr-1);
022200090921                 Segno = +1;
022300090921              endif;
022400090921
022500090921              // Ri-aggiorno la lunghezza della stringa numerica senza i caratteri di editazione
022600090921              LenWrkStr=%len(%trim(WrkStr));
022700080611
022800080611              // Se il campo è editato con il separatore delle migliaia =>
022900080611              // elimino il carattere di editazione
023000080611              Indice = 1;
023100080611              dow Indice<=LenWrkStr;
023200080611                  SingleChar=%subst(WrkStr:Indice:1);
023300080611                  if SingleChar=StrMilChr;
023400080611                     WrkStr=%subst(WrkStr:1:Indice-1) +
023500080611                            %subst(WrkStr:Indice+1);
023600080611                  endif;
023700080611                  Indice=Indice+1;
023800080611              enddo;
023900090921
024000090921              // Verifico se la stringa è espresa in "notazione scientifica"
024100100827              if %scan('e+':WrkStr) > *zeros OR
024200100827                 %scan('E+':WrkStr) > *zeros OR
024300100827                 %scan('e-':WrkStr) > *zeros OR
024400100827                 %scan('E-':WrkStr) > *zeros;
024500090921                 pOutPiVal = %float(WrkStr);
024600090921                 WrkStr = %editc(pOutPiVal:'L');
024700090921                 if pInPiDecChr = ',';
024800090921                    WrkStr = %xlate('.':pInPiDecChr:WrkStr);
024900090921                 else;
025000090921                    WrkStr = %xlate(',':pInPiDecChr:WrkStr);
025100090921                 endif;
025200090921                 WrkStr = %trim(WrkStr);
025300090921              endif;
025400080611
025500080611              // Ri-aggiorno la lunghezza della stringa numerica senza i caratteri di editazione
025600080611              LenWrkStr=%len(%trim(WrkStr));
025700080611
025800080611              // Inizializzo flag che indica la presenza del carattere d separatore decimale
025900080611              wContaDecChr=*zeros;
026000080611
026100080611              // Determino se trattasi di stringa valida ai fini numerici
026200080611              Indice = 1;
026300080611              dow Indice<=LenWrkStr;
026400080611                  SingleChar=%subst(WrkStr:Indice:1);
026500080611                  if %scan(SingleChar:ElencoNum) = *zeros AND
026600080611                     SingleChar<>pInPiDecChr;
026700080611                     pOutPiInt='0';
026800080611                     pOutPiNum='0';
026900080611                     leave;
027000080611                  else;
027100080611                     if SingleChar=pInPiDecChr;
027200080611                        if wContaDecChr=*zeros;
027300080611                           wContaDecChr=wContaDecChr+1;
027400080611                        else;
027500080611                           pOutPiInt='0';
027600080611                           pOutPiNum='0';
027700080611                           leave;
027800080611                        endif;
027900080611                     endif;
028000080611                  endif;
028100080611                  Indice=Indice+1;
028200080611              enddo;
028300080611
028400080611
028500080611         // In caso di stringa valida a fini numerici verifico se trattasi di
028600080611         // numero intero o decimale (il carattere di separazione decimale è
028700080611         // parametrizzato in input: o "," o ".")
028800080611            if pOutPiNum='1';
028900080611               DecSepPos = %scan(pInPiDecChr:WrkStr);
029000080611               if DecSepPos > *zeros;
029100080611                  // Verifico che nn vi siano + di 30 cifre decimali
029200080611                  if LenWrkStr-DecSepPos > 30;
029300080611                     %subst(WrkStr:(DecSepPos+30+1))=*blanks;
029400080611                     LenWrkStr=%len(%trim(WrkStr));
029500080611                     %len(WrkDec)=0;
029600080611                  else;
029700080611                     %len(WrkDec)=(30-(LenWrkStr-DecSepPos));
029800080611                  endif;
029900080611
030000080611                  // Verifico che qualora esista il segno di separatore decimali i decimali
030100080611                  // nn siano tutti a zero altrimenti lo considero comunque un intero
030200080611                  if %subst(WrkStr:(DecSepPos+1):(%len(%trim(WrkStr))
030300080611                                   -DecSepPos))=*zeros;
030400080611                     pOutPiInt='1';
030500080611                  endif;
030600080611
030700080611                  WrkStr=%subst(WrkStr:1:(DecSepPos-1))+
030800080611                         %subst(WrkStr:(DecSepPos+1):
030900080611                         (LenWrkStr-DecSepPos))+WrkDec;
031000080611                  LenFin=%len(%trim(WrkStr));
031100080611                         %subst(FinStr:(100-LenFin+1):LenFin)=%trim(WrkStr);
031200080611               else;
031300080611                  pOutPiInt='1';
031400080611                  WrkStr=%trim(WrkStr) + WrkDec;
031500080611                  LenFin=%len(%trim(WrkStr));
031600080611                  %subst(FinStr:(100-LenFin+1):LenFin)=%trim(WrkStr);
031700080611               endif;
031800080611               pOutPiVal=%dec(%subst(FinStr:%size(FinStr)-63+1:63-30)+
031900080611                         pInPiDecChr+
032000080611                         %subst(FinStr:%size(FinStr)-63+1+(63-30):30):63:30);
032100080611           endif;
032200080611
032300080611           // Applico il segno algebrico positivo o negativo (ma solo se il valore reperito è <> 0)
032400080611           if pOutPiVal <> *zeros;
032500080611              pOutPiVal=pOutPiVal*Segno;
032600080611           endif;
032700080611         endif;
032800071129        endif;
032900071129
033000071205
033100080611        // Valorizzo opcode di metodo
033200080611        locMethodCode = *on;
033300080611        InitDone = *off;
033400080611
033500080611        return locMethodCode;
033600071129
033700071129      /END-FREE
033800071129
033900080611     P UBISNUM_Check...
034000071129     P                 E
