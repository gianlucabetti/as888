000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500001120     D* VARIABILI DI WRK
000600001120     D*-----------------
000700001120     D Indice          S              5i 0
000800001120     D NumOK           C                   CONST('1234567890')
000900001120     D MaiuOK          C                   CONST('QWERTYUIOPASDFGHJKLZXCVBNM')
001000001120     D MinuOK          C                   CONST('qwertyuiopasdfghjklzxcvbnm')
001100161115     D SimbolOK        C                   CONST('_+-.§@&')
001200170426     D SimbolOKSt      C                   CONST('_+-§&')
001300170426     D CharOK          S             69    INZ(*blanks)
001400170426     D WrkStr          S            100    INZ(*blanks)
001500170426     D DepWrkStr       S            100    INZ(*blanks)
001600001120     D SingleChar      S              1
001700001120     D LenWrkStr       S              3  0 INZ(*zeros)
001800001120     D PosChar         S              3  0 INZ(*zeros)
001900001120     D NumAt           S              3  0 INZ(*zeros)
002000001120     D PosAt           S              3  0 INZ(*zeros)
002100001120     D errl00          S              1    INZ('0')
002200001120     D depmsg          S             80    INZ(*blanks)
002300060316     D i               S              3  0 INZ(*zeros)
002400001120
002500001120     D*-----------------
002600001120     D* DS DI PROCEDURA
002700001120     D*-----------------
002800001120     D DSEMAIL       E DS
002900001120
003000000411
003100000414     C*------------------------------------------------------------------------*
003200000414     C* MAIN LINE
003300000414     C*------------------------------------------------------------------------*
003400000414     C*
003500001108     C* Prima di tutto verifico che il parametro da "convertire" non sia vuoto
003600001120     C                   IF        §EMLINDI <> *blanks
003700001120     C*
003800000427     C* Tolgo gli spazi a sinistra e a destra
003900001120     C                   EVAL      WrkStr=%trim(§EMLINDI)
004000100830     C*
004100100830     C* Reperisco lunghezza del contenuto della stringa "spurgato" dagli spazi
004200100830     C                   EVAL      LenWrkStr=%len(%trim(WrkStr))
004300100830 xxx C*
004400100830     C* Compongo l'elenco completo dei caratteri validi per l'inizio/fine d un indirizzo email
004500111103     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOKSt
004600100830     C*
004700100830     C* Verifico caratteri iniziali/finali del indirizzo email corrente
004800100830     C                   EVAL      SingleChar=%subst(WrkStr:1:1)
004900100830     C     SingleChar    SCAN      CharOK
005000100830     C                   IF        NOT %found
005100100830     C                   MOVEL     '1'           errl00
005200100830     C                   EVAL      depmsg='13 Indirizzo e-mail non valido: ' +
005300100830     C                                      'inizia con carattere non valido '+
005400100830     C                                      '('+ SingleChar + ')'
005500100830     C                   ENDIF
005600100830     C*
005700100830     C                   EVAL      SingleChar=%subst(WrkStr:LenWrkStr:1)
005800100830     C     SingleChar    SCAN      CharOK
005900100830     C                   IF        NOT %found
006000100830     C                   MOVEL     '1'           errl00
006100100830     C                   EVAL      depmsg='14 Indirizzo e-mail non valido: ' +
006200100830     C                                      'termina con carattere non valido '+
006300100830     C                                      '('+ SingleChar + ')'
006400100830     C                   ENDIF
006500100830     C*
006600100830     C* Verifico se tutto Ok e proseguo
006700100830     C                   IF        errl00 = '0'
006800100830     C*
006900100830     C* Compongo l'elenco completo dei caratteri validi in un indirizzo e-mail
007000100830     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOK
007100001120     C*
007200001120     C* A questo punto inizio i controllo formali
007300001120     C                   EXSR      CHKEMAIL
007400100830     C*
007500100830     C                   ENDIF
007600001120     C*
007700001120     C* Verifico se tutto Ok e valorizzo i parametri di OUTPUT
007800001120     C                   IF        errl00 = '0'
007900001120     C                   EVAL      §EMLINDO = WrkStr
008000001120     C                   EVAL      §EMLERRO = '0'
008100001120     C                   EVAL      §EMLMSGO = *blanks
008200001120     C                   ELSE
008300001120     C                   EVAL      §EMLINDO = *blanks
008400001120     C                   EVAL      §EMLERRO = '1'
008500001120     C                   EVAL      §EMLMSGO = depmsg
008600001120     C                   ENDIF
008700001120     C*
008800001120     C                   ELSE
008900001120     C                   EVAL      §EMLINDO = *blanks
009000001121     C                   EVAL      §EMLERRO = '2'
009100001120     C                   EVAL      §EMLMSGO = 'Passati parametri vuoti.'
009200001120     C                   ENDIF
009300001120     C*
009400001120     C                   SETON                                        LR
009500001120
009600001120
009700000414
009800001120     C*------------------------------------------------------------------------*
009900001120     C* CHKEMAIL - OPERAZIONI DI VERIFICA FORMALE
010000001120     C*------------------------------------------------------------------------*
010100001120     C     CHKEMAIL      BEGSR
010200001120     C*
010300001120     C                   MOVEL     '0'           errl00
010400060316     C*
010500060316     C* 1° Controllo: controlla lunghezza complessiva dell'indirizzo e-mail => NN deve essere inferiore a 6
010600060316     C                   IF        errl00 = '0'
010700060316     C                   IF        LenWrkStr < 6
010800060316     C                   MOVEL     '1'           errl00
010900100614     C                   EVAL      depmsg='1 E-mail errata, ' +
011000100614     C                                      'lunghezza minima 6 caratteri: ' +
011100100614     C                                      '"' + %trim(§EMLINDI) + '"'
011200060316     C                   ENDIF
011300060316     C                   ENDIF
011400001120     C*
011500060316     C* 2° Controllo: stringa non intervallata da spazi
011600001120     C                   IF        errl00 = '0'
011700001120     C     ' '           SCAN      WrkStr        PosChar
011800001120     C                   IF        PosChar > LenWrkStr
011900001120     C                   ELSE
012000001120     C                   MOVEL     '1'           errl00
012100060317     C                   EVAL      depmsg='2 Indirizzo e-mail non valido: ' +
012200001120     C                                      'presenti spazi "interni".'
012300001120     C                   ENDIF
012400001120     C                   ENDIF
012500001120     C*
012600060316     C* 3° Controllo: stringa con caratteri non ammessi
012700001120     C                   IF        errl00 = '0'
012800001120     C                   Z-ADD     1             Indice
012900001120     C                   DOW       Indice<=LenWrkStr
013000001120     C                   EVAL      SingleChar=%subst(WrkStr:Indice:1)
013100060316     C     SingleChar    SCAN      CharOK
013200001120     C                   IF        (NOT %found)
013300001120     C                   MOVEL     '1'           errl00
013400060317     C                   EVAL      depmsg='3 Indirizzo e-mail non valido: ' +
013500001120     C                                      'presenti caratteri non validi '+
013600001120     C                                      '('+ SingleChar + ')'
013700001120     C                   LEAVE
013800001120     C                   ENDIF
013900001120     C                   ADD       1             Indice
014000001120     C                   ENDDO
014100001120     C                   ENDIF
014200001120     C*
014300060316     C* 4° Controllo: se presente carattere '§' lo sostituisce con '@'
014400001120     C                   IF        errl00 = '0'
014500001120     C     '§':'@'       XLATE     WrkStr        WrkStr
014600001120     C                   ENDIF
014700001120     C*
014800060316     C* 5° Controllo: presenza del carattere '@'
014900001120     C                   IF        errl00 = '0'
015000001120     C                   Z-ADD     1             Indice
015100001120     C                   Z-ADD     *zeros        NumAt
015200001120     C                   DOW       Indice<=LenWrkStr
015300001120     C                   EVAL      SingleChar=%subst(WrkStr:Indice:1)
015400001120     C                   IF        SingleChar='@'
015500001120     C                   ADD       1             NumAt
015600001120     C                   Z-ADD     Indice        PosAt
015700001120     C                   ENDIF
015800001120     C                   ADD       1             Indice
015900001120     C                   ENDDO
016000001120     C                   IF        NumAt = *zeros
016100001120     C                   MOVEL     '1'           errl00
016200060317     C                   EVAL      depmsg='4 Indirizzo e-mail non valido: ' +
016300001120     C                                      'mancanza del carattere "@" o "§".'
016400001120     C                   ENDIF
016500001120     C                   IF        NumAt > 1
016600001120     C                   MOVEL     '1'           errl00
016700060317     C                   EVAL      depmsg='5 Indirizzo e-mail non valido: ' +
016800001120     C                                      'presenti troppi caratteri "@".'
016900001120     C                   ENDIF
017000001120     C                   ENDIF
017100060316     C*
017200060316     C* 6° Controllo: se posizinoe del carattere '@' è >= lunghezza indirizzo e-mail - 3 => ERRORE
017300060316     C                   IF        errl00 = '0'
017400060316     C                   IF        %scan('@':WrkStr) >= LenWrkStr - 3
017500060316     C                   MOVEL     '1'           errl00
017600060317     C                   EVAL      depmsg='6 Indirizzo e-mail non valido: pre' +
017700060316     C                                      'senti errori formali nel dominio.'
017800060316     C                   ENDIF
017900060316     C                   ENDIF
018000060316     C*
018100060316     C* 7° Controllo: l'indirizzo e-mail NN può iniziare con la "@"
018200060316     C                   IF        errl00 = '0'
018300060316     C                   IF        %subst(WrkStr:1:1) = '@'
018400060316     C                   MOVEL     '1'           errl00
018500060317     C                   EVAL      depmsg='7 Indirizzo e-mail non valido: non '+
018600060316     C                                      'può iniziare con il carattere "@".'
018700060316     C                   ENDIF
018800060316     C                   ENDIF
018900060316     C*
019000060316     C* Reperisco la posizione della "@"
019100060316     C                   EVAL      PosAt = %scan('@':WrkStr)
019200060316     C*
019300060316     C* 8° Controllo: il primo carattere '.' partendo da destra deve essere seguito da almeno 2 caratteri
019400060316     C* - Es scorretto => a@b.i
019500060316     C* - Es corretto  => a@b.it
019600060316     C                   IF        errl00 = '0'
019700060316     C                   FOR       i = %len(%trim(WrkStr)) By -1
019800060316     C                   IF        i = 0
019900060316     C                   LEAVE
020000060316     C                   ELSE
020100060316     C                   IF        %subst(WrkStr:i:1) = '.'
020200060316     C                   LEAVE
020300060316     C                   ENDIF
020400060316     C                   ENDIF
020500060316     C                   ENDFOR
020600060316     C                   IF        i > (LenWrkStr - 2)
020700060316     C                   MOVEL     '1'           errl00
020800060317     C                   EVAL      depmsg='8 Indirizzo e-mail non valido: pre' +
020900060316     C                                      'senti errori formali nel dominio.'
021000060316     C                   ENDIF
021100060316     C                   ENDIF
021200050307     C*
021300060316     C* 9° Controllo: no presenza del carattere '.' subito prima o subito dopo il carattere '@'
021400050307     C* - Es scorretto => a@.b.it
021500060316     C* - Es scorretto => a.@b.it
021600060316     C* - Es scorretto => a.@.b.it
021700050307     C* - Es corretto  => a@b.it
021800050307     C                   IF        errl00 = '0'
021900060316     C                   IF        PosAt > *zeros
022000080416     C                   IF        %subst(WrkStr:PosAt+1:1) = '.'  OR
022100080416     C                             %subst(WrkStr:PosAt-1:1) = '.'
022200050307     C                   MOVEL     '1'           errl00
022300060317     C                   EVAL      depmsg='9 Indirizzo e-mail non valido: ' +
022400060316     C                                      'il carattere "." è adiacente ' +
022500060316     C                                      'al carattere "@".'
022600050307     C                   ENDIF
022700050307     C                   ENDIF
022800050307     C                   ENDIF
022900001120     C*
023000060316     C* 10° Controllo: presenza del carattere '.' dopo il carattere '@'
023100050307     C* - Es scorretto => a@it
023200050307     C* - Es corretto  => a@b.it
023300001120     C                   IF        errl00 = '0'
023400001120     C                   IF        PosAt > *zeros
023500060316     C                   EVAL      DepWrkStr = %subst(WrkStr:PosAt+1:
023600001120     C                                         (LenWrkStr-PosAt))
023700001120     C     '.'           SCAN      DepWrkStr
023800001120     C                   IF        (NOT %found)
023900001120     C                   MOVEL     '1'           errl00
024000060317     C                   EVAL      depmsg='10 Indirizzo e-mail non valido: pre'+
024100060316     C                                      'senti errori formali nel dominio.'
024200001120     C                   ENDIF
024300001120     C                   ENDIF
024400001120     C                   ENDIF
024500060316     C*
024600060316     C* 11° Controllo: la parte di indirizzo e-mail dopo il carattere "@" deve essere lunga almeno 4 caratteri
024700060317     C*                  IF        errl00 = '0'
024800060317     C*                  IF        %len(%trim(DepWrkStr)) < 4
024900060317     C*                  MOVEL     '1'           errl00
025000060317     C*                  EVAL      depmsg = 'Indirizzo e-mail non valido: pre' +
025100060317     C*                                     'senti errori formali nel dominio.'
025200060317     C*                  ENDIF
025300060317     C*                  ENDIF
025400080415     C*
025500080415     C* 12° Controllo: no presenza sequenza caratteri '..'
025600080415     C                   IF        errl00 = '0'
025700080415     C     '..'          SCAN      WrkStr
025800080415     C                   IF        %found
025900080415     C                   MOVEL     '1'           errl00
026000080415     C                   EVAL      depmsg='12 Indirizzo e-mail non valido: pr' +
026100080415     C                                    'esente sequenza caratteri ..'
026200080415     C                   ENDIF
026300080415     C                   ENDIF
026400001120     C*
026500001120     C                   ENDSR
026600001120     C*------------------------------------------------------------------------*
026700000411
026800001120
026900001120
027000001120
027100000414     C*------------------------------------------------------------------------*
027200000414     C* *INZSR - OPERAZIONI INIZIALI
027300000414     C*------------------------------------------------------------------------*
027400000414     C     *INZSR        BEGSR
027500001120     C*
027600000414     C*--------------------
027700000414     C* RICEVIMENTO PARAMETRI
027800000414     C*--------------------
027900000414     C     *ENTRY        PLIST
028000001120     C                   PARM                    DSEMAIL
028100100830 xxx C*
028200001120     C* Compongo l'elenco completo dei caratteri validi in un indirizzo e-mail
028300001120     C                   EVAL      CharOK = NumOK + MinuOK + MaiuOK + SimbolOK
028400001120     C*
028500000414     C                   ENDSR
