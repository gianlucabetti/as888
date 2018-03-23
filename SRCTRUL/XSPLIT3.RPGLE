000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500041130     D* VARIABILI DI PROCEDURA
000600001120     D*-----------------
000700170616     D InStringa       S         131072A   VARYING                              (stringa input)
000800100607     D InSepara        S             10A                                        (separatore)
000900170616     D InTypeOut       S              2A                                        (tipo output)
001000170616     D wSkParam        S         131072A                                        (output)
001100170620     D OutIdxMax       S              4S 0                                      (massimo indice valorizzato)
001200100607     D OutErrore       S              1A                                        (flag errore)
001300100607     D OutMsg          S             80A                                        (messaggio errore)
001400001120
001500041130     D*------------------
001600041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
001700041130     D*------------------
001800100607     D SkSplitCSV_1    S             16    DIM(4096)
001900100607     D SkSplitCSV_2    S             32    DIM(2048)
002000100607     D SkSplitCSV_3    S             64    DIM(1024)
002100100607     D SkSplitCSV_4    S            128    DIM(512)
002200100607     D SkSplitCSV_5    S            256    DIM(256)
002300100607     D SkSplitCSV_6    S            512    DIM(128)
002400100607     D SkSplitCSV_7    S           1024    DIM(64)
002500100607     D SkSplitCSV_8    S           2048    DIM(32)
002600170616     D SkSplitCSV_9    S           2048    DIM(64)
002700170627     D wElemento       S           2048    VARYING INZ
002800041130     D lenInSepara     S              2  0 INZ(*zeros)
002900170629     D lenInStringa    S              6  0 INZ(*zeros)
003000170616     D posDa           S              6  0 INZ(*zeros)
003100170616     D posA            S              6  0 INZ(*zeros)
003200041130     D i               s              4  0 INZ(1)
003300001120
003400000411
003500000414     C*------------------------------------------------------------------------*
003600000414     C* MAIN LINE
003700000414     C*------------------------------------------------------------------------*
003800041130     C*
003900170616     C* Innanzitutto inizializzo i parametri di OUT
004000041130     C*                  CLEAR                   OutArray
004100041130     C                   CLEAR                   wSkParam
004200170620     C                   CLEAR                   OutIdxMax
004300041130     C                   CLEAR                   OutErrore
004400041130     C                   CLEAR                   OutMsg
004500170616     C*
004600170616     C* Normalizzo parametri di input
004700170616     C                   EVAL      InTypeOut = %trim(InTypeOut)
004800041130     C*
004900041130     C* Determino subito la lunghezza del "separatore"
005000170629     C                   EVAL      lenInSepara = %len(%trim(InSepara))
005100170629     C*
005200170629     C* Determino subito la lunghezza dei dati da "splittare"
005300170629     C                   EVAL      lenInStringa = %len(%trimr(InStringa))
005400041130     C**********
005500041130     C* Effettuo lo split della stringa dati d input
005600041130     C**********
005700041130     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
005800170629     C                   DOW       posDa <= lenInStringa
005900041130     C*
006000041130     C* Gestisco il 1° elemento
006100041130     C                   IF        i = 1
006200041130     C*
006300041130     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemento)
006400041130     C                   EVAL      posDa = 1
006500041130     C*
006600041130     C* Eseguo lo scan x trovare la fine del primo campo
006700170627     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
006800170627     C                                     posDa)
006900041130     C*
007000041130     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
007100041130     C* l'intero contenuto della stringa è l'unico elemento presente
007200041130     C                   IF        posA = *zeros
007300100607     C                   CLEAR                   SkSplitCSV_1
007400100607     C                   CLEAR                   SkSplitCSV_2
007500100607     C                   CLEAR                   SkSplitCSV_3
007600100607     C                   CLEAR                   SkSplitCSV_4
007700100607     C                   CLEAR                   SkSplitCSV_5
007800100607     C                   CLEAR                   SkSplitCSV_6
007900100607     C                   CLEAR                   SkSplitCSV_7
008000100607     C                   CLEAR                   SkSplitCSV_8
008100170616     C                   CLEAR                   SkSplitCSV_9
008200170629     C                   EVAL      wElemento = %trimr(InStringa)
008300100607     C                   EXSR      VAL_SK_IN
008400041130     C                   LEAVE
008500041130     C                   ELSE
008600041130     C*
008700041130     C* A questo punto "estraggo" l'elemento corrente
008800041130     C                   IF        posA = 1
008900100607     C                   EVAL      wElemento = *blanks
009000100607     C                   EXSR      VAL_SK_IN
009100041130     C                   ELSE
009200100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
009300041130     C                                                    (posA-posDa))
009400100607     C                   EXSR      VAL_SK_IN
009500041130     C                   ENDIF
009600041130     C                   ENDIF
009700041130     C*
009800041130     C* X gli elementi successivi al 1°
009900041130     C                   ELSE
010000041130     C*
010100041130     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
010200041130     C                   EVAL      posDa = posA + lenInSepara
010300041130     C*
010400110617     C* Verifico che c sia ancora "capienza"
010500170629     C                   IF        posDa > lenInStringa
010600110617     C                   LEAVE
010700110617     C                   ELSE
010800110617     C*
010900110617     C* Verifico che nn vi sia il campo nn valorizzato
011000041130     C                   IF        %subst(InStringa:posDa:lenInSepara) =
011100041130     C                             %trim(InSepara)
011200041130     C*
011300041130     C* Se elemento nn valorizzato skippo al prossimo
011400100607     C                   EVAL      wElemento = *blanks
011500100607     C                   EXSR      VAL_SK_IN
011600041130     C                   EVAL      posA  = posA + 1
011700041130     C                   ELSE
011800041130     C*
011900041130     C* Eseguo lo scan x trovare la fine dell'elemento corrente
012000170629     C                   IF        posDa+1 <= lenInStringa
012100041130     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
012200041130     C                                     posDa+1)
012300170627     C                   ELSE
012400170629     C                   EVAL      posA  = lenInStringa + 1
012500170627     C                   ENDIF
012600041130     C*
012700041130     C* Gestisco l'ultimo campo
012800041130     C                   IF        posA = *zeros
012900170629     C                   EVAL      posA = lenInStringa + 1
013000041130     C*
013100041130     C* A questo punto "estraggo" l'elemento corrente
013200100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
013300041130     C                                                    (posA-posDa))
013400100607     C                   EXSR      VAL_SK_IN
013500041130     C                   LEAVE
013600041130     C                   ELSE
013700041130     C* A questo punto "estraggo" l'elemento corrente
013800100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
013900041130     C                                                    (posA-posDa))
014000100607     C                   EXSR      VAL_SK_IN
014100041130     C                   ENDIF
014200041130     C                   ENDIF
014300110617     C                   ENDIF
014400041130     C                   ENDIF
014500041130     C* Incremento il contatore d campo
014600041130     C                   EVAL      i = i +1
014700041130     C                   ENDDO
014800100607     C*
014900100607     C                   EXSR      VAL_SK_OUT
015000040827     C*
015100040827     C                   SETON                                        LR
015200001120     C*------------------------------------------------------------------------*
015300100607
015400100607
015500100607
015600100607
015700100607     C*------------------------------------------------------------------------*
015800100607     C* VAL_SK_IN - Valorizzazione schiera elemeneti
015900100607     C*------------------------------------------------------------------------*
016000100607     C     VAL_SK_IN     BEGSR
016100100607     C*
016200100607     C                   select
016300100607     C                   when      InTypeOut = '1'
016400170627     C                   eval      SkSplitCSV_1(i) = wElemento
016500100607     C                   when      InTypeOut = '2'
016600170627     C                   eval      SkSplitCSV_2(i) = wElemento
016700100607     C                   when      InTypeOut = '3'
016800170627     C                   eval      SkSplitCSV_3(i) = wElemento
016900100607     C                   when      InTypeOut = '4'
017000170627     C                   eval      SkSplitCSV_4(i) = wElemento
017100100607     C                   when      InTypeOut = '5'
017200170627     C                   eval      SkSplitCSV_5(i) = wElemento
017300100607     C                   when      InTypeOut = '6'
017400170627     C                   eval      SkSplitCSV_6(i) = wElemento
017500100607     C                   when      InTypeOut = '7'
017600170627     C                   eval      SkSplitCSV_7(i) = wElemento
017700100607     C                   when      InTypeOut = '8'
017800170627     C                   eval      SkSplitCSV_8(i) = wElemento
017900170616     C                   when      InTypeOut = '9'
018000170627     C                   eval      SkSplitCSV_9(i) = wElemento
018100100607     C                   other
018200170627     C                   eval      SkSplitCSV_7(i) = wElemento
018300100607     C                   endsl
018400100607     C*
018500100607     C                   ENDSR
018600100607
018700100607
018800100607
018900100607
019000100607     C*------------------------------------------------------------------------*
019100100607     C* VAL_SK_OUT - Valorizzazione campo output
019200100607     C*------------------------------------------------------------------------*
019300100607     C     VAL_SK_OUT    BEGSR
019400100607     C*
019500100607     C                   select
019600100607     C                   when      InTypeOut = '1'
019700100607     C                   movea     SkSplitCSV_1  wSkParam
019800100607     C                   when      InTypeOut = '2'
019900100607     C                   movea     SkSplitCSV_2  wSkParam
020000100607     C                   when      InTypeOut = '3'
020100100607     C                   movea     SkSplitCSV_3  wSkParam
020200100607     C                   when      InTypeOut = '4'
020300100607     C                   movea     SkSplitCSV_4  wSkParam
020400100607     C                   when      InTypeOut = '5'
020500100607     C                   movea     SkSplitCSV_5  wSkParam
020600100607     C                   when      InTypeOut = '6'
020700100607     C                   movea     SkSplitCSV_6  wSkParam
020800100607     C                   when      InTypeOut = '7'
020900100607     C                   movea     SkSplitCSV_7  wSkParam
021000100607     C                   when      InTypeOut = '8'
021100100607     C                   movea     SkSplitCSV_8  wSkParam
021200170616     C                   when      InTypeOut = '9'
021300170616     C                   movea     SkSplitCSV_9  wSkParam
021400100607     C                   other
021500100607     C                   movea     SkSplitCSV_7  wSkParam
021600100607     C                   endsl
021700170620     C*
021800170620     C                   eval      OutIdxMax = i
021900100607     C*
022000100607     C                   ENDSR
022100000411
022200001120
022300001120
022400001120
022500000414     C*------------------------------------------------------------------------*
022600000414     C* *INZSR - OPERAZIONI INIZIALI
022700000414     C*------------------------------------------------------------------------*
022800000414     C     *INZSR        BEGSR
022900001120     C*
023000000414     C*--------------------
023100000414     C* RICEVIMENTO PARAMETRI
023200000414     C*--------------------
023300000414     C     *ENTRY        PLIST
023400041130     C                   PARM                    InStringa
023500041130     C                   PARM                    InSepara
023600100607     C                   PARM                    InTypeOut
023700041130     C                   PARM                    wSkParam
023800170620     C                   PARM                    OutIdxMax
023900041130     C                   PARM                    OutErrore
024000041130     C                   PARM                    OutMsg
024100001120     C*
024200000414     C                   ENDSR
