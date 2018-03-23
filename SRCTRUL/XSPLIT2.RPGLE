000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500041130     D* VARIABILI DI PROCEDURA
000600001120     D*-----------------
000700110616     D InStringa       S          65535A   VARYING                              (stringa input)
000800100607     D InSepara        S             10A                                        (separatore)
000900100607     D InTypeOut       S              1A                                        (tipo output)
001000110617     D wSkParam        S          65535A                                        (output)
001100100607     D OutErrore       S              1A                                        (flag errore)
001200100607     D OutMsg          S             80A                                        (messaggio errore)
001300001120
001400041130     D*------------------
001500041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
001600041130     D*------------------
001700100607     D SkSplitCSV_1    S             16    DIM(4096)
001800100607     D SkSplitCSV_2    S             32    DIM(2048)
001900100607     D SkSplitCSV_3    S             64    DIM(1024)
002000100607     D SkSplitCSV_4    S            128    DIM(512)
002100100607     D SkSplitCSV_5    S            256    DIM(256)
002200100607     D SkSplitCSV_6    S            512    DIM(128)
002300100607     D SkSplitCSV_7    S           1024    DIM(64)
002400100607     D SkSplitCSV_8    S           2048    DIM(32)
002500100607     D wElemento       S           2048    INZ
002600041130     D lenInSepara     S              2  0 INZ(*zeros)
002700041130     D posDa           S              5  0 INZ(*zeros)
002800041130     D posA            S              5  0 INZ(*zeros)
002900041130     D i               s              4  0 INZ(1)
003000001120
003100000411
003200000414     C*------------------------------------------------------------------------*
003300000414     C* MAIN LINE
003400000414     C*------------------------------------------------------------------------*
003500041130     C*
003600041130     C* Innanzitutto inizializzo i parametri d OUT
003700041130     C*                  CLEAR                   OutArray
003800041130     C                   CLEAR                   wSkParam
003900041130     C                   CLEAR                   OutErrore
004000041130     C                   CLEAR                   OutMsg
004100041130     C*
004200041130     C* Determino subito la lunghezza del "separatore"
004300041130     C                   EVAL      lenInSepara = %len(%trim(InSepara))
004400041130     C**********
004500041130     C* Effettuo lo split della stringa dati d input
004600041130     C**********
004700041130     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
004800041130     C                   DOW       posDa <= %len(%trim(InStringa))
004900041130     C*
005000041130     C* Gestisco il 1° elemento
005100041130     C                   IF        i = 1
005200041130     C*
005300041130     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemento)
005400041130     C                   EVAL      posDa = 1
005500041130     C*
005600041130     C* Eseguo lo scan x trovare la fine del primo campo
005700041130     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
005800041130     C                                     posDa)
005900041130     C*
006000041130     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
006100041130     C* l'intero contenuto della stringa è l'unico elemento presente
006200041130     C                   IF        posA = *zeros
006300100607     C                   CLEAR                   SkSplitCSV_1
006400100607     C                   CLEAR                   SkSplitCSV_2
006500100607     C                   CLEAR                   SkSplitCSV_3
006600100607     C                   CLEAR                   SkSplitCSV_4
006700100607     C                   CLEAR                   SkSplitCSV_5
006800100607     C                   CLEAR                   SkSplitCSV_6
006900100607     C                   CLEAR                   SkSplitCSV_7
007000100607     C                   CLEAR                   SkSplitCSV_8
007100100607     C                   EVAL      wElemento = %trim(InStringa)
007200100607     C                   EXSR      VAL_SK_IN
007300041130     C                   LEAVE
007400041130     C                   ELSE
007500041130     C*
007600041130     C* A questo punto "estraggo" l'elemento corrente
007700041130     C                   IF        posA = 1
007800100607     C                   EVAL      wElemento = *blanks
007900100607     C                   EXSR      VAL_SK_IN
008000041130     C                   ELSE
008100100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
008200041130     C                                                    (posA-posDa))
008300100607     C                   EXSR      VAL_SK_IN
008400041130     C                   ENDIF
008500041130     C                   ENDIF
008600041130     C*
008700041130     C* X gli elementi successivi al 1°
008800041130     C                   ELSE
008900041130     C*
009000041130     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
009100041130     C                   EVAL      posDa = posA + lenInSepara
009200041130     C*
009300110617     C* Verifico che c sia ancora "capienza"
009400110617     C                   IF        posDa + lenInSepara > %len(InStringa)
009500110617     C                   LEAVE
009600110617     C                   ELSE
009700110617     C*
009800110617     C* Verifico che nn vi sia il campo nn valorizzato
009900041130     C                   IF        %subst(InStringa:posDa:lenInSepara) =
010000041130     C                             %trim(InSepara)
010100041130     C*
010200041130     C* Se elemento nn valorizzato skippo al prossimo
010300100607     C                   EVAL      wElemento = *blanks
010400100607     C                   EXSR      VAL_SK_IN
010500041130     C                   EVAL      posA  = posA + 1
010600041130     C                   ELSE
010700041130     C*
010800041130     C* Eseguo lo scan x trovare la fine dell'elemento corrente
010900041130     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
011000041130     C                                     posDa+1)
011100041130     C*
011200041130     C* Gestisco l'ultimo campo
011300041130     C                   IF        posA = *zeros
011400110509     C                   EVAL      posA = %len(%trimr(InStringa)) + 1
011500041130     C*
011600041130     C* A questo punto "estraggo" l'elemento corrente
011700100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
011800041130     C                                                    (posA-posDa))
011900100607     C                   EXSR      VAL_SK_IN
012000041130     C                   LEAVE
012100041130     C                   ELSE
012200041130     C* A questo punto "estraggo" l'elemento corrente
012300100607     C                   EVAL      wElemento = %subst(InStringa:posDa:
012400041130     C                                                    (posA-posDa))
012500100607     C                   EXSR      VAL_SK_IN
012600041130     C                   ENDIF
012700041130     C                   ENDIF
012800110617     C                   ENDIF
012900041130     C                   ENDIF
013000041130     C* Incremento il contatore d campo
013100041130     C                   EVAL      i = i +1
013200041130     C                   ENDDO
013300100607     C*
013400100607     C                   EXSR      VAL_SK_OUT
013500040827     C*
013600040827     C                   SETON                                        LR
013700001120     C*------------------------------------------------------------------------*
013800100607
013900100607
014000100607
014100100607
014200100607     C*------------------------------------------------------------------------*
014300100607     C* VAL_SK_IN - Valorizzazione schiera elemeneti
014400100607     C*------------------------------------------------------------------------*
014500100607     C     VAL_SK_IN     BEGSR
014600100607     C*
014700100607     C                   select
014800100607     C                   when      InTypeOut = '1'
014900100607     C                   eval      SkSplitCSV_1(i) = %trim(wElemento)
015000100607     C                   when      InTypeOut = '2'
015100100607     C                   eval      SkSplitCSV_2(i) = %trim(wElemento)
015200100607     C                   when      InTypeOut = '3'
015300100607     C                   eval      SkSplitCSV_3(i) = %trim(wElemento)
015400100607     C                   when      InTypeOut = '4'
015500100607     C                   eval      SkSplitCSV_4(i) = %trim(wElemento)
015600100607     C                   when      InTypeOut = '5'
015700100607     C                   eval      SkSplitCSV_5(i) = %trim(wElemento)
015800100607     C                   when      InTypeOut = '6'
015900100607     C                   eval      SkSplitCSV_6(i) = %trim(wElemento)
016000100607     C                   when      InTypeOut = '7'
016100100607     C                   eval      SkSplitCSV_7(i) = %trim(wElemento)
016200100607     C                   when      InTypeOut = '8'
016300100607     C                   eval      SkSplitCSV_8(i) = %trim(wElemento)
016400100607     C                   other
016500100607     C                   eval      SkSplitCSV_7(i) = %trim(wElemento)
016600100607     C                   endsl
016700100607     C*
016800100607     C                   ENDSR
016900100607
017000100607
017100100607
017200100607
017300100607     C*------------------------------------------------------------------------*
017400100607     C* VAL_SK_OUT - Valorizzazione campo output
017500100607     C*------------------------------------------------------------------------*
017600100607     C     VAL_SK_OUT    BEGSR
017700100607     C*
017800100607     C                   select
017900100607     C                   when      InTypeOut = '1'
018000100607     C                   movea     SkSplitCSV_1  wSkParam
018100100607     C                   when      InTypeOut = '2'
018200100607     C                   movea     SkSplitCSV_2  wSkParam
018300100607     C                   when      InTypeOut = '3'
018400100607     C                   movea     SkSplitCSV_3  wSkParam
018500100607     C                   when      InTypeOut = '4'
018600100607     C                   movea     SkSplitCSV_4  wSkParam
018700100607     C                   when      InTypeOut = '5'
018800100607     C                   movea     SkSplitCSV_5  wSkParam
018900100607     C                   when      InTypeOut = '6'
019000100607     C                   movea     SkSplitCSV_6  wSkParam
019100100607     C                   when      InTypeOut = '7'
019200100607     C                   movea     SkSplitCSV_7  wSkParam
019300100607     C                   when      InTypeOut = '8'
019400100607     C                   movea     SkSplitCSV_8  wSkParam
019500100607     C                   other
019600100607     C                   movea     SkSplitCSV_7  wSkParam
019700100607     C                   endsl
019800100607     C*
019900100607     C                   ENDSR
020000000411
020100001120
020200001120
020300001120
020400000414     C*------------------------------------------------------------------------*
020500000414     C* *INZSR - OPERAZIONI INIZIALI
020600000414     C*------------------------------------------------------------------------*
020700000414     C     *INZSR        BEGSR
020800001120     C*
020900000414     C*--------------------
021000000414     C* RICEVIMENTO PARAMETRI
021100000414     C*--------------------
021200000414     C     *ENTRY        PLIST
021300041130     C                   PARM                    InStringa
021400041130     C                   PARM                    InSepara
021500100607     C                   PARM                    InTypeOut
021600041130     C                   PARM                    wSkParam
021700041130     C                   PARM                    OutErrore
021800041130     C                   PARM                    OutMsg
021900001120     C*
022000000414     C                   ENDSR
