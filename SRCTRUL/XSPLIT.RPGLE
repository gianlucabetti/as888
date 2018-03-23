000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400001120     D*-----------------
000500041130     D* VARIABILI DI PROCEDURA
000600001120     D*-----------------
000700041130     D InStringa       S          10240                                         (stringa input)
000800041130     D InSepara        S             10                                         (separatore)
000900041130     D*OutArray        S               *                                        (array output)
001000041130     D wSkParam        s          30000
001100041130     D OutErrore       S              1                                         (flag errore)
001200041130     D OutMsg          S             80                                         (messaggio errore)
001300001120
001400041130     D*------------------
001500041130     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
001600041130     D*------------------
001700041130     D SkSplitCSV      S            600    DIM(50)                              *XXX MOMENTANEO
001800041130     D*SkSplitCSV      S           1024    DIM(2048)
001900041130     D*                                    BASED(OutArray)
002000041130     D lenInSepara     S              2  0 INZ(*zeros)
002100041130     D posDa           S              5  0 INZ(*zeros)
002200041130     D posA            S              5  0 INZ(*zeros)
002300041130     D i               s              4  0 INZ(1)
002400001120
002500000411
002600000414     C*------------------------------------------------------------------------*
002700000414     C* MAIN LINE
002800000414     C*------------------------------------------------------------------------*
002900041130     C*
003000041130     C* Innanzitutto inizializzo i parametri d OUT
003100041130     C*                  CLEAR                   OutArray
003200041130     C                   CLEAR                   wSkParam
003300041130     C                   CLEAR                   OutErrore
003400041130     C                   CLEAR                   OutMsg
003500041130     C*
003600041130     C* Determino subito la lunghezza del "separatore"
003700041130     C                   EVAL      lenInSepara = %len(%trim(InSepara))
003800041130     C**********
003900041130     C* Effettuo lo split della stringa dati d input
004000041130     C**********
004100041130     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
004200041130     C                   DOW       posDa <= %len(%trim(InStringa))
004300041130     C*
004400041130     C* Gestisco il 1° elemento
004500041130     C                   IF        i = 1
004600041130     C*
004700041130     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemento)
004800041130     C                   EVAL      posDa = 1
004900041130     C*
005000041130     C* Eseguo lo scan x trovare la fine del primo campo
005100041130     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
005200041130     C                                     posDa)
005300041130     C*
005400041130     C* Verifico se il separatore è presente nella stringa d input, se nn è presente assumo che
005500041130     C* l'intero contenuto della stringa è l'unico elemento presente
005600041130     C                   IF        posA = *zeros
005700041130     C                   CLEAR                   SkSplitCSV
005800041130     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
005900041130     C                   LEAVE
006000041130     C                   ELSE
006100041130     C*
006200041130     C* A questo punto "estraggo" l'elemento corrente
006300041130     C                   IF        posA = 1
006400041130     C                   EVAL      SkSplitCSV(i) = *blanks
006500041130     C                   ELSE
006600041130     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
006700041130     C                                                    (posA-posDa))
006800041130     C                   ENDIF
006900041130     C                   ENDIF
007000041130     C*
007100041130     C* X gli elementi successivi al 1°
007200041130     C                   ELSE
007300041130     C*
007400041130     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
007500041130     C                   EVAL      posDa = posA + lenInSepara
007600041130     C*
007700041130     C* Verifico che nn vi sia il campo nn valorizzato
007800041130     C                   IF        %subst(InStringa:posDa:lenInSepara) =
007900041130     C                             %trim(InSepara)
008000041130     C*
008100041130     C* Se elemento nn valorizzato skippo al prossimo
008200041130     C                   EVAL      SkSplitCSV(i) = *blanks
008300041130     C                   EVAL      posA  = posA + 1
008400041130     C                   ELSE
008500041130     C*
008600041130     C* Eseguo lo scan x trovare la fine dell'elemento corrente
008700041130     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
008800041130     C                                     posDa+1)
008900041130     C*
009000041130     C* Gestisco l'ultimo campo
009100041130     C                   IF        posA = *zeros
009200041130     C                   EVAL      posA = %len(%trim(InStringa)) + 1
009300041130     C*
009400041130     C* A questo punto "estraggo" l'elemento corrente
009500041130     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
009600041130     C                                                    (posA-posDa))
009700041130     C                   LEAVE
009800041130     C                   ELSE
009900041130     C* A questo punto "estraggo" l'elemento corrente
010000041130     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
010100041130     C                                                    (posA-posDa))
010200041130     C                   ENDIF
010300041130     C                   ENDIF
010400041130     C                   ENDIF
010500041130     C* Incremento il contatore d campo
010600041130     C                   EVAL      i = i +1
010700041130     C                   ENDDO
010800041130     C*
010900041130     C                   MOVEA     SkSplitCSV    wSkParam
011000040827     C*
011100040827     C                   SETON                                        LR
011200001120     C*------------------------------------------------------------------------*
011300000411
011400001120
011500001120
011600001120
011700000414     C*------------------------------------------------------------------------*
011800000414     C* *INZSR - OPERAZIONI INIZIALI
011900000414     C*------------------------------------------------------------------------*
012000000414     C     *INZSR        BEGSR
012100001120     C*
012200000414     C*--------------------
012300000414     C* RICEVIMENTO PARAMETRI
012400000414     C*--------------------
012500000414     C     *ENTRY        PLIST
012600041130     C                   PARM                    InStringa
012700041130     C                   PARM                    InSepara
012800041130     C*                  PARM                    OutArray
012900041130     C                   PARM                    wSkParam
013000041130     C                   PARM                    OutErrore
013100041130     C                   PARM                    OutMsg
013200001120     C*
013300000414     C                   ENDSR
