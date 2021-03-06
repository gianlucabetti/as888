000100000000     H*------------------------------------------------------------------------*
000200000104     H* IMPORTAZIONE CAPPARIO DPD
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060427     F*------------------------------------------------------------------------*
000600060427     F* DATA BASE
000700060427     F*------------------------------------------------------------------------*
000800060427     FDPCSC01L  UF A E           K DISK
000900060427     FWFDPCSCF  IF   E           K DISK
001000060427     FPRTF198   O    F  198        PRINTER
001100060427     D*----------------
001200060427     D* DS PARAMETRI DI LANCIO PGM
001300060427     D*-------------------
001400060427     D PARAM           DS                  INZ
001500060427     D  PARVER                        5  0
001600000000     D*-------------------
001700000000     D* ARCHITETTURA
001800000000     D*-------------------
001900000000     D KPJBA         E DS
002000060426     D*-------------------
002100060427     D*------------------
002200060427     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
002300060427     D*------------------
002400060427     D InStringa       S          10240    VARYING                              (stringa input)
002500060427     D InSepara        S             10                                         (separatore)
002600060428     D skSplitCSV      S           4096    DIM(1000) VARYING
002700060428     D lenInSepara     S              2  0 INZ(*zeros)
002800060428     D posDa           S              5  0 INZ(*zeros)
002900060428     D posA            S              5  0 INZ(*zeros)
003000060428     D i               s              4  0 INZ(1)
003100060428     D wSize           S             10I 0 INZ(*zeros)
003200060427     D*-------------------
003300060427     D* VARIABILI DI WRK
003400060427     D*-------------------
003500060427     D wCharCSV        s              8    inz('|')
003600060427     D wAlfa           s           4096    inz varying
003700060427     D wRiga           s            198    inz
003800000000     C*------------------------------------------------------------------------*
003900000000     C* MAIN LINE
004000000000     C*------------------------------------------------------------------------*
004100060427     C*
004200060427     C* Effettuo elaborazione
004300060427     C                   EXSR      EXECSC
004400000000     C*
004500000000     C                   SETON                                        LR
004600060427     C*------------------------------------------------------------------------*
004700060427     C*  EXECSC - IMPORTA L' ARCHIVIO "SERVICE"
004800060427     C*------------------------------------------------------------------------*
004900060427     C     EXECSC        BEGSR
005000060427     C*
005100060427     C* Scorro tutto il file d wrk
005200060427     C     *START        SETLL     WFDPCSCF
005300060427     C                   READ      WFDPCSCF
005400060427     C                   DOW       not %eof(WFDPCSCF)
005500060427     C                   IF        %subst(%trim(WDCSCDTA):1:1) <> '#'
005600060428     C*
005700060428     C* Eseguo "splittamento" dei dati
005800060428     C                   EVAL      InStringa = WDCSCDTA
005900060428     C                   EVAL      InSepara  = wCharCSV
006000060428     C                   EXSR      SPLIT
006100060428     C*
006200060428     C* Se nn reperiti elementi listo in stampa
006300060428     C                   IF        i = *zeros
006400060427     C                   EVAL      wRiga = 'WFDPCSCF' + ' ' + WDCSCDTA
006500060427     C                   EXCEPT    rigaErr
006600060427     C                   ELSE
006700060427     C*
006800060427     C* Imposto il buffer del file d output
006900060427     C                   CLEAR                   DPCSC000
007000060427     C                   EVAL      CSCVER  = PARVER
007100060428     C                   EVAL      wAlfa = %trim(skSplitCSV(1))
007200060427     C                   MOVE      wAlfa         CSCSRVC
007300060428     C                   EVAL      wAlfa = %trim(skSplitCSV(2))
007400060427     C                   MOVEL     wAlfa         CSCSRVD
007500060428     C                   EVAL      wAlfa = %trim(skSplitCSV(3))
007600060427     C                   MOVEL     wAlfa         CSCSRVM
007700060428     C                   EVAL      wAlfa = %trim(skSplitCSV(4))
007800060508     C                   EVAL      CSCSRVE = wAlfa
007900060427     C                   EVAL      CSCATB  = *blanks
008000060427     C*
008100060427     C* Quindi scarico il buffer
008200060427     C                   WRITE     DPCSC000
008300060427     C                   ENDIF
008400060427     C                   ENDIF
008500060427     C                   READ      WFDPCSCF
008600060427     C                   ENDDO
008700060427     C*
008800060427     C                   ENDSR
008900060428     C*------------------------------------------------------------------------*
009000060428     C* SPLIT - Routine di reperimento PORTO e VARIE fatturate
009100060428     C*------------------------------------------------------------------------*
009200060428     C     SPLIT         BEGSR
009300060428     C*
009400060428     C* Innanzitutto inizializzo i parametri d OUT
009500060428     C                   RESET                   lenInSepara
009600060428     C                   RESET                   posDa
009700060428     C                   RESET                   posA
009800060428     C                   RESET                   i
009900060428     C                   RESET                   wSize
010000060428     C*
010100060428     C* Determino subito la lunghezza del "separatore"
010200060428     C                   EVAL      lenInSepara = %len(%trim(InSepara))
010300060428     C**********
010400060428     C* Effettuo lo split della stringa dati d input
010500060428     C**********
010600060428     C*
010700060428     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in elementi
010800060428     C                   DOW       posDa <= %len(%trim(InStringa))
010900060428     C*
011000060428     C* Gestisco il 1� elemento
011100060428     C                   IF        i = 1
011200060428     C*
011300060428     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo elemen
011400060428     C                   EVAL      posDa = 1
011500060428     C*
011600060428     C* Eseguo lo scan x trovare la fine del primo campo
011700060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
011800060428     C                                     posDa)
011900060428     C*
012000060428     C* Verifico se il separatore � presente nella stringa d input, se nn � presente assumo che
012100060428     C* l'intero contenuto della stringa � l'unico elemento presente
012200060428     C                   IF        posA = *zeros
012300060428     C                   CLEAR                   SkSplitCSV
012400060428     C                   EVAL      SkSplitCSV(i) = %trim(InStringa)
012500060428     C                   LEAVE
012600060428     C                   ELSE
012700060428     C*
012800060428     C* A questo punto "estraggo" l'elemento corrente
012900060428     C                   IF        posA = 1
013000060428     C                   EVAL      SkSplitCSV(i) = *blanks
013100060428     C                   ELSE
013200060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
013300060428     C                                                    (posA-posDa))
013400060428     C                   ENDIF
013500060428     C                   ENDIF
013600060428     C*
013700060428     C* X gli elementi successivi al 1�
013800060428     C                   ELSE
013900060428     C*
014000060428     C* Parto a considerare l'elementoo corrente dal precedente separatore in poi
014100060428     C                   EVAL      posDa = posA + lenInSepara
014200060428     C*
014300060428     C* Verifico che nn vi sia il campo nn valorizzato
014400060428     C                   IF        %subst(InStringa:posDa:lenInSepara) =
014500060428     C                             %trim(InSepara)
014600060428     C*
014700060428     C* Se elemento nn valorizzato skippo al prossimo
014800060428     C                   EVAL      SkSplitCSV(i) = *blanks
014900060428     C                   EVAL      posA  = posA + 1
015000060428     C                   ELSE
015100060428     C*
015200060428     C* Eseguo lo scan x trovare la fine dell'elemento corrente
015300060428     C                   EVAL      posA  = %scan(%trim(InSepara):InStringa:
015400060428     C                                     posDa+1)
015500060428     C*
015600060428     C* Gestisco l'ultimo campo
015700060428     C                   IF        posA = *zeros
015800060428     C                   EVAL      posA = %len(%trim(InStringa)) + 1
015900060428     C*
016000060428     C* A questo punto "estraggo" l'elemento corrente
016100060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
016200060428     C                                                    (posA-posDa))
016300060428     C                   LEAVE
016400060428     C                   ELSE
016500060428     C*
016600060428     C* A questo punto "estraggo" l'elemento corrente
016700060428     C                   EVAL      SkSplitCSV(i) = %subst(InStringa:posDa:
016800060428     C                                                    (posA-posDa))
016900060428     C                   ENDIF
017000060428     C                   ENDIF
017100060428     C                   ENDIF
017200060428     C*
017300060428     C* Incremento il contatore d campo
017400060428     C                   EVAL      i = i +1
017500060428     C                   ENDDO
017600060428     C*
017700060428     C                   ENDSR
017800000000     C*------------------------------------------------------------------------*
017900980403     C* *INZSR - OPERAZIONI INIZIALI
018000000000     C*------------------------------------------------------------------------*
018100980403     C     *INZSR        BEGSR
018200000000     C*
018300000000     C* RICEVIMENTO PARAMETRI
018400000000     C     *ENTRY        PLIST
018500000000     C                   PARM                    KPJBA
018600060427     C                   MOVEL     KPJBU         PARAM
018700000000     C*
018800000000     C                   ENDSR
018900060427     C*------------------------------------------------------------------------*
019000060427     OPRTF198   E            rigaErr     1
019100060427     O                       wRiga
