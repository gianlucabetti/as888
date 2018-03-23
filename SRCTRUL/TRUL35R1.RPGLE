000100020219     H DECEDIT('0,') DATEDIT(*DMY.)
000200020219      *
000300020219      * SCHIERE
000400020911     D  SkChkDgt       S              1    DIM(50)
000500020221
000600020219      *
000700020219      * VARIABILI
000800020219      *
000900020911     D lung            S              2  0 inz(*zeros)
001000020911     D WrkChkDgt       S              1  0 inz(*zeros)
001100020911     D WrkChkDgtNum    S              1  0 inz(*zeros)
001200020911     D WrkChkDgtAlfa   S              1    inz(*blanks)
001300081010     D WrkChkDgtAlfaT  S             36
001400020911     D SommaPari       S              5  0 inz(*zeros)
001500020911     D SommaDispari    S              5  0 inz(*zeros)
001600020911     D SommaTot        S              5  0 inz(*zeros)
001700020911     D Risultato       S              3  1 inz(*zeros)
001800020911     D I               S              2  0 inz(*zeros)
001900020911     D RestoDec        S              1  1 inz(*zeros)
002000020911     D Multiplo10      S              5  0 inz(*zeros)
002100081010     D Messaggio       S             50    inz(*blanks)
002200020219
002300020221      ****************************************************************
002400020221
002500081010     C*
002600081010     C     *entry        plist
002700081010     C                   parm                    InBarcode        35
002800081010     C                   parm                    InTip             4
002900081010     C                   parm                    InDsply           1
003000081010     C                   parm                    WrkChkDgtAlfaT
003100081010     C*
003200081010     C                   movel     InBarcode     WrkChkDgtAlfaT
003300081010     C*
003400081010     C                   if        InDsply = 'S'
003500081010     C                   seton                                        20
003600081010     C                   else
003700081010     C                   setoff                                       20
003800081010     C                   endif
003900081010     C*
004000081010     C* EAN   - Barcode secondo standerd EAN = EAN/UCC-13 - somma PARI
004100081010     C     InTip         caseq     'EANP'        CtrEAN_P
004200081010     C*
004300081010     C* EAN   - Barcode secondo standerd EAN = EAN/UCC-13 - somma DISPARI
004400081010     C     InTip         caseq     'EAND'        CtrEAN_D
004500081010     C*
004600081010     C* M1031 - Barcode generico Modulo 10
004700081010     C*    InTip         caseq     'M10'         CtrM10
004800081010     C*
004900081010     C                   endcs
005000020911     C*
005100020911     C* Fine
005200020219     C                   exsr      ENDPGM
005300020911     C****************************************************************
005400020911
005500020911
005600020911
005700020911     C****************************************************************
005800081010     C** CTREAN_P ** Calcolo check-digits secondo standard EAN/UCC-13 - somma PARI
005900020911     C****************************************************************
006000081010     C     CTREAN_P      BEGSR
006100081010     C*
006200081010     C* Verifico se il bacose ha lunghezza pari o dispari
006300081010     C                   EVAL      Risultato = %len(%trim(InBarcode))/2
006400081010     C                   Z-ADD     Risultato     RestoDec
006500081010     C                   IF        RestoDec = *zeros                            * è pari
006600081010     C                   SETON                                        30
006700081010     C                   ELSE
006800081010     C                   SETOFF                                       30
006900081010     C                   ENDIF
007000020911     C*
007100020911     C                   z-add     *zeros        lung
007200081010     C                   eval      lung = %len(%trim(InBarcode))
007300020911     C*
007400020911     C                   CLEAR                   SkChkDgt
007500081010     C                   MOVEA     InBarcode     SkChkDgt
007600020911     C* Inizializzo variabili di wrk
007700020911     C                   Z-ADD     *zeros        WrkChkDgt
007800020911     C                   Z-ADD     *zeros        WrkChkDgtNum
007900020911     C                   MOVEL     *blanks       WrkChkDgtAlfa
008000081010     C                   MOVEL     *blanks       WrkChkDgtAlfaT
008100020911     C                   Z-ADD     *zeros        SommaPari
008200020911     C                   Z-ADD     *zeros        SommaDispari
008300020911     C                   Z-ADD     *zeros        SommaTot
008400020911     C                   Z-ADD     *zeros        Risultato
008500020911     C*
008600020911     C* Effettuo il calcolo
008700020911     C     1             DO        lung          I
008800020911     C                   IF        SkChkDgt(I) <> *blanks
008900081010     C*
009000020911     C* Stabilisco se trattasi di cifra in posizione pari o dispari
009100020911     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
009200020911     C                   EVAL      Risultato = I/2
009300020911     C                   Z-ADD     Risultato     RestoDec
009400020911     C                   IF        RestoDec = *zeros                            * è pari
009500020911     C                   ADD       WrkChkDgt     SommaPari
009600020911     C                   ELSE                                                   * è dispari
009700020911     C                   ADD       WrkChkDgt     SommaDispari
009800020911     C                   ENDIF
009900020911     C                   ENDIF
010000020911     C                   ENDDO
010100020911     C*
010200081010     C* Item x lunghezza dispari
010300081010     C                   IF        *IN30 = *ON
010400020911     C                   EVAL      SommaTot = (SommaDispari * 3) + SommaPari
010500020911     C                   Z-ADD     *zeros        Multiplo10
010600020911     C                   DOW       Multiplo10 < SommaTot
010700020911     C                   EVAL      Multiplo10 = Multiplo10 + 10
010800020911     C                   ENDDO
010900081010     C                   ENDIF
011000081010     C*
011100081010     C* Item x lunghezza pari
011200081010     C                   IF        *IN30 = *OFF
011300081010     C                   EVAL      SommaTot = (SommaPari * 3) + SommaDispari
011400081010     C                   Z-ADD     *zeros        Multiplo10
011500081010     C                   DOW       Multiplo10 < SommaTot
011600081010     C                   EVAL      Multiplo10 = Multiplo10 + 10
011700081010     C                   ENDDO
011800081010     C                   ENDIF
011900020911     C*
012000020911     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
012100020911     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
012200020911     C*
012300020911     C* Imposto i valori calcolati in output a video
012400081010     C                   eval      WrkChkDgtAlfaT = %trim(InBarcode) +
012500081010     C                                              WrkChkDgtAlfa
012600081010     C   20WrkChkDgtAlfa dsply
012700081010     C   20WrkChkDgtAlfaTdsply
012800020911     C*
012900020911     C                   ENDSR
013000020911     C****************************************************************
013100081010
013200081010
013300081010
013400081010     C****************************************************************
013500081010     C** CTREAN_D ** Calcolo check-digits secondo standard EAN/UCC-13 - somma DISPARI
013600081010     C****************************************************************
013700081010     C     CTREAN_D      BEGSR
013800081010     C*
013900081010     C* Verifico se il bacose ha lunghezza pari o dispari
014000081010     C                   EVAL      Risultato = %len(%trim(InBarcode))/2
014100081010     C                   Z-ADD     Risultato     RestoDec
014200081010     C                   IF        RestoDec = *zeros                            * è pari
014300081010     C                   SETOFF                                       30
014400081010     C                   ELSE
014500081010     C                   SETON                                        30
014600081010     C                   ENDIF
014700081010     C*
014800081010     C                   z-add     *zeros        lung
014900081010     C                   eval      lung = %len(%trim(InBarcode))
015000081010     C*
015100081010     C                   CLEAR                   SkChkDgt
015200081010     C                   MOVEA     InBarcode     SkChkDgt
015300081010     C* Inizializzo variabili di wrk
015400081010     C                   Z-ADD     *zeros        WrkChkDgt
015500081010     C                   Z-ADD     *zeros        WrkChkDgtNum
015600081010     C                   MOVEL     *blanks       WrkChkDgtAlfa
015700081010     C                   MOVEL     *blanks       WrkChkDgtAlfaT
015800081010     C                   Z-ADD     *zeros        SommaPari
015900081010     C                   Z-ADD     *zeros        SommaDispari
016000081010     C                   Z-ADD     *zeros        SommaTot
016100081010     C                   Z-ADD     *zeros        Risultato
016200081010     C*
016300081010     C* Effettuo il calcolo
016400081010     C     1             DO        lung          I
016500081010     C                   IF        SkChkDgt(I) <> *blanks
016600081010     C*
016700081010     C* Stabilisco se trattasi di cifra in posizione pari o dispari
016800081010     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
016900081010     C                   EVAL      Risultato = I/2
017000081010     C                   Z-ADD     Risultato     RestoDec
017100081010     C                   IF        RestoDec = *zeros                            * è pari
017200081010     C                   ADD       WrkChkDgt     SommaPari
017300081010     C                   ELSE                                                   * è dispari
017400081010     C                   ADD       WrkChkDgt     SommaDispari
017500081010     C                   ENDIF
017600081010     C                   ENDIF
017700081010     C                   ENDDO
017800081010     C*
017900081010     C* Item x lunghezza dispari
018000081010     C                   IF        *IN30 = *ON
018100081010     C                   EVAL      SommaTot = (SommaDispari * 3) + SommaPari
018200081010     C                   Z-ADD     *zeros        Multiplo10
018300081010     C                   DOW       Multiplo10 < SommaTot
018400081010     C                   EVAL      Multiplo10 = Multiplo10 + 10
018500081010     C                   ENDDO
018600081010     C                   ENDIF
018700081010     C*
018800081010     C* Item x lunghezza pari
018900081010     C                   IF        *IN30 = *OFF
019000081010     C                   EVAL      SommaTot = (SommaPari * 3) + SommaDispari
019100081010     C                   Z-ADD     *zeros        Multiplo10
019200081010     C                   DOW       Multiplo10 < SommaTot
019300081010     C                   EVAL      Multiplo10 = Multiplo10 + 10
019400081010     C                   ENDDO
019500081010     C                   ENDIF
019600081010     C*
019700081010     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
019800081010     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
019900081010     C*
020000081010     C* Imposto i valori calcolati in output a video
020100081010     C                   eval      WrkChkDgtAlfaT = %trim(InBarcode) +
020200081010     C                                              WrkChkDgtAlfa
020300081010     C   20WrkChkDgtAlfa dsply
020400081010     C   20WrkChkDgtAlfaTdsply
020500081010     C*
020600081010     C                   ENDSR
020700081010     C****************************************************************
020800020911
020900020911
021000020911
021100020911     C****************************************************************
021200020911     C** ENDPGM    ** Uscita programma                             **
021300020911     C****************************************************************
021400020911     C     ENDPGM        BEGSR
021500020911     C*
021600020911     C                   seton                                        LR
021700020911     C*
021800020911     C                   ENDSR
021900020911     C****************************************************************
