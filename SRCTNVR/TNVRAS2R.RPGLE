000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030929     FTITAS30C  IF   E           K DISK
000300030930     FTITA730C  IF   E           K DISK    ignore(tita7p00)
000400030930     FTITAA30C  IF   E           K DISK    IGNORE(TITAaP00)
000500030930     FTIASC03L  IF   E           K DISK
000600030930     FTIASC999  UF A E             DISK    rename(tiasc000:tiasc99)
000700980709     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000800030929     D PARAMT          DS
000900030929     D  PARCA                256    256
001000030929     D KPJBA         E DS
001100030929     D DTAS          E DS
001200030930     D  dstasflo     E                     extfld(tasflo)
001300030929     D DTASV         E DS
001400030929     D  SV                     1     20
001500030929     D                                     DIM(20)                              SIGLE VARIE
001600030929     D  VA                    21    140P 3
001700030929     D                                     DIM(20)                              IMPORTI VARIE
001800030930     D TIBS69        E DS                  EXTNAME(TIBS69DS)
001900030930     D CNACO         E DS                  EXTNAME(CNACO00F)
002000030930     D CNIND         E DS                  EXTNAME(CNIND00F)
002100030930     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002200030930     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002300030930
002400030930     d §trc            s                   like(taatrc)
002500961108     C*
002600030929     C     *ENTRY        PLIST
002700030929     C                   PARM                    KPJBA
002800980703     C     KASC          KLIST
002900030929     C                   KFLD                    ascAAS
003000030929     C                   KFLD                    ascLNP
003100030929     C                   KFLD                    ascNRS
003200030929     C                   KFLD                    ascNSP
003300030929     C                   KFLD                    ascTBL
003400030930     c*
003500030930     C     KTAA          KLIST
003600030930     C                   KFLD                    TASAAS
003700030930     C                   KFLD                    TASLNP
003800030930     C                   KFLD                    TASNRS
003900030930     C                   KFLD                    TASNSP
004000030930     C                   KFLD                    §trc
004100030930     c*
004200030929      *
004300030929     C                   Z-ADD     20030731      DFT               8 0
004400980703     C**
004500030930     C     dft           SETLL     TIasc03l
004600980703     C**
004700030929    1C                   DO        *hival
004800030930     C                   READ      TIasc03l
004900030929      *
005000030930     c                   if        %eof
005100030929     c                   leave
005200030929     c                   endif
005300030929      *
005400030929     c                   if        ascdft > 20030801
005500030929     c                   leave
005600030929     c                   endif
005700030929      *
005800030929     c                   clear                   dtas
005900030929     c                   clear                   dtasv
006000030929      * aggancio titas per verificare se fattura di sede
006100980703     C**
006200030929     C     Kasc          chain     TITAS30C
006300030929      *
006400030930    2c                   if        %found(titas30c)
006500030929      * verifico registro iva
006600030929     c                   if        tasfiv < 900
006700030929     c                   iter
006800030929     c                   endif
006900030929      *
007000030929     C** PULIZIA SCHIERE
007100030930    3c                   if        tassv3 <> *blanks
007200030929      *
007300030929     C     Kasc          SETLL     TITA7000
007400030930     C                   Z-ADD     3             K                 2 0
007500030929     C**
007600030929     C     SUTA7         TAG
007700030929     C     Kasc          READE     TITA7000
007800030930     C                   if        not %eof
007900030929     C                   ADD       1             K
008000030929     C                   MOVEL     TA7SVN        SV(K)
008100030929     C                   Z-ADD     TA7VAN        VA(K)
008200030929     C                   GOTO      SUTA7
008300030929     c                   endif
008400030930    3c                   endif
008500030930      * data spedizione
008600030930     c                   movel     tasaas        tasdsp
008700030930     c                   move      tasmgs        tasdsp
008800030930     C                   Z-ADD     dft           tasdct
008900030930      * se importo d'assicurare inserito in fatturazione non lo passo al TNSF20R
009000030930     c                   if        tasfcm = 'F'
009100030930     c                   clear                   tasias
009200030930     c                   clear                   tasvas
009300030930     c                   endif
009400030929      *
009500030929      * richiamo TNSF20R
009600030929     c                   eval      tassva = 'R'
009700030929     C                   MOVEL     'S'           PARCA
009800030929     C                   MOVEL     PARAMT        KPJBU
009900030929      *
010000030929     c                   call      'TNSF20R'
010100030929     C                   PARM                    KPJBA
010200030929     C                   PARM                    DTAS
010300030929     C                   PARM                    DTASV
010400030929     c*
010500030930    3c                   if        taserr = ' '
010600030930    4C     TASK88        IFEQ      'S'
010700030930     C     TASIAS        ANDGT     0
010800030930     C                   SETON                                        87
010900030930     C                   EXSR      MEMASS
011000030930    4C                   ENDIF
011100030930     C**
011200030930     C** CLIENTE CODIFICATO
011300030930    4C     TASK88        IFEQ      'N'
011400030930     C                   SETOFF                                       87
011500030930     C** MEMORIZZATA BOLLA IN TNASC00F
011600030930     C                   EXSR      MEMASS
011700030930    4C                   ENDIF
011800030930    3C                   ENDIF
011900030930     c*
012000030930    2c                   endif
012100030930    1c                   enddo
012200030930     C*
012300030930     C                   CLEAR                   TIBS69
012400030930     C                   MOVEL     'C'           I69TLA
012500030930     C                   CALL      'TIBS69R'
012600030930     C                   PARM                    TIBS69
012700030930     C                   PARM                    CNACO
012800030930     C                   PARM                    CNIND
012900030930     C                   PARM                    CNCLP
013000030930     C                   PARM                    FNCLS
013100000000     C                   SETON                                        LR
013200030930     C********************************************************
013300030930     C** MEMORIZZO SU TNASC00F LE BOLLE CON ASS. PER ASS. X CONTO
013400030930     C********************************************************
013500030930     C     MEMASS        BEGSR
013600030930     c                   clear                   tiasc99
013700030930     C                   Z-ADD     TASLNP        ASCLNP
013800030930     C                   Z-ADD     TASNRS        ASCNRS
013900030930     C                   Z-ADD     TASNSP        ASCNSP
014000030930     C                   MOVEL     TASTBL        ASCTBL
014100030930     C                   Z-ADD     TASKSC        ASCKSC
014200030930     C                   MOVEL     TASDSP        ANNODS            4 0
014300030930     C                   MOVE      TASDSP        MGS               4 0
014400030930     C                   Z-ADD     MGS           ASCMGS
014500030930     C                   MOVE      ANNODS        ASCAAS
014600030930     C                   Z-ADD     TASNFT        ASCNFT
014700030930     C                   Z-ADD     TASDFT        ASCDFT
014800030930     C                   Z-ADD     0             ASCDST
014900030930      * valorizzo la divisa dell'assicurazione uguale alla divisa del cliente
015000030930     C                   MOVEL     TASDIV        ASCVAS
015100030930      *
015200030930     C                   Z-ADD     TASIAL        ASCIAS
015300030930     C** N10 --> CLIENTE VARIO 8888
015400030930     C                   MOVE      TASKSC        KSCCOD            4 0
015500030930     C     KSCCOD        IFEQ      8888
015600030930     C                   SETOFF                                       10
015700030930     c                   clear                   titaa000
015800030930     c                   movel     'M'           §trc
015900030930     c     ktaa          chain     titaa000                           05
016000030930     c                   else
016100030930     c                   seton                                        10
016200030930     c                   endif
016300030930     C  N10              MOVEL     taarsc        ASCRSD
016400030930     C  N10              MOVEL     taaind        ASCIND
016500030930     C  N10              MOVEL     taacap        ASCCAP
016600030930     C  N10              MOVEL     taaloc        ASCLOC
016700030930     C**
016800030930     C   10              MOVEL     *BLANKS       ASCRSD
016900030930     C   10              MOVEL     *BLANKS       ASCIND
017000030930     C   10              MOVEL     *BLANKS       ASCCAP
017100030930     C   10              MOVEL     *BLANKS       ASCLOC
017200030930
017300030930     C                   IF        TASSPC='S'
017400030930     c                   eval      ascpkg=TaspkcN
017500030930     c                   else
017600030930     c                   eval      ascpkg=taspkf
017700030930     C                   ENDIF
017800030930     C*
017900030930     C* 87 ON --> CLIENTE CODIDICATO MA VARIO PER L'ASSICURAZIONE
018000030930     C*   SENZA MANDATO O CON MANDATO INFERIORE ALL'IMPORTO INDICATO
018100030930     C*   IN BOLLA
018200030930     C   87              DO
018300030930     C                   MOVE      8888          ASCKSC
018400030930      * CHIAMO TIBS69 PER RICERCA RAGIONE SOCIALE CLIENTE
018500030930     C                   CLEAR                   TIBS69
018600030930     C                   CLEAR                   CNACO
018700030930     C                   CLEAR                   CNIND
018800030930     C                   CLEAR                   CNCLP
018900030930     C                   CLEAR                   FNCLS
019000030930     C                   MOVEL     ' '           I69TLA
019100030930     C                   MOVEL     KNSIF         I69SIF
019200030930     C                   Z-ADD     tasksc        I69KAC
019300030930     C                   Z-ADD     tasksc        I69KIN
019400030930     C                   CALL      'TIBS69R'
019500030930     C                   PARM                    TIBS69
019600030930     C                   PARM                    CNACO
019700030930     C                   PARM                    CNIND
019800030930     C                   PARM                    CNCLP
019900030930     C                   PARM                    FNCLS
020000030930     C     O69ERR        IFEQ      *BLANK
020100030930     C                   MOVEL     ACORAG        ASCRSD
020200030930     C                   MOVEL     indVIA        ASCIND
020300030930     C                   MOVEL     indCAP        ASCCAP
020400030930     C                   MOVEL     indCIT        ASCLOC
020500030930     C                   ENDIF
020600030930     C                   END
020700030930     C                   Z-ADD     0             ASCVRR
020800030930     C                   Z-ADD     1             K
020900030930     C     'R'           LOOKUP    SV(K)                                  05
021000030930     C   05              Z-ADD     VA(K)         ASCVRR
021100030930      * valorizzo la divisa della varia "R"
021200030930     C   05              move      tasdiv        ascvvr
021300030930
021400030930     C                   WRITE     TIASC99
021500030930     C                   SETOFF                                       87
021600030930     C                   ENDSR
021700980709     OPRTF198   E            ERR1        1
021800980709     O                                         +  1 'BOLLA:'
021900980709     O                       TASKSC        Z   +  2
022000980709     O                       TASAAS        Z   +  2
022100980709     O                       TASLNP        Z   +  2
022200980709     O                       TASNRS        Z   +  2
022300980709     O                       TASNSP        Z   +  2
022400980709     O                       TASTBL            +  2
022500980709     O                       TASLNA        Z   +  2
022600980709     O                       TASIAS        4   +  2
022700980709     O                       TASNFT        Z   +  2
022800980709     O                       TASDFT            +  2 '    /  /  '
022900980709     O                                         +  2 'NON TROVATO TNASC'
