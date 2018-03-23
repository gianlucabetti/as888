000100990910     h DECEDIT('0,') DATEDIT(*DMY.)
000200990910
000300991027     fFNARBT0E  IP   E             DISK     prefix(O)
000400991027     fFIARBT0E  O    E             DISK
000500991027     fFIARBU0E  O    E             DISK
000600990910     fprtf198   O    F  198        PRINTER  oflind(*INOF)
000700990910
000800990914     d Ktrc            S                   LIKE(ARBtrc)  INZ('1')
000900990914     d Wsvn            S                   LIKE(ARBsvn)
001000990914     d Wvan            S                   LIKE(ARBvan)
001100990914     d comoinl         S                   LIKE(OARBPOR)
001200990914     d comodif         S                   LIKE(OARBPOR)
001300990914     d comores         S                   LIKE(OARBPOR)
001400990910      *-----------------------------------------------------------------------------------------
001500990910
001600990914     c                   clear                   fiarbt00
001700990914     c                   clear                   fiarbu00
001800990910      *
001900990914     c                   eval      ARBatb = OARBatb
002000990914     c                   eval      ARBaas = OARBaas
002100990914     c                   eval      ARBlnp = OARBlnp
002200990914     c                   eval      ARBnrs = OARBnrs
002300990914     c                   eval      ARBnsp = OARBnsp
002400990914     c                   eval      ARBtrc = KTRC
002500990914     c                   eval      ARBcbo = OARBcbo
002600990914     c                   eval      ARBdtv = OARBdtv
002700990914     c                   eval      ARBorv = OARBorv
002800990914     c                   eval      ARBcvb = OARBcvb
002900990914     c                   eval      ARBpru = OARBpru
003000990914     c                   eval      ARBksc = OARBksc
003100990914     c                   eval      ARBctr = OARBctr
003200990914     c                   eval      ARBqft = OARBqft
003300990914     c                   eval      ARBvmd = OARBvmd
003400990914     c                   if          ARBvmd > *zeros  and  OARBvad = *blanks
003500990914     c                   eval      ARBvad = 'ITL'
003600990914     c                   else
003700990914     c                   eval      ARBvad = OARBvad
003800990914     c                   endif
003900990914     c                   eval      ARBias = OARBias
004000990914     c                   if          ARBias > *zeros  and  OARBvas = *blanks
004100990914     c                   eval      ARBvas = 'ITL'
004200990914     c                   else
004300990914     c                   eval      ARBvas = OARBvas
004400990914     c                   endif
004500991027     c                   eval      ARBiap = OARBiap
004600991027     c                   if          ARBiap > *zeros  and  OARBvap = *blanks
004700991027     c                   eval      ARBvap = 'ITL'
004800991027     c                   else
004900991027     c                   eval      ARBvap = OARBvap
005000991027     c                   endif
005100990914     c                   eval      ARBsv1 = OARBsv1
005200990914     c                   eval      ARBva1 = OARBva1
005300990914     c                   eval      ARBsv2 = OARBsv2
005400990914     c                   eval      ARBva2 = OARBva2
005500990914     c                   eval      ARBsv3 = OARBsv3
005600990914     c                   eval      ARBva3 = OARBva3
005700990914     c                   eval      ARBimv = OARBimv
005800990914     c                   eval      ARBali = OARBali
005900990914     c                   eval      ARBcei = OARBcei
006000990914     c                   eval      ARBift = OARBift
006100990914     c                   eval      ARBnft = OARBnft
006200990914     c                   eval      ARBdft = OARBdft
006300990914     c                   eval      ARBfiv = OARBfiv
006400991027     c                   eval      ARBlna = OARBlna
006500991027     c                   eval      ARBfev = OARBfev
006600991027     c                   eval      ARBdat = OARBdat
006700991027     c                   eval      ARBder = OARBder
006800991027     c                   eval      ARBvar = OARBvar
006900991122      *
007000991122      * Trascodifico PORTO, INOLTRO e DIRITTO FISSO
007100991122     c                   exsr      TRAS_porto
007200990910      *  IVA
007300990914     c                   if        OARBiva > *zeros
007400990914     c                   eval      Wsvn = '8'
007500990914     c                   eval      Wvan = OARBiva
007600990910     c                   exsr      aggvar
007700990910     c                   endif
007800990910      *  BOLLO
007900990914     c                   if        OARBbol > *zeros
008000990914     c                   eval      Wsvn = '6'
008100990914     c                   eval      Wvan = OARBbol
008200990910     c                   exsr      aggvar
008300990910     c                   endif
008400990910      *
008500990910      * Se ho almeno un importo scrivo la divisa
008600990914     c                   if        ARBpor > *zeros  or  ARBva1> *zeros
008700990914     c                   eval      ARBdiv = 'ITL'
008800990910     c                   endif
008900990910      *
009000990910      * Se ho l'anticipata stampo una segnalazione
009100990914     c                   if        OARBant > *zeros
009200990910     c                   except    ANTIC
009300990910     c                   endif
009400990910      *
009500990914     c                   WRITE     FIARBt00
009600990910      *--------------------------------------------------------------------------------------------*
009700990910     c     AGGVAR        BEGSR
009800990910
009900990910     c                   SELECT
010000990910      *
010100990914     c                   WHEN        ARBsv1 = *blanks
010200990914     c                   eval      ARBsv1 = Wsvn
010300990914     c                   eval      ARBva1 = Wvan
010400990910      *
010500990914     c                   WHEN        ARBsv2 = *blanks
010600990914     c                   eval      ARBsv2 = Wsvn
010700990914     c                   eval      ARBva2 = Wvan
010800990910      *
010900990914     c                   WHEN        ARBsv3 = *blanks
011000990914     c                   eval      ARBsv3 = Wsvn
011100990914     c                   eval      ARBva3 = Wvan
011200990910      *
011300990910     c                   OTHER
011400990914     c                   eval      ARBsvn = Wsvn
011500990914     c                   eval      ARBvan = Wvan
011600991126     c                   write     FIARBu00                             98
011700990910      *
011800990910     c                   ENDSL
011900990910
012000990910     c                   ENDSR
012100990914      *--------------------------------------------------------------------------------------------*
012200990914     c     TRAS_PORTO    BEGSR
012300990914      *
012400990914    1c                   IF        OARBpor <= 99999999
012500990914     c                   eval      ARBpor = OARBpor                             Porto
012600990914      *
012700990914     c                   if        OARBinl > *zeros                             Inoltro
012800990914     c                   eval      Wsvn = '2'
012900990914     c                   eval      Wvan = OARBinl
013000990914     c                   EXSR      aggvar
013100990914     c                   endif
013200990914      *
013300990914     c                   if        OARBdif > *zeros                             Diritto Fisso
013400990914     c                   eval      Wsvn = 'H'
013500990914     c                   eval      Wvan = OARBdif
013600990914     c                   EXSR      aggvar
013700990914     c                   endif
013800990914      *
013900990914      * porto superiore al nuovo limite degli interi (da 9 a 8)
014000990914   x1C                   ELSE
014100990914      *   stampo segnalazione
014200990914     c                   except    bolla
014300990914      *   provo a spostare la differenza nell'inoltro
014400990914     c                   eval      ARBpor =  99999000
014500990914     c                   eval      comoinl = OARBinl + (OARBpor - 99999000)
014600990914      *  Inoltro entro il limite
014700990914    2c                   IF        comoinl <= 99999999
014800990914     c                   eval      Wsvn = '2'
014900990914     c                   eval      Wvan = comoinl
015000990914     c                   EXSR      aggvar
015100990914      *
015200990914     c                   if        OARBdif > 0                                  Diritto Fisso
015300990914     c                   eval      Wsvn = 'H'
015400990914     c                   eval      Wvan = OARBdif
015500990914     c                   EXSR      aggvar
015600990914     c                   endif
015700990914      *
015800990914      *  Inoltro superiore al nuovo limite degli interi (da 9 a 8)
015900990914   x2C                   ELSE
016000990914      *     scrivo la varia inoltro e sposto la differenza nel diritto fisso
016100990914     c                   eval      Wsvn = '2'
016200990914     c                   eval      Wvan = 99999000
016300990914     c                   EXSR      aggvar
016400990914      *     sposto la differenza nel diritto fisso
016500990914     c                   eval      comores = comoinl - 99999000
016600990914     c                   eval      comodif = OARBdif + comores
016700990914      *
016800990914      *  Diritto fisso entro il limite
016900990914    3c                   IF        comodif <= 99999999
017000990914     c                   eval      Wsvn = 'H'
017100990914     c                   eval      Wvan = comodif
017200990914     c                   exsr      aggvar
017300990914   X3c                   ELSE
017400990914      *  Diritto fisso superiore al limite degli interi (da 9 a 8). Scrivo varia com' è e stampo.
017500990914     c                   if        OARBdif > 0
017600990914     c                   eval      Wsvn = 'H'
017700990914     c                   eval      Wvan = OARBdif
017800990914     c                   EXSR      aggvar
017900990914     c                   endif
018000990914      * stampo
018100990914     c                   except    bollanoag
018200990914   E3c                   ENDIF
018300990914      *
018400990914   E2c                   ENDIF
018500990914   E1c                   ENDIF
018600990914
018700990914     c                   ENDSR
018800990910      *--------------------------------------------------------------------------------------------*
018900990910     c     *INZSR        BEGSR
019000990910
019100990913     c     KBOLLE        KLIST
019200990914     c                   KFLD                    OARBaas
019300990914     c                   KFLD                    OARBlnp
019400990914     c                   KFLD                    OARBnrs
019500990914     c                   KFLD                    OARBnsp
019600990913
019700990910     c                   ENDSR
019800990910      *--------------------------------------------------------------------------------------------*
019900990910     OPRTF198   E            ANTIC       1
020000990913     o                                        +   2 'Bolla con anticipata'
020100990914     o                       OARBaas       Z  +   3
020200990914     o                       OARBlnp       Z  +   1
020300990914     o                       OARBnrs       Z  +   1
020400990914     o                       OARBnsp       Z  +   1
020500990914     o                       OARBant       2  +   2
020600990914     O          E            BOLLA       1
020700991123     o                                              'ARBT0E con porto > 99.999.-
020800991123     o                                              999'
020900990914     o                       OARBaas       Z  +   1
021000990914     o                       OARBlnp       Z  +   1
021100990914     o                       OARBnrs       Z  +   1
021200990914     o                       OARBnsp       Z  +   1
021300990914     o                       OARBpor       2  +   1
021400990914     o                       OARBinl       2  +   1
021500990914     o                       OARBdif       2  +   1
021600990914     o                       OARBsv1          +   1
021700990914     o                       OARBva1       2  +   1
021800990914     o                       OARBsv2          +   1
021900990914     o                       OARBva2       2  +   1
022000990914     o                       OARBsv3          +   1
022100990914     o                       OARBva3       2  +   1
022200990914     O          E            BOLLAnoag   1
022300990914     o                                              '                          '
022400990914     o                       OARBaas       Z  +   1
022500990914     o                       OARBlnp       Z  +   1
022600990914     o                       OARBnrs       Z  +   1
022700990914     o                       OARBnsp       Z  +   1
022800990914     o                       OARBpor       2  +   1
022900990914     o                       OARBinl       2  +   1
023000990914     o                       OARBdif       2  +   1
023100990914     o                       OARBsv1          +   1
023200990914     o                       OARBva1       2  +   1
023300990914     o                       OARBsv2          +   1
023400990914     o                       OARBva2       2  +   1
023500990914     o                       OARBsv3          +   1
023600990914     o                       OARBva3       2  +   1
023700990914     o                                        +   1 'Importo non aggiornato:'
023800990914     o                       comores       2  +   1
