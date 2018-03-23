000100990910     h DECEDIT('0,') DATEDIT(*DMY.)
000200990910
000300990913     fFNARBT0F  IP   E             DISK     prefix(O)
000400990913     fFIARBT0F  O    E             DISK
000500990913     fFIARBU0F  O    E             DISK
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
004500990914     c                   eval      ARBsv1 = OARBsv1
004600990914     c                   eval      ARBva1 = OARBva1
004700990914     c                   eval      ARBsv2 = OARBsv2
004800990914     c                   eval      ARBva2 = OARBva2
004900990914     c                   eval      ARBsv3 = OARBsv3
005000990914     c                   eval      ARBva3 = OARBva3
005100990914     c                   eval      ARBimv = OARBimv
005200990914     c                   eval      ARBali = OARBali
005300990914     c                   eval      ARBcei = OARBcei
005400990914     c                   eval      ARBift = OARBift
005500990914     c                   eval      ARBnft = OARBnft
005600990914     c                   eval      ARBdft = OARBdft
005700990914     c                   eval      ARBfiv = OARBfiv
005800990914     c                   eval      ARBfst = OARBfst
005900990914      *
006000990914      * Trascodifico PORTO, INOLTRO e DIRITTO FISSO
006100990914     c                   exsr      TRAS_porto
006200991122      *  IVA
006300991122     c                   if        OARBiva > *zeros
006400991122     c                   eval      Wsvn = '8'
006500991122     c                   eval      Wvan = OARBiva
006600991122     c                   exsr      aggvar
006700991122     c                   endif
006800991122      *  BOLLO
006900991122     c                   if        OARBbol > *zeros
007000991122     c                   eval      Wsvn = '6'
007100991122     c                   eval      Wvan = OARBbol
007200991122     c                   exsr      aggvar
007300991122     c                   endif
007400990910      *
007500990910      * Se ho almeno un importo scrivo la divisa
007600990914     c                   if        ARBpor > *zeros  or  ARBva1> *zeros
007700990914     c                   eval      ARBdiv = 'ITL'
007800990910     c                   endif
007900990910      *
008000990910      * Se ho l'anticipata stampo una segnalazione
008100990914     c                   if        OARBant > *zeros
008200990910     c                   except    ANTIC
008300990910     c                   endif
008400990910      *
008500990914     c                   WRITE     FIARBt00
008600990910      *--------------------------------------------------------------------------------------------*
008700990910     c     AGGVAR        BEGSR
008800990910
008900990910     c                   SELECT
009000990910      *
009100990914     c                   WHEN        ARBsv1 = *blanks
009200990914     c                   eval      ARBsv1 = Wsvn
009300990914     c                   eval      ARBva1 = Wvan
009400990910      *
009500990914     c                   WHEN        ARBsv2 = *blanks
009600990914     c                   eval      ARBsv2 = Wsvn
009700990914     c                   eval      ARBva2 = Wvan
009800990910      *
009900990914     c                   WHEN        ARBsv3 = *blanks
010000990914     c                   eval      ARBsv3 = Wsvn
010100990914     c                   eval      ARBva3 = Wvan
010200990910      *
010300990910     c                   OTHER
010400990914     c                   eval      ARBsvn = Wsvn
010500990914     c                   eval      ARBvan = Wvan
010600990914     c                   write     FIARBu00
010700990910      *
010800990910     c                   ENDSL
010900990910
011000990910     c                   ENDSR
011100990914      *--------------------------------------------------------------------------------------------*
011200990914     c     TRAS_PORTO    BEGSR
011300990914      *
011400990914    1c                   IF        OARBpor <= 99999999
011500990914     c                   eval      ARBpor = OARBpor                             Porto
011600990914      *
011700990914     c                   if        OARBinl > *zeros                             Inoltro
011800990914     c                   eval      Wsvn = '2'
011900990914     c                   eval      Wvan = OARBinl
012000990914     c                   EXSR      aggvar
012100990914     c                   endif
012200990914      *
012300990914     c                   if        OARBdif > *zeros                             Diritto Fisso
012400990914     c                   eval      Wsvn = 'H'
012500990914     c                   eval      Wvan = OARBdif
012600990914     c                   EXSR      aggvar
012700990914     c                   endif
012800990914      *
012900990914      * porto superiore al nuovo limite degli interi (da 9 a 8)
013000990914   x1C                   ELSE
013100990914      *   stampo segnalazione
013200990914     c                   except    bolla
013300990914      *   provo a spostare la differenza nell'inoltro
013400990914     c                   eval      ARBpor =  99999000
013500990914     c                   eval      comoinl = OARBinl + (OARBpor - 99999000)
013600990914      *  Inoltro entro il limite
013700990914    2c                   IF        comoinl <= 99999999
013800990914     c                   eval      Wsvn = '2'
013900990914     c                   eval      Wvan = comoinl
014000990914     c                   EXSR      aggvar
014100990914      *
014200990914     c                   if        OARBdif > 0                                  Diritto Fisso
014300990914     c                   eval      Wsvn = 'H'
014400990914     c                   eval      Wvan = OARBdif
014500990914     c                   EXSR      aggvar
014600990914     c                   endif
014700990914      *
014800990914      *  Inoltro superiore al nuovo limite degli interi (da 9 a 8)
014900990914   x2C                   ELSE
015000990914      *     scrivo la varia inoltro e sposto la differenza nel diritto fisso
015100990914     c                   eval      Wsvn = '2'
015200990914     c                   eval      Wvan = 99999000
015300990914     c                   EXSR      aggvar
015400990914      *     sposto la differenza nel diritto fisso
015500990914     c                   eval      comores = comoinl - 99999000
015600990914     c                   eval      comodif = OARBdif + comores
015700990914      *
015800990914      *  Diritto fisso entro il limite
015900990914    3c                   IF        comodif <= 99999999
016000990914     c                   eval      Wsvn = 'H'
016100990914     c                   eval      Wvan = comodif
016200990914     c                   exsr      aggvar
016300990914   X3c                   ELSE
016400990914      *  Diritto fisso superiore al limite degli interi (da 9 a 8). Scrivo varia com' è e stampo.
016500990914     c                   if        OARBdif > 0
016600990914     c                   eval      Wsvn = 'H'
016700990914     c                   eval      Wvan = OARBdif
016800990914     c                   EXSR      aggvar
016900990914     c                   endif
017000990914      * stampo
017100990914     c                   except    bollanoag
017200990914   E3c                   ENDIF
017300990914      *
017400990914   E2c                   ENDIF
017500990914   E1c                   ENDIF
017600990914
017700990914     c                   ENDSR
017800990910      *--------------------------------------------------------------------------------------------*
017900990910     c     *INZSR        BEGSR
018000990910
018100990913     c     KBOLLE        KLIST
018200990914     c                   KFLD                    OARBaas
018300990914     c                   KFLD                    OARBlnp
018400990914     c                   KFLD                    OARBnrs
018500990914     c                   KFLD                    OARBnsp
018600990913
018700990910     c                   ENDSR
018800990910      *--------------------------------------------------------------------------------------------*
018900990910     OPRTF198   E            ANTIC       1
019000990913     o                                        +   2 'Bolla con anticipata'
019100990914     o                       OARBaas       Z  +   3
019200990914     o                       OARBlnp       Z  +   1
019300990914     o                       OARBnrs       Z  +   1
019400990914     o                       OARBnsp       Z  +   1
019500990914     o                       OARBant       2  +   2
019600990914     O          E            BOLLA       1
019700991123     o                                              'FNARBT con porto > 99.999.-
019800991123     o                                              999'
019900990914     o                       OARBaas       Z  +   1
020000990914     o                       OARBlnp       Z  +   1
020100990914     o                       OARBnrs       Z  +   1
020200990914     o                       OARBnsp       Z  +   1
020300990914     o                       OARBpor       2  +   1
020400990914     o                       OARBinl       2  +   1
020500990914     o                       OARBdif       2  +   1
020600990914     o                       OARBsv1          +   1
020700990914     o                       OARBva1       2  +   1
020800990914     o                       OARBsv2          +   1
020900990914     o                       OARBva2       2  +   1
021000990914     o                       OARBsv3          +   1
021100990914     o                       OARBva3       2  +   1
021200990914     O          E            BOLLAnoag   1
021300990914     o                                              '                          '
021400990914     o                       OARBaas       Z  +   1
021500990914     o                       OARBlnp       Z  +   1
021600990914     o                       OARBnrs       Z  +   1
021700990914     o                       OARBnsp       Z  +   1
021800990914     o                       OARBpor       2  +   1
021900990914     o                       OARBinl       2  +   1
022000990914     o                       OARBdif       2  +   1
022100990914     o                       OARBsv1          +   1
022200990914     o                       OARBva1       2  +   1
022300990914     o                       OARBsv2          +   1
022400990914     o                       OARBva2       2  +   1
022500990914     o                       OARBsv3          +   1
022600990914     o                       OARBva3       2  +   1
022700990914     o                                        +   1 'Importo non aggiornato:'
022800990914     o                       comores       2  +   1
