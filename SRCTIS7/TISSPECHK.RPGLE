000100041222      /TITLE Calcola check code numero spedizione
000200040610     ***********************************************************************************************
000300030709     **?
000400041222     **?Questo modulo esegue il calcolo del check code del numero spedizione.
000500030709     **?
000600041222     **?Modulo......: TISCHKSPE
000700041222     **?Descrizione.: Calcola check code numero spedizione.
000800030709     **?
000900041222     **?Compilazione: CRTRPGMOD  MODULE(GAITRAFIL/TISCHKSPE)
001000030709     **?                         SRCFILE(GAITRASRC/SRCYCON)
001100041222     **?                         SRCMBR(TISCHKSPE)
001200030709     **?                         REPLACE(*YES)
001300030709     **?
001400041222     **?              CRTSRVPGM  SRVPGM(GAITRAFIL/TISCHKSPE)
001500030709     **?                         EXPORT(*ALL)
001600030709     **?
001700041222     **?              DLTMOD     MODULE(GAITRAFIL/TISCHKSPE)
001800030709     **?
001900030709     **?              ADDBNDDIRE BNDDIR(GAITRAFIL/YNSF)
002000041222     **?                         OBJ((*LIBL/TISCHKSPE *SRVPGM))
002100030709     **?                         (da eseguire solo la prima volta)
002200030709     **?
002300040610     ***********************************************************************************************
002400030708
002500040315     H NOMAIN
002600030709
002700041222     D GetSpeChkCde    PR            10U 0
002800041222     D  Anno                          4
002900041222     D                                     VALUE
003000041221     D  IdSpedizione                 12
003100040226     D                                     VALUE
003200041222     D  ChkCode                      10U 0
003300041221     D  Esito                         5I 0
003400040610
003500040610     ***********************************************************************************************
003600040610     **?
003700041221     **?Calcolo codice controllo spedizione.
003800040611     **?
003900040611     **?Legenda degli errori:
004000041223     **?-1 = L'anno non è di 4 cife.
004100041223     **?-2 = Il numero spedizione non è di 12 cifre.
004200040610     **?
004300040610     ***********************************************************************************************
004400041222     P GetSpeChkCde    B
004500040610     P                                     EXPORT
004600040610
004700041222     D GetSpeChkCde    PI            10U 0
004800041222     D  Anno                          4
004900041222     D                                     VALUE
005000041221     D  IdSpedizione                 12
005100041221     D                                     VALUE
005200041222     D  ChkCode                      10U 0
005300041221     D  Esito                         5I 0
005400041221
005500041222     D IdDS            DS            16
005600041221     D                                     INZ
005700041221     D  Id                            1S 0
005800041222     D                                     DIM(16)
005900041222     D  IdAAS                  1      4S 0
006000041222     D  IdLnP                  5      7S 0
006100041222     D  IdNrS                  8      9S 0
006200041222     D  IdNSp                 10     16S 0
006300041221
006400041222     D WrkCurDate      S               D                                        Data corrente
006500041222     D WrkCurYear      S              5U 0                                      Anno corrente
006600041221     D i               S              5U 0
006700041222     D CtrZero         S              5U 0                                      Contatore zeri
006800041222     D SumZero         S              5U 0                                      Somma posiz. zeri
006900041222     D SumNonZero      S              5U 0                                      Somma non 0
007000041222     D CtrPari         S              5U 0                                      Contatore pari
007100041222     D SumPari         S              5U 0                                      Somma pari
007200041222     D CtrDispari      S              5U 0                                      Contatore dispari
007300041222     D SumDispari      S              5U 0                                      Somma dispari
007400040610
007500041221     C                   CLEAR                   Esito
007600041222     C                   CLEAR                   ChkCode
007700041222
007800041223     C                   IF        Anno = *BLANK OR Anno = *ZERO
007900041222     C                   TIME                    WrkCurDate
008000041222     C                   EXTRCT    WrkCurDate:*Y WrkCurYear
008100041222     C                   EVALR     Anno = %EDITC(WrkCurYear:'X')
008200041222     C                   ENDIF
008300041222
008400041222     **?Anno deve essere di 4 cifre.
008500041222     C                   TESTN                   Anno                 01
008600041222     C                   IF        NOT *IN01
008700041222     C                   EVAL      Esito = -1
008800041222     C                   RETURN    ChkCode
008900041222     C                   ENDIF
009000041221
009100041221     **?Id spedizione deve essere di 12 cifre.
009200041221     C                   TESTN                   IdSpedizione         01
009300041221     C                   IF        NOT *IN01
009400041222     C                   EVAL      Esito = -2
009500041222     C                   RETURN    ChkCode
009600041221     C                   ENDIF
009700041221
009800041221     **?Imposto le variabili per il calcolo.
009900041221     C                   CLEAR                   CtrZero
010000041222     C                   CLEAR                   SumZero
010100041221     C                   CLEAR                   SumNonZero
010200041221     C                   CLEAR                   CtrPari
010300041221     C                   CLEAR                   SumPari
010400041221     C                   CLEAR                   CtrDispari
010500041221     C                   CLEAR                   SumDispari
010600041221
010700041222     C                   EVAL      IdDS = Anno + IdSpedizione
010800041222     C                   EVAL      ChkCode = IdLnP + IdNrS
010900041222
011000041222     C                   DO        16            i
011100041221     C
011200041221     C                   IF        Id(i) = 0
011300041221     C                   EVAL      CtrZero = CtrZero + 1
011400041222     C                   EVAL      SumZero = SumZero + i
011500041221     C                   ELSE
011600041221     C                   EVAL      Id(i) = 10 - Id(i)
011700041222     C                   EVAL      SumNonZero = SumNonZero + Id(i) + i
011800041221     C                   ENDIF
011900041222     **?Conto le cifre pari e dispari e ne calcolo la sommatoria.
012000041221     C                   IF        Id(i) = 0 OR Id(i) = 2 OR Id(i) = 4 OR
012100041221     C                             Id(i) = 6 OR Id(i) = 8
012200041221     C                   EVAL      CtrPari = CtrPari + 1
012300041222     C                   EVAL      SumPari = SumPari + Id(i) + i
012400041221     C                   ELSE
012500041221     C                   EVAL      CtrDispari = CtrDispari + 1
012600041222     C                   EVAL      SumDispari = SumDispari + Id(i) + i
012700041221     C                   ENDIF
012800041221     C
012900041221     C                   ENDDO
013000041221
013100041221     **?Calcolo codice controllo (niente di fantascientifico).
013200041221     C                   IF        CtrZero > 0
013300041222     C                   EVAL      ChkCode = ChkCode * CtrZero
013400041221     C                   ENDIF
013500041221     C
013600041221     C                   IF        CtrPari > 0
013700041222     C                   EVAL      ChkCode = ChkCode * CtrPari
013800041221     C                   ENDIF
013900041221     C
014000041221     C                   IF        SumPari > 0
014100041222     C                   EVAL      ChkCode = ChkCode * SumPari
014200041221     C                   ENDIF
014300041221     C
014400041221     C                   IF        CtrDispari > 0
014500041222     C                   EVAL      ChkCode = ChkCode * CtrDispari
014600041221     C                   ENDIF
014700041221     C
014800041221     C                   IF        SumDispari > 0
014900041222     C                   EVAL      ChkCode = ChkCode * SumDispari
015000041221     C                   ENDIF
015100041221     C
015200041222     C                   EVAL      ChkCode = ChkCode + CtrZero + CtrPari +
015300041222     C                             CtrDispari + SumZero + SumNonZero + SumPari +
015400041221     C                             SumDispari
015500040610
015600041222     C                   RETURN    ChkCode
015700040610
015800041222     P GetSpeChkCde    E
