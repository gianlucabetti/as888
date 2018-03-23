000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300040319     fFiar530c  if   e           k Disk
000400040319     f                                     Ignore(fiar5p00)
000500040319     fTitas30c  if   e           k Disk
000600040319     f                                     Ignore(Titas000)
000700030206     f                                     Ignore(Titasp00)
000800040319     fsisde01l  uf a e           k Disk
000900040319     ftabel00f  if   e           k Disk
001000040319     fazorg01l  if   e           k Disk
001100040319     D* FILIALI CON RELATIVO RAGGRUPPAMENTO REGIONE
001200040319     D FIL             S              3  0 DIM(500)
001300040319     D RRN             S              3  0 DIM(500)
001400040319     D FPC             S              2    DIM(20)
001500040319     D KIC             S              6  0 DIM(20)
001600040319     D KFC             S              6  0 DIM(20)
001700040319     D RBL             S              1    DIM(50)
001800040319     D TPO             S              1    DIM(50)
001900040319     D Tbl             S              2    DIM(50)
002000040319
002100040319     D Ds14          E DS
002200040319     D Ds2l          E DS
002300040319     D Dstb          E DS
002400040319     D DAR5BNB       E DS
002500040319     D DAR5BNBSDE    E DS
002600040319     D DAR5BAN       E DS
002700040319     D DAR5BANSDE    E DS
002800040319
002900030206
003000040319     C     2004          Setll     fiar530c
003100040319     C     2004          reade     fiar530c
003200040319     c                   Dow       NOT %eof(fiar530c)
003300040319     c                   if        ar5trd = 'BNB' or ar5trd = 'BAN'
003400040319     c     keytas        chain     titas30c
003500040319     c                   if        %found(titas30c)
003600040319     c                   clear                   sisde000
003700040319     c                   select
003800040319     c                   when      %subst(tasgva: 1: 1) = 'B'
003900040319     c                   movel     ar5uni        dar5ban
004000040319     c                   movel     'BAN'         sdetrd
004100040319     c                   exsr      aggsde
004200040319     c                   when      %subst(tasgva: 1: 1) = 'O'
004300040319     c                   movel     ar5uni        dar5bnb
004400040319     c                   movel     'BNB'         sdetrd
004500040319     c                   exsr      aggsde
004600040319     c                   endsl
004700040319     c                   endif
004800040319     c                   endif
004900040319     C     2004          reade     fiar530c
005000040319     c                   enddo
005100030206     c                   Eval      *InLr = *On
005200040319     c**********************************************************************
005300040319     c     aggsde        begsr
005400040319     c**********************************************************************
005500040319     c*
005600040319     C* RECUPERO LA REGIONE PARTENZA
005700040319     c                   z-add     1             i
005800040319     C     taslnp        LOOKUP    FIL(I)                                 99
005900040319     C                   Z-ADD     RRN(I)        sderep
006000040319     C*
006100040319     C* RECUPERO LA REGIONE DI ARRIVO
006200040319     c                   z-add     1             i
006300040319     C     taslna        LOOKUP    FIL(I)                                 99
006400040319     C                   Z-ADD     RRN(I)        sderea
006500040319     C*
006600040319     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
006700040319     C     taspkf        IFEQ      *ZEROS
006800040319     C                   MOVEL     FPC(1)        sdefpe
006900040319     C                   ELSE
007000040319     C                   DO        20            I
007100040319     C     taspkf        IFGT      KIC(I)
007200040319     C     taspkf        ANDLE     KFC(I)
007300040319     C                   MOVEL     FPC(I)        sdefpe
007400040319     C                   LEAVE
007500040319     C                   ENDIF
007600040319     C                   ENDDO
007700040319     C                   ENDIF
007800040319     C*
007900040319     C* tipo bolla e raggruppamento bolla
008000040319     C                   Z-ADD     1             I                 6 0
008100040319     C     tastbl        LOOKUP    TBL(I)                                 99
008200040319     C                   MOVEL     TPO(I)        sdeTPO
008300040319     C                   MOVEL     RBL(I)        sdeRBL
008400040319
008500040319     c                   movel     'SDC'         sDEFILE
008600040319     c                   z-add     tasksc        sDEKSC
008700040319     c                   z-add     tasaas        sDEANN
008800040319     c                   movel     tasmgs        sDEMES
008900040319     c                   movel     tastsp        sDETSP
009000040319     c                   z-add     tasctr        sDECTR
009100040319     c                   clear                   sDETPR
009200040319     c* aggancio SISDE01l
009300040319     C     KEYSDE        CHAIN     SISDE01L                           99
009400040319     C     *IN99         IFEQ      '0'
009500040319     C                   EXSR      ADDSDE
009600040319     C                   UPDATE    SISDE000
009700040319     C                   ELSE
009800040319     C                   EXSR      AZZSDE
009900040319     C                   EXSR      ADDSDE
010000040319     C                   WRITE     SISDE000
010100040319     C                   END
010200040319     c
010300040319     c                   endsr
010400040319     C********************************************************************
010500040319     C*   AZZSDE - AZZERA I CAMPI DEL FILE SISDE00F (ESTENSIONE SALDI)      )
010600040319     C********************************************************************
010700040319     C     AZZSDE        BEGSR
010800040319     c                   select
010900040319     c* tipo record BNB
011000040319     C                   when      sdetrd='BNB'
011100040319     C                   CLEAR                   DAR5BNBSDE
011200040403     C                   eval      sdeuni = DAR5BNBSDE
011300040319     c* tipo record BAN
011400040319     C                   when      sdetrd='BAN'
011500040319     C                   CLEAR                   DAR5BANSDE
011600040403     C                   eval      sdeuni = DAR5BANSDE
011700040319     C                   ENDsl
011800040319     C                   MOVEL     *BLANKS       SDEFTR
011900040319     C                   Z-ADD     *ZEROS        SDEDTR
012000040319     C                   ENDSR
012100040319     C********************************************************************
012200040319     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
012300040319     C********************************************************************
012400040319     C     ADDSDE        BEGSR
012500040319     c                   select
012600040319     c* tipo record BNB
012700040319     C                   when      sdetrd='BNB'
012800040403     C                   eval      DAR5BNBSDE = sdeuni
012900040319     C                   add       §ar5bcsf      §bnbBCSF
013000040319     C                   add       §ar5bcor      §bnbBCor
013100040319     C                   add       §ar5bnba      §bnbBnba
013200040319     C                   add       §ar5bnb2      §bnbBnb2
013300040403     C                   eval      sdeuni = DAR5BNBSDE
013400040319     c* tipo record BAN
013500040319     C                   when      sdetrd='BAN'
013600040403     C                   eval      DAR5BanSDE = sdeuni
013700040319     C                   add       §ar5nba       §bannba
013800040319     C                   add       §ar5nb2       §bannb2
013900040403     C                   eval      sdeuni = DAR5BanSDE
014000040319     C                   ENDsl
014100040319     C                   MOVEL     *BLANKS       SDEFTR
014200040319     C                   Z-ADD     *ZEROS        SDEDTR
014300040319     C                   ENDSR
014400040319     c**********************************************************************
014500040319     c     *inzsr        begsr
014600040319     c**********************************************************************
014700040319
014800040319     c     keysde        Klist
014900040319     c                   Kfld                    sDEFILE
015000040319     c                   Kfld                    sDETRD
015100040319     c                   Kfld                    sDEKSC
015200040319     c                   Kfld                    sDEANN
015300040319     c                   Kfld                    sDEMES
015400040319     c                   Kfld                    sDEREP
015500040319     c                   Kfld                    sDEREA
015600040319     c                   Kfld                    sDEFPE
015700040319     c                   Kfld                    sDETSP
015800040319     c                   Kfld                    sDECTR
015900040319     c                   Kfld                    sDETPO
016000040319     c                   Kfld                    sDERBL
016100040319     c                   Kfld                    sDETPR
016200040319     c     keytas        Klist
016300040319     c                   Kfld                    ar5aas
016400040319     c                   Kfld                    ar5lnp
016500040319     c                   Kfld                    ar5nrs
016600040319     c                   Kfld                    ar5nsp
016700040319     c     ktab          Klist
016800040319     c                   Kfld                    tblkut
016900040319     c                   Kfld                    tblcod
017000040319     c     ktab1         Klist
017100040319     c                   Kfld                    tblkut
017200040319     c                   Kfld                    tblcod
017300040319     c                   Kfld                    tblkey
017400040319     c                   z-add     1             tblkut
017500040319     C* CARICA ORGANIGRAMMA
017600040319     C                   Z-ADD     *ZEROS        orgFIL
017700040319     C     orgfil        SETLL     AZORG01L                           99
017800040319     C  N99              READ      AZORG01L                               99
017900040319     C                   Z-ADD     *ZEROS        I
018000040319DO  2C     *IN99         DOWEQ     *OFF
018100040319IF  3C     ORGFVA        IFEQ      *BLANKS
018200040319IF  4C     ORGFAG        andne     'V'
018300040319     C                   ADD       1             I
018400040319     C                   Z-ADD     ORGFIL        FIL(I)
018500040319     C                   movel(p)  ORGcre        tblkey
018600040319     C* RAGGRUPPAMENTI
018700040319     c                   eval      tblcod = '14'
018800040319     C     ktab1         chain     tabel00f
018900040319IF  5C                   IF        %found(tabel00f)
019000040319     c                   eval      ds14= tbluni
019100040319     C                   Z-ADD     §14crn        RRN(I)
019200040319E   5C                   ENDIF
019300040319E   3C                   ENDIF
019400040319     C                   READ      AZORG01L                               99
019500040319E   2C                   ENDDO
019600040319     C* CARICA TABELLA -2L- FASCIE DI PESO
019700040319     c                   eval      tblcod = '2L'
019800040319     C                   Z-ADD     *ZEROS        I                 6 0
019900040319     C     Ktab          SETLL     TABEL00F                           99
020000040319     C  N99ktab          READe     TABEL00F                               99
020100040319     C     *IN99         DOWEQ     '0'
020200040319     C     TBLFLG        IFEQ      *BLANKS
020300040319     C                   MOVEL     TBLKEY        WTM03             3
020400040319     C                   MOVEL     TBLKEY        WTM05             5
020500040319     C                   MOVEL     TBLUNI        DS2L
020600040319     C     WTM03         ifeq      'S2C'
020700040319     C                   ADD       1             I
020800040319     C                   MOVE      WTM05         FPC(I)
020900040319     C                   Z-ADD     §2LKGI        KIC(I)
021000040319     C                   Z-ADD     §2LKGF        KFC(I)
021100040319     C                   ENDIF
021200040319     C                   ENDIF
021300040319     C     ktab          READe     TABEL00F                               99
021400040319     C                   ENDDO
021500040319     C*
021600040319     C* CARICA TABELLA -TB- TIPI BOLLA
021700040319     C                   Z-ADD     0             I
021800040319     C                   MOVEL     'TB'          tblcod
021900040319     C     Ktab          SETLL     TABEL00F                           99
022000040319     C  N99ktab          READe     TABEL00F                               99
022100040319     C     *IN99         DOWEQ     '0'
022200040319     C     TBLFLG        IFEQ      *BLANKS
022300040319     C                   ADD       1             I
022400040319     C                   MOVEL     TBLUNI        DSTB
022500040319     C                   MOVEL     tblkey        Tbl(I)
022600040319     C                   MOVEL     §TBTPO        TPO(I)
022700040319     C                   MOVEL     §TBRBL        RBL(I)
022800040319     C                   END
022900040319     C     ktab          READe     TABEL00F                               99
023000040319     C                   ENDdo
023100040319     c                   endsr
