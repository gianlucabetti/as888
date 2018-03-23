000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300160405     fecced30c  if   e           k Disk
000301160405     f                                     Ignore(ECCEDP00)
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
001906160405     D imp             S                   like(§logicmp)
002000040319
002100040319     D Ds14          E DS
002200040319     D Ds2l          E DS
002300040319     D Dstb          E DS
002400160405     D Dsdelog       E DS
002401160405     D Dtas          E DS                  extname(titas10f)
002800040319
002900030206
002901160405     c                   Eval      *InLr = *On
002902160405     C/EXEC SQL
002903160412     C+ DECLARE TAS CURSOR FOR SELECT *
002904160412     C+ FROM titas10f WHERE tasaas = 2016 and
002905160419     C+ tasmgs between 0201 and 0331 and
002906160412     C+ tasksc in ( SELECT
002907160405     C+ EVDCLI FROM ecevd00f WHERE EVDVOC between 350 and 359 and EVDTTV
002908160412     C+ = 'K' )
002909160405     C/END-EXEC
002910160405
002911160405     C/EXEC SQL
002912160405     C+ OPEN tas
002913160405     C/END-EXEC
002914160405
002915160405     C                   DO        *HIVAL
002916160405
002917160405     C/EXEC SQL
002918160412     C+ FETCH NEXT FROM tas INTO :dtas
002919160405     C/END-EXEC
002920160405
002921160405     C                   SELECT
002922160405     C
002923160405     C                   WHEN      SQLCod = 100
002924160405     C                   LEAVE
002925160405     C
002926160405     C                   WHEN      SQLCod < 0
002927160405     C                   EVAL      *INH1 = *ON
002928160405     C                   LEAVE
002929160405     C
002930160405     C                   OTHER
002931160405     c                   clear                   imp
002932160405     c                   z-add     300           cedcmp
003000160405     C     ksped3        Setll     ecced30c
003001160405     c                   do        *hival
003002160405     c     ksped         reade     ecced30c
003300160405     c                   if        %eof(ecced30c)
003301160405     c                   leave
003302160405     c                   end
003303160405     c                   if        cedcmp > 309
003304160405     c                   leave
003305160405     c                   end
003306160405     c                   add       cedimp        imp
003307160405     C                   ENDDO
003308160405     c                   if        imp <> 0
004500040319     c                   exsr      aggsde
004501160405     c                   endif
005101160405     C                   ENDSL
005102160405
005103160405     C                   ENDDO
005105160405
005106160405     C/EXEC SQL
005107160405     C+ CLOSE tas
005108160405     C/END-EXEC
005113160405     C                   RETURN
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
008500160405     c                   movel     'LOG'         sDETRD
008501160405     c                   movel     'SDC'         sDEFILE
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
011100160405     C                   CLEAR                   §logicmp
011200160405     C                   eval      sdeuni = Dsdelog
011800040319     C                   MOVEL     *BLANKS       SDEFTR
011900040319     C                   Z-ADD     *ZEROS        SDEDTR
012000040319     C                   ENDSR
012100040319     C********************************************************************
012200040319     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
012300040319     C********************************************************************
012400040319     C     ADDSDE        BEGSR
012800160405     C                   eval      Dsdelog = sdeuni
012900160405     C                   add       imp           §logicmp
013300160405     C                   eval      sdeuni = Dsdelog
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
016200160405     c     ksped3        Klist
016300160412     c                   Kfld                    tasaas
016400160412     c                   Kfld                    taslnp
016500160412     c                   Kfld                    tasnrs
016600160412     c                   Kfld                    tasnsp
016601160412     c                   Kfld                    tastbl
016602160405     c                   Kfld                    cedcmp
016603160405     c     ksped         Klist
016604160412     c                   Kfld                    tasaas
016605160412     c                   Kfld                    taslnp
016606160412     c                   Kfld                    tasnrs
016607160412     c                   Kfld                    tasnsp
016608160412     c                   Kfld                    tastbl
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
