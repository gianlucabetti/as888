000100030206     H DECEDIT('0,') DATEDIT(*YMD.)
000200030206
000300040319     fFiar530c  if   e           k Disk
000400040319     f                                     Ignore(fiar5p00)
000500040319     fTitas30c  if   e           k Disk
000600040428     f                                     Ignore(Titas010)
000700030206     f                                     Ignore(Titasp00)
000800040428     fsicop01l  uf a e           k Disk
000900040428     D*-------------------
001000040428     D DS$ECO        E DS
001100040428     D*-------------------
001200040428     D* DS RIDEFINIZIONE IMMAGINE PRIMA E DOPO DI TNTAS10F
001300040428     D*-------------------
001400040428     D TRECDS        E DS
001500040428     D  D$PDS1                 1    250
001600040428     D  D$PDS2               251    500
001700040428     D  D$DDS1               501    750
001800040428     D  D$DDS2               751   1000
001900040428     D  D$ODS1              1001   1250
002000040428     D  D$ODS2              1251   1500
002100040319
002200040428     D* SCHIERE CODICI E IMPORTI ALFANUMERICHE (PER PASSAGIO DATI)
002300040428     D CAL             S              2    DIM(75)
002400040428     D IAL             S              7    DIM(75)
002500040428     D DAT             S              1    DIM(75)
002600040428     D CMPV            S              2    DIM(75)
002700040428     D IMDV            S             13  4 DIM(999)
002800040428     D DATDV           S              1    DIM(999)
002900040428     D CMPDV           S              3  0 DIM(999)
003000040428     D                 DS
003100040428     D  DSWCN                  1      3P 0
003200040428     D  DSWCA                  1      3
003300040428     D  DSWIN                  4     13P 4
003400040428     D  DSWIA                  4     13
003500030206
003600040428     C     2004          Setll     titas30c
003700040428     C     2004          reade     titas30c
003800040428     c                   Dow       NOT %eof(titas30c)
003900040428     c                   if        tasflp <> ' '  or tasfl2 <> ' '
004000040428     c     keytas        chain     fiar530c
004100040428     c                   if        %found(fiar530c)
004200040428     c                   clear                   sicop000
004300040428      /COPY GAITRASRC/SRCTREC,TRECDT
004400040428     c                   exsr      apdtaq
004500040428     c                   exsr      addtaq
004600040428     c                   exsr      aodtaq
004700040428     c                   exsr      cpdtaq
004800040428     c                   exsr      cddtaq
004900040428     c                   exsr      codtaq
005000040428     C* LANCIO PGM CHE MI RITORNA LE COMPETENZE -DOPO-
005100040428     C                   CALL      'TRECD8R'                            98
005200040428     C                   PARM                    DS$ECO                         *DS IMMAGINI BOLLA
005300040428     C*
005400040428     C     D$GERR        IFEQ      *BLANKS
005500040428     c*
005600040428     C* VOCI
005700040428     C* TRATTO CODICI E IMPORTI VOCI -DOPO-
005800040428     C                   MOVEA     D$DCVD        CAL
005900040428     C                   MOVEA     D$DIVD        IAL
006000040428     C                   MOVEA     D$DDAT        DAT
006100040428     C                   MOVEA     D$DCMP        CMPV
006200040428DO  3C     1             DO        75            I
006300040428     C                   MOVE      CAL(I)        DSWCA
006400040428     C                   Z-ADD     DSWCN         CD                5 0
006500040428IF  4C     CD            IFGT      *ZEROS
006600040428     C                   MOVE      IAL(I)        DSWIA
006700040428     C                   Z-ADD     DSWIN         IMDV(CD)
006800040428     C                   MOVEL     DAT(I)        DATDV(CD)
006900040428     C                   MOVE      CMPV(I)       DSWCA
007000040428     C                   Z-ADD     DSWCN         CMPDV(CD)
007100040428E   4C                   ENDIF
007200040428E   3C                   ENDDO
007300040428     C*
007400040428     C                   ENDIF
007500040428     c                   exsr      aggcop
007600040319     c                   endif
007700040319     c                   endif
007800040428     C     2004          reade     titas30c
007900040319     c                   enddo
008000030206     c                   Eval      *InLr = *On
008100040428     C                   MOVEL     'C'           D$GSTS
008200040428     C                   CALL      'TRECD8R'                            98
008300040428     C                   PARM                    DS$ECO
008400040319     c**********************************************************************
008500040428     c     aggcop        begsr
008600040319     c**********************************************************************
008700040319     c*
008800040319     C* RECUPERO LA REGIONE PARTENZA
008900040319     c                   z-add     1             i
009000040319     C     taslnp        LOOKUP    FIL(I)                                 99
009100040319     C                   Z-ADD     RRN(I)        sderep
009200040319     C*
009300040319     C* RECUPERO LA REGIONE DI ARRIVO
009400040319     c                   z-add     1             i
009500040319     C     taslna        LOOKUP    FIL(I)                                 99
009600040319     C                   Z-ADD     RRN(I)        sderea
009700040319     C*
009800040319     C* FASCIA DI PESO DA TABELLA '2L' X FILE CLIENTI
009900040319     C     taspkf        IFEQ      *ZEROS
010000040319     C                   MOVEL     FPC(1)        sdefpe
010100040319     C                   ELSE
010200040319     C                   DO        20            I
010300040319     C     taspkf        IFGT      KIC(I)
010400040319     C     taspkf        ANDLE     KFC(I)
010500040319     C                   MOVEL     FPC(I)        sdefpe
010600040319     C                   LEAVE
010700040319     C                   ENDIF
010800040319     C                   ENDDO
010900040319     C                   ENDIF
011000040319     C*
011100040319     C* tipo bolla e raggruppamento bolla
011200040319     C                   Z-ADD     1             I                 6 0
011300040319     C     tastbl        LOOKUP    TBL(I)                                 99
011400040319     C                   MOVEL     TPO(I)        sdeTPO
011500040319     C                   MOVEL     RBL(I)        sdeRBL
011600040319
011700040319     c                   movel     'SDC'         sDEFILE
011800040319     c                   z-add     tasksc        sDEKSC
011900040319     c                   z-add     tasaas        sDEANN
012000040319     c                   movel     tasmgs        sDEMES
012100040319     c                   movel     tastsp        sDETSP
012200040319     c                   z-add     tasctr        sDECTR
012300040319     c                   clear                   sDETPR
012400040319     c* aggancio SISDE01l
012500040319     C     KEYSDE        CHAIN     SISDE01L                           99
012600040319     C     *IN99         IFEQ      '0'
012700040319     C                   EXSR      ADDSDE
012800040319     C                   UPDATE    SISDE000
012900040319     C                   ELSE
013000040319     C                   EXSR      AZZSDE
013100040319     C                   EXSR      ADDSDE
013200040319     C                   WRITE     SISDE000
013300040319     C                   END
013400040319     c
013500040319     c                   endsr
013600040319     C********************************************************************
013700040319     C*   AZZSDE - AZZERA I CAMPI DEL FILE SISDE00F (ESTENSIONE SALDI)      )
013800040319     C********************************************************************
013900040319     C     AZZSDE        BEGSR
014000040319     c                   select
014100040319     c* tipo record BNB
014200040319     C                   when      sdetrd='BNB'
014300040319     C                   CLEAR                   DAR5BNBSDE
014400040403     C                   eval      sdeuni = DAR5BNBSDE
014500040319     c* tipo record BAN
014600040319     C                   when      sdetrd='BAN'
014700040319     C                   CLEAR                   DAR5BANSDE
014800040403     C                   eval      sdeuni = DAR5BANSDE
014900040319     C                   ENDsl
015000040319     C                   MOVEL     *BLANKS       SDEFTR
015100040319     C                   Z-ADD     *ZEROS        SDEDTR
015200040319     C                   ENDSR
015300040319     C********************************************************************
015400040319     C*   ADDSDE - AGGIUNGE AI VALORI DEL FILE SISDE00F (ESTENSIONE SALDI)  EG)
015500040319     C********************************************************************
015600040319     C     ADDSDE        BEGSR
015700040319     c                   select
015800040319     c* tipo record BNB
015900040319     C                   when      sdetrd='BNB'
016000040403     C                   eval      DAR5BNBSDE = sdeuni
016100040319     C                   add       §ar5bcsf      §bnbBCSF
016200040319     C                   add       §ar5bcor      §bnbBCor
016300040319     C                   add       §ar5bnba      §bnbBnba
016400040319     C                   add       §ar5bnb2      §bnbBnb2
016500040403     C                   eval      sdeuni = DAR5BNBSDE
016600040319     c* tipo record BAN
016700040319     C                   when      sdetrd='BAN'
016800040403     C                   eval      DAR5BanSDE = sdeuni
016900040319     C                   add       §ar5nba       §bannba
017000040319     C                   add       §ar5nb2       §bannb2
017100040403     C                   eval      sdeuni = DAR5BanSDE
017200040319     C                   ENDsl
017300040319     C                   MOVEL     *BLANKS       SDEFTR
017400040319     C                   Z-ADD     *ZEROS        SDEDTR
017500040319     C                   ENDSR
017600040319     c**********************************************************************
017700040319     c     *inzsr        begsr
017800040319     c**********************************************************************
017900040319
018000040319     c     keysde        Klist
018100040319     c                   Kfld                    sDEFILE
018200040319     c                   Kfld                    sDETRD
018300040319     c                   Kfld                    sDEKSC
018400040319     c                   Kfld                    sDEANN
018500040319     c                   Kfld                    sDEMES
018600040319     c                   Kfld                    sDEREP
018700040319     c                   Kfld                    sDEREA
018800040319     c                   Kfld                    sDEFPE
018900040319     c                   Kfld                    sDETSP
019000040319     c                   Kfld                    sDECTR
019100040319     c                   Kfld                    sDETPO
019200040319     c                   Kfld                    sDERBL
019300040319     c                   Kfld                    sDETPR
019400040319     c     keytas        Klist
019500040319     c                   Kfld                    ar5aas
019600040319     c                   Kfld                    ar5lnp
019700040319     c                   Kfld                    ar5nrs
019800040319     c                   Kfld                    ar5nsp
019900040319     c     ktab          Klist
020000040319     c                   Kfld                    tblkut
020100040319     c                   Kfld                    tblcod
020200040319     c     ktab1         Klist
020300040319     c                   Kfld                    tblkut
020400040319     c                   Kfld                    tblcod
020500040319     c                   Kfld                    tblkey
020600040319     c                   z-add     1             tblkut
020700040319     C* CARICA ORGANIGRAMMA
020800040319     C                   Z-ADD     *ZEROS        orgFIL
020900040319     C     orgfil        SETLL     AZORG01L                           99
021000040319     C  N99              READ      AZORG01L                               99
021100040319     C                   Z-ADD     *ZEROS        I
021200040319DO  2C     *IN99         DOWEQ     *OFF
021300040319IF  3C     ORGFVA        IFEQ      *BLANKS
021400040319IF  4C     ORGFAG        andne     'V'
021500040319     C                   ADD       1             I
021600040319     C                   Z-ADD     ORGFIL        FIL(I)
021700040319     C                   movel(p)  ORGcre        tblkey
021800040319     C* RAGGRUPPAMENTI
021900040319     c                   eval      tblcod = '14'
022000040319     C     ktab1         chain     tabel00f
022100040319IF  5C                   IF        %found(tabel00f)
022200040319     c                   eval      ds14= tbluni
022300040319     C                   Z-ADD     §14crn        RRN(I)
022400040319E   5C                   ENDIF
022500040319E   3C                   ENDIF
022600040319     C                   READ      AZORG01L                               99
022700040319E   2C                   ENDDO
022800040319     C* CARICA TABELLA -2L- FASCIE DI PESO
022900040319     c                   eval      tblcod = '2L'
023000040319     C                   Z-ADD     *ZEROS        I                 6 0
023100040319     C     Ktab          SETLL     TABEL00F                           99
023200040319     C  N99ktab          READe     TABEL00F                               99
023300040319     C     *IN99         DOWEQ     '0'
023400040319     C     TBLFLG        IFEQ      *BLANKS
023500040319     C                   MOVEL     TBLKEY        WTM03             3
023600040319     C                   MOVEL     TBLKEY        WTM05             5
023700040319     C                   MOVEL     TBLUNI        DS2L
023800040319     C     WTM03         ifeq      'S2C'
023900040319     C                   ADD       1             I
024000040319     C                   MOVE      WTM05         FPC(I)
024100040319     C                   Z-ADD     §2LKGI        KIC(I)
024200040319     C                   Z-ADD     §2LKGF        KFC(I)
024300040319     C                   ENDIF
024400040319     C                   ENDIF
024500040319     C     ktab          READe     TABEL00F                               99
024600040319     C                   ENDDO
024700040319     C*
024800040319     C* CARICA TABELLA -TB- TIPI BOLLA
024900040319     C                   Z-ADD     0             I
025000040319     C                   MOVEL     'TB'          tblcod
025100040319     C     Ktab          SETLL     TABEL00F                           99
025200040319     C  N99ktab          READe     TABEL00F                               99
025300040319     C     *IN99         DOWEQ     '0'
025400040319     C     TBLFLG        IFEQ      *BLANKS
025500040319     C                   ADD       1             I
025600040319     C                   MOVEL     TBLUNI        DSTB
025700040319     C                   MOVEL     tblkey        Tbl(I)
025800040319     C                   MOVEL     §TBTPO        TPO(I)
025900040319     C                   MOVEL     §TBRBL        RBL(I)
026000040319     C                   END
026100040319     C     ktab          READe     TABEL00F                               99
026200040319     C                   ENDdo
026300040319     c                   endsr
