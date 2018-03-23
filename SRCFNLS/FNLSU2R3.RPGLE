000100021017     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200160726      * FNLSU2R3* ---------------------------------------------------*
000300160726      *    SCRITTUA FISTA00F PER RIELABORAZIONE STATISTICHE
000400960124      *--------------------------------------------------------------*
000500021021     fFista01l  uf a e           k Disk
000600970227     FTABEL00F  IF   E           K DISK
000700160726
000800160726     d sktrc           s              1    dim(3)
000900160726     d
001000970227     D DS7AF         E DS
001100970227     D  ALFDPE                68     75
001200970227     D  ALFDUE                76     83
001300960124     D                 DS
001400960124     D  WGG                    1      2  0
001500960124     D  WMM                    3      4  0
001600960124     D  WAA                    5      8  0
001700960124     D  WDATA                  1      8  0
001800160726     d
001900160726     D  FNLSU2R3DS     DS
002000160726     D  sa2tla                        1
002100160726     D  sa2trc                        1
002200160726     D  sa2dts                        8  0
002300160726     D  sa2lna                        3  0
002400160726     D  sa2ncl                        5  0
002500160726     D  sa2tfp                        3  0
002600021016
002700021016     d Tibs02Ds      e ds
002800021016      * Ds date statistiche
002900021016     d dSdf          e ds
003000160726     d
003100021016     d Ut§dse0f      e ds
003200021016     d Cncr80        e ds
003300021016     d Kpjba         e ds
003400160726     d
003500160726     d Ktfp            s                   like(simfel)
003600160726     d savtfp          s                   like(simfel) inz
003700160726     d ktrc            s              1
003800160726     d xx              s              3  0
003900021016
004000960124     C     *ENTRY        PLIST
004100160726     C                   PARM                    fnlsu2r3ds
004200160726     c
004300160726     c                   if        sa2tfp>0
004400160726     c                   eval      ktfp=sa2tfp
004500160726     c                   else
004600160726     c                   eval      ktfp=simfel
004700160726     c                   endif
004800970227     C*
004900160726     c                   if        sa2ncl>0
005000090330     c                   z-add     sa2ncl        rielancl
005100090330     c                   else
005200090330     c                   z-add     1             rielancl          5 0
005300090330     c                   endif
005400970227     C**
005500960124     C* CHIUSURA FILE
005600960124    1C     SA2TLA        IFEQ      'C'
005700960124     C                   SETON                                        LR
005800960124   X1C                   ELSE
005900160726     c
006000160726    2c                   if        sa2trc='E'
006100160726     c                   eval      sktrc(1)='P'
006200160726     c                   eval      sktrc(2)='T'
006300160726     c                   else
006400160726     c                   eval      sktrc(1)=sa2trc
006500160726     c                   eval      sktrc(2)=' '
006600160726    2c                   endif
006700160726     c                   eval      xx=1
006800160726
006900160726   1ac                   dow       sktrc(xx)<>' '
007000160726     c                   eval      ktrc=sktrc(xx)
007100160726     C***
007200160726     c* STATISTICA Consegne
007300160726     C***
007400160726    2C                   IF        ktrc= 'C'
007500970227     C*
007600160726     C* Verifico data ultima elaborazione da tabella 7A
007700970227    3C     SA2LNA        IFNE      SAVLNA
007800970227     C                   MOVEL     SA2LNA        KKEY
007900970227     C     KTAB1         CHAIN     TABEL                              34
008000970227     C*
008100970227    4C     *IN34         IFEQ      *ON
008200970227     C                   CLEAR                   DS7AF
008300970227   X4C                   ELSE
008400970227     C*
008500970227     C                   MOVEL     TBLUNI        DS7AF
008600970227    5C     ALFDPE        IFLE      *ZERO
008700970227     C                   MOVEL     *ZEROS        ALFDPE
008800970227     C                   MOVEL     *ZEROS        ALFDUE
008900970227    5C                   ENDIF
009000970227    4C                   ENDIF
009100970227     C*
009200970227     C                   MOVEL     SA2LNA        SAVLNA
009300970227    3C                   ENDIF
009400970227     C**
009500970227    3C     SA2DTS        IFLE      §7ACUE
009600970227     C     SA2DTS        ANDGE     §7ACPE
009700970227     C**
009800160726     C* Leggo l'ultimo record scritto (setgt+redpe) perchè se ce n'è
009900160726     C* più di uno con la stessa chiave significa che il primo è allo-
010000160726     C* cato da FNLSC8R
010100970227     C                   MOVEL     'S'           SCRIVI            1
010200021021     C     Kfista        SETGT     FiSTA01L
010300021021     C     Kfista        READPE    FiSTA01L                             3421
010400970227   X3C                   ELSE
010500970227     C**
010600970227     C**  NON DEVO ESEGUIRE LA WRITE
010700970227     C                   MOVEL     'N'           SCRIVI            1
010800970227    3C                   ENDIF
010900970227     C**
011000970124   X2C                   ELSE
011100160726     C***
011200160726     C* STATISTICA Arrivi / Partito / A terra
011300160726     C**
011400160726      * Cerco la data ultima elaborazione dalla tabella
011500160726    3c                   if        savtfp=0 or savtfp<>ktfp
011600160726     c                   Clear                   Tibs02Ds
011700160726     c                   Clear                   dSdf
011800160726     c                   Eval      T02Mod = 'C'
011900160726     c                   Eval      T02Sif = Knsif
012000160726     c                   Eval      T02Cod = 'SDF'
012100160726     c                   Movel(p)  ktfp          T02Ke1
012200160726     c                   Call      'TIBS02R'
012300160726     c                   Parm                    Kpjba
012400160726     c                   Parm                    Tibs02Ds
012500160726     c                   If        T02Err = *Blanks
012600160726     c                   Movel     T02Uni        dSdf
012700160726     c                   EndIf
012800160726     c
012900160726     c                   eval      savtfp=ktfp
013000160726    3c                   endif
013100160726     c
013200160726     c
013300160726     c
013400021021     C     Kfista        SETGT     FiSTA01L
013500021021     C     Kfista        READPE    FiSTA01L                             3421
013600970227     C                   MOVEL     'S'           SCRIVI            1
013700160726     c
013800160726     C* ELABORO SOLO SE DATA <= DATA ULTIMA ELABORAZIONE
013900160726     c                   If        ktrc='A' and Sa2Dts > §SdfArrU
014000970606     C                   MOVE      *BLANKS       SCRIVI
014100160726     C                   ENDif
014200160726     c                   If        ktrc='P' and Sa2Dts > §SdfParU
014300160726     C                   MOVE      *BLANKS       SCRIVI
014400160726     C                   ENDif
014500160726     c                   If        ktrc='T' and Sa2Dts > §SdfTerU
014600160726     C                   MOVE      *BLANKS       SCRIVI
014700160726     C                   ENDif
014800160726     c
014900970124    2C                   ENDIF
015000970227     C**
015100960124     C* (Gestisco l'indicatore di record allocato perchè potrebbe
015200960124     C*  essere allocato dal pgm FNLSC8R )
015300970227    2C     SCRIVI        IFEQ      'S'
015400970227     C**
015500970227    3C     *IN21         IFEQ      *ON
015600960124     C     *IN34         OREQ      *ON
015700960124     C* Scrivo record
015800160726     C                   MOVEL     ktrc          STATRC
015900960206     C                   MOVEL     SA2DTS        STADTS
016000021022     c                   Clear                   StaUti
016100160726     c                   Movel     ktfp          StaTfp
016200021023     c                   Z-Add     Sa2Lna        StaFil
016300021023     c                   Movel     Dateu         StaDtw
016400001027     C**
016500160726    4C     ktrc          IFNE      'C'
016600090330     c                   Z-Add     rielancl      StaNcl
016700970124     C                   ELSE
016800021021     c                   Clear                   StaNcl
016900970227    4C                   ENDIF
017000970124     C*
017100021017     c                   Write     Fista000
017200001027     C**
017300970227   X3C                   ELSE
017400021022     C* Aggiorno numero colli SE NON E' STATISTICA CONSEGNE
017500160726     c                   If        ktrc   <> 'C'
017600090330     c                   Add       rielancl      Stancl
017700021022     c                   EndIf
017800021022     c                   Update    Fista000
017900021017
018000970227    3C                   END
018100970227    2C                   END
018200160726     c
018300160726     c                   add       1             xx
018400160726   1ac                   enddo
018500960124     C*
018600970609     C     SA2TLA        IFEQ      ' '
018700960124     C                   SETON                                        RT
018800970609     C                   ELSE
018900970609     C                   SETON                                        LR
019000970609     C                   ENDIF
019100970609     C*
019200960124    1C                   ENDIF
019300960124     C*
019400960124     C* GIRO UDATE --------------------------------------------------*
019500960124     C     *INZSR        BEGSR
019600960124     C                   TIME                    W0140            14 0
019700960124     C                   MOVE      W0140         WDTGIO            8 0
019800960124     C                   MOVEL     WDTGIO        WDATA             8 0
019900960124     C                   MOVEL     WMM           W0040             4 0
020000960124     C                   MOVE      WGG           W0040
020100960124     C                   MOVEL     WAA           DATEU             8 0
020200960124     C                   MOVE      W0040         DATEU             8 0
020300970227     C**
020400970227     C                   MOVEL     '7A'          KCOD
020500970227     C                   Z-ADD     1             CODUT
020600021016     C* Reperimento parametri aziendali
020700021016     C                   CALL      'X§PARUT'
020800021016     C                   PARM                    Ut§dse0f
020900021016     C                   MOVEL     REC80         CNCR80
021000160726     c
021100160726     c     Kfista        Klist
021200160726     c                   Kfld                    ktrc
021300160726     c                   Kfld                    ktfp
021400160726     c                   Kfld                    Sa2Lna
021500160726     c                   Kfld                    Sa2Dts
021600160726
021700160726     C     KTAB1         KLIST
021800160726     C                   KFLD                    CODUT
021900160726     C                   KFLD                    KCOD
022000160726     C                   KFLD                    KKEY
022100160726     C*
022200160726     C     *LIKE         DEFINE    SA2LNA        SAVLNA
022300160726     C     *LIKE         DEFINE    TBLCOD        KCOD
022400160726     C     *LIKE         DEFINE    TBLKEY        KKEY
022500160726     C     *LIKE         DEFINE    TBLKUT        CODUT
022600970606     C*
022700960124     C                   ENDSR
