000100021017     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200960124      * FNLSA2R2*----------------------------------------------------*
000300021021      *         - SCRITTUA FISTA00F PER RIELABORAZIONE STATISTICHE
000400960124      *--------------------------------------------------------------*
000500021021     fFista01l  uf a e           k Disk
000600970227     FTABEL00F  IF   E           K DISK
000700970227     D DS7AF         E DS
000800970227     D  ALFDPE                68     75
000900970227     D  ALFDUE                76     83
001000960124     D                 DS
001100960124     D  WGG                    1      2  0
001200960124     D  WMM                    3      4  0
001300960124     D  WAA                    5      8  0
001400960124     D  WDATA                  1      8  0
001500021016
001600021016     d Tibs02Ds      e ds
001700021016      * Ds date statistiche
001800021016     d dSdf          e ds
001900021016
002000021016     d Ut§dse0f      e ds
002100021016     d Cncr80        e ds
002200021016     d Kpjba         e ds
002300021016
002400960124     C     *ENTRY        PLIST
002500960124     C                   PARM                    SA2TLA            1
002600960124     C                   PARM                    SA2TRC            1
002700960124     C                   PARM                    SA2DTS            8 0
002800970124     C                   PARM                    SA2LNA            3 0
002900090330     C                   PARM                    SA2NCl            5 0
003000960124     C**
003100021017
003200021017     c     Kfista        Klist
003300021017     c                   Kfld                    Sa2Trc
003400021017     c                   Kfld                    Simfel
003500021022     c                   Kfld                    Sa2Lna
003600021017     c                   Kfld                    Sa2Dts
003700021017
003800970227     C     KTAB1         KLIST
003900970227     C                   KFLD                    CODUT
004000970227     C                   KFLD                    KCOD
004100970227     C                   KFLD                    KKEY
004200970124     C*
004300970227     C     *LIKE         DEFINE    SA2LNA        SAVLNA
004400970227     C     *LIKE         DEFINE    TBLCOD        KCOD
004500970227     C     *LIKE         DEFINE    TBLKEY        KKEY
004600970227     C     *LIKE         DEFINE    TBLKUT        CODUT
004700970227     C*
004800970227     C* Leggo l'ultimo record scritto (setgt+redpe) perchè se ce n'è
004900970227     C* più di uno con la stessa chiave significa che il primo è allo-
005000970227     C* cato da FNLSC8R
005100090330     c* Vedo se passato l'ulitmo parametro
005200090330     c                   if        %parms=5
005300090330     c                   z-add     sa2ncl        rielancl
005400090330     c                   else
005500090330     c                   z-add     1             rielancl          5 0
005600090330     c                   endif
005700970227     C**
005800960124     C* CHIUSURA FILE
005900960124    1C     SA2TLA        IFEQ      'C'
006000960124     C                   SETON                                        LR
006100960124   X1C                   ELSE
006200960124     C*
006300970124    2C     SA2TRC        IFEQ      'C'
006400970227     C*
006500970227    3C     SA2LNA        IFNE      SAVLNA
006600970227     C                   MOVEL     SA2LNA        KKEY
006700970227     C     KTAB1         CHAIN     TABEL                              34
006800970227     C*
006900970227    4C     *IN34         IFEQ      *ON
007000970227     C                   CLEAR                   DS7AF
007100970227   X4C                   ELSE
007200970227     C*
007300970227     C                   MOVEL     TBLUNI        DS7AF
007400970227    5C     ALFDPE        IFLE      *ZERO
007500970227     C                   MOVEL     *ZEROS        ALFDPE
007600970227     C                   MOVEL     *ZEROS        ALFDUE
007700970227    5C                   ENDIF
007800970227    4C                   ENDIF
007900970227     C*
008000970227     C                   MOVEL     SA2LNA        SAVLNA
008100970227    3C                   ENDIF
008200970227     C**
008300970227    3C     SA2DTS        IFLE      §7ACUE
008400970227     C     SA2DTS        ANDGE     §7ACPE
008500970227     C**
008600970227     C                   MOVEL     'S'           SCRIVI            1
008700021021     C     Kfista        SETGT     FiSTA01L
008800021021     C     Kfista        READPE    FiSTA01L                             3421
008900970227   X3C                   ELSE
009000970227     C**
009100970227     C**  NON DEVO ESEGUIRE LA WRITE
009200970227     C                   MOVEL     'N'           SCRIVI            1
009300970227    3C                   ENDIF
009400970227     C**
009500970124   X2C                   ELSE
009600970227     C**
009700970227     C* NON E' STATISTICA CONSEGNE
009800021021     C     Kfista        SETGT     FiSTA01L
009900021021     C     Kfista        READPE    FiSTA01L                             3421
010000970227     C                   MOVEL     'S'           SCRIVI            1
010100970606     C* SE E' STATISTICA ARRIVI ELABORO SOLO SE DATA <= DATA ULTIMA
010200970606     C* ELABORAZIONE
010300970606     C     SA2TRC        IFEQ      'A'
010400021016     c                   If        Sa2Dts > §SdfArrU
010500970606     C                   MOVE      *BLANKS       SCRIVI
010600970606     C                   END
010700970606     C                   END
010800970124    2C                   ENDIF
010900970227     C**
011000960124     C* (Gestisco l'indicatore di record allocato perchè potrebbe
011100960124     C*  essere allocato dal pgm FNLSC8R )
011200970227    2C     SCRIVI        IFEQ      'S'
011300970227     C**
011400970227    3C     *IN21         IFEQ      *ON
011500960124     C     *IN34         OREQ      *ON
011600960124     C* Scrivo record
011700960206     C                   MOVEL     SA2TRC        STATRC
011800960206     C                   MOVEL     SA2DTS        STADTS
011900021022     c                   Clear                   StaUti
012000021021     c                   Movel     Simfel        StaTfp
012100021023     c                   Z-Add     Sa2Lna        StaFil
012200021023     c                   Movel     Dateu         StaDtw
012300001027     C**
012400970227    4C     SA2TRC        IFNE      'C'
012500090330     c                   Z-Add     rielancl      StaNcl
012600970124     C                   ELSE
012700021021     c                   Clear                   StaNcl
012800970227    4C                   ENDIF
012900970124     C*
013000021017     c                   Write     Fista000
013100001027     C**
013200970227   X3C                   ELSE
013300021022     C* Aggiorno numero colli SE NON E' STATISTICA CONSEGNE
013400021017     c                   If        Sa2Trc <> 'C'
013500090330     c                   Add       rielancl      Stancl
013600021022     c                   EndIf
013700021022     c                   Update    Fista000
013800021017
013900970227    3C                   END
014000970227    2C                   END
014100960124     C*
014200970609     C     SA2TLA        IFEQ      ' '
014300960124     C                   SETON                                        RT
014400970609     C                   ELSE
014500970609     C                   SETON                                        LR
014600970609     C                   ENDIF
014700970609     C*
014800960124    1C                   ENDIF
014900960124     C*
015000960124     C* GIRO UDATE --------------------------------------------------*
015100960124     C     *INZSR        BEGSR
015200960124     C                   TIME                    W0140            14 0
015300960124     C                   MOVE      W0140         WDTGIO            8 0
015400960124     C                   MOVEL     WDTGIO        WDATA             8 0
015500960124     C                   MOVEL     WMM           W0040             4 0
015600960124     C                   MOVE      WGG           W0040
015700960124     C                   MOVEL     WAA           DATEU             8 0
015800960124     C                   MOVE      W0040         DATEU             8 0
015900970227     C**
016000970227     C                   MOVEL     '7A'          KCOD
016100970227     C                   Z-ADD     1             CODUT
016200021016     C* Reperimento parametri aziendali
016300021016     C                   CALL      'X§PARUT'
016400021016     C                   PARM                    Ut§dse0f
016500021016     C                   MOVEL     REC80         CNCR80
016600021016
016700021016      * Cerco la data ultima elaborazione dalla tabella
016800021016     c                   Clear                   Tibs02Ds
016900021016     c                   Clear                   dSdf
017000021016     c                   Eval      T02Mod = 'C'
017100021016     c                   Eval      T02Sif = Knsif
017200021016     c                   Eval      T02Cod = 'SDF'
017300021016     c                   Movel(p)  Simfel        T02Ke1
017400021016     c                   Call      'TIBS02R'
017500021016     c                   Parm                    Kpjba
017600021016     c                   Parm                    Tibs02Ds
017700021016     c                   If        T02Err = *Blanks
017800021016     c                   Movel     T02Uni        dSdf
017900021016     c                   EndIf
018000970606     C*
018100960124     C                   ENDSR
