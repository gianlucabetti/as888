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
002900050323     C                   PARM                    SA2fel            3 0
003000960124     C**
003100021017
003200021017     c     Kfista        Klist
003300021017     c                   Kfld                    Sa2Trc
003400050323     c                   Kfld                    sa2fel
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
005100970227     C**
005200960124     C* CHIUSURA FILE
005300960124    1C     SA2TLA        IFEQ      'C'
005400960124     C                   SETON                                        LR
005500960124   X1C                   ELSE
005600970227     C**
005700970227     C* NON E' STATISTICA CONSEGNE
005800021021     C     Kfista        SETGT     FiSTA01L
005900021021     C     Kfista        READPE    FiSTA01L                             3421
006000970227     C                   MOVEL     'S'           SCRIVI            1
006100970227     C**
006200960124     C* (Gestisco l'indicatore di record allocato perchè potrebbe
006300960124     C*  essere allocato dal pgm FNLSC8R )
006400970227    2C     SCRIVI        IFEQ      'S'
006500970227     C**
006600970227    3C     *IN21         IFEQ      *ON
006700960124     C     *IN34         OREQ      *ON
006800960124     C* Scrivo record
006900960206     C                   MOVEL     SA2TRC        STATRC
007000960206     C                   MOVEL     SA2DTS        STADTS
007100021022     c                   Clear                   StaUti
007200050323     c                   Movel     sa2fel        StaTfp
007300021023     c                   Z-Add     Sa2Lna        StaFil
007400021023     c                   Movel     Dateu         StaDtw
007500001027     C**
007600970227    4C     SA2TRC        IFNE      'C'
007700021021     c                   Z-Add     1             StaNcl
007800970124     C                   ELSE
007900021021     c                   Clear                   StaNcl
008000970227    4C                   ENDIF
008100970124     C*
008200021017     c                   Write     Fista000
008300001027     C**
008400970227   X3C                   ELSE
008500021022     C* Aggiorno numero colli SE NON E' STATISTICA CONSEGNE
008600021017     c                   If        Sa2Trc <> 'C'
008700021021     c                   Add       1             Stancl
008800021022     c                   EndIf
008900021022     c                   Update    Fista000
009000021017
009100970227    3C                   END
009200970227    2C                   END
009300960124     C*
009400970609     C     SA2TLA        IFEQ      ' '
009500960124     C                   SETON                                        RT
009600970609     C                   ELSE
009700970609     C                   SETON                                        LR
009800970609     C                   ENDIF
009900970609     C*
010000960124    1C                   ENDIF
010100960124     C*
010200960124     C* GIRO UDATE --------------------------------------------------*
010300960124     C     *INZSR        BEGSR
010400960124     C                   TIME                    W0140            14 0
010500960124     C                   MOVE      W0140         WDTGIO            8 0
010600960124     C                   MOVEL     WDTGIO        WDATA             8 0
010700960124     C                   MOVEL     WMM           W0040             4 0
010800960124     C                   MOVE      WGG           W0040
010900960124     C                   MOVEL     WAA           DATEU             8 0
011000960124     C                   MOVE      W0040         DATEU             8 0
011100970227     C**
011200970227     C                   MOVEL     '7A'          KCOD
011300970227     C                   Z-ADD     1             CODUT
011400970606     C*
011500960124     C                   ENDSR
