000100040205     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200040205
000300040205     fWFMRL00F  uP   E             DISK
000400040204     fTITAS32C  IF   E           K DISK
000500040205     fTITAS30C  IF   E           K DISK
000600040205     f                                     rename(TITAS000:TITAS031)
000700040205     f                                     rename(TITAS010:TITAS131)
000800040205     f                                     rename(TITASP00:TITASP31)
000900040204     fTITA435C  IF   E           K DISK
001000040204     fTABEL00F  IF   E           K DISK
001100040204
001200040205     D SK_TB           S              2    DIM(30)
001300040205
001400040205     D Kcod            S                   LIKE(TBLcod)
001500040205     d wNtara          s                   Like(TasPkf)
001600040205     d wTara           s                   Like(§QtTpc)
001700040205
001800040204     D CODUT           S              1  0 INZ(1)
001900040205     d Trovata         s              1    inz(*off)
002000040205     d xx              s              3  0
002100040205     d w015            s             15  0
002200040205     d w015a           s             15
002300040205
002400040205     D DSTB          E DS
002500040205     d dsqt1         e ds
002600040205
002700040205     D KPJBA         E DS
002800030926
002900030205     C*---------------------------------------------------------------*
003000040206
003100040206     c                   Eval      trovata = *Off
003200030926      *
003300040204      * Cerco riferimento come numerico
003400040205     C                   Z-ADD     EXMRM1        W015
003500040204     C     W015          setll     TITAS32C
003600040204     C     W015          reade     TITAS32C
003700040204     C                   DOW       NOT %EOF(TITAS32C)
003800040205
003900040204      *
004000040204     C                   IF        TASKSC = 0660898
004100040206     C                             AND TASLNP = 66
004200040206      * controllare tipo bolla
004300040205     c                   z-add     1             xx
004400040204     c     TASTBL        lookup    SK_TB(xx)                              99
004500040204     c                   If        *in99 = *off
004600040205     c                   Eval      trovata = *On
004700040204     C                   exsr      AGGIORNA
004800040204     C                   LEAVE
004900040204     c                   ENDIF
005000040204     c                   ENDIF
005100040204      *
005200040204     C     W015          reade     TITAS32C
005300040204     C                   ENDDO
005400040204      *
005500040205      * Cerco riferimento come alfanumerico
005600040205     c                   IF        TROVATA = *Off
005700040205     C                   MOVEL     EXMRM1        W015A
005800040204     C     W015A         setll     TITA435C
005900040204     C     W015A         reade     TITA435C
006000040204     C                   DOW       NOT %EOF(TITA435C)
006100040204      *
006200040204     C     kbol          CHAIN     TITAS30C
006300040204     C                   IF        %FOUND(TITAS30C)
006400040204      *
006500040204      * controllare tipo bolla
006600040204     C                   IF        TASKSC = 0660898
006700040206     C                             AND TASLNP = 66
006800040205     c                   z-add     1             xx
006900040204     c     TASTBL        lookup    SK_TB(xx)                              99
007000040204     c                   If        *in99 = *off
007100040205     c                   Eval      trovata = *On
007200040204     C                   exsr      AGGIORNA
007300040204     C                   LEAVE
007400040204     c                   ENDIF
007500040204     c                   ENDIF
007600040205     c                   ENDIF
007700040204      *
007800040205     C     W015A         reade     TITA435C
007900040204     C                   ENDDO
008000040205     c                   ENDIF
008100040204      *
008200040205
008300040205      *---------------------------------------------------------------*
008400040205      * Aggiorna il file WFMRL00F
008500030926      **-------------------------------------------------------------**
008600040205     c     Aggiorna      BegSr
008700040205
008800040205      * Calcolo il peso fatturato
008900040205     c                   Clear                   taspkff
009000040205
009100040205      * Peso fatturato solo se la bolla è fatturata
009200040205If  1c                   If        TasDft > *Zeros
009300040205      * Prendo il peso fatturato
009400040205     c                   If        TasFpf = 'R'
009500040205     c                   Z-Add     TasPkf        taspkff
009600040205     c                   EndIf
009700040205      * Prendo il peso da cml - la tara
009800040205If  2c                   If        TasFpf = 'V'
009900040205     c                   Clear                   dsQt1
010000040205     c                   Movel     'QT'          Kcod
010100040205     c     Ktab          Chain     Tabel00f
010200040205     c                   If        %Found(Tabel00f) and TblFlg = *Blanks
010300040205     c                   Movel     TblUni        dsQt1
010400040205     c                   EndIf
010500040205     c     §QtTpc        Mult      TasNcp        wTara
010600040205     c     TasPkc        Sub(h)    wTara         wNtara
010700040205     c                   Z-Add     wNtara        taspkff
010800040205    2c                   EndIf
010900040205    1c                   EndIf
011000040205
011100040205     c                   Update    Wfmrl000
011200040205
011300040205     c                   EndSr
011400040205
011500040205      **-------------------------------------------------------------**
011600040204     C     *INZSR        BEGSR
011700040205
011800040205     C     *ENTRY        PLIST
011900040205     C                   PARM                    KPJBA
012000040205      *
012100040205     C     KTAB          KLIST
012200040205     C                   KFLD                    CODUT
012300040205     C                   KFLD                    KCOD
012400040205      *
012500040205     C     KBOL          KLIST
012600040205     C                   KFLD                    Tasaas
012700040205     C                   KFLD                    Taslnp
012800040205     C                   KFLD                    Tasnrs
012900040205     C                   KFLD                    Tasnsp
013000040205     C                   KFLD                    Tastbl
013100040205
013200040205      **  carico tipi bolla da escludere - bolle recupero
013300040205     c                   Movel     'TB'          Kcod
013400040204     C     ktab          SETLL     TABEL
013500040204     C     ktab          READE     TABEL
013600040204     C                   DOW       NOT %EOF(TABEL00F)
013700040204     C                   movel     TBLUNI        DSTB
013800040204     c                   if        §TBrbl = 'R'
013900040204     c                   EVAL      XX = XX +1
014000040204     C                   MOVEL     TBLKEY        SK_TB(XX)
014100040204     C                   ENDIF
014200040204     C     ktab          READE     TABEL
014300040204     C                   ENDDO
014400040204      *
014500040204     C                   ENDSR
