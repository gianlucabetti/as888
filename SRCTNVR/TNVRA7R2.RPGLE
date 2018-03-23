000100941214     H DECEDIT('0,') DATEDIT(*DMY.)
000200941214     F* FNLRA7R *-----------------------------------------------------*
000300941214     F*   PULIZIA F.VIAGGIO / FOGLI VARI / F.VIAGGIO PART. IN ARRIVO  *
000400921111     F*---------------------------------------------------------------*
000500051102     FFNFGV01L  iF   E           K DISK
000600000223     FFNFVA03L  UF   E           K DISK
000700000502     FFNFWA01L  UF   E           K DISK
000800000223     FFNFVA01L  IF   E           K DISK
000900000223     F                                     RENAME(FNFVA000:FNFVA001)
001000951207     FFNFV201L  UF   E           K DISK
001100951207     FFNFV401L  UF   E           K DISK
001200921111     D*
001300020809      * Variabili
001400020809      *
001500050103     D Klnp            S                   LIKE(fgvlnp)
001600921111     C*---------------------------------------------------------------*
001700921111     C*  INDICATORI USATI                                             *
001800921111     C*---------------------------------------------------------------*
001900941215     C* 01    - DELETO LA SPUNTA
002000941215     C* 30/36 - DI COMODO
002100921111     C*---------------------------------------------------------------*
002200930216     C*
002300930216     C* DEFINIZIONE VARIABILI E KLIST
002400921111     C                   EXSR      DEFVAR
002500941214     C****
002600941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
002700941214     C** I N   A R R I V O
002800941214     C****
002900941215     C                   Z-ADD     0             FIL               3 0
003000941215     C                   SETOFF                                       33
003100941215     C*
003200941215    3C     *IN33         DOWEQ     *OFF
003300000223     C     FIL           SETGT     FNFVA03L
003400000223     C                   READ      FNFVA03L                               33
003500941215     C*
003600941215    4C     *IN33         IFEQ      *OFF
003700941215     C                   Z-ADD     FVALNP        FIL
003800941215     C*
003900941215     C     FIL           SETLL     FNFVA000
004000941215     C     FIL           READE     FNFVA000                               31
004100941215     C*
004200941215    5C     *IN31         DOWEQ     '0'
004300941215     C*
004400051102     c                   if        (fvattr='L' and fvanfa=0) or
004500051102     c                             (fvanfa=83589 and fvalai=11)
004600941215     C                   SETOFF                                       01
004700051102     C     KFWA          delete    FNFWA000                           36
004800941215     C  N01              DELETE    FNFVA000
004900051102     c                   endif
005000941215     C*
005100941215     C     FIL           READE     FNFVA000                               31
005200941215    5C                   ENDDO
005300051102    4C                   ENDIF
005400051102    3C                   ENDDO
005500941215     C*
005600021104     C****
005700021104     C** P U L I Z I A   F V 4
005800021104     C****
005900941215     C*
006000960214     C* CANCELLO FV4 SOLO SE NON ESISTE PIU' FNFVA
006100021104      * e FnFgv
006200960214     C     *LOVAL        SETLL     FNFV401L
006300960214     C                   READ      FNFV401L                             0136
006400960214    3C     *IN36         DOWEQ     *OFF
006500960214     C     *IN01         ANDEQ     *OFF
006600021104      * Per prima cosa devo controllare che non esista FGV
006700021104     c     Kfgv          setll     Fnfgv01l
006800021104If  4c                   If        Not %Equal(Fnfgv01l)
006900021104      * Poi controllo che non esista FVA
007000021104If  5c                   If        Fv4Lai > *Zeros
007100021104     c     Kfva          Setll     Fnfva01l
007200021104   x5c                   Else
007300021104     c     Kfva1         Setll     Fnfva01l
007400021104     c                   EndIf
007500021104If  5c                   If        Not %Equal(Fnfva01l)
007600960214     C* SE NON TROVATO CANCELLO
007700021104     C                   DELETE    FNFV4000
007800021104    5C                   ENDIF
007900021104    4C                   ENDIF
008000960314     C                   READ      FNFV401L                             0136
008100960214    3C                   ENDDO
008200921111     C*
008300921111     C                   SETON                                        LR
008400921111     C*---------------------------------------------------------------*
008500921111     C*                    DEFVAR                                     *
008600921111     C*  ROUTINE DI DEFINIZIONE KEYLIST E VARIABILI                   *
008700921111     C*---------------------------------------------------------------*
008800921111     C     DEFVAR        BEGSR
008900000502     C     KFWA          KLIST
009000000502     C                   KFLD                    FVALNP
009100000502     C                   KFLD                    FVANFV
009200000502     C                   KFLD                    FVALAI
009300930930     C*
009400021104      * Fnfgv01
009500021104     c     Kfgv          Klist
009600021104     c                   Kfld                    Fv4Nfv
009700051102     c                   Kfld                    FvaLnp
009800021104      * Fnfva01
009900021104     c     Kfva          Klist
010000021104     c                   Kfld                    Fv4Lnp
010100021104     c                   Kfld                    Fv4Nfv
010200021104     c                   Kfld                    Fv4Lai
010300021104     c     Kfva1         KLIST
010400021104     C                   KFLD                    FV4LNP
010500021104     C                   KFLD                    FV4NFV
010600921111     C                   ENDSR
