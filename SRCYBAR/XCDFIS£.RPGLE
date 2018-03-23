000100000000     H
000200000000     F*-------------------------------------------------------------*
000300000000     F*- Controllo formale CODICE FISCALE                          -*
000400000000     F*- Nel caso sia erratO il campo XSTATO = -1                  -*
000500880704     F*-------------------------------------------------------------*
000600880704     D CF              S              1    DIM(16)                              COD.FISC.D
000700880704     D CF1             S              2    DIM(8)                               SK COMODO
000800880704     D CF2             S              1    DIM(8)                               SK COMODO
000900880704     D CF3             S              1    DIM(7)                               SK COMODO
001000880704     D CF4             S              1    DIM(36) CTDATA PERRCD(36)
001100880704     D CF5             S              2  0 DIM(36) CTDATA PERRCD(36)
001200880704     D CF6             S              2  0 DIM(36) CTDATA PERRCD(36)
001300880704     D CF7             S              2  0 DIM(26) CTDATA PERRCD(26)
001400880704     D CF8             S              1    DIM(26) CTDATA PERRCD(26)
001500880704     C*
001600880704     C     *ENTRY        PLIST
001700880704     C                   PARM                    XCDFIS           16
001800880704     C                   PARM                    XSTATO            1 0
001900880704     C*
002000880704     C                   Z-ADD     0             XSTATO
002100880704     C                   MOVEA     XCDFIS        CF(1)
002200880704     C                   MOVEA     XCDFIS        CF1
002300070606     c                   if        XCDFIS='XXXXXXXXXXXXXXXX' or
002400070606     c                             XCDFIS='CCCCCCCCCCCCCCCC' or
002500070606     c                             XCDFIS='GGGGGGGGGGGGGGGG'
002600070606     C                   Z-SUB     1             XSTATO                         -1 = ERROR
002700070606     c                   goto      fine
002800070606     c                   endif
002900880704     C*
003000880704     C* DIVIDO I CARATTERI PARI DA QUELLI DISPARI
003100880704     C                   MOVEL     CF1           CF2
003200880704     C                   MOVE      CF1           CF3
003300880704     C*
003400880704     C* CF2 - DISPARI. FACCIO IL LOOP PER LA SOMMATORIA
003500880704     C                   Z-ADD     0             K                 1 0
003600880704     C     RUN16         TAG
003700880704     C     1             ADD       K             K
003800880704     C     K             COMP      8                                  16
003900880704     C   16              GOTO      RU16
004000880704     C                   Z-ADD     1             W                 2 0
004100880704     C     CF2(K)        LOOKUP    CF4(W)                                 28
004200880704     C  N28              SETON                                        19        ON = ERROR
004300880704     C   19              GOTO      RU19
004400880704     C     CF5(W)        ADD       CFI1          CFI1              3 0          SOMMATORIA
004500880704     C                   GOTO      RUN16
004600880704     C     RU16          TAG
004700880704     C*
004800880704     C* CF3 -  PARI. FACCIO IL LOOP PER LA SOMMATORIA
004900880704     C                   Z-ADD     0             K
005000880704     C     RUN18         TAG
005100880704     C     1             ADD       K             K
005200880704     C     K             COMP      7                                  18
005300880704     C   18              GOTO      RU18
005400880704     C                   Z-ADD     1             W
005500880704     C     CF3(K)        LOOKUP    CF4(W)                                 28
005600880704     C  N28              SETON                                        19        19 = ERROR
005700880704     C   19              GOTO      RU19
005800880704     C     CF6(W)        ADD       CFI1          CFI1                           SOMMATORIA
005900880704     C                   GOTO      RUN18
006000880704     C     RU18          TAG
006100880704     C*
006200880704     C* DIVIDO LA SOMMATORIA PER 26 E SALVO IL RESTO PER IL TEST FINALE
006300880704     C     CFI1          DIV       26            CFI2              2 0
006400880704     C                   MVR                     CFI3              2 0
006500880704     C*
006600880704     C* CERCO IL CARATTERE CORRISPONDENTE AL RESTO E TESTO SE IL C.F.
006700880704     C* E' OK
006800880704     C                   Z-ADD     1             W
006900880704     C     CFI3          LOOKUP    CF7(W)                                 17
007000880704     C     CF(16)        COMP      CF8(W)                             1919      19 = ERROR
007100880704     C     RU19          TAG
007200880704     C   19              Z-SUB     1             XSTATO                         -1 = ERROR
007300070606     c
007400070606     c     fine          tag
007500880704     C                   SETON                                        LR
007600880704**        CF4
0077008807040123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
007800880704**        CF5
007900880704010005070913151719210100050709131517192102041820110306081214161022252423
008000880704**        CF6
008100880704000102030405060708090001020304050607080910111213141516171819202122232425
008200880704**        CF7
0083008807040001020304050607080910111213141516171819202122232425
008400880704**        CF8
008500880704ABCDEFGHIJKLMNOPQRSTUVWXYZ
