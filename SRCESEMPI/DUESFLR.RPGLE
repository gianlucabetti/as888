000100011026      /TITLE Gestione di 2 subfile affiancati.
000200011026
000300011026     FDueSflD   CF   E             WORKSTN
000400011026     F                                     SFILE(S01:S01RRN)
000500011026     F                                     SFILE(S02:S02RRN)
000600011026
000700011026     D Sinistra        C                   'S'
000800011026     D Destra          C                   'D'
000900011026     D WrkSflChoice    S              1
001000011026     D                                     INZ(Sinistra)
001100011026
001200011026     C* Scrittura subfile.
001300011026     C                   DO        30            S01RRN
001400011026     C                   EVAL      S02RRN = S01RRN
001500011026     C                   WRITE     S01
001600011026     C                   WRITE     S02
001700011026     C                   ENDDO
001800011026     C* Emissione testata, comandi e subfile di destra.
001900011026     C                   WRITE     T01
002000011026     C                   WRITE     F01
002100011026     C                   WRITE     C02
002200011026     C*
002300011026     C                   DOU       *INKC
002400011026     C*
002500011026     C                   SELECT
002600011026     C* Emissione del subfile di sinistra,
002700011026     C* su cui si posiziona il cursore.
002800011026     C                   WHEN      WrkSflChoice = Sinistra
002900011026     C                   EXFMT     C01
003000011026     C                   IF        *INKK
003100011026     C                   EVAL      WrkSflChoice = Destra
003200011026     C                   ENDIF
003300011026     C* Emissione del subfile di destra,
003400011026     C* su cui si posiziona il cursore.
003500011026     C                   WHEN      WrkSflChoice = Destra
003600011026     C                   EXFMT     C02
003700011026     C                   IF        *INKK
003800011026     C                   EVAL      WrkSflChoice = Sinistra
003900011026     C                   ENDIF
004000011026     C*
004100011026     C                   ENDSL
004200011026     C*
004300011026     C                   ENDDO
004400011026     C*
004500011026     C                   EVAL      *INLR = *ON
