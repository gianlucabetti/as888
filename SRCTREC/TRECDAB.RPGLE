000100021223     C*------------------------------------------------------------------------*
000200021223     C*    -  IMPOSTA IMMAGINE PRIMA E DOPO DI UN ESTENSIONE BANCALI  -       *
000300021223     C*------------------------------------------------------------------------*
000400021223     C*------------------------------------------------------------------------*
000500021223     C*?APDABQ - AZZERA TUTTI I CAMPI DELLA DS -PRIMA-
000600021223     C*------------------------------------------------------------------------*
000700021223     C     APDABQ        BEGSR
000800021223     C*
000900021223     C                   MOVEL     *BLANKS       DABPATB
001000021223     C                   Z-ADD     *ZEROS        DABPAAS
001100021223     C                   Z-ADD     *ZEROS        DABPLNP
001200021223     C                   Z-ADD     *ZEROS        DABPNRS
001300021223     C                   Z-ADD     *ZEROS        DABPNSP
001400021223     C                   MOVEL     *BLANKS       DABPTRD
001500021223     C                   Z-ADD     *ZEROS        DABPDAC
001600021223     C                   Z-ADD     *ZEROS        DABPORC
001700021223     C                   Z-ADD     *ZEROS        DABPPRG
001800021223     C                   MOVEL     *BLANKS       DABPUNI
001900021223     C                   MOVEL     *BLANKS       DABPFT1
002000021223     C                   Z-ADD     *ZEROS        DABPDT1
002100021223     C                   MOVEL     *BLANKS       DABPFT2
002200021223     C                   Z-ADD     *ZEROS        DABPDT2
002300021223     C                   MOVEL     *BLANKS       DABPFT3
002400021223     C                   Z-ADD     *ZEROS        DABPDT3
002500021223     C                   MOVEL     *BLANKS       DABPXX1
002600021223     C*
002700021223     C                   ENDSR
002800021223     C*------------------------------------------------------------------------*
002900021223     C*?ADDABQ - AZZERA TUTTI I CAMPI DELLA DS -DOPO-
003000021223     C*------------------------------------------------------------------------*
003100021223     C     ADDABQ        BEGSR
003200021223     C*
003300021223     C                   MOVEL     *BLANKS       DABDATB
003400021223     C                   Z-ADD     *ZEROS        DABDAAS
003500021223     C                   Z-ADD     *ZEROS        DABDLNP
003600021223     C                   Z-ADD     *ZEROS        DABDNRS
003700021223     C                   Z-ADD     *ZEROS        DABDNSP
003800021223     C                   MOVEL     *BLANKS       DABDTRD
003900021223     C                   Z-ADD     *ZEROS        DABDDAC
004000021223     C                   Z-ADD     *ZEROS        DABDORC
004100021223     C                   Z-ADD     *ZEROS        DABDPRG
004200021223     C                   MOVEL     *BLANKS       DABDUNI
004300021223     C                   MOVEL     *BLANKS       DABDFT1
004400021223     C                   Z-ADD     *ZEROS        DABDDT1
004500021223     C                   MOVEL     *BLANKS       DABDFT2
004600021223     C                   Z-ADD     *ZEROS        DABDDT2
004700021223     C                   MOVEL     *BLANKS       DABDFT3
004800021223     C                   Z-ADD     *ZEROS        DABDDT3
004900021223     C                   MOVEL     *BLANKS       DABDXX1
005000021223     C*
005100021223     C                   ENDSR
005200021223     C*------------------------------------------------------------------------*
005300021223     C*?CPDABQ - IMPOSTA TUTTI I CAMPI DELLA DS -PRIMA-
005400021223     C*------------------------------------------------------------------------*
005500021223     C     CPDABQ        BEGSR
005600021223     C*
005700021223     C                   MOVEL     AR5ATB        DABPATB
005800021223     C                   Z-ADD     AR5AAS        DABPAAS
005900021223     C                   Z-ADD     AR5LNP        DABPLNP
006000021223     C                   Z-ADD     AR5NRS        DABPNRS
006100021223     C                   Z-ADD     AR5NSP        DABPNSP
006200021223     C                   MOVEL     AR5TRD        DABPTRD
006300021223     C                   Z-ADD     AR5DAC        DABPDAC
006400021223     C                   Z-ADD     AR5ORC        DABPORC
006500021223     C                   Z-ADD     AR5PRG        DABPPRG
006600021223     C                   MOVEL     AR5UNI        DABPUNI
006700021223     C                   MOVEL     AR5FT1        DABPFT1
006800021223     C                   Z-ADD     AR5DT1        DABPDT1
006900021223     C                   MOVEL     AR5FT2        DABPFT2
007000021223     C                   Z-ADD     AR5DT2        DABPDT2
007100021223     C                   MOVEL     AR5FT3        DABPFT3
007200021223     C                   Z-ADD     AR5DT3        DABPDT3
007300021223     C                   MOVEL     AR5XX1        DABPXX1
007400021223     C*
007500021223     C                   ENDSR
007600021223     C*------------------------------------------------------------------------*
007700021223     C*?CDDABQ - IMPOSTA TUTTI I CAMPI DELLA DS -DOPO-
007800021223     C*------------------------------------------------------------------------*
007900021223     C     CDDABQ        BEGSR
008000021223     C*
008100021223     C                   MOVEL     AR5ATB        DABDATB
008200021223     C                   Z-ADD     AR5AAS        DABDAAS
008300021223     C                   Z-ADD     AR5LNP        DABDLNP
008400021223     C                   Z-ADD     AR5NRS        DABDNRS
008500021223     C                   Z-ADD     AR5NSP        DABDNSP
008600021223     C                   MOVEL     AR5TRD        DABDTRD
008700021223     C                   Z-ADD     AR5DAC        DABDDAC
008800021223     C                   Z-ADD     AR5ORC        DABDORC
008900021223     C                   Z-ADD     AR5PRG        DABDPRG
009000021223     C                   MOVEL     AR5UNI        DABDUNI
009100021223     C                   MOVEL     AR5FT1        DABDFT1
009200021223     C                   Z-ADD     AR5DT1        DABDDT1
009300021223     C                   MOVEL     AR5FT2        DABDFT2
009400021223     C                   Z-ADD     AR5DT2        DABDDT2
009500021223     C                   MOVEL     AR5FT3        DABDFT3
009600021223     C                   Z-ADD     AR5DT3        DABDDT3
009700021223     C                   MOVEL     AR5XX1        DABDXX1
009800021223     C*
009900021223     C                   ENDSR
010000021223     C*------------------------------------------------------------------------*
