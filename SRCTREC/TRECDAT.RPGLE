000100021223     C*------------------------------------------------------------------------*
000200021223     C*    -  IMPOSTA IMMAGINE PRIMA E DOPO DI UN ESTENSIONE TRANSITI  -       *
000300021223     C*------------------------------------------------------------------------*
000400021223     C*------------------------------------------------------------------------*
000500021223     C*?APDATQ - AZZERA TUTTI I CAMPI DELLA DS -PRIMA-
000600021223     C*------------------------------------------------------------------------*
000700021223     C     APDATQ        BEGSR
000800021223     C*
000900021223     C                   MOVEL     *BLANKS       DATPATB
001000021223     C                   Z-ADD     *ZEROS        DATPAAS
001100021223     C                   Z-ADD     *ZEROS        DATPLNP
001200021223     C                   Z-ADD     *ZEROS        DATPNRS
001300021223     C                   Z-ADD     *ZEROS        DATPNSP
001400021223     C                   MOVEL     *BLANKS       DATPTRD
001500021223     C                   Z-ADD     *ZEROS        DATPDAC
001600021223     C                   Z-ADD     *ZEROS        DATPORC
001700021223     C                   Z-ADD     *ZEROS        DATPPRG
001800021223     C                   MOVEL     *BLANKS       DATPUNI
001900021223     C                   MOVEL     *BLANKS       DATPFT1
002000021223     C                   Z-ADD     *ZEROS        DATPDT1
002100021223     C                   MOVEL     *BLANKS       DATPFT2
002200021223     C                   Z-ADD     *ZEROS        DATPDT2
002300021223     C                   MOVEL     *BLANKS       DATPFT3
002400021223     C                   Z-ADD     *ZEROS        DATPDT3
002500021223     C                   MOVEL     *BLANKS       DATPXX1
002600021223     C*
002700021223     C                   ENDSR
002800021223     C*------------------------------------------------------------------------*
002900021223     C*?ADDATQ - AZZERA TUTTI I CAMPI DELLA DS -DOPO-
003000021223     C*------------------------------------------------------------------------*
003100021223     C     ADDATQ        BEGSR
003200021223     C*
003300021223     C                   MOVEL     *BLANKS       DATDATB
003400021223     C                   Z-ADD     *ZEROS        DATDAAS
003500021223     C                   Z-ADD     *ZEROS        DATDLNP
003600021223     C                   Z-ADD     *ZEROS        DATDNRS
003700021223     C                   Z-ADD     *ZEROS        DATDNSP
003800021223     C                   MOVEL     *BLANKS       DATDTRD
003900021223     C                   Z-ADD     *ZEROS        DATDDAC
004000021223     C                   Z-ADD     *ZEROS        DATDORC
004100021223     C                   Z-ADD     *ZEROS        DATDPRG
004200021223     C                   MOVEL     *BLANKS       DATDUNI
004300021223     C                   MOVEL     *BLANKS       DATDFT1
004400021223     C                   Z-ADD     *ZEROS        DATDDT1
004500021223     C                   MOVEL     *BLANKS       DATDFT2
004600021223     C                   Z-ADD     *ZEROS        DATDDT2
004700021223     C                   MOVEL     *BLANKS       DATDFT3
004800021223     C                   Z-ADD     *ZEROS        DATDDT3
004900021223     C                   MOVEL     *BLANKS       DATDXX1
005000021223     C*
005100021223     C                   ENDSR
005200021223     C*------------------------------------------------------------------------*
005300021223     C*?CPDATQ - IMPOSTA TUTTI I CAMPI DELLA DS -PRIMA-
005400021223     C*------------------------------------------------------------------------*
005500021223     C     CPDATQ        BEGSR
005600021223     C*
005700021223     C                   MOVEL     AR5ATB        DATPATB
005800021223     C                   Z-ADD     AR5AAS        DATPAAS
005900021223     C                   Z-ADD     AR5LNP        DATPLNP
006000021223     C                   Z-ADD     AR5NRS        DATPNRS
006100021223     C                   Z-ADD     AR5NSP        DATPNSP
006200021223     C                   MOVEL     AR5TRD        DATPTRD
006300021223     C                   Z-ADD     AR5DAC        DATPDAC
006400021223     C                   Z-ADD     AR5ORC        DATPORC
006500021223     C                   Z-ADD     AR5PRG        DATPPRG
006600021223     C                   MOVEL     AR5UNI        DATPUNI
006700021223     C                   MOVEL     AR5FT1        DATPFT1
006800021223     C                   Z-ADD     AR5DT1        DATPDT1
006900021223     C                   MOVEL     AR5FT2        DATPFT2
007000021223     C                   Z-ADD     AR5DT2        DATPDT2
007100021223     C                   MOVEL     AR5FT3        DATPFT3
007200021223     C                   Z-ADD     AR5DT3        DATPDT3
007300021223     C                   MOVEL     AR5XX1        DATPXX1
007400021223     C*
007500021223     C                   ENDSR
007600021223     C*------------------------------------------------------------------------*
007700021223     C*?CDDATQ - IMPOSTA TUTTI I CAMPI DELLA DS -DOPO-
007800021223     C*------------------------------------------------------------------------*
007900021223     C     CDDATQ        BEGSR
008000021223     C*
008100021223     C                   MOVEL     AR5ATB        DATDATB
008200021223     C                   Z-ADD     AR5AAS        DATDAAS
008300021223     C                   Z-ADD     AR5LNP        DATDLNP
008400021223     C                   Z-ADD     AR5NRS        DATDNRS
008500021223     C                   Z-ADD     AR5NSP        DATDNSP
008600021223     C                   MOVEL     AR5TRD        DATDTRD
008700021223     C                   Z-ADD     AR5DAC        DATDDAC
008800021223     C                   Z-ADD     AR5ORC        DATDORC
008900021223     C                   Z-ADD     AR5PRG        DATDPRG
009000021223     C                   MOVEL     AR5UNI        DATDUNI
009100021223     C                   MOVEL     AR5FT1        DATDFT1
009200021223     C                   Z-ADD     AR5DT1        DATDDT1
009300021223     C                   MOVEL     AR5FT2        DATDFT2
009400021223     C                   Z-ADD     AR5DT2        DATDDT2
009500021223     C                   MOVEL     AR5FT3        DATDFT3
009600021223     C                   Z-ADD     AR5DT3        DATDDT3
009700021223     C                   MOVEL     AR5XX1        DATDXX1
009800021223     C*
009900021223     C                   ENDSR
010000021223     C*------------------------------------------------------------------------*
