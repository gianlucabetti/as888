000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300041222     fFNARB27L  UF   E           K DISK
000400041222     fFNANM02L  IF   E           K DISK
000500041222     fFNART01L  IF   E           K DISK
000600990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000700980611      * ---------------------------------------------------------
000800041222     C     KARB          KLIST
000900041222     C                   KFLD                    KLNP
001000041222     C                   KFLD                    KAAS              4 0
001100041222     C                   KFLD                    KMGS              4 0
001200041222     C     KARB2         KLIST
001300041222     C                   KFLD                    KLNP
001400041222     C                   KFLD                    KAAS
001500041222     C     KART          KLIST
001600041222     C                   KFLD                    ARBAAS
001700041222     C                   KFLD                    ARBLNP
001800041222     C                   KFLD                    ARBNRS
001900041222     C                   KFLD                    ARBNSP
002000041222     C     KANM          KLIST
002100041222     C                   KFLD                    ARTFLS
002200041222     C                   KFLD                    ARTLNA
002300041222     C                   KFLD                    ARTNRS
002400041222     C                   KFLD                    ARTNSC
002500041222     C                   KFLD                    KCAA              3 0
002600041222     C
002700041222     C                   EVAL      KAAS=2004
002800041222     C                   EVAL      KMGS=1217
002900041222     C                   EVAL      KCAA=010
003000041222     C
003100041222     c                   clear                   KLNP              3 0
003200041222     c     kLNP          SETLL     fnarb000                           30
003300041222     c                   READ      fnarb000
003400041222     C
003500041222    1C                   DOW       NOT %EOF(FNARB27L)
003600041222     C                   EVAL      KLNP=ARBLNP
003700041222     C     KARB          SETLL     FNARB27L
003800041222     C     KARB2         READE     FNARB27L
003900041222     C
004000041222    2C                   DOW       NOT %EOF(FNARB27L)
004100041222     C                   CLEAR                   WAGGARB           1
004200041222     C* LETTO FNART
004300041222     C     KART          SETLL     FNART01L
004400041222     C     KART          READE     FNART01L
004500041222     C
004600041222    3C                   DOW       NOT %EOF(FNART01L)
004700041222     C     KANM          SETLL     FNANM02L                               31
004800041222    4C                   if        *in31
004900041222     c                   eval      waggarb='S'
005000041222     c                   leave
005100041222    4c                   endif
005200041222     c
005300041222     C     KART          READE     FNART01L
005400041222    3c                   enddo
005500041222     c
005600041222    3c                   if        waggarb='S'
005700041222     C                   EXCEPT    TESTA
005800041222     C
005900041222     c                   eval      arbdti=0
006000041222     c                   eval      arbhti=0
006100041222     c                   eval      arbfBs='S'
006200041222     c                   update    fnarb000
006300041222    3c                   endif
006400041222     c
006500041222     C     KARB2         READE     FNARB27L
006600041222    2c                   enddo
006700041222     c
006800041222     c     klnp          setgt     fnarb27l
006900041222     c                   read      fnarb27l
007000041222    1c                   enddo
007100041222     c
007200041222     c                   seton                                        lr
007300041222     c
007400041222     c
007500980611      *------------------------------------------------------------
007600041222     OPRTF198   E            TESTA       2
007700041222     O                                        +   0 'RICALCOLO DATA ARR '
007800041222     O                                        +   0 'ULTIMO COLLO:'
007900041222     O                       arbaas           +   4
008000041222     O                       arblnp           +   2
008100041222     O                       arbnrs        z  +   2
008200041222     O                       arbnsp        z  +   2
008300041222     O                       arbdti           +   2 '    /  /  '
008400041222     O                       arbhti           +   2 '  :  '
