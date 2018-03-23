000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300041222     fFNARB27L  UF   E           K DISK
000400041222     fFNART01L  IF   E           K DISK
000500990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000600980611      * ---------------------------------------------------------
000700041222     C     KARB          KLIST
000800041222     C                   KFLD                    KLNP
000900041222     C                   KFLD                    KAAS              4 0
001000041222     C                   KFLD                    KMGS              4 0
001100041222     C     KARB2         KLIST
001200041222     C                   KFLD                    KLNP
001300041222     C                   KFLD                    KAAS
001400041222     C     KART          KLIST
001500041222     C                   KFLD                    ARBAAS
001600041222     C                   KFLD                    ARBLNP
001700041222     C                   KFLD                    ARBNRS
001800041222     C                   KFLD                    ARBNSP
001900041222     C
002000050224     C                   EVAL      KAAS=2005
002100050224     C                   EVAL      KMGS=0201
002200041222     C
002300041222     c                   clear                   KLNP              3 0
002400041222     c     kLNP          SETLL     fnarb000                           30
002500041222     c                   READ      fnarb000
002600041222     C
002700041222    1C                   DOW       NOT %EOF(FNARB27L)
002800041222     C                   EVAL      KLNP=ARBLNP
002900041222     C     KARB          SETLL     FNARB27L
003000041222     C     KARB2         READE     FNARB27L
003100041222     C
003200041222    2C                   DOW       NOT %EOF(FNARB27L)
003300050224     c                   clear                   waggvuc           1
003400050224     c                   clear                   waggpuc           1
003500041222     C* LETTO FNART
003600041222     C     KART          SETLL     FNART01L
003700041222     C     KART          READE     FNART01L
003800041222     C
003900041222    3C                   DOW       NOT %EOF(FNART01L)
004000050224     c
004100050224     c                   if        artvuc>0
004200050224     c                   eval      waggvuc='S'
004300050224     c                   endif
004400050224     c                   if        artpuc>0
004500050224     c                   eval      waggpuc='S'
004600050224     c                   endif
004700050224     c
004800050224     c                   if        waggpuc='S' and waggvuc='S'
004900050224     c                   leave
005000050224     c                   endif
005100041222     c
005200041222     C     KART          READE     FNART01L
005300041222    3c                   enddo
005400041222     c
005500050224    3c                   if        (waggvuc='S' and arbvlc=0) or
005600050224     c                             (waggpuc='S' and arbpkc=0)
005700050224     c                   eval      arbfbs='E'
005800050224     c                   update    fnarb000
005900050224     c
006000050224     c                   add       1             cont              9 0
006100041222     C                   EXCEPT    TESTA
006200050224     c                   endif
006300041222     C
006400041222     c
006500041222     C     KARB2         READE     FNARB27L
006600041222    2c                   enddo
006700041222     c
006800041222     c     klnp          setgt     fnarb27l
006900041222     c                   read      fnarb27l
007000041222    1c                   enddo
007100041222     c
007200050224     c                   except    conta
007300041222     c                   seton                                        lr
007400041222     c
007500980611      *------------------------------------------------------------
007600041222     OPRTF198   E            TESTA       2
007700050224     O                                        +   0 'Bolle da aggiornare'
007800041222     O                       arbaas           +   4
007900041222     O                       arblnp           +   2
008000041222     O                       arbnrs        z  +   2
008100041222     O                       arbnsp        z  +   2
008200050224     O                       arblna           +   2
008300050224     O                       arbmgs           +   2 '  /  '
008400050224     OPRTF198   E            conta       2
008500050224     O                                        +   0 'totale:'
008600050224     O                       cont          2  +   2
