000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300060912     ffiarg01l  iF a E           K DISK
000400060912     fFNARb26l  IF   E           K DISK
000500990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000600060912     d
000700060912     dkaasarg          s              4  0 inz
000800980611      * ---------------------------------------------------------
000900041222     C     KARB          KLIST
001000060912     C                   KFLD                    klna              3 0
001100041222     C                   KFLD                    KAAS              4 0
001200041222     C                   KFLD                    KMGS              4 0
001300041222     C
001400060912     C                   EVAL      Klna=048
001500060912     C                   EVAL      KAAS=2006
001600060912     C                   EVAL      KMGS=0208
001700060912     C                   EVAL      Kaasarg=9999
001800041222     C
001900060912     C     kaasarg       SETLL     Fiarg01l
002000060912     C     KARB          SETLL     FNARB26L
002100060912     C     KARB          READE     FNARB26L
002200041222     C
002300060912    2C                   DOW       NOT %EOF(FNARB26L)
002400060912     c* Leggo anche fiarg
002500060912     c     kaasarg       reade     fiarg01l
002600060912     c                   if        not %eof(fiarg01l)
002700060912     c                   eval      argaas=arbaas
002800060912     c                   eval      arglnp=arblnp
002900060912     c                   eval      argnrs=arbnrs
003000060912     c                   eval      argnsp=arbnsp
003100060912     c                   write     fiarg000
003200060912     c                   endif
003300041222     c
003400060912     C     KARB          READE     FNARB26L
003500041222    2c                   enddo
003600060912     c
003700060912     c                   seton                                        LR
