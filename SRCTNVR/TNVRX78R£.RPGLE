000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300120111     FFNARB01L  IF   E           K DISK
000400150121     FFNANMWUF  o    E             DISK
000500150121     FFnanm04L  iF   E           K DISK
000600150121     FFnanm15L  iF   E           K DISK    rename(fnanm000:fnanm015)
000700150121     f                                     prefix(an2:3)
000800000602      *--------------------------------------------------
000900120111     c     karb          klist
001000120111     c                   kfld                    anmaas
001100120111     c                   kfld                    anmlnp
001200120111     c                   kfld                    anmnrs
001300120111     c                   kfld                    anmnsp
001400150121     c     kanm15        klist
001500150121     c                   kfld                    kcaa              3 0
001600150121     c                   kfld                    anmscn
001700150121     c                   z-add     010           kcaa
001800120111     C*
001900120111     c                   clear                   conta             7 0
002000021216      *
002100120111     C     115           setll     fnanm04l
002200120111     C     115           reade     fnanm04l
002300120111    1c                   DOW       not %EOF(FNAnm04l)
002400150121     c*
002500150121     c                   if        anmaas=2015
002600150121    2c                   if        (anmnrs=40 and anmfls=63) or
002700150121     c                             (anmnrs=40 and anmfls=43)
002800150121     c* verifico se c'è anomalia 010
002900150121
003000150121     c     kanm15        setll     fnanm15l
003100150121     c     kanm15        reade     fnanm15l
003200150121    3c                   dow       not %eof(fnanm15l)
003300150121    4c                   if        an2nrs=40 and an2lna=anmlna
003400150121     c                   leave
003500150121     c                   else
003600150121     c     kanm15        reade     fnanm15l
003700150121    4c                   endif
003800150121    3c                   enddo
003900150121
004000150121    3c                   if        not %eof(fnanm15l)
004100120111     c     karb          chain     fnarb01l
004200150121    4c                   if        %found(fnarb01l)
004300150121     c                   eval      anmfls=an2fls
004400150121     c                   write     wnanm000
004500021213      *
004600150121    4c                   endif
004700150121    3c                   endif
004800150121    2c                   endif
004900150121    2c                   endif
005000120111     C     115           reade     fnanm04l
005100120111    1c                   ENDDO
005200021213      *
005300021213     C                   eval       *inlr = *on
