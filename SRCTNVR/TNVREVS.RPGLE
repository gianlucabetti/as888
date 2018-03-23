000100990914     H DECEDIT('0,') DATEDIT(*DMY.)
000200040324     Ffnevs02l  UF   E           K DISK
000300040324     Ffnart27l  IF   E           K DISK
000400040324     Ffibrv07l  IF   E           K DISK
000500990914     FQsysprt   O    F  132        printer oflind(*inof)
000600990914     C*--------------------------------------------------------------------*
000700990914     C*  Ciclo Principale
000800990914     C*--------------------------------------------------------------------*
000900040324     c     *entry        plist
001000040324     c                   parm                    waggio            1
001100040324     C     Kart          KLIST
001200040324     C                   Kfld                    evsfls
001300040324     C                   Kfld                    evslna
001400040324     C                   Kfld                    evsnrs
001500040324     C                   Kfld                    evsnsc
001600040324     c                   clear                   wcont             7 0
001700040324     c     *loval        setll     fnevs02l
001800040324     c                   do        *hival
001900040324     c     waggio        ifeq      'S'
002000040324     c                   read      fnevs02l
002100040324     c                   else
002200040324     c                   read(N)   fnevs02l
002300040324     c                   endif
002400040324     c                   if        %eof(fnevs02l)
002500040324     c                   leave
002600040324     c                   endif
002700040325     c     evsnsc        ifeq      *zeros
002800040325     c                   iter
002900040325     c                   endif
003000040324     c     kart          setll     fnart27l                               30
003100040324     c     *in30         ifeq      *off
003200040324     c     kart          setll     fibrv07l                               30
003300040324     c     *in30         ifeq      *off
003400040324     c* prima di cancellare stampo l'evento
003500040324     c   of              except    testa
003600040324     c                   add       1             wcont
003700040324     C                   except    detta
003800040324     c     waggio        ifeq      'S'
003900040324     c                   delete    fnevs000
004000040324     c                   endif
004100040324     c                   endif
004200040324     c                   endif
004300040324     c                   enddo
004400040324     c                   seton                                        lr
004500040324     c*--------------------------------------------------------------*
004600990914     o* Stampa
004700990914     o*-------------------------------------------------------------*
004800990914     OQsysPrt   E            Testa            01
004900040324     o                                           66 'Elenco eventi segnacollo c'
005000040324     o                                              'ancellati                 '
005100990914     O          E            Testa          1 03
005200040324     o                                            4 'Fls'
005300040324     o                                            8 'Lna'
005400040324     o                                           11 'Sr'
005500040324     o                                           19 'Segnac.'
005600040324     o                                        +   2 'Evento'
005700990914     O          E            detta          1
005800040324     O                       evsfls               4
005900040324     O                       evslna               8
006000040324     O                       evsnrs              11
006100040324     O                       evsnsc              19
006200040324     O                       evscev              23
006300040324     O                       evsdev           +   2
006400040324     o                       wcont         z  +   2
