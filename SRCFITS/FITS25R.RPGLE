000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040915      * FITS25R  *----------------------------------------------------*
000400040915      *  e.d.i.  edrde00f
000500021010      *---------------------------------------------------------------* c
000600040915     fedrdeNEW  o  A e           k disk    rename(edrde000:rdeNEW)
000700040915     fedrde00F  If   e             disk
000800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021008      *
001000021010     d KPJBA         E DS
001100040805     D* Schiera p.o. da trasferire
001200040805     D  SKPO           s              3    dim(500)
001300040805      *
001400021008      *****************************************************************
001500021010     c     *ENTRY        PLIST
001600021010     c                   PARM                    KPJBA
001700040805     C                   parm                    PARAM          1500
001800040805     c
001900040805     c     ' ':'0'       xlate     param         param
002000040805     c                   movea     param         skpo
002100021010      *---------------------------------------------------------------*
002200021008      ****
002300040910      ** LETTURA
002400021008      ****
002500040915     c                   READ      edrde00f                               22
002600040910    1c     *IN22         DOWEQ     *OFF
002700040805     c
002800040915     c                   movel     rdelnp        wlinea            3
002900040805     c     wlinea        lookup    skpo                                   35
003000040913     C
003100040910    2c                   if        *in35
003200040915     c                   write     rdeNEW                               99
003300040915     c  n99              add       1             Totrde            7 0
003400040910    3c                   endif
003500021008      *
003600040915     c                   READ      edrde00f                               22
003700040910    1c                   ENDDO
003800021010      *
003900040805     c                   EXCEPT    RINUM1
004000021011     c                   seton                                        LR
004100021010      *
004200021010      *---------------------------------------------------------------*
004300040805     oQSYSPRT   E            RINUM1         2 02
004400040915     o                                         +  5 'E.D.I.'
004500040915     o                                         +  5 'Fits25R'
004600021010     o                       UDATE             +  2 '  /  /  '
004700021010     o          E            RINUM1      1
004800040915     o                                         +  3 'EDRDE00F:'
004900040915     o                       TOTrde        2   +  2
