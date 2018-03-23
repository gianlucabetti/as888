000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200020125     H* FICNB3R *----------------------------------------------------*
000300020417     H* questo pgm deve leggere le distinte di ritiro di determinate dat
000400020417     H* e e aggiornare il numero stop su fiftd
000500950127     H*--------------------------------------------------------------*
000600020417     FFIFTD04L  UF   E           K DISK
000700020417     D                 DS
000800020417     D  ftdPDR                 1      7  0
000900020417     D  ftdtsr                 8      8
001000020417     D  ftdndc                 9     15  0
001100020417     D  ftdddc                16     23  0
001200020417     d  ftdkey1                1     23
001300020417     D  ftdksc                24     30  0
001400020417     D  ftdrsc                31     65
001500020417     D  ftdkey                 1     65
001600020417     D                 DS
001700020417     D  savPDR                 1      7  0
001800020417     D  savtsr                 8      8
001900020417     D  savndc                 9     15  0
002000020417     D  savddc                16     23  0
002100020417     D  savkey1                1     23
002200020417     D  savksc                24     30  0
002300020417     D  savrsc                31     65
002400020417     D  savkey                 1     65
002500020408      *
002600950127     C*
002700020416     c     *loval        setll     fiftd04l
002800020416     c                   read      fiftd04l                               30
002900020417    1c     *in30         doweq     *off
003000020417    2c     ftdddc        ifeq      20020415
003100020417     c     ftdstp        andge     1000
003200020417     c* rottura distinta
003300020417     c     ftdkey1       ifne      savkey1
003400020417     c                   z-add     1000          contstp           5 0
003500020417     c                   move      ftdkey        savkey
003600020417     c                   endif
003700020417     c* rottura cliente
003800020417    3c     ftdkey        ifne      savkey
003900020417     c                   add       1             contstp
004000020417     c                   move      ftdkey        savkey
004100020417    3c                   endif
004200020417     c                   z-add     contstp       ftdstp
004300020417     c                   update    fiftd000
004400020417    2c                   endif
004500020416     c                   read      fiftd04l                               30
004600020417    1c                   enddo
004700020416     c
004800950127     C                   SETON                                        LR
