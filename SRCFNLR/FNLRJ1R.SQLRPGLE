000100120523     H DATEDIT(*YMD)
000200000000      *****************************************************************
000300120523      *  Nome programma:  FNLR97R1
000400120523      *
000500120523      *  Descrizione   :  Pulisce il file di Work FNSVBW0F eliminando
000600120523      *      i dati più vecchi di 1 anno (x data variazione)
000700000000      *****************************************************************
000800120523     d dataoggi        s               d   Datfmt(*iso)
000900120523     d unannofa        s               d   Datfmt(*iso)
001000120523      *
001100120528     D KPJBA         E DS
001200941230     D WLBDAT          DS
001300941230     D  G02DAT                 1      8  0
001400941230     D  G02INV                 9     16  0
001500941230     D  G02ERR                17     17
001600941230     D  G02TGI                18     22  0
001700941230      **************************************************************************
001800120528     C     *ENTRY        PLIST
001900120528     C                   PARM                    KPJBA
002000000000      *
002100120523     c     *iso          move      *date         dataoggi
002200120523     c     dataoggi      subdur    1:*Y          unannofa
002300120523     c     *JIS          move      unannofa      data8_0           8 0
002400000000      *
002500120523     c/EXEC SQL
002600120523     c+  DELETE  FROM Fnsvbw0f WHERE WVBDTV < :data8_0
002700120523     C/END-EXEC
002800000000      *
002900950102     C     FINE          TAG
003000000000     C                   SETON                                        LR
003100000000      *****************************************************************
