000100000000      *****************************************************************
000200060801      *  Pulisce   TNECF e TNEFR
000300000000      *****************************************************************
000400040819     H DECEDIT('0,') DATEDIT(*DMY.)
000500030116      *****************************************************************
000600030116     d KPJBA         E DS
000700040819     D*
000800040819     D fieu88ds      e DS                  inz
000900940926     D*
001000000000     D WLBDAT          DS
001100940926     D  G02DAT                 1      8  0
001200940926     D  G02INV                 9     16  0
001300940926     D  G02ERR                17     17
001400940926     D  G02TGI                18     22  0
001500040819      *
001600040819     D  data_Oggi      s                   like(G02INV) INZ(0)
001700040923     D  data_0         s                   like(G02INV) INZ(0)
001800030205      *
001900030116     D digits          C                   CONST('0123456789')
002000030116      *****************************************************************
002100000000     C     *ENTRY        PLIST
002200000000     C                   PARM                    KPJBA
002300040819     C                   movel     Kpjbu         Fieu88ds
002400040819      *
002500040819      *  imposta UDATE di elaborazione
002600040819     C                   move      *date         G02DAT
002700040819     C                   MOVE      *ZEROS        G02INV
002800040819     C                   MOVE      *BLANKS       G02ERR
002900040819     C                   CALL      'XSRDA8'
003000040819     C                   PARM                    WLBDAT
003100040819     C                   Z-ADD     G02INV        data_oggi
003200040923      *
003300000000      *--------------------------------------------------------------*
003400060801      * ?   Tutto il saldificato entro i giorni
003500060801      *--------------------------------------------------------------*
003600060801      *
003700060801      *  Dati di Confronto (File principale)
003800060801     C/EXEC SQL
003900060801     C+ delete from TNECF00F
004000060801     C+  where ecfdtg > 0 and ecfNTW= :E88ntw and
004100060802     C+   ((ecfDFP > 0 and  ecfDFP <= :E88dtf) or
004200060802     C+    (ecfDFP = 0 and ecfaas*10000+ecfmgs <= :E88dsp) or
004300060802     C+    (ecfDFP = 0 and ecfaas*10000+ecfmgs = 0))
004400060801     C/END-EXEC
004500060801      *
004600060801      *  Dati del Partner (File arrivato dal Partner)
004700060801     C/EXEC SQL
004800060801     C+ delete from TNEFR00F
004900060801     C+  where efrdtc > 0 and efrNTW= :E88ntw and
005000060802     C+    ((efrDFT > 0 and efrDFT <= :E88dtf) or
005100060802     C+    (efrDFT = 0 and efrDTE <= :E88dtf))
005200060801     C/END-EXEC
005300060801      *
005400060801      *  pulisce anche il vecchio storico FEDEX
005500060801     c                   if        e88NTW ='FED'
005600060801      *
005700060801      *  x FEDEX pulisce anche lo storico dei dati originali
005800060801      *          importati da Fedex x il controllo fatture.
005900060801     C/EXEC SQL
006000060801     C+ delete   from fifeis0w where rrn(fifeis0w) <
006100060801     C+  (SELECT min(rrn(fifeis0w)) FROM fifeis0w
006200060801     C+   WHERE WRKDAT like 'HA1INVOICE%'
006300060801     C+     and dec(substr(wrkdat, 57, 8)) >= :e88DTF)
006400060801     C/END-EXEC
006500060801      *
006600060801     c                   end
006700040903      *
006800000000     C     FINE          TAG
006900000000     C                   SETON                                        LR
007000000000      *--------------------------------------------------------------*
