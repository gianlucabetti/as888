000100990520      *PARMS DFTACTGRP(*YES)
000200990616      /TITLE Aggiunta dei riferimenti ai file.
000300990824
000400990824      **********************************************************************
000500990824      *    Questo programma analizza i campi dei DSPF e PRTF
000600990824      * e memorizza nel file dei riferimenti del data base i file a cui
000700990824      * fanno riferimento i campi.
000800990824      *    Siccome la libreria WHRLI del file di riferimento potrebbe
000900990824      * non esistere nell'AS/400, viene impostata uguale alla libreria
001000990824      * del DSPF/PRTF; questo per evitare di scrivere dei
001100990824      * record che sfuggirebbero alla pre-cancellazione per libreria.
001200990824      *
001300990824      *                                              Danilo Cussini
001400990824      **********************************************************************
001500990824
001600990616     Fqadspffd  ip   e             disk
001700990616     Fxrdbr01l  if a e           k disk
001800990524
001900990616     C                   if        whrfil <> *blanks
002000990616
002100990616     C                   eval      whrfi = whrfil
002200990824     C                   eval      whrli = whlib
002300990616     C                   eval      whrefi = whfile
002400990616     C                   eval      whreli = whlib
002500990616     C     k04dbr01      chain     xrdbr01l
002600990524     C                   if        not %found
002700990616     C                   eval      whrtyp = 'P'
002800990616     C                   write     qwhdrdbr
002900990524     C                   endif
003000990616
003100990616     C                   endif
003200990524
003300990524      /TITLE Operazioni iniziali.
003400990524     C     *inzsr        begsr
003500990524
003600990524     C     *entry        plist
003700990616     C                   parm                    libreria         10
003800990524
003900990616     C     k04dbr01      klist
004000990616     C                   kfld                    whrli
004100990616     C                   kfld                    whrfi
004200990616     C                   kfld                    whrefi
004300990616     C                   kfld                    whreli
004400990524
004500990524     C                   endsr
