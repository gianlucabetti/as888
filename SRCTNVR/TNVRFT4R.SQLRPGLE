000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300160610     ftitas40c  if   e           k disk
000400160610     d wrkgetlista     s           4096    varying
000500020828     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600000607      *--------------------------------------------------------------------
000700000608      * reperimento dati foglio
000800160610     c                   clear                   savfiv            3 0
000900160610     c                   clear                   savnft
001000160610     c                   clear                   wmancafat         1
001100160610     c                   clear                   wnft              6 0
001200160613     c                   clear                   assap             7 0
001300160613     c                   clear                   assann            7 0
001400160613     c                   clear                   asssto            7 0
001500160613     c                   clear                   presto            7 0
001600160613     c                   clear                   preok             7 0
001700160613     c                   clear                   assok             7 0
001800160610
001900160613     c     001           setll     titas40c
002000160613     c                   read      titas40c
002100160613    1c                   dow       not %eof(titas40c)
002200160613    2c                   if        tasfiv<401   and tasaas=2016
002300160610
002400160613    3c                   if        savfiv<>tasfiv
002500160613     c                   except    tot
002600160610     c                   clear                   buchi             7 0
002700160610     c                   clear                   savnft            6 0
002800160613     c                   eval      savfiv=tasfiv
002900160613     c                   eval      savnft=tasnft
003000160613     c                   clear                   assap             7 0
003100160613     c                   clear                   assann            7 0
003200160613     c                   clear                   asssto            7 0
003300160613     c                   clear                   presto            7 0
003400160613     c                   clear                   preok             7 0
003500160613     c                   clear                   assok             7 0
003600160610    3c                   endif
003700160613
003800160613     c                   select
003900160613    4c                   when      tascbo='M '
004000160613     c                   add       1             presto            7 0
004100160613    4c                   when      tastbl='AS'
004200160613     c                   add       1             asssto            7 0
004300160613    4c                   when      tastbl='AP'
004400160613     c                   add       1             assAP             7 0
004500160613    4c                   when      tastbl='FS' or tastbl='F7'
004600160613     c                   add       1             presto            7 0
004700160613    4c                   when      %subst(tastbl:1:1)='A'
004800160613     c                   add       1             assok             7 0
004900160613    4c                   other
005000160613     c                   add       1             preok
005100160613    4c                   endsl
005200160610
005300160613     c     tasnft        sub       savnft        resto             7 0
005400160610     c
005500160610    3c                   if        resto>1
005600160610     c                   eval      savnft=savnft+1
005700160610
005800160613    4c                   dow       tasnft>savnft
005900160610     c                   except    det
006000160613     c                   add       1             assann
006100160610
006200160610     c                   eval      savnft=savnft+1
006300160610    4c                   enddo
006400160613    3c                   endif
006500160610     c
006600160613     c                   eval      savnft=tasnft
006700160610
006800160613    2c                   endif
006900160610     c
007000160613     c                   read      titas40c
007100160610    1c                   enddo
007200160610     c
007300160613     c                   except    tot
007400160610     c                   seton                                        lr
007500160610      /free
007600160610
007700160613     OQSYSPRT   E            det        02
007800160610     O                                         +  0 'Fatture mancanti'
007900160610     O                       savfiv        z   +  2
008000160610     o                       savnft        z   +  2
008100160613     OQSYSPRT   E            tot        01
008200160613     o                                         +  0 'Filiale iva'
008300160613     O                       savfiv            +  1
008400160613     OQSYSPRT   E            tot        01
008500160613     O                                         +  0 'Totale PREPAGATI     '
008600160613     O                       preOK         z   +  1
008700160613     O                                         +  2 'Prepagati stornati'
008800160613     O                       presto        z   +  1
008900160613     OQSYSPRT   E            tot        01
009000160613     O                                         +  0 'Totale FATT.ASSEGNATO'
009100160613     O                       ASSOK         z   +  1
009200160613     O                                         +  2 'Assegnati stornati'
009300160613     O                       asssto        z   +  1
009400160613     O                                         +  2 'Assegnati AP'
009500160613     O                       assAP         z   +  1
009600160613     O                                         +  2 'Assegnati annullati'
009700160613     O                       assann        z   +  1
