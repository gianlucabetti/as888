000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300160610     ffiar635l  if   e           k disk
000400160610     ffiarbt0f  if   e             disk
000500160610     ftitas40c  if   e           k disk
000600160613     ffnblp01l  if   e           k disk
000700160610     d wrkgetlista     s           4096    varying
000800020828     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900000607      *--------------------------------------------------------------------
001000000608      * reperimento dati foglio
001100160610     c                   clear                   savfiv            3 0
001200160610     c                   clear                   savnft
001300160610     c                   clear                   wmancafat         1
001400160610     c                   clear                   wnft              6 0
001500160613     c                   clear                   assap             7 0
001600160613     c                   clear                   assann            7 0
001700160613     c                   clear                   asssto            7 0
001800160613     c                   clear                   presto            7 0
001900160613     c                   clear                   preok             7 0
002000160613     c                   clear                   assok             7 0
002100160613     c     karb          klist
002200160613     c                   kfld                    ar6aas
002300160613     c                   kfld                    ar6lnp
002400160613     c                   kfld                    ar6nrs
002500160613     c                   kfld                    ar6nsp
002600160610
002700160610     c     001           setll     fiar635l
002800160610     c                   read      fiar635l
002900160610    1c                   dow       not %eof(fiar635l)
003000160610     c* elaboro solo   fatture di maggio
003100160613    2c                   if        ar6dft>20160506
003200160613     c     karb          chain     fnblp01l
003300160613
003400160610
003500160610    3c                   if        savfiv<>ar6fiv
003600160613     c                   except    tot
003700160610     c                   clear                   buchi             7 0
003800160610     c                   clear                   savnft            6 0
003900160610     c                   eval      savfiv=ar6fiv
004000160610     c                   eval      savnft=ar6nft
004100160613     c                   clear                   assap             7 0
004200160613     c                   clear                   assann            7 0
004300160613     c                   clear                   asssto            7 0
004400160613     c                   clear                   presto            7 0
004500160613     c                   clear                   preok             7 0
004600160613     c                   clear                   assok             7 0
004700160610    3c                   endif
004800160613
004900160613     c                   select
005000160613    4c                   when      blpcbo='M '
005100160613     c                   add       1             presto            7 0
005200160613    4c                   when      blpcbo='P '  or blpcbo='Z '
005300160613     c                   add       1             asssto            7 0
005400160613    4c                   when      ar6trc='2' and  (arbcbo='I' or arbcbo='Y')
005500160613     c                   add       1             assAP             7 0
005600160613    4c                   when      ar6trc='2'
005700160613     c                   add       1             assok             7 0
005800160613    4c                   when      blpcbo='1 '  or blpcbo='4 '
005900160613     c                   add       1             preok
006000160613     c                   other
006100160613     c                   add       1             assok
006200160613    4c                   endsl
006300160610
006400160610     c     ar6nft        sub       savnft        resto             7 0
006500160610     c
006600160610    3c                   if        resto>1
006700160610     c                   eval      savnft=savnft+1
006800160610
006900160610    4c                   dow       ar6nft>savnft
007000160610     c                   exsr      SQLGEN
007100160610     c
007200160610    5c                   if        wmancafat<>'N'
007300160610     c                   except    det
007400160610    5c                   endif
007500160610
007600160610     c                   eval      savnft=savnft+1
007700160610    4c                   enddo
007800160613    3c                   endif
007900160610     c
008000160610     c                   eval      savnft=ar6nft
008100160610
008200160613    2c                   endif
008300160610     c
008400160610     c                   read      fiar635l
008500160610    1c                   enddo
008600160610     c
008700160613     c                   except    tot
008800160610     c                   seton                                        lr
008900160610      /free
009000160610       BEGSR  SQLGEN                 ;
009100160610       clear wmancafat   ;
009200160610        wrkgetlista ='select arbnft from FIARBT0F where arbnft='    +
009300160610                     %editc(savnft:'X') + ' and arbfiv=' + %editc(savfiv:'X') +
009400160610                     ' and  arbaas=2016' ;
009500160610
009600160610       EXEC sql  prepare s1 from : wrkgetlista ;
009700160610       EXEC sql  declare a1 cursor for s1      ;
009800160610       EXEC sql  open    a1                    ;
009900160610
010000160610       EXEC sql fetch next from a1 into :arbnft ;
010100160610
010200160610
010300160610       if  sqlcod=0    ;
010400160610       eval wmancafat='N'  ;
010500160613       assann=assann+1   ;
010600160610       endif   ;
010700160610
010800160610       EXEC sql  CLOSE   a1                    ;
010900160610
011000160610       if wmancafat<>'N'  ;
011100160610       setll (savfiv:savnft)   titas40c   ;
011200160610       reade (savfiv:savnft)   titas40c   ;
011300160610       dow not  %eof(TITAS40C)  and wmancafat<>'N'  ;
011400160610       if tasaas=2016  ;
011500160610       eval wmancafat='N'  ;
011600160613       assAP=assAP  +1   ;
011700160610       endif  ;
011800160610
011900160610       reade (savfiv:savnft)   titas40c   ;
012000160610       enddo  ;
012100160610       endif  ;
012200160610
012300160610       ENDSR    ;
012400160610      /end-free
012500160610
012600160613     OQSYSPRT   E            det        02
012700160610     O                                         +  0 'Fatture mancanti'
012800160610     O                       savfiv        z   +  2
012900160610     o                       savnft        z   +  2
013000160613     OQSYSPRT   E            tot        01
013100160613     o                                         +  0 'Filiale iva'
013200160613     O                       savfiv            +  1
013300160613     OQSYSPRT   E            tot        01
013400160613     O                                         +  0 'Totale PREPAGATI     '
013500160613     O                       preOK         z   +  1
013600160613     O                                         +  2 'Prepagati stornati'
013700160613     O                       presto        z   +  1
013800160613     OQSYSPRT   E            tot        01
013900160613     O                                         +  0 'Totale FATT.ASSEGNATO'
014000160613     O                       ASSOK         z   +  1
014100160613     O                                         +  2 'Assegnati stornati'
014200160613     O                       asssto        z   +  1
014300160613     O                                         +  2 'Assegnati AP'
014400160613     O                       assAP         z   +  1
014500160613     O                                         +  2 'Assegnati annullati'
014600160613     O                       assann        z   +  1
