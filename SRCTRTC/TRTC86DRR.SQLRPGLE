000100081013      * Trtc86drr ----------------------------------------------------*
000200081013      *?Aggiornamento key spedizione su firab                        ?*
000300070228      *---------------------------------------------------------------*
000400090923     H DFTACTGRP(*NO) ACTGRP('QILE')
000500070228     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070228
000700121205     ffirab01l  Uf   e           k disk
000800121205
000900070228      *---------------------------------------------------------------*
001000070302      *?D S - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
001100070228      * - Parametri
001200081013     d Trtc86drds    e ds
001300070228      *
001400130125     d wvabaas         s              4  0
001500130125     d wvabaas_1       s              4  0
001600070228
001700070228      *---------------------------------------------------------------*
001800070302      *?RIEPILOGO INDICATORI                                         ?*
001900070228      *---------------------------------------------------------------*
002000070228      * 10   - Comodo                                                 *
002100070228      *---------------------------------------------------------------*
002200070228      *
002300070228     c     *Entry        plist
002400081014     c                   parm                    trtc86drds
002500130125     c
002600130125     c* Per modo UV non tengo conto dell'anno del FIVAB ma visto che ci sono clienti che
002700130125     c*  passano record con anno xxxx e anno xxx+1, uso l'anno della bolla confermata e poi faccio
002800130125     c*  due tentativi: uno con anno bolla e uno con anno bolla - 1
002900130125     c                   if        i86mod='UV'
003000130125     c                   eval      wvabaas=i86aasb
003100130125     c                   eval      wvabaas_1=wvabaas-1
003200130125     c                   endif
003300130125     c
003400070228      *
003500081013     c/exec sql
003600081013     c+     SET  OPTION  DYNUSRPRF = *OWNER,
003700081013     c+                  CLOSQLCSR = *ENDMOD
003800081013     c/end-exec
003900081014
004000081020     c                   select
004100081020     c                   when      i86mod='UV'
004200081014     C/EXEC SQL
004300081014     C+ UPDATE FIRAB00F SET rabaas = :i86aasb,
004400081014     C+ RABLNP = :i86lnpb, RABNRS = :i86nrsb,
004500081014     c+ RABNSP = :i86nspb, RABCCM = :i86ccmb,
004600081014     c+ RABRMN = :i86rmnb, RABRMA = :i86rmab,
004700081014     c+ rabfop = 'S' concat substr(rabfop, 2)
004800081014     c+ where
004900130125     c+ rabfgs = :i86fgsv and rabaas = :wvabaas and
005000091001     C+ rablnp = :i86lnpv and RABNRS = :i86nrsv and
005100090917     c+ RABNSP = :i86nspv and RABCCM = :i86ccmv and
005200090917     c+ RABRMN = :i86rmnv and
005300081014     c+ RABRMA = :i86rmav and SUBSTR(rabfop, 1, 1)=' '
005400081014     C/END-EXEC
005500130125     c
005600130125     C/EXEC SQL
005700130125     C+ UPDATE FIRAB00F SET rabaas = :i86aasb,
005800130125     C+ RABLNP = :i86lnpb, RABNRS = :i86nrsb,
005900130125     c+ RABNSP = :i86nspb, RABCCM = :i86ccmb,
006000130125     c+ RABRMN = :i86rmnb, RABRMA = :i86rmab,
006100130125     c+ rabfop = 'S' concat substr(rabfop, 2)
006200130125     c+ where
006300130125     c+ rabfgs = :i86fgsv and rabaas = :wvabaas_1 and
006400130125     C+ rablnp = :i86lnpv and RABNRS = :i86nrsv and
006500130125     c+ RABNSP = :i86nspv and RABCCM = :i86ccmv and
006600130125     c+ RABRMN = :i86rmnv and
006700130125     c+ RABRMA = :i86rmav and SUBSTR(rabfop, 1, 1)=' '
006800130125     C/END-EXEC
006900081020     c                   when      i86mod='DC'
007000121205     c     krab01        klist
007100121205     c                   kfld                    i86aasb
007200121205     c                   kfld                    i86lnpb
007300121205     c                   kfld                    i86nrsb
007400121205     c                   kfld                    i86nspb
007500121205     c
007600121205     c     krab01        setll     firab01l
007700121205     c     krab01        reade     firab01l
007800121205     c
007900121205     c                   dow       not %eof(firab01l)
008000121205     c                   delete    firab000
008100121205     c
008200121205     c     krab01        reade     firab01l
008300121205     c                   enddo
008400121205     c
008500121205     C* /EXEC SQL
008600121205     C* + delete from FIRAB00F where
008700121205     c* + rabaas = :i86aasb and RABlnp = :i86lnpb and
008800121205     c* + rabnrs = :i86nrsb and
008900121205     c* + RABNSP = :i86nspb and SUBSTR(rabfop, 1, 1)='S'
009000121205     C* /END-EXEC
009100070228      *
009200081020     c                   endsl
009300080930
009400070228     c                   eval      *inLR  = *on
009500070228      *
