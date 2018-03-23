000100081013      * Trtc86drr ----------------------------------------------------*
000200081013      *?Aggiornamento key spedizione su firab                        ?*
000300070228      *---------------------------------------------------------------*
000400090923     H DFTACTGRP(*NO) ACTGRP('QILE')
000500070228     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070228
000700070228      *---------------------------------------------------------------*
000800070302      *?D S - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
000900070228      * - Parametri
001000081013     d Trtc86drds    e ds
001100070228      *
001200070228
001300070228      *---------------------------------------------------------------*
001400070302      *?RIEPILOGO INDICATORI                                         ?*
001500070228      *---------------------------------------------------------------*
001600070228      * 10   - Comodo                                                 *
001700070228      *---------------------------------------------------------------*
001800070228      *
001900070228     c     *Entry        plist
002000081014     c                   parm                    trtc86drds
002100070228      *
002200081013     c/exec sql
002300081013     c+     SET  OPTION  DYNUSRPRF = *OWNER,
002400081013     c+                  CLOSQLCSR = *ENDMOD
002500081013     c/end-exec
002600081014
002700081020     c                   select
002800081020     c                   when      i86mod='UV'
002900081014     C/EXEC SQL
003000081014     C+ UPDATE FIRAB00F SET rabaas = :i86aasb,
003100081014     C+ RABLNP = :i86lnpb, RABNRS = :i86nrsb,
003200081014     c+ RABNSP = :i86nspb, RABCCM = :i86ccmb,
003300081014     c+ RABRMN = :i86rmnb, RABRMA = :i86rmab,
003400081014     c+ rabfop = 'S' concat substr(rabfop, 2)
003500081014     c+ where
003600091001     c+ rabfgs = :i86fgsv and rabaas = :i86aasv and
003700091001     C+ rablnp = :i86lnpv and RABNRS = :i86nrsv and
003800090917     c+ RABNSP = :i86nspv and RABCCM = :i86ccmv and
003900090917     c+ RABRMN = :i86rmnv and
004000081014     c+ RABRMA = :i86rmav and SUBSTR(rabfop, 1, 1)=' '
004100081014     C/END-EXEC
004200081020     c                   when      i86mod='DC'
004300081020     C/EXEC SQL
004400081020     C+ delete from FIRAB00F where
004500090917     c+ rabaas = :i86aasb and RABlnp = :i86lnpb and
004600090917     c+ rabnrs = :i86nrsb and
004700081020     c+ RABNSP = :i86nspb and SUBSTR(rabfop, 1, 1)='S'
004800081020     C/END-EXEC
004900070228      *
005000081020     c                   endsl
005100080930
005200070228     c                   eval      *inLR  = *on
005300070228      *
