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
001400070228
001500070228      *---------------------------------------------------------------*
001600070302      *?RIEPILOGO INDICATORI                                         ?*
001700070228      *---------------------------------------------------------------*
001800070228      * 10   - Comodo                                                 *
001900070228      *---------------------------------------------------------------*
002000070228      *
002100070228     c     *Entry        plist
002200081014     c                   parm                    trtc86drds
002300070228      *
002400081013     c/exec sql
002500081013     c+     SET  OPTION  DYNUSRPRF = *OWNER,
002600081013     c+                  CLOSQLCSR = *ENDMOD
002700081013     c/end-exec
002800081014
002900081020     c                   select
003000081020     c                   when      i86mod='UV'
003100081014     C/EXEC SQL
003200081014     C+ UPDATE FIRAB00F SET rabaas = :i86aasb,
003300081014     C+ RABLNP = :i86lnpb, RABNRS = :i86nrsb,
003400081014     c+ RABNSP = :i86nspb, RABCCM = :i86ccmb,
003500081014     c+ RABRMN = :i86rmnb, RABRMA = :i86rmab,
003600081014     c+ rabfop = 'S' concat substr(rabfop, 2)
003700081014     c+ where
003800091001     c+ rabfgs = :i86fgsv and rabaas = :i86aasv and
003900091001     C+ rablnp = :i86lnpv and RABNRS = :i86nrsv and
004000090917     c+ RABNSP = :i86nspv and RABCCM = :i86ccmv and
004100090917     c+ RABRMN = :i86rmnv and
004200081014     c+ RABRMA = :i86rmav and SUBSTR(rabfop, 1, 1)=' '
004300081014     C/END-EXEC
004400081020     c                   when      i86mod='DC'
004500121205     c     krab01        klist
004600121205     c                   kfld                    i86aasb
004700121205     c                   kfld                    i86lnpb
004800121205     c                   kfld                    i86nrsb
004900121205     c                   kfld                    i86nspb
005000121205     c
005100121205     c     krab01        setll     firab01l
005200121205     c     krab01        reade     firab01l
005300121205     c
005400121205     c                   dow       not %eof(firab01l)
005500121205     c                   delete    firab000
005600121205     c
005700121205     c     krab01        reade     firab01l
005800121205     c                   enddo
005900121205     c
006000121205     C* /EXEC SQL
006100121205     C* + delete from FIRAB00F where
006200121205     c* + rabaas = :i86aasb and RABlnp = :i86lnpb and
006300121205     c* + rabnrs = :i86nrsb and
006400121205     c* + RABNSP = :i86nspb and SUBSTR(rabfop, 1, 1)='S'
006500121205     C* /END-EXEC
006600070228      *
006700081020     c                   endsl
006800080930
006900070228     c                   eval      *inLR  = *on
007000070228      *
