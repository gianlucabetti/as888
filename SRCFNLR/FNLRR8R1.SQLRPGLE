000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000401020624      *
000402020624     d kpjba         e ds
000403020624     D param           DS
000404020624     D  datadal                       8s 0
000405020624     D  dataal                        8s 0
000406041119     D  filges                        3s 0
000407020624     D*
001000011026
002900000000     C*---------------------------------------------------------------*
039601020624     c     *entry        plist
039602020624     c                   parm                    kpjba
039603020624     c                   movel     kpjbu         param
039604041119      * tutte le filiali
039605041119     c                   if        filges = 0
039606020624
041600020313     C/EXEC SQL
041700041119     C+ delete from FIqdc00F where qdcddc between :datadal and :dataal
041800020313     C/END-EXEC
041801020515      *
041802020624     C/EXEC SQL
041803041119     C+ update FIqdt00F set qdtdaeco = 0, qdtoraco = 0, qdtnblaf = 0,
041804041119     c+ qdtnclaf = 0, qdtpkfaf = 0, qdtvlfaf = 0, qdtcasaf = 0, qdtcaeaf = 0,
041805041119     c+ qdtiftaf = 0, qdtnblnc = 0, qdtnclnc = 0, qdtnclns = 0, qdtpkfnc = 0,
041806041119     c+ qdtvlfnc = 0, qdtcasnc = 0, qdtcaenc = 0, qdtiftnc = 0, qdtcasnf = 0,
041807041119     c+ qdtcaenf = 0, qdtiftnf = 0
041808041119     c+ where qdtddc between :datadal and :dataal
041809020624     C/END-EXEC
041810041119
041811041119     c                   ELSE
041812041119
041813041119     C/EXEC SQL
041814041119     C+ delete from FIqdc00F where qdcddc between :datadal and :dataal and
041815041119     c+ qdcfgs = :filges
041816041119     C/END-EXEC
041817041119      *
041818041119     C/EXEC SQL
041819041119     C+ update FIqdt00F set qdtdaeco = 0, qdtoraco = 0, qdtnblaf = 0,
041820041119     c+ qdtnclaf = 0, qdtpkfaf = 0, qdtvlfaf = 0, qdtcasaf = 0, qdtcaeaf = 0,
041821041119     c+ qdtiftaf = 0, qdtnblnc = 0, qdtnclnc = 0, qdtnclns = 0, qdtpkfnc = 0,
041822041119     c+ qdtvlfnc = 0, qdtcasnc = 0, qdtcaenc = 0, qdtiftnc = 0, qdtcasnf = 0,
041823041119     c+ qdtcaenf = 0, qdtiftnf = 0
041824041119     c+ where qdtddc between :datadal and :dataal and qdtfgs = :filges
041825041119     C/END-EXEC
041826041119
041827041119     c                   endif
041828020624      *
041901020624     c                   seton                                        lr
042000011129     C*--------------------------------------------------------------------
