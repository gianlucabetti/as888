000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     d kpjba         e ds
000600020624     D param           DS
000700020624     D  datadal                       8s 0
000800020624     D  dataal                        8s 0
000900020624     D*
001000011026
001100000000     C*---------------------------------------------------------------*
001200020624     c     *entry        plist
001300020624     c                   parm                    kpjba
001400020624     c                   movel     kpjbu         param
001500020624
001501060221      * ?   Tutto il saldificato entro i giorni
001600020313     C/EXEC SQL
001700020624     C+ delete from FIFss00F where fssddc between :datadal and :dataal
001800020313     C/END-EXEC
001900020515      *
001902060221      * ?   Per i costi delle distinte di padroncini NON FITTIZI
002000020624     C/EXEC SQL
002100020624     C+ update FIFce00F set fcesal = ' '
002200020624     c+ where fceddc between :datadal and :dataal
002201060123     c+ and
002202060123     c+ digits(FCEPDR)Concat(digits(FCEDDC)) in
002203060123     c+ ( Select digits(FTTPDR)Concat(digits(FTTDDC))
002204060123     c+ from FIFTT00F where
002205060123     c+ FTTDDC between :datadal and :dataal and
002206060123     c+ FTTTSR=' ' and FTTFTR='R' and FTTFVL='C' )
002300020624     C/END-EXEC
002400020624      *
002402060221      * ?   Per i costi dei SOLI padroncini FITTIZI
002403060221     C/EXEC SQL
002404060221     C+ update FIFce00F set fcesal = ' '
002405060221     c+ where fceddc between :datadal and :dataal
002406060221     c+ and FCEPDR in
002407060221     c+ ( Select APDPDR from FIAPD00F where
002409060221     c+ APDPDD = 'S' )
002411060221     C/END-EXEC
002412060221      *
002500050207     c                   call      'FICN31R1'
002600050207     c                   parm                    kpjba
002700050207      *
002800020624     c                   seton                                        lr
002900011129     C*--------------------------------------------------------------------
