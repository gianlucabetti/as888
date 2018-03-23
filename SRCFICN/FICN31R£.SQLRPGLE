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
001600060221      * ?   Tutto il saldificato entro i giorni
001700020313     C/EXEC SQL
001800020624     C+ delete from FIFss00F where fssddc between :datadal and :dataal
001900020313     C/END-EXEC
002000020515      *
002100060221      * ?   Per i costi delle distinte di padroncini NON FITTIZI
002200020624     C/EXEC SQL
002300020624     C+ update FIFce00F set fcesal = ' '
002400020624     c+ where fceddc between :datadal and :dataal
002500060123     c+ and
002600060123     c+ digits(FCEPDR)Concat(digits(FCEDDC)) in
002700060123     c+ ( Select digits(FTTPDR)Concat(digits(FTTDDC))
002800060123     c+ from FIFTT00F where
002900060123     c+ FTTDDC between :datadal and :dataal and
003000060123     c+ FTTTSR=' ' and FTTFTR='R' and FTTFVL='C' )
003100020624     C/END-EXEC
003200020624      *
003300060221      * ?   Per i costi dei SOLI padroncini FITTIZI
003400060221     C/EXEC SQL
003500060221     C+ update FIFce00F set fcesal = ' '
003600060221     c+ where fceddc between :datadal and :dataal
003700060221     c+ and FCEPDR in
003800060221     c+ ( Select APDPDR from FIAPD00F where
003900060221     c+ APDPDD = 'S' )
004000060221     C/END-EXEC
004100161114      *--------------------------------------------------------------*
004200161114      * ?   Adesso chiama il pgm per sistemare FTT e FCE
004300161114      *--------------------------------------------------------------*
004400161114      * Questo programma serve per poter allineare FIFCE scritto in
004500161114      * filiale in modo errato ove non è possibile spalmare corretta-
004600161114      * mente le tariffe sulle voci di C/E i costi dovuti.
004700161114      * In tal modo si ottiene un allineamento fra FIFTT e FIFCE x non
004800161114      * creare scompensi in seguito sui saldi fra COSTI e COMPETENZE.
004900161114      * I casi da rettificare sono sporadici e non rappresentano la
005000161114      * normalità della procedura di conferma valorizzazioni Padroncini
005100161114      * comunque questo pgm serve per coprire le eccezioni non gestibili.
005200161114      *--------------------------------------------------------------*
005300161114      **
005400161114     c                   z-add     datadal       dal               8 0
005500161114     c                   z-add     dataal        al                8 0
005600161114     c                   move      'R'           trasmessi         1
005700060221      *
005800161114      ** pgm per sistemare le differenze fra FCE e FTT (di FTT trasemssi e NON)
005900161114     c                   call      'FICN43R2'
006000161114     c                   parm                    trasmessi         1
006100161114     c                   parm                    dal               8 0
006200161114     c                   parm                    al                8 0
006300050207      *
006400020624     c                   seton                                        lr
006500011129     C*--------------------------------------------------------------------
