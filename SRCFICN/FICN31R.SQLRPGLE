000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     d kpjba         e ds
000600170421     D*
000700020624     D param           DS
000800020624     D  datadal                       8s 0
000900020624     D  dataal                        8s 0
001000020624     D*
001100170323     d dataiso         s               d   datfmt(*iso)
001200170323     d dataiso_A       s               d   datfmt(*iso)
001300170323     D  giorno         s              8s 0
001400170323     D KPJBUs          s                   like(KPJBU)
001500170323     d
001600011026
001700000000     C*---------------------------------------------------------------*
001800020624     c     *entry        plist
001900020624     c                   parm                    kpjba
002000020624     c                   movel     kpjbu         param
002100020624
002200060221      * ?   Tutto il saldificato entro i giorni
002300020313     C/EXEC SQL
002400020624     C+ delete from FIFss00F where fssddc between :datadal and :dataal
002500020313     C/END-EXEC
002600020515      *
002700060221      * ?   Per i costi delle distinte di padroncini NON FITTIZI
002800020624     C/EXEC SQL
002900020624     C+ update FIFce00F set fcesal = ' '
003000020624     c+ where fceddc between :datadal and :dataal
003100060123     c+ and
003200060123     c+ digits(FCEPDR)Concat(digits(FCEDDC)) in
003300060123     c+ ( Select digits(FTTPDR)Concat(digits(FTTDDC))
003400060123     c+ from FIFTT00F where
003500060123     c+ FTTDDC between :datadal and :dataal and
003600060123     c+ FTTTSR=' ' and FTTFTR='R' and FTTFVL='C' )
003700020624     C/END-EXEC
003800020624      *
003900060221      * ?   Per i costi dei SOLI padroncini FITTIZI
004000060221     C/EXEC SQL
004100060221     C+ update FIFce00F set fcesal = ' '
004200060221     c+ where fceddc between :datadal and :dataal
004300060221     c+ and FCEPDR in
004400060221     c+ ( Select APDPDR from FIAPD00F where
004500060221     c+ APDPDD = 'S' )
004600060221     C/END-EXEC
004700161114      *--------------------------------------------------------------*
004800161114      * ?   Adesso chiama il pgm per sistemare FTT e FCE
004900161114      *--------------------------------------------------------------*
005000161114      * Questo programma serve per poter allineare FIFCE scritto in
005100161114      * filiale in modo errato ove non è possibile spalmare corretta-
005200161114      * mente le tariffe sulle voci di C/E i costi dovuti.
005300161114      * In tal modo si ottiene un allineamento fra FIFTT e FIFCE x non
005400161114      * creare scompensi in seguito sui saldi fra COSTI e COMPETENZE.
005500161114      * I casi da rettificare sono sporadici e non rappresentano la
005600161114      * normalità della procedura di conferma valorizzazioni Padroncini
005700161114      * comunque questo pgm serve per coprire le eccezioni non gestibili.
005800161114      *--------------------------------------------------------------*
005900161114      **
006000161114     c                   z-add     datadal       dal               8 0
006100161114     c                   z-add     dataal        al                8 0
006200161114     c                   move      'R'           trasmessi         1
006300060221      *
006400161114      ** pgm per sistemare le differenze fra FCE e FTT (di FTT trasemssi e NON)
006500161114     c                   call      'FICN43R2'
006600161114     c                   parm                    trasmessi         1
006700161114     c                   parm                    dal               8 0
006800161114     c                   parm                    al                8 0
006900170323      *
007000170323      *--------------------------------------------------------------*
007100170323      *  per i dati della B.I. ricalcoliamo gli stessi giorni
007200170323      *--------------------------------------------------------------*
007300170323      *    salva KPJBU
007400170323     c                   eval      kpjbus = kpjbu
007500170323      *  rigenera i dati della B.I. per i giorni che aveva ricalcolato
007600170421     c                   movel(p)  param         kpjbu
007700170323     c                   call      'TRBI03C'
007800170323     c                   parm                    kpjba
007900170323      *  reimposta KPJBU
008000170323     c                   eval      kpjbu = kpjbus
008100170323      *
008200020624     c                   seton                                        lr
008300011129     C*--------------------------------------------------------------------
