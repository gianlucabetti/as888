000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200050107      * TIBS85R  *---------------------------------------------------*
000300050107      *  - MODIFICA PUNTO OPERATIVO: sistemo menù su profili
000400940915      *--------------------------------------------------------------*
000500050107     Fazute02l  iF   E           K DISK
000600050107     Fkpprf01l  UF   E           K DISK
000700050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000800960302     D*
000900050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001000040917     C**********************************************************************
001100040917     C     *ENTRY        PLIST
001200050103     C                   PARM                    parpo             3 0
001300050103     C                   PARM                    partpo            3 0
001400050103     C                   PARM                    partpn            3 0
001500050103     c
001600050103     c                   eval      comman=ovrp(1)
001700050103     C                   CALL      'QCMDEXC'
001800050103     C                   PARM                    COMMAN           80
001900050103     C                   PARM      80            LUNG             15 5
002000050103     c
002100050103     c                   open      qsysprt
002200050103     c
002300050103     c
002400050215     c* Leggo tutti i record di fisgn solo per il 1 livello (terminal di
002500050215     c*  partenza che diventa 2 livello)
002600050215     c                   if        partpo=parpo
002700050107     c     partpo        setll     azute02l
002800050107     c     partpo        reade     azute02l
002900050107    1c                   dow       not %eof(azute02l)
003000050107
003100050107     c* Aggancio kpprf per aggiornare menù iniziale
003200050107     c                   movel     uteute        winizioute        3
003300050107     c                   if        winizioute<>'EDP'
003400050107     c     uteute        chain     kpprf01l
003500050110    2c                   if        %found(kpprf01l)
003600050110
003700050110    2c                   if        prfnsi=('FILTRA201')
003800050107     c                   except    aggioprf
003900050107     c                   eval      prfnmi='FS88'
004000050107     c                   update    kpprf000
004100050107     c                   add       1             contaprf          7 0
004200050110     c                   endif
004300050110     c
004400050107   x2c                   else
004500050107     c                   except    notrovaprf
004600050107     c                   add       1             contanotrova      7 0
004700050103    2c                   endif
004800050107    2c                   endif
004900050104    c
005000050107     c     partpo        reade     azute02l
005100050107    1C                   ENDDO
005200050103     c
005300050103     c* Stampo totale bolle aggiornate
005400050103     c                   except    totale
005500050215     c                   endif
005600050103     c
005700050103     c                   seton                                        lr
005800050107     oQsysprt   e            aggioprf    2
005900050107     o                                         +  0 'Profilo da aggiornare con '
006000050107     o                                         +  0 'menù "FS88" :'
006100050107     o                       uteute            +  2
006200050107     o                                         +  2 'menù'
006300050107     o                       prfnmi            +  2
006400050107
006500050107     oQsysprt   e            notrovaprf  2
006600050107     o                                         +  0 'Profilo non trovato su KPP'
006700050107     o                                         +  0 'RF01L :'
006800050107     o                       uteute            +  2
006900050107     oQsysprt   e            totale      2
007000050107     o                                         +  0 'Totale KPPRF01L aggiornati'
007100050107     o                       CONTAprf      2   +  2
007200050107     o                                         +  4 'Totale KPPRF01L non trovat'
007300050107     o                                         +  0 'i'
007400050107     o                       CONTAnotrova  2   +  2
007500050103** ovrp
007600050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
