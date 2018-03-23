000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200050107      * TIBS85R  *---------------------------------------------------*
000300050107      *  - MODIFICA PUNTO OPERATIVO: sistemo menù su profili
000400940915      *--------------------------------------------------------------*
000500050107     Fazute02l  iF   E           K DISK
000600050107     Fkpprf01l  UF   E           K DISK
000700060102     Fazorg01l  iF   E           K DISK
000800050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000900960302     D*
001000050103     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001100040917     C**********************************************************************
001200040917     C     *ENTRY        PLIST
001300050103     C                   PARM                    parpo             3 0
001400050103     C                   PARM                    partpo            3 0
001500050103     C                   PARM                    partpn            3 0
001600050103     c
001700050103     c                   eval      comman=ovrp(1)
001800050103     C                   CALL      'QCMDEXC'
001900050103     C                   PARM                    COMMAN           80
002000050103     C                   PARM      80            LUNG             15 5
002100050103     c
002200050103     c                   open      qsysprt
002300060102     c
002400060102     c     parpo         chain     azorg01l
002500060102     c
002600060102     c                   if        %found(azorg01l)
002700060102     c                   movel     orgdes        despo            25
002800060102     c                   else
002900060102     c                   clear                   despo
003000060102     c                   endif
003100060102     c
003200050103     c
003300050215     c* Leggo tutti i record di fisgn solo per il 1 livello (terminal di
003400050215     c*  partenza che diventa 2 livello)
003500050215     c                   if        partpo=parpo
003600050107     c     partpo        setll     azute02l
003700050107     c     partpo        reade     azute02l
003800050107    1c                   dow       not %eof(azute02l)
003900050107
004000050107     c* Aggancio kpprf per aggiornare menù iniziale
004100050107     c                   movel     uteute        winizioute        3
004200050107     c                   if        winizioute<>'EDP'
004300050107     c     uteute        chain     kpprf01l
004400050110    2c                   if        %found(kpprf01l)
004500050110
004600050110    2c                   if        prfnsi=('FILTRA201')
004700050107     c                   except    aggioprf
004800050107     c                   eval      prfnmi='FS88'
004900050107     c                   update    kpprf000
005000050107     c                   add       1             contaprf          7 0
005100050110     c                   endif
005200050110     c
005300050107   x2c                   else
005400050107     c                   except    notrovaprf
005500050107     c                   add       1             contanotrova      7 0
005600050103    2c                   endif
005700050107    2c                   endif
005800050104    c
005900050107     c     partpo        reade     azute02l
006000050107    1C                   ENDDO
006100050103     c
006200050103     c* Stampo totale bolle aggiornate
006300050103     c                   except    totale
006400050215     c                   endif
006500050103     c
006600050103     c                   seton                                        lr
006700050107     oQsysprt   e            aggioprf    2
006800050107     o                                         +  0 'Profilo da aggiornare con '
006900050107     o                                         +  0 'menù "FS88" :'
007000050107     o                       uteute            +  2
007100050107     o                                         +  2 'menù'
007200050107     o                       prfnmi            +  2
007300050107
007400050107     oQsysprt   e            notrovaprf  2
007500050107     o                                         +  0 'Profilo non trovato su KPP'
007600050107     o                                         +  0 'RF01L :'
007700050107     o                       uteute            +  2
007800060102     oQsysprt   e            totale      2
007900060102     o                                         +  0 'Punto Operativo Modificato'
008000060102     o                       parpo             +  2
008100060102     o                       despo             +  2
008200060102     o                       *date             + 25 '  /  /    '
008300050107     oQsysprt   e            totale      2
008400050107     o                                         +  0 'Totale KPPRF01L aggiornati'
008500050107     o                       CONTAprf      2   +  2
008600050107     o                                         +  4 'Totale KPPRF01L non trovat'
008700050107     o                                         +  0 'i'
008800050107     o                       CONTAnotrova  2   +  2
008900050103** ovrp
009000050103OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('ModificaPO')
