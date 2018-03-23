000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200161104     H* UNIBO04R4 *---------------------------------------------------*
000300161104     H*         - UNISCO FNBLT FNART IN FIART00F senza letture delle spunte
000400000000     H*--------------------------------------------------------------*
000500120618     fFNart01L  if   e           k Disk
000600090225     f                                     prefix(old:3)
000700090225     fFNBLt01L  if   e           k Disk
000800051012     F
000900051012     F* FILE NUOVO
001000161113     fFIART01L  if a e           k Disk    usropn
001100051012     F
001200051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001300051012     D
001400161111     d OVRP            s             80    Dim(1) Ctdata Perrcd(1)
001500060215     d
001600051012     D
001700161111     d STAFILE         s              5
001800120618     d og143         e ds
001900051012     C
002000161113     c     Kcollo        Klist
002100161113     c                   Kfld                    artaas
002200161113     c                   Kfld                    artlnp
002300161113     c                   Kfld                    artnrs
002400161113     c                   Kfld                    artnsp
002500161113     c                   Kfld                    artnsc
002600161113     c     Kcolloa       Klist
002700161113     c                   Kfld                    oldaas
002800161113     c                   Kfld                    oldlnp
002900161113     c                   Kfld                    oldnrs
003000161113     c                   Kfld                    oldnsp
003100161113     c                   Kfld                    oldnsc
003200161113     c     Kcollop       Klist
003300161113     c                   Kfld                    bltaas
003400161113     c                   Kfld                    bltlnp
003500161113     c                   Kfld                    bltnrs
003600161113     c                   Kfld                    bltnsp
003700161113     c                   Kfld                    bltnsc
003800060217      *
003900090225     c                   open      fiart01l
004000051019     c* Eseguo ovrprtf su qsysprt
004100051019     C                   MOVEA     ovrp          COMMAN           80
004200051019     C                   CALL      'QCMDEXC'                            47
004300051019     C                   PARM                    COMMAN
004400051019     C                   PARM      80            LUNG             15 5
004500051019     c
004600051019     c                   open      qsysprt
004700980130     C***
004800161113     C     *LOVAL        SETLL     Fiart01L
004900161113     C                   READ      Fiart01L
005000051012     C
005100161113    1C                   DOW       NOT %EOF(Fiart01L)
005200161104     c*
005300161113     c     kcollo        chain     fnblt01l
005400161113   1ac                   if        not %found(fnblt01l)
005500161113     c     kcollo        chain     fnart01l
005600161113   1ac                   if        not %found(fnart01l)
005700161113     c                   add       1             delart            9 0
005800161113     c                   endif
005900161113     c                   endif
006000051012     C
006100161113     C                   READ      Fiart01L
006200051012    1c                   enddo
006300051012     C
006400051012     C***
006500090225     C     *LOVAL        SETLL     FNart01L
006600090225     C                   READ      FNart01L
006700051012     C
006800090225    1C                   DOW       NOT %EOF(FNart01L)
006900051012     C                   SETOFF                                       0302
007000090225     c     kcolloa       chain     fiart01l
007100060209     c
007200090226   1ac                   if        not %found(fiart01l)
007300161113     c                   add       1             mancaart          9 0
007400161113     c                   endif
007500090226     C                   READ      FNart01L
007600051012    1c                   enddo
007700161113
007800161113     C     *LOVAL        SETLL     FNblt01L
007900161113     C                   READ      FNblt01L
008000161113     C
008100161113    1C                   DOW       NOT %EOF(FNblt01L)
008200161113     C                   SETOFF                                       0302
008300161113     c     kcollop       chain     fiart01l
008400161113     c
008500161113   1ac                   if        not %found(fiart01l)
008600161113     c                   add       1             mancablt          9 0
008700161113     c                   endif
008800161113     C                   READ      FNblt01L
008900161113    1c                   enddo
009000051012     c***
009100051012     C
009200051012     c                   except    totali
009300120618     c
009400020502     C                   SETON                                        LR
009500051012      *  T E S T A T A
009600161113     oqsysprt   e            TOTALI      2
009700161113     o                                         +  0 'da cancellare FIART'
009800161113     o                       delart        2   +  1
009900161113     o                                         +  1 'mancante fa fnart  '
010000161113     o                       mancaart      2   +  1
010100161113     o                                         +  1 'mancante fa fnblt  '
010200161113     o                       mancablt      2    105
010300051012     O
010400051019**
010500161111OVRPRTF FILE(QSYSPRT) OUTQ(P046197EDP) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
