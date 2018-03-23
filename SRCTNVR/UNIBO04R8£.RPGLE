000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300120618     fFNart01L  if   e           k Disk
000400090225     f                                     prefix(old:3)
000500090225     fFNBLt01L  if   e           k Disk
000600051012     F
000700051012     F* FILE NUOVO
000800161113     fFIART01L  uf   e           k Disk    usropn
000900051012     F
001000051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001100051012     D
001200161111     d OVRP            s             80    Dim(1) Ctdata Perrcd(1)
001300060215     d
001400051012     D
001500161111     d STAFILE         s              5
001600120618     d og143         e ds
001700051012     C
001800161113     c     Kcollo        Klist
001900161113     c                   Kfld                    artaas
002000161113     c                   Kfld                    artlnp
002100161113     c                   Kfld                    artnrs
002200161113     c                   Kfld                    artnsp
002300161113     c                   Kfld                    artnsc
002400060217      *
002500090225     c                   open      fiart01l
002600051019     c* Eseguo ovrprtf su qsysprt
002700051019     C                   MOVEA     ovrp          COMMAN           80
002800051019     C                   CALL      'QCMDEXC'                            47
002900051019     C                   PARM                    COMMAN
003000051019     C                   PARM      80            LUNG             15 5
003100051019     c
003200051019     c                   open      qsysprt
003300980130     C***
003400161113     C     *LOVAL        SETLL     Fiart01L
003500161113     C                   READ      Fiart01L
003600051012     C
003700161113    1C                   DOW       NOT %EOF(Fiart01L)
003800161104     c*
003900161113     c     kcollo        chain     fnblt01l
004000161113   1ac                   if        not %found(fnblt01l)
004100161113     c     kcollo        chain     fnart01l
004200161113   1ac                   if        not %found(fnart01l)
004300161113     c                   add       1             delart            9 0
004400161113     c                   delete    fiart000
004500161113     c                   endif
004600161113     c                   endif
004700051012     C
004800161113     C                   READ      Fiart01L
004900051012    1c                   enddo
005000051012     C
005100051012     c                   except    totali
005200120618     c
005300020502     C                   SETON                                        LR
005400051012      *  T E S T A T A
005500161113     oqsysprt   e            TOTALI      2
005600161113     o                                         +  0 'da cancellare FIART'
005700161113     o                       delart        2   +  1
005800051019**
005900161111OVRPRTF FILE(QSYSPRT) OUTQ(P046197EDP) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
