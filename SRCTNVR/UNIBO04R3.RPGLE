000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090225     H* UNIBO04R *---------------------------------------------------*
000300090225     H*         - UNISCO FNBLT FNART IN FIART00F
000400000000     H*--------------------------------------------------------------*
000700090225     fFNBLt01L  if   e           k Disk
001400051012     F
001500051012     F* FILE NUOVO
001600140102     fFIART27L  uf a e           k Disk
001700051012     F
001800051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001900051012     D
002000090226     d Te              s             66    Dim(7) Ctdata Perrcd(1)
002100090225     d ERR             s             35    Dim(3) Ctdata Perrcd(1)
002200051019     d OVRP            s             78    Dim(1) Ctdata Perrcd(1)
002300060215     d
002400051012     D
002500051012     d STAFILE         s              5
002701140102     d kaas            s              4  0
002800120618     d dar5gen       e ds
002900120618     d og143         e ds
003200051012     C
003701140102     c     KNSC          Klist
003702140102     c                   Kfld                    bltfls
003703140102     c                   Kfld                    bltlna
003704140102     c                   Kfld                    bltnrs
003705140102     c                   Kfld                    bltnsc
007900051019     c
008000051019     c                   open      qsysprt
008100051019     c
008300051012     c                   except    TESTA
008400980130     C***
008500090226     C* FNBLt00F
008600980130     C***
008601140102     c                   eval      kaas=2014
008700140102     C     kaas          SETLL     FNBLt01L
008800140102     C     kaas          READe     FNBLt01L
008900051012     C
009000090225    1C                   DOW       NOT %EOF(FNBLt01L)
009100051012     C                   SETOFF                                       0102
009300051012     c
009401140102     c     knsc          chain     fiart27l
009402140102     c                   if        %found(fiart27l)
009403140102     c                   except    errore
009404140102     c                   eval      artaas=bltaas
009405140102     c                   eval      artnsp=bltnsp
009406140102     c                   update    fiart000
009407140102     c                   endif
022500051012     C
022600140102     C     kaas          READe     FNBLt01L
022700051012    1c                   enddo
022800051012     C
031600120618     c
031700020502     C                   SETON                                        LR
031800051012     c*------------------------------------------------------------------
044400051012
044500051012      *  T E S T A T A
044600051012     oQsysprt   e            Testa            02
044700051012     o                       Te(1)               66
044800051012     o                       Te(2)              132
044900051114     o                       *Date              120 '  /  /    '
045000051012     o                       Page          Z    132
045100051012     O
045200140102     oqsysprt   e            ERRORE      1
045500090225     o                       BLtLNp           +   1
045600090225     o                       BLtNRS           +   1
045800090225     o                       BLtnsc           +   1
045900090225     o                       BLtlna           +   1
045901140102     o                       artaas           +   1
045902140102     o                       artNSp        2  +   1
045903140102     o                       bltaas           +   1
045904140102     o                       bltNSp        2  +   1
045905140102     o
046200090225     o
048900051012**         TE
049000140102                            **  aggiorna FIART00F  **                1
049100140102                            UNIBO04R                     PAG. XXXX   2
049200090225FNBLT00F : già in Fiart00f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
049300090226x          Aggiornati da BLTxxx.xxx.xxx      con FNART xxx.xxx.xxx   4
049400090226FNART00F :                              Non presente FNARBx.xxx.xx   5
049500090226x                                            Aggiornatixxx.xxx.xxx   6
049600090226TOTALI GENERALI:                             AGGIORNATI XXX.XXX.XXX  7
049700051012**    ERR
049800051114MANCA bolla in FNBLP01L                                              1
049900051114MANCA bolla in FNARB01L
050000090225Già presente in FIART
050100051019**
050200090226OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIART00F')
