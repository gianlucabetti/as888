000100021017     H DECEDIT('0,') DATEDIT(*DMY.)
000200021017
000300021017      *--------------------------------------------------------------*
000400041130      * Accoda numeratori di aznum da gaitra a filtra e poi sposta in
000500041130      * UNI
000600041130      *--------------------------------------------------------------*
000700021017
000800041130     fAZNUM00f  if   e             Disk    usropn                               Numeratore vecchio
000900041130     F                                     RENAME(aznum000:aznum0ff)
001000041130     fAZNUM01l  uf a e           k Disk    usropn                               Numeratore vecchio
001100041130     D DSBS50        E DS                  EXTNAME(TIBS55DS)
001200041130     drecordnum      E DS                  extname(aznum00f)
001300041130     dsavrecord        s                   like(recordnum)
001400041130     D OVRnum0f        S              1    DIM(39) CTDATA PERRCD(39)
001500041130     D OVRnum1l        S              1    DIM(39) CTDATA PERRCD(39)
001600041130     D Cnum0f          C                   CONST('/AZNUM00F)')
001700041130     D Cnum1l          C                   CONST('/AZNUM01L)')
001800021017
001900021017      *Klist
002000041130     c     Knum          Klist
002100041130     c                   Kfld                    Numsoc
002200041130     c                   Kfld                    Numaaa
002300041130     c                   Kfld                    Numcnu
002400041130     c                   Kfld                    Numpo1
002500041130     c                   Kfld                    Numpo2
002600041130     C*
002700041130     C                   read      AZNUM00f
002800041130     c                   dow       not %eof(aznum00f)
002900041130     c
003000041130     c                   eval      savrecord=recordnum
003100041130     c
003200041130     c* chain su azmnum della filiale per vedere se c'e'
003300041130     c     knum          chain     aznum01l                           30
003400041130     c                   eval      recordnum=savrecord
003500041130     c  n30              update    aznum000
003600041130     c   30              write     aznum000
003700041130     c
003800041130     C                   read      AZNUM00f
003900041130     c                   enddo
004000021017
004100021017
004200021017     c                   Eval      *InLr = *On
004300041130     c**************************************************************
004400041130     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
004500041130     C     *INZSR        begsr
004600041130     C* CERCO CONIL TIBS55 LA LIBRERIA DI GRUPPO DI SEDE
004700041130     C                   CLEAR                   DSBS50
004800041130     c                   Movel     046           I50apo
004900041130     C                   CALL      'TIBS55R'
005000041130     C                   PARM                    DSBS50
005100041130     C
005200041130     C* overdbf  DI FILTRA FNLBL00F
005300041130     c                   movea     o50alg        OVRNUM0F(30)
005400041130     C                   SETOFF                                       01
005500041130     C                   CLEAR                   COMMAN           80
005600041130     C                   MOVEA     ovrnum0F      COMMAN
005700041130     c                   eval      comman=%trim(comman)+cnum0f
005800041130     C                   CALL      'QCMDEXC'
005900041130     C                   PARM                    COMMAN
006000041130     C                   PARM      50            LUNG             15 5
006100041130     C*
006200041130     C                   OPEN      AZNUM00F
006300041130     C
006400041130     C* overdbf  DI GAITRA FNLBL02L
006500041130     C                   CLEAR                   DSBS50
006600041130     c                   movel     'L'           i50tla
006700041130     c                   Movel     043           I50apo
006800041130     C                   CALL      'TIBS55R'
006900041130     C                   PARM                    DSBS50
007000041130     c                   movea     o50Alg        OVRNUM1L(30)
007100041130     C                   SETOFF                                       01
007200041130     C                   CLEAR                   COMMAN           80
007300041130     C                   MOVEA     ovrnum1l      COMMAN
007400041130     c                   eval      comman=%trim(comman)+cnum1l
007500041130     C                   CALL      'QCMDEXC'
007600041130     C                   PARM                    COMMAN
007700041130     C                   PARM      50            LUNG             15 5
007800041130     C*
007900041130     C                   OPEN      AZNUM01L
008000041130     c                   ENDSR
008100041130**         OVRnum0F
008200041130OVRDBF FILE(AZNUM00F) TOFILE(XXXXXXXXXX/AZNUM00F)
008300041130**         OVRnum1L
008400041130OVRDBF FILE(AZNUM01L) TOFILE(XXXXXXXXXX/AZNUM01L)
