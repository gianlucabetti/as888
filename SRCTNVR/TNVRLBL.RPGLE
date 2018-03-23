000100950323     H DECEDIT('0,') DATEDIT(*DMY.)
000200041119     FFNLBL00f  IF   E             DISK    usropn
000300041119     FFNLBL02L  uF a E           K DISK    usropn
000400041119     F                                     RENAME(FNlbl000:FNlbl002)
000500041119     D DSBS50        E DS                  EXTNAME(TIBS55DS)
000600041119     drecordlbl      E DS                  extname(fnlbl00f)
000700041119     dsavrecord        s                   like(recordlbl)
000800041122     D OVRlbl0f        S              1    DIM(39) CTDATA PERRCD(39)
000900041122     D OVRlbl2l        S              1    DIM(39) CTDATA PERRCD(39)
001000041122     D Clbl0f          C                   CONST('/FNLBL00F)')
001100041122     D Clbl2l          C                   CONST('/FNLBL02L)')
001200041119      *
001300041119     c
001400950627     C*
001500041119     C                   read      FNLBL00f
001600950328     C*
001700950612    1C     *IN31         DOWEQ     *OFF
001800041119     c* se sflaggato aggiorno comunque se c'e' il record
001900041119     c                   movel     recordlbl     savrecord
002000041119     c
002100041119     c     klbl2         chain     fnlbl02l
002200041119     c
002300041119     c                   if        not %found(fnlbl02l)
002400041119     c                   write     fnlbl002
002500041122     c                   feod      fnlbl02l
002600041119     c                   else
002700041119     c* Aggiorno se sflaggato
002800041119     c                   movel     savrecord     recordlbl
002900041119     c                   if        lblft2=' '
003000041119     c                   update    fnlbl002
003100041122     c                   feod      fnlbl02l
003200041119     c                   endif
003300041119     c                   endif
003400041119     c* richiamo fnlg25r per aggiornare titas
003500041119     c                   if        lblft2=' '
003600041119     c                   call      'FNLG25R'
003700041119     c                   parm                    recordlbl
003800041119     c                   parm                    i25tla            1
003900041119     c                   endif
004000041119     c
004100041119     C                   READ      FNLBL00f                               31
004200950627     C                   ENDDO
004300950627     C*
004400950201     C                   SETON                                        LR
004500041119     C*---------------------------------------------------------------
004600041119     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
004700041119     C     *INZSR        begsr
004800041119     C* CERCO CONIL TIBS55 LA LIBRERIA DI GRUPPO DI SEDE
004900041119     C                   CLEAR                   DSBS50
005000041119     c                   Movel     043           I50apo
005100041119     C                   CALL      'TIBS55R'
005200041119     C                   PARM                    DSBS50
005300041119     C
005400041119     C* overdbf  DI FILTRA FNLBL00F
005500041119     c                   movea     o50alg        OVRLBL0F(30)
005600041119     C                   SETOFF                                       01
005700041119     C                   CLEAR                   COMMAN           80
005800041119     C                   MOVEA     ovrLBL0F      COMMAN
005900041122     c                   eval      comman=%trim(comman)+clbl0f
006000041119     C                   CALL      'QCMDEXC'
006100041119     C                   PARM                    COMMAN
006200041119     C                   PARM      50            LUNG             15 5
006300041119     C*
006400041119     C                   OPEN      FNLBL00F
006500041119     C
006600041119     C* overdbf  DI GAITRA FNLBL02L
006700041119     C                   CLEAR                   DSBS50
006800041119     c                   movel     'L'           i50tla
006900041119     c                   Movel     046           I50apo
007000041119     C                   CALL      'TIBS55R'
007100041119     C                   PARM                    DSBS50
007200041119     c                   movea     o50Alg        OVRLBL2L(30)
007300041119     C                   SETOFF                                       01
007400041119     C                   CLEAR                   COMMAN           80
007500041119     C                   MOVEA     ovrlbl2l      COMMAN
007600041122     c                   eval      comman=%trim(comman)+clbl2l
007700041119     C                   CALL      'QCMDEXC'
007800041119     C                   PARM                    COMMAN
007900041119     C                   PARM      50            LUNG             15 5
008000041119     C*
008100041119     C                   OPEN      FNLBL02L
008200041119     c
008300041119     C     KLBL2         KLIST
008400041119     C                   KFLD                    LBLAAP
008500041119     C                   KFLD                    LBLLPP
008600041119     C                   KFLD                    LBLNRP
008700041119     C                   KFLD                    LBLNSP
008800041119     C                   KFLD                    LBLAAN
008900041119     C                   KFLD                    LBLLPN
009000041119     C                   KFLD                    LBLNRN
009100041119     C                   KFLD                    LBLNSN
009200041119     C*
009300041119     C                   ENDSR
009400041119**         OVRLBL0F
009500041119OVRDBF FILE(FNLBL00F) TOFILE(XXXXXXXXXX/FNLBL00F)
009600041119**         OVRLBL2L
009700041119OVRDBF FILE(FNLBL02L) TOFILE(XXXXXXXXXX/FNLBL02L)
