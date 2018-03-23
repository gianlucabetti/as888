000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200051114     H* UNIBO02R *---------------------------------------------------*
000300051114     H*         - UNISCO FNBL9 FNAR9 IN FIAR900F
000400000000     H*--------------------------------------------------------------*
000500051114     fFNar901L  if   e           k Disk
000600051114     f                                     prefix(bl9:3)
000700051114     fFNBL901L  if   e           k Disk
000800051114     fFnarb01L  if   e           k Disk
000900051114     fFnBLp01L  if   e           k Disk
001000051012     F
001100051012     F* FILE NUOVO
001200051114     fFIAR901L  if a e           k Disk    usropn
001300051012     F
001400051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001500051012     D
001600051012     d Te              s             66    Dim(7) Ctdata Perrcd(1)
001700051013     d ERR             s             35    Dim(4) Ctdata Perrcd(1)
001800051019     d OVRP            s             78    Dim(1) Ctdata Perrcd(1)
001900051012     D
002000051012     d STAFILE         s              5
002100051012     C
002200051114     c     Kbl9          Klist
002300051114     c                   Kfld                    bl9aas
002400051114     c                   Kfld                    bl9lnp
002500051114     c                   Kfld                    bl9nrs
002600051114     c                   Kfld                    bl9nsp
002700051012     c***
002800051114     c                   open      fiar901l
002900051019     c* Eseguo ovrprtf su qsysprt
003000051019     C                   MOVEA     ovrp          COMMAN           80
003100051019     C                   CALL      'QCMDEXC'                            47
003200051019     C                   PARM                    COMMAN
003300051019     C                   PARM      80            LUNG             15 5
003400051019     c
003500051019     c                   open      qsysprt
003600051019     c
003700051012     c***
003800051012     c                   except    TESTA
003900980130     C***
004000051114     C* FNBL900F
004100980130     C***
004200051114     C     *LOVAL        SETLL     FNBL901L
004300051114     C                   READ      FNBL901L
004400051012     C
004500051114    1C                   DOW       NOT %EOF(FNBL901L)
004600051012     C                   SETOFF                                       0102
004700051012     c
004800051114     c* Se record già presente in fiar9
004900051114     c     kbl9          chain     fiar901l
005000051114    2c                   if        %found(fiar901l)
005100051012     c* conto
005200051013     c                   seton                                        04
005300051013     c                   except    errore
005400051013     c                   setoff                                       04
005500051114     c                   add       1             giabl9            9 0
005600051012   X2c                   else
005700051012     c
005800051114     C* VERIFICO SE c'e' record in fnblp
005900051114     c     kbl9          chain     fnblp01l
006000051114    3c                   if        not %found(fnblp01l)
006100051114     c* Stampo errore manca bolla
006200051012     c* non lo stampo
006300051114     c                   eval      stafile='FNBL9'
006400051012     c                   seton                                        01
006500051012     c                   except    Errore
006600051012     c                   setoff                                       01
006700051114     c                   add       1             noblp             7 0
006800051114   x3c                   else
006900051012     c
007000051114     c                   eval      ar9aas=bl9aas
007100051114     c                   eval      ar9lnp=bl9lnp
007200051114     c                   eval      ar9nrs=bl9nrs
007300051114     c                   eval      ar9nsp=bl9nsp
007400051114     c                   eval      ar9tic=bl9tic
007500051114     c                   eval      ar9cas=bl9cas
007600051114     c                   eval      ar9cmb=bl9cmb
007700051114     c                   eval      ar9vca=bl9vca
007800051114     c                   eval      ar9gca=bl9gca
007900051012     c
008000051114     c                   write     fiar9000
008100051114     c                   add       1             aggbl9            9 0
008200051012    3c                   endif
008300051012    2c                   endif
008400051012     C
008500051114     C                   READ      FNBL901L
008600051012    1c                   enddo
008700051012     C
008800051012     c***
008900051114     C*   FNAR900f
009000051012     C***
009100051114     C     *LOVAL        SETLL     FNar901L
009200051114     C                   READ      FNar901L
009300051012     C
009400051114    1C                   DOW       NOT %EOF(FNar901L)
009500051012     C                   SETOFF                                       0302
009600051012     c
009700051114     c* Se record già presente in fiar9, non lo scrivo
009800051114     c     kbl9          chain     fiar901l
009900051114    2c                   if        %found(fiar901l)
010000051012     c* conto
010100051114     c                   add       1             giaar9            9 0
010200051012   X2c                   else
010300051012     c
010400051114     C* VERIFICO SE c'e' record in fnARb
010500051114     c     kbl9          chain     fnarb01l
010600051114    3c                   if        not %found(fnARb01l)
010700051114     c* Stampo errore manca bolla
010800051114     c                   eval      stafile='FNAR9'
010900051012     c                   seton                                        03
011000051012     c                   except    Errore
011100051012     c                   setoff                                       03
011200051114     c                   add       1             noarb             7 0
011300051012     c
011400051012   X3c                   else
011500051114     c
011600051114     c                   eval      ar9aas=bl9aas
011700051114     c                   eval      ar9lnp=bl9lnp
011800051114     c                   eval      ar9nrs=bl9nrs
011900051114     c                   eval      ar9nsp=bl9nsp
012000051114     c                   eval      ar9tic=bl9tic
012100051114     c                   eval      ar9cas=bl9cas
012200051114     c                   eval      ar9cmb=bl9cmb
012300051114     c                   eval      ar9vca=bl9vca
012400051114     c                   eval      ar9gca=bl9gca
012500051012     c
012600051114     c                   write     fiar9000
012700051114     c                   add       1             aggar9            9 0
012800051012    3c                   endif
012900051012     c
013000051012    2c                   endif
013100051012     C
013200051114     C                   READ      FNar901L
013300051012    1c                   enddo
013400051012     c***
013500051114     C     GIABL9        ADD       GIAAR9        GIATOT            9 0
013600051114     C     AGGBL9        ADD       AGGAR9        AGGTOT            9 0
013700051012     C
013800051012     c                   except    totali
013900051012     c***
014000020502     C                   SETON                                        LR
014100051012     c*------------------------------------------------------------------
014200051012
014300051012      *  T E S T A T A
014400051012     oQsysprt   e            Testa            02
014500051012     o                       Te(1)               66
014600051012     o                       Te(2)              132
014700051114     o                       *Date              120 '  /  /    '
014800051012     o                       Page          Z    132
014900051012     O
015000051012     o          e            ERRORE      1
015100051012     o                       STAFILE
015200051114     o                       BL9aas           +   1
015300051114     o                       BL9LNp           +   1
015400051114     o                       BL9NRS           +   1
015500051114     o                       BL9NSp        2  +   1
015600051114     o                       BL9tic           +   1
015700051114     o                       BL9cas        2  +   1
015800051114     o                       BL9vca           +   1
015900051114     o                       BL9cmb        2  +   1
016000051114     o                       BL9gca           +   1
016100051012     o               01      err(1)             132
016200051012     o               03      err(3)             132
016300051013     o               04      err(4)             132
016400051012     o
016500051012     o          e            TOTALI      2
016600051012     o                       Te(3)               66
016700051012     o                       Te(4)              132
016800051114     o                       GIABL9        2     38
016900051114     o                       NOBLp         2     67
017000051114     o                       AGGBL9        2    132
017100051012     O
017200051012     o          e            TOTALI      2
017300051012     o                       Te(5)               66
017400051012     o                       Te(6)              132
017500051114     o                       GIAAR9        2     38
017600051114     o                       NOARb         2     67
017700051114     o                       AGGAR9        2    132
017800051012     O
017900051012     o          e            TOTALI      2
018000051012     o                       Te(7)               66
018100051013     o                       GIATOT        2     43
018200051012     o                       AGGTOT        2     67
018300051012**         TE
018400051114                            **  UNISCI FNBL900F FNAR900F IN   F I    1
018500051114A R 9 O O F  **             UNIBO02R                     PAG. XXXX   2
018600051114FNBL900F : già in Fiar900f xxx.xxx.xxx  Non presente FNBLPx.xxx.xx   3
018700051114x                                            Aggiornatixxx.xxx.xxx   4
018800051114FNAR900F : già in Fiar900f xxx.xxx.xxx  Non presente FNARBx.xxx.xx   5
018900051114x                                            Aggiornatixxx.xxx.xxx   6
019000051114TOTALI GENERALI:già in Fiar900f xxx.xxx.xxx  AGGIORNATI XXX.XXX.XXX  7
019100051012**    ERR
019200051114MANCA bolla in FNBLP01L                                              1
019300051114
019400051114MANCA bolla in FNARB01L
019500051114Già presente in FIAR9
019600051019**
019700051114OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIAR900F')
