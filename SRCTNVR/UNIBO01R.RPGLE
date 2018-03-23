000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200051012     H* UNIBO01R *---------------------------------------------------*
000300051012     H*         - UNISCO FNBLS FNARS IN FIARS00F
000400000000     H*--------------------------------------------------------------*
000500051012     fFNARS01L  if   e           k Disk
000600051012     f                                     prefix(BLS:3)
000700051012     fFNBLS01L  if   e           k Disk
000800051012     fFnART27L  if   e           k Disk
000900051012     fFnBLT27L  if   e           k Disk
001000051012     F
001100051012     F* FILE NUOVO
001200051012     fFIARS01L  if a e           k Disk    usropn
001300051012     F
001400051019     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001500051012     D
001600051012     d Te              s             66    Dim(7) Ctdata Perrcd(1)
001700051013     d ERR             s             35    Dim(4) Ctdata Perrcd(1)
001800051019     d OVRP            s             78    Dim(1) Ctdata Perrcd(1)
001900051012     D
002000051012     d STAFILE         s              5
002100051012     D                 DS
002200051012     D  BLTLNP                 1      3  0
002300051012     D  BLTNRS                 4      5  0
002400051012     D  BLTNSP                 6     12  0
002500051012     D  BLTAAS                13     16  0
002600051012     D  BLTSPE                 1     16  0
002700051012     D                 DS
002800051012     D  ARTLNP                 1      3  0
002900051012     D  ARTNRS                 4      5  0
003000051012     D  ARTNSP                 6     12  0
003100051012     D  ARTAAS                13     16  0
003200051012     D  ARTSPE                 1     16  0
003300051012     c     *entry        plist
003400051012     c                   parm                    waggio            1
003500051012     C
003600051012     c     Kbls          Klist
003700051012     c                   Kfld                    blsfls
003800051012     c                   Kfld                    blslna
003900051012     c                   Kfld                    blsnrs
004000051012     c                   Kfld                    blsnsc
004100051012     c                   Kfld                    blstrc
004200051012     c     KblsT         Klist
004300051012     c                   Kfld                    blsfls
004400051012     c                   Kfld                    blslna
004500051012     c                   Kfld                    blsnrs
004600051012     c                   Kfld                    blsnsc
004700051012     c***
004800051012     c                   if        waggio='S'
004900051012     c                   open      fiars01l
005000051012     c                   endif
005100051019     c* Eseguo ovrprtf su qsysprt
005200051019     C                   MOVEA     ovrp          COMMAN           80
005300051019     C                   CALL      'QCMDEXC'                            47
005400051019     C                   PARM                    COMMAN
005500051019     C                   PARM      80            LUNG             15 5
005600051019     c
005700051019     c                   open      qsysprt
005800051019     c
005900051012     c***
006000051012     c                   except    TESTA
006100980130     C***
006200051012     C* FNBLS00F
006300980130     C***
006400051012     C     *LOVAL        SETLL     FNBLS01L
006500051012     C                   READ      FNBLS01L
006600051012     C
006700051012    1C                   DOW       NOT %EOF(FNBLS01L)
006800051012     C                   SETOFF                                       0102
006900051012     c
007000051012     c* Se record già presente in fiars, non lo scrivo
007100051012     c     kbls          chain     fiars01l
007200051012    2c                   if        %found(fiars01l)
007300051012     c* conto
007400051013     c                   seton                                        04
007500051013     c                   except    errore
007600051013     c                   setoff                                       04
007700051013     c                   add       1             giabls            9 0
007800051012   X2c                   else
007900051012     c
008000051012     C* VERIFICO SE c'e' record in fnblt
008100051012     c     kblsT         chain     fnblt27l
008200051012    3c                   if        not %found(fnblt27l)
008300051012     c* Stampo errore manca dettaglio colli
008400051012     c* non lo stampo
008500051012     c                   eval      stafile='FNBLS'
008600051012     c                   seton                                        01
008700051012     c                   except    Errore
008800051012     c                   setoff                                       01
008900051012     c                   add       1             noblt             7 0
009000051012     c
009100051012   X3c                   else
009200051012     c* Verifico se presente anche in art, che abbiano la stessa
009300051012     c**  spedizione
009400051012     c     kblsT         chain     fnart27l
009500051012    4C                   if        %found(fnart27l) AND
009600051012     c                             bltspe<>artspe
009700051012     c* Stampo errore
009800051012     c                   eval      stafile='FNBLS'
009900051012     c                   seton                                        02
010000051012     c                   except    Errore
010100051012     c                   add       1             NOSTESSoart       7 0
010200051012    4c                   endif
010300051012     c
010400051012     C* SE c'e' anche in fnart, con sped <>, vince fnbls
010500051012     c                   if        waggio='S'
010600051012     c                   eval      arsfls=blsfls
010700051012     c                   eval      arslna=blslna
010800051012     c                   eval      arsnrs=blsnrs
010900051012     c                   eval      arsnsc=blsnsc
011000051012     c                   eval      arstrc=blstrc
011100051012     c                   eval      arsnot=blsnot
011200051012     c                   eval      arscan=blscan
011300051012     c                   eval      arsdat=blsdat
011400051012     c                   eval      arsduv=blsdtr
011500051012     c
011600051012     c                   write     fiars000
011700051012    3c                   endif
011800051012    c
011900051012     c                   add       1             aggbls            9 0
012000051012     c
012100051012    3c                   endif
012200051012    2c                   endif
012300051012     C
012400051012     C                   READ      FNBLS01L
012500051012    1c                   enddo
012600051012     C
012700051012     c***
012800051012     C*   FNARS00f
012900051012     C***
013000051012     C     *LOVAL        SETLL     FNarS01L
013100051012     C                   READ      FNarS01L
013200051012     C
013300051012    1C                   DOW       NOT %EOF(FNarS01L)
013400051012     C                   SETOFF                                       0302
013500051012     c
013600051012     c* Se record già presente in fiars, non lo scrivo
013700051012     c     kbls          chain     fiars01l
013800051012    2c                   if        %found(fiars01l)
013900051012     c* conto
014000051012     c                   add       1             giaars            9 0
014100051012   X2c                   else
014200051012     c
014300051012     C* VERIFICO SE c'e' record in fnART
014400051012     c     kblsT         chain     fnart27l
014500051012    3c                   if        not %found(fnART27l)
014600051012     c* Stampo errore manca dettaglio colli
014700051012     c* non lo stampo
014800051012     c                   eval      stafile='FNARS'
014900051012     c                   seton                                        03
015000051012     c                   except    Errore
015100051012     c                   setoff                                       03
015200051012     c                   add       1             noart             7 0
015300051012     c
015400051012   X3c                   else
015500051012     c* Verifico se presente anche in blt, che abbiano la stessa
015600051012     c**  spedizione
015700051012     c     kblsT         chain     fnblt27l
015800051012    4C                   if        %found(fnblt27l) AND
015900051012     c                             bltspe<>artspe
016000051012     c* Stampo errore
016100051012     c                   eval      stafile='FNARS'
016200051012     c                   seton                                        02
016300051012     c                   except    Errore
016400051012     c                   add       1             NOSTESSoblt       7 0
016500051012    4c                   endif
016600051012     c
016700051012     C* SE c'e' anche in fnart, con sped <>, vince fnbls
016800051012     c                   if        waggio='S' and not *in02
016900051012     c                   eval      arsfls=blsfls
017000051012     c                   eval      arslna=blslna
017100051012     c                   eval      arsnrs=blsnrs
017200051012     c                   eval      arsnsc=blsnsc
017300051012     c                   eval      arstrc=blstrc
017400051012     c                   eval      arsnot=blsnot
017500051012     c                   eval      arscan=blscan
017600051012     c                   eval      arsdat=blsdat
017700051012     c                   eval      arsduv=blsdtr
017800051012     c
017900051012     c                   write     fiars000
018000051012    3c                   endif
018100051012     c                   add       1             aggars            9 0
018200051012     c
018300051012    3c                   endif
018400051012    2c                   endif
018500051012     C
018600051012     C                   READ      FNarS01L
018700051012    1c                   enddo
018800051012     c***
018900051012     C     GIABLS        ADD       GIAARS        GIATOT            9 0
019000051012     C     AGGBLS        ADD       AGGARS        AGGTOT            9 0
019100051012     C
019200051012     c                   except    totali
019300051012     c***
019400020502     C                   SETON                                        LR
019500051012     c*------------------------------------------------------------------
019600051012
019700051012      *  T E S T A T A
019800051012     oQsysprt   e            Testa            02
019900051012     o                       Te(1)               66
020000051012     o                       Te(2)              132
020100051012     o                       *Date         8    120
020200051012     o                       Page          Z    132
020300051012     O
020400051012     o          e            ERRORE      1
020500051012     o                       STAFILE
020600051012     o                       BLSFLS           +   1
020700051012     o                       BLSLNA           +   1
020800051012     o                       BLSNRS           +   1
020900051012     o                       BLSNSC        Z  +   1
021000051012     o                       BLSTRC           +   1
021100051012     o                       BLSNOT           +   1
021200051012     o                       BLSDAT        Z  +   1
021300051012     o               01      err(1)             132
021400051012     o               02      err(2)             132
021500051012     o               03      err(3)             132
021600051013     o               04      err(4)             132
021700051012     o
021800051012     o          e            TOTALI      2
021900051012     o                       Te(3)               66
022000051012     o                       Te(4)              132
022100051012     o                       GIABLS        2     38
022200051013     o                       NOBLT         2     67
022300051013     o                       NOSTESSOART   2    109
022400051013     o                       AGGBLS        2    132
022500051012     O
022600051012     o          e            TOTALI      2
022700051012     o                       Te(5)               66
022800051012     o                       Te(6)              132
022900051012     o                       GIAARS        2     38
023000051013     o                       NOART         2     67
023100051013     o                       NOSTESSOBLT   2    109
023200051013     o                       AGGARS        2    132
023300051012     O
023400051012     o          e            TOTALI      2
023500051012     o                       Te(7)               66
023600051013     o                       GIATOT        2     43
023700051012     o                       AGGTOT        2     67
023800051012**         TE
023900051013                            **  UNISCI FNBLS00F FNARS00F IN   F I    1
024000051012A R S O O F  **             UNIBO01R                     PAG. XXXX   2
024100051012FNBLS00F : già in Fiars00f xxx.xxx.xxx  Non presente FNBLTx.xxx.xx   3
024200051012x  Non ha stessa spedizioni in ARTx.xxx.xxx  Aggiornatixxx.xxx.xxx   4
024300051012FNARS00F : già in Fiars00f xxx.xxx.xxx  Non presente FNARTx.xxx.xx   5
024400051012x  Non ha stessa spedizioni in BLTx.xxx.xxx  Aggiornatixxx.xxx.xxx   6
024500051012TOTALI GENERALI:già in Fiars00f xxx.xxx.xxx  AGGIORNATI XXX.XXX.XXX  7
024600051012**    ERR
024700051012MANCA DETTAGLIO COLLI IN FNBLT00F                                    1
024800051013in art e blt num.spedizione diverso                                  2
024900051012MANCA DETTAGLIO COLLI IN FNART00F                                    3
025000051013Già presente in FIARS
025100051019**
025200051019OVRPRTF FILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES) USRDTA('FIARS00F')
