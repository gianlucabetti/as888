000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020909
000300020909      * -------------------------------------------------------------*
000400041201      * Crea numeratori fdi SEDE   da FLNUF a AZNUM
000500041201      * Fissi 103  109
000600020114      *--------------------------------------------------------------*
000700020909
000800020909     fFlnuf01l  if   e           k Disk                                         Numeratore vecchio
000900020909     fAznum01L  uf a e           k Disk                                         Numeratore nuovo
001000020909     fQsysprt   o    f  198        Printer Oflind(*InOf)
001100041201     F                                     usropn
001200020909
001300041201     D OVRPRTF         S             75    DIM(1) CTDATA PERRCD(1)
001400020909     d Trul33Ds      e ds
001500041201     d Kpjba         e ds
001600020909
001700020909      *Klist
001800020909
001900020909     c     Knuf          Klist
002000020909     c                   Kfld                    NufAaa
002100020909     c                   Kfld                    NufCnu
002200041201     c
002300041201     c                   eval      comman=ovrprtf(1)
002400041201     C                   CALL      'QCMDEXC'
002500041201     C                   PARM                    COMMAN           80
002600041201     C                   PARM      80            LUNG             15 5
002700041201     c                   open      qsysprt
002800020909
002900020909     c                   Except    Testa
003000020909
003100020910      *-------------------------
003200041201      * N U M E R A T O R E  1 0 3
003300020910      *-------------------------
003400020909      * Cerco il vecchio numero
003500041201     c                   z-add     2004          NufAaa
003600041201     c                   Z-Add     103           NufCnu
003700041201     c     KNuf          setll     Flnuf01l
003800041201     c     KNuf          reade     Flnuf01l
003900041201If  1c                   dow       not %eof(Flnuf01l)
004000020910      * Creo il nuovo numero
004100020910     c                   Clear                   Trul33Ds
004200020910     c                   Eval      I33Ope = 2
004300020910     c                   Eval      I33Cnu = NufCnu
004400041201     c                   Eval      I33Po1 = nuffil
004500020910     c                   Eval      I33Unp = NufNum
004600020910     c                   Movel(p)  Trul33Ds      Kpjbu
004700020910     c                   Call      'TRUL33R'
004800020910     c                   Parm                    Kpjba
004900020910     c                   Movel     Kpjbu         Trul33Ds
005000020910If  2c                   If        O33Err <> *Zeros
005100020910     c                   Except    RigaErr
005200020910  x 2c                   Else
005300020910     c                   Except    RigaOk
005400020910    2c                   EndIf
005500041201     c
005600041201     c     KNuf          reade     Flnuf01l
005700041201    1c                   Enddo
005800020910
005900041201      *-------------------------
006000041201      * N U M E R A T O R E  50
006100041201      *-------------------------
006200041201      * Cerco il vecchio numero
006300041201     c                   z-add     2004          NufAaa
006400041201     c                   Z-Add     50            NufCnu
006500041201     c     KNuf          setll     Flnuf01l
006600041201     c     KNuf          reade     Flnuf01l
006700041201If  1c                   dow       not %eof(Flnuf01l)
006800041201      * Creo il nuovo numero
006900041201     c                   Clear                   Trul33Ds
007000041201     c                   Eval      I33Ope = 2
007100041201     c                   Eval      I33Cnu = NufCnu
007200041201     c                   Eval      I33Po1 = nuffil
007300041201     c                   Eval      I33Unp = NufNum
007400041201     c                   Movel(p)  Trul33Ds      Kpjbu
007500041201     c                   Call      'TRUL33R'
007600041201     c                   Parm                    Kpjba
007700041201     c                   Movel     Kpjbu         Trul33Ds
007800041201If  2c                   If        O33Err <> *Zeros
007900041201     c                   Except    RigaErr
008000041201  x 2c                   Else
008100041201     c                   Except    RigaOk
008200041201    2c                   EndIf
008300041201     c
008400041201     c     KNuf          reade     Flnuf01l
008500041201    1c                   Enddo
008600020910      *-------------------------
008700041201      * N U M E R A T O R E  1 09
008800020910      *-------------------------
008900020910      * Cerco il vecchio numero
009000020910     c                   Clear                   NufAaa
009100041201     c                   Z-Add     109           NufCnu
009200041201     c     KNuf          setll     Flnuf01l
009300041201     c     KNuf          reade     Flnuf01l
009400041201If  1c                   dow       not %eof(Flnuf01l)
009500020909     c                   Clear                   Trul33Ds
009600020909     c                   Eval      I33Ope = 2
009700020910     c                   Eval      I33Cnu = NufCnu
009800041201     c                   Eval      I33Po1 = nuFfil
009900041201     c                   Eval      I33Unp = NufNum
010000041201     c                   Movel(p)  Trul33Ds      Kpjbu
010100041201     c                   Call      'TRUL33R'
010200041201     c                   Parm                    Kpjba
010300041201     c                   Movel     Kpjbu         Trul33Ds
010400041201If  2c                   If        O33Err <> *Zeros
010500041201     c                   Except    RigaErr
010600041201  x 2c                   Else
010700041201     c                   Except    RigaOk
010800041201    2c                   EndIf
010900041201     c
011000041201     c     KNuf          reade     Flnuf01l
011100041201    1c                   Enddo
011200020910
011300020909     c                   Clear                   Trul33Ds
011400020909     c                   Eval      I33Tla = 'C'
011500020909     c                   Movel(p)  Trul33Ds      Kpjbu
011600020909     c                   Call      'TRUL33R'
011700020909     c                   Parm                    Kpjba
011800020909
011900041201     c                   close     qsysprt
012000020909     c                   Eval      *InLr = *On
012100020909
012200020909     oQsysprt   e            Testa          2 02
012300020909     o                                         +  2 'Creazione nuovi'
012400041201     o                                         +  1 'Numeratori di SEDE'
012500041201     o                                         +  5 'TNVRNUFSED'
012600020909     o                       Udate             +  2 '  /  /  '
012700020909     o                                         +  5 'Pag.'
012800020909     o                       Page          Z
012900020909     o          e            Testa          2
013000020909     o                                         +  5 'Punto Operativo'
013100020909     o                                         +  3 'Numeratore'
013200041201     o                                         +  3 'Numero  Vecchio'
013300041201     o                                         +  3 'Numero   Nuovo'
013400041201     o                                         +  3 'Anno'
013500020909     o          e            RigaOk
013600020909     o                       O33Po1            + 12
013700020909     o                       I33Cnu            +  9
013800041201     o                       I33Unp        Z   +  9
013900041201     o                       O33Nri        Z   +  2
014000041201     o                       o33aaa            +  3
014100020909     o          e            RigaErr
014200041201     o                       I33Po1            + 12
014300020909     o                       I33Cnu            +  9
014400041201     o                       I33Unp        z   +  9
014500041201     o                       O33Nri        z   +  2
014600041201     o                       o33aaa            +  3
014700041201     o                       O33Err            +  2
014800041201     o                       O33Msg            +  2
014900041201
015000041201**
015100041201OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) OUTQ(LJ4200CED) HOLD(*YES) SAVE(*YES)
