000100021017     H DECEDIT('0,') DATEDIT(*DMY.)
000200021017
000300021017      *--------------------------------------------------------------*
000400021017      * Crea numeratore copiandolo da FLNUF a AZNUM
000500021017      * (passato me parametro - alfanumerico)
000600021017      * Prima 1 numeratore per ogni elaboratore
000700021017      * Ora   1 numeratore per Terminal (SIMFEL)
000800021017      *--------------------------------------------------------------*
000900021017
001000021017     fFlnuf01l  if   e           k Disk                                         Numeratore vecchio
001100021017     fQsysprt   o    f  198        Printer Oflind(*InOf)
001200021017
001300021017     d Ut§dse0f      e ds                  Inz
001400021017     d Cncr80        e ds
001500021017     d Trul33Ds      e ds
001600021017     d Kpjba         e ds
001700021017
001800021017     d Status         sds           333
001900021017     d  SDSpgm           *proc
002000021017
002100021017      *Klist
002200021017     c     Knuf          Klist
002300021017     c                   Kfld                    NufAaa
002400021017     c                   Kfld                    NufCnu
002500021017
002600021017     c     *Entry        Plist
002700021017     c                   parm                    P_Num             3
002800021017
002900021017     c                   Call      'X§PARUT'
003000021017     c                   Parm                    Ut§dse0f
003100021017     c                   Movel     Rec80         Cncr80
003200021017
003300021017     c                   Except    Testa
003400021017
003500021017      *-----------------------------
003600021017      * N U M E R A T O R E   0 0 4
003700021017      *-----------------------------
003800021017      * Fare solo per Simfel
003900021017      * Cerco il vecchio numero
004000021017     c                   Clear                   NufAaa
004100021017     c                   Move      P_Num         NufCnu
004200021017     c     KNuf          Chain     Flnuf01l
004300021017If  1c                   If        %Found(Flnuf01l)
004400021017      * Creo il nuovo numero
004500021017     c                   Clear                   Trul33Ds
004600021017     c                   Eval      I33Ope = 2
004700021017     c                   Eval      I33Cnu = NufCnu
004800021017     c                   Eval      I33Po1 = Simfel
004900021017     c                   Eval      I33Unp = NufNum
005000021017     c                   Movel(p)  Trul33Ds      Kpjbu
005100021017     c                   Call      'TRUL33R'
005200021017     c                   Parm                    Kpjba
005300021017     c                   Movel     Kpjbu         Trul33Ds
005400021017If  2c                   If        O33Err <> *Zeros
005500021017     c                   Except    RigaErr
005600021017  x 2c                   Else
005700021017     c                   Except    RigaOk
005800021017E   2c                   EndIf
005900021017E   1c                   EndIf
006000021017
006100021017      * Fine
006200021017
006300021017     c                   Clear                   Trul33Ds
006400021017     c                   Eval      I33Tla = 'C'
006500021017     c                   Movel(p)  Trul33Ds      Kpjbu
006600021017     c                   Call      'TRUL33R'
006700021017     c                   Parm                    Kpjba
006800021017
006900021017     c                   Eval      *InLr = *On
007000021017
007100021017      * S T A M P A
007200021017
007300021017     oQsysprt   e            Testa          2 02
007400021017     o                                         +  2 'Creazione nuovi'
007500021017     o                                         +  1 'Numeratori Fogli Viaggio'
007600021017     o                                         +  1 '('
007700021017     o                       P_Num             +  0
007800021017     o                                         +  0 ')'
007900021017     o                                         +  2 'Nel p.o.'
008000021017     o                       Simfel            +  1
008100021017     o                       SDSpgm            +  5
008200021017     o                       *date             +  5 '  /  /    '
008300021017     o                                         +  5 'Pag.'
008400021017     o                       Page          Z
008500021017     o          e            Testa          2
008600021017     o                                         +  5 'Punto Operativo'
008700021017     o                                         +  3 'Numeratore'
008800021017     o                                         +  3 'Numero Vecchio'
008900021017     o                                         +  3 'Numero Nuovo'
009000021017     o          e            RigaOk
009100021017     o                       O33Po1            + 12
009200021017     o                       I33Cnu            +  9
009300021017     o                       I33Unp            +  9
009400021017     o                       O33Nri            +  9
009500021017     o          e            RigaErr
009600021017     o                       O33Po1            + 12
009700021017     o                       I33Cnu            +  9
009800021017     o                       I33Unp            +  9
009900021017     o                       O33Nri            +  9
010000021017     o                       O33Err            +  1
010100021017     o                       O33Msg            +  1
