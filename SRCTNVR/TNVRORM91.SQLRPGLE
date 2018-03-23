000100020808      *PARMS CLOSQLCSR(*ENDMOD)
000200020808      *PARMS DYNUSRPRF(*OWNER)
000300041201
000400041201     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000500041201
000600020808     ffnorm00f  if   e             disk
000700010606     ffnorf01l  if   e           k disk
000800041201     fQsysprt   o    f  132        printer oflind(*InOf)
000900020808
001000020808     d StringaSql      s            500    Varying
001100010605
001200041201     d conta           s              7  0 inz
001300020807
001400020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
001500010604
001600010604      *--------------------------------------------------------------*
001700010604
001800020807      * Sql per selezionare gli ORM
001900020807      *                            ========
002000020808     c                   ExSr      SqlOrmPos
002100020807      *                            ========
002200041201
002300010614      *                            ========
002400010614     c                   exsr      SR_LEGGI
002500010614      *                            ========
002600010605
002700010608      * fine programma
002800010605     c                   eval      *inlr = *on
002900010614
003000020807      *--------------------------------------------------------------*
003100020807      * SQL  - Preparazione sql FNORM00F
003200020807      *--------------------------------------------------------------*
003300020807     c     SqlOrmPos     BegSr
003400020807
003500020808      * Preparo la stringa sql
003600020808     c                   Eval      StringaSql =
003700020808     c                             'select * '                                 +
003800041201     c                             'from fnorm00f where ormdao between ' +
003900041201     c                             %editc(pdatada:'4') +
004000041201     c                             ' and ' +
004100041201     c                              %editc(pdataal:'4') +
004200041201     c                             ' ORDER BY ormdao, ormpor for fetch ' +
004300041201     c                             'only'
004400020807
004500020807     C/EXEC SQL
004600020808     C+ prepare S1 from :StringaSql
004700020807     C/END-EXEC
004800020807
004900020808     C/EXEC SQL
005000020808     C+ declare C1 cursor for S1
005100020808     C/END-EXEC
005200020807
005300020807     c                   EndSr
005400020807
005500010604      *--------------------------------------------------------------*
005600020808      * LEGGO IL FILE DEGLI ORM DA SQL
005700010604      *--------------------------------------------------------------*
005800010614     c     SR_LEGGI      begsr
005900010604
006000020808     C/EXEC SQL
006100020808     C+ Open C1
006200020808     C/END-EXEC
006300020808
006400020808     C/EXEC SQL
006500020808     C+ Fetch C1 into :fnormxxds
006600020808     C/END-EXEC
006700020808
006800041201    1c                   Dow       Sqlcod = 0                                   *record letto
006900030321
007000041201    2c                   If        OrmFao = 999
007100041201     c     korf          Chain     fnorf01l
007200041201    3C                   if        orfcar = '91'
007300041201     C     korf1         readpe    fnorf01l
007400041201    4C                   if        orffar = 400
007500041201     c                   add       1             conta
007600041201     c                   Except    riga
007700041201    4c                   endif
007800041201    3c                   endif
007900041201    2c                   endif
008000020808
008100020808     C/EXEC SQL
008200020808     C+ Fetch C1 into :fnormxxds
008300020808     C/END-EXEC
008400020808
008500041201    1c                   EndDo
008600020808
008700020808     C/EXEC SQL
008800020808     C+ close C1
008900020808     C/END-EXEC
009000020808
009100020808     c                   EndSr
009200020808
009300020808      *--------------------------------------------------------------*
009400020808      * ELABORO I DATI LETTI
009500020808      *--------------------------------------------------------------*
009600020808     c     Sr_Elabora    BegSr
009700020808
009800020808     c                   EndSr
009900010614
010000010604      *--------------------------------------------------------------*
010100010604      * ROUTINE INIZIALE
010200010604      *--------------------------------------------------------------*
010300010604     c     *inzsr        begsr
010400010604      *
010500010604     c     *entry        plist
010600041201     c                   parm                    xdatada           8
010700041201     c                   parm                    xdataal           8
010800010621
010900041201     c                   movel     xdatada       pdatada           8 0
011000041201     c                   movel     xdataal       pdataal           8 0
011100041201
011200041201     c     kOrf          klist
011300041201     c                   kfld                    OrmPoe
011400041201     c                   kfld                    OrmNsr
011500041201     c                   kfld                    OrmNor
011600041201     c                   kfld                    OrmNrv
011700041201     c                   kfld                    OrmDfo
011800041201     C                   kfld                    OrmOfo
011900041201     C                   kfld                    OrmFao
012000041201
012100041201     c     kOrf1         klist
012200041201     c                   kfld                    OrmPoe
012300041201     c                   kfld                    OrmNsr
012400041201     c                   kfld                    OrmNor
012500041201     c                   kfld                    OrmNrv
012600010606
012700010604     c                   endsr
012800041201     oQsysprt   e            riga        1
012900041201     o                       Ormpoe               3
013000041201     o                       Ormnsr           +   1
013100041201     o                       Ormnor           +   1
013200041201     o                       Ormnrv           +   1
013300041201     o                       Ormpor           +   3
013400041201     o                       Ormpag           +   3
013500041201     o                       Ormksc           +   3
013600041201     o                       Ormcra           +   3
013700041201     o                       Ormcor           +   3
013800041201     o                       Ormcrc           +   3
013900041201     o                       conta         z  +   1
