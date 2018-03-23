000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200050228     FTITAS02L  UF   E           K DISK
000300050301     FQSYSPRT   O    F  132        PRINTER oflind(*inof)
000400050228
000500050301     d wksc            s              7
000600050228     d Wvlf            S                   like(TASvlf)
000700050228     d Wfvf            S                   like(TASfvf)
000800050228     d Kksc            s                   like(TASksc)
000900050228     d ktbl            s                   like(TAStbl) inz('F1')
001000050228     d kaas            s                   like(TASaas) inz(2005)
001100050228     d kmgs            s                   like(TASmgs) inz(0201)
001200050228
001300050301     c     *entry        plist
001400050301     c                   parm                    wksc
001500050228      *
001600050228     C     KTAS4         KLIST
001700050228     C                   KFLD                    Kksc
001800050228     C                   KFLD                    ktbl
001900050228     C                   KFLD                    kaas
002000050228     C                   KFLD                    kmgs
002100050228      *
002200050228     C     KTAS3         KLIST
002300050228     C                   KFLD                    Kksc
002400050228     C                   KFLD                    ktbl
002500050228     C                   KFLD                    kaas
002600050301
002700050301     c                   Move      wksc          kksc
002800050228
002900050228      *
003000050228     C     Ktas4         SETLL     TITAS000
003100050228     C     Ktas3         READE     TITAS000
003200050228      *
003300050228    1C                   DOW       not %EOF(TITAS02L)
003400050228      * Elaboro solo Febbraio e lnp 5, 93 o 110
003500050228     c                   IF        TASmgs < 0301  and
003600050228     c                             (TASlnp = 005 or TASlnp = 093 or
003700050301     c                              TASlnp = 110)  and
003800050228      *
003900050228      * Elaboro se il VOLUME DA FATTURARE è impostato dal cliente "R"
004000050301     C                             TASfvf = 'R'
004100050228      *
004200050228      * Se pesata totale dal VDL fatturo con il volume VDL
004300050228     c                   IF        TASncl = TASncr
004400050228     c                   eval      Wvlf = TASvlc
004500050228     c                   eval      Wfvf = 'T'
004600050228      *  Altrimenti lo calcolo da tariffa
004700050228     c                   ELSE
004800050228     c     TASpkb        div(H)    200           Wvlf
004900050301     c                   eval      Wfvf = 'K'
005000050228     C                   ENDIF
005100050228      *
005200050301     c                   If        Not *In99 or *Inof
005300050301     c                   Except    Testa
005400050301     c                   Eval      *In99 = *On
005500050301     c                   EndIf
005600000524     C                   EXCEPT    STAMPA
005700050228      *
005800050228     c                   eval      TASvlf = Wvlf
005900050228     c                   eval      TASfvf = Wfvf
006000050228      *
006100000524     C                   UPDATE    TITAS000
006200000524     C                   ENDIF
006300980703     C**
006400050228     C     Ktas3         READE     TITAS000
006500980703     C                   ENDDO
006600000000     C*
006700050228     C                   EVAL      *INLR = *ON
006800050228
006900050301     OQSYSPRT   E            testa          2 01
007000050301     o                                           19 'Numero Spedizione'
007100050301     o                                         +  1 'Lna'
007200050301     o                                         +  1 'mm/gg'
007300050301     o                                         +  1 'colli'
007400050301     o                                         +  1 'peso bol'
007500050301     o                                         +  2 'Vol vdl'
007600050301     o                                         +  1 'colli'
007700050301     o                                         +  3 'Vol boll'
007800050301     o                                         +  3 'Vol fatt'
007900050301     o                                         +  3 'Vol f new'
008000050301     O          E            STAMPA         1
008100050228     O                       TASAAS        Z
008200050228     O                       TASLNP        Z   +  1
008300050228     O                       TASNRS        Z   +  1
008400050228     O                       TASNSP        Z   +  1
008500050301     O                       TASLNA        Z   +  1
008600050228     O                       TASMGS            +  1 '  /  '
008700050228     O                       TASNCL        Z   +  1
008800050301     O                       TASpkb        4   +  1
008900050228     O                       TASVLC        4   +  3
009000050228     O                       TASNCR        Z   +  1
009100050228     O                       TASFVB            +  3
009200050228     O                       TASVLB        4   +  1
009300050228     O                       TASFVF            +  3
009400050228     O                       TASVLF        4   +  1
009500050301     o                       wfvf              +  3
009600050301     o                       wvlf          4   +  1
