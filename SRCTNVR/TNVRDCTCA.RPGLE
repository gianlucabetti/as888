000100110419     FFNDCT01L  uF   E           K DISK
000200110419     FFNarb01l  iF   E           K DISK
000300110419     FFNblp01l  iF   E           K DISK
000400010801     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500010706
000600010801     D DDCT01        E DS
000700010706
000800110419     c
000900110419     c     kdct          klist
001000110419     c                   kfld                    dctaas
001100110419     c                   kfld                    dctlnp
001200110419     c                   kfld                    dctnrs
001300110419     c                   kfld                    dctnsp
001400010620
001500110419     C     *LOVAL        SETLL     FNDCT01L
001600010801     C                   READ      FNDCT000
001700981125
001800110419    1C                   DOW       NOT %EOF(FNDCT01L)
001900110419     c                   eval      ddct01=dctflo
002000110419     c*
002100110419     c
002200110419     c* cerco bolla in fnarb: se trovata imposto consegna anomala
002300110419     c     kdct          chain     fnarb01l
002400110419    2c                   if        %found(fnarb01l)
002500110419    3c                   if        arbcca<>' '
002600110419     c                   eval      §dctcca=arbcca
002700110419   x3c                   else
002800110419     c*  se non c'e' consegna anomala ma si tratta di bolla legata, imposto
002900110419     c*  dirottamento
003000110419    4c                   if        arbfbr<>'S' and §dctlega='S'
003100110419     c                   eval      §dctcca='1'
003200110419    4c                   endif
003300110419    3c                   endif
003400110419     c
003500110419   x2c                   else
003600110419     c     kdct          chain     fnblp01l
003700110419    3c                   if        %found(fnblp01l)
003800110419     c
003900110419    4c                   if        blpcca<>' '
004000110419     c                   eval      §dctcca=blpcca
004100110419   x4c                   else
004200110419     c*  se non c'e' consegna anomala ma si tratta di bolla legata, imposto
004300110419     c*  dirottamento
004400110419    5c                   if        blpfbr<>'S' and §dctlega='S'
004500110419     c                   eval      §dctcca='1'
004600110419    5c                   endif
004700110419    4c                   endif
004800110419    3c                   endif
004900110419    2c                   endif
005000010620
005100110419     c                   if        §dctcca<>' '
005200110419     c                   eval      dctflo=ddct01
005300110419     c                   update    fndct000
005400110419     c                   except    detta
005500110419     c                   else
005600110419     c                   unlock    fndct01l
005700110419     c                   endif
005800110419     c
005900010801     C                   READ      FNDCT000
006000010706
006100110419    1C                   ENDDO
006200010620
006300010801     C                   EVAL      *INLR = *ON
006400010706      *-------------------------------------------------
006500010801     O*
006600110419     Oqsysprt   E            DETTA       1
006700110419     O                                              'Consegna anomala in C.A.'
006800110419     O                       DCTAAC            +  1
006900110419     O                       DCTFIL            +  1
007000110419     O                       DCTNCA        2   +  1
007100110419     O                       DCTaas            +  1
007200110419     O                       DCTlnp            +  1
007300110419     O                       dctnrs            +  1
007400110419     O                       DCTnsp        2   +  1
007500110419     O                       dctflo            +  1
