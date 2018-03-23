000100950202     H DECEDIT('0,') DATEDIT(*DMY.)
000200950202      **************************************************************************
000300080904     Ftntam01l  iF   E           K DISK
000400950512     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500021023     F*
000600080905     d risultato       s             10  0
000700080905     d resto           s              7  0
000800080905     d warl            s              4  0
000900080905     d waro            s              4  0
001000950202     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
001100950202     C*
001200950202     C* MI POSIZIONO PER DATA INIZIALE E LEGGO SEQUENZIALMENTE
001300080904     C     *LOVAL        SETLL     tntam01l
001400080905     C                   READ      tntam01l                               31
001500950202     C*
001600950512    1C     *IN31         DOWEQ     *OFF
001700080905     c                   if        tamddt<=20080901 and tamdst>=20080901
001800080904     c                             and tamatb=' '
001900080904     c
002000080904     c                   if        (tamctr>=000 and tamctr<=099) or
002100080904     c                             (tamctr>=300 and tamctr<=399)
002200080905     c
002300080904     c
002400080905     c                   if        tamarl>0   and tamaro<>100 and tamaro<>1
002500080905     c                   if        %subst(%editc(tamarl:'X'):5:3)<>'000'  or
002600080905     c                             %subst(%editc(tamaro:'X'):5:3)<>'000'
002700080905     c                   except    stampa
002800080905     c                   else
002900080905     c
003000080905     c                   if        tamarl<100 and tamctr<=099
003100080905     c                   eval      warl=100
003200080905     c                   else
003300080905     c                   eval      warl=tamarl
003400080905     c                   endif
003500080905     c                   eval      waro=tamaro
003600080905     c                   eval      risultato=%div(warl:waro)
003700080905     c                   eval      resto    =%rem(warl:waro)
003800080904     c
003900080904     c                   if        resto>0
004000080904     c                   except    stampa
004100021023     c                   endif
004200080905     c                   endif
004300080904     c
004400080904     c                   endif
004500080904     c                   endif
004600080904     c                   endif
004700021023     c*
004800080904     C                   READ      tntam01l                               31
004900021023     c                   enddo
005000021023     C*
005100950512     C                   SETON                                        LR
005200950202     C*
005300021023     OQSYSPRT   E            stampa      1
005400080904     O                                              'Tariffa'
005500080904     O                       tamksc        Z   +  1
005600080904     O                       tamctr            +  1
005700080905     O                                         +  2 'progr.'
005800080905     O                       tamprg        z   +  1
005900080905     O                                         +  2 'Dec/Scad'
006000080905     O                       tamddt            +  1 '    /  /  '
006100080905     O                       tamdst            +  1 '    /  /  '
006200080905     O                                         +  2 'Arrot.'
006300080905     O                       tamarl        2   +  1
006400080904     O                       tamarf        2   +  1
006500080904     O                       tamaro        2   +  1
