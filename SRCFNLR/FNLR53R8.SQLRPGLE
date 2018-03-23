000100090122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(*caller)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600090122     D  pardis                        6s 0
000700090122     D  parfil                        3s 0
000800140207      *
000900140207      *  pulizia FICAU
001000140207     d fnlrx4ds      e ds                  INZ
001100140207     d kpjba         e ds                  INZ
001200140207      *
001300090122     D* file da cancellare
001400090122     d* FIPCT00F/FIPCD00F/- con esiti ricevuti
001500090122     d* FIPDC00T/FIPDD00T - con le spedizioni trasmesse al PDA
001600000000     C*---------------------------------------------------------------*
001700020624     c     *entry        plist
001800030113     c                   parm                    param
001900020624
002000020313     C/EXEC SQL
002100090122     C+ delete from FIpct00F where pctfgs = :parfil and pctndc = :pardis
002200020313     C/END-EXEC
002300020624      *
002400090122     C/EXEC SQL
002500090122     C+ delete from FIpcd00F where pcdfgs = :parfil and pcdndc = :pardis
002600090122     C/END-EXEC
002700090122      *
002800090122     C/EXEC SQL
002900090122     C+ delete from FIpdc00t where pdcfgs = :parfil and pdcndc = :pardis
003000090122     C/END-EXEC
003100090122      *
003200090122     C/EXEC SQL
003300090126     C+ delete from FIpdd00t where pddfgs = :parfil and pddndc = :pardis
003400090122     C/END-EXEC
003500090122      *
003600140207      *  Pulizia del FICAU
003700140207      *
003800140207     c                   clear                   fnlrx4ds
003900140207     c                   eval      IX4FGS =  parfil
004000140207     c                   eval      IX4NFV =  pardis
004100140207     c                   call      'FNLRX4R'
004200140207     c                   parm                    kpjba
004300140207     c                   parm                    fnlrx4ds
004400140207      *
004500140207      *
004600020624     c                   seton                                        lr
004700011129     C*--------------------------------------------------------------------
