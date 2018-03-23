000100090122     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(*caller)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600090122     D  pardis                        6s 0
000700090122     D  parfil                        3s 0
000800090122     D* file da cancellare
000900090122     d* FIPCT00F/FIPCD00F/- con esiti ricevuti
001000090122     d* FIPDC00T/FIPDD00T - con le spedizioni trasmesse al PDA
001100000000     C*---------------------------------------------------------------*
001200020624     c     *entry        plist
001300030113     c                   parm                    param
001400020624
001500020313     C/EXEC SQL
001600090122     C+ delete from FIpct00F where pctfgs = :parfil and pctndc = :pardis
001700020313     C/END-EXEC
001800020624      *
001900090122     C/EXEC SQL
002000090122     C+ delete from FIpcd00F where pcdfgs = :parfil and pcdndc = :pardis
002100090122     C/END-EXEC
002200090122      *
002300090122     C/EXEC SQL
002400090122     C+ delete from FIpdc00t where pdcfgs = :parfil and pdcndc = :pardis
002500090122     C/END-EXEC
002600090122      *
002700090122     C/EXEC SQL
002800090126     C+ delete from FIpdd00t where pddfgs = :parfil and pddndc = :pardis
002900090122     C/END-EXEC
003000090122      *
003100020624     c                   seton                                        lr
003200011129     C*--------------------------------------------------------------------
