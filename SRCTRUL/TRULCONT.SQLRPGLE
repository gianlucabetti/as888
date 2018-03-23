000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400120216      * utility data una partita IVA verifica se esiste nell'anagrafica certificazione traini di Pedini
000500020624      *
000600020624     D param           DS
000700120919     D  parfil                        3s 0
000800120919     D  pardat                        8s 0
000900120919     D  partsr                        1
001000120919     d  esito                         1
001100120919     d*
001200120919     d  parcoddal      s              7s 0
001300120919     d  parcodal       s              7s 0
001400120502      * ?_______________________________________________________________________
001500120502      *?  in caso di nuovi errori possibili aggiungere nella legenda le specifiche
001600120502      * ?Legenda errori possibili di ritorno
001700120502     C*Legenda errori possibili di ritorno
001800120528     C* esito =  '1' trovata tariffa aperta
001900120502      * ?_______________________________________________________________________
002000020624     c     *entry        plist
002100030109     c                   parm                    param
002200120306     c*
002300120919     c                   z-add     9999999       parcodal
002400120919     c                   z-add     0             parcoddal
002500120919     c                   movel     parfil        parcodal
002600120919     c                   movel     parfil        parcoddal
002700120216     c                   clear                   esito
002800030109
002900120919     c                   select
003000120919     c                   when      partsr = 'A'
003100020313     C/EXEC SQL
003200120528     C+ SELECT '1' into :esito
003300120919     C+    FROM fiftt00F WHERE fttpdr between :parcoddal and :parcodal and
003400120919     C+    fttddc <= :pardat and fttfvl = 'C' and substr(fttflr, 1, 1) = ' '
003500120919     C+    fetch first row only
003600020313     C/END-EXEC
003700030109      *
003800120919     c                   when      partsr = 'C'
003900120919     C/EXEC SQL
004000120919     C+ SELECT '1' into :esito
004100120919     C+    FROM fictt00F WHERE cttpdr between :parcoddal and :parcodal and
004200120919     C+    cttddc <= :pardat and cttfvl = 'C' and cttfsr = ' '
004300120919     C+    fetch first row only
004400120919     C/END-EXEC
004500030109     c
004600120919     c                   when      partsr = 'D'
004700120919     C/EXEC SQL
004800120919     C+ SELECT '1' into :esito
004900120919     C+    FROM fiatt00F WHERE attpdr between :parcoddal and :parcodal and
005000120919     C+    attddc <= :pardat and attflg = 'C' and substr(attflr, 1, 1) = ' '
005100120919     C+    fetch first row only
005200120919     C/END-EXEC
005300020624      *
005400120919     c                   endsl
005500120919     c
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
