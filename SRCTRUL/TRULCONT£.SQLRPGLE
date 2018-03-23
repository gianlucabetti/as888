000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000800120216      * utility data una partita IVA verifica se esiste nell'anagrafica certificazione traini di Pedini
000900020624      *
001000020624     D param           DS
001100120528     D  parnrc                        7s 0
001101120528     D  pardt                         8s 0
001200120216     d  esito                         1
001401120502      * ?_______________________________________________________________________
001402120502      *?  in caso di nuovi errori possibili aggiungere nella legenda le specifiche
001404120502      * ?Legenda errori possibili di ritorno
001405120502     C*Legenda errori possibili di ritorno
001406120528     C* esito =  '1' trovata tariffa aperta
001410120502      * ?_______________________________________________________________________
001500020624     c     *entry        plist
001600030109     c                   parm                    param
001800120306     c*
003800120216     c                   clear                   esito
004100030109
004400020313     C/EXEC SQL
004500120528     C+ SELECT '1' into :esito
004600120605     C+    FROM fiadt00F WHERE adtnrc = :parnrc  and
004700120605     C+         :pardt < adtdst
004800120528     C+         and adtatb = ' ' fetch first row only
004900020313     C/END-EXEC
005000030109      *
005400030109     c
005401120502      * se lo trovo fra i trazionisti e ho inserito le date verifico anche periodo di validità
005500020624      *
005600030109     c                   seton                                        lr
005700011129     C*--------------------------------------------------------------------
