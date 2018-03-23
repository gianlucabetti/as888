000100180115      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVADWR/EDIVAD1WR.
000200141028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400000801     D*----------------------------------------------------
000500000801     D* DICHIARAZIOINE VARIABILI DI WRK
000600000801     D*----------------------------------------------------
000700180116
000800990920     D dscmz         e ds                  inz
000900180116     D esito           s              1
001000141028
001100010201
001200010201
001300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
001400000616     C*
001500010201     C                   seton                                        LR
001600990908
001700000801
001800910830     C*--------------------------------------------------------
001900171219     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR EDIVATWR e EDIVAD1WR   *
002000910830     C*--------------------------------------------------------
002100040526     C     RWFILE        BEGSR
002200990910     C*
002300180116     C                   exsr      invio
002400990920     C*
002500910830     C                   ENDSR
002600000613     C***
002700140922
002800000801
002900990920      /TITLE Invio dei dati al punto operativo.
003000010202     C     invio         BEGSR
003100180116     C*
003200180116     C* 1°bis invio EDIVAD10
003300180116     C                   reset                   dscmz
003400180116     C                   move      '046'         cmzdst
003500180116     C                   eval      cmzfld = 'EDIVAD1WR'
003600180116     C                   eval      cmzmbd = ' TESTINVIO'
003700180116     C                   eval      %subst(cmzmbd:1:1) = 'M'
003800180116     C***                if        prmfir = *blanks
003900180116     C                   eval      cmzfla = 'EDIVAD10F'
004000180116     C                   eval      cmzmba = 'EDIVAD10F'
004100180116     C***                else
004200180116     C***                eval      cmzfla = prmfir
004300180116     C***                eval      cmzmba = prmfir
004400180116     C***                endif
004500180116     C                   eval      cmznrr = *zeros
004600180116     C**lk               move      '05'          cmznrr
004700180116     C                   eval      cmzlba = 'UNITRAGRU'
004800180116     C                   call(e)   'TIS711C'
004900180116     C                   parm                    dscmz
005000180116     C                   parm      *blanks       esito
005100990920     C*
005200000613     C                   ENDSR
005300000613     C***
005400070411
