000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020114      *--------------------------------------------------------------*
000300041201     d Kpjba         e ds
000400041201     D PrmFnz          S              2                                         Funzione chiamante
000500041201     D PrmKsc          S              7  0                                      Codice cliente
000600041201     D PrmRMN          S             15  0                                      Rif. mittente num.
000700041201     D PrmCmt          S              1                                         Sincronia file
000800041201     D PrmTLa          S              1                                         Tipo lancio
000900041201     D PrmEsi          S              1                                         Esito
001000041201     D PrmAAS          S              4  0
001100041201     D PrmLnP          S              3  0
001200041201     D PrmNrS          S              2  0
001300041201     D PrmNSp          S              7  0
001400041201     D PARAM           DS
001500041201     D  PARMAG                 1      3  0
001600041201     D  PARDAT                 4     11  0
001700041201     D  PARRIS                12     12
001800020909
001900041201     c                   eval      parmag=001
002000041201     c                   eval      parDAT=20020510
002100041201     C                   MOVEL     PARAM         KPJBU
002200041201     C                   CALL      'TNSDUBC'
002300041201     C                   PARM                    KPJBA
002400041201     C*
002500041201     C                   CALL      'TNSB55R'
002600041201     C                   PARM      'IN'          prmfnz
002700041201     C                   PARM      0010076       PrmKsc
002800041201     C                   PARM      0010076       PrmRMN
002900041201     C                   PARM      '0'           PrmCmt
003000041201     C                   PARM      'L'           PrmTLa
003100041201     C                   PARM                    PrmEsi
003200041201     C                   PARM                    PrmAAS
003300041201     C                   PARM                    PrmLnP
003400041201     C                   PARM                    PrmNrS
003500041201     C                   PARM                    PrmNSp
003600020909
003700020909     c                   Eval      *InLr = *On
