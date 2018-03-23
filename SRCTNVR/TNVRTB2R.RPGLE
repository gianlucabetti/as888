000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300020722     H/TITLE  tarscodifica tabella TABEL00F a tabella TNTBE00F
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600020723      * tnvrtb2r                                                     *
000700951009      *     *----------------------------------------------*         *
000800020723      *         Chiamata XSOC
000900951009      *     *---------------------------------------------*          *
001000020723      *                                                              *
001100020423     D soc001        E DS                  EXTNAME(xsoc001ds)
001200020423     D xsocds          DS          1000
001300020723     D KPJBA         E DS
001400020723     C     *ENTRY        PLIST
001500020723     C                   PARM                    xscsocs           3
001600020723     C                   PARM                    ok                1
001700020723     c                   clear                   ok
001800020723     c                   clear                   xscsoc
001900980911     C*
002000020723     C*---------- RICERCA DITTA :
002100020723     C                   MOVEL     'SOC001'      TIPXSC
002200020723     C                   MOVEL     *BLANK        SOCXSC
002300980911     C                   CALLB     'XSOC'
002400980911     C                   PARM                    TIPXSC            6
002500980911     C                   PARM                    SOCXSC            3
002600980911     C                   PARM                    CDSXSC            9 0
002700020415     C                   PARM      *blanks       MODXSC            3
002800980911     C                   PARM      *blanks       RTNXSC            1
002900980911     C                   PARM                    XSOCDS
003000980911     C                   PARM                    KPJBA
003100020723     C     RTNXSC        IFNE      '1'
003200020723     C                   MOVEL     XSOCDS        SOC001
003300020723     c                   eval      xscsocs = xscsoc
003400020723     c                   else
003500020723     c                   eval      ok = '1'
003600020723     c                   end
003700020723     C                   SETON                                        LR
003800980911     C*
