000100110808      /TITLE Upload via FTP: traduzione file LED WP2LIV2P
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500990908
000600990920     D tivlrds       e ds                  extname(tivlr00f)
000700990910     D esito           s              1
000800000724     D prmlit          s             10
000900000710     D prmfir          s             10
001000990921     D wrkesito        s                   like(esito)
001100990908
001200110816
001300990921     C                   reset                   esito
001400990921     C                   reset                   wrkesito
001500000613     C*
001600090109     C* ELABORAZIONE
001700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
001800100623     C*
001900100623     C                   if        wrkesito = *blanks
002000100623     C                   eval      wrkesito = '0'
002100100623     C                   endif
002200000616     C*
002300090109     C                   seton                                        lr
002400000613
002500000724
002600910830     C*--------------------------------------------------------
002700110808     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WP2LIV2P     *
002800910830     C*--------------------------------------------------------
002900000724     C     RWFIL         BEGSR
003000990910     C*
003100990914     C                   if        not %open(tivin00r)
003200990908     C                   open      tivin00r
003300990914     C                   endif
003400990910     C*
003500921023     C                   DO        *HIVAL
003600990915     C                   READ      tivin00r                               70
003700000905     C                   eval      vinflg = '1'
003800000905     C  N70              update    tivin000
003900991022     C  N70              ENDdo
004000110816     C*
004100990921     C                   eval      wrkesito = '0'
004200990910     C*
004300990914     C                   if        %open(tivin00r)
004400990908     C                   close     tivin00r
004500990914     C                   endif
004600000724     C*
004700910830     C                   ENDSR
004800000613     C***
004900990920
005000990910
005100000613     C     *inzsr        BEGSR
005200990910     C*
005300990910     C     *entry        plist
005400990920     C                   parm                    tivlrds
005500990921     C                   parm      wrkesito      esito
005600000724     C                   parm                    prmlit
005700000724     C                   parm                    prmfir
005800000613     C*
005900000613     C                   ENDSR
006000000613     C***
006100000725
