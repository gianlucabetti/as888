000100110808      /TITLE Upload via FTP: traduzione file LED WP2LIV1P
000200130115     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110808     FWP2LIV1P  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800000512     D* COMANDI
000900000512     D*------------
001000090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001100000512     D*------------
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500110808     D passaggio     e ds                  extname(wp2liv1p)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100000724     D depcmd          s            100    INZ(*blanks)
002200000724     D §ctrok          s              5  0 INZ(*zeros)
002300000724     D §ctrno          s              5  0 INZ(*zeros)
002400090109     D*-------------------
002500090109     D* COSTANTI
002600090109     D*-------------------
002700090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002800090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
002900990908
003000000913     C                   reset                   rrnum
003100990921     C                   reset                   esito
003200990921     C                   reset                   wrkesito
003300090109     C*
003400090109     C* OPERAZIONI INIZIALI
003500090109     C                   exsr      PREELA
003600000613     C*
003700090109     C* ELABORAZIONE
003800000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
003900100623     C*
004000100623     C                   if        wrkesito = *blanks
004100100623     C                   eval      wrkesito = '0'
004200100623     C                   endif
004300090121     C*
004400090121     C* OPERAZIONI FINALI
004500090121     C                   exsr      ENDELA
004600000616     C*
004700090109     C                   seton                                        lr
004800090109
004900090109
005000090109     C*--------------------------------------------------------
005100090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
005200090109     C*--------------------------------------------------------
005300090109     C     PREELA        BEGSR
005400090109     C*
005500090109     C                   ENDSR
005600090109     C***
005700090109
005800090109
005900090109     C*--------------------------------------------------------
006000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
006100090109     C*--------------------------------------------------------
006200090109     C     ENDELA        BEGSR
006300090109     C*
006400090109     C                   ENDSR
006500090109     C***
006600090109
006700090109
006800000613
006900000724
007000910830     C*--------------------------------------------------------
007100110808     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WP2LIV1P     *
007200910830     C*--------------------------------------------------------
007300000724     C     RWFIL         BEGSR
007400990910     C*
007500990914     C                   if        not %open(tivin00r)
007600990908     C                   open      tivin00r
007700990914     C                   endif
007800110808     C                   if        not %open(wp2liv1p)
007900110808     C                   open      wp2liv1p
008000990914     C                   endif
008100990910     C*
008200990910     C                   clear                   §CTROK
008300990910     C                   clear                   §CTRNO
008400990910     C*
008500921023     C                   DO        *HIVAL
008600990913     C*
008700990915     C                   READ      tivin00r                               70
008800000905     C                   if        vindta > *blanks
008900000613     C                   add       1             rrnum
009000000724     C                   if        *in70 = *off and
009100990913     C                             (vinflg = *blanks
009200990913     C                              or vinflg = '0'
009300990913     C                              or vinflg = '2')
009400990913     C*
009500990913     C                   clear                   vinmsg
009600000724     C                   if        vindta <> *blanks
009700000724     C                   movel(p)  vindta        passaggio
009800110808     C                   write(e)  XX2LIV1
009900990910     C*
010000000724     C                   if        not %error
010100000724     C                   add       1             §ctrok
010200990910     C                   eval      vinflg = '1'
010300990910     C                   else
010400000724     C                   add       1             §ctrno
010500990910     C                   eval      vinflg = '2'
010600990910     C                   endif
010700000724     C                   endif
010800991022     C                   endif
010900000905     C*
011000000905     C                   else
011100000905     C                   eval      vinflg = '1'
011200000905     C                   endif
011300000905     C*
011400000905     C  N70              update    tivin000
011500991022     C*
011600991022     C  N70              ENDdo
011700990910
011800990910     C* Se non ci sono record con errori ...
011900000710     C                   if        §ctrno = 0
012000990910     C* ... restituisco esito OK.
012100990921     C                   eval      wrkesito = '0'
012200990910     C                   else
012300000710     C                   if        §ctrok > 0
012400990921     C                   eval      wrkesito = '1'
012500000710     C                   else
012600000710     C                   eval      wrkesito = '2'
012700990910     C                   endif
012800000710     C                   endif
012900990910     C*
013000990914     C                   if        %open(tivin00r)
013100990908     C                   close     tivin00r
013200990914     C                   endif
013300110808     C                   if        %open(wp2liv1p)
013400110808     C                   close     wp2liv1p
013500990914     C                   endif
013600990920     C*
013700000724     C                   if        wrkesito <> '2'
013800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
013900070913     C                   movel(p)  CMD(8)        depcmd
014000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
014100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
014200000724     C                   Z-ADD     100           LENGH            15 5
014300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000724     C                   PARM                    depcmd
014500000724     C                   PARM                    LENGH
014600000724     C*
014700070913     C                   movel(p)  CMD(9)        depcmd
014800000724     C                   eval      depcmd = %trim(depcmd)
014900000724     C                   Z-ADD     100           LENGH            15 5
015000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015100000724     C                   PARM                    depcmd
015200000724     C                   PARM                    LENGH
015300000724     C*
015400000724     C                   movel(p)  CMD(4)        depcmd
015500000724     C                   eval      depcmd = %trim(depcmd) +
015600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
015700110808     C                             %trim('XX2LIV1P') +
015800070913     C                             %trim(CMD(6)) +
015900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
016000070913     C                             %trim(CMD(7))
016100000724     C                   Z-ADD     100           LENGH            15 5
016200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016300000724     C                   PARM                    depcmd
016400000724     C                   PARM                    LENGH
016500000724     C*
016600000724     C                   endif
016700000724     C*
016800910830     C                   ENDSR
016900000613     C***
017000990920
017100990910
017200000613     C     *inzsr        BEGSR
017300990910     C*
017400990910     C     *entry        plist
017500990920     C                   parm                    tivlrds
017600990921     C                   parm      wrkesito      esito
017700000724     C                   parm                    prmlit
017800000724     C                   parm                    prmfir
017900000613     C*
018000000613     C                   ENDSR
018100000613     C***
018200000512** CMD - COMANDI CL
018300130115
018400130115
018500130115
018600110808CPYF FROMFILE(WP2LIV1P) TOFILE(
018700070913/
018800070913) FROMMBR(
018900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
019000110808DLCOBJ OBJ((WP2LIV1P *FILE *EXCL
019100110808DLTOVR FILE(WP2LIV1P) LVL(*JOB)
