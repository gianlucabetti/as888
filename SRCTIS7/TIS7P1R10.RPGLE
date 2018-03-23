000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
000300150610
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040203     FWWRCE00F  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800000512     D* COMANDI
000900000512     D*------------
001000090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001100000512     D*------------
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500040203     D passaggio     e ds                  extname(wwrce00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000990915     D wrkdata         s               d
002100990915     D wrkora          s               t
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300000621     D recko           s            150    INZ(*blanks)
002400000724     D depcmd          s            100    INZ(*blanks)
002500000724     D §ctrok          s              5  0 INZ(*zeros)
002600000724     D §ctrno          s              5  0 INZ(*zeros)
002700090109     D wNomeFile       s             30    INZ(*blanks)
002800090121     D CurrOpz         s              2    INZ
002900090121     D CurrFileAS      s             10    INZ
003000090129     D CurrPostPgm     s             10    INZ
003100090129     D CurrPostLib     s             10    INZ
003200090121     D CurrSito        s              2    INZ
003300090121     D CurrDepot       s              8    INZ
003400090121     D CurrEnv         s              1    INZ
003500090121     D CurrTip         s              2    INZ
003600090109     D*-------------------
003700090109     D* COSTANTI
003800090109     D*-------------------
003900090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004000090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004100990908
004200990915     C                   time                    wrkdata
004300990915     C                   time                    wrkora
004400000913     C                   reset                   rrnum
004500990921     C                   reset                   esito
004600990921     C                   reset                   wrkesito
004700090109     C*
004800090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
004900090121     C                   EVAL      CurrOpz     = *blanks
005000090121     C                   EVAL      CurrFileAS  = *blanks
005100090129     C                   EVAL      CurrPostPgm = *blanks
005200090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005300090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005400090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005500090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
005600090121     C*
005700090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
005800090109     C                   MOVE(P)   vlrMSG        wNomeFile
005900090109     C* Porto il nome file sempre in maiuscolo
006000090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006100090109     C*
006200090109     C* ...verifico particolarità cliente...
006300090109     C*
006400110504     C* => DEC
006500110617     C                   IF        CurrSito  = '14'   AND
006600110617     C                             CurrDepot = 'FERRINO'
006700110617     C                   IF        %subst(wNomeFile:1:6) = 'XX3TES'
006800090112     C                   EVAL      CurrOpz     = 'GO'
006900110617     C                   EVAL      CurrFileAS  = 'WP3FRTP'
007000110621     C***                EVAL      CurrPostPgm = 'LTCB400C'
007100110617     C                   EVAL      CurrTip     = 'LU'
007200110617     C                   EVAL      CurrEnv     = 'P'
007300090109     C                   ENDIF
007400110617     C                   IF        %subst(wNomeFile:1:6) = 'XX3DET'
007500110617     C                   EVAL      CurrOpz     = 'GO'
007600110617     C                   EVAL      CurrFileAS  = 'WP3FRDP'
007700110621     C***                EVAL      CurrPostPgm = 'LTCB400C'
007800110617     C                   EVAL      CurrTip     = 'LU'
007900110617     C                   EVAL      CurrEnv     = 'P'
008000110617     C                   ENDIF
008100110617     C                   IF        %subst(wNomeFile:1:7) = 'TXX3TES'
008200110617     C                   EVAL      CurrOpz     = 'GO'
008300110617     C                   EVAL      CurrFileAS  = 'WP3FRTP'
008400110621     C***                EVAL      CurrPostPgm = 'LTCB400C'
008500110617     C                   EVAL      CurrTip     = 'LU'
008600110617     C                   EVAL      CurrEnv     = 'T'
008700110617     C                   ENDIF
008800110617     C                   IF        %subst(wNomeFile:1:7) = 'TXX3DET'
008900110617     C                   EVAL      CurrOpz     = 'GO'
009000110617     C                   EVAL      CurrFileAS  = 'WP3FRDP'
009100110621     C***                EVAL      CurrPostPgm = 'LTCB400C'
009200110617     C                   EVAL      CurrTip     = 'LU'
009300110617     C                   EVAL      CurrEnv     = 'T'
009400110617     C                   ENDIF
009500110617     C*
009600110427     C                   ENDIF
009700090109     C*
009800090109     C                   ELSE
009900090112     C                   EVAL      CurrOpz     = 'GO'
010000090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010100090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
010200090109     C                   ENDIF
010300090109     C*
010400090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010500090109     C                   select
010600090109     C                   when      CurrOpz = *blanks
010700090109     C                   eval      wrkesito = '2'
010800090109     C                   when      CurrOpz = 'GO'
010900090109     C*
011000090109     C* OPERAZIONI INIZIALI
011100090109     C                   exsr      PREELA
011200000613     C*
011300090109     C* ELABORAZIONE
011400000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011500090109     C*
011600090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011700090129     C                   if        CurrPostPgm <> *blanks AND
011800090129     C                             CurrPostPgm <> '*NONE'
011900090129     C                   if        CurrPostLib = *blanks
012000090129     C                   if        CurrEnv = 'T'
012100150609     C                   eval      CurrPostLib = 'GEOV72SBT'
012200090129     C                   else
012300150609     C                   eval      CurrPostLib = 'GEOV72SB'
012400090129     C                   endif
012500090129     C                   endif
012600090121     C*
012700110427     C                   call      'TIS7P1C'
012800090129     C                   parm                    CurrPostLib
012900090129     C                   parm                    CurrPostPgm
013000090121     C                   parm                    CurrEnv
013100090121     C                   parm                    CurrSito
013200090121     C                   parm                    CurrDepot
013300090121     C                   parm                    CurrTip
013400090121     C                   parm                    wrkesito
013500090112     C                   endif
013600090121     C*
013700090121     C* OPERAZIONI FINALI
013800090121     C                   exsr      ENDELA
013900090112     C*
014000090109     C                   endsl
014100000616     C*
014200090109     C                   seton                                        lr
014300090109
014400090109
014500090109     C*--------------------------------------------------------
014600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014700090109     C*--------------------------------------------------------
014800090109     C     PREELA        BEGSR
014900090109     C*
015000090109     C*
015100090109     C                   ENDSR
015200090109     C***
015300090109
015400090109
015500090109     C*--------------------------------------------------------
015600090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015700090109     C*--------------------------------------------------------
015800090109     C     ENDELA        BEGSR
015900090109     C*
016000090109     C*
016100090109     C                   ENDSR
016200090109     C***
016300090109
016400090109
016500000613
016600000724
016700910830     C*--------------------------------------------------------
016800040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016900910830     C*--------------------------------------------------------
017000000724     C     RWFIL         BEGSR
017100990910     C*
017200990914     C                   if        not %open(tivin00r)
017300990908     C                   open      tivin00r
017400990914     C                   endif
017500040203     C                   if        not %open(wwrce00f)
017600040203     C                   open      wwrce00f
017700990914     C                   endif
017800990910     C*
017900990910     C                   clear                   §CTROK
018000990910     C                   clear                   §CTRNO
018100001009     C                   clear                   recko
018200990910     C*
018300921023     C                   DO        *HIVAL
018400990913     C*
018500990915     C                   READ      tivin00r                               70
018600000905     C                   if        vindta > *blanks
018700000613     C                   add       1             rrnum
018800000724     C                   if        *in70 = *off and
018900990913     C                             (vinflg = *blanks
019000990913     C                              or vinflg = '0'
019100990913     C                              or vinflg = '2')
019200990913     C*
019300990913     C                   clear                   vinmsg
019400000724     C                   if        vindta <> *blanks
019500000724     C                   movel(p)  vindta        passaggio
019600040203     C                   write(e)  WWRCE
019700990910     C*
019800000724     C                   if        not %error
019900000724     C                   add       1             §ctrok
020000990910     C                   eval      vinflg = '1'
020100990910     C                   else
020200000724     C                   add       1             §ctrno
020300000621     C                   eval      recko = vindta
020400990910     C                   eval      vinflg = '2'
020500990910     C                   endif
020600000724     C                   endif
020700991022     C                   endif
020800000905     C*
020900000905     C                   else
021000000905     C                   eval      vinflg = '1'
021100000905     C                   endif
021200000905     C*
021300000905     C  N70              update    tivin000
021400991022     C*
021500991022     C  N70              ENDdo
021600990910
021700990910     C* Se non ci sono record con errori ...
021800000710     C                   if        §ctrno = 0
021900990910     C* ... restituisco esito OK.
022000990921     C                   eval      wrkesito = '0'
022100990910     C                   else
022200000710     C                   if        §ctrok > 0
022300990921     C                   eval      wrkesito = '1'
022400000710     C                   else
022500000710     C                   eval      wrkesito = '2'
022600990910     C                   endif
022700000710     C                   endif
022800990910     C*
022900990914     C                   if        %open(tivin00r)
023000990908     C                   close     tivin00r
023100990914     C                   endif
023200040203     C                   if        %open(wwrce00f)
023300040203     C                   close     wwrce00f
023400990914     C                   endif
023500990920     C*
023600000724     C                   if        wrkesito <> '2'
023700000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023800070913     C                   movel(p)  CMD(8)        depcmd
023900000724     C                   eval      depcmd = %trim(depcmd) + ' '+
024000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
024100000724     C                   Z-ADD     100           LENGH            15 5
024200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024300000724     C                   PARM                    depcmd
024400000724     C                   PARM                    LENGH
024500000724     C*
024600070913     C                   movel(p)  CMD(9)        depcmd
024700000724     C                   eval      depcmd = %trim(depcmd)
024800000724     C                   Z-ADD     100           LENGH            15 5
024900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025000000724     C                   PARM                    depcmd
025100000724     C                   PARM                    LENGH
025200000724     C*
025300000724     C                   movel(p)  CMD(4)        depcmd
025400000724     C                   eval      depcmd = %trim(depcmd) +
025500090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025600090109     C                             %trim(CurrFileAS) +
025700070913     C                             %trim(CMD(6)) +
025800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025900070913     C                             %trim(CMD(7))
026000000724     C                   Z-ADD     100           LENGH            15 5
026100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026200000724     C                   PARM                    depcmd
026300000724     C                   PARM                    LENGH
026400000724     C*
026500000724     C                   endif
026600000724     C*
026700910830     C                   ENDSR
026800000613     C***
026900000725
027000000725
027100000613     C     *inzsr        BEGSR
027200990910     C*
027300990910     C     *entry        plist
027400990920     C                   parm                    tivlrds
027500990921     C                   parm      wrkesito      esito
027600000724     C                   parm                    prmlit
027700000724     C                   parm                    prmfir
027800000613     C*
027900000613     C                   ENDSR
028000000613     C***
028100000725
028200000512** CMD - COMANDI CL
028300161207
028400161207
028500161207
028600040203CPYF FROMFILE(WWRCE00F) TOFILE(
028700070913/
028800070913) FROMMBR(
028900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
029000040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
029100040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
