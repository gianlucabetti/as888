000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
000300990908
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
006400140220     C* => SWISS EYEWEAR GROUP
006500150608     C                   IF        CurrSito  = '01'   AND
006600140220     C                             CurrDepot = 'SWISSEG'
006700121228     C*
006800140220     C                   IF        %subst(wNomeFile:1:2) = 'IT'
006900120727     C                   EVAL      CurrOpz     = 'GO'
007000140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007100120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
007200140220     C                   EVAL      CurrTip     = 'AX'
007300120727     C                   ENDIF
007400140220     C*
007500140220     C                   IF        %subst(wNomeFile:1:2) = 'SO'
007600140220     C                   EVAL      CurrOpz     = 'GO'
007700140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007800140220     C                   EVAL      CurrPostPgm = 'LTCB400C'
007900140220     C                   EVAL      CurrTip     = 'CX'
008000140220     C                   ENDIF
008100140220     C*
008200140220     C                   IF        %subst(wNomeFile:1:2) = 'PO'
008300140220     C                   EVAL      CurrOpz     = 'GO'
008400140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
008500140220     C                   EVAL      CurrPostPgm = 'LTCB400C'
008600140220     C                   EVAL      CurrTip     = 'EX'
008700140220     C                   ENDIF
008800120727     C*
008900110427     C                   ENDIF
009000090109     C*
009100090109     C                   ELSE
009200090112     C                   EVAL      CurrOpz     = 'GO'
009300090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009400090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
009500090109     C                   ENDIF
009600090109     C*
009700090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
009800090109     C                   select
009900090109     C                   when      CurrOpz = *blanks
010000090109     C                   eval      wrkesito = '2'
010100090109     C                   when      CurrOpz = 'GO'
010200090109     C*
010300090109     C* OPERAZIONI INIZIALI
010400090109     C                   exsr      PREELA
010500000613     C*
010600090109     C* ELABORAZIONE
010700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010800090109     C*
010900090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011000090129     C                   if        CurrPostPgm <> *blanks AND
011100090129     C                             CurrPostPgm <> '*NONE'
011200090129     C                   if        CurrPostLib = *blanks
011300090129     C                   if        CurrEnv = 'T'
011400150608     C                   eval      CurrPostLib = 'GEOV72SBT'
011500090129     C                   else
011600150608     C                   eval      CurrPostLib = 'GEOV72SB'
011700090129     C                   endif
011800090129     C                   endif
011900090121     C*
012000110427     C                   call      'TIS7P1C'
012100090129     C                   parm                    CurrPostLib
012200090129     C                   parm                    CurrPostPgm
012300090121     C                   parm                    CurrEnv
012400090121     C                   parm                    CurrSito
012500090121     C                   parm                    CurrDepot
012600090121     C                   parm                    CurrTip
012700090121     C                   parm                    wrkesito
012800090112     C                   endif
012900090121     C*
013000090121     C* OPERAZIONI FINALI
013100090121     C                   exsr      ENDELA
013200090112     C*
013300090109     C                   endsl
013400000616     C*
013500090109     C                   seton                                        lr
013600090109
013700090109
013800090109     C*--------------------------------------------------------
013900090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014000090109     C*--------------------------------------------------------
014100090109     C     PREELA        BEGSR
014200090109     C*
014300090109     C*
014400090109     C                   ENDSR
014500090109     C***
014600090109
014700090109
014800090109     C*--------------------------------------------------------
014900090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015000090109     C*--------------------------------------------------------
015100090109     C     ENDELA        BEGSR
015200090109     C*
015300090109     C*
015400090109     C                   ENDSR
015500090109     C***
015600090109
015700090109
015800000613
015900000724
016000910830     C*--------------------------------------------------------
016100040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016200910830     C*--------------------------------------------------------
016300000724     C     RWFIL         BEGSR
016400990910     C*
016500990914     C                   if        not %open(tivin00r)
016600990908     C                   open      tivin00r
016700990914     C                   endif
016800040203     C                   if        not %open(wwrce00f)
016900040203     C                   open      wwrce00f
017000990914     C                   endif
017100990910     C*
017200990910     C                   clear                   §CTROK
017300990910     C                   clear                   §CTRNO
017400001009     C                   clear                   recko
017500990910     C*
017600921023     C                   DO        *HIVAL
017700990913     C*
017800990915     C                   READ      tivin00r                               70
017900000905     C                   if        vindta > *blanks
018000000613     C                   add       1             rrnum
018100000724     C                   if        *in70 = *off and
018200990913     C                             (vinflg = *blanks
018300990913     C                              or vinflg = '0'
018400990913     C                              or vinflg = '2')
018500990913     C*
018600990913     C                   clear                   vinmsg
018700000724     C                   if        vindta <> *blanks
018800000724     C                   movel(p)  vindta        passaggio
018900040203     C                   write(e)  WWRCE
019000990910     C*
019100000724     C                   if        not %error
019200000724     C                   add       1             §ctrok
019300990910     C                   eval      vinflg = '1'
019400990910     C                   else
019500000724     C                   add       1             §ctrno
019600000621     C                   eval      recko = vindta
019700990910     C                   eval      vinflg = '2'
019800990910     C                   endif
019900000724     C                   endif
020000991022     C                   endif
020100000905     C*
020200000905     C                   else
020300000905     C                   eval      vinflg = '1'
020400000905     C                   endif
020500000905     C*
020600000905     C  N70              update    tivin000
020700991022     C*
020800991022     C  N70              ENDdo
020900990910
021000990910     C* Se non ci sono record con errori ...
021100000710     C                   if        §ctrno = 0
021200990910     C* ... restituisco esito OK.
021300990921     C                   eval      wrkesito = '0'
021400990910     C                   else
021500000710     C                   if        §ctrok > 0
021600990921     C                   eval      wrkesito = '1'
021700000710     C                   else
021800000710     C                   eval      wrkesito = '2'
021900990910     C                   endif
022000000710     C                   endif
022100990910     C*
022200990914     C                   if        %open(tivin00r)
022300990908     C                   close     tivin00r
022400990914     C                   endif
022500040203     C                   if        %open(wwrce00f)
022600040203     C                   close     wwrce00f
022700990914     C                   endif
022800990920     C*
022900000724     C                   if        wrkesito <> '2'
023000000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023100070913     C                   movel(p)  CMD(8)        depcmd
023200000724     C                   eval      depcmd = %trim(depcmd) + ' '+
023300000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
023400000724     C                   Z-ADD     100           LENGH            15 5
023500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023600000724     C                   PARM                    depcmd
023700000724     C                   PARM                    LENGH
023800000724     C*
023900070913     C                   movel(p)  CMD(9)        depcmd
024000000724     C                   eval      depcmd = %trim(depcmd)
024100000724     C                   Z-ADD     100           LENGH            15 5
024200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024300000724     C                   PARM                    depcmd
024400000724     C                   PARM                    LENGH
024500000724     C*
024600000724     C                   movel(p)  CMD(4)        depcmd
024700000724     C                   eval      depcmd = %trim(depcmd) +
024800090109     C                             %trim(prmlit) + %trim(CMD(5)) +
024900090109     C                             %trim(CurrFileAS) +
025000070913     C                             %trim(CMD(6)) +
025100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025200070913     C                             %trim(CMD(7))
025300000724     C                   Z-ADD     100           LENGH            15 5
025400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025500000724     C                   PARM                    depcmd
025600000724     C                   PARM                    LENGH
025700000724     C*
025800000724     C                   endif
025900000724     C*
026000910830     C                   ENDSR
026100000613     C***
026200000725
026300000725
026400000613     C     *inzsr        BEGSR
026500990910     C*
026600990910     C     *entry        plist
026700990920     C                   parm                    tivlrds
026800990921     C                   parm      wrkesito      esito
026900000724     C                   parm                    prmlit
027000000724     C                   parm                    prmfir
027100000613     C*
027200000613     C                   ENDSR
027300000613     C***
027400000725
027500000512** CMD - COMANDI CL
027600161207
027700161207
027800161207
027900040203CPYF FROMFILE(WWRCE00F) TOFILE(
028000070913/
028100070913) FROMMBR(
028200070913) MBROPT(*ADD) FMTOPT(*NOCHK)
028300040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
028400040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
