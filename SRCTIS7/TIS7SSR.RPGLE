000100180111      /TITLE Upload via FTP: traduzione file WP3SBSP
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300150609
000400990910     Ftivin00r  uF   E             DISK    usropn
000500180111     FWP3SBSP   O    E             DISK    usropn RENAME(WP3SBSP:WP3SBS)
000600180111     F                                     prefix(F_)
000700180111
000800000512     D*------------
000900000512     D* COMANDI
001000000512     D*------------
001100090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001200000512     D*------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600180112     D passaggio     e ds                  extname(WP3SBSP) prefix(F_)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100990915     D wrkdata         s               d
002200990915     D wrkora          s               t
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400000621     D recko           s            150    INZ(*blanks)
002500000724     D depcmd          s            100    INZ(*blanks)
002600000724     D §ctrok          s              5  0 INZ(*zeros)
002700000724     D §ctrno          s              5  0 INZ(*zeros)
002800090109     D wNomeFile       s             30    INZ(*blanks)
002900090121     D CurrOpz         s              2    INZ
003000090121     D CurrFileAS      s             10    INZ
003100100408     D CurrPostPgm     s             10    INZ
003200090129     D CurrPostLib     s             10    INZ
003300090121     D CurrSito        s              2    INZ
003400090121     D CurrDepot       s              8    INZ
003500090121     D CurrEnv         s              1    INZ
003600090121     D CurrTip         s              2    INZ
003700090109     D*-------------------
003800090109     D* COSTANTI
003900090109     D*-------------------
004000090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004100090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004200990908
004300990915     C                   time                    wrkdata
004400990915     C                   time                    wrkora
004500000913     C                   reset                   rrnum
004600990921     C                   reset                   esito
004700990921     C                   reset                   wrkesito
004800090109     C*
004900090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005000090121     C                   EVAL      CurrOpz     = *blanks
005100090121     C                   EVAL      CurrFileAS  = *blanks
005200090129     C                   EVAL      CurrPostPgm = *blanks
005300090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005400100408     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
005500090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005600090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005700090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
005800090121     C*
005900090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006000090109     C                   MOVE(P)   vlrMSG        wNomeFile
006100090109     C* Porto il nome file sempre in maiuscolo
006200090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006300100408     C*
006400180111     C                   IF        %subst(wNomeFile:1:6) = 'XX3SBS'
006500090112     C                   EVAL      CurrOpz     = 'GO'
006600180111     C                   EVAL      CurrFileAS  = 'WP2SBSP'
006700180111     C                   EVAL      CurrTip     = 'SS'
006800090109     C                   ENDIF
006900100615     C*
007000100615     C* Personalizzazioni
007100100615     C                   SELECT
007200100615     C*
007300100615     C*
007400100615     C                   ENDSL
007500090109     C*
007600090109     C                   ELSE
007700090112     C                   EVAL      CurrOpz     = 'GO'
007800090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
007900100623     C                   EVAL      CurrTip     = vlrtip
008000090109     C                   ENDIF
008100090109     C*
008200090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
008300090109     C                   select
008400090109     C                   when      CurrOpz = *blanks
008500090109     C                   eval      wrkesito = '2'
008600090109     C                   when      CurrOpz = 'GO'
008700090109     C*
008800090109     C* OPERAZIONI INIZIALI
008900090109     C                   exsr      PREELA
009000000613     C*
009100090109     C* ELABORAZIONE
009200000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
009300090109     C*
009400090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
009500090129     C                   if        CurrPostPgm <> *blanks AND
009600090129     C                             CurrPostPgm <> '*NONE'
009700090129     C                   if        CurrPostLib = *blanks
009800090129     C                   if        CurrEnv = 'T'
009900150609     C                   eval      CurrPostLib = 'GEOV72SBT'
010000090129     C                   else
010100150609     C                   eval      CurrPostLib = 'GEOV72SB'
010200090129     C                   endif
010300090129     C                   endif
010400090121     C*
010500180111     C                   call      'TIS7SSC'
010600090129     C                   parm                    CurrPostLib
010700090129     C                   parm                    CurrPostPgm
010800090121     C                   parm                    CurrEnv
010900090121     C                   parm                    CurrSito
011000090121     C                   parm                    CurrDepot
011100090121     C                   parm                    CurrTip
011200090121     C                   parm                    wrkesito
011300100623     C*
011400100623     C                   if        wrkesito = *blanks
011500100623     C                   eval      wrkesito = '0'
011600100623     C                   endif
011700100623     C*
011800090112     C                   endif
011900090121     C*
012000090121     C* OPERAZIONI FINALI
012100090121     C                   exsr      ENDELA
012200090112     C*
012300090109     C                   endsl
012400000616     C*
012500090109     C                   seton                                        lr
012600090109
012700090109
012800090109     C*--------------------------------------------------------
012900090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013000090109     C*--------------------------------------------------------
013100090109     C     PREELA        BEGSR
013200090109     C*
013300090109     C*
013400090109     C                   ENDSR
013500090109     C***
013600090109
013700090109
013800090109     C*--------------------------------------------------------
013900090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
014000090109     C*--------------------------------------------------------
014100090109     C     ENDELA        BEGSR
014200090109     C*
014300090109     C*
014400090109     C                   ENDSR
014500090109     C***
014600090109
014700090109
014800000613
014900000724
015000910830     C*--------------------------------------------------------
015100180111     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WP3SBSP      *
015200910830     C*--------------------------------------------------------
015300000724     C     RWFIL         BEGSR
015400990910     C*
015500990914     C                   if        not %open(tivin00r)
015600990908     C                   open      tivin00r
015700990914     C                   endif
015800180111     C                   if        not %open(WP3SBSP)
015900180111     C                   open      WP3SBSP
016000990914     C                   endif
016100990910     C*
016200990910     C                   clear                   §CTROK
016300990910     C                   clear                   §CTRNO
016400001009     C                   clear                   recko
016500990910     C*
016600921023     C                   DO        *HIVAL
016700990913     C*
016800990915     C                   READ      tivin00r                               70
016900000905     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100000724     C                   if        *in70 = *off and
017200990913     C                             (vinflg = *blanks
017300990913     C                              or vinflg = '0'
017400990913     C                              or vinflg = '2')
017500990913     C*
017600990913     C                   clear                   vinmsg
017700000724     C                   if        vindta <> *blanks
017800000724     C                   movel(p)  vindta        passaggio
017900180111     C                   write(e)  WP3SBS
018000990910     C*
018100000724     C                   if        not %error
018200000724     C                   add       1             §ctrok
018300990910     C                   eval      vinflg = '1'
018400990910     C                   else
018500000724     C                   add       1             §ctrno
018600000621     C                   eval      recko = vindta
018700990910     C                   eval      vinflg = '2'
018800990910     C                   endif
018900000724     C                   endif
019000991022     C                   endif
019100000905     C*
019200000905     C                   else
019300000905     C                   eval      vinflg = '1'
019400000905     C                   endif
019500000905     C*
019600000905     C  N70              update    tivin000
019700991022     C*
019800991022     C  N70              ENDdo
019900990910
020000990910     C* Se non ci sono record con errori ...
020100000710     C                   if        §ctrno = 0
020200990910     C* ... restituisco esito OK.
020300990921     C                   eval      wrkesito = '0'
020400990910     C                   else
020500000710     C                   if        §ctrok > 0
020600990921     C                   eval      wrkesito = '1'
020700000710     C                   else
020800000710     C                   eval      wrkesito = '2'
020900990910     C                   endif
021000000710     C                   endif
021100990910     C*
021200990914     C                   if        %open(tivin00r)
021300990908     C                   close     tivin00r
021400990914     C                   endif
021500180111     C                   if        %open(WP3SBSP)
021600180111     C                   close     WP3SBSP
021700990914     C                   endif
021800990920     C*
021900000724     C                   if        wrkesito <> '2'
022000000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
022100070913     C                   movel(p)  CMD(8)        depcmd
022200000724     C                   eval      depcmd = %trim(depcmd) + ' '+
022300000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
022400000724     C                   Z-ADD     100           LENGH            15 5
022500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022600000724     C                   PARM                    depcmd
022700000724     C                   PARM                    LENGH
022800000724     C*
022900070913     C                   movel(p)  CMD(9)        depcmd
023000000724     C                   eval      depcmd = %trim(depcmd)
023100000724     C                   Z-ADD     100           LENGH            15 5
023200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023300000724     C                   PARM                    depcmd
023400000724     C                   PARM                    LENGH
023500000724     C*
023600000724     C                   movel(p)  CMD(4)        depcmd
023700000724     C                   eval      depcmd = %trim(depcmd) +
023800090109     C                             %trim(prmlit) + %trim(CMD(5)) +
023900090109     C                             %trim(CurrFileAS) +
024000070913     C                             %trim(CMD(6)) +
024100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
024200070913     C                             %trim(CMD(7))
024300000724     C                   Z-ADD     100           LENGH            15 5
024400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024500000724     C                   PARM                    depcmd
024600000724     C                   PARM                    LENGH
024700000724     C*
024800000724     C                   endif
024900000724     C*
025000910830     C                   ENDSR
025100000613     C***
025200000725
025300000725
025400000613     C     *inzsr        BEGSR
025500990910     C*
025600990910     C     *entry        plist
025700990920     C                   parm                    tivlrds
025800990921     C                   parm      wrkesito      esito
025900000724     C                   parm                    prmlit
026000000724     C                   parm                    prmfir
026100000613     C*
026200000613     C                   ENDSR
026300000613     C***
026400000512** CMD - COMANDI CL
026500161207
026600161207
026700161207
026800180111CPYF FROMFILE(WP3SBSP)  TOFILE(
026900070913/
027000070913) FROMMBR(
027100070913) MBROPT(*ADD) FMTOPT(*NOCHK)
027200180111DLCOBJ OBJ((WP3SBSP  *FILE *EXCL
027300180111DLTOVR FILE(WP3SBSP)  LVL(*JOB)
