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
006400111003     C* => G.A. OPERATIONS SPA
006500111003     C                   IF        CurrSito  = '09'   AND
006600140411     C                             CurrDepot = 'GAO'
006700140411     C                   IF        %subst(wNomeFile:1:7) = 'INBOUND'
006800090112     C                   EVAL      CurrOpz     = 'GO'
006900111003     C                   EVAL      CurrFileAS  = 'WP3GCDP'
007000110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007100140411     C                   EVAL      CurrTip     = 'EX'
007200090109     C                   ENDIF
007300111003     C                   IF        %subst(wNomeFile:1:3) = 'DBC'
007400100503     C                   EVAL      CurrOpz     = 'GO'
007500111003     C                   EVAL      CurrFileAS  = 'WP3GBCP'
007600110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007700140411     C                   EVAL      CurrTip     = 'CX'
007800100503     C                   ENDIF
007900111003     C                   IF        %subst(wNomeFile:1:3) = 'DCC'
008000090112     C                   EVAL      CurrOpz     = 'GO'
008100111003     C                   EVAL      CurrFileAS  = 'WP3GCCP'
008200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008300140411     C                   EVAL      CurrTip     = 'CX'
008400090112     C                   ENDIF
008500140429     C                   IF        %subst(wNomeFile:1:6) = 'MFMDBA'
008600140411     C                   EVAL      CurrOpz     = 'GO'
008700140411     C                   EVAL      CurrFileAS  = 'WP3GDTP'
008800140411     C                   EVAL      CurrPostPgm = 'LTCB400C'
008900140411     C                   EVAL      CurrTip     = 'P1'
009000140411     C                   ENDIF
009100090109     C                   ENDIF
009200090109     C*
009300090109     C                   ELSE
009400090112     C                   EVAL      CurrOpz     = 'GO'
009500090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009600090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
009700090109     C                   ENDIF
009800090109     C*
009900090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010000090109     C                   select
010100090109     C                   when      CurrOpz = *blanks
010200090109     C                   eval      wrkesito = '2'
010300090109     C                   when      CurrOpz = 'GO'
010400090109     C*
010500090109     C* OPERAZIONI INIZIALI
010600090109     C                   exsr      PREELA
010700000613     C*
010800090109     C* ELABORAZIONE
010900000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011000090109     C*
011100090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011200090129     C                   if        CurrPostPgm <> *blanks AND
011300090129     C                             CurrPostPgm <> '*NONE'
011400090129     C                   if        CurrPostLib = *blanks
011500090129     C                   if        CurrEnv = 'T'
011600111003     C                   eval      CurrPostLib = 'GEOV72SBT'
011700090129     C                   else
011800111003     C                   eval      CurrPostLib = 'GEOV72SB'
011900090129     C                   endif
012000090129     C                   endif
012100090121     C*
012200110428     C                   call      'TIS7P1C'
012300090129     C                   parm                    CurrPostLib
012400090129     C                   parm                    CurrPostPgm
012500090121     C                   parm                    CurrEnv
012600090121     C                   parm                    CurrSito
012700090121     C                   parm                    CurrDepot
012800090121     C                   parm                    CurrTip
012900090121     C                   parm                    wrkesito
013000090112     C                   endif
013100090121     C*
013200090121     C* OPERAZIONI FINALI
013300090121     C                   exsr      ENDELA
013400090112     C*
013500090109     C                   endsl
013600000616     C*
013700090109     C                   seton                                        lr
013800090109
013900090109
014000090109     C*--------------------------------------------------------
014100090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014200090109     C*--------------------------------------------------------
014300090109     C     PREELA        BEGSR
014400090109     C*
014500090109     C*
014600090109     C                   ENDSR
014700090109     C***
014800090109
014900090109
015000090109     C*--------------------------------------------------------
015100090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015200090109     C*--------------------------------------------------------
015300090109     C     ENDELA        BEGSR
015400090109     C*
015500090109     C*
015600090109     C                   ENDSR
015700090109     C***
015800090109
015900090109
016000000613
016100000724
016200910830     C*--------------------------------------------------------
016300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016400910830     C*--------------------------------------------------------
016500000724     C     RWFIL         BEGSR
016600990910     C*
016700990914     C                   if        not %open(tivin00r)
016800990908     C                   open      tivin00r
016900990914     C                   endif
017000040203     C                   if        not %open(wwrce00f)
017100040203     C                   open      wwrce00f
017200990914     C                   endif
017300990910     C*
017400990910     C                   clear                   §CTROK
017500990910     C                   clear                   §CTRNO
017600001009     C                   clear                   recko
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100000905     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000724     C                   if        *in70 = *off and
018400990913     C                             (vinflg = *blanks
018500990913     C                              or vinflg = '0'
018600990913     C                              or vinflg = '2')
018700990913     C*
018800990913     C                   clear                   vinmsg
018900000724     C                   if        vindta <> *blanks
019000000724     C                   movel(p)  vindta        passaggio
019100040203     C                   write(e)  WWRCE
019200990910     C*
019300000724     C                   if        not %error
019400000724     C                   add       1             §ctrok
019500990910     C                   eval      vinflg = '1'
019600990910     C                   else
019700000724     C                   add       1             §ctrno
019800000621     C                   eval      recko = vindta
019900990910     C                   eval      vinflg = '2'
020000990910     C                   endif
020100000724     C                   endif
020200991022     C                   endif
020300000905     C*
020400000905     C                   else
020500000905     C                   eval      vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800000905     C  N70              update    tivin000
020900991022     C*
021000991022     C  N70              ENDdo
021100990910
021200990910     C* Se non ci sono record con errori ...
021300000710     C                   if        §ctrno = 0
021400990910     C* ... restituisco esito OK.
021500990921     C                   eval      wrkesito = '0'
021600990910     C                   else
021700000710     C                   if        §ctrok > 0
021800990921     C                   eval      wrkesito = '1'
021900000710     C                   else
022000000710     C                   eval      wrkesito = '2'
022100990910     C                   endif
022200000710     C                   endif
022300990910     C*
022400990914     C                   if        %open(tivin00r)
022500990908     C                   close     tivin00r
022600990914     C                   endif
022700040203     C                   if        %open(wwrce00f)
022800040203     C                   close     wwrce00f
022900990914     C                   endif
023000990920     C*
023100000724     C                   if        wrkesito <> '2'
023200000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023300070913     C                   movel(p)  CMD(8)        depcmd
023400000724     C                   eval      depcmd = %trim(depcmd) + ' '+
023500000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
023600000724     C                   Z-ADD     100           LENGH            15 5
023700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023800000724     C                   PARM                    depcmd
023900000724     C                   PARM                    LENGH
024000000724     C*
024100070913     C                   movel(p)  CMD(9)        depcmd
024200000724     C                   eval      depcmd = %trim(depcmd)
024300000724     C                   Z-ADD     100           LENGH            15 5
024400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024500000724     C                   PARM                    depcmd
024600000724     C                   PARM                    LENGH
024700000724     C*
024800000724     C                   movel(p)  CMD(4)        depcmd
024900000724     C                   eval      depcmd = %trim(depcmd) +
025000090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025100090109     C                             %trim(CurrFileAS) +
025200070913     C                             %trim(CMD(6)) +
025300000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025400070913     C                             %trim(CMD(7))
025500000724     C                   Z-ADD     100           LENGH            15 5
025600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025700000724     C                   PARM                    depcmd
025800000724     C                   PARM                    LENGH
025900000724     C*
026000000724     C                   endif
026100000724     C*
026200910830     C                   ENDSR
026300000613     C***
026400990920
026500990910
026600000613     C     *inzsr        BEGSR
026700990910     C*
026800990910     C     *entry        plist
026900990920     C                   parm                    tivlrds
027000990921     C                   parm      wrkesito      esito
027100000724     C                   parm                    prmlit
027200000724     C                   parm                    prmfir
027300000613     C*
027400000613     C                   ENDSR
027500000613     C***
027600000725
027700000512** CMD - COMANDI CL
027800161207
027900161207
028000161207
028100040203CPYF FROMFILE(WWRCE00F) TOFILE(
028200070913/
028300070913) FROMMBR(
028400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
028500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
028600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
