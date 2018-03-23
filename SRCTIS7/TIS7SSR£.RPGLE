000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300150609
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
003000100408     D CurrPostPgm     s             10    INZ
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
005300100408     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
005400090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005500090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005600090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
005700090121     C*
005800090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
005900090109     C                   MOVE(P)   vlrMSG        wNomeFile
006000090109     C* Porto il nome file sempre in maiuscolo
006100090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006200100408     C*
006300100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL1' OR
006400100408     C                             %subst(wNomeFile:1:3) = 'OL1'
006500090112     C                   EVAL      CurrOpz     = 'GO'
006600100408     C                   EVAL      CurrFileAS  = 'WP3OL1P'
006700100408     C                   EVAL      CurrTip     = 'C1'
006800090109     C                   ENDIF
006900100408     C*
007000100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL2' OR
007100100408     C                             %subst(wNomeFile:1:3) = 'OL2'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300100408     C                   EVAL      CurrFileAS  = 'WP3OL2P'
007400100408     C                   EVAL      CurrTip     = 'C2'
007500090109     C                   ENDIF
007600100408     C*
007700100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL3' OR
007800100408     C                             %subst(wNomeFile:1:3) = 'OL3'
007900090112     C                   EVAL      CurrOpz     = 'GO'
008000100408     C                   EVAL      CurrFileAS  = 'WP3OL3P'
008100100408     C                   EVAL      CurrTip     = 'C3'
008200090112     C                   ENDIF
008300100408     C*
008400100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL4' OR
008500100408     C                             %subst(wNomeFile:1:3) = 'OL4'
008600090112     C                   EVAL      CurrOpz     = 'GO'
008700100408     C                   EVAL      CurrFileAS  = 'WP3OL4P'
008800100408     C                   EVAL      CurrTip     = 'C4'
008900090109     C                   ENDIF
009000100408     C*
009100100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL5' OR
009200100408     C                             %subst(wNomeFile:1:3) = 'OL5'
009300100408     C                   EVAL      CurrOpz     = 'GO'
009400100408     C                   EVAL      CurrFileAS  = 'WP3OL5P'
009500100408     C                   EVAL      CurrTip     = 'C5'
009600100408     C                   ENDIF
009700100524     C*
009800100524     C                   IF        %subst(wNomeFile:1:6) = 'XX3OLX' OR
009900100524     C                             %subst(wNomeFile:1:3) = 'OLX'
010000100524     C                   EVAL      CurrOpz     = 'GO'
010100100524     C                   EVAL      CurrFileAS  = 'WP3OLXP'
010200100524     C                   EVAL      CurrTip     = 'CX'
010300100524     C                   ENDIF
010400100615     C*
010500100615     C* Personalizzazioni
010600100615     C                   SELECT
010700100615     C*
010800100615     C* - CAMILLATEX - 16 CAMILLAT
010900100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
011000100615     C*
011100100615     C                   IF        %subst(wNomeFile:1:3) = 'AOU'
011200100615     C                   EVAL      CurrOpz     = 'GO'
011300100615     C                   EVAL      CurrFileAS  = 'WP3AOUP'
011400100615     C                   EVAL      CurrTip     = 'CX'
011500100615     C                   ENDIF
011600100615     C*
011700100615     C                   ENDSL
011800090109     C*
011900090109     C                   ELSE
012000090112     C                   EVAL      CurrOpz     = 'GO'
012100090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
012200100623     C                   EVAL      CurrTip     = vlrtip
012300090109     C                   ENDIF
012400090109     C*
012500090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
012600090109     C                   select
012700090109     C                   when      CurrOpz = *blanks
012800090109     C                   eval      wrkesito = '2'
012900090109     C                   when      CurrOpz = 'GO'
013000090109     C*
013100090109     C* OPERAZIONI INIZIALI
013200090109     C                   exsr      PREELA
013300000613     C*
013400090109     C* ELABORAZIONE
013500000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
013600090109     C*
013700090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
013800090129     C                   if        CurrPostPgm <> *blanks AND
013900090129     C                             CurrPostPgm <> '*NONE'
014000090129     C                   if        CurrPostLib = *blanks
014100090129     C                   if        CurrEnv = 'T'
014200150609     C                   eval      CurrPostLib = 'GEOV72SBT'
014300090129     C                   else
014400150609     C                   eval      CurrPostLib = 'GEOV72SB'
014500090129     C                   endif
014600090129     C                   endif
014700090121     C*
014800100408     C                   call      'TIS7CXC'
014900090129     C                   parm                    CurrPostLib
015000090129     C                   parm                    CurrPostPgm
015100090121     C                   parm                    CurrEnv
015200090121     C                   parm                    CurrSito
015300090121     C                   parm                    CurrDepot
015400090121     C                   parm                    CurrTip
015500090121     C                   parm                    wrkesito
015600100623     C*
015700100623     C                   if        wrkesito = *blanks
015800100623     C                   eval      wrkesito = '0'
015900100623     C                   endif
016000100623     C*
016100090112     C                   endif
016200090121     C*
016300090121     C* OPERAZIONI FINALI
016400090121     C                   exsr      ENDELA
016500090112     C*
016600090109     C                   endsl
016700000616     C*
016800090109     C                   seton                                        lr
016900090109
017000090109
017100090109     C*--------------------------------------------------------
017200090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
017300090109     C*--------------------------------------------------------
017400090109     C     PREELA        BEGSR
017500090109     C*
017600090109     C*
017700090109     C                   ENDSR
017800090109     C***
017900090109
018000090109
018100090109     C*--------------------------------------------------------
018200090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
018300090109     C*--------------------------------------------------------
018400090109     C     ENDELA        BEGSR
018500090109     C*
018600090109     C*
018700090109     C                   ENDSR
018800090109     C***
018900090109
019000090109
019100000613
019200000724
019300910830     C*--------------------------------------------------------
019400040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
019500910830     C*--------------------------------------------------------
019600000724     C     RWFIL         BEGSR
019700990910     C*
019800990914     C                   if        not %open(tivin00r)
019900990908     C                   open      tivin00r
020000990914     C                   endif
020100040203     C                   if        not %open(wwrce00f)
020200040203     C                   open      wwrce00f
020300990914     C                   endif
020400990910     C*
020500990910     C                   clear                   §CTROK
020600990910     C                   clear                   §CTRNO
020700001009     C                   clear                   recko
020800990910     C*
020900921023     C                   DO        *HIVAL
021000990913     C*
021100990915     C                   READ      tivin00r                               70
021200000905     C                   if        vindta > *blanks
021300000613     C                   add       1             rrnum
021400000724     C                   if        *in70 = *off and
021500990913     C                             (vinflg = *blanks
021600990913     C                              or vinflg = '0'
021700990913     C                              or vinflg = '2')
021800990913     C*
021900990913     C                   clear                   vinmsg
022000000724     C                   if        vindta <> *blanks
022100000724     C                   movel(p)  vindta        passaggio
022200040203     C                   write(e)  WWRCE
022300990910     C*
022400000724     C                   if        not %error
022500000724     C                   add       1             §ctrok
022600990910     C                   eval      vinflg = '1'
022700990910     C                   else
022800000724     C                   add       1             §ctrno
022900000621     C                   eval      recko = vindta
023000990910     C                   eval      vinflg = '2'
023100990910     C                   endif
023200000724     C                   endif
023300991022     C                   endif
023400000905     C*
023500000905     C                   else
023600000905     C                   eval      vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900000905     C  N70              update    tivin000
024000991022     C*
024100991022     C  N70              ENDdo
024200990910
024300990910     C* Se non ci sono record con errori ...
024400000710     C                   if        §ctrno = 0
024500990910     C* ... restituisco esito OK.
024600990921     C                   eval      wrkesito = '0'
024700990910     C                   else
024800000710     C                   if        §ctrok > 0
024900990921     C                   eval      wrkesito = '1'
025000000710     C                   else
025100000710     C                   eval      wrkesito = '2'
025200990910     C                   endif
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800040203     C                   if        %open(wwrce00f)
025900040203     C                   close     wwrce00f
026000990914     C                   endif
026100990920     C*
026200000724     C                   if        wrkesito <> '2'
026300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
026400070913     C                   movel(p)  CMD(8)        depcmd
026500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
026600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
026700000724     C                   Z-ADD     100           LENGH            15 5
026800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026900000724     C                   PARM                    depcmd
027000000724     C                   PARM                    LENGH
027100000724     C*
027200070913     C                   movel(p)  CMD(9)        depcmd
027300000724     C                   eval      depcmd = %trim(depcmd)
027400000724     C                   Z-ADD     100           LENGH            15 5
027500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027600000724     C                   PARM                    depcmd
027700000724     C                   PARM                    LENGH
027800000724     C*
027900000724     C                   movel(p)  CMD(4)        depcmd
028000000724     C                   eval      depcmd = %trim(depcmd) +
028100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028200090109     C                             %trim(CurrFileAS) +
028300070913     C                             %trim(CMD(6)) +
028400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
028500070913     C                             %trim(CMD(7))
028600000724     C                   Z-ADD     100           LENGH            15 5
028700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028800000724     C                   PARM                    depcmd
028900000724     C                   PARM                    LENGH
029000000724     C*
029100000724     C                   endif
029200000724     C*
029300910830     C                   ENDSR
029400000613     C***
029500000725
029600000725
029700000613     C     *inzsr        BEGSR
029800990910     C*
029900990910     C     *entry        plist
030000990920     C                   parm                    tivlrds
030100990921     C                   parm      wrkesito      esito
030200000724     C                   parm                    prmlit
030300000724     C                   parm                    prmfir
030400000613     C*
030500000613     C                   ENDSR
030600000613     C***
030700000512** CMD - COMANDI CL
030800161207
030900161207
031000161207
031100040203CPYF FROMFILE(WWRCE00F) TOFILE(
031200070913/
031300070913) FROMMBR(
031400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
031500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
031600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
