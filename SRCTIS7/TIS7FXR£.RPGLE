000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
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
006300110105     C                   IF        %subst(wNomeFile:1:6) = 'XX3OF5'
006400090112     C                   EVAL      CurrOpz     = 'GO'
006500110105     C                   EVAL      CurrFileAS  = 'WP3OF5P'
006600110105     C                   EVAL      CurrTip     = 'FK'
006700090109     C                   ENDIF
006800110105     C*
006900110105     C                   IF        %subst(wNomeFile:1:6) = 'XX3OF1'
007000110105     C                   EVAL      CurrOpz     = 'GO'
007100110105     C                   EVAL      CurrFileAS  = 'WP3OF1P'
007200110105     C                   EVAL      CurrTip     = 'FO'
007300110105     C                   ENDIF
007400100615     C*
007500100615     C* Personalizzazioni
007600100615     C                   SELECT
007700100615     C*
007800110105     C* - XXXXXXXXXX - 00 XXXXXXXX
007900110105     C                   WHEN      %trim(CurrSito+CurrDepot) = '00XXXXXXXX'
008000100615     C*
008100110105     C                   IF        %subst(wNomeFile:1:3) = '***'
008200100615     C                   EVAL      CurrOpz     = 'GO'
008300110105     C                   EVAL      CurrFileAS  = '*******'
008400100615     C                   EVAL      CurrTip     = 'CX'
008500100615     C                   ENDIF
008600100615     C*
008700100615     C                   ENDSL
008800090109     C*
008900090109     C                   ELSE
009000090112     C                   EVAL      CurrOpz     = 'GO'
009100090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009200100623     C                   EVAL      CurrTip     = vlrtip
009300090109     C                   ENDIF
009400090109     C*
009500090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
009600090109     C                   select
009700090109     C                   when      CurrOpz = *blanks
009800090109     C                   eval      wrkesito = '2'
009900090109     C                   when      CurrOpz = 'GO'
010000090109     C*
010100090109     C* OPERAZIONI INIZIALI
010200090109     C                   exsr      PREELA
010300000613     C*
010400090109     C* ELABORAZIONE
010500000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010600090109     C*
010700090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
010800090129     C                   if        CurrPostPgm <> *blanks AND
010900090129     C                             CurrPostPgm <> '*NONE'
011000090129     C                   if        CurrPostLib = *blanks
011100090129     C                   if        CurrEnv = 'T'
011200150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011300090129     C                   else
011400150609     C                   eval      CurrPostLib = 'GEOV72SB'
011500090129     C                   endif
011600090129     C                   endif
011700090121     C*
011800110105     C                   call      'TIS7FXC'
011900090129     C                   parm                    CurrPostLib
012000090129     C                   parm                    CurrPostPgm
012100090121     C                   parm                    CurrEnv
012200090121     C                   parm                    CurrSito
012300090121     C                   parm                    CurrDepot
012400090121     C                   parm                    CurrTip
012500090121     C                   parm                    wrkesito
012600100623     C*
012700100623     C                   if        wrkesito = *blanks
012800100623     C                   eval      wrkesito = '0'
012900100623     C                   endif
013000100623     C*
013100090112     C                   endif
013200090121     C*
013300090121     C* OPERAZIONI FINALI
013400090121     C                   exsr      ENDELA
013500090112     C*
013600090109     C                   endsl
013700000616     C*
013800090109     C                   seton                                        lr
013900090109
014000090109
014100090109     C*--------------------------------------------------------
014200090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014300090109     C*--------------------------------------------------------
014400090109     C     PREELA        BEGSR
014500090109     C*
014600090109     C*
014700090109     C                   ENDSR
014800090109     C***
014900090109
015000090109
015100090109     C*--------------------------------------------------------
015200090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015300090109     C*--------------------------------------------------------
015400090109     C     ENDELA        BEGSR
015500090109     C*
015600090109     C*
015700090109     C                   ENDSR
015800090109     C***
015900090109
016000090109
016100000613
016200000724
016300910830     C*--------------------------------------------------------
016400040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016500910830     C*--------------------------------------------------------
016600000724     C     RWFIL         BEGSR
016700990910     C*
016800990914     C                   if        not %open(tivin00r)
016900990908     C                   open      tivin00r
017000990914     C                   endif
017100040203     C                   if        not %open(wwrce00f)
017200040203     C                   open      wwrce00f
017300990914     C                   endif
017400990910     C*
017500990910     C                   clear                   §CTROK
017600990910     C                   clear                   §CTRNO
017700001009     C                   clear                   recko
017800990910     C*
017900921023     C                   DO        *HIVAL
018000990913     C*
018100990915     C                   READ      tivin00r                               70
018200000905     C                   if        vindta > *blanks
018300000613     C                   add       1             rrnum
018400000724     C                   if        *in70 = *off and
018500990913     C                             (vinflg = *blanks
018600990913     C                              or vinflg = '0'
018700990913     C                              or vinflg = '2')
018800990913     C*
018900990913     C                   clear                   vinmsg
019000000724     C                   if        vindta <> *blanks
019100000724     C                   movel(p)  vindta        passaggio
019200040203     C                   write(e)  WWRCE
019300990910     C*
019400000724     C                   if        not %error
019500000724     C                   add       1             §ctrok
019600990910     C                   eval      vinflg = '1'
019700990910     C                   else
019800000724     C                   add       1             §ctrno
019900000621     C                   eval      recko = vindta
020000990910     C                   eval      vinflg = '2'
020100990910     C                   endif
020200000724     C                   endif
020300991022     C                   endif
020400000905     C*
020500000905     C                   else
020600000905     C                   eval      vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900000905     C  N70              update    tivin000
021000991022     C*
021100991022     C  N70              ENDdo
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800000710     C                   if        §ctrok > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800040203     C                   if        %open(wwrce00f)
022900040203     C                   close     wwrce00f
023000990914     C                   endif
023100990920     C*
023200000724     C                   if        wrkesito <> '2'
023300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023400070913     C                   movel(p)  CMD(8)        depcmd
023500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
023600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
023700000724     C                   Z-ADD     100           LENGH            15 5
023800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023900000724     C                   PARM                    depcmd
024000000724     C                   PARM                    LENGH
024100000724     C*
024200070913     C                   movel(p)  CMD(9)        depcmd
024300000724     C                   eval      depcmd = %trim(depcmd)
024400000724     C                   Z-ADD     100           LENGH            15 5
024500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024600000724     C                   PARM                    depcmd
024700000724     C                   PARM                    LENGH
024800000724     C*
024900000724     C                   movel(p)  CMD(4)        depcmd
025000000724     C                   eval      depcmd = %trim(depcmd) +
025100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025200090109     C                             %trim(CurrFileAS) +
025300070913     C                             %trim(CMD(6)) +
025400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025500070913     C                             %trim(CMD(7))
025600000724     C                   Z-ADD     100           LENGH            15 5
025700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025800000724     C                   PARM                    depcmd
025900000724     C                   PARM                    LENGH
026000000724     C*
026100000724     C                   endif
026200000724     C*
026300910830     C                   ENDSR
026400000613     C***
026500000725
026600000725
026700000613     C     *inzsr        BEGSR
026800990910     C*
026900990910     C     *entry        plist
027000990920     C                   parm                    tivlrds
027100990921     C                   parm      wrkesito      esito
027200000724     C                   parm                    prmlit
027300000724     C                   parm                    prmfir
027400000613     C*
027500000613     C                   ENDSR
027600000613     C***
027700000725
027800000512** CMD - COMANDI CL
027900161207
028000161207
028100161207
028200040203CPYF FROMFILE(WWRCE00F) TOFILE(
028300070913/
028400070913) FROMMBR(
028500070913) MBROPT(*ADD) FMTOPT(*NOCHK)
028600040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
028700040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
