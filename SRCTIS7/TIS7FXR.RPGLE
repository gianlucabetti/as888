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
007400170217     C*
007500170217     C                   IF        %subst(wNomeFile:1:6) = 'XX3OFX'
007600170217     C                   EVAL      CurrOpz     = 'GO'
007700170217     C                   EVAL      CurrFileAS  = 'WP3OFXP'
007800170217     C                   EVAL      CurrTip     = 'FX'
007900170217     C                   ENDIF
008000100615     C*
008100100615     C* Personalizzazioni
008200100615     C                   SELECT
008300100615     C*
008400110105     C* - XXXXXXXXXX - 00 XXXXXXXX
008500110105     C                   WHEN      %trim(CurrSito+CurrDepot) = '00XXXXXXXX'
008600100615     C*
008700110105     C                   IF        %subst(wNomeFile:1:3) = '***'
008800100615     C                   EVAL      CurrOpz     = 'GO'
008900110105     C                   EVAL      CurrFileAS  = '*******'
009000100615     C                   EVAL      CurrTip     = 'CX'
009100100615     C                   ENDIF
009200100615     C*
009300100615     C                   ENDSL
009400090109     C*
009500090109     C                   ELSE
009600090112     C                   EVAL      CurrOpz     = 'GO'
009700090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009800100623     C                   EVAL      CurrTip     = vlrtip
009900090109     C                   ENDIF
010000090109     C*
010100090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010200090109     C                   select
010300090109     C                   when      CurrOpz = *blanks
010400090109     C                   eval      wrkesito = '2'
010500090109     C                   when      CurrOpz = 'GO'
010600090109     C*
010700090109     C* OPERAZIONI INIZIALI
010800090109     C                   exsr      PREELA
010900000613     C*
011000090109     C* ELABORAZIONE
011100000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011200090109     C*
011300090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011400090129     C                   if        CurrPostPgm <> *blanks AND
011500090129     C                             CurrPostPgm <> '*NONE'
011600090129     C                   if        CurrPostLib = *blanks
011700090129     C                   if        CurrEnv = 'T'
011800150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011900090129     C                   else
012000150609     C                   eval      CurrPostLib = 'GEOV72SB'
012100090129     C                   endif
012200090129     C                   endif
012300090121     C*
012400110105     C                   call      'TIS7FXC'
012500090129     C                   parm                    CurrPostLib
012600090129     C                   parm                    CurrPostPgm
012700090121     C                   parm                    CurrEnv
012800090121     C                   parm                    CurrSito
012900090121     C                   parm                    CurrDepot
013000090121     C                   parm                    CurrTip
013100090121     C                   parm                    wrkesito
013200100623     C*
013300100623     C                   if        wrkesito = *blanks
013400100623     C                   eval      wrkesito = '0'
013500100623     C                   endif
013600100623     C*
013700090112     C                   endif
013800090121     C*
013900090121     C* OPERAZIONI FINALI
014000090121     C                   exsr      ENDELA
014100090112     C*
014200090109     C                   endsl
014300000616     C*
014400090109     C                   seton                                        lr
014500090109
014600090109
014700090109     C*--------------------------------------------------------
014800090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014900090109     C*--------------------------------------------------------
015000090109     C     PREELA        BEGSR
015100090109     C*
015200090109     C*
015300090109     C                   ENDSR
015400090109     C***
015500090109
015600090109
015700090109     C*--------------------------------------------------------
015800090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015900090109     C*--------------------------------------------------------
016000090109     C     ENDELA        BEGSR
016100090109     C*
016200090109     C*
016300090109     C                   ENDSR
016400090109     C***
016500090109
016600090109
016700000613
016800000724
016900910830     C*--------------------------------------------------------
017000040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
017100910830     C*--------------------------------------------------------
017200000724     C     RWFIL         BEGSR
017300990910     C*
017400990914     C                   if        not %open(tivin00r)
017500990908     C                   open      tivin00r
017600990914     C                   endif
017700040203     C                   if        not %open(wwrce00f)
017800040203     C                   open      wwrce00f
017900990914     C                   endif
018000990910     C*
018100990910     C                   clear                   §CTROK
018200990910     C                   clear                   §CTRNO
018300001009     C                   clear                   recko
018400990910     C*
018500921023     C                   DO        *HIVAL
018600990913     C*
018700990915     C                   READ      tivin00r                               70
018800000905     C                   if        vindta > *blanks
018900000613     C                   add       1             rrnum
019000000724     C                   if        *in70 = *off and
019100990913     C                             (vinflg = *blanks
019200990913     C                              or vinflg = '0'
019300990913     C                              or vinflg = '2')
019400990913     C*
019500990913     C                   clear                   vinmsg
019600000724     C                   if        vindta <> *blanks
019700000724     C                   movel(p)  vindta        passaggio
019800040203     C                   write(e)  WWRCE
019900990910     C*
020000000724     C                   if        not %error
020100000724     C                   add       1             §ctrok
020200990910     C                   eval      vinflg = '1'
020300990910     C                   else
020400000724     C                   add       1             §ctrno
020500000621     C                   eval      recko = vindta
020600990910     C                   eval      vinflg = '2'
020700990910     C                   endif
020800000724     C                   endif
020900991022     C                   endif
021000000905     C*
021100000905     C                   else
021200000905     C                   eval      vinflg = '1'
021300000905     C                   endif
021400000905     C*
021500000905     C  N70              update    tivin000
021600991022     C*
021700991022     C  N70              ENDdo
021800990910
021900990910     C* Se non ci sono record con errori ...
022000000710     C                   if        §ctrno = 0
022100990910     C* ... restituisco esito OK.
022200990921     C                   eval      wrkesito = '0'
022300990910     C                   else
022400000710     C                   if        §ctrok > 0
022500990921     C                   eval      wrkesito = '1'
022600000710     C                   else
022700000710     C                   eval      wrkesito = '2'
022800990910     C                   endif
022900000710     C                   endif
023000990910     C*
023100990914     C                   if        %open(tivin00r)
023200990908     C                   close     tivin00r
023300990914     C                   endif
023400040203     C                   if        %open(wwrce00f)
023500040203     C                   close     wwrce00f
023600990914     C                   endif
023700990920     C*
023800000724     C                   if        wrkesito <> '2'
023900000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
024000070913     C                   movel(p)  CMD(8)        depcmd
024100000724     C                   eval      depcmd = %trim(depcmd) + ' '+
024200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
024300000724     C                   Z-ADD     100           LENGH            15 5
024400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024500000724     C                   PARM                    depcmd
024600000724     C                   PARM                    LENGH
024700000724     C*
024800070913     C                   movel(p)  CMD(9)        depcmd
024900000724     C                   eval      depcmd = %trim(depcmd)
025000000724     C                   Z-ADD     100           LENGH            15 5
025100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025200000724     C                   PARM                    depcmd
025300000724     C                   PARM                    LENGH
025400000724     C*
025500000724     C                   movel(p)  CMD(4)        depcmd
025600000724     C                   eval      depcmd = %trim(depcmd) +
025700090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025800090109     C                             %trim(CurrFileAS) +
025900070913     C                             %trim(CMD(6)) +
026000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
026100070913     C                             %trim(CMD(7))
026200000724     C                   Z-ADD     100           LENGH            15 5
026300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026400000724     C                   PARM                    depcmd
026500000724     C                   PARM                    LENGH
026600000724     C*
026700000724     C                   endif
026800000724     C*
026900910830     C                   ENDSR
027000000613     C***
027100000725
027200000725
027300000613     C     *inzsr        BEGSR
027400990910     C*
027500990910     C     *entry        plist
027600990920     C                   parm                    tivlrds
027700990921     C                   parm      wrkesito      esito
027800000724     C                   parm                    prmlit
027900000724     C                   parm                    prmfir
028000000613     C*
028100000613     C                   ENDSR
028200000613     C***
028300000725
028400000512** CMD - COMANDI CL
028500161207
028600161207
028700161207
028800040203CPYF FROMFILE(WWRCE00F) TOFILE(
028900070913/
029000070913) FROMMBR(
029100070913) MBROPT(*ADD) FMTOPT(*NOCHK)
029200040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
029300040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
