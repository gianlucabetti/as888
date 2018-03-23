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
003600101007     D wPosDaA         s              2    INZ(*blanks)
003700101007     D wPosDa          s              2  0 INZ(*zeros)
003800090109     D*-------------------
003900090109     D* COSTANTI
004000090109     D*-------------------
004100090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004200090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004300990908
004400990915     C                   time                    wrkdata
004500990915     C                   time                    wrkora
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900090109     C*
005000090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005100090121     C                   EVAL      CurrOpz     = *blanks
005200090121     C                   EVAL      CurrFileAS  = *blanks
005300090129     C                   EVAL      CurrPostPgm = *blanks
005400090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005500100408     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
005600090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005700090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005800090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
005900101007     C*
006000101007     C* Reperisco subito il nome del file "UPLOADATO" (se c'è') e lo porto in maiuscolo
006100101007     C                   MOVE(P)   vlrMSG        wNomeFile
006200101007     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006300090121     C*
006400090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006500100408     C*
006600150907     C                   IF        %subst(wNomeFile:1:5) = 'XX3BS'
006700140429     C                   EVAL      CurrOpz     = 'GO'
006800140429     C                   EVAL      CurrFileAS  = 'WP3BSLP'
006900140429     C                   EVAL      CurrTip     = 'BX'
007000090109     C                   ENDIF
007100100615     C*
007200100615     C* Personalizzazioni
007300100615     C                   SELECT
007400100615     C*
007500140429     C* - CAMILLATEX - 99 XXXXXXXX
007600140429     C                   WHEN      %trim(CurrSito+CurrDepot) = '99XXXXXXXX'
007700100615     C*
007800100615     C                   IF        %subst(wNomeFile:1:3) = 'ART'
007900100615     C                   EVAL      CurrOpz     = 'GO'
008000140429     C                   EVAL      CurrFileAS  = 'WP3BSLP'
008100140429     C                   EVAL      CurrTip     = 'BX'
008200100615     C                   ENDIF
008300100615     C*
008400100615     C                   ENDSL
008500090109     C*
008600090109     C                   ELSE
008700090112     C                   EVAL      CurrOpz     = 'GO'
008800090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
008900101007     C*
009000101007     C                   IF        %subst(vlrppt:42:2) <> *blanks
009100101007     C                   EVAL      wPosDaA = %subst(vlrppt:42:2)
009200101007     C                   MOVE(P)   wPosDaA       wPosDa
009300101007     C                   EVAL      CurrTip=%subst(wNomeFile:wPosDa:2)
009400101007     C                   ELSE
009500101007     C                   EVAL      CurrTip     = vlrtip
009600101007     C                   ENDIF
009700101007     C*
009800090109     C                   ENDIF
009900090109     C*
010000090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010100090109     C                   select
010200090109     C                   when      CurrOpz = *blanks
010300090109     C                   eval      wrkesito = '2'
010400090109     C                   when      CurrOpz = 'GO'
010500090109     C*
010600090109     C* OPERAZIONI INIZIALI
010700090109     C                   exsr      PREELA
010800000613     C*
010900090109     C* ELABORAZIONE
011000000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011100090109     C*
011200090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011300090129     C                   if        CurrPostPgm <> *blanks AND
011400090129     C                             CurrPostPgm <> '*NONE'
011500090129     C                   if        CurrPostLib = *blanks
011600090129     C                   if        CurrEnv = 'T'
011700150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011800090129     C                   else
011900150609     C                   eval      CurrPostLib = 'GEOV72SB'
012000090129     C                   endif
012100090129     C                   endif
012200090121     C*
012300140429     C                   call      'TIS7BXC'
012400090129     C                   parm                    CurrPostLib
012500090129     C                   parm                    CurrPostPgm
012600090121     C                   parm                    CurrEnv
012700090121     C                   parm                    CurrSito
012800090121     C                   parm                    CurrDepot
012900090121     C                   parm                    CurrTip
013000090121     C                   parm                    wrkesito
013100100623     C*
013200100623     C                   if        wrkesito = *blanks
013300100623     C                   eval      wrkesito = '0'
013400100623     C                   endif
013500100623     C*
013600090112     C                   endif
013700090121     C*
013800090121     C* OPERAZIONI FINALI
013900090121     C                   exsr      ENDELA
014000090112     C*
014100090109     C                   endsl
014200000616     C*
014300090109     C                   seton                                        lr
014400090109
014500090109
014600090109     C*--------------------------------------------------------
014700090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014800090109     C*--------------------------------------------------------
014900090109     C     PREELA        BEGSR
015000090109     C*
015100090109     C*
015200090109     C                   ENDSR
015300090109     C***
015400090109
015500090109
015600090109     C*--------------------------------------------------------
015700090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015800090109     C*--------------------------------------------------------
015900090109     C     ENDELA        BEGSR
016000090109     C*
016100090109     C*
016200090109     C                   ENDSR
016300090109     C***
016400090109
016500090109
016600000613
016700000724
016800910830     C*--------------------------------------------------------
016900040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
017000910830     C*--------------------------------------------------------
017100000724     C     RWFIL         BEGSR
017200990910     C*
017300990914     C                   if        not %open(tivin00r)
017400990908     C                   open      tivin00r
017500990914     C                   endif
017600040203     C                   if        not %open(wwrce00f)
017700040203     C                   open      wwrce00f
017800990914     C                   endif
017900990910     C*
018000990910     C                   clear                   §CTROK
018100990910     C                   clear                   §CTRNO
018200001009     C                   clear                   recko
018300990910     C*
018400921023     C                   DO        *HIVAL
018500990913     C*
018600990915     C                   READ      tivin00r                               70
018700000905     C                   if        vindta > *blanks
018800000613     C                   add       1             rrnum
018900000724     C                   if        *in70 = *off and
019000990913     C                             (vinflg = *blanks
019100990913     C                              or vinflg = '0'
019200990913     C                              or vinflg = '2')
019300990913     C*
019400990913     C                   clear                   vinmsg
019500000724     C                   if        vindta <> *blanks
019600000724     C                   movel(p)  vindta        passaggio
019700040203     C                   write(e)  WWRCE
019800990910     C*
019900000724     C                   if        not %error
020000000724     C                   add       1             §ctrok
020100990910     C                   eval      vinflg = '1'
020200990910     C                   else
020300000724     C                   add       1             §ctrno
020400000621     C                   eval      recko = vindta
020500990910     C                   eval      vinflg = '2'
020600990910     C                   endif
020700000724     C                   endif
020800991022     C                   endif
020900000905     C*
021000000905     C                   else
021100000905     C                   eval      vinflg = '1'
021200000905     C                   endif
021300000905     C*
021400000905     C  N70              update    tivin000
021500991022     C*
021600991022     C  N70              ENDdo
021700990910
021800990910     C* Se non ci sono record con errori ...
021900000710     C                   if        §ctrno = 0
022000990910     C* ... restituisco esito OK.
022100990921     C                   eval      wrkesito = '0'
022200990910     C                   else
022300000710     C                   if        §ctrok > 0
022400990921     C                   eval      wrkesito = '1'
022500000710     C                   else
022600000710     C                   eval      wrkesito = '2'
022700990910     C                   endif
022800000710     C                   endif
022900990910     C*
023000990914     C                   if        %open(tivin00r)
023100990908     C                   close     tivin00r
023200990914     C                   endif
023300040203     C                   if        %open(wwrce00f)
023400040203     C                   close     wwrce00f
023500990914     C                   endif
023600990920     C*
023700000724     C                   if        wrkesito <> '2'
023800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023900070913     C                   movel(p)  CMD(8)        depcmd
024000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
024100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
024200000724     C                   Z-ADD     100           LENGH            15 5
024300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024400000724     C                   PARM                    depcmd
024500000724     C                   PARM                    LENGH
024600000724     C*
024700070913     C                   movel(p)  CMD(9)        depcmd
024800000724     C                   eval      depcmd = %trim(depcmd)
024900000724     C                   Z-ADD     100           LENGH            15 5
025000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025100000724     C                   PARM                    depcmd
025200000724     C                   PARM                    LENGH
025300000724     C*
025400000724     C                   movel(p)  CMD(4)        depcmd
025500000724     C                   eval      depcmd = %trim(depcmd) +
025600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025700090109     C                             %trim(CurrFileAS) +
025800070913     C                             %trim(CMD(6)) +
025900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
026000070913     C                             %trim(CMD(7))
026100000724     C                   Z-ADD     100           LENGH            15 5
026200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026300000724     C                   PARM                    depcmd
026400000724     C                   PARM                    LENGH
026500000724     C*
026600000724     C                   endif
026700000724     C*
026800910830     C                   ENDSR
026900000613     C***
027000000725
027100000725
027200000613     C     *inzsr        BEGSR
027300990910     C*
027400990910     C     *entry        plist
027500990920     C                   parm                    tivlrds
027600990921     C                   parm      wrkesito      esito
027700000724     C                   parm                    prmlit
027800000724     C                   parm                    prmfir
027900000613     C*
028000000613     C                   ENDSR
028100000613     C***
028200000725
028300000512** CMD - COMANDI CL
028400161207
028500161207
028600161207
028700040203CPYF FROMFILE(WWRCE00F) TOFILE(
028800070913/
028900070913) FROMMBR(
029000070913) MBROPT(*ADD) FMTOPT(*NOCHK)
029100040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
029200040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
