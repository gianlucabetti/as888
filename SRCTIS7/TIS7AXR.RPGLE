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
006600100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR1' OR
006700100504     C                             %subst(wNomeFile:1:3) = 'AR1'
006800090112     C                   EVAL      CurrOpz     = 'GO'
006900100504     C                   EVAL      CurrFileAS  = 'WP3AR1P'
007000100504     C                   EVAL      CurrTip     = 'A1'
007100090109     C                   ENDIF
007200100408     C*
007300100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR2' OR
007400100504     C                             %subst(wNomeFile:1:3) = 'AR2'
007500090112     C                   EVAL      CurrOpz     = 'GO'
007600100504     C                   EVAL      CurrFileAS  = 'WP3AR2P'
007700100504     C                   EVAL      CurrTip     = 'A2'
007800090109     C                   ENDIF
007900100408     C*
008000100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR3' OR
008100100504     C                             %subst(wNomeFile:1:3) = 'AR3'
008200090112     C                   EVAL      CurrOpz     = 'GO'
008300100504     C                   EVAL      CurrFileAS  = 'WP3AR3P'
008400100504     C                   EVAL      CurrTip     = 'A3'
008500090112     C                   ENDIF
008600100408     C*
008700100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR4' OR
008800100504     C                             %subst(wNomeFile:1:3) = 'AR4'
008900090112     C                   EVAL      CurrOpz     = 'GO'
009000100504     C                   EVAL      CurrFileAS  = 'WP3AR4P'
009100100504     C                   EVAL      CurrTip     = 'A4'
009200090109     C                   ENDIF
009300100408     C*
009400100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR5' OR
009500100504     C                             %subst(wNomeFile:1:3) = 'AR5'
009600100408     C                   EVAL      CurrOpz     = 'GO'
009700100531     C                   EVAL      CurrFileAS  = 'WP3AR50P'
009800100504     C                   EVAL      CurrTip     = 'A5'
009900100408     C                   ENDIF
010000100601     C*
010100100601     C                   IF        %subst(wNomeFile:1:6) = 'XX3ARX' OR
010200100601     C                             %subst(wNomeFile:1:3) = 'ARX'
010300100601     C                   EVAL      CurrOpz     = 'GO'
010400100601     C                   EVAL      CurrFileAS  = 'WP3ARXP'
010500100601     C                   EVAL      CurrTip     = 'AX'
010600100601     C                   ENDIF
010700100615     C*
010800100615     C* Personalizzazioni
010900100615     C                   SELECT
011000100615     C*
011100100615     C* - CAMILLATEX - 16 CAMILLAT
011200100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
011300100615     C*
011400100615     C                   IF        %subst(wNomeFile:1:3) = 'ART'
011500100615     C                   EVAL      CurrOpz     = 'GO'
011600100615     C                   EVAL      CurrFileAS  = 'WP3CARP'
011700100615     C                   EVAL      CurrTip     = 'AX'
011800100615     C                   ENDIF
011900100615     C*
012000100615     C                   ENDSL
012100090109     C*
012200090109     C                   ELSE
012300090112     C                   EVAL      CurrOpz     = 'GO'
012400090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
012500101007     C*
012600101007     C                   IF        %subst(vlrppt:42:2) <> *blanks
012700101007     C                   EVAL      wPosDaA = %subst(vlrppt:42:2)
012800101007     C                   MOVE(P)   wPosDaA       wPosDa
012900101007     C                   EVAL      CurrTip=%subst(wNomeFile:wPosDa:2)
013000101007     C                   ELSE
013100101007     C                   EVAL      CurrTip     = vlrtip
013200101007     C                   ENDIF
013300101007     C*
013400090109     C                   ENDIF
013500090109     C*
013600090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
013700090109     C                   select
013800090109     C                   when      CurrOpz = *blanks
013900090109     C                   eval      wrkesito = '2'
014000090109     C                   when      CurrOpz = 'GO'
014100090109     C*
014200090109     C* OPERAZIONI INIZIALI
014300090109     C                   exsr      PREELA
014400000613     C*
014500090109     C* ELABORAZIONE
014600000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
014700090109     C*
014800090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
014900090129     C                   if        CurrPostPgm <> *blanks AND
015000090129     C                             CurrPostPgm <> '*NONE'
015100090129     C                   if        CurrPostLib = *blanks
015200090129     C                   if        CurrEnv = 'T'
015300150609     C                   eval      CurrPostLib = 'GEOV72SBT'
015400090129     C                   else
015500150609     C                   eval      CurrPostLib = 'GEOV72SB'
015600090129     C                   endif
015700090129     C                   endif
015800090121     C*
015900100504     C                   call      'TIS7AXC'
016000090129     C                   parm                    CurrPostLib
016100090129     C                   parm                    CurrPostPgm
016200090121     C                   parm                    CurrEnv
016300090121     C                   parm                    CurrSito
016400090121     C                   parm                    CurrDepot
016500090121     C                   parm                    CurrTip
016600090121     C                   parm                    wrkesito
016700100623     C*
016800100623     C                   if        wrkesito = *blanks
016900100623     C                   eval      wrkesito = '0'
017000100623     C                   endif
017100100623     C*
017200090112     C                   endif
017300090121     C*
017400090121     C* OPERAZIONI FINALI
017500090121     C                   exsr      ENDELA
017600090112     C*
017700090109     C                   endsl
017800000616     C*
017900090109     C                   seton                                        lr
018000090109
018100090109
018200090109     C*--------------------------------------------------------
018300090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
018400090109     C*--------------------------------------------------------
018500090109     C     PREELA        BEGSR
018600090109     C*
018700090109     C*
018800090109     C                   ENDSR
018900090109     C***
019000090109
019100090109
019200090109     C*--------------------------------------------------------
019300090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
019400090109     C*--------------------------------------------------------
019500090109     C     ENDELA        BEGSR
019600090109     C*
019700090109     C*
019800090109     C                   ENDSR
019900090109     C***
020000090109
020100090109
020200000613
020300000724
020400910830     C*--------------------------------------------------------
020500040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020600910830     C*--------------------------------------------------------
020700000724     C     RWFIL         BEGSR
020800990910     C*
020900990914     C                   if        not %open(tivin00r)
021000990908     C                   open      tivin00r
021100990914     C                   endif
021200040203     C                   if        not %open(wwrce00f)
021300040203     C                   open      wwrce00f
021400990914     C                   endif
021500990910     C*
021600990910     C                   clear                   §CTROK
021700990910     C                   clear                   §CTRNO
021800001009     C                   clear                   recko
021900990910     C*
022000921023     C                   DO        *HIVAL
022100990913     C*
022200990915     C                   READ      tivin00r                               70
022300000905     C                   if        vindta > *blanks
022400000613     C                   add       1             rrnum
022500000724     C                   if        *in70 = *off and
022600990913     C                             (vinflg = *blanks
022700990913     C                              or vinflg = '0'
022800990913     C                              or vinflg = '2')
022900990913     C*
023000990913     C                   clear                   vinmsg
023100000724     C                   if        vindta <> *blanks
023200000724     C                   movel(p)  vindta        passaggio
023300040203     C                   write(e)  WWRCE
023400990910     C*
023500000724     C                   if        not %error
023600000724     C                   add       1             §ctrok
023700990910     C                   eval      vinflg = '1'
023800990910     C                   else
023900000724     C                   add       1             §ctrno
024000000621     C                   eval      recko = vindta
024100990910     C                   eval      vinflg = '2'
024200990910     C                   endif
024300000724     C                   endif
024400991022     C                   endif
024500000905     C*
024600000905     C                   else
024700000905     C                   eval      vinflg = '1'
024800000905     C                   endif
024900000905     C*
025000000905     C  N70              update    tivin000
025100991022     C*
025200991022     C  N70              ENDdo
025300990910
025400990910     C* Se non ci sono record con errori ...
025500000710     C                   if        §ctrno = 0
025600990910     C* ... restituisco esito OK.
025700990921     C                   eval      wrkesito = '0'
025800990910     C                   else
025900000710     C                   if        §ctrok > 0
026000990921     C                   eval      wrkesito = '1'
026100000710     C                   else
026200000710     C                   eval      wrkesito = '2'
026300990910     C                   endif
026400000710     C                   endif
026500990910     C*
026600990914     C                   if        %open(tivin00r)
026700990908     C                   close     tivin00r
026800990914     C                   endif
026900040203     C                   if        %open(wwrce00f)
027000040203     C                   close     wwrce00f
027100990914     C                   endif
027200990920     C*
027300000724     C                   if        wrkesito <> '2'
027400000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027500070913     C                   movel(p)  CMD(8)        depcmd
027600000724     C                   eval      depcmd = %trim(depcmd) + ' '+
027700000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
027800000724     C                   Z-ADD     100           LENGH            15 5
027900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028000000724     C                   PARM                    depcmd
028100000724     C                   PARM                    LENGH
028200000724     C*
028300070913     C                   movel(p)  CMD(9)        depcmd
028400000724     C                   eval      depcmd = %trim(depcmd)
028500000724     C                   Z-ADD     100           LENGH            15 5
028600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028700000724     C                   PARM                    depcmd
028800000724     C                   PARM                    LENGH
028900000724     C*
029000000724     C                   movel(p)  CMD(4)        depcmd
029100000724     C                   eval      depcmd = %trim(depcmd) +
029200090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029300090109     C                             %trim(CurrFileAS) +
029400070913     C                             %trim(CMD(6)) +
029500000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
029600070913     C                             %trim(CMD(7))
029700000724     C                   Z-ADD     100           LENGH            15 5
029800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029900000724     C                   PARM                    depcmd
030000000724     C                   PARM                    LENGH
030100000724     C*
030200000724     C                   endif
030300000724     C*
030400910830     C                   ENDSR
030500000613     C***
030600000725
030700000725
030800000613     C     *inzsr        BEGSR
030900990910     C*
031000990910     C     *entry        plist
031100990920     C                   parm                    tivlrds
031200990921     C                   parm      wrkesito      esito
031300000724     C                   parm                    prmlit
031400000724     C                   parm                    prmfir
031500000613     C*
031600000613     C                   ENDSR
031700000613     C***
031800000512** CMD - COMANDI CL
031900161207
032000161207
032100161207
032200040203CPYF FROMFILE(WWRCE00F) TOFILE(
032300070913/
032400070913) FROMMBR(
032500070913) MBROPT(*ADD) FMTOPT(*NOCHK)
032600040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
032700040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
