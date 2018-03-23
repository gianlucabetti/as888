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
006300100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3EA1' OR
006400100504     C                             %subst(wNomeFile:1:3) = 'EA1'
006500090112     C                   EVAL      CurrOpz     = 'GO'
006600100504     C                   EVAL      CurrFileAS  = 'WP3EA1P'
006700100504     C                   EVAL      CurrTip     = 'E1'
006800090109     C                   ENDIF
006900100408     C*
007000100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3EA2' OR
007100100504     C                             %subst(wNomeFile:1:3) = 'EA2'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300100504     C                   EVAL      CurrFileAS  = 'WP3EA2P'
007400100504     C                   EVAL      CurrTip     = 'E2'
007500090109     C                   ENDIF
007600100601     C*
007700100601     C                   IF        %subst(wNomeFile:1:6) = 'XX3EAX' OR
007800100601     C                             %subst(wNomeFile:1:3) = 'EAX'
007900100601     C                   EVAL      CurrOpz     = 'GO'
008000100601     C                   EVAL      CurrFileAS  = 'WP3EAXP'
008100100601     C                   EVAL      CurrTip     = 'EX'
008200100601     C                   ENDIF
008300100615     C*
008400100615     C* Personalizzazioni
008500100615     C                   SELECT
008600100615     C*
008700100615     C* - CAMILLATEX - 16 CAMILLAT
008800100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
008900100615     C*
009000100615     C                   IF        %subst(wNomeFile:1:3) = 'AIN'
009100100615     C                   EVAL      CurrOpz     = 'GO'
009200100615     C                   EVAL      CurrFileAS  = 'WP3AINP'
009300100615     C                   EVAL      CurrTip     = 'EX'
009400100615     C                   ENDIF
009500100615     C*
009600100615     C                   ENDSL
009700090109     C*
009800090109     C                   ELSE
009900090112     C                   EVAL      CurrOpz     = 'GO'
010000090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010100100623     C                   EVAL      CurrTip     = vlrtip
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
012700100504     C                   call      'TIS7EXC'
012800090129     C                   parm                    CurrPostLib
012900090129     C                   parm                    CurrPostPgm
013000090121     C                   parm                    CurrEnv
013100090121     C                   parm                    CurrSito
013200090121     C                   parm                    CurrDepot
013300090121     C                   parm                    CurrTip
013400090121     C                   parm                    wrkesito
013500100623     C*
013600100623     C                   if        wrkesito = *blanks
013700100623     C                   eval      wrkesito = '0'
013800100623     C                   endif
013900100623     C*
014000090112     C                   endif
014100090121     C*
014200090121     C* OPERAZIONI FINALI
014300090121     C                   exsr      ENDELA
014400090112     C*
014500090109     C                   endsl
014600000616     C*
014700090109     C                   seton                                        lr
014800090109
014900090109
015000090109     C*--------------------------------------------------------
015100090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015200090109     C*--------------------------------------------------------
015300090109     C     PREELA        BEGSR
015400090109     C*
015500090109     C*
015600090109     C                   ENDSR
015700090109     C***
015800090109
015900090109
016000090109     C*--------------------------------------------------------
016100090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
016200090109     C*--------------------------------------------------------
016300090109     C     ENDELA        BEGSR
016400090109     C*
016500090109     C*
016600090109     C                   ENDSR
016700090109     C***
016800090109
016900090109
017000000613
017100000724
017200910830     C*--------------------------------------------------------
017300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
017400910830     C*--------------------------------------------------------
017500000724     C     RWFIL         BEGSR
017600990910     C*
017700990914     C                   if        not %open(tivin00r)
017800990908     C                   open      tivin00r
017900990914     C                   endif
018000040203     C                   if        not %open(wwrce00f)
018100040203     C                   open      wwrce00f
018200990914     C                   endif
018300990910     C*
018400990910     C                   clear                   §CTROK
018500990910     C                   clear                   §CTRNO
018600001009     C                   clear                   recko
018700990910     C*
018800921023     C                   DO        *HIVAL
018900990913     C*
019000990915     C                   READ      tivin00r                               70
019100000905     C                   if        vindta > *blanks
019200000613     C                   add       1             rrnum
019300000724     C                   if        *in70 = *off and
019400990913     C                             (vinflg = *blanks
019500990913     C                              or vinflg = '0'
019600990913     C                              or vinflg = '2')
019700990913     C*
019800990913     C                   clear                   vinmsg
019900000724     C                   if        vindta <> *blanks
020000000724     C                   movel(p)  vindta        passaggio
020100040203     C                   write(e)  WWRCE
020200990910     C*
020300000724     C                   if        not %error
020400000724     C                   add       1             §ctrok
020500990910     C                   eval      vinflg = '1'
020600990910     C                   else
020700000724     C                   add       1             §ctrno
020800000621     C                   eval      recko = vindta
020900990910     C                   eval      vinflg = '2'
021000990910     C                   endif
021100000724     C                   endif
021200991022     C                   endif
021300000905     C*
021400000905     C                   else
021500000905     C                   eval      vinflg = '1'
021600000905     C                   endif
021700000905     C*
021800000905     C  N70              update    tivin000
021900991022     C*
022000991022     C  N70              ENDdo
022100990910
022200990910     C* Se non ci sono record con errori ...
022300000710     C                   if        §ctrno = 0
022400990910     C* ... restituisco esito OK.
022500990921     C                   eval      wrkesito = '0'
022600990910     C                   else
022700000710     C                   if        §ctrok > 0
022800990921     C                   eval      wrkesito = '1'
022900000710     C                   else
023000000710     C                   eval      wrkesito = '2'
023100990910     C                   endif
023200000710     C                   endif
023300990910     C*
023400990914     C                   if        %open(tivin00r)
023500990908     C                   close     tivin00r
023600990914     C                   endif
023700040203     C                   if        %open(wwrce00f)
023800040203     C                   close     wwrce00f
023900990914     C                   endif
024000990920     C*
024100000724     C                   if        wrkesito <> '2'
024200000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
024300070913     C                   movel(p)  CMD(8)        depcmd
024400000724     C                   eval      depcmd = %trim(depcmd) + ' '+
024500000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
024600000724     C                   Z-ADD     100           LENGH            15 5
024700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024800000724     C                   PARM                    depcmd
024900000724     C                   PARM                    LENGH
025000000724     C*
025100070913     C                   movel(p)  CMD(9)        depcmd
025200000724     C                   eval      depcmd = %trim(depcmd)
025300000724     C                   Z-ADD     100           LENGH            15 5
025400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025500000724     C                   PARM                    depcmd
025600000724     C                   PARM                    LENGH
025700000724     C*
025800000724     C                   movel(p)  CMD(4)        depcmd
025900000724     C                   eval      depcmd = %trim(depcmd) +
026000090109     C                             %trim(prmlit) + %trim(CMD(5)) +
026100090109     C                             %trim(CurrFileAS) +
026200070913     C                             %trim(CMD(6)) +
026300000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
026400070913     C                             %trim(CMD(7))
026500000724     C                   Z-ADD     100           LENGH            15 5
026600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026700000724     C                   PARM                    depcmd
026800000724     C                   PARM                    LENGH
026900000724     C*
027000000724     C                   endif
027100000724     C*
027200910830     C                   ENDSR
027300000613     C***
027400000725
027500000725
027600000613     C     *inzsr        BEGSR
027700990910     C*
027800990910     C     *entry        plist
027900990920     C                   parm                    tivlrds
028000990921     C                   parm      wrkesito      esito
028100000724     C                   parm                    prmlit
028200000724     C                   parm                    prmfir
028300000613     C*
028400000613     C                   ENDSR
028500000613     C***
028600000725
028700000512** CMD - COMANDI CL
028800161207
028900161207
029000161207
029100040203CPYF FROMFILE(WWRCE00F) TOFILE(
029200070913/
029300070913) FROMMBR(
029400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
029500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
029600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
