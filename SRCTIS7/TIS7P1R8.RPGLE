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
006400101026     C* => TROPS 2
006500110825     C*//                IF        CurrSito  = '11'   AND
006600131010     C*//                IF        CurrSito  = '02'   AND
006700131010     C                   IF        CurrSito  = '18'   AND
006800101008     C                             CurrDepot = 'TROPS2'
006900101008     C                   IF        %subst(wNomeFile:1:3) = 'UPL'
007000090112     C                   EVAL      CurrOpz     = 'GO'
007100101008     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007300101008     C                   EVAL      CurrTip     = 'UP'
007400090109     C                   ENDIF
007500101008     C                   IF        %subst(wNomeFile:1:3) = 'ART'
007600100503     C                   EVAL      CurrOpz     = 'GO'
007700101008     C                   EVAL      CurrFileAS  = 'WP3T2AP'
007800110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007900101008     C                   EVAL      CurrTip     = 'AR'
008000100503     C                   ENDIF
008100101026     C                   IF        %subst(wNomeFile:1:3) = 'BAR'
008200090112     C                   EVAL      CurrOpz     = 'GO'
008300101008     C                   EVAL      CurrFileAS  = 'WP3T2BP'
008400110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008500101008     C                   EVAL      CurrTip     = 'BA'
008600090112     C                   ENDIF
008700090109     C                   ENDIF
008800090109     C*
008900090109     C                   ELSE
009000090112     C                   EVAL      CurrOpz     = 'GO'
009100090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009200090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
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
011800110428     C                   call      'TIS7P1C'
011900090129     C                   parm                    CurrPostLib
012000090129     C                   parm                    CurrPostPgm
012100090121     C                   parm                    CurrEnv
012200090121     C                   parm                    CurrSito
012300090121     C                   parm                    CurrDepot
012400090121     C                   parm                    CurrTip
012500090121     C                   parm                    wrkesito
012600090112     C                   endif
012700090121     C*
012800090121     C* OPERAZIONI FINALI
012900090121     C                   exsr      ENDELA
013000090112     C*
013100090109     C                   endsl
013200000616     C*
013300090109     C                   seton                                        lr
013400090109
013500090109
013600090109     C*--------------------------------------------------------
013700090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013800090109     C*--------------------------------------------------------
013900090109     C     PREELA        BEGSR
014000090109     C*
014100090109     C*
014200090109     C                   ENDSR
014300090109     C***
014400090109
014500090109
014600090109     C*--------------------------------------------------------
014700090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
014800090109     C*--------------------------------------------------------
014900090109     C     ENDELA        BEGSR
015000090109     C*
015100090109     C*
015200090109     C                   ENDSR
015300090109     C***
015400090109
015500090109
015600000613
015700000724
015800910830     C*--------------------------------------------------------
015900040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016000910830     C*--------------------------------------------------------
016100000724     C     RWFIL         BEGSR
016200990910     C*
016300990914     C                   if        not %open(tivin00r)
016400990908     C                   open      tivin00r
016500990914     C                   endif
016600040203     C                   if        not %open(wwrce00f)
016700040203     C                   open      wwrce00f
016800990914     C                   endif
016900990910     C*
017000990910     C                   clear                   §CTROK
017100990910     C                   clear                   §CTRNO
017200001009     C                   clear                   recko
017300990910     C*
017400921023     C                   DO        *HIVAL
017500990913     C*
017600990915     C                   READ      tivin00r                               70
017700000905     C                   if        vindta > *blanks
017800000613     C                   add       1             rrnum
017900000724     C                   if        *in70 = *off and
018000990913     C                             (vinflg = *blanks
018100990913     C                              or vinflg = '0'
018200990913     C                              or vinflg = '2')
018300990913     C*
018400990913     C                   clear                   vinmsg
018500000724     C                   if        vindta <> *blanks
018600000724     C                   movel(p)  vindta        passaggio
018700040203     C                   write(e)  WWRCE
018800990910     C*
018900000724     C                   if        not %error
019000000724     C                   add       1             §ctrok
019100990910     C                   eval      vinflg = '1'
019200990910     C                   else
019300000724     C                   add       1             §ctrno
019400000621     C                   eval      recko = vindta
019500990910     C                   eval      vinflg = '2'
019600990910     C                   endif
019700000724     C                   endif
019800991022     C                   endif
019900000905     C*
020000000905     C                   else
020100000905     C                   eval      vinflg = '1'
020200000905     C                   endif
020300000905     C*
020400000905     C  N70              update    tivin000
020500991022     C*
020600991022     C  N70              ENDdo
020700990910
020800990910     C* Se non ci sono record con errori ...
020900000710     C                   if        §ctrno = 0
021000990910     C* ... restituisco esito OK.
021100990921     C                   eval      wrkesito = '0'
021200990910     C                   else
021300000710     C                   if        §ctrok > 0
021400990921     C                   eval      wrkesito = '1'
021500000710     C                   else
021600000710     C                   eval      wrkesito = '2'
021700990910     C                   endif
021800000710     C                   endif
021900990910     C*
022000990914     C                   if        %open(tivin00r)
022100990908     C                   close     tivin00r
022200990914     C                   endif
022300040203     C                   if        %open(wwrce00f)
022400040203     C                   close     wwrce00f
022500990914     C                   endif
022600990920     C*
022700000724     C                   if        wrkesito <> '2'
022800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
022900070913     C                   movel(p)  CMD(8)        depcmd
023000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
023100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
023200000724     C                   Z-ADD     100           LENGH            15 5
023300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023400000724     C                   PARM                    depcmd
023500000724     C                   PARM                    LENGH
023600000724     C*
023700070913     C                   movel(p)  CMD(9)        depcmd
023800000724     C                   eval      depcmd = %trim(depcmd)
023900000724     C                   Z-ADD     100           LENGH            15 5
024000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024100000724     C                   PARM                    depcmd
024200000724     C                   PARM                    LENGH
024300000724     C*
024400000724     C                   movel(p)  CMD(4)        depcmd
024500000724     C                   eval      depcmd = %trim(depcmd) +
024600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
024700090109     C                             %trim(CurrFileAS) +
024800070913     C                             %trim(CMD(6)) +
024900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025000070913     C                             %trim(CMD(7))
025100000724     C                   Z-ADD     100           LENGH            15 5
025200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025300000724     C                   PARM                    depcmd
025400000724     C                   PARM                    LENGH
025500000724     C*
025600000724     C                   endif
025700000724     C*
025800910830     C                   ENDSR
025900000613     C***
026000000725
026100000725
026200000613     C     *inzsr        BEGSR
026300990910     C*
026400990910     C     *entry        plist
026500990920     C                   parm                    tivlrds
026600990921     C                   parm      wrkesito      esito
026700000724     C                   parm                    prmlit
026800000724     C                   parm                    prmfir
026900000613     C*
027000000613     C                   ENDSR
027100000613     C***
027200000725
027300000512** CMD - COMANDI CL
027400161207
027500161207
027600161207
027700040203CPYF FROMFILE(WWRCE00F) TOFILE(
027800070913/
027900070913) FROMMBR(
028000070913) MBROPT(*ADD) FMTOPT(*NOCHK)
028100040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
028200040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
