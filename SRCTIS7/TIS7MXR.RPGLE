000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
000300150610
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040203     FWWRCE00F  O    E             DISK    usropn
000600030613     Ftis7pxp   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800030613     Ftis7pxps  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001500000512     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900040203     D passaggio     e ds                  extname(wwrce00f)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400990915     D wrkdata         s               d
002500990915     D wrkora          s               t
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700000621     D recko           s            150    INZ(*blanks)
002800000724     D depcmd          s            100    INZ(*blanks)
002900000724     D §ctrok          s              5  0 INZ(*zeros)
003000000724     D §ctrno          s              5  0 INZ(*zeros)
003100090109     D wNomeFile       s             30    INZ(*blanks)
003200090121     D CurrOpz         s              2    INZ
003300090121     D CurrFileAS      s             10    INZ
003400100408     D CurrPostPgm     s             10    INZ
003500090129     D CurrPostLib     s             10    INZ
003600090121     D CurrSito        s              2    INZ
003700090121     D CurrDepot       s              8    INZ
003800090121     D CurrEnv         s              1    INZ
003900090121     D CurrTip         s              2    INZ
004000090109     D*-------------------
004100090109     D* COSTANTI
004200090109     D*-------------------
004300090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004400090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004500990908
004600990915     C                   time                    wrkdata
004700990915     C                   time                    wrkora
004800000913     C                   reset                   rrnum
004900990921     C                   reset                   esito
005000990921     C                   reset                   wrkesito
005100090109     C*
005200090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005300090121     C                   EVAL      CurrOpz     = *blanks
005400090121     C                   EVAL      CurrFileAS  = *blanks
005500090129     C                   EVAL      CurrPostPgm = *blanks
005600090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005700100408     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
005800090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005900090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
006000090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
006100090121     C*
006200090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006300090109     C                   MOVE(P)   vlrMSG        wNomeFile
006400090109     C* Porto il nome file sempre in maiuscolo
006500090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006600100408     C*
006700100513     C                   IF        %subst(wNomeFile:1:6) = 'XX3MDC'
006800090112     C                   EVAL      CurrOpz     = 'GO'
006900100513     C                   EVAL      CurrFileAS  = 'WP3MDCP'
007000100513     C                   EVAL      CurrTip     = 'MD'
007100090109     C                   ENDIF
007200090109     C*
007300090109     C                   ELSE
007400090112     C                   EVAL      CurrOpz     = 'GO'
007500090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
007600090109     C                   ENDIF
007700090109     C*
007800090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
007900090109     C                   select
008000090109     C                   when      CurrOpz = *blanks
008100090109     C                   eval      wrkesito = '2'
008200090109     C                   when      CurrOpz = 'GO'
008300090109     C*
008400090109     C* OPERAZIONI INIZIALI
008500090109     C                   exsr      PREELA
008600000613     C*
008700090109     C* ELABORAZIONE
008800000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
008900000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009000090109     C*
009100090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
009200090129     C                   if        CurrPostPgm <> *blanks AND
009300090129     C                             CurrPostPgm <> '*NONE'
009400090129     C                   if        CurrPostLib = *blanks
009500090129     C                   if        CurrEnv = 'T'
009600150609     C                   eval      CurrPostLib = 'GEOV72SBT'
009700090129     C                   else
009800150609     C                   eval      CurrPostLib = 'GEOV72SB'
009900090129     C                   endif
010000090129     C                   endif
010100090121     C*
010200100513     C                   call      'TIS7MXC'
010300090129     C                   parm                    CurrPostLib
010400090129     C                   parm                    CurrPostPgm
010500090121     C                   parm                    CurrEnv
010600090121     C                   parm                    CurrSito
010700090121     C                   parm                    CurrDepot
010800090121     C                   parm                    CurrTip
010900090121     C                   parm                    wrkesito
011000090112     C                   endif
011100090121     C*
011200090121     C* OPERAZIONI FINALI
011300090121     C                   exsr      ENDELA
011400090112     C*
011500090109     C                   endsl
011600000616     C*
011700090109     C                   seton                                        lr
011800090109
011900090109
012000090109     C*--------------------------------------------------------
012100090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
012200090109     C*--------------------------------------------------------
012300090109     C     PREELA        BEGSR
012400090109     C*
012500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600090109     C                   movel(p)  CMD(1)        depcmd
012700090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
012800090109     C*
012900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
013000090109     C                   Z-ADD     100           LENGH            15 5
013100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013200090109     C                   PARM                    depcmd
013300090109     C                   PARM                    LENGH
013400090109     C*
013500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013600090109     C                   movel(p)  CMD(2)        depcmd
013700090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013800090109     C*
013900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014000090109     C                   Z-ADD     100           LENGH            15 5
014100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014200090109     C                   PARM                    depcmd
014300090109     C                   PARM                    LENGH
014400090109     C*
014500090109     C                   if        not %open(tis7pxps)
014600090109     C                   open      tis7pxps
014700090109     C                   except    testdett
014800090109     C                   endif
014900090109     C*
015000090109     C                   ENDSR
015100090109     C***
015200090109
015300090109
015400090109     C*--------------------------------------------------------
015500090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
015600090109     C*--------------------------------------------------------
015700090109     C     ENDELA        BEGSR
015800090109     C*
015900090109     C                   if        %open(tis7pxps)
016000090109     C                   except    findett
016100090109     C                   close     tis7pxps
016200090109     C                   endif
016300090109     C*
016400090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016500090109     C                   movel(p)  CMD(3)        depcmd
016600090109     C                   Z-ADD     100           LENGH            15 5
016700090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016800090109     C                   PARM                    depcmd
016900090109     C                   PARM                    LENGH
017000090109     C*
017100090109     C                   ENDSR
017200090109     C***
017300090109
017400090109
017500000613
017600000724
017700910830     C*--------------------------------------------------------
017800040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
017900910830     C*--------------------------------------------------------
018000000724     C     RWFIL         BEGSR
018100990910     C*
018200990914     C                   if        not %open(tivin00r)
018300990908     C                   open      tivin00r
018400990914     C                   endif
018500040203     C                   if        not %open(wwrce00f)
018600040203     C                   open      wwrce00f
018700990914     C                   endif
018800990910     C*
018900990910     C                   clear                   §CTROK
019000990910     C                   clear                   §CTRNO
019100001009     C                   clear                   recko
019200990910     C*
019300921023     C                   DO        *HIVAL
019400990913     C*
019500990915     C                   READ      tivin00r                               70
019600000905     C                   if        vindta > *blanks
019700000613     C                   add       1             rrnum
019800000724     C                   if        *in70 = *off and
019900990913     C                             (vinflg = *blanks
020000990913     C                              or vinflg = '0'
020100990913     C                              or vinflg = '2')
020200990913     C*
020300990913     C                   clear                   vinmsg
020400000724     C                   if        vindta <> *blanks
020500000724     C                   movel(p)  vindta        passaggio
020600040203     C                   write(e)  WWRCE
020700990910     C*
020800000724     C                   if        not %error
020900000724     C                   add       1             §ctrok
021000990910     C                   eval      vinflg = '1'
021100990910     C                   else
021200000724     C                   add       1             §ctrno
021300000621     C                   eval      recko = vindta
021400000616     C                   except    rigadett
021500990910     C                   eval      vinflg = '2'
021600990910     C                   endif
021700000724     C                   endif
021800991022     C                   endif
021900000905     C*
022000000905     C                   else
022100000905     C                   eval      vinflg = '1'
022200000905     C                   endif
022300000905     C*
022400000905     C  N70              update    tivin000
022500991022     C*
022600991022     C  N70              ENDdo
022700990910
022800990910     C* Se non ci sono record con errori ...
022900000710     C                   if        §ctrno = 0
023000990910     C* ... restituisco esito OK.
023100990921     C                   eval      wrkesito = '0'
023200990910     C                   else
023300000710     C                   if        §ctrok > 0
023400990921     C                   eval      wrkesito = '1'
023500000710     C                   else
023600000710     C                   eval      wrkesito = '2'
023700990910     C                   endif
023800000710     C                   endif
023900990910     C*
024000990914     C                   if        %open(tivin00r)
024100990908     C                   close     tivin00r
024200990914     C                   endif
024300040203     C                   if        %open(wwrce00f)
024400040203     C                   close     wwrce00f
024500990914     C                   endif
024600990920     C*
024700000724     C                   if        wrkesito <> '2'
024800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
024900070913     C                   movel(p)  CMD(8)        depcmd
025000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
025100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
025200000724     C                   Z-ADD     100           LENGH            15 5
025300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025400000724     C                   PARM                    depcmd
025500000724     C                   PARM                    LENGH
025600000724     C*
025700070913     C                   movel(p)  CMD(9)        depcmd
025800000724     C                   eval      depcmd = %trim(depcmd)
025900000724     C                   Z-ADD     100           LENGH            15 5
026000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026100000724     C                   PARM                    depcmd
026200000724     C                   PARM                    LENGH
026300000724     C*
026400000724     C                   movel(p)  CMD(4)        depcmd
026500000724     C                   eval      depcmd = %trim(depcmd) +
026600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
026700090109     C                             %trim(CurrFileAS) +
026800070913     C                             %trim(CMD(6)) +
026900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
027000070913     C                             %trim(CMD(7))
027100000724     C                   Z-ADD     100           LENGH            15 5
027200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027300000724     C                   PARM                    depcmd
027400000724     C                   PARM                    LENGH
027500000724     C*
027600000724     C                   endif
027700000724     C*
027800910830     C                   ENDSR
027900000613     C***
028000000725
028100000725
028200000725     C*--------------------------------------------------------
028300000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
028400000725     C*--------------------------------------------------------
028500000725     C     STPR          BEGSR
028600000725     C*
028700030613     C                   if        not %open(tis7pxp)
028800030613     C                   open      tis7pxp
028900000725     C                   endif
029000000725     C*
029100000725     C                   except    riepilogo
029200000725     C*
029300030613     C                   if        %open(tis7pxp)
029400030613     C                   close     tis7pxp
029500000725     C                   endif
029600000725     C*
029700000725     C                   ENDSR
029800000725     C***
029900990920
030000990910
030100000613     C     *inzsr        BEGSR
030200990910     C*
030300990910     C     *entry        plist
030400990920     C                   parm                    tivlrds
030500990921     C                   parm      wrkesito      esito
030600000724     C                   parm                    prmlit
030700000724     C                   parm                    prmfir
030800000613     C*
030900000613     C                   ENDSR
031000000613     C***
031100000725
031200030613     Otis7pxp   E            riepilogo         2
031300000725     O                                              'Upload via FTP'
031400000725     O                                           +1 'Traduzione TIVIN00R -'
031500040203     O                                           +1 'WWRCE00F'
031600000725     O                                           +1 'Entrate'
031700000725     O          E            riepilogo   2
031800000725     O                       wrkdata
031900000725     O                       wrkora              +1
032000000725     O                       procname            +1
032100000725     O          E            riepilogo   2
032200000725     O                                              'Cliente..................:'
032300030613     O                       VLRKSC              +1
032400000725     O          E            riepilogo   2
032500000725     O                                              'Riferimento Strategi.....:'
032600000725     O                       vlrhdl              +1
032700000725     O          E            riepilogo   2
032800000725     O                                              'Giusti...................:'
032900000725     O                       §CTROK        2   +  1
033000000725     O          E            riepilogo   2
033100000725     O                                              'Sbagliati e scartati.....:'
033200000725     O                       §CTRNO        2   +  1
033300990908
033400000724
033500030613     Otis7pxps  E            testdett          2
033600000724     O                                              'Upload via FTP'
033700000613     O                                           +1 'Traduzione TIVIN00R -'
033800040203     O                                           +1 'WWRCE00F'
033900000725     O                                           +1 'Entrate'
034000000616     O          E            testdett    3
034100000613     O                                           +2 'N° rec'
034200000613     O                                           +3 'Anteprima contenuto'
034300000616     O          E            rigadett    2
034400000613     O                       rrnum               +2
034500000621     O                       recko               +3
034600000616     O          E            findett     2
034700000613     O                       wrkdata
034800000613     O                       wrkora              +1
034900000613     O                       procname            +1
035000000616     O          E            findett     2
035100000613     O                                              'Cliente..................:'
035200030613     O                       VLRKSC              +1
035300000616     O          E            findett     2
035400000613     O                                              'Riferimento Strategi.....:'
035500000613     O                       vlrhdl              +1
035600000616     O          E            findett     2
035700000613     O                                              'Giusti...................:'
035800000613     O                       §CTROK        2   +  1
035900000616     O          E            findett     2
036000000613     O                                              'Sbagliati e scartati.....:'
036100000613     O                       §CTRNO        2   +  1
036200000512** CMD - COMANDI CL
036300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
036400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
036500030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
036600040203CPYF FROMFILE(WWRCE00F) TOFILE(
036700070913/
036800070913) FROMMBR(
036900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
037000040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
037100040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
