000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
000300990908
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
003400090129     D CurrPostPgm     s             10    INZ
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
005700090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
005800090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
005900090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
006000090121     C*
006100090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006200090109     C                   MOVE(P)   vlrMSG        wNomeFile
006300090109     C* Porto il nome file sempre in maiuscolo
006400090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006500090109     C*
006600090109     C* ...verifico particolarità cliente...
006700090109     C*
006800140220     C* => SWISS EYEWEAR GROUP
006900150608     C                   IF        CurrSito  = '01'   AND
007000140220     C                             CurrDepot = 'SWISSEG'
007100121228     C*
007200140220     C                   IF        %subst(wNomeFile:1:2) = 'IT'
007300120727     C                   EVAL      CurrOpz     = 'GO'
007400140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007500120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
007600140220     C                   EVAL      CurrTip     = 'AX'
007700120727     C                   ENDIF
007800140220     C*
007900140220     C                   IF        %subst(wNomeFile:1:2) = 'SO'
008000140220     C                   EVAL      CurrOpz     = 'GO'
008100140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
008200140220     C                   EVAL      CurrPostPgm = 'LTCB400C'
008300140220     C                   EVAL      CurrTip     = 'CX'
008400140220     C                   ENDIF
008500140220     C*
008600140220     C                   IF        %subst(wNomeFile:1:2) = 'PO'
008700140220     C                   EVAL      CurrOpz     = 'GO'
008800140220     C                   EVAL      CurrFileAS  = 'WP3UPLP'
008900140220     C                   EVAL      CurrPostPgm = 'LTCB400C'
009000140220     C                   EVAL      CurrTip     = 'EX'
009100140220     C                   ENDIF
009200120727     C*
009300110427     C                   ENDIF
009400090109     C*
009500090109     C                   ELSE
009600090112     C                   EVAL      CurrOpz     = 'GO'
009700090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009800090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
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
011200000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011300090109     C*
011400090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011500090129     C                   if        CurrPostPgm <> *blanks AND
011600090129     C                             CurrPostPgm <> '*NONE'
011700090129     C                   if        CurrPostLib = *blanks
011800090129     C                   if        CurrEnv = 'T'
011900150608     C                   eval      CurrPostLib = 'GEOV72SBT'
012000090129     C                   else
012100150608     C                   eval      CurrPostLib = 'GEOV72SB'
012200090129     C                   endif
012300090129     C                   endif
012400090121     C*
012500110427     C                   call      'TIS7P1C'
012600090129     C                   parm                    CurrPostLib
012700090129     C                   parm                    CurrPostPgm
012800090121     C                   parm                    CurrEnv
012900090121     C                   parm                    CurrSito
013000090121     C                   parm                    CurrDepot
013100090121     C                   parm                    CurrTip
013200090121     C                   parm                    wrkesito
013300090112     C                   endif
013400090121     C*
013500090121     C* OPERAZIONI FINALI
013600090121     C                   exsr      ENDELA
013700090112     C*
013800090109     C                   endsl
013900000616     C*
014000090109     C                   seton                                        lr
014100090109
014200090109
014300090109     C*--------------------------------------------------------
014400090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014500090109     C*--------------------------------------------------------
014600090109     C     PREELA        BEGSR
014700090109     C*
014800090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014900090109     C                   movel(p)  CMD(1)        depcmd
015000090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
015100090109     C*
015200090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
015300090109     C                   Z-ADD     100           LENGH            15 5
015400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015500090109     C                   PARM                    depcmd
015600090109     C                   PARM                    LENGH
015700090109     C*
015800090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015900090109     C                   movel(p)  CMD(2)        depcmd
016000090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016100090109     C*
016200090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016300090109     C                   Z-ADD     100           LENGH            15 5
016400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016500090109     C                   PARM                    depcmd
016600090109     C                   PARM                    LENGH
016700090109     C*
016800090109     C                   if        not %open(tis7pxps)
016900090109     C                   open      tis7pxps
017000090109     C                   except    testdett
017100090109     C                   endif
017200090109     C*
017300090109     C                   ENDSR
017400090109     C***
017500090109
017600090109
017700090109     C*--------------------------------------------------------
017800090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
017900090109     C*--------------------------------------------------------
018000090109     C     ENDELA        BEGSR
018100090109     C*
018200090109     C                   if        %open(tis7pxps)
018300090109     C                   except    findett
018400090109     C                   close     tis7pxps
018500090109     C                   endif
018600090109     C*
018700090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018800090109     C                   movel(p)  CMD(3)        depcmd
018900090109     C                   Z-ADD     100           LENGH            15 5
019000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019100090109     C                   PARM                    depcmd
019200090109     C                   PARM                    LENGH
019300090109     C*
019400090109     C                   ENDSR
019500090109     C***
019600090109
019700090109
019800000613
019900000724
020000910830     C*--------------------------------------------------------
020100040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020200910830     C*--------------------------------------------------------
020300000724     C     RWFIL         BEGSR
020400990910     C*
020500990914     C                   if        not %open(tivin00r)
020600990908     C                   open      tivin00r
020700990914     C                   endif
020800040203     C                   if        not %open(wwrce00f)
020900040203     C                   open      wwrce00f
021000990914     C                   endif
021100990910     C*
021200990910     C                   clear                   §CTROK
021300990910     C                   clear                   §CTRNO
021400001009     C                   clear                   recko
021500990910     C*
021600921023     C                   DO        *HIVAL
021700990913     C*
021800990915     C                   READ      tivin00r                               70
021900000905     C                   if        vindta > *blanks
022000000613     C                   add       1             rrnum
022100000724     C                   if        *in70 = *off and
022200990913     C                             (vinflg = *blanks
022300990913     C                              or vinflg = '0'
022400990913     C                              or vinflg = '2')
022500990913     C*
022600990913     C                   clear                   vinmsg
022700000724     C                   if        vindta <> *blanks
022800000724     C                   movel(p)  vindta        passaggio
022900040203     C                   write(e)  WWRCE
023000990910     C*
023100000724     C                   if        not %error
023200000724     C                   add       1             §ctrok
023300990910     C                   eval      vinflg = '1'
023400990910     C                   else
023500000724     C                   add       1             §ctrno
023600000621     C                   eval      recko = vindta
023700000616     C                   except    rigadett
023800990910     C                   eval      vinflg = '2'
023900990910     C                   endif
024000000724     C                   endif
024100991022     C                   endif
024200000905     C*
024300000905     C                   else
024400000905     C                   eval      vinflg = '1'
024500000905     C                   endif
024600000905     C*
024700000905     C  N70              update    tivin000
024800991022     C*
024900991022     C  N70              ENDdo
025000990910
025100990910     C* Se non ci sono record con errori ...
025200000710     C                   if        §ctrno = 0
025300990910     C* ... restituisco esito OK.
025400990921     C                   eval      wrkesito = '0'
025500990910     C                   else
025600000710     C                   if        §ctrok > 0
025700990921     C                   eval      wrkesito = '1'
025800000710     C                   else
025900000710     C                   eval      wrkesito = '2'
026000990910     C                   endif
026100000710     C                   endif
026200990910     C*
026300990914     C                   if        %open(tivin00r)
026400990908     C                   close     tivin00r
026500990914     C                   endif
026600040203     C                   if        %open(wwrce00f)
026700040203     C                   close     wwrce00f
026800990914     C                   endif
026900990920     C*
027000000724     C                   if        wrkesito <> '2'
027100000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027200070913     C                   movel(p)  CMD(8)        depcmd
027300000724     C                   eval      depcmd = %trim(depcmd) + ' '+
027400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
027500000724     C                   Z-ADD     100           LENGH            15 5
027600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027700000724     C                   PARM                    depcmd
027800000724     C                   PARM                    LENGH
027900000724     C*
028000070913     C                   movel(p)  CMD(9)        depcmd
028100000724     C                   eval      depcmd = %trim(depcmd)
028200000724     C                   Z-ADD     100           LENGH            15 5
028300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028400000724     C                   PARM                    depcmd
028500000724     C                   PARM                    LENGH
028600000724     C*
028700000724     C                   movel(p)  CMD(4)        depcmd
028800000724     C                   eval      depcmd = %trim(depcmd) +
028900090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029000090109     C                             %trim(CurrFileAS) +
029100070913     C                             %trim(CMD(6)) +
029200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
029300070913     C                             %trim(CMD(7))
029400000724     C                   Z-ADD     100           LENGH            15 5
029500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029600000724     C                   PARM                    depcmd
029700000724     C                   PARM                    LENGH
029800000724     C*
029900000724     C                   endif
030000000724     C*
030100910830     C                   ENDSR
030200000613     C***
030300000725
030400000725
030500000725     C*--------------------------------------------------------
030600000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
030700000725     C*--------------------------------------------------------
030800000725     C     STPR          BEGSR
030900000725     C*
031000030613     C                   if        not %open(tis7pxp)
031100030613     C                   open      tis7pxp
031200000725     C                   endif
031300000725     C*
031400000725     C                   except    riepilogo
031500000725     C*
031600030613     C                   if        %open(tis7pxp)
031700030613     C                   close     tis7pxp
031800000725     C                   endif
031900000725     C*
032000000725     C                   ENDSR
032100000725     C***
032200990920
032300990910
032400000613     C     *inzsr        BEGSR
032500990910     C*
032600990910     C     *entry        plist
032700990920     C                   parm                    tivlrds
032800990921     C                   parm      wrkesito      esito
032900000724     C                   parm                    prmlit
033000000724     C                   parm                    prmfir
033100000613     C*
033200000613     C                   ENDSR
033300000613     C***
033400000725
033500030613     Otis7pxp   E            riepilogo         2
033600000725     O                                              'Upload via FTP'
033700000725     O                                           +1 'Traduzione TIVIN00R -'
033800040203     O                                           +1 'WWRCE00F'
033900000725     O                                           +1 'Entrate'
034000000725     O          E            riepilogo   2
034100000725     O                       wrkdata
034200000725     O                       wrkora              +1
034300000725     O                       procname            +1
034400000725     O          E            riepilogo   2
034500000725     O                                              'Cliente..................:'
034600030613     O                       VLRKSC              +1
034700000725     O          E            riepilogo   2
034800000725     O                                              'Riferimento Strategi.....:'
034900000725     O                       vlrhdl              +1
035000000725     O          E            riepilogo   2
035100000725     O                                              'Giusti...................:'
035200000725     O                       §CTROK        2   +  1
035300000725     O          E            riepilogo   2
035400000725     O                                              'Sbagliati e scartati.....:'
035500000725     O                       §CTRNO        2   +  1
035600990908
035700000724
035800030613     Otis7pxps  E            testdett          2
035900000724     O                                              'Upload via FTP'
036000000613     O                                           +1 'Traduzione TIVIN00R -'
036100040203     O                                           +1 'WWRCE00F'
036200000725     O                                           +1 'Entrate'
036300000616     O          E            testdett    3
036400000613     O                                           +2 'N° rec'
036500000613     O                                           +3 'Anteprima contenuto'
036600000616     O          E            rigadett    2
036700000613     O                       rrnum               +2
036800000621     O                       recko               +3
036900000616     O          E            findett     2
037000000613     O                       wrkdata
037100000613     O                       wrkora              +1
037200000613     O                       procname            +1
037300000616     O          E            findett     2
037400000613     O                                              'Cliente..................:'
037500030613     O                       VLRKSC              +1
037600000616     O          E            findett     2
037700000613     O                                              'Riferimento Strategi.....:'
037800000613     O                       vlrhdl              +1
037900000616     O          E            findett     2
038000000613     O                                              'Giusti...................:'
038100000613     O                       §CTROK        2   +  1
038200000616     O          E            findett     2
038300000613     O                                              'Sbagliati e scartati.....:'
038400000613     O                       §CTRNO        2   +  1
038500000512** CMD - COMANDI CL
038600030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
038700030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
038800030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
038900040203CPYF FROMFILE(WWRCE00F) TOFILE(
039000070913/
039100070913) FROMMBR(
039200070913) MBROPT(*ADD) FMTOPT(*NOCHK)
039300040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
039400040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
