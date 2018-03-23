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
006800090330     C* => DECAR 2
006900100212     C                   IF        CurrSito  = '11'   AND
007000100212     C                             CurrDepot = 'TARGA'
007100090330     C*
007200100212     C                   IF        %scan('UBI_SAVARENT':wNomeFile:1)  > *zeros
007300090112     C                   EVAL      CurrOpz     = 'GO'
007400100212     C                   EVAL      CurrFileAS  = 'WP3TARP'
007500110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007600100212     C                   EVAL      CurrTip     = 'UB'
007700090109     C                   ENDIF
007800090330     C*
007900090330     C                   ENDIF
008000090109     C*
008100090109     C                   ELSE
008200090112     C                   EVAL      CurrOpz     = 'GO'
008300090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
008400090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
008500090109     C                   ENDIF
008600090109     C*
008700090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
008800090109     C                   select
008900090109     C                   when      CurrOpz = *blanks
009000090109     C                   eval      wrkesito = '2'
009100090109     C                   when      CurrOpz = 'GO'
009200090109     C*
009300090109     C* OPERAZIONI INIZIALI
009400090109     C                   exsr      PREELA
009500000613     C*
009600090109     C* ELABORAZIONE
009700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
009800000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009900090109     C*
010000090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
010100090129     C                   if        CurrPostPgm <> *blanks AND
010200090129     C                             CurrPostPgm <> '*NONE'
010300090129     C                   if        CurrPostLib = *blanks
010400090129     C                   if        CurrEnv = 'T'
010500150609     C                   eval      CurrPostLib = 'GEOV72SBT'
010600090129     C                   else
010700150609     C                   eval      CurrPostLib = 'GEOV72SB'
010800090129     C                   endif
010900090129     C                   endif
011000090121     C*
011100110428     C                   call      'TIS7P1C'
011200090129     C                   parm                    CurrPostLib
011300090129     C                   parm                    CurrPostPgm
011400090121     C                   parm                    CurrEnv
011500090121     C                   parm                    CurrSito
011600090121     C                   parm                    CurrDepot
011700090121     C                   parm                    CurrTip
011800090121     C                   parm                    wrkesito
011900090112     C                   endif
012000090121     C*
012100090121     C* OPERAZIONI FINALI
012200090121     C                   exsr      ENDELA
012300090112     C*
012400090109     C                   endsl
012500000616     C*
012600090109     C                   seton                                        lr
012700090109
012800090109
012900090109     C*--------------------------------------------------------
013000090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013100090109     C*--------------------------------------------------------
013200090109     C     PREELA        BEGSR
013300090109     C*
013400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013500090109     C                   movel(p)  CMD(1)        depcmd
013600090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
013700090109     C*
013800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
013900090109     C                   Z-ADD     100           LENGH            15 5
014000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014100090109     C                   PARM                    depcmd
014200090109     C                   PARM                    LENGH
014300090109     C*
014400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014500090109     C                   movel(p)  CMD(2)        depcmd
014600090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014700090109     C*
014800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014900090109     C                   Z-ADD     100           LENGH            15 5
015000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015100090109     C                   PARM                    depcmd
015200090109     C                   PARM                    LENGH
015300090109     C*
015400090109     C                   if        not %open(tis7pxps)
015500090109     C                   open      tis7pxps
015600090109     C                   except    testdett
015700090109     C                   endif
015800090109     C*
015900090109     C                   ENDSR
016000090109     C***
016100090109
016200090109
016300090109     C*--------------------------------------------------------
016400090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
016500090109     C*--------------------------------------------------------
016600090109     C     ENDELA        BEGSR
016700090109     C*
016800090109     C                   if        %open(tis7pxps)
016900090109     C                   except    findett
017000090109     C                   close     tis7pxps
017100090109     C                   endif
017200090109     C*
017300090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017400090109     C                   movel(p)  CMD(3)        depcmd
017500090109     C                   Z-ADD     100           LENGH            15 5
017600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017700090109     C                   PARM                    depcmd
017800090109     C                   PARM                    LENGH
017900090109     C*
018000090109     C                   ENDSR
018100090109     C***
018200090109
018300090109
018400000613
018500000724
018600910830     C*--------------------------------------------------------
018700040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
018800910830     C*--------------------------------------------------------
018900000724     C     RWFIL         BEGSR
019000990910     C*
019100990914     C                   if        not %open(tivin00r)
019200990908     C                   open      tivin00r
019300990914     C                   endif
019400040203     C                   if        not %open(wwrce00f)
019500040203     C                   open      wwrce00f
019600990914     C                   endif
019700990910     C*
019800990910     C                   clear                   §CTROK
019900990910     C                   clear                   §CTRNO
020000001009     C                   clear                   recko
020100990910     C*
020200921023     C                   DO        *HIVAL
020300990913     C*
020400990915     C                   READ      tivin00r                               70
020500000905     C                   if        vindta > *blanks
020600000613     C                   add       1             rrnum
020700000724     C                   if        *in70 = *off and
020800990913     C                             (vinflg = *blanks
020900990913     C                              or vinflg = '0'
021000990913     C                              or vinflg = '2')
021100990913     C*
021200990913     C                   clear                   vinmsg
021300000724     C                   if        vindta <> *blanks
021400000724     C                   movel(p)  vindta        passaggio
021500040203     C                   write(e)  WWRCE
021600990910     C*
021700000724     C                   if        not %error
021800000724     C                   add       1             §ctrok
021900990910     C                   eval      vinflg = '1'
022000990910     C                   else
022100000724     C                   add       1             §ctrno
022200000621     C                   eval      recko = vindta
022300000616     C                   except    rigadett
022400990910     C                   eval      vinflg = '2'
022500990910     C                   endif
022600000724     C                   endif
022700991022     C                   endif
022800000905     C*
022900000905     C                   else
023000000905     C                   eval      vinflg = '1'
023100000905     C                   endif
023200000905     C*
023300000905     C  N70              update    tivin000
023400991022     C*
023500991022     C  N70              ENDdo
023600990910
023700990910     C* Se non ci sono record con errori ...
023800000710     C                   if        §ctrno = 0
023900990910     C* ... restituisco esito OK.
024000990921     C                   eval      wrkesito = '0'
024100990910     C                   else
024200000710     C                   if        §ctrok > 0
024300990921     C                   eval      wrkesito = '1'
024400000710     C                   else
024500000710     C                   eval      wrkesito = '2'
024600990910     C                   endif
024700000710     C                   endif
024800990910     C*
024900990914     C                   if        %open(tivin00r)
025000990908     C                   close     tivin00r
025100990914     C                   endif
025200040203     C                   if        %open(wwrce00f)
025300040203     C                   close     wwrce00f
025400990914     C                   endif
025500990920     C*
025600000724     C                   if        wrkesito <> '2'
025700000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
025800070913     C                   movel(p)  CMD(8)        depcmd
025900000724     C                   eval      depcmd = %trim(depcmd) + ' '+
026000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
026100000724     C                   Z-ADD     100           LENGH            15 5
026200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026300000724     C                   PARM                    depcmd
026400000724     C                   PARM                    LENGH
026500000724     C*
026600070913     C                   movel(p)  CMD(9)        depcmd
026700000724     C                   eval      depcmd = %trim(depcmd)
026800000724     C                   Z-ADD     100           LENGH            15 5
026900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027000000724     C                   PARM                    depcmd
027100000724     C                   PARM                    LENGH
027200000724     C*
027300000724     C                   movel(p)  CMD(4)        depcmd
027400000724     C                   eval      depcmd = %trim(depcmd) +
027500090109     C                             %trim(prmlit) + %trim(CMD(5)) +
027600090109     C                             %trim(CurrFileAS) +
027700070913     C                             %trim(CMD(6)) +
027800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
027900070913     C                             %trim(CMD(7))
028000000724     C                   Z-ADD     100           LENGH            15 5
028100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028200000724     C                   PARM                    depcmd
028300000724     C                   PARM                    LENGH
028400000724     C*
028500000724     C                   endif
028600000724     C*
028700910830     C                   ENDSR
028800000613     C***
028900000725
029000000725
029100000725     C*--------------------------------------------------------
029200000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
029300000725     C*--------------------------------------------------------
029400000725     C     STPR          BEGSR
029500000725     C*
029600030613     C                   if        not %open(tis7pxp)
029700030613     C                   open      tis7pxp
029800000725     C                   endif
029900000725     C*
030000000725     C                   except    riepilogo
030100000725     C*
030200030613     C                   if        %open(tis7pxp)
030300030613     C                   close     tis7pxp
030400000725     C                   endif
030500000725     C*
030600000725     C                   ENDSR
030700000725     C***
030800990920
030900990910
031000000613     C     *inzsr        BEGSR
031100990910     C*
031200990910     C     *entry        plist
031300990920     C                   parm                    tivlrds
031400990921     C                   parm      wrkesito      esito
031500000724     C                   parm                    prmlit
031600000724     C                   parm                    prmfir
031700000613     C*
031800000613     C                   ENDSR
031900000613     C***
032000000725
032100030613     Otis7pxp   E            riepilogo         2
032200000725     O                                              'Upload via FTP'
032300000725     O                                           +1 'Traduzione TIVIN00R -'
032400040203     O                                           +1 'WWRCE00F'
032500000725     O                                           +1 'Entrate'
032600000725     O          E            riepilogo   2
032700000725     O                       wrkdata
032800000725     O                       wrkora              +1
032900000725     O                       procname            +1
033000000725     O          E            riepilogo   2
033100000725     O                                              'Cliente..................:'
033200030613     O                       VLRKSC              +1
033300000725     O          E            riepilogo   2
033400000725     O                                              'Riferimento Strategi.....:'
033500000725     O                       vlrhdl              +1
033600000725     O          E            riepilogo   2
033700000725     O                                              'Giusti...................:'
033800000725     O                       §CTROK        2   +  1
033900000725     O          E            riepilogo   2
034000000725     O                                              'Sbagliati e scartati.....:'
034100000725     O                       §CTRNO        2   +  1
034200990908
034300000724
034400030613     Otis7pxps  E            testdett          2
034500000724     O                                              'Upload via FTP'
034600000613     O                                           +1 'Traduzione TIVIN00R -'
034700040203     O                                           +1 'WWRCE00F'
034800000725     O                                           +1 'Entrate'
034900000616     O          E            testdett    3
035000000613     O                                           +2 'N° rec'
035100000613     O                                           +3 'Anteprima contenuto'
035200000616     O          E            rigadett    2
035300000613     O                       rrnum               +2
035400000621     O                       recko               +3
035500000616     O          E            findett     2
035600000613     O                       wrkdata
035700000613     O                       wrkora              +1
035800000613     O                       procname            +1
035900000616     O          E            findett     2
036000000613     O                                              'Cliente..................:'
036100030613     O                       VLRKSC              +1
036200000616     O          E            findett     2
036300000613     O                                              'Riferimento Strategi.....:'
036400000613     O                       vlrhdl              +1
036500000616     O          E            findett     2
036600000613     O                                              'Giusti...................:'
036700000613     O                       §CTROK        2   +  1
036800000616     O          E            findett     2
036900000613     O                                              'Sbagliati e scartati.....:'
037000000613     O                       §CTRNO        2   +  1
037100000512** CMD - COMANDI CL
037200030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
037300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
037400030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
037500040203CPYF FROMFILE(WWRCE00F) TOFILE(
037600070913/
037700070913) FROMMBR(
037800070913) MBROPT(*ADD) FMTOPT(*NOCHK)
037900040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
038000040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
