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
006800120727     C* => VIVENDI GAMES
006900120727     C                   IF        CurrSito  = '05'   AND
007000120727     C                             CurrDepot = 'VIVENDI'
007100120727     C                   IF        %scan('IT_ITEMO_':wNomeFile:1)    > *zeros OR
007200120727     C                             %scan('ITBAR_ITEMO_':wNomeFile:1) > *zeros
007300120727     C                   EVAL      CurrOpz     = 'GO'
007400120727     C                   EVAL      CurrFileAS  = 'WP3AITP'
007500120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
007600120727     C                   EVAL      CurrTip     = 'IT'
007700120727     C                   ENDIF
007800120727     C                   IF        %scan('IT_POEXO_':wNomeFile:1)    > *zeros OR
007900120727     C                             %scan('ITBAR_POEXO_':wNomeFile:1) > *zeros
008000120727     C                   EVAL      CurrOpz     = 'GO'
008100120727     C                   EVAL      CurrFileAS  = 'WP3APOP'
008200120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
008300120727     C                   EVAL      CurrTip     = 'PO'
008400120727     C                   ENDIF
008500120727     C                   IF        %scan('IT_SORDO_':wNomeFile:1)    > *zeros OR
008600120727     C                             %scan('ITBAR_SORDO_':wNomeFile:1) > *zeros
008700120727     C                   EVAL      CurrOpz     = 'GO'
008800120727     C                   EVAL      CurrFileAS  = 'WP3ASOP'
008900120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
009000120727     C                   EVAL      CurrTip     = 'SO'
009100120727     C                   ENDIF
009200120727     C                   IF        %scan('IT_RMAOO_':wNomeFile:1)    > *zeros OR
009300120727     C                             %scan('ITBAR_RMAOO_':wNomeFile:1) > *zeros
009400120727     C                   EVAL      CurrOpz     = 'GO'
009500120727     C                   EVAL      CurrFileAS  = 'WP3ARMP'
009600120727     C                   EVAL      CurrPostPgm = 'LTCB400C'
009700120727     C                   EVAL      CurrTip     = 'RM'
009800120727     C                   ENDIF
009900120727     C*
010000110427     C                   ENDIF
010100090109     C*
010200090109     C                   ELSE
010300090112     C                   EVAL      CurrOpz     = 'GO'
010400090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010500090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
010600090109     C                   ENDIF
010700090109     C*
010800090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010900090109     C                   select
011000090109     C                   when      CurrOpz = *blanks
011100090109     C                   eval      wrkesito = '2'
011200090109     C                   when      CurrOpz = 'GO'
011300090109     C*
011400090109     C* OPERAZIONI INIZIALI
011500090109     C                   exsr      PREELA
011600000613     C*
011700090109     C* ELABORAZIONE
011800000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011900000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012000090109     C*
012100090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
012200090129     C                   if        CurrPostPgm <> *blanks AND
012300090129     C                             CurrPostPgm <> '*NONE'
012400090129     C                   if        CurrPostLib = *blanks
012500090129     C                   if        CurrEnv = 'T'
012600150609     C                   eval      CurrPostLib = 'GEOV72SBT'
012700090129     C                   else
012800150609     C                   eval      CurrPostLib = 'GEOV72SB'
012900090129     C                   endif
013000090129     C                   endif
013100090121     C*
013200110427     C                   call      'TIS7P1C'
013300090129     C                   parm                    CurrPostLib
013400090129     C                   parm                    CurrPostPgm
013500090121     C                   parm                    CurrEnv
013600090121     C                   parm                    CurrSito
013700090121     C                   parm                    CurrDepot
013800090121     C                   parm                    CurrTip
013900090121     C                   parm                    wrkesito
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
015500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015600090109     C                   movel(p)  CMD(1)        depcmd
015700090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
015800090109     C*
015900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016000090109     C                   Z-ADD     100           LENGH            15 5
016100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016200090109     C                   PARM                    depcmd
016300090109     C                   PARM                    LENGH
016400090109     C*
016500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016600090109     C                   movel(p)  CMD(2)        depcmd
016700090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016800090109     C*
016900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017000090109     C                   Z-ADD     100           LENGH            15 5
017100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017200090109     C                   PARM                    depcmd
017300090109     C                   PARM                    LENGH
017400090109     C*
017500090109     C                   if        not %open(tis7pxps)
017600090109     C                   open      tis7pxps
017700090109     C                   except    testdett
017800090109     C                   endif
017900090109     C*
018000090109     C                   ENDSR
018100090109     C***
018200090109
018300090109
018400090109     C*--------------------------------------------------------
018500090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
018600090109     C*--------------------------------------------------------
018700090109     C     ENDELA        BEGSR
018800090109     C*
018900090109     C                   if        %open(tis7pxps)
019000090109     C                   except    findett
019100090109     C                   close     tis7pxps
019200090109     C                   endif
019300090109     C*
019400090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019500090109     C                   movel(p)  CMD(3)        depcmd
019600090109     C                   Z-ADD     100           LENGH            15 5
019700090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019800090109     C                   PARM                    depcmd
019900090109     C                   PARM                    LENGH
020000090109     C*
020100090109     C                   ENDSR
020200090109     C***
020300090109
020400090109
020500000613
020600000724
020700910830     C*--------------------------------------------------------
020800040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020900910830     C*--------------------------------------------------------
021000000724     C     RWFIL         BEGSR
021100990910     C*
021200990914     C                   if        not %open(tivin00r)
021300990908     C                   open      tivin00r
021400990914     C                   endif
021500040203     C                   if        not %open(wwrce00f)
021600040203     C                   open      wwrce00f
021700990914     C                   endif
021800990910     C*
021900990910     C                   clear                   §CTROK
022000990910     C                   clear                   §CTRNO
022100001009     C                   clear                   recko
022200990910     C*
022300921023     C                   DO        *HIVAL
022400990913     C*
022500990915     C                   READ      tivin00r                               70
022600000905     C                   if        vindta > *blanks
022700000613     C                   add       1             rrnum
022800000724     C                   if        *in70 = *off and
022900990913     C                             (vinflg = *blanks
023000990913     C                              or vinflg = '0'
023100990913     C                              or vinflg = '2')
023200990913     C*
023300990913     C                   clear                   vinmsg
023400000724     C                   if        vindta <> *blanks
023500000724     C                   movel(p)  vindta        passaggio
023600040203     C                   write(e)  WWRCE
023700990910     C*
023800000724     C                   if        not %error
023900000724     C                   add       1             §ctrok
024000990910     C                   eval      vinflg = '1'
024100990910     C                   else
024200000724     C                   add       1             §ctrno
024300000621     C                   eval      recko = vindta
024400000616     C                   except    rigadett
024500990910     C                   eval      vinflg = '2'
024600990910     C                   endif
024700000724     C                   endif
024800991022     C                   endif
024900000905     C*
025000000905     C                   else
025100000905     C                   eval      vinflg = '1'
025200000905     C                   endif
025300000905     C*
025400000905     C  N70              update    tivin000
025500991022     C*
025600991022     C  N70              ENDdo
025700990910
025800990910     C* Se non ci sono record con errori ...
025900000710     C                   if        §ctrno = 0
026000990910     C* ... restituisco esito OK.
026100990921     C                   eval      wrkesito = '0'
026200990910     C                   else
026300000710     C                   if        §ctrok > 0
026400990921     C                   eval      wrkesito = '1'
026500000710     C                   else
026600000710     C                   eval      wrkesito = '2'
026700990910     C                   endif
026800000710     C                   endif
026900990910     C*
027000990914     C                   if        %open(tivin00r)
027100990908     C                   close     tivin00r
027200990914     C                   endif
027300040203     C                   if        %open(wwrce00f)
027400040203     C                   close     wwrce00f
027500990914     C                   endif
027600990920     C*
027700000724     C                   if        wrkesito <> '2'
027800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027900070913     C                   movel(p)  CMD(8)        depcmd
028000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
028200000724     C                   Z-ADD     100           LENGH            15 5
028300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028400000724     C                   PARM                    depcmd
028500000724     C                   PARM                    LENGH
028600000724     C*
028700070913     C                   movel(p)  CMD(9)        depcmd
028800000724     C                   eval      depcmd = %trim(depcmd)
028900000724     C                   Z-ADD     100           LENGH            15 5
029000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029100000724     C                   PARM                    depcmd
029200000724     C                   PARM                    LENGH
029300000724     C*
029400000724     C                   movel(p)  CMD(4)        depcmd
029500000724     C                   eval      depcmd = %trim(depcmd) +
029600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029700090109     C                             %trim(CurrFileAS) +
029800070913     C                             %trim(CMD(6)) +
029900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
030000070913     C                             %trim(CMD(7))
030100000724     C                   Z-ADD     100           LENGH            15 5
030200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
030300000724     C                   PARM                    depcmd
030400000724     C                   PARM                    LENGH
030500000724     C*
030600000724     C                   endif
030700000724     C*
030800910830     C                   ENDSR
030900000613     C***
031000000725
031100000725
031200000725     C*--------------------------------------------------------
031300000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
031400000725     C*--------------------------------------------------------
031500000725     C     STPR          BEGSR
031600000725     C*
031700030613     C                   if        not %open(tis7pxp)
031800030613     C                   open      tis7pxp
031900000725     C                   endif
032000000725     C*
032100000725     C                   except    riepilogo
032200000725     C*
032300030613     C                   if        %open(tis7pxp)
032400030613     C                   close     tis7pxp
032500000725     C                   endif
032600000725     C*
032700000725     C                   ENDSR
032800000725     C***
032900990920
033000990910
033100000613     C     *inzsr        BEGSR
033200990910     C*
033300990910     C     *entry        plist
033400990920     C                   parm                    tivlrds
033500990921     C                   parm      wrkesito      esito
033600000724     C                   parm                    prmlit
033700000724     C                   parm                    prmfir
033800000613     C*
033900000613     C                   ENDSR
034000000613     C***
034100000725
034200030613     Otis7pxp   E            riepilogo         2
034300000725     O                                              'Upload via FTP'
034400000725     O                                           +1 'Traduzione TIVIN00R -'
034500040203     O                                           +1 'WWRCE00F'
034600000725     O                                           +1 'Entrate'
034700000725     O          E            riepilogo   2
034800000725     O                       wrkdata
034900000725     O                       wrkora              +1
035000000725     O                       procname            +1
035100000725     O          E            riepilogo   2
035200000725     O                                              'Cliente..................:'
035300030613     O                       VLRKSC              +1
035400000725     O          E            riepilogo   2
035500000725     O                                              'Riferimento Strategi.....:'
035600000725     O                       vlrhdl              +1
035700000725     O          E            riepilogo   2
035800000725     O                                              'Giusti...................:'
035900000725     O                       §CTROK        2   +  1
036000000725     O          E            riepilogo   2
036100000725     O                                              'Sbagliati e scartati.....:'
036200000725     O                       §CTRNO        2   +  1
036300990908
036400000724
036500030613     Otis7pxps  E            testdett          2
036600000724     O                                              'Upload via FTP'
036700000613     O                                           +1 'Traduzione TIVIN00R -'
036800040203     O                                           +1 'WWRCE00F'
036900000725     O                                           +1 'Entrate'
037000000616     O          E            testdett    3
037100000613     O                                           +2 'N° rec'
037200000613     O                                           +3 'Anteprima contenuto'
037300000616     O          E            rigadett    2
037400000613     O                       rrnum               +2
037500000621     O                       recko               +3
037600000616     O          E            findett     2
037700000613     O                       wrkdata
037800000613     O                       wrkora              +1
037900000613     O                       procname            +1
038000000616     O          E            findett     2
038100000613     O                                              'Cliente..................:'
038200030613     O                       VLRKSC              +1
038300000616     O          E            findett     2
038400000613     O                                              'Riferimento Strategi.....:'
038500000613     O                       vlrhdl              +1
038600000616     O          E            findett     2
038700000613     O                                              'Giusti...................:'
038800000613     O                       §CTROK        2   +  1
038900000616     O          E            findett     2
039000000613     O                                              'Sbagliati e scartati.....:'
039100000613     O                       §CTRNO        2   +  1
039200000512** CMD - COMANDI CL
039300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
039400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
039500030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
039600040203CPYF FROMFILE(WWRCE00F) TOFILE(
039700070913/
039800070913) FROMMBR(
039900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
040000040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040100040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
