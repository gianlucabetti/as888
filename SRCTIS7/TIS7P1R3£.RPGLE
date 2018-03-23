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
006800090109     C* => VGAMES
006900090121     C                   IF        CurrSito  = '05'   AND
007000090121     C                             CurrDepot = 'VIVENDI'
007100100310     C                   IF        %scan('IT_ITEMO_':wNomeFile:1)    > *zeros OR
007200100310     C                             %scan('ITBAR_ITEMO_':wNomeFile:1) > *zeros
007300090112     C                   EVAL      CurrOpz     = 'GO'
007400090112     C                   EVAL      CurrFileAS  = 'WP3AITP'
007500110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007600090121     C                   EVAL      CurrTip     = 'IT'
007700090109     C                   ENDIF
007800100503     C                   IF        %scan('IT_POEXO_':wNomeFile:1)    > *zeros OR
007900100503     C                             %scan('ITBAR_POEXO_':wNomeFile:1) > *zeros
008000100503     C                   EVAL      CurrOpz     = 'GO'
008100100503     C                   EVAL      CurrFileAS  = 'WP3APOP'
008200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008300100503     C                   EVAL      CurrTip     = 'PO'
008400100503     C                   ENDIF
008500100310     C                   IF        %scan('IT_SORDO_':wNomeFile:1)    > *zeros OR
008600100310     C                             %scan('ITBAR_SORDO_':wNomeFile:1) > *zeros
008700090112     C                   EVAL      CurrOpz     = 'GO'
008800090112     C                   EVAL      CurrFileAS  = 'WP3ASOP'
008900110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
009000090121     C                   EVAL      CurrTip     = 'SO'
009100090112     C                   ENDIF
009200100310     C                   IF        %scan('IT_RMAOO_':wNomeFile:1)    > *zeros OR
009300100310     C                             %scan('ITBAR_RMAOO_':wNomeFile:1) > *zeros
009400090112     C                   EVAL      CurrOpz     = 'GO'
009500090112     C                   EVAL      CurrFileAS  = 'WP3ARMP'
009600110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
009700090121     C                   EVAL      CurrTip     = 'RM'
009800090109     C                   ENDIF
009900090109     C                   ENDIF
010000090109     C*
010100090109     C                   ELSE
010200090112     C                   EVAL      CurrOpz     = 'GO'
010300090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010400090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
010500090109     C                   ENDIF
010600090109     C*
010700090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010800090109     C                   select
010900090109     C                   when      CurrOpz = *blanks
011000090109     C                   eval      wrkesito = '2'
011100090109     C                   when      CurrOpz = 'GO'
011200090109     C*
011300090109     C* OPERAZIONI INIZIALI
011400090109     C                   exsr      PREELA
011500000613     C*
011600090109     C* ELABORAZIONE
011700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011800000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900090109     C*
012000090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
012100090129     C                   if        CurrPostPgm <> *blanks AND
012200090129     C                             CurrPostPgm <> '*NONE'
012300090129     C                   if        CurrPostLib = *blanks
012400090129     C                   if        CurrEnv = 'T'
012500150609     C                   eval      CurrPostLib = 'GEOV72SBT'
012600090129     C                   else
012700150609     C                   eval      CurrPostLib = 'GEOV72SB'
012800090129     C                   endif
012900090129     C                   endif
013000090121     C*
013100110428     C                   call      'TIS7P1C'
013200090129     C                   parm                    CurrPostLib
013300090129     C                   parm                    CurrPostPgm
013400090121     C                   parm                    CurrEnv
013500090121     C                   parm                    CurrSito
013600090121     C                   parm                    CurrDepot
013700090121     C                   parm                    CurrTip
013800090121     C                   parm                    wrkesito
013900090112     C                   endif
014000090121     C*
014100090121     C* OPERAZIONI FINALI
014200090121     C                   exsr      ENDELA
014300090112     C*
014400090109     C                   endsl
014500000616     C*
014600090109     C                   seton                                        lr
014700090109
014800090109
014900090109     C*--------------------------------------------------------
015000090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015100090109     C*--------------------------------------------------------
015200090109     C     PREELA        BEGSR
015300090109     C*
015400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015500090109     C                   movel(p)  CMD(1)        depcmd
015600090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
015700090109     C*
015800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
015900090109     C                   Z-ADD     100           LENGH            15 5
016000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016100090109     C                   PARM                    depcmd
016200090109     C                   PARM                    LENGH
016300090109     C*
016400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016500090109     C                   movel(p)  CMD(2)        depcmd
016600090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016700090109     C*
016800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016900090109     C                   Z-ADD     100           LENGH            15 5
017000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017100090109     C                   PARM                    depcmd
017200090109     C                   PARM                    LENGH
017300090109     C*
017400090109     C                   if        not %open(tis7pxps)
017500090109     C                   open      tis7pxps
017600090109     C                   except    testdett
017700090109     C                   endif
017800090109     C*
017900090109     C                   ENDSR
018000090109     C***
018100090109
018200090109
018300090109     C*--------------------------------------------------------
018400090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
018500090109     C*--------------------------------------------------------
018600090109     C     ENDELA        BEGSR
018700090109     C*
018800090109     C                   if        %open(tis7pxps)
018900090109     C                   except    findett
019000090109     C                   close     tis7pxps
019100090109     C                   endif
019200090109     C*
019300090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019400090109     C                   movel(p)  CMD(3)        depcmd
019500090109     C                   Z-ADD     100           LENGH            15 5
019600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019700090109     C                   PARM                    depcmd
019800090109     C                   PARM                    LENGH
019900090109     C*
020000090109     C                   ENDSR
020100090109     C***
020200090109
020300090109
020400000613
020500000724
020600910830     C*--------------------------------------------------------
020700040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020800910830     C*--------------------------------------------------------
020900000724     C     RWFIL         BEGSR
021000990910     C*
021100990914     C                   if        not %open(tivin00r)
021200990908     C                   open      tivin00r
021300990914     C                   endif
021400040203     C                   if        not %open(wwrce00f)
021500040203     C                   open      wwrce00f
021600990914     C                   endif
021700990910     C*
021800990910     C                   clear                   §CTROK
021900990910     C                   clear                   §CTRNO
022000001009     C                   clear                   recko
022100990910     C*
022200921023     C                   DO        *HIVAL
022300990913     C*
022400990915     C                   READ      tivin00r                               70
022500000905     C                   if        vindta > *blanks
022600000613     C                   add       1             rrnum
022700000724     C                   if        *in70 = *off and
022800990913     C                             (vinflg = *blanks
022900990913     C                              or vinflg = '0'
023000990913     C                              or vinflg = '2')
023100990913     C*
023200990913     C                   clear                   vinmsg
023300000724     C                   if        vindta <> *blanks
023400000724     C                   movel(p)  vindta        passaggio
023500040203     C                   write(e)  WWRCE
023600990910     C*
023700000724     C                   if        not %error
023800000724     C                   add       1             §ctrok
023900990910     C                   eval      vinflg = '1'
024000990910     C                   else
024100000724     C                   add       1             §ctrno
024200000621     C                   eval      recko = vindta
024300000616     C                   except    rigadett
024400990910     C                   eval      vinflg = '2'
024500990910     C                   endif
024600000724     C                   endif
024700991022     C                   endif
024800000905     C*
024900000905     C                   else
025000000905     C                   eval      vinflg = '1'
025100000905     C                   endif
025200000905     C*
025300000905     C  N70              update    tivin000
025400991022     C*
025500991022     C  N70              ENDdo
025600990910
025700990910     C* Se non ci sono record con errori ...
025800000710     C                   if        §ctrno = 0
025900990910     C* ... restituisco esito OK.
026000990921     C                   eval      wrkesito = '0'
026100990910     C                   else
026200000710     C                   if        §ctrok > 0
026300990921     C                   eval      wrkesito = '1'
026400000710     C                   else
026500000710     C                   eval      wrkesito = '2'
026600990910     C                   endif
026700000710     C                   endif
026800990910     C*
026900990914     C                   if        %open(tivin00r)
027000990908     C                   close     tivin00r
027100990914     C                   endif
027200040203     C                   if        %open(wwrce00f)
027300040203     C                   close     wwrce00f
027400990914     C                   endif
027500990920     C*
027600000724     C                   if        wrkesito <> '2'
027700000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027800070913     C                   movel(p)  CMD(8)        depcmd
027900000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
028100000724     C                   Z-ADD     100           LENGH            15 5
028200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028300000724     C                   PARM                    depcmd
028400000724     C                   PARM                    LENGH
028500000724     C*
028600070913     C                   movel(p)  CMD(9)        depcmd
028700000724     C                   eval      depcmd = %trim(depcmd)
028800000724     C                   Z-ADD     100           LENGH            15 5
028900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029000000724     C                   PARM                    depcmd
029100000724     C                   PARM                    LENGH
029200000724     C*
029300000724     C                   movel(p)  CMD(4)        depcmd
029400000724     C                   eval      depcmd = %trim(depcmd) +
029500090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029600090109     C                             %trim(CurrFileAS) +
029700070913     C                             %trim(CMD(6)) +
029800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
029900070913     C                             %trim(CMD(7))
030000000724     C                   Z-ADD     100           LENGH            15 5
030100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
030200000724     C                   PARM                    depcmd
030300000724     C                   PARM                    LENGH
030400000724     C*
030500000724     C                   endif
030600000724     C*
030700910830     C                   ENDSR
030800000613     C***
030900000725
031000000725
031100000725     C*--------------------------------------------------------
031200000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
031300000725     C*--------------------------------------------------------
031400000725     C     STPR          BEGSR
031500000725     C*
031600030613     C                   if        not %open(tis7pxp)
031700030613     C                   open      tis7pxp
031800000725     C                   endif
031900000725     C*
032000000725     C                   except    riepilogo
032100000725     C*
032200030613     C                   if        %open(tis7pxp)
032300030613     C                   close     tis7pxp
032400000725     C                   endif
032500000725     C*
032600000725     C                   ENDSR
032700000725     C***
032800990920
032900990910
033000000613     C     *inzsr        BEGSR
033100990910     C*
033200990910     C     *entry        plist
033300990920     C                   parm                    tivlrds
033400990921     C                   parm      wrkesito      esito
033500000724     C                   parm                    prmlit
033600000724     C                   parm                    prmfir
033700000613     C*
033800000613     C                   ENDSR
033900000613     C***
034000000725
034100030613     Otis7pxp   E            riepilogo         2
034200000725     O                                              'Upload via FTP'
034300000725     O                                           +1 'Traduzione TIVIN00R -'
034400040203     O                                           +1 'WWRCE00F'
034500000725     O                                           +1 'Entrate'
034600000725     O          E            riepilogo   2
034700000725     O                       wrkdata
034800000725     O                       wrkora              +1
034900000725     O                       procname            +1
035000000725     O          E            riepilogo   2
035100000725     O                                              'Cliente..................:'
035200030613     O                       VLRKSC              +1
035300000725     O          E            riepilogo   2
035400000725     O                                              'Riferimento Strategi.....:'
035500000725     O                       vlrhdl              +1
035600000725     O          E            riepilogo   2
035700000725     O                                              'Giusti...................:'
035800000725     O                       §CTROK        2   +  1
035900000725     O          E            riepilogo   2
036000000725     O                                              'Sbagliati e scartati.....:'
036100000725     O                       §CTRNO        2   +  1
036200990908
036300000724
036400030613     Otis7pxps  E            testdett          2
036500000724     O                                              'Upload via FTP'
036600000613     O                                           +1 'Traduzione TIVIN00R -'
036700040203     O                                           +1 'WWRCE00F'
036800000725     O                                           +1 'Entrate'
036900000616     O          E            testdett    3
037000000613     O                                           +2 'N° rec'
037100000613     O                                           +3 'Anteprima contenuto'
037200000616     O          E            rigadett    2
037300000613     O                       rrnum               +2
037400000621     O                       recko               +3
037500000616     O          E            findett     2
037600000613     O                       wrkdata
037700000613     O                       wrkora              +1
037800000613     O                       procname            +1
037900000616     O          E            findett     2
038000000613     O                                              'Cliente..................:'
038100030613     O                       VLRKSC              +1
038200000616     O          E            findett     2
038300000613     O                                              'Riferimento Strategi.....:'
038400000613     O                       vlrhdl              +1
038500000616     O          E            findett     2
038600000613     O                                              'Giusti...................:'
038700000613     O                       §CTROK        2   +  1
038800000616     O          E            findett     2
038900000613     O                                              'Sbagliati e scartati.....:'
039000000613     O                       §CTRNO        2   +  1
039100000512** CMD - COMANDI CL
039200030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
039300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
039400030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
039500040203CPYF FROMFILE(WWRCE00F) TOFILE(
039600070913/
039700070913) FROMMBR(
039800070913) MBROPT(*ADD) FMTOPT(*NOCHK)
039900040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040000040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
