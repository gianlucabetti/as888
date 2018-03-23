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
006800111003     C* => G.A. OPERATIONS SPA
006900111003     C                   IF        CurrSito  = '09'   AND
007000140411     C                             CurrDepot = 'GAO'
007100140411     C                   IF        %subst(wNomeFile:1:7) = 'INBOUND'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300111003     C                   EVAL      CurrFileAS  = 'WP3GCDP'
007400110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007500140411     C                   EVAL      CurrTip     = 'EX'
007600090109     C                   ENDIF
007700111003     C                   IF        %subst(wNomeFile:1:3) = 'DBC'
007800100503     C                   EVAL      CurrOpz     = 'GO'
007900111003     C                   EVAL      CurrFileAS  = 'WP3GBCP'
008000110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008100140411     C                   EVAL      CurrTip     = 'CX'
008200100503     C                   ENDIF
008300111003     C                   IF        %subst(wNomeFile:1:3) = 'DCC'
008400090112     C                   EVAL      CurrOpz     = 'GO'
008500111003     C                   EVAL      CurrFileAS  = 'WP3GCCP'
008600110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008700140411     C                   EVAL      CurrTip     = 'CX'
008800090112     C                   ENDIF
008900140411     C                   IF        %subst(wNomeFile:1:6) = 'MFMDPA'
009000140411     C                   EVAL      CurrOpz     = 'GO'
009100140411     C                   EVAL      CurrFileAS  = 'WP3GDTP'
009200140411     C                   EVAL      CurrPostPgm = 'LTCB400C'
009300140411     C                   EVAL      CurrTip     = 'P1'
009400140411     C                   ENDIF
009500090109     C                   ENDIF
009600090109     C*
009700090109     C                   ELSE
009800090112     C                   EVAL      CurrOpz     = 'GO'
009900090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010000090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
010100090109     C                   ENDIF
010200090109     C*
010300090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010400090109     C                   select
010500090109     C                   when      CurrOpz = *blanks
010600090109     C                   eval      wrkesito = '2'
010700090109     C                   when      CurrOpz = 'GO'
010800090109     C*
010900090109     C* OPERAZIONI INIZIALI
011000090109     C                   exsr      PREELA
011100000613     C*
011200090109     C* ELABORAZIONE
011300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011500090109     C*
011600090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011700090129     C                   if        CurrPostPgm <> *blanks AND
011800090129     C                             CurrPostPgm <> '*NONE'
011900090129     C                   if        CurrPostLib = *blanks
012000090129     C                   if        CurrEnv = 'T'
012100111003     C                   eval      CurrPostLib = 'GEOV72SBT'
012200090129     C                   else
012300111003     C                   eval      CurrPostLib = 'GEOV72SB'
012400090129     C                   endif
012500090129     C                   endif
012600090121     C*
012700110428     C                   call      'TIS7P1C'
012800090129     C                   parm                    CurrPostLib
012900090129     C                   parm                    CurrPostPgm
013000090121     C                   parm                    CurrEnv
013100090121     C                   parm                    CurrSito
013200090121     C                   parm                    CurrDepot
013300090121     C                   parm                    CurrTip
013400090121     C                   parm                    wrkesito
013500090112     C                   endif
013600090121     C*
013700090121     C* OPERAZIONI FINALI
013800090121     C                   exsr      ENDELA
013900090112     C*
014000090109     C                   endsl
014100000616     C*
014200090109     C                   seton                                        lr
014300090109
014400090109
014500090109     C*--------------------------------------------------------
014600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014700090109     C*--------------------------------------------------------
014800090109     C     PREELA        BEGSR
014900090109     C*
015000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015100090109     C                   movel(p)  CMD(1)        depcmd
015200090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
015300090109     C*
015400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
015500090109     C                   Z-ADD     100           LENGH            15 5
015600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015700090109     C                   PARM                    depcmd
015800090109     C                   PARM                    LENGH
015900090109     C*
016000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016100090109     C                   movel(p)  CMD(2)        depcmd
016200090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016300090109     C*
016400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016500090109     C                   Z-ADD     100           LENGH            15 5
016600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016700090109     C                   PARM                    depcmd
016800090109     C                   PARM                    LENGH
016900090109     C*
017000090109     C                   if        not %open(tis7pxps)
017100090109     C                   open      tis7pxps
017200090109     C                   except    testdett
017300090109     C                   endif
017400090109     C*
017500090109     C                   ENDSR
017600090109     C***
017700090109
017800090109
017900090109     C*--------------------------------------------------------
018000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
018100090109     C*--------------------------------------------------------
018200090109     C     ENDELA        BEGSR
018300090109     C*
018400090109     C                   if        %open(tis7pxps)
018500090109     C                   except    findett
018600090109     C                   close     tis7pxps
018700090109     C                   endif
018800090109     C*
018900090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019000090109     C                   movel(p)  CMD(3)        depcmd
019100090109     C                   Z-ADD     100           LENGH            15 5
019200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019300090109     C                   PARM                    depcmd
019400090109     C                   PARM                    LENGH
019500090109     C*
019600090109     C                   ENDSR
019700090109     C***
019800090109
019900090109
020000000613
020100000724
020200910830     C*--------------------------------------------------------
020300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020400910830     C*--------------------------------------------------------
020500000724     C     RWFIL         BEGSR
020600990910     C*
020700990914     C                   if        not %open(tivin00r)
020800990908     C                   open      tivin00r
020900990914     C                   endif
021000040203     C                   if        not %open(wwrce00f)
021100040203     C                   open      wwrce00f
021200990914     C                   endif
021300990910     C*
021400990910     C                   clear                   §CTROK
021500990910     C                   clear                   §CTRNO
021600001009     C                   clear                   recko
021700990910     C*
021800921023     C                   DO        *HIVAL
021900990913     C*
022000990915     C                   READ      tivin00r                               70
022100000905     C                   if        vindta > *blanks
022200000613     C                   add       1             rrnum
022300000724     C                   if        *in70 = *off and
022400990913     C                             (vinflg = *blanks
022500990913     C                              or vinflg = '0'
022600990913     C                              or vinflg = '2')
022700990913     C*
022800990913     C                   clear                   vinmsg
022900000724     C                   if        vindta <> *blanks
023000000724     C                   movel(p)  vindta        passaggio
023100040203     C                   write(e)  WWRCE
023200990910     C*
023300000724     C                   if        not %error
023400000724     C                   add       1             §ctrok
023500990910     C                   eval      vinflg = '1'
023600990910     C                   else
023700000724     C                   add       1             §ctrno
023800000621     C                   eval      recko = vindta
023900000616     C                   except    rigadett
024000990910     C                   eval      vinflg = '2'
024100990910     C                   endif
024200000724     C                   endif
024300991022     C                   endif
024400000905     C*
024500000905     C                   else
024600000905     C                   eval      vinflg = '1'
024700000905     C                   endif
024800000905     C*
024900000905     C  N70              update    tivin000
025000991022     C*
025100991022     C  N70              ENDdo
025200990910
025300990910     C* Se non ci sono record con errori ...
025400000710     C                   if        §ctrno = 0
025500990910     C* ... restituisco esito OK.
025600990921     C                   eval      wrkesito = '0'
025700990910     C                   else
025800000710     C                   if        §ctrok > 0
025900990921     C                   eval      wrkesito = '1'
026000000710     C                   else
026100000710     C                   eval      wrkesito = '2'
026200990910     C                   endif
026300000710     C                   endif
026400990910     C*
026500990914     C                   if        %open(tivin00r)
026600990908     C                   close     tivin00r
026700990914     C                   endif
026800040203     C                   if        %open(wwrce00f)
026900040203     C                   close     wwrce00f
027000990914     C                   endif
027100990920     C*
027200000724     C                   if        wrkesito <> '2'
027300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027400070913     C                   movel(p)  CMD(8)        depcmd
027500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
027600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
027700000724     C                   Z-ADD     100           LENGH            15 5
027800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027900000724     C                   PARM                    depcmd
028000000724     C                   PARM                    LENGH
028100000724     C*
028200070913     C                   movel(p)  CMD(9)        depcmd
028300000724     C                   eval      depcmd = %trim(depcmd)
028400000724     C                   Z-ADD     100           LENGH            15 5
028500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028600000724     C                   PARM                    depcmd
028700000724     C                   PARM                    LENGH
028800000724     C*
028900000724     C                   movel(p)  CMD(4)        depcmd
029000000724     C                   eval      depcmd = %trim(depcmd) +
029100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029200090109     C                             %trim(CurrFileAS) +
029300070913     C                             %trim(CMD(6)) +
029400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
029500070913     C                             %trim(CMD(7))
029600000724     C                   Z-ADD     100           LENGH            15 5
029700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029800000724     C                   PARM                    depcmd
029900000724     C                   PARM                    LENGH
030000000724     C*
030100000724     C                   endif
030200000724     C*
030300910830     C                   ENDSR
030400000613     C***
030500000725
030600000725
030700000725     C*--------------------------------------------------------
030800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
030900000725     C*--------------------------------------------------------
031000000725     C     STPR          BEGSR
031100000725     C*
031200030613     C                   if        not %open(tis7pxp)
031300030613     C                   open      tis7pxp
031400000725     C                   endif
031500000725     C*
031600000725     C                   except    riepilogo
031700000725     C*
031800030613     C                   if        %open(tis7pxp)
031900030613     C                   close     tis7pxp
032000000725     C                   endif
032100000725     C*
032200000725     C                   ENDSR
032300000725     C***
032400990920
032500990910
032600000613     C     *inzsr        BEGSR
032700990910     C*
032800990910     C     *entry        plist
032900990920     C                   parm                    tivlrds
033000990921     C                   parm      wrkesito      esito
033100000724     C                   parm                    prmlit
033200000724     C                   parm                    prmfir
033300000613     C*
033400000613     C                   ENDSR
033500000613     C***
033600000725
033700030613     Otis7pxp   E            riepilogo         2
033800000725     O                                              'Upload via FTP'
033900000725     O                                           +1 'Traduzione TIVIN00R -'
034000040203     O                                           +1 'WWRCE00F'
034100000725     O                                           +1 'Entrate'
034200000725     O          E            riepilogo   2
034300000725     O                       wrkdata
034400000725     O                       wrkora              +1
034500000725     O                       procname            +1
034600000725     O          E            riepilogo   2
034700000725     O                                              'Cliente..................:'
034800030613     O                       VLRKSC              +1
034900000725     O          E            riepilogo   2
035000000725     O                                              'Riferimento Strategi.....:'
035100000725     O                       vlrhdl              +1
035200000725     O          E            riepilogo   2
035300000725     O                                              'Giusti...................:'
035400000725     O                       §CTROK        2   +  1
035500000725     O          E            riepilogo   2
035600000725     O                                              'Sbagliati e scartati.....:'
035700000725     O                       §CTRNO        2   +  1
035800990908
035900000724
036000030613     Otis7pxps  E            testdett          2
036100000724     O                                              'Upload via FTP'
036200000613     O                                           +1 'Traduzione TIVIN00R -'
036300040203     O                                           +1 'WWRCE00F'
036400000725     O                                           +1 'Entrate'
036500000616     O          E            testdett    3
036600000613     O                                           +2 'N° rec'
036700000613     O                                           +3 'Anteprima contenuto'
036800000616     O          E            rigadett    2
036900000613     O                       rrnum               +2
037000000621     O                       recko               +3
037100000616     O          E            findett     2
037200000613     O                       wrkdata
037300000613     O                       wrkora              +1
037400000613     O                       procname            +1
037500000616     O          E            findett     2
037600000613     O                                              'Cliente..................:'
037700030613     O                       VLRKSC              +1
037800000616     O          E            findett     2
037900000613     O                                              'Riferimento Strategi.....:'
038000000613     O                       vlrhdl              +1
038100000616     O          E            findett     2
038200000613     O                                              'Giusti...................:'
038300000613     O                       §CTROK        2   +  1
038400000616     O          E            findett     2
038500000613     O                                              'Sbagliati e scartati.....:'
038600000613     O                       §CTRNO        2   +  1
038700000512** CMD - COMANDI CL
038800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
038900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
039000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
039100040203CPYF FROMFILE(WWRCE00F) TOFILE(
039200070913/
039300070913) FROMMBR(
039400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
039500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
039600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
