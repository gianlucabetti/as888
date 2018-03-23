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
006800111214     C* => CHAMPION S.P.A.
006900111003     C                   IF        CurrSito  = '09'   AND
007000111214     C                             CurrDepot = 'CHAMPION'
007100111214     C                   IF        %subst(wNomeFile:1:7) = 'CHCOLLI'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300111214     C                   EVAL      CurrFileAS  = 'WP3CCHP'
007400110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007500111214     C                   EVAL      CurrTip     = 'EX'
007600090109     C                   ENDIF
007700111214     C                   IF        %subst(wNomeFile:1:3) = 'MAG'
007800100503     C                   EVAL      CurrOpz     = 'GO'
007900111214     C                   EVAL      CurrFileAS  = 'WP3CMAP'
008000110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008100111214     C                   EVAL      CurrTip     = 'CX'
008200100503     C                   ENDIF
008300090109     C                   ENDIF
008400090109     C*
008500090109     C                   ELSE
008600090112     C                   EVAL      CurrOpz     = 'GO'
008700090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
008800090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
008900090109     C                   ENDIF
009000090109     C*
009100090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
009200090109     C                   select
009300090109     C                   when      CurrOpz = *blanks
009400090109     C                   eval      wrkesito = '2'
009500090109     C                   when      CurrOpz = 'GO'
009600090109     C*
009700090109     C* OPERAZIONI INIZIALI
009800090109     C                   exsr      PREELA
009900000613     C*
010000090109     C* ELABORAZIONE
010100000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010200000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010300090109     C*
010400090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
010500090129     C                   if        CurrPostPgm <> *blanks AND
010600090129     C                             CurrPostPgm <> '*NONE'
010700090129     C                   if        CurrPostLib = *blanks
010800090129     C                   if        CurrEnv = 'T'
010900111003     C                   eval      CurrPostLib = 'GEOV72SBT'
011000090129     C                   else
011100111003     C                   eval      CurrPostLib = 'GEOV72SB'
011200090129     C                   endif
011300090129     C                   endif
011400090121     C*
011500110428     C                   call      'TIS7P1C'
011600090129     C                   parm                    CurrPostLib
011700090129     C                   parm                    CurrPostPgm
011800090121     C                   parm                    CurrEnv
011900090121     C                   parm                    CurrSito
012000090121     C                   parm                    CurrDepot
012100090121     C                   parm                    CurrTip
012200090121     C                   parm                    wrkesito
012300090112     C                   endif
012400090121     C*
012500090121     C* OPERAZIONI FINALI
012600090121     C                   exsr      ENDELA
012700090112     C*
012800090109     C                   endsl
012900000616     C*
013000090109     C                   seton                                        lr
013100090109
013200090109
013300090109     C*--------------------------------------------------------
013400090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013500090109     C*--------------------------------------------------------
013600090109     C     PREELA        BEGSR
013700090109     C*
013800090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013900090109     C                   movel(p)  CMD(1)        depcmd
014000090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
014100090109     C*
014200090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
014300090109     C                   Z-ADD     100           LENGH            15 5
014400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500090109     C                   PARM                    depcmd
014600090109     C                   PARM                    LENGH
014700090109     C*
014800090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014900090109     C                   movel(p)  CMD(2)        depcmd
015000090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015100090109     C*
015200090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015300090109     C                   Z-ADD     100           LENGH            15 5
015400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015500090109     C                   PARM                    depcmd
015600090109     C                   PARM                    LENGH
015700090109     C*
015800090109     C                   if        not %open(tis7pxps)
015900090109     C                   open      tis7pxps
016000090109     C                   except    testdett
016100090109     C                   endif
016200090109     C*
016300090109     C                   ENDSR
016400090109     C***
016500090109
016600090109
016700090109     C*--------------------------------------------------------
016800090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
016900090109     C*--------------------------------------------------------
017000090109     C     ENDELA        BEGSR
017100090109     C*
017200090109     C                   if        %open(tis7pxps)
017300090109     C                   except    findett
017400090109     C                   close     tis7pxps
017500090109     C                   endif
017600090109     C*
017700090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017800090109     C                   movel(p)  CMD(3)        depcmd
017900090109     C                   Z-ADD     100           LENGH            15 5
018000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018100090109     C                   PARM                    depcmd
018200090109     C                   PARM                    LENGH
018300090109     C*
018400090109     C                   ENDSR
018500090109     C***
018600090109
018700090109
018800000613
018900000724
019000910830     C*--------------------------------------------------------
019100040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
019200910830     C*--------------------------------------------------------
019300000724     C     RWFIL         BEGSR
019400990910     C*
019500990914     C                   if        not %open(tivin00r)
019600990908     C                   open      tivin00r
019700990914     C                   endif
019800040203     C                   if        not %open(wwrce00f)
019900040203     C                   open      wwrce00f
020000990914     C                   endif
020100990910     C*
020200990910     C                   clear                   §CTROK
020300990910     C                   clear                   §CTRNO
020400001009     C                   clear                   recko
020500990910     C*
020600921023     C                   DO        *HIVAL
020700990913     C*
020800990915     C                   READ      tivin00r                               70
020900000905     C                   if        vindta > *blanks
021000000613     C                   add       1             rrnum
021100000724     C                   if        *in70 = *off and
021200990913     C                             (vinflg = *blanks
021300990913     C                              or vinflg = '0'
021400990913     C                              or vinflg = '2')
021500990913     C*
021600990913     C                   clear                   vinmsg
021700000724     C                   if        vindta <> *blanks
021800000724     C                   movel(p)  vindta        passaggio
021900040203     C                   write(e)  WWRCE
022000990910     C*
022100000724     C                   if        not %error
022200000724     C                   add       1             §ctrok
022300990910     C                   eval      vinflg = '1'
022400990910     C                   else
022500000724     C                   add       1             §ctrno
022600000621     C                   eval      recko = vindta
022700000616     C                   except    rigadett
022800990910     C                   eval      vinflg = '2'
022900990910     C                   endif
023000000724     C                   endif
023100991022     C                   endif
023200000905     C*
023300000905     C                   else
023400000905     C                   eval      vinflg = '1'
023500000905     C                   endif
023600000905     C*
023700000905     C  N70              update    tivin000
023800991022     C*
023900991022     C  N70              ENDdo
024000990910
024100990910     C* Se non ci sono record con errori ...
024200000710     C                   if        §ctrno = 0
024300990910     C* ... restituisco esito OK.
024400990921     C                   eval      wrkesito = '0'
024500990910     C                   else
024600000710     C                   if        §ctrok > 0
024700990921     C                   eval      wrkesito = '1'
024800000710     C                   else
024900000710     C                   eval      wrkesito = '2'
025000990910     C                   endif
025100000710     C                   endif
025200990910     C*
025300990914     C                   if        %open(tivin00r)
025400990908     C                   close     tivin00r
025500990914     C                   endif
025600040203     C                   if        %open(wwrce00f)
025700040203     C                   close     wwrce00f
025800990914     C                   endif
025900990920     C*
026000000724     C                   if        wrkesito <> '2'
026100000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
026200070913     C                   movel(p)  CMD(8)        depcmd
026300000724     C                   eval      depcmd = %trim(depcmd) + ' '+
026400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
026500000724     C                   Z-ADD     100           LENGH            15 5
026600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026700000724     C                   PARM                    depcmd
026800000724     C                   PARM                    LENGH
026900000724     C*
027000070913     C                   movel(p)  CMD(9)        depcmd
027100000724     C                   eval      depcmd = %trim(depcmd)
027200000724     C                   Z-ADD     100           LENGH            15 5
027300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027400000724     C                   PARM                    depcmd
027500000724     C                   PARM                    LENGH
027600000724     C*
027700000724     C                   movel(p)  CMD(4)        depcmd
027800000724     C                   eval      depcmd = %trim(depcmd) +
027900090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028000090109     C                             %trim(CurrFileAS) +
028100070913     C                             %trim(CMD(6)) +
028200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
028300070913     C                             %trim(CMD(7))
028400000724     C                   Z-ADD     100           LENGH            15 5
028500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028600000724     C                   PARM                    depcmd
028700000724     C                   PARM                    LENGH
028800000724     C*
028900000724     C                   endif
029000000724     C*
029100910830     C                   ENDSR
029200000613     C***
029300000725
029400000725
029500000725     C*--------------------------------------------------------
029600000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
029700000725     C*--------------------------------------------------------
029800000725     C     STPR          BEGSR
029900000725     C*
030000030613     C                   if        not %open(tis7pxp)
030100030613     C                   open      tis7pxp
030200000725     C                   endif
030300000725     C*
030400000725     C                   except    riepilogo
030500000725     C*
030600030613     C                   if        %open(tis7pxp)
030700030613     C                   close     tis7pxp
030800000725     C                   endif
030900000725     C*
031000000725     C                   ENDSR
031100000725     C***
031200990920
031300990910
031400000613     C     *inzsr        BEGSR
031500990910     C*
031600990910     C     *entry        plist
031700990920     C                   parm                    tivlrds
031800990921     C                   parm      wrkesito      esito
031900000724     C                   parm                    prmlit
032000000724     C                   parm                    prmfir
032100000613     C*
032200000613     C                   ENDSR
032300000613     C***
032400000725
032500030613     Otis7pxp   E            riepilogo         2
032600000725     O                                              'Upload via FTP'
032700000725     O                                           +1 'Traduzione TIVIN00R -'
032800040203     O                                           +1 'WWRCE00F'
032900000725     O                                           +1 'Entrate'
033000000725     O          E            riepilogo   2
033100000725     O                       wrkdata
033200000725     O                       wrkora              +1
033300000725     O                       procname            +1
033400000725     O          E            riepilogo   2
033500000725     O                                              'Cliente..................:'
033600030613     O                       VLRKSC              +1
033700000725     O          E            riepilogo   2
033800000725     O                                              'Riferimento Strategi.....:'
033900000725     O                       vlrhdl              +1
034000000725     O          E            riepilogo   2
034100000725     O                                              'Giusti...................:'
034200000725     O                       §CTROK        2   +  1
034300000725     O          E            riepilogo   2
034400000725     O                                              'Sbagliati e scartati.....:'
034500000725     O                       §CTRNO        2   +  1
034600990908
034700000724
034800030613     Otis7pxps  E            testdett          2
034900000724     O                                              'Upload via FTP'
035000000613     O                                           +1 'Traduzione TIVIN00R -'
035100040203     O                                           +1 'WWRCE00F'
035200000725     O                                           +1 'Entrate'
035300000616     O          E            testdett    3
035400000613     O                                           +2 'N° rec'
035500000613     O                                           +3 'Anteprima contenuto'
035600000616     O          E            rigadett    2
035700000613     O                       rrnum               +2
035800000621     O                       recko               +3
035900000616     O          E            findett     2
036000000613     O                       wrkdata
036100000613     O                       wrkora              +1
036200000613     O                       procname            +1
036300000616     O          E            findett     2
036400000613     O                                              'Cliente..................:'
036500030613     O                       VLRKSC              +1
036600000616     O          E            findett     2
036700000613     O                                              'Riferimento Strategi.....:'
036800000613     O                       vlrhdl              +1
036900000616     O          E            findett     2
037000000613     O                                              'Giusti...................:'
037100000613     O                       §CTROK        2   +  1
037200000616     O          E            findett     2
037300000613     O                                              'Sbagliati e scartati.....:'
037400000613     O                       §CTRNO        2   +  1
037500000512** CMD - COMANDI CL
037600030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
037700030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
037800030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
037900040203CPYF FROMFILE(WWRCE00F) TOFILE(
038000070913/
038100070913) FROMMBR(
038200070913) MBROPT(*ADD) FMTOPT(*NOCHK)
038300040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
038400040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
