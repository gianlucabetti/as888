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
006900090330     C                   IF        CurrSito  = '14'   AND
007000090330     C                             CurrDepot = 'DECAR2'
007100090330     C*
007200090330     C                   IF        %scan('DECAR_AR':wNomeFile:1)  > *zeros
007300090112     C                   EVAL      CurrOpz     = 'GO'
007400090330     C                   EVAL      CurrFileAS  = 'WP3DARP'
007500110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007600090330     C                   EVAL      CurrTip     = 'AR'
007700090109     C                   ENDIF
007800090330     C                   IF        %scan('DECAR_DO':wNomeFile:1)  > *zeros
007900090330     C                   EVAL      CurrOpz     = 'GO'
008000090330     C                   EVAL      CurrFileAS  = 'WP3DDOP'
008100110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008200090330     C                   EVAL      CurrTip     = 'DO'
008300090330     C                   ENDIF
008400090330     C*
008500090330     C                   ENDIF
008600090109     C*
008700090109     C                   ELSE
008800090112     C                   EVAL      CurrOpz     = 'GO'
008900090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009000090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
009100090109     C                   ENDIF
009200090109     C*
009300090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
009400090109     C                   select
009500090109     C                   when      CurrOpz = *blanks
009600090109     C                   eval      wrkesito = '2'
009700090109     C                   when      CurrOpz = 'GO'
009800090109     C*
009900090109     C* OPERAZIONI INIZIALI
010000090109     C                   exsr      PREELA
010100000613     C*
010200090109     C* ELABORAZIONE
010300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010500090109     C*
010600090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
010700090129     C                   if        CurrPostPgm <> *blanks AND
010800090129     C                             CurrPostPgm <> '*NONE'
010900090129     C                   if        CurrPostLib = *blanks
011000090129     C                   if        CurrEnv = 'T'
011100150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011200090129     C                   else
011300150609     C                   eval      CurrPostLib = 'GEOV72SB'
011400090129     C                   endif
011500090129     C                   endif
011600090121     C*
011700110428     C                   call      'TIS7P1C'
011800090129     C                   parm                    CurrPostLib
011900090129     C                   parm                    CurrPostPgm
012000090121     C                   parm                    CurrEnv
012100090121     C                   parm                    CurrSito
012200090121     C                   parm                    CurrDepot
012300090121     C                   parm                    CurrTip
012400090121     C                   parm                    wrkesito
012500090112     C                   endif
012600090121     C*
012700090121     C* OPERAZIONI FINALI
012800090121     C                   exsr      ENDELA
012900090112     C*
013000090109     C                   endsl
013100000616     C*
013200090109     C                   seton                                        lr
013300090109
013400090109
013500090109     C*--------------------------------------------------------
013600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013700090109     C*--------------------------------------------------------
013800090109     C     PREELA        BEGSR
013900090109     C*
014000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100090109     C                   movel(p)  CMD(1)        depcmd
014200090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
014300090109     C*
014400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
014500090109     C                   Z-ADD     100           LENGH            15 5
014600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700090109     C                   PARM                    depcmd
014800090109     C                   PARM                    LENGH
014900090109     C*
015000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015100090109     C                   movel(p)  CMD(2)        depcmd
015200090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015300090109     C*
015400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015500090109     C                   Z-ADD     100           LENGH            15 5
015600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015700090109     C                   PARM                    depcmd
015800090109     C                   PARM                    LENGH
015900090109     C*
016000090109     C                   if        not %open(tis7pxps)
016100090109     C                   open      tis7pxps
016200090109     C                   except    testdett
016300090109     C                   endif
016400090109     C*
016500090109     C                   ENDSR
016600090109     C***
016700090109
016800090109
016900090109     C*--------------------------------------------------------
017000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
017100090109     C*--------------------------------------------------------
017200090109     C     ENDELA        BEGSR
017300090109     C*
017400090109     C                   if        %open(tis7pxps)
017500090109     C                   except    findett
017600090109     C                   close     tis7pxps
017700090109     C                   endif
017800090109     C*
017900090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018000090109     C                   movel(p)  CMD(3)        depcmd
018100090109     C                   Z-ADD     100           LENGH            15 5
018200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018300090109     C                   PARM                    depcmd
018400090109     C                   PARM                    LENGH
018500090109     C*
018600090109     C                   ENDSR
018700090109     C***
018800090109
018900090109
019000000613
019100000724
019200910830     C*--------------------------------------------------------
019300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
019400910830     C*--------------------------------------------------------
019500000724     C     RWFIL         BEGSR
019600990910     C*
019700990914     C                   if        not %open(tivin00r)
019800990908     C                   open      tivin00r
019900990914     C                   endif
020000040203     C                   if        not %open(wwrce00f)
020100040203     C                   open      wwrce00f
020200990914     C                   endif
020300990910     C*
020400990910     C                   clear                   §CTROK
020500990910     C                   clear                   §CTRNO
020600001009     C                   clear                   recko
020700990910     C*
020800921023     C                   DO        *HIVAL
020900990913     C*
021000990915     C                   READ      tivin00r                               70
021100000905     C                   if        vindta > *blanks
021200000613     C                   add       1             rrnum
021300000724     C                   if        *in70 = *off and
021400990913     C                             (vinflg = *blanks
021500990913     C                              or vinflg = '0'
021600990913     C                              or vinflg = '2')
021700990913     C*
021800990913     C                   clear                   vinmsg
021900000724     C                   if        vindta <> *blanks
022000000724     C                   movel(p)  vindta        passaggio
022100040203     C                   write(e)  WWRCE
022200990910     C*
022300000724     C                   if        not %error
022400000724     C                   add       1             §ctrok
022500990910     C                   eval      vinflg = '1'
022600990910     C                   else
022700000724     C                   add       1             §ctrno
022800000621     C                   eval      recko = vindta
022900000616     C                   except    rigadett
023000990910     C                   eval      vinflg = '2'
023100990910     C                   endif
023200000724     C                   endif
023300991022     C                   endif
023400000905     C*
023500000905     C                   else
023600000905     C                   eval      vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900000905     C  N70              update    tivin000
024000991022     C*
024100991022     C  N70              ENDdo
024200990910
024300990910     C* Se non ci sono record con errori ...
024400000710     C                   if        §ctrno = 0
024500990910     C* ... restituisco esito OK.
024600990921     C                   eval      wrkesito = '0'
024700990910     C                   else
024800000710     C                   if        §ctrok > 0
024900990921     C                   eval      wrkesito = '1'
025000000710     C                   else
025100000710     C                   eval      wrkesito = '2'
025200990910     C                   endif
025300000710     C                   endif
025400990910     C*
025500990914     C                   if        %open(tivin00r)
025600990908     C                   close     tivin00r
025700990914     C                   endif
025800040203     C                   if        %open(wwrce00f)
025900040203     C                   close     wwrce00f
026000990914     C                   endif
026100990920     C*
026200000724     C                   if        wrkesito <> '2'
026300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
026400070913     C                   movel(p)  CMD(8)        depcmd
026500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
026600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
026700000724     C                   Z-ADD     100           LENGH            15 5
026800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
026900000724     C                   PARM                    depcmd
027000000724     C                   PARM                    LENGH
027100000724     C*
027200070913     C                   movel(p)  CMD(9)        depcmd
027300000724     C                   eval      depcmd = %trim(depcmd)
027400000724     C                   Z-ADD     100           LENGH            15 5
027500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027600000724     C                   PARM                    depcmd
027700000724     C                   PARM                    LENGH
027800000724     C*
027900000724     C                   movel(p)  CMD(4)        depcmd
028000000724     C                   eval      depcmd = %trim(depcmd) +
028100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028200090109     C                             %trim(CurrFileAS) +
028300070913     C                             %trim(CMD(6)) +
028400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
028500070913     C                             %trim(CMD(7))
028600000724     C                   Z-ADD     100           LENGH            15 5
028700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028800000724     C                   PARM                    depcmd
028900000724     C                   PARM                    LENGH
029000000724     C*
029100000724     C                   endif
029200000724     C*
029300910830     C                   ENDSR
029400000613     C***
029500000725
029600000725
029700000725     C*--------------------------------------------------------
029800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
029900000725     C*--------------------------------------------------------
030000000725     C     STPR          BEGSR
030100000725     C*
030200030613     C                   if        not %open(tis7pxp)
030300030613     C                   open      tis7pxp
030400000725     C                   endif
030500000725     C*
030600000725     C                   except    riepilogo
030700000725     C*
030800030613     C                   if        %open(tis7pxp)
030900030613     C                   close     tis7pxp
031000000725     C                   endif
031100000725     C*
031200000725     C                   ENDSR
031300000725     C***
031400990920
031500990910
031600000613     C     *inzsr        BEGSR
031700990910     C*
031800990910     C     *entry        plist
031900990920     C                   parm                    tivlrds
032000990921     C                   parm      wrkesito      esito
032100000724     C                   parm                    prmlit
032200000724     C                   parm                    prmfir
032300000613     C*
032400000613     C                   ENDSR
032500000613     C***
032600000725
032700030613     Otis7pxp   E            riepilogo         2
032800000725     O                                              'Upload via FTP'
032900000725     O                                           +1 'Traduzione TIVIN00R -'
033000040203     O                                           +1 'WWRCE00F'
033100000725     O                                           +1 'Entrate'
033200000725     O          E            riepilogo   2
033300000725     O                       wrkdata
033400000725     O                       wrkora              +1
033500000725     O                       procname            +1
033600000725     O          E            riepilogo   2
033700000725     O                                              'Cliente..................:'
033800030613     O                       VLRKSC              +1
033900000725     O          E            riepilogo   2
034000000725     O                                              'Riferimento Strategi.....:'
034100000725     O                       vlrhdl              +1
034200000725     O          E            riepilogo   2
034300000725     O                                              'Giusti...................:'
034400000725     O                       §CTROK        2   +  1
034500000725     O          E            riepilogo   2
034600000725     O                                              'Sbagliati e scartati.....:'
034700000725     O                       §CTRNO        2   +  1
034800990908
034900000724
035000030613     Otis7pxps  E            testdett          2
035100000724     O                                              'Upload via FTP'
035200000613     O                                           +1 'Traduzione TIVIN00R -'
035300040203     O                                           +1 'WWRCE00F'
035400000725     O                                           +1 'Entrate'
035500000616     O          E            testdett    3
035600000613     O                                           +2 'N° rec'
035700000613     O                                           +3 'Anteprima contenuto'
035800000616     O          E            rigadett    2
035900000613     O                       rrnum               +2
036000000621     O                       recko               +3
036100000616     O          E            findett     2
036200000613     O                       wrkdata
036300000613     O                       wrkora              +1
036400000613     O                       procname            +1
036500000616     O          E            findett     2
036600000613     O                                              'Cliente..................:'
036700030613     O                       VLRKSC              +1
036800000616     O          E            findett     2
036900000613     O                                              'Riferimento Strategi.....:'
037000000613     O                       vlrhdl              +1
037100000616     O          E            findett     2
037200000613     O                                              'Giusti...................:'
037300000613     O                       §CTROK        2   +  1
037400000616     O          E            findett     2
037500000613     O                                              'Sbagliati e scartati.....:'
037600000613     O                       §CTRNO        2   +  1
037700000512** CMD - COMANDI CL
037800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
037900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
038000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
038100040203CPYF FROMFILE(WWRCE00F) TOFILE(
038200070913/
038300070913) FROMMBR(
038400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
038500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
038600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
