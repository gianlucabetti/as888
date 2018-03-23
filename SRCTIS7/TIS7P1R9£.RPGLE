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
006800110504     C* => DEC
006900110427     C                   IF        CurrSito  = '09'   AND
007000110504     C                             CurrDepot = 'DEC'
007100110427     C                   IF        %subst(wNomeFile:1:4) = 'ANAR'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300101008     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007400110427     C                   EVAL      CurrPostPgm = 'LTCB400C'
007500110427     C                   EVAL      CurrTip     = 'AX'
007600090109     C                   ENDIF
007700110427     C                   IF        %subst(wNomeFile:1:4) = 'ORVE'
007800110427     C                   EVAL      CurrOpz     = 'GO'
007900110427     C                   EVAL      CurrFileAS  = 'WP3UPLP'
008000110427     C                   EVAL      CurrPostPgm = 'LTCB400C'
008100110427     C                   EVAL      CurrTip     = 'CX'
008200110427     C                   ENDIF
008300110427     C                   IF        %subst(wNomeFile:1:4) = 'ORAQ'
008400110427     C                   EVAL      CurrOpz     = 'GO'
008500110427     C                   EVAL      CurrFileAS  = 'WP3UPLP'
008600110427     C                   EVAL      CurrPostPgm = 'LTCB400C'
008700110427     C                   EVAL      CurrTip     = 'EX'
008800110427     C                   ENDIF
008900110427     C                   IF        %subst(wNomeFile:1:4) = 'RIRE'
009000110427     C                   EVAL      CurrOpz     = 'GO'
009100110427     C                   EVAL      CurrFileAS  = 'WP3UPLP'
009200110427     C                   EVAL      CurrPostPgm = 'LTCB400C'
009300110427     C                   EVAL      CurrTip     = 'RX'
009400110427     C                   ENDIF
009500110427     C                   IF        %subst(wNomeFile:1:4) = 'ANLO'
009600110427     C                   EVAL      CurrOpz     = 'GO'
009700110427     C                   EVAL      CurrFileAS  = 'WP3UPLP'
009800110427     C                   EVAL      CurrPostPgm = 'LTCB400C'
009900110427     C                   EVAL      CurrTip     = 'LX'
010000110427     C                   ENDIF
010100110523     C                   IF        %subst(wNomeFile:1:4) = 'RITR'
010200110523     C                   EVAL      CurrOpz     = 'GO'
010300110523     C                   EVAL      CurrFileAS  = 'WP3UPLP'
010400110523     C                   EVAL      CurrPostPgm = 'LTCB400C'
010500110523     C                   EVAL      CurrTip     = 'TX'
010600110523     C                   ENDIF
010700110427     C                   ENDIF
010800090109     C*
010900090109     C                   ELSE
011000090112     C                   EVAL      CurrOpz     = 'GO'
011100090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
011200090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
011300090109     C                   ENDIF
011400090109     C*
011500090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
011600090109     C                   select
011700090109     C                   when      CurrOpz = *blanks
011800090109     C                   eval      wrkesito = '2'
011900090109     C                   when      CurrOpz = 'GO'
012000090109     C*
012100090109     C* OPERAZIONI INIZIALI
012200090109     C                   exsr      PREELA
012300000613     C*
012400090109     C* ELABORAZIONE
012500000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
012600000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012700090109     C*
012800090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
012900090129     C                   if        CurrPostPgm <> *blanks AND
013000090129     C                             CurrPostPgm <> '*NONE'
013100090129     C                   if        CurrPostLib = *blanks
013200090129     C                   if        CurrEnv = 'T'
013300150609     C                   eval      CurrPostLib = 'GEOV72SBT'
013400090129     C                   else
013500150609     C                   eval      CurrPostLib = 'GEOV72SB'
013600090129     C                   endif
013700090129     C                   endif
013800090121     C*
013900110427     C                   call      'TIS7P1C'
014000090129     C                   parm                    CurrPostLib
014100090129     C                   parm                    CurrPostPgm
014200090121     C                   parm                    CurrEnv
014300090121     C                   parm                    CurrSito
014400090121     C                   parm                    CurrDepot
014500090121     C                   parm                    CurrTip
014600090121     C                   parm                    wrkesito
014700090112     C                   endif
014800090121     C*
014900090121     C* OPERAZIONI FINALI
015000090121     C                   exsr      ENDELA
015100090112     C*
015200090109     C                   endsl
015300000616     C*
015400090109     C                   seton                                        lr
015500090109
015600090109
015700090109     C*--------------------------------------------------------
015800090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015900090109     C*--------------------------------------------------------
016000090109     C     PREELA        BEGSR
016100090109     C*
016200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016300090109     C                   movel(p)  CMD(1)        depcmd
016400090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
016500090109     C*
016600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016700090109     C                   Z-ADD     100           LENGH            15 5
016800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016900090109     C                   PARM                    depcmd
017000090109     C                   PARM                    LENGH
017100090109     C*
017200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017300090109     C                   movel(p)  CMD(2)        depcmd
017400090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
017500090109     C*
017600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017700090109     C                   Z-ADD     100           LENGH            15 5
017800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017900090109     C                   PARM                    depcmd
018000090109     C                   PARM                    LENGH
018100090109     C*
018200090109     C                   if        not %open(tis7pxps)
018300090109     C                   open      tis7pxps
018400090109     C                   except    testdett
018500090109     C                   endif
018600090109     C*
018700090109     C                   ENDSR
018800090109     C***
018900090109
019000090109
019100090109     C*--------------------------------------------------------
019200090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
019300090109     C*--------------------------------------------------------
019400090109     C     ENDELA        BEGSR
019500090109     C*
019600090109     C                   if        %open(tis7pxps)
019700090109     C                   except    findett
019800090109     C                   close     tis7pxps
019900090109     C                   endif
020000090109     C*
020100090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020200090109     C                   movel(p)  CMD(3)        depcmd
020300090109     C                   Z-ADD     100           LENGH            15 5
020400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020500090109     C                   PARM                    depcmd
020600090109     C                   PARM                    LENGH
020700090109     C*
020800090109     C                   ENDSR
020900090109     C***
021000090109
021100090109
021200000613
021300000724
021400910830     C*--------------------------------------------------------
021500040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
021600910830     C*--------------------------------------------------------
021700000724     C     RWFIL         BEGSR
021800990910     C*
021900990914     C                   if        not %open(tivin00r)
022000990908     C                   open      tivin00r
022100990914     C                   endif
022200040203     C                   if        not %open(wwrce00f)
022300040203     C                   open      wwrce00f
022400990914     C                   endif
022500990910     C*
022600990910     C                   clear                   §CTROK
022700990910     C                   clear                   §CTRNO
022800001009     C                   clear                   recko
022900990910     C*
023000921023     C                   DO        *HIVAL
023100990913     C*
023200990915     C                   READ      tivin00r                               70
023300000905     C                   if        vindta > *blanks
023400000613     C                   add       1             rrnum
023500000724     C                   if        *in70 = *off and
023600990913     C                             (vinflg = *blanks
023700990913     C                              or vinflg = '0'
023800990913     C                              or vinflg = '2')
023900990913     C*
024000990913     C                   clear                   vinmsg
024100000724     C                   if        vindta <> *blanks
024200000724     C                   movel(p)  vindta        passaggio
024300040203     C                   write(e)  WWRCE
024400990910     C*
024500000724     C                   if        not %error
024600000724     C                   add       1             §ctrok
024700990910     C                   eval      vinflg = '1'
024800990910     C                   else
024900000724     C                   add       1             §ctrno
025000000621     C                   eval      recko = vindta
025100000616     C                   except    rigadett
025200990910     C                   eval      vinflg = '2'
025300990910     C                   endif
025400000724     C                   endif
025500991022     C                   endif
025600000905     C*
025700000905     C                   else
025800000905     C                   eval      vinflg = '1'
025900000905     C                   endif
026000000905     C*
026100000905     C  N70              update    tivin000
026200991022     C*
026300991022     C  N70              ENDdo
026400990910
026500990910     C* Se non ci sono record con errori ...
026600000710     C                   if        §ctrno = 0
026700990910     C* ... restituisco esito OK.
026800990921     C                   eval      wrkesito = '0'
026900990910     C                   else
027000000710     C                   if        §ctrok > 0
027100990921     C                   eval      wrkesito = '1'
027200000710     C                   else
027300000710     C                   eval      wrkesito = '2'
027400990910     C                   endif
027500000710     C                   endif
027600990910     C*
027700990914     C                   if        %open(tivin00r)
027800990908     C                   close     tivin00r
027900990914     C                   endif
028000040203     C                   if        %open(wwrce00f)
028100040203     C                   close     wwrce00f
028200990914     C                   endif
028300990920     C*
028400000724     C                   if        wrkesito <> '2'
028500000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
028600070913     C                   movel(p)  CMD(8)        depcmd
028700000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
028900000724     C                   Z-ADD     100           LENGH            15 5
029000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029100000724     C                   PARM                    depcmd
029200000724     C                   PARM                    LENGH
029300000724     C*
029400070913     C                   movel(p)  CMD(9)        depcmd
029500000724     C                   eval      depcmd = %trim(depcmd)
029600000724     C                   Z-ADD     100           LENGH            15 5
029700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029800000724     C                   PARM                    depcmd
029900000724     C                   PARM                    LENGH
030000000724     C*
030100000724     C                   movel(p)  CMD(4)        depcmd
030200000724     C                   eval      depcmd = %trim(depcmd) +
030300090109     C                             %trim(prmlit) + %trim(CMD(5)) +
030400090109     C                             %trim(CurrFileAS) +
030500070913     C                             %trim(CMD(6)) +
030600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
030700070913     C                             %trim(CMD(7))
030800000724     C                   Z-ADD     100           LENGH            15 5
030900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031000000724     C                   PARM                    depcmd
031100000724     C                   PARM                    LENGH
031200000724     C*
031300000724     C                   endif
031400000724     C*
031500910830     C                   ENDSR
031600000613     C***
031700000725
031800000725
031900000725     C*--------------------------------------------------------
032000000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
032100000725     C*--------------------------------------------------------
032200000725     C     STPR          BEGSR
032300000725     C*
032400030613     C                   if        not %open(tis7pxp)
032500030613     C                   open      tis7pxp
032600000725     C                   endif
032700000725     C*
032800000725     C                   except    riepilogo
032900000725     C*
033000030613     C                   if        %open(tis7pxp)
033100030613     C                   close     tis7pxp
033200000725     C                   endif
033300000725     C*
033400000725     C                   ENDSR
033500000725     C***
033600990920
033700990910
033800000613     C     *inzsr        BEGSR
033900990910     C*
034000990910     C     *entry        plist
034100990920     C                   parm                    tivlrds
034200990921     C                   parm      wrkesito      esito
034300000724     C                   parm                    prmlit
034400000724     C                   parm                    prmfir
034500000613     C*
034600000613     C                   ENDSR
034700000613     C***
034800000725
034900030613     Otis7pxp   E            riepilogo         2
035000000725     O                                              'Upload via FTP'
035100000725     O                                           +1 'Traduzione TIVIN00R -'
035200040203     O                                           +1 'WWRCE00F'
035300000725     O                                           +1 'Entrate'
035400000725     O          E            riepilogo   2
035500000725     O                       wrkdata
035600000725     O                       wrkora              +1
035700000725     O                       procname            +1
035800000725     O          E            riepilogo   2
035900000725     O                                              'Cliente..................:'
036000030613     O                       VLRKSC              +1
036100000725     O          E            riepilogo   2
036200000725     O                                              'Riferimento Strategi.....:'
036300000725     O                       vlrhdl              +1
036400000725     O          E            riepilogo   2
036500000725     O                                              'Giusti...................:'
036600000725     O                       §CTROK        2   +  1
036700000725     O          E            riepilogo   2
036800000725     O                                              'Sbagliati e scartati.....:'
036900000725     O                       §CTRNO        2   +  1
037000990908
037100000724
037200030613     Otis7pxps  E            testdett          2
037300000724     O                                              'Upload via FTP'
037400000613     O                                           +1 'Traduzione TIVIN00R -'
037500040203     O                                           +1 'WWRCE00F'
037600000725     O                                           +1 'Entrate'
037700000616     O          E            testdett    3
037800000613     O                                           +2 'N° rec'
037900000613     O                                           +3 'Anteprima contenuto'
038000000616     O          E            rigadett    2
038100000613     O                       rrnum               +2
038200000621     O                       recko               +3
038300000616     O          E            findett     2
038400000613     O                       wrkdata
038500000613     O                       wrkora              +1
038600000613     O                       procname            +1
038700000616     O          E            findett     2
038800000613     O                                              'Cliente..................:'
038900030613     O                       VLRKSC              +1
039000000616     O          E            findett     2
039100000613     O                                              'Riferimento Strategi.....:'
039200000613     O                       vlrhdl              +1
039300000616     O          E            findett     2
039400000613     O                                              'Giusti...................:'
039500000613     O                       §CTROK        2   +  1
039600000616     O          E            findett     2
039700000613     O                                              'Sbagliati e scartati.....:'
039800000613     O                       §CTRNO        2   +  1
039900000512** CMD - COMANDI CL
040000030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
040100030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
040200030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
040300040203CPYF FROMFILE(WWRCE00F) TOFILE(
040400070913/
040500070913) FROMMBR(
040600070913) MBROPT(*ADD) FMTOPT(*NOCHK)
040700040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040800040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
