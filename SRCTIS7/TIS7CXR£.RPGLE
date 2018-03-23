000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300150609
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
006700100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL1' OR
006800100408     C                             %subst(wNomeFile:1:3) = 'OL1'
006900090112     C                   EVAL      CurrOpz     = 'GO'
007000100408     C                   EVAL      CurrFileAS  = 'WP3OL1P'
007100100408     C                   EVAL      CurrTip     = 'C1'
007200090109     C                   ENDIF
007300100408     C*
007400100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL2' OR
007500100408     C                             %subst(wNomeFile:1:3) = 'OL2'
007600090112     C                   EVAL      CurrOpz     = 'GO'
007700100408     C                   EVAL      CurrFileAS  = 'WP3OL2P'
007800100408     C                   EVAL      CurrTip     = 'C2'
007900090109     C                   ENDIF
008000100408     C*
008100100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL3' OR
008200100408     C                             %subst(wNomeFile:1:3) = 'OL3'
008300090112     C                   EVAL      CurrOpz     = 'GO'
008400100408     C                   EVAL      CurrFileAS  = 'WP3OL3P'
008500100408     C                   EVAL      CurrTip     = 'C3'
008600090112     C                   ENDIF
008700100408     C*
008800100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL4' OR
008900100408     C                             %subst(wNomeFile:1:3) = 'OL4'
009000090112     C                   EVAL      CurrOpz     = 'GO'
009100100408     C                   EVAL      CurrFileAS  = 'WP3OL4P'
009200100408     C                   EVAL      CurrTip     = 'C4'
009300090109     C                   ENDIF
009400100408     C*
009500100408     C                   IF        %subst(wNomeFile:1:6) = 'XX3OL5' OR
009600100408     C                             %subst(wNomeFile:1:3) = 'OL5'
009700100408     C                   EVAL      CurrOpz     = 'GO'
009800100408     C                   EVAL      CurrFileAS  = 'WP3OL5P'
009900100408     C                   EVAL      CurrTip     = 'C5'
010000100408     C                   ENDIF
010100100524     C*
010200100524     C                   IF        %subst(wNomeFile:1:6) = 'XX3OLX' OR
010300100524     C                             %subst(wNomeFile:1:3) = 'OLX'
010400100524     C                   EVAL      CurrOpz     = 'GO'
010500100524     C                   EVAL      CurrFileAS  = 'WP3OLXP'
010600100524     C                   EVAL      CurrTip     = 'CX'
010700100524     C                   ENDIF
010800100615     C*
010900100615     C* Personalizzazioni
011000100615     C                   SELECT
011100100615     C*
011200100615     C* - CAMILLATEX - 16 CAMILLAT
011300100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
011400100615     C*
011500100615     C                   IF        %subst(wNomeFile:1:3) = 'AOU'
011600100615     C                   EVAL      CurrOpz     = 'GO'
011700100615     C                   EVAL      CurrFileAS  = 'WP3AOUP'
011800100615     C                   EVAL      CurrTip     = 'CX'
011900100615     C                   ENDIF
012000100615     C*
012100100615     C                   ENDSL
012200090109     C*
012300090109     C                   ELSE
012400090112     C                   EVAL      CurrOpz     = 'GO'
012500090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
012600100623     C                   EVAL      CurrTip     = vlrtip
012700090109     C                   ENDIF
012800090109     C*
012900090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
013000090109     C                   select
013100090109     C                   when      CurrOpz = *blanks
013200090109     C                   eval      wrkesito = '2'
013300090109     C                   when      CurrOpz = 'GO'
013400090109     C*
013500090109     C* OPERAZIONI INIZIALI
013600090109     C                   exsr      PREELA
013700000613     C*
013800090109     C* ELABORAZIONE
013900000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
014000000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014100090109     C*
014200090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
014300090129     C                   if        CurrPostPgm <> *blanks AND
014400090129     C                             CurrPostPgm <> '*NONE'
014500090129     C                   if        CurrPostLib = *blanks
014600090129     C                   if        CurrEnv = 'T'
014700150609     C                   eval      CurrPostLib = 'GEOV72SBT'
014800090129     C                   else
014900150609     C                   eval      CurrPostLib = 'GEOV72SB'
015000090129     C                   endif
015100090129     C                   endif
015200090121     C*
015300100408     C                   call      'TIS7CXC'
015400090129     C                   parm                    CurrPostLib
015500090129     C                   parm                    CurrPostPgm
015600090121     C                   parm                    CurrEnv
015700090121     C                   parm                    CurrSito
015800090121     C                   parm                    CurrDepot
015900090121     C                   parm                    CurrTip
016000090121     C                   parm                    wrkesito
016100100623     C*
016200100623     C                   if        wrkesito = *blanks
016300100623     C                   eval      wrkesito = '0'
016400100623     C                   endif
016500100623     C*
016600090112     C                   endif
016700090121     C*
016800090121     C* OPERAZIONI FINALI
016900090121     C                   exsr      ENDELA
017000090112     C*
017100090109     C                   endsl
017200000616     C*
017300090109     C                   seton                                        lr
017400090109
017500090109
017600090109     C*--------------------------------------------------------
017700090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
017800090109     C*--------------------------------------------------------
017900090109     C     PREELA        BEGSR
018000090109     C*
018100090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018200090109     C                   movel(p)  CMD(1)        depcmd
018300090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
018400090109     C*
018500090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
018600090109     C                   Z-ADD     100           LENGH            15 5
018700090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018800090109     C                   PARM                    depcmd
018900090109     C                   PARM                    LENGH
019000090109     C*
019100090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
019200090109     C                   movel(p)  CMD(2)        depcmd
019300090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
019400090109     C*
019500090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
019600090109     C                   Z-ADD     100           LENGH            15 5
019700090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019800090109     C                   PARM                    depcmd
019900090109     C                   PARM                    LENGH
020000090109     C*
020100090109     C                   if        not %open(tis7pxps)
020200090109     C                   open      tis7pxps
020300090109     C                   except    testdett
020400090109     C                   endif
020500090109     C*
020600090109     C                   ENDSR
020700090109     C***
020800090109
020900090109
021000090109     C*--------------------------------------------------------
021100090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
021200090109     C*--------------------------------------------------------
021300090109     C     ENDELA        BEGSR
021400090109     C*
021500090109     C                   if        %open(tis7pxps)
021600090109     C                   except    findett
021700090109     C                   close     tis7pxps
021800090109     C                   endif
021900090109     C*
022000090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
022100090109     C                   movel(p)  CMD(3)        depcmd
022200090109     C                   Z-ADD     100           LENGH            15 5
022300090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
022400090109     C                   PARM                    depcmd
022500090109     C                   PARM                    LENGH
022600090109     C*
022700090109     C                   ENDSR
022800090109     C***
022900090109
023000090109
023100000613
023200000724
023300910830     C*--------------------------------------------------------
023400040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
023500910830     C*--------------------------------------------------------
023600000724     C     RWFIL         BEGSR
023700990910     C*
023800990914     C                   if        not %open(tivin00r)
023900990908     C                   open      tivin00r
024000990914     C                   endif
024100040203     C                   if        not %open(wwrce00f)
024200040203     C                   open      wwrce00f
024300990914     C                   endif
024400990910     C*
024500990910     C                   clear                   §CTROK
024600990910     C                   clear                   §CTRNO
024700001009     C                   clear                   recko
024800990910     C*
024900921023     C                   DO        *HIVAL
025000990913     C*
025100990915     C                   READ      tivin00r                               70
025200000905     C                   if        vindta > *blanks
025300000613     C                   add       1             rrnum
025400000724     C                   if        *in70 = *off and
025500990913     C                             (vinflg = *blanks
025600990913     C                              or vinflg = '0'
025700990913     C                              or vinflg = '2')
025800990913     C*
025900990913     C                   clear                   vinmsg
026000000724     C                   if        vindta <> *blanks
026100000724     C                   movel(p)  vindta        passaggio
026200040203     C                   write(e)  WWRCE
026300990910     C*
026400000724     C                   if        not %error
026500000724     C                   add       1             §ctrok
026600990910     C                   eval      vinflg = '1'
026700990910     C                   else
026800000724     C                   add       1             §ctrno
026900000621     C                   eval      recko = vindta
027000000616     C                   except    rigadett
027100990910     C                   eval      vinflg = '2'
027200990910     C                   endif
027300000724     C                   endif
027400991022     C                   endif
027500000905     C*
027600000905     C                   else
027700000905     C                   eval      vinflg = '1'
027800000905     C                   endif
027900000905     C*
028000000905     C  N70              update    tivin000
028100991022     C*
028200991022     C  N70              ENDdo
028300990910
028400990910     C* Se non ci sono record con errori ...
028500000710     C                   if        §ctrno = 0
028600990910     C* ... restituisco esito OK.
028700990921     C                   eval      wrkesito = '0'
028800990910     C                   else
028900000710     C                   if        §ctrok > 0
029000990921     C                   eval      wrkesito = '1'
029100000710     C                   else
029200000710     C                   eval      wrkesito = '2'
029300990910     C                   endif
029400000710     C                   endif
029500990910     C*
029600990914     C                   if        %open(tivin00r)
029700990908     C                   close     tivin00r
029800990914     C                   endif
029900040203     C                   if        %open(wwrce00f)
030000040203     C                   close     wwrce00f
030100990914     C                   endif
030200990920     C*
030300000724     C                   if        wrkesito <> '2'
030400000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
030500070913     C                   movel(p)  CMD(8)        depcmd
030600000724     C                   eval      depcmd = %trim(depcmd) + ' '+
030700000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
030800000724     C                   Z-ADD     100           LENGH            15 5
030900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031000000724     C                   PARM                    depcmd
031100000724     C                   PARM                    LENGH
031200000724     C*
031300070913     C                   movel(p)  CMD(9)        depcmd
031400000724     C                   eval      depcmd = %trim(depcmd)
031500000724     C                   Z-ADD     100           LENGH            15 5
031600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031700000724     C                   PARM                    depcmd
031800000724     C                   PARM                    LENGH
031900000724     C*
032000000724     C                   movel(p)  CMD(4)        depcmd
032100000724     C                   eval      depcmd = %trim(depcmd) +
032200090109     C                             %trim(prmlit) + %trim(CMD(5)) +
032300090109     C                             %trim(CurrFileAS) +
032400070913     C                             %trim(CMD(6)) +
032500000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
032600070913     C                             %trim(CMD(7))
032700000724     C                   Z-ADD     100           LENGH            15 5
032800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
032900000724     C                   PARM                    depcmd
033000000724     C                   PARM                    LENGH
033100000724     C*
033200000724     C                   endif
033300000724     C*
033400910830     C                   ENDSR
033500000613     C***
033600000725
033700000725
033800000725     C*--------------------------------------------------------
033900000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
034000000725     C*--------------------------------------------------------
034100000725     C     STPR          BEGSR
034200000725     C*
034300030613     C                   if        not %open(tis7pxp)
034400030613     C                   open      tis7pxp
034500000725     C                   endif
034600000725     C*
034700000725     C                   except    riepilogo
034800000725     C*
034900030613     C                   if        %open(tis7pxp)
035000030613     C                   close     tis7pxp
035100000725     C                   endif
035200000725     C*
035300000725     C                   ENDSR
035400000725     C***
035500990920
035600990910
035700000613     C     *inzsr        BEGSR
035800990910     C*
035900990910     C     *entry        plist
036000990920     C                   parm                    tivlrds
036100990921     C                   parm      wrkesito      esito
036200000724     C                   parm                    prmlit
036300000724     C                   parm                    prmfir
036400000613     C*
036500000613     C                   ENDSR
036600000613     C***
036700000725
036800030613     Otis7pxp   E            riepilogo         2
036900000725     O                                              'Upload via FTP'
037000000725     O                                           +1 'Traduzione TIVIN00R -'
037100040203     O                                           +1 'WWRCE00F'
037200000725     O                                           +1 'Entrate'
037300000725     O          E            riepilogo   2
037400000725     O                       wrkdata
037500000725     O                       wrkora              +1
037600000725     O                       procname            +1
037700000725     O          E            riepilogo   2
037800000725     O                                              'Cliente..................:'
037900030613     O                       VLRKSC              +1
038000000725     O          E            riepilogo   2
038100000725     O                                              'Riferimento Strategi.....:'
038200000725     O                       vlrhdl              +1
038300000725     O          E            riepilogo   2
038400000725     O                                              'Giusti...................:'
038500000725     O                       §CTROK        2   +  1
038600000725     O          E            riepilogo   2
038700000725     O                                              'Sbagliati e scartati.....:'
038800000725     O                       §CTRNO        2   +  1
038900990908
039000000724
039100030613     Otis7pxps  E            testdett          2
039200000724     O                                              'Upload via FTP'
039300000613     O                                           +1 'Traduzione TIVIN00R -'
039400040203     O                                           +1 'WWRCE00F'
039500000725     O                                           +1 'Entrate'
039600000616     O          E            testdett    3
039700000613     O                                           +2 'N° rec'
039800000613     O                                           +3 'Anteprima contenuto'
039900000616     O          E            rigadett    2
040000000613     O                       rrnum               +2
040100000621     O                       recko               +3
040200000616     O          E            findett     2
040300000613     O                       wrkdata
040400000613     O                       wrkora              +1
040500000613     O                       procname            +1
040600000616     O          E            findett     2
040700000613     O                                              'Cliente..................:'
040800030613     O                       VLRKSC              +1
040900000616     O          E            findett     2
041000000613     O                                              'Riferimento Strategi.....:'
041100000613     O                       vlrhdl              +1
041200000616     O          E            findett     2
041300000613     O                                              'Giusti...................:'
041400000613     O                       §CTROK        2   +  1
041500000616     O          E            findett     2
041600000613     O                                              'Sbagliati e scartati.....:'
041700000613     O                       §CTRNO        2   +  1
041800000512** CMD - COMANDI CL
041900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
042000030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
042100030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
042200040203CPYF FROMFILE(WWRCE00F) TOFILE(
042300070913/
042400070913) FROMMBR(
042500070913) MBROPT(*ADD) FMTOPT(*NOCHK)
042600040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
042700040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
