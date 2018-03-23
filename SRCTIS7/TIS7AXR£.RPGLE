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
004000101007     D wPosDaA         s              2    INZ(*blanks)
004100101007     D wPosDa          s              2  0 INZ(*zeros)
004200090109     D*-------------------
004300090109     D* COSTANTI
004400090109     D*-------------------
004500090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004600090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004700990908
004800990915     C                   time                    wrkdata
004900990915     C                   time                    wrkora
005000000913     C                   reset                   rrnum
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300090109     C*
005400090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005500090121     C                   EVAL      CurrOpz     = *blanks
005600090121     C                   EVAL      CurrFileAS  = *blanks
005700090129     C                   EVAL      CurrPostPgm = *blanks
005800090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
005900100408     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
006000090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
006100090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
006200090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
006300101007     C*
006400101007     C* Reperisco subito il nome del file "UPLOADATO" (se c'è') e lo porto in maiuscolo
006500101007     C                   MOVE(P)   vlrMSG        wNomeFile
006600101007     C     minu:maiu     XLATE     wNomeFile     wNomeFile
006700090121     C*
006800090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006900100408     C*
007000100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR1' OR
007100100504     C                             %subst(wNomeFile:1:3) = 'AR1'
007200090112     C                   EVAL      CurrOpz     = 'GO'
007300100504     C                   EVAL      CurrFileAS  = 'WP3AR1P'
007400100504     C                   EVAL      CurrTip     = 'A1'
007500090109     C                   ENDIF
007600100408     C*
007700100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR2' OR
007800100504     C                             %subst(wNomeFile:1:3) = 'AR2'
007900090112     C                   EVAL      CurrOpz     = 'GO'
008000100504     C                   EVAL      CurrFileAS  = 'WP3AR2P'
008100100504     C                   EVAL      CurrTip     = 'A2'
008200090109     C                   ENDIF
008300100408     C*
008400100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR3' OR
008500100504     C                             %subst(wNomeFile:1:3) = 'AR3'
008600090112     C                   EVAL      CurrOpz     = 'GO'
008700100504     C                   EVAL      CurrFileAS  = 'WP3AR3P'
008800100504     C                   EVAL      CurrTip     = 'A3'
008900090112     C                   ENDIF
009000100408     C*
009100100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR4' OR
009200100504     C                             %subst(wNomeFile:1:3) = 'AR4'
009300090112     C                   EVAL      CurrOpz     = 'GO'
009400100504     C                   EVAL      CurrFileAS  = 'WP3AR4P'
009500100504     C                   EVAL      CurrTip     = 'A4'
009600090109     C                   ENDIF
009700100408     C*
009800100504     C                   IF        %subst(wNomeFile:1:6) = 'XX3AR5' OR
009900100504     C                             %subst(wNomeFile:1:3) = 'AR5'
010000100408     C                   EVAL      CurrOpz     = 'GO'
010100100531     C                   EVAL      CurrFileAS  = 'WP3AR50P'
010200100504     C                   EVAL      CurrTip     = 'A5'
010300100408     C                   ENDIF
010400100601     C*
010500100601     C                   IF        %subst(wNomeFile:1:6) = 'XX3ARX' OR
010600100601     C                             %subst(wNomeFile:1:3) = 'ARX'
010700100601     C                   EVAL      CurrOpz     = 'GO'
010800100601     C                   EVAL      CurrFileAS  = 'WP3ARXP'
010900100601     C                   EVAL      CurrTip     = 'AX'
011000100601     C                   ENDIF
011100100615     C*
011200100615     C* Personalizzazioni
011300100615     C                   SELECT
011400100615     C*
011500100615     C* - CAMILLATEX - 16 CAMILLAT
011600100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
011700100615     C*
011800100615     C                   IF        %subst(wNomeFile:1:3) = 'ART'
011900100615     C                   EVAL      CurrOpz     = 'GO'
012000100615     C                   EVAL      CurrFileAS  = 'WP3CARP'
012100100615     C                   EVAL      CurrTip     = 'AX'
012200100615     C                   ENDIF
012300100615     C*
012400100615     C                   ENDSL
012500090109     C*
012600090109     C                   ELSE
012700090112     C                   EVAL      CurrOpz     = 'GO'
012800090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
012900101007     C*
013000101007     C                   IF        %subst(vlrppt:42:2) <> *blanks
013100101007     C                   EVAL      wPosDaA = %subst(vlrppt:42:2)
013200101007     C                   MOVE(P)   wPosDaA       wPosDa
013300101007     C                   EVAL      CurrTip=%subst(wNomeFile:wPosDa:2)
013400101007     C                   ELSE
013500101007     C                   EVAL      CurrTip     = vlrtip
013600101007     C                   ENDIF
013700101007     C*
013800090109     C                   ENDIF
013900090109     C*
014000090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
014100090109     C                   select
014200090109     C                   when      CurrOpz = *blanks
014300090109     C                   eval      wrkesito = '2'
014400090109     C                   when      CurrOpz = 'GO'
014500090109     C*
014600090109     C* OPERAZIONI INIZIALI
014700090109     C                   exsr      PREELA
014800000613     C*
014900090109     C* ELABORAZIONE
015000000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
015100000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015200090109     C*
015300090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
015400090129     C                   if        CurrPostPgm <> *blanks AND
015500090129     C                             CurrPostPgm <> '*NONE'
015600090129     C                   if        CurrPostLib = *blanks
015700090129     C                   if        CurrEnv = 'T'
015800150609     C                   eval      CurrPostLib = 'GEOV72SBT'
015900090129     C                   else
016000150609     C                   eval      CurrPostLib = 'GEOV72SB'
016100090129     C                   endif
016200090129     C                   endif
016300090121     C*
016400100504     C                   call      'TIS7AXC'
016500090129     C                   parm                    CurrPostLib
016600090129     C                   parm                    CurrPostPgm
016700090121     C                   parm                    CurrEnv
016800090121     C                   parm                    CurrSito
016900090121     C                   parm                    CurrDepot
017000090121     C                   parm                    CurrTip
017100090121     C                   parm                    wrkesito
017200100623     C*
017300100623     C                   if        wrkesito = *blanks
017400100623     C                   eval      wrkesito = '0'
017500100623     C                   endif
017600100623     C*
017700090112     C                   endif
017800090121     C*
017900090121     C* OPERAZIONI FINALI
018000090121     C                   exsr      ENDELA
018100090112     C*
018200090109     C                   endsl
018300000616     C*
018400090109     C                   seton                                        lr
018500090109
018600090109
018700090109     C*--------------------------------------------------------
018800090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
018900090109     C*--------------------------------------------------------
019000090109     C     PREELA        BEGSR
019100090109     C*
019200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
019300090109     C                   movel(p)  CMD(1)        depcmd
019400090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
019500090109     C*
019600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
019700090109     C                   Z-ADD     100           LENGH            15 5
019800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019900090109     C                   PARM                    depcmd
020000090109     C                   PARM                    LENGH
020100090109     C*
020200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
020300090109     C                   movel(p)  CMD(2)        depcmd
020400090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
020500090109     C*
020600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
020700090109     C                   Z-ADD     100           LENGH            15 5
020800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020900090109     C                   PARM                    depcmd
021000090109     C                   PARM                    LENGH
021100090109     C*
021200090109     C                   if        not %open(tis7pxps)
021300090109     C                   open      tis7pxps
021400090109     C                   except    testdett
021500090109     C                   endif
021600090109     C*
021700090109     C                   ENDSR
021800090109     C***
021900090109
022000090109
022100090109     C*--------------------------------------------------------
022200090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
022300090109     C*--------------------------------------------------------
022400090109     C     ENDELA        BEGSR
022500090109     C*
022600090109     C                   if        %open(tis7pxps)
022700090109     C                   except    findett
022800090109     C                   close     tis7pxps
022900090109     C                   endif
023000090109     C*
023100090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
023200090109     C                   movel(p)  CMD(3)        depcmd
023300090109     C                   Z-ADD     100           LENGH            15 5
023400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023500090109     C                   PARM                    depcmd
023600090109     C                   PARM                    LENGH
023700090109     C*
023800090109     C                   ENDSR
023900090109     C***
024000090109
024100090109
024200000613
024300000724
024400910830     C*--------------------------------------------------------
024500040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
024600910830     C*--------------------------------------------------------
024700000724     C     RWFIL         BEGSR
024800990910     C*
024900990914     C                   if        not %open(tivin00r)
025000990908     C                   open      tivin00r
025100990914     C                   endif
025200040203     C                   if        not %open(wwrce00f)
025300040203     C                   open      wwrce00f
025400990914     C                   endif
025500990910     C*
025600990910     C                   clear                   §CTROK
025700990910     C                   clear                   §CTRNO
025800001009     C                   clear                   recko
025900990910     C*
026000921023     C                   DO        *HIVAL
026100990913     C*
026200990915     C                   READ      tivin00r                               70
026300000905     C                   if        vindta > *blanks
026400000613     C                   add       1             rrnum
026500000724     C                   if        *in70 = *off and
026600990913     C                             (vinflg = *blanks
026700990913     C                              or vinflg = '0'
026800990913     C                              or vinflg = '2')
026900990913     C*
027000990913     C                   clear                   vinmsg
027100000724     C                   if        vindta <> *blanks
027200000724     C                   movel(p)  vindta        passaggio
027300040203     C                   write(e)  WWRCE
027400990910     C*
027500000724     C                   if        not %error
027600000724     C                   add       1             §ctrok
027700990910     C                   eval      vinflg = '1'
027800990910     C                   else
027900000724     C                   add       1             §ctrno
028000000621     C                   eval      recko = vindta
028100000616     C                   except    rigadett
028200990910     C                   eval      vinflg = '2'
028300990910     C                   endif
028400000724     C                   endif
028500991022     C                   endif
028600000905     C*
028700000905     C                   else
028800000905     C                   eval      vinflg = '1'
028900000905     C                   endif
029000000905     C*
029100000905     C  N70              update    tivin000
029200991022     C*
029300991022     C  N70              ENDdo
029400990910
029500990910     C* Se non ci sono record con errori ...
029600000710     C                   if        §ctrno = 0
029700990910     C* ... restituisco esito OK.
029800990921     C                   eval      wrkesito = '0'
029900990910     C                   else
030000000710     C                   if        §ctrok > 0
030100990921     C                   eval      wrkesito = '1'
030200000710     C                   else
030300000710     C                   eval      wrkesito = '2'
030400990910     C                   endif
030500000710     C                   endif
030600990910     C*
030700990914     C                   if        %open(tivin00r)
030800990908     C                   close     tivin00r
030900990914     C                   endif
031000040203     C                   if        %open(wwrce00f)
031100040203     C                   close     wwrce00f
031200990914     C                   endif
031300990920     C*
031400000724     C                   if        wrkesito <> '2'
031500000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
031600070913     C                   movel(p)  CMD(8)        depcmd
031700000724     C                   eval      depcmd = %trim(depcmd) + ' '+
031800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
031900000724     C                   Z-ADD     100           LENGH            15 5
032000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
032100000724     C                   PARM                    depcmd
032200000724     C                   PARM                    LENGH
032300000724     C*
032400070913     C                   movel(p)  CMD(9)        depcmd
032500000724     C                   eval      depcmd = %trim(depcmd)
032600000724     C                   Z-ADD     100           LENGH            15 5
032700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
032800000724     C                   PARM                    depcmd
032900000724     C                   PARM                    LENGH
033000000724     C*
033100000724     C                   movel(p)  CMD(4)        depcmd
033200000724     C                   eval      depcmd = %trim(depcmd) +
033300090109     C                             %trim(prmlit) + %trim(CMD(5)) +
033400090109     C                             %trim(CurrFileAS) +
033500070913     C                             %trim(CMD(6)) +
033600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
033700070913     C                             %trim(CMD(7))
033800000724     C                   Z-ADD     100           LENGH            15 5
033900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
034000000724     C                   PARM                    depcmd
034100000724     C                   PARM                    LENGH
034200000724     C*
034300000724     C                   endif
034400000724     C*
034500910830     C                   ENDSR
034600000613     C***
034700000725
034800000725
034900000725     C*--------------------------------------------------------
035000000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
035100000725     C*--------------------------------------------------------
035200000725     C     STPR          BEGSR
035300000725     C*
035400030613     C                   if        not %open(tis7pxp)
035500030613     C                   open      tis7pxp
035600000725     C                   endif
035700000725     C*
035800000725     C                   except    riepilogo
035900000725     C*
036000030613     C                   if        %open(tis7pxp)
036100030613     C                   close     tis7pxp
036200000725     C                   endif
036300000725     C*
036400000725     C                   ENDSR
036500000725     C***
036600990920
036700990910
036800000613     C     *inzsr        BEGSR
036900990910     C*
037000990910     C     *entry        plist
037100990920     C                   parm                    tivlrds
037200990921     C                   parm      wrkesito      esito
037300000724     C                   parm                    prmlit
037400000724     C                   parm                    prmfir
037500000613     C*
037600000613     C                   ENDSR
037700000613     C***
037800000725
037900030613     Otis7pxp   E            riepilogo         2
038000000725     O                                              'Upload via FTP'
038100000725     O                                           +1 'Traduzione TIVIN00R -'
038200040203     O                                           +1 'WWRCE00F'
038300000725     O                                           +1 'Entrate'
038400000725     O          E            riepilogo   2
038500000725     O                       wrkdata
038600000725     O                       wrkora              +1
038700000725     O                       procname            +1
038800000725     O          E            riepilogo   2
038900000725     O                                              'Cliente..................:'
039000030613     O                       VLRKSC              +1
039100000725     O          E            riepilogo   2
039200000725     O                                              'Riferimento Strategi.....:'
039300000725     O                       vlrhdl              +1
039400000725     O          E            riepilogo   2
039500000725     O                                              'Giusti...................:'
039600000725     O                       §CTROK        2   +  1
039700000725     O          E            riepilogo   2
039800000725     O                                              'Sbagliati e scartati.....:'
039900000725     O                       §CTRNO        2   +  1
040000990908
040100000724
040200030613     Otis7pxps  E            testdett          2
040300000724     O                                              'Upload via FTP'
040400000613     O                                           +1 'Traduzione TIVIN00R -'
040500040203     O                                           +1 'WWRCE00F'
040600000725     O                                           +1 'Entrate'
040700000616     O          E            testdett    3
040800000613     O                                           +2 'N° rec'
040900000613     O                                           +3 'Anteprima contenuto'
041000000616     O          E            rigadett    2
041100000613     O                       rrnum               +2
041200000621     O                       recko               +3
041300000616     O          E            findett     2
041400000613     O                       wrkdata
041500000613     O                       wrkora              +1
041600000613     O                       procname            +1
041700000616     O          E            findett     2
041800000613     O                                              'Cliente..................:'
041900030613     O                       VLRKSC              +1
042000000616     O          E            findett     2
042100000613     O                                              'Riferimento Strategi.....:'
042200000613     O                       vlrhdl              +1
042300000616     O          E            findett     2
042400000613     O                                              'Giusti...................:'
042500000613     O                       §CTROK        2   +  1
042600000616     O          E            findett     2
042700000613     O                                              'Sbagliati e scartati.....:'
042800000613     O                       §CTRNO        2   +  1
042900000512** CMD - COMANDI CL
043000030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
043100030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
043200030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
043300040203CPYF FROMFILE(WWRCE00F) TOFILE(
043400070913/
043500070913) FROMMBR(
043600070913) MBROPT(*ADD) FMTOPT(*NOCHK)
043700040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
043800040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
