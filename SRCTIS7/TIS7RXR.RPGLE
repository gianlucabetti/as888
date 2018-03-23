000100040203      /TITLE Upload via FTP: traduzione file LED WWRCE00F
000200990908     H dftactgrp(*yes)
000300150610
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
006700120507     C                   IF        %subst(wNomeFile:1:6) = 'XX3SC1' OR
006800120507     C                             %subst(wNomeFile:1:3) = 'SC1'
006900090112     C                   EVAL      CurrOpz     = 'GO'
007000120507     C                   EVAL      CurrFileAS  = 'WP3SC1P'
007100120507     C                   EVAL      CurrTip     = 'R1'
007200090109     C                   ENDIF
007300120507     C*
007400120507     C                   IF        %subst(wNomeFile:1:6) = 'XX3SC2' OR
007500120507     C                             %subst(wNomeFile:1:3) = 'SC2'
007600120507     C                   EVAL      CurrOpz     = 'GO'
007700120507     C                   EVAL      CurrFileAS  = 'WP3SC2P'
007800120507     C                   EVAL      CurrTip     = 'R2'
007900120507     C                   ENDIF
008000100524     C*
008100120507     C                   IF        %subst(wNomeFile:1:6) = 'XX3SCX' OR
008200120507     C                             %subst(wNomeFile:1:3) = 'SCX'
008300100524     C                   EVAL      CurrOpz     = 'GO'
008400120507     C                   EVAL      CurrFileAS  = 'WP3SCXP'
008500120507     C                   EVAL      CurrTip     = 'RX'
008600100524     C                   ENDIF
008700100615     C*
008800100615     C* Personalizzazioni
008900100615     C                   SELECT
009000100615     C*
009100100615     C* - CAMILLATEX - 16 CAMILLAT
009200100615     C                   WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
009300100615     C*
009400120507     C                   IF        %subst(wNomeFile:1:3) = 'XXX'
009500100615     C                   EVAL      CurrOpz     = 'GO'
009600120507     C                   EVAL      CurrFileAS  = 'WP3SCXP'
009700120507     C                   EVAL      CurrTip     = 'RX'
009800100615     C                   ENDIF
009900100615     C*
010000100615     C                   ENDSL
010100090109     C*
010200090109     C                   ELSE
010300090112     C                   EVAL      CurrOpz     = 'GO'
010400090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010500100623     C                   EVAL      CurrTip     = vlrtip
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
013200100408     C                   call      'TIS7CXC'
013300090129     C                   parm                    CurrPostLib
013400090129     C                   parm                    CurrPostPgm
013500090121     C                   parm                    CurrEnv
013600090121     C                   parm                    CurrSito
013700090121     C                   parm                    CurrDepot
013800090121     C                   parm                    CurrTip
013900090121     C                   parm                    wrkesito
014000100623     C*
014100100623     C                   if        wrkesito = *blanks
014200100623     C                   eval      wrkesito = '0'
014300100623     C                   endif
014400100623     C*
014500090112     C                   endif
014600090121     C*
014700090121     C* OPERAZIONI FINALI
014800090121     C                   exsr      ENDELA
014900090112     C*
015000090109     C                   endsl
015100000616     C*
015200090109     C                   seton                                        lr
015300090109
015400090109
015500090109     C*--------------------------------------------------------
015600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015700090109     C*--------------------------------------------------------
015800090109     C     PREELA        BEGSR
015900090109     C*
016000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016100090109     C                   movel(p)  CMD(1)        depcmd
016200090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
016300090109     C*
016400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016500090109     C                   Z-ADD     100           LENGH            15 5
016600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016700090109     C                   PARM                    depcmd
016800090109     C                   PARM                    LENGH
016900090109     C*
017000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017100090109     C                   movel(p)  CMD(2)        depcmd
017200090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
017300090109     C*
017400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017500090109     C                   Z-ADD     100           LENGH            15 5
017600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017700090109     C                   PARM                    depcmd
017800090109     C                   PARM                    LENGH
017900090109     C*
018000090109     C                   if        not %open(tis7pxps)
018100090109     C                   open      tis7pxps
018200090109     C                   except    testdett
018300090109     C                   endif
018400090109     C*
018500090109     C                   ENDSR
018600090109     C***
018700090109
018800090109
018900090109     C*--------------------------------------------------------
019000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
019100090109     C*--------------------------------------------------------
019200090109     C     ENDELA        BEGSR
019300090109     C*
019400090109     C                   if        %open(tis7pxps)
019500090109     C                   except    findett
019600090109     C                   close     tis7pxps
019700090109     C                   endif
019800090109     C*
019900090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020000090109     C                   movel(p)  CMD(3)        depcmd
020100090109     C                   Z-ADD     100           LENGH            15 5
020200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020300090109     C                   PARM                    depcmd
020400090109     C                   PARM                    LENGH
020500090109     C*
020600090109     C                   ENDSR
020700090109     C***
020800090109
020900090109
021000000613
021100000724
021200910830     C*--------------------------------------------------------
021300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
021400910830     C*--------------------------------------------------------
021500000724     C     RWFIL         BEGSR
021600990910     C*
021700990914     C                   if        not %open(tivin00r)
021800990908     C                   open      tivin00r
021900990914     C                   endif
022000040203     C                   if        not %open(wwrce00f)
022100040203     C                   open      wwrce00f
022200990914     C                   endif
022300990910     C*
022400990910     C                   clear                   §CTROK
022500990910     C                   clear                   §CTRNO
022600001009     C                   clear                   recko
022700990910     C*
022800921023     C                   DO        *HIVAL
022900990913     C*
023000990915     C                   READ      tivin00r                               70
023100000905     C                   if        vindta > *blanks
023200000613     C                   add       1             rrnum
023300000724     C                   if        *in70 = *off and
023400990913     C                             (vinflg = *blanks
023500990913     C                              or vinflg = '0'
023600990913     C                              or vinflg = '2')
023700990913     C*
023800990913     C                   clear                   vinmsg
023900000724     C                   if        vindta <> *blanks
024000000724     C                   movel(p)  vindta        passaggio
024100040203     C                   write(e)  WWRCE
024200990910     C*
024300000724     C                   if        not %error
024400000724     C                   add       1             §ctrok
024500990910     C                   eval      vinflg = '1'
024600990910     C                   else
024700000724     C                   add       1             §ctrno
024800000621     C                   eval      recko = vindta
024900000616     C                   except    rigadett
025000990910     C                   eval      vinflg = '2'
025100990910     C                   endif
025200000724     C                   endif
025300991022     C                   endif
025400000905     C*
025500000905     C                   else
025600000905     C                   eval      vinflg = '1'
025700000905     C                   endif
025800000905     C*
025900000905     C  N70              update    tivin000
026000991022     C*
026100991022     C  N70              ENDdo
026200990910
026300990910     C* Se non ci sono record con errori ...
026400000710     C                   if        §ctrno = 0
026500990910     C* ... restituisco esito OK.
026600990921     C                   eval      wrkesito = '0'
026700990910     C                   else
026800000710     C                   if        §ctrok > 0
026900990921     C                   eval      wrkesito = '1'
027000000710     C                   else
027100000710     C                   eval      wrkesito = '2'
027200990910     C                   endif
027300000710     C                   endif
027400990910     C*
027500990914     C                   if        %open(tivin00r)
027600990908     C                   close     tivin00r
027700990914     C                   endif
027800040203     C                   if        %open(wwrce00f)
027900040203     C                   close     wwrce00f
028000990914     C                   endif
028100990920     C*
028200000724     C                   if        wrkesito <> '2'
028300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
028400070913     C                   movel(p)  CMD(8)        depcmd
028500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
028700000724     C                   Z-ADD     100           LENGH            15 5
028800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028900000724     C                   PARM                    depcmd
029000000724     C                   PARM                    LENGH
029100000724     C*
029200070913     C                   movel(p)  CMD(9)        depcmd
029300000724     C                   eval      depcmd = %trim(depcmd)
029400000724     C                   Z-ADD     100           LENGH            15 5
029500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029600000724     C                   PARM                    depcmd
029700000724     C                   PARM                    LENGH
029800000724     C*
029900000724     C                   movel(p)  CMD(4)        depcmd
030000000724     C                   eval      depcmd = %trim(depcmd) +
030100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
030200090109     C                             %trim(CurrFileAS) +
030300070913     C                             %trim(CMD(6)) +
030400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
030500070913     C                             %trim(CMD(7))
030600000724     C                   Z-ADD     100           LENGH            15 5
030700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
030800000724     C                   PARM                    depcmd
030900000724     C                   PARM                    LENGH
031000000724     C*
031100000724     C                   endif
031200000724     C*
031300910830     C                   ENDSR
031400000613     C***
031500000725
031600000725
031700000725     C*--------------------------------------------------------
031800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
031900000725     C*--------------------------------------------------------
032000000725     C     STPR          BEGSR
032100000725     C*
032200030613     C                   if        not %open(tis7pxp)
032300030613     C                   open      tis7pxp
032400000725     C                   endif
032500000725     C*
032600000725     C                   except    riepilogo
032700000725     C*
032800030613     C                   if        %open(tis7pxp)
032900030613     C                   close     tis7pxp
033000000725     C                   endif
033100000725     C*
033200000725     C                   ENDSR
033300000725     C***
033400990920
033500990910
033600000613     C     *inzsr        BEGSR
033700990910     C*
033800990910     C     *entry        plist
033900990920     C                   parm                    tivlrds
034000990921     C                   parm      wrkesito      esito
034100000724     C                   parm                    prmlit
034200000724     C                   parm                    prmfir
034300000613     C*
034400000613     C                   ENDSR
034500000613     C***
034600000725
034700030613     Otis7pxp   E            riepilogo         2
034800000725     O                                              'Upload via FTP'
034900000725     O                                           +1 'Traduzione TIVIN00R -'
035000040203     O                                           +1 'WWRCE00F'
035100000725     O                                           +1 'Entrate'
035200000725     O          E            riepilogo   2
035300000725     O                       wrkdata
035400000725     O                       wrkora              +1
035500000725     O                       procname            +1
035600000725     O          E            riepilogo   2
035700000725     O                                              'Cliente..................:'
035800030613     O                       VLRKSC              +1
035900000725     O          E            riepilogo   2
036000000725     O                                              'Riferimento Strategi.....:'
036100000725     O                       vlrhdl              +1
036200000725     O          E            riepilogo   2
036300000725     O                                              'Giusti...................:'
036400000725     O                       §CTROK        2   +  1
036500000725     O          E            riepilogo   2
036600000725     O                                              'Sbagliati e scartati.....:'
036700000725     O                       §CTRNO        2   +  1
036800990908
036900000724
037000030613     Otis7pxps  E            testdett          2
037100000724     O                                              'Upload via FTP'
037200000613     O                                           +1 'Traduzione TIVIN00R -'
037300040203     O                                           +1 'WWRCE00F'
037400000725     O                                           +1 'Entrate'
037500000616     O          E            testdett    3
037600000613     O                                           +2 'N° rec'
037700000613     O                                           +3 'Anteprima contenuto'
037800000616     O          E            rigadett    2
037900000613     O                       rrnum               +2
038000000621     O                       recko               +3
038100000616     O          E            findett     2
038200000613     O                       wrkdata
038300000613     O                       wrkora              +1
038400000613     O                       procname            +1
038500000616     O          E            findett     2
038600000613     O                                              'Cliente..................:'
038700030613     O                       VLRKSC              +1
038800000616     O          E            findett     2
038900000613     O                                              'Riferimento Strategi.....:'
039000000613     O                       vlrhdl              +1
039100000616     O          E            findett     2
039200000613     O                                              'Giusti...................:'
039300000613     O                       §CTROK        2   +  1
039400000616     O          E            findett     2
039500000613     O                                              'Sbagliati e scartati.....:'
039600000613     O                       §CTRNO        2   +  1
039700000512** CMD - COMANDI CL
039800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
039900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
040000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
040100040203CPYF FROMFILE(WWRCE00F) TOFILE(
040200070913/
040300070913) FROMMBR(
040400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
040500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
