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
007000130206     C                   IF        %subst(wNomeFile:1:5) = 'ESITI'
007100090112     C                   EVAL      CurrOpz     = 'GO'
007200130206     C                   EVAL      CurrFileAS  = 'WP3ECTP'
007300130206     C                   EVAL      CurrTip     = 'IC'
007400090109     C                   ENDIF
007500130206     C*
007600130206     C                   IF        %subst(wNomeFile:1:7) = 'X_ESITI'
007700130206     C                   EVAL      CurrOpz     = 'GO'
007800130206     C                   EVAL      CurrFileAS  = 'WP3ECTP'
007900130206     C                   EVAL      CurrTip     = 'IC'
008000130206     C                   EVAL      CurrEnv     = 'T'
008100130206     C                   ENDIF
008200100615     C*
008300100615     C* Personalizzazioni
008400130206     C***                SELECT
008500100615     C*
008600130206     C* - CAMILLATEX - 16 CAMILLAT
008700130206     C***                WHEN      %trim(CurrSito+CurrDepot) = '16CAMILLAT'
008800100615     C*
008900130206     C***                IF        %subst(wNomeFile:1:3) = 'ART'
009000130206     C***                EVAL      CurrOpz     = 'GO'
009100130206     C***                EVAL      CurrFileAS  = 'WP3ECTP'
009200130206     C***                EVAL      CurrTip     = 'IC'
009300130206     C***                ENDIF
009400100615     C*
009500130206     C***                ENDSL
009600090109     C*
009700090109     C                   ELSE
009800090112     C                   EVAL      CurrOpz     = 'GO'
009900090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
010000101007     C*
010100101007     C                   IF        %subst(vlrppt:42:2) <> *blanks
010200101007     C                   EVAL      wPosDaA = %subst(vlrppt:42:2)
010300101007     C                   MOVE(P)   wPosDaA       wPosDa
010400101007     C                   EVAL      CurrTip=%subst(wNomeFile:wPosDa:2)
010500101007     C                   ELSE
010600101007     C                   EVAL      CurrTip     = vlrtip
010700101007     C                   ENDIF
010800101007     C*
010900090109     C                   ENDIF
011000090109     C*
011100090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
011200090109     C                   select
011300090109     C                   when      CurrOpz = *blanks
011400090109     C                   eval      wrkesito = '2'
011500090109     C                   when      CurrOpz = 'GO'
011600090109     C*
011700090109     C* OPERAZIONI INIZIALI
011800090109     C                   exsr      PREELA
011900000613     C*
012000090109     C* ELABORAZIONE
012100000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
012200000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012300090109     C*
012400090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
012500090129     C                   if        CurrPostPgm <> *blanks AND
012600090129     C                             CurrPostPgm <> '*NONE'
012700090129     C                   if        CurrPostLib = *blanks
012800090129     C                   if        CurrEnv = 'T'
012900150609     C                   eval      CurrPostLib = 'GEOV72SBT'
013000090129     C                   else
013100150609     C                   eval      CurrPostLib = 'GEOV72SB'
013200090129     C                   endif
013300090129     C                   endif
013400090121     C*
013500100504     C                   call      'TIS7AXC'
013600090129     C                   parm                    CurrPostLib
013700090129     C                   parm                    CurrPostPgm
013800090121     C                   parm                    CurrEnv
013900090121     C                   parm                    CurrSito
014000090121     C                   parm                    CurrDepot
014100090121     C                   parm                    CurrTip
014200090121     C                   parm                    wrkesito
014300100623     C*
014400100623     C                   if        wrkesito = *blanks
014500100623     C                   eval      wrkesito = '0'
014600100623     C                   endif
014700100623     C*
014800090112     C                   endif
014900090121     C*
015000090121     C* OPERAZIONI FINALI
015100090121     C                   exsr      ENDELA
015200090112     C*
015300090109     C                   endsl
015400000616     C*
015500090109     C                   seton                                        lr
015600090109
015700090109
015800090109     C*--------------------------------------------------------
015900090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
016000090109     C*--------------------------------------------------------
016100090109     C     PREELA        BEGSR
016200090109     C*
016300090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016400090109     C                   movel(p)  CMD(1)        depcmd
016500090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
016600090109     C*
016700090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016800090109     C                   Z-ADD     100           LENGH            15 5
016900090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017000090109     C                   PARM                    depcmd
017100090109     C                   PARM                    LENGH
017200090109     C*
017300090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017400090109     C                   movel(p)  CMD(2)        depcmd
017500090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
017600090109     C*
017700090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017800090109     C                   Z-ADD     100           LENGH            15 5
017900090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018000090109     C                   PARM                    depcmd
018100090109     C                   PARM                    LENGH
018200090109     C*
018300090109     C                   if        not %open(tis7pxps)
018400090109     C                   open      tis7pxps
018500090109     C                   except    testdett
018600090109     C                   endif
018700090109     C*
018800090109     C                   ENDSR
018900090109     C***
019000090109
019100090109
019200090109     C*--------------------------------------------------------
019300090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
019400090109     C*--------------------------------------------------------
019500090109     C     ENDELA        BEGSR
019600090109     C*
019700090109     C                   if        %open(tis7pxps)
019800090109     C                   except    findett
019900090109     C                   close     tis7pxps
020000090109     C                   endif
020100090109     C*
020200090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020300090109     C                   movel(p)  CMD(3)        depcmd
020400090109     C                   Z-ADD     100           LENGH            15 5
020500090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020600090109     C                   PARM                    depcmd
020700090109     C                   PARM                    LENGH
020800090109     C*
020900090109     C                   ENDSR
021000090109     C***
021100090109
021200090109
021300000613
021400000724
021500910830     C*--------------------------------------------------------
021600040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
021700910830     C*--------------------------------------------------------
021800000724     C     RWFIL         BEGSR
021900990910     C*
022000990914     C                   if        not %open(tivin00r)
022100990908     C                   open      tivin00r
022200990914     C                   endif
022300040203     C                   if        not %open(wwrce00f)
022400040203     C                   open      wwrce00f
022500990914     C                   endif
022600990910     C*
022700990910     C                   clear                   §CTROK
022800990910     C                   clear                   §CTRNO
022900001009     C                   clear                   recko
023000990910     C*
023100921023     C                   DO        *HIVAL
023200990913     C*
023300990915     C                   READ      tivin00r                               70
023400000905     C                   if        vindta > *blanks
023500000613     C                   add       1             rrnum
023600000724     C                   if        *in70 = *off and
023700990913     C                             (vinflg = *blanks
023800990913     C                              or vinflg = '0'
023900990913     C                              or vinflg = '2')
024000990913     C*
024100990913     C                   clear                   vinmsg
024200000724     C                   if        vindta <> *blanks
024300000724     C                   movel(p)  vindta        passaggio
024400040203     C                   write(e)  WWRCE
024500990910     C*
024600000724     C                   if        not %error
024700000724     C                   add       1             §ctrok
024800990910     C                   eval      vinflg = '1'
024900990910     C                   else
025000000724     C                   add       1             §ctrno
025100000621     C                   eval      recko = vindta
025200000616     C                   except    rigadett
025300990910     C                   eval      vinflg = '2'
025400990910     C                   endif
025500000724     C                   endif
025600991022     C                   endif
025700000905     C*
025800000905     C                   else
025900000905     C                   eval      vinflg = '1'
026000000905     C                   endif
026100000905     C*
026200000905     C  N70              update    tivin000
026300991022     C*
026400991022     C  N70              ENDdo
026500990910
026600990910     C* Se non ci sono record con errori ...
026700000710     C                   if        §ctrno = 0
026800990910     C* ... restituisco esito OK.
026900990921     C                   eval      wrkesito = '0'
027000990910     C                   else
027100000710     C                   if        §ctrok > 0
027200990921     C                   eval      wrkesito = '1'
027300000710     C                   else
027400000710     C                   eval      wrkesito = '2'
027500990910     C                   endif
027600000710     C                   endif
027700990910     C*
027800990914     C                   if        %open(tivin00r)
027900990908     C                   close     tivin00r
028000990914     C                   endif
028100040203     C                   if        %open(wwrce00f)
028200040203     C                   close     wwrce00f
028300990914     C                   endif
028400990920     C*
028500000724     C                   if        wrkesito <> '2'
028600000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
028700070913     C                   movel(p)  CMD(8)        depcmd
028800000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
029000000724     C                   Z-ADD     100           LENGH            15 5
029100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029200000724     C                   PARM                    depcmd
029300000724     C                   PARM                    LENGH
029400000724     C*
029500070913     C                   movel(p)  CMD(9)        depcmd
029600000724     C                   eval      depcmd = %trim(depcmd)
029700000724     C                   Z-ADD     100           LENGH            15 5
029800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029900000724     C                   PARM                    depcmd
030000000724     C                   PARM                    LENGH
030100000724     C*
030200000724     C                   movel(p)  CMD(4)        depcmd
030300000724     C                   eval      depcmd = %trim(depcmd) +
030400090109     C                             %trim(prmlit) + %trim(CMD(5)) +
030500090109     C                             %trim(CurrFileAS) +
030600070913     C                             %trim(CMD(6)) +
030700000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
030800070913     C                             %trim(CMD(7))
030900000724     C                   Z-ADD     100           LENGH            15 5
031000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031100000724     C                   PARM                    depcmd
031200000724     C                   PARM                    LENGH
031300000724     C*
031400000724     C                   endif
031500000724     C*
031600910830     C                   ENDSR
031700000613     C***
031800000725
031900000725
032000000725     C*--------------------------------------------------------
032100000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
032200000725     C*--------------------------------------------------------
032300000725     C     STPR          BEGSR
032400000725     C*
032500030613     C                   if        not %open(tis7pxp)
032600030613     C                   open      tis7pxp
032700000725     C                   endif
032800000725     C*
032900000725     C                   except    riepilogo
033000000725     C*
033100030613     C                   if        %open(tis7pxp)
033200030613     C                   close     tis7pxp
033300000725     C                   endif
033400000725     C*
033500000725     C                   ENDSR
033600000725     C***
033700990920
033800990910
033900000613     C     *inzsr        BEGSR
034000990910     C*
034100990910     C     *entry        plist
034200990920     C                   parm                    tivlrds
034300990921     C                   parm      wrkesito      esito
034400000724     C                   parm                    prmlit
034500000724     C                   parm                    prmfir
034600000613     C*
034700000613     C                   ENDSR
034800000613     C***
034900000725
035000030613     Otis7pxp   E            riepilogo         2
035100000725     O                                              'Upload via FTP'
035200000725     O                                           +1 'Traduzione TIVIN00R -'
035300040203     O                                           +1 'WWRCE00F'
035400000725     O                                           +1 'Entrate'
035500000725     O          E            riepilogo   2
035600000725     O                       wrkdata
035700000725     O                       wrkora              +1
035800000725     O                       procname            +1
035900000725     O          E            riepilogo   2
036000000725     O                                              'Cliente..................:'
036100030613     O                       VLRKSC              +1
036200000725     O          E            riepilogo   2
036300000725     O                                              'Riferimento Strategi.....:'
036400000725     O                       vlrhdl              +1
036500000725     O          E            riepilogo   2
036600000725     O                                              'Giusti...................:'
036700000725     O                       §CTROK        2   +  1
036800000725     O          E            riepilogo   2
036900000725     O                                              'Sbagliati e scartati.....:'
037000000725     O                       §CTRNO        2   +  1
037100990908
037200000724
037300030613     Otis7pxps  E            testdett          2
037400000724     O                                              'Upload via FTP'
037500000613     O                                           +1 'Traduzione TIVIN00R -'
037600040203     O                                           +1 'WWRCE00F'
037700000725     O                                           +1 'Entrate'
037800000616     O          E            testdett    3
037900000613     O                                           +2 'N° rec'
038000000613     O                                           +3 'Anteprima contenuto'
038100000616     O          E            rigadett    2
038200000613     O                       rrnum               +2
038300000621     O                       recko               +3
038400000616     O          E            findett     2
038500000613     O                       wrkdata
038600000613     O                       wrkora              +1
038700000613     O                       procname            +1
038800000616     O          E            findett     2
038900000613     O                                              'Cliente..................:'
039000030613     O                       VLRKSC              +1
039100000616     O          E            findett     2
039200000613     O                                              'Riferimento Strategi.....:'
039300000613     O                       vlrhdl              +1
039400000616     O          E            findett     2
039500000613     O                                              'Giusti...................:'
039600000613     O                       §CTROK        2   +  1
039700000616     O          E            findett     2
039800000613     O                                              'Sbagliati e scartati.....:'
039900000613     O                       §CTRNO        2   +  1
040000000512** CMD - COMANDI CL
040100030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
040200030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
040300030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
040400040203CPYF FROMFILE(WWRCE00F) TOFILE(
040500070913/
040600070913) FROMMBR(
040700070913) MBROPT(*ADD) FMTOPT(*NOCHK)
040800040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040900040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
