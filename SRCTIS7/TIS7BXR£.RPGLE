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
007000150907     C                   IF        %subst(wNomeFile:1:5) = 'XX3BS'
007100140429     C                   EVAL      CurrOpz     = 'GO'
007200140429     C                   EVAL      CurrFileAS  = 'WP3BSLP'
007300140429     C                   EVAL      CurrTip     = 'BX'
007400090109     C                   ENDIF
007500100615     C*
007600100615     C* Personalizzazioni
007700100615     C                   SELECT
007800100615     C*
007900140429     C* - CAMILLATEX - 99 XXXXXXXX
008000140429     C                   WHEN      %trim(CurrSito+CurrDepot) = '99XXXXXXXX'
008100100615     C*
008200100615     C                   IF        %subst(wNomeFile:1:3) = 'ART'
008300100615     C                   EVAL      CurrOpz     = 'GO'
008400140429     C                   EVAL      CurrFileAS  = 'WP3BSLP'
008500140429     C                   EVAL      CurrTip     = 'BX'
008600100615     C                   ENDIF
008700100615     C*
008800100615     C                   ENDSL
008900090109     C*
009000090109     C                   ELSE
009100090112     C                   EVAL      CurrOpz     = 'GO'
009200090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009300101007     C*
009400101007     C                   IF        %subst(vlrppt:42:2) <> *blanks
009500101007     C                   EVAL      wPosDaA = %subst(vlrppt:42:2)
009600101007     C                   MOVE(P)   wPosDaA       wPosDa
009700101007     C                   EVAL      CurrTip=%subst(wNomeFile:wPosDa:2)
009800101007     C                   ELSE
009900101007     C                   EVAL      CurrTip     = vlrtip
010000101007     C                   ENDIF
010100101007     C*
010200090109     C                   ENDIF
010300090109     C*
010400090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010500090109     C                   select
010600090109     C                   when      CurrOpz = *blanks
010700090109     C                   eval      wrkesito = '2'
010800090109     C                   when      CurrOpz = 'GO'
010900090109     C*
011000090109     C* OPERAZIONI INIZIALI
011100090109     C                   exsr      PREELA
011200000613     C*
011300090109     C* ELABORAZIONE
011400000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011500000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011600090109     C*
011700090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011800090129     C                   if        CurrPostPgm <> *blanks AND
011900090129     C                             CurrPostPgm <> '*NONE'
012000090129     C                   if        CurrPostLib = *blanks
012100090129     C                   if        CurrEnv = 'T'
012200150609     C                   eval      CurrPostLib = 'GEOV72SBT'
012300090129     C                   else
012400150609     C                   eval      CurrPostLib = 'GEOV72SB'
012500090129     C                   endif
012600090129     C                   endif
012700090121     C*
012800140429     C                   call      'TIS7BXC'
012900090129     C                   parm                    CurrPostLib
013000090129     C                   parm                    CurrPostPgm
013100090121     C                   parm                    CurrEnv
013200090121     C                   parm                    CurrSito
013300090121     C                   parm                    CurrDepot
013400090121     C                   parm                    CurrTip
013500090121     C                   parm                    wrkesito
013600100623     C*
013700100623     C                   if        wrkesito = *blanks
013800100623     C                   eval      wrkesito = '0'
013900100623     C                   endif
014000100623     C*
014100090112     C                   endif
014200090121     C*
014300090121     C* OPERAZIONI FINALI
014400090121     C                   exsr      ENDELA
014500090112     C*
014600090109     C                   endsl
014700000616     C*
014800090109     C                   seton                                        lr
014900090109
015000090109
015100090109     C*--------------------------------------------------------
015200090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015300090109     C*--------------------------------------------------------
015400090109     C     PREELA        BEGSR
015500090109     C*
015600090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015700090109     C                   movel(p)  CMD(1)        depcmd
015800090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
015900090109     C*
016000090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016100090109     C                   Z-ADD     100           LENGH            15 5
016200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016300090109     C                   PARM                    depcmd
016400090109     C                   PARM                    LENGH
016500090109     C*
016600090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016700090109     C                   movel(p)  CMD(2)        depcmd
016800090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016900090109     C*
017000090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017100090109     C                   Z-ADD     100           LENGH            15 5
017200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017300090109     C                   PARM                    depcmd
017400090109     C                   PARM                    LENGH
017500090109     C*
017600090109     C                   if        not %open(tis7pxps)
017700090109     C                   open      tis7pxps
017800090109     C                   except    testdett
017900090109     C                   endif
018000090109     C*
018100090109     C                   ENDSR
018200090109     C***
018300090109
018400090109
018500090109     C*--------------------------------------------------------
018600090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
018700090109     C*--------------------------------------------------------
018800090109     C     ENDELA        BEGSR
018900090109     C*
019000090109     C                   if        %open(tis7pxps)
019100090109     C                   except    findett
019200090109     C                   close     tis7pxps
019300090109     C                   endif
019400090109     C*
019500090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019600090109     C                   movel(p)  CMD(3)        depcmd
019700090109     C                   Z-ADD     100           LENGH            15 5
019800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019900090109     C                   PARM                    depcmd
020000090109     C                   PARM                    LENGH
020100090109     C*
020200090109     C                   ENDSR
020300090109     C***
020400090109
020500090109
020600000613
020700000724
020800910830     C*--------------------------------------------------------
020900040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
021000910830     C*--------------------------------------------------------
021100000724     C     RWFIL         BEGSR
021200990910     C*
021300990914     C                   if        not %open(tivin00r)
021400990908     C                   open      tivin00r
021500990914     C                   endif
021600040203     C                   if        not %open(wwrce00f)
021700040203     C                   open      wwrce00f
021800990914     C                   endif
021900990910     C*
022000990910     C                   clear                   §CTROK
022100990910     C                   clear                   §CTRNO
022200001009     C                   clear                   recko
022300990910     C*
022400921023     C                   DO        *HIVAL
022500990913     C*
022600990915     C                   READ      tivin00r                               70
022700000905     C                   if        vindta > *blanks
022800000613     C                   add       1             rrnum
022900000724     C                   if        *in70 = *off and
023000990913     C                             (vinflg = *blanks
023100990913     C                              or vinflg = '0'
023200990913     C                              or vinflg = '2')
023300990913     C*
023400990913     C                   clear                   vinmsg
023500000724     C                   if        vindta <> *blanks
023600000724     C                   movel(p)  vindta        passaggio
023700040203     C                   write(e)  WWRCE
023800990910     C*
023900000724     C                   if        not %error
024000000724     C                   add       1             §ctrok
024100990910     C                   eval      vinflg = '1'
024200990910     C                   else
024300000724     C                   add       1             §ctrno
024400000621     C                   eval      recko = vindta
024500000616     C                   except    rigadett
024600990910     C                   eval      vinflg = '2'
024700990910     C                   endif
024800000724     C                   endif
024900991022     C                   endif
025000000905     C*
025100000905     C                   else
025200000905     C                   eval      vinflg = '1'
025300000905     C                   endif
025400000905     C*
025500000905     C  N70              update    tivin000
025600991022     C*
025700991022     C  N70              ENDdo
025800990910
025900990910     C* Se non ci sono record con errori ...
026000000710     C                   if        §ctrno = 0
026100990910     C* ... restituisco esito OK.
026200990921     C                   eval      wrkesito = '0'
026300990910     C                   else
026400000710     C                   if        §ctrok > 0
026500990921     C                   eval      wrkesito = '1'
026600000710     C                   else
026700000710     C                   eval      wrkesito = '2'
026800990910     C                   endif
026900000710     C                   endif
027000990910     C*
027100990914     C                   if        %open(tivin00r)
027200990908     C                   close     tivin00r
027300990914     C                   endif
027400040203     C                   if        %open(wwrce00f)
027500040203     C                   close     wwrce00f
027600990914     C                   endif
027700990920     C*
027800000724     C                   if        wrkesito <> '2'
027900000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
028000070913     C                   movel(p)  CMD(8)        depcmd
028100000724     C                   eval      depcmd = %trim(depcmd) + ' '+
028200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
028300000724     C                   Z-ADD     100           LENGH            15 5
028400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028500000724     C                   PARM                    depcmd
028600000724     C                   PARM                    LENGH
028700000724     C*
028800070913     C                   movel(p)  CMD(9)        depcmd
028900000724     C                   eval      depcmd = %trim(depcmd)
029000000724     C                   Z-ADD     100           LENGH            15 5
029100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029200000724     C                   PARM                    depcmd
029300000724     C                   PARM                    LENGH
029400000724     C*
029500000724     C                   movel(p)  CMD(4)        depcmd
029600000724     C                   eval      depcmd = %trim(depcmd) +
029700090109     C                             %trim(prmlit) + %trim(CMD(5)) +
029800090109     C                             %trim(CurrFileAS) +
029900070913     C                             %trim(CMD(6)) +
030000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
030100070913     C                             %trim(CMD(7))
030200000724     C                   Z-ADD     100           LENGH            15 5
030300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
030400000724     C                   PARM                    depcmd
030500000724     C                   PARM                    LENGH
030600000724     C*
030700000724     C                   endif
030800000724     C*
030900910830     C                   ENDSR
031000000613     C***
031100000725
031200000725
031300000725     C*--------------------------------------------------------
031400000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
031500000725     C*--------------------------------------------------------
031600000725     C     STPR          BEGSR
031700000725     C*
031800030613     C                   if        not %open(tis7pxp)
031900030613     C                   open      tis7pxp
032000000725     C                   endif
032100000725     C*
032200000725     C                   except    riepilogo
032300000725     C*
032400030613     C                   if        %open(tis7pxp)
032500030613     C                   close     tis7pxp
032600000725     C                   endif
032700000725     C*
032800000725     C                   ENDSR
032900000725     C***
033000990920
033100990910
033200000613     C     *inzsr        BEGSR
033300990910     C*
033400990910     C     *entry        plist
033500990920     C                   parm                    tivlrds
033600990921     C                   parm      wrkesito      esito
033700000724     C                   parm                    prmlit
033800000724     C                   parm                    prmfir
033900000613     C*
034000000613     C                   ENDSR
034100000613     C***
034200000725
034300030613     Otis7pxp   E            riepilogo         2
034400000725     O                                              'Upload via FTP'
034500000725     O                                           +1 'Traduzione TIVIN00R -'
034600040203     O                                           +1 'WWRCE00F'
034700000725     O                                           +1 'Entrate'
034800000725     O          E            riepilogo   2
034900000725     O                       wrkdata
035000000725     O                       wrkora              +1
035100000725     O                       procname            +1
035200000725     O          E            riepilogo   2
035300000725     O                                              'Cliente..................:'
035400030613     O                       VLRKSC              +1
035500000725     O          E            riepilogo   2
035600000725     O                                              'Riferimento Strategi.....:'
035700000725     O                       vlrhdl              +1
035800000725     O          E            riepilogo   2
035900000725     O                                              'Giusti...................:'
036000000725     O                       §CTROK        2   +  1
036100000725     O          E            riepilogo   2
036200000725     O                                              'Sbagliati e scartati.....:'
036300000725     O                       §CTRNO        2   +  1
036400990908
036500000724
036600030613     Otis7pxps  E            testdett          2
036700000724     O                                              'Upload via FTP'
036800000613     O                                           +1 'Traduzione TIVIN00R -'
036900040203     O                                           +1 'WWRCE00F'
037000000725     O                                           +1 'Entrate'
037100000616     O          E            testdett    3
037200000613     O                                           +2 'N° rec'
037300000613     O                                           +3 'Anteprima contenuto'
037400000616     O          E            rigadett    2
037500000613     O                       rrnum               +2
037600000621     O                       recko               +3
037700000616     O          E            findett     2
037800000613     O                       wrkdata
037900000613     O                       wrkora              +1
038000000613     O                       procname            +1
038100000616     O          E            findett     2
038200000613     O                                              'Cliente..................:'
038300030613     O                       VLRKSC              +1
038400000616     O          E            findett     2
038500000613     O                                              'Riferimento Strategi.....:'
038600000613     O                       vlrhdl              +1
038700000616     O          E            findett     2
038800000613     O                                              'Giusti...................:'
038900000613     O                       §CTROK        2   +  1
039000000616     O          E            findett     2
039100000613     O                                              'Sbagliati e scartati.....:'
039200000613     O                       §CTRNO        2   +  1
039300000512** CMD - COMANDI CL
039400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
039500030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
039600030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
039700040203CPYF FROMFILE(WWRCE00F) TOFILE(
039800070913/
039900070913) FROMMBR(
040000070913) MBROPT(*ADD) FMTOPT(*NOCHK)
040100040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
040200040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
