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
006800101026     C* => TROPS 2
006900110825     C*//                IF        CurrSito  = '11'   AND
007000131010     C*//                IF        CurrSito  = '02'   AND
007100131010     C                   IF        CurrSito  = '18'   AND
007200101008     C                             CurrDepot = 'TROPS2'
007300101008     C                   IF        %subst(wNomeFile:1:3) = 'UPL'
007400090112     C                   EVAL      CurrOpz     = 'GO'
007500101008     C                   EVAL      CurrFileAS  = 'WP3UPLP'
007600110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
007700101008     C                   EVAL      CurrTip     = 'UP'
007800090109     C                   ENDIF
007900101008     C                   IF        %subst(wNomeFile:1:3) = 'ART'
008000100503     C                   EVAL      CurrOpz     = 'GO'
008100101008     C                   EVAL      CurrFileAS  = 'WP3T2AP'
008200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008300101008     C                   EVAL      CurrTip     = 'AR'
008400100503     C                   ENDIF
008500101026     C                   IF        %subst(wNomeFile:1:3) = 'BAR'
008600090112     C                   EVAL      CurrOpz     = 'GO'
008700101008     C                   EVAL      CurrFileAS  = 'WP3T2BP'
008800110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008900101008     C                   EVAL      CurrTip     = 'BA'
009000090112     C                   ENDIF
009100090109     C                   ENDIF
009200090109     C*
009300090109     C                   ELSE
009400090112     C                   EVAL      CurrOpz     = 'GO'
009500090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009600090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
009700090109     C                   ENDIF
009800090109     C*
009900090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
010000090109     C                   select
010100090109     C                   when      CurrOpz = *blanks
010200090109     C                   eval      wrkesito = '2'
010300090109     C                   when      CurrOpz = 'GO'
010400090109     C*
010500090109     C* OPERAZIONI INIZIALI
010600090109     C                   exsr      PREELA
010700000613     C*
010800090109     C* ELABORAZIONE
010900000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
011000000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011100090109     C*
011200090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
011300090129     C                   if        CurrPostPgm <> *blanks AND
011400090129     C                             CurrPostPgm <> '*NONE'
011500090129     C                   if        CurrPostLib = *blanks
011600090129     C                   if        CurrEnv = 'T'
011700150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011800090129     C                   else
011900150609     C                   eval      CurrPostLib = 'GEOV72SB'
012000090129     C                   endif
012100090129     C                   endif
012200090121     C*
012300110428     C                   call      'TIS7P1C'
012400090129     C                   parm                    CurrPostLib
012500090129     C                   parm                    CurrPostPgm
012600090121     C                   parm                    CurrEnv
012700090121     C                   parm                    CurrSito
012800090121     C                   parm                    CurrDepot
012900090121     C                   parm                    CurrTip
013000090121     C                   parm                    wrkesito
013100090112     C                   endif
013200090121     C*
013300090121     C* OPERAZIONI FINALI
013400090121     C                   exsr      ENDELA
013500090112     C*
013600090109     C                   endsl
013700000616     C*
013800090109     C                   seton                                        lr
013900090109
014000090109
014100090109     C*--------------------------------------------------------
014200090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
014300090109     C*--------------------------------------------------------
014400090109     C     PREELA        BEGSR
014500090109     C*
014600090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014700090109     C                   movel(p)  CMD(1)        depcmd
014800090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
014900090109     C*
015000090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
015100090109     C                   Z-ADD     100           LENGH            15 5
015200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300090109     C                   PARM                    depcmd
015400090109     C                   PARM                    LENGH
015500090109     C*
015600090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015700090109     C                   movel(p)  CMD(2)        depcmd
015800090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015900090109     C*
016000090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016100090109     C                   Z-ADD     100           LENGH            15 5
016200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016300090109     C                   PARM                    depcmd
016400090109     C                   PARM                    LENGH
016500090109     C*
016600090109     C                   if        not %open(tis7pxps)
016700090109     C                   open      tis7pxps
016800090109     C                   except    testdett
016900090109     C                   endif
017000090109     C*
017100090109     C                   ENDSR
017200090109     C***
017300090109
017400090109
017500090109     C*--------------------------------------------------------
017600090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
017700090109     C*--------------------------------------------------------
017800090109     C     ENDELA        BEGSR
017900090109     C*
018000090109     C                   if        %open(tis7pxps)
018100090109     C                   except    findett
018200090109     C                   close     tis7pxps
018300090109     C                   endif
018400090109     C*
018500090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018600090109     C                   movel(p)  CMD(3)        depcmd
018700090109     C                   Z-ADD     100           LENGH            15 5
018800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018900090109     C                   PARM                    depcmd
019000090109     C                   PARM                    LENGH
019100090109     C*
019200090109     C                   ENDSR
019300090109     C***
019400090109
019500090109
019600000613
019700000724
019800910830     C*--------------------------------------------------------
019900040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
020000910830     C*--------------------------------------------------------
020100000724     C     RWFIL         BEGSR
020200990910     C*
020300990914     C                   if        not %open(tivin00r)
020400990908     C                   open      tivin00r
020500990914     C                   endif
020600040203     C                   if        not %open(wwrce00f)
020700040203     C                   open      wwrce00f
020800990914     C                   endif
020900990910     C*
021000990910     C                   clear                   §CTROK
021100990910     C                   clear                   §CTRNO
021200001009     C                   clear                   recko
021300990910     C*
021400921023     C                   DO        *HIVAL
021500990913     C*
021600990915     C                   READ      tivin00r                               70
021700000905     C                   if        vindta > *blanks
021800000613     C                   add       1             rrnum
021900000724     C                   if        *in70 = *off and
022000990913     C                             (vinflg = *blanks
022100990913     C                              or vinflg = '0'
022200990913     C                              or vinflg = '2')
022300990913     C*
022400990913     C                   clear                   vinmsg
022500000724     C                   if        vindta <> *blanks
022600000724     C                   movel(p)  vindta        passaggio
022700040203     C                   write(e)  WWRCE
022800990910     C*
022900000724     C                   if        not %error
023000000724     C                   add       1             §ctrok
023100990910     C                   eval      vinflg = '1'
023200990910     C                   else
023300000724     C                   add       1             §ctrno
023400000621     C                   eval      recko = vindta
023500000616     C                   except    rigadett
023600990910     C                   eval      vinflg = '2'
023700990910     C                   endif
023800000724     C                   endif
023900991022     C                   endif
024000000905     C*
024100000905     C                   else
024200000905     C                   eval      vinflg = '1'
024300000905     C                   endif
024400000905     C*
024500000905     C  N70              update    tivin000
024600991022     C*
024700991022     C  N70              ENDdo
024800990910
024900990910     C* Se non ci sono record con errori ...
025000000710     C                   if        §ctrno = 0
025100990910     C* ... restituisco esito OK.
025200990921     C                   eval      wrkesito = '0'
025300990910     C                   else
025400000710     C                   if        §ctrok > 0
025500990921     C                   eval      wrkesito = '1'
025600000710     C                   else
025700000710     C                   eval      wrkesito = '2'
025800990910     C                   endif
025900000710     C                   endif
026000990910     C*
026100990914     C                   if        %open(tivin00r)
026200990908     C                   close     tivin00r
026300990914     C                   endif
026400040203     C                   if        %open(wwrce00f)
026500040203     C                   close     wwrce00f
026600990914     C                   endif
026700990920     C*
026800000724     C                   if        wrkesito <> '2'
026900000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
027000070913     C                   movel(p)  CMD(8)        depcmd
027100000724     C                   eval      depcmd = %trim(depcmd) + ' '+
027200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
027300000724     C                   Z-ADD     100           LENGH            15 5
027400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027500000724     C                   PARM                    depcmd
027600000724     C                   PARM                    LENGH
027700000724     C*
027800070913     C                   movel(p)  CMD(9)        depcmd
027900000724     C                   eval      depcmd = %trim(depcmd)
028000000724     C                   Z-ADD     100           LENGH            15 5
028100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028200000724     C                   PARM                    depcmd
028300000724     C                   PARM                    LENGH
028400000724     C*
028500000724     C                   movel(p)  CMD(4)        depcmd
028600000724     C                   eval      depcmd = %trim(depcmd) +
028700090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028800090109     C                             %trim(CurrFileAS) +
028900070913     C                             %trim(CMD(6)) +
029000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
029100070913     C                             %trim(CMD(7))
029200000724     C                   Z-ADD     100           LENGH            15 5
029300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029400000724     C                   PARM                    depcmd
029500000724     C                   PARM                    LENGH
029600000724     C*
029700000724     C                   endif
029800000724     C*
029900910830     C                   ENDSR
030000000613     C***
030100000725
030200000725
030300000725     C*--------------------------------------------------------
030400000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
030500000725     C*--------------------------------------------------------
030600000725     C     STPR          BEGSR
030700000725     C*
030800030613     C                   if        not %open(tis7pxp)
030900030613     C                   open      tis7pxp
031000000725     C                   endif
031100000725     C*
031200000725     C                   except    riepilogo
031300000725     C*
031400030613     C                   if        %open(tis7pxp)
031500030613     C                   close     tis7pxp
031600000725     C                   endif
031700000725     C*
031800000725     C                   ENDSR
031900000725     C***
032000990920
032100990910
032200000613     C     *inzsr        BEGSR
032300990910     C*
032400990910     C     *entry        plist
032500990920     C                   parm                    tivlrds
032600990921     C                   parm      wrkesito      esito
032700000724     C                   parm                    prmlit
032800000724     C                   parm                    prmfir
032900000613     C*
033000000613     C                   ENDSR
033100000613     C***
033200000725
033300030613     Otis7pxp   E            riepilogo         2
033400000725     O                                              'Upload via FTP'
033500000725     O                                           +1 'Traduzione TIVIN00R -'
033600040203     O                                           +1 'WWRCE00F'
033700000725     O                                           +1 'Entrate'
033800000725     O          E            riepilogo   2
033900000725     O                       wrkdata
034000000725     O                       wrkora              +1
034100000725     O                       procname            +1
034200000725     O          E            riepilogo   2
034300000725     O                                              'Cliente..................:'
034400030613     O                       VLRKSC              +1
034500000725     O          E            riepilogo   2
034600000725     O                                              'Riferimento Strategi.....:'
034700000725     O                       vlrhdl              +1
034800000725     O          E            riepilogo   2
034900000725     O                                              'Giusti...................:'
035000000725     O                       §CTROK        2   +  1
035100000725     O          E            riepilogo   2
035200000725     O                                              'Sbagliati e scartati.....:'
035300000725     O                       §CTRNO        2   +  1
035400990908
035500000724
035600030613     Otis7pxps  E            testdett          2
035700000724     O                                              'Upload via FTP'
035800000613     O                                           +1 'Traduzione TIVIN00R -'
035900040203     O                                           +1 'WWRCE00F'
036000000725     O                                           +1 'Entrate'
036100000616     O          E            testdett    3
036200000613     O                                           +2 'N° rec'
036300000613     O                                           +3 'Anteprima contenuto'
036400000616     O          E            rigadett    2
036500000613     O                       rrnum               +2
036600000621     O                       recko               +3
036700000616     O          E            findett     2
036800000613     O                       wrkdata
036900000613     O                       wrkora              +1
037000000613     O                       procname            +1
037100000616     O          E            findett     2
037200000613     O                                              'Cliente..................:'
037300030613     O                       VLRKSC              +1
037400000616     O          E            findett     2
037500000613     O                                              'Riferimento Strategi.....:'
037600000613     O                       vlrhdl              +1
037700000616     O          E            findett     2
037800000613     O                                              'Giusti...................:'
037900000613     O                       §CTROK        2   +  1
038000000616     O          E            findett     2
038100000613     O                                              'Sbagliati e scartati.....:'
038200000613     O                       §CTRNO        2   +  1
038300000512** CMD - COMANDI CL
038400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
038500030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
038600030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
038700040203CPYF FROMFILE(WWRCE00F) TOFILE(
038800070913/
038900070913) FROMMBR(
039000070913) MBROPT(*ADD) FMTOPT(*NOCHK)
039100040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
039200040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
