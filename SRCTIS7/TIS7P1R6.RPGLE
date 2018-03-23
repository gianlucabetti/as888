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
006800090710     C* => ITALSERVICES
006900090713     C                   IF        %trim(%subst(vlrppt:22:2)) = '11'  AND       * sito
007000090713     C                             %trim(%subst(vlrppt:24:8)) = 'ITALSERV'      * depositante
007100090710     C                   IF        %scan('TRT':wNomeFile:1)  > *zeros
007200090710     C                   EVAL      CurrOpz    = 'GO'
007300090710     C                   EVAL      CurrFileAS = 'WP3TRTP'
007400090710     C                   ENDIF
007500090710     C                   IF        %scan('TRC':wNomeFile:1)  > *zeros
007600090710     C                   EVAL      CurrOpz    = 'GO'
007700090710     C                   EVAL      CurrFileAS = 'WP3TRCP'
007800090710     C                   ENDIF
007900090710     C                   IF        %scan('DPT':wNomeFile:1)  > *zeros
008000090710     C                   EVAL      CurrOpz    = 'GO'
008100090710     C                   EVAL      CurrFileAS = 'WP3DPTP'
008200090710     C                   ENDIF
008300090710     C                   IF        %scan('DPC':wNomeFile:1)  > *zeros
008400090710     C                   EVAL      CurrOpz    = 'GO'
008500090710     C                   EVAL      CurrFileAS = 'WP3DPCP'
008600090710     C                   ENDIF
008700090710     C                   ENDIF
008800090109     C*
008900090109     C                   ELSE
009000090112     C                   EVAL      CurrOpz     = 'GO'
009100090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
009200090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
009300090109     C                   ENDIF
009400090109     C*
009500090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
009600090109     C                   select
009700090109     C                   when      CurrOpz = *blanks
009800090109     C                   eval      wrkesito = '2'
009900090109     C                   when      CurrOpz = 'GO'
010000090109     C*
010100090109     C* OPERAZIONI INIZIALI
010200090109     C                   exsr      PREELA
010300000613     C*
010400090109     C* ELABORAZIONE
010500000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010600000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010700090109     C*
010800090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
010900090129     C                   if        CurrPostPgm <> *blanks AND
011000090129     C                             CurrPostPgm <> '*NONE'
011100090129     C                   if        CurrPostLib = *blanks
011200090129     C                   if        CurrEnv = 'T'
011300150609     C                   eval      CurrPostLib = 'GEOV72SBT'
011400090129     C                   else
011500150609     C                   eval      CurrPostLib = 'GEOV72SB'
011600090129     C                   endif
011700090129     C                   endif
011800090121     C*
011900090121     C                   call      'TIS7P1C3'
012000090129     C                   parm                    CurrPostLib
012100090129     C                   parm                    CurrPostPgm
012200090121     C                   parm                    CurrEnv
012300090121     C                   parm                    CurrSito
012400090121     C                   parm                    CurrDepot
012500090121     C                   parm                    CurrTip
012600090121     C                   parm                    wrkesito
012700090112     C                   endif
012800090121     C*
012900090121     C* OPERAZIONI FINALI
013000090121     C                   exsr      ENDELA
013100090112     C*
013200090109     C                   endsl
013300000616     C*
013400090109     C                   seton                                        lr
013500090109
013600090109
013700090109     C*--------------------------------------------------------
013800090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
013900090109     C*--------------------------------------------------------
014000090109     C     PREELA        BEGSR
014100090109     C*
014200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014300090109     C                   movel(p)  CMD(1)        depcmd
014400090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
014500090109     C*
014600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
014700090109     C                   Z-ADD     100           LENGH            15 5
014800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900090109     C                   PARM                    depcmd
015000090109     C                   PARM                    LENGH
015100090109     C*
015200090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015300090109     C                   movel(p)  CMD(2)        depcmd
015400090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015500090109     C*
015600090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015700090109     C                   Z-ADD     100           LENGH            15 5
015800090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015900090109     C                   PARM                    depcmd
016000090109     C                   PARM                    LENGH
016100090109     C*
016200090109     C                   if        not %open(tis7pxps)
016300090109     C                   open      tis7pxps
016400090109     C                   except    testdett
016500090109     C                   endif
016600090109     C*
016700090109     C                   ENDSR
016800090109     C***
016900090109
017000090109
017100090109     C*--------------------------------------------------------
017200090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
017300090109     C*--------------------------------------------------------
017400090109     C     ENDELA        BEGSR
017500090109     C*
017600090109     C                   if        %open(tis7pxps)
017700090109     C                   except    findett
017800090109     C                   close     tis7pxps
017900090109     C                   endif
018000090109     C*
018100090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018200090109     C                   movel(p)  CMD(3)        depcmd
018300090109     C                   Z-ADD     100           LENGH            15 5
018400090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018500090109     C                   PARM                    depcmd
018600090109     C                   PARM                    LENGH
018700090109     C*
018800090109     C                   ENDSR
018900090109     C***
019000090109
019100090109
019200000613
019300000724
019400910830     C*--------------------------------------------------------
019500040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
019600910830     C*--------------------------------------------------------
019700000724     C     RWFIL         BEGSR
019800990910     C*
019900990914     C                   if        not %open(tivin00r)
020000990908     C                   open      tivin00r
020100990914     C                   endif
020200040203     C                   if        not %open(wwrce00f)
020300040203     C                   open      wwrce00f
020400990914     C                   endif
020500990910     C*
020600990910     C                   clear                   §CTROK
020700990910     C                   clear                   §CTRNO
020800001009     C                   clear                   recko
020900990910     C*
021000921023     C                   DO        *HIVAL
021100990913     C*
021200990915     C                   READ      tivin00r                               70
021300000905     C                   if        vindta > *blanks
021400000613     C                   add       1             rrnum
021500000724     C                   if        *in70 = *off and
021600990913     C                             (vinflg = *blanks
021700990913     C                              or vinflg = '0'
021800990913     C                              or vinflg = '2')
021900990913     C*
022000990913     C                   clear                   vinmsg
022100000724     C                   if        vindta <> *blanks
022200000724     C                   movel(p)  vindta        passaggio
022300040203     C                   write(e)  WWRCE
022400990910     C*
022500000724     C                   if        not %error
022600000724     C                   add       1             §ctrok
022700990910     C                   eval      vinflg = '1'
022800990910     C                   else
022900000724     C                   add       1             §ctrno
023000000621     C                   eval      recko = vindta
023100000616     C                   except    rigadett
023200990910     C                   eval      vinflg = '2'
023300990910     C                   endif
023400000724     C                   endif
023500991022     C                   endif
023600000905     C*
023700000905     C                   else
023800000905     C                   eval      vinflg = '1'
023900000905     C                   endif
024000000905     C*
024100000905     C  N70              update    tivin000
024200991022     C*
024300991022     C  N70              ENDdo
024400990910
024500990910     C* Se non ci sono record con errori ...
024600000710     C                   if        §ctrno = 0
024700990910     C* ... restituisco esito OK.
024800990921     C                   eval      wrkesito = '0'
024900990910     C                   else
025000000710     C                   if        §ctrok > 0
025100990921     C                   eval      wrkesito = '1'
025200000710     C                   else
025300000710     C                   eval      wrkesito = '2'
025400990910     C                   endif
025500000710     C                   endif
025600990910     C*
025700990914     C                   if        %open(tivin00r)
025800990908     C                   close     tivin00r
025900990914     C                   endif
026000040203     C                   if        %open(wwrce00f)
026100040203     C                   close     wwrce00f
026200990914     C                   endif
026300990920     C*
026400000724     C                   if        wrkesito <> '2'
026500000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
026600070913     C                   movel(p)  CMD(8)        depcmd
026700000724     C                   eval      depcmd = %trim(depcmd) + ' '+
026800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
026900000724     C                   Z-ADD     100           LENGH            15 5
027000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027100000724     C                   PARM                    depcmd
027200000724     C                   PARM                    LENGH
027300000724     C*
027400070913     C                   movel(p)  CMD(9)        depcmd
027500000724     C                   eval      depcmd = %trim(depcmd)
027600000724     C                   Z-ADD     100           LENGH            15 5
027700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027800000724     C                   PARM                    depcmd
027900000724     C                   PARM                    LENGH
028000000724     C*
028100000724     C                   movel(p)  CMD(4)        depcmd
028200000724     C                   eval      depcmd = %trim(depcmd) +
028300090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028400090109     C                             %trim(CurrFileAS) +
028500070913     C                             %trim(CMD(6)) +
028600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
028700070913     C                             %trim(CMD(7))
028800000724     C                   Z-ADD     100           LENGH            15 5
028900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029000000724     C                   PARM                    depcmd
029100000724     C                   PARM                    LENGH
029200000724     C*
029300000724     C                   endif
029400000724     C*
029500910830     C                   ENDSR
029600000613     C***
029700000725
029800000725
029900000725     C*--------------------------------------------------------
030000000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
030100000725     C*--------------------------------------------------------
030200000725     C     STPR          BEGSR
030300000725     C*
030400030613     C                   if        not %open(tis7pxp)
030500030613     C                   open      tis7pxp
030600000725     C                   endif
030700000725     C*
030800000725     C                   except    riepilogo
030900000725     C*
031000030613     C                   if        %open(tis7pxp)
031100030613     C                   close     tis7pxp
031200000725     C                   endif
031300000725     C*
031400000725     C                   ENDSR
031500000725     C***
031600990920
031700990910
031800000613     C     *inzsr        BEGSR
031900990910     C*
032000990910     C     *entry        plist
032100990920     C                   parm                    tivlrds
032200990921     C                   parm      wrkesito      esito
032300000724     C                   parm                    prmlit
032400000724     C                   parm                    prmfir
032500000613     C*
032600000613     C                   ENDSR
032700000613     C***
032800000725
032900030613     Otis7pxp   E            riepilogo         2
033000000725     O                                              'Upload via FTP'
033100000725     O                                           +1 'Traduzione TIVIN00R -'
033200040203     O                                           +1 'WWRCE00F'
033300000725     O                                           +1 'Entrate'
033400000725     O          E            riepilogo   2
033500000725     O                       wrkdata
033600000725     O                       wrkora              +1
033700000725     O                       procname            +1
033800000725     O          E            riepilogo   2
033900000725     O                                              'Cliente..................:'
034000030613     O                       VLRKSC              +1
034100000725     O          E            riepilogo   2
034200000725     O                                              'Riferimento Strategi.....:'
034300000725     O                       vlrhdl              +1
034400000725     O          E            riepilogo   2
034500000725     O                                              'Giusti...................:'
034600000725     O                       §CTROK        2   +  1
034700000725     O          E            riepilogo   2
034800000725     O                                              'Sbagliati e scartati.....:'
034900000725     O                       §CTRNO        2   +  1
035000990908
035100000724
035200030613     Otis7pxps  E            testdett          2
035300000724     O                                              'Upload via FTP'
035400000613     O                                           +1 'Traduzione TIVIN00R -'
035500040203     O                                           +1 'WWRCE00F'
035600000725     O                                           +1 'Entrate'
035700000616     O          E            testdett    3
035800000613     O                                           +2 'N° rec'
035900000613     O                                           +3 'Anteprima contenuto'
036000000616     O          E            rigadett    2
036100000613     O                       rrnum               +2
036200000621     O                       recko               +3
036300000616     O          E            findett     2
036400000613     O                       wrkdata
036500000613     O                       wrkora              +1
036600000613     O                       procname            +1
036700000616     O          E            findett     2
036800000613     O                                              'Cliente..................:'
036900030613     O                       VLRKSC              +1
037000000616     O          E            findett     2
037100000613     O                                              'Riferimento Strategi.....:'
037200000613     O                       vlrhdl              +1
037300000616     O          E            findett     2
037400000613     O                                              'Giusti...................:'
037500000613     O                       §CTROK        2   +  1
037600000616     O          E            findett     2
037700000613     O                                              'Sbagliati e scartati.....:'
037800000613     O                       §CTRNO        2   +  1
037900000512** CMD - COMANDI CL
038000030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
038100030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
038200030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
038300040203CPYF FROMFILE(WWRCE00F) TOFILE(
038400070913/
038500070913) FROMMBR(
038600070913) MBROPT(*ADD) FMTOPT(*NOCHK)
038700040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
038800040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
