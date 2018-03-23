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
001400070913     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
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
003200090109     D CurrOpz         s              2
003300090109     D CurrFileAS      s             10
003400090109     D*-------------------
003500090109     D* COSTANTI
003600090109     D*-------------------
003700090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003800090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003900990908
004000990915     C                   time                    wrkdata
004100990915     C                   time                    wrkora
004200000913     C                   reset                   rrnum
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500090109     C*
004600090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
004700090109     C                   EVAL      CurrOpz    = *blanks
004800090109     C                   EVAL      CurrFileAS = *blanks
004900090109     C                   IF        %trim(%subst(vlrppt:1:10)) = '*FILE'
005000090109     C                   MOVE(P)   vlrMSG        wNomeFile
005100090109     C* Porto il nome file sempre in maiuscolo
005200090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
005300090109     C*
005400090109     C* ...verifico particolarità cliente...
005500090109     C*
005600090109     C* => VGAMES
005700090109     C                   IF        %trim(%subst(vlrppt:11:2)) = '02'  AND       * sito
005800090109     C                             %trim(%subst(vlrppt:13:8)) = 'INDUSTRI'      * depositante
005900090112     C                   IF        %scan('.PAL':wNomeFile:1)  > *zeros
006000090109     C                   EVAL      CurrOpz    = 'GO'
006100090109     C                   EVAL      CurrFileAS = 'WP3IENP'
006200090109     C                   ENDIF
006300090112     C                   IF        %scan('.TES':wNomeFile:1)  > *zeros
006400090109     C                   EVAL      CurrOpz    = 'GO'
006500090109     C                   EVAL      CurrFileAS = 'WP3ICTP'
006600090109     C                   ENDIF
006700090112     C                   IF        %scan('.COL':wNomeFile:1)  > *zeros
006800090109     C                   EVAL      CurrOpz    = 'GO'
006900090109     C                   EVAL      CurrFileAS = 'WP3ICDP'
007000090109     C                   ENDIF
007100090112     C                   IF        %scan('.END':wNomeFile:1)  > *zeros
007200090109     C                   EVAL      CurrOpz    = 'EM'
007300090109     C                   EVAL      CurrFileAS = *blanks
007400090109     C                   ENDIF
007500090109     C                   ENDIF
007600090109     C*
007700090109     C                   ELSE
007800090112     C                   EVAL      CurrOpz    = 'GO'
007900090112     C                   EVAL      CurrFileAS = %trim(%subst(vlrppt:1:10))
008000090109     C                   ENDIF
008100090109     C*
008200090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
008300090109     C                   select
008400090109     C                   when      CurrOpz = *blanks
008500090109     C                   eval      wrkesito = '2'
008600090109     C                   when      CurrOpz = 'EM'
008700090109     C                   eval      wrkesito = '0'
008800090109     C                   when      CurrOpz = 'GO'
008900090109     C*
009000090109     C* OPERAZIONI INIZIALI
009100090109     C                   exsr      PREELA
009200000613     C*
009300090109     C* ELABORAZIONE
009400000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
009500000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009600090109     C*
009700090109     C* OPERAZIONI FINALI
009800090109     C                   exsr      ENDELA
009900090109     C*
010000090109     C                   endsl
010100000616     C*
010200090109     C                   seton                                        lr
010300090109
010400090109
010500090109     C*--------------------------------------------------------
010600090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
010700090109     C*--------------------------------------------------------
010800090109     C     PREELA        BEGSR
010900090109     C*
011000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100090109     C                   movel(p)  CMD(1)        depcmd
011200090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
011300090109     C*
011400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
011500090109     C                   Z-ADD     100           LENGH            15 5
011600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700090109     C                   PARM                    depcmd
011800090109     C                   PARM                    LENGH
011900090109     C*
012000090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012100090109     C                   movel(p)  CMD(2)        depcmd
012200090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012300090109     C*
012400090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012500090109     C                   Z-ADD     100           LENGH            15 5
012600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700090109     C                   PARM                    depcmd
012800090109     C                   PARM                    LENGH
012900090109     C*
013000090109     C                   if        not %open(tis7pxps)
013100090109     C                   open      tis7pxps
013200090109     C                   except    testdett
013300090109     C                   endif
013400090109     C*
013500090109     C                   ENDSR
013600090109     C***
013700090109
013800090109
013900090109     C*--------------------------------------------------------
014000090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
014100090109     C*--------------------------------------------------------
014200090109     C     ENDELA        BEGSR
014300090109     C*
014400090109     C                   if        %open(tis7pxps)
014500090109     C                   except    findett
014600090109     C                   close     tis7pxps
014700090109     C                   endif
014800090109     C*
014900090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015000090109     C                   movel(p)  CMD(3)        depcmd
015100090109     C                   Z-ADD     100           LENGH            15 5
015200090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300090109     C                   PARM                    depcmd
015400090109     C                   PARM                    LENGH
015500090109     C*
015600090109     C                   ENDSR
015700090109     C***
015800090109
015900090109
016000000613
016100000724
016200910830     C*--------------------------------------------------------
016300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
016400910830     C*--------------------------------------------------------
016500000724     C     RWFIL         BEGSR
016600990910     C*
016700990914     C                   if        not %open(tivin00r)
016800990908     C                   open      tivin00r
016900990914     C                   endif
017000040203     C                   if        not %open(wwrce00f)
017100040203     C                   open      wwrce00f
017200990914     C                   endif
017300990910     C*
017400990910     C                   clear                   §CTROK
017500990910     C                   clear                   §CTRNO
017600001009     C                   clear                   recko
017700990910     C*
017800921023     C                   DO        *HIVAL
017900990913     C*
018000990915     C                   READ      tivin00r                               70
018100000905     C                   if        vindta > *blanks
018200000613     C                   add       1             rrnum
018300000724     C                   if        *in70 = *off and
018400990913     C                             (vinflg = *blanks
018500990913     C                              or vinflg = '0'
018600990913     C                              or vinflg = '2')
018700990913     C*
018800990913     C                   clear                   vinmsg
018900000724     C                   if        vindta <> *blanks
019000000724     C                   movel(p)  vindta        passaggio
019100040203     C                   write(e)  WWRCE
019200990910     C*
019300000724     C                   if        not %error
019400000724     C                   add       1             §ctrok
019500990910     C                   eval      vinflg = '1'
019600990910     C                   else
019700000724     C                   add       1             §ctrno
019800000621     C                   eval      recko = vindta
019900000616     C                   except    rigadett
020000990910     C                   eval      vinflg = '2'
020100990910     C                   endif
020200000724     C                   endif
020300991022     C                   endif
020400000905     C*
020500000905     C                   else
020600000905     C                   eval      vinflg = '1'
020700000905     C                   endif
020800000905     C*
020900000905     C  N70              update    tivin000
021000991022     C*
021100991022     C  N70              ENDdo
021200990910
021300990910     C* Se non ci sono record con errori ...
021400000710     C                   if        §ctrno = 0
021500990910     C* ... restituisco esito OK.
021600990921     C                   eval      wrkesito = '0'
021700990910     C                   else
021800000710     C                   if        §ctrok > 0
021900990921     C                   eval      wrkesito = '1'
022000000710     C                   else
022100000710     C                   eval      wrkesito = '2'
022200990910     C                   endif
022300000710     C                   endif
022400990910     C*
022500990914     C                   if        %open(tivin00r)
022600990908     C                   close     tivin00r
022700990914     C                   endif
022800040203     C                   if        %open(wwrce00f)
022900040203     C                   close     wwrce00f
023000990914     C                   endif
023100990920     C*
023200000724     C                   if        wrkesito <> '2'
023300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
023400070913     C                   movel(p)  CMD(8)        depcmd
023500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
023600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
023700000724     C                   Z-ADD     100           LENGH            15 5
023800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
023900000724     C                   PARM                    depcmd
024000000724     C                   PARM                    LENGH
024100000724     C*
024200070913     C                   movel(p)  CMD(9)        depcmd
024300000724     C                   eval      depcmd = %trim(depcmd)
024400000724     C                   Z-ADD     100           LENGH            15 5
024500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
024600000724     C                   PARM                    depcmd
024700000724     C                   PARM                    LENGH
024800000724     C*
024900000724     C                   movel(p)  CMD(4)        depcmd
025000000724     C                   eval      depcmd = %trim(depcmd) +
025100090109     C                             %trim(prmlit) + %trim(CMD(5)) +
025200090109     C                             %trim(CurrFileAS) +
025300070913     C                             %trim(CMD(6)) +
025400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
025500070913     C                             %trim(CMD(7))
025600000724     C                   Z-ADD     100           LENGH            15 5
025700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
025800000724     C                   PARM                    depcmd
025900000724     C                   PARM                    LENGH
026000000724     C*
026100000724     C                   endif
026200000724     C*
026300910830     C                   ENDSR
026400000613     C***
026500000725
026600000725
026700000725     C*--------------------------------------------------------
026800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
026900000725     C*--------------------------------------------------------
027000000725     C     STPR          BEGSR
027100000725     C*
027200030613     C                   if        not %open(tis7pxp)
027300030613     C                   open      tis7pxp
027400000725     C                   endif
027500000725     C*
027600000725     C                   except    riepilogo
027700000725     C*
027800030613     C                   if        %open(tis7pxp)
027900030613     C                   close     tis7pxp
028000000725     C                   endif
028100000725     C*
028200000725     C                   ENDSR
028300000725     C***
028400990920
028500990910
028600000613     C     *inzsr        BEGSR
028700990910     C*
028800990910     C     *entry        plist
028900990920     C                   parm                    tivlrds
029000990921     C                   parm      wrkesito      esito
029100000724     C                   parm                    prmlit
029200000724     C                   parm                    prmfir
029300000613     C*
029400000613     C                   ENDSR
029500000613     C***
029600000725
029700030613     Otis7pxp   E            riepilogo         2
029800000725     O                                              'Upload via FTP'
029900000725     O                                           +1 'Traduzione TIVIN00R -'
030000040203     O                                           +1 'WWRCE00F'
030100000725     O                                           +1 'Entrate'
030200000725     O          E            riepilogo   2
030300000725     O                       wrkdata
030400000725     O                       wrkora              +1
030500000725     O                       procname            +1
030600000725     O          E            riepilogo   2
030700000725     O                                              'Cliente..................:'
030800030613     O                       VLRKSC              +1
030900000725     O          E            riepilogo   2
031000000725     O                                              'Riferimento Strategi.....:'
031100000725     O                       vlrhdl              +1
031200000725     O          E            riepilogo   2
031300000725     O                                              'Giusti...................:'
031400000725     O                       §CTROK        2   +  1
031500000725     O          E            riepilogo   2
031600000725     O                                              'Sbagliati e scartati.....:'
031700000725     O                       §CTRNO        2   +  1
031800990908
031900000724
032000030613     Otis7pxps  E            testdett          2
032100000724     O                                              'Upload via FTP'
032200000613     O                                           +1 'Traduzione TIVIN00R -'
032300040203     O                                           +1 'WWRCE00F'
032400000725     O                                           +1 'Entrate'
032500000616     O          E            testdett    3
032600000613     O                                           +2 'N° rec'
032700000613     O                                           +3 'Anteprima contenuto'
032800000616     O          E            rigadett    2
032900000613     O                       rrnum               +2
033000000621     O                       recko               +3
033100000616     O          E            findett     2
033200000613     O                       wrkdata
033300000613     O                       wrkora              +1
033400000613     O                       procname            +1
033500000616     O          E            findett     2
033600000613     O                                              'Cliente..................:'
033700030613     O                       VLRKSC              +1
033800000616     O          E            findett     2
033900000613     O                                              'Riferimento Strategi.....:'
034000000613     O                       vlrhdl              +1
034100000616     O          E            findett     2
034200000613     O                                              'Giusti...................:'
034300000613     O                       §CTROK        2   +  1
034400000616     O          E            findett     2
034500000613     O                                              'Sbagliati e scartati.....:'
034600000613     O                       §CTRNO        2   +  1
034700000512** CMD - COMANDI CL
034800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
034900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
035000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
035100040203CPYF FROMFILE(WWRCE00F) TOFILE(
035200070913/
035300070913) FROMMBR(
035400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
035500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
035600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
