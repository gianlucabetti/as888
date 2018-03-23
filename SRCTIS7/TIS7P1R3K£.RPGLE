000100101220      /TITLE Upload via FTP: traduzione file LED WWRCL00F
000200990908     H dftactgrp(*yes)
000300150610
000400990910     Ftivin00r  uF   E             DISK    usropn
000500101220     FWWRCL00F  O    E             DISK    usropn
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
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300990915     D wrkdata         s               d
002400990915     D wrkora          s               t
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600000621     D recko           s            150    INZ(*blanks)
002700000724     D depcmd          s            100    INZ(*blanks)
002800000724     D §ctrok          s              5  0 INZ(*zeros)
002900000724     D §ctrno          s              5  0 INZ(*zeros)
003000090109     D wNomeFile       s             30    INZ(*blanks)
003100090121     D CurrOpz         s              2    INZ
003200090121     D CurrFileAS      s             10    INZ
003300090129     D CurrPostPgm     s             10    INZ
003400090129     D CurrPostLib     s             10    INZ
003500090121     D CurrSito        s              2    INZ
003600090121     D CurrDepot       s              8    INZ
003700090121     D CurrEnv         s              1    INZ
003800090121     D CurrTip         s              2    INZ
003900101220
004000101220
004100101220     D*-------------------
004200101220     D* DS RIAGGREGAZIONE SEGMENTI RECORD
004300101220     D*-------------------
004400101220     D  DS_INPUT       DS
004500101220     D  DS_I_HEADER                   1A
004600101220     D  DS_I_SEQUEN                   7S 0
004700101220     D  DS_I_DATI                  2040A
004800101220
004900101220
005000090109     D*-------------------
005100090109     D* COSTANTI
005200090109     D*-------------------
005300090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005400090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005500990908
005600990915     C                   time                    wrkdata
005700990915     C                   time                    wrkora
005800000913     C                   reset                   rrnum
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100090109     C*
006200090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
006300090121     C                   EVAL      CurrOpz     = *blanks
006400090121     C                   EVAL      CurrFileAS  = *blanks
006500090129     C                   EVAL      CurrPostPgm = *blanks
006600090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
006700090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
006800090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
006900090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
007000090121     C*
007100090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
007200090109     C                   MOVE(P)   vlrMSG        wNomeFile
007300090109     C* Porto il nome file sempre in maiuscolo
007400090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
007500090109     C*
007600090109     C* ...verifico particolarità cliente...
007700090109     C*
007800090109     C* => VGAMES
007900150427     C                   IF        (CurrSito='05' AND CurrDepot='VIVENDI') or
008000150427     C                             (CurrSito='05' AND CurrDepot='ABCOOP')
008100100310     C                   IF        %scan('IT_ITEMO_':wNomeFile:1)    > *zeros OR
008200100310     C                             %scan('ITBAR_ITEMO_':wNomeFile:1) > *zeros
008300090112     C                   EVAL      CurrOpz     = 'GO'
008400090112     C                   EVAL      CurrFileAS  = 'WP3AITP'
008500110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008600090121     C                   EVAL      CurrTip     = 'IT'
008700090109     C                   ENDIF
008800100503     C                   IF        %scan('IT_POEXO_':wNomeFile:1)    > *zeros OR
008900100503     C                             %scan('ITBAR_POEXO_':wNomeFile:1) > *zeros
009000100503     C                   EVAL      CurrOpz     = 'GO'
009100100503     C                   EVAL      CurrFileAS  = 'WP3APOP'
009200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
009300100503     C                   EVAL      CurrTip     = 'PO'
009400100503     C                   ENDIF
009500100310     C                   IF        %scan('IT_SORDO_':wNomeFile:1)    > *zeros OR
009600100310     C                             %scan('ITBAR_SORDO_':wNomeFile:1) > *zeros
009700090112     C                   EVAL      CurrOpz     = 'GO'
009800090112     C                   EVAL      CurrFileAS  = 'WP3ASOP'
009900110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
010000090121     C                   EVAL      CurrTip     = 'SO'
010100090112     C                   ENDIF
010200100310     C                   IF        %scan('IT_RMAOO_':wNomeFile:1)    > *zeros OR
010300100310     C                             %scan('ITBAR_RMAOO_':wNomeFile:1) > *zeros
010400090112     C                   EVAL      CurrOpz     = 'GO'
010500090112     C                   EVAL      CurrFileAS  = 'WP3ARMP'
010600110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
010700090121     C                   EVAL      CurrTip     = 'RM'
010800090109     C                   ENDIF
010900090109     C                   ENDIF
011000090109     C*
011100090109     C                   ELSE
011200090112     C                   EVAL      CurrOpz     = 'GO'
011300090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
011400090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
011500090109     C                   ENDIF
011600090109     C*
011700090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
011800090109     C                   select
011900090109     C                   when      CurrOpz = *blanks
012000090109     C                   eval      wrkesito = '2'
012100090109     C                   when      CurrOpz = 'GO'
012200090109     C*
012300090109     C* OPERAZIONI INIZIALI
012400090109     C                   exsr      PREELA
012500000613     C*
012600090109     C* ELABORAZIONE
012700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
012800000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012900090109     C*
013000090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
013100090129     C                   if        CurrPostPgm <> *blanks AND
013200090129     C                             CurrPostPgm <> '*NONE'
013300090129     C                   if        CurrPostLib = *blanks
013400090129     C                   if        CurrEnv = 'T'
013500150609     C                   eval      CurrPostLib = 'GEOV72SBT'
013600090129     C                   else
013700150609     C                   eval      CurrPostLib = 'GEOV72SB'
013800090129     C                   endif
013900090129     C                   endif
014000090121     C*
014100110428     C                   call      'TIS7P1C'
014200090129     C                   parm                    CurrPostLib
014300090129     C                   parm                    CurrPostPgm
014400090121     C                   parm                    CurrEnv
014500090121     C                   parm                    CurrSito
014600090121     C                   parm                    CurrDepot
014700090121     C                   parm                    CurrTip
014800090121     C                   parm                    wrkesito
014900090112     C                   endif
015000090121     C*
015100090121     C* OPERAZIONI FINALI
015200090121     C                   exsr      ENDELA
015300090112     C*
015400090109     C                   endsl
015500000616     C*
015600090109     C                   seton                                        lr
015700090109
015800090109
015900090109     C*--------------------------------------------------------
016000090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
016100090109     C*--------------------------------------------------------
016200090109     C     PREELA        BEGSR
016300090109     C*
016400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016500090109     C                   movel(p)  CMD(1)        depcmd
016600090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
016700090109     C*
016800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
016900090109     C                   Z-ADD     100           LENGH            15 5
017000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017100090109     C                   PARM                    depcmd
017200090109     C                   PARM                    LENGH
017300090109     C*
017400090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
017500090109     C                   movel(p)  CMD(2)        depcmd
017600090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
017700090109     C*
017800090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
017900090109     C                   Z-ADD     100           LENGH            15 5
018000090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018100090109     C                   PARM                    depcmd
018200090109     C                   PARM                    LENGH
018300090109     C*
018400090109     C                   if        not %open(tis7pxps)
018500090109     C                   open      tis7pxps
018600090109     C                   except    testdett
018700090109     C                   endif
018800090109     C*
018900090109     C                   ENDSR
019000090109     C***
019100090109
019200090109
019300090109     C*--------------------------------------------------------
019400090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
019500090109     C*--------------------------------------------------------
019600090109     C     ENDELA        BEGSR
019700090109     C*
019800090109     C                   if        %open(tis7pxps)
019900090109     C                   except    findett
020000090109     C                   close     tis7pxps
020100090109     C                   endif
020200090109     C*
020300090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020400090109     C                   movel(p)  CMD(3)        depcmd
020500090109     C                   Z-ADD     100           LENGH            15 5
020600090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020700090109     C                   PARM                    depcmd
020800090109     C                   PARM                    LENGH
020900090109     C*
021000090109     C                   ENDSR
021100090109     C***
021200090109
021300090109
021400000613
021500000724
021600910830     C*--------------------------------------------------------
021700101220     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCL00F     *
021800910830     C*--------------------------------------------------------
021900000724     C     RWFIL         BEGSR
022000990910     C*
022100990914     C                   if        not %open(tivin00r)
022200990908     C                   open      tivin00r
022300990914     C                   endif
022400101220     C                   if        not %open(wwrcl00f)
022500101220     C                   open      wwrcl00f
022600990914     C                   endif
022700990910     C*
022800990910     C                   clear                   §CTROK
022900990910     C                   clear                   §CTRNO
023000001009     C                   clear                   recko
023100101220     C*
023200101220     C                   z-add     *zeros        wRecCount         9 0
023300990910     C*
023400921023     C                   DO        *HIVAL
023500101220     C*
023600990915     C                   READ      tivin00r                               70
023700000905     C                   if        vindta > *blanks
023800000613     C                   add       1             rrnum
023900000724     C                   if        *in70 = *off and
024000990913     C                             (vinflg = *blanks
024100990913     C                              or vinflg = '0'
024200990913     C                              or vinflg = '2')
024300990913     C*
024400990913     C                   clear                   vinmsg
024500000724     C                   if        vindta <> *blanks
024600101220     C                   movel(p)  vindta        DS_INPUT
024700101220     C*
024800101220     C                   select
024900101220     C                   when      DS_I_HEADER = 'H' and wRecCount = 0
025000101220     C                   clear                   DATI
025100101220     C                   eval      DATI = DS_I_DATI
025200101220     C*
025300101220     C                   when      DS_I_HEADER = 'H' and wRecCount > 0
025400101220     C                   write(e)  WWRCL000
025500101220     C                   clear                   DATI
025600101220     C                   eval      DATI = DS_I_DATI
025700101220     C*
025800101220     C                   other
025900101220     C                   eval      DATI = %subst(DATI:1:
026000101220     C                                    %size(DS_I_DATI)*(DS_I_SEQUEN-1))+
026100101220     C                                    DS_I_DATI
026200101220     C                   endsl
026300101220     C                   add       1             wRecCount
026400990910     C*
026500000724     C                   if        not %error
026600000724     C                   add       1             §ctrok
026700990910     C                   eval      vinflg = '1'
026800990910     C                   else
026900000724     C                   add       1             §ctrno
027000000621     C                   eval      recko = vindta
027100000616     C                   except    rigadett
027200990910     C                   eval      vinflg = '2'
027300990910     C                   endif
027400000724     C                   endif
027500991022     C                   endif
027600000905     C*
027700000905     C                   else
027800000905     C                   eval      vinflg = '1'
027900000905     C                   endif
028000000905     C*
028100000905     C  N70              update    tivin000
028200991022     C*
028300991022     C  N70              ENDdo
028400101221     C*
028500101221     C* Scarico il buffer rimasto "in canna"
028600101221     C                   write(e)  WWRCL000
028700101221     C*
028800990910     C* Se non ci sono record con errori ...
028900000710     C                   if        §ctrno = 0
029000990910     C* ... restituisco esito OK.
029100990921     C                   eval      wrkesito = '0'
029200990910     C                   else
029300000710     C                   if        §ctrok > 0
029400990921     C                   eval      wrkesito = '1'
029500000710     C                   else
029600000710     C                   eval      wrkesito = '2'
029700990910     C                   endif
029800000710     C                   endif
029900990910     C*
030000990914     C                   if        %open(tivin00r)
030100990908     C                   close     tivin00r
030200990914     C                   endif
030300101220     C                   if        %open(wwrcl00f)
030400101220     C                   close     wwrcl00f
030500990914     C                   endif
030600990920     C*
030700000724     C                   if        wrkesito <> '2'
030800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
030900070913     C                   movel(p)  CMD(8)        depcmd
031000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
031100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
031200000724     C                   Z-ADD     100           LENGH            15 5
031300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
031400000724     C                   PARM                    depcmd
031500000724     C                   PARM                    LENGH
031600000724     C*
031700070913     C                   movel(p)  CMD(9)        depcmd
031800000724     C                   eval      depcmd = %trim(depcmd)
031900000724     C                   Z-ADD     100           LENGH            15 5
032000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
032100000724     C                   PARM                    depcmd
032200000724     C                   PARM                    LENGH
032300000724     C*
032400000724     C                   movel(p)  CMD(4)        depcmd
032500000724     C                   eval      depcmd = %trim(depcmd) +
032600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
032700090109     C                             %trim(CurrFileAS) +
032800070913     C                             %trim(CMD(6)) +
032900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
033000070913     C                             %trim(CMD(7))
033100000724     C                   Z-ADD     100           LENGH            15 5
033200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
033300000724     C                   PARM                    depcmd
033400000724     C                   PARM                    LENGH
033500000724     C*
033600000724     C                   endif
033700000724     C*
033800910830     C                   ENDSR
033900000613     C***
034000000725
034100000725
034200000725     C*--------------------------------------------------------
034300000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
034400000725     C*--------------------------------------------------------
034500000725     C     STPR          BEGSR
034600000725     C*
034700030613     C                   if        not %open(tis7pxp)
034800030613     C                   open      tis7pxp
034900000725     C                   endif
035000000725     C*
035100000725     C                   except    riepilogo
035200000725     C*
035300030613     C                   if        %open(tis7pxp)
035400030613     C                   close     tis7pxp
035500000725     C                   endif
035600000725     C*
035700000725     C                   ENDSR
035800000725     C***
035900990920
036000990910
036100000613     C     *inzsr        BEGSR
036200990910     C*
036300990910     C     *entry        plist
036400990920     C                   parm                    tivlrds
036500990921     C                   parm      wrkesito      esito
036600000724     C                   parm                    prmlit
036700000724     C                   parm                    prmfir
036800000613     C*
036900000613     C                   ENDSR
037000000613     C***
037100000725
037200030613     Otis7pxp   E            riepilogo         2
037300000725     O                                              'Upload via FTP'
037400000725     O                                           +1 'Traduzione TIVIN00R -'
037500101220     O                                           +1 'WWRCL00F'
037600000725     O                                           +1 'Entrate'
037700000725     O          E            riepilogo   2
037800000725     O                       wrkdata
037900000725     O                       wrkora              +1
038000000725     O                       procname            +1
038100000725     O          E            riepilogo   2
038200000725     O                                              'Cliente..................:'
038300030613     O                       VLRKSC              +1
038400000725     O          E            riepilogo   2
038500000725     O                                              'Riferimento Strategi.....:'
038600000725     O                       vlrhdl              +1
038700000725     O          E            riepilogo   2
038800000725     O                                              'Giusti...................:'
038900000725     O                       §CTROK        2   +  1
039000000725     O          E            riepilogo   2
039100000725     O                                              'Sbagliati e scartati.....:'
039200000725     O                       §CTRNO        2   +  1
039300990908
039400000724
039500030613     Otis7pxps  E            testdett          2
039600000724     O                                              'Upload via FTP'
039700000613     O                                           +1 'Traduzione TIVIN00R -'
039800101220     O                                           +1 'WWRCL00F'
039900000725     O                                           +1 'Entrate'
040000000616     O          E            testdett    3
040100000613     O                                           +2 'N° rec'
040200000613     O                                           +3 'Anteprima contenuto'
040300000616     O          E            rigadett    2
040400000613     O                       rrnum               +2
040500000621     O                       recko               +3
040600000616     O          E            findett     2
040700000613     O                       wrkdata
040800000613     O                       wrkora              +1
040900000613     O                       procname            +1
041000000616     O          E            findett     2
041100000613     O                                              'Cliente..................:'
041200030613     O                       VLRKSC              +1
041300000616     O          E            findett     2
041400000613     O                                              'Riferimento Strategi.....:'
041500000613     O                       vlrhdl              +1
041600000616     O          E            findett     2
041700000613     O                                              'Giusti...................:'
041800000613     O                       §CTROK        2   +  1
041900000616     O          E            findett     2
042000000613     O                                              'Sbagliati e scartati.....:'
042100000613     O                       §CTRNO        2   +  1
042200000512** CMD - COMANDI CL
042300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
042400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
042500030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
042600101220CPYF FROMFILE(WWRCL00F) TOFILE(
042700070913/
042800070913) FROMMBR(
042900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
043000101220DLCOBJ OBJ((WWRCL00F *FILE *EXCL
043100101220DLTOVR FILE(WWRCL00F) LVL(*JOB)
