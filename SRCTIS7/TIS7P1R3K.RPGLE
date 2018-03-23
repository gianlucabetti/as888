000100101220      /TITLE Upload via FTP: traduzione file LED WWRCL00F
000200990908     H dftactgrp(*yes)
000300150610
000400990910     Ftivin00r  uF   E             DISK    usropn
000500101220     FWWRCL00F  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800000512     D* COMANDI
000900000512     D*------------
001000090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001100000512     D*------------
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500990910     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900990915     D wrkdata         s               d
002000990915     D wrkora          s               t
002100000613     D rrnum           s              6  0 INZ(*zeros)
002200000621     D recko           s            150    INZ(*blanks)
002300000724     D depcmd          s            100    INZ(*blanks)
002400000724     D §ctrok          s              5  0 INZ(*zeros)
002500000724     D §ctrno          s              5  0 INZ(*zeros)
002600090109     D wNomeFile       s             30    INZ(*blanks)
002700090121     D CurrOpz         s              2    INZ
002800090121     D CurrFileAS      s             10    INZ
002900090129     D CurrPostPgm     s             10    INZ
003000090129     D CurrPostLib     s             10    INZ
003100090121     D CurrSito        s              2    INZ
003200090121     D CurrDepot       s              8    INZ
003300090121     D CurrEnv         s              1    INZ
003400090121     D CurrTip         s              2    INZ
003500101220
003600101220
003700101220     D*-------------------
003800101220     D* DS RIAGGREGAZIONE SEGMENTI RECORD
003900101220     D*-------------------
004000101220     D  DS_INPUT       DS
004100101220     D  DS_I_HEADER                   1A
004200101220     D  DS_I_SEQUEN                   7S 0
004300101220     D  DS_I_DATI                  2040A
004400101220
004500101220
004600090109     D*-------------------
004700090109     D* COSTANTI
004800090109     D*-------------------
004900090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100990908
005200990915     C                   time                    wrkdata
005300990915     C                   time                    wrkora
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700090109     C*
005800090109     C* EFFETTUO CONSIDERAZIONI SUL FILE DESTINAZINOE
005900090121     C                   EVAL      CurrOpz     = *blanks
006000090121     C                   EVAL      CurrFileAS  = *blanks
006100090129     C                   EVAL      CurrPostPgm = *blanks
006200090121     C                   EVAL      CurrEnv     = %trim(%subst(vlrppt:1:1))
006300090121     C                   EVAL      CurrSito    = %trim(%subst(vlrppt:22:2))
006400090121     C                   EVAL      CurrDepot   = %trim(%subst(vlrppt:24:8))
006500090129     C                   EVAL      CurrPostLib = %trim(%subst(vlrppt:32:10))
006600090121     C*
006700090121     C                   IF        %trim(%subst(vlrppt:2:10)) = '*FILE'
006800090109     C                   MOVE(P)   vlrMSG        wNomeFile
006900090109     C* Porto il nome file sempre in maiuscolo
007000090109     C     minu:maiu     XLATE     wNomeFile     wNomeFile
007100090109     C*
007200090109     C* ...verifico particolarità cliente...
007300090109     C*
007400090109     C* => VGAMES
007500150427     C                   IF        (CurrSito='05' AND CurrDepot='VIVENDI') or
007600150427     C                             (CurrSito='05' AND CurrDepot='ABCOOP')
007700100310     C                   IF        %scan('IT_ITEMO_':wNomeFile:1)    > *zeros OR
007800100310     C                             %scan('ITBAR_ITEMO_':wNomeFile:1) > *zeros
007900090112     C                   EVAL      CurrOpz     = 'GO'
008000090112     C                   EVAL      CurrFileAS  = 'WP3AITP'
008100110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008200090121     C                   EVAL      CurrTip     = 'IT'
008300090109     C                   ENDIF
008400100503     C                   IF        %scan('IT_POEXO_':wNomeFile:1)    > *zeros OR
008500100503     C                             %scan('ITBAR_POEXO_':wNomeFile:1) > *zeros
008600100503     C                   EVAL      CurrOpz     = 'GO'
008700100503     C                   EVAL      CurrFileAS  = 'WP3APOP'
008800110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
008900100503     C                   EVAL      CurrTip     = 'PO'
009000100503     C                   ENDIF
009100100310     C                   IF        %scan('IT_SORDO_':wNomeFile:1)    > *zeros OR
009200100310     C                             %scan('ITBAR_SORDO_':wNomeFile:1) > *zeros
009300090112     C                   EVAL      CurrOpz     = 'GO'
009400090112     C                   EVAL      CurrFileAS  = 'WP3ASOP'
009500110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
009600090121     C                   EVAL      CurrTip     = 'SO'
009700090112     C                   ENDIF
009800100310     C                   IF        %scan('IT_RMAOO_':wNomeFile:1)    > *zeros OR
009900100310     C                             %scan('ITBAR_RMAOO_':wNomeFile:1) > *zeros
010000090112     C                   EVAL      CurrOpz     = 'GO'
010100090112     C                   EVAL      CurrFileAS  = 'WP3ARMP'
010200110428     C                   EVAL      CurrPostPgm = 'LTCB400C'
010300090121     C                   EVAL      CurrTip     = 'RM'
010400090109     C                   ENDIF
010500090109     C                   ENDIF
010600090109     C*
010700090109     C                   ELSE
010800090112     C                   EVAL      CurrOpz     = 'GO'
010900090121     C                   EVAL      CurrFileAS  = %trim(%subst(vlrppt:2:10))
011000090129     C                   EVAL      CurrPostPgm = %trim(%subst(vlrppt:12:10))
011100090109     C                   ENDIF
011200090109     C*
011300090109     C* VERIFICO LE CONDIZIONI ATTUALI (SE TUTTO OK)
011400090109     C                   select
011500090109     C                   when      CurrOpz = *blanks
011600090109     C                   eval      wrkesito = '2'
011700090109     C                   when      CurrOpz = 'GO'
011800090109     C*
011900090109     C* OPERAZIONI INIZIALI
012000090109     C                   exsr      PREELA
012100000613     C*
012200090109     C* ELABORAZIONE
012300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
012400090109     C*
012500090112     C* RICHIEMO *PGM IN USCITA (se richiesto)
012600090129     C                   if        CurrPostPgm <> *blanks AND
012700090129     C                             CurrPostPgm <> '*NONE'
012800090129     C                   if        CurrPostLib = *blanks
012900090129     C                   if        CurrEnv = 'T'
013000150609     C                   eval      CurrPostLib = 'GEOV72SBT'
013100090129     C                   else
013200150609     C                   eval      CurrPostLib = 'GEOV72SB'
013300090129     C                   endif
013400090129     C                   endif
013500090121     C*
013600110428     C                   call      'TIS7P1C'
013700090129     C                   parm                    CurrPostLib
013800090129     C                   parm                    CurrPostPgm
013900090121     C                   parm                    CurrEnv
014000090121     C                   parm                    CurrSito
014100090121     C                   parm                    CurrDepot
014200090121     C                   parm                    CurrTip
014300090121     C                   parm                    wrkesito
014400090112     C                   endif
014500090121     C*
014600090121     C* OPERAZIONI FINALI
014700090121     C                   exsr      ENDELA
014800090112     C*
014900090109     C                   endsl
015000000616     C*
015100090109     C                   seton                                        lr
015200090109
015300090109
015400090109     C*--------------------------------------------------------
015500090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
015600090109     C*--------------------------------------------------------
015700090109     C     PREELA        BEGSR
015800090109     C*
015900090109     C*
016000090109     C                   ENDSR
016100090109     C***
016200090109
016300090109
016400090109     C*--------------------------------------------------------
016500090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
016600090109     C*--------------------------------------------------------
016700090109     C     ENDELA        BEGSR
016800090109     C*
016900090109     C*
017000090109     C                   ENDSR
017100090109     C***
017200090109
017300090109
017400000613
017500000724
017600910830     C*--------------------------------------------------------
017700101220     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCL00F     *
017800910830     C*--------------------------------------------------------
017900000724     C     RWFIL         BEGSR
018000990910     C*
018100990914     C                   if        not %open(tivin00r)
018200990908     C                   open      tivin00r
018300990914     C                   endif
018400101220     C                   if        not %open(wwrcl00f)
018500101220     C                   open      wwrcl00f
018600990914     C                   endif
018700990910     C*
018800990910     C                   clear                   §CTROK
018900990910     C                   clear                   §CTRNO
019000001009     C                   clear                   recko
019100101220     C*
019200101220     C                   z-add     *zeros        wRecCount         9 0
019300990910     C*
019400921023     C                   DO        *HIVAL
019500101220     C*
019600990915     C                   READ      tivin00r                               70
019700000905     C                   if        vindta > *blanks
019800000613     C                   add       1             rrnum
019900000724     C                   if        *in70 = *off and
020000990913     C                             (vinflg = *blanks
020100990913     C                              or vinflg = '0'
020200990913     C                              or vinflg = '2')
020300990913     C*
020400990913     C                   clear                   vinmsg
020500000724     C                   if        vindta <> *blanks
020600101220     C                   movel(p)  vindta        DS_INPUT
020700101220     C*
020800101220     C                   select
020900101220     C                   when      DS_I_HEADER = 'H' and wRecCount = 0
021000101220     C                   clear                   DATI
021100101220     C                   eval      DATI = DS_I_DATI
021200101220     C*
021300101220     C                   when      DS_I_HEADER = 'H' and wRecCount > 0
021400101220     C                   write(e)  WWRCL000
021500101220     C                   clear                   DATI
021600101220     C                   eval      DATI = DS_I_DATI
021700101220     C*
021800101220     C                   other
021900101220     C                   eval      DATI = %subst(DATI:1:
022000101220     C                                    %size(DS_I_DATI)*(DS_I_SEQUEN-1))+
022100101220     C                                    DS_I_DATI
022200101220     C                   endsl
022300101220     C                   add       1             wRecCount
022400990910     C*
022500000724     C                   if        not %error
022600000724     C                   add       1             §ctrok
022700990910     C                   eval      vinflg = '1'
022800990910     C                   else
022900000724     C                   add       1             §ctrno
023000000621     C                   eval      recko = vindta
023100990910     C                   eval      vinflg = '2'
023200990910     C                   endif
023300000724     C                   endif
023400991022     C                   endif
023500000905     C*
023600000905     C                   else
023700000905     C                   eval      vinflg = '1'
023800000905     C                   endif
023900000905     C*
024000000905     C  N70              update    tivin000
024100991022     C*
024200991022     C  N70              ENDdo
024300101221     C*
024400101221     C* Scarico il buffer rimasto "in canna"
024500101221     C                   write(e)  WWRCL000
024600101221     C*
024700990910     C* Se non ci sono record con errori ...
024800000710     C                   if        §ctrno = 0
024900990910     C* ... restituisco esito OK.
025000990921     C                   eval      wrkesito = '0'
025100990910     C                   else
025200000710     C                   if        §ctrok > 0
025300990921     C                   eval      wrkesito = '1'
025400000710     C                   else
025500000710     C                   eval      wrkesito = '2'
025600990910     C                   endif
025700000710     C                   endif
025800990910     C*
025900990914     C                   if        %open(tivin00r)
026000990908     C                   close     tivin00r
026100990914     C                   endif
026200101220     C                   if        %open(wwrcl00f)
026300101220     C                   close     wwrcl00f
026400990914     C                   endif
026500990920     C*
026600000724     C                   if        wrkesito <> '2'
026700000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
026800070913     C                   movel(p)  CMD(8)        depcmd
026900000724     C                   eval      depcmd = %trim(depcmd) + ' '+
027000000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
027100000724     C                   Z-ADD     100           LENGH            15 5
027200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
027300000724     C                   PARM                    depcmd
027400000724     C                   PARM                    LENGH
027500000724     C*
027600070913     C                   movel(p)  CMD(9)        depcmd
027700000724     C                   eval      depcmd = %trim(depcmd)
027800000724     C                   Z-ADD     100           LENGH            15 5
027900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
028000000724     C                   PARM                    depcmd
028100000724     C                   PARM                    LENGH
028200000724     C*
028300000724     C                   movel(p)  CMD(4)        depcmd
028400000724     C                   eval      depcmd = %trim(depcmd) +
028500090109     C                             %trim(prmlit) + %trim(CMD(5)) +
028600090109     C                             %trim(CurrFileAS) +
028700070913     C                             %trim(CMD(6)) +
028800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
028900070913     C                             %trim(CMD(7))
029000000724     C                   Z-ADD     100           LENGH            15 5
029100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
029200000724     C                   PARM                    depcmd
029300000724     C                   PARM                    LENGH
029400000724     C*
029500000724     C                   endif
029600000724     C*
029700910830     C                   ENDSR
029800000613     C***
029900000725
030000000725
030100000613     C     *inzsr        BEGSR
030200990910     C*
030300990910     C     *entry        plist
030400990920     C                   parm                    tivlrds
030500990921     C                   parm      wrkesito      esito
030600000724     C                   parm                    prmlit
030700000724     C                   parm                    prmfir
030800000613     C*
030900000613     C                   ENDSR
031000000613     C***
031100000725
031200000512** CMD - COMANDI CL
031300161207
031400161207
031500161207
031600101220CPYF FROMFILE(WWRCL00F) TOFILE(
031700070913/
031800070913) FROMMBR(
031900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
032000101220DLCOBJ OBJ((WWRCL00F *FILE *EXCL
032100101220DLTOVR FILE(WWRCL00F) LVL(*JOB)
