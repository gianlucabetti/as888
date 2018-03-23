000100010313      /TITLE Upload via FTP: traduzione file genericxo UPLOAD x INET
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500010313     Fedfil00w  O    E             DISK    usropn
000600010313     F                                     rename(edfil00w:edfil000)
000700010313     F                                     prefix(f_)
000800010313     Ftis7edps  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400040211     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
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
002700010905     D depcmd          s            300    INZ(*blanks)
002800010905     D lengh           s             15  5 INZ(*zeros)
002900000724     D §ctrok          s              5  0 INZ(*zeros)
003000000724     D §ctrno          s              5  0 INZ(*zeros)
003100990908
003200990915     C                   time                    wrkdata
003300990915     C                   time                    wrkora
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700010905     C*
003800010905     C* IMPOSTO LA LUNGHEZZA DELLA STRINGA DI COMANDO
003900010905     C                   EVAL      LENGH = %len(depcmd)
004000010905     C*
004100010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
004200010905     C                   MOVEL(p)  CMD(4)        depcmd
004300010905     C*
004400010905     C* PROVO A ALLOCARE LA DATA AREA DI WRK
004500010905     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
004600010905     C                   PARM                    depcmd
004700010905     C                   PARM                    LENGH
004800010905     C*
004900010905     C* SE NON RIESCE L'ALLOCAZIONE MANDO 2 MESSAGGIO A VIDEO, ALTRIMENTI PROCEDO NORMALMENTE
005000010905     C                   IF        %error
005100010905     C*
005200010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
005300010905     C*                  EVAL      depcmd = %trim(CMD(5)) + ' ' +
005400010905     C*                                     %trim(CMD(6)) + ' ' +
005500010905     C*                                     %trim(CMD(7))
005600010905     C*
005700010905     C* MANDO IL PRIMO MESSAGGIO IN *BREAK SULLA CODA DELL'OPERATORE
005800010905     C*                  CALL(e)   'QCMDEXC'                                    *LANCIA CMD
005900010905     C*                  PARM                    depcmd
006000010905     C*                  PARM                    LENGH
006100010905     C*
006200010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
006300010905     C                   EVAL      depcmd = %trim(CMD(5)) + ' ' +
006400010905     C                                      %trim(CMD(6)) + ' ' +
006500010905     C                                      %trim(CMD(8))
006600010905     C*
006700010905     C* MANDO IL PRIMO MESSAGGIO INFORMATIVO A EDPAB
006800010905     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
006900010905     C                   PARM                    depcmd
007000010905     C                   PARM                    LENGH
007100010905     C*
007200010905     C                   ELSE
007300010314     C*
007400010314     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007500010314     C                   MOVEL(p)  CMD(3)        depcmd
007600010314     C*
007700010314     C* CREAZIONE MONITORIZZATA DEL FILE TRADOTTO IN QUANTO PUO ESSERE MANCANTE X ERRORI DA INTESA
007800010314     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
007900010314     C                   PARM                    depcmd
008000010314     C                   PARM                    LENGH
008100040211     C*
008200040211     C* GESTISCO L'ERRORE EVENTUALE, SE NN RIESCO A CREARE IL FILE => DISALLOCO LA *DTAARA EDISNDRCV
008300040211     C                   IF        %error
008400040211     C*
008500040211     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
008600040211     C                   MOVEL(p)  CMD(9)        depcmd
008700040211     C*
008800040211     C* PROVO A ALLOCARE LA DATA AREA DI WRK
008900040211     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009000040211     C                   PARM                    depcmd
009100040211     C                   PARM                    LENGH
009200040211     C                   ENDIF
009300000725     C*
009400000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500010314     C                   MOVEL(p)  CMD(1)        depcmd
009600000725     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009700000725     C*
009800000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009900000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000725     C                   PARM                    depcmd
010100000725     C                   PARM                    LENGH
010200000616     C*
010300010313     C                   if        not %open(tis7edps)
010400010313     C                   open      tis7edps
010500000616     C                   except    testdett
010600000613     C                   endif
010700000613     C*
010800000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
010900000613     C*
011000010313     C                   if        %open(tis7edps)
011100000616     C                   except    findett
011200010313     C                   close     tis7edps
011300000613     C                   endif
011400000616     C*
011500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011600010313     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011800000724     C                   PARM                    depcmd
011900000616     C                   PARM                    LENGH
012000010905     C*
012100010905     C                   ENDIF
012200000613     C*
012300000724     C
012400010313     C                   seton                                        LR
012500000613
012600000724
012700910830     C*--------------------------------------------------------
012800010313     C* RWFIL   LEGGE tivin00r E SCRIVE FILE EDI EDFIL00W     *
012900910830     C*--------------------------------------------------------
013000000724     C     RWFIL         BEGSR
013100990910     C*
013200990914     C                   if        not %open(tivin00r)
013300990908     C                   open      tivin00r
013400990914     C                   endif
013500010313     C                   if        not %open(edfil00w)
013600010313     C                   open      edfil00w
013700990914     C                   endif
013800990910     C*
013900990910     C                   clear                   §CTROK
014000990910     C                   clear                   §CTRNO
014100001009     C                   clear                   recko
014200990910     C*
014300921023     C                   DO        *HIVAL
014400990913     C*
014500990915     C                   READ      tivin00r                               70
014600000905     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000724     C                   if        *in70 = *off and
014900990913     C                             (vinflg = *blanks
015000990913     C                              or vinflg = '0'
015100990913     C                              or vinflg = '2')
015200990913     C*
015300990913     C                   clear                   vinmsg
015400000724     C                   if        vindta <> *blanks
015500010313     C                   movel(p)  vindta        f_edfil00w
015600010313     C                   write(e)  edfil000
015700990910     C*
015800000724     C                   if        not %error
015900000724     C                   add       1             §ctrok
016000990910     C                   eval      vinflg = '1'
016100990910     C                   else
016200000724     C                   add       1             §ctrno
016300000621     C                   eval      recko = vindta
016400000616     C                   except    rigadett
016500990910     C                   eval      vinflg = '2'
016600990910     C                   endif
016700000724     C                   endif
016800991022     C                   endif
016900000905     C*
017000000905     C                   else
017100000905     C                   eval      vinflg = '1'
017200000905     C                   endif
017300000905     C*
017400000905     C  N70              update    tivin000
017500991022     C*
017600991022     C  N70              ENDdo
017700990910
017800990910     C* Se non ci sono record con errori ...
017900000710     C                   if        §ctrno = 0
018000990910     C* ... restituisco esito OK.
018100990921     C                   eval      wrkesito = '0'
018200990910     C                   else
018300000710     C                   if        §ctrok > 0
018400990921     C                   eval      wrkesito = '1'
018500000710     C                   else
018600000710     C                   eval      wrkesito = '2'
018700990910     C                   endif
018800000710     C                   endif
018900990910     C*
019000990914     C                   if        %open(tivin00r)
019100990908     C                   close     tivin00r
019200990914     C                   endif
019300010313     C                   if        %open(edfil00w)
019400010313     C                   close     edfil00w
019500990914     C                   endif
019600000724     C*
019700910830     C                   ENDSR
019800000613     C***
019900000725
020000000725
020100990910
020200000613     C     *inzsr        BEGSR
020300990910     C*
020400990910     C     *entry        plist
020500990920     C                   parm                    tivlrds
020600990921     C                   parm      wrkesito      esito
020700000724     C                   parm                    prmlit
020800000724     C                   parm                    prmfir
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000725
021300990908
021400000724
021500010313     Otis7edps  E            testdett          2
021600000724     O                                              'Upload via FTP'
021700000613     O                                           +1 'Traduzione TIVIN00R -'
021800010313     O                                           +1 'EDFIL00W'
021900000725     O                                           +1 'Entrate'
022000000616     O          E            testdett    3
022100000613     O                                           +2 'N° rec'
022200000613     O                                           +3 'Anteprima contenuto'
022300000616     O          E            rigadett    2
022400000613     O                       rrnum               +2
022500000621     O                       recko               +3
022600000616     O          E            findett     2
022700000613     O                       wrkdata
022800000613     O                       wrkora              +1
022900000613     O                       procname            +1
023000000616     O          E            findett     2
023100000613     O                                              'Cliente..................:'
023200010313     O                                           +1 'INET'
023300000616     O          E            findett     2
023400000613     O                                              'Riferimento Strategi.....:'
023500000613     O                       vlrhdl              +1
023600000616     O          E            findett     2
023700000613     O                                              'Giusti...................:'
023800000613     O                       §CTROK        2   +  1
023900000616     O          E            findett     2
024000000613     O                                              'Sbagliati e scartati.....:'
024100000613     O                       §CTRNO        2   +  1
024200000512** CMD - COMANDI CL
024300010313OVRPRTF FILE(TIS7EDPS) TOFILE(TIS7RXPS) OVRSCOPE(*CALLLVL) OUTQ(
024400010313DLTOVR FILE(TIS7EDPS) LVL(*)
024500010314CRTPF FILE(GAITRAGRU/EDFIL00W) RCDLEN(80)
024600010905ALCOBJ OBJ((GAITRAGRU/EDISNDRCV *DTAARA *EXCL)) WAIT(2)
024700010905SNDBRKMSG  MSG('INET/SMART - conflitto in ricezione
024800010905area dati EDSNDRCV allocata.')
024900010905TOMSGQ(DSP01) MSGTYPE(*INQ)
025000010905TOUSR(EDPAB)
025100040211DLCOBJ OBJ((EDISNDRCV *DTAARA *EXCL))
