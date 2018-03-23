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
001400040325     D cmd             S             80    DIM(11) CTDATA PERRCD(1)
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
003700040322     C*
003800040322     C* INIZIALIZZO FLAG D ERRORE X CONFLITTO EDI
003900040322     C                   MOVEL     '0'           wERREDI           1
004000010905     C*
004100010905     C* IMPOSTO LA LUNGHEZZA DELLA STRINGA DI COMANDO
004200010905     C                   EVAL      LENGH = %len(depcmd)
004300010905     C*
004400010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
004500010905     C                   MOVEL(p)  CMD(4)        depcmd
004600010905     C*
004700010905     C* PROVO A ALLOCARE LA DATA AREA DI WRK
004800010905     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
004900010905     C                   PARM                    depcmd
005000010905     C                   PARM                    LENGH
005100010905     C*
005200040322     C* SE NON RIESCE L'ALLOCAZIONE MANDO MESSAGGIO A VIDEO, ALTRIMENTI PROCEDO NORMALMENTE
005300010905     C                   IF        %error
005400040322     C                   MOVEL     '1'           wERREDI
005500010905     C*
005600010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
005700010905     C*                  EVAL      depcmd = %trim(CMD(5)) + ' ' +
005800010905     C*                                     %trim(CMD(6)) + ' ' +
005900010905     C*                                     %trim(CMD(7))
006000010905     C*
006100010905     C* MANDO IL PRIMO MESSAGGIO IN *BREAK SULLA CODA DELL'OPERATORE
006200010905     C*                  CALL(e)   'QCMDEXC'                                    *LANCIA CMD
006300010905     C*                  PARM                    depcmd
006400010905     C*                  PARM                    LENGH
006500010905     C*
006600010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
006700010905     C                   EVAL      depcmd = %trim(CMD(5)) + ' ' +
006800010905     C                                      %trim(CMD(6)) + ' ' +
006900010905     C                                      %trim(CMD(8))
007000010905     C*
007100010905     C* MANDO IL PRIMO MESSAGGIO INFORMATIVO A EDPAB
007200010905     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
007300010905     C                   PARM                    depcmd
007400010905     C                   PARM                    LENGH
007500040322     C*
007600040322     C                   ENDIF
007700010905     C*
007800040322     C* VERIFICO SE NN ANCORA INCORSI ERRORI X CONFLITTI CON EDI
007900040322     C                   IF        wERREDI = '0'
008000040322     C*
008100040322     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008200040322     C                   MOVEL(p)  CMD(10)       depcmd
008300040322     C*
008400040322     C* CONTROLLO ESISTENZA FILE EDFIL00W IN GAITRAGRU
008500040322     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
008600040322     C                   PARM                    depcmd
008700040322     C                   PARM                    LENGH
008800040322     C*
008900040322     C* GESTISCO L'ERRORE EVENTUALE, SE NN TROVATO FILE => PROVO A CREARLO
009000040322     C                   IF        %error
009100010314     C*
009200040322     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300010314     C                   MOVEL(p)  CMD(3)        depcmd
009400010314     C*
009500010314     C* CREAZIONE MONITORIZZATA DEL FILE TRADOTTO IN QUANTO PUO ESSERE MANCANTE X ERRORI DA INTESA
009600010314     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
009700010314     C                   PARM                    depcmd
009800010314     C                   PARM                    LENGH
009900040211     C*
010000040211     C* GESTISCO L'ERRORE EVENTUALE, SE NN RIESCO A CREARE IL FILE => DISALLOCO LA *DTAARA EDISNDRCV
010100040211     C                   IF        %error
010200040322     C                   MOVEL     '1'           wERREDI
010300040322     C                   ENDIF
010400040322     C                   ENDIF
010500040322     C                   ENDIF
010600040322     C*
010700040322     C* VERIFICO SE NN ANCORA INCORSI ERRORI X CONFLITTI CON EDI
010800040322     C                   IF        wERREDI = '0'
010900040325     C*
011000040325     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100040325     C                   MOVEL(p)  CMD(11)       depcmd
011200040325     C*
011300040325     C* ALLOCO IL FILE EDFIL00W IN GAITRAGRU
011400040325     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
011500040325     C                   PARM                    depcmd
011600040325     C                   PARM                    LENGH
011700040325     C*
011800040325     C* GESTISCO L'ERRORE EVENTUALE, SE NN TROVATO FILE => PROVO A CREARLO
011900040325     C                   IF        %error
012000040325     C                   ELSE
012100000725     C*
012200000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012300010314     C                   MOVEL(p)  CMD(1)        depcmd
012400000725     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012500000725     C*
012600000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012700000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012800000725     C                   PARM                    depcmd
012900000725     C                   PARM                    LENGH
013000000616     C*
013100010313     C                   if        not %open(tis7edps)
013200010313     C                   open      tis7edps
013300000616     C                   except    testdett
013400000613     C                   endif
013500000613     C*
013600000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
013700000613     C*
013800010313     C                   if        %open(tis7edps)
013900000616     C                   except    findett
014000010313     C                   close     tis7edps
014100000613     C                   endif
014200000616     C*
014300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014400010313     C                   movel(p)  CMD(2)        depcmd
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000724     C                   PARM                    depcmd
014700000616     C                   PARM                    LENGH
014800040322     C*
014900040322     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
015000040322     C                   MOVEL(p)  CMD(9)        depcmd
015100040322     C*
015200040322     C* PROVO A DISALLOCARE LA DATA AREA DI WRK
015300040322     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015400040322     C                   PARM                    depcmd
015500040322     C                   PARM                    LENGH
015600040322     C                   ENDIF
015700040325     C                   ENDIF
015800000613     C*
015900010313     C                   seton                                        LR
016000000613
016100000724
016200910830     C*--------------------------------------------------------
016300010313     C* RWFIL   LEGGE tivin00r E SCRIVE FILE EDI EDFIL00W     *
016400910830     C*--------------------------------------------------------
016500000724     C     RWFIL         BEGSR
016600990910     C*
016700990914     C                   if        not %open(tivin00r)
016800990908     C                   open      tivin00r
016900990914     C                   endif
017000010313     C                   if        not %open(edfil00w)
017100010313     C                   open      edfil00w
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
019000010313     C                   movel(p)  vindta        f_edfil00w
019100010313     C                   write(e)  edfil000
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
022800010313     C                   if        %open(edfil00w)
022900010313     C                   close     edfil00w
023000990914     C                   endif
023100000724     C*
023200910830     C                   ENDSR
023300000613     C***
023400000725
023500000725
023600990910
023700000613     C     *inzsr        BEGSR
023800990910     C*
023900990910     C     *entry        plist
024000990920     C                   parm                    tivlrds
024100990921     C                   parm      wrkesito      esito
024200000724     C                   parm                    prmlit
024300000724     C                   parm                    prmfir
024400000613     C*
024500000613     C                   ENDSR
024600000613     C***
024700000725
024800990908
024900000724
025000010313     Otis7edps  E            testdett          2
025100000724     O                                              'Upload via FTP'
025200000613     O                                           +1 'Traduzione TIVIN00R -'
025300010313     O                                           +1 'EDFIL00W'
025400000725     O                                           +1 'Entrate'
025500000616     O          E            testdett    3
025600000613     O                                           +2 'N° rec'
025700000613     O                                           +3 'Anteprima contenuto'
025800000616     O          E            rigadett    2
025900000613     O                       rrnum               +2
026000000621     O                       recko               +3
026100000616     O          E            findett     2
026200000613     O                       wrkdata
026300000613     O                       wrkora              +1
026400000613     O                       procname            +1
026500000616     O          E            findett     2
026600000613     O                                              'Cliente..................:'
026700010313     O                                           +1 'INET'
026800000616     O          E            findett     2
026900000613     O                                              'Riferimento Strategi.....:'
027000000613     O                       vlrhdl              +1
027100000616     O          E            findett     2
027200000613     O                                              'Giusti...................:'
027300000613     O                       §CTROK        2   +  1
027400000616     O          E            findett     2
027500000613     O                                              'Sbagliati e scartati.....:'
027600000613     O                       §CTRNO        2   +  1
027700000512** CMD - COMANDI CL
027800010313OVRPRTF FILE(TIS7EDPS) TOFILE(TIS7RXPS) OVRSCOPE(*CALLLVL) OUTQ(
027900010313DLTOVR FILE(TIS7EDPS) LVL(*)
028000010314CRTPF FILE(GAITRAGRU/EDFIL00W) RCDLEN(80)
028100040322ALCOBJ OBJ((GAITRAGRU/EDISNDRCV *DTAARA *EXCL)) WAIT(10)
028200010905SNDBRKMSG  MSG('INET/SMART - conflitto in ricezione
028300010905area dati EDSNDRCV allocata.')
028400010905TOMSGQ(DSP01) MSGTYPE(*INQ)
028500010905TOUSR(EDPAB)
028600040211DLCOBJ OBJ((EDISNDRCV *DTAARA *EXCL))
028700040322CHKOBJ OBJ(GAITRAGRU/EDFIL00W) OBJTYPE(*FILE)
028800040325ALCOBJ OBJ((GAITRAGRU/EDFIL00W *FILE *EXCL))
