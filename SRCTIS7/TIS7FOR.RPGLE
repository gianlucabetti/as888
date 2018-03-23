000100040609      /TITLE Upload via FTP: traduzione LED - Tipo File "FO"
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040609     FWP3OF1P   O    E             DISK    usropn
000600040609     Ftis7fop   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800040609     Ftis7fops  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400000724     D cmd             S             80    DIM(8) CTDATA PERRCD(1)
001500000512     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900040609     D passaggio     e ds                  extname(WP3OF1P)
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
003100990908
003200990915     C                   time                    wrkdata
003300990915     C                   time                    wrkora
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700000725     C*
003800000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
003900000725     C                   movel(p)  CMD(1)        depcmd
004000000725     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
004100000725     C*
004200000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
004300000725     C                   Z-ADD     100           LENGH            15 5
004400000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
004500000725     C                   PARM                    depcmd
004600000725     C                   PARM                    LENGH
004700000616     C*
004800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
004900000616     C                   movel(p)  CMD(2)        depcmd
005000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
005100000616     C*
005200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
005300000724     C                   Z-ADD     100           LENGH            15 5
005400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005500000616     C                   PARM                    depcmd
005600000616     C                   PARM                    LENGH
005700000616     C*
005800040609     C                   if        not %open(tis7fops)
005900040609     C                   open      tis7fops
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600040609     C                   if        %open(tis7fops)
006700000616     C                   except    findett
006800040609     C                   close     tis7fops
006900000613     C                   endif
007000000616     C*
007100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
007200000724     C                   movel(p)  CMD(3)        depcmd
007300000724     C                   Z-ADD     100           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000724     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000616     C*
007800000613     C*
007900000724     C
008000000613     C                   return
008100000613
008200000724
008300910830     C*--------------------------------------------------------
008400000724     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED WP3OL1       *
008500910830     C*--------------------------------------------------------
008600000724     C     RWFIL         BEGSR
008700990910     C*
008800990914     C                   if        not %open(tivin00r)
008900990908     C                   open      tivin00r
009000990914     C                   endif
009100040609     C                   if        not %open(WP3OF1P)
009200040609     C                   open      WP3OF1P
009300990914     C                   endif
009400990910     C*
009500990910     C                   clear                   §CTROK
009600990910     C                   clear                   §CTRNO
009700040609     C                   clear                   X1DEP
009800001009     C                   clear                   recko
009900990910     C*
010000921023     C                   DO        *HIVAL
010100990913     C*
010200990915     C                   READ      tivin00r                               70
010300000905     C                   if        vindta > *blanks
010400000613     C                   add       1             rrnum
010500000724     C                   if        *in70 = *off and
010600990913     C                             (vinflg = *blanks
010700990913     C                              or vinflg = '0'
010800990913     C                              or vinflg = '2')
010900990913     C*
011000990913     C                   clear                   vinmsg
011100000724     C                   if        vindta <> *blanks
011200000724     C                   movel(p)  vindta        passaggio
011300040609     C                   write(e)  XX3OF1
011400990910     C*
011500000724     C                   if        not %error
011600000724     C                   add       1             §ctrok
011700990910     C                   eval      vinflg = '1'
011800990910     C                   else
011900000724     C                   add       1             §ctrno
012000000621     C                   eval      recko = vindta
012100000616     C                   except    rigadett
012200990910     C                   eval      vinflg = '2'
012300990910     C                   endif
012400000724     C                   endif
012500000905     C                   endif
012600000905     C*
012700000905     C                   else
012800000905     C                   eval      vinflg = '1'
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200000905     C*
013300991022     C  N70              ENDdo
013400990910
013500990910     C* Se non ci sono record con errori ...
013600000710     C                   if        §ctrno = 0
013700990910     C* ... restituisco esito OK.
013800990921     C                   eval      wrkesito = '0'
013900990910     C                   else
014000000710     C                   if        §ctrok > 0
014100990921     C                   eval      wrkesito = '1'
014200000710     C                   else
014300000710     C                   eval      wrkesito = '2'
014400990910     C                   endif
014500000710     C                   endif
014600990910     C*
014700990914     C                   if        %open(tivin00r)
014800990908     C                   close     tivin00r
014900990914     C                   endif
015000040609     C                   if        %open(WP3OF1P)
015100040609     C                   close     WP3OF1P
015200990914     C                   endif
015300990920     C*
015400000724     C                   if        wrkesito <> '2'
015500000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
015600000724     C                   movel(p)  CMD(7)        depcmd
015700000724     C                   eval      depcmd = %trim(depcmd) + ' '+
015800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
015900000724     C                   Z-ADD     100           LENGH            15 5
016000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016100000724     C                   PARM                    depcmd
016200000724     C                   PARM                    LENGH
016300000724     C*
016400000724     C                   movel(p)  CMD(8)        depcmd
016500000724     C                   eval      depcmd = %trim(depcmd)
016600000724     C                   Z-ADD     100           LENGH            15 5
016700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016800000724     C                   PARM                    depcmd
016900000724     C                   PARM                    LENGH
017000000724     C*
017100000724     C                   movel(p)  CMD(4)        depcmd
017200000724     C                   eval      depcmd = %trim(depcmd) +
017300000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017500000724     C                             %trim(CMD(6))
017600000724     C                   Z-ADD     100           LENGH            15 5
017700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017800000724     C                   PARM                    depcmd
017900000724     C                   PARM                    LENGH
018000000724     C*
018100000724     C                   endif
018200000724     C*
018300910830     C                   ENDSR
018400000613     C***
018500990920
018600000725
018700000725     C*--------------------------------------------------------
018800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018900000725     C*--------------------------------------------------------
019000000725     C     STPR          BEGSR
019100000725     C*
019200040609     C                   if        not %open(tis7fop)
019300040609     C                   open      tis7fop
019400000725     C                   endif
019500000725     C*
019600000725     C                   except    riepilogo
019700000725     C*
019800040609     C                   if        %open(tis7fop)
019900040609     C                   close     tis7fop
020000000725     C                   endif
020100000725     C*
020200000725     C                   ENDSR
020300000725     C***
020400000725
020500990910
020600000613     C     *inzsr        BEGSR
020700990910     C*
020800990910     C     *entry        plist
020900990920     C                   parm                    tivlrds
021000990921     C                   parm      wrkesito      esito
021100000724     C                   parm                    prmlit
021200000724     C                   parm                    prmfir
021300000613     C*
021400000613     C                   ENDSR
021500000613     C***
021600990908
021700000725
021800040609     Otis7fop   E            riepilogo         2
021900000725     O                                              'Upload via FTP'
022000000725     O                                           +1 'Traduzione TIVIN00R -'
022100040609     O                                           +1 'WP3OF1P'
022200000725     O                                           +1 'Ordini consegna'
022300000725     O          E            riepilogo   2
022400000725     O                       wrkdata
022500000725     O                       wrkora              +1
022600000725     O                       procname            +1
022700000725     O          E            riepilogo   2
022800000725     O                                              'Cliente..................:'
022900040609     O                       X1DEP               +1
023000000725     O          E            riepilogo   2
023100000725     O                                              'Riferimento Strategi.....:'
023200000725     O                       vlrhdl              +1
023300000725     O          E            riepilogo   2
023400000725     O                                              'Giusti...................:'
023500000725     O                       §CTROK        2   +  1
023600000725     O          E            riepilogo   2
023700000725     O                                              'Sbagliati e scartati.....:'
023800000725     O                       §CTRNO        2   +  1
023900000725
024000000724
024100040609     Otis7fops  E            testdett          2
024200000724     O                                              'Upload via FTP'
024300000613     O                                           +1 'Traduzione TIVIN00R -'
024400040609     O                                           +1 'WP3OF1P'
024500000724     O                                           +1 'Ordini consegna'
024600000616     O          E            testdett    3
024700000613     O                                           +2 'N° rec'
024800000613     O                                           +3 'Anteprima contenuto'
024900000616     O          E            rigadett    2
025000000613     O                       rrnum               +2
025100000621     O                       recko               +3
025200000616     O          E            findett     2
025300000613     O                       wrkdata
025400000613     O                       wrkora              +1
025500000613     O                       procname            +1
025600000616     O          E            findett     2
025700000613     O                                              'Cliente..................:'
025800040609     O                       X1DEP               +1
025900000616     O          E            findett     2
026000000613     O                                              'Riferimento Strategi.....:'
026100000613     O                       vlrhdl              +1
026200000616     O          E            findett     2
026300000613     O                                              'Giusti...................:'
026400000613     O                       §CTROK        2   +  1
026500000616     O          E            findett     2
026600000613     O                                              'Sbagliati e scartati.....:'
026700000613     O                       §CTRNO        2   +  1
026800000512** CMD - COMANDI CL
026900040609OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7FOP)  OUTQ(
027000040609OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7FOPS) OUTQ(
027100040609DLTOVR FILE(TIS7FOPS) LVL(*)
027200040609CPYF FROMFILE(WP3OF1P) TOFILE(
027300040609/WP3OF1P) FROMMBR(
027400040609) TOMBR(WP3OF1P) MBROPT(*ADD)
027500040609DLCOBJ OBJ((WP3OF1P *FILE *EXCL
027600040609DLTOVR FILE(WP3OF1P) LVL(*JOB)
