000100001009      /TITLE Upload via FTP: traduzione file LED XX3AR1P
000200990908     H dftactgrp(*yes)
000300990908
000400060119     Ftivin00r  uF   E             DISK    usropn
000500060119     FWP3SC1P   O    E             DISK    usropn
000600001009     Ftis7rxp   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800001009     Ftis7rxps  O    f  198        PRINTER usropn
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
001900060119     D passaggio     e ds                  extname(WP3SC1P)
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
003700000616     C*
003800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
003900000725     C                   movel(p)  CMD(1)        depcmd
004000000725     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
004100000616     C*
004200000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
004300000724     C                   Z-ADD     100           LENGH            15 5
004400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
004500000616     C                   PARM                    depcmd
004600000616     C                   PARM                    LENGH
004700000725     C*
004800000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
004900000725     C                   movel(p)  CMD(2)        depcmd
005000000725     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
005100000725     C*
005200000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
005300000725     C                   Z-ADD     100           LENGH            15 5
005400000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005500000725     C                   PARM                    depcmd
005600000725     C                   PARM                    LENGH
005700000616     C*
005800001009     C                   if        not %open(tis7rxps)
005900001009     C                   open      tis7rxps
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600001009     C                   if        %open(tis7rxps)
006700000616     C                   except    findett
006800001009     C                   close     tis7rxps
006900000613     C                   endif
007000000616     C*
007100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
007200000724     C                   movel(p)  CMD(3)        depcmd
007300000724     C                   Z-ADD     100           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000724     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000613     C*
007800000724     C
007900000613     C                   return
008000000613
008100000724
008200910830     C*--------------------------------------------------------
008300001009     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED WP3AR1       *
008400910830     C*--------------------------------------------------------
008500000724     C     RWFIL         BEGSR
008600990910     C*
008700990914     C                   if        not %open(tivin00r)
008800990908     C                   open      tivin00r
008900990914     C                   endif
009000060119     C                   if        not %open(WP3SC1P)
009100060119     C                   open      WP3SC1P
009200990914     C                   endif
009300990910     C*
009400990910     C                   clear                   §CTROK
009500990910     C                   clear                   §CTRNO
009600001009     C                   clear                   X1DEP
009700001009     C                   clear                   recko
009800990910     C*
009900921023     C                   DO        *HIVAL
010000990913     C*
010100990915     C                   READ      tivin00r                               70
010200000905     C                   if        vindta > *blanks
010300000613     C                   add       1             rrnum
010400000724     C                   if        *in70 = *off and
010500990913     C                             (vinflg = *blanks
010600990913     C                              or vinflg = '0'
010700990913     C                              or vinflg = '2')
010800990913     C*
010900990913     C                   clear                   vinmsg
011000000724     C                   if        vindta <> *blanks
011100000724     C                   movel(p)  vindta        passaggio
011200060119     C                   write(e)  XX3SC1
011300990910     C*
011400000724     C                   if        not %error
011500000724     C                   add       1             §ctrok
011600990910     C                   eval      vinflg = '1'
011700990910     C                   else
011800000724     C                   add       1             §ctrno
011900000621     C                   eval      recko = vindta
012000000616     C                   except    rigadett
012100990910     C                   eval      vinflg = '2'
012200990910     C                   endif
012300000724     C                   endif
012400991022     C                   endif
012500000905     C*
012600000905     C                   else
012700000905     C                   eval      vinflg = '1'
012800000905     C                   endif
012900000905     C*
013000000905     C  N70              update    tivin000
013100991022     C*
013200991022     C  N70              ENDdo
013300990910
013400990910     C* Se non ci sono record con errori ...
013500000710     C                   if        §ctrno = 0
013600990910     C* ... restituisco esito OK.
013700990921     C                   eval      wrkesito = '0'
013800990910     C                   else
013900000710     C                   if        §ctrok > 0
014000990921     C                   eval      wrkesito = '1'
014100000710     C                   else
014200000710     C                   eval      wrkesito = '2'
014300990910     C                   endif
014400000710     C                   endif
014500990910     C*
014600990914     C                   if        %open(tivin00r)
014700990908     C                   close     tivin00r
014800990914     C                   endif
014900060119     C                   if        %open(WP3SC1P)
015000060119     C                   close     WP3SC1P
015100990914     C                   endif
015200990920     C*
015300000724     C                   if        wrkesito <> '2'
015400000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
015500000724     C                   movel(p)  CMD(7)        depcmd
015600000724     C                   eval      depcmd = %trim(depcmd) + ' '+
015700000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
015800000724     C                   Z-ADD     100           LENGH            15 5
015900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016000000724     C                   PARM                    depcmd
016100000724     C                   PARM                    LENGH
016200000724     C*
016300000724     C                   movel(p)  CMD(8)        depcmd
016400000724     C                   eval      depcmd = %trim(depcmd)
016500000724     C                   Z-ADD     100           LENGH            15 5
016600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016700000724     C                   PARM                    depcmd
016800000724     C                   PARM                    LENGH
016900000724     C*
017000000724     C                   movel(p)  CMD(4)        depcmd
017100000724     C                   eval      depcmd = %trim(depcmd) +
017200000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017300000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017400000724     C                             %trim(CMD(6))
017500000724     C                   Z-ADD     100           LENGH            15 5
017600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017700000724     C                   PARM                    depcmd
017800000724     C                   PARM                    LENGH
017900000724     C*
018000000724     C                   endif
018100000724     C*
018200910830     C                   ENDSR
018300000613     C***
018400000725
018500000725
018600000725     C*--------------------------------------------------------
018700000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018800000725     C*--------------------------------------------------------
018900000725     C     STPR          BEGSR
019000000725     C*
019100001009     C                   if        not %open(tis7rxp)
019200001009     C                   open      tis7rxp
019300000725     C                   endif
019400000725     C*
019500000725     C                   except    riepilogo
019600000725     C*
019700001009     C                   if        %open(tis7rxp)
019800001009     C                   close     tis7rxp
019900000725     C                   endif
020000000725     C*
020100000725     C                   ENDSR
020200000725     C***
020300990920
020400990910
020500000613     C     *inzsr        BEGSR
020600990910     C*
020700990910     C     *entry        plist
020800990920     C                   parm                    tivlrds
020900990921     C                   parm      wrkesito      esito
021000000724     C                   parm                    prmlit
021100000724     C                   parm                    prmfir
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000725
021600001009     Otis7rxp   E            riepilogo         2
021700000725     O                                              'Upload via FTP'
021800000725     O                                           +1 'Traduzione TIVIN00R -'
021900060119     O                                           +1 'WP3SC1P'
022000000725     O                                           +1 'Entrate'
022100000725     O          E            riepilogo   2
022200000725     O                       wrkdata
022300000725     O                       wrkora              +1
022400000725     O                       procname            +1
022500000725     O          E            riepilogo   2
022600000725     O                                              'Cliente..................:'
022700001009     O                       X1DEP               +1
022800000725     O          E            riepilogo   2
022900000725     O                                              'Riferimento Strategi.....:'
023000000725     O                       vlrhdl              +1
023100000725     O          E            riepilogo   2
023200000725     O                                              'Giusti...................:'
023300000725     O                       §CTROK        2   +  1
023400000725     O          E            riepilogo   2
023500000725     O                                              'Sbagliati e scartati.....:'
023600000725     O                       §CTRNO        2   +  1
023700990908
023800000724
023900001009     Otis7rxps  E            testdett          2
024000000724     O                                              'Upload via FTP'
024100000613     O                                           +1 'Traduzione TIVIN00R -'
024200060119     O                                           +1 'WP3SC1P'
024300000725     O                                           +1 'Entrate'
024400000616     O          E            testdett    3
024500000613     O                                           +2 'N° rec'
024600000613     O                                           +3 'Anteprima contenuto'
024700000616     O          E            rigadett    2
024800000613     O                       rrnum               +2
024900000621     O                       recko               +3
025000000616     O          E            findett     2
025100000613     O                       wrkdata
025200000613     O                       wrkora              +1
025300000613     O                       procname            +1
025400000616     O          E            findett     2
025500000613     O                                              'Cliente..................:'
025600001009     O                       X1DEP               +1
025700000616     O          E            findett     2
025800000613     O                                              'Riferimento Strategi.....:'
025900000613     O                       vlrhdl              +1
026000000616     O          E            findett     2
026100000613     O                                              'Giusti...................:'
026200000613     O                       §CTROK        2   +  1
026300000616     O          E            findett     2
026400000613     O                                              'Sbagliati e scartati.....:'
026500000613     O                       §CTRNO        2   +  1
026600000512** CMD - COMANDI CL
026700001009OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7RXP)  OUTQ(
026800001009OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7RXPS) OUTQ(
026900001009DLTOVR FILE(TIS7RXP TIS7RXPS) LVL(*)
027000060119CPYF FROMFILE(WP3SC1P) TOFILE(
027100060119/WP3SC1P) FROMMBR(
027200060119) TOMBR(WP3SC1P) MBROPT(*ADD)
027300060119DLCOBJ OBJ((WP3SC1P *FILE *EXCL
027400060119DLTOVR FILE(WP3SC1P) LVL(*JOB)
