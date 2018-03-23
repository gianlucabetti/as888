000100001102      /TITLE Upload via FTP: traduzione file LED EXPORT*
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500001102     FWP3ARAP   O    E             DISK    usropn
000600001102     Ftis7asp1p O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800001102     Ftis7asp1psO    f  198        PRINTER usropn
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
001900001102     D passaggio     e ds                  extname(wp3arap)
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
005800001102     C                   if        not %open(tis7asp1ps)
005900001102     C                   open      tis7asp1ps
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600001102     C                   if        %open(tis7asp1ps)
006700000616     C                   except    findett
006800001102     C                   close     tis7asp1ps
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
008400001102     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED WP3AA1       *
008500910830     C*--------------------------------------------------------
008600000724     C     RWFIL         BEGSR
008700990910     C*
008800990914     C                   if        not %open(tivin00r)
008900990908     C                   open      tivin00r
009000990914     C                   endif
009100001102     C                   if        not %open(wp3arap)
009200001102     C                   open      wp3arap
009300990914     C                   endif
009400990910     C*
009500990910     C                   clear                   §CTROK
009600990910     C                   clear                   §CTRNO
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
011300001102     C                   write(e)  artas
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
012500991022     C                   endif
012600000905     C*
012700000905     C                   else
012800000905     C                   eval      vinflg = '1'
012900000905     C                   endif
013000000905     C*
013100000905     C  N70              update    tivin000
013200991022     C*
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
015000001102     C                   if        %open(wp3arap)
015100001102     C                   close     wp3arap
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
018500000725
018600000725
018700000725     C*--------------------------------------------------------
018800000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018900000725     C*--------------------------------------------------------
019000000725     C     STPR          BEGSR
019100000725     C*
019200001102     C                   if        not %open(tis7asp1p)
019300001102     C                   open      tis7asp1p
019400000725     C                   endif
019500000725     C*
019600000725     C                   except    riepilogo
019700000725     C*
019800001102     C                   if        %open(tis7asp1p)
019900001102     C                   close     tis7asp1p
020000000725     C                   endif
020100000725     C*
020200000725     C                   ENDSR
020300000725     C***
020400990920
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
021600000725
021700001102     Otis7asp1p E            riepilogo         2
021800000725     O                                              'Upload via FTP'
021900000725     O                                           +1 'Traduzione TIVIN00R -'
022000001102     O                                           +1 'WP3ARAP'
022100000725     O                                           +1 'Entrate'
022200000725     O          E            riepilogo   2
022300000725     O                       wrkdata
022400000725     O                       wrkora              +1
022500000725     O                       procname            +1
022600000725     O          E            riepilogo   2
022700000725     O                                              'Cliente..................:'
022800001102     O                                           +1 '01ASPSERVI'
022900000725     O          E            riepilogo   2
023000000725     O                                              'Riferimento Strategi.....:'
023100000725     O                       vlrhdl              +1
023200000725     O          E            riepilogo   2
023300000725     O                                              'Giusti...................:'
023400000725     O                       §CTROK        2   +  1
023500000725     O          E            riepilogo   2
023600000725     O                                              'Sbagliati e scartati.....:'
023700000725     O                       §CTRNO        2   +  1
023800990908
023900000724
024000001102     Otis7asp1psE            testdett          2
024100000724     O                                              'Upload via FTP'
024200000613     O                                           +1 'Traduzione TIVIN00R -'
024300001102     O                                           +1 'WP3ARAP'
024400000725     O                                           +1 'Entrate'
024500000616     O          E            testdett    3
024600000613     O                                           +2 'N° rec'
024700000613     O                                           +3 'Anteprima contenuto'
024800000616     O          E            rigadett    2
024900000613     O                       rrnum               +2
025000000621     O                       recko               +3
025100000616     O          E            findett     2
025200000613     O                       wrkdata
025300000613     O                       wrkora              +1
025400000613     O                       procname            +1
025500000616     O          E            findett     2
025600000613     O                                              'Cliente..................:'
025700001102     O                                           +1 '01ASPSERVI'
025800000616     O          E            findett     2
025900000613     O                                              'Riferimento Strategi.....:'
026000000613     O                       vlrhdl              +1
026100000616     O          E            findett     2
026200000613     O                                              'Giusti...................:'
026300000613     O                       §CTROK        2   +  1
026400000616     O          E            findett     2
026500000613     O                                              'Sbagliati e scartati.....:'
026600000613     O                       §CTRNO        2   +  1
026700000512** CMD - COMANDI CL
026800001102OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7ASP1P)  OUTQ(
026900001102OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7ASP1PS) OUTQ(
027000001102DLTOVR FILE(TIS7ASP1P TIS7ASP1PS) LVL(*)
027100001102CPYF FROMFILE(WP3ARAP) TOFILE(
027200001102/WP3ARAP) FROMMBR(
027300001102) TOMBR(WP3ARAP) MBROPT(*ADD)
027400001102DLCOBJ OBJ((WP3ARAP *FILE *EXCL
027500001102DLTOVR FILE(WP3ARAP) LVL(*JOB)
