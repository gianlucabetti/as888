000100050512      /TITLE Upload via FTP: traduzione file LED WP3SMWAP
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050512     FWP3SMWAP  O    E             DISK    usropn
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
001900050512     D passaggio     e ds                  extname(WP3SMWAP)
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
007800000613     C                   return
007900000613
008000000724
008100910830     C*--------------------------------------------------------
008200050512     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED WP3SMWAP     *
008300910830     C*--------------------------------------------------------
008400000724     C     RWFIL         BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900050512     C                   if        not %open(WP3SMWAP)
009000050512     C                   open      WP3SMWAP
009100990914     C                   endif
009200990910     C*
009300990910     C                   clear                   §CTROK
009400990910     C                   clear                   §CTRNO
009500001009     C                   clear                   X1DEP
009600001009     C                   clear                   recko
009700990910     C*
009800921023     C                   DO        *HIVAL
009900990913     C*
010000990915     C                   READ      tivin00r                               70
010100000905     C                   if        vindta > *blanks
010200000613     C                   add       1             rrnum
010300000724     C                   if        *in70 = *off and
010400990913     C                             (vinflg = *blanks
010500990913     C                              or vinflg = '0'
010600990913     C                              or vinflg = '2')
010700990913     C*
010800990913     C                   clear                   vinmsg
010900000724     C                   if        vindta <> *blanks
011000000724     C                   movel(p)  vindta        passaggio
011100050512     C                   write(e)  XX3SMWA
011200990910     C*
011300000724     C                   if        not %error
011400000724     C                   add       1             §ctrok
011500990910     C                   eval      vinflg = '1'
011600990910     C                   else
011700000724     C                   add       1             §ctrno
011800000621     C                   eval      recko = vindta
011900000616     C                   except    rigadett
012000990910     C                   eval      vinflg = '2'
012100990910     C                   endif
012200000724     C                   endif
012300991022     C                   endif
012400000905     C*
012500000905     C                   else
012600000905     C                   eval      vinflg = '1'
012700000905     C                   endif
012800000905     C*
012900000905     C  N70              update    tivin000
013000991022     C*
013100991022     C  N70              ENDdo
013200990910
013300990910     C* Se non ci sono record con errori ...
013400000710     C                   if        §ctrno = 0
013500990910     C* ... restituisco esito OK.
013600990921     C                   eval      wrkesito = '0'
013700990910     C                   else
013800000710     C                   if        §ctrok > 0
013900990921     C                   eval      wrkesito = '1'
014000000710     C                   else
014100000710     C                   eval      wrkesito = '2'
014200990910     C                   endif
014300000710     C                   endif
014400990910     C*
014500990914     C                   if        %open(tivin00r)
014600990908     C                   close     tivin00r
014700990914     C                   endif
014800050512     C                   if        %open(WP3SMWAP)
014900050512     C                   close     WP3SMWAP
015000990914     C                   endif
015100990920     C*
015200000724     C                   if        wrkesito <> '2'
015300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
015400000724     C                   movel(p)  CMD(7)        depcmd
015500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
015600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
015700000724     C                   Z-ADD     100           LENGH            15 5
015800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015900000724     C                   PARM                    depcmd
016000000724     C                   PARM                    LENGH
016100000724     C*
016200000724     C                   movel(p)  CMD(8)        depcmd
016300000724     C                   eval      depcmd = %trim(depcmd)
016400000724     C                   Z-ADD     100           LENGH            15 5
016500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016600000724     C                   PARM                    depcmd
016700000724     C                   PARM                    LENGH
016800000724     C*
016900000724     C                   movel(p)  CMD(4)        depcmd
017000000724     C                   eval      depcmd = %trim(depcmd) +
017100000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017300000724     C                             %trim(CMD(6))
017400000724     C                   Z-ADD     100           LENGH            15 5
017500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017600000724     C                   PARM                    depcmd
017700000724     C                   PARM                    LENGH
017800000724     C*
017900000724     C                   endif
018000000724     C*
018100910830     C                   ENDSR
018200000613     C***
018300000725
018400000725
018500000725     C*--------------------------------------------------------
018600000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018700000725     C*--------------------------------------------------------
018800000725     C     STPR          BEGSR
018900000725     C*
019000001009     C                   if        not %open(tis7rxp)
019100001009     C                   open      tis7rxp
019200000725     C                   endif
019300000725     C*
019400000725     C                   except    riepilogo
019500000725     C*
019600001009     C                   if        %open(tis7rxp)
019700001009     C                   close     tis7rxp
019800000725     C                   endif
019900000725     C*
020000000725     C                   ENDSR
020100000725     C***
020200990920
020300990910
020400000613     C     *inzsr        BEGSR
020500990910     C*
020600990910     C     *entry        plist
020700990920     C                   parm                    tivlrds
020800990921     C                   parm      wrkesito      esito
020900000724     C                   parm                    prmlit
021000000724     C                   parm                    prmfir
021100000613     C*
021200000613     C                   ENDSR
021300000613     C***
021400000725
021500001009     Otis7rxp   E            riepilogo         2
021600000725     O                                              'Upload via FTP'
021700000725     O                                           +1 'Traduzione TIVIN00R -'
021800050512     O                                           +1 'WP3SMWAP'
021900000725     O                                           +1 'Entrate'
022000000725     O          E            riepilogo   2
022100000725     O                       wrkdata
022200000725     O                       wrkora              +1
022300000725     O                       procname            +1
022400000725     O          E            riepilogo   2
022500000725     O                                              'Cliente..................:'
022600001009     O                       X1DEP               +1
022700000725     O          E            riepilogo   2
022800000725     O                                              'Riferimento Strategi.....:'
022900000725     O                       vlrhdl              +1
023000000725     O          E            riepilogo   2
023100000725     O                                              'Giusti...................:'
023200000725     O                       §CTROK        2   +  1
023300000725     O          E            riepilogo   2
023400000725     O                                              'Sbagliati e scartati.....:'
023500000725     O                       §CTRNO        2   +  1
023600990908
023700000724
023800001009     Otis7rxps  E            testdett          2
023900000724     O                                              'Upload via FTP'
024000000613     O                                           +1 'Traduzione TIVIN00R -'
024100050512     O                                           +1 'WP3SMWAP'
024200000725     O                                           +1 'Entrate'
024300000616     O          E            testdett    3
024400000613     O                                           +2 'N° rec'
024500000613     O                                           +3 'Anteprima contenuto'
024600000616     O          E            rigadett    2
024700000613     O                       rrnum               +2
024800000621     O                       recko               +3
024900000616     O          E            findett     2
025000000613     O                       wrkdata
025100000613     O                       wrkora              +1
025200000613     O                       procname            +1
025300000616     O          E            findett     2
025400000613     O                                              'Cliente..................:'
025500001009     O                       X1DEP               +1
025600000616     O          E            findett     2
025700000613     O                                              'Riferimento Strategi.....:'
025800000613     O                       vlrhdl              +1
025900000616     O          E            findett     2
026000000613     O                                              'Giusti...................:'
026100000613     O                       §CTROK        2   +  1
026200000616     O          E            findett     2
026300000613     O                                              'Sbagliati e scartati.....:'
026400000613     O                       §CTRNO        2   +  1
026500000512** CMD - COMANDI CL
026600001009OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7RXP)  OUTQ(
026700001009OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7RXPS) OUTQ(
026800001009DLTOVR FILE(TIS7RXP TIS7RXPS) LVL(*)
026900050512CPYF FROMFILE(WP3SMWAP) TOFILE(
027000050512/WP3SMWAP) FROMMBR(
027100050512) TOMBR(WP3SMWAP) MBROPT(*ADD)
027200050512DLCOBJ OBJ((WP3SMWAP *FILE *EXCL
027300050512DLTOVR FILE(WP3SMWAP) LVL(*JOB)
