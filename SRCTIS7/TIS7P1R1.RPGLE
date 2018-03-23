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
005800030613     C                   if        not %open(tis7pxps)
005900030613     C                   open      tis7pxps
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600030613     C                   if        %open(tis7pxps)
006700000616     C                   except    findett
006800030613     C                   close     tis7pxps
006900000613     C                   endif
007000000616     C*
007100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
007200000724     C                   movel(p)  CMD(3)        depcmd
007300000724     C                   Z-ADD     100           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000724     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000616     C*
007800000724     C
007900000613     C                   return
008000000613
008100000724
008200910830     C*--------------------------------------------------------
008300040203     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WWRCE00F     *
008400910830     C*--------------------------------------------------------
008500000724     C     RWFIL         BEGSR
008600990910     C*
008700990914     C                   if        not %open(tivin00r)
008800990908     C                   open      tivin00r
008900990914     C                   endif
009000040203     C                   if        not %open(wwrce00f)
009100040203     C                   open      wwrce00f
009200990914     C                   endif
009300990910     C*
009400990910     C                   clear                   §CTROK
009500990910     C                   clear                   §CTRNO
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
011100040203     C                   write(e)  WWRCE
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
014800040203     C                   if        %open(wwrce00f)
014900040203     C                   close     wwrce00f
015000990914     C                   endif
015100990920     C*
015200000724     C                   if        wrkesito <> '2'
015300000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
015400070913     C                   movel(p)  CMD(8)        depcmd
015500000724     C                   eval      depcmd = %trim(depcmd) + ' '+
015600000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
015700000724     C                   Z-ADD     100           LENGH            15 5
015800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015900000724     C                   PARM                    depcmd
016000000724     C                   PARM                    LENGH
016100000724     C*
016200070913     C                   movel(p)  CMD(9)        depcmd
016300000724     C                   eval      depcmd = %trim(depcmd)
016400000724     C                   Z-ADD     100           LENGH            15 5
016500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016600000724     C                   PARM                    depcmd
016700000724     C                   PARM                    LENGH
016800000724     C*
016900000724     C                   movel(p)  CMD(4)        depcmd
017000000724     C                   eval      depcmd = %trim(depcmd) +
017100000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017200070913     C                             %trim(%subst(vlrppt:1:10)) +
017300070913     C                             %trim(CMD(6)) +
017400000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017500070913     C                             %trim(CMD(7))
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
019200030613     C                   if        not %open(tis7pxp)
019300030613     C                   open      tis7pxp
019400000725     C                   endif
019500000725     C*
019600000725     C                   except    riepilogo
019700000725     C*
019800030613     C                   if        %open(tis7pxp)
019900030613     C                   close     tis7pxp
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
021700030613     Otis7pxp   E            riepilogo         2
021800000725     O                                              'Upload via FTP'
021900000725     O                                           +1 'Traduzione TIVIN00R -'
022000040203     O                                           +1 'WWRCE00F'
022100000725     O                                           +1 'Entrate'
022200000725     O          E            riepilogo   2
022300000725     O                       wrkdata
022400000725     O                       wrkora              +1
022500000725     O                       procname            +1
022600000725     O          E            riepilogo   2
022700000725     O                                              'Cliente..................:'
022800030613     O                       VLRKSC              +1
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
024000030613     Otis7pxps  E            testdett          2
024100000724     O                                              'Upload via FTP'
024200000613     O                                           +1 'Traduzione TIVIN00R -'
024300040203     O                                           +1 'WWRCE00F'
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
025700030613     O                       VLRKSC              +1
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
026800030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
026900030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
027000030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
027100040203CPYF FROMFILE(WWRCE00F) TOFILE(
027200070913/
027300070913) FROMMBR(
027400070913) MBROPT(*ADD) FMTOPT(*NOCHK)
027500040203DLCOBJ OBJ((WWRCE00F *FILE *EXCL
027600040203DLTOVR FILE(WWRCE00F) LVL(*JOB)
