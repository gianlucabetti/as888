000100020710      /TITLE Upload via FTP: traduzione file LED LDTUT00T
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020710     FLDTUT00T  O    E             DISK    usropn
000600020710     Ftis7lu1p  O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800020710     Ftis7lu1ps O    f  198        PRINTER usropn
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
001900020710     D passaggio     e ds                  extname(LDTUT00T)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400990915     D wrkdata         s               d
002500990915     D wrkora          s               t
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700000621     D recko           s            150    INZ(*blanks)
002800000724     D depcmd          s            100    INZ(*blanks)
002900000724     D �ctrok          s              5  0 INZ(*zeros)
003000000724     D �ctrno          s              5  0 INZ(*zeros)
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
005800020710     C                   if        not %open(tis7lu1ps)
005900020710     C                   open      tis7lu1ps
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600020710     C                   if        %open(tis7lu1ps)
006700000616     C                   except    findett
006800020710     C                   close     tis7lu1ps
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
008400020710     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED LDTUT00T     *
008500910830     C*--------------------------------------------------------
008600000724     C     RWFIL         BEGSR
008700990910     C*
008800990914     C                   if        not %open(tivin00r)
008900990908     C                   open      tivin00r
009000990914     C                   endif
009100020710     C                   if        not %open(LDTUT00T)
009200020710     C                   open      LDTUT00T
009300990914     C                   endif
009400990910     C*
009500990910     C                   clear                   �CTROK
009600990910     C                   clear                   �CTRNO
009700001009     C                   clear                   recko
009800990910     C*
009900921023     C                   DO        *HIVAL
010000990913     C*
010100990915     C                   READ      tivin00r                               70
010200010209     C*                  if        vindta > *blanks
010300000613     C                   add       1             rrnum
010400000724     C                   if        *in70 = *off and
010500990913     C                             (vinflg = *blanks
010600990913     C                              or vinflg = '0'
010700990913     C                              or vinflg = '2')
010800990913     C*
010900990913     C                   clear                   vinmsg
011000010209     C*                  if        vindta <> *blanks
011100020710     C                   movel(p)  vindta        passaggio
011200010209     C*
011300010209     C* Chiamo il PGM "normalizzatore" dei tabulatori
011400010209     C                   call      'TIS7MBR1'
011500010209     C                   parm                    passaggio
011600010209     C*
011700020710     C                   write(e)  LDTUT
011800990910     C*
011900000724     C                   if        not %error
012000000724     C                   add       1             �ctrok
012100990910     C                   eval      vinflg = '1'
012200990910     C                   else
012300000724     C                   add       1             �ctrno
012400000621     C                   eval      recko = vindta
012500000616     C                   except    rigadett
012600990910     C                   eval      vinflg = '2'
012700990910     C                   endif
012800010209     C*                  endif
012900000905     C                   endif
013000000905     C*
013100010209     C*                  else
013200010209     C*                  eval      vinflg = '1'
013300010209     C*                  endif
013400000905     C*
013500000905     C  N70              update    tivin000
013600000905     C*
013700991022     C  N70              ENDdo
013800990910
013900990910     C* Se non ci sono record con errori ...
014000000710     C                   if        �ctrno = 0
014100990910     C* ... restituisco esito OK.
014200990921     C                   eval      wrkesito = '0'
014300990910     C                   else
014400000710     C                   if        �ctrok > 0
014500990921     C                   eval      wrkesito = '1'
014600000710     C                   else
014700000710     C                   eval      wrkesito = '2'
014800990910     C                   endif
014900000710     C                   endif
015000990910     C*
015100990914     C                   if        %open(tivin00r)
015200990908     C                   close     tivin00r
015300990914     C                   endif
015400020710     C                   if        %open(LDTUT00T)
015500020710     C                   close     LDTUT00T
015600990914     C                   endif
015700990920     C*
015800000724     C                   if        wrkesito <> '2'
015900000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
016000000724     C                   movel(p)  CMD(7)        depcmd
016100000724     C                   eval      depcmd = %trim(depcmd) + ' '+
016200000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
016300000724     C                   Z-ADD     100           LENGH            15 5
016400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016500000724     C                   PARM                    depcmd
016600000724     C                   PARM                    LENGH
016700000724     C*
016800000724     C                   movel(p)  CMD(8)        depcmd
016900000724     C                   eval      depcmd = %trim(depcmd)
017000000724     C                   Z-ADD     100           LENGH            15 5
017100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017200000724     C                   PARM                    depcmd
017300000724     C                   PARM                    LENGH
017400000724     C*
017500000724     C                   movel(p)  CMD(4)        depcmd
017600000724     C                   eval      depcmd = %trim(depcmd) +
017700000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017800000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017900000724     C                             %trim(CMD(6))
018000000724     C                   Z-ADD     100           LENGH            15 5
018100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018200000724     C                   PARM                    depcmd
018300000724     C                   PARM                    LENGH
018400000724     C*
018500000724     C                   endif
018600000724     C*
018700910830     C                   ENDSR
018800000613     C***
018900990920
019000000725
019100000725     C*--------------------------------------------------------
019200000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019300000725     C*--------------------------------------------------------
019400000725     C     STPR          BEGSR
019500000725     C*
019600020710     C                   if        not %open(tis7lu1p)
019700020710     C                   open      tis7lu1p
019800000725     C                   endif
019900000725     C*
020000000725     C                   except    riepilogo
020100000725     C*
020200020710     C                   if        %open(tis7lu1p)
020300020710     C                   close     tis7lu1p
020400000725     C                   endif
020500000725     C*
020600000725     C                   ENDSR
020700000725     C***
020800000725
020900990910
021000000613     C     *inzsr        BEGSR
021100990910     C*
021200990910     C     *entry        plist
021300990920     C                   parm                    tivlrds
021400990921     C                   parm      wrkesito      esito
021500000724     C                   parm                    prmlit
021600000724     C                   parm                    prmfir
021700000613     C*
021800000613     C                   ENDSR
021900000613     C***
022000990908
022100000725
022200020710     Otis7lu1ps E            riepilogo         2
022300000725     O                                              'Upload via FTP'
022400000725     O                                           +1 'Traduzione TIVIN00R -'
022500020710     O                                           +1 'LDTUT00T'
022600000725     O          E            riepilogo   2
022700000725     O                       wrkdata
022800000725     O                       wrkora              +1
022900000725     O                       procname            +1
023000000725     O          E            riepilogo   2
023100000725     O                                              'Cliente..................:'
023200010209     O                                           +1 'MOTOROLA'
023300000725     O          E            riepilogo   2
023400000725     O                                              'Riferimento Strategi.....:'
023500000725     O                       vlrhdl              +1
023600000725     O          E            riepilogo   2
023700000725     O                                              'Giusti...................:'
023800000725     O                       �CTROK        2   +  1
023900000725     O          E            riepilogo   2
024000000725     O                                              'Sbagliati e scartati.....:'
024100000725     O                       �CTRNO        2   +  1
024200000725
024300000724
024400020710     Otis7lu1ps E            testdett          2
024500000724     O                                              'Upload via FTP'
024600000613     O                                           +1 'Traduzione TIVIN00R -'
024700020710     O                                           +1 'LDTUT00T'
024800000616     O          E            testdett    3
024900000613     O                                           +2 'N� rec'
025000000613     O                                           +3 'Anteprima contenuto'
025100000616     O          E            rigadett    2
025200000613     O                       rrnum               +2
025300000621     O                       recko               +3
025400000616     O          E            findett     2
025500000613     O                       wrkdata
025600000613     O                       wrkora              +1
025700000613     O                       procname            +1
025800000616     O          E            findett     2
025900000613     O                                              'Cliente..................:'
026000010209     O                                           +1 'MOTOROLA'
026100000616     O          E            findett     2
026200000613     O                                              'Riferimento Strategi.....:'
026300000613     O                       vlrhdl              +1
026400000616     O          E            findett     2
026500000613     O                                              'Giusti...................:'
026600000613     O                       �CTROK        2   +  1
026700000616     O          E            findett     2
026800000613     O                                              'Sbagliati e scartati.....:'
026900000613     O                       �CTRNO        2   +  1
027000000512** CMD - COMANDI CL
027100020710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7LU1P)  TOFILE(TIS7LXP)  OUTQ(
027200020710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7LU1PS) TOFILE(TIS7LXPS) OUTQ(
027300020710DLTOVR FILE(TIS7LU1P TIS7LU1PS) LVL(*)
027400020710CPYF FROMFILE(LDTUT00T) TOFILE(
027500020710/LDTUT00F) FROMMBR(
027600020710) TOMBR(LDTUT00F) MBROPT(*ADD)
027700020710DLCOBJ OBJ((LDTUT00T *FILE *EXCL
027800020710DLTOVR FILE(LDTUT00T) LVL(*JOB)
