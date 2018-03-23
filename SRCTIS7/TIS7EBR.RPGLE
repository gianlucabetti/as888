000100030120      /TITLE Upload via FTP: traduzione file LED EMMBOLLE
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500030120     FEMMBOLLE  O    E             DISK    usropn
000600030120     Ftis7ebp   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800030120     Ftis7ebps  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400030121     D cmd             S            100    DIM(8) CTDATA PERRCD(1)
001500000512     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900030120     D passaggio     e ds                  extname(emmbolle)
002000990910     D esito           s              1
002100000724     D prmlit          s             10
002200000710     D prmfir          s             10
002300990921     D wrkesito        s                   like(esito)
002400990915     D wrkdata         s               d
002500990915     D wrkora          s               t
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700000621     D recko           s            150    INZ(*blanks)
002800030121     D depcmd          s            150    INZ(*blanks)
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
004300030121     C                   Z-ADD     150           LENGH            15 5
004400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
004500000616     C                   PARM                    depcmd
004600000616     C                   PARM                    LENGH
004700000725     C*
004800000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
004900000725     C                   movel(p)  CMD(2)        depcmd
005000000725     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
005100000725     C*
005200000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
005300030121     C                   Z-ADD     150           LENGH            15 5
005400000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005500000725     C                   PARM                    depcmd
005600000725     C                   PARM                    LENGH
005700000616     C*
005800030120     C                   if        not %open(tis7ebps)
005900030120     C                   open      tis7ebps
006000000616     C                   except    testdett
006100000613     C                   endif
006200000613     C*
006300000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006400000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
006500000613     C*
006600030120     C                   if        %open(tis7ebps)
006700000616     C                   except    findett
006800030120     C                   close     tis7ebps
006900000613     C                   endif
007000000616     C*
007100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
007200000724     C                   movel(p)  CMD(3)        depcmd
007300030121     C                   Z-ADD     150           LENGH            15 5
007400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007500000724     C                   PARM                    depcmd
007600000616     C                   PARM                    LENGH
007700000613     C*
007800000613     C                   return
007900000613
008000000724
008100910830     C*--------------------------------------------------------
008200030120     C* RWFIL   LEGGE tivin00r E SCRIVE FILE LED EMMBOLLE     *
008300910830     C*--------------------------------------------------------
008400000724     C     RWFIL         BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900030120     C                   if        not %open(emmbolle)
009000030120     C                   open      emmbolle
009100990914     C                   endif
009200990910     C*
009300990910     C                   clear                   §CTROK
009400990910     C                   clear                   §CTRNO
009500001009     C                   clear                   recko
009600990910     C*
009700921023     C                   DO        *HIVAL
009800990913     C*
009900990915     C                   READ      tivin00r                               70
010000000905     C                   if        vindta > *blanks
010100000613     C                   add       1             rrnum
010200000724     C                   if        *in70 = *off and
010300990913     C                             (vinflg = *blanks
010400990913     C                              or vinflg = '0'
010500990913     C                              or vinflg = '2')
010600990913     C*
010700990913     C                   clear                   vinmsg
010800000724     C                   if        vindta <> *blanks
010900000724     C                   movel(p)  vindta        passaggio
011000030120     C                   write(e)  bolle
011100990910     C*
011200000724     C                   if        not %error
011300000724     C                   add       1             §ctrok
011400990910     C                   eval      vinflg = '1'
011500990910     C                   else
011600000724     C                   add       1             §ctrno
011700000621     C                   eval      recko = vindta
011800000616     C                   except    rigadett
011900990910     C                   eval      vinflg = '2'
012000990910     C                   endif
012100000724     C                   endif
012200991022     C                   endif
012300000905     C*
012400000905     C                   else
012500000905     C                   eval      vinflg = '1'
012600000905     C                   endif
012700000905     C*
012800000905     C  N70              update    tivin000
012900991022     C*
013000991022     C  N70              ENDdo
013100990910
013200990910     C* Se non ci sono record con errori ...
013300000710     C                   if        §ctrno = 0
013400990910     C* ... restituisco esito OK.
013500990921     C                   eval      wrkesito = '0'
013600990910     C                   else
013700000710     C                   if        §ctrok > 0
013800990921     C                   eval      wrkesito = '1'
013900000710     C                   else
014000000710     C                   eval      wrkesito = '2'
014100990910     C                   endif
014200000710     C                   endif
014300990910     C*
014400990914     C                   if        %open(tivin00r)
014500990908     C                   close     tivin00r
014600990914     C                   endif
014700030120     C                   if        %open(emmbolle)
014800030120     C                   close     emmbolle
014900990914     C                   endif
015000990920     C*
015100000724     C                   if        wrkesito <> '2'
015200000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
015300000724     C                   movel(p)  CMD(7)        depcmd
015400000724     C                   eval      depcmd = %trim(depcmd) + ' '+
015500000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
015600030121     C                   Z-ADD     150           LENGH            15 5
015700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015800000724     C                   PARM                    depcmd
015900000724     C                   PARM                    LENGH
016000000724     C*
016100000724     C                   movel(p)  CMD(8)        depcmd
016200000724     C                   eval      depcmd = %trim(depcmd)
016300030121     C                   Z-ADD     150           LENGH            15 5
016400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016500000724     C                   PARM                    depcmd
016600000724     C                   PARM                    LENGH
016700000724     C*
016800000724     C                   movel(p)  CMD(4)        depcmd
016900000724     C                   eval      depcmd = %trim(depcmd) +
017000000724     C                             %trim(prmlit) + %trim(CMD(5)) +
017100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
017200000724     C                             %trim(CMD(6))
017300030121     C                   Z-ADD     150           LENGH            15 5
017400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017500000724     C                   PARM                    depcmd
017600000724     C                   PARM                    LENGH
017700000724     C*
017800000724     C                   endif
017900000724     C*
018000910830     C                   ENDSR
018100000613     C***
018200000725
018300000725
018400000725     C*--------------------------------------------------------
018500000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018600000725     C*--------------------------------------------------------
018700000725     C     STPR          BEGSR
018800000725     C*
018900030120     C                   if        not %open(tis7ebp)
019000030120     C                   open      tis7ebp
019100000725     C                   endif
019200000725     C*
019300000725     C                   except    riepilogo
019400000725     C*
019500030120     C                   if        %open(tis7ebp)
019600030120     C                   close     tis7ebp
019700000725     C                   endif
019800000725     C*
019900000725     C                   ENDSR
020000000725     C***
020100990920
020200990910
020300000613     C     *inzsr        BEGSR
020400990910     C*
020500990910     C     *entry        plist
020600990920     C                   parm                    tivlrds
020700990921     C                   parm      wrkesito      esito
020800000724     C                   parm                    prmlit
020900000724     C                   parm                    prmfir
021000000613     C*
021100000613     C                   ENDSR
021200000613     C***
021300000725
021400030120     Otis7ebp   E            riepilogo         2
021500000725     O                                              'Upload via FTP'
021600000725     O                                           +1 'Traduzione TIVIN00R -'
021700030120     O                                           +1 'EMMBOLLE'
021800000725     O                                           +1 'Entrate'
021900000725     O          E            riepilogo   2
022000000725     O                       wrkdata
022100000725     O                       wrkora              +1
022200000725     O                       procname            +1
022300000725     O          E            riepilogo   2
022400000725     O                                              'Cliente..................:'
022500030120     O                                           +1 'Emmeci - In Fiore'
022600000725     O          E            riepilogo   2
022700000725     O                                              'Riferimento Strategi.....:'
022800000725     O                       vlrhdl              +1
022900000725     O          E            riepilogo   2
023000000725     O                                              'Giusti...................:'
023100000725     O                       §CTROK        2   +  1
023200000725     O          E            riepilogo   2
023300000725     O                                              'Sbagliati e scartati.....:'
023400000725     O                       §CTRNO        2   +  1
023500990908
023600000724
023700030120     Otis7ebps  E            testdett          2
023800000724     O                                              'Upload via FTP'
023900000613     O                                           +1 'Traduzione TIVIN00R -'
024000030120     O                                           +1 'EMMBOLLE'
024100000725     O                                           +1 'Entrate'
024200000616     O          E            testdett    3
024300000613     O                                           +2 'N° rec'
024400000613     O                                           +3 'Anteprima contenuto'
024500000616     O          E            rigadett    2
024600000613     O                       rrnum               +2
024700000621     O                       recko               +3
024800000616     O          E            findett     2
024900000613     O                       wrkdata
025000000613     O                       wrkora              +1
025100000613     O                       procname            +1
025200000616     O          E            findett     2
025300000613     O                                              'Cliente..................:'
025400030120     O                                           +1 'EmmeCi - In Fiore'
025500000616     O          E            findett     2
025600000613     O                                              'Riferimento Strategi.....:'
025700000613     O                       vlrhdl              +1
025800000616     O          E            findett     2
025900000613     O                                              'Giusti...................:'
026000000613     O                       §CTROK        2   +  1
026100000616     O          E            findett     2
026200000613     O                                              'Sbagliati e scartati.....:'
026300000613     O                       §CTRNO        2   +  1
026400000512** CMD - COMANDI CL
026500030121OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7EBP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
026600030121OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7EBPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
026700030120DLTOVR FILE(TIS7EBP TIS7EBPS) LVL(*)
026800030120CPYF FROMFILE(EMMBOLLE) TOFILE(
026900030120/EMMBOLLE) FROMMBR(
027000030120) TOMBR(EMMBOLLE) MBROPT(*ADD)
027100030120DLCOBJ OBJ((EMMBOLLE *FILE *EXCL
027200030120DLTOVR FILE(EMMBOLLE) LVL(*JOB)
