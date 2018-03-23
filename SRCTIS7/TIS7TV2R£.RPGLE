000100110808      /TITLE Upload via FTP: traduzione file LED WP2LIV2P
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110808     FWP2LIV2P  O    E             DISK    usropn
000600030613     Ftis7pxp   O    f  132        PRINTER usropn
000700000725     F                                     oflind(*inoa)
000800030613     Ftis7pxps  O    f  198        PRINTER usropn
000900000621     F                                     oflind(*inob)
001000990908
001100000512     D*------------
001200000512     D* COMANDI
001300000512     D*------------
001400090121     D cmd             S             80    DIM(9) CTDATA PERRCD(1)
001500000512     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900110808     D passaggio     e ds                  extname(wp2liv2p)
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
003100090109     D*-------------------
003200090109     D* COSTANTI
003300090109     D*-------------------
003400090109     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003500090109     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003600990908
003700990915     C                   time                    wrkdata
003800990915     C                   time                    wrkora
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200090109     C*
004300090109     C* OPERAZIONI INIZIALI
004400090109     C                   exsr      PREELA
004500000613     C*
004600090109     C* ELABORAZIONE
004700000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
004800000725     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
004900100623     C*
005000100623     C                   if        wrkesito = *blanks
005100100623     C                   eval      wrkesito = '0'
005200100623     C                   endif
005300090121     C*
005400090121     C* OPERAZIONI FINALI
005500090121     C                   exsr      ENDELA
005600000616     C*
005700090109     C                   seton                                        lr
005800090109
005900090109
006000090109     C*--------------------------------------------------------
006100090109     C* PREELA - ROUTINE OPERAZIONI INIZIALI                  *
006200090109     C*--------------------------------------------------------
006300090109     C     PREELA        BEGSR
006400090109     C*
006500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006600090109     C                   movel(p)  CMD(1)        depcmd
006700090109     C                   EVAL      depcmd=%trim(depcmd)+'GEODETR'+')'
006800090109     C*
006900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA X LED
007000090109     C                   Z-ADD     100           LENGH            15 5
007100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200090109     C                   PARM                    depcmd
007300090109     C                   PARM                    LENGH
007400090109     C*
007500090109     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600090109     C                   movel(p)  CMD(2)        depcmd
007700090109     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007800090109     C*
007900090109     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008000090109     C                   Z-ADD     100           LENGH            15 5
008100090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200090109     C                   PARM                    depcmd
008300090109     C                   PARM                    LENGH
008400090109     C*
008500090109     C                   if        not %open(tis7pxps)
008600090109     C                   open      tis7pxps
008700090109     C                   except    testdett
008800090109     C                   endif
008900090109     C*
009000090109     C                   ENDSR
009100090109     C***
009200090109
009300090109
009400090109     C*--------------------------------------------------------
009500090109     C* ENDELA - ROUTINE OPERAZIONI FINALI                    *
009600090109     C*--------------------------------------------------------
009700090109     C     ENDELA        BEGSR
009800090109     C*
009900090109     C                   if        %open(tis7pxps)
010000090109     C                   except    findett
010100090109     C                   close     tis7pxps
010200090109     C                   endif
010300090109     C*
010400090109     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
010500090109     C                   movel(p)  CMD(3)        depcmd
010600090109     C                   Z-ADD     100           LENGH            15 5
010700090109     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800090109     C                   PARM                    depcmd
010900090109     C                   PARM                    LENGH
011000090109     C*
011100090109     C                   ENDSR
011200090109     C***
011300090109
011400090109
011500000613
011600000724
011700910830     C*--------------------------------------------------------
011800110808     C* RWFIL   LEGGE TIVIN00R E SCRIVE FILE LED WP2LIV2P     *
011900910830     C*--------------------------------------------------------
012000000724     C     RWFIL         BEGSR
012100990910     C*
012200990914     C                   if        not %open(tivin00r)
012300990908     C                   open      tivin00r
012400990914     C                   endif
012500110808     C                   if        not %open(wp2liv2p)
012600110808     C                   open      wp2liv2p
012700990914     C                   endif
012800990910     C*
012900990910     C                   clear                   §CTROK
013000990910     C                   clear                   §CTRNO
013100001009     C                   clear                   recko
013200990910     C*
013300921023     C                   DO        *HIVAL
013400990913     C*
013500990915     C                   READ      tivin00r                               70
013600000905     C                   if        vindta > *blanks
013700000613     C                   add       1             rrnum
013800000724     C                   if        *in70 = *off and
013900990913     C                             (vinflg = *blanks
014000990913     C                              or vinflg = '0'
014100990913     C                              or vinflg = '2')
014200990913     C*
014300990913     C                   clear                   vinmsg
014400000724     C                   if        vindta <> *blanks
014500000724     C                   movel(p)  vindta        passaggio
014600110808     C                   write(e)  XX2LIV2
014700990910     C*
014800000724     C                   if        not %error
014900000724     C                   add       1             §ctrok
015000990910     C                   eval      vinflg = '1'
015100990910     C                   else
015200000724     C                   add       1             §ctrno
015300000621     C                   eval      recko = vindta
015400000616     C                   except    rigadett
015500990910     C                   eval      vinflg = '2'
015600990910     C                   endif
015700000724     C                   endif
015800991022     C                   endif
015900000905     C*
016000000905     C                   else
016100000905     C                   eval      vinflg = '1'
016200000905     C                   endif
016300000905     C*
016400000905     C  N70              update    tivin000
016500991022     C*
016600991022     C  N70              ENDdo
016700990910
016800990910     C* Se non ci sono record con errori ...
016900000710     C                   if        §ctrno = 0
017000990910     C* ... restituisco esito OK.
017100990921     C                   eval      wrkesito = '0'
017200990910     C                   else
017300000710     C                   if        §ctrok > 0
017400990921     C                   eval      wrkesito = '1'
017500000710     C                   else
017600000710     C                   eval      wrkesito = '2'
017700990910     C                   endif
017800000710     C                   endif
017900990910     C*
018000990914     C                   if        %open(tivin00r)
018100990908     C                   close     tivin00r
018200990914     C                   endif
018300110808     C                   if        %open(wp2liv2p)
018400110808     C                   close     wp2liv2p
018500990914     C                   endif
018600990920     C*
018700000724     C                   if        wrkesito <> '2'
018800000724     C* DISALLOCO E AGGIUNGO QUANTO APPENA SCRITTO NELLA LIBRERIA/FILE SPECIFICA DEL CLIENTE
018900070913     C                   movel(p)  CMD(8)        depcmd
019000000724     C                   eval      depcmd = %trim(depcmd) + ' '+
019100000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) + '))'
019200000724     C                   Z-ADD     100           LENGH            15 5
019300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019400000724     C                   PARM                    depcmd
019500000724     C                   PARM                    LENGH
019600000724     C*
019700070913     C                   movel(p)  CMD(9)        depcmd
019800000724     C                   eval      depcmd = %trim(depcmd)
019900000724     C                   Z-ADD     100           LENGH            15 5
020000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020100000724     C                   PARM                    depcmd
020200000724     C                   PARM                    LENGH
020300000724     C*
020400000724     C                   movel(p)  CMD(4)        depcmd
020500000724     C                   eval      depcmd = %trim(depcmd) +
020600090109     C                             %trim(prmlit) + %trim(CMD(5)) +
020700110808     C                             %trim('XX2LIV2P') +
020800070913     C                             %trim(CMD(6)) +
020900000724     C                             'M'+ %trim(%subst(vlrhdl:2:9)) +
021000070913     C                             %trim(CMD(7))
021100000724     C                   Z-ADD     100           LENGH            15 5
021200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
021300000724     C                   PARM                    depcmd
021400000724     C                   PARM                    LENGH
021500000724     C*
021600000724     C                   endif
021700000724     C*
021800910830     C                   ENDSR
021900000613     C***
022000000725
022100000725
022200000725     C*--------------------------------------------------------
022300000725     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
022400000725     C*--------------------------------------------------------
022500000725     C     STPR          BEGSR
022600000725     C*
022700030613     C                   if        not %open(tis7pxp)
022800030613     C                   open      tis7pxp
022900000725     C                   endif
023000000725     C*
023100000725     C                   except    riepilogo
023200000725     C*
023300030613     C                   if        %open(tis7pxp)
023400030613     C                   close     tis7pxp
023500000725     C                   endif
023600000725     C*
023700000725     C                   ENDSR
023800000725     C***
023900990920
024000990910
024100000613     C     *inzsr        BEGSR
024200990910     C*
024300990910     C     *entry        plist
024400990920     C                   parm                    tivlrds
024500990921     C                   parm      wrkesito      esito
024600000724     C                   parm                    prmlit
024700000724     C                   parm                    prmfir
024800000613     C*
024900000613     C                   ENDSR
025000000613     C***
025100000725
025200030613     Otis7pxp   E            riepilogo         2
025300000725     O                                              'Upload via FTP'
025400000725     O                                           +1 'Traduzione TIVIN00R -'
025500110808     O                                           +1 'WP2LIV2P'
025600000725     O                                           +1 'Entrate'
025700000725     O          E            riepilogo   2
025800000725     O                       wrkdata
025900000725     O                       wrkora              +1
026000000725     O                       procname            +1
026100000725     O          E            riepilogo   2
026200000725     O                                              'Cliente..................:'
026300030613     O                       VLRKSC              +1
026400000725     O          E            riepilogo   2
026500000725     O                                              'Riferimento Strategi.....:'
026600000725     O                       vlrhdl              +1
026700000725     O          E            riepilogo   2
026800000725     O                                              'Giusti...................:'
026900000725     O                       §CTROK        2   +  1
027000000725     O          E            riepilogo   2
027100000725     O                                              'Sbagliati e scartati.....:'
027200000725     O                       §CTRNO        2   +  1
027300990908
027400000724
027500030613     Otis7pxps  E            testdett          2
027600000724     O                                              'Upload via FTP'
027700000613     O                                           +1 'Traduzione TIVIN00R -'
027800110808     O                                           +1 'WP2LIV2P'
027900000725     O                                           +1 'Entrate'
028000000616     O          E            testdett    3
028100000613     O                                           +2 'N° rec'
028200000613     O                                           +3 'Anteprima contenuto'
028300000616     O          E            rigadett    2
028400000613     O                       rrnum               +2
028500000621     O                       recko               +3
028600000616     O          E            findett     2
028700000613     O                       wrkdata
028800000613     O                       wrkora              +1
028900000613     O                       procname            +1
029000000616     O          E            findett     2
029100000613     O                                              'Cliente..................:'
029200030613     O                       VLRKSC              +1
029300000616     O          E            findett     2
029400000613     O                                              'Riferimento Strategi.....:'
029500000613     O                       vlrhdl              +1
029600000616     O          E            findett     2
029700000613     O                                              'Giusti...................:'
029800000613     O                       §CTROK        2   +  1
029900000616     O          E            findett     2
030000000613     O                                              'Sbagliati e scartati.....:'
030100000613     O                       §CTRNO        2   +  1
030200000512** CMD - COMANDI CL
030300030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXP)  OUTQ(
030400030613OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7PXPS) OUTQ(
030500030613DLTOVR FILE(TIS7PXP TIS7PXPS) LVL(*)
030600110808CPYF FROMFILE(WP2LIV2P) TOFILE(
030700070913/
030800070913) FROMMBR(
030900070913) MBROPT(*ADD) FMTOPT(*NOCHK)
031000110808DLCOBJ OBJ((WP2LIV2P *FILE *EXCL
031100110808DLTOVR FILE(WP2LIV2P) LVL(*JOB)
