000100010313      /TITLE Upload via FTP: traduzione file genericxo UPLOAD x INET
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050909     Fwfrcv00w  O    E             DISK    usropn
000600010313     Ftis7edps  O    f  198        PRINTER usropn
000700000621     F                                     oflind(*inob)
000800990908
000900000512     D*------------
001000000512     D* COMANDI
001100000512     D*------------
001200050912     D cmd             S             80    DIM(4) CTDATA PERRCD(1)
001300000512     D*------------
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100990915     D wrkdata         s               d
002200990915     D wrkora          s               t
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400000621     D recko           s            150    INZ(*blanks)
002500010905     D depcmd          s            300    INZ(*blanks)
002600010905     D lengh           s             15  5 INZ(*zeros)
002700000724     D §ctrok          s              5  0 INZ(*zeros)
002800000724     D §ctrno          s              5  0 INZ(*zeros)
002900990908
003000990915     C                   time                    wrkdata
003100990915     C                   time                    wrkora
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010905     C*
003600010905     C* IMPOSTO LA LUNGHEZZA DELLA STRINGA DI COMANDO
003700010905     C                   EVAL      LENGH = %len(depcmd)
003800010905     C*
003900010905     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
004000050912     C                   MOVEL(p)  CMD(3)        depcmd
004100010905     C*
004200050912     C* PROVO A ALLOCARE IL FILE D RICEZIONE DATI EDI IN UPLOAD (WFRCV00W)
004300010905     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
004400010905     C                   PARM                    depcmd
004500010905     C                   PARM                    LENGH
004600010905     C*
004700050912     C* SE NON RIESCE L'ALLOCAZIONE MANDO MESSAGGIO A VIDEO, ALTRIMENTI PROCEDO NORMALMENTE
004800010905     C                   IF        %error
004900050912     C                   MOVEL     '2'           wrkesito
005000040325     C                   ELSE
005100000725     C*
005200000725     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
005300010314     C                   MOVEL(p)  CMD(1)        depcmd
005400000725     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
005500000725     C*
005600000725     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
005700000725     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005800000725     C                   PARM                    depcmd
005900000725     C                   PARM                    LENGH
006000000616     C*
006100010313     C                   if        not %open(tis7edps)
006200010313     C                   open      tis7edps
006300000616     C                   except    testdett
006400000613     C                   endif
006500000613     C*
006600000724     C                   EXSR      RWFIL                                        LETT/SCR. FILE
006700000613     C*
006800010313     C                   if        %open(tis7edps)
006900000616     C                   except    findett
007000010313     C                   close     tis7edps
007100000613     C                   endif
007200000616     C*
007300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
007400010313     C                   movel(p)  CMD(2)        depcmd
007500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007600000724     C                   PARM                    depcmd
007700000616     C                   PARM                    LENGH
007800040322     C*
007900040322     C* COSTRUISCO IL COMANDO PER ISTRUZIONE SEGUENTE
008000050912     C                   MOVEL(p)  CMD(4)        depcmd
008100040322     C*
008200050912     C* PROVO A DISALLOCARE IL FILE D RICEZIONE DATI EDI IN UPLOAD (WFRCV00W)
008300040322     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400040322     C                   PARM                    depcmd
008500040322     C                   PARM                    LENGH
008600040322     C                   ENDIF
008700000613     C*
008800010313     C                   seton                                        LR
008900000613
009000000724
009100910830     C*--------------------------------------------------------
009200050909     C* RWFIL   LEGGE tivin00r E SCRIVE FILE EDI WFRCV00W     *
009300910830     C*--------------------------------------------------------
009400000724     C     RWFIL         BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900050909     C                   if        not %open(wfrcv00w)
010000050909     C                   open      wfrcv00w
010100990914     C                   endif
010200990910     C*
010300990910     C                   clear                   §CTROK
010400990910     C                   clear                   §CTRNO
010500001009     C                   clear                   recko
010600990910     C*
010700921023     C                   DO        *HIVAL
010800990913     C*
010900990915     C                   READ      tivin00r                               70
011000000905     C                   if        vindta > *blanks
011100000613     C                   add       1             rrnum
011200000724     C                   if        *in70 = *off and
011300990913     C                             (vinflg = *blanks
011400990913     C                              or vinflg = '0'
011500990913     C                              or vinflg = '2')
011600990913     C*
011700990913     C                   clear                   vinmsg
011800000724     C                   if        vindta <> *blanks
011900050909     C                   movel(p)  vindta        flatrec
012000050909     C                   write(e)  wfrcv000
012100990910     C*
012200000724     C                   if        not %error
012300000724     C                   add       1             §ctrok
012400990910     C                   eval      vinflg = '1'
012500990910     C                   else
012600000724     C                   add       1             §ctrno
012700000621     C                   eval      recko = vindta
012800000616     C                   except    rigadett
012900990910     C                   eval      vinflg = '2'
013000990910     C                   endif
013100000724     C                   endif
013200991022     C                   endif
013300000905     C*
013400000905     C                   else
013500000905     C                   eval      vinflg = '1'
013600000905     C                   endif
013700000905     C*
013800000905     C  N70              update    tivin000
013900991022     C*
014000991022     C  N70              ENDdo
014100990910
014200990910     C* Se non ci sono record con errori ...
014300000710     C                   if        §ctrno = 0
014400990910     C* ... restituisco esito OK.
014500990921     C                   eval      wrkesito = '0'
014600990910     C                   else
014700000710     C                   if        §ctrok > 0
014800990921     C                   eval      wrkesito = '1'
014900000710     C                   else
015000000710     C                   eval      wrkesito = '2'
015100990910     C                   endif
015200000710     C                   endif
015300990910     C*
015400990914     C                   if        %open(tivin00r)
015500990908     C                   close     tivin00r
015600990914     C                   endif
015700050909     C                   if        %open(wfrcv00w)
015800050909     C                   close     wfrcv00w
015900990914     C                   endif
016000000724     C*
016100910830     C                   ENDSR
016200000613     C***
016300000725
016400000725
016500990910
016600000613     C     *inzsr        BEGSR
016700990910     C*
016800990910     C     *entry        plist
016900990920     C                   parm                    tivlrds
017000990921     C                   parm      wrkesito      esito
017100000724     C                   parm                    prmlit
017200000724     C                   parm                    prmfir
017300000613     C*
017400000613     C                   ENDSR
017500000613     C***
017600000725
017700990908
017800000724
017900010313     Otis7edps  E            testdett          2
018000000724     O                                              'Upload via FTP'
018100000613     O                                           +1 'Traduzione TIVIN00R -'
018200050909     O                                           +1 'WFRCV00W'
018300000725     O                                           +1 'Entrate'
018400000616     O          E            testdett    3
018500000613     O                                           +2 'N° rec'
018600000613     O                                           +3 'Anteprima contenuto'
018700000616     O          E            rigadett    2
018800000613     O                       rrnum               +2
018900000621     O                       recko               +3
019000000616     O          E            findett     2
019100000613     O                       wrkdata
019200000613     O                       wrkora              +1
019300000613     O                       procname            +1
019400000616     O          E            findett     2
019500000613     O                                              'Cliente..................:'
019600010313     O                                           +1 'INET'
019700000616     O          E            findett     2
019800000613     O                                              'Riferimento Strategi.....:'
019900000613     O                       vlrhdl              +1
020000000616     O          E            findett     2
020100000613     O                                              'Giusti...................:'
020200000613     O                       §CTROK        2   +  1
020300000616     O          E            findett     2
020400000613     O                                              'Sbagliati e scartati.....:'
020500000613     O                       §CTRNO        2   +  1
020600000512** CMD - COMANDI CL
020700010313OVRPRTF FILE(TIS7EDPS) TOFILE(TIS7RXPS) OVRSCOPE(*CALLLVL) OUTQ(
020800010313DLTOVR FILE(TIS7EDPS) LVL(*)
020900050912ALCOBJ OBJ((WFRCV00W *FILE *EXCL))
021000050912DLCOBJ OBJ((WFRSV00W *FILE *EXCL))
