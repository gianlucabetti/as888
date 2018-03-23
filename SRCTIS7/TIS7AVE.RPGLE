000100010116      /TITLE Upload: traduzione file AV00BOLL cliente AVERNA
000200010116     H dftactgrp(*yes)
000300010116
000400010116     Fazorg01l  if   e           k disk
000500010116     Ftivin00r  uF   E             DISK    usropn
000600010116     FFIAVE00T  O    E             DISK    usropn
000700010116     F                                     rename(AV00BOLL:FIAVE000)
000800010116     Ftis7t1p   O    f  132        PRINTER usropn
000900010116     F                                     oflind(*inoa)
001000010116     Ftis7t1ps  O    f  198        PRINTER usropn
001100010116     F                                     oflind(*inob)
001200010116
001300010116     D*------------
001400010116     D* COMANDI
001500010116     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700010116     D*------------
001800010116     D dscmz         e ds                  inz
001900010116     D psds           sds
002000010116     D  procname         *PROC
002100010116     D tivlrds       e ds                  extname(tivlr00f)
002200010116     D fnvabaar      e ds                  inz
002300010116     D esito           s              1
002400010116     D prmlit          s             10
002500010116     D prmfir          s             10
002600010116     D wrkesito        s                   like(esito)
002700010116     D wrkdata         s               d
002800010116     D wrkora          s               t
002900010116     D rrnum           s              6  0 INZ(*zeros)
003000010116     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200010116
003300010116     C                   time                    wrkdata
003400010116     C                   time                    wrkora
003500010116     C                   reset                   rrnum
003600010116     C                   reset                   esito
003700010116     C                   reset                   wrkesito
003800010116     C*
003900010116     C* SE OCCORRE SPEDIRE IN FILIALE
004000010116     C                   if        vlrpoi <> *zeros
004100010116     C*
004200010116     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
004300010116     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
004400010116     C     vlrpoi        chain     azorg01l
004500010116     C                   if        %found
004600010116     C                   movel(p)  CMD(1)        depcmd
004700020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
004800010116     C*
004900010116     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
005000011113     C                   Z-ADD     150           LENGH            15 5
005100010116     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
005200010116     C                   PARM                    depcmd
005300010116     C                   PARM                    LENGH
005400010116     C*
005500010116     C                   endif
005600010116     C                   endif
005700010116     C*
005800010116     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
005900010116     C                   movel(p)  CMD(2)        depcmd
006000010116     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
006100010116     C*
006200010116     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
006300011113     C                   Z-ADD     150           LENGH            15 5
006400010116     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
006500010116     C                   PARM                    depcmd
006600010116     C                   PARM                    LENGH
006700010116     C*
006800010116     C                   if        not %open(tis7t1ps)
006900010116     C                   open      tis7t1ps
007000010116     C                   except    testdett
007100010116     C                   endif
007200010116     C*
007300010116     C                   EXSR      RWVAB                                        LETT/SCR. VAB
007400010116     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
007500010116     C*
007600010116     C                   if        %open(tis7t1ps)
007700010116     C                   except    findett
007800010116     C                   close     tis7t1ps
007900010116     C                   endif
008000010116     C*
008100010116     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300010116     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400010116     C                   PARM                    CMD(3)
008500010116     C                   PARM                    LENGH
008600010116     C*
008700010116     C*
008800010116
008900010116     C                   return
009000010116
009100010116
009200010116     C*--------------------------------------------------------
009300010116     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
009400010116     C*--------------------------------------------------------
009500010116     C     STPR          BEGSR
009600010116     C*
009700010116     C                   if        not %open(tis7t1p)
009800010116     C                   open      tis7t1p
009900010116     C                   endif
010000010116     C*
010100010116     C                   except    riepilogo
010200010116     C*
010300010116     C                   if        %open(tis7t1p)
010400010116     C                   close     tis7t1p
010500010116     C                   endif
010600010116     C*
010700010116     C                   ENDSR
010800010116     C***
010900010116
011000010116     C*--------------------------------------------------------
011100010116     C* RWVAB   LEGGE tivin00r E SCRIVE FIAVE00F              *
011200010116     C*--------------------------------------------------------
011300010116     C     RWVAB         BEGSR
011400010116     C*
011500010116     C                   if        not %open(tivin00r)
011600010116     C                   open      tivin00r
011700010116     C                   endif
011800010116     C                   if        not %open(fiave00t)
011900010116     C                   open      fiave00t
012000010116     C                   endif
012100010116      *
012200010116     C                   clear                   §CTROK
012300010116      *
012400010116     C                   DO        *HIVAL
012500010116      *
012600010116     C                   READ      tivin00r                               70
012700010116     C                   if        vindta > *blanks
012800010116     C                   add       1             rrnum
012900010116      *
013000010116     C                   if        *in70 = *off
013100010116     C                             and
013200010116     C                             (vinflg = *blanks
013300010116     C                              or vinflg = '0'
013400010116     C                              or vinflg = '2')
013500010116      *
013600010116     C                   clear                   vinmsg
013700010116      *
013800010116     c                   movel     vindta        Av00boll
013900010116     C                   ADD       1             §CTROK            5 0
014000010116     C                   WRITE     fiave000
014100010116      *
014200010116     C                   eval      vinflg = '1'
014300010116     C                   endif
014400010116      *
014500010116     C                   else
014600010116     C                   eval      vinflg = '1'
014700010116     C                   endif
014800010116      *
014900010116     C  N70              update    tivin000
015000010116      *
015100010116     C  N70              ENDdo
015200010116
015300010116     C                   if        §ctrok > 0
015400010119     C                   eval      wrkesito = '0'
015500010116     C                   else
015600010116     C                   eval      wrkesito = '2'
015700010116     C                   endif
015800010116      *
015900010116     C                   if        %open(tivin00r)
016000010116     C                   close     tivin00r
016100010116     C                   endif
016200010116     C                   if        %open(fiave00t)
016300010116     C                   close     fiave00t
016400010116     C                   endif
016500010116      *
016600010116     C                   if        §ctrok > 0
016700010116     C                             and vlrpoi <> *zeros
016800010116     C                   exsr      invio
016900010116     C                   endif
017000010116      *
017100010116     C                   ENDSR
017200010116     C***
017300010116
017400010116      /TITLE Invio dei dati al punto operativo.
017500010116     C     invio         BEGSR
017600010116     C*
017700010116     C                   reset                   dscmz
017800010116     C                   move      vlrpoi        cmzdst
017900010116     C                   eval      cmzfld = vlrfou
018000010116     C                   eval      cmzmbd = vlrhdl
018100010116     C                   eval      %subst(cmzmbd:1:1) = 'M'
018200021009     C***                if        prmfir = *blanks
018300010116     C                   eval      cmzfla = 'FIAVE00R'
018400010116     C                   eval      cmzmba = 'FIAVE00R'
018500021009     C***                else
018600021009     C***                eval      cmzfla = prmfir
018700021009     C***                eval      cmzmba = prmfir
018800021009     C***                endif
018900010116     C                   eval      cmznrr = *zeros
019000010116     C                   move      §ctrok        cmznrr
019100021025     C                   eval      cmzlba = vlrfl1
019200010116     C                   call(e)   'TIS711C'
019300010116     C                   parm                    dscmz
019400010116     C                   parm      *blanks       esito
019500010116     C                   if        %error
019600010116     C                             or cmzerr = '1'
019700010116     C                             or esito  = '1'
019800010116     C                   eval      wrkesito = '3'
019900010116     C                   endif
020000010116     C*
020100010116     C                   ENDSR
020200010116     C***
020300010116
020400010116     C     *inzsr        BEGSR
020500010116     C*
020600010116     C     *entry        plist
020700010116     C                   parm                    tivlrds
020800010116     C                   parm      wrkesito      esito
020900010116     C                   parm                    prmlit
021000010116     C                   parm                    prmfir
021100010116     C*
021200010116     C                   ENDSR
021300010116     C***
021400010116
021500010116     Otis7t1p   E            riepilogo         2
021600010116     O                                              'Upload via Internet'
021700010116     O                                           +1 'Traduzione TIVIN00R -'
021800010116     O                                           +1 'FIAVE00T'
021900010116     O                                           +1 'Testate bolle'
022000010116     O          E            riepilogo   2
022100010116     O                       wrkdata
022200010116     O                       wrkora              +1
022300010116     O                       procname            +1
022400010116     O          E            riepilogo   2
022500010116     O                                              'Cliente..................:'
022600010116     O                                           +1 'Averna '
022700010116     O          E            riepilogo   2
022800010116     O                                              'Riferimento Strategi.....:'
022900010116     O                       vlrhdl              +1
023000010116     O          E            riepilogo   2
023100010116     O                                              'Giusti...................:'
023200010116     O                       §CTROK        2   +  1
023300010116     O          E            riepilogo   2
023400010116
023500010116     Otis7t1ps  E            testdett          2
023600010116     O                                              'Upload via Internet'
023700010116     O                                           +1 'Traduzione TIVIN00R -'
023800010116     O                                           +1 'FIAVE00T'
023900010116     O                                           +1 'Testate bolle'
024000010116     O          E            testdett    3
024100010116     O                                           +2 'N° rec'
024200010116     O                                           +3 'Anteprima contenuto'
024300010116     O          E            findett     2
024400010116     O                       wrkdata
024500010116     O                       wrkora              +1
024600010116     O                       procname            +1
024700010116     O          E            findett     2
024800010116     O                                              'Cliente..................:'
024900010116     O                                           +1 'Averna '
025000010116     O          E            findett     2
025100010116     O                                              'Riferimento Strategi.....:'
025200010116     O                       vlrhdl              +1
025300010116     O          E            findett     2
025400010116     O                                              'Giusti...................:'
025500010116     O                       §CTROK        2   +  1
025600010116     O          E            findett     2
025700010116** CMD - COMANDI CL
025800011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
025900011113OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
026000010116DLTOVR FILE(TIS7T1P TIS7T1PS) LVL(*)
026100010116
026200010116
