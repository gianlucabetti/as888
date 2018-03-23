000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200001127      *
000300001127      * FNLS84R *----------------------------------------------------*
000400001127      *         - STAMPA SEGNACOLLI DA FISGN
000500001127      *--------------------------------------------------------------*
000600001127      *
000700010405     FFISGN06L  iF   E           K DISK
000800001127     F                                     RENAME(FISGN000:FISGN006)
000900010405     FFISGN07L  iF   E           K DISK
001000001127     F                                     RENAME(FISGN000:FISGN007)
001100010405     FFISGN08L  iF   E           K DISK
001200001127     F                                     RENAME(FISGN000:FISGN008)
001300010405     FFISGN09L  iF   E           K DISK
001400001127     F                                     RENAME(FISGN000:FISGN009)
001500010405     FFISGN10L  iF   E           K DISK
001600010122     F                                     RENAME(FISGN000:FISGN010)
001700010405     FFISGN11L  iF   E           K DISK
001800010122     F                                     RENAME(FISGN000:FISGN011)
001900010523     FFISGN01L  iF   E           K DISK
002000010523     F                                     RENAME(FISGN000:FISGN001)
002100050418     fFIAR501L  if   e           k disk
002200941228     FFNLV22P   O    F  132        PRINTER
002300951102     D K15             S              1    DIM(15)
002400900523     D PARAM           DS
002500910205     D  VIDKSC                11     17
002600941228     D  VIDCPR                18     24
002700001127      * ORDINAMENTO PADRONCINO: M - MITTENTE
002800001127      *                         D - DESTINATARIO
002900010122      *                         C - CHI SONO
003000960320     D  VIDOR1                25     25
003100001127      * DATA SPEDIZIONE AAMMGG
003200941228     D  VIDDSP                26     33  0
003300010405      * S= RISTAMPA
003400941228     D  VIDRSN                34     34
003500001127      * PARZIALIZZAZIONE PER RAGIONE SOCIALE
003600951102     D  VIDRSM                35     49
003700010122      * ORDINAMENTO MITTENTE  : P - CODICE PRODOTTO
003800001127      *                         T - TERMINAL DI ARRIVO
003900010122      *                         C - CHI SONO
004000960320     D  VIDOR2                50     50
004100001127      * PGM STAMPA SEGNACOLLI
004200941228     D  PSS                  227    236
004300010523      * CODICE "CHI SONO"
004400010523     D  VIDWHO                60     94
004500910412     D                 DS
004600001127     D  SGNAAS                 1      4  0
004700001127     D  SGNMGS                 5      8  0
004800001127     D  SGNDSP                 1      8  0
004900971217     D DSBLD         E DS
005000971216     D DSBLDE        E DS
005100050418     d dAR5gen       e ds                  inz
005200910205     D KPJBA         E DS
005300010122     D CNCR80        E DS
005400010122     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005500001127      *
005600001127      *---------------------------------------------------------------*
005700001127      *    O P E R A Z I O N I    I N I Z I A L I
005800001127      *---------------------------------------------------------------*
005900001127      *
006000000000     C     *ENTRY        PLIST
006100000000     C                   PARM                    KPJBA
006200010122      *
006300010122     C                   Z-ADD     1             CODUT
006400010122     C                   CALL      'X§PARUT'
006500010122     C                   PARM                    UT§DSE
006600010122     C                   MOVEL     REC80         CNCR80
006700001127      *
006800001127     C     KSGN1         KLIST
006900010122     C                   KFLD                    SIMFEL
007000010122     C                   KFLD                    KSC
007100001127     C                   KFLD                    ST2               1 0
007200001127      *
007300001127     C     KSGN2         KLIST
007400001127     C                   KFLD                    LNP               3 0
007500001127     C                   KFLD                    PDR               3 0
007600001127     C                   KFLD                    ST2
007700010405      *
007800010405     C     KSGN1A        KLIST
007900010405     C                   KFLD                    SIMFEL
008000010405     C                   KFLD                    KSC
008100010405      *
008200010405     C     KSGN2A        KLIST
008300010405     C                   KFLD                    LNP               3 0
008400010405     C                   KFLD                    PDR               3 0
008500001127      *
008600010523     C     KSGN1B        KLIST
008700010523     C                   KFLD                    SIMFEL
008800010523     C                   KFLD                    VIDWHO
008900050418      * FIAR501L
009000050418     c     k05AR501      klist
009100050418     c                   kfld                    SGNaas
009200050418     c                   kfld                    SGNfls
009300050418     c                   kfld                    SGNnrs
009400050418     c                   kfld                    SGNnsp
009500050418     c                   kfld                    AR5trd
009600050418     c                   eval      AR5trd = 'GEN'
009700010523      *
009800010405     C                   Z-ADD     0             ST2
009900010405     c*
010000910205     C                   MOVEL     KPJBU         PARAM
010100010405      *
010200010405     c     vidrsn        ifeq      'N'
010300010405     c                   setoff                                       10
010400050418     ***c                   clear                   §ldrst
010500010405     c                   else
010600010405     c                   seton                                        10
010700050418     ***c                   movel     'R'           §ldrst
010800010405     c                   endif
010900001127      **
011000001127      **  IMMESSO  C O D I C E   C L I E N T E   M I T T E N T E
011100001127      **
011200910205     C     VIDKSC        IFGT      *ZEROS
011300910205     C                   MOVEL     VIDKSC        KSC               7 0
011400010405      * 08 ON - ORDINATO PER CODICE PRODOTTO
011500960320     C     VIDOR2        IFEQ      'P'
011600960320     C                   SETON                                        08
011700960320     C                   ENDIF
011800001127      * 09 ON - ORDINATO PER TERMINAL ARRIVO
011900960320     C     VIDOR2        IFEQ      'T'
012000960320     C                   SETON                                        09
012100960320     C                   ENDIF
012200010122      * 11 ON - ORDINATO PER CHI SONO
012300010122     C     VIDOR2        IFEQ      'C'
012400010122     C                   SETON                                        11
012500010122     C                   ENDIF
012600910205     C                   END
012700001127      **
012800001127      **  IMMESSO  C O D I C E   P A D R O N C I N O
012900001127      **
013000910205     C     VIDCPR        IFGT      *ZEROS
013100001127     C                   MOVEL     VIDCPR        LNP
013200001127     C                   MOVE      VIDCPR        PDR
013300960320     C* 04 ON - ORDINATO PER MITTENTE
013400960320     C     VIDOR1        IFEQ      'M'
013500910412     C                   SETON                                        04
013600951102     C* VEDO SE IMMESSA LA RAGIONE SOCIALE
013700951102     C     VIDRSM        IFNE      *BLANKS
013800951102     C     ' '           CHECKR    VIDRSM        WPOS              2 0
013900960320     C                   ENDIF
014000960320     C                   ENDIF
014100960320     C* 07 ON - ORDINATO PER DESTINATARIO
014200960320     C     VIDOR1        IFEQ      'D'
014300960320     C                   SETON                                        07
014400960320     C                   ENDIF
014500010122      * 12 ON - ORDINATO PER CHI SONO
014600010122     C     VIDOR1        IFEQ      'C'
014700010122     C                   SETON                                        12
014800010122     C                   ENDIF
014900960320     C                   ENDIF
015000010523      **
015100010523      **  IMMESSO  C O D I C E   " C H I   S O N O "
015200010523      **
015300010523     C     VIDWHO        IFNE      *BLANKS
015400010523     C                   SETON                                        13
015500010523     C                   ENDIF
015600010523      *
015700001127      *---------------------------------------------------------------*
015800001127      *    R O U T I N E    P R I N C I P A L E
015900001127      *---------------------------------------------------------------*
016000010405      *** 10 OFF=   STAMPA ***
016100010405     c     *IN10         IFEQ      *OFF
016200001127      * PADRONCINO
016300001127     C   07KSGN2         SETLL     FISGN07L
016400001127     C   04KSGN2         SETLL     FISGN09L
016500010122     C   12KSGN2         SETLL     FISGN11L
016600001127      * MITTENTE
016700001127     C   08KSGN1         SETLL     FISGN06L
016800001127     C   09KSGN1         SETLL     FISGN08L
016900010122     C   11KSGN1         SETLL     FISGN10L
017000001127      * PADRONCINO
017100001127     C   07KSGN2         READE     FISGN07L                               31
017200001127     C   04KSGN2         READE     FISGN09L                               31
017300010122     C   12KSGN2         READE     FISGN11L                               31
017400001127      * MITTENTE
017500001127     C   08KSGN1         READE     FISGN06L                               31
017600001127     C   09KSGN1         READE     FISGN08L                               31
017700010122     C   11KSGN1         READE     FISGN10L                               31
017800010523      * CODICE "CHI SONO"
017900010523     C   13KSGN1B        CHAIN     FISGN01L                           31
018000010405     c                   ELSE
018100010405      *** 10 ON = RISTAMPA ***
018200010405      * PADRONCINO
018300010405     C   07KSGN2         SETGT     FISGN07L
018400010405     C   04KSGN2         SETGT     FISGN09L
018500010405     C   12KSGN2         SETGT     FISGN11L
018600010405      * MITTENTE
018700010405     C   08KSGN1         SETGT     FISGN06L
018800010405     C   09KSGN1         SETGT     FISGN08L
018900010405     C   11KSGN1         SETGT     FISGN10L
019000010405      * PADRONCINO
019100010405     C   07KSGN2A        READE     FISGN07L                               31
019200010405     C   04KSGN2A        READE     FISGN09L                               31
019300010405     C   12KSGN2A        READE     FISGN11L                               31
019400010405      * MITTENTE
019500010405     C   08KSGN1A        READE     FISGN06L                               31
019600010523     C   09KSGN1A        READE     FISGN08L                               31
019700010405     C   11KSGN1A        READE     FISGN10L                               31
019800010523      * CODICE "CHI SONO"
019900010523     C   13KSGN1B        CHAIN     FISGN01L                           31
020000010405     c                   ENDIF
020100001127      *
020200001127     C     *IN31         DOWEQ     *OFF
020300001127      *
020400001127     C     SGNATB        IFEQ      ' '
020500010405     c* scarto record di sola ristampa (no disk "C")
020600010405     c     sgnfl2        andne     'N'
020700001127      *
020800001127      * NEL CASO IN CUI SIA VALORIZZATA LA DATA DI SPEDIZIONE
020900001127      * CONTROLLO CHE LA DATA SPEDIZIONE SEGNACOLLI
021000001127      * LETTI SIA UGUALE A QUELLA RICHIESTA A VIDEO
021100001127      *
021200001127     C     VIDDSP        IFNE      *ZEROS
021300001127     C     SGNDSP        ANDEQ     VIDDSP
021400001127     C     VIDDSP        OREQ      *ZEROS
021500010607     C     *IN13         OREQ      *ON
021600001127      *
021700951102     C                   MOVEL     'S'           WSTA              1
021800001127      * PER MITTENTE CON RAGIONE SOCIALE CONTROLLO LA PARZIALIZZAZ.
021900001127     C   04VIDRSM        IFNE      *BLANKS
022000960320     C                   EXSR      CTRPAR
022100001127     C                   ENDIF
022200001127      * STAMPO
022300001127     C     WSTA          IFEQ      'S'
022400910205     C                   EXSR      STASCD
022500001127     C                   ENDIF
022600001127      *
022700001127     C                   ENDIF
022800001127      *
022900001127     C                   ENDIF
023000010405      *** 10 OFF=   STAMPA ***
023100010405     c     *IN10         IFEQ      *OFF
023200010524      * CODICE "CHI SONO"
023300010524     C   13              LEAVE
023400001127      * PADRONCINO
023500001127     C   07KSGN2         READE     FISGN07L                               31
023600001127     C   04KSGN2         READE     FISGN09L                               31
023700010122     C   12KSGN2         READE     FISGN11L                               31
023800001127      * MITTENTE
023900001127     C   08KSGN1         READE     FISGN06L                               31
024000001127     C   09KSGN1         READE     FISGN08L                               31
024100010122     C   11KSGN1         READE     FISGN10L                               31
024200010405     C                   ELSE
024300010405      *** 10 ON = RISTAMPA ***
024400010524      * CODICE "CHI SONO"
024500010524     C   13              LEAVE
024600010405      * PADRONCINO
024700010405     C   07KSGN2A        READE     FISGN07L                               31
024800010405     C   04KSGN2A        READE     FISGN09L                               31
024900010405     C   12KSGN2A        READE     FISGN11L                               31
025000010405      * MITTENTE
025100010405     C   08KSGN1A        READE     FISGN06L                               31
025200010405     C   09KSGN1A        READE     FISGN08L                               31
025300010405     C   11KSGN1A        READE     FISGN10L                               31
025400010405     c                   ENDIF
025500001127     C                   ENDDO
025600001127      *
025700000000     C                   SETON                                        LR
025800001127      *
025900001127      *---------------------------------------------------------------*
026000001127      *    S U B R O U T I N E S
026100001127      *---------------------------------------------------------------*
026200001127      *
026300001127      *    CTRPAR: CONTROLLO PARZIALIZZAZIONE PER RAGIONE SOCIALE
026400001127      *    ======================================================
026500001127      *
026600951102     C     CTRPAR        BEGSR
026700001127      *
026800951122     C     WPOS          IFLT      15
026900951102     C     WPOS          ADD       1             X                 2 0
027000001127     C                   MOVEA     SGNRSM        K15
027100951102     C                   MOVEA     *BLANKS       K15(X)
027200951102     C                   MOVEA     K15           W015A            15
027300951122     C                   ELSE
027400001127     C                   MOVEL     SGNRSM        W015A
027500951122     C                   ENDIF
027600001127      *
027700951102     C     W015A         IFNE      VIDRSM
027800951102     C                   CLEAR                   WSTA
027900951102     C                   ENDIF
028000001127      *
028100951102     C                   ENDSR
028200001127      *
028300001127      *    STASCD: STAMPA BOLLA
028400001127      *    ====================
028500001127      *
028600910205     C     STASCD        BEGSR
028700050418      *
028800050418      * Verifica se spedizione "di valore"
028900050418     c                   exsr      CtrSpeVal
029000050418      *
029100050418      * impostazione flag ristampa/di_valore
029200050418     c                   clear                   §LDrst
029300050418sel 1c                   select
029400050418      * - ristampa collo appartenente a spedizione "di valore"
029500050419w   1c*** impossibile:   when          SGNrst  =  'R'
029600050419     c***                          and §AR5bva =  'V'
029700050419     c***                movel     'X'           §LDrst
029800050419      * - modifica collo appartenente a spedizione "di valore"
029900050419w   1c                   when          SGNrst  =  'M'
030000050419     c                             and §AR5bva =  'V'
030100050419     c                   movel     'W'           §LDrst
030200050418      * - stampa   collo appartenente a spedizione "di valore"
030300050419w   1c                   when          SGNrst  =  *blanks
030400050419     c                             and §AR5bva =  'V'
030500050418     c                   movel     'V'           §LDrst
030600050419      * - stampa/ristampa/modifica collo di spedizione NON "di valore"
030700050419w   1c                   other
030800050419     c                   movel     SGNrst        §LDrst
030900050418e   1c                   endsl
031000001127      *
031100001127     C                   move      sgnfls        §ldlnp
031200001127     C                   move      sgndlp        §lddlp
031300001127     C                   move      sgncdp        §ldcdp
031400010122      *    Se richiesto ordinam. per CHI SONO lo stampo nel Rif.
031500010122     C     *in11         ifeq      *on
031600010122     C     *in12         oreq      *on
031700010524     C     *in13         oreq      *on
031800010122     C                   movel     sgnwho        §ldcdp
031900010122     C                   else
032000010122     C                   move      sgncdp        §ldcdp
032100010122     C                   endif
032200001127     C                   move      sgnnrs        §ldnrs
032300001127     C                   move      sgnncd        §ldncd
032400001127     C                   move      sgnlna        §ldlna
032500001127     C                   move      sgndla        §lddla
032600001127     C                   move      sgntna        §ldtna
032700001127     C                   move      sgntsp        §ldtsp
032800001127     C                   move      sgnzsc        §ldzsc
032900001127     C                   move      sgnnc1        §ldnc1
033000001127     C                   move      sgnnc2        §ldnc2
033100001127     C                   move      sgnrsm        §ldrsm
033200001127     C                   move      sgnprm        §ldprm
033300001127     C                   move      sgnrsd        §ldrsd
033400001127     C                   move      sgncad        §ldcad
033500001127     C                   move      sgnind        §ldind
033600001127     C                   move      sgnlod        §ldlod
033700001127     C                   move      sgnprd        §ldprd
033800001127     C                   move      sgncpd        §ldcpd
033900001127     C                   move      sgnncp        §ldncp
034000001127     C                   move      sgnncu        §ldncu
034100050418     C*** (vedi sopra)   move      sgnrst        §ldrst
034200001127     C                   move      sgnvol        §ldvol
034300001127     C                   move      sgnpkg        §ldpkg
034400001127     C                   move      sgnfvr        §ldfvr
034500001127     C                   move      sgnaas        §ldaas
034600001127     C                   move      sgnmgs        §ldmgs
034700001127      *
034800001127     C                   move      sgntst        §detst
034900001127     C                   move      sgntse        §detse
035000001127     C                   move      sgnsse        §desse
035100001127     C                   move      sgnsip        §desip
035200001127     C                   move      sgnsia        §desia
035300001127     C                   move      sgnawb        §deawb
035400001127     C                   move      sgnisa        §deisa
035500001127     C                   move      sgnfca        §defca
035600001127     C                   move      sgneur        §deeur
035700001127     C                   move      sgnxx1        §dexx1
035800001127     C                   move      sgnxx2        §dexx2
035900001127      *
036000941228     C                   CALL      PSS
036100910205     C                   PARM                    DSBLD
036200971217     C                   PARM                    DSBLDE
036300001127      *
036400910205     C                   ENDSR
036500050418      *---------------------------------------------------------------*
036600050418      *  Verifica se spedizione "DI VALORE"                           *
036700050418      *---------------------------------------------------------------*
036800050418     c     CtrSpeVal     BEGSR
036900050418      *
037000050418      * Verifica se spedizione in esame = ultima "reperita" su FIAR5
037100050418if  1c                   if            SGNaas = AR5aas
037200050418     c                             and SGNfls = AR5lnp
037300050418     c                             and SGNnrs = AR5nrs
037400050418     c                             and SGNnsp = AR5nsp
037500050418     c                   leavesr
037600050418e   1c                   endif
037700050418      *
037800050418      * Reperisce i dati da FIAR5
037900050418     c                   clear                   dAR5gen
038000050418     c     K05AR501      setgt     FIAR5000
038100050418do  1c                   dou            %eof(FIAR501L)
038200050418     c                             or   AR5atb =  *blanks
038300050418     c     K05AR501      readpe    FIAR5000
038400050418e   1c                   enddo
038500050418if  1c                   if             NOT %eof(FIAR501L)
038600050418     c                             and  AR5atb =  *blanks
038700050418     c                   movel     AR5uni        dAR5gen
038800050418e   1c                   endif
038900050418      *
039000050418     c                   ENDSR
039100001127      *
039200941228     OFNLV22P   T    LRNLR
