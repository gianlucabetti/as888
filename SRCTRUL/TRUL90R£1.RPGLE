000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030610      * TRUL90R  *---------------------------------------------------*
000300030609      * - RICHIESTA STAMPANTI                                        *
000400030609      *--------------------------------------------------------------*
000500030612     FTRUL90D   CF   E             WORKSTN  USROPN
000600970701     FTABEL00F  IF   E           K DISK
000700030609
000800940928     D CM              S              1    DIM(37) CTDATA PERRCD(37)
000900030610     D TRUL90DS      E DS
001000030609     D UT§DSE0F      E DS
001100940928     D CNCR80        E DS
001200970630     D DS5P          E DS
001300030609     D DS5PB         E DS
001400970630     D DS5PM         E DS
001500040316     D KPJBA         E DS
001600020503     D  REM                    1      3
001700040316     D                 DS
001800040316     D  ESSE                   1      2    inz('S')
001900040316     D  inizio                 2      4
002000040316     D  fine                   5      7
002100040316     D  stampante              1      7
002200030609      *
002300030609     d kcod            S                   like(TBLCOD)
002400030609     d kkey            S                   like(TBLKEY)
002500030609     d W5pmod          S                   like(§5pmod)
002600030609     d W5pdft          S                   like(§5potq)
002700030609     d lung            S             15  5 inz(37)
002800030609     d comman          S             80
002900030609
003000030609      *---------------------------------------------------------------------
003100921102     C                   MOVEL     *BLANKS       VIDPRE
003200940928     C                   MOVEL     *BLANKS       VIDPRB
003300030609     C                   MOVEL     *BLANKS       VIDPRB4
003400030609     C                   MOVEL     *BLANKS       VIDPRB5
003500030610     C                   MOVEL     'N'           VIDBTS
003600030609     C                   MOVEL     *BLANKS       VIDPRT
003700030609      *
003800030609      * VEDO QUALI STAMPANTI VISUALIZZARE
003900080709     c                   eval      *in10 = (d90rsb = 'S' or d90rsb = 'X'  or
004000080709     c                                      d90rsb = 'O' or d90rsb = 'L')
004100030609     c                   eval      *in20 = *off
004200080709     c                   eval      *in23 = *off
004300030612     c                   eval      *in11 = (d90rse = 'S' or d90rse = 'X')
004400030612     c                   eval      *in12 = (d90rst = 'S' or d90rst = 'X')
004500030612     c                   eval      *in13 = (d90rso = 'S' or d90rso = 'X')
004600030612      *
004700030612      * Se ho almeno un parametro ad "X" non emetto il video (in99 *on)
004800030612      *  in caso contrario lo apro (in99 = *off)
004900030612     c                   IF        d90rsb = 'X' or d90rse = 'X' or
005000030612     c                             d90rst = 'X' or d90rso = 'X'
005100030612     C                   eval      *in99 = *on
005200030612     c                   ELSE
005300030612     C                   eval      *in99 = *off
005400030612     C                   open      TRUL90D
005500030612     c                   ENDIF
005600030609      *
005700030609      * VEDO SE SONO SIMFEL O REMOTO
005800020503     C     SIMTPP        IFEQ      '2'
005900020503     C     SIMTPP        OREQ      *BLANKS
006000020503     C                   MOVEL     SIMPOU        WFIL
006100940928     C                   ELSE
006200940928     C                   MOVEL     SIMFEL        WFIL              3 0
006300940928     C                   ENDIF
006400030609      * SE SONO IN SEDE METTO 46
006500960118     C     WFIL          IFEQ      0
006600020503     C                   Z-ADD     SIMPOU        WFIL
006700960118     C                   ENDIF
006800030609     C                   MOVEL     WFIL          WFILA             3
006900030609      *
007000030609      * PRENDO DALLA TABELLA 5P LE STAMPANTI DA PROPORRE
007100030609      *
007200030609      * Imposto codice p.o. o EDP per utenti EDPxxxxx
007300970630    1C     REM           IFEQ      'EDP'
007400970630     C                   MOVE      'EDP'         W004A
007500970630   X1C                   ELSE
007600970630     C                   MOVE      WFIL          W004A
007700000323    1C                   END
007800030609      *
007900030609     C* Stampante bolle/fatture
008000030609     C                   MOVEL     'B'           W004A             4
008100030609     C                   MOVEL     '5P'          KCOD
008200030609     C                   MOVEL(P)  W004A         KKEY
008300030609     C     KTAB          CHAIN     TABEL00F                           30
008400030609    1C     *IN30         IFEQ      *OFF
008500030609     C                   MOVEL     TBLUNI        DS5PB
008600030609     C                   MOVEL     §5PBOL        VIDPRB
008700030610     C                   MOVEL     §5PBA4        VIDPRB4
008800030610     C                   MOVEL     §5PBA5        VIDPRB5
008900080709     c*  Se passata una 'O' significa che voglio solo la stampante vecchia
009000080709    2c                   if        d90rsb<>'O' and §5pbus<>'O'
009100080709    3C                   if        §5PBTS = 'S' or  §5PBTS = 'X'
009200030612     C                                          or  §5PBTS = d90rsb
009300080709     c                   eval      *in21 = *on
009400080709     c                   eval      *in22 = *on
009500030609     C                   MOVEL     'S'           VIDBTS
009600080709     c* Se §5pbus='4' --> solo stampante per moduli in A4
009700080709    4c                   if        §5pbus='4'
009800080709     c                   eval      *in20 =*off
009900080709     c                   eval      *in22 =*off
010000080709    4C                   endif
010100080709     c* Se §5pbus='5' --> solo stampante per moduli in A5
010200080709    4c                   if        §5pbus='5'
010300080709     c                   eval      *in20 =*off
010400080709     c                   eval      *in21 =*off
010500080709    4C                   endif
010600080709    3C                   END
010700080709     c                   else
010800080709     c* solo stampate old
010900080709     c                   eval      *in23=*on
011000080709    2C                   END
011100080709    1C                   END
011200030609      *
011300030609      * Stampante etichette
011400030609     C                   MOVEL     'S'           W004A             4
011500970630     C                   MOVE      *BLANKS       W5PMOD
011600970630     C                   MOVE      *BLANKS       W5PDFT
011700970630     C                   MOVEL     '5P'          KCOD
011800970630     C                   MOVEL(P)  W004A         KKEY
011900970630     C     KTAB          CHAIN     TABEL00F                           30
012000970630    1C     *IN30         IFEQ      *OFF
012100970630     C                   MOVEL     TBLUNI        DS5P
012200970630     C                   MOVEL     §5POTQ        VIDPRE
012300970630     C                   MOVEL     §5POTQ        W5PDFT
012400970630     C                   MOVE      §5PMOD        W5PMOD
012500970630    1C                   END
012600030609      *
012700970630     C* Stampante tabulati
012800030609     C                   MOVEL     'T'           W004A             4
012900970630     C                   MOVEL     '5P'          KCOD
013000970630     C                   MOVEL(P)  W004A         KKEY
013100970630     C     KTAB          CHAIN     TABEL00F                           30
013200970630    1C     *IN30         IFEQ      *OFF
013300970630     C                   MOVEL     TBLUNI        DS5P
013400970630     C                   MOVEL     §5POTQ        VIDPRT
013500970630    1C                   END
013600030609      *
013700010314     C* Stampante O.R.M.
013800030609     C                   MOVEL     'O'           W004A             4
013900010314     C                   MOVEL     '5P'          KCOD
014000010314     C                   MOVEL(P)  W004A         KKEY
014100010314     C     KTAB          CHAIN     TABEL00F                           30
014200010314    1C     *IN30         IFEQ      *OFF
014300010314     C                   MOVEL     TBLUNI        DS5P
014400010314     C                   MOVEL     §5POTQ        VIDPRO
014500010314    1C                   END
014600030612      *
014700940928     C     FOR01         TAG
014800030612      *
014900030612     C                   IF        *IN99 = *off
015000030612     C                   EXFMT     UL90d01
015100030612     c                   SETOFF                                       404142
015200030612     c                   SETOFF                                       434445
015300030612     c                   ENDIF
015400030612      *
015500030612      * Se emetto il video controllo che il primo carattere delle
015600030612      * stampanti non sia blank
015700030609     C                   SELECT
015800030612     c                   WHEN      *in99 = *on
015900090623     c                   WHEN      *in11 and VIDpre <> *blanks and
016000030609     c                             %subst(vidpre:1:1) = ' '
016100030609     C                   eval      *in41 = *on
016200030612     c                   GOTO      FOR01
016300090623     c                   WHEN      *in10 and *in23 and VIDprb <> *blanks and
016400030609     c                             %subst(vidprb:1:1) = ' '
016500030609     C                   eval      *in40 = *on
016600030612     c                   GOTO      FOR01
016700090623     c                   WHEN      *in10 and *in21 and VIDprb4 <> *blanks and
016800030609     c                             %subst(vidprb4:1:1) = ' '
016900030609     C                   eval      *in44 = *on
017000030612     c                   GOTO      FOR01
017100090623     c                   WHEN      *in10 and *in22 and VIDprb5 <> *blanks and
017200030609     c                             %subst(vidprb5:1:1) = ' '
017300030609     C                   eval      *in45 = *on
017400030612     c                   GOTO      FOR01
017500090623     c                   WHEN      *in12 and VIDprt <> *blanks and
017600030609     c                             %subst(vidprT:1:1) = ' '
017700030609     C                   eval      *in42 = *on
017800030612     c                   GOTO      FOR01
017900090623     c                   WHEN      *in13 and VIDpro <> *blanks and
018000030609     c                             %subst(vidpro:1:1) = ' '
018100030609     C                   eval      *in43 = *on
018200030612     c                   GOTO      FOR01
018300030609     C                   ENDSL
018400030612      *
018500030609      * F3 FINE
018600030609    1C     *INKC         IFEQ      *ON
018700030610     C                   MOVEL     '1'           d90F3
018800030609   x1C                   ELSE
018900030609      *
019000030609      * CONTROLLO LA ESISTENZA DELLE STAMPANTI
019100090623      * Se non trovata e non emetto il video
019200090623      *  o non serve --> forzo  QPRINT
019300030609      *
019400030609      * Etichette
019500940928     C                   MOVEA     VIDPRE        CM(12)
019600030609     C                   MOVEA     CM            COMMAN
019700940928     C                   CALL      'QCMDEXC'                            41
019800940928     C                   PARM                    COMMAN
019900940928     C                   PARM                    LUNG
020000030612      *
020100030612     c                   Select
020200030612     c                   When      *in41 = *off
020300090623     c                   When      *in99 = *on  or not *in11
020400090623     c                   eval      VIDPRE = 'QPRINT    '
020500030612     c                   Other
020600030612     C                   GOTO      FOR01
020700030612     c                   EndSl
020800030612      *
020900030609      *
021000030609      * BOLLE/FATTURE
021100080709      * Stampante OLD
021200080709     c
021300940928     C                   MOVEA     VIDPRB        CM(12)                         BOLLE
021400030609     C                   MOVEA     CM            COMMAN
021500940928     C                   CALL      'QCMDEXC'                            40
021600940928     C                   PARM                    COMMAN
021700940928     C                   PARM                    LUNG
021800030612      *
021900030612     c                   Select
022000030612     c                   When      *in40 = *off
022100090623     c                   When      *in99 = *on or not *in10 or not *in23
022200090623     c                   eval      VIDPRB = 'QPRINT    '
022300030612     c                   Other
022400030612     C                   GOTO      FOR01
022500080709     c                   EndSl
022600030609      *
022700080709      * Stampante A4
022800030609     C                   MOVEA     VIDPRB4       CM(12)                         BOLLE
022900030609     C                   MOVEA     CM            COMMAN
023000030611     C                   CALL      'QCMDEXC'                            44
023100030609     C                   PARM                    COMMAN
023200030609     C                   PARM                    LUNG
023300030612      *
023400030612     c                   Select
023500030612     c                   When      *in44 = *off
023600090623     c                   When      *in99 = *on or not *in10 or not *in21
023700090623     c                   eval      VIDPRB4 = 'QPRINT    '
023800030612     c                   Other
023900030612     C                   GOTO      FOR01
024000030612     c                   EndSl
024100030612      *
024200080709      * Stampante A5
024300030609     C                   MOVEA     VIDPRB5       CM(12)                         BOLLE
024400030609     C                   MOVEA     CM            COMMAN
024500030612     C                   CALL      'QCMDEXC'                            45
024600030609     C                   PARM                    COMMAN
024700030609     C                   PARM                    LUNG
024800030612      *
024900030612     c                   Select
025000030612     c                   When      *in45 = *off
025100090623     c                   When      *in99 = *on or not *in10 or not *in22
025200090623     c                   eval      VIDPRB5 = 'QPRINT    '
025300030612     c                   Other
025400030612     C                   GOTO      FOR01
025500030612     c                   EndSl
025600030612      *
025700030609      *
025800030609      * Tabulati
025900940928     C                   MOVEA     VIDPRT        CM(12)                         TABULATI
026000030609     C                   MOVEA     CM            COMMAN
026100940928     C                   CALL      'QCMDEXC'                            42
026200940928     C                   PARM                    COMMAN
026300940928     C                   PARM                    LUNG
026400030612      *
026500030612     c                   Select
026600030612     c                   When      *in42 = *off
026700090623     c                   When      *in99 = *on or not *in12
026800090623     c                   eval      VIDPRT = 'QPRINT    '
026900030612     c                   Other
027000030612     C                   GOTO      FOR01
027100030612     c                   EndSl
027200030609      *
027300010314      * O.R.M.
027400010314     C                   MOVEA     VIDPRO        CM(12)                         O.R.M.
027500030609     C                   MOVEA     CM            COMMAN
027600010314     C                   CALL      'QCMDEXC'                            43
027700010314     C                   PARM                    COMMAN
027800010314     C                   PARM                    LUNG
027900030612      *
028000030612     c                   Select
028100030612     c                   When      *in43 = *off
028200090623     c                   When      *in99 = *on  or not *in13
028300090623     c                   eval      VIDPRT = 'QPRINT    '
028400030612     c                   Other
028500030612     C                   GOTO      FOR01
028600030612     c                   EndSl
028700030612      *
028800030609      *
028900030609      * IMPOSTO I MODULI
029000030609      *
029100030609      * Etichette
029200030609    2C     *IN11         IFEQ      *ON                                          ETICHETTE
029300030609      *
029400030610    3C     d90MDE        IFEQ      *BLANKS
029500030610     C                   MOVEL     'ETICH'       d90MDE
029600030609    3C                   ENDIF
029700030609      * VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
029800030609    4C     SIMTPP        IFEQ      '2'
029900020503     C     SIMTPP        OREQ      *BLANKS
030000030610     C                   MOVE      WFIL          d90MDE
030100030609    4C                   ENDIF
030200030610     C                   MOVEL     VIDPRE        d90PRE
030300970701     C*****************************************************************
030400970701     C* VARIATA STAMPANTE ETICHETTE RISPETTO AL DEFAULT:
030500041203     c*
030600970630    3C     VIDPRE        IFNE      W5PDFT
030700040316     c* as unico
030800040316     c                   eval      inizio=%subst(vidpre:2:3)
030900040316     c                   eval      fine=%subst(vidpre:8:3)
031000040316     C                   MOVEL(p)  stampante     KKEY
031100040316     c* CHAIN STAMPANTE
031200970630     C                   MOVEL     '5P'          KCOD
031300970630     C     KTAB          CHAIN     TABEL00F                           30
031400970630    4C     *IN30         IFEQ      *OFF
031500970630     C                   MOVEL     TBLUNI        DS5P
031600970630     C                   MOVEL     §5PMOD        W5PMOD
031700970701   x4C                   ELSE
031800040316     c
031900970701     C* SE NON HO TROVATO IL RECORD RICHIESTO CERCO IL DEFAULT
032000970701     C                   MOVE      '    '        KKEY
032100040316     c
032200970701     C     KTAB          CHAIN     TABEL00F                           30
032300970701    5C     *IN30         IFEQ      *OFF
032400970701     C                   MOVEL     TBLUNI        DS5P
032500970701     C                   MOVEL     §5PMOD        W5PMOD
032600970701    5C                   END
032700970630    4C                   END
032800970630    3C                   END
032900970701     C*****************************************************************
033000970701     C* COL MODELLO PRENDO, SEMPRE DALLA "5P", IL NOME DEI PGM DI
033100970701     C* DI STAMPA ETICHETTE
033200970630     C                   MOVEL     'M'           W009A             9
033300970630     C                   MOVE      W5PMOD        W009A
033400970701     C                   MOVEL     W009A         KKEY
033500970630     C                   MOVEL     '5P'          KCOD
033600970701     C     KTAB          CHAIN     TABEL00F                           30
033700970701    3C     *IN30         IFEQ      *OFF
033800970630     C                   MOVEL     TBLUNI        DS5PM
033900970630     C* IMPOSTO PGM DI STAMPA ETICHETTE
034000030610     C                   MOVEL     §5PPSS        d90PSS
034100030610     C                   MOVEL     §5PPSB        d90PSB
034200030610     C                   MOVEL     §5PPSD        d90PSD
034300030610     C                   MOVEL     §5PPSF        d90PSF
034400970701    3C                   END
034500030609    2C                   ENDIF
034600030609      *
034700030609      * BOLLE/FATTURE
034800030609    2c                   IF        *IN10 = *ON
034900030609      *
035000030610     c                   If        d90MDB = *blanks
035100030610     c                   eval      d90MDB = 'BOLLE' + wfilA
035200030609     c                   Else
035300030610     c                   eval      %subst(d90MDB:8:3) = wfilA
035400030609     c                   EndIf
035500030609      *
035600030610     c                   If        d90MDB4 = *blanks
035700030610     c                   eval      d90MDB4 = 'BOLLE' + wfilA +'A4'
035800030609     c                   Else
035900030612     c                   eval      %subst(d90MDB4:6:5) = wfilA + 'A4'
036000030609     c                   EndIf
036100030609      *
036200030610     c                   If        d90MDB5 = *blanks
036300030610     c                   eval      d90MDB5 = 'BOLLE' + wfilA +'A5'
036400030609     c                   Else
036500030612     c                   eval      %subst(d90MDB5:6:5) = wfilA + 'A5'
036600030609     c                   EndIf
036700030609      *
036800030610     C                   MOVEL     VIDPRB        d90PRB
036900030610     C                   MOVEL     VIDPRB4       d90PRB4
037000030610     C                   MOVEL     VIDPRB5       d90PRB5
037100030610     C                   MOVEL     VIDBTS        d90BSL
037200030609      * Pgm di stampa
037300030609     C                   SELECT
037400030610     c                   WHEN      d90BSL = 'S' and §5PBPL <> *blanks
037500030610     C                   movel     §5pbpl        d90PSL
037600030610     c                   WHEN      d90BSL = 'S'
037700030610     C                   movel     'FNLSB5R'     d90PSL
037800030610     c                   WHEN      d90BSL = *blank and §5PBPG <> *blanks
037900030610     C                   movel     §5pbpg        d90PSL
038000030609     c                   OTHER
038100030610     C                   movel     'FNLSB0R'     d90PSL
038200030609     c                   ENDSL
038300030609      *
038400030609    2C                   ENDIF
038500030609      *
038600030609      * Tabulati
038700030609    2C     *IN12         IFEQ      *ON                                          TABULATI
038800030609      *
038900030610    3C     d90MDT        IFEQ      *BLANKS
039000940928     C* VEDO QUANTE COPIE SONO
039100030610    4C     d90NCP        IFLE      '1'
039200030610     C                   MOVEL     '*STD '       d90MDT
039300940928     C                   ELSE
039400030610     C                   MOVEL     '  COPIE'     d90MDT
039500030610     C                   MOVEL     d90NCP        d90MDT
039600030609    4C                   ENDIF
039700030609    3C                   ENDIF
039800940928     C* VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
039900030609    3C     SIMTPP        IFEQ      '2'
040000020503     C     SIMTPP        OREQ      *BLANKS
040100030610     C                   MOVE      WFIL          d90MDT
040200030609    3C                   ENDIF
040300030609      *
040400030610     C                   MOVEL     VIDPRT        d90PRT
040500030609    2C                   ENDIF
040600010314     C*
040700010314      * O.R.M.
040800030609    2C     *IN13         IFEQ      *ON                                          O.R.M.
040900030609      *
041000030610     C     d90MDO        IFEQ      *BLANKS
041100040316     C                   movel     Wfil          W003A             3
041200040202     c                   eval      d90mdo = 'ORMA5_' + W003A
041300010314     C                   ENDIF
041400030609      *
041500030610     C                   MOVEL     VIDPRO        d90PRO
041600030609    2C                   ENDIF
041700030610      *
041800030609    1C                   ENDIF
041900030609      *
042000030612      *  se non ho emesso il video e il pgm di stampa LDV non è impostato
042100030612      *  forzo il pgm di stamp asu modulo "vecchio"
042200030612     c                   IF        *in99 = *on and D90PSL = *blanks
042300030612     c                   eval      D90PSL = 'FNLSB0R'
042400030612     c                   eval      D90BSL = *blanks
042500030612     c                   ENDIF
042600030612      *
042700030609     C                   EVAL      *INLR = *ON
042800030609      *---------------------------------------------------------------------
042900030609      * SBR INIZIALE
043000030609      *---------------------------------------------------------------------
043100030609     c     *inzsr        begsr
043200030609      *
043300030609     C     *ENTRY        PLIST
043400030609     C                   PARM                    KPJBA
043500030610     C                   PARM                    TRUL90DS
043600030609     C*
043700030609     C     KTAB          KLIST
043800030609     C                   KFLD                    CODUT
043900030609     C                   KFLD                    KCOD
044000030609     C                   KFLD                    KKEY
044100030609     C*
044200030610     C                   CLEAR                   d90F3
044300030610     C                   CLEAR                   d90PRE
044400030610     C                   CLEAR                   d90PRB
044500030610     C                   CLEAR                   d90PRT
044600030610     C                   CLEAR                   d90PRO                         O.R.M.
044700030610     C                   CLEAR                   d90PSS
044800030610     C                   CLEAR                   d90PSB
044900030610     C                   CLEAR                   d90PSD
045000030610     C                   CLEAR                   d90PSF
045100030610     C                   CLEAR                   d90BSL
045200030610     C                   CLEAR                   d90PSL
045300030610     C                   CLEAR                   d90PRB4
045400030610     C                   CLEAR                   d90PRB5
045500030609     C*
045600030609     C* PRENDO DATI DELL'AZIENDA
045700030609     C                   Z-ADD     1             CODUT
045800030609     C                   CALL      'X§PARUT'
045900030609     C                   PARM                    UT§DSE0F
046000030609     C                   MOVEL     RAGUT         VIDDSA
046100030609     C                   MOVEL     REC80         CNCR80
046200030609      *
046300030609     c                   endsr
046400030609      *---------------------------------------------------------------------
046500940928**
046600940928CHKOBJ OBJ(XXXXXXXXXX) OBJTYPE(*OUTQ)
