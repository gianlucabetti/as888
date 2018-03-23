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
002300131023     d k_key           S              3
002400131023     d par_key         S              3
002500131023     d kcod            S                   like(TBLCOD)
002600030609     d kkey            S                   like(TBLKEY)
002700030609     d W5pmod          S                   like(§5pmod)
002800030609     d W5pdft          S                   like(§5potq)
002900030609     d lung            S             15  5 inz(37)
003000030609     d comman          S             80
003100030609
003200030609      *---------------------------------------------------------------------
003300921102     C                   MOVEL     *BLANKS       VIDPRE
003400940928     C                   MOVEL     *BLANKS       VIDPRB
003500030609     C                   MOVEL     *BLANKS       VIDPRB4
003600030609     C                   MOVEL     *BLANKS       VIDPRB5
003700030610     C                   MOVEL     'N'           VIDBTS
003800030609     C                   MOVEL     *BLANKS       VIDPRT
003900030609      *
004000030609      * VEDO QUALI STAMPANTI VISUALIZZARE
004100080709     c                   eval      *in10 = (d90rsb = 'S' or d90rsb = 'X'  or
004200080709     c                                      d90rsb = 'O' or d90rsb = 'L')
004300030609     c                   eval      *in20 = *off
004400080709     c                   eval      *in23 = *off
004500030612     c                   eval      *in11 = (d90rse = 'S' or d90rse = 'X')
004600030612     c                   eval      *in12 = (d90rst = 'S' or d90rst = 'X')
004700030612     c                   eval      *in13 = (d90rso = 'S' or d90rso = 'X')
004800030612      *
004900030612      * Se ho almeno un parametro ad "X" non emetto il video (in99 *on)
005000030612      *  in caso contrario lo apro (in99 = *off)
005100030612     c                   IF        d90rsb = 'X' or d90rse = 'X' or
005200030612     c                             d90rst = 'X' or d90rso = 'X'
005300030612     C                   eval      *in99 = *on
005400030612     c                   ELSE
005500030612     C                   eval      *in99 = *off
005600030612     C                   open      TRUL90D
005700030612     c                   ENDIF
005800030609      *
005900030609      * VEDO SE SONO SIMFEL O REMOTO
006000020503     C     SIMTPP        IFEQ      '2'
006100020503     C     SIMTPP        OREQ      *BLANKS
006200020503     C                   MOVEL     SIMPOU        WFIL
006300940928     C                   ELSE
006400940928     C                   MOVEL     SIMFEL        WFIL              3 0
006500940928     C                   ENDIF
006600030609      * SE SONO IN SEDE METTO 46
006700960118     C     WFIL          IFEQ      0
006800020503     C                   Z-ADD     SIMPOU        WFIL
006900960118     C                   ENDIF
007000030609     C                   MOVEL     WFIL          WFILA             3
007100030609      *
007200030609      * PRENDO DALLA TABELLA 5P LE STAMPANTI DA PROPORRE
007300030609      *
007400030609      * Imposto codice p.o. o EDP per utenti EDPxxxxx
007500131023     c                   select
007600131023     c                   when      k_key<>*blanks
007700131023     C                   MOVE      k_key         W004A
007800131023     c                   when      rem='EDP'
007900131023     C                   MOVE      'EDP'         W004A
008000131023     c                   other
008100131023     C                   MOVE      WFIL          W004A
008200131023     c                   endsl
008300131023
008400131023
008500131023    1C**   REM           IFEQ      'EDP'
008600131023     C**                 MOVE      'EDP'         W004A
008700131023   X1C**                 ELSE
008800131023     C**                 MOVE      WFIL          W004A
008900131023    1C**                 END
009000030609      *
009100030609     C* Stampante bolle/fatture
009200030609     C                   MOVEL     'B'           W004A             4
009300030609     C                   MOVEL     '5P'          KCOD
009400030609     C                   MOVEL(P)  W004A         KKEY
009500030609     C     KTAB          CHAIN     TABEL00F                           30
009600030609    1C     *IN30         IFEQ      *OFF
009700030609     C                   MOVEL     TBLUNI        DS5PB
009800030609     C                   MOVEL     §5PBOL        VIDPRB
009900030610     C                   MOVEL     §5PBA4        VIDPRB4
010000030610     C                   MOVEL     §5PBA5        VIDPRB5
010100080709     c*  Se passata una 'O' significa che voglio solo la stampante vecchia
010200080709    2c                   if        d90rsb<>'O' and §5pbus<>'O'
010300080709    3C                   if        §5PBTS = 'S' or  §5PBTS = 'X'
010400030612     C                                          or  §5PBTS = d90rsb
010500080709     c                   eval      *in21 = *on
010600080709     c                   eval      *in22 = *on
010700030609     C                   MOVEL     'S'           VIDBTS
010800080709     c* Se §5pbus='4' --> solo stampante per moduli in A4
010900080709    4c                   if        §5pbus='4'
011000080709     c                   eval      *in20 =*off
011100080709     c                   eval      *in22 =*off
011200080709    4C                   endif
011300080709     c* Se §5pbus='5' --> solo stampante per moduli in A5
011400080709    4c                   if        §5pbus='5'
011500080709     c                   eval      *in20 =*off
011600080709     c                   eval      *in21 =*off
011700080709    4C                   endif
011800080709    3C                   END
011900080709     c                   else
012000080709     c* solo stampate old
012100080709     c                   eval      *in23=*on
012200080709    2C                   END
012300080709    1C                   END
012400030609      *
012500030609      * Stampante etichette
012600030609     C                   MOVEL     'S'           W004A             4
012700970630     C                   MOVE      *BLANKS       W5PMOD
012800970630     C                   MOVE      *BLANKS       W5PDFT
012900970630     C                   MOVEL     '5P'          KCOD
013000970630     C                   MOVEL(P)  W004A         KKEY
013100970630     C     KTAB          CHAIN     TABEL00F                           30
013200970630    1C     *IN30         IFEQ      *OFF
013300970630     C                   MOVEL     TBLUNI        DS5P
013400970630     C                   MOVEL     §5POTQ        VIDPRE
013500970630     C                   MOVEL     §5POTQ        W5PDFT
013600970630     C                   MOVE      §5PMOD        W5PMOD
013700970630    1C                   END
013800030609      *
013900970630     C* Stampante tabulati
014000030609     C                   MOVEL     'T'           W004A             4
014100970630     C                   MOVEL     '5P'          KCOD
014200970630     C                   MOVEL(P)  W004A         KKEY
014300970630     C     KTAB          CHAIN     TABEL00F                           30
014400970630    1C     *IN30         IFEQ      *OFF
014500970630     C                   MOVEL     TBLUNI        DS5P
014600970630     C                   MOVEL     §5POTQ        VIDPRT
014700970630    1C                   END
014800030609      *
014900010314     C* Stampante O.R.M.
015000030609     C                   MOVEL     'O'           W004A             4
015100010314     C                   MOVEL     '5P'          KCOD
015200010314     C                   MOVEL(P)  W004A         KKEY
015300010314     C     KTAB          CHAIN     TABEL00F                           30
015400010314    1C     *IN30         IFEQ      *OFF
015500010314     C                   MOVEL     TBLUNI        DS5P
015600010314     C                   MOVEL     §5POTQ        VIDPRO
015700010314    1C                   END
015800030612      *
015900940928     C     FOR01         TAG
016000030612      *
016100030612     C                   IF        *IN99 = *off
016200030612     C                   EXFMT     UL90d01
016300030612     c                   SETOFF                                       404142
016400030612     c                   SETOFF                                       434445
016500030612     c                   ENDIF
016600030612      *
016700030612      * Se emetto il video controllo che il primo carattere delle
016800030612      * stampanti non sia blank
016900030609     C                   SELECT
017000030612     c                   WHEN      *in99 = *on
017100090623     c                   WHEN      *in11 and VIDpre <> *blanks and
017200030609     c                             %subst(vidpre:1:1) = ' '
017300030609     C                   eval      *in41 = *on
017400030612     c                   GOTO      FOR01
017500090623     c                   WHEN      *in10 and *in23 and VIDprb <> *blanks and
017600030609     c                             %subst(vidprb:1:1) = ' '
017700030609     C                   eval      *in40 = *on
017800030612     c                   GOTO      FOR01
017900090623     c                   WHEN      *in10 and *in21 and VIDprb4 <> *blanks and
018000030609     c                             %subst(vidprb4:1:1) = ' '
018100030609     C                   eval      *in44 = *on
018200030612     c                   GOTO      FOR01
018300090623     c                   WHEN      *in10 and *in22 and VIDprb5 <> *blanks and
018400030609     c                             %subst(vidprb5:1:1) = ' '
018500030609     C                   eval      *in45 = *on
018600030612     c                   GOTO      FOR01
018700090623     c                   WHEN      *in12 and VIDprt <> *blanks and
018800030609     c                             %subst(vidprT:1:1) = ' '
018900030609     C                   eval      *in42 = *on
019000030612     c                   GOTO      FOR01
019100090623     c                   WHEN      *in13 and VIDpro <> *blanks and
019200030609     c                             %subst(vidpro:1:1) = ' '
019300030609     C                   eval      *in43 = *on
019400030612     c                   GOTO      FOR01
019500030609     C                   ENDSL
019600030612      *
019700030609      * F3 FINE
019800030609    1C     *INKC         IFEQ      *ON
019900030610     C                   MOVEL     '1'           d90F3
020000030609   x1C                   ELSE
020100030609      *
020200030609      * CONTROLLO LA ESISTENZA DELLE STAMPANTI
020300090623      * Se non trovata e non emetto il video
020400090623      *  o non serve --> forzo  QPRINT
020500030609      *
020600030609      * Etichette
020700940928     C                   MOVEA     VIDPRE        CM(12)
020800030609     C                   MOVEA     CM            COMMAN
020900940928     C                   CALL      'QCMDEXC'                            41
021000940928     C                   PARM                    COMMAN
021100940928     C                   PARM                    LUNG
021200030612      *
021300030612     c                   Select
021400030612     c                   When      *in41 = *off
021500090623     c                   When      *in99 = *on  or not *in11
021600090623     c                   eval      VIDPRE = 'QPRINT    '
021700030612     c                   Other
021800030612     C                   GOTO      FOR01
021900030612     c                   EndSl
022000030612      *
022100030609      *
022200030609      * BOLLE/FATTURE
022300080709      * Stampante OLD
022400080709     c
022500940928     C                   MOVEA     VIDPRB        CM(12)                         BOLLE
022600030609     C                   MOVEA     CM            COMMAN
022700940928     C                   CALL      'QCMDEXC'                            40
022800940928     C                   PARM                    COMMAN
022900940928     C                   PARM                    LUNG
023000030612      *
023100030612     c                   Select
023200030612     c                   When      *in40 = *off
023300090623     c                   When      *in99 = *on or not *in10 or not *in23
023400090623     c                   eval      VIDPRB = 'QPRINT    '
023500030612     c                   Other
023600030612     C                   GOTO      FOR01
023700080709     c                   EndSl
023800030609      *
023900080709      * Stampante A4
024000030609     C                   MOVEA     VIDPRB4       CM(12)                         BOLLE
024100030609     C                   MOVEA     CM            COMMAN
024200030611     C                   CALL      'QCMDEXC'                            44
024300030609     C                   PARM                    COMMAN
024400030609     C                   PARM                    LUNG
024500030612      *
024600030612     c                   Select
024700030612     c                   When      *in44 = *off
024800090623     c                   When      *in99 = *on or not *in10 or not *in21
024900090623     c                   eval      VIDPRB4 = 'QPRINT    '
025000030612     c                   Other
025100030612     C                   GOTO      FOR01
025200030612     c                   EndSl
025300030612      *
025400080709      * Stampante A5
025500030609     C                   MOVEA     VIDPRB5       CM(12)                         BOLLE
025600030609     C                   MOVEA     CM            COMMAN
025700030612     C                   CALL      'QCMDEXC'                            45
025800030609     C                   PARM                    COMMAN
025900030609     C                   PARM                    LUNG
026000030612      *
026100030612     c                   Select
026200030612     c                   When      *in45 = *off
026300090623     c                   When      *in99 = *on or not *in10 or not *in22
026400090623     c                   eval      VIDPRB5 = 'QPRINT    '
026500030612     c                   Other
026600030612     C                   GOTO      FOR01
026700030612     c                   EndSl
026800030612      *
026900030609      *
027000030609      * Tabulati
027100940928     C                   MOVEA     VIDPRT        CM(12)                         TABULATI
027200030609     C                   MOVEA     CM            COMMAN
027300940928     C                   CALL      'QCMDEXC'                            42
027400940928     C                   PARM                    COMMAN
027500940928     C                   PARM                    LUNG
027600030612      *
027700030612     c                   Select
027800030612     c                   When      *in42 = *off
027900090623     c                   When      *in99 = *on or not *in12
028000090623     c                   eval      VIDPRT = 'QPRINT    '
028100030612     c                   Other
028200030612     C                   GOTO      FOR01
028300030612     c                   EndSl
028400030609      *
028500010314      * O.R.M.
028600010314     C                   MOVEA     VIDPRO        CM(12)                         O.R.M.
028700030609     C                   MOVEA     CM            COMMAN
028800010314     C                   CALL      'QCMDEXC'                            43
028900010314     C                   PARM                    COMMAN
029000010314     C                   PARM                    LUNG
029100030612      *
029200030612     c                   Select
029300030612     c                   When      *in43 = *off
029400090623     c                   When      *in99 = *on  or not *in13
029500131031     c                   eval      VIDPRO = 'QPRINT    '
029600030612     c                   Other
029700030612     C                   GOTO      FOR01
029800030612     c                   EndSl
029900030612      *
030000030609      *
030100030609      * IMPOSTO I MODULI
030200030609      *
030300030609      * Etichette
030400030609    2C     *IN11         IFEQ      *ON                                          ETICHETTE
030500030609      *
030600030610    3C     d90MDE        IFEQ      *BLANKS
030700030610     C                   MOVEL     'ETICH'       d90MDE
030800030609    3C                   ENDIF
030900030609      * VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
031000030609    4C     SIMTPP        IFEQ      '2'
031100020503     C     SIMTPP        OREQ      *BLANKS
031200030610     C                   MOVE      WFIL          d90MDE
031300030609    4C                   ENDIF
031400030610     C                   MOVEL     VIDPRE        d90PRE
031500970701     C*****************************************************************
031600970701     C* VARIATA STAMPANTE ETICHETTE RISPETTO AL DEFAULT:
031700041203     c*
031800970630    3C     VIDPRE        IFNE      W5PDFT
031900040316     c* as unico
032000040316     c                   eval      inizio=%subst(vidpre:2:3)
032100040316     c                   eval      fine=%subst(vidpre:8:3)
032200040316     C                   MOVEL(p)  stampante     KKEY
032300040316     c* CHAIN STAMPANTE
032400970630     C                   MOVEL     '5P'          KCOD
032500970630     C     KTAB          CHAIN     TABEL00F                           30
032600970630    4C     *IN30         IFEQ      *OFF
032700970630     C                   MOVEL     TBLUNI        DS5P
032800970630     C                   MOVEL     §5PMOD        W5PMOD
032900970701   x4C                   ELSE
033000040316     c
033100970701     C* SE NON HO TROVATO IL RECORD RICHIESTO CERCO IL DEFAULT
033200970701     C                   MOVE      '    '        KKEY
033300040316     c
033400970701     C     KTAB          CHAIN     TABEL00F                           30
033500970701    5C     *IN30         IFEQ      *OFF
033600970701     C                   MOVEL     TBLUNI        DS5P
033700970701     C                   MOVEL     §5PMOD        W5PMOD
033800970701    5C                   END
033900970630    4C                   END
034000970630    3C                   END
034100970701     C*****************************************************************
034200970701     C* COL MODELLO PRENDO, SEMPRE DALLA "5P", IL NOME DEI PGM DI
034300970701     C* DI STAMPA ETICHETTE
034400970630     C                   MOVEL     'M'           W009A             9
034500970630     C                   MOVE      W5PMOD        W009A
034600970701     C                   MOVEL     W009A         KKEY
034700970630     C                   MOVEL     '5P'          KCOD
034800970701     C     KTAB          CHAIN     TABEL00F                           30
034900970701    3C     *IN30         IFEQ      *OFF
035000970630     C                   MOVEL     TBLUNI        DS5PM
035100970630     C* IMPOSTO PGM DI STAMPA ETICHETTE
035200030610     C                   MOVEL     §5PPSS        d90PSS
035300030610     C                   MOVEL     §5PPSB        d90PSB
035400030610     C                   MOVEL     §5PPSD        d90PSD
035500030610     C                   MOVEL     §5PPSF        d90PSF
035600970701    3C                   END
035700030609    2C                   ENDIF
035800030609      *
035900030609      * BOLLE/FATTURE
036000030609    2c                   IF        *IN10 = *ON
036100030609      *
036200030610     c                   If        d90MDB = *blanks
036300030610     c                   eval      d90MDB = 'BOLLE' + wfilA
036400030609     c                   Else
036500030610     c                   eval      %subst(d90MDB:8:3) = wfilA
036600030609     c                   EndIf
036700030609      *
036800030610     c                   If        d90MDB4 = *blanks
036900030610     c                   eval      d90MDB4 = 'BOLLE' + wfilA +'A4'
037000030609     c                   Else
037100030612     c                   eval      %subst(d90MDB4:6:5) = wfilA + 'A4'
037200030609     c                   EndIf
037300030609      *
037400030610     c                   If        d90MDB5 = *blanks
037500030610     c                   eval      d90MDB5 = 'BOLLE' + wfilA +'A5'
037600030609     c                   Else
037700030612     c                   eval      %subst(d90MDB5:6:5) = wfilA + 'A5'
037800030609     c                   EndIf
037900030609      *
038000030610     C                   MOVEL     VIDPRB        d90PRB
038100030610     C                   MOVEL     VIDPRB4       d90PRB4
038200030610     C                   MOVEL     VIDPRB5       d90PRB5
038300030610     C                   MOVEL     VIDBTS        d90BSL
038400030609      * Pgm di stampa
038500030609     C                   SELECT
038600030610     c                   WHEN      d90BSL = 'S' and §5PBPL <> *blanks
038700030610     C                   movel     §5pbpl        d90PSL
038800030610     c                   WHEN      d90BSL = 'S'
038900030610     C                   movel     'FNLSB5R'     d90PSL
039000030610     c                   WHEN      d90BSL = *blank and §5PBPG <> *blanks
039100030610     C                   movel     §5pbpg        d90PSL
039200030609     c                   OTHER
039300030610     C                   movel     'FNLSB0R'     d90PSL
039400030609     c                   ENDSL
039500030609      *
039600030609    2C                   ENDIF
039700030609      *
039800030609      * Tabulati
039900030609    2C     *IN12         IFEQ      *ON                                          TABULATI
040000030609      *
040100030610    3C     d90MDT        IFEQ      *BLANKS
040200940928     C* VEDO QUANTE COPIE SONO
040300030610    4C     d90NCP        IFLE      '1'
040400030610     C                   MOVEL     '*STD '       d90MDT
040500940928     C                   ELSE
040600030610     C                   MOVEL     '  COPIE'     d90MDT
040700030610     C                   MOVEL     d90NCP        d90MDT
040800030609    4C                   ENDIF
040900030609    3C                   ENDIF
041000940928     C* VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
041100030609    3C     SIMTPP        IFEQ      '2'
041200020503     C     SIMTPP        OREQ      *BLANKS
041300030610     C                   MOVE      WFIL          d90MDT
041400030609    3C                   ENDIF
041500030609      *
041600030610     C                   MOVEL     VIDPRT        d90PRT
041700030609    2C                   ENDIF
041800010314     C*
041900010314      * O.R.M.
042000030609    2C     *IN13         IFEQ      *ON                                          O.R.M.
042100030609      *
042200030610     C     d90MDO        IFEQ      *BLANKS
042300040316     C                   movel     Wfil          W003A             3
042400110427     c                   eval      d90mdo = 'ORMA4_' + W003A
042500010314     C                   ENDIF
042600030609      *
042700030610     C                   MOVEL     VIDPRO        d90PRO
042800030609    2C                   ENDIF
042900030610      *
043000030609    1C                   ENDIF
043100030609      *
043200030612      *  se non ho emesso il video e il pgm di stampa LDV non è impostato
043300030612      *  forzo il pgm di stamp asu modulo "vecchio"
043400030612     c                   IF        *in99 = *on and D90PSL = *blanks
043500030612     c                   eval      D90PSL = 'FNLSB0R'
043600030612     c                   eval      D90BSL = *blanks
043700030612     c                   ENDIF
043800030612      *
043900030609     C                   EVAL      *INLR = *ON
044000030609      *---------------------------------------------------------------------
044100030609      * SBR INIZIALE
044200030609      *---------------------------------------------------------------------
044300030609     c     *inzsr        begsr
044400030609      *
044500030609     C     *ENTRY        PLIST
044600030609     C                   PARM                    KPJBA
044700030610     C                   PARM                    TRUL90DS
044800131023     C                   PARM                    par_key
044900030609     C*
045000030609     C     KTAB          KLIST
045100030609     C                   KFLD                    CODUT
045200030609     C                   KFLD                    KCOD
045300030609     C                   KFLD                    KKEY
045400030609     C*
045500030610     C                   CLEAR                   d90F3
045600030610     C                   CLEAR                   d90PRE
045700030610     C                   CLEAR                   d90PRB
045800030610     C                   CLEAR                   d90PRT
045900030610     C                   CLEAR                   d90PRO                         O.R.M.
046000030610     C                   CLEAR                   d90PSS
046100030610     C                   CLEAR                   d90PSB
046200030610     C                   CLEAR                   d90PSD
046300030610     C                   CLEAR                   d90PSF
046400030610     C                   CLEAR                   d90BSL
046500030610     C                   CLEAR                   d90PSL
046600030610     C                   CLEAR                   d90PRB4
046700030610     C                   CLEAR                   d90PRB5
046800030609     C*
046900030609     C* PRENDO DATI DELL'AZIENDA
047000030609     C                   Z-ADD     1             CODUT
047100030609     C                   CALL      'X§PARUT'
047200030609     C                   PARM                    UT§DSE0F
047300030609     C                   MOVEL     RAGUT         VIDDSA
047400030609     C                   MOVEL     REC80         CNCR80
047500131023     c
047600131023     c                   if        %parms=3
047700131023     c                   eval      k_key=par_key
047800131023     c                   else
047900131023     c                   clear                   k_key
048000131023     c                   endif
048100030609      *
048200030609     c                   endsr
048300030609      *---------------------------------------------------------------------
048400940928**
048500940928CHKOBJ OBJ(XXXXXXXXXX) OBJTYPE(*OUTQ)
