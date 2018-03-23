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
007300140124     c                   exsr      Impokey
007400030609      *
007500030609     C* Stampante bolle/fatture
007600030609     C                   MOVEL     'B'           W004A             4
007700030609     C                   MOVEL     '5P'          KCOD
007800030609     C                   MOVEL(P)  W004A         KKEY
007900030609     C     KTAB          CHAIN     TABEL00F                           30
008000140124     c
008100140124     c* se non trovato e passato SED richaino con SED
008200140124     c                   if        *in30 and k_key='SED'
008300140124     c                   eval      %subst(kkey:2:3)='SED'
008400140124     C     KTAB          CHAIN     TABEL00F                           30
008500140124     c                   endif
008600140124     c
008700030609    1C     *IN30         IFEQ      *OFF
008800030609     C                   MOVEL     TBLUNI        DS5PB
008900030609     C                   MOVEL     §5PBOL        VIDPRB
009000030610     C                   MOVEL     §5PBA4        VIDPRB4
009100030610     C                   MOVEL     §5PBA5        VIDPRB5
009200080709     c*  Se passata una 'O' significa che voglio solo la stampante vecchia
009300080709    2c                   if        d90rsb<>'O' and §5pbus<>'O'
009400080709    3C                   if        §5PBTS = 'S' or  §5PBTS = 'X'
009500030612     C                                          or  §5PBTS = d90rsb
009600080709     c                   eval      *in21 = *on
009700080709     c                   eval      *in22 = *on
009800030609     C                   MOVEL     'S'           VIDBTS
009900080709     c* Se §5pbus='4' --> solo stampante per moduli in A4
010000080709    4c                   if        §5pbus='4'
010100080709     c                   eval      *in20 =*off
010200080709     c                   eval      *in22 =*off
010300080709    4C                   endif
010400080709     c* Se §5pbus='5' --> solo stampante per moduli in A5
010500080709    4c                   if        §5pbus='5'
010600080709     c                   eval      *in20 =*off
010700080709     c                   eval      *in21 =*off
010800080709    4C                   endif
010900080709    3C                   END
011000080709     c                   else
011100080709     c* solo stampate old
011200080709     c                   eval      *in23=*on
011300080709    2C                   END
011400080709    1C                   END
011500030609      *
011600030609      * Stampante etichette
011700030609     C                   MOVEL     'S'           W004A             4
011800970630     C                   MOVE      *BLANKS       W5PMOD
011900970630     C                   MOVE      *BLANKS       W5PDFT
012000970630     C                   MOVEL     '5P'          KCOD
012100970630     C                   MOVEL(P)  W004A         KKEY
012200970630     C     KTAB          CHAIN     TABEL00F                           30
012300140124
012400140124     c* se non trovato e passato SED richaino con SED
012500140124     c                   if        *in30 and k_key='SED'
012600140124     c                   eval      %subst(kkey:2:3)='SED'
012700140124     C     KTAB          CHAIN     TABEL00F                           30
012800140124     c                   endif
012900140124
013000970630    1C     *IN30         IFEQ      *OFF
013100970630     C                   MOVEL     TBLUNI        DS5P
013200970630     C                   MOVEL     §5POTQ        VIDPRE
013300970630     C                   MOVEL     §5POTQ        W5PDFT
013400970630     C                   MOVE      §5PMOD        W5PMOD
013500970630    1C                   END
013600030609      *
013700970630     C* Stampante tabulati
013800030609     C                   MOVEL     'T'           W004A             4
013900970630     C                   MOVEL     '5P'          KCOD
014000970630     C                   MOVEL(P)  W004A         KKEY
014100970630     C     KTAB          CHAIN     TABEL00F                           30
014200140124
014300140124     c* se non trovato e passato SED richaino con SED
014400140124     c                   if        *in30 and k_key='SED'
014500140124     c                   eval      %subst(kkey:2:3)='SED'
014600140124     C     KTAB          CHAIN     TABEL00F                           30
014700140124     c                   endif
014800140124
014900970630    1C     *IN30         IFEQ      *OFF
015000970630     C                   MOVEL     TBLUNI        DS5P
015100970630     C                   MOVEL     §5POTQ        VIDPRT
015200970630    1C                   END
015300030609      *
015400010314     C* Stampante O.R.M.
015500030609     C                   MOVEL     'O'           W004A             4
015600010314     C                   MOVEL     '5P'          KCOD
015700010314     C                   MOVEL(P)  W004A         KKEY
015800010314     C     KTAB          CHAIN     TABEL00F                           30
015900140124
016000140124     c* se non trovato e passato SED richaino con SED
016100140124     c                   if        *in30 and k_key='SED'
016200140124     c                   eval      %subst(kkey:2:3)='SED'
016300140124     C     KTAB          CHAIN     TABEL00F                           30
016400140124     c                   endif
016500140124
016600010314    1C     *IN30         IFEQ      *OFF
016700010314     C                   MOVEL     TBLUNI        DS5P
016800010314     C                   MOVEL     §5POTQ        VIDPRO
016900010314    1C                   END
017000030612      *
017100940928     C     FOR01         TAG
017200030612      *
017300030612     C                   IF        *IN99 = *off
017400030612     C                   EXFMT     UL90d01
017500030612     c                   SETOFF                                       404142
017600030612     c                   SETOFF                                       434445
017700030612     c                   ENDIF
017800030612      *
017900030612      * Se emetto il video controllo che il primo carattere delle
018000030612      * stampanti non sia blank
018100030609     C                   SELECT
018200030612     c                   WHEN      *in99 = *on
018300090623     c                   WHEN      *in11 and VIDpre <> *blanks and
018400030609     c                             %subst(vidpre:1:1) = ' '
018500030609     C                   eval      *in41 = *on
018600030612     c                   GOTO      FOR01
018700090623     c                   WHEN      *in10 and *in23 and VIDprb <> *blanks and
018800030609     c                             %subst(vidprb:1:1) = ' '
018900030609     C                   eval      *in40 = *on
019000030612     c                   GOTO      FOR01
019100090623     c                   WHEN      *in10 and *in21 and VIDprb4 <> *blanks and
019200030609     c                             %subst(vidprb4:1:1) = ' '
019300030609     C                   eval      *in44 = *on
019400030612     c                   GOTO      FOR01
019500090623     c                   WHEN      *in10 and *in22 and VIDprb5 <> *blanks and
019600030609     c                             %subst(vidprb5:1:1) = ' '
019700030609     C                   eval      *in45 = *on
019800030612     c                   GOTO      FOR01
019900090623     c                   WHEN      *in12 and VIDprt <> *blanks and
020000030609     c                             %subst(vidprT:1:1) = ' '
020100030609     C                   eval      *in42 = *on
020200030612     c                   GOTO      FOR01
020300090623     c                   WHEN      *in13 and VIDpro <> *blanks and
020400030609     c                             %subst(vidpro:1:1) = ' '
020500030609     C                   eval      *in43 = *on
020600030612     c                   GOTO      FOR01
020700030609     C                   ENDSL
020800030612      *
020900030609      * F3 FINE
021000030609    1C     *INKC         IFEQ      *ON
021100030610     C                   MOVEL     '1'           d90F3
021200030609   x1C                   ELSE
021300030609      *
021400030609      * CONTROLLO LA ESISTENZA DELLE STAMPANTI
021500090623      * Se non trovata e non emetto il video
021600090623      *  o non serve --> forzo  QPRINT
021700030609      *
021800030609      * Etichette
021900940928     C                   MOVEA     VIDPRE        CM(12)
022000030609     C                   MOVEA     CM            COMMAN
022100940928     C                   CALL      'QCMDEXC'                            41
022200940928     C                   PARM                    COMMAN
022300940928     C                   PARM                    LUNG
022400030612      *
022500030612     c                   Select
022600030612     c                   When      *in41 = *off
022700090623     c                   When      *in99 = *on  or not *in11
022800090623     c                   eval      VIDPRE = 'QPRINT    '
022900030612     c                   Other
023000030612     C                   GOTO      FOR01
023100030612     c                   EndSl
023200030612      *
023300030609      *
023400030609      * BOLLE/FATTURE
023500080709      * Stampante OLD
023600080709     c
023700940928     C                   MOVEA     VIDPRB        CM(12)                         BOLLE
023800030609     C                   MOVEA     CM            COMMAN
023900940928     C                   CALL      'QCMDEXC'                            40
024000940928     C                   PARM                    COMMAN
024100940928     C                   PARM                    LUNG
024200030612      *
024300030612     c                   Select
024400030612     c                   When      *in40 = *off
024500090623     c                   When      *in99 = *on or not *in10 or not *in23
024600090623     c                   eval      VIDPRB = 'QPRINT    '
024700030612     c                   Other
024800030612     C                   GOTO      FOR01
024900080709     c                   EndSl
025000030609      *
025100080709      * Stampante A4
025200030609     C                   MOVEA     VIDPRB4       CM(12)                         BOLLE
025300030609     C                   MOVEA     CM            COMMAN
025400030611     C                   CALL      'QCMDEXC'                            44
025500030609     C                   PARM                    COMMAN
025600030609     C                   PARM                    LUNG
025700030612      *
025800030612     c                   Select
025900030612     c                   When      *in44 = *off
026000090623     c                   When      *in99 = *on or not *in10 or not *in21
026100090623     c                   eval      VIDPRB4 = 'QPRINT    '
026200030612     c                   Other
026300030612     C                   GOTO      FOR01
026400030612     c                   EndSl
026500030612      *
026600080709      * Stampante A5
026700030609     C                   MOVEA     VIDPRB5       CM(12)                         BOLLE
026800030609     C                   MOVEA     CM            COMMAN
026900030612     C                   CALL      'QCMDEXC'                            45
027000030609     C                   PARM                    COMMAN
027100030609     C                   PARM                    LUNG
027200030612      *
027300030612     c                   Select
027400030612     c                   When      *in45 = *off
027500090623     c                   When      *in99 = *on or not *in10 or not *in22
027600090623     c                   eval      VIDPRB5 = 'QPRINT    '
027700030612     c                   Other
027800030612     C                   GOTO      FOR01
027900030612     c                   EndSl
028000030612      *
028100030609      *
028200030609      * Tabulati
028300940928     C                   MOVEA     VIDPRT        CM(12)                         TABULATI
028400030609     C                   MOVEA     CM            COMMAN
028500940928     C                   CALL      'QCMDEXC'                            42
028600940928     C                   PARM                    COMMAN
028700940928     C                   PARM                    LUNG
028800030612      *
028900030612     c                   Select
029000030612     c                   When      *in42 = *off
029100090623     c                   When      *in99 = *on or not *in12
029200090623     c                   eval      VIDPRT = 'QPRINT    '
029300030612     c                   Other
029400030612     C                   GOTO      FOR01
029500030612     c                   EndSl
029600030609      *
029700010314      * O.R.M.
029800010314     C                   MOVEA     VIDPRO        CM(12)                         O.R.M.
029900030609     C                   MOVEA     CM            COMMAN
030000010314     C                   CALL      'QCMDEXC'                            43
030100010314     C                   PARM                    COMMAN
030200010314     C                   PARM                    LUNG
030300030612      *
030400030612     c                   Select
030500030612     c                   When      *in43 = *off
030600090623     c                   When      *in99 = *on  or not *in13
030700131031     c                   eval      VIDPRO = 'QPRINT    '
030800030612     c                   Other
030900030612     C                   GOTO      FOR01
031000030612     c                   EndSl
031100030612      *
031200030609      *
031300030609      * IMPOSTO I MODULI
031400030609      *
031500030609      * Etichette
031600030609    2C     *IN11         IFEQ      *ON                                          ETICHETTE
031700030609      *
031800030610    3C     d90MDE        IFEQ      *BLANKS
031900030610     C                   MOVEL     'ETICH'       d90MDE
032000030609    3C                   ENDIF
032100030609      * VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
032200030609    4C     SIMTPP        IFEQ      '2'
032300020503     C     SIMTPP        OREQ      *BLANKS
032400030610     C                   MOVE      WFIL          d90MDE
032500030609    4C                   ENDIF
032600030610     C                   MOVEL     VIDPRE        d90PRE
032700970701     C*****************************************************************
032800970701     C* VARIATA STAMPANTE ETICHETTE RISPETTO AL DEFAULT:
032900041203     c*
033000970630    3C     VIDPRE        IFNE      W5PDFT
033100040316     c* as unico
033200040316     c                   eval      inizio=%subst(vidpre:2:3)
033300040316     c                   eval      fine=%subst(vidpre:8:3)
033400040316     C                   MOVEL(p)  stampante     KKEY
033500040316     c* CHAIN STAMPANTE
033600970630     C                   MOVEL     '5P'          KCOD
033700970630     C     KTAB          CHAIN     TABEL00F                           30
033800970630    4C     *IN30         IFEQ      *OFF
033900970630     C                   MOVEL     TBLUNI        DS5P
034000970630     C                   MOVEL     §5PMOD        W5PMOD
034100970701   x4C                   ELSE
034200040316     c
034300970701     C* SE NON HO TROVATO IL RECORD RICHIESTO CERCO IL DEFAULT
034400970701     C                   MOVE      '    '        KKEY
034500040316     c
034600970701     C     KTAB          CHAIN     TABEL00F                           30
034700970701    5C     *IN30         IFEQ      *OFF
034800970701     C                   MOVEL     TBLUNI        DS5P
034900970701     C                   MOVEL     §5PMOD        W5PMOD
035000970701    5C                   END
035100970630    4C                   END
035200970630    3C                   END
035300970701     C*****************************************************************
035400970701     C* COL MODELLO PRENDO, SEMPRE DALLA "5P", IL NOME DEI PGM DI
035500970701     C* DI STAMPA ETICHETTE
035600970630     C                   MOVEL     'M'           W009A             9
035700970630     C                   MOVE      W5PMOD        W009A
035800970701     C                   MOVEL     W009A         KKEY
035900970630     C                   MOVEL     '5P'          KCOD
036000970701     C     KTAB          CHAIN     TABEL00F                           30
036100970701    3C     *IN30         IFEQ      *OFF
036200970630     C                   MOVEL     TBLUNI        DS5PM
036300970630     C* IMPOSTO PGM DI STAMPA ETICHETTE
036400030610     C                   MOVEL     §5PPSS        d90PSS
036500030610     C                   MOVEL     §5PPSB        d90PSB
036600030610     C                   MOVEL     §5PPSD        d90PSD
036700030610     C                   MOVEL     §5PPSF        d90PSF
036800970701    3C                   END
036900030609    2C                   ENDIF
037000030609      *
037100030609      * BOLLE/FATTURE
037200030609    2c                   IF        *IN10 = *ON
037300030609      *
037400030610     c                   If        d90MDB = *blanks
037500030610     c                   eval      d90MDB = 'BOLLE' + wfilA
037600030609     c                   Else
037700030610     c                   eval      %subst(d90MDB:8:3) = wfilA
037800030609     c                   EndIf
037900030609      *
038000030610     c                   If        d90MDB4 = *blanks
038100030610     c                   eval      d90MDB4 = 'BOLLE' + wfilA +'A4'
038200030609     c                   Else
038300030612     c                   eval      %subst(d90MDB4:6:5) = wfilA + 'A4'
038400030609     c                   EndIf
038500030609      *
038600030610     c                   If        d90MDB5 = *blanks
038700030610     c                   eval      d90MDB5 = 'BOLLE' + wfilA +'A5'
038800030609     c                   Else
038900030612     c                   eval      %subst(d90MDB5:6:5) = wfilA + 'A5'
039000030609     c                   EndIf
039100030609      *
039200030610     C                   MOVEL     VIDPRB        d90PRB
039300030610     C                   MOVEL     VIDPRB4       d90PRB4
039400030610     C                   MOVEL     VIDPRB5       d90PRB5
039500030610     C                   MOVEL     VIDBTS        d90BSL
039600030609      * Pgm di stampa
039700030609     C                   SELECT
039800030610     c                   WHEN      d90BSL = 'S' and §5PBPL <> *blanks
039900030610     C                   movel     §5pbpl        d90PSL
040000030610     c                   WHEN      d90BSL = 'S'
040100030610     C                   movel     'FNLSB5R'     d90PSL
040200030610     c                   WHEN      d90BSL = *blank and §5PBPG <> *blanks
040300030610     C                   movel     §5pbpg        d90PSL
040400030609     c                   OTHER
040500030610     C                   movel     'FNLSB0R'     d90PSL
040600030609     c                   ENDSL
040700030609      *
040800030609    2C                   ENDIF
040900030609      *
041000030609      * Tabulati
041100030609    2C     *IN12         IFEQ      *ON                                          TABULATI
041200030609      *
041300030610    3C     d90MDT        IFEQ      *BLANKS
041400940928     C* VEDO QUANTE COPIE SONO
041500030610    4C     d90NCP        IFLE      '1'
041600030610     C                   MOVEL     '*STD '       d90MDT
041700940928     C                   ELSE
041800030610     C                   MOVEL     '  COPIE'     d90MDT
041900030610     C                   MOVEL     d90NCP        d90MDT
042000030609    4C                   ENDIF
042100030609    3C                   ENDIF
042200940928     C* VEDO SE IMPOSTARE O MENO LA FILIALE NEL MODULO
042300030609    3C     SIMTPP        IFEQ      '2'
042400020503     C     SIMTPP        OREQ      *BLANKS
042500030610     C                   MOVE      WFIL          d90MDT
042600030609    3C                   ENDIF
042700030609      *
042800030610     C                   MOVEL     VIDPRT        d90PRT
042900030609    2C                   ENDIF
043000010314     C*
043100010314      * O.R.M.
043200030609    2C     *IN13         IFEQ      *ON                                          O.R.M.
043300030609      *
043400030610     C     d90MDO        IFEQ      *BLANKS
043500040316     C                   movel     Wfil          W003A             3
043600110427     c                   eval      d90mdo = 'ORMA4_' + W003A
043700010314     C                   ENDIF
043800030609      *
043900030610     C                   MOVEL     VIDPRO        d90PRO
044000030609    2C                   ENDIF
044100030610      *
044200030609    1C                   ENDIF
044300030609      *
044400030612      *  se non ho emesso il video e il pgm di stampa LDV non è impostato
044500030612      *  forzo il pgm di stamp asu modulo "vecchio"
044600030612     c                   IF        *in99 = *on and D90PSL = *blanks
044700030612     c                   eval      D90PSL = 'FNLSB0R'
044800030612     c                   eval      D90BSL = *blanks
044900030612     c                   ENDIF
045000030612      *
045100030609     C                   EVAL      *INLR = *ON
045200030609      *---------------------------------------------------------------------
045300030609      * SBR INIZIALE
045400030609      *---------------------------------------------------------------------
045500030609     c     *inzsr        begsr
045600030609      *
045700030609     C     *ENTRY        PLIST
045800030609     C                   PARM                    KPJBA
045900030610     C                   PARM                    TRUL90DS
046000131023     C                   PARM                    par_key
046100030609     C*
046200030609     C     KTAB          KLIST
046300030609     C                   KFLD                    CODUT
046400030609     C                   KFLD                    KCOD
046500030609     C                   KFLD                    KKEY
046600030609     C*
046700030610     C                   CLEAR                   d90F3
046800030610     C                   CLEAR                   d90PRE
046900030610     C                   CLEAR                   d90PRB
047000030610     C                   CLEAR                   d90PRT
047100030610     C                   CLEAR                   d90PRO                         O.R.M.
047200030610     C                   CLEAR                   d90PSS
047300030610     C                   CLEAR                   d90PSB
047400030610     C                   CLEAR                   d90PSD
047500030610     C                   CLEAR                   d90PSF
047600030610     C                   CLEAR                   d90BSL
047700030610     C                   CLEAR                   d90PSL
047800030610     C                   CLEAR                   d90PRB4
047900030610     C                   CLEAR                   d90PRB5
048000030609     C*
048100030609     C* PRENDO DATI DELL'AZIENDA
048200030609     C                   Z-ADD     1             CODUT
048300030609     C                   CALL      'X§PARUT'
048400030609     C                   PARM                    UT§DSE0F
048500030609     C                   MOVEL     RAGUT         VIDDSA
048600030609     C                   MOVEL     REC80         CNCR80
048700131023     c
048800131023     c                   if        %parms=3
048900131023     c                   eval      k_key=par_key
049000131023     c                   else
049100131023     c                   clear                   k_key
049200131023     c                   endif
049300030609      *
049400030609     c                   endsr
049500140124      *---------------------------------------------------------------------
049600140124     c     Impokey       BEGSR
049700140124     c                   clear                   w004a
049800140124      *
049900140124     c                   select
050000140124     c* se passato "SED" faccio ulteriore chain con le prime 3 lettere del profilo,
050100140124     c*  per cercare una key di sede più specifica
050200140203     c*  Fa eccezione EDP . Visto che EDP è il nostro generico, per i profili EDP elaboro con CED
050300140124     c                   when      k_key= 'SED'
050400140124     C                   MOVE      rem           W004A
050500140124     c                   if        rem='EDP'
050600140203     c                   move      'CED'         w004a
050700140124     c                   endif
050800140124     c
050900140124     c                   when      k_key<>*blanks
051000140124     C                   MOVE      k_key         W004A
051100140124     c
051200140124      * Imposto codice p.o. o EDP per utenti EDPxxxxx
051300140124     c                   when      rem='EDP'
051400140124     C                   MOVE      'EDP'         W004A
051500140124
051600140124     c                   other
051700140124     C                   MOVE      WFIL          W004A
051800140124     c                   endsl
051900140124
052000140124
052100140124    1C**   REM           IFEQ      'EDP'
052200140124     C**                 MOVE      'EDP'         W004A
052300140124   X1C**                 ELSE
052400140124     C**                 MOVE      WFIL          W004A
052500140124    1C**                 END
052600140124     c                   ENDSR
052700030609      *---------------------------------------------------------------------
052800940928**
052900940928CHKOBJ OBJ(XXXXXXXXXX) OBJTYPE(*OUTQ)
