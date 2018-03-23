000100020513     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020513     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400020403     H/TITLE  contabilizzazione conteggi
000500951002?     *--------------------------------------------------------------*
000600020513      * FICN61R                                                      *
000700951002      *     *----------------------------------------------*         *
000800030828      *         Gestione tabella BLC/BLO/BLD                         *
000900020510      *     *----------------------------------------------*         *
001000951002      *                                                              *
001100951002?     *--------------------------------------------------------------*
001200020513     Ftntbe01l  uF a E           K DISK
001300170120     fTNTBEx1L  Uf A e           k disk    rename(tntbe000:tntbex) prefix(x)
001400170120      *
001401171117     Fantxt01l  iF   E           K DISK
001500020513     Fficn61D   CF   E             WORKSTN
001600951002     D*---------------------------------------------------------------*
001700980910     D* Reperimento nome PGM
001800020510     D                SDS
001900020510     D  nompgm           *PROC
002000170120      *-
002100170120      * Tracciato record file TNTBE00F
002200170120     d TNTBEds       e ds                  extname(TNTBE00F) inz
002300170120     d xTNTBEds      e ds                  extname(TNTBEx1l) inz
002400170120     d                                     prefix(x)
002500170120      *-
002600951002     D KPJBA         E DS
002700020822     D dblc          E DS                  inz
002800030211     D dblo          E DS                  inz
002900030828     D dbld          E DS                  inz
003000060704     D dblt          E DS                  inz
003100020513     D soc001        E DS                  EXTNAME(xsoc001ds)
003200020513     D xsocds          DS          1000
003300020513     Ddataiso                          d   datfmt(*iso)
003400020513     Ddataeur                          d   datfmt(*eur)
003500951002?     *--------------------------------------------------------------*
003600951002?     *  CICLO PRINCIPALE                                            *
003700951002?     *--------------------------------------------------------------*
003800951002      *  Impostazione parametri prima videata
003900951002     C                   EXSR      INZ01
004000951002      *  Loop gestione videata
004100951002     C     WFINE         DOUEQ     'S'
004200951002     C                   EXSR      GESD01
004300951002     C                   END
004400951002      *  Fine Lavoro
004500951002     C     FINE          TAG
004600951002     C                   SETON                                        LR
004700951002?     *--------------------------------------------------------------*
004800951002?     *  INZ01: Inizializzazione prima videata                       *
004900951002?     *--------------------------------------------------------------*
005000951002     C     INZ01         BEGSR
005100951002      *
005200020513     C                   SETOFF                                       99
005300020513     c     ktbe          chain     tntbe01l
005400020513     c                   if        %found(tntbe01l)
005500030828     c                   select
005600030828     c                   when      tipo = 'A'
005700020513     c                   movel     tbeuni        dblc
005800030828     c                   when      tipo = 'C'
005900030211     c                   movel     tbeuni        dblo
006000030828     c                   when      tipo = 'D'
006100030828     c                   movel     tbeuni        dbld
006200060704     c                   when      tipo = 'T'
006300060704     c                   movel     tbeuni        dblt
006400030828     c                   endsl
006500020513     c                   else
006600030828     c                   select
006700030828     c                   when      tipo = 'A'
006800020513     c                   clear                   dblc
006900030828     c                   when      tipo = 'C'
007000030211     c                   clear                   dblo
007100030828     c                   when      tipo = 'D'
007200030828     c                   clear                   dbld
007300060704     c                   when      tipo = 'T'
007400060704     c                   clear                   dblt
007500030828     c                   endsl
007600030211     c                   end
007700030828     c                   select
007800030828     c                   when      tipo = 'A'
007900020513     c                   movel     §blcdtp       dataiso
008000020513     c                   movel     dataiso       dataeur
008100020513     c                   movel     dataeur       dtprov
008200020513     c                   movel     §blcdbl       dataiso
008300020513     c                   movel     dataiso       dataeur
008400020513     c                   movel     dataeur       dtblcv
008500021111     c                   z-add     §blcmax       impmax
008600040914     c                   z-add     §blcmin       impmin
008700021111     c                   movel     §blccau       cauret
008701171117     c                   movel     §blcauto      cauto
008702171117     c                   movel     §blccorpo     ccorpo
008703171117     c                   movel     §bLcottico    cottico
008704171117     c                   movel     §blcsoc       csoc
008705171117     c                   movel     §blccdtes     ccdtes
008800020822     c                   if        §blcsav <> 0
008900020822     c                   movel     §blcsav       dataiso
009000020822     c                   movel     dataiso       dataeur
009100020822     c                   movel     dataeur       dtsavv
009200020822     c                   end
009300020822     c                   if        §blcclr <> 0
009400020822     c                   movel     §blcclr       dataiso
009500020822     c                   movel     dataiso       dataeur
009600020822     c                   movel     dataeur       dtclrv
009700020822     c                   end
009800030828     c                   when      tipo = 'C'
009900030211     c                   movel     §blodtp       dataiso
010000030211     c                   movel     dataiso       dataeur
010100030211     c                   movel     dataeur       dtprov
010200030211     c                   movel     §blodbl       dataiso
010300030211     c                   movel     dataiso       dataeur
010400030211     c                   movel     dataeur       dtblcv
010500030211     c                   z-add     §blomax       impmax
010600040914     c                   z-add     §blomin       impmin
010700030211     c                   movel     §blocau       cauret
010800030211     c                   if        §blosav <> 0
010900030211     c                   movel     §blosav       dataiso
011000030211     c                   movel     dataiso       dataeur
011100030211     c                   movel     dataeur       dtsavv
011200030211     c                   end
011300030211     c                   if        §bloclr <> 0
011400030211     c                   movel     §bloclr       dataiso
011500030211     c                   movel     dataiso       dataeur
011600030211     c                   movel     dataeur       dtclrv
011700030211     c                   end
011800030828     c                   when      tipo = 'D'
011900030828     c                   movel     §blddtp       dataiso
012000030828     c                   movel     dataiso       dataeur
012100030828     c                   movel     dataeur       dtprov
012200030828     c                   movel     §blddbl       dataiso
012300030828     c                   movel     dataiso       dataeur
012400030828     c                   movel     dataeur       dtblcv
012500030828     c                   z-add     §bldmax       impmax
012600040914     c                   z-add     §bldmin       impmin
012700030828     c                   movel     §bldcau       cauret
012701170717     c                   z-add     §bldimmx      impmmx
012800030828     c                   if        §bldsav <> 0
012900030828     c                   movel     §bldsav       dataiso
013000030828     c                   movel     dataiso       dataeur
013100030828     c                   movel     dataeur       dtsavv
013200030828     c                   end
013300030828     c                   if        §bldclr <> 0
013400030828     c                   movel     §bldclr       dataiso
013500030828     c                   movel     dataiso       dataeur
013600030828     c                   movel     dataeur       dtclrv
013700030828     c                   end
013800060704     c                   when      tipo = 'T'
013900060704     c                   movel     §bltdtp       dataiso
014000060704     c                   movel     dataiso       dataeur
014100060704     c                   movel     dataeur       dtprov
014200060704     c                   movel     §bltdbl       dataiso
014300060704     c                   movel     dataiso       dataeur
014400060704     c                   movel     dataeur       dtblcv
014500060704     c                   z-add     §bltmax       impmax
014600060704     c                   z-add     §bltmin       impmin
014700060704     c                   movel     §bltcau       cauret
014800060704     c                   if        §bltsav <> 0
014900060704     c                   movel     §bltsav       dataiso
015000060704     c                   movel     dataiso       dataeur
015100060704     c                   movel     dataeur       dtsavv
015200060704     c                   end
015300060704     c                   if        §bltclr <> 0
015400060704     c                   movel     §bltclr       dataiso
015500060704     c                   movel     dataiso       dataeur
015600060704     c                   movel     dataeur       dtclrv
015700060704     c                   end
015800030828     c                   endsl
015900951002     C*
016000170120     c                   clear                   aggexe
016100170125     c                   eval      aggexe = 'e aggiorna SUBITO anche in '
016200170125     c                             + 'ambiente di Filiale'
016300170120     C*
016400951002     C                   ENDSR
016500951002?     *--------------------------------------------------------------*
016600951002?     *  GESD01: Gestione prima videata                              *
016700951002?     *--------------------------------------------------------------*
016800951002     C     GESD01        BEGSR
016900951002      *
017000020513     c                   if        *in99
017100020513     c                   setoff                                       99
017200020513     C                   write     y350d01
017300020513     c                   seton                                        99
017400020513     c                   end
017500020513     C                   EXFMT     y350d01
017600951002      *  Fine Lavoro
017700951002     C     *INKC         IFEQ      '1'
017800951002     C                   MOVEL     'S'           WFINE
017900951002     C                   GOTO      FINVD1
018000951002     C                   END
018100951002      *  Controlli
018200951002     C                   EXSR      CTR01
018300020513     C   99              GOTO      FINVD1
018400020510      *  No errori
018500020510     c                   if        *inkf
018600020510     C                   EXSR      aggio
018700170125     C                   MOVEL     'S'           WFINE
018800170125     C*******            EXSR      inz01
018900020510     c                   end
019000951002      *
019100951002     C     FINVD1        ENDSR
019200951002?     *--------------------------------------------------------------*
019300951002?     *  CTR01: Controlli prima videata                              *
019400951002?     *--------------------------------------------------------------*
019500951002     C     CTR01         BEGSR
019600170120     c                   clear                   aggexe
019700020513     C                   SETOFF                                       99
019800020513     c* data protocollo
019900020513     c                   if        dtprov <> 0
020000020513     c     *eur          test(d)                 dtprov                 01
020100020513     c                   if        *in01
020200020513     c     *dmy          test(d)                 dtprov                 01
020300020513     c  n01*dmy          move      dtprov        dataeur
020400020513     c  n01              move      dataeur       dtprov
020500020513     c                   end
020600020513     c                   else
020700020513     c                   seton                                        01
020800020513     c                   end
020900020513     c* data blocco
021000020513     c                   if        dtblcv <> 0
021100020513     c     *eur          test(d)                 dtblcv                 02
021200020513     c                   if        *in02
021300020513     c     *dmy          test(d)                 dtblcv                 02
021400020513     c  n02*dmy          move      dtblcv        dataeur
021500020513     c  n02              move      dataeur       dtblcv
021600020513     c                   end
021700020513     c                   else
021800020513     c                   seton                                          02
021900020513     c                   end
022000040914     c* importo minimo da fatturare
022100040914     c                   if        impmin = 0
022200040914     c                   seton                                          06
022300040914     c                   end
022400020822     c* data salvataggio
022500020822     c                   if        %subst(knmus: 1: 3) = 'EDP'
022600020822     c                   if        dtsavv <> 0
022700020822     c     *eur          test(d)                 dtsavv                 03
022800020822     c                   if        *in03
022900020822     c     *dmy          test(d)                 dtsavv                 03
023000020822     c  n03*dmy          move      dtsavv        dataeur
023100020822     c  n03              move      dataeur       dtsavv
023200020822     c                   end
023300020822     c                   else
023400020822     c                   seton                                          03
023500020822     c                   end
023600020822     c                   end
023700021111     c*
023800021111     c* importo massimo per rettifica in sede
023900021111     c                   if        impmax = 0
024000021111     c                   seton                                          04
024100021111     c                   end
024200020513     c*
024300021111      * causale rettifica
024400021111     c                   if        cauret = *blank
024500021111     c                   seton                                          05
024600021111     c                   end
024700171117     c* ricerca test se campi diversi da blank e se sono previsti
024701171117     c                   if        *in12 and
024702171117     c                             (csoc <> *blank or ccdtes <> *blank)
024703171117     c     ktxt          chain     antxt01l
024704171117     c                   if        not %found(antxt01l)
024705171117     C                   SETON                                          13
024706171117     c                   endif
024707171117     c                   endif
024708171117      *
024800021111     c                   if        *in01 or *in02 or *in03 or *in04 or *in05
024900171117     c                             or *in06 or *in13
025000020513     c                   seton                                          99
025100020513     c                   end
025200020822     c*
025300020513     c                   if        not *in99
025400030211     c* autotrasp.
025500030828     c                   select
025600030828     c                   when      tipo = 'A'
025700020513     c                   movel     dtprov        dataeur
025800020513     c                   movel     dataeur       dataiso
025900020513     c                   movel     dataiso       §blcdtp
026000020513     c                   movel     dtblcv        dataeur
026100020513     c                   movel     dataeur       dataiso
026200020513     c                   movel     dataiso       §blcdbl
026300020822     c                   movel     dtsavv        dataeur
026400020822     c                   movel     dataeur       dataiso
026500020822     c                   movel     dataiso       §blcsav
026600021111     c                   z-add     impmax        §blcmax
026700040914     c                   z-add     impmin        §blcmin
026800021111     c                   movel     cauret        §blccau
026801171117     c                   movel     cauto         §blcauto
026802171117     c                   movel     ccorpo        §blccorpo
026803171117     c                   movel     cottico       §blcottico
026808171117     c                   movel     csoc          §blcsoc
026809171117     c                   movel     ccdtes        §blccdtes
026900020822      *  Controllo limiti due date
027000020822     C     §blcdtp       IFGT      §blcdbl
027100020822     C                   SETON                                        010299
027200020822     C     §blcsav       IFGT      §blcdtp
027300020822     C                   SETON                                        010399
027400020822     C                   END
027500020822     C                   END
027600030211     c* cooperative
027700030828     c                   when      tipo = 'C'
027800030211     c                   movel     dtprov        dataeur
027900030211     c                   movel     dataeur       dataiso
028000030211     c                   movel     dataiso       §blodtp
028100030211     c                   movel     dtblcv        dataeur
028200030211     c                   movel     dataeur       dataiso
028300030211     c                   movel     dataiso       §blodbl
028400030211     c                   movel     dtsavv        dataeur
028500030211     c                   movel     dataeur       dataiso
028600030211     c                   movel     dataiso       §blosav
028700030211     c                   z-add     impmax        §blomax
028800040914     c                   z-add     impmin        §blomin
028900030211     c                   movel     cauret        §blocau
029000030211      *  Controllo limiti due date
029100030211     C     §blodtp       IFGT      §blodbl
029200030211     C                   SETON                                        010299
029300030211     C     §blosav       IFGT      §blodtp
029400030211     C                   SETON                                        010399
029500030211     C                   END
029600030211     C                   END
029700030828     c* aff/defl
029800030828     c                   when      tipo = 'D'
029900030828     c                   movel     dtprov        dataeur
030000030828     c                   movel     dataeur       dataiso
030100030828     c                   movel     dataiso       §blddtp
030200030828     c                   movel     dtblcv        dataeur
030300030828     c                   movel     dataeur       dataiso
030400030828     c                   movel     dataiso       §blddbl
030500030828     c                   movel     dtsavv        dataeur
030600030828     c                   movel     dataeur       dataiso
030700030828     c                   movel     dataiso       §bldsav
030800030828     c                   z-add     impmax        §bldmax
030900040914     c                   z-add     impmin        §bldmin
031000030828     c                   movel     cauret        §bldcau
031001170717     c                   z-add     impmmx        §bldimmx
031100030828      *  Controllo limiti due date
031200030828     C     §blddtp       IFGT      §blddbl
031300030828     C                   SETON                                        010299
031400030828     C     §bldsav       IFGT      §blddtp
031500030828     C                   SETON                                        010399
031600030828     C                   END
031700030828     C                   END
031800060704     c* trazionisti
031900060704     c                   when      tipo = 'T'
032000060704     c                   movel     dtprov        dataeur
032100060704     c                   movel     dataeur       dataiso
032200060704     c                   movel     dataiso       §bltdtp
032300060704     c                   movel     dtblcv        dataeur
032400060704     c                   movel     dataeur       dataiso
032500060704     c                   movel     dataiso       §bltdbl
032600060704     c                   movel     dtsavv        dataeur
032700060704     c                   movel     dataeur       dataiso
032800060704     c                   movel     dataiso       §bltsav
032900060704     c                   z-add     impmax        §bltmax
033000060704     c                   z-add     impmin        §bltmin
033100060704     c                   movel     cauret        §bltcau
033200060704      *  Controllo limiti due date
033300060704     C     §bltdtp       IFGT      §bltdbl
033400060704     C                   SETON                                        010299
033500060704     C     §bltsav       IFGT      §bltdtp
033600060704     C                   SETON                                        010399
033700060704     C                   END
033800060704     C                   END
033900030828     c                   endsl
034000030211     c                   end
034100951003      *
034200020513     C                   ENDSR
034300951006?     *--------------------------------------------------------------*
034400020513?     *  AGGIORNAMENTO TNTBE00F                                      *
034500951006?     *--------------------------------------------------------------*
034600020510     C     aggio         BEGSR
034700951006      *
034800020513     c     ktbe          chain     tntbe01l
034900170120      *
035000100423     c                   select
035100030828     c                   when      tipo = 'A'
035200100423     c                   clear                   TBEFTR
035300100423     c                   clear                   TBEDTR
035400020513     c                   movel     dblc          tbeuni
035500030828     c                   when      tipo = 'C'
035600100423     c                   clear                   TBEFTR
035700100423     c                   clear                   TBEDTR
035800030211     c                   movel     dblo          tbeuni
035900030828     c                   when      tipo = 'D'
036000100423     c                   clear                   TBEFTR
036100100423     c                   clear                   TBEDTR
036200030828     c                   movel     dbld          tbeuni
036300060704     c                   when      tipo = 'T'
036400060704     c                   movel     dblt          tbeuni
036500030828     c                   endsl
036600170120      *
036700020513     c                   if        %found(tntbe01l)
036800020513     c                   update    tntbe000
036900020513     c                   else
037000020513     c                   write     tntbe000
037100020510     c                   end
037200170120      *
037300170120     c                   exsr      AggTbeFil
037400951006      *
037500951006     C                   ENDSR
037600170120      *---------------------------------------------------------------*
037700170120      * AggTbeFil * Riempimento dati tabella in filiale
037800170120      *---------------------------------------------------------------*
037900170120     c     AggTbeFil     BEGSR
038000170120      *
038100170120     c     Ktbe          chain     tntbex1l
038200170120      *
038300170120     c                   movel     TNTBEds       xTNTBEds
038400170120      *
038500170120     c                   if        %found(tntbex1l)
038600170120     c                   update    tntbex
038700170120     c                   else
038800170120     c                   write     tntbex
038900170120     c                   endif
039000170120
039100170120     c                   ENDSR
039200020513     C*----------------------------------------------------*
039300020513     C* Reperimento dati società
039400020513     C*----------------------------------------------------*
039500020513     C     REPSOC        BEGSR
039600020513     C*
039700170120     C                   CALL      'XSOC'
039800020513     C                   PARM                    TIPXSC            6
039900020513     C                   PARM                    SOCXSC            3
040000020513     C                   PARM                    CDSXSC            9 0
040100020513     C                   PARM                    MODXSC            3
040200020513     C                   PARM      *blanks       RTNXSC            1
040300020513     C                   PARM                    XSOCDS
040400020513     C                   PARM                    KPJBA
040500020513     C*
040600020513     C                   ENDSR
040700951006?     *--------------------------------------------------------------*
040800951006?     * *INZSR: Operazioni iniziali                                  *
040900951006?     *--------------------------------------------------------------*
041000951009     C     *INZSR        BEGSR
041100951006      *
041200951006     C     *ENTRY        PLIST
041300951006     C                   PARM                    KPJBA
041400030211     c                   movel     kpjbu         tipo              1
041500030828     c                   select
041600030828     c                   when      tipo = 'A'
041700030211     c                   eval      destip = 'AUTOTRASPORTATORI'
041800030828     c                   when      tipo = 'C'
041900030211     c                   eval      destip = '   COOPERATIVE   '
042000030828     c                   when      tipo = 'D'
042100030828     c                   eval      destip = 'AFFLUEN./DEFLUEN.'
042200060704     c                   when      tipo = 'T'
042300060704     c                   eval      destip = '   TRAZIONISTI   '
042400030828     c                   endsl
042500020513     C*---------- RICERCA DITTA :
042600020513     C                   MOVEL     'SOC001'      TIPXSC
042700020513     C                   MOVEL     *BLANK        SOCXSC
042800020513     C                   EXSR      REPSOC
042900020513     C     RTNXSC        IFNE      '1'
043000020513     C                   MOVEL     XSOCDS        SOC001
043100020513     C                   MOVEL     xscrgs        RSUT             20
043200020513     c                   else
043300020513     c                   goto      fine
043400020513     c                   end
043500020513     C     Ktbe          KLIST
043600020513     C                   KFLD                    tbecod
043700020513     C                   KFLD                    tbeke1
043701171117     C     Ktxt          KLIST
043702171117     C                   KFLD                    csoc
043703171117     C                   KFLD                    unita             8
043704171117     C                   KFLD                    ccdtes
043800171117     c                   select
043900030828     c                   when      tipo = 'A'
044000020513     c                   movel(p)  'BLC'         tbecod
044001171117     c                   seton                                        12
044100030828     c                   when      tipo = 'C'
044200030211     c                   movel(p)  'BLO'         tbecod
044300030828     c                   when      tipo = 'D'
044400030828     c                   movel(p)  'BLD'         tbecod
044401170717     c                   seton                                        11
044500060704     c                   when      tipo = 'T'
044600060704     c                   movel(p)  'BLT'         tbecod
044700030828     c                   endsl
044800020513     c                   movel(p)  '1'           tbeke1
044900951006      *  Inizializzo variabili
045000951006     C                   MOVEL     'N'           WFINE             1
045100020822     c*  rendo modificabile la data dei salvataggi
045200020822     c                   if        %subst(knmus: 1: 3) = 'EDP'
045300020822     c                   seton                                        10
045400020822     c                   end
045500980910      *
045600980910     c                   endsr
