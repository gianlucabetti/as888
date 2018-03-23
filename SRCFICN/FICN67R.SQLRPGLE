000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030520     H/TITLE  cambio codice fornitore nel dettaglio cooperative
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600030520      * ficn67R                                                      *
000700951009      *     *----------------------------------------------*         *
000800020415      *         cambio codice fornitore                              *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500030520     Fficn67D   CF   E             WORKSTN
001600030520     Ffictt06l  iF   E           K DISK
001700021203     Ffiapd01l  iF   E           K DISK
001800020415     Fanfrn01l  iF   E           K DISK
001900020415     Fansog01l  iF   E           K DISK
002000020415     Fanrco01l  iF   E           K DISK
002100020411     D* Reperimento nome PGM
002200020411     D STATUS         SDS           333
002300020411     D  nompgm           *PROC
002400020415     D* posizione cursore
002500020415     D CURSOR          DS
002600020415     D  RIRI                   1      2B 0 INZ
002700020415     D  R$$                    2      2
002800020415     D  COCO                   3      4B 0 INZ
002900020415     D  C$$                    4      4
003000020415     D DSFMT           DS
003100020415     D  $TASTO               369    369
003200020415     D  NRG                  370    370
003300020415     D  NCL                  371    371
003400020411     D*---------------------------------------------------------------*
003500020411     D* CAMPI INTERNI
003600020411     D*---------------------------------------------------------------*
003700020411     D dataiso         s               d   datfmt(*iso)
003800020411     D dataeur         s               d   datfmt(*eur)
003900030520     D tsr             s                   like(tsrv)
004000030520     D fvl             s                   like(cttfvl)
004100030520     D x               s              2  0 inz
004200030520     D xx              s              2  0 inz
004300980911     D soc001        E DS                  EXTNAME(xsoc001ds)
004400980911     D xsocds          DS          1000
004500021203     D fnlv24ds      E DS
004600951009     D KPJBA         E DS
004700020415     D ana007ds      e ds                  inz
004800020411     c* SQL
004900020412     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
005000020405     D WrkSqlCmd       S           1024
005100951009?     *--------------------------------------------------------------*
005200951009?     *  CICLO PRINCIPALE                                            *
005300951009?     *--------------------------------------------------------------*
005400951009      *  Impostazione parametri prima videata
005500020418     c                   if        wfine <> 'S'
005600020415     C                   EXSR      inz01
005700951009      *  Loop gestione videata
005800951009     C     WFINE         DOUEQ     'S'
005900000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
006000951009     C                   END
006100951012     C                   END
006200020418     C                   END
006300980312     C*
006400951009     C                   SETON                                        LR
006500951013?     *--------------------------------------------------------------*
006600020415?     *  INZ01: Inizializzazione videata                             *
006700951013?     *--------------------------------------------------------------*
006800951013     C     INZ01         BEGSR
006900951013      *
007000020415      *  Inizializzo videata
007100951013     C                   MOVEL     '1'           WTPVID
007200020415     C                   move      xscsoc        newsoc
007300020415     C                   move      xscsoc        oldsoc
007400030520     C                   move      'X'           tsrv
007500951013      *
007600951013     C                   ENDSR
007700020405?     *--------------------------------------------------------------*
007800020405?     *  GESS01: Gestione prima videata                              *
007900020405?     *--------------------------------------------------------------*
008000020405     C     GESS01        BEGSR
008100020405      *
008200020419     c                   if        *in28
008300020419     c                   setoff                                       28
008400020419     C                   write     y350d01
008500020419     c                   seton                                        28
008600020419     C                   END
008700020415     C                   EXFMT     y350d01
008800020415      *  Fine Lavoro
008900020415     C     *INKC         IFEQ      '1'
009000020415     C                   MOVEL     'S'           WFINE
009100020415     c                   goto      finvd1
009200020415     C                   END
009300020415     C*  effettua ricerche
009400020415     c                   if        *inkd = *on
009500020415     c                   exsr      search
009600020415     c                   end
009700951009      *  Controlli
009800951009     C                   EXSR      CTR01
009900020709     c                   if        *in28 and not *inkh
010000020709     C                   GOTO      FINVD1
010100020709     c                   end
010200020408      *  CONFERMA
010300951013     C     *INKF         IFEQ      '1'
010400020709     C     *INKh         oreq      '1'
010500020415      * Richiamo istruzione sql x modificare il codice
010600020415     C                   EXSR      CALagg
010700951013      *  Se quando esco dal programma di prima nota è tutto O.K
010800980914     C                   MOVEL     'S'           WFINE
010900980911     C                   END                                                    V3CTOT = y35FAT
011000951009      *
011100951009     C     FINVD1        ENDSR
011200020415     C************************************************************
011300020415     C* RICERCHE
011400020415     C************************************************************
011500020415     C     SEARCH        BEGSR
011600020415     C*
011700020415     C* determino Riga/Colonna del cursore
011800020415     C                   MOVE      nrg           R$$
011900020415     C                   MOVE      ncl           C$$
012000020415     C                   Z-ADD     RIRI          CURR
012100020415     C                   Z-ADD     COCO          CURC
012200020418     c                   setoff                                       5354
012300020415     C*
0124000204151    C                   SELECT
012500020415     C* FMTD1  - ricerca società
012600020415    >C                   WHEn      h1nmfl = 'OLDSOC' or
012700020419    >C                             h1nmfl = 'NEWSOC'
012800020415     C                   reset                   ana007ds
012900020415     C                   eval      opz007 = '01'
013000020415     C                   eval      kpjbu = ana007ds
013100020415     C                   call      'ANA007R'
013200020415     C                   parm                    kpjba
013300020415     C                   eval      ana007ds = kpjbu
013400020415     C                   if        err007 <> '1' and soc007 <> *blanks
013500020419    >C                   if        h1nmfl = 'OLDSOC'
013600020415     C                   eval      oldsoc = soc007
013700020415     c                   else
013800020415     C                   eval      newsoc = soc007
013900020415     C                   endif
014000020415     C                   endif
014100030520     C* FMTD1  - ricerca coop
014200020415    >C                   WHEn      h1nmfl = 'V1CPDR'
014300021203     C                   clear                   fnlv24ds
014400030520     C                   MOVEL     'C'           d24tip
014500021203     C                   MOVEL     'R'           d24FLG
014600021203     C                   MOVEL(p)  fnlv24ds      KPJBU
014700020415     C                   CALL      'FNLV24R'
014800020415     C                   PARM                    KPJBA
014900021203     C                   MOVEL     KPJBU         fnlv24ds
015000021203     c                   move      d24pdr        v1cpdr
015100021203     c                   movel     d24rsc        v1cdpr
015200020415     C                   ENDsl
015300020415     C* imposto pos. del cursore
015400020415     C                   Z-ADD     CURR          H1RIGA
015500020415     C                   Z-ADD     CURC          H1COLO
015600020415     c*
015700020415     C                   ENDsr
015800951009?     *--------------------------------------------------------------*
015900951009?     *  CTR01: Controlli prima videata                              *
016000951009?     *--------------------------------------------------------------*
016100951009     C     CTR01         BEGSR
016200951009      *
016300020709     c                   setoff                                       2806
016400020415     C* Controllo società old
016500020415     C                   if        OLDSOC = *blanks
016600020415     C                   seton                                        2833
016700020415     c                   else
016800020415     C                   eval      tipxsc = 'SOC002'
016900020415     C                   eval      socxsc = oldsoc
017000020415     C                   exsr      repsoc
017100020415     C                   if        rtnxsc = '1'
017200020415     C                   seton                                        2833
017300020415     C                   else
017400020415     C                   if        xscscf = 'S'
017500020415     C                   seton                                        2833
017600020415     C                   endif
017700020415     C                   endif
017800020415     C                   endif
017900020415     C* Controllo fornitore old
018000020415     C     oldfor        IFEQ      *blanks
018100020415     C                   SETON                                        4328
018200020415     C                   else
018300020415     C                   move      oldsoc        rcosocieta
018400020415     C                   move      FORITA        rcokcc
018500020415     C                   move      oldfor        rcoksc
018600020415     c                   movel     *blank        sogdes
018700020415     C     Kfrn          CHAIN     anfrn01l                           30
018800020415     C  N30Krco          CHAIN     anrco01l                           30
018900020415     C  N30rcosogg       CHAIN     ansog01l                           30
019000020415     C                   MOVEL(p)  sogdes        desold
019100020415     C     *IN30         IFEQ      '1'
019200020415     C                   SETON                                        4328
019300020415     C                   END
019400020415     c                   end
019500020415     C* Controllo società new
019600020415     C                   if        newSOC = *blanks
019700020415     C                   seton                                        9934
019800020415     c                   else
019900020415     C                   eval      tipxsc = 'SOC002'
020000020415     C                   eval      socxsc = newsoc
020100020415     C                   exsr      repsoc
020200020415     C                   if        rtnxsc = '1'
020300020415     C                   seton                                        9934
020400020415     C                   else
020500020415     C                   if        xscscf = 'S'
020600020415     C                   seton                                        9934
020700020415     C                   endif
020800020415     C                   endif
020900020415     C                   endif
021000020415     C* Controllo fornitore new
021100020415     C     newfor        IFEQ      *blanks
021200020415     C                   SETON                                        4428
021300020415     C                   else
021400020415     C                   move      newsoc        rcosocieta
021500020415     C                   move      FORITA        rcokcc
021600020415     C                   move      newfor        rcoksc
021700020415     c                   movel     *blank        sogdes
021800020415     C     Kfrn          CHAIN     anfrn01l                           30
021900020415     C  N30Krco          CHAIN     anrco01l                           30
022000020415     C  N30rcosogg       CHAIN     ansog01l                           30
022100020415     C                   MOVEL(p)  sogdes        desnew
022200020415     C     *IN30         IFEQ      '1'
022300020415     C                   SETON                                        4428
022400020415     C                   END
022500020415     c                   end
022600020419     c* controllo che non immesso stesso codice
022700020419     c                   if        oldsoc = newsoc and oldfor = newfor
022800020419     C                   SETON                                        442834
022900020419     C                   SETON                                        4333
023000020419     c                   end
023100030520     c* coop
023200020415     c                   clear                   v1cdpr
023300020415    2C     v1cpdr        IFeq      0
023400020415     C                   SETON                                        4028
023500020415     c                   else
023600021203     C     kapdv         CHAIN     fiapd01L                           40
023700020415     C     *IN40         IFEQ      *ON
023800080206     C     apdksc        oreq      0
023900020703     C                   SETON                                        28
024000020415     c                   else
024100020419     c                   move      *zeros        com8              8
024200020419     c                   move      apdksc        com8              8
024300080409     c                   movel     apdrsf        v1cdpr
024400030520     c* controllo che attualmente l'associazione coop. e fornitore sia
024500020704     c* corretta
024600020704     c                   if        com8 <> newfor or apdcsf <> newsoc
024700020709     C                   SETON                                        46
024800020419     c                   end
024900020415    3C                   ENDIF
025000020415    1C                   ENDIF
025100020415     c* data da
025200020415     c                   if        v1cdti <> 0
025300020415     c     *eur          test(d)                 v1cdti                 41
025400020415     c                   if        *in41
025500020415     c     *dmy          test(d)                 v1cdti                 41
025600020415     c  n41*dmy          move      v1cdti        dataeur
025700020415     c  n41              move      dataeur       v1cdti
025800020415     c                   end
025900020415     c                   else
026000020415     c                   seton                                        41
026100020415     c                   end
026200020415     c* data al
026300020415     c                   if        v1cdtf <> 0
026400020415     c     *eur          test(d)                 v1cdtf                 45
026500020415     c                   if        *in45
026600020415     c     *dmy          test(d)                 v1cdtf                 45
026700020415     c  n45*dmy          move      v1cdtf        dataeur
026800020415     c  n45              move      dataeur       v1cdtf
026900020415     c                   end
027000020415     c                   else
027100020415     c                   seton                                          2845
027200020415     c                   end
027300020415     c                   if        *in41 or *in45
027400020415     c                   seton                                          28
027500020415     c                   else
027600020415     c                   movel     v1cdti        dataeur
027700020415     c                   movel     dataeur       dataiso
027800020415     c                   movel     dataiso       wdatda            8 0
027900020415     c                   movel     v1cdtf        dataeur
028000020415     c                   movel     dataeur       dataiso
028100020415     c                   movel     dataiso       wdataa            8 0
028200020415      *  Controllo limiti due date
028300020415     C     WDATDA        IFGT      WDATAA
028400020415     C                   SETON                                        414528
028500020415     C                   END
028600020415     c                   end
028700030520     c* a seconda della scelta del tipo servizio fatta a video mi metto
028800030520     c* in loop
028900020709     c                   if        not *in28
029000030520     c                   seton                                        5628
029100030520     c                   if        tsrv = ' '
029200030520     c                   eval      x = 3
029300030520     c                   else
029400030520     c                   eval      x = 1
029500030520     c                   end
029600030520     c                   do        x             xx
029700030520     c                   select
029800030520     c                   when      x = 1
029900030520     c                   eval      tsr = tsrv
030000030520     c                   when      x = 3 and xx = 1
030100030520     c                   eval      tsr = 'X'
030200030520     c                   when      x = 3 and xx = 2
030300030520     c                   eval      tsr = 'P'
030400030520     c                   when      x = 3 and xx = 3
030500030520     c                   eval      tsr = 'T'
030600030520     c                   endsl
030700030520     c* controllo che non sia già contabilizzato
030800030520     c                   movel     'C'           fvl
030900030520     c     kctt          setll     fictt06l
031000030520     c                   do        *hival
031100030520     c     kctt1         reade     fictt06l
031200030520     c                   if        %eof(fictt06l)
031300030520     c                   leave
031400030520     c                   end
031500030520     c                   if        cttddc > wdataa
031600030520     c                   leave
031700030520     c                   end
031800030520     c                   if        cttnff <> 0
031900030520     c                   seton                                        2855
032000030520     c* spengo ind. 56 di conteggi non trovati
032100030520     C                   setoff                                       56
032200030520     c                   leave
032300030520     c                   else
032400030520     c                   setoff                                       5628
032500030520     c                   end
032600030520     c                   enddo
032700030520     c* se ho l'errore non continuo con gli altri controlli
032800030520     c                   if        *in55
032900030520     c                   leave
033000030520     c                   end
033100030520     C* controllo se ho dei conteggi non confermati
033200030520     c                   if        *in56
033300030520     c                   clear                   fvl
033400030520     c     kctt          setll     fictt06l
033500030520     c                   do        *hival
033600030520     c     kctt1         reade     fictt06l
033700030520     c                   if        %eof(fictt06l)
033800030520     c                   leave
033900030520     c                   end
034000030520     c                   if        cttddc > wdataa
034100030520     c                   leave
034200030520     c                   end
034300030520     c                   setoff                                       5628
034400030520     c                   leave
034500030520     c                   enddo
034600030520     c                   end
034700030520     c                   enddo
034800020709     c                   end
034900020709     c* abilito il comando di forzatura solo se l'errore non è bloccante
035000020709     c                   if        not *in28 and *in46
035100020709     c                   seton                                        2806
035200020709     c                   end
035300951010      *
035400020419     C                   ENDSR
035500020415     C*----------------------------------------------------*
035600020415     C* AGGIORNO
035700020415     C*----------------------------------------------------*
035800020415     C     CALAGG        BEGSR
035900020415     c* MI COMPONGO L'ISTRUZIONE SQL
036000020415     c                   eval      %subst(cmd(1): 31: 3) = newSOC
036100020415     c                   eval      %subst(cmd(1): 47: 8) = newfor
036200020415     c                   eval      %subst(cmd(2): 11: 3) = oldsoc
036300020415     c                   eval      %subst(cmd(2): 30: 8) = oldfor
036400020415     c                   move      v1cpdr        com7              7
036500020415     c                   eval      %subst(cmd(2): 53: 7) = com7
036600020415     c                   move      wdatda        com8              8
036700020415     c                   eval      %subst(cmd(3): 11: 8) = com8
036800020415     c                   move      wdataa        com8
036900020415     c                   eval      %subst(cmd(3): 34: 8) = com8
037000030520     c                   if        tsrv = ' '
037100020415     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
037200030520     c                   else
037300030520     c                   eval      %subst(cmd(4): 15: 1) = tsrv
037400030520     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)+CMD(4)
037500030520     c                   end
037600020415     c*
037700030520     c* aggiorno tutti i record di fictt00f che hanno fornitore old,
037800030520     c* coop. e data conteggio come da parametri video e numero
037900020415     c* fattura = 0 con il nuovo fornitore
038000020415     c*
038100020415     C/EXEC SQL
038200020415     C+ EXECUTE IMMEDIATE :WrkSqlCmd
038300020415     C/END-EXEC
038400020415     C                   ENDSR
038500980911     C*----------------------------------------------------*
038600980911     C* Reperimento dati società
038700980911     C*----------------------------------------------------*
038800980911     C     REPSOC        BEGSR
038900980911     C*
039000980911     C                   CALLB     'XSOC'
039100980911     C                   PARM                    TIPXSC            6
039200980911     C                   PARM                    SOCXSC            3
039300980911     C                   PARM                    CDSXSC            9 0
039400020415     C                   PARM      *blanks       MODXSC            3
039500980911     C                   PARM      *blanks       RTNXSC            1
039600980911     C                   PARM                    XSOCDS
039700980911     C                   PARM                    KPJBA
039800980911     C*
039900980911     C                   ENDSR
040000951011?     *--------------------------------------------------------------*
040100951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
040200951011?     *--------------------------------------------------------------*
040300951011     C     *INZSR        BEGSR
040400951011      *
040500951011     C     *ENTRY        PLIST
040600951011     C                   PARM                    KPJBA
040700020418      *  Inizializzo variabili
040800020418     C                   MOVEL     '1'           WTPVID            1
040900020418     C                   MOVEL     'N'           WFINE             1
041000020418     C                   Z-ADD     0             CURR              2 0
041100020418     C                   Z-ADD     0             CURC              2 0
041200020415     C*---------- RICERCA DITTA :
041300020415     C                   MOVEL     'SOC001'      TIPXSC
041400020415     C                   MOVEL     *BLANK        SOCXSC
041500020415     C                   EXSR      REPSOC
041600980911     C     RTNXSC        IFNE      '1'
041700980911     C                   MOVEL     XSOCDS        SOC001
041800980911     C                   MOVEL     xscrgs        RSUT             20
041900020418     c                   else
042000020418     C                   MOVEL     'S'           WFINE             1
042100980911     c                   end
042200020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
042300020415     C                   movel     'F     '      $kcc              6
042400020415     C                   movel     *blanks       $ksc              8
042500020415     C                   callb     'XCAPCLIFOR'
042600020415     C                   parm                    xscsoc
042700020415     C                   parm                    $kcc
042800020415     C                   parm                    $ksc
042900020415     C                   movel     $kcc          forita            6
043000030520     C     Kctt          KLIST
043100020703     C                   KFLD                    oldsoc
043200020703     C                   KFLD                    oldfor
043300030520     C                   KFLD                    fvl
043400030520     C                   KFLD                    tsr
043500020703     C                   KFLD                    v1cpdr
043600020703     C                   KFLD                    wdatda
043700030520     C     Kctt1         KLIST
043800020703     C                   KFLD                    oldsoc
043900020703     C                   KFLD                    oldfor
044000030520     C                   KFLD                    fvl
044100030520     C                   KFLD                    tsr
044200020703     C                   KFLD                    v1cpdr
044300020703     C     Kfrn          KLIST
044400020703     C                   KFLD                    rcosocieta
044500020703     C                   KFLD                    rcoksc
044600020415     C     Krco          KLIST
044700020415     C                   KFLD                    rcosocieta
044800020415     C                   KFLD                    rcokcc
044900020415     C                   KFLD                    rcoksc
045000021203     C     Kapdv         KLIST
045100021203     C                   KFLD                    apdtip
045200021203     C                   KFLD                    v1cpdr
045300030520     c                   movel     'C'           apdtip
045400951012     C                   ENDSR
045500020415**
045600030520UPDATE fictt00f set cttsoc = '000', cttcdf = '00000000' where         1
045700030520cttsoc = '000' and cttcdf = '00000000' and cttpdr = 0000000 and       2
045800030520cttddc >= 00000000 and cttddc <= 00000000 and cttnff = 0              3
045900030520and ctttsr = '0'                                                      4
