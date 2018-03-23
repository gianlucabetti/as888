000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030828     H/TITLE  cambio codice fornitore nel dettaglio aut/aff/defl
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600090710      * ficn89R                                                      *
000700951009      *     *----------------------------------------------*         *
000800090710      *         ristampa autofatture                                 *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500090710     Fficn89D   CF   E             WORKSTN
001600020703     Ffiftt06l  iF   E           K DISK
001700030828     Ffiatt06l  iF   E           K DISK
001701090710     Ffictt06l  iF   E           K DISK
001800021203     Ffiapd01l  iF   E           K DISK
001900020415     Fanfrn01l  iF   E           K DISK
002000020415     Fansog01l  iF   E           K DISK
002100020415     Fanrco01l  iF   E           K DISK
002200020411     D* Reperimento nome PGM
002300020411     D STATUS         SDS           333
002400020411     D  nompgm           *PROC
002500020415     D* posizione cursore
002600020415     D CURSOR          DS
002700020415     D  RIRI                   1      2B 0 INZ
002800020415     D  R$$                    2      2
002900020415     D  COCO                   3      4B 0 INZ
003000020415     D  C$$                    4      4
003100020415     D DSFMT           DS
003200020415     D  $TASTO               369    369
003300020415     D  NRG                  370    370
003400020415     D  NCL                  371    371
003500020411     D*---------------------------------------------------------------*
003600020411     D* CAMPI INTERNI
003700020411     D*---------------------------------------------------------------*
003800020411     D dataiso         s               d   datfmt(*iso)
003900020411     D dataeur         s               d   datfmt(*eur)
004000980911     D soc001        E DS                  EXTNAME(xsoc001ds)
004100980911     D xsocds          DS          1000
004200021203     D fnlv24ds      E DS
004300951009     D KPJBA         E DS
004400020415     D ana007ds      e ds                  inz
004500020415     D*------------------
004600020415     D* DS "TRUL16R" - RICERCA ALFABETICA autotrasportatore
004700020415     D*------------------
004800020415     D PARAM           DS                  INZ
004900020415     D* TIPOLOGIA P_=autotrasport.
005000020415     D  PARTIP                 1      2
005100020415     D* RAGIONE SOCIALE aoutotras.
005200020415     D  PARRSC                 3     37
005300020415     D* S.I.
005400020415     D  PARCSF                38     40
005500020415     D* CODICE
005600020415     D  PARPDR                41     47  0
005700020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
005800020415     D*                  ALFABETICA RICHIESTA
005900020415     D  PARFLG                48     48
006000020411     c* SQL
006100020405     D WrkSqlCmd       S           1024
006200951009?     *--------------------------------------------------------------*
006300951009?     *  CICLO PRINCIPALE                                            *
006400951009?     *--------------------------------------------------------------*
006500951009      *  Impostazione parametri prima videata
006600020418     c                   if        wfine <> 'S'
006700020415     C                   EXSR      inz01
006800951009      *  Loop gestione videata
006900951009     C     WFINE         DOUEQ     'S'
007000000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
007100951009     C                   END
007200951012     C                   END
007300020418     C                   END
007400980312     C*
007500951009     C                   SETON                                        LR
007600951013?     *--------------------------------------------------------------*
007700020415?     *  INZ01: Inizializzazione videata                             *
007800951013?     *--------------------------------------------------------------*
007900951013     C     INZ01         BEGSR
008000951013      *
008100020415      *  Inizializzo videata
008200951013     C                   MOVEL     '1'           WTPVID
008400020415     C                   move      xscsoc        oldsoc
008500951013      *
008600951013     C                   ENDSR
008700020405?     *--------------------------------------------------------------*
008800020405?     *  GESS01: Gestione prima videata                              *
008900020405?     *--------------------------------------------------------------*
009000020405     C     GESS01        BEGSR
009100020405      *
009200020419     c                   if        *in28
009300020419     c                   setoff                                       28
009400020419     C                   write     y350d01
009500020419     c                   seton                                        28
009600020419     C                   END
009700020415     C                   EXFMT     y350d01
009800020415      *  Fine Lavoro
009900020415     C     *INKC         IFEQ      '1'
010000020415     C                   MOVEL     'S'           WFINE
010100020415     c                   goto      finvd1
010200020415     C                   END
010300020415     C*  effettua ricerche
010400020415     c                   if        *inkd = *on
010500020415     c                   exsr      search
010600020415     c                   end
010700951009      *  Controlli
010800951009     C                   EXSR      CTR01
010900020709     c                   if        *in28 and not *inkh
011000020709     C                   GOTO      FINVD1
011100020709     c                   end
011200020408      *  CONFERMA
011300951013     C     *INKF         IFEQ      '1'
011400020709     C     *INKh         oreq      '1'
011500020415      * Richiamo istruzione sql x modificare il codice
011600020415     C                   EXSR      CALagg
011800980914     C                   MOVEL     'S'           WFINE
011900980911     C                   END                                                    V3CTOT = y35FAT
012000951009      *
012100951009     C     FINVD1        ENDSR
012200020415     C************************************************************
012300020415     C* RICERCHE
012400020415     C************************************************************
012500020415     C     SEARCH        BEGSR
012600020415     C*
012700020415     C* determino Riga/Colonna del cursore
012800020415     C                   MOVE      nrg           R$$
012900020415     C                   MOVE      ncl           C$$
013000020415     C                   Z-ADD     RIRI          CURR
013100020415     C                   Z-ADD     COCO          CURC
013200020418     c                   setoff                                       5354
013300020415     C*
013400020415     C* I campi che al momento dell'F4 risultano protetti non devono
013500020415     C* essere abilitati alla funzione di ricerca
013600020415     C*
0137000204151    C                   SELECT
013800020415     C* FMTD1  - ricerca società
013900090710    >C                   WHEn      h1nmfl = 'OLDSOC'
014100020415     C                   reset                   ana007ds
014200020415     C                   eval      opz007 = '01'
014300020415     C                   eval      kpjbu = ana007ds
014400020415     C                   call      'ANA007R'
014500020415     C                   parm                    kpjba
014600020415     C                   eval      ana007ds = kpjbu
014700020415     C                   if        err007 <> '1' and soc007 <> *blanks
014900020415     C                   eval      oldsoc = soc007
015300020415     C                   endif
015400020415     C* FMTD1  - ricerca fornitore
015500090710    >C                   WHEn      h1nmfl = 'OLDFOR'
015700020415     C*  deposita la KPJBU
015800020415     C                   MOVEL     KPJBU         DKPJBU          256
016000020415     C                   MOVEl(p)  desold        PARRSC
016100020419     C                   movel     oldsoc        parcsf
016600020419     C                   Z-ADD     0             PARPDR
016700020415     C                   eval      partip = 'P '
016800020415     C                   MOVEL     *BLANKS       PARFLG
016900020419     C                   MOVEL(P)  PARAM         KPJBU
017000020415     C                   CALL      'TRUL16R'
017100020415     C                   PARM                    KPJBA
017200020415     C                   MOVEL     KPJBU         PARAM
017300020415IF  2C     PARFLG        IFEQ      '3'
017500020415     C                   MOVEL     *BLANKS       oldfor
017600020415     C                   SETON                                        53
018100020415     c                   if        *in53 or *in54
018200020415     C                   SETON                                        28
018300020415     c                   end
018400020415X   2C                   ELSE
018600020419     c                   move      *zeros        oldfor
018700020419     C                   MOVE      PARPDR        oldfor
018800020415     C                   MOVEL     PARRSC        desold
019400020415E   2C                   ENDIF
019500020415     C* REIMPOSTA LA KPJBU
019600020415     C                   MOVEL     DKPJBU        KPJBU
019700020415     C* FMTD1  - ricerca padroncino
019800020415    >C                   WHEn      h1nmfl = 'V1CPDR'
019900021203     C                   clear                   fnlv24ds
020000090710     C                   MOVEL     v1ctip        d24tip
020100021203     C                   MOVEL     'R'           d24FLG
020200021203     C                   MOVEL(p)  fnlv24ds      KPJBU
020300020415     C                   CALL      'FNLV24R'
020400020415     C                   PARM                    KPJBA
020500021203     C                   MOVEL     KPJBU         fnlv24ds
020600021203     c                   move      d24pdr        v1cpdr
020700021203     c                   movel     d24rsc        v1cdpr
020800020415     C                   ENDsl
020900020415     C* imposto pos. del cursore
021000020415     C                   Z-ADD     CURR          H1RIGA
021100020415     C                   Z-ADD     CURC          H1COLO
021200020415     c*
021300020415     C                   ENDsr
021400951009?     *--------------------------------------------------------------*
021500951009?     *  CTR01: Controlli prima videata                              *
021600951009?     *--------------------------------------------------------------*
021700951009     C     CTR01         BEGSR
021800951009      *
021900020709     c                   setoff                                       2806
021901090710     c* tipo
021902090710     c                   if        v1ctip = ' '
021903090710     c                   seton                                        90
021904090710     c                   leavesr
021905090710     c                   end
022000090710     C* Controllo società
022100020415     C                   if        OLDSOC = *blanks
022200020415     C                   seton                                        2833
022300020415     c                   else
022400020415     C                   eval      tipxsc = 'SOC002'
022500020415     C                   eval      socxsc = oldsoc
022600020415     C                   exsr      repsoc
022700020415     C                   if        rtnxsc = '1'
022800020415     C                   seton                                        2833
022900020415     C                   else
023000020415     C                   if        xscscf = 'S'
023100020415     C                   seton                                        2833
023200020415     C                   endif
023300020415     C                   endif
023400020415     C                   endif
023500090710     C* Controllo fornitore
023600020415     C     oldfor        IFEQ      *blanks
023700020415     C                   SETON                                        4328
023800020415     C                   else
023900020415     C                   move      oldsoc        rcosocieta
024000020415     C                   move      FORITA        rcokcc
024100020415     C                   move      oldfor        rcoksc
024200020415     c                   movel     *blank        sogdes
024300020415     C     Kfrn          CHAIN     anfrn01l                           30
024400020415     C  N30Krco          CHAIN     anrco01l                           30
024500020415     C  N30rcosogg       CHAIN     ansog01l                           30
024600020415     C                   MOVEL(p)  sogdes        desold
024700020415     C     *IN30         IFEQ      '1'
024800020415     C                   SETON                                        4328
024900020415     C                   END
024901090710     c                   if        v1ctip = 'A'
025000020527     C                   if        rcoctgan02 <> 'N   ' and
025100020527     C                             rcoctgan02 <> 'I   ' and
025200030828     C                             rcoctgan02 <> 'M   ' and
025300030828     C                             tipo = 'A'
025400020415     C                   SETON                                        4328
025500020415     c                   end
025501090710     c                   end
025600020415     c                   end
029900020415     c* autotrasport.
030000020415     c                   clear                   v1cdpr
030100020415    2C     v1cpdr        IFeq      0
030200020415     C                   SETON                                        4028
030300020415     c                   else
030400021203     C     kapdv         CHAIN     fiapd01L                           40
030500020415     C     *IN40         IFEQ      *ON
030600080206     C     apdksc        oreq      0
030700020703     C                   SETON                                        28
030800020415     c                   else
030900020419     c                   move      *zeros        com8              8
031000020419     c                   move      apdksc        com8              8
031100080409     c                   movel     apdrsf        v1cdpr
031200020704     c* controllo che attualmente l'associazione autot. e fornitore sia
031300020704     c* corretta
031400020704     c                   if        com8 <> newfor or apdcsf <> newsoc
031500020709     C                   SETON                                        46
031600020419     c                   end
031700020415    3C                   ENDIF
031800020415    1C                   ENDIF
031900020415     c* data da
032000020415     c                   if        v1cdti <> 0
032100020415     c     *eur          test(d)                 v1cdti                 41
032200020415     c                   if        *in41
032300020415     c     *dmy          test(d)                 v1cdti                 41
032400020415     c  n41*dmy          move      v1cdti        dataeur
032500020415     c  n41              move      dataeur       v1cdti
032600020415     c                   end
032700020415     c                   else
032800020415     c                   seton                                        41
032900020415     c                   end
033000020415     c* data al
033100020415     c                   if        v1cdtf <> 0
033200020415     c     *eur          test(d)                 v1cdtf                 45
033300020415     c                   if        *in45
033400020415     c     *dmy          test(d)                 v1cdtf                 45
033500020415     c  n45*dmy          move      v1cdtf        dataeur
033600020415     c  n45              move      dataeur       v1cdtf
033700020415     c                   end
033800020415     c                   else
033900020415     c                   seton                                          2845
034000020415     c                   end
034100020415     c                   if        *in41 or *in45
034200020415     c                   seton                                          28
034300020415     c                   else
034400020415     c                   movel     v1cdti        dataeur
034500020415     c                   movel     dataeur       dataiso
034600020415     c                   movel     dataiso       wdatda            8 0
034700020415     c                   movel     v1cdtf        dataeur
034800020415     c                   movel     dataeur       dataiso
034900020415     c                   movel     dataiso       wdataa            8 0
035000020415      *  Controllo limiti due date
035100020415     C     WDATDA        IFGT      WDATAA
035200020415     C                   SETON                                        414528
035300020415     C                   END
035400020415     c                   end
035500090710     c* controllo che sia già stampato
035600020709     c                   if        not *in28
035700020709     c                   seton                                        2856
035800030828     c* autotrasp.
035801090710     c                   select
035900090710     c                   when      v1ctip='A'
036000030828     c                   exsr      srftt
036200030828     c* aff/defl
036201090710     c                   when      v1ctip='D'
036300030828     c                   exsr      sratt
036301090710     c* coop
036302090710     c                   when      v1ctip='C'
036303090710     c                   exsr      srctt
036400090710     c                   endsl
036500020709     c                   end
036600020709     c* abilito il comando di forzatura solo se l'errore non è bloccante
036700020709     c                   if        not *in28 and *in46
036800020709     c                   seton                                        2806
036900020709     c                   end
037000951010      *
037100020419     C                   ENDSR
037200020415     C*----------------------------------------------------*
037300030828     C* controllo fiftt
037400020415     C*----------------------------------------------------*
037500030828     C     srftt         BEGSR
037600030828     c     kff           setll     fiftt06l
037700030828     c                   do        *hival
037800030828     c     kff1          reade     fiftt06l
037900030828     c                   if        %eof(fiftt06l)
038000030828     c                   leave
038100030828     c                   end
038200030828     c                   if        fttddc > wdataa
038300030828     c                   leave
038400030828     c                   end
038401090710     c                   if        ftttsr <> ' '
038402090710     c                   iter
038403090710     c                   end
038500090710     c                   if        fttdcn =  0
038600030828     c                   seton                                        2855
038700030828     c                   leave
038800030828     c                   else
038900030828     c                   setoff                                       2856
039000030828     c                   end
039100030828     c                   enddo
039200030828     C                   ENDSR
039300030828     C*----------------------------------------------------*
039400030828     C* controllo fiatt
039500030828     C*----------------------------------------------------*
039600030828     C     sratt         BEGSR
039700030828     c                   eval      attflg = 'C'
039800030828     c     katt          setll     fiatt06l
039900030828     c                   do        *hival
040000030828     c     katt          reade     fiatt06l
040100030828     c                   if        %eof(fiatt06l)
040200030828     c                   leave
040300030828     c                   end
040400030828     c* data conteggi e padroncino
040500030828     c                   if        attddc > wdataa or
040600030828     c                             attddc < wdatda or
040700030828     c                             attpdr <> v1cpdr
040800030828     c                   iter
040900030828     c                   end
041000090710     c                   if        attdcn = 0
041100030828     c                   seton                                        2855
041200030828     c                   leave
041300030828     c                   else
041400030828     c                   setoff                                       2856
041500030828     c                   end
041600030828     c                   enddo
041700030828     C                   ENDSR
041701090710     C*----------------------------------------------------*
041702090710     C* controllo fictt
041703090710     C*----------------------------------------------------*
041704090710     C     srctt         BEGSR
041706090710     c     kctt          setll     fiatt06l
041707090710     c                   do        *hival
041708090710     c     kctt          reade     fiatt06l
041709090710     c                   if        %eof(fiatt06l)
041710090710     c                   leave
041711090710     c                   end
041712090710     c* data conteggi e padroncino
041713090710     c                   if        attddc > wdataa or
041714090710     c                             attddc < wdatda or
041715090710     c                             attpdr <> v1cpdr
041716090710     c                   iter
041717090710     c                   end
041718090710     c                   if        attdcn = 0
041719090710     c                   seton                                        2855
041720090710     c                   leave
041721090710     c                   else
041722090710     c                   setoff                                       2856
041723090710     c                   end
041724090710     c                   enddo
041725090710     C                   ENDSR
041800030828     C*----------------------------------------------------*
041900030828     C* AGGIORNO
042000030828     C*----------------------------------------------------*
042100030828     C     CALAGG        BEGSR
042200020415     c* MI COMPONGO L'ISTRUZIONE SQL
042300030828     c                   exsr      istruz
042400020415     c* aggiorno tutti i record di fiftt00f che hanno fornitore old,
042500020415     c* padroncino e data conteggio come da parametri video e numero
042600020415     c* fattura = 0 con il nuovo fornitore
042700020415     c*
042800020415     C/EXEC SQL
042900020415     C+ EXECUTE IMMEDIATE :WrkSqlCmd
043000020415     C/END-EXEC
043100020415     C                   ENDSR
043200980911     C*----------------------------------------------------*
043300980911     C* Reperimento dati società
043400980911     C*----------------------------------------------------*
043500980911     C     REPSOC        BEGSR
043600980911     C*
043700980911     C                   CALLB     'XSOC'
043800980911     C                   PARM                    TIPXSC            6
043900980911     C                   PARM                    SOCXSC            3
044000980911     C                   PARM                    CDSXSC            9 0
044100020415     C                   PARM      *blanks       MODXSC            3
044200980911     C                   PARM      *blanks       RTNXSC            1
044300980911     C                   PARM                    XSOCDS
044400980911     C                   PARM                    KPJBA
044500980911     C*
044600980911     C                   ENDSR
044700951011?     *--------------------------------------------------------------*
044800951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
044900951011?     *--------------------------------------------------------------*
045000951011     C     *INZSR        BEGSR
045100951011      *
045200951011     C     *ENTRY        PLIST
045300951011     C                   PARM                    KPJBA
045400030828     C                   MOVEL     kpjbu         tipo              1
045500030828     c                   if        tipo = 'A'
045600030828     c                   eval      destip = 'AUTOTRASPORTATORI'
045700030828     c                   else
045800030828     c                   eval      destip = 'AFFLUEN./DEFLUEN.'
045900030828     c                   end
046000020418      *  Inizializzo variabili
046100020418     C                   MOVEL     '1'           WTPVID            1
046200020418     C                   MOVEL     'N'           WFINE             1
046300020418     C                   Z-ADD     0             CURR              2 0
046400020418     C                   Z-ADD     0             CURC              2 0
046500020415     C*---------- RICERCA DITTA :
046600020415     C                   MOVEL     'SOC001'      TIPXSC
046700020415     C                   MOVEL     *BLANK        SOCXSC
046800020415     C                   EXSR      REPSOC
046900980911     C     RTNXSC        IFNE      '1'
047000980911     C                   MOVEL     XSOCDS        SOC001
047100980911     C                   MOVEL     xscrgs        RSUT             20
047200020418     c                   else
047300020418     C                   MOVEL     'S'           WFINE             1
047400980911     c                   end
047500020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
047600020415     C                   movel     'F     '      $kcc              6
047700020415     C                   movel     *blanks       $ksc              8
047800020415     C                   callb     'XCAPCLIFOR'
047900020415     C                   parm                    xscsoc
048000020415     C                   parm                    $kcc
048100020415     C                   parm                    $ksc
048200020415     C                   movel     $kcc          forita            6
048300020703     C     Kff           KLIST
048400020703     C                   KFLD                    oldsoc
048500020703     C                   KFLD                    oldfor
048600020703     C                   KFLD                    ftttsr
048700020703     C                   KFLD                    fttfvl
048800020703     C                   KFLD                    v1cpdr
048900020703     C                   KFLD                    wdatda
049000020703     C     Kff1          KLIST
049100020703     C                   KFLD                    oldsoc
049200020703     C                   KFLD                    oldfor
049300020703     C                   KFLD                    ftttsr
049400020703     C                   KFLD                    fttfvl
049500020703     C                   KFLD                    v1cpdr
049600020703     c                   clear                   ftttsr
049700020703     c                   movel     'C'           fttfvl
049800030828     C     Katt          KLIST
049900030828     C                   KFLD                    oldsoc
050000030828     C                   KFLD                    oldfor
050100030828     C                   KFLD                    attflg
050200020703     C     Kfrn          KLIST
050300020703     C                   KFLD                    rcosocieta
050400020703     C                   KFLD                    rcoksc
050500020415     C     Krco          KLIST
050600020415     C                   KFLD                    rcosocieta
050700020415     C                   KFLD                    rcokcc
050800020415     C                   KFLD                    rcoksc
050900021203     C     Kapdv         KLIST
051000021203     C                   KFLD                    apdtip
051100021203     C                   KFLD                    v1cpdr
051200030828     c                   movel     tipo          apdtip
051300951012     C                   ENDSR
051400030828?     *--------------------------------------------------------------*
051500030828?     *  istruz: Operazioni di inizializzazione dati                 *
051600030828?     *--------------------------------------------------------------*
051700030828     C     istruz        BEGSR
051800030828     c                   select
051900030828     c* autotrasportatori
052000030828     c                   when      tipo = 'A'
052100030828     C                   EVAL      WrkSqlCmd
052200030828     C                             =
052300030828     C                             'UPDATE '
052400030828     C                             +
052500030828     C                             ' fiftt00f set fttsoc = '
052600030828     C                             +
052700030828     C                             '''' + newsoc  + ''''
052800030828     C                             +
052900030828     C                             ', fttcdf = '
053000030828     C                             +
053100030828     C                             '''' + newfor + ''''
053200030828     C                             +
053300030828     C                             ' where fttsoc = '
053400030828     C                             +
053500030828     C                             '''' + oldsoc  + ''''
053600030828     C                             +
053700030828     C                             ' AND '
053800030828     C                             +
053900030828     C                             'fttcdf = '
054000030828     C                             +
054100030828     C                             '''' + oldfor + ''''
054200030828     C                             +
054300030828     C                             ' AND '
054400030828     C                             +
054500030828     C                             'fttpdr = '
054600030828     C                             +
054700030828     C                             %editc(v1cpdr:'X')
054800030828     C                             +
054900030828     C                             ' AND '
055000030828     C                             +
055100030828     C                             ' Fttddc BETWEEN '
055200030828     C                             + %editc(wdatda:'X')
055300030828     C                             +
055400030828     C                             ' AND ' + %editc(wdataa:'X')
055500030828     C                             +
055600030828     C                             ' AND '
055700030828     C                             +
055800030828     C                             'fttnff = 0'
055900030828     c* aff/defl.
056000030828     c                   when      tipo = 'D'
056100030828     C                   EVAL      WrkSqlCmd
056200030828     C                             =
056300030828     C                             'UPDATE '
056400030828     C                             +
056500030828     C                             ' fiatt00f set attsoc = '
056600030828     C                             +
056700030828     C                             '''' + newsoc  + ''''
056800030828     C                             +
056900030828     C                             ', attcdf = '
057000030828     C                             +
057100030828     C                             '''' + newfor + ''''
057200030828     C                             +
057300030828     C                             ' where attsoc = '
057400030828     C                             +
057500030828     C                             '''' + oldsoc  + ''''
057600030828     C                             +
057700030828     C                             ' AND '
057800030828     C                             +
057900030828     C                             'attcdf = '
058000030828     C                             +
058100030828     C                             '''' + oldfor + ''''
058200030828     C                             +
058300030828     C                             ' AND '
058400030828     C                             +
058500030828     C                             'attpdr = '
058600030828     C                             + %editc(v1cpdr:'X')
058700030828     C                             +
058800030828     C                             ' AND '
058900030828     C                             +
059000030828     C                             ' attddc BETWEEN '
059100030828     C                             + %editc(wdatda:'X')
059200030828     C                             +
059300030828     C                             ' AND ' + %editc(wdataa:'X')
059400030828     C                             +
059500030828     C                             ' AND '
059600030828     C                             +
059700030828     C                             'attnff = 0'
059800030828     c                   endsl
059900030828     C                   ENDSR
