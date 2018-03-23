000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030207     H/TITLE  Lancio stampa autofattura per conteggi aut./coop.
000400020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951009?     *--------------------------------------------------------------*
000600020419      * ficn54R                                                      *
000700951009      *     *----------------------------------------------*         *
000800030207      *         autofattura conteggi aut./coop.                      *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500020419     Fficn54D   CF   E             WORKSTN
001600020415     Fanfrn01l  iF   E           K DISK
001700020415     Fansog01l  iF   E           K DISK
001800020415     Fanrco01l  iF   E           K DISK
001900020423     Fanuni01l  iF   E           K DISK
002000020531     Ftntbe01l  iF   E           K DISK
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
003900980911     D soc001        E DS                  EXTNAME(xsoc001ds)
004000980911     D xsocds          DS          1000
004100951009     D KPJBA         E DS
004200020415     D ana007ds      e ds                  inz
004300020419     D ficn54ds      e ds                  inz
004400020531     D dblc          e ds                  inz
004500030207     D dblo          e ds                  inz
004600030827     D dbld          e ds                  inz
004601060712     D dblt          e ds                  inz
004700020423     D A009DS        E DS                  EXTNAME(ANA009DS) INZ
004800020423     D XAUTUNIDS     e ds                  inz
004900020415     D*------------------
005000030207     D* DS "TRUL16R" - RICERCA ALFABETICA aut./coop.
005100020415     D*------------------
005200020415     D PARAM           DS                  INZ
005300020415     D* TIPOLOGIA P_=autotrasport.
005400020415     D  PARTIP                 1      2
005500020415     D* RAGIONE SOCIALE aoutotras.
005600020415     D  PARRSC                 3     37
005700020415     D* S.I.
005800020415     D  PARCSF                38     40
005900020415     D* CODICE
006000020415     D  PARPDR                41     47  0
006100020415     D* PARFLG = "3" --> NON ESISTONO autotras. PER LA CHIAVE
006200020415     D*                  ALFABETICA RICHIESTA
006300020415     D  PARFLG                48     48
006301031124     D dspar           DS
006303031124     D  tipo                          1
006304031124     D  causale                       4
006400951009?     *--------------------------------------------------------------*
006500951009?     *  CICLO PRINCIPALE                                            *
006600951009?     *--------------------------------------------------------------*
006700951009      *  Loop gestione videata
006800951009     C     WFINE         DOUEQ     'S'
006900000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
007000951012     C                   END
007100020418     C                   END
007200980312     C*
007300020507      *  Fine Lavoro
007400020507     C     FINE          TAG
007500951009     C                   SETON                                        LR
007600020405?     *--------------------------------------------------------------*
007700020405?     *  GESS01: Gestione prima videata                              *
007800020405?     *--------------------------------------------------------------*
007900020405     C     GESS01        BEGSR
008000020405      *
008100020419     c                   if        *in28
008200020419     c                   setoff                                       28
008300020419     C                   write     y350d01
008400020419     c                   seton                                        28
008500020419     C                   END
008600020415     C                   EXFMT     y350d01
008700020415      *  Fine Lavoro
008800020415     C     *INKC         IFEQ      '1'
008900020415     C                   MOVEL     'S'           WFINE
009000020415     c                   goto      finvd1
009100020415     C                   END
009200020415     C*  effettua ricerche
009300020415     c                   if        *inkd = *on
009400020415     c                   exsr      search
009500020415     c                   end
009600951009      *  Controlli
009700951009     C                   EXSR      CTR01
009800020405     C   28              GOTO      FINVD1
009900020408      *  CONFERMA
010000951013     C     *INKF         IFEQ      '1'
010100020423     c                   clear                   ficn54ds
010200020419     c                   move      xscsoc        f54soc
010300020423     c                   move      v1cuni        f54uni
010400020423     c                   if        oldfor <> *blanks
010500020419     c                   move      oldfor        f54pd1
010600020423     c                   else
010700020423     c                   move      *all'0'       f54pd1
010800020423     c                   end
010900020422     c                   movel     desold        f54dp1
011000020423     c                   if        newfor <> *blanks
011100020419     c                   move      newfor        f54pd2
011200020423     c                   else
011300020423     c                   move      *all'9'       f54pd2
011400020423     c                   end
011500020422     c                   movel     desnew        f54dp2
011600020419     c                   move      wdatda        f54dti
011700020419     c                   move      wdataa        f54dtf
011800020422     c                   move      lisfat        f54lis
011801031124     c                   movel     tipo          f54tip
011802031124     c                   movel     causale       f54cau
011900020419     C                   MOVEL(p)  ficn54ds      KPJBU
012000020419     c     knmus         ifeq      *all'1'
012100030827     c                   select
012200031124     c* prestazioni occasionali
012300031124     c                   when      causale <> ' '
012400031124     c                   call      'FICN96C'
012500020419     c                   parm                    kpjba
012501031124     c* autotrasportatori
012502031124     c                   when      tipo = 'A'
012503031124     c                   call      'FICN56C'
012504031124     c                   parm                    kpjba
012600030207     c* cooperative
012700030827     c                   when      tipo = 'C'
012800030211     c                   call      'FICN65C'
012900030207     c                   parm                    kpjba
013000030827     c* aff/defl
013100030827     c                   when      tipo = 'D'
013200030827     c                   call      'FICN68C'
013300030827     c                   parm                    kpjba
013301060712     c* trazionisti
013302060712     c                   when      tipo = 'T'
013303060712     c                   call      'YCO358C'
013304060712     c                   parm                    kpjba
013305061205     c                   eval      wfine = 'S'
013400030827     c                   endsl
013500020419     c                   else
013700031124     c                   select
013701031124     c* prestaz. occasionali
013800031124     c                   when      causale <> ' '
013900031124     C                   MOVEL     'FN96'        KCOAZ
013901031124     c* autotrasportatori
013902031124     c                   when      tipo = 'A'
013903031124     C                   MOVEL     'FN55'        KCOAZ
014000030207     c* cooperative
014100030827     c                   when      tipo = 'C'
014200030207     C                   MOVEL     'FN65'        KCOAZ
014300030827     c* aff/defl
014400030827     c                   when      tipo = 'D'
014500030827     C                   MOVEL     'FN68'        KCOAZ
014501060712     c* trazionisti
014502060712     c                   when      tipo = 'T'
014503060720     C                   MOVEL     'Y35B'        KCOAZ
014504061205     c                   eval      wfine = 'S'
014600030827     c                   endsl
014700030214     C                   CALL      'BCH10'
014800030214     C                   PARM                    KPJBA
014900020419     c                   end
015000020423     c                   exsr      inz01
015100980911     C                   END                                                    V3CTOT = y35FAT
015200951009      *
015300951009     C     FINVD1        ENDSR
015400020415     C************************************************************
015500020415     C* RICERCHE
015600020415     C************************************************************
015700020415     C     SEARCH        BEGSR
015800020415     C*
015900020415     C* determino Riga/Colonna del cursore
016000020415     C                   MOVE      nrg           R$$
016100020415     C                   MOVE      ncl           C$$
016200020415     C                   Z-ADD     RIRI          CURR
016300020415     C                   Z-ADD     COCO          CURC
016400020418     c                   setoff                                       5354
016500020415     C*
016600020415     C* I campi che al momento dell'F4 risultano protetti non devono
016700020415     C* essere abilitati alla funzione di ricerca
016800020415     C*
0169000204151    C                   SELECT
017000020423     C* FMTD1  - ricerca unità di reg.
017100020423    >C     H1NMFL        WHEneq    'V1CUNI'
017200020423     C                   RESET                   A009DS
017300020423     C                   MOVE      XSCSOC        SOC009
017400020423     C                   EVAL      OPZ009 = '01'
017500020423     C                   EVAL      Kpjbu = A009DS
017600020423     C                   CALL      'ANA009R'
017700020423     C                   PARM                    KPJBA
017800020423     C                   EVAL      A009DS = Kpjbu
017900020423      *
018000020423     C     UNI009        IFNE      *BLANK
018100020423     C                   MOVE      UNI009        v1cuni
018200020423     C                   ENDIF
018300020415     C* FMTD1  - ricerca fornitore
018400030827    >C                   WHEn      (h1nmfl = 'OLDFOR' or
018500060719    >C                             h1nmfl = 'NEWFOR') and
018501060719    >C                             (tipo = 'T' or tipo = 'A')
018600020419     C                   movel     xscsoc        parcsf
018700020415     C*  deposita la KPJBU
018800020415     C                   MOVEL     KPJBU         DKPJBU          256
018900020419     c                   if        h1nmfl = 'OLDFOR'
019000020415     C                   MOVEl(p)  desold        PARRSC
019100020415     c                   else
019200020415     C                   MOVEl(p)  desnew        PARRSC
019300020415     c                   end
019400020419     C                   Z-ADD     0             PARPDR
019401060719     c                   if        tipo = 'A'
019500020415     C                   eval      partip = 'P '
019501060719     c                   else
019502060719     C                   eval      partip = 'T '
019503060719     c                   end
019600020415     C                   MOVEL     *BLANKS       PARFLG
019700020419     C                   MOVEL(p)  PARAM         KPJBU
019800020415     C                   CALL      'TRUL16R'
019900020415     C                   PARM                    KPJBA
020000020415     C                   MOVEL     KPJBU         PARAM
020100020415IF  2C     PARFLG        IFEQ      '3'
020200020419     c                   if        h1nmfl = 'OLDFOR'
020300020415     C                   MOVEL     *BLANKS       oldfor
020400020415     C                   SETON                                        53
020500020415     c                   else
020600020415     C                   MOVEL     *BLANKS       newfor
020700020415     C                   SETON                                        54
020800020415     c                   end
020900020415     c                   if        *in53 or *in54
021000020415     C                   SETON                                        28
021100020415     c                   end
021200020415X   2C                   ELSE
021300020419     c                   if        h1nmfl = 'OLDFOR'
021400020419     c                   move      *zeros        oldfor
021500020419     C                   MOVE      PARPDR        oldfor
021600020415     C                   MOVEL     PARRSC        desold
021700020415     c                   else
021800020419     c                   move      *zeros        newfor
021900020419     C                   MOVE      PARPDR        newfor
022000020415     C                   MOVEL     PARRSC        desnew
022100020415E   2C                   ENDIF
022200020415E   2C                   ENDIF
022300020415     C* REIMPOSTA LA KPJBU
022400020415     C                   MOVEL     DKPJBU        KPJBU
022500020415     C                   ENDsl
022600020415     C* imposto pos. del cursore
022700020415     C                   Z-ADD     CURR          H1RIGA
022800020415     C                   Z-ADD     CURC          H1COLO
022900020415     c*
023000020415     C                   ENDsr
023100951009?     *--------------------------------------------------------------*
023200951009?     *  CTR01: Controlli prima videata                              *
023300951009?     *--------------------------------------------------------------*
023400951009     C     CTR01         BEGSR
023500951009      *
023600020419     c                   setoff                                       28
023700020423     C* Verifico unità di registrazione
023800020423     c                   movel     *blanks       v1cdun
023801060712     c                   if        tipo <> 'T'
023900020423     c                   if        v1cuni = *blanks
024000020423     C                   SETON                                        4228
024100020423     c                   else
024200020423     c     kuni          chain     anuni01l                           42
024300020423     c                   if        not *in42
024400020423     c                   movel     unidesbrev    v1cdun
024500020423     C* CONTROLLO SE L'UTENTE E' ABILITATO AD USARE IL CDC
024600020423     C                   CLEAR                   XAUTUNIDS
024700020423     C                   MOVEL     KNMUS         XAUPRF
024800020423     C                   MOVEL     XSCSOC        XAUSOC
024900020423     C                   MOVEL     V1cuni        XAUUNI
025000020423     C                   CALL      'XAUTUNI'
025100020423     C                   PARM                    XAUTUNIDS
025200020423     C                   IF        XAUABL = '0'
025300020423     C                   SETON                                        4228
025400020423     C                   END
025500020423     c                   else
025600020423     C                   SETON                                        28
025700020423     c                   end
025800020423     c                   end
025801060712     c                   end
025900020415     C* Controllo fornitore old
026000020423     C     oldfor        IFne      *blanks
026100020419     C                   move      xscsoc        rcosocieta
026200020415     C                   move      FORITA        rcokcc
026300020415     C                   move      oldfor        rcoksc
026400020415     c                   movel     *blank        sogdes
026500020415     C     Kfrn          CHAIN     anfrn01l                           30
026600020415     C  N30Krco          CHAIN     anrco01l                           30
026700020415     C  N30rcosogg       CHAIN     ansog01l                           30
026800020415     C                   MOVEL(p)  sogdes        desold
026900020415     C     *IN30         IFEQ      '1'
027000020415     C                   SETON                                        4328
027100020415     C                   END
027101060712     c                   if        tipo <> 'T'
027200020527     C                   if        rcoctgan02 <> 'N   ' and
027300020527     C                             rcoctgan02 <> 'I   ' and
027400030827     C                             rcoctgan02 <> 'M   ' and
027500030827     C                             tipo = 'A'
027600020415     C                   SETON                                        4328
027700020415     c                   end
027800020423     C                   if        rcounita <> v1cuni
027900020423     C                   SETON                                        432842
028000020423     c                   end
028001060712     c                   else
028002060712     C                   if        rcoctgan02 <> 'T   '
028006060712     C                   SETON                                        4328
028007060712     c                   end
028100020415     c                   end
028101060712     c                   end
028200020415     C* Controllo fornitore new
028300020415     C     newfor        IFEQ      *blanks
028400020419     C                   movel     oldfor        newfor
028500020423     C                   end
028600020423     C     newfor        IFne      *blanks
028700020419     C                   move      xscsoc        rcosocieta
028800020415     C                   move      FORITA        rcokcc
028900020415     C                   move      newfor        rcoksc
029000020415     c                   movel     *blank        sogdes
029100020415     C     Kfrn          CHAIN     anfrn01l                           30
029200020415     C  N30Krco          CHAIN     anrco01l                           30
029300020415     C  N30rcosogg       CHAIN     ansog01l                           30
029400020415     C                   MOVEL(p)  sogdes        desnew
029500020415     C     *IN30         IFEQ      '1'
029600020415     C                   SETON                                        4428
029700020415     C                   END
029701060712     c                   if        tipo <> 'T'
029800020527     C                   if        rcoctgan02 <> 'N   ' and
029900020527     C                             rcoctgan02 <> 'I   ' and
030000030827     C                             rcoctgan02 <> 'M   ' and
030100030827     C                             tipo = 'A'
030200020415     C                   SETON                                        4428
030300020415     c                   end
030400020423     C                   if        rcounita <> v1cuni
030500020423     C                   SETON                                        442842
030600020423     c                   end
030601060712     c                   else
030602060712     C                   if        rcoctgan02 <> 'T   '
030606060712     C                   SETON                                        4428
030607060712     c                   end
030700020415     c                   end
030701060712     c                   end
030800020415     c* data da
030900020415     c                   if        v1cdti <> 0
031000020415     c     *eur          test(d)                 v1cdti                 41
031100020415     c                   if        *in41
031200020415     c     *dmy          test(d)                 v1cdti                 41
031300020415     c  n41*dmy          move      v1cdti        dataeur
031400020415     c  n41              move      dataeur       v1cdti
031500020415     c                   end
031600020415     c                   else
031700020415     c                   seton                                        41
031800020415     c                   end
031900020415     c* data al
032000020415     c                   if        v1cdtf <> 0
032100020415     c     *eur          test(d)                 v1cdtf                 45
032200020415     c                   if        *in45
032300020415     c     *dmy          test(d)                 v1cdtf                 45
032400020415     c  n45*dmy          move      v1cdtf        dataeur
032500020415     c  n45              move      dataeur       v1cdtf
032600020415     c                   end
032700020415     c                   else
032800020423     c                   if        not *in41
032900020423     c                   move      v1cdti        v1cdtf
033000020423     c                   else
033100020415     c                   seton                                          2845
033200020415     c                   end
033300020423     c                   end
033400020415     c                   if        *in41 or *in45
033500020415     c                   seton                                          28
033600020415     c                   else
033700020415     c                   movel     v1cdti        dataeur
033800020415     c                   movel     dataeur       dataiso
033900020415     c                   movel     dataiso       wdatda            8 0
034000020415     c                   movel     v1cdtf        dataeur
034100020415     c                   movel     dataeur       dataiso
034200020415     c                   movel     dataiso       wdataa            8 0
034300020415      *  Controllo limiti due date
034400020415     C     WDATDA        IFGT      WDATAA
034500020415     C                   SETON                                        414528
034600020415     C                   END
034700020415     c                   end
034800951010      *
034900020419     C                   ENDSR
035000020423?     *--------------------------------------------------------------*
035100020423?     *  inz01  inizializza video                                    *
035200020423?     *--------------------------------------------------------------*
035300020423     C     inz01         BEGSR
035400020423      *
035500020423     c                   setoff                                       28
035600020423     c                   movel     *blanks       v1cdun
035700020423     c                   movel     *blanks       v1cuni
035800020423     C                   movel     *blanks       oldfor
035900020423     C                   movel     *blanks       newfor
036000020423     C                   movel     *blanks       desold
036100020423     C                   movel     *blanks       desnew
036300020423      *
036400020423     C                   ENDSR
036500980911     C*----------------------------------------------------*
036600980911     C* Reperimento dati società
036700980911     C*----------------------------------------------------*
036800980911     C     REPSOC        BEGSR
036900980911     C*
037000980911     C                   CALLB     'XSOC'
037100980911     C                   PARM                    TIPXSC            6
037200980911     C                   PARM                    SOCXSC            3
037300980911     C                   PARM                    CDSXSC            9 0
037400020415     C                   PARM      *blanks       MODXSC            3
037500980911     C                   PARM      *blanks       RTNXSC            1
037600980911     C                   PARM                    XSOCDS
037700980911     C                   PARM                    KPJBA
037800980911     C*
037900980911     C                   ENDSR
038000951011?     *--------------------------------------------------------------*
038100951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
038200951011?     *--------------------------------------------------------------*
038300951011     C     *INZSR        BEGSR
038400951011      *
038500951011     C     *ENTRY        PLIST
038600951011     C                   PARM                    KPJBA
038700031124     c                   movel     kpjbu         dspar
038800020418      *  Inizializzo variabili
038900020418     C                   MOVEL     '1'           WTPVID            1
039000020418     C                   MOVEL     'N'           WFINE             1
039100020503     C                   MOVEL     'S'           lisfat
039200020418     C                   Z-ADD     0             CURR              2 0
039300020418     C                   Z-ADD     0             CURC              2 0
039400020415     C*---------- RICERCA DITTA :
039500020415     C                   MOVEL     'SOC001'      TIPXSC
039600020415     C                   MOVEL     *BLANK        SOCXSC
039700020415     C                   EXSR      REPSOC
039800980911     C     RTNXSC        IFNE      '1'
039900980911     C                   MOVEL     XSOCDS        SOC001
040000980911     C                   MOVEL     xscrgs        RSUT             20
040100020418     c                   else
040200020507     C                   goto      FINE
040300980911     c                   end
040400020415     C* Richiamo XCAPCLIFOR per il reperimento del capoconto fornitori
040500020415     C                   movel     'F     '      $kcc              6
040600020415     C                   movel     *blanks       $ksc              8
040700020415     C                   callb     'XCAPCLIFOR'
040800020415     C                   parm                    xscsoc
040900020415     C                   parm                    $kcc
041000020415     C                   parm                    $ksc
041100020415     C                   movel     $kcc          forita            6
041200020423     C     Kuni          KLIST
041300020423     C                   KFLD                    xscsoc
041400020423     C                   KFLD                    v1cuni
041500020423     C     Kfrn          KLIST
041600020423     C                   KFLD                    rcosocieta
041700020423     C                   KFLD                    rcoksc
041800020415     C     Krco          KLIST
041900020415     C                   KFLD                    rcosocieta
042000020415     C                   KFLD                    rcokcc
042100020415     C                   KFLD                    rcoksc
042200020531     C     Ktbe          KLIST
042300020531     C                   KFLD                    tbecod
042400020531     C                   KFLD                    tbeke1
042401061206     C                   movel     'S'           lisfat
042402061129     c                   eval      vdes1 = 'Lista fatture emesse..:'
042500030827     c                   select
042600020531      *  lettura tabella BLC
042700030827     c                   when      tipo = 'A'
042800020531     c                   movel     'BLC'         tbecod
042900020531     c                   movel(p)  '1'           tbeke1
043000020531     c     ktbe          chain     tntbe01l
043100020531     c                   if        %found(tntbe01l)
043200020531     c                   movel     tbeuni        dblc
043300020531     c* data protocollo
043400020531     c                   move      §blcdtp       dataiso
043500020531     c                   move      dataiso       dataeur
043600020531     c                   else
043700020531     c                   clear                   dblc
043800020531     c                   end
043900030207      *  lettura tabella BLO
044000030827     c                   when      tipo = 'C'
044100030207     c                   movel     'BLO'         tbecod
044200030207     c                   movel(p)  '1'           tbeke1
044300030207     c     ktbe          chain     tntbe01l
044400030207     c                   if        %found(tntbe01l)
044500030207     c                   movel     tbeuni        dblo
044600030207     c* data protocollo
044700030207     c                   move      §blodtp       dataiso
044800030207     c                   else
044900030207     c                   clear                   dblo
045000030207     c                   end
045100030827      *  lettura tabella BLD
045200030827     c                   when      tipo = 'D'
045300030827     c                   movel     'BLD'         tbecod
045400030827     c                   movel(p)  '1'           tbeke1
045500030827     c     ktbe          chain     tntbe01l
045600030827     c                   if        %found(tntbe01l)
045700030827     c                   movel     tbeuni        dbld
045800030827     c* data protocollo
045900030827     c                   move      §blddtp       dataiso
046000030827     c                   else
046100030827     c                   clear                   dbld
046200030827     c                   end
046201060712     c* trazionisti
046202060712     c                   when      tipo = 'T'
046203061129     c                   eval      vdes1 = 'Elenco raccomandate...:'
046204061206     C                   movel     'N'           lisfat
046205060712     c                   seton                                        88
046206060712     c                   movel     'BLT'         tbecod
046207060712     c                   movel(p)  '1'           tbeke1
046208060712     c     ktbe          chain     tntbe01l
046209060712     c                   if        %found(tntbe01l)
046210060712     c                   movel     tbeuni        dblt
046211060712     c* data protocollo
046212060712     c                   move      §bltdtp       dataiso
046213060712     c                   move      dataiso       dataeur
046214060712     c                   else
046215060712     c                   clear                   dblt
046216060712     c                   end
046217060719     c     kcdaz         comp      'FN4S'                                 22
046300030827     c                   endsl
046400030827     c                   if        %found(tntbe01l)
046500030827     c                   move      dataiso       dataeur
046600030827     c                   move      dataeur       v1cdti
046700030827     c                   move      dataeur       v1cdtf
046800030827     c                   else
046900030827     c                   move      0             v1cdti
047000030827     c                   move      0             v1cdtf
047100030827     c                   end
047200951012     C                   ENDSR
